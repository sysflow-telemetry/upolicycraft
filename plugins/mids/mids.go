//
// Copyright (C) 2021 IBM Corporation.
//
// Authors:
// William Blair <wdblair@ibm.com>
// Frederico Araujo <frederico.araujo@ibm.com>
// Teryl Taylor <terylt@ibm.com>
//
package main

import (
	"fmt"
	"os"
	"sync"
	"time"

	"github.com/sysflow-telemetry/sf-apis/go/logger"
	"github.com/sysflow-telemetry/sf-apis/go/plugins"
	"github.com/sysflow-telemetry/sf-processor/core/cache"
	"github.com/sysflow-telemetry/sf-processor/core/flattener"
	"github.com/sysflow-telemetry/sf-processor/core/policyengine/engine"
)

const (
	pluginName  string = "mids"
	channelName string = "eventchan"
	historySize int    = 2
)

// Incident type
type Incident struct {
	State string
	Desc  string
}

// Plugin exports a symbol for this plugin.
var Plugin IntrusionDetectionSystem

// IntrusionDetectionSystem defines a Microservice-Aware Intrusion Detection System (MIDS)
type IntrusionDetectionSystem struct {
	stopped bool
	outCh   chan *engine.Record
	new     func(int64) *SecurityAutomaton
	sas     map[int64]*SecurityAutomaton
	history []*engine.Record
	last    int64
}

// NewIntrusionDetectionSystem creates a new plugin instance.
func NewIntrusionDetectionSystem() plugins.SFProcessor {
	return new(IntrusionDetectionSystem)
}

// GetName returns the plugin name.
func (ids *IntrusionDetectionSystem) GetName() string {
	return pluginName
}

// Init initializes the plugin with a configuration map.
func (ids *IntrusionDetectionSystem) Init(conf map[string]string) error {
	c := CreateConfig(conf)
	logger.Trace.Printf("Monitoring Model: %s", c.Model)

	ids.Compile(c.Model)

	return nil
}

// Register registers the MIDS to the plugin cache.
func (ids *IntrusionDetectionSystem) Register(pc plugins.SFPluginCache) {
	logger.Trace.Println("\nLoaded Intrusion Detection System!")
	pc.AddProcessor(pluginName, NewIntrusionDetectionSystem)
}

// Process implements the main interface of the plugin.
func (ids *IntrusionDetectionSystem) Process(ch interface{}, wg *sync.WaitGroup) {
	in := ch.(*flattener.FlatChannel).In

	defer wg.Done()

	logger.Trace.Println("\nStarting Intrusion Detection System")
	logger.Trace.Println("\nExample channel capacity:", cap(in))

	out := func(r *engine.Record) { ids.outCh <- r }

	start := time.Now()

	for {
		fc, ok := <-in

		if !ok {
			logger.Trace.Println("\nChannel closed. Shutting down.")
			break
		}

		record := engine.NewRecord(*fc, cache.GetInstance())
		ids.Event(record, out)
	}

	elapsed := time.Since(start)

	f, _ := os.Create("/tmp/mids-perf")
	fmt.Fprintf(f, "MIDS:%d\n", elapsed.Nanoseconds())
	f.Close()
	logger.Trace.Println("\nExiting Intrusion Detection System")
	ids.Cleanup()
}

// SetOutChan sets the output channel of the plugin.
func (ids *IntrusionDetectionSystem) SetOutChan(ch interface{}) {
	ids.outCh = (ch.(*engine.RecordChannel)).In
}

// Cleanup tears down plugin resources.
func (ids *IntrusionDetectionSystem) Cleanup() {
	out := func(r *engine.Record) { ids.outCh <- r }

	if ids.outCh != nil && !ids.stopped {
		logger.Trace.Println("\nCleaning up MIDS")
		for tid := range ids.sas {
			logger.Trace.Printf("Cleaning up %d\n", tid)
			sa := ids.sas[tid]
			sa.TypeCheckTrace(out)
		}
		close(ids.outCh)
		ids.stopped = true
	}
}

// Compile parses and interprets an input policy defined in path.
func (ids *IntrusionDetectionSystem) Compile(path string) {
	ids.new = func(tID int64) *SecurityAutomaton {
		logger.Trace.Printf("\nGenerating a new automaton for thread %d", tID)
		sa := NewSecurityAutomatonFromJSON(tID, path)
		sa.parent = ids
		return sa
	}
	ids.sas = make(map[int64]*SecurityAutomaton)
}

// Event handles an event given to the Intrusion Detection System
func (ids *IntrusionDetectionSystem) Event(r *engine.Record, out func(r *engine.Record)) {
	tid := engine.Mapper.MapInt(engine.SF_PROC_TID)(r)
	op := engine.Mapper.MapStr(engine.SF_OPFLAGS)(r)
	ppid := engine.Mapper.MapInt(engine.SF_PPROC_PID)(r)

	logger.Trace.Printf("\nExamining record for tid: %d ppid: %d", tid, ppid)
	ids.AddToHistory(r)

	// Does there exist a security automaton for this thread?
	if sa, ok := ids.sas[tid]; ok {
		logger.Trace.Printf("\nDispatching event to tid: %d", tid)
		sa.Event(r, out)
	} else {
		logger.Trace.Printf("\nCreating new SA for tid: %d ppid: %d", tid, ppid)
		sa := ids.new(tid)
		ids.sas[tid] = sa

		logger.Trace.Printf("Looking for ppid: %d", ppid)

		// Does there exist a SA for the parent thread and is this a CLONE?
		if psa, ok := ids.sas[ppid]; ok {
			if op == "CLONE" && psa.FSM.Current() != psa.Initial {
				logger.Trace.Printf("Advancing to parents state!\nState: %s\n", psa.FSM.Current())
				sa.FSM.SetState(psa.FSM.Current())
				return
			}
		}

		sa.Event(r, out)
	}
}

// AddToHistory appends a record to a sliding window of records
func (ids *IntrusionDetectionSystem) AddToHistory(r *engine.Record) {
	ids.history = append([]*engine.Record{r}, ids.history...)
	if len(ids.history) > historySize {
		ids.history = ids.history[:historySize]
	}
}

var incidentCtxKey int = 0

type MRMContext engine.Context

// AddIncident adds an Alert generated by the Reference Monitor to the context.
func (c MRMContext) AddIncident(a Incident) {
	if incidentCtxKey == 0 {
		incidentCtxKey = len(c)
		c = append(c, nil)
	}

	if c[incidentCtxKey] == nil {
		c[incidentCtxKey] = make([]Incident, 0)
	}
	c[incidentCtxKey] = append(c[incidentCtxKey].([]Incident), a)
}

// GetIncidents fetches the alerts in the context.
func (c MRMContext) GetIncidents() []Incident {
	if incidentCtxKey == 0 {
		incidentCtxKey = len(c)
		c = append(c, make([]interface{}, 1))
	}

	if c[incidentCtxKey] != nil {
		return c[incidentCtxKey].([]Incident)
	}
	return nil
}

// This function is not run when module is used as a plugin.
func main() {}
