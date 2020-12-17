//
// Copyright (C) 2020 IBM Corporation.
//
// Authors:
// William Blair <wdblair@ibm.com>
// Frederico Araujo <frederico.araujo@ibm.com>
// Teryl Taylor <terylt@ibm.com>
//
package main

import (
	"sync"

	"github.com/sysflow-telemetry/sf-apis/go/logger"
	"github.com/sysflow-telemetry/sf-apis/go/plugins"
	"github.com/sysflow-telemetry/sf-processor/core/cache"
	"github.com/sysflow-telemetry/sf-processor/core/flattener"
	"github.com/sysflow-telemetry/sf-processor/core/policyengine/engine"
)

const (
	pluginName  string = "mrm"
	channelName string = "eventchan"
	historySize int    = 2
)

// Incident type
type Incident struct {
	State string
	Desc  string
}

// Plugin exports a symbol for this plugin.
var Plugin ReferenceMonitor

// ReferenceMonitor defines a Microservice-Aware Reference Monitor (MRM)
type ReferenceMonitor struct {
	stopped bool
	outCh   chan *engine.Record
	new     func(int64) *SecurityAutomaton
	sas     map[int64]*SecurityAutomaton
	history []*engine.Record
	last    int64
}

// NewReferenceMonitor creates a new plugin instance.
func NewReferenceMonitor() plugins.SFProcessor {
	return new(ReferenceMonitor)
}

// GetName returns the plugin name.
func (rm *ReferenceMonitor) GetName() string {
	return pluginName
}

// Init initializes the plugin with a configuration map.
func (rm *ReferenceMonitor) Init(conf map[string]string) error {
	c := CreateConfig(conf)
	logger.Trace.Printf("Monitoring Model: %s", c.Model)

	rm.Compile(c.Model)

	return nil
}

// Register registers plugin to plugin cache.
func (rm *ReferenceMonitor) Register(pc plugins.SFPluginCache) {
	logger.Trace.Println("Loaded Reference Monitor!")
	pc.AddProcessor(pluginName, NewReferenceMonitor)
}

// Process implements the main interface of the plugin.
func (rm *ReferenceMonitor) Process(ch interface{}, wg *sync.WaitGroup) {
	in := ch.(*flattener.FlatChannel).In

	defer wg.Done()

	logger.Trace.Println("Starting Reference Monitor")
	logger.Trace.Println("Example channel capacity:", cap(in))

	out := func(r *engine.Record) { rm.outCh <- r }

	for {
		fc, ok := <-in

		if !ok {
			logger.Trace.Println("Channel closed. Shutting down.")
			break
		}
		record := engine.NewRecord(*fc, cache.GetInstance())
		rm.Event(record, out)
	}
	logger.Trace.Println("Exiting Reference Monitor")
	rm.Cleanup()
}

// SetOutChan sets the output channel of the plugin.
func (rm *ReferenceMonitor) SetOutChan(ch interface{}) {
	rm.outCh = (ch.(*engine.RecordChannel)).In
}

// Cleanup tears down plugin resources.
func (rm *ReferenceMonitor) Cleanup() {
	logger.Trace.Println("Cleaning up MRM")
	if rm.outCh != nil && !rm.stopped {
		close(rm.outCh)
		rm.stopped = true
	}
}

// Compile parses and interprets an input policy defined in path.
func (rm *ReferenceMonitor) Compile(path string) {
	rm.new = func(tID int64) *SecurityAutomaton {
		logger.Trace.Printf("Generating a new automaton for thread %d", tID)
		return NewSecurityAutomatonFromJSON(tID, path)
	}
	rm.sas = make(map[int64]*SecurityAutomaton)
}

// Event handles an event given to the Reference Monitor
func (rm *ReferenceMonitor) Event(r *engine.Record, out func(r *engine.Record)) {
	tid := engine.Mapper.MapInt(engine.SF_PROC_TID)(r)
	ppid := engine.Mapper.MapInt(engine.SF_PPROC_PID)(r)

	logger.Trace.Printf("Examining record for tid: %d ppid: %d", tid, ppid)
	rm.AddToHistory(r)

	// Does there exist a security automaton for this thread?
	if sa, ok := rm.sas[tid]; ok {
		logger.Trace.Printf("Dispatching event to tid: %d", tid)
		sa.Event(r, out)
	} else {
		logger.Trace.Printf("Creating new SA for tid: %d ppid: %d", tid, ppid)
		sa := rm.new(tid)
		sa.Event(r, out)
		rm.sas[tid] = sa
	}
}

// AddToHistory appends a record to a sliding window of records
func (rm *ReferenceMonitor) AddToHistory(r *engine.Record) {
	rm.history = append([]*engine.Record{r}, rm.history...)
	if len(rm.history) > historySize {
		rm.history = rm.history[:historySize]
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
