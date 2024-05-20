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
	pluginName  string = "mpm"
	channelName string = "eventchan"
	historySize int    = 16
)

// Incident type
type Incident struct {
	State string
	Desc  string
}

// Plugin exports a symbol for this plugin.
var Plugin PolicyMonitor

// PolicyMonitor defines a Microservice-Aware Policy Monitor (MPM)
type PolicyMonitor struct {
	stopped bool
	outChs  []chan *engine.Record
	new     func(int64) *SecurityAutomaton
	sas     map[int64]*SecurityAutomaton
	history []*engine.Record
	last    int64
}

// NewPolicyMonitor creates a new plugin instance.
func NewPolicyMonitor() plugins.SFProcessor {
	return new(PolicyMonitor)
}

// GetName returns the plugin name.
func (ids *PolicyMonitor) GetName() string {
	return pluginName
}

// Init initializes the plugin with a configuration map.
func (mpm *PolicyMonitor) Init(conf map[string]interface{}) error {
	c := CreateConfig(conf)
	logger.Trace.Printf("Monitoring Effect Graph: %s", c.Model)

	mpm.Compile(c.Model)

	return nil
}

// Register registers the MIDS to the plugin cache.
func (mpm *PolicyMonitor) Register(pc plugins.SFPluginCache) {
	logger.Trace.Println("\nLoaded Policy Monitor!")
	pc.AddProcessor(pluginName, NewPolicyMonitor)
}

// Process implements the main interface of the plugin.
func (mpm *PolicyMonitor) Process(ch interface{}, wg *sync.WaitGroup) {
	in := ch.(*flattener.FlatChannel).In

	defer wg.Done()

	logger.Trace.Println("\nStarting Microservice-Aware Policy Monitor (MPM)")
	logger.Trace.Println("\nExample channel capacity:", cap(in))

	out := func(r *engine.Record) {
		for _, ch := range mpm.outChs {
			ch <- r
		}
	}

	for {
		fc, ok := <-in

		if !ok {
			logger.Trace.Println("\nChannel closed. Shutting down.")
			break
		}

		record := engine.NewRecord(*fc, cache.GetInstance())
		mpm.Event(record, out)
	}

	logger.Trace.Println("\nExiting Policy Monitor")
	mpm.Cleanup()
}

// SetOutChan sets the output channel of the plugin.
func (mpm *PolicyMonitor) SetOutChan(chs []interface{}) {
	for _, ch := range chs {
		mpm.outChs = append(ids.outChs, ch.(*engine.RecordChannel).In)
	}
	//mpm.outCh = (chs[0].(*engine.RecordChannel)).In
}

// ChannelsEmpty Are the MPM channels empty?
func (mpm *PolicyMonitor) ChannelsEmpty() bool {
	for _, ch := range ids.outChs {
		if ch != nil {
			return false
		}
	}
	return true
}

// BroadcastRecord submits a record to the output channels.
func (mpm *PolicyMonitor) BroadcastRecord(r *engine.Record) {
	for _, ch := range mpm.outChs {
		if ch != nil {
			ch <- r
		}
	}
}

// CloseChannels closes all output channels.
func (ids *PolicyMonitor) CloseChannels() {
	for _, ch := range ids.outChs {
		if ch != nil {
			close(ch)
		}
	}
}

// Cleanup tears down plugin resources.
func (mpm *PolicyMonitor) Cleanup() {
	out := func(r *engine.Record) { mpm.BroadcastRecord(r) }

	if !mpm.stopped {
		logger.Trace.Println("\nCleaning up MPM")
		for tid := range ids.sas {
			logger.Trace.Printf("Cleaning up %d\n", tid)
			sa := mpm.sas[tid]
			sa.TypeCheckTrace(out)
		}
		mpm.CloseChannels()
		mpm.stopped = true
	}
}

// Compile parses and interprets an input policy defined in path.
func (mpm *PolicyMonitor) Compile(path string) {
	mpm.new = func(tID int64) *SecurityAutomaton {
		logger.Trace.Printf("\nGenerating a new automaton for thread %d", tID)
		sa := NewSecurityAutomatonFromJSON(tID, path)
		sa.parent = mpm
		return sa
	}
	mpm.sas = make(map[int64]*SecurityAutomaton)
}

// Event handles an event given to the PolicyMonitor.
func (mpm *PolicyMonitor) Event(r *engine.Record, out func(r *engine.Record)) {
	tid := engine.Mapper.MapInt(engine.SF_PROC_TID)(r)
	op := engine.Mapper.MapStr(engine.SF_OPFLAGS)(r)
	ppid := engine.Mapper.MapInt(engine.SF_PPROC_PID)(r)

	logger.Trace.Printf("\nExamining record for tid: %d ppid: %d", tid, ppid)
	mpm.AddToHistory(r)

	// Does there exist a security automaton for this thread?
	if sa, ok := mpm.sas[tid]; ok {
		logger.Trace.Printf("\nDispatching event to tid: %d", tid)
		sa.Event(r, out)
	} else {
		logger.Trace.Printf("\nCreating new SA for tid: %d ppid: %d", tid, ppid)
		sa := mpm.new(tid)
		mpm.sas[tid] = sa

		logger.Trace.Printf("Looking for ppid: %d", ppid)

		// Does there exist a SA for the parent thread and is this a CLONE?
		if psa, ok := mpm.sas[ppid]; ok {
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
func (mpm *PolicyMonitor) AddToHistory(r *engine.Record) {
	mpm.history = append([]*engine.Record{r}, mpm.history...)
	if len(mpm.history) > historySize {
		mpm.history = mpm.history[:historySize]
	}
}

var incidentCtxKey int = 0

type MPMContext engine.Context

// AddIncident adds an Alert generated by the Policy Monitor to the context.
func (c MPMContext) AddIncident(a Incident) {
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
func (c MPMContext) GetIncidents() []Incident {
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
