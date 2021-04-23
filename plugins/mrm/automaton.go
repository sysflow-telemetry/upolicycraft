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
	"encoding/json"
	"errors"
	"fmt"
	"io/ioutil"
	"os"
	"regexp"
	"strings"

	"github.com/looplab/fsm"
	"github.com/sysflow-telemetry/sf-apis/go/logger"
	"github.com/sysflow-telemetry/sf-processor/core/policyengine/engine"
)

const (
	SA_MMAP_EVENT string = "MMAP"
)

const (
	EXIT_SUCCESS      string = "0"
	EXIT_ERROR        string = "1"
	EXIT_SHELL_MISUSE string = "2"
	EXIT_SIGKILL      string = "9"
)

const (
	SA_EXIT_EDGE string = "EXIT"
	SA_EXIT_NODE string = "NORMAL_EXIT"
)

type Constraint struct {
	Node        string                 `json:"node"`
	Constraints map[string]interface{} `json:"constraints"`
}

type Edge struct {
	Src   string `json:"src"`
	Dst   string `json:"dst"`
	Label string `json:"label"`
}

type BehaviorModel struct {
	Image        string        `json:"image"`
	ImageId      string        `json:"imageId"`
	ModelVersion string        `json:"modelVersion"`
	Initial      string        `json:"initial"`
	Nodes        []string      `json:"nodes"`
	Constraints  []*Constraint `json:"constraints"`
	Edges        []*Edge       `json:"edges"`
}

type Observation struct {
	Used   bool
	Record *engine.Record
}

// SecurityAutomaton defines a Security Automaton for a monitored application thread.
type SecurityAutomaton struct {
	TID           int64
	FileWhiteList []string
	Initial       string
	FSM           *fsm.FSM
	Model         *BehaviorModel
	History       []Observation
}

// RegexpArrayMem checks whether the string x matches any of the regexes in options.
func RegexpArrayMem(options []string, x string) bool {
	for _, o := range options {
		match, _ := regexp.MatchString(o, x)
		if match {
			return true
		}
	}
	return false
}

// ParseSlice obtains a set of strings from a JSON slice
func ParseSlice(slice interface{}) map[string]bool {
	elements := make(map[string]bool)

	sliceInterface := slice.([]interface{})

	for _, v := range sliceInterface {
		elements[v.(string)] = true
	}

	return elements
}

//FilterOperations removes operations not found in the behavior model
func FilterOperations(ops map[string]bool) {
	ops["TRUNCATE"] = false
}

//TranslateOperation adapts a SysFlow OperationName to a MRM Operation
func TranslateOperation(op string) string {
	var translation = map[string]string{
		"SEND": "WRITE",
		"RECV": "READ",
	}

	return translation[op]
}

// SetOfOps transforms a sequence of operations into a set.
func SetOfOps(op string) map[string]bool {
	logger.Trace.Printf("\nOPS: %s\n", op)
	ops := make(map[string]bool)

	seq := strings.Split(op, ",")
	for _, o := range seq {
		ops[o] = true
	}

	FilterOperations(ops)

	return ops
}

// SetOfOpsOverTime transforms a sequence of operations across multiple records into a set.
func SetOfOpsOverTime(rs []*engine.Record) map[string]bool {
	r := rs[0]
	ty := engine.Mapper.MapStr(engine.SF_TYPE)(r)
	op := engine.Mapper.MapStr(engine.SF_OPFLAGS)(r)
	path := engine.Mapper.MapStr(engine.SF_FILE_PATH)(r)

	ops := SetOfOps(op)

	// Look ahead to see if we can merge future events.
	if len(rs) == 1 {
		return ops
	}

	r1 := rs[1]

	ty1 := engine.Mapper.MapStr(engine.SF_TYPE)(r1)
	op1 := engine.Mapper.MapStr(engine.SF_OPFLAGS)(r1)
	path1 := engine.Mapper.MapStr(engine.SF_FILE_PATH)(r1)

	// This record isn't on the same flow.
	if !(ty1 == ty && path == path1) {
		return ops
	}

	ops1 := SetOfOps(op1)

	for key := range ops1 {
		ops[key] = ops1[key]
	}

	return ops
}

// Difference returns the set difference of r - q
func Difference(r map[string]bool, q map[string]bool) []string {
	missing := make([]string, 0)

	for k := range r {
		if r[k] && !q[k] {
			missing = append(missing, k)
		}
	}
	return missing
}

func (b *BehaviorModel) findNode(state string) *Constraint {

	for _, constraint := range b.Constraints {
		if state == constraint.Node {
			logger.Trace.Printf("\nConstraints %s", constraint.Constraints)
			return constraint
		}
	}

	panic("Missing Label!")
}

// NewSecurityAutomaton creates a new security automaton instance.
func NewSecurityAutomaton(tID int64) *SecurityAutomaton {
	var sa = new(SecurityAutomaton)
	sa.TID = tID
	s1 := "docker-runc []"
	s2 := "docker-runc [docker-runc]"
	s3 := "/usr/local/bin/httpd-foreground [docker-runc]"
	s4 := "/usr/local/bin/httpd-foreground [/usr/local/bin/httpd-foreground]"
	s5 := "/bin/rm [/usr/local/bin/httpd-foreground]"
	s6 := "/usr/local/apache2/bin/httpd [/usr/local/bin/httpd-foreground]"
	s7 := "/usr/local/apache2/bin/httpd [/usr/local/apache2/bin/httpd]"
	sa.FSM = fsm.NewFSM(
		s1,
		fsm.Events{
			{Name: "CLONE [docker-runc]", Src: []string{s1, s2}, Dst: s2},
			{Name: "EXEC [docker-runc]", Src: []string{s2}, Dst: s3},
			{Name: "CLONE [/usr/local/bin/httpd-foreground]", Src: []string{s3}, Dst: s4},
			{Name: "EXEC [/usr/local/bin/httpd-foreground]", Src: []string{s4}, Dst: s5},
			{Name: "EXEC [/usr/local/bin/httpd-foreground]", Src: []string{s3}, Dst: s6},
			{Name: "CLONE [/usr/local/apache2/bin/httpd]", Src: []string{s6}, Dst: s7},
		},
		fsm.Callbacks{
			"enter_state": func(e *fsm.Event) {
				sa.enterState(e)
			},
		},
	)

	return sa
}

// AllowedExitCodes returns a vector of Error Codes that are Allowed by Default.
func AllowedExitCodes() []interface{} {
	validExits := make([]interface{}, 3)
	validExits[0] = EXIT_SUCCESS
	validExits[1] = EXIT_ERROR
	validExits[2] = EXIT_SHELL_MISUSE

	return validExits
}

// ParseSecurityAutomaton transforms an FSM loaded from a JSON file into a SecurityAutomaton
func ParseSecurityAutomaton(model BehaviorModel) *SecurityAutomaton {
	var sa = new(SecurityAutomaton)
	m := make(map[string]string)

	sa.Model = &model

	for i, c := range model.Constraints {
		m[c.Node] = c.Node
		logger.Trace.Printf("\nparse: %d %s", i, c.Node)
	}

	var events []fsm.EventDesc
	exitConstraints := make(map[string]interface{})

	exitConstraints[engine.SF_RET] = AllowedExitCodes()

	model.Nodes = append(model.Nodes, SA_EXIT_NODE)
	constraint := Constraint{SA_EXIT_NODE, exitConstraints}
	model.Constraints = append(model.Constraints, &constraint)

	for _, e := range model.Edges {
		var src = m[e.Src]
		var dst = m[e.Dst]

		var e = fsm.EventDesc{Name: e.Label, Src: []string{src}, Dst: dst}

		events = append(events, e)
	}

	for _, n := range model.Nodes {
		var src = m[n]
		var ex = fsm.EventDesc{Name: SA_EXIT_EDGE, Src: []string{src}, Dst: SA_EXIT_NODE}
		var edge = Edge{src, SA_EXIT_NODE, SA_EXIT_EDGE}

		events = append(events, ex)
		model.Edges = append(model.Edges, &edge)
	}

	sa.Initial = model.Initial

	for _, c := range model.Constraints {
		if c.Node == sa.Initial {
			logger.Trace.Printf("\nConstraints: %v", c.Constraints[engine.SF_PROC_ARGS])
			// TODO: Clean this up.
			for k := range ParseSlice(c.Constraints[engine.SF_PROC_ARGS]) {
				files := strings.Split(k, " ")
				sa.FileWhiteList = append(sa.FileWhiteList, files...)
			}
			break
		}
	}

	sa.FSM = fsm.NewFSM(
		model.Initial,
		fsm.Events(events),
		fsm.Callbacks{
			"enter_state": func(e *fsm.Event) {
				sa.enterState(e)
			},
		},
	)

	return sa
}

// EventFromTransition extract the transition given in a SysFlow event
func EventFromTransition(transition string) string {
	var ts = strings.Split(transition, " ")

	return ts[0]
}

// NewSecurityAutomatonFromJSON creates a new security automaton instance from a JSON file
func NewSecurityAutomatonFromJSON(tID int64, path string) *SecurityAutomaton {
	var fsm BehaviorModel

	jsonFile, err := os.Open(path)

	if err != nil {
		fmt.Println(err)
	}

	defer jsonFile.Close()

	logger.Trace.Printf("\nmodel file: %s", jsonFile)

	// read our opened jsonFile as a byte array.
	byteValue, _ := ioutil.ReadAll(jsonFile)

	json.Unmarshal([]byte(byteValue), &fsm)

	logger.Trace.Println("\nParsed FSM from JSON file!")
	logger.Trace.Println("\n", fsm.Nodes[0])
	logger.Trace.Println("\n", fsm.Constraints[0].Constraints)
	logger.Trace.Println("\n", fsm.Edges[0].Src)

	sa := ParseSecurityAutomaton(fsm)
	sa.TID = tID

	return sa
}

// TransitionAvailable checks whether the SecurityAutomaton can transition to a specific operation.
func (s *SecurityAutomaton) TransitionAvailable(op string) bool {
	for _, transition := range s.FSM.AvailableTransitions() {
		if op == transition {
			return true
		}
	}
	return false
}

// FindEdge finds the edge in the SecurityAutomaton that corresponds to an event.
func (s *SecurityAutomaton) FindEdge(e string) (*Edge, error) {
	current := s.FSM.Current()
	currentNode := s.Model.findNode(current).Node

	for _, edge := range s.Model.Edges {
		if edge.Src == currentNode && edge.Label == e {
			return edge, nil
		}
	}
	return nil, errors.New("no edge found")
}

// Can checks whether the Security Automaton can transition with an event extracted from the record.
func (s *SecurityAutomaton) Can(r *engine.Record) bool {
	op := engine.Mapper.MapStr(engine.SF_OPFLAGS)(r)

	return s.CanEvent(op, r)
}

// CanEvent checks whether the Security Automaton can transition to this event.
func (s *SecurityAutomaton) CanEvent(e string, r *engine.Record) bool {
	exe := engine.Mapper.MapStr(engine.SF_PROC_EXE)(r)
	pexe := engine.Mapper.MapStr(engine.SF_PPROC_EXE)(r)
	logger.Trace.Printf("\nChecking if FSM can transition with %s\n", e)
	logger.Trace.Printf("\n\tCurrent state: %s\n\tAvailable transitions: %v\n\tEvent: %v\n\texe: %s, pexe:%s",
		s.FSM.Current(), s.FSM.AvailableTransitions(), e, exe, pexe)

	edge, _ := s.FindEdge(e)

	if edge == nil {
		logger.Trace.Println("\nNo edge found!")
		return false
	}

	dst := edge.Dst

	logger.Trace.Printf("\nLooking for node %s\n", dst)

	node := s.Model.findNode(dst)

	if node.Constraints == nil {
		logger.Trace.Println("\nNo constraints!")
		return true
	}

	logger.Trace.Printf("\n\tconstraints:")
	for field, optionsJSON := range node.Constraints {
		v := engine.Mapper.MapStr(field)(r)
		options := ParseSlice(optionsJSON)
		found := false

		for opt := range options {
			logger.Trace.Println("\n\t\topt:", opt)
			// Check for special identifiers
			if strings.HasPrefix(opt, "%") {
				components := strings.Split(opt[1:], ".")
				id := components[0]
				pfield := strings.Join(components[1:], ".")
				if id == "pred" && len(s.History) > 0 {
					// Obtain the predecessor's record
					pred := s.History[len(s.History)-1].Record
					pv := engine.Mapper.MapStr(pfield)(pred)
					if pv == v {
						found = true
						break
					}
				}
			} else {
				logger.Trace.Println("\n\tv:", v)
				if opt == v {
					found = true
					break
				}
			}
		}

		// All constraints need to be satisfied
		if found == false {
			logger.Trace.Println("\n\tCould not satisfy constraint.")
			return false
		}
	}

	return true
}

func (s *SecurityAutomaton) enterState(e *fsm.Event) {
	logger.Trace.Printf("\nThread %d is entering state %s\n", s.TID, e.Dst)
}

// IsActive checks whether the SA is active.
func (s *SecurityAutomaton) IsActive() bool {
	return s.Initial != s.FSM.Current() && s.FSM.Current() != SA_EXIT_NODE
}

// ReportIncident submits an incident to the output Channel.
func (s *SecurityAutomaton) ReportIncident(event string, r *engine.Record, out func(r *engine.Record)) {
	// Submit an error to the out-channel.
	logger.Trace.Println("\n\tSecurity Violation submitting to out channel!")
	msg := fmt.Sprintf("Static model forbids %s from this state.", event)
	r.Ctx.SetTags([]string{msg})
	//ctx := MRMContext(r.Ctx)
	//ctx.AddIncident(Incident{s.FSM.Current(), msg})
	out(r)
}

// AddObservation records an observation to the SecurityAutomaton's History
func (s *SecurityAutomaton) AddObservation(r *engine.Record) {
	s.History = append(s.History, Observation{false, r})
}

// TypeCheckTrace checks whether the run of flows given in the history inhabits the model.
// A run of flows inhabits the model if every item in the flow can be used to traverse the FSM corresponding
// to the program's execution.
func (s *SecurityAutomaton) TypeCheckTrace(out func(r *engine.Record)) {
	logger.Trace.Printf("\nTypeChecking Trace!")

	// Ignore activity on files used by the linker or the entrypoint.
	whiteListedFiles := []string{
		".*lib.so$",
		".*libc.so.6$",
		"/etc/ld.so.cache",
		"/proc/self/fd/[0-9]+"}

	abilities := make(map[string][]Observation)

	// Build a map that summarizes these flow's capabilities
	for _, obs := range s.History {
		r := obs.Record
		ty := engine.Mapper.MapStr(engine.SF_TYPE)(r)
		op := engine.Mapper.MapStr(engine.SF_OPFLAGS)(r)
		path := engine.Mapper.MapStr(engine.SF_FILE_PATH)(r)

		logger.Trace.Printf("\n\nType: %s\n    Ops: %s\n   Path: %s", ty, op, path)

		switch ty {
		case engine.TyPE:
			abilities[op] = append(abilities[op], Observation{false, r})
			break
		case engine.TyFF:
			if RegexpArrayMem(whiteListedFiles, path) || path == "" {
				logger.Trace.Printf("\nSkipping whitelisted file %s", path)
				continue
			}
			fallthrough
		case engine.TyNF:
			ops := SetOfOps(op)
			for o := range ops {
				if ops[o] {
					abilities[o] = append(abilities[o], Observation{false, r})
				}
			}
		}
	}

	// Debugging
	logger.Trace.Printf("\nObserved the following operations:\n")
	for op, obs := range abilities {
		logger.Trace.Printf("\n%s:\n", op)

		for _, ob := range obs {
			r := ob.Record
			ty := engine.Mapper.MapStr(engine.SF_TYPE)(r)
			op := engine.Mapper.MapStr(engine.SF_OPFLAGS)(r)

			switch ty {
			case engine.TyPE:
				exe := engine.Mapper.MapStr(engine.SF_PROC_EXE)(r)
				logger.Trace.Printf("\n%4s", exe)
				break
			case engine.TyNF:
				port := engine.Mapper.MapStr(engine.SF_NET_DPORT)(r)
				logger.Trace.Printf("\n%4s", port)
				ops := SetOfOps(op)
				for o := range ops {
					if ops[o] {
						logger.Trace.Printf("\n%s ", o)
					}
					logger.Trace.Printf("\n")
				}
				break
			case engine.TyFF:
				ops := SetOfOps(op)
				path := engine.Mapper.MapStr(engine.SF_FILE_PATH)(r)
				logger.Trace.Printf("\n%s: ", path)
				for o := range ops {
					if ops[o] {
						logger.Trace.Printf("\n%s ", o)
					}
					logger.Trace.Printf("\n")
				}
			}
		}
	}

	// Enumerate the FSM with these abilities
	visitedStates := make(map[string]bool)

	for {
		// Have we visited all states reachable from non-process events?
		transitions := s.FSM.AvailableTransitions()
		logger.Trace.Printf("\nPossible Transitions: %v", transitions)

		// MMAP events don't appear in the SysFlow traces, so assume we always have that ability.
		if Contains(transitions, SA_MMAP_EVENT) {
			s.FSM.Event(SA_MMAP_EVENT)
			visitedStates[SA_MMAP_EVENT] = true
		}

		fallback := s.FSM.Current()

		// Attempt to use our abilities to traverse the FSM.
		for op, obs := range abilities {
			for _, ob := range obs {
				logger.Trace.Printf("\nAttempting to use op %s\n", op)
				modelOp := TranslateOperation(op)
				if s.CanEvent(modelOp, ob.Record) {
					s.HandleEvent(modelOp, ob.Record, out)
					if visitedStates[s.FSM.Current()] {
						s.FSM.SetState(fallback)
					} else {
						ob.Used = true
						visitedStates[s.FSM.Current()] = true
					}
				}
			}
		}

		if fallback == s.FSM.Current() {
			break
		}
	}
}

// ClearHistory erases the type context
func (s *SecurityAutomaton) ClearHistory() {
	s.History = nil
}

// HandleEvent submits an event to the SecurityAutomaton, if the SA can handle the event.
func (s *SecurityAutomaton) HandleEvent(event string, r *engine.Record, out func(r *engine.Record)) {
	ty := engine.Mapper.MapStr(engine.SF_TYPE)(r)
	exe := engine.Mapper.MapStr(engine.SF_PROC_EXE)(r)
	pexe := engine.Mapper.MapStr(engine.SF_PPROC_EXE)(r)

	logger.Trace.Printf("\n\tCurrent state: %s\n\tAvailable transitions: %v\n\tEvent: %v\n\texe: %s, pexe:%s",
		s.FSM.Current(), s.FSM.AvailableTransitions(), event, exe, pexe)

	if s.CanEvent(event, r) {
		logger.Trace.Printf("\nAdvancing FSM!")
		s.FSM.Event(event)
	} else if s.IsActive() {
		if ty == engine.TyPE {
			s.ReportIncident(event, r, out)
		}
	} else {
		s.AddObservation(r)
	}
}

// Event processes an Individual Record
func (s *SecurityAutomaton) Event(r *engine.Record, out func(r *engine.Record)) {
	logger.Trace.Printf("\nProcessing Record\n")

	ty := engine.Mapper.MapStr(engine.SF_TYPE)(r)
	op := engine.Mapper.MapStr(engine.SF_OPFLAGS)(r)
	port := engine.Mapper.MapStr(engine.SF_NET_DPORT)(r)
	tid := engine.Mapper.MapStr(engine.SF_PROC_TID)(r)

	logger.Trace.Printf("\nOP: %s %s %s %s", tid, port, ty, op)

	if !s.IsActive() {
		if ty != engine.TyPE {
			return
		}
		logger.Trace.Printf("\nChecking whether PE %s can initialize FSM", op)
		s.HandleEvent(op, r, out)
		// Start collecting events using a clean history.
		if s.IsActive() {
			s.ClearHistory()
		}
	} else {
		if ty == engine.TyPE {
			// Check the validity of all the events we've seen so far.
			s.TypeCheckTrace(out)
			s.ClearHistory()
			s.HandleEvent(op, r, out)
		} else {
			// Add this record to the history of elements.
			s.AddObservation(r)
		}
	}
}

/**
This was  ad-hoc
// Event advances the FSM with an event given in the policy engine record
func (s *SecurityAutomaton) Event1(rs []*engine.Record, out func(r *engine.Record)) {
	r := rs[0]
	logger.Trace.Printf("\nProcessing Record\n")

	ty := engine.Mapper.MapStr(engine.SF_TYPE)(r)
	op := engine.Mapper.MapStr(engine.SF_OPFLAGS)(r)
	port := engine.Mapper.MapStr(engine.SF_NET_DPORT)(r)
	tid := engine.Mapper.MapStr(engine.SF_PROC_TID)(r)
	path := engine.Mapper.MapStr(engine.SF_FILE_PATH)(r)

	logger.Trace.Printf("OP: %s %s %s %s", tid, port, ty, op)

	s.History = append(s.History, Observation{false, r})

	switch ty {
	case engine.TyNF:
		observations := SetOfOps(op)
		visitedStates := make(map[string]bool)
		visitedTransitions := make(map[string]bool)

		if observations[engine.SF_OP_ACCEPT] {
			s.HandleEvent(engine.SF_OP_ACCEPT, rs, out)
			visitedTransitions[engine.SF_OP_ACCEPT] = true
		} else {
			logger.Trace.Println("Network Flow missing an ACCEPT event")
		}

		for {
			if visitedStates[s.FSM.Current()] {
				logger.Trace.Println("Repeated State!")
				break
			}
			var transitions = s.FSM.AvailableTransitions()
			var found = false

			for _, t := range transitions {
				// Do not exit while evaluating Network Flows
				if t == "EXIT" {
					continue
				}

				logger.Trace.Printf("Checking transition %s", t)

				var event = EventFromTransition(t)

				logger.Trace.Printf("event %s found", event)

				if observations[event] {
					visitedStates[s.FSM.Current()] = true
					visitedTransitions[event] = true
					s.HandleEvent(event, rs, out)
					found = true
					break
				}
			}

			if !found {
				break
			}
		}

		// Did we utilize all the observed transitions?
		missingEvents := Difference(observations, visitedTransitions)
		if len(missingEvents) > 0 {
			missingMsg := strings.Join(missingEvents, ",")
			logger.Trace.Printf("Security Violation! Observed events not expressed in behavior model [%s]", missingMsg)
		}
	case engine.TyFF:
		logger.Trace.Println("Starting File Flow!")

		observations := SetOfOpsOverTime(rs)
		visitedStates := make(map[string]bool)
		visitedTransitions := make(map[string]bool)

		logger.Trace.Println("Observations:")
		for k := range observations {
			logger.Trace.Printf("  %s", k)
		}

		// Ignore activity on files used by the linker or the entrypoint.
		whiteListedFiles := []string{
			".*libc.so.6$",
			"/etc/ld.so.cache",
			"/proc/self/fd/[0-9]+"}

		whiteListedFiles = append(whiteListedFiles, s.FileWhiteList...)

		logger.Trace.Printf("Found File Flow at path: %s", path)

		if s.Initial == s.FSM.Current() {
			logger.Trace.Printf("Ignoring file flow at %s before automaton has started.", path)
			return
		}

		logger.Trace.Printf("WhiteList: %v", whiteListedFiles)

		if RegexpArrayMem(whiteListedFiles, path) || path == "" {
			logger.Trace.Printf("Skipping whitelisted file %s", path)
			return
		}

		if observations[engine.SF_OP_OPEN] && s.TransitionAvailable(engine.SF_OP_OPEN) {
			s.HandleEvent(engine.SF_OP_OPEN, rs, out)
			visitedTransitions[engine.SF_OP_OPEN] = true
		} else {
			logger.Trace.Println("File Flow missing a OPEN event")
		}

		for {
			var transitions = s.FSM.AvailableTransitions()
			var found = false

			for _, t := range transitions {
				// Do not exit while evaluating File Flows
				if t == engine.SF_OP_EXIT {
					continue
				}
				logger.Trace.Printf("Checking transition %s", t)

				var event = EventFromTransition(t)

				logger.Trace.Printf("event %s found", event)

				if observations[event] {
					// Would taking this transition lead to a new state?
					currentState := s.FSM.Current()
					s.HandleEvent(event, rs, out)
					nextState := s.FSM.Current()

					if visitedStates[nextState] {
						// Count the observation and rollback
						logger.Trace.Printf("  %s will repeat state! Rolling back", event)
						visitedTransitions[event] = true
						s.FSM.SetState(currentState)
						continue
					}

					visitedStates[currentState] = true
					visitedTransitions[event] = true

					found = true
					break
				}
			}

			if !found {
				break
			}
		}

		/**
		// Did we utilize all the observed transitions?
		missingEvents := Difference(observations, visitedTransitions)
		if len(missingEvents) > 0 {
			missingMsg := strings.Join(missingEvents, ",")
			logger.Trace.Printf("\nPossible Violation! Observed events not expressed in behavior model [%s]", missingMsg)
			s.SuspiciousRecord = r
		} else if s.SuspiciousRecord != nil {
			// More context alleviated the problem.
			if s.SuspiciousRecord == rs[0] || s.SuspiciousRecord == rs[1] {
				s.SuspiciousRecord = nil
			} else {
				logger.Trace.Printf("\nSecurity Violation! Observed events not expressed in behavior model")
			}
		}
		logger.Trace.Println("\nFinished File Flow!")
	case engine.TyPE:
		logger.Trace.Printf("\nHandling PE %s", op)
		s.HandleEvent(op, rs, out)
	}
} */
