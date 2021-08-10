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
	"github.com/sysflow-telemetry/sf-apis/go/sfgo"
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
	Node        string                   `json:"node"`
	Constraints []map[string]interface{} `json:"constraints"`
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

// SecurityAutomaton for a monitored application thread.
type SecurityAutomaton struct {
	parent        *IntrusionDetectionSystem
	TID           int64
	FileWhiteList []string
	Initial       string
	FSM           *fsm.FSM
	Model         *BehaviorModel
	History       []Observation
	Capabilities  map[string][]Observation
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
	delete(ops, "TRUNCATE")
	delete(ops, "SHUTDOWN")
}

//TranslateOperation adapts a SysFlow OperationName to a MRM Operation
func TranslateOperation(op string) string {
	var translation = map[string]string{
		"SEND": "WRITE",
		"RECV": "READ",
	}

	if top, ok := translation[op]; ok {
		return top
	}

	return op
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

	sa.Capabilities = make(map[string][]Observation)
	sa.Model = &model

	for i, c := range model.Constraints {
		m[c.Node] = c.Node
		logger.Trace.Printf("\nparse: %d %s", i, c.Node)
		logger.Trace.Println("constraints: ", c.Constraints[0])
	}

	var events []fsm.EventDesc

	exitConstraints := make(map[string]interface{})

	exitConstraints[engine.SF_RET] = AllowedExitCodes()

	var constraints []map[string]interface{}

	model.Nodes = append(model.Nodes, SA_EXIT_NODE)
	constraints = append(constraints, exitConstraints)
	constraint := Constraint{SA_EXIT_NODE, constraints}

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
			logger.Trace.Printf("\nConstraints: %v", c.Constraints)
			// TODO: Clean this up.
			for k := range ParseSlice(c.Constraints[0][engine.SF_PROC_ARGS]) {
				files := strings.Split(k, " ")
				sa.FileWhiteList = append(sa.FileWhiteList, files...)
			}
			// Ignore activity on files used by the linker or the entrypoint.
			whiteListedFiles := []string{
				".*lib.so$",
				".*libc.so.6$",
				"/etc/nsswitch.conf",
				"/etc/ld.so.cache",
				"/lib/x86_64-linux-gnu/*",
				"/proc/self/fd/[0-9]+"}
			sa.FileWhiteList = append(sa.FileWhiteList, whiteListedFiles...)
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
		logger.Error.Printf("Could not open %s\n", path)
		os.Exit(1)
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
	logger.Trace.Printf("\nChecking if FSM can transition with %s\n\n\tCurrent state: %s\n"+
		"\tAvailable transitions: %v\n\tEvent: %v\n\texe: %s, pexe:%s", e, s.FSM.Current(), s.FSM.AvailableTransitions(), e, exe, pexe)

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

	logger.Trace.Printf("\n\tConstraints:")
	for i := range node.Constraints {
		neededMatches := len(node.Constraints[i])
		matches := 0
		for field, optionsJSON := range node.Constraints[i] {
			v := engine.Mapper.MapStr(field)(r)
			options := ParseSlice(optionsJSON)

			for opt := range options {
				logger.Trace.Println("\n\t\topt:", opt, "\n\t\tv:", v)
				// Check for special identifiers
				if strings.HasPrefix(opt, "%") {
					logger.Trace.Println("Parsing special constraint!")
					components := strings.Split(opt[1:], ".")
					id := components[0]
					pfield := strings.Join(components[1:], ".")
					history := s.parent.history
					if id == "pred" && len(history) > 0 {
						// Obtain the predecessor's record
						pred := history[len(history)-1]
						pv := engine.Mapper.MapStr(pfield)(pred)
						logger.Trace.Println("\n\t\tpv:", pv)
						if pv == v {
							matches += 1
							break
						}
					}
				} else {
					if opt == v {
						matches += 1
						break
					}
				}
			}
		}
		if matches == neededMatches {
			return true
		}
	}

	logger.Trace.Println("\n\tCould not satisfy constraint.")
	return false
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

// ContainsRecord checks whether the flow has already been seen
func (s *SecurityAutomaton) ContainsRecord(op string, r *engine.Record) bool {
	ty := engine.Mapper.MapStr(engine.SF_TYPE)(r)
	if records, ok := s.Capabilities[op]; ok {
		for _, obs := range records {
			existing := obs.Record
			existingty := engine.Mapper.MapStr(engine.SF_TYPE)(existing)

			if existingty != ty {
				continue
			}

			switch ty {
			case sfgo.TyPEStr:
				prevexe := engine.Mapper.MapStr(engine.SF_PROC_EXE)(existing)
				exe := engine.Mapper.MapStr(engine.SF_PROC_EXE)(r)
				if prevexe == exe {
					return true
				}
				break
			case sfgo.TyNFStr:
				prevport := engine.Mapper.MapStr(engine.SF_NET_DPORT)(existing)
				port := engine.Mapper.MapStr(engine.SF_NET_DPORT)(r)
				if prevport == port {
					return true
				}
				break
			case sfgo.TyFFStr:
				prevpath := engine.Mapper.MapStr(engine.SF_FILE_PATH)(existing)
				path := engine.Mapper.MapStr(engine.SF_FILE_PATH)(r)
				if prevpath == path {
					return true
				}
			}
		}
	}
	return false
}

// AddObservation records an observation to the SecurityAutomaton's History
func (s *SecurityAutomaton) AddObservation(r *engine.Record) {
	logger.Trace.Println("Adding observation!")
	//s.History = append(s.History, Observation{false, r})

	//sensitiveFiles := []string{
	//	"/etc/passwd",
	//	"/etc/group"}

	ty := engine.Mapper.MapStr(engine.SF_TYPE)(r)
	op := engine.Mapper.MapStr(engine.SF_OPFLAGS)(r)
	path := engine.Mapper.MapStr(engine.SF_FILE_PATH)(r)

	// After an exec, the process close sensitive files, which we can ignore.
	//if Contains(sensitiveFiles, path) {
	//	return
	//}

	if ty == sfgo.TyPEStr {
		logger.Trace.Printf("Erroneously adding Process Event to History!")
	}

	if ty == sfgo.TyFFStr && (RegexpArrayMem(s.FileWhiteList, path) || path == "") {
		logger.Trace.Printf("\nSkipping whitelisted file: %s", path)
		return
	}

	ops := SetOfOps(op)
	for op := range ops {
		modelOp := TranslateOperation(op)
		// TODO store a table to enforce heuristics like these

		// Add op record to the Capabilities map.
		if !s.ContainsRecord(modelOp, r) {
			logger.Trace.Printf("Adding to %s!", modelOp)
			s.Capabilities[modelOp] = append(s.Capabilities[modelOp], Observation{false, r})
		} else {
			logger.Trace.Printf("Not adding to %s!", modelOp)
		}
	}
}

// TypeCheckTrace checks whether the run of flows given in the history inhabits the model.
// A run of flows inhabits the model if every item in the flow can be used to traverse the FSM corresponding
// to the program's execution.
func (s *SecurityAutomaton) TypeCheckTrace(out func(r *engine.Record)) {
	logger.Trace.Printf("\nTypeChecking Trace! TID = %d\n", s.TID)
	// Debugging
	logger.Trace.Printf("\nObserved the following capabilities:\nTID: %d", s.TID)

	for op, obs := range s.Capabilities {
		logger.Trace.Printf("\n\t%s: Count %d\n", op, len(obs))

		for _, ob := range obs {
			r := ob.Record
			ty := engine.Mapper.MapStr(engine.SF_TYPE)(r)

			switch ty {
			case sfgo.TyPEStr:
				exe := engine.Mapper.MapStr(engine.SF_PROC_EXE)(r)
				logger.Trace.Printf("\nPE:\n\t%4s", exe)
				break
			case sfgo.TyNFStr:
				port := engine.Mapper.MapStr(engine.SF_NET_DPORT)(r)
				logger.Trace.Printf("\nNF:\n\t%4s", port)
				break
			case sfgo.TyFFStr:
				path := engine.Mapper.MapStr(engine.SF_FILE_PATH)(r)
				logger.Trace.Printf("\nFF:\n\t%s: ", path)
			}
		}
	}

	logger.Trace.Printf("\nFSM File:\n%s\n", fsm.Visualize(s.FSM))

	// Enumerate the FSM with these abilities
	visitedStates := make(map[string]bool)

	stateQueue := []string{s.FSM.Current()}

	for {
		if len(stateQueue) == 0 {
			break
		}

		nextState := stateQueue[0]
		stateQueue = stateQueue[1:]

		if visitedStates[nextState] {
			continue
		}

		s.FSM.SetState(nextState)

		visitedStates[nextState] = true

		// Have we visited all states reachable from non-process events?
		transitions := s.FSM.AvailableTransitions()
		logger.Trace.Printf("\nVisiting State: %s", nextState)
		logger.Trace.Printf("\nPossible Transitions: %v", transitions)

		for _, op := range transitions {
			for i := range s.Capabilities[op] {
				if s.CanEvent(op, s.Capabilities[op][i].Record) {
					logger.Trace.Printf("\nMarking %s as used!", op)
					s.Capabilities[op][i].Used = true
					edge, _ := s.FindEdge(op)
					dst := edge.Dst

					if !Contains(stateQueue, dst) {
						stateQueue = append(stateQueue, dst)
					}
				}
			}
		}
	}

	for op, obs := range s.Capabilities {
		for _, ob := range obs {
			if !ob.Used {
				logger.Trace.Printf("\nSecurity Violation! Did not see:\n\t%s", op)
				r := ob.Record
				ty := engine.Mapper.MapStr(engine.SF_TYPE)(r)

				switch ty {
				case sfgo.TyPEStr:
					exe := engine.Mapper.MapStr(engine.SF_PROC_EXE)(r)
					logger.Trace.Printf("\nPE:\n\t%4s", exe)
					break
				case sfgo.TyNFStr:
					port := engine.Mapper.MapStr(engine.SF_NET_DPORT)(r)
					logger.Trace.Printf("\nNF:\n\t%4s", port)
					break
				case sfgo.TyFFStr:
					path := engine.Mapper.MapStr(engine.SF_FILE_PATH)(r)
					logger.Trace.Printf("\nFF:\n\t%s: ", path)
				}
				s.ReportIncident(op, r, out)
			} else {
				logger.Trace.Printf("\nSaw:%s", op)
			}
		}
	}
}

// ClearHistory erases the type context
func (s *SecurityAutomaton) ClearHistory() {
	logger.Trace.Println("Clearing History!")
	s.History = nil
	s.Capabilities = make(map[string][]Observation)
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
		if ty == sfgo.TyPEStr {
			s.ReportIncident(event, r, out)
		} else {
			s.AddObservation(r)
		}
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
		if ty != sfgo.TyPEStr {
			return
		}
		logger.Trace.Printf("\nChecking whether PE %s can initialize FSM", op)
		s.HandleEvent(op, r, out)
		// Start collecting events using a clean history.
		if s.IsActive() {
			s.ClearHistory()
		}
	} else {
		if ty == sfgo.TyPEStr {
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
