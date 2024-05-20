// Authors:
// William Blair <wdblair@bu.edu>
// Frederico Araujo <frederico.araujo@ibm.com>
// Teryl Taylor <terylt@ibm.com>
//
module github.com/sysflow-telemetry/sf-processor/plugins/mpm

go 1.14

require (
	github.com/actgardner/gogen-avro v6.5.0+incompatible // indirect
	github.com/jinzhu/copier v0.3.2
	github.com/looplab/fsm v0.1.0
	github.com/sysflow-telemetry/sf-apis/go v0.0.0-20210713130650-c57558d63305
	github.com/sysflow-telemetry/sf-processor/core v0.0.0-20210723173640-2a427a55a202
)

// replace github.com/sysflow-telemetry/sf-processor/core => ../../../sf-processor/core
