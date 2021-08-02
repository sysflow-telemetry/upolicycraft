//
// Copyright (C) 2021 IBM Corporation.
//
// Authors:
// William Blair <wdblair@ibm.com>
// Frederico Araujo <frederico.araujo@ibm.com>
// Teryl Taylor <terylt@ibm.com>
//
module github.com/sysflow-telemetry/sf-processor/plugins/mids

go 1.14

require (
	github.com/actgardner/gogen-avro v6.5.0+incompatible // indirect
	github.com/jinzhu/copier v0.3.2
	github.com/looplab/fsm v0.1.0
	github.com/sysflow-telemetry/sf-apis/go v0.0.0-20210720205833-7d3c76ce0587
	github.com/sysflow-telemetry/sf-processor/core v0.0.0-20210723173640-2a427a55a202
)

// replace github.com/sysflow-telemetry/sf-processor/core => ../../../sf-processor/core
