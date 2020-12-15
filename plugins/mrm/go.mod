//
// Copyright (C) 2020 IBM Corporation.
//
// Authors:
// William Blair <wdblair@ibm.com>
// Frederico Araujo <frederico.araujo@ibm.com>
// Teryl Taylor <terylt@ibm.com>
//
module github.com/sysflow-telemetry/sf-processor/plugins/mrm

go 1.14

require (
	github.com/looplab/fsm v0.1.0
	github.com/sysflow-telemetry/sf-apis/go v0.0.0-20201202234011-cb9c5a4dc2b1
	github.com/sysflow-telemetry/sf-processor/core v0.0.0-20201207150522-7bba90fbb326
//  github.ibm.com/sysflow/sf-processor/core v0.0.0-20201027030609-879f8d66a4f0
)

// replace github.ibm.com/sysflow/sf-processor/core => ../../core

// replace github.ibm.com/sysflow/goutils => ../../modules/goutils
// replace github.com/sysflow-telemetry/sf-apis/go => ../../modules/sf-apis/go
