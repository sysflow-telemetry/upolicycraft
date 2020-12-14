//
// Copyright (C) 2020 IBM Corporation.
//
// Authors:
// William Blair <wdblair@ibm.com>
// Frederico Araujo <frederico.araujo@ibm.com>
// Teryl Taylor <terylt@ibm.com>
//
module github.ibm.com/sf-model/plugins/mrmexporter

go 1.14

require (
	github.com/RackSec/srslog v0.0.0-20180709174129-a4725f04ec91
	github.com/sysflow-telemetry/sf-apis/go v0.0.0-20201202234011-cb9c5a4dc2b1
	github.com/sysflow-telemetry/sf-processor/core v0.0.0-20201207150522-7bba90fbb326
)

// replace github.ibm.com/sysflow/sf-processor/core => ../../core
// replace github.ibm.com/sysflow/goutils => ../../modules/goutils
// replace github.com/sysflow-telemetry/sf-apis/go => ../../modules/sf-apis/go
