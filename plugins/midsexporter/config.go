//
// Copyright (C) 2021 IBM Corporation.
//
// Authors:
// Frederico Araujo <frederico.araujo@ibm.com>
// Teryl Taylor <terylt@ibm.com>
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//     http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//
package main

import (
	"fmt"
	"strconv"
)

// Configuration keys.
const (
	ExportConfigKey      string = "export"
	ExpTypeConfigKey     string = "type"
	FormatConfigKey      string = "format"
	FlatConfigKey        string = "flat"
	ProtoConfigKey       string = "proto"
	TagConfigKey         string = "tag"
	LogSourceConfigKey   string = "source"
	HostConfigKey        string = "host"
	PortConfigKey        string = "port"
	PathConfigKey        string = "path"
	EventBufferConfigKey string = "buffer"
	VersionKey           string = "version"
	JSONSchemaVersionKey string = "jsonschemaversion"
	BuildNumberKey       string = "buildnumber"
)

// Config defines a configuration object for the exporter.
type Config struct {
	Export            Export
	ExpType           ExportType
	Format            Format
	Flat              bool
	Proto             Proto
	Tag               string
	LogSource         string
	Host              string
	Port              int
	Path              string
	EventBuffer       int
	Version           string
	JSONSchemaVersion string
	BuildNumber       string
}

// StringOfInterface transforms an interface{} into a String
func StringOfInterface(v interface{}) string {
	return fmt.Sprintf("%v", v)
}

// CreateConfig creates a new config object from config dictionary.
func CreateConfig(conf map[string]interface{}) Config {
	var c Config = Config{Host: "localhost", Port: 514, Path: "./export.out", Tag: "sysflow"} // default values
	if v, ok := conf[ExportConfigKey]; ok {
		s := StringOfInterface(v)
		c.Export = parseExportConfig(s)
	}
	if v, ok := conf[ExpTypeConfigKey]; ok {
		s := StringOfInterface(v)
		c.ExpType = parseExportTypeConfig(s)
	}
	if v, ok := conf[FormatConfigKey]; ok {
		s := StringOfInterface(v)
		c.Format = parseFormatConfig(s)
	}
	if v, ok := conf[FlatConfigKey]; ok && v == "true" {
		c.Flat = true
	}
	if v, ok := conf[ProtoConfigKey]; ok {
		s := StringOfInterface(v)
		c.Proto = parseProtoConfig(s)
	}
	if v, ok := conf[TagConfigKey]; ok {
		s := StringOfInterface(v)
		c.Tag = s
	}
	if v, ok := conf[LogSourceConfigKey]; ok {
		s := StringOfInterface(v)
		c.LogSource = s
	}
	if v, ok := conf[HostConfigKey]; ok {
		s := StringOfInterface(v)
		c.Host = s
	}
	if v, ok := conf[PortConfigKey]; ok {
		s := StringOfInterface(v)
		c.Port, _ = strconv.Atoi(s)
	}
	if v, ok := conf[PathConfigKey]; ok {
		s := StringOfInterface(v)
		c.Path = s
	}
	if v, ok := conf[EventBufferConfigKey]; ok {
		s := StringOfInterface(v)
		c.EventBuffer, _ = strconv.Atoi(s)
	}
	if v, ok := conf[VersionKey]; ok {
		s := StringOfInterface(v)
		c.Version = s
	}
	if v, ok := conf[JSONSchemaVersionKey]; ok {
		s := StringOfInterface(v)
		c.JSONSchemaVersion = s
	}
	if v, ok := conf[BuildNumberKey]; ok {
		s := StringOfInterface(v)
		c.BuildNumber = s
	}
	return c
}

// Export type.
type Export int

// Export config options.
const (
	StdOutExport Export = iota
	FileExport
	SyslogExport
)

func (s Export) String() string {
	return [...]string{"terminal", "file", "syslog"}[s]
}

func parseExportConfig(s string) Export {
	if FileExport.String() == s {
		return FileExport
	}
	if SyslogExport.String() == s {
		return SyslogExport
	}
	return StdOutExport
}

// ExportType type.
type ExportType int

// ExportType config options.
const (
	TelemetryType ExportType = iota
	BatchType
)

func (s ExportType) String() string {
	return [...]string{"telemetry", "batch"}[s]
}

func parseExportTypeConfig(s string) ExportType {
	if BatchType.String() == s {
		return BatchType
	}
	return TelemetryType
}

// Format type.
type Format int

// Format config options.
const (
	JSONFormat Format = iota
)

func (s Format) String() string {
	return [...]string{"json"}[s]
}

func parseFormatConfig(s string) Format {
	return JSONFormat
}

// Proto denotes protocol type.
type Proto int

// Proto config options.
const (
	TCPProto Proto = iota
	TCPTLSProto
	UDPProto
)

func (s Proto) String() string {
	return [...]string{"tcp", "tls", "udp"}[s]
}

func parseProtoConfig(s string) Proto {
	switch s {
	case TCPProto.String():
		return TCPProto
	case TCPTLSProto.String():
		return TCPTLSProto
	case UDPProto.String():
		return UDPProto
	}
	return TCPProto
}
