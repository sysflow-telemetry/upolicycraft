// Authors:
// William Blair <wdblair@bu.edu>
// Frederico Araujo <frederico.araujo@ibm.com>
// Teryl Taylor <terylt@ibm.com>
//
package main

import "fmt"

const (
	ModelConfigKey string = "model"
)

type Config struct {
	Model string
}

// CreateConfig creates a new config object from config dictionary.
func CreateConfig(conf map[string]interface{}) Config {
	var config = Config{}

	if v, ok := conf[ModelConfigKey]; ok {
		config.Model = fmt.Sprintf("%v", v)
	}

	return config
}
