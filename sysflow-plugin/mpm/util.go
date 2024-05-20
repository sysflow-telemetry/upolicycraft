// Authors:
// William Blair <wdblair@bu.edu>
// Frederico Araujo <frederico.araujo@ibm.com>
// Teryl Taylor <terylt@ibm.com>
//
package main

// Contains determines whether a value appears in a slice.
func Contains(slice []string, val string) bool {
	for _, item := range slice {
		if item == val {
			return true
		}
	}
	return false
}
