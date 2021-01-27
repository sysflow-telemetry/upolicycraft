/*

Author: Steve Wood <swood@cromulence.com>

Copyright (c) 2016 Cromulence LLC

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.

*/

#include <libcgc.h>
#include "stdlib.h"
#include "service.h"
#include "commands.h"
#include "printf.h"


airportInfoType *airports;

int main(void) {

char command[150];
int retval;
unsigned int databaseCheck;


	// compare the consistency of the database (magic page)
	databaseCheck = check_db();

	printf("Database checksum: @d\n", databaseCheck);

	// load data from the magic page
	if (loadDB(&airports) == -1)
		terminate(-1);


	while (1) {
		uids_log("Accepting input");
		getline(command, sizeof(command));
		uids_log(command);
		retval = execute_cmd(&airports, command);


		if (retval == COMMAND_OK) {
			printf("OK\n");
		} else if  (retval == BAD_COMMAND) {
			printf("BAD COMMAND FORMAT\n");
		} else if (retval == DUPLICATE_CODE) {
			printf("AIRPORT CODE EXISTS\n");
		} else if (retval == UNKN_CODE) {
			printf("UNKNOWN AIRPORT\n");
		} else if (retval == DATABASE_EMPTY) {
			printf("EMPTY DB\n");
		} else if (retval == COMMAND_TERMINATED) {
			printf("COMMAND TERMINATED\n");
		} else if (retval == NO_RESULTS) {
			printf("NO RESULTS\n");
		} else if (retval == UNRECOVERABLE_ERROR) {
			printf("TERMINATING\n");
			terminate(-1);
		} else if (retval == -99) {
			printf("OK\n");
		}

		if (retval == -99)
			break;

	} // while

    terminate(0);

}  // main  

