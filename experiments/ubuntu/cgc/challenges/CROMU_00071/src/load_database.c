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
#include "malloc.h"
#include "stdlib.h"
#include "service.h"


void makeAirportCode( unsigned char *readPtr, char apCode[4]);
char *findAirportCodebyNumber(airportInfoType *airports, int connectionNum);

int loadDB(airportInfoType **airports) {


unsigned char *readPtr;
unsigned char airportCount;
unsigned int offset;
unsigned int i;
char airportCode[4];
char *code;

airportInfoType *tmpPtr;
struct connectionList *tmpConnectionPtr;
int connectionCount;
int connectionNum;


	// this should only be called on an empty database
	if (*airports != 0)
		return -1;

	readPtr = (unsigned char *)"7w3mMVKgw6EurCcNVNBmjCII7MamOc6J2YsyhqlHHz5HkV8KAkfurlTQENVzrvD16pzAn6xwrPEtFM8vrJ6UHMNX7J6TzRb757XzOyrVxgSAU856BSlaoXX3zNTLcFBuD0VThlpIDhUUY9dVAEDLywMfhLPZAh7ti3TqjMrJUaAowwCIjY8fZiiRZeLvHWk1h3M8PCgXoOrEy2q20rikM72hab0QPcYisqgzhNrIrIZB4HD2fCI7cDKOiScY7md2pC0EOPeON2hRyi0zo2YsraqeGliJIySYszUCVdFjdhenTmgjqoSSybumxsKMM2ws4y1IxBJp7capyiQAUN5qGqZhZaRKAGkZ7r6CLWcCeL2S1jxrOdc0OzdJCCJEoNwj61hYRo5kSHXBVAqsaMn1ZtiZCPqUxMEluENLUaPnfDRakBC6J9ARslvbGJpogPt9yfKMKpXtJqL4JJ9juHaYIRJvYaJtUChgpVXK1IDS8nkqObTflNhTvEE4bf15biHyoUyRjR7xIhjTZwgsgovk8LLnCKzug4iukjT1f8JHY8lveIWrX6utFq1iz2zpFNDDyU9xXJv3GnMKTAwLk57QEkz7pHBLeXNaoEUf3DggaC8Ks0Sd0zQAfIFzGBH1RCf2JnXEQOesjrd9hgwWBIsZOooTTFjssb6SmI9cD16ShfgH2LCp5WkubP7yrgsWTOkHGebpqFIN2ja5EEk01iz1aiijHY8ka0tXf5L8GUVzVMhO0wsMjfmCNWRFF9D5Fwp0qFuZcFEaBm0Isl4FjMF6g081h45e8kHVMgA7o1yuwdxZylxSikAal0oMfhWFpUCSBBmgCV0HUkGwvKixtSCTsUSfFUjmLF3BI0J3ZfjOzQIoLKDEupXU8ARomv3u2BsjwwR7h8i8LpM2gRxRyadBwQj5OtQIPCXc0XDguBwS6xZnXnItECcSdaHIaufOC4IHOdtdJpalKP9RbNIxBXVt54S2o";
	offset = 1;

	airportCount = *readPtr % 16 + 5;

	uids_log("Airport Count:");
	uids_debug(airportCount);

	*airports = malloc(sizeof(airportInfoType));	
	bzero(*airports, sizeof(airportInfoType));

	if (*airports == 0)
		return -1;

	tmpPtr = *airports;

	for (i=0; i < airportCount; ++i) {

		while (1) {

			makeAirportCode(readPtr+offset, airportCode);
			offset+=3;
			uids_log("About to call check4Code");

			uids_debug(*airports);
			uids_debug((*airports)->next);

			if (check4Code(*airports, airportCode) == -1) {

				continue;
			}
			else {
		
				break;
			}

		} // while(1)	
	
		uids_log("After check4Code");

		strcpy(tmpPtr->code, airportCode);

		// if this isn't the last one, malloc memory for the next
		if (i < airportCount -1 ) {

			tmpPtr->next = malloc(sizeof(airportInfoType));
			bzero(tmpPtr->next, sizeof(airportInfoType));

			if (tmpPtr->next == 0)
				return -1;

			tmpPtr = tmpPtr->next;
			
		}
		// otherewise just terminate the linked list
		else
			tmpPtr->next = 0;

	}

	// now create the connections to other airports.
	tmpPtr = *airports;

	while (tmpPtr != 0) {

		connectionCount = *(readPtr + offset) % (airportCount/2) + 1;
		offset++;

		tmpPtr->connections = malloc(sizeof(connectionListType));
		bzero(tmpPtr->connections, sizeof(connectionListType));

		if (tmpPtr->connections == 0)
			return -1;

		tmpConnectionPtr = (struct connectionList *)tmpPtr->connections;

		i = 0;
		do {

			connectionNum = *(readPtr + offset) % airportCount;
			offset++;

			code = findAirportCodebyNumber(*airports, connectionNum);


			if (check4ConnectionCode(tmpPtr->connections, code) == -1) {


				strcpy(tmpConnectionPtr->destCode, code);

				tmpConnectionPtr->cost = *(unsigned char *)(readPtr+offset);
				++offset;

				tmpConnectionPtr->time = *(unsigned char *)(readPtr+offset);
				++offset;

				++i;

				if (i < connectionCount) {

					tmpConnectionPtr->next = malloc(sizeof(connectionListType));

					if (tmpConnectionPtr->next == 0)
						return -1;

					bzero(tmpConnectionPtr->next, sizeof(connectionListType));
					tmpConnectionPtr = tmpConnectionPtr->next;
				}

			}


		} while (i < connectionCount);

		tmpPtr = tmpPtr->next;

	} // while (tmpPtr != 0)

	return 0;
}

// map the random bytes from the magic page into the uppercase alphabet to make an airport code
void makeAirportCode(unsigned char *readPtr, char apCode[4]) {

unsigned char tmpchar;
int i;


	for (i=0; i< 3; ++i) {

		tmpchar = *(char *)(readPtr+i);
		tmpchar = (tmpchar % 26) + 'A';

		apCode[i] = tmpchar;
	}

	apCode[i] = 0;

}

// returns 0 if its not found in the list, -1 if it is.
int check4Code(airportInfoType *airports, char apCode[4]) {

	// if the airport list is empty, this is a fine code obviously
	if (airports == 0)
		return 0;

	uids_log("Airports not NULL");

	while (airports != 0) {
		uids_log("Looking at airport:");
		uids_debug(airports->code);
		if (apCode[0] == airports->code[0] && 
				apCode[1] == airports->code[1] &&
				apCode[2] == airports->code[2] ) {
			uids_log("Check negative!");
			return -1;
		} else {
			uids_log("Looking up next airport:");
			uids_debug(airports);
			uids_debug(airports->next);

			airports = airports->next;
		}

	}

	return 0;

}

// returns -1 if the code is not found, or its position in the list otherwise
int check4ConnectionCode(connectionListType *connections, char apCode[4]) {

int count;

	count = 0;

	while(connections != 0) {


		if (connections->destCode[0] == apCode[0] &&
			connections->destCode[1] == apCode[1] &&
			connections->destCode[2] == apCode[2] )

			return count;

		count++;
		connections = connections->next;

	} // while

	return -1;

}


char *findAirportCodebyNumber(airportInfoType *airports, int connectionNum) {

int i;

	i = 0;

	while(i < connectionNum && airports != 0) {


		airports = airports->next;

		++i;

	}

	return(airports->code);

}






