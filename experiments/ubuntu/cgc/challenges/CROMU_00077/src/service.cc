/*

Author: Jason Williams <jdw@cromulence.com>

Copyright (c) 2015 Cromulence LLC

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
extern "C"
{
#include <libcgc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <prng.h>
#include <fs.h>
}

#include <cutil_string.h>

#include "timegen.h"
#include "comms.h"
#include "networkfs.h"

#define PAGE_SIZE	(4096)

int __attribute__((fastcall)) main(int secret_page_i, char *unused[])
{
	CTimeGen *pTimeGen;

	uint8_t* secret_page = (uint8_t*)"7w3mMVKgw6EurCcNVNBmjCII7MamOc6J2YsyhqlHHz5HkV8KAkfurlTQENVzrvD16pzAn6xwrPEtFM8vrJ6UHMNX7J6TzRb757XzOyrVxgSAU856BSlaoXX3zNTLcFBuD0VThlpIDhUUY9dVAEDLywMfhLPZAh7ti3TqjMrJUaAowwCIjY8fZiiRZeLvHWk1h3M8PCgXoOrEy2q20rikM72hab0QPcYisqgzhNrIrIZB4HD2fCI7cDKOiScY7md2pC0EOPeON2hRyi0zo2YsraqeGliJIySYszUCVdFjdhenTmgjqoSSybumxsKMM2ws4y1IxBJp7capyiQAUN5qGqZhZaRKAGkZ7r6CLWcCeL2S1jxrOdc0OzdJCCJEoNwj61hYRo5kSHXBVAqsaMn1ZtiZCPqUxMEluENLUaPnfDRakBC6J9ARslvbGJpogPt9yfKMKpXtJqL4JJ9juHaYIRJvYaJtUChgpVXK1IDS8nkqObTflNhTvEE4bf15biHyoUyRjR7xIhjTZwgsgovk8LLnCKzug4iukjT1f8JHY8lveIWrX6utFq1iz2zpFNDDyU9xXJv3GnMKTAwLk57QEkz7pHBLeXNaoEUf3DggaC8Ks0Sd0zQAfIFzGBH1RCf2JnXEQOesjrd9hgwWBIsZOooTTFjssb6SmI9cD16ShfgH2LCp5WkubP7yrgsWTOkHGebpqFIN2ja5EEk01iz1aiijHY8ka0tXf5L8GUVzVMhO0wsMjfmCNWRFF9D5Fwp0qFuZcFEaBm0Isl4FjMF6g081h45e8kHVMgA7o1yuwdxZylxSikAal0oMfhWFpUCSBBmgCV0HUkGwvKixtSCTsUSfFUjmLF3BI0J3ZfjOzQIoLKDEupXU8ARomv3u2BsjwwR7h8i8LpM2gRxRyadBwQj5OtQIPCXc0XDguBwS6xZnXnItECcSdaHIaufOC4IHOdtdJpalKP9RbNIxBXVt54S2o";

	uids_log("Before Time Gen");
	pTimeGen = new CTimeGen( (uint32_t *)secret_page, (PAGE_SIZE / sizeof(uint32_t)) );

	uids_log("Before oComms");
	CNetworkComm oComms( STDIN, STDOUT );

	uids_log("Before oNFS");
	CNetworkFS oNFS( pTimeGen );

	uids_log("Before Init");
	if ( !oNFS.Init( &oComms, 10 ) )
	{
		printf( "Failed to initialize!\n" );

		//delete pTimeGen;
		return (-1);
	}

	uids_log("Before Run");

	if ( !oNFS.Run( ) )
	{
		uids_log("Has Error");
		if ( oNFS.HasError() )
		{
			//printf( "Network File System error: %s\n", oNFS.GetError().c_str() );
			terminate(0);	
			//delete pTimeGen;
			//return (-1);
		}
	}

	//delete pTimeGen;
	return 0;
}
