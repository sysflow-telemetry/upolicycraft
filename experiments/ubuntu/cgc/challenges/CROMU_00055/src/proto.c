/*

Author: Joe Rogers <joe@cromulence.com>

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
#include <libcgc.h>
#include "stdlib.h"
#include "stdio.h"
#include "string.h"
#include "malloc.h"
#include "fs.h"
#include "proto.h"

uint32_t ReadBytes(char *buf, uint32_t len) {
	uint32_t TotalBytes = 0;
	size_t rx_bytes;

	while (TotalBytes < len) {
		if (receive(STDIN, buf+TotalBytes, len-TotalBytes, &rx_bytes) != 0) {
			terminate(0);
		}	
		if (rx_bytes == 0) {
			uids_log("ending since nothing was received");
			terminate(0);
		}
        	char debug[50];
        	sprintf(debug, "received %d\n", rx_bytes);
		uids_log(debug);
		TotalBytes += rx_bytes;
	}

	return(TotalBytes);
}

uint32_t SendBytes(char *buf, uint32_t len) {
	uint32_t TotalBytes = 0;
	size_t tx_bytes;

	while (TotalBytes < len) {
		if (transmit(STDOUT, buf+TotalBytes, len-TotalBytes, &tx_bytes) != 0) {
			uids_log("transmit not equal to 0");
			terminate(0);
		}
		if (tx_bytes != len-TotalBytes) {
			uids_log("tx_bytes != len-TotalBytes");
			terminate(0);
		}
		TotalBytes += tx_bytes;
	}

	return(TotalBytes);
}

#ifdef PATCHED_1
void* CallocAndRead(uint32_t len) {
#else 
void* CallocAndRead(uint8_t len) {
#endif
	char *buf;
	// extra byte to be sure strings are null terminated
	len++;
	if ((buf = calloc(len, 1)) == NULL) {
		terminate(0);
	}

#ifdef DEBUG
	char debug[50];
	sprintf(debug, "calloc0($d)=$08x\n", len, *buf);
	transmit(STDERR, debug, strlen(debug), 0);
#endif
	len--;
	ReadBytes(buf, len);
	return buf;
}

pRequest ReceiveRequest(void) {
	char buf[10];
	pRequestHeader pReqHdr = (pRequestHeader)buf;
	pRequest pReq;

	// Allocate a struct to hold the request
	if ((pReq = calloc(sizeof(Request), 1)) == NULL) {
		terminate(0);
	}
#ifdef DEBUG
	char debug[50];
	sprintf(debug, "calloc0($d)=$08x\n", sizeof(Request), pReq);
	transmit(STDERR, debug, strlen(debug), 0);
#endif
	uids_log("Parsing Bytes!");
	// Receive until we have RequestHeader length bytes
	ReadBytes(buf, REQUEST_HEADER_LEN);

	uids_log("Read Header");

	// See what type of packet we have and
	// receive the bytes for the specified type header
	pReq->Type = pReqHdr->Type;
        char debug[50];
        sprintf(debug, "type %d\n", pReqHdr->Type);
	uids_log(debug);

	printf("pReqHdr->Type: %d\n", pReqHdr->Type);

	if (pReqHdr->Type == CFS_LOGIN) {
			uids_log("Logging in");
			ReadBytes(buf+REQUEST_HEADER_LEN, sizeof(LoginHeader));
			pLoginHeader pLoginHdr = (pLoginHeader)(buf+REQUEST_HEADER_LEN);

			// allocate space to hold the username and read it in
			pReq->Username = CallocAndRead(pLoginHdr->UsernameLen);

			// allocate space to hold the password and read it in
			pReq->Password = CallocAndRead(pLoginHdr->PasswordLen);

			uids_log("Credentials");
			uids_log(pReq->Username);
			uids_log(pReq->Password);
	} else if (pReqHdr->Type == CFS_DIR) {
			// nothing more to parse
	} else if (pReqHdr->Type == CFS_READ) {
			uids_log("Reading");
			ReadBytes(buf+REQUEST_HEADER_LEN, sizeof(ReadHeader));
			pReadHeader pReadHdr = (pReadHeader)(buf+REQUEST_HEADER_LEN);

			// alocate space to hold the filename and read it in
			pReq->Filename = CallocAndRead(pReadHdr->FilenameLen);

			// Get the read Offset and Length values
			pReq->Offset = pReadHdr->Offset;
			pReq->ReadWriteLength = pReadHdr->Length;
	} else if (pReqHdr->Type == CFS_WRITE || pReqHdr->Type == CFS_WRITE_APPEND) {
			uids_log("Writing");
			ReadBytes(buf+REQUEST_HEADER_LEN, sizeof(WriteHeader));
			pWriteHeader pWriteHdr = (pWriteHeader)(buf+REQUEST_HEADER_LEN);

			// alocate space to hold the filename and read it in
			pReq->Filename = CallocAndRead(pWriteHdr->FilenameLen);

			// Get the Length value
			pReq->ReadWriteLength = pWriteHdr->Length;

			// read in the bytes to be written
			pReq->DataLen = pWriteHdr->Length;
			pReq->Data = CallocAndRead(pWriteHdr->Length);
	} else if (pReqHdr->Type == CFS_DEL) {
			uids_log("Deleting");
			ReadBytes(buf+REQUEST_HEADER_LEN, sizeof(DelHeader));
			pDelHeader pDelHdr = (pDelHeader)(buf+REQUEST_HEADER_LEN);

			// alocate space to hold the filename and read it in
			pReq->Filename = CallocAndRead(pDelHdr->FilenameLen);
	} else if (pReqHdr->Type == CFS_RENAME) {
			uids_log("Renaming");
			ReadBytes(buf+REQUEST_HEADER_LEN, sizeof(RenameHeader));
			pRenameHeader pRenameHdr = (pRenameHeader)(buf+REQUEST_HEADER_LEN);

			// alocate space to hold the original filename and read it in
			pReq->Filename = CallocAndRead(pRenameHdr->OldFilenameLen);
			
			// alocate space to hold the new filename and read it in
			pReq->Filename2	= CallocAndRead(pRenameHdr->NewFilenameLen);
	} else {
#ifdef DEBUG
			sprintf(debug, "free($08x)\n", pReq);
			transmit(STDERR, debug, strlen(debug), 0);
#endif
			free(pReq);
			pReq = NULL;
	}

	// Return the populated Request
	return(pReq);
}

uint8_t HandleLogin(pRequest pReq, pResponse pResp) {
	uids_log("Handling login");
	if (!pReq || !pResp) {
		uids_log("pReq or pResp missing");
		return(0);
	}

	uids_log(pReq->Username);
	uids_log(pReq->Password);

	if (!CheckPasswd(pReq->Username, pReq->Password)) {
		// login failed
		pResp->Code = RESP_LOGIN_FAILED;
		return(0);
	}
	if (!Login(pReq->Username)) {
		// system error logging in
		pResp->Code = RESP_SYSTEM_FAILURE;
		return(0);
	}

	// login success
	pResp->Code = RESP_SUCCESS;

	return(1);
}

uint8_t HandleDir(pRequest pReq, pResponse pResp) {
	char *Buf;

	if (!pReq || !pResp) {
		return(0);
	}

	// get the listing
	if (!ListFiles(&Buf)) {
		pResp->Code = RESP_SYSTEM_FAILURE;
		return(0);
	}

	// Send back the listing
	pResp->Code = RESP_SUCCESS;
	pResp->DataLen = strlen(Buf);
	pResp->Data = Buf;

	return(1);

}

uint8_t HandleRead(pRequest pReq, pResponse pResp) {
	FILE *in;
	uint32_t i;
	uint32_t rx_bytes;
	char buf[128];

	if (!pReq || !pResp) {
		return(0);
	}

	// open the requested file
	if (pReq->Filename == NULL) {
		pResp->Code = RESP_INVALID_FILE;
		return(0);
	}
	if ((in = fopen0(pReq->Filename, "r")) == NULL) {
		pResp->Code = RESP_INVALID_FILE;
		return(0);
	}
	
	// seek to the requested offset
	for (i = 0; i < pReq->Offset; ) {
		if ((pReq->Offset-i) > 128) {
			if ((rx_bytes = fread0(buf, 128, 1, in)) == 0) {
				pResp->Code = RESP_SYSTEM_FAILURE;
				fclose0(in);
				return(0);
			}
		} else {
			if ((rx_bytes = fread0(buf, pReq->Offset-i, 1, in)) == 0) {
				pResp->Code = RESP_SYSTEM_FAILURE;
				fclose0(in);
				return(0);
			}
		}
		i += rx_bytes;
	}
	
	// allocate space to hold the response
	if ((pResp->Data = calloc(pReq->ReadWriteLength, 1)) == NULL) {
		pResp->Code = RESP_SYSTEM_FAILURE;
		fclose0(in);
		return(0);
	}
#ifdef DEBUG
	char debug[50];
	sprintf(debug, "calloc0($d)=$08x\n", pReq->ReadWriteLength, pResp);
	transmit(STDERR, debug, strlen(debug), 0);
#endif
	
	// read the requested number of bytes
	if ((rx_bytes = fread0(pResp->Data, pReq->ReadWriteLength, 1, in)) == 0) {
		pResp->Code = RESP_SYSTEM_FAILURE;
		fclose0(in);
		return(0);
	}
	pResp->DataLen = rx_bytes;

	// request complete
	fclose0(in);
	pResp->Code = RESP_SUCCESS;
	return(1);

}

uint8_t HandleWrite(pRequest pReq, pResponse pResp) {
	FILE *out;
	uint32_t i;

	if (!pReq || !pResp) {
		return(0);
	}

	// open the requested file
	if (pReq->Filename == NULL) {
		pResp->Code = RESP_INVALID_FILE;
		return(0);
	}
	if ((out = fopen0(pReq->Filename, "w")) == NULL) {
		pResp->Code = RESP_INVALID_FILE;
		return(0);
	}
	
	// write the requested number of bytes
	if (fwrite0(pReq->Data, pReq->DataLen, 1, out) == 0) {
		pResp->Code = RESP_SYSTEM_FAILURE;
		fclose0(out);
		return(0);
	}

	// request complete
	fclose0(out);
	pResp->Code = RESP_SUCCESS;
	return(1);

}

uint8_t HandleWriteAppend(pRequest pReq, pResponse pResp) {
	char *CurrContents;
	uint32_t CurrFileLen;
	FILE *in, *out;

	if (!pReq || !pResp) {
		return(0);
	}

	// read in the file's current contents
	if (pReq->Filename == NULL) {
		pResp->Code = RESP_INVALID_FILE;
		return(0);
	}
	if ((in = fopen0(pReq->Filename, "r")) == NULL) {
		pResp->Code = RESP_INVALID_FILE;
		return(0);
	}
	CurrFileLen = in->Inode->FileSize;
	if ((CurrContents = calloc(CurrFileLen, 1)) == NULL) {
		pResp->Code = RESP_SYSTEM_FAILURE;
		fclose0(in);
		return(0);
	}
#ifdef DEBUG
	char debug[50];
	sprintf(debug, "calloc0($d)=$08x\n", CurrFileLen, CurrContents);
	transmit(STDERR, debug, strlen(debug), 0);
#endif
	if (fread0(CurrContents, CurrFileLen, 1, in) != CurrFileLen) {
		pResp->Code = RESP_SYSTEM_FAILURE;
		free(CurrContents);
		fclose0(in);
		return(0);
	}
	fclose0(in);

	// write the current contents back to the file
	if ((out = fopen0(pReq->Filename, "w")) == NULL) {
		pResp->Code = RESP_SYSTEM_FAILURE;
		free(CurrContents);
		return(0);
	}
	if (fwrite0(CurrContents, CurrFileLen, 1, out) != CurrFileLen) {
		pResp->Code = RESP_SYSTEM_FAILURE;
		free(CurrContents);
		fclose0(out);
		return(0);
	}
	free(CurrContents);
	
	// write the new data to the end of the file 
	if (fwrite0(pReq->Data, pReq->DataLen, 1, out) == 0) {
		pResp->Code = RESP_SYSTEM_FAILURE;
		fclose0(out);
		return(0);
	}

	// request complete
	fclose0(out);
	pResp->Code = RESP_SUCCESS;
	return(1);
}

uint8_t HandleDel(pRequest pReq, pResponse pResp) {
	if (!pReq || !pResp) {
		return(0);
	}

	// attempt to delete the requested file
	if (!DeleteFile(pReq->Filename)) {
		pResp->Code = RESP_DELETE_FAILED;
		return(0);
	}
	
	// request complete
	pResp->Code = RESP_SUCCESS;
	return(1);
}

uint8_t HandleRename(pRequest pReq, pResponse pResp) {
	if (!pReq || !pResp) {
		return(0);
	}

	// attempt to rename the requested file
	if (!RenameFile(pReq->Filename, pReq->Filename2)) {
		pResp->Code = RESP_RENAME_FAILED;
		return(0);
	}
	
	// request complete
	pResp->Code = RESP_SUCCESS;
	return(1);
}

pResponse HandleRequest(pRequest pReq) {
	pResponse pResp;

	if (!pReq) {
		uids_log("missing pReq!");
		return(NULL);
	}

	// Allocate a response
	if ((pResp = calloc(sizeof(Response), 1)) == NULL) {
		terminate(0);
	}
#ifdef DEBUG
	char debug[50];
	sprintf(debug, "calloc0($d)=$08x\n", sizeof(Response), pResp);
	transmit(STDERR, debug, strlen(debug), 0);
#endif
	pResp->Type = pReq->Type;
	
	// See what type of request we have
        if (pReq->Type == CFS_LOGIN) {
		HandleLogin(pReq, pResp);
	} else if (pReq->Type == CFS_DIR) {
		HandleDir(pReq, pResp);
	} else if (pReq->Type == CFS_READ) {
		HandleRead(pReq, pResp);
	} else if (pReq->Type == CFS_WRITE) {
		HandleWrite(pReq, pResp);
	} else if (pReq->Type == CFS_WRITE_APPEND) {
		HandleWriteAppend(pReq, pResp);
	} else if (pReq->Type == CFS_DEL) {
		HandleDel(pReq, pResp);
	} else if (pReq->Type == CFS_RENAME) {
		HandleRename(pReq, pResp);
	} else {
		uids_log("pReq type not found");
		free(pResp);
		pResp = NULL;
	}

	// Return the reponse
	return(pResp);

}

uint8_t SendResponse(pResponse pResp) {

	if (!pResp) {
		return(0);
	}

	// Send the Response header 
	if (SendBytes((char *)pResp, RESPONSE_HEADER_LEN) != RESPONSE_HEADER_LEN) {
		return(0);
	}

	uids_log("Sent header");

	// Return if there's no extra data to send
	if (pResp->DataLen == 0) {
		uids_log("no data to send!");
		return(1);
	}

	uids_log(pResp->Data);

	// Send the extra data
	if (SendBytes(pResp->Data, pResp->DataLen) != pResp->DataLen) {
		return(0);
	}

	uids_log("Sent Data");

	return(1);
}

uint8_t FreeRequest(pRequest pReq) {

	if (!pReq) {
		return(0);
	}

#ifdef DEBUG
	char debug[50];
#endif

	// check and free the Username
	if (pReq->Username) {
		uids_log("username");
#ifdef DEBUG
		sprintf(debug, "free($08x)\n", pReq->Username);
		transmit(STDERR, debug, strlen(debug), 0);
#endif
		free(pReq->Username);
		pReq->Username = NULL;
		uids_log("after username");
	}

	// check and free the Password
	if (pReq->Password) {
		uids_log("password");
#ifdef DEBUG
		sprintf(debug, "free($08x)\n", pReq->Password);
		transmit(STDERR, debug, strlen(debug), 0);
#endif
		free(pReq->Password);
		pReq->Password = NULL;
	}

	// check and free the Filename
	if (pReq->Filename) {
		uids_log("filename");
#ifdef DEBUG
		sprintf(debug, "free($08x)\n", pReq->Filename);
		transmit(STDERR, debug, strlen(debug), 0);
#endif
		free(pReq->Filename);
		pReq->Filename = NULL;
	}

	// check and free the Filename2
	if (pReq->Filename2) {
		uids_log("filename2");
#ifdef DEBUG
		sprintf(debug, "free($08x)\n", pReq->Filename2);
		transmit(STDERR, debug, strlen(debug), 0);
#endif
		free(pReq->Filename2);
		pReq->Filename2 = NULL;
	}

	// check and free the Data
	if (pReq->Data) {
		uids_log("data");
#ifdef DEBUG
		sprintf(debug, "free($08x)\n", pReq->Data);
		transmit(STDERR, debug, strlen(debug), 0);
#endif
		free(pReq->Data);
		pReq->Data = NULL;
	}

#ifdef DEBUG
	sprintf(debug, "free($08x)\n", pReq);
	transmit(STDERR, debug, strlen(debug), 0);
#endif
	uids_log("before free pReq");
	free(pReq);
	pReq = NULL;

	return(1);
}

uint8_t FreeResponse(pResponse pResp) {

	if (!pResp) {
		return(0);
	}

#ifdef DEBUG
	char debug[50];
#endif 

	if (pResp->Data) {
#ifdef DEBUG
		char debug[50];
		sprintf(debug, "free($08x)\n", pResp->Data);
		transmit(STDERR, debug, strlen(debug), 0);
#endif
		free(pResp->Data);
		pResp->Data = NULL;
	}
#ifdef DEBUG
	sprintf(debug, "free($08x)\n", pResp);
	transmit(STDERR, debug, strlen(debug), 0);
#endif
	free(pResp);
	pResp = NULL;

	return(1);
}
