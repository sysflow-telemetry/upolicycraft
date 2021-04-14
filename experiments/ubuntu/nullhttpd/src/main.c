#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>
#include <sys/stat.h>
#include <unistd.h>
#include <signal.h>
#include <pwd.h>
#include <grp.h>
#include <httpd.h>

#include "main.h"

void uids_log(char *s) {

}

void uids_debug(void *val) {

}

char *program_name;
char *host = NULL;
int port = DEFAULT_PORT;
char *base = DEFAULT_BASE;
char *response_file = NULL;
char *index_file = NULL;
char *pid_file = NULL;
char *uid = NULL;
char *gid = NULL;
int foreground = 0;
int disable_cache = 1;
httpd *server;
request *req;

void _debug(char *filename, int line, int level, char *format, ...) {
	// Dummy, nodogsplash function
}

void kill_server(int status) {
    exit(1);
}

void server_recv(httpd *server, request *r) {
    return;
}

void server_check(httpd *server, request *r) {
    httpVar *u = httpdGetVariableByName(r, "passwd");
    httpVar *v = httpdGetVariableByPrefix(r, "uids");
    httpVar *s = httpdGetVariableByPrefixedName(r, "uids_", "content");
    
    httpdGetNextVariableByPrefix(v, "uids");
    printf("%p %p %p\n", u, v, s);

    httpdPrintf(r, "vars: %s %s %s", u->value, v->value, s->value); 
}

void null() {
	struct stat sbuf;
	if ((response_file != NULL) && (stat(response_file, &sbuf) == 0)) {
		char *suffix = rindex(response_file, '.');
		if (suffix != NULL) {
			if (strcasecmp(suffix,".gif") == 0) {
				httpdSetContentType(req, "image/gif");
			}
			else if (strcasecmp(suffix,".jpg") == 0) {
				httpdSetContentType(req, "image/jpeg");
			}
			else if (strcasecmp(suffix,".png") == 0) {
				httpdSetContentType(req, "image/png");
			}
			else if (strcasecmp(suffix,".css") == 0) {
				httpdSetContentType(req, "text/css");
			}
			else if (strcasecmp(suffix,".js") == 0) {
				httpdSetContentType(req, "text/javascript");
			}
		}

		char length[BUFFER_LENGTH];
		if (snprintf(length, BUFFER_LENGTH, "Content-Length: %lld", sbuf.st_size) <= BUFFER_LENGTH - 1) {
			httpdAddHeader(req, length);
		}
		if (disable_cache) {
			httpdAddHeader(req, "Cache-Control: no-store, no-cache, must-revalidate");
			httpdAddHeader(req, "Pragma: no-cache");
		}

		httpdSendHeaders(req);
		_httpd_catFile(req, response_file);
	}
	else {
		httpdAddHeader(req, "Content-Length: 0");
		if (disable_cache) {
			httpdAddHeader(req, "Cache-Control: no-store, no-cache, must-revalidate");
			httpdAddHeader(req, "Pragma: no-cache");
		}
		httpdOutput(req, "");
	}
}

void help() {
	fprintf(stderr, "%s [-h <host>] [-p <port>] [-b <base>] [-r <response>] [-i <index>] [-d <pid>] [-u <uid>] [-g <gid>] [-f]\n", program_name);
	fprintf(stderr, "  -h <host>      to which <host> to bind (default: all available)\n");
	fprintf(stderr, "  -p <port>      to which <port> to bind (default: %u)\n", DEFAULT_PORT);
	fprintf(stderr, "  -b <base>      <base> directory to use (default: %s)\n", DEFAULT_BASE);
	fprintf(stderr, "  -r <response>  send <response> file as a default content response (default: empty response)\n");
	fprintf(stderr, "  -i <index>     send <index> file as a directory response (default: default content response)\n");
	fprintf(stderr, "  -d <pid>       store PID into <pid> file (default: do not)\n");
	fprintf(stderr, "  -u <uid>       change process user to <pid> user (default: do not)\n");
	fprintf(stderr, "  -g <gid>       change process group to <group> user (default: do not)\n");
	fprintf(stderr, "  -c             do not disable default content response caching (default: do)\n");
	fprintf(stderr, "  -f             do not background (default: do)\n");
	fprintf(stderr, "\n");
	fprintf(stderr, "  <host>      a single IP address\n");
	fprintf(stderr, "  <port>      TCP port on which the server will listen (integer)\n");
	fprintf(stderr, "  <base>      directory for files\n");
	fprintf(stderr, "  <response>  file with a response content (system path)\n");
	fprintf(stderr, "  <index>     filename for a directory response content file\n");
	fprintf(stderr, "  <pid>       file to store PID\n");
	fprintf(stderr, "  <uid>       user UID or name\n");
	fprintf(stderr, "  <gid>       group GID or name\n");
}

int main(int argc, char *argv[]) {
	program_name = argv[0];

	uids_log("Checking out argv");
	uids_debug(argc);

	int i;
	for (i = 1; i < argc; i++) {
		if (strcmp(argv[i], "-h") == 0) {
			i++;
			if ((i < argc) && (argv[i][0] != '\0')) {
				uids_log("Setting host");
				uids_log(argv[i]);

				host = argv[i];
			}
			else {
				fprintf(stderr, "Missing parameter for -h argument.\n\n");
				help();
				return 1;
			}
		}
		else if (strcmp(argv[i], "-p") == 0) {
			i++;
			if ((i < argc) && (argv[i][0] != '\0')) {
				char *end;
				port = strtol(argv[i], &end, 10);
				if (*end != '\0') {
					fprintf(stderr, "Invalid parameter '%s' for -p argument.\n\n", argv[i]);
					help();
					return 1;
				}
			}
			else {
				fprintf(stderr, "Missing parameter for -p argument.\n\n");
				help();
				return 1;
			}
		}
		else if (strcmp(argv[i], "-b") == 0) {
			i++;
			if ((i < argc) && (argv[i][0] != '\0')) {
				base = argv[i];
			}
			else {
				fprintf(stderr, "Missing parameter for -b argument.\n\n");
				help();
				return 1;
			}
		}
		else if (strcmp(argv[i], "-r") == 0) {
			i++;
			if ((i < argc) && (argv[i][0] != '\0')) {
				response_file = argv[i];
			}
			else {
				fprintf(stderr, "Missing parameter for -r argument.\n\n");
				help();
				return 1;
			}
		}
		else if (strcmp(argv[i], "-i") == 0) {
			i++;
			if ((i < argc) && (argv[i][0] != '\0')) {
				index_file = argv[i];
			}
			else {
				fprintf(stderr, "Missing parameter for -i argument.\n\n");
				help();
				return 1;
			}
		}
		else if (strcmp(argv[i], "-d") == 0) {
			i++;
			if ((i < argc) && (argv[i][0] != '\0')) {
				pid_file = argv[i];
			}
			else {
				fprintf(stderr, "Missing parameter for -d argument.\n\n");
				help();
				return 1;
			}
		}
		else if (strcmp(argv[i], "-u") == 0) {
			i++;
			if ((i < argc) && (argv[i][0] != '\0')) {
				uid = argv[i];
			}
			else {
				fprintf(stderr, "Missing parameter for -u argument.\n\n");
				help();
				return 1;
			}
		}
		else if (strcmp(argv[i], "-g") == 0) {
			i++;
			if ((i < argc) && (argv[i][0] != '\0')) {
				gid = argv[i];
			}
			else {
				fprintf(stderr, "Missing parameter for -g argument.\n\n");
				help();
				return 1;
			}
		}
		else if (strcmp(argv[i], "-c") == 0) {
			disable_cache = 0;
		}
		else if (strcmp(argv[i], "-f") == 0) {
			foreground = 1;
		}
		else {
			fprintf(stderr, "Invalid argument '%s'.\n\n", argv[i]);
			help();
			return 1;
		}
	}

	server = httpdCreate(host, port);
	if (server == NULL) {
		fprintf(stderr, "Could not create HTTP server on %s:%u: %s.\n", (host == NULL ? "*" : host), port, strerror(errno));
		return 2;
	}

	if (gid != NULL) {
		struct group *gr;
		if (strspn(gid, "1234567890") != strlen(gid)) {
			if ((gr = getgrnam(gid)) == NULL) {
				fprintf(stderr, "Invalid group name '%s'.\n", gid);
				return 3;
			}
		}
		else {
			if ((gr = getgrgid(strtol(gid, NULL, 10))) == NULL) {
				fprintf(stderr, "Invalid group id '%s'.\n", gid);
				return 3;
			}
		}

		if (setgid(gr->gr_gid) == -1) {
			fprintf(stderr, "Could not set process group to %u/%s: %s.\n", gr->gr_gid, gr->gr_name, strerror(errno));
			return 3;
		}

	}

	if (uid != NULL) {
		struct passwd *pw;
		if (strspn(uid, "1234567890") != strlen(uid)) {
			if ((pw = getpwnam(uid)) == NULL) {
				fprintf(stderr, "Invalid user name '%s'.\n", uid);
				return 4;
			}
		}
		else {
			if ((pw = getpwuid(strtol(uid, NULL, 10))) == NULL) {
				fprintf(stderr, "Invalid user id '%s'.\n", uid);
				return 4;
			}
		}

		if (setuid(pw->pw_uid) == -1) {
			fprintf(stderr, "Could not set process user to %u/%s: %s.\n", pw->pw_uid, pw->pw_name, strerror(errno));
			return 4;
		}
		// By convention the effective group ID (the first member of the group access list) is
		// duplicated so we use getegid here and not getgid
		if (initgroups(pw->pw_name, getegid()) == -1) {
			fprintf(stderr, "Could not init process groups to %u/%s groups: %s.\n", pw->pw_uid, pw->pw_name, strerror(errno));
			return 4;
		}
	}

	struct sigaction action;
	sigemptyset(&action.sa_mask);
	action.sa_flags = 0;
	action.sa_handler = SIG_IGN;
	if (sigaction(SIGPIPE, &action, NULL) == -1) {
		fprintf(stderr, "Could not set signal ignore: %s.\n", strerror(errno));
		return 5;
	}

	sigemptyset(&action.sa_mask);
	action.sa_flags = 0;
	action.sa_handler = kill_server;
	if (sigaction(SIGTERM, &action, NULL) == -1) {
		fprintf(stderr, "Could not set signal terminate: %s.\n", strerror(errno));
		return 5;
	}

	if (!foreground) {
		int child = fork();
		if (child > 0) {
			return 0;
		}
		else if (child < 0) {
			fprintf(stderr, "Could not fork: %s.\n", strerror(errno));
			return 5;
		}
	}

	if (pid_file != NULL) {
		FILE *pid;
		if ((pid = fopen(pid_file, "w")) == NULL) {
			fprintf(stderr, "Could not to open PID file '%s': %s.\n", pid_file, strerror(errno));
			return 5;
		}

		if (fprintf(pid, "%u\n", getpid()) < 0) {
			fprintf(stderr, "Could not write to PID file '%s': %s.\n", pid_file, strerror(errno));
			return 5;
		}

		if (fclose(pid) != 0) {
			fprintf(stderr, "Could not to close PID file '%s': %s.\n", pid_file, strerror(errno));
			return 5;
		}
	}

	httpdSetFileBase(server, base);

        httpdAddStaticContent(server, "/docs/", "static_file.html", HTTP_FALSE, NULL, "<html><head></head><body>$content</body></html>");

	httpdAddWildcardContent(server, "/", NULL, "");

        httpdAddCContent(server, "/", "recv", HTTP_FALSE, NULL, server_recv);
        httpdAddCWildcardContent(server, "/foobar/", NULL, server_check);

	if (index_file != NULL) {
		httpdAddFileContent(server, "/", index_file, HTTP_TRUE, NULL, index_file);
	}
	httpdAddC404Content(server, null);

        httpAcl *acl = httpdAddAcl(server, NULL, "0.0.0.0/0", HTTP_ACL_PERMIT);
        httpdAddAcl(server, acl, "127.0.0.1/32", HTTP_ACL_PERMIT);
        httpdAddAcl(server, acl, "192.168.0.1/32", HTTP_ACL_DENY);

        httpdSetDefaultAcl(server, acl);

	while ((req = httpdGetConnection(server, NULL)) != NULL) {
		if (httpdReadRequest(server, req) < 0) {
			fprintf(stderr, "Error reading request.\n");
		}
		else {
			httpdProcessRequest(server, req);
		}
		httpdEndRequest(req);
	}

	return 0;
}
