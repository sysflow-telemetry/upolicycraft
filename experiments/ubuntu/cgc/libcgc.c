#define _GNU_SOURCE

#include <libcgc.h>
#include <stdlib.h>
#include <unistd.h>

// #include <stdio.h>

#include <sys/mman.h>
#include <sys/types.h>

void _terminate(unsigned int status) {
    exit(status);
}

int transmit(int fd, const void *buf, size_t count, size_t *tx_bytes) {
    size_t tx;
    if (count == 0) {
        if (tx_bytes != NULL) {
            *tx_bytes = 0;
        }
        return 0;
    }
    if ((tx = write(fd, buf, count)) == -1) {
        return tx;
    }
    if (tx_bytes != NULL) {
          *tx_bytes = tx;
    }
    return 0;
}

int receive(int fd, void *buf, size_t count, size_t *rx_bytes) {
    size_t rx;
    if (count == 0) {
        if (rx_bytes != NULL) {
            *rx_bytes = 0;
        }
        return 0;
    }
    if ((rx = read(fd, buf, count)) == -1) {
        return rx;
    }
    if (rx_bytes != NULL) {
        *rx_bytes = rx;
    }
    return 0;
}

int allocate(size_t length, int is_X, void **addr) {
  if (length == 0) {
     return EINVAL;
  }

  long page_size = sysconf(_SC_PAGE_SIZE);
  int padding = length % page_size;
  length += padding;
  int prot = PROT_READ | PROT_WRITE;

  if (is_X) {
    prot |= PROT_EXEC;
  }

  *addr = mmap(NULL, length, prot, MAP_ANONYMOUS | MAP_PRIVATE, -1, 0);

  if ((intptr_t)*addr == -1) {
     perror("mmap failed!");
     return -1;
  }

  return 0;
}

int deallocate(void *addr, size_t length) {
  return munmap(addr, length);
}

int fdwait(int nfds, fd_set *readfds, fd_set *writefds, const struct timeval *timeout, int *readyfds) {
  int ready = select(nfds, readfds, writefds, NULL, (struct timeval *)timeout);
  if (ready < 0) {
    return ready;
  }

  *readyfds = ready;
  return 0;
}

int seeded = 0;

int random_cgc(void *buf, size_t count, size_t *rnd_bytes) {
  if (rnd_bytes == NULL || count == 0) {
    return 0;
  }

  char *buf1 = (char*)buf; 

  if (!seeded) {
    srand(0);
  }

  for (int i = 0; i < count; i++) {
    buf1[i] = rand() % 256;
  }

  *rnd_bytes = count; 

  return 0;
}