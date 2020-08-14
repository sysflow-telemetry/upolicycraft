#include <libcgc.h>
#include <stdlib.h>
#include <unistd.h>

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

