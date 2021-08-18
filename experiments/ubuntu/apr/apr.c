#include "apr.h"
#include "apr_strings.h"
#include "apr_getopt.h"
#include "apr_general.h"
#include "apr_lib.h"
#include "apr_md5.h"
#include "apr_hash.h"
#include "apr_tables.h"
#include "apr_time.h"
#include "apr_version.h"
#include "apu_version.h"

#include <assert.h>
#include <stdio.h>
#include <stdint.h>
#include <strings.h>

int main(int argc, char * const argv[]) {
    apr_status_t stat = apr_pool_initialize();

    assert(stat == APR_SUCCESS);
    
    apr_pool_t *pool;
    stat = apr_pool_create(&pool, NULL);

    assert(stat == APR_SUCCESS);

    puts("Initialized pool\n");

    apr_hash_t *ht = apr_hash_make(pool);

    puts("Created hash table\n");

    char *msg = "Hello World!";

    int64_t key = 0xdeadbeef;
    int64_t data[] = {0xdeadbeef, 0xfabc0de};
    int64_t data1[] = {0x0};

    apr_hash_set(ht, (void*)msg, APR_HASH_KEY_STRING, (void*)data);
    apr_hash_set(ht, &key, sizeof(int64_t), (void*)data1);

    puts("Set hash keys\n");

    int64_t *rdata = (int64_t*)apr_hash_get(ht, (void*)msg, APR_HASH_KEY_STRING);
    int64_t *rdata1 = (int64_t*)apr_hash_get(ht, (void*)&key, sizeof(int64_t));

    puts("Retrieved hashes\n");

    assert(data == rdata);
    assert(data1 == rdata1);

    apr_array_header_t  *at = apr_array_make(pool, 10, sizeof(int64_t));

    puts("Made Array\n");
    int64_t *p = apr_array_push(at);
    *p = 0xdeadc0de;
    puts("Pushed Array\n");

    p = apr_array_pop(at);
    puts("Popped Array\n");

    assert(*p == 0xdeadc0de);
}
