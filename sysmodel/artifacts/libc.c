#include "mylibc.h"

/**
   From glibc:

   1fa9b:	45 31 e4             	xor    r12d,r12d <-- SYSCALL for 0x1fa9b
   1fa9e:	bd 01 00 00 00       	mov    ebp,0x1 <-- SYSCALL for 0x1fae8
   1faa3:	ba 00 04 00 00       	mov    edx,0x400
   1faa8:	4c 89 d6             	mov    rsi,r10
   1faab:	4c 89 c7             	mov    rdi,r8
   1faae:	44 89 e0             	mov    eax,r12d
   1fab1:	0f 05                	syscall
   1fab3:	48 3d 00 f0 ff ff    	cmp    rax,0xfffffffffffff000
   1fab9:	49 89 c1             	mov    r9,rax
   1fabc:	76 1a                	jbe    1fad8 <*ABS*+0x8fa00@plt+0x248>
   1fabe:	48 8b 05 b3 43 3a 00 	mov    rax,QWORD PTR [rip+0x3a43b3]        # 3c3e78 <_IO_file_jumps@@GLIBC_2.2.5+0x798>
   1fac5:	41 f7 d9             	neg    r9d
   1fac8:	64 44 89 08          	mov    DWORD PTR fs:[rax],r9d
   1facc:	4c 89 c7             	mov    rdi,r8
   1facf:	b8 03 00 00 00       	mov    eax,0x3
   1fad4:	0f 05                	syscall
   1fad6:	eb 31                	jmp    1fb09 <*ABS*+0x8fa00@plt+0x279>
   1fad8:	48 85 c0             	test   rax,rax
   1fadb:	7e ef                	jle    1facc <*ABS*+0x8fa00@plt+0x23c>
   1fadd:	4c 89 ca             	mov    rdx,r9
   1fae0:	4c 89 d6             	mov    rsi,r10
   1fae3:	48 89 df             	mov    rdi,rbx
   1fae6:	89 e8                	mov    eax,ebp
   1fae8:	0f 05                   syscall
*/

int myread(int fd, char *buf, int nbytes) {
    __asm__ ("xor %r12d, %r12d\n\t"
             "mov $0x128, %esi\n\t"
             "mov %r12d, %eax\n\t"
             "syscall");
}

int myputs(char *s) {
    __asm__ ("mov $0x1, %eax\n\t"
             "syscall");
    return 0;
}

void myexit(int status) {
    __asm__ ("mov $0x3c, %eax\n\t"
             "syscall");
}

void branching(int status) {
    if (status == 1) {
        __asm__ ("mov $0x3c, %eax\n\t"
                 "syscall");
    } else {
        __asm__ ("mov $0x10, %eax\n\t"
                 "syscall");
    }
}

void nothing() {
    int x = 0;

    for (int i = 0; i < 10; i++) {
	    x++;
    }

    __asm__ ("syscall");
    return;
}
