
cron:     file format elf64-x86-64


Disassembly of section .init:

0000000000401bf0 <_init>:
  401bf0:	48 83 ec 08          	sub    $0x8,%rsp
  401bf4:	48 8b 05 fd 93 20 00 	mov    0x2093fd(%rip),%rax        # 60aff8 <_DYNAMIC+0x1d0>
  401bfb:	48 85 c0             	test   %rax,%rax
  401bfe:	74 05                	je     401c05 <_init+0x15>
  401c00:	e8 bb 06 00 00       	callq  4022c0 <seteuid@plt+0x10>
  401c05:	48 83 c4 08          	add    $0x8,%rsp
  401c09:	c3                   	retq   

Disassembly of section .plt:

0000000000401c10 <sigprocmask@plt-0x10>:
  401c10:	ff 35 f2 93 20 00    	pushq  0x2093f2(%rip)        # 60b008 <_GLOBAL_OFFSET_TABLE_+0x8>
  401c16:	ff 25 f4 93 20 00    	jmpq   *0x2093f4(%rip)        # 60b010 <_GLOBAL_OFFSET_TABLE_+0x10>
  401c1c:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000401c20 <sigprocmask@plt>:
  401c20:	ff 25 f2 93 20 00    	jmpq   *0x2093f2(%rip)        # 60b018 <_GLOBAL_OFFSET_TABLE_+0x18>
  401c26:	68 00 00 00 00       	pushq  $0x0
  401c2b:	e9 e0 ff ff ff       	jmpq   401c10 <_init+0x20>

0000000000401c30 <free@plt>:
  401c30:	ff 25 ea 93 20 00    	jmpq   *0x2093ea(%rip)        # 60b020 <_GLOBAL_OFFSET_TABLE_+0x20>
  401c36:	68 01 00 00 00       	pushq  $0x1
  401c3b:	e9 d0 ff ff ff       	jmpq   401c10 <_init+0x20>

0000000000401c40 <strcasecmp@plt>:
  401c40:	ff 25 e2 93 20 00    	jmpq   *0x2093e2(%rip)        # 60b028 <_GLOBAL_OFFSET_TABLE_+0x28>
  401c46:	68 02 00 00 00       	pushq  $0x2
  401c4b:	e9 c0 ff ff ff       	jmpq   401c10 <_init+0x20>

0000000000401c50 <closelog@plt>:
  401c50:	ff 25 da 93 20 00    	jmpq   *0x2093da(%rip)        # 60b030 <_GLOBAL_OFFSET_TABLE_+0x30>
  401c56:	68 03 00 00 00       	pushq  $0x3
  401c5b:	e9 b0 ff ff ff       	jmpq   401c10 <_init+0x20>

0000000000401c60 <localtime@plt>:
  401c60:	ff 25 d2 93 20 00    	jmpq   *0x2093d2(%rip)        # 60b038 <_GLOBAL_OFFSET_TABLE_+0x38>
  401c66:	68 04 00 00 00       	pushq  $0x4
  401c6b:	e9 a0 ff ff ff       	jmpq   401c10 <_init+0x20>

0000000000401c70 <abort@plt>:
  401c70:	ff 25 ca 93 20 00    	jmpq   *0x2093ca(%rip)        # 60b040 <_GLOBAL_OFFSET_TABLE_+0x40>
  401c76:	68 05 00 00 00       	pushq  $0x5
  401c7b:	e9 90 ff ff ff       	jmpq   401c10 <_init+0x20>

0000000000401c80 <__errno_location@plt>:
  401c80:	ff 25 c2 93 20 00    	jmpq   *0x2093c2(%rip)        # 60b048 <_GLOBAL_OFFSET_TABLE_+0x48>
  401c86:	68 06 00 00 00       	pushq  $0x6
  401c8b:	e9 80 ff ff ff       	jmpq   401c10 <_init+0x20>

0000000000401c90 <strncpy@plt>:
  401c90:	ff 25 ba 93 20 00    	jmpq   *0x2093ba(%rip)        # 60b050 <_GLOBAL_OFFSET_TABLE_+0x50>
  401c96:	68 07 00 00 00       	pushq  $0x7
  401c9b:	e9 70 ff ff ff       	jmpq   401c10 <_init+0x20>

0000000000401ca0 <strncmp@plt>:
  401ca0:	ff 25 b2 93 20 00    	jmpq   *0x2093b2(%rip)        # 60b058 <_GLOBAL_OFFSET_TABLE_+0x58>
  401ca6:	68 08 00 00 00       	pushq  $0x8
  401cab:	e9 60 ff ff ff       	jmpq   401c10 <_init+0x20>

0000000000401cb0 <_exit@plt>:
  401cb0:	ff 25 aa 93 20 00    	jmpq   *0x2093aa(%rip)        # 60b060 <_GLOBAL_OFFSET_TABLE_+0x60>
  401cb6:	68 09 00 00 00       	pushq  $0x9
  401cbb:	e9 50 ff ff ff       	jmpq   401c10 <_init+0x20>

0000000000401cc0 <strcpy@plt>:
  401cc0:	ff 25 a2 93 20 00    	jmpq   *0x2093a2(%rip)        # 60b068 <_GLOBAL_OFFSET_TABLE_+0x68>
  401cc6:	68 0a 00 00 00       	pushq  $0xa
  401ccb:	e9 40 ff ff ff       	jmpq   401c10 <_init+0x20>

0000000000401cd0 <__isoc99_fscanf@plt>:
  401cd0:	ff 25 9a 93 20 00    	jmpq   *0x20939a(%rip)        # 60b070 <_GLOBAL_OFFSET_TABLE_+0x70>
  401cd6:	68 0b 00 00 00       	pushq  $0xb
  401cdb:	e9 30 ff ff ff       	jmpq   401c10 <_init+0x20>

0000000000401ce0 <mkdir@plt>:
  401ce0:	ff 25 92 93 20 00    	jmpq   *0x209392(%rip)        # 60b078 <_GLOBAL_OFFSET_TABLE_+0x78>
  401ce6:	68 0c 00 00 00       	pushq  $0xc
  401ceb:	e9 20 ff ff ff       	jmpq   401c10 <_init+0x20>

0000000000401cf0 <toupper@plt>:
  401cf0:	ff 25 8a 93 20 00    	jmpq   *0x20938a(%rip)        # 60b080 <_GLOBAL_OFFSET_TABLE_+0x80>
  401cf6:	68 0d 00 00 00       	pushq  $0xd
  401cfb:	e9 10 ff ff ff       	jmpq   401c10 <_init+0x20>

0000000000401d00 <puts@plt>:
  401d00:	ff 25 82 93 20 00    	jmpq   *0x209382(%rip)        # 60b088 <_GLOBAL_OFFSET_TABLE_+0x88>
  401d06:	68 0e 00 00 00       	pushq  $0xe
  401d0b:	e9 00 ff ff ff       	jmpq   401c10 <_init+0x20>

0000000000401d10 <ferror@plt>:
  401d10:	ff 25 7a 93 20 00    	jmpq   *0x20937a(%rip)        # 60b090 <_GLOBAL_OFFSET_TABLE_+0x90>
  401d16:	68 0f 00 00 00       	pushq  $0xf
  401d1b:	e9 f0 fe ff ff       	jmpq   401c10 <_init+0x20>

0000000000401d20 <fread@plt>:
  401d20:	ff 25 72 93 20 00    	jmpq   *0x209372(%rip)        # 60b098 <_GLOBAL_OFFSET_TABLE_+0x98>
  401d26:	68 10 00 00 00       	pushq  $0x10
  401d2b:	e9 e0 fe ff ff       	jmpq   401c10 <_init+0x20>

0000000000401d30 <fcntl@plt>:
  401d30:	ff 25 6a 93 20 00    	jmpq   *0x20936a(%rip)        # 60b0a0 <_GLOBAL_OFFSET_TABLE_+0xa0>
  401d36:	68 11 00 00 00       	pushq  $0x11
  401d3b:	e9 d0 fe ff ff       	jmpq   401c10 <_init+0x20>

0000000000401d40 <getpid@plt>:
  401d40:	ff 25 62 93 20 00    	jmpq   *0x209362(%rip)        # 60b0a8 <_GLOBAL_OFFSET_TABLE_+0xa8>
  401d46:	68 12 00 00 00       	pushq  $0x12
  401d4b:	e9 c0 fe ff ff       	jmpq   401c10 <_init+0x20>

0000000000401d50 <fclose@plt>:
  401d50:	ff 25 5a 93 20 00    	jmpq   *0x20935a(%rip)        # 60b0b0 <_GLOBAL_OFFSET_TABLE_+0xb0>
  401d56:	68 13 00 00 00       	pushq  $0x13
  401d5b:	e9 b0 fe ff ff       	jmpq   401c10 <_init+0x20>

0000000000401d60 <opendir@plt>:
  401d60:	ff 25 52 93 20 00    	jmpq   *0x209352(%rip)        # 60b0b8 <_GLOBAL_OFFSET_TABLE_+0xb8>
  401d66:	68 14 00 00 00       	pushq  $0x14
  401d6b:	e9 a0 fe ff ff       	jmpq   401c10 <_init+0x20>

0000000000401d70 <strlen@plt>:
  401d70:	ff 25 4a 93 20 00    	jmpq   *0x20934a(%rip)        # 60b0c0 <_GLOBAL_OFFSET_TABLE_+0xc0>
  401d76:	68 15 00 00 00       	pushq  $0x15
  401d7b:	e9 90 fe ff ff       	jmpq   401c10 <_init+0x20>

0000000000401d80 <__lxstat@plt>:
  401d80:	ff 25 42 93 20 00    	jmpq   *0x209342(%rip)        # 60b0c8 <_GLOBAL_OFFSET_TABLE_+0xc8>
  401d86:	68 16 00 00 00       	pushq  $0x16
  401d8b:	e9 80 fe ff ff       	jmpq   401c10 <_init+0x20>

0000000000401d90 <chdir@plt>:
  401d90:	ff 25 3a 93 20 00    	jmpq   *0x20933a(%rip)        # 60b0d0 <_GLOBAL_OFFSET_TABLE_+0xd0>
  401d96:	68 17 00 00 00       	pushq  $0x17
  401d9b:	e9 70 fe ff ff       	jmpq   401c10 <_init+0x20>

0000000000401da0 <getuid@plt>:
  401da0:	ff 25 32 93 20 00    	jmpq   *0x209332(%rip)        # 60b0d8 <_GLOBAL_OFFSET_TABLE_+0xd8>
  401da6:	68 18 00 00 00       	pushq  $0x18
  401dab:	e9 60 fe ff ff       	jmpq   401c10 <_init+0x20>

0000000000401db0 <dup2@plt>:
  401db0:	ff 25 2a 93 20 00    	jmpq   *0x20932a(%rip)        # 60b0e0 <_GLOBAL_OFFSET_TABLE_+0xe0>
  401db6:	68 19 00 00 00       	pushq  $0x19
  401dbb:	e9 50 fe ff ff       	jmpq   401c10 <_init+0x20>

0000000000401dc0 <strchr@plt>:
  401dc0:	ff 25 22 93 20 00    	jmpq   *0x209322(%rip)        # 60b0e8 <_GLOBAL_OFFSET_TABLE_+0xe8>
  401dc6:	68 1a 00 00 00       	pushq  $0x1a
  401dcb:	e9 40 fe ff ff       	jmpq   401c10 <_init+0x20>

0000000000401dd0 <rewind@plt>:
  401dd0:	ff 25 1a 93 20 00    	jmpq   *0x20931a(%rip)        # 60b0f0 <_GLOBAL_OFFSET_TABLE_+0xf0>
  401dd6:	68 1b 00 00 00       	pushq  $0x1b
  401ddb:	e9 30 fe ff ff       	jmpq   401c10 <_init+0x20>

0000000000401de0 <_IO_putc@plt>:
  401de0:	ff 25 12 93 20 00    	jmpq   *0x209312(%rip)        # 60b0f8 <_GLOBAL_OFFSET_TABLE_+0xf8>
  401de6:	68 1c 00 00 00       	pushq  $0x1c
  401deb:	e9 20 fe ff ff       	jmpq   401c10 <_init+0x20>

0000000000401df0 <snprintf@plt>:
  401df0:	ff 25 0a 93 20 00    	jmpq   *0x20930a(%rip)        # 60b100 <_GLOBAL_OFFSET_TABLE_+0x100>
  401df6:	68 1d 00 00 00       	pushq  $0x1d
  401dfb:	e9 10 fe ff ff       	jmpq   401c10 <_init+0x20>

0000000000401e00 <ftruncate@plt>:
  401e00:	ff 25 02 93 20 00    	jmpq   *0x209302(%rip)        # 60b108 <_GLOBAL_OFFSET_TABLE_+0x108>
  401e06:	68 1e 00 00 00       	pushq  $0x1e
  401e0b:	e9 00 fe ff ff       	jmpq   401c10 <_init+0x20>

0000000000401e10 <memset@plt>:
  401e10:	ff 25 fa 92 20 00    	jmpq   *0x2092fa(%rip)        # 60b110 <_GLOBAL_OFFSET_TABLE_+0x110>
  401e16:	68 1f 00 00 00       	pushq  $0x1f
  401e1b:	e9 f0 fd ff ff       	jmpq   401c10 <_init+0x20>

0000000000401e20 <geteuid@plt>:
  401e20:	ff 25 f2 92 20 00    	jmpq   *0x2092f2(%rip)        # 60b118 <_GLOBAL_OFFSET_TABLE_+0x118>
  401e26:	68 20 00 00 00       	pushq  $0x20
  401e2b:	e9 e0 fd ff ff       	jmpq   401c10 <_init+0x20>

0000000000401e30 <strnlen@plt>:
  401e30:	ff 25 ea 92 20 00    	jmpq   *0x2092ea(%rip)        # 60b120 <_GLOBAL_OFFSET_TABLE_+0x120>
  401e36:	68 21 00 00 00       	pushq  $0x21
  401e3b:	e9 d0 fd ff ff       	jmpq   401c10 <_init+0x20>

0000000000401e40 <close@plt>:
  401e40:	ff 25 e2 92 20 00    	jmpq   *0x2092e2(%rip)        # 60b128 <_GLOBAL_OFFSET_TABLE_+0x128>
  401e46:	68 22 00 00 00       	pushq  $0x22
  401e4b:	e9 c0 fd ff ff       	jmpq   401c10 <_init+0x20>

0000000000401e50 <pipe@plt>:
  401e50:	ff 25 da 92 20 00    	jmpq   *0x2092da(%rip)        # 60b130 <_GLOBAL_OFFSET_TABLE_+0x130>
  401e56:	68 23 00 00 00       	pushq  $0x23
  401e5b:	e9 b0 fd ff ff       	jmpq   401c10 <_init+0x20>

0000000000401e60 <setsid@plt>:
  401e60:	ff 25 d2 92 20 00    	jmpq   *0x2092d2(%rip)        # 60b138 <_GLOBAL_OFFSET_TABLE_+0x138>
  401e66:	68 24 00 00 00       	pushq  $0x24
  401e6b:	e9 a0 fd ff ff       	jmpq   401c10 <_init+0x20>

0000000000401e70 <strspn@plt>:
  401e70:	ff 25 ca 92 20 00    	jmpq   *0x2092ca(%rip)        # 60b140 <_GLOBAL_OFFSET_TABLE_+0x140>
  401e76:	68 25 00 00 00       	pushq  $0x25
  401e7b:	e9 90 fd ff ff       	jmpq   401c10 <_init+0x20>

0000000000401e80 <closedir@plt>:
  401e80:	ff 25 c2 92 20 00    	jmpq   *0x2092c2(%rip)        # 60b148 <_GLOBAL_OFFSET_TABLE_+0x148>
  401e86:	68 26 00 00 00       	pushq  $0x26
  401e8b:	e9 80 fd ff ff       	jmpq   401c10 <_init+0x20>

0000000000401e90 <regcomp@plt>:
  401e90:	ff 25 ba 92 20 00    	jmpq   *0x2092ba(%rip)        # 60b150 <_GLOBAL_OFFSET_TABLE_+0x150>
  401e96:	68 27 00 00 00       	pushq  $0x27
  401e9b:	e9 70 fd ff ff       	jmpq   401c10 <_init+0x20>

0000000000401ea0 <fputc@plt>:
  401ea0:	ff 25 b2 92 20 00    	jmpq   *0x2092b2(%rip)        # 60b158 <_GLOBAL_OFFSET_TABLE_+0x158>
  401ea6:	68 28 00 00 00       	pushq  $0x28
  401eab:	e9 60 fd ff ff       	jmpq   401c10 <_init+0x20>

0000000000401eb0 <strcspn@plt>:
  401eb0:	ff 25 aa 92 20 00    	jmpq   *0x2092aa(%rip)        # 60b160 <_GLOBAL_OFFSET_TABLE_+0x160>
  401eb6:	68 29 00 00 00       	pushq  $0x29
  401ebb:	e9 50 fd ff ff       	jmpq   401c10 <_init+0x20>

0000000000401ec0 <__libc_start_main@plt>:
  401ec0:	ff 25 a2 92 20 00    	jmpq   *0x2092a2(%rip)        # 60b168 <_GLOBAL_OFFSET_TABLE_+0x168>
  401ec6:	68 2a 00 00 00       	pushq  $0x2a
  401ecb:	e9 40 fd ff ff       	jmpq   401c10 <_init+0x20>

0000000000401ed0 <fgets@plt>:
  401ed0:	ff 25 9a 92 20 00    	jmpq   *0x20929a(%rip)        # 60b170 <_GLOBAL_OFFSET_TABLE_+0x170>
  401ed6:	68 2b 00 00 00       	pushq  $0x2b
  401edb:	e9 30 fd ff ff       	jmpq   401c10 <_init+0x20>

0000000000401ee0 <calloc@plt>:
  401ee0:	ff 25 92 92 20 00    	jmpq   *0x209292(%rip)        # 60b178 <_GLOBAL_OFFSET_TABLE_+0x178>
  401ee6:	68 2c 00 00 00       	pushq  $0x2c
  401eeb:	e9 20 fd ff ff       	jmpq   401c10 <_init+0x20>

0000000000401ef0 <tmpfile@plt>:
  401ef0:	ff 25 8a 92 20 00    	jmpq   *0x20928a(%rip)        # 60b180 <_GLOBAL_OFFSET_TABLE_+0x180>
  401ef6:	68 2d 00 00 00       	pushq  $0x2d
  401efb:	e9 10 fd ff ff       	jmpq   401c10 <_init+0x20>

0000000000401f00 <strcmp@plt>:
  401f00:	ff 25 82 92 20 00    	jmpq   *0x209282(%rip)        # 60b188 <_GLOBAL_OFFSET_TABLE_+0x188>
  401f06:	68 2e 00 00 00       	pushq  $0x2e
  401f0b:	e9 00 fd ff ff       	jmpq   401c10 <_init+0x20>

0000000000401f10 <signal@plt>:
  401f10:	ff 25 7a 92 20 00    	jmpq   *0x20927a(%rip)        # 60b190 <_GLOBAL_OFFSET_TABLE_+0x190>
  401f16:	68 2f 00 00 00       	pushq  $0x2f
  401f1b:	e9 f0 fc ff ff       	jmpq   401c10 <_init+0x20>

0000000000401f20 <getpwnam@plt>:
  401f20:	ff 25 72 92 20 00    	jmpq   *0x209272(%rip)        # 60b198 <_GLOBAL_OFFSET_TABLE_+0x198>
  401f26:	68 30 00 00 00       	pushq  $0x30
  401f2b:	e9 e0 fc ff ff       	jmpq   401c10 <_init+0x20>

0000000000401f30 <fprintf@plt>:
  401f30:	ff 25 6a 92 20 00    	jmpq   *0x20926a(%rip)        # 60b1a0 <_GLOBAL_OFFSET_TABLE_+0x1a0>
  401f36:	68 31 00 00 00       	pushq  $0x31
  401f3b:	e9 d0 fc ff ff       	jmpq   401c10 <_init+0x20>

0000000000401f40 <sigemptyset@plt>:
  401f40:	ff 25 62 92 20 00    	jmpq   *0x209262(%rip)        # 60b1a8 <_GLOBAL_OFFSET_TABLE_+0x1a8>
  401f46:	68 32 00 00 00       	pushq  $0x32
  401f4b:	e9 c0 fc ff ff       	jmpq   401c10 <_init+0x20>

0000000000401f50 <ftell@plt>:
  401f50:	ff 25 5a 92 20 00    	jmpq   *0x20925a(%rip)        # 60b1b0 <_GLOBAL_OFFSET_TABLE_+0x1b0>
  401f56:	68 33 00 00 00       	pushq  $0x33
  401f5b:	e9 b0 fc ff ff       	jmpq   401c10 <_init+0x20>

0000000000401f60 <umask@plt>:
  401f60:	ff 25 52 92 20 00    	jmpq   *0x209252(%rip)        # 60b1b8 <_GLOBAL_OFFSET_TABLE_+0x1b8>
  401f66:	68 34 00 00 00       	pushq  $0x34
  401f6b:	e9 a0 fc ff ff       	jmpq   401c10 <_init+0x20>

0000000000401f70 <getgrnam@plt>:
  401f70:	ff 25 4a 92 20 00    	jmpq   *0x20924a(%rip)        # 60b1c0 <_GLOBAL_OFFSET_TABLE_+0x1c0>
  401f76:	68 35 00 00 00       	pushq  $0x35
  401f7b:	e9 90 fc ff ff       	jmpq   401c10 <_init+0x20>

0000000000401f80 <time@plt>:
  401f80:	ff 25 42 92 20 00    	jmpq   *0x209242(%rip)        # 60b1c8 <_GLOBAL_OFFSET_TABLE_+0x1c8>
  401f86:	68 36 00 00 00       	pushq  $0x36
  401f8b:	e9 80 fc ff ff       	jmpq   401c10 <_init+0x20>

0000000000401f90 <fileno@plt>:
  401f90:	ff 25 3a 92 20 00    	jmpq   *0x20923a(%rip)        # 60b1d0 <_GLOBAL_OFFSET_TABLE_+0x1d0>
  401f96:	68 37 00 00 00       	pushq  $0x37
  401f9b:	e9 70 fc ff ff       	jmpq   401c10 <_init+0x20>

0000000000401fa0 <getgid@plt>:
  401fa0:	ff 25 32 92 20 00    	jmpq   *0x209232(%rip)        # 60b1d8 <_GLOBAL_OFFSET_TABLE_+0x1d8>
  401fa6:	68 38 00 00 00       	pushq  $0x38
  401fab:	e9 60 fc ff ff       	jmpq   401c10 <_init+0x20>

0000000000401fb0 <__xstat@plt>:
  401fb0:	ff 25 2a 92 20 00    	jmpq   *0x20922a(%rip)        # 60b1e0 <_GLOBAL_OFFSET_TABLE_+0x1e0>
  401fb6:	68 39 00 00 00       	pushq  $0x39
  401fbb:	e9 50 fc ff ff       	jmpq   401c10 <_init+0x20>

0000000000401fc0 <readdir@plt>:
  401fc0:	ff 25 22 92 20 00    	jmpq   *0x209222(%rip)        # 60b1e8 <_GLOBAL_OFFSET_TABLE_+0x1e8>
  401fc6:	68 3a 00 00 00       	pushq  $0x3a
  401fcb:	e9 40 fc ff ff       	jmpq   401c10 <_init+0x20>

0000000000401fd0 <malloc@plt>:
  401fd0:	ff 25 1a 92 20 00    	jmpq   *0x20921a(%rip)        # 60b1f0 <_GLOBAL_OFFSET_TABLE_+0x1f0>
  401fd6:	68 3b 00 00 00       	pushq  $0x3b
  401fdb:	e9 30 fc ff ff       	jmpq   401c10 <_init+0x20>

0000000000401fe0 <fflush@plt>:
  401fe0:	ff 25 12 92 20 00    	jmpq   *0x209212(%rip)        # 60b1f8 <_GLOBAL_OFFSET_TABLE_+0x1f8>
  401fe6:	68 3c 00 00 00       	pushq  $0x3c
  401feb:	e9 20 fc ff ff       	jmpq   401c10 <_init+0x20>

0000000000401ff0 <_IO_getc@plt>:
  401ff0:	ff 25 0a 92 20 00    	jmpq   *0x20920a(%rip)        # 60b200 <_GLOBAL_OFFSET_TABLE_+0x200>
  401ff6:	68 3d 00 00 00       	pushq  $0x3d
  401ffb:	e9 10 fc ff ff       	jmpq   401c10 <_init+0x20>

0000000000402000 <ungetc@plt>:
  402000:	ff 25 02 92 20 00    	jmpq   *0x209202(%rip)        # 60b208 <_GLOBAL_OFFSET_TABLE_+0x208>
  402006:	68 3e 00 00 00       	pushq  $0x3e
  40200b:	e9 00 fc ff ff       	jmpq   401c10 <_init+0x20>

0000000000402010 <syslog@plt>:
  402010:	ff 25 fa 91 20 00    	jmpq   *0x2091fa(%rip)        # 60b210 <_GLOBAL_OFFSET_TABLE_+0x210>
  402016:	68 3f 00 00 00       	pushq  $0x3f
  40201b:	e9 f0 fb ff ff       	jmpq   401c10 <_init+0x20>

0000000000402020 <__fxstat@plt>:
  402020:	ff 25 f2 91 20 00    	jmpq   *0x2091f2(%rip)        # 60b218 <_GLOBAL_OFFSET_TABLE_+0x218>
  402026:	68 40 00 00 00       	pushq  $0x40
  40202b:	e9 e0 fb ff ff       	jmpq   401c10 <_init+0x20>

0000000000402030 <endpwent@plt>:
  402030:	ff 25 ea 91 20 00    	jmpq   *0x2091ea(%rip)        # 60b220 <_GLOBAL_OFFSET_TABLE_+0x220>
  402036:	68 41 00 00 00       	pushq  $0x41
  40203b:	e9 d0 fb ff ff       	jmpq   401c10 <_init+0x20>

0000000000402040 <regexec@plt>:
  402040:	ff 25 e2 91 20 00    	jmpq   *0x2091e2(%rip)        # 60b228 <_GLOBAL_OFFSET_TABLE_+0x228>
  402046:	68 42 00 00 00       	pushq  $0x42
  40204b:	e9 c0 fb ff ff       	jmpq   401c10 <_init+0x20>

0000000000402050 <getegid@plt>:
  402050:	ff 25 da 91 20 00    	jmpq   *0x2091da(%rip)        # 60b230 <_GLOBAL_OFFSET_TABLE_+0x230>
  402056:	68 43 00 00 00       	pushq  $0x43
  40205b:	e9 b0 fb ff ff       	jmpq   401c10 <_init+0x20>

0000000000402060 <fseek@plt>:
  402060:	ff 25 d2 91 20 00    	jmpq   *0x2091d2(%rip)        # 60b238 <_GLOBAL_OFFSET_TABLE_+0x238>
  402066:	68 44 00 00 00       	pushq  $0x44
  40206b:	e9 a0 fb ff ff       	jmpq   401c10 <_init+0x20>

0000000000402070 <chown@plt>:
  402070:	ff 25 ca 91 20 00    	jmpq   *0x2091ca(%rip)        # 60b240 <_GLOBAL_OFFSET_TABLE_+0x240>
  402076:	68 45 00 00 00       	pushq  $0x45
  40207b:	e9 90 fb ff ff       	jmpq   401c10 <_init+0x20>

0000000000402080 <realloc@plt>:
  402080:	ff 25 c2 91 20 00    	jmpq   *0x2091c2(%rip)        # 60b248 <_GLOBAL_OFFSET_TABLE_+0x248>
  402086:	68 46 00 00 00       	pushq  $0x46
  40208b:	e9 80 fb ff ff       	jmpq   401c10 <_init+0x20>

0000000000402090 <fdopen@plt>:
  402090:	ff 25 ba 91 20 00    	jmpq   *0x2091ba(%rip)        # 60b250 <_GLOBAL_OFFSET_TABLE_+0x250>
  402096:	68 47 00 00 00       	pushq  $0x47
  40209b:	e9 70 fb ff ff       	jmpq   401c10 <_init+0x20>

00000000004020a0 <setgid@plt>:
  4020a0:	ff 25 b2 91 20 00    	jmpq   *0x2091b2(%rip)        # 60b258 <_GLOBAL_OFFSET_TABLE_+0x258>
  4020a6:	68 48 00 00 00       	pushq  $0x48
  4020ab:	e9 60 fb ff ff       	jmpq   401c10 <_init+0x20>

00000000004020b0 <waitpid@plt>:
  4020b0:	ff 25 aa 91 20 00    	jmpq   *0x2091aa(%rip)        # 60b260 <_GLOBAL_OFFSET_TABLE_+0x260>
  4020b6:	68 49 00 00 00       	pushq  $0x49
  4020bb:	e9 50 fb ff ff       	jmpq   401c10 <_init+0x20>

00000000004020c0 <open@plt>:
  4020c0:	ff 25 a2 91 20 00    	jmpq   *0x2091a2(%rip)        # 60b268 <_GLOBAL_OFFSET_TABLE_+0x268>
  4020c6:	68 4a 00 00 00       	pushq  $0x4a
  4020cb:	e9 40 fb ff ff       	jmpq   401c10 <_init+0x20>

00000000004020d0 <access@plt>:
  4020d0:	ff 25 9a 91 20 00    	jmpq   *0x20919a(%rip)        # 60b270 <_GLOBAL_OFFSET_TABLE_+0x270>
  4020d6:	68 4b 00 00 00       	pushq  $0x4b
  4020db:	e9 30 fb ff ff       	jmpq   401c10 <_init+0x20>

00000000004020e0 <fopen@plt>:
  4020e0:	ff 25 92 91 20 00    	jmpq   *0x209192(%rip)        # 60b278 <_GLOBAL_OFFSET_TABLE_+0x278>
  4020e6:	68 4c 00 00 00       	pushq  $0x4c
  4020eb:	e9 20 fb ff ff       	jmpq   401c10 <_init+0x20>

00000000004020f0 <perror@plt>:
  4020f0:	ff 25 8a 91 20 00    	jmpq   *0x20918a(%rip)        # 60b280 <_GLOBAL_OFFSET_TABLE_+0x280>
  4020f6:	68 4d 00 00 00       	pushq  $0x4d
  4020fb:	e9 10 fb ff ff       	jmpq   401c10 <_init+0x20>

0000000000402100 <strtok@plt>:
  402100:	ff 25 82 91 20 00    	jmpq   *0x209182(%rip)        # 60b288 <_GLOBAL_OFFSET_TABLE_+0x288>
  402106:	68 4e 00 00 00       	pushq  $0x4e
  40210b:	e9 00 fb ff ff       	jmpq   401c10 <_init+0x20>

0000000000402110 <bzero@plt>:
  402110:	ff 25 7a 91 20 00    	jmpq   *0x20917a(%rip)        # 60b290 <_GLOBAL_OFFSET_TABLE_+0x290>
  402116:	68 4f 00 00 00       	pushq  $0x4f
  40211b:	e9 f0 fa ff ff       	jmpq   401c10 <_init+0x20>

0000000000402120 <gmtime@plt>:
  402120:	ff 25 72 91 20 00    	jmpq   *0x209172(%rip)        # 60b298 <_GLOBAL_OFFSET_TABLE_+0x298>
  402126:	68 50 00 00 00       	pushq  $0x50
  40212b:	e9 e0 fa ff ff       	jmpq   401c10 <_init+0x20>

0000000000402130 <getopt@plt>:
  402130:	ff 25 6a 91 20 00    	jmpq   *0x20916a(%rip)        # 60b2a0 <_GLOBAL_OFFSET_TABLE_+0x2a0>
  402136:	68 51 00 00 00       	pushq  $0x51
  40213b:	e9 d0 fa ff ff       	jmpq   401c10 <_init+0x20>

0000000000402140 <execvp@plt>:
  402140:	ff 25 62 91 20 00    	jmpq   *0x209162(%rip)        # 60b2a8 <_GLOBAL_OFFSET_TABLE_+0x2a8>
  402146:	68 52 00 00 00       	pushq  $0x52
  40214b:	e9 c0 fa ff ff       	jmpq   401c10 <_init+0x20>

0000000000402150 <flock@plt>:
  402150:	ff 25 5a 91 20 00    	jmpq   *0x20915a(%rip)        # 60b2b0 <_GLOBAL_OFFSET_TABLE_+0x2b0>
  402156:	68 53 00 00 00       	pushq  $0x53
  40215b:	e9 b0 fa ff ff       	jmpq   401c10 <_init+0x20>

0000000000402160 <atoi@plt>:
  402160:	ff 25 52 91 20 00    	jmpq   *0x209152(%rip)        # 60b2b8 <_GLOBAL_OFFSET_TABLE_+0x2b8>
  402166:	68 54 00 00 00       	pushq  $0x54
  40216b:	e9 a0 fa ff ff       	jmpq   401c10 <_init+0x20>

0000000000402170 <strcat@plt>:
  402170:	ff 25 4a 91 20 00    	jmpq   *0x20914a(%rip)        # 60b2c0 <_GLOBAL_OFFSET_TABLE_+0x2c0>
  402176:	68 55 00 00 00       	pushq  $0x55
  40217b:	e9 90 fa ff ff       	jmpq   401c10 <_init+0x20>

0000000000402180 <openlog@plt>:
  402180:	ff 25 42 91 20 00    	jmpq   *0x209142(%rip)        # 60b2c8 <_GLOBAL_OFFSET_TABLE_+0x2c8>
  402186:	68 56 00 00 00       	pushq  $0x56
  40218b:	e9 80 fa ff ff       	jmpq   401c10 <_init+0x20>

0000000000402190 <creat@plt>:
  402190:	ff 25 3a 91 20 00    	jmpq   *0x20913a(%rip)        # 60b2d0 <_GLOBAL_OFFSET_TABLE_+0x2d0>
  402196:	68 57 00 00 00       	pushq  $0x57
  40219b:	e9 70 fa ff ff       	jmpq   401c10 <_init+0x20>

00000000004021a0 <gethostname@plt>:
  4021a0:	ff 25 32 91 20 00    	jmpq   *0x209132(%rip)        # 60b2d8 <_GLOBAL_OFFSET_TABLE_+0x2d8>
  4021a6:	68 58 00 00 00       	pushq  $0x58
  4021ab:	e9 60 fa ff ff       	jmpq   401c10 <_init+0x20>

00000000004021b0 <sprintf@plt>:
  4021b0:	ff 25 2a 91 20 00    	jmpq   *0x20912a(%rip)        # 60b2e0 <_GLOBAL_OFFSET_TABLE_+0x2e0>
  4021b6:	68 59 00 00 00       	pushq  $0x59
  4021bb:	e9 50 fa ff ff       	jmpq   401c10 <_init+0x20>

00000000004021c0 <exit@plt>:
  4021c0:	ff 25 22 91 20 00    	jmpq   *0x209122(%rip)        # 60b2e8 <_GLOBAL_OFFSET_TABLE_+0x2e8>
  4021c6:	68 5a 00 00 00       	pushq  $0x5a
  4021cb:	e9 40 fa ff ff       	jmpq   401c10 <_init+0x20>

00000000004021d0 <execle@plt>:
  4021d0:	ff 25 1a 91 20 00    	jmpq   *0x20911a(%rip)        # 60b2f0 <_GLOBAL_OFFSET_TABLE_+0x2f0>
  4021d6:	68 5b 00 00 00       	pushq  $0x5b
  4021db:	e9 30 fa ff ff       	jmpq   401c10 <_init+0x20>

00000000004021e0 <fwrite@plt>:
  4021e0:	ff 25 12 91 20 00    	jmpq   *0x209112(%rip)        # 60b2f8 <_GLOBAL_OFFSET_TABLE_+0x2f8>
  4021e6:	68 5c 00 00 00       	pushq  $0x5c
  4021eb:	e9 20 fa ff ff       	jmpq   401c10 <_init+0x20>

00000000004021f0 <setuid@plt>:
  4021f0:	ff 25 0a 91 20 00    	jmpq   *0x20910a(%rip)        # 60b300 <_GLOBAL_OFFSET_TABLE_+0x300>
  4021f6:	68 5d 00 00 00       	pushq  $0x5d
  4021fb:	e9 10 fa ff ff       	jmpq   401c10 <_init+0x20>

0000000000402200 <strdup@plt>:
  402200:	ff 25 02 91 20 00    	jmpq   *0x209102(%rip)        # 60b308 <_GLOBAL_OFFSET_TABLE_+0x308>
  402206:	68 5e 00 00 00       	pushq  $0x5e
  40220b:	e9 00 fa ff ff       	jmpq   401c10 <_init+0x20>

0000000000402210 <strerror@plt>:
  402210:	ff 25 fa 90 20 00    	jmpq   *0x2090fa(%rip)        # 60b310 <_GLOBAL_OFFSET_TABLE_+0x310>
  402216:	68 5f 00 00 00       	pushq  $0x5f
  40221b:	e9 f0 f9 ff ff       	jmpq   401c10 <_init+0x20>

0000000000402220 <initgroups@plt>:
  402220:	ff 25 f2 90 20 00    	jmpq   *0x2090f2(%rip)        # 60b318 <_GLOBAL_OFFSET_TABLE_+0x318>
  402226:	68 60 00 00 00       	pushq  $0x60
  40222b:	e9 e0 f9 ff ff       	jmpq   401c10 <_init+0x20>

0000000000402230 <sleep@plt>:
  402230:	ff 25 ea 90 20 00    	jmpq   *0x2090ea(%rip)        # 60b320 <_GLOBAL_OFFSET_TABLE_+0x320>
  402236:	68 61 00 00 00       	pushq  $0x61
  40223b:	e9 d0 f9 ff ff       	jmpq   401c10 <_init+0x20>

0000000000402240 <wait@plt>:
  402240:	ff 25 e2 90 20 00    	jmpq   *0x2090e2(%rip)        # 60b328 <_GLOBAL_OFFSET_TABLE_+0x328>
  402246:	68 62 00 00 00       	pushq  $0x62
  40224b:	e9 c0 f9 ff ff       	jmpq   401c10 <_init+0x20>

0000000000402250 <sigaddset@plt>:
  402250:	ff 25 da 90 20 00    	jmpq   *0x2090da(%rip)        # 60b330 <_GLOBAL_OFFSET_TABLE_+0x330>
  402256:	68 63 00 00 00       	pushq  $0x63
  40225b:	e9 b0 f9 ff ff       	jmpq   401c10 <_init+0x20>

0000000000402260 <setegid@plt>:
  402260:	ff 25 d2 90 20 00    	jmpq   *0x2090d2(%rip)        # 60b338 <_GLOBAL_OFFSET_TABLE_+0x338>
  402266:	68 64 00 00 00       	pushq  $0x64
  40226b:	e9 a0 f9 ff ff       	jmpq   401c10 <_init+0x20>

0000000000402270 <fork@plt>:
  402270:	ff 25 ca 90 20 00    	jmpq   *0x2090ca(%rip)        # 60b340 <_GLOBAL_OFFSET_TABLE_+0x340>
  402276:	68 65 00 00 00       	pushq  $0x65
  40227b:	e9 90 f9 ff ff       	jmpq   401c10 <_init+0x20>

0000000000402280 <strstr@plt>:
  402280:	ff 25 c2 90 20 00    	jmpq   *0x2090c2(%rip)        # 60b348 <_GLOBAL_OFFSET_TABLE_+0x348>
  402286:	68 66 00 00 00       	pushq  $0x66
  40228b:	e9 80 f9 ff ff       	jmpq   401c10 <_init+0x20>

0000000000402290 <getdtablesize@plt>:
  402290:	ff 25 ba 90 20 00    	jmpq   *0x2090ba(%rip)        # 60b350 <_GLOBAL_OFFSET_TABLE_+0x350>
  402296:	68 67 00 00 00       	pushq  $0x67
  40229b:	e9 70 f9 ff ff       	jmpq   401c10 <_init+0x20>

00000000004022a0 <__ctype_b_loc@plt>:
  4022a0:	ff 25 b2 90 20 00    	jmpq   *0x2090b2(%rip)        # 60b358 <_GLOBAL_OFFSET_TABLE_+0x358>
  4022a6:	68 68 00 00 00       	pushq  $0x68
  4022ab:	e9 60 f9 ff ff       	jmpq   401c10 <_init+0x20>

00000000004022b0 <seteuid@plt>:
  4022b0:	ff 25 aa 90 20 00    	jmpq   *0x2090aa(%rip)        # 60b360 <_GLOBAL_OFFSET_TABLE_+0x360>
  4022b6:	68 69 00 00 00       	pushq  $0x69
  4022bb:	e9 50 f9 ff ff       	jmpq   401c10 <_init+0x20>

Disassembly of section .plt.got:

00000000004022c0 <.plt.got>:
  4022c0:	ff 25 32 8d 20 00    	jmpq   *0x208d32(%rip)        # 60aff8 <_DYNAMIC+0x1d0>
  4022c6:	66 90                	xchg   %ax,%ax

Disassembly of section .text:

00000000004022d0 <_start>:
  4022d0:	31 ed                	xor    %ebp,%ebp
  4022d2:	49 89 d1             	mov    %rdx,%r9
  4022d5:	5e                   	pop    %rsi
  4022d6:	48 89 e2             	mov    %rsp,%rdx
  4022d9:	48 83 e4 f0          	and    $0xfffffffffffffff0,%rsp
  4022dd:	50                   	push   %rax
  4022de:	54                   	push   %rsp
  4022df:	49 c7 c0 f0 87 40 00 	mov    $0x4087f0,%r8
  4022e6:	48 c7 c1 80 87 40 00 	mov    $0x408780,%rcx
  4022ed:	48 c7 c7 f4 23 40 00 	mov    $0x4023f4,%rdi
  4022f4:	e8 c7 fb ff ff       	callq  401ec0 <__libc_start_main@plt>
  4022f9:	f4                   	hlt    
  4022fa:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000402300 <deregister_tm_clones>:
  402300:	b8 bf b7 60 00       	mov    $0x60b7bf,%eax
  402305:	55                   	push   %rbp
  402306:	48 2d b8 b7 60 00    	sub    $0x60b7b8,%rax
  40230c:	48 83 f8 0e          	cmp    $0xe,%rax
  402310:	48 89 e5             	mov    %rsp,%rbp
  402313:	76 1b                	jbe    402330 <deregister_tm_clones+0x30>
  402315:	b8 00 00 00 00       	mov    $0x0,%eax
  40231a:	48 85 c0             	test   %rax,%rax
  40231d:	74 11                	je     402330 <deregister_tm_clones+0x30>
  40231f:	5d                   	pop    %rbp
  402320:	bf b8 b7 60 00       	mov    $0x60b7b8,%edi
  402325:	ff e0                	jmpq   *%rax
  402327:	66 0f 1f 84 00 00 00 	nopw   0x0(%rax,%rax,1)
  40232e:	00 00 
  402330:	5d                   	pop    %rbp
  402331:	c3                   	retq   
  402332:	0f 1f 40 00          	nopl   0x0(%rax)
  402336:	66 2e 0f 1f 84 00 00 	nopw   %cs:0x0(%rax,%rax,1)
  40233d:	00 00 00 

0000000000402340 <register_tm_clones>:
  402340:	be b8 b7 60 00       	mov    $0x60b7b8,%esi
  402345:	55                   	push   %rbp
  402346:	48 81 ee b8 b7 60 00 	sub    $0x60b7b8,%rsi
  40234d:	48 c1 fe 03          	sar    $0x3,%rsi
  402351:	48 89 e5             	mov    %rsp,%rbp
  402354:	48 89 f0             	mov    %rsi,%rax
  402357:	48 c1 e8 3f          	shr    $0x3f,%rax
  40235b:	48 01 c6             	add    %rax,%rsi
  40235e:	48 d1 fe             	sar    %rsi
  402361:	74 15                	je     402378 <register_tm_clones+0x38>
  402363:	b8 00 00 00 00       	mov    $0x0,%eax
  402368:	48 85 c0             	test   %rax,%rax
  40236b:	74 0b                	je     402378 <register_tm_clones+0x38>
  40236d:	5d                   	pop    %rbp
  40236e:	bf b8 b7 60 00       	mov    $0x60b7b8,%edi
  402373:	ff e0                	jmpq   *%rax
  402375:	0f 1f 00             	nopl   (%rax)
  402378:	5d                   	pop    %rbp
  402379:	c3                   	retq   
  40237a:	66 0f 1f 44 00 00    	nopw   0x0(%rax,%rax,1)

0000000000402380 <__do_global_dtors_aux>:
  402380:	80 3d 61 94 20 00 00 	cmpb   $0x0,0x209461(%rip)        # 60b7e8 <completed.7594>
  402387:	75 11                	jne    40239a <__do_global_dtors_aux+0x1a>
  402389:	55                   	push   %rbp
  40238a:	48 89 e5             	mov    %rsp,%rbp
  40238d:	e8 6e ff ff ff       	callq  402300 <deregister_tm_clones>
  402392:	5d                   	pop    %rbp
  402393:	c6 05 4e 94 20 00 01 	movb   $0x1,0x20944e(%rip)        # 60b7e8 <completed.7594>
  40239a:	f3 c3                	repz retq 
  40239c:	0f 1f 40 00          	nopl   0x0(%rax)

00000000004023a0 <frame_dummy>:
  4023a0:	bf 20 ae 60 00       	mov    $0x60ae20,%edi
  4023a5:	48 83 3f 00          	cmpq   $0x0,(%rdi)
  4023a9:	75 05                	jne    4023b0 <frame_dummy+0x10>
  4023ab:	eb 93                	jmp    402340 <register_tm_clones>
  4023ad:	0f 1f 00             	nopl   (%rax)
  4023b0:	b8 00 00 00 00       	mov    $0x0,%eax
  4023b5:	48 85 c0             	test   %rax,%rax
  4023b8:	74 f1                	je     4023ab <frame_dummy+0xb>
  4023ba:	55                   	push   %rbp
  4023bb:	48 89 e5             	mov    %rsp,%rbp
  4023be:	ff d0                	callq  *%rax
  4023c0:	5d                   	pop    %rbp
  4023c1:	e9 7a ff ff ff       	jmpq   402340 <register_tm_clones>

00000000004023c6 <usage>:
  4023c6:	55                   	push   %rbp
  4023c7:	48 89 e5             	mov    %rsp,%rbp
  4023ca:	48 8b 15 b7 a1 20 00 	mov    0x20a1b7(%rip),%rdx        # 60c588 <ProgramName>
  4023d1:	48 8b 05 08 94 20 00 	mov    0x209408(%rip),%rax        # 60b7e0 <stderr@@GLIBC_2.2.5>
  4023d8:	be e3 88 40 00       	mov    $0x4088e3,%esi
  4023dd:	48 89 c7             	mov    %rax,%rdi
  4023e0:	b8 00 00 00 00       	mov    $0x0,%eax
  4023e5:	e8 46 fb ff ff       	callq  401f30 <fprintf@plt>
  4023ea:	bf 01 00 00 00       	mov    $0x1,%edi
  4023ef:	e8 cc fd ff ff       	callq  4021c0 <exit@plt>

00000000004023f4 <main>:
  4023f4:	55                   	push   %rbp
  4023f5:	48 89 e5             	mov    %rsp,%rbp
  4023f8:	48 83 ec 10          	sub    $0x10,%rsp
  4023fc:	89 7d fc             	mov    %edi,-0x4(%rbp)
  4023ff:	48 89 75 f0          	mov    %rsi,-0x10(%rbp)
  402403:	bf ee 88 40 00       	mov    $0x4088ee,%edi
  402408:	e8 f3 f8 ff ff       	callq  401d00 <puts@plt>
  40240d:	b8 00 00 00 00       	mov    $0x0,%eax
  402412:	c9                   	leaveq 
  402413:	c3                   	retq   

0000000000402414 <run_reboot_jobs>:
  402414:	55                   	push   %rbp
  402415:	48 89 e5             	mov    %rsp,%rbp
  402418:	41 54                	push   %r12
  40241a:	53                   	push   %rbx
  40241b:	48 83 ec 20          	sub    $0x20,%rsp
  40241f:	48 89 7d d8          	mov    %rdi,-0x28(%rbp)
  402423:	be 00 00 00 00       	mov    $0x0,%esi
  402428:	bf fc 88 40 00       	mov    $0x4088fc,%edi
  40242d:	e8 9e fc ff ff       	callq  4020d0 <access@plt>
  402432:	85 c0                	test   %eax,%eax
  402434:	75 20                	jne    402456 <run_reboot_jobs+0x42>
  402436:	e8 05 f9 ff ff       	callq  401d40 <getpid@plt>
  40243b:	b9 18 89 40 00       	mov    $0x408918,%ecx
  402440:	ba 44 89 40 00       	mov    $0x408944,%edx
  402445:	89 c6                	mov    %eax,%esi
  402447:	bf 49 89 40 00       	mov    $0x408949,%edi
  40244c:	e8 88 50 00 00       	callq  4074d9 <log_it>
  402451:	e9 9e 00 00 00       	jmpq   4024f4 <run_reboot_jobs+0xe0>
  402456:	be 00 00 00 00       	mov    $0x0,%esi
  40245b:	bf fc 88 40 00       	mov    $0x4088fc,%edi
  402460:	e8 2b fd ff ff       	callq  402190 <creat@plt>
  402465:	89 45 ec             	mov    %eax,-0x14(%rbp)
  402468:	83 7d ec 00          	cmpl   $0x0,-0x14(%rbp)
  40246c:	79 25                	jns    402493 <run_reboot_jobs+0x7f>
  40246e:	e8 cd f8 ff ff       	callq  401d40 <getpid@plt>
  402473:	b9 50 89 40 00       	mov    $0x408950,%ecx
  402478:	ba 6f 89 40 00       	mov    $0x40896f,%edx
  40247d:	89 c6                	mov    %eax,%esi
  40247f:	bf 49 89 40 00       	mov    $0x408949,%edi
  402484:	e8 50 50 00 00       	callq  4074d9 <log_it>
  402489:	bf 00 00 00 00       	mov    $0x0,%edi
  40248e:	e8 2d fd ff ff       	callq  4021c0 <exit@plt>
  402493:	8b 45 ec             	mov    -0x14(%rbp),%eax
  402496:	89 c7                	mov    %eax,%edi
  402498:	e8 a3 f9 ff ff       	callq  401e40 <close@plt>
  40249d:	e8 9e f8 ff ff       	callq  401d40 <getpid@plt>
  4024a2:	b9 75 89 40 00       	mov    $0x408975,%ecx
  4024a7:	ba 44 89 40 00       	mov    $0x408944,%edx
  4024ac:	89 c6                	mov    %eax,%esi
  4024ae:	bf 49 89 40 00       	mov    $0x408949,%edi
  4024b3:	e8 21 50 00 00       	callq  4074d9 <log_it>
  4024b8:	48 8b 45 d8          	mov    -0x28(%rbp),%rax
  4024bc:	48 8b 18             	mov    (%rax),%rbx
  4024bf:	eb 29                	jmp    4024ea <run_reboot_jobs+0xd6>
  4024c1:	4c 8b 63 20          	mov    0x20(%rbx),%r12
  4024c5:	eb 1b                	jmp    4024e2 <run_reboot_jobs+0xce>
  4024c7:	41 8b 44 24 34       	mov    0x34(%r12),%eax
  4024cc:	83 e0 04             	and    $0x4,%eax
  4024cf:	85 c0                	test   %eax,%eax
  4024d1:	74 0b                	je     4024de <run_reboot_jobs+0xca>
  4024d3:	48 89 de             	mov    %rbx,%rsi
  4024d6:	4c 89 e7             	mov    %r12,%rdi
  4024d9:	e8 72 32 00 00       	callq  405750 <job_add>
  4024de:	4d 8b 24 24          	mov    (%r12),%r12
  4024e2:	4d 85 e4             	test   %r12,%r12
  4024e5:	75 e0                	jne    4024c7 <run_reboot_jobs+0xb3>
  4024e7:	48 8b 1b             	mov    (%rbx),%rbx
  4024ea:	48 85 db             	test   %rbx,%rbx
  4024ed:	75 d2                	jne    4024c1 <run_reboot_jobs+0xad>
  4024ef:	e8 ea 32 00 00       	callq  4057de <job_runqueue>
  4024f4:	48 83 c4 20          	add    $0x20,%rsp
  4024f8:	5b                   	pop    %rbx
  4024f9:	41 5c                	pop    %r12
  4024fb:	5d                   	pop    %rbp
  4024fc:	c3                   	retq   

00000000004024fd <find_jobs>:
  4024fd:	55                   	push   %rbp
  4024fe:	48 89 e5             	mov    %rsp,%rbp
  402501:	41 57                	push   %r15
  402503:	41 56                	push   %r14
  402505:	41 55                	push   %r13
  402507:	41 54                	push   %r12
  402509:	53                   	push   %rbx
  40250a:	48 83 ec 38          	sub    $0x38,%rsp
  40250e:	89 7d bc             	mov    %edi,-0x44(%rbp)
  402511:	48 89 75 b0          	mov    %rsi,-0x50(%rbp)
  402515:	89 55 b8             	mov    %edx,-0x48(%rbp)
  402518:	89 4d ac             	mov    %ecx,-0x54(%rbp)
  40251b:	8b 45 bc             	mov    -0x44(%rbp),%eax
  40251e:	c1 e0 02             	shl    $0x2,%eax
  402521:	89 c2                	mov    %eax,%edx
  402523:	c1 e2 04             	shl    $0x4,%edx
  402526:	29 c2                	sub    %eax,%edx
  402528:	89 d0                	mov    %edx,%eax
  40252a:	48 98                	cltq   
  40252c:	48 89 45 c8          	mov    %rax,-0x38(%rbp)
  402530:	48 8d 45 c8          	lea    -0x38(%rbp),%rax
  402534:	48 89 c7             	mov    %rax,%rdi
  402537:	e8 e4 fb ff ff       	callq  402120 <gmtime@plt>
  40253c:	48 89 c3             	mov    %rax,%rbx
  40253f:	44 8b 7b 04          	mov    0x4(%rbx),%r15d
  402543:	8b 43 08             	mov    0x8(%rbx),%eax
  402546:	89 45 a8             	mov    %eax,-0x58(%rbp)
  402549:	8b 43 0c             	mov    0xc(%rbx),%eax
  40254c:	44 8d 70 ff          	lea    -0x1(%rax),%r14d
  402550:	8b 43 10             	mov    0x10(%rbx),%eax
  402553:	89 45 a4             	mov    %eax,-0x5c(%rbp)
  402556:	44 8b 6b 18          	mov    0x18(%rbx),%r13d
  40255a:	48 8b 45 b0          	mov    -0x50(%rbp),%rax
  40255e:	4c 8b 20             	mov    (%rax),%r12
  402561:	e9 80 01 00 00       	jmpq   4026e6 <find_jobs+0x1e9>
  402566:	49 8b 5c 24 20       	mov    0x20(%r12),%rbx
  40256b:	e9 69 01 00 00       	jmpq   4026d9 <find_jobs+0x1dc>
  402570:	44 89 f8             	mov    %r15d,%eax
  402573:	c1 f8 03             	sar    $0x3,%eax
  402576:	48 98                	cltq   
  402578:	0f b6 44 03 20       	movzbl 0x20(%rbx,%rax,1),%eax
  40257d:	0f b6 d0             	movzbl %al,%edx
  402580:	44 89 f8             	mov    %r15d,%eax
  402583:	83 e0 07             	and    $0x7,%eax
  402586:	89 c1                	mov    %eax,%ecx
  402588:	d3 fa                	sar    %cl,%edx
  40258a:	89 d0                	mov    %edx,%eax
  40258c:	83 e0 01             	and    $0x1,%eax
  40258f:	85 c0                	test   %eax,%eax
  402591:	0f 84 3f 01 00 00    	je     4026d6 <find_jobs+0x1d9>
  402597:	8b 75 a8             	mov    -0x58(%rbp),%esi
  40259a:	89 f0                	mov    %esi,%eax
  40259c:	c1 f8 03             	sar    $0x3,%eax
  40259f:	48 98                	cltq   
  4025a1:	0f b6 44 03 28       	movzbl 0x28(%rbx,%rax,1),%eax
  4025a6:	0f b6 d0             	movzbl %al,%edx
  4025a9:	89 f0                	mov    %esi,%eax
  4025ab:	83 e0 07             	and    $0x7,%eax
  4025ae:	89 c1                	mov    %eax,%ecx
  4025b0:	d3 fa                	sar    %cl,%edx
  4025b2:	89 d0                	mov    %edx,%eax
  4025b4:	83 e0 01             	and    $0x1,%eax
  4025b7:	85 c0                	test   %eax,%eax
  4025b9:	0f 84 17 01 00 00    	je     4026d6 <find_jobs+0x1d9>
  4025bf:	8b 7d a4             	mov    -0x5c(%rbp),%edi
  4025c2:	89 f8                	mov    %edi,%eax
  4025c4:	c1 f8 03             	sar    $0x3,%eax
  4025c7:	48 98                	cltq   
  4025c9:	0f b6 44 03 2f       	movzbl 0x2f(%rbx,%rax,1),%eax
  4025ce:	0f b6 d0             	movzbl %al,%edx
  4025d1:	89 f8                	mov    %edi,%eax
  4025d3:	83 e0 07             	and    $0x7,%eax
  4025d6:	89 c1                	mov    %eax,%ecx
  4025d8:	d3 fa                	sar    %cl,%edx
  4025da:	89 d0                	mov    %edx,%eax
  4025dc:	83 e0 01             	and    $0x1,%eax
  4025df:	85 c0                	test   %eax,%eax
  4025e1:	0f 84 ef 00 00 00    	je     4026d6 <find_jobs+0x1d9>
  4025e7:	8b 43 34             	mov    0x34(%rbx),%eax
  4025ea:	83 e0 01             	and    $0x1,%eax
  4025ed:	85 c0                	test   %eax,%eax
  4025ef:	75 0a                	jne    4025fb <find_jobs+0xfe>
  4025f1:	8b 43 34             	mov    0x34(%rbx),%eax
  4025f4:	83 e0 02             	and    $0x2,%eax
  4025f7:	85 c0                	test   %eax,%eax
  4025f9:	74 57                	je     402652 <find_jobs+0x155>
  4025fb:	44 89 e8             	mov    %r13d,%eax
  4025fe:	c1 f8 03             	sar    $0x3,%eax
  402601:	48 98                	cltq   
  402603:	0f b6 44 03 31       	movzbl 0x31(%rbx,%rax,1),%eax
  402608:	0f b6 d0             	movzbl %al,%edx
  40260b:	44 89 e8             	mov    %r13d,%eax
  40260e:	83 e0 07             	and    $0x7,%eax
  402611:	89 c1                	mov    %eax,%ecx
  402613:	d3 fa                	sar    %cl,%edx
  402615:	89 d0                	mov    %edx,%eax
  402617:	83 e0 01             	and    $0x1,%eax
  40261a:	85 c0                	test   %eax,%eax
  40261c:	74 2a                	je     402648 <find_jobs+0x14b>
  40261e:	44 89 f0             	mov    %r14d,%eax
  402621:	c1 f8 03             	sar    $0x3,%eax
  402624:	48 98                	cltq   
  402626:	0f b6 44 03 2b       	movzbl 0x2b(%rbx,%rax,1),%eax
  40262b:	0f b6 d0             	movzbl %al,%edx
  40262e:	44 89 f0             	mov    %r14d,%eax
  402631:	83 e0 07             	and    $0x7,%eax
  402634:	89 c1                	mov    %eax,%ecx
  402636:	d3 fa                	sar    %cl,%edx
  402638:	89 d0                	mov    %edx,%eax
  40263a:	83 e0 01             	and    $0x1,%eax
  40263d:	85 c0                	test   %eax,%eax
  40263f:	74 07                	je     402648 <find_jobs+0x14b>
  402641:	b8 01 00 00 00       	mov    $0x1,%eax
  402646:	eb 05                	jmp    40264d <find_jobs+0x150>
  402648:	b8 00 00 00 00       	mov    $0x0,%eax
  40264d:	83 e0 01             	and    $0x1,%eax
  402650:	eb 55                	jmp    4026a7 <find_jobs+0x1aa>
  402652:	44 89 e8             	mov    %r13d,%eax
  402655:	c1 f8 03             	sar    $0x3,%eax
  402658:	48 98                	cltq   
  40265a:	0f b6 44 03 31       	movzbl 0x31(%rbx,%rax,1),%eax
  40265f:	0f b6 d0             	movzbl %al,%edx
  402662:	44 89 e8             	mov    %r13d,%eax
  402665:	83 e0 07             	and    $0x7,%eax
  402668:	89 c1                	mov    %eax,%ecx
  40266a:	d3 fa                	sar    %cl,%edx
  40266c:	89 d0                	mov    %edx,%eax
  40266e:	83 e0 01             	and    $0x1,%eax
  402671:	85 c0                	test   %eax,%eax
  402673:	75 23                	jne    402698 <find_jobs+0x19b>
  402675:	44 89 f0             	mov    %r14d,%eax
  402678:	c1 f8 03             	sar    $0x3,%eax
  40267b:	48 98                	cltq   
  40267d:	0f b6 44 03 2b       	movzbl 0x2b(%rbx,%rax,1),%eax
  402682:	0f b6 d0             	movzbl %al,%edx
  402685:	44 89 f0             	mov    %r14d,%eax
  402688:	83 e0 07             	and    $0x7,%eax
  40268b:	89 c1                	mov    %eax,%ecx
  40268d:	d3 fa                	sar    %cl,%edx
  40268f:	89 d0                	mov    %edx,%eax
  402691:	83 e0 01             	and    $0x1,%eax
  402694:	85 c0                	test   %eax,%eax
  402696:	74 07                	je     40269f <find_jobs+0x1a2>
  402698:	b8 01 00 00 00       	mov    $0x1,%eax
  40269d:	eb 05                	jmp    4026a4 <find_jobs+0x1a7>
  40269f:	b8 00 00 00 00       	mov    $0x0,%eax
  4026a4:	83 e0 01             	and    $0x1,%eax
  4026a7:	84 c0                	test   %al,%al
  4026a9:	74 2b                	je     4026d6 <find_jobs+0x1d9>
  4026ab:	83 7d ac 00          	cmpl   $0x0,-0x54(%rbp)
  4026af:	74 0a                	je     4026bb <find_jobs+0x1be>
  4026b1:	8b 43 34             	mov    0x34(%rbx),%eax
  4026b4:	83 e0 18             	and    $0x18,%eax
  4026b7:	85 c0                	test   %eax,%eax
  4026b9:	74 10                	je     4026cb <find_jobs+0x1ce>
  4026bb:	83 7d b8 00          	cmpl   $0x0,-0x48(%rbp)
  4026bf:	74 15                	je     4026d6 <find_jobs+0x1d9>
  4026c1:	8b 43 34             	mov    0x34(%rbx),%eax
  4026c4:	83 e0 18             	and    $0x18,%eax
  4026c7:	85 c0                	test   %eax,%eax
  4026c9:	74 0b                	je     4026d6 <find_jobs+0x1d9>
  4026cb:	4c 89 e6             	mov    %r12,%rsi
  4026ce:	48 89 df             	mov    %rbx,%rdi
  4026d1:	e8 7a 30 00 00       	callq  405750 <job_add>
  4026d6:	48 8b 1b             	mov    (%rbx),%rbx
  4026d9:	48 85 db             	test   %rbx,%rbx
  4026dc:	0f 85 8e fe ff ff    	jne    402570 <find_jobs+0x73>
  4026e2:	4d 8b 24 24          	mov    (%r12),%r12
  4026e6:	4d 85 e4             	test   %r12,%r12
  4026e9:	0f 85 77 fe ff ff    	jne    402566 <find_jobs+0x69>
  4026ef:	90                   	nop
  4026f0:	48 83 c4 38          	add    $0x38,%rsp
  4026f4:	5b                   	pop    %rbx
  4026f5:	41 5c                	pop    %r12
  4026f7:	41 5d                	pop    %r13
  4026f9:	41 5e                	pop    %r14
  4026fb:	41 5f                	pop    %r15
  4026fd:	5d                   	pop    %rbp
  4026fe:	c3                   	retq   

00000000004026ff <set_time>:
  4026ff:	55                   	push   %rbp
  402700:	48 89 e5             	mov    %rsp,%rbp
  402703:	48 83 ec 50          	sub    $0x50,%rsp
  402707:	89 7d bc             	mov    %edi,-0x44(%rbp)
  40270a:	bf 00 00 00 00       	mov    $0x0,%edi
  40270f:	e8 6c f8 ff ff       	callq  401f80 <time@plt>
  402714:	48 89 05 5d 9e 20 00 	mov    %rax,0x209e5d(%rip)        # 60c578 <StartTime>
  40271b:	bf 78 c5 60 00       	mov    $0x60c578,%edi
  402720:	e8 3b f5 ff ff       	callq  401c60 <localtime@plt>
  402725:	48 8b 10             	mov    (%rax),%rdx
  402728:	48 89 55 c0          	mov    %rdx,-0x40(%rbp)
  40272c:	48 8b 50 08          	mov    0x8(%rax),%rdx
  402730:	48 89 55 c8          	mov    %rdx,-0x38(%rbp)
  402734:	48 8b 50 10          	mov    0x10(%rax),%rdx
  402738:	48 89 55 d0          	mov    %rdx,-0x30(%rbp)
  40273c:	48 8b 50 18          	mov    0x18(%rax),%rdx
  402740:	48 89 55 d8          	mov    %rdx,-0x28(%rbp)
  402744:	48 8b 50 20          	mov    0x20(%rax),%rdx
  402748:	48 89 55 e0          	mov    %rdx,-0x20(%rbp)
  40274c:	48 8b 50 28          	mov    0x28(%rax),%rdx
  402750:	48 89 55 e8          	mov    %rdx,-0x18(%rbp)
  402754:	48 8b 40 30          	mov    0x30(%rax),%rax
  402758:	48 89 45 f0          	mov    %rax,-0x10(%rbp)
  40275c:	83 7d bc 00          	cmpl   $0x0,-0x44(%rbp)
  402760:	75 0d                	jne    40276f <set_time+0x70>
  402762:	8b 55 e0             	mov    -0x20(%rbp),%edx
  402765:	8b 05 85 94 20 00    	mov    0x209485(%rip),%eax        # 60bbf0 <isdst.5020>
  40276b:	39 c2                	cmp    %eax,%edx
  40276d:	74 21                	je     402790 <set_time+0x91>
  40276f:	8b 45 e0             	mov    -0x20(%rbp),%eax
  402772:	89 05 78 94 20 00    	mov    %eax,0x209478(%rip)        # 60bbf0 <isdst.5020>
  402778:	48 8d 45 c0          	lea    -0x40(%rbp),%rax
  40277c:	48 89 c6             	mov    %rax,%rsi
  40277f:	bf 78 c5 60 00       	mov    $0x60c578,%edi
  402784:	e8 e7 4f 00 00       	callq  407770 <get_gmtoff>
  402789:	48 89 05 58 94 20 00 	mov    %rax,0x209458(%rip)        # 60bbe8 <GMToff>
  402790:	48 8b 15 e1 9d 20 00 	mov    0x209de1(%rip),%rdx        # 60c578 <StartTime>
  402797:	48 8b 05 4a 94 20 00 	mov    0x20944a(%rip),%rax        # 60bbe8 <GMToff>
  40279e:	48 8d 0c 02          	lea    (%rdx,%rax,1),%rcx
  4027a2:	48 ba 89 88 88 88 88 	movabs $0x8888888888888889,%rdx
  4027a9:	88 88 88 
  4027ac:	48 89 c8             	mov    %rcx,%rax
  4027af:	48 f7 ea             	imul   %rdx
  4027b2:	48 8d 04 0a          	lea    (%rdx,%rcx,1),%rax
  4027b6:	48 c1 f8 05          	sar    $0x5,%rax
  4027ba:	48 89 c2             	mov    %rax,%rdx
  4027bd:	48 89 c8             	mov    %rcx,%rax
  4027c0:	48 c1 f8 3f          	sar    $0x3f,%rax
  4027c4:	48 29 c2             	sub    %rax,%rdx
  4027c7:	48 89 d0             	mov    %rdx,%rax
  4027ca:	89 05 a0 9d 20 00    	mov    %eax,0x209da0(%rip)        # 60c570 <clockTime>
  4027d0:	90                   	nop
  4027d1:	c9                   	leaveq 
  4027d2:	c3                   	retq   

00000000004027d3 <cron_sleep>:
  4027d3:	55                   	push   %rbp
  4027d4:	48 89 e5             	mov    %rsp,%rbp
  4027d7:	48 83 ec 20          	sub    $0x20,%rsp
  4027db:	89 7d ec             	mov    %edi,-0x14(%rbp)
  4027de:	bf 00 00 00 00       	mov    $0x0,%edi
  4027e3:	e8 98 f7 ff ff       	callq  401f80 <time@plt>
  4027e8:	48 89 c2             	mov    %rax,%rdx
  4027eb:	48 8b 05 f6 93 20 00 	mov    0x2093f6(%rip),%rax        # 60bbe8 <GMToff>
  4027f2:	48 01 d0             	add    %rdx,%rax
  4027f5:	48 89 45 f8          	mov    %rax,-0x8(%rbp)
  4027f9:	8b 45 ec             	mov    -0x14(%rbp),%eax
  4027fc:	c1 e0 02             	shl    $0x2,%eax
  4027ff:	89 c2                	mov    %eax,%edx
  402801:	c1 e2 04             	shl    $0x4,%edx
  402804:	29 c2                	sub    %eax,%edx
  402806:	89 d0                	mov    %edx,%eax
  402808:	89 c2                	mov    %eax,%edx
  40280a:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  40280e:	29 c2                	sub    %eax,%edx
  402810:	89 d0                	mov    %edx,%eax
  402812:	83 c0 01             	add    $0x1,%eax
  402815:	89 45 f4             	mov    %eax,-0xc(%rbp)
  402818:	83 7d f4 00          	cmpl   $0x0,-0xc(%rbp)
  40281c:	7e 10                	jle    40282e <cron_sleep+0x5b>
  40281e:	83 7d f4 40          	cmpl   $0x40,-0xc(%rbp)
  402822:	7f 0a                	jg     40282e <cron_sleep+0x5b>
  402824:	8b 45 f4             	mov    -0xc(%rbp),%eax
  402827:	89 c7                	mov    %eax,%edi
  402829:	e8 02 fa ff ff       	callq  402230 <sleep@plt>
  40282e:	90                   	nop
  40282f:	c9                   	leaveq 
  402830:	c3                   	retq   

0000000000402831 <sigchld_handler>:
  402831:	55                   	push   %rbp
  402832:	48 89 e5             	mov    %rsp,%rbp
  402835:	48 83 ec 20          	sub    $0x20,%rsp
  402839:	89 7d ec             	mov    %edi,-0x14(%rbp)
  40283c:	e8 3f f4 ff ff       	callq  401c80 <__errno_location@plt>
  402841:	8b 00                	mov    (%rax),%eax
  402843:	89 45 fc             	mov    %eax,-0x4(%rbp)
  402846:	48 8d 45 f4          	lea    -0xc(%rbp),%rax
  40284a:	ba 01 00 00 00       	mov    $0x1,%edx
  40284f:	48 89 c6             	mov    %rax,%rsi
  402852:	bf ff ff ff ff       	mov    $0xffffffff,%edi
  402857:	e8 54 f8 ff ff       	callq  4020b0 <waitpid@plt>
  40285c:	89 45 f8             	mov    %eax,-0x8(%rbp)
  40285f:	8b 45 f8             	mov    -0x8(%rbp),%eax
  402862:	83 f8 ff             	cmp    $0xffffffff,%eax
  402865:	74 06                	je     40286d <sigchld_handler+0x3c>
  402867:	85 c0                	test   %eax,%eax
  402869:	74 11                	je     40287c <sigchld_handler+0x4b>
  40286b:	eb 1e                	jmp    40288b <sigchld_handler+0x5a>
  40286d:	e8 0e f4 ff ff       	callq  401c80 <__errno_location@plt>
  402872:	48 89 c2             	mov    %rax,%rdx
  402875:	8b 45 fc             	mov    -0x4(%rbp),%eax
  402878:	89 02                	mov    %eax,(%rdx)
  40287a:	eb 11                	jmp    40288d <sigchld_handler+0x5c>
  40287c:	e8 ff f3 ff ff       	callq  401c80 <__errno_location@plt>
  402881:	48 89 c2             	mov    %rax,%rdx
  402884:	8b 45 fc             	mov    -0x4(%rbp),%eax
  402887:	89 02                	mov    %eax,(%rdx)
  402889:	eb 02                	jmp    40288d <sigchld_handler+0x5c>
  40288b:	eb b9                	jmp    402846 <sigchld_handler+0x15>
  40288d:	c9                   	leaveq 
  40288e:	c3                   	retq   

000000000040288f <sighup_handler>:
  40288f:	55                   	push   %rbp
  402890:	48 89 e5             	mov    %rsp,%rbp
  402893:	48 83 ec 10          	sub    $0x10,%rsp
  402897:	89 7d fc             	mov    %edi,-0x4(%rbp)
  40289a:	e8 9b 4c 00 00       	callq  40753a <log_close>
  40289f:	be 8f 28 40 00       	mov    $0x40288f,%esi
  4028a4:	bf 01 00 00 00       	mov    $0x1,%edi
  4028a9:	e8 62 f6 ff ff       	callq  401f10 <signal@plt>
  4028ae:	90                   	nop
  4028af:	c9                   	leaveq 
  4028b0:	c3                   	retq   

00000000004028b1 <parse_args>:
  4028b1:	55                   	push   %rbp
  4028b2:	48 89 e5             	mov    %rsp,%rbp
  4028b5:	48 83 ec 20          	sub    $0x20,%rsp
  4028b9:	89 7d ec             	mov    %edi,-0x14(%rbp)
  4028bc:	48 89 75 e0          	mov    %rsi,-0x20(%rbp)
  4028c0:	c7 05 fe 8b 20 00 01 	movl   $0x1,0x208bfe(%rip)        # 60b4c8 <log_level>
  4028c7:	00 00 00 
  4028ca:	c7 05 98 9c 20 00 00 	movl   $0x0,0x209c98(%rip)        # 60c56c <stay_foreground>
  4028d1:	00 00 00 
  4028d4:	c7 05 a2 9c 20 00 00 	movl   $0x0,0x209ca2(%rip)        # 60c580 <lsbsysinit_mode>
  4028db:	00 00 00 
  4028de:	eb 6b                	jmp    40294b <parse_args+0x9a>
  4028e0:	8b 45 fc             	mov    -0x4(%rbp),%eax
  4028e3:	83 f8 66             	cmp    $0x66,%eax
  4028e6:	74 1b                	je     402903 <parse_args+0x52>
  4028e8:	83 f8 66             	cmp    $0x66,%eax
  4028eb:	7f 07                	jg     4028f4 <parse_args+0x43>
  4028ed:	83 f8 4c             	cmp    $0x4c,%eax
  4028f0:	74 43                	je     402935 <parse_args+0x84>
  4028f2:	eb 0a                	jmp    4028fe <parse_args+0x4d>
  4028f4:	83 f8 6c             	cmp    $0x6c,%eax
  4028f7:	74 30                	je     402929 <parse_args+0x78>
  4028f9:	83 f8 78             	cmp    $0x78,%eax
  4028fc:	74 11                	je     40290f <parse_args+0x5e>
  4028fe:	e8 c3 fa ff ff       	callq  4023c6 <usage>
  402903:	c7 05 5f 9c 20 00 01 	movl   $0x1,0x209c5f(%rip)        # 60c56c <stay_foreground>
  40290a:	00 00 00 
  40290d:	eb 3c                	jmp    40294b <parse_args+0x9a>
  40290f:	48 8b 05 aa 8e 20 00 	mov    0x208eaa(%rip),%rax        # 60b7c0 <optarg@@GLIBC_2.2.5>
  402916:	48 89 c7             	mov    %rax,%rdi
  402919:	e8 43 42 00 00       	callq  406b61 <set_debug_flags>
  40291e:	85 c0                	test   %eax,%eax
  402920:	75 29                	jne    40294b <parse_args+0x9a>
  402922:	e8 9f fa ff ff       	callq  4023c6 <usage>
  402927:	eb 22                	jmp    40294b <parse_args+0x9a>
  402929:	c7 05 4d 9c 20 00 01 	movl   $0x1,0x209c4d(%rip)        # 60c580 <lsbsysinit_mode>
  402930:	00 00 00 
  402933:	eb 16                	jmp    40294b <parse_args+0x9a>
  402935:	48 8b 05 84 8e 20 00 	mov    0x208e84(%rip),%rax        # 60b7c0 <optarg@@GLIBC_2.2.5>
  40293c:	48 89 c7             	mov    %rax,%rdi
  40293f:	e8 1c f8 ff ff       	callq  402160 <atoi@plt>
  402944:	89 05 7e 8b 20 00    	mov    %eax,0x208b7e(%rip)        # 60b4c8 <log_level>
  40294a:	90                   	nop
  40294b:	48 8b 4d e0          	mov    -0x20(%rbp),%rcx
  40294f:	8b 45 ec             	mov    -0x14(%rbp),%eax
  402952:	ba 8a 89 40 00       	mov    $0x40898a,%edx
  402957:	48 89 ce             	mov    %rcx,%rsi
  40295a:	89 c7                	mov    %eax,%edi
  40295c:	e8 cf f7 ff ff       	callq  402130 <getopt@plt>
  402961:	89 45 fc             	mov    %eax,-0x4(%rbp)
  402964:	83 7d fc ff          	cmpl   $0xffffffff,-0x4(%rbp)
  402968:	0f 85 72 ff ff ff    	jne    4028e0 <parse_args+0x2f>
  40296e:	90                   	nop
  40296f:	c9                   	leaveq 
  402970:	c3                   	retq   

0000000000402971 <load_database>:
  402971:	55                   	push   %rbp
  402972:	48 89 e5             	mov    %rsp,%rbp
  402975:	48 81 ec d0 23 00 00 	sub    $0x23d0,%rsp
  40297c:	48 89 bd 38 dc ff ff 	mov    %rdi,-0x23c8(%rbp)
  402983:	c7 45 f4 00 00 00 00 	movl   $0x0,-0xc(%rbp)
  40298a:	48 8d 85 40 ff ff ff 	lea    -0xc0(%rbp),%rax
  402991:	48 89 c6             	mov    %rax,%rsi
  402994:	bf 98 89 40 00       	mov    $0x408998,%edi
  402999:	e8 62 5e 00 00       	callq  408800 <__stat>
  40299e:	85 c0                	test   %eax,%eax
  4029a0:	79 23                	jns    4029c5 <load_database+0x54>
  4029a2:	e8 99 f3 ff ff       	callq  401d40 <getpid@plt>
  4029a7:	b9 98 89 40 00       	mov    $0x408998,%ecx
  4029ac:	ba a1 89 40 00       	mov    $0x4089a1,%edx
  4029b1:	89 c6                	mov    %eax,%esi
  4029b3:	bf ad 89 40 00       	mov    $0x4089ad,%edi
  4029b8:	e8 1c 4b 00 00       	callq  4074d9 <log_it>
  4029bd:	48 c7 45 98 00 00 00 	movq   $0x0,-0x68(%rbp)
  4029c4:	00 
  4029c5:	48 8d 85 b0 fe ff ff 	lea    -0x150(%rbp),%rax
  4029cc:	48 89 c6             	mov    %rax,%rsi
  4029cf:	bf b2 89 40 00       	mov    $0x4089b2,%edi
  4029d4:	e8 27 5e 00 00       	callq  408800 <__stat>
  4029d9:	85 c0                	test   %eax,%eax
  4029db:	79 26                	jns    402a03 <load_database+0x92>
  4029dd:	e8 5e f3 ff ff       	callq  401d40 <getpid@plt>
  4029e2:	b9 b2 89 40 00       	mov    $0x4089b2,%ecx
  4029e7:	ba a1 89 40 00       	mov    $0x4089a1,%edx
  4029ec:	89 c6                	mov    %eax,%esi
  4029ee:	bf ad 89 40 00       	mov    $0x4089ad,%edi
  4029f3:	e8 e1 4a 00 00       	callq  4074d9 <log_it>
  4029f8:	48 c7 85 08 ff ff ff 	movq   $0x0,-0xf8(%rbp)
  4029ff:	00 00 00 00 
  402a03:	48 8d 85 f0 fd ff ff 	lea    -0x210(%rbp),%rax
  402a0a:	48 89 c6             	mov    %rax,%rsi
  402a0d:	bf bf 89 40 00       	mov    $0x4089bf,%edi
  402a12:	e8 e9 5d 00 00       	callq  408800 <__stat>
  402a17:	85 c0                	test   %eax,%eax
  402a19:	79 26                	jns    402a41 <load_database+0xd0>
  402a1b:	e8 20 f3 ff ff       	callq  401d40 <getpid@plt>
  402a20:	b9 bf 89 40 00       	mov    $0x4089bf,%ecx
  402a25:	ba a1 89 40 00       	mov    $0x4089a1,%edx
  402a2a:	89 c6                	mov    %eax,%esi
  402a2c:	bf ad 89 40 00       	mov    $0x4089ad,%edi
  402a31:	e8 a3 4a 00 00       	callq  4074d9 <log_it>
  402a36:	48 c7 85 48 fe ff ff 	movq   $0x0,-0x1b8(%rbp)
  402a3d:	00 00 00 00 
  402a41:	48 8b 85 38 dc ff ff 	mov    -0x23c8(%rbp),%rax
  402a48:	48 8b 50 20          	mov    0x20(%rax),%rdx
  402a4c:	48 8b 85 48 fe ff ff 	mov    -0x1b8(%rbp),%rax
  402a53:	48 39 c2             	cmp    %rax,%rdx
  402a56:	74 0c                	je     402a64 <load_database+0xf3>
  402a58:	c7 45 f4 01 00 00 00 	movl   $0x1,-0xc(%rbp)
  402a5f:	e9 b6 00 00 00       	jmpq   402b1a <load_database+0x1a9>
  402a64:	48 8b 85 38 dc ff ff 	mov    -0x23c8(%rbp),%rax
  402a6b:	48 8b 00             	mov    (%rax),%rax
  402a6e:	48 89 45 e8          	mov    %rax,-0x18(%rbp)
  402a72:	e9 88 00 00 00       	jmpq   402aff <load_database+0x18e>
  402a77:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  402a7b:	48 8b 40 10          	mov    0x10(%rax),%rax
  402a7f:	48 8d 50 08          	lea    0x8(%rax),%rdx
  402a83:	48 8d 85 50 ed ff ff 	lea    -0x12b0(%rbp),%rax
  402a8a:	48 89 d1             	mov    %rdx,%rcx
  402a8d:	ba bf 89 40 00       	mov    $0x4089bf,%edx
  402a92:	be cb 89 40 00       	mov    $0x4089cb,%esi
  402a97:	48 89 c7             	mov    %rax,%rdi
  402a9a:	b8 00 00 00 00       	mov    $0x0,%eax
  402a9f:	e8 0c f7 ff ff       	callq  4021b0 <sprintf@plt>
  402aa4:	48 8d 95 60 fd ff ff 	lea    -0x2a0(%rbp),%rdx
  402aab:	48 8d 85 50 ed ff ff 	lea    -0x12b0(%rbp),%rax
  402ab2:	48 89 d6             	mov    %rdx,%rsi
  402ab5:	48 89 c7             	mov    %rax,%rdi
  402ab8:	e8 43 5d 00 00       	callq  408800 <__stat>
  402abd:	85 c0                	test   %eax,%eax
  402abf:	79 0b                	jns    402acc <load_database+0x15b>
  402ac1:	48 c7 85 b8 fd ff ff 	movq   $0x0,-0x248(%rbp)
  402ac8:	00 00 00 00 
  402acc:	48 8b 95 b8 fd ff ff 	mov    -0x248(%rbp),%rdx
  402ad3:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  402ad7:	48 8b 40 18          	mov    0x18(%rax),%rax
  402adb:	48 39 c2             	cmp    %rax,%rdx
  402ade:	75 0d                	jne    402aed <load_database+0x17c>
  402ae0:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  402ae4:	48 8b 40 18          	mov    0x18(%rax),%rax
  402ae8:	48 85 c0             	test   %rax,%rax
  402aeb:	75 07                	jne    402af4 <load_database+0x183>
  402aed:	c7 45 f4 01 00 00 00 	movl   $0x1,-0xc(%rbp)
  402af4:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  402af8:	48 8b 00             	mov    (%rax),%rax
  402afb:	48 89 45 e8          	mov    %rax,-0x18(%rbp)
  402aff:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  402b03:	48 89 c7             	mov    %rax,%rdi
  402b06:	e8 20 0c 00 00       	callq  40372b <get_next_system_crontab>
  402b0b:	48 89 45 e8          	mov    %rax,-0x18(%rbp)
  402b0f:	48 83 7d e8 00       	cmpq   $0x0,-0x18(%rbp)
  402b14:	0f 85 5d ff ff ff    	jne    402a77 <load_database+0x106>
  402b1a:	48 8b 85 38 dc ff ff 	mov    -0x23c8(%rbp),%rax
  402b21:	48 8b 50 10          	mov    0x10(%rax),%rdx
  402b25:	48 8b 45 98          	mov    -0x68(%rbp),%rax
  402b29:	48 39 c2             	cmp    %rax,%rdx
  402b2c:	75 21                	jne    402b4f <load_database+0x1de>
  402b2e:	48 8b 85 38 dc ff ff 	mov    -0x23c8(%rbp),%rax
  402b35:	48 8b 50 18          	mov    0x18(%rax),%rdx
  402b39:	48 8b 85 08 ff ff ff 	mov    -0xf8(%rbp),%rax
  402b40:	48 39 c2             	cmp    %rax,%rdx
  402b43:	75 0a                	jne    402b4f <load_database+0x1de>
  402b45:	83 7d f4 00          	cmpl   $0x0,-0xc(%rbp)
  402b49:	0f 84 36 03 00 00    	je     402e85 <load_database+0x514>
  402b4f:	48 8b 45 98          	mov    -0x68(%rbp),%rax
  402b53:	48 89 85 90 fe ff ff 	mov    %rax,-0x170(%rbp)
  402b5a:	48 8b 85 08 ff ff ff 	mov    -0xf8(%rbp),%rax
  402b61:	48 89 85 98 fe ff ff 	mov    %rax,-0x168(%rbp)
  402b68:	48 8b 85 48 fe ff ff 	mov    -0x1b8(%rbp),%rax
  402b6f:	48 89 85 a0 fe ff ff 	mov    %rax,-0x160(%rbp)
  402b76:	48 c7 85 88 fe ff ff 	movq   $0x0,-0x178(%rbp)
  402b7d:	00 00 00 00 
  402b81:	48 8b 85 88 fe ff ff 	mov    -0x178(%rbp),%rax
  402b88:	48 89 85 80 fe ff ff 	mov    %rax,-0x180(%rbp)
  402b8f:	48 8b 85 08 ff ff ff 	mov    -0xf8(%rbp),%rax
  402b96:	48 85 c0             	test   %rax,%rax
  402b99:	74 32                	je     402bcd <load_database+0x25c>
  402b9b:	48 8b 8d 38 dc ff ff 	mov    -0x23c8(%rbp),%rcx
  402ba2:	48 8d 95 80 fe ff ff 	lea    -0x180(%rbp),%rdx
  402ba9:	48 8d 85 b0 fe ff ff 	lea    -0x150(%rbp),%rax
  402bb0:	49 89 c9             	mov    %rcx,%r9
  402bb3:	49 89 d0             	mov    %rdx,%r8
  402bb6:	48 89 c1             	mov    %rax,%rcx
  402bb9:	ba b2 89 40 00       	mov    $0x4089b2,%edx
  402bbe:	be d1 89 40 00       	mov    $0x4089d1,%esi
  402bc3:	bf da 89 40 00       	mov    $0x4089da,%edi
  402bc8:	e8 e6 03 00 00       	callq  402fb3 <process_crontab>
  402bcd:	bf bf 89 40 00       	mov    $0x4089bf,%edi
  402bd2:	e8 89 f1 ff ff       	callq  401d60 <opendir@plt>
  402bd7:	48 89 45 e0          	mov    %rax,-0x20(%rbp)
  402bdb:	48 83 7d e0 00       	cmpq   $0x0,-0x20(%rbp)
  402be0:	0f 85 df 00 00 00    	jne    402cc5 <load_database+0x354>
  402be6:	e8 55 f1 ff ff       	callq  401d40 <getpid@plt>
  402beb:	b9 bf 89 40 00       	mov    $0x4089bf,%ecx
  402bf0:	ba df 89 40 00       	mov    $0x4089df,%edx
  402bf5:	89 c6                	mov    %eax,%esi
  402bf7:	bf ad 89 40 00       	mov    $0x4089ad,%edi
  402bfc:	e8 d8 48 00 00       	callq  4074d9 <log_it>
  402c01:	e9 bf 00 00 00       	jmpq   402cc5 <load_database+0x354>
  402c06:	48 8b 45 d8          	mov    -0x28(%rbp),%rax
  402c0a:	0f b6 40 13          	movzbl 0x13(%rax),%eax
  402c0e:	3c 2e                	cmp    $0x2e,%al
  402c10:	0f 84 ab 00 00 00    	je     402cc1 <load_database+0x350>
  402c16:	48 8b 45 d8          	mov    -0x28(%rbp),%rax
  402c1a:	48 83 c0 13          	add    $0x13,%rax
  402c1e:	48 89 c7             	mov    %rax,%rdi
  402c21:	e8 b4 09 00 00       	callq  4035da <valid_name>
  402c26:	85 c0                	test   %eax,%eax
  402c28:	0f 84 96 00 00 00    	je     402cc4 <load_database+0x353>
  402c2e:	48 8d 85 40 dc ff ff 	lea    -0x23c0(%rbp),%rax
  402c35:	48 b9 2a 73 79 73 74 	movabs $0x2a6d65747379732a,%rcx
  402c3c:	65 6d 2a 
  402c3f:	48 89 08             	mov    %rcx,(%rax)
  402c42:	c6 40 08 00          	movb   $0x0,0x8(%rax)
  402c46:	48 8b 45 d8          	mov    -0x28(%rbp),%rax
  402c4a:	48 8d 50 13          	lea    0x13(%rax),%rdx
  402c4e:	48 8d 85 40 dc ff ff 	lea    -0x23c0(%rbp),%rax
  402c55:	48 89 d6             	mov    %rdx,%rsi
  402c58:	48 89 c7             	mov    %rax,%rdi
  402c5b:	e8 10 f5 ff ff       	callq  402170 <strcat@plt>
  402c60:	48 8b 45 d8          	mov    -0x28(%rbp),%rax
  402c64:	48 8d 50 13          	lea    0x13(%rax),%rdx
  402c68:	48 8d 85 40 dd ff ff 	lea    -0x22c0(%rbp),%rax
  402c6f:	48 89 d1             	mov    %rdx,%rcx
  402c72:	ba bf 89 40 00       	mov    $0x4089bf,%edx
  402c77:	be cb 89 40 00       	mov    $0x4089cb,%esi
  402c7c:	48 89 c7             	mov    %rax,%rdi
  402c7f:	b8 00 00 00 00       	mov    $0x0,%eax
  402c84:	e8 27 f5 ff ff       	callq  4021b0 <sprintf@plt>
  402c89:	48 8b bd 38 dc ff ff 	mov    -0x23c8(%rbp),%rdi
  402c90:	48 8d b5 80 fe ff ff 	lea    -0x180(%rbp),%rsi
  402c97:	48 8d 8d 40 ff ff ff 	lea    -0xc0(%rbp),%rcx
  402c9e:	48 8d 95 40 dd ff ff 	lea    -0x22c0(%rbp),%rdx
  402ca5:	48 8d 85 40 dc ff ff 	lea    -0x23c0(%rbp),%rax
  402cac:	49 89 f9             	mov    %rdi,%r9
  402caf:	49 89 f0             	mov    %rsi,%r8
  402cb2:	48 89 c6             	mov    %rax,%rsi
  402cb5:	bf da 89 40 00       	mov    $0x4089da,%edi
  402cba:	e8 f4 02 00 00       	callq  402fb3 <process_crontab>
  402cbf:	eb 04                	jmp    402cc5 <load_database+0x354>
  402cc1:	90                   	nop
  402cc2:	eb 01                	jmp    402cc5 <load_database+0x354>
  402cc4:	90                   	nop
  402cc5:	48 83 7d e0 00       	cmpq   $0x0,-0x20(%rbp)
  402cca:	74 1b                	je     402ce7 <load_database+0x376>
  402ccc:	48 8b 45 e0          	mov    -0x20(%rbp),%rax
  402cd0:	48 89 c7             	mov    %rax,%rdi
  402cd3:	e8 e8 f2 ff ff       	callq  401fc0 <readdir@plt>
  402cd8:	48 89 45 d8          	mov    %rax,-0x28(%rbp)
  402cdc:	48 83 7d d8 00       	cmpq   $0x0,-0x28(%rbp)
  402ce1:	0f 85 1f ff ff ff    	jne    402c06 <load_database+0x295>
  402ce7:	48 83 7d e0 00       	cmpq   $0x0,-0x20(%rbp)
  402cec:	74 0c                	je     402cfa <load_database+0x389>
  402cee:	48 8b 45 e0          	mov    -0x20(%rbp),%rax
  402cf2:	48 89 c7             	mov    %rax,%rdi
  402cf5:	e8 86 f1 ff ff       	callq  401e80 <closedir@plt>
  402cfa:	bf 98 89 40 00       	mov    $0x408998,%edi
  402cff:	e8 5c f0 ff ff       	callq  401d60 <opendir@plt>
  402d04:	48 89 45 e0          	mov    %rax,-0x20(%rbp)
  402d08:	48 83 7d e0 00       	cmpq   $0x0,-0x20(%rbp)
  402d0d:	0f 85 ad 00 00 00    	jne    402dc0 <load_database+0x44f>
  402d13:	e8 28 f0 ff ff       	callq  401d40 <getpid@plt>
  402d18:	b9 98 89 40 00       	mov    $0x408998,%ecx
  402d1d:	ba df 89 40 00       	mov    $0x4089df,%edx
  402d22:	89 c6                	mov    %eax,%esi
  402d24:	bf ad 89 40 00       	mov    $0x4089ad,%edi
  402d29:	e8 ab 47 00 00       	callq  4074d9 <log_it>
  402d2e:	e9 8d 00 00 00       	jmpq   402dc0 <load_database+0x44f>
  402d33:	48 8b 45 d8          	mov    -0x28(%rbp),%rax
  402d37:	0f b6 40 13          	movzbl 0x13(%rax),%eax
  402d3b:	3c 2e                	cmp    $0x2e,%al
  402d3d:	75 02                	jne    402d41 <load_database+0x3d0>
  402d3f:	eb 7f                	jmp    402dc0 <load_database+0x44f>
  402d41:	48 8b 45 d8          	mov    -0x28(%rbp),%rax
  402d45:	48 8d 50 13          	lea    0x13(%rax),%rdx
  402d49:	48 8d 85 40 dc ff ff 	lea    -0x23c0(%rbp),%rax
  402d50:	48 89 d6             	mov    %rdx,%rsi
  402d53:	48 89 c7             	mov    %rax,%rdi
  402d56:	e8 65 ef ff ff       	callq  401cc0 <strcpy@plt>
  402d5b:	48 8d 95 40 dc ff ff 	lea    -0x23c0(%rbp),%rdx
  402d62:	48 8d 85 40 dd ff ff 	lea    -0x22c0(%rbp),%rax
  402d69:	49 89 d0             	mov    %rdx,%r8
  402d6c:	b9 98 89 40 00       	mov    $0x408998,%ecx
  402d71:	ba cb 89 40 00       	mov    $0x4089cb,%edx
  402d76:	be 01 10 00 00       	mov    $0x1001,%esi
  402d7b:	48 89 c7             	mov    %rax,%rdi
  402d7e:	b8 00 00 00 00       	mov    $0x0,%eax
  402d83:	e8 68 f0 ff ff       	callq  401df0 <snprintf@plt>
  402d88:	4c 8b 85 38 dc ff ff 	mov    -0x23c8(%rbp),%r8
  402d8f:	48 8d bd 80 fe ff ff 	lea    -0x180(%rbp),%rdi
  402d96:	48 8d 8d 40 ff ff ff 	lea    -0xc0(%rbp),%rcx
  402d9d:	48 8d 95 40 dd ff ff 	lea    -0x22c0(%rbp),%rdx
  402da4:	48 8d b5 40 dc ff ff 	lea    -0x23c0(%rbp),%rsi
  402dab:	48 8d 85 40 dc ff ff 	lea    -0x23c0(%rbp),%rax
  402db2:	4d 89 c1             	mov    %r8,%r9
  402db5:	49 89 f8             	mov    %rdi,%r8
  402db8:	48 89 c7             	mov    %rax,%rdi
  402dbb:	e8 f3 01 00 00       	callq  402fb3 <process_crontab>
  402dc0:	48 83 7d e0 00       	cmpq   $0x0,-0x20(%rbp)
  402dc5:	74 1b                	je     402de2 <load_database+0x471>
  402dc7:	48 8b 45 e0          	mov    -0x20(%rbp),%rax
  402dcb:	48 89 c7             	mov    %rax,%rdi
  402dce:	e8 ed f1 ff ff       	callq  401fc0 <readdir@plt>
  402dd3:	48 89 45 d8          	mov    %rax,-0x28(%rbp)
  402dd7:	48 83 7d d8 00       	cmpq   $0x0,-0x28(%rbp)
  402ddc:	0f 85 51 ff ff ff    	jne    402d33 <load_database+0x3c2>
  402de2:	48 83 7d e0 00       	cmpq   $0x0,-0x20(%rbp)
  402de7:	74 0c                	je     402df5 <load_database+0x484>
  402de9:	48 8b 45 e0          	mov    -0x20(%rbp),%rax
  402ded:	48 89 c7             	mov    %rax,%rdi
  402df0:	e8 8b f0 ff ff       	callq  401e80 <closedir@plt>
  402df5:	e8 36 f2 ff ff       	callq  402030 <endpwent@plt>
  402dfa:	48 8b 85 38 dc ff ff 	mov    -0x23c8(%rbp),%rax
  402e01:	48 8b 00             	mov    (%rax),%rax
  402e04:	48 89 45 f8          	mov    %rax,-0x8(%rbp)
  402e08:	eb 35                	jmp    402e3f <load_database+0x4ce>
  402e0a:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  402e0e:	48 8b 00             	mov    (%rax),%rax
  402e11:	48 89 45 d0          	mov    %rax,-0x30(%rbp)
  402e15:	48 8b 55 f8          	mov    -0x8(%rbp),%rdx
  402e19:	48 8b 85 38 dc ff ff 	mov    -0x23c8(%rbp),%rax
  402e20:	48 89 d6             	mov    %rdx,%rsi
  402e23:	48 89 c7             	mov    %rax,%rdi
  402e26:	e8 c6 00 00 00       	callq  402ef1 <unlink_user>
  402e2b:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  402e2f:	48 89 c7             	mov    %rax,%rdi
  402e32:	e8 1a 0d 00 00       	callq  403b51 <free_user>
  402e37:	48 8b 45 d0          	mov    -0x30(%rbp),%rax
  402e3b:	48 89 45 f8          	mov    %rax,-0x8(%rbp)
  402e3f:	48 83 7d f8 00       	cmpq   $0x0,-0x8(%rbp)
  402e44:	75 c4                	jne    402e0a <load_database+0x499>
  402e46:	48 8b 85 38 dc ff ff 	mov    -0x23c8(%rbp),%rax
  402e4d:	48 8b 95 80 fe ff ff 	mov    -0x180(%rbp),%rdx
  402e54:	48 89 10             	mov    %rdx,(%rax)
  402e57:	48 8b 95 88 fe ff ff 	mov    -0x178(%rbp),%rdx
  402e5e:	48 89 50 08          	mov    %rdx,0x8(%rax)
  402e62:	48 8b 95 90 fe ff ff 	mov    -0x170(%rbp),%rdx
  402e69:	48 89 50 10          	mov    %rdx,0x10(%rax)
  402e6d:	48 8b 95 98 fe ff ff 	mov    -0x168(%rbp),%rdx
  402e74:	48 89 50 18          	mov    %rdx,0x18(%rax)
  402e78:	48 8b 95 a0 fe ff ff 	mov    -0x160(%rbp),%rdx
  402e7f:	48 89 50 20          	mov    %rdx,0x20(%rax)
  402e83:	eb 01                	jmp    402e86 <load_database+0x515>
  402e85:	90                   	nop
  402e86:	c9                   	leaveq 
  402e87:	c3                   	retq   

0000000000402e88 <link_user>:
  402e88:	55                   	push   %rbp
  402e89:	48 89 e5             	mov    %rsp,%rbp
  402e8c:	48 89 7d f8          	mov    %rdi,-0x8(%rbp)
  402e90:	48 89 75 f0          	mov    %rsi,-0x10(%rbp)
  402e94:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  402e98:	48 8b 00             	mov    (%rax),%rax
  402e9b:	48 85 c0             	test   %rax,%rax
  402e9e:	75 0b                	jne    402eab <link_user+0x23>
  402ea0:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  402ea4:	48 8b 55 f0          	mov    -0x10(%rbp),%rdx
  402ea8:	48 89 10             	mov    %rdx,(%rax)
  402eab:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  402eaf:	48 8b 40 08          	mov    0x8(%rax),%rax
  402eb3:	48 85 c0             	test   %rax,%rax
  402eb6:	74 0f                	je     402ec7 <link_user+0x3f>
  402eb8:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  402ebc:	48 8b 40 08          	mov    0x8(%rax),%rax
  402ec0:	48 8b 55 f0          	mov    -0x10(%rbp),%rdx
  402ec4:	48 89 10             	mov    %rdx,(%rax)
  402ec7:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  402ecb:	48 8b 50 08          	mov    0x8(%rax),%rdx
  402ecf:	48 8b 45 f0          	mov    -0x10(%rbp),%rax
  402ed3:	48 89 50 08          	mov    %rdx,0x8(%rax)
  402ed7:	48 8b 45 f0          	mov    -0x10(%rbp),%rax
  402edb:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  402ee2:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  402ee6:	48 8b 55 f0          	mov    -0x10(%rbp),%rdx
  402eea:	48 89 50 08          	mov    %rdx,0x8(%rax)
  402eee:	90                   	nop
  402eef:	5d                   	pop    %rbp
  402ef0:	c3                   	retq   

0000000000402ef1 <unlink_user>:
  402ef1:	55                   	push   %rbp
  402ef2:	48 89 e5             	mov    %rsp,%rbp
  402ef5:	48 89 7d f8          	mov    %rdi,-0x8(%rbp)
  402ef9:	48 89 75 f0          	mov    %rsi,-0x10(%rbp)
  402efd:	48 8b 45 f0          	mov    -0x10(%rbp),%rax
  402f01:	48 8b 40 08          	mov    0x8(%rax),%rax
  402f05:	48 85 c0             	test   %rax,%rax
  402f08:	75 10                	jne    402f1a <unlink_user+0x29>
  402f0a:	48 8b 45 f0          	mov    -0x10(%rbp),%rax
  402f0e:	48 8b 10             	mov    (%rax),%rdx
  402f11:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  402f15:	48 89 10             	mov    %rdx,(%rax)
  402f18:	eb 12                	jmp    402f2c <unlink_user+0x3b>
  402f1a:	48 8b 45 f0          	mov    -0x10(%rbp),%rax
  402f1e:	48 8b 40 08          	mov    0x8(%rax),%rax
  402f22:	48 8b 55 f0          	mov    -0x10(%rbp),%rdx
  402f26:	48 8b 12             	mov    (%rdx),%rdx
  402f29:	48 89 10             	mov    %rdx,(%rax)
  402f2c:	48 8b 45 f0          	mov    -0x10(%rbp),%rax
  402f30:	48 8b 00             	mov    (%rax),%rax
  402f33:	48 85 c0             	test   %rax,%rax
  402f36:	75 12                	jne    402f4a <unlink_user+0x59>
  402f38:	48 8b 45 f0          	mov    -0x10(%rbp),%rax
  402f3c:	48 8b 50 08          	mov    0x8(%rax),%rdx
  402f40:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  402f44:	48 89 50 08          	mov    %rdx,0x8(%rax)
  402f48:	eb 13                	jmp    402f5d <unlink_user+0x6c>
  402f4a:	48 8b 45 f0          	mov    -0x10(%rbp),%rax
  402f4e:	48 8b 00             	mov    (%rax),%rax
  402f51:	48 8b 55 f0          	mov    -0x10(%rbp),%rdx
  402f55:	48 8b 52 08          	mov    0x8(%rdx),%rdx
  402f59:	48 89 50 08          	mov    %rdx,0x8(%rax)
  402f5d:	90                   	nop
  402f5e:	5d                   	pop    %rbp
  402f5f:	c3                   	retq   

0000000000402f60 <find_user>:
  402f60:	55                   	push   %rbp
  402f61:	48 89 e5             	mov    %rsp,%rbp
  402f64:	48 83 ec 20          	sub    $0x20,%rsp
  402f68:	48 89 7d e8          	mov    %rdi,-0x18(%rbp)
  402f6c:	48 89 75 e0          	mov    %rsi,-0x20(%rbp)
  402f70:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  402f74:	48 8b 00             	mov    (%rax),%rax
  402f77:	48 89 45 f8          	mov    %rax,-0x8(%rbp)
  402f7b:	eb 26                	jmp    402fa3 <find_user+0x43>
  402f7d:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  402f81:	48 8b 40 10          	mov    0x10(%rax),%rax
  402f85:	48 8b 55 e0          	mov    -0x20(%rbp),%rdx
  402f89:	48 89 d6             	mov    %rdx,%rsi
  402f8c:	48 89 c7             	mov    %rax,%rdi
  402f8f:	e8 6c ef ff ff       	callq  401f00 <strcmp@plt>
  402f94:	85 c0                	test   %eax,%eax
  402f96:	74 14                	je     402fac <find_user+0x4c>
  402f98:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  402f9c:	48 8b 00             	mov    (%rax),%rax
  402f9f:	48 89 45 f8          	mov    %rax,-0x8(%rbp)
  402fa3:	48 83 7d f8 00       	cmpq   $0x0,-0x8(%rbp)
  402fa8:	75 d3                	jne    402f7d <find_user+0x1d>
  402faa:	eb 01                	jmp    402fad <find_user+0x4d>
  402fac:	90                   	nop
  402fad:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  402fb1:	c9                   	leaveq 
  402fb2:	c3                   	retq   

0000000000402fb3 <process_crontab>:
  402fb3:	55                   	push   %rbp
  402fb4:	48 89 e5             	mov    %rsp,%rbp
  402fb7:	48 83 ec 50          	sub    $0x50,%rsp
  402fbb:	48 89 7d d8          	mov    %rdi,-0x28(%rbp)
  402fbf:	48 89 75 d0          	mov    %rsi,-0x30(%rbp)
  402fc3:	48 89 55 c8          	mov    %rdx,-0x38(%rbp)
  402fc7:	48 89 4d c0          	mov    %rcx,-0x40(%rbp)
  402fcb:	4c 89 45 b8          	mov    %r8,-0x48(%rbp)
  402fcf:	4c 89 4d b0          	mov    %r9,-0x50(%rbp)
  402fd3:	48 c7 45 f8 00 00 00 	movq   $0x0,-0x8(%rbp)
  402fda:	00 
  402fdb:	c7 45 f4 ff ff ff ff 	movl   $0xffffffff,-0xc(%rbp)
  402fe2:	48 c7 45 e8 00 00 00 	movq   $0x0,-0x18(%rbp)
  402fe9:	00 
  402fea:	48 8b 45 d0          	mov    -0x30(%rbp),%rax
  402fee:	ba 08 00 00 00       	mov    $0x8,%edx
  402ff3:	be d1 89 40 00       	mov    $0x4089d1,%esi
  402ff8:	48 89 c7             	mov    %rax,%rdi
  402ffb:	e8 a0 ec ff ff       	callq  401ca0 <strncmp@plt>
  403000:	85 c0                	test   %eax,%eax
  403002:	74 6e                	je     403072 <process_crontab+0xbf>
  403004:	48 8b 45 d8          	mov    -0x28(%rbp),%rax
  403008:	48 89 c7             	mov    %rax,%rdi
  40300b:	e8 10 ef ff ff       	callq  401f20 <getpwnam@plt>
  403010:	48 89 45 f8          	mov    %rax,-0x8(%rbp)
  403014:	48 83 7d f8 00       	cmpq   $0x0,-0x8(%rbp)
  403019:	75 57                	jne    403072 <process_crontab+0xbf>
  40301b:	48 8b 45 d0          	mov    -0x30(%rbp),%rax
  40301f:	ba 04 00 00 00       	mov    $0x4,%edx
  403024:	be ee 89 40 00       	mov    $0x4089ee,%esi
  403029:	48 89 c7             	mov    %rax,%rdi
  40302c:	e8 6f ec ff ff       	callq  401ca0 <strncmp@plt>
  403031:	85 c0                	test   %eax,%eax
  403033:	0f 84 8d 05 00 00    	je     4035c6 <process_crontab+0x613>
  403039:	e8 02 ed ff ff       	callq  401d40 <getpid@plt>
  40303e:	89 c6                	mov    %eax,%esi
  403040:	48 8b 45 d0          	mov    -0x30(%rbp),%rax
  403044:	b9 f3 89 40 00       	mov    $0x4089f3,%ecx
  403049:	ba 03 8a 40 00       	mov    $0x408a03,%edx
  40304e:	48 89 c7             	mov    %rax,%rdi
  403051:	e8 83 44 00 00       	callq  4074d9 <log_it>
  403056:	48 8b 55 c8          	mov    -0x38(%rbp),%rdx
  40305a:	48 8b 4d d0          	mov    -0x30(%rbp),%rcx
  40305e:	48 8b 45 d8          	mov    -0x28(%rbp),%rax
  403062:	48 89 ce             	mov    %rcx,%rsi
  403065:	48 89 c7             	mov    %rax,%rdi
  403068:	e8 3b 09 00 00       	callq  4039a8 <add_orphan>
  40306d:	e9 54 05 00 00       	jmpq   4035c6 <process_crontab+0x613>
  403072:	48 83 7d f8 00       	cmpq   $0x0,-0x8(%rbp)
  403077:	0f 84 b1 01 00 00    	je     40322e <process_crontab+0x27b>
  40307d:	48 8b 45 c8          	mov    -0x38(%rbp),%rax
  403081:	ba 00 00 00 00       	mov    $0x0,%edx
  403086:	be 00 00 02 00       	mov    $0x20000,%esi
  40308b:	48 89 c7             	mov    %rax,%rdi
  40308e:	b8 00 00 00 00       	mov    $0x0,%eax
  403093:	e8 28 f0 ff ff       	callq  4020c0 <open@plt>
  403098:	89 45 f4             	mov    %eax,-0xc(%rbp)
  40309b:	83 7d f4 00          	cmpl   $0x0,-0xc(%rbp)
  40309f:	79 24                	jns    4030c5 <process_crontab+0x112>
  4030a1:	e8 9a ec ff ff       	callq  401d40 <getpid@plt>
  4030a6:	89 c6                	mov    %eax,%esi
  4030a8:	48 8b 55 c8          	mov    -0x38(%rbp),%rdx
  4030ac:	48 8b 45 d0          	mov    -0x30(%rbp),%rax
  4030b0:	48 89 d1             	mov    %rdx,%rcx
  4030b3:	ba 0a 8a 40 00       	mov    $0x408a0a,%edx
  4030b8:	48 89 c7             	mov    %rax,%rdi
  4030bb:	e8 19 44 00 00       	callq  4074d9 <log_it>
  4030c0:	e9 02 05 00 00       	jmpq   4035c7 <process_crontab+0x614>
  4030c5:	48 8b 55 c0          	mov    -0x40(%rbp),%rdx
  4030c9:	8b 45 f4             	mov    -0xc(%rbp),%eax
  4030cc:	48 89 d6             	mov    %rdx,%rsi
  4030cf:	89 c7                	mov    %eax,%edi
  4030d1:	e8 3a 57 00 00       	callq  408810 <__fstat>
  4030d6:	85 c0                	test   %eax,%eax
  4030d8:	79 24                	jns    4030fe <process_crontab+0x14b>
  4030da:	e8 61 ec ff ff       	callq  401d40 <getpid@plt>
  4030df:	89 c6                	mov    %eax,%esi
  4030e1:	48 8b 55 c8          	mov    -0x38(%rbp),%rdx
  4030e5:	48 8b 45 d0          	mov    -0x30(%rbp),%rax
  4030e9:	48 89 d1             	mov    %rdx,%rcx
  4030ec:	ba 15 8a 40 00       	mov    $0x408a15,%edx
  4030f1:	48 89 c7             	mov    %rax,%rdi
  4030f4:	e8 e0 43 00 00       	callq  4074d9 <log_it>
  4030f9:	e9 c9 04 00 00       	jmpq   4035c7 <process_crontab+0x614>
  4030fe:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  403102:	8b 50 1c             	mov    0x1c(%rax),%edx
  403105:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  403109:	8b 40 10             	mov    0x10(%rax),%eax
  40310c:	39 c2                	cmp    %eax,%edx
  40310e:	74 48                	je     403158 <process_crontab+0x1a5>
  403110:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  403114:	8b 40 1c             	mov    0x1c(%rax),%eax
  403117:	85 c0                	test   %eax,%eax
  403119:	74 3d                	je     403158 <process_crontab+0x1a5>
  40311b:	e8 20 ec ff ff       	callq  401d40 <getpid@plt>
  403120:	89 c6                	mov    %eax,%esi
  403122:	48 8b 55 c8          	mov    -0x38(%rbp),%rdx
  403126:	48 8b 45 d0          	mov    -0x30(%rbp),%rax
  40312a:	48 89 d1             	mov    %rdx,%rcx
  40312d:	ba 22 8a 40 00       	mov    $0x408a22,%edx
  403132:	48 89 c7             	mov    %rax,%rdi
  403135:	e8 9f 43 00 00       	callq  4074d9 <log_it>
  40313a:	48 8b 55 d0          	mov    -0x30(%rbp),%rdx
  40313e:	48 8b 75 b8          	mov    -0x48(%rbp),%rsi
  403142:	48 8b 45 b0          	mov    -0x50(%rbp),%rax
  403146:	b9 00 00 00 00       	mov    $0x0,%ecx
  40314b:	48 89 c7             	mov    %rax,%rdi
  40314e:	e8 32 06 00 00       	callq  403785 <force_rescan_user>
  403153:	e9 6f 04 00 00       	jmpq   4035c7 <process_crontab+0x614>
  403158:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  40315c:	8b 40 18             	mov    0x18(%rax),%eax
  40315f:	25 00 f0 00 00       	and    $0xf000,%eax
  403164:	3d 00 80 00 00       	cmp    $0x8000,%eax
  403169:	74 24                	je     40318f <process_crontab+0x1dc>
  40316b:	e8 d0 eb ff ff       	callq  401d40 <getpid@plt>
  403170:	89 c6                	mov    %eax,%esi
  403172:	48 8b 55 c8          	mov    -0x38(%rbp),%rdx
  403176:	48 8b 45 d0          	mov    -0x30(%rbp),%rax
  40317a:	48 89 d1             	mov    %rdx,%rcx
  40317d:	ba 33 8a 40 00       	mov    $0x408a33,%edx
  403182:	48 89 c7             	mov    %rax,%rdi
  403185:	e8 4f 43 00 00       	callq  4074d9 <log_it>
  40318a:	e9 38 04 00 00       	jmpq   4035c7 <process_crontab+0x614>
  40318f:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  403193:	8b 40 18             	mov    0x18(%rax),%eax
  403196:	25 ff 0f 00 00       	and    $0xfff,%eax
  40319b:	3d 80 01 00 00       	cmp    $0x180,%eax
  4031a0:	74 3d                	je     4031df <process_crontab+0x22c>
  4031a2:	e8 99 eb ff ff       	callq  401d40 <getpid@plt>
  4031a7:	89 c6                	mov    %eax,%esi
  4031a9:	48 8b 55 c8          	mov    -0x38(%rbp),%rdx
  4031ad:	48 8b 45 d0          	mov    -0x30(%rbp),%rax
  4031b1:	48 89 d1             	mov    %rdx,%rcx
  4031b4:	ba 48 8a 40 00       	mov    $0x408a48,%edx
  4031b9:	48 89 c7             	mov    %rax,%rdi
  4031bc:	e8 18 43 00 00       	callq  4074d9 <log_it>
  4031c1:	48 8b 55 d0          	mov    -0x30(%rbp),%rdx
  4031c5:	48 8b 75 b8          	mov    -0x48(%rbp),%rsi
  4031c9:	48 8b 45 b0          	mov    -0x50(%rbp),%rax
  4031cd:	b9 00 00 00 00       	mov    $0x0,%ecx
  4031d2:	48 89 c7             	mov    %rax,%rdi
  4031d5:	e8 ab 05 00 00       	callq  403785 <force_rescan_user>
  4031da:	e9 e8 03 00 00       	jmpq   4035c7 <process_crontab+0x614>
  4031df:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  4031e3:	48 8b 40 10          	mov    0x10(%rax),%rax
  4031e7:	48 83 f8 01          	cmp    $0x1,%rax
  4031eb:	0f 84 c7 02 00 00    	je     4034b8 <process_crontab+0x505>
  4031f1:	e8 4a eb ff ff       	callq  401d40 <getpid@plt>
  4031f6:	89 c6                	mov    %eax,%esi
  4031f8:	48 8b 55 c8          	mov    -0x38(%rbp),%rdx
  4031fc:	48 8b 45 d0          	mov    -0x30(%rbp),%rax
  403200:	48 89 d1             	mov    %rdx,%rcx
  403203:	ba 6b 8a 40 00       	mov    $0x408a6b,%edx
  403208:	48 89 c7             	mov    %rax,%rdi
  40320b:	e8 c9 42 00 00       	callq  4074d9 <log_it>
  403210:	48 8b 55 d0          	mov    -0x30(%rbp),%rdx
  403214:	48 8b 75 b8          	mov    -0x48(%rbp),%rsi
  403218:	48 8b 45 b0          	mov    -0x50(%rbp),%rax
  40321c:	b9 00 00 00 00       	mov    $0x0,%ecx
  403221:	48 89 c7             	mov    %rax,%rdi
  403224:	e8 5c 05 00 00       	callq  403785 <force_rescan_user>
  403229:	e9 99 03 00 00       	jmpq   4035c7 <process_crontab+0x614>
  40322e:	48 8b 55 c0          	mov    -0x40(%rbp),%rdx
  403232:	48 8b 45 c8          	mov    -0x38(%rbp),%rax
  403236:	48 89 d6             	mov    %rdx,%rsi
  403239:	48 89 c7             	mov    %rax,%rdi
  40323c:	e8 df 55 00 00       	callq  408820 <__lstat>
  403241:	85 c0                	test   %eax,%eax
  403243:	79 24                	jns    403269 <process_crontab+0x2b6>
  403245:	e8 f6 ea ff ff       	callq  401d40 <getpid@plt>
  40324a:	89 c6                	mov    %eax,%esi
  40324c:	48 8b 55 c8          	mov    -0x38(%rbp),%rdx
  403250:	48 8b 45 d0          	mov    -0x30(%rbp),%rax
  403254:	48 89 d1             	mov    %rdx,%rcx
  403257:	ba 84 8a 40 00       	mov    $0x408a84,%edx
  40325c:	48 89 c7             	mov    %rax,%rdi
  40325f:	e8 75 42 00 00       	callq  4074d9 <log_it>
  403264:	e9 5e 03 00 00       	jmpq   4035c7 <process_crontab+0x614>
  403269:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  40326d:	8b 40 18             	mov    0x18(%rax),%eax
  403270:	25 00 f0 00 00       	and    $0xf000,%eax
  403275:	3d 00 a0 00 00       	cmp    $0xa000,%eax
  40327a:	75 48                	jne    4032c4 <process_crontab+0x311>
  40327c:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  403280:	8b 40 1c             	mov    0x1c(%rax),%eax
  403283:	85 c0                	test   %eax,%eax
  403285:	74 3d                	je     4032c4 <process_crontab+0x311>
  403287:	e8 b4 ea ff ff       	callq  401d40 <getpid@plt>
  40328c:	89 c6                	mov    %eax,%esi
  40328e:	48 8b 55 c8          	mov    -0x38(%rbp),%rdx
  403292:	48 8b 45 d0          	mov    -0x30(%rbp),%rax
  403296:	48 89 d1             	mov    %rdx,%rcx
  403299:	ba 91 8a 40 00       	mov    $0x408a91,%edx
  40329e:	48 89 c7             	mov    %rax,%rdi
  4032a1:	e8 33 42 00 00       	callq  4074d9 <log_it>
  4032a6:	48 8b 55 d0          	mov    -0x30(%rbp),%rdx
  4032aa:	48 8b 75 b8          	mov    -0x48(%rbp),%rsi
  4032ae:	48 8b 45 b0          	mov    -0x50(%rbp),%rax
  4032b2:	b9 00 00 00 00       	mov    $0x0,%ecx
  4032b7:	48 89 c7             	mov    %rax,%rdi
  4032ba:	e8 c6 04 00 00       	callq  403785 <force_rescan_user>
  4032bf:	e9 03 03 00 00       	jmpq   4035c7 <process_crontab+0x614>
  4032c4:	48 8b 45 c8          	mov    -0x38(%rbp),%rax
  4032c8:	ba 00 00 00 00       	mov    $0x0,%edx
  4032cd:	be 00 00 00 00       	mov    $0x0,%esi
  4032d2:	48 89 c7             	mov    %rax,%rdi
  4032d5:	b8 00 00 00 00       	mov    $0x0,%eax
  4032da:	e8 e1 ed ff ff       	callq  4020c0 <open@plt>
  4032df:	89 45 f4             	mov    %eax,-0xc(%rbp)
  4032e2:	83 7d f4 00          	cmpl   $0x0,-0xc(%rbp)
  4032e6:	79 74                	jns    40335c <process_crontab+0x3a9>
  4032e8:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  4032ec:	8b 40 18             	mov    0x18(%rax),%eax
  4032ef:	25 00 f0 00 00       	and    $0xf000,%eax
  4032f4:	3d 00 a0 00 00       	cmp    $0xa000,%eax
  4032f9:	75 3d                	jne    403338 <process_crontab+0x385>
  4032fb:	e8 40 ea ff ff       	callq  401d40 <getpid@plt>
  403300:	89 c6                	mov    %eax,%esi
  403302:	48 8b 55 c8          	mov    -0x38(%rbp),%rdx
  403306:	48 8b 45 d0          	mov    -0x30(%rbp),%rax
  40330a:	48 89 d1             	mov    %rdx,%rcx
  40330d:	ba a5 8a 40 00       	mov    $0x408aa5,%edx
  403312:	48 89 c7             	mov    %rax,%rdi
  403315:	e8 bf 41 00 00       	callq  4074d9 <log_it>
  40331a:	48 8b 55 d0          	mov    -0x30(%rbp),%rdx
  40331e:	48 8b 75 b8          	mov    -0x48(%rbp),%rsi
  403322:	48 8b 45 b0          	mov    -0x50(%rbp),%rax
  403326:	b9 00 00 00 00       	mov    $0x0,%ecx
  40332b:	48 89 c7             	mov    %rax,%rdi
  40332e:	e8 52 04 00 00       	callq  403785 <force_rescan_user>
  403333:	e9 8f 02 00 00       	jmpq   4035c7 <process_crontab+0x614>
  403338:	e8 03 ea ff ff       	callq  401d40 <getpid@plt>
  40333d:	89 c6                	mov    %eax,%esi
  40333f:	48 8b 55 c8          	mov    -0x38(%rbp),%rdx
  403343:	48 8b 45 d0          	mov    -0x30(%rbp),%rax
  403347:	48 89 d1             	mov    %rdx,%rcx
  40334a:	ba 0a 8a 40 00       	mov    $0x408a0a,%edx
  40334f:	48 89 c7             	mov    %rax,%rdi
  403352:	e8 82 41 00 00       	callq  4074d9 <log_it>
  403357:	e9 6b 02 00 00       	jmpq   4035c7 <process_crontab+0x614>
  40335c:	48 8b 55 c0          	mov    -0x40(%rbp),%rdx
  403360:	8b 45 f4             	mov    -0xc(%rbp),%eax
  403363:	48 89 d6             	mov    %rdx,%rsi
  403366:	89 c7                	mov    %eax,%edi
  403368:	e8 a3 54 00 00       	callq  408810 <__fstat>
  40336d:	85 c0                	test   %eax,%eax
  40336f:	79 24                	jns    403395 <process_crontab+0x3e2>
  403371:	e8 ca e9 ff ff       	callq  401d40 <getpid@plt>
  403376:	89 c6                	mov    %eax,%esi
  403378:	48 8b 55 c8          	mov    -0x38(%rbp),%rdx
  40337c:	48 8b 45 d0          	mov    -0x30(%rbp),%rax
  403380:	48 89 d1             	mov    %rdx,%rcx
  403383:	ba 15 8a 40 00       	mov    $0x408a15,%edx
  403388:	48 89 c7             	mov    %rax,%rdi
  40338b:	e8 49 41 00 00       	callq  4074d9 <log_it>
  403390:	e9 32 02 00 00       	jmpq   4035c7 <process_crontab+0x614>
  403395:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  403399:	8b 40 1c             	mov    0x1c(%rax),%eax
  40339c:	85 c0                	test   %eax,%eax
  40339e:	74 3d                	je     4033dd <process_crontab+0x42a>
  4033a0:	e8 9b e9 ff ff       	callq  401d40 <getpid@plt>
  4033a5:	89 c6                	mov    %eax,%esi
  4033a7:	48 8b 55 c8          	mov    -0x38(%rbp),%rdx
  4033ab:	48 8b 45 d0          	mov    -0x30(%rbp),%rax
  4033af:	48 89 d1             	mov    %rdx,%rcx
  4033b2:	ba 22 8a 40 00       	mov    $0x408a22,%edx
  4033b7:	48 89 c7             	mov    %rax,%rdi
  4033ba:	e8 1a 41 00 00       	callq  4074d9 <log_it>
  4033bf:	48 8b 55 d0          	mov    -0x30(%rbp),%rdx
  4033c3:	48 8b 75 b8          	mov    -0x48(%rbp),%rsi
  4033c7:	48 8b 45 b0          	mov    -0x50(%rbp),%rax
  4033cb:	b9 00 00 00 00       	mov    $0x0,%ecx
  4033d0:	48 89 c7             	mov    %rax,%rdi
  4033d3:	e8 ad 03 00 00       	callq  403785 <force_rescan_user>
  4033d8:	e9 ea 01 00 00       	jmpq   4035c7 <process_crontab+0x614>
  4033dd:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  4033e1:	8b 40 18             	mov    0x18(%rax),%eax
  4033e4:	25 00 f0 00 00       	and    $0xf000,%eax
  4033e9:	3d 00 80 00 00       	cmp    $0x8000,%eax
  4033ee:	74 24                	je     403414 <process_crontab+0x461>
  4033f0:	e8 4b e9 ff ff       	callq  401d40 <getpid@plt>
  4033f5:	89 c6                	mov    %eax,%esi
  4033f7:	48 8b 55 c8          	mov    -0x38(%rbp),%rdx
  4033fb:	48 8b 45 d0          	mov    -0x30(%rbp),%rax
  4033ff:	48 89 d1             	mov    %rdx,%rcx
  403402:	ba 33 8a 40 00       	mov    $0x408a33,%edx
  403407:	48 89 c7             	mov    %rax,%rdi
  40340a:	e8 ca 40 00 00       	callq  4074d9 <log_it>
  40340f:	e9 b3 01 00 00       	jmpq   4035c7 <process_crontab+0x614>
  403414:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  403418:	8b 40 18             	mov    0x18(%rax),%eax
  40341b:	83 e0 10             	and    $0x10,%eax
  40341e:	85 c0                	test   %eax,%eax
  403420:	75 0e                	jne    403430 <process_crontab+0x47d>
  403422:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  403426:	8b 40 18             	mov    0x18(%rax),%eax
  403429:	83 e0 02             	and    $0x2,%eax
  40342c:	85 c0                	test   %eax,%eax
  40342e:	74 3d                	je     40346d <process_crontab+0x4ba>
  403430:	e8 0b e9 ff ff       	callq  401d40 <getpid@plt>
  403435:	89 c6                	mov    %eax,%esi
  403437:	48 8b 55 c8          	mov    -0x38(%rbp),%rdx
  40343b:	48 8b 45 d0          	mov    -0x30(%rbp),%rax
  40343f:	48 89 d1             	mov    %rdx,%rcx
  403442:	ba b8 8a 40 00       	mov    $0x408ab8,%edx
  403447:	48 89 c7             	mov    %rax,%rdi
  40344a:	e8 8a 40 00 00       	callq  4074d9 <log_it>
  40344f:	48 8b 55 d0          	mov    -0x30(%rbp),%rdx
  403453:	48 8b 75 b8          	mov    -0x48(%rbp),%rsi
  403457:	48 8b 45 b0          	mov    -0x50(%rbp),%rax
  40345b:	b9 00 00 00 00       	mov    $0x0,%ecx
  403460:	48 89 c7             	mov    %rax,%rdi
  403463:	e8 1d 03 00 00       	callq  403785 <force_rescan_user>
  403468:	e9 5a 01 00 00       	jmpq   4035c7 <process_crontab+0x614>
  40346d:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  403471:	48 8b 40 10          	mov    0x10(%rax),%rax
  403475:	48 83 f8 01          	cmp    $0x1,%rax
  403479:	74 3d                	je     4034b8 <process_crontab+0x505>
  40347b:	e8 c0 e8 ff ff       	callq  401d40 <getpid@plt>
  403480:	89 c6                	mov    %eax,%esi
  403482:	48 8b 55 c8          	mov    -0x38(%rbp),%rdx
  403486:	48 8b 45 d0          	mov    -0x30(%rbp),%rax
  40348a:	48 89 d1             	mov    %rdx,%rcx
  40348d:	ba 6b 8a 40 00       	mov    $0x408a6b,%edx
  403492:	48 89 c7             	mov    %rax,%rdi
  403495:	e8 3f 40 00 00       	callq  4074d9 <log_it>
  40349a:	48 8b 55 d0          	mov    -0x30(%rbp),%rdx
  40349e:	48 8b 75 b8          	mov    -0x48(%rbp),%rsi
  4034a2:	48 8b 45 b0          	mov    -0x50(%rbp),%rax
  4034a6:	b9 00 00 00 00       	mov    $0x0,%ecx
  4034ab:	48 89 c7             	mov    %rax,%rdi
  4034ae:	e8 d2 02 00 00       	callq  403785 <force_rescan_user>
  4034b3:	e9 0f 01 00 00       	jmpq   4035c7 <process_crontab+0x614>
  4034b8:	48 83 7d b0 00       	cmpq   $0x0,-0x50(%rbp)
  4034bd:	74 17                	je     4034d6 <process_crontab+0x523>
  4034bf:	48 8b 55 d0          	mov    -0x30(%rbp),%rdx
  4034c3:	48 8b 45 b0          	mov    -0x50(%rbp),%rax
  4034c7:	48 89 d6             	mov    %rdx,%rsi
  4034ca:	48 89 c7             	mov    %rax,%rdi
  4034cd:	e8 8e fa ff ff       	callq  402f60 <find_user>
  4034d2:	48 89 45 e8          	mov    %rax,-0x18(%rbp)
  4034d6:	48 83 7d e8 00       	cmpq   $0x0,-0x18(%rbp)
  4034db:	74 7e                	je     40355b <process_crontab+0x5a8>
  4034dd:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  4034e1:	48 8b 50 18          	mov    0x18(%rax),%rdx
  4034e5:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  4034e9:	48 8b 40 58          	mov    0x58(%rax),%rax
  4034ed:	48 39 c2             	cmp    %rax,%rdx
  4034f0:	75 2b                	jne    40351d <process_crontab+0x56a>
  4034f2:	48 8b 55 e8          	mov    -0x18(%rbp),%rdx
  4034f6:	48 8b 45 b0          	mov    -0x50(%rbp),%rax
  4034fa:	48 89 d6             	mov    %rdx,%rsi
  4034fd:	48 89 c7             	mov    %rax,%rdi
  403500:	e8 ec f9 ff ff       	callq  402ef1 <unlink_user>
  403505:	48 8b 55 e8          	mov    -0x18(%rbp),%rdx
  403509:	48 8b 45 b8          	mov    -0x48(%rbp),%rax
  40350d:	48 89 d6             	mov    %rdx,%rsi
  403510:	48 89 c7             	mov    %rax,%rdi
  403513:	e8 70 f9 ff ff       	callq  402e88 <link_user>
  403518:	e9 aa 00 00 00       	jmpq   4035c7 <process_crontab+0x614>
  40351d:	48 8b 55 e8          	mov    -0x18(%rbp),%rdx
  403521:	48 8b 45 b0          	mov    -0x50(%rbp),%rax
  403525:	48 89 d6             	mov    %rdx,%rsi
  403528:	48 89 c7             	mov    %rax,%rdi
  40352b:	e8 c1 f9 ff ff       	callq  402ef1 <unlink_user>
  403530:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  403534:	48 89 c7             	mov    %rax,%rdi
  403537:	e8 15 06 00 00       	callq  403b51 <free_user>
  40353c:	e8 ff e7 ff ff       	callq  401d40 <getpid@plt>
  403541:	89 c6                	mov    %eax,%esi
  403543:	48 8b 55 c8          	mov    -0x38(%rbp),%rdx
  403547:	48 8b 45 d0          	mov    -0x30(%rbp),%rax
  40354b:	48 89 d1             	mov    %rdx,%rcx
  40354e:	ba dd 8a 40 00       	mov    $0x408add,%edx
  403553:	48 89 c7             	mov    %rax,%rdi
  403556:	e8 7e 3f 00 00       	callq  4074d9 <log_it>
  40355b:	48 8b 7d c8          	mov    -0x38(%rbp),%rdi
  40355f:	48 8b 4d d0          	mov    -0x30(%rbp),%rcx
  403563:	48 8b 55 d8          	mov    -0x28(%rbp),%rdx
  403567:	48 8b 75 f8          	mov    -0x8(%rbp),%rsi
  40356b:	8b 45 f4             	mov    -0xc(%rbp),%eax
  40356e:	49 89 f8             	mov    %rdi,%r8
  403571:	89 c7                	mov    %eax,%edi
  403573:	e8 38 06 00 00       	callq  403bb0 <load_user>
  403578:	48 89 45 e8          	mov    %rax,-0x18(%rbp)
  40357c:	48 83 7d e8 00       	cmpq   $0x0,-0x18(%rbp)
  403581:	74 25                	je     4035a8 <process_crontab+0x5f5>
  403583:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  403587:	48 8b 50 58          	mov    0x58(%rax),%rdx
  40358b:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  40358f:	48 89 50 18          	mov    %rdx,0x18(%rax)
  403593:	48 8b 55 e8          	mov    -0x18(%rbp),%rdx
  403597:	48 8b 45 b8          	mov    -0x48(%rbp),%rax
  40359b:	48 89 d6             	mov    %rdx,%rsi
  40359e:	48 89 c7             	mov    %rax,%rdi
  4035a1:	e8 e2 f8 ff ff       	callq  402e88 <link_user>
  4035a6:	eb 1f                	jmp    4035c7 <process_crontab+0x614>
  4035a8:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  4035ac:	48 8b 48 58          	mov    0x58(%rax),%rcx
  4035b0:	48 8b 55 d0          	mov    -0x30(%rbp),%rdx
  4035b4:	48 8b 75 b8          	mov    -0x48(%rbp),%rsi
  4035b8:	48 8b 45 b0          	mov    -0x50(%rbp),%rax
  4035bc:	48 89 c7             	mov    %rax,%rdi
  4035bf:	e8 c1 01 00 00       	callq  403785 <force_rescan_user>
  4035c4:	eb 01                	jmp    4035c7 <process_crontab+0x614>
  4035c6:	90                   	nop
  4035c7:	83 7d f4 00          	cmpl   $0x0,-0xc(%rbp)
  4035cb:	78 0a                	js     4035d7 <process_crontab+0x624>
  4035cd:	8b 45 f4             	mov    -0xc(%rbp),%eax
  4035d0:	89 c7                	mov    %eax,%edi
  4035d2:	e8 69 e8 ff ff       	callq  401e40 <close@plt>
  4035d7:	90                   	nop
  4035d8:	c9                   	leaveq 
  4035d9:	c3                   	retq   

00000000004035da <valid_name>:
  4035da:	55                   	push   %rbp
  4035db:	48 89 e5             	mov    %rsp,%rbp
  4035de:	48 83 ec 10          	sub    $0x10,%rsp
  4035e2:	48 89 7d f8          	mov    %rdi,-0x8(%rbp)
  4035e6:	8b 05 1c 86 20 00    	mov    0x20861c(%rip),%eax        # 60bc08 <donere.5167>
  4035ec:	85 c0                	test   %eax,%eax
  4035ee:	0f 85 8f 00 00 00    	jne    403683 <valid_name+0xa9>
  4035f4:	c7 05 0a 86 20 00 01 	movl   $0x1,0x20860a(%rip)        # 60bc08 <donere.5167>
  4035fb:	00 00 00 
  4035fe:	ba 09 00 00 00       	mov    $0x9,%edx
  403603:	be e4 8a 40 00       	mov    $0x408ae4,%esi
  403608:	bf 20 bc 60 00       	mov    $0x60bc20,%edi
  40360d:	e8 7e e8 ff ff       	callq  401e90 <regcomp@plt>
  403612:	85 c0                	test   %eax,%eax
  403614:	75 48                	jne    40365e <valid_name+0x84>
  403616:	ba 09 00 00 00       	mov    $0x9,%edx
  40361b:	be 01 8b 40 00       	mov    $0x408b01,%esi
  403620:	bf 60 bc 60 00       	mov    $0x60bc60,%edi
  403625:	e8 66 e8 ff ff       	callq  401e90 <regcomp@plt>
  40362a:	85 c0                	test   %eax,%eax
  40362c:	75 30                	jne    40365e <valid_name+0x84>
  40362e:	ba 08 00 00 00       	mov    $0x8,%edx
  403633:	be 1e 8b 40 00       	mov    $0x408b1e,%esi
  403638:	bf a0 bc 60 00       	mov    $0x60bca0,%edi
  40363d:	e8 4e e8 ff ff       	callq  401e90 <regcomp@plt>
  403642:	85 c0                	test   %eax,%eax
  403644:	75 18                	jne    40365e <valid_name+0x84>
  403646:	ba 09 00 00 00       	mov    $0x9,%edx
  40364b:	be 33 8b 40 00       	mov    $0x408b33,%esi
  403650:	bf e0 bc 60 00       	mov    $0x60bce0,%edi
  403655:	e8 36 e8 ff ff       	callq  401e90 <regcomp@plt>
  40365a:	85 c0                	test   %eax,%eax
  40365c:	74 25                	je     403683 <valid_name+0xa9>
  40365e:	e8 dd e6 ff ff       	callq  401d40 <getpid@plt>
  403663:	b9 44 8b 40 00       	mov    $0x408b44,%ecx
  403668:	ba 4f 8b 40 00       	mov    $0x408b4f,%edx
  40366d:	89 c6                	mov    %eax,%esi
  40366f:	bf ad 89 40 00       	mov    $0x4089ad,%edi
  403674:	e8 60 3e 00 00       	callq  4074d9 <log_it>
  403679:	bf 01 00 00 00       	mov    $0x1,%edi
  40367e:	e8 3d eb ff ff       	callq  4021c0 <exit@plt>
  403683:	8b 05 f7 8e 20 00    	mov    0x208ef7(%rip),%eax        # 60c580 <lsbsysinit_mode>
  403689:	85 c0                	test   %eax,%eax
  40368b:	74 73                	je     403700 <valid_name+0x126>
  40368d:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  403691:	41 b8 00 00 00 00    	mov    $0x0,%r8d
  403697:	b9 00 00 00 00       	mov    $0x0,%ecx
  40369c:	ba 00 00 00 00       	mov    $0x0,%edx
  4036a1:	48 89 c6             	mov    %rax,%rsi
  4036a4:	bf 20 bc 60 00       	mov    $0x60bc20,%edi
  4036a9:	e8 92 e9 ff ff       	callq  402040 <regexec@plt>
  4036ae:	85 c0                	test   %eax,%eax
  4036b0:	75 23                	jne    4036d5 <valid_name+0xfb>
  4036b2:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  4036b6:	41 b8 00 00 00 00    	mov    $0x0,%r8d
  4036bc:	b9 00 00 00 00       	mov    $0x0,%ecx
  4036c1:	ba 00 00 00 00       	mov    $0x0,%edx
  4036c6:	48 89 c6             	mov    %rax,%rsi
  4036c9:	bf 60 bc 60 00       	mov    $0x60bc60,%edi
  4036ce:	e8 6d e9 ff ff       	callq  402040 <regexec@plt>
  4036d3:	eb 54                	jmp    403729 <valid_name+0x14f>
  4036d5:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  4036d9:	41 b8 00 00 00 00    	mov    $0x0,%r8d
  4036df:	b9 00 00 00 00       	mov    $0x0,%ecx
  4036e4:	ba 00 00 00 00       	mov    $0x0,%edx
  4036e9:	48 89 c6             	mov    %rax,%rsi
  4036ec:	bf a0 bc 60 00       	mov    $0x60bca0,%edi
  4036f1:	e8 4a e9 ff ff       	callq  402040 <regexec@plt>
  4036f6:	85 c0                	test   %eax,%eax
  4036f8:	0f 94 c0             	sete   %al
  4036fb:	0f b6 c0             	movzbl %al,%eax
  4036fe:	eb 29                	jmp    403729 <valid_name+0x14f>
  403700:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  403704:	41 b8 00 00 00 00    	mov    $0x0,%r8d
  40370a:	b9 00 00 00 00       	mov    $0x0,%ecx
  40370f:	ba 00 00 00 00       	mov    $0x0,%edx
  403714:	48 89 c6             	mov    %rax,%rsi
  403717:	bf e0 bc 60 00       	mov    $0x60bce0,%edi
  40371c:	e8 1f e9 ff ff       	callq  402040 <regexec@plt>
  403721:	85 c0                	test   %eax,%eax
  403723:	0f 94 c0             	sete   %al
  403726:	0f b6 c0             	movzbl %al,%eax
  403729:	c9                   	leaveq 
  40372a:	c3                   	retq   

000000000040372b <get_next_system_crontab>:
  40372b:	55                   	push   %rbp
  40372c:	48 89 e5             	mov    %rsp,%rbp
  40372f:	48 83 ec 10          	sub    $0x10,%rsp
  403733:	48 89 7d f8          	mov    %rdi,-0x8(%rbp)
  403737:	eb 3c                	jmp    403775 <get_next_system_crontab+0x4a>
  403739:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  40373d:	48 8b 40 10          	mov    0x10(%rax),%rax
  403741:	ba 08 00 00 00       	mov    $0x8,%edx
  403746:	be d1 89 40 00       	mov    $0x4089d1,%esi
  40374b:	48 89 c7             	mov    %rax,%rdi
  40374e:	e8 4d e5 ff ff       	callq  401ca0 <strncmp@plt>
  403753:	85 c0                	test   %eax,%eax
  403755:	75 13                	jne    40376a <get_next_system_crontab+0x3f>
  403757:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  40375b:	48 8b 40 10          	mov    0x10(%rax),%rax
  40375f:	48 83 c0 08          	add    $0x8,%rax
  403763:	0f b6 00             	movzbl (%rax),%eax
  403766:	84 c0                	test   %al,%al
  403768:	75 14                	jne    40377e <get_next_system_crontab+0x53>
  40376a:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  40376e:	48 8b 00             	mov    (%rax),%rax
  403771:	48 89 45 f8          	mov    %rax,-0x8(%rbp)
  403775:	48 83 7d f8 00       	cmpq   $0x0,-0x8(%rbp)
  40377a:	75 bd                	jne    403739 <get_next_system_crontab+0xe>
  40377c:	eb 01                	jmp    40377f <get_next_system_crontab+0x54>
  40377e:	90                   	nop
  40377f:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  403783:	c9                   	leaveq 
  403784:	c3                   	retq   

0000000000403785 <force_rescan_user>:
  403785:	55                   	push   %rbp
  403786:	48 89 e5             	mov    %rsp,%rbp
  403789:	48 83 ec 30          	sub    $0x30,%rsp
  40378d:	48 89 7d e8          	mov    %rdi,-0x18(%rbp)
  403791:	48 89 75 e0          	mov    %rsi,-0x20(%rbp)
  403795:	48 89 55 d8          	mov    %rdx,-0x28(%rbp)
  403799:	48 89 4d d0          	mov    %rcx,-0x30(%rbp)
  40379d:	48 8b 55 d8          	mov    -0x28(%rbp),%rdx
  4037a1:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  4037a5:	48 89 d6             	mov    %rdx,%rsi
  4037a8:	48 89 c7             	mov    %rax,%rdi
  4037ab:	e8 b0 f7 ff ff       	callq  402f60 <find_user>
  4037b0:	48 89 45 f8          	mov    %rax,-0x8(%rbp)
  4037b4:	48 83 7d f8 00       	cmpq   $0x0,-0x8(%rbp)
  4037b9:	74 1f                	je     4037da <force_rescan_user+0x55>
  4037bb:	48 8b 55 f8          	mov    -0x8(%rbp),%rdx
  4037bf:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  4037c3:	48 89 d6             	mov    %rdx,%rsi
  4037c6:	48 89 c7             	mov    %rax,%rdi
  4037c9:	e8 23 f7 ff ff       	callq  402ef1 <unlink_user>
  4037ce:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  4037d2:	48 89 c7             	mov    %rax,%rdi
  4037d5:	e8 77 03 00 00       	callq  403b51 <free_user>
  4037da:	bf 28 00 00 00       	mov    $0x28,%edi
  4037df:	e8 ec e7 ff ff       	callq  401fd0 <malloc@plt>
  4037e4:	48 89 45 f8          	mov    %rax,-0x8(%rbp)
  4037e8:	48 83 7d f8 00       	cmpq   $0x0,-0x8(%rbp)
  4037ed:	75 0b                	jne    4037fa <force_rescan_user+0x75>
  4037ef:	e8 8c e4 ff ff       	callq  401c80 <__errno_location@plt>
  4037f4:	c7 00 0c 00 00 00    	movl   $0xc,(%rax)
  4037fa:	48 8b 45 d8          	mov    -0x28(%rbp),%rax
  4037fe:	48 89 c7             	mov    %rax,%rdi
  403801:	e8 fa e9 ff ff       	callq  402200 <strdup@plt>
  403806:	48 89 c2             	mov    %rax,%rdx
  403809:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  40380d:	48 89 50 10          	mov    %rdx,0x10(%rax)
  403811:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  403815:	48 8b 40 10          	mov    0x10(%rax),%rax
  403819:	48 85 c0             	test   %rax,%rax
  40381c:	75 17                	jne    403835 <force_rescan_user+0xb0>
  40381e:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  403822:	48 89 c7             	mov    %rax,%rdi
  403825:	e8 06 e4 ff ff       	callq  401c30 <free@plt>
  40382a:	e8 51 e4 ff ff       	callq  401c80 <__errno_location@plt>
  40382f:	c7 00 0c 00 00 00    	movl   $0xc,(%rax)
  403835:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  403839:	48 8b 55 d0          	mov    -0x30(%rbp),%rdx
  40383d:	48 89 50 18          	mov    %rdx,0x18(%rax)
  403841:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  403845:	48 c7 40 20 00 00 00 	movq   $0x0,0x20(%rax)
  40384c:	00 
  40384d:	48 8b 55 f8          	mov    -0x8(%rbp),%rdx
  403851:	48 8b 45 e0          	mov    -0x20(%rbp),%rax
  403855:	48 89 d6             	mov    %rdx,%rsi
  403858:	48 89 c7             	mov    %rax,%rdi
  40385b:	e8 28 f6 ff ff       	callq  402e88 <link_user>
  403860:	90                   	nop
  403861:	c9                   	leaveq 
  403862:	c3                   	retq   

0000000000403863 <free_orphan>:
  403863:	55                   	push   %rbp
  403864:	48 89 e5             	mov    %rsp,%rbp
  403867:	48 83 ec 10          	sub    $0x10,%rsp
  40386b:	48 89 7d f8          	mov    %rdi,-0x8(%rbp)
  40386f:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  403873:	48 8b 40 18          	mov    0x18(%rax),%rax
  403877:	48 89 c7             	mov    %rax,%rdi
  40387a:	e8 b1 e3 ff ff       	callq  401c30 <free@plt>
  40387f:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  403883:	48 8b 40 10          	mov    0x10(%rax),%rax
  403887:	48 89 c7             	mov    %rax,%rdi
  40388a:	e8 a1 e3 ff ff       	callq  401c30 <free@plt>
  40388f:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  403893:	48 8b 40 08          	mov    0x8(%rax),%rax
  403897:	48 89 c7             	mov    %rax,%rdi
  40389a:	e8 91 e3 ff ff       	callq  401c30 <free@plt>
  40389f:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  4038a3:	48 89 c7             	mov    %rax,%rdi
  4038a6:	e8 85 e3 ff ff       	callq  401c30 <free@plt>
  4038ab:	90                   	nop
  4038ac:	c9                   	leaveq 
  4038ad:	c3                   	retq   

00000000004038ae <check_orphans>:
  4038ae:	55                   	push   %rbp
  4038af:	48 89 e5             	mov    %rsp,%rbp
  4038b2:	48 81 ec c0 00 00 00 	sub    $0xc0,%rsp
  4038b9:	48 89 bd 48 ff ff ff 	mov    %rdi,-0xb8(%rbp)
  4038c0:	48 c7 45 f8 00 00 00 	movq   $0x0,-0x8(%rbp)
  4038c7:	00 
  4038c8:	48 8b 05 31 83 20 00 	mov    0x208331(%rip),%rax        # 60bc00 <orphans>
  4038cf:	48 89 45 f0          	mov    %rax,-0x10(%rbp)
  4038d3:	e9 c2 00 00 00       	jmpq   40399a <check_orphans+0xec>
  4038d8:	48 8b 45 f0          	mov    -0x10(%rbp),%rax
  4038dc:	48 8b 40 08          	mov    0x8(%rax),%rax
  4038e0:	48 89 c7             	mov    %rax,%rdi
  4038e3:	e8 38 e6 ff ff       	callq  401f20 <getpwnam@plt>
  4038e8:	48 85 c0             	test   %rax,%rax
  4038eb:	0f 84 96 00 00 00    	je     403987 <check_orphans+0xd9>
  4038f1:	48 8b 45 f0          	mov    -0x10(%rbp),%rax
  4038f5:	48 8b 00             	mov    (%rax),%rax
  4038f8:	48 89 45 e8          	mov    %rax,-0x18(%rbp)
  4038fc:	48 83 7d f8 00       	cmpq   $0x0,-0x8(%rbp)
  403901:	75 0d                	jne    403910 <check_orphans+0x62>
  403903:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  403907:	48 89 05 f2 82 20 00 	mov    %rax,0x2082f2(%rip)        # 60bc00 <orphans>
  40390e:	eb 0b                	jmp    40391b <check_orphans+0x6d>
  403910:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  403914:	48 8b 55 e8          	mov    -0x18(%rbp),%rdx
  403918:	48 89 10             	mov    %rdx,(%rax)
  40391b:	48 8b 45 f0          	mov    -0x10(%rbp),%rax
  40391f:	48 8b 50 18          	mov    0x18(%rax),%rdx
  403923:	48 8b 45 f0          	mov    -0x10(%rbp),%rax
  403927:	48 8b 70 10          	mov    0x10(%rax),%rsi
  40392b:	48 8b 45 f0          	mov    -0x10(%rbp),%rax
  40392f:	48 8b 40 08          	mov    0x8(%rax),%rax
  403933:	48 8b bd 48 ff ff ff 	mov    -0xb8(%rbp),%rdi
  40393a:	48 8d 8d 50 ff ff ff 	lea    -0xb0(%rbp),%rcx
  403941:	41 b9 00 00 00 00    	mov    $0x0,%r9d
  403947:	49 89 f8             	mov    %rdi,%r8
  40394a:	48 89 c7             	mov    %rax,%rdi
  40394d:	e8 61 f6 ff ff       	callq  402fb3 <process_crontab>
  403952:	48 83 7d f8 00       	cmpq   $0x0,-0x8(%rbp)
  403957:	75 18                	jne    403971 <check_orphans+0xc3>
  403959:	48 8b 05 a0 82 20 00 	mov    0x2082a0(%rip),%rax        # 60bc00 <orphans>
  403960:	48 3b 45 e8          	cmp    -0x18(%rbp),%rax
  403964:	74 0b                	je     403971 <check_orphans+0xc3>
  403966:	48 8b 05 93 82 20 00 	mov    0x208293(%rip),%rax        # 60bc00 <orphans>
  40396d:	48 89 45 f8          	mov    %rax,-0x8(%rbp)
  403971:	48 8b 45 f0          	mov    -0x10(%rbp),%rax
  403975:	48 89 c7             	mov    %rax,%rdi
  403978:	e8 e6 fe ff ff       	callq  403863 <free_orphan>
  40397d:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  403981:	48 89 45 f0          	mov    %rax,-0x10(%rbp)
  403985:	eb 13                	jmp    40399a <check_orphans+0xec>
  403987:	48 8b 45 f0          	mov    -0x10(%rbp),%rax
  40398b:	48 89 45 f8          	mov    %rax,-0x8(%rbp)
  40398f:	48 8b 45 f0          	mov    -0x10(%rbp),%rax
  403993:	48 8b 00             	mov    (%rax),%rax
  403996:	48 89 45 f0          	mov    %rax,-0x10(%rbp)
  40399a:	48 83 7d f0 00       	cmpq   $0x0,-0x10(%rbp)
  40399f:	0f 85 33 ff ff ff    	jne    4038d8 <check_orphans+0x2a>
  4039a5:	90                   	nop
  4039a6:	c9                   	leaveq 
  4039a7:	c3                   	retq   

00000000004039a8 <add_orphan>:
  4039a8:	55                   	push   %rbp
  4039a9:	48 89 e5             	mov    %rsp,%rbp
  4039ac:	48 83 ec 30          	sub    $0x30,%rsp
  4039b0:	48 89 7d e8          	mov    %rdi,-0x18(%rbp)
  4039b4:	48 89 75 e0          	mov    %rsi,-0x20(%rbp)
  4039b8:	48 89 55 d8          	mov    %rdx,-0x28(%rbp)
  4039bc:	be 20 00 00 00       	mov    $0x20,%esi
  4039c1:	bf 01 00 00 00       	mov    $0x1,%edi
  4039c6:	e8 15 e5 ff ff       	callq  401ee0 <calloc@plt>
  4039cb:	48 89 45 f8          	mov    %rax,-0x8(%rbp)
  4039cf:	48 83 7d f8 00       	cmpq   $0x0,-0x8(%rbp)
  4039d4:	0f 84 b1 00 00 00    	je     403a8b <add_orphan+0xe3>
  4039da:	48 83 7d e8 00       	cmpq   $0x0,-0x18(%rbp)
  4039df:	74 24                	je     403a05 <add_orphan+0x5d>
  4039e1:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  4039e5:	48 89 c7             	mov    %rax,%rdi
  4039e8:	e8 13 e8 ff ff       	callq  402200 <strdup@plt>
  4039ed:	48 89 c2             	mov    %rax,%rdx
  4039f0:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  4039f4:	48 89 50 08          	mov    %rdx,0x8(%rax)
  4039f8:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  4039fc:	48 8b 40 08          	mov    0x8(%rax),%rax
  403a00:	48 85 c0             	test   %rax,%rax
  403a03:	74 71                	je     403a76 <add_orphan+0xce>
  403a05:	48 83 7d e0 00       	cmpq   $0x0,-0x20(%rbp)
  403a0a:	74 24                	je     403a30 <add_orphan+0x88>
  403a0c:	48 8b 45 e0          	mov    -0x20(%rbp),%rax
  403a10:	48 89 c7             	mov    %rax,%rdi
  403a13:	e8 e8 e7 ff ff       	callq  402200 <strdup@plt>
  403a18:	48 89 c2             	mov    %rax,%rdx
  403a1b:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  403a1f:	48 89 50 10          	mov    %rdx,0x10(%rax)
  403a23:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  403a27:	48 8b 40 10          	mov    0x10(%rax),%rax
  403a2b:	48 85 c0             	test   %rax,%rax
  403a2e:	74 49                	je     403a79 <add_orphan+0xd1>
  403a30:	48 83 7d d8 00       	cmpq   $0x0,-0x28(%rbp)
  403a35:	74 24                	je     403a5b <add_orphan+0xb3>
  403a37:	48 8b 45 d8          	mov    -0x28(%rbp),%rax
  403a3b:	48 89 c7             	mov    %rax,%rdi
  403a3e:	e8 bd e7 ff ff       	callq  402200 <strdup@plt>
  403a43:	48 89 c2             	mov    %rax,%rdx
  403a46:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  403a4a:	48 89 50 18          	mov    %rdx,0x18(%rax)
  403a4e:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  403a52:	48 8b 40 18          	mov    0x18(%rax),%rax
  403a56:	48 85 c0             	test   %rax,%rax
  403a59:	74 21                	je     403a7c <add_orphan+0xd4>
  403a5b:	48 8b 15 9e 81 20 00 	mov    0x20819e(%rip),%rdx        # 60bc00 <orphans>
  403a62:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  403a66:	48 89 10             	mov    %rdx,(%rax)
  403a69:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  403a6d:	48 89 05 8c 81 20 00 	mov    %rax,0x20818c(%rip)        # 60bc00 <orphans>
  403a74:	eb 16                	jmp    403a8c <add_orphan+0xe4>
  403a76:	90                   	nop
  403a77:	eb 04                	jmp    403a7d <add_orphan+0xd5>
  403a79:	90                   	nop
  403a7a:	eb 01                	jmp    403a7d <add_orphan+0xd5>
  403a7c:	90                   	nop
  403a7d:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  403a81:	48 89 c7             	mov    %rax,%rdi
  403a84:	e8 da fd ff ff       	callq  403863 <free_orphan>
  403a89:	eb 01                	jmp    403a8c <add_orphan+0xe4>
  403a8b:	90                   	nop
  403a8c:	c9                   	leaveq 
  403a8d:	c3                   	retq   

0000000000403a8e <crontab_error>:
  403a8e:	55                   	push   %rbp
  403a8f:	48 89 e5             	mov    %rsp,%rbp
  403a92:	48 83 ec 20          	sub    $0x20,%rsp
  403a96:	48 89 7d e8          	mov    %rdi,-0x18(%rbp)
  403a9a:	48 8b 05 7f 82 20 00 	mov    0x20827f(%rip),%rax        # 60bd20 <err_user>
  403aa1:	be 60 8b 40 00       	mov    $0x408b60,%esi
  403aa6:	48 89 c7             	mov    %rax,%rdi
  403aa9:	e8 52 e4 ff ff       	callq  401f00 <strcmp@plt>
  403aae:	85 c0                	test   %eax,%eax
  403ab0:	75 22                	jne    403ad4 <crontab_error+0x46>
  403ab2:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  403ab6:	b9 69 8b 40 00       	mov    $0x408b69,%ecx
  403abb:	48 89 c2             	mov    %rax,%rdx
  403abe:	be 76 8b 40 00       	mov    $0x408b76,%esi
  403ac3:	bf 4b 00 00 00       	mov    $0x4b,%edi
  403ac8:	b8 00 00 00 00       	mov    $0x0,%eax
  403acd:	e8 3e e5 ff ff       	callq  402010 <syslog@plt>
  403ad2:	eb 7a                	jmp    403b4e <crontab_error+0xc0>
  403ad4:	48 8b 05 45 82 20 00 	mov    0x208245(%rip),%rax        # 60bd20 <err_user>
  403adb:	ba 08 00 00 00       	mov    $0x8,%edx
  403ae0:	be 60 8b 40 00       	mov    $0x408b60,%esi
  403ae5:	48 89 c7             	mov    %rax,%rdi
  403ae8:	e8 b3 e1 ff ff       	callq  401ca0 <strncmp@plt>
  403aed:	85 c0                	test   %eax,%eax
  403aef:	75 38                	jne    403b29 <crontab_error+0x9b>
  403af1:	48 8b 05 28 82 20 00 	mov    0x208228(%rip),%rax        # 60bd20 <err_user>
  403af8:	48 83 c0 08          	add    $0x8,%rax
  403afc:	48 89 45 f8          	mov    %rax,-0x8(%rbp)
  403b00:	48 8b 55 f8          	mov    -0x8(%rbp),%rdx
  403b04:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  403b08:	49 89 d0             	mov    %rdx,%r8
  403b0b:	b9 92 8b 40 00       	mov    $0x408b92,%ecx
  403b10:	48 89 c2             	mov    %rax,%rdx
  403b13:	be a0 8b 40 00       	mov    $0x408ba0,%esi
  403b18:	bf 4b 00 00 00       	mov    $0x4b,%edi
  403b1d:	b8 00 00 00 00       	mov    $0x0,%eax
  403b22:	e8 e9 e4 ff ff       	callq  402010 <syslog@plt>
  403b27:	eb 25                	jmp    403b4e <crontab_error+0xc0>
  403b29:	48 8b 15 f0 81 20 00 	mov    0x2081f0(%rip),%rdx        # 60bd20 <err_user>
  403b30:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  403b34:	48 89 d1             	mov    %rdx,%rcx
  403b37:	48 89 c2             	mov    %rax,%rdx
  403b3a:	be c0 8b 40 00       	mov    $0x408bc0,%esi
  403b3f:	bf 4b 00 00 00       	mov    $0x4b,%edi
  403b44:	b8 00 00 00 00       	mov    $0x0,%eax
  403b49:	e8 c2 e4 ff ff       	callq  402010 <syslog@plt>
  403b4e:	90                   	nop
  403b4f:	c9                   	leaveq 
  403b50:	c3                   	retq   

0000000000403b51 <free_user>:
  403b51:	55                   	push   %rbp
  403b52:	48 89 e5             	mov    %rsp,%rbp
  403b55:	48 83 ec 20          	sub    $0x20,%rsp
  403b59:	48 89 7d e8          	mov    %rdi,-0x18(%rbp)
  403b5d:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  403b61:	48 8b 40 10          	mov    0x10(%rax),%rax
  403b65:	48 89 c7             	mov    %rax,%rdi
  403b68:	e8 c3 e0 ff ff       	callq  401c30 <free@plt>
  403b6d:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  403b71:	48 8b 40 20          	mov    0x20(%rax),%rax
  403b75:	48 89 45 f8          	mov    %rax,-0x8(%rbp)
  403b79:	eb 1f                	jmp    403b9a <free_user+0x49>
  403b7b:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  403b7f:	48 8b 00             	mov    (%rax),%rax
  403b82:	48 89 45 f0          	mov    %rax,-0x10(%rbp)
  403b86:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  403b8a:	48 89 c7             	mov    %rax,%rdi
  403b8d:	e8 b3 02 00 00       	callq  403e45 <free_entry>
  403b92:	48 8b 45 f0          	mov    -0x10(%rbp),%rax
  403b96:	48 89 45 f8          	mov    %rax,-0x8(%rbp)
  403b9a:	48 83 7d f8 00       	cmpq   $0x0,-0x8(%rbp)
  403b9f:	75 da                	jne    403b7b <free_user+0x2a>
  403ba1:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  403ba5:	48 89 c7             	mov    %rax,%rdi
  403ba8:	e8 83 e0 ff ff       	callq  401c30 <free@plt>
  403bad:	90                   	nop
  403bae:	c9                   	leaveq 
  403baf:	c3                   	retq   

0000000000403bb0 <load_user>:
  403bb0:	55                   	push   %rbp
  403bb1:	48 89 e5             	mov    %rsp,%rbp
  403bb4:	48 81 ec 50 04 00 00 	sub    $0x450,%rsp
  403bbb:	89 bd dc fb ff ff    	mov    %edi,-0x424(%rbp)
  403bc1:	48 89 b5 d0 fb ff ff 	mov    %rsi,-0x430(%rbp)
  403bc8:	48 89 95 c8 fb ff ff 	mov    %rdx,-0x438(%rbp)
  403bcf:	48 89 8d c0 fb ff ff 	mov    %rcx,-0x440(%rbp)
  403bd6:	4c 89 85 b8 fb ff ff 	mov    %r8,-0x448(%rbp)
  403bdd:	48 c7 45 f0 00 00 00 	movq   $0x0,-0x10(%rbp)
  403be4:	00 
  403be5:	8b 85 dc fb ff ff    	mov    -0x424(%rbp),%eax
  403beb:	be ed 8b 40 00       	mov    $0x408bed,%esi
  403bf0:	89 c7                	mov    %eax,%edi
  403bf2:	e8 99 e4 ff ff       	callq  402090 <fdopen@plt>
  403bf7:	48 89 45 e8          	mov    %rax,-0x18(%rbp)
  403bfb:	48 83 7d e8 00       	cmpq   $0x0,-0x18(%rbp)
  403c00:	75 14                	jne    403c16 <load_user+0x66>
  403c02:	bf f0 8b 40 00       	mov    $0x408bf0,%edi
  403c07:	e8 e4 e4 ff ff       	callq  4020f0 <perror@plt>
  403c0c:	b8 00 00 00 00       	mov    $0x0,%eax
  403c11:	e9 2d 02 00 00       	jmpq   403e43 <load_user+0x293>
  403c16:	bf 28 00 00 00       	mov    $0x28,%edi
  403c1b:	e8 b0 e3 ff ff       	callq  401fd0 <malloc@plt>
  403c20:	48 89 45 f8          	mov    %rax,-0x8(%rbp)
  403c24:	48 83 7d f8 00       	cmpq   $0x0,-0x8(%rbp)
  403c29:	75 15                	jne    403c40 <load_user+0x90>
  403c2b:	e8 50 e0 ff ff       	callq  401c80 <__errno_location@plt>
  403c30:	c7 00 0c 00 00 00    	movl   $0xc,(%rax)
  403c36:	b8 00 00 00 00       	mov    $0x0,%eax
  403c3b:	e9 03 02 00 00       	jmpq   403e43 <load_user+0x293>
  403c40:	48 8b 85 c0 fb ff ff 	mov    -0x440(%rbp),%rax
  403c47:	48 89 c7             	mov    %rax,%rdi
  403c4a:	e8 b1 e5 ff ff       	callq  402200 <strdup@plt>
  403c4f:	48 89 c2             	mov    %rax,%rdx
  403c52:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  403c56:	48 89 50 10          	mov    %rdx,0x10(%rax)
  403c5a:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  403c5e:	48 8b 40 10          	mov    0x10(%rax),%rax
  403c62:	48 85 c0             	test   %rax,%rax
  403c65:	75 21                	jne    403c88 <load_user+0xd8>
  403c67:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  403c6b:	48 89 c7             	mov    %rax,%rdi
  403c6e:	e8 bd df ff ff       	callq  401c30 <free@plt>
  403c73:	e8 08 e0 ff ff       	callq  401c80 <__errno_location@plt>
  403c78:	c7 00 0c 00 00 00    	movl   $0xc,(%rax)
  403c7e:	b8 00 00 00 00       	mov    $0x0,%eax
  403c83:	e9 bb 01 00 00       	jmpq   403e43 <load_user+0x293>
  403c88:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  403c8c:	48 c7 40 20 00 00 00 	movq   $0x0,0x20(%rax)
  403c93:	00 
  403c94:	e8 ea 3b 00 00       	callq  407883 <env_init>
  403c99:	48 89 45 f0          	mov    %rax,-0x10(%rbp)
  403c9d:	48 83 7d f0 00       	cmpq   $0x0,-0x10(%rbp)
  403ca2:	75 26                	jne    403cca <load_user+0x11a>
  403ca4:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  403ca8:	48 8b 40 10          	mov    0x10(%rax),%rax
  403cac:	48 89 c7             	mov    %rax,%rdi
  403caf:	e8 7c df ff ff       	callq  401c30 <free@plt>
  403cb4:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  403cb8:	48 89 c7             	mov    %rax,%rdi
  403cbb:	e8 70 df ff ff       	callq  401c30 <free@plt>
  403cc0:	b8 00 00 00 00       	mov    $0x0,%eax
  403cc5:	e9 79 01 00 00       	jmpq   403e43 <load_user+0x293>
  403cca:	48 8b 55 e8          	mov    -0x18(%rbp),%rdx
  403cce:	48 8d 85 e0 fb ff ff 	lea    -0x420(%rbp),%rax
  403cd5:	48 89 d6             	mov    %rdx,%rsi
  403cd8:	48 89 c7             	mov    %rax,%rdi
  403cdb:	e8 9d 3e 00 00       	callq  407b7d <load_env>
  403ce0:	89 45 e4             	mov    %eax,-0x1c(%rbp)
  403ce3:	8b 45 e4             	mov    -0x1c(%rbp),%eax
  403ce6:	85 c0                	test   %eax,%eax
  403ce8:	74 5b                	je     403d45 <load_user+0x195>
  403cea:	83 f8 01             	cmp    $0x1,%eax
  403ced:	0f 84 e6 00 00 00    	je     403dd9 <load_user+0x229>
  403cf3:	83 f8 ff             	cmp    $0xffffffff,%eax
  403cf6:	0f 85 1e 01 00 00    	jne    403e1a <load_user+0x26a>
  403cfc:	0f b6 85 e0 fb ff ff 	movzbl -0x420(%rbp),%eax
  403d03:	84 c0                	test   %al,%al
  403d05:	0f 84 1b 01 00 00    	je     403e26 <load_user+0x276>
  403d0b:	e8 30 e0 ff ff       	callq  401d40 <getpid@plt>
  403d10:	89 c6                	mov    %eax,%esi
  403d12:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  403d16:	48 8b 40 10          	mov    0x10(%rax),%rax
  403d1a:	b9 18 8c 40 00       	mov    $0x408c18,%ecx
  403d1f:	ba 56 8c 40 00       	mov    $0x408c56,%edx
  403d24:	48 89 c7             	mov    %rax,%rdi
  403d27:	e8 ad 37 00 00       	callq  4074d9 <log_it>
  403d2c:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  403d30:	48 89 c7             	mov    %rax,%rdi
  403d33:	e8 19 fe ff ff       	callq  403b51 <free_user>
  403d38:	48 c7 45 f8 00 00 00 	movq   $0x0,-0x8(%rbp)
  403d3f:	00 
  403d40:	e9 e1 00 00 00       	jmpq   403e26 <load_user+0x276>
  403d45:	48 8b 85 c0 fb ff ff 	mov    -0x440(%rbp),%rax
  403d4c:	48 89 05 cd 7f 20 00 	mov    %rax,0x207fcd(%rip)        # 60bd20 <err_user>
  403d53:	48 8b 4d f0          	mov    -0x10(%rbp),%rcx
  403d57:	48 8b 95 d0 fb ff ff 	mov    -0x430(%rbp),%rdx
  403d5e:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  403d62:	be 8e 3a 40 00       	mov    $0x403a8e,%esi
  403d67:	48 89 c7             	mov    %rax,%rdi
  403d6a:	e8 11 01 00 00       	callq  403e80 <load_entry>
  403d6f:	48 89 45 d8          	mov    %rax,-0x28(%rbp)
  403d73:	48 c7 05 a2 7f 20 00 	movq   $0x0,0x207fa2(%rip)        # 60bd20 <err_user>
  403d7a:	00 00 00 00 
  403d7e:	48 83 7d d8 00       	cmpq   $0x0,-0x28(%rbp)
  403d83:	74 1d                	je     403da2 <load_user+0x1f2>
  403d85:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  403d89:	48 8b 50 20          	mov    0x20(%rax),%rdx
  403d8d:	48 8b 45 d8          	mov    -0x28(%rbp),%rax
  403d91:	48 89 10             	mov    %rdx,(%rax)
  403d94:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  403d98:	48 8b 55 d8          	mov    -0x28(%rbp),%rdx
  403d9c:	48 89 50 20          	mov    %rdx,0x20(%rax)
  403da0:	eb 78                	jmp    403e1a <load_user+0x26a>
  403da2:	e8 99 df ff ff       	callq  401d40 <getpid@plt>
  403da7:	89 c6                	mov    %eax,%esi
  403da9:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  403dad:	48 8b 40 10          	mov    0x10(%rax),%rax
  403db1:	b9 60 8c 40 00       	mov    $0x408c60,%ecx
  403db6:	ba 56 8c 40 00       	mov    $0x408c56,%edx
  403dbb:	48 89 c7             	mov    %rax,%rdi
  403dbe:	e8 16 37 00 00       	callq  4074d9 <log_it>
  403dc3:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  403dc7:	48 89 c7             	mov    %rax,%rdi
  403dca:	e8 82 fd ff ff       	callq  403b51 <free_user>
  403dcf:	48 c7 45 f8 00 00 00 	movq   $0x0,-0x8(%rbp)
  403dd6:	00 
  403dd7:	eb 4e                	jmp    403e27 <load_user+0x277>
  403dd9:	48 8d 95 e0 fb ff ff 	lea    -0x420(%rbp),%rdx
  403de0:	48 8b 45 f0          	mov    -0x10(%rbp),%rax
  403de4:	48 89 d6             	mov    %rdx,%rsi
  403de7:	48 89 c7             	mov    %rax,%rdi
  403dea:	e8 04 3c 00 00       	callq  4079f3 <env_set>
  403def:	48 89 45 d0          	mov    %rax,-0x30(%rbp)
  403df3:	48 83 7d d0 00       	cmpq   $0x0,-0x30(%rbp)
  403df8:	74 0a                	je     403e04 <load_user+0x254>
  403dfa:	48 8b 45 d0          	mov    -0x30(%rbp),%rax
  403dfe:	48 89 45 f0          	mov    %rax,-0x10(%rbp)
  403e02:	eb 16                	jmp    403e1a <load_user+0x26a>
  403e04:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  403e08:	48 89 c7             	mov    %rax,%rdi
  403e0b:	e8 41 fd ff ff       	callq  403b51 <free_user>
  403e10:	48 c7 45 f8 00 00 00 	movq   $0x0,-0x8(%rbp)
  403e17:	00 
  403e18:	eb 0d                	jmp    403e27 <load_user+0x277>
  403e1a:	83 7d e4 00          	cmpl   $0x0,-0x1c(%rbp)
  403e1e:	0f 89 a6 fe ff ff    	jns    403cca <load_user+0x11a>
  403e24:	eb 01                	jmp    403e27 <load_user+0x277>
  403e26:	90                   	nop
  403e27:	48 8b 45 f0          	mov    -0x10(%rbp),%rax
  403e2b:	48 89 c7             	mov    %rax,%rdi
  403e2e:	e8 7c 3a 00 00       	callq  4078af <env_free>
  403e33:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  403e37:	48 89 c7             	mov    %rax,%rdi
  403e3a:	e8 11 df ff ff       	callq  401d50 <fclose@plt>
  403e3f:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  403e43:	c9                   	leaveq 
  403e44:	c3                   	retq   

0000000000403e45 <free_entry>:
  403e45:	55                   	push   %rbp
  403e46:	48 89 e5             	mov    %rsp,%rbp
  403e49:	48 83 ec 10          	sub    $0x10,%rsp
  403e4d:	48 89 7d f8          	mov    %rdi,-0x8(%rbp)
  403e51:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  403e55:	48 8b 40 18          	mov    0x18(%rax),%rax
  403e59:	48 89 c7             	mov    %rax,%rdi
  403e5c:	e8 cf dd ff ff       	callq  401c30 <free@plt>
  403e61:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  403e65:	48 8b 40 10          	mov    0x10(%rax),%rax
  403e69:	48 89 c7             	mov    %rax,%rdi
  403e6c:	e8 3e 3a 00 00       	callq  4078af <env_free>
  403e71:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  403e75:	48 89 c7             	mov    %rax,%rdi
  403e78:	e8 b3 dd ff ff       	callq  401c30 <free@plt>
  403e7d:	90                   	nop
  403e7e:	c9                   	leaveq 
  403e7f:	c3                   	retq   

0000000000403e80 <load_entry>:
  403e80:	55                   	push   %rbp
  403e81:	48 89 e5             	mov    %rsp,%rbp
  403e84:	41 57                	push   %r15
  403e86:	41 56                	push   %r14
  403e88:	41 55                	push   %r13
  403e8a:	41 54                	push   %r12
  403e8c:	53                   	push   %rbx
  403e8d:	48 81 ec 28 08 00 00 	sub    $0x828,%rsp
  403e94:	48 89 bd c8 f7 ff ff 	mov    %rdi,-0x838(%rbp)
  403e9b:	48 89 b5 c0 f7 ff ff 	mov    %rsi,-0x840(%rbp)
  403ea2:	48 89 95 b8 f7 ff ff 	mov    %rdx,-0x848(%rbp)
  403ea9:	48 89 8d b0 f7 ff ff 	mov    %rcx,-0x850(%rbp)
  403eb0:	c7 45 cc 00 00 00 00 	movl   $0x0,-0x34(%rbp)
  403eb7:	48 8b 85 c8 f7 ff ff 	mov    -0x838(%rbp),%rax
  403ebe:	48 89 c7             	mov    %rax,%rdi
  403ec1:	e8 03 34 00 00       	callq  4072c9 <skip_comments>
  403ec6:	48 8b 85 c8 f7 ff ff 	mov    -0x838(%rbp),%rax
  403ecd:	48 89 c7             	mov    %rax,%rdi
  403ed0:	e8 16 33 00 00       	callq  4071eb <get_char>
  403ed5:	89 45 c8             	mov    %eax,-0x38(%rbp)
  403ed8:	83 7d c8 ff          	cmpl   $0xffffffff,-0x38(%rbp)
  403edc:	75 0a                	jne    403ee8 <load_entry+0x68>
  403ede:	b8 00 00 00 00       	mov    $0x0,%eax
  403ee3:	e9 c0 12 00 00       	jmpq   4051a8 <load_entry+0x1328>
  403ee8:	be 01 00 00 00       	mov    $0x1,%esi
  403eed:	bf 38 00 00 00       	mov    $0x38,%edi
  403ef2:	e8 e9 df ff ff       	callq  401ee0 <calloc@plt>
  403ef7:	48 89 45 c0          	mov    %rax,-0x40(%rbp)
  403efb:	83 7d c8 40          	cmpl   $0x40,-0x38(%rbp)
  403eff:	0f 85 9e 0c 00 00    	jne    404ba3 <load_entry+0xd23>
  403f05:	48 8b 95 c8 f7 ff ff 	mov    -0x838(%rbp),%rdx
  403f0c:	48 8d 85 c0 fb ff ff 	lea    -0x440(%rbp),%rax
  403f13:	b9 04 8d 40 00       	mov    $0x408d04,%ecx
  403f18:	be e8 03 00 00       	mov    $0x3e8,%esi
  403f1d:	48 89 c7             	mov    %rax,%rdi
  403f20:	e8 33 33 00 00       	callq  407258 <get_string>
  403f25:	89 45 c8             	mov    %eax,-0x38(%rbp)
  403f28:	48 8d 85 c0 fb ff ff 	lea    -0x440(%rbp),%rax
  403f2f:	48 89 c6             	mov    %rax,%rsi
  403f32:	bf 08 8d 40 00       	mov    $0x408d08,%edi
  403f37:	e8 c4 df ff ff       	callq  401f00 <strcmp@plt>
  403f3c:	85 c0                	test   %eax,%eax
  403f3e:	75 18                	jne    403f58 <load_entry+0xd8>
  403f40:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  403f44:	8b 40 34             	mov    0x34(%rax),%eax
  403f47:	83 c8 04             	or     $0x4,%eax
  403f4a:	89 c2                	mov    %eax,%edx
  403f4c:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  403f50:	89 50 34             	mov    %edx,0x34(%rax)
  403f53:	e9 12 0e 00 00       	jmpq   404d6a <load_entry+0xeea>
  403f58:	48 8d 85 c0 fb ff ff 	lea    -0x440(%rbp),%rax
  403f5f:	48 89 c6             	mov    %rax,%rsi
  403f62:	bf 0f 8d 40 00       	mov    $0x408d0f,%edi
  403f67:	e8 94 df ff ff       	callq  401f00 <strcmp@plt>
  403f6c:	85 c0                	test   %eax,%eax
  403f6e:	74 1c                	je     403f8c <load_entry+0x10c>
  403f70:	48 8d 85 c0 fb ff ff 	lea    -0x440(%rbp),%rax
  403f77:	48 89 c6             	mov    %rax,%rsi
  403f7a:	bf 16 8d 40 00       	mov    $0x408d16,%edi
  403f7f:	e8 7c df ff ff       	callq  401f00 <strcmp@plt>
  403f84:	85 c0                	test   %eax,%eax
  403f86:	0f 85 30 01 00 00    	jne    4040bc <load_entry+0x23c>
  403f8c:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  403f90:	0f b6 40 20          	movzbl 0x20(%rax),%eax
  403f94:	83 c8 01             	or     $0x1,%eax
  403f97:	89 c2                	mov    %eax,%edx
  403f99:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  403f9d:	88 50 20             	mov    %dl,0x20(%rax)
  403fa0:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  403fa4:	0f b6 40 28          	movzbl 0x28(%rax),%eax
  403fa8:	83 c8 01             	or     $0x1,%eax
  403fab:	89 c2                	mov    %eax,%edx
  403fad:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  403fb1:	88 50 28             	mov    %dl,0x28(%rax)
  403fb4:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  403fb8:	0f b6 40 2b          	movzbl 0x2b(%rax),%eax
  403fbc:	83 c8 01             	or     $0x1,%eax
  403fbf:	89 c2                	mov    %eax,%edx
  403fc1:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  403fc5:	88 50 2b             	mov    %dl,0x2b(%rax)
  403fc8:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  403fcc:	0f b6 40 2f          	movzbl 0x2f(%rax),%eax
  403fd0:	83 c8 01             	or     $0x1,%eax
  403fd3:	89 c2                	mov    %eax,%edx
  403fd5:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  403fd9:	88 50 2f             	mov    %dl,0x2f(%rax)
  403fdc:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  403fe0:	4c 8d 60 31          	lea    0x31(%rax),%r12
  403fe4:	41 bf 00 00 00 00    	mov    $0x0,%r15d
  403fea:	41 be 08 00 00 00    	mov    $0x8,%r14d
  403ff0:	44 89 fb             	mov    %r15d,%ebx
  403ff3:	c1 fb 03             	sar    $0x3,%ebx
  403ff6:	45 89 f5             	mov    %r14d,%r13d
  403ff9:	41 c1 fd 03          	sar    $0x3,%r13d
  403ffd:	44 39 eb             	cmp    %r13d,%ebx
  404000:	75 41                	jne    404043 <load_entry+0x1c3>
  404002:	48 63 c3             	movslq %ebx,%rax
  404005:	4c 01 e0             	add    %r12,%rax
  404008:	48 63 d3             	movslq %ebx,%rdx
  40400b:	4c 01 e2             	add    %r12,%rdx
  40400e:	0f b6 12             	movzbl (%rdx),%edx
  404011:	41 89 d0             	mov    %edx,%r8d
  404014:	44 89 fa             	mov    %r15d,%edx
  404017:	83 e2 07             	and    $0x7,%edx
  40401a:	be ff 00 00 00       	mov    $0xff,%esi
  40401f:	89 d1                	mov    %edx,%ecx
  404021:	d3 e6                	shl    %cl,%esi
  404023:	89 f2                	mov    %esi,%edx
  404025:	89 d6                	mov    %edx,%esi
  404027:	44 89 f2             	mov    %r14d,%edx
  40402a:	f7 d2                	not    %edx
  40402c:	83 e2 07             	and    $0x7,%edx
  40402f:	bf ff 00 00 00       	mov    $0xff,%edi
  404034:	89 d1                	mov    %edx,%ecx
  404036:	d3 ff                	sar    %cl,%edi
  404038:	89 fa                	mov    %edi,%edx
  40403a:	21 f2                	and    %esi,%edx
  40403c:	44 09 c2             	or     %r8d,%edx
  40403f:	88 10                	mov    %dl,(%rax)
  404041:	eb 61                	jmp    4040a4 <load_entry+0x224>
  404043:	48 63 c3             	movslq %ebx,%rax
  404046:	4c 01 e0             	add    %r12,%rax
  404049:	48 63 d3             	movslq %ebx,%rdx
  40404c:	4c 01 e2             	add    %r12,%rdx
  40404f:	0f b6 12             	movzbl (%rdx),%edx
  404052:	89 d7                	mov    %edx,%edi
  404054:	44 89 fa             	mov    %r15d,%edx
  404057:	83 e2 07             	and    $0x7,%edx
  40405a:	be ff 00 00 00       	mov    $0xff,%esi
  40405f:	89 d1                	mov    %edx,%ecx
  404061:	d3 e6                	shl    %cl,%esi
  404063:	89 f2                	mov    %esi,%edx
  404065:	09 fa                	or     %edi,%edx
  404067:	88 10                	mov    %dl,(%rax)
  404069:	eb 09                	jmp    404074 <load_entry+0x1f4>
  40406b:	48 63 c3             	movslq %ebx,%rax
  40406e:	4c 01 e0             	add    %r12,%rax
  404071:	c6 00 ff             	movb   $0xff,(%rax)
  404074:	83 c3 01             	add    $0x1,%ebx
  404077:	44 39 eb             	cmp    %r13d,%ebx
  40407a:	7c ef                	jl     40406b <load_entry+0x1eb>
  40407c:	49 63 c5             	movslq %r13d,%rax
  40407f:	4c 01 e0             	add    %r12,%rax
  404082:	49 63 d5             	movslq %r13d,%rdx
  404085:	4c 01 e2             	add    %r12,%rdx
  404088:	0f b6 12             	movzbl (%rdx),%edx
  40408b:	89 d7                	mov    %edx,%edi
  40408d:	44 89 f2             	mov    %r14d,%edx
  404090:	f7 d2                	not    %edx
  404092:	83 e2 07             	and    $0x7,%edx
  404095:	be ff 00 00 00       	mov    $0xff,%esi
  40409a:	89 d1                	mov    %edx,%ecx
  40409c:	d3 fe                	sar    %cl,%esi
  40409e:	89 f2                	mov    %esi,%edx
  4040a0:	09 fa                	or     %edi,%edx
  4040a2:	88 10                	mov    %dl,(%rax)
  4040a4:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  4040a8:	8b 40 34             	mov    0x34(%rax),%eax
  4040ab:	83 c8 02             	or     $0x2,%eax
  4040ae:	89 c2                	mov    %eax,%edx
  4040b0:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  4040b4:	89 50 34             	mov    %edx,0x34(%rax)
  4040b7:	e9 ae 0c 00 00       	jmpq   404d6a <load_entry+0xeea>
  4040bc:	48 8d 85 c0 fb ff ff 	lea    -0x440(%rbp),%rax
  4040c3:	48 89 c6             	mov    %rax,%rsi
  4040c6:	bf 1f 8d 40 00       	mov    $0x408d1f,%edi
  4040cb:	e8 30 de ff ff       	callq  401f00 <strcmp@plt>
  4040d0:	85 c0                	test   %eax,%eax
  4040d2:	0f 85 e4 01 00 00    	jne    4042bc <load_entry+0x43c>
  4040d8:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  4040dc:	0f b6 40 20          	movzbl 0x20(%rax),%eax
  4040e0:	83 c8 01             	or     $0x1,%eax
  4040e3:	89 c2                	mov    %eax,%edx
  4040e5:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  4040e9:	88 50 20             	mov    %dl,0x20(%rax)
  4040ec:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  4040f0:	0f b6 40 28          	movzbl 0x28(%rax),%eax
  4040f4:	83 c8 01             	or     $0x1,%eax
  4040f7:	89 c2                	mov    %eax,%edx
  4040f9:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  4040fd:	88 50 28             	mov    %dl,0x28(%rax)
  404100:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  404104:	0f b6 40 2b          	movzbl 0x2b(%rax),%eax
  404108:	83 c8 01             	or     $0x1,%eax
  40410b:	89 c2                	mov    %eax,%edx
  40410d:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  404111:	88 50 2b             	mov    %dl,0x2b(%rax)
  404114:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  404118:	4c 8d 60 2f          	lea    0x2f(%rax),%r12
  40411c:	41 bf 00 00 00 00    	mov    $0x0,%r15d
  404122:	41 be 0c 00 00 00    	mov    $0xc,%r14d
  404128:	44 89 fb             	mov    %r15d,%ebx
  40412b:	c1 fb 03             	sar    $0x3,%ebx
  40412e:	45 89 f5             	mov    %r14d,%r13d
  404131:	41 c1 fd 03          	sar    $0x3,%r13d
  404135:	44 39 eb             	cmp    %r13d,%ebx
  404138:	75 41                	jne    40417b <load_entry+0x2fb>
  40413a:	48 63 c3             	movslq %ebx,%rax
  40413d:	4c 01 e0             	add    %r12,%rax
  404140:	48 63 d3             	movslq %ebx,%rdx
  404143:	4c 01 e2             	add    %r12,%rdx
  404146:	0f b6 12             	movzbl (%rdx),%edx
  404149:	41 89 d0             	mov    %edx,%r8d
  40414c:	44 89 fa             	mov    %r15d,%edx
  40414f:	83 e2 07             	and    $0x7,%edx
  404152:	be ff 00 00 00       	mov    $0xff,%esi
  404157:	89 d1                	mov    %edx,%ecx
  404159:	d3 e6                	shl    %cl,%esi
  40415b:	89 f2                	mov    %esi,%edx
  40415d:	89 d6                	mov    %edx,%esi
  40415f:	44 89 f2             	mov    %r14d,%edx
  404162:	f7 d2                	not    %edx
  404164:	83 e2 07             	and    $0x7,%edx
  404167:	bf ff 00 00 00       	mov    $0xff,%edi
  40416c:	89 d1                	mov    %edx,%ecx
  40416e:	d3 ff                	sar    %cl,%edi
  404170:	89 fa                	mov    %edi,%edx
  404172:	21 f2                	and    %esi,%edx
  404174:	44 09 c2             	or     %r8d,%edx
  404177:	88 10                	mov    %dl,(%rax)
  404179:	eb 61                	jmp    4041dc <load_entry+0x35c>
  40417b:	48 63 c3             	movslq %ebx,%rax
  40417e:	4c 01 e0             	add    %r12,%rax
  404181:	48 63 d3             	movslq %ebx,%rdx
  404184:	4c 01 e2             	add    %r12,%rdx
  404187:	0f b6 12             	movzbl (%rdx),%edx
  40418a:	89 d7                	mov    %edx,%edi
  40418c:	44 89 fa             	mov    %r15d,%edx
  40418f:	83 e2 07             	and    $0x7,%edx
  404192:	be ff 00 00 00       	mov    $0xff,%esi
  404197:	89 d1                	mov    %edx,%ecx
  404199:	d3 e6                	shl    %cl,%esi
  40419b:	89 f2                	mov    %esi,%edx
  40419d:	09 fa                	or     %edi,%edx
  40419f:	88 10                	mov    %dl,(%rax)
  4041a1:	eb 09                	jmp    4041ac <load_entry+0x32c>
  4041a3:	48 63 c3             	movslq %ebx,%rax
  4041a6:	4c 01 e0             	add    %r12,%rax
  4041a9:	c6 00 ff             	movb   $0xff,(%rax)
  4041ac:	83 c3 01             	add    $0x1,%ebx
  4041af:	44 39 eb             	cmp    %r13d,%ebx
  4041b2:	7c ef                	jl     4041a3 <load_entry+0x323>
  4041b4:	49 63 c5             	movslq %r13d,%rax
  4041b7:	4c 01 e0             	add    %r12,%rax
  4041ba:	49 63 d5             	movslq %r13d,%rdx
  4041bd:	4c 01 e2             	add    %r12,%rdx
  4041c0:	0f b6 12             	movzbl (%rdx),%edx
  4041c3:	89 d7                	mov    %edx,%edi
  4041c5:	44 89 f2             	mov    %r14d,%edx
  4041c8:	f7 d2                	not    %edx
  4041ca:	83 e2 07             	and    $0x7,%edx
  4041cd:	be ff 00 00 00       	mov    $0xff,%esi
  4041d2:	89 d1                	mov    %edx,%ecx
  4041d4:	d3 fe                	sar    %cl,%esi
  4041d6:	89 f2                	mov    %esi,%edx
  4041d8:	09 fa                	or     %edi,%edx
  4041da:	88 10                	mov    %dl,(%rax)
  4041dc:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  4041e0:	4c 8d 60 31          	lea    0x31(%rax),%r12
  4041e4:	41 bf 00 00 00 00    	mov    $0x0,%r15d
  4041ea:	41 be 08 00 00 00    	mov    $0x8,%r14d
  4041f0:	44 89 fb             	mov    %r15d,%ebx
  4041f3:	c1 fb 03             	sar    $0x3,%ebx
  4041f6:	45 89 f5             	mov    %r14d,%r13d
  4041f9:	41 c1 fd 03          	sar    $0x3,%r13d
  4041fd:	44 39 eb             	cmp    %r13d,%ebx
  404200:	75 41                	jne    404243 <load_entry+0x3c3>
  404202:	48 63 c3             	movslq %ebx,%rax
  404205:	4c 01 e0             	add    %r12,%rax
  404208:	48 63 d3             	movslq %ebx,%rdx
  40420b:	4c 01 e2             	add    %r12,%rdx
  40420e:	0f b6 12             	movzbl (%rdx),%edx
  404211:	41 89 d0             	mov    %edx,%r8d
  404214:	44 89 fa             	mov    %r15d,%edx
  404217:	83 e2 07             	and    $0x7,%edx
  40421a:	be ff 00 00 00       	mov    $0xff,%esi
  40421f:	89 d1                	mov    %edx,%ecx
  404221:	d3 e6                	shl    %cl,%esi
  404223:	89 f2                	mov    %esi,%edx
  404225:	89 d6                	mov    %edx,%esi
  404227:	44 89 f2             	mov    %r14d,%edx
  40422a:	f7 d2                	not    %edx
  40422c:	83 e2 07             	and    $0x7,%edx
  40422f:	bf ff 00 00 00       	mov    $0xff,%edi
  404234:	89 d1                	mov    %edx,%ecx
  404236:	d3 ff                	sar    %cl,%edi
  404238:	89 fa                	mov    %edi,%edx
  40423a:	21 f2                	and    %esi,%edx
  40423c:	44 09 c2             	or     %r8d,%edx
  40423f:	88 10                	mov    %dl,(%rax)
  404241:	eb 61                	jmp    4042a4 <load_entry+0x424>
  404243:	48 63 c3             	movslq %ebx,%rax
  404246:	4c 01 e0             	add    %r12,%rax
  404249:	48 63 d3             	movslq %ebx,%rdx
  40424c:	4c 01 e2             	add    %r12,%rdx
  40424f:	0f b6 12             	movzbl (%rdx),%edx
  404252:	89 d7                	mov    %edx,%edi
  404254:	44 89 fa             	mov    %r15d,%edx
  404257:	83 e2 07             	and    $0x7,%edx
  40425a:	be ff 00 00 00       	mov    $0xff,%esi
  40425f:	89 d1                	mov    %edx,%ecx
  404261:	d3 e6                	shl    %cl,%esi
  404263:	89 f2                	mov    %esi,%edx
  404265:	09 fa                	or     %edi,%edx
  404267:	88 10                	mov    %dl,(%rax)
  404269:	eb 09                	jmp    404274 <load_entry+0x3f4>
  40426b:	48 63 c3             	movslq %ebx,%rax
  40426e:	4c 01 e0             	add    %r12,%rax
  404271:	c6 00 ff             	movb   $0xff,(%rax)
  404274:	83 c3 01             	add    $0x1,%ebx
  404277:	44 39 eb             	cmp    %r13d,%ebx
  40427a:	7c ef                	jl     40426b <load_entry+0x3eb>
  40427c:	49 63 c5             	movslq %r13d,%rax
  40427f:	4c 01 e0             	add    %r12,%rax
  404282:	49 63 d5             	movslq %r13d,%rdx
  404285:	4c 01 e2             	add    %r12,%rdx
  404288:	0f b6 12             	movzbl (%rdx),%edx
  40428b:	89 d7                	mov    %edx,%edi
  40428d:	44 89 f2             	mov    %r14d,%edx
  404290:	f7 d2                	not    %edx
  404292:	83 e2 07             	and    $0x7,%edx
  404295:	be ff 00 00 00       	mov    $0xff,%esi
  40429a:	89 d1                	mov    %edx,%ecx
  40429c:	d3 fe                	sar    %cl,%esi
  40429e:	89 f2                	mov    %esi,%edx
  4042a0:	09 fa                	or     %edi,%edx
  4042a2:	88 10                	mov    %dl,(%rax)
  4042a4:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  4042a8:	8b 40 34             	mov    0x34(%rax),%eax
  4042ab:	83 c8 02             	or     $0x2,%eax
  4042ae:	89 c2                	mov    %eax,%edx
  4042b0:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  4042b4:	89 50 34             	mov    %edx,0x34(%rax)
  4042b7:	e9 ae 0a 00 00       	jmpq   404d6a <load_entry+0xeea>
  4042bc:	48 8d 85 c0 fb ff ff 	lea    -0x440(%rbp),%rax
  4042c3:	48 89 c6             	mov    %rax,%rsi
  4042c6:	bf 27 8d 40 00       	mov    $0x408d27,%edi
  4042cb:	e8 30 dc ff ff       	callq  401f00 <strcmp@plt>
  4042d0:	85 c0                	test   %eax,%eax
  4042d2:	0f 85 9b 02 00 00    	jne    404573 <load_entry+0x6f3>
  4042d8:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  4042dc:	0f b6 40 20          	movzbl 0x20(%rax),%eax
  4042e0:	83 c8 01             	or     $0x1,%eax
  4042e3:	89 c2                	mov    %eax,%edx
  4042e5:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  4042e9:	88 50 20             	mov    %dl,0x20(%rax)
  4042ec:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  4042f0:	0f b6 40 28          	movzbl 0x28(%rax),%eax
  4042f4:	83 c8 01             	or     $0x1,%eax
  4042f7:	89 c2                	mov    %eax,%edx
  4042f9:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  4042fd:	88 50 28             	mov    %dl,0x28(%rax)
  404300:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  404304:	4c 8d 60 2b          	lea    0x2b(%rax),%r12
  404308:	41 bf 00 00 00 00    	mov    $0x0,%r15d
  40430e:	41 be 1f 00 00 00    	mov    $0x1f,%r14d
  404314:	44 89 fb             	mov    %r15d,%ebx
  404317:	c1 fb 03             	sar    $0x3,%ebx
  40431a:	45 89 f5             	mov    %r14d,%r13d
  40431d:	41 c1 fd 03          	sar    $0x3,%r13d
  404321:	44 39 eb             	cmp    %r13d,%ebx
  404324:	75 41                	jne    404367 <load_entry+0x4e7>
  404326:	48 63 c3             	movslq %ebx,%rax
  404329:	4c 01 e0             	add    %r12,%rax
  40432c:	48 63 d3             	movslq %ebx,%rdx
  40432f:	4c 01 e2             	add    %r12,%rdx
  404332:	0f b6 12             	movzbl (%rdx),%edx
  404335:	41 89 d0             	mov    %edx,%r8d
  404338:	44 89 fa             	mov    %r15d,%edx
  40433b:	83 e2 07             	and    $0x7,%edx
  40433e:	be ff 00 00 00       	mov    $0xff,%esi
  404343:	89 d1                	mov    %edx,%ecx
  404345:	d3 e6                	shl    %cl,%esi
  404347:	89 f2                	mov    %esi,%edx
  404349:	89 d6                	mov    %edx,%esi
  40434b:	44 89 f2             	mov    %r14d,%edx
  40434e:	f7 d2                	not    %edx
  404350:	83 e2 07             	and    $0x7,%edx
  404353:	bf ff 00 00 00       	mov    $0xff,%edi
  404358:	89 d1                	mov    %edx,%ecx
  40435a:	d3 ff                	sar    %cl,%edi
  40435c:	89 fa                	mov    %edi,%edx
  40435e:	21 f2                	and    %esi,%edx
  404360:	44 09 c2             	or     %r8d,%edx
  404363:	88 10                	mov    %dl,(%rax)
  404365:	eb 61                	jmp    4043c8 <load_entry+0x548>
  404367:	48 63 c3             	movslq %ebx,%rax
  40436a:	4c 01 e0             	add    %r12,%rax
  40436d:	48 63 d3             	movslq %ebx,%rdx
  404370:	4c 01 e2             	add    %r12,%rdx
  404373:	0f b6 12             	movzbl (%rdx),%edx
  404376:	89 d7                	mov    %edx,%edi
  404378:	44 89 fa             	mov    %r15d,%edx
  40437b:	83 e2 07             	and    $0x7,%edx
  40437e:	be ff 00 00 00       	mov    $0xff,%esi
  404383:	89 d1                	mov    %edx,%ecx
  404385:	d3 e6                	shl    %cl,%esi
  404387:	89 f2                	mov    %esi,%edx
  404389:	09 fa                	or     %edi,%edx
  40438b:	88 10                	mov    %dl,(%rax)
  40438d:	eb 09                	jmp    404398 <load_entry+0x518>
  40438f:	48 63 c3             	movslq %ebx,%rax
  404392:	4c 01 e0             	add    %r12,%rax
  404395:	c6 00 ff             	movb   $0xff,(%rax)
  404398:	83 c3 01             	add    $0x1,%ebx
  40439b:	44 39 eb             	cmp    %r13d,%ebx
  40439e:	7c ef                	jl     40438f <load_entry+0x50f>
  4043a0:	49 63 c5             	movslq %r13d,%rax
  4043a3:	4c 01 e0             	add    %r12,%rax
  4043a6:	49 63 d5             	movslq %r13d,%rdx
  4043a9:	4c 01 e2             	add    %r12,%rdx
  4043ac:	0f b6 12             	movzbl (%rdx),%edx
  4043af:	89 d7                	mov    %edx,%edi
  4043b1:	44 89 f2             	mov    %r14d,%edx
  4043b4:	f7 d2                	not    %edx
  4043b6:	83 e2 07             	and    $0x7,%edx
  4043b9:	be ff 00 00 00       	mov    $0xff,%esi
  4043be:	89 d1                	mov    %edx,%ecx
  4043c0:	d3 fe                	sar    %cl,%esi
  4043c2:	89 f2                	mov    %esi,%edx
  4043c4:	09 fa                	or     %edi,%edx
  4043c6:	88 10                	mov    %dl,(%rax)
  4043c8:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  4043cc:	8b 40 34             	mov    0x34(%rax),%eax
  4043cf:	83 c8 01             	or     $0x1,%eax
  4043d2:	89 c2                	mov    %eax,%edx
  4043d4:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  4043d8:	89 50 34             	mov    %edx,0x34(%rax)
  4043db:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  4043df:	4c 8d 60 2f          	lea    0x2f(%rax),%r12
  4043e3:	41 bf 00 00 00 00    	mov    $0x0,%r15d
  4043e9:	41 be 0c 00 00 00    	mov    $0xc,%r14d
  4043ef:	44 89 fb             	mov    %r15d,%ebx
  4043f2:	c1 fb 03             	sar    $0x3,%ebx
  4043f5:	45 89 f5             	mov    %r14d,%r13d
  4043f8:	41 c1 fd 03          	sar    $0x3,%r13d
  4043fc:	44 39 eb             	cmp    %r13d,%ebx
  4043ff:	75 41                	jne    404442 <load_entry+0x5c2>
  404401:	48 63 c3             	movslq %ebx,%rax
  404404:	4c 01 e0             	add    %r12,%rax
  404407:	48 63 d3             	movslq %ebx,%rdx
  40440a:	4c 01 e2             	add    %r12,%rdx
  40440d:	0f b6 12             	movzbl (%rdx),%edx
  404410:	41 89 d0             	mov    %edx,%r8d
  404413:	44 89 fa             	mov    %r15d,%edx
  404416:	83 e2 07             	and    $0x7,%edx
  404419:	be ff 00 00 00       	mov    $0xff,%esi
  40441e:	89 d1                	mov    %edx,%ecx
  404420:	d3 e6                	shl    %cl,%esi
  404422:	89 f2                	mov    %esi,%edx
  404424:	89 d6                	mov    %edx,%esi
  404426:	44 89 f2             	mov    %r14d,%edx
  404429:	f7 d2                	not    %edx
  40442b:	83 e2 07             	and    $0x7,%edx
  40442e:	bf ff 00 00 00       	mov    $0xff,%edi
  404433:	89 d1                	mov    %edx,%ecx
  404435:	d3 ff                	sar    %cl,%edi
  404437:	89 fa                	mov    %edi,%edx
  404439:	21 f2                	and    %esi,%edx
  40443b:	44 09 c2             	or     %r8d,%edx
  40443e:	88 10                	mov    %dl,(%rax)
  404440:	eb 61                	jmp    4044a3 <load_entry+0x623>
  404442:	48 63 c3             	movslq %ebx,%rax
  404445:	4c 01 e0             	add    %r12,%rax
  404448:	48 63 d3             	movslq %ebx,%rdx
  40444b:	4c 01 e2             	add    %r12,%rdx
  40444e:	0f b6 12             	movzbl (%rdx),%edx
  404451:	89 d7                	mov    %edx,%edi
  404453:	44 89 fa             	mov    %r15d,%edx
  404456:	83 e2 07             	and    $0x7,%edx
  404459:	be ff 00 00 00       	mov    $0xff,%esi
  40445e:	89 d1                	mov    %edx,%ecx
  404460:	d3 e6                	shl    %cl,%esi
  404462:	89 f2                	mov    %esi,%edx
  404464:	09 fa                	or     %edi,%edx
  404466:	88 10                	mov    %dl,(%rax)
  404468:	eb 09                	jmp    404473 <load_entry+0x5f3>
  40446a:	48 63 c3             	movslq %ebx,%rax
  40446d:	4c 01 e0             	add    %r12,%rax
  404470:	c6 00 ff             	movb   $0xff,(%rax)
  404473:	83 c3 01             	add    $0x1,%ebx
  404476:	44 39 eb             	cmp    %r13d,%ebx
  404479:	7c ef                	jl     40446a <load_entry+0x5ea>
  40447b:	49 63 c5             	movslq %r13d,%rax
  40447e:	4c 01 e0             	add    %r12,%rax
  404481:	49 63 d5             	movslq %r13d,%rdx
  404484:	4c 01 e2             	add    %r12,%rdx
  404487:	0f b6 12             	movzbl (%rdx),%edx
  40448a:	89 d7                	mov    %edx,%edi
  40448c:	44 89 f2             	mov    %r14d,%edx
  40448f:	f7 d2                	not    %edx
  404491:	83 e2 07             	and    $0x7,%edx
  404494:	be ff 00 00 00       	mov    $0xff,%esi
  404499:	89 d1                	mov    %edx,%ecx
  40449b:	d3 fe                	sar    %cl,%esi
  40449d:	89 f2                	mov    %esi,%edx
  40449f:	09 fa                	or     %edi,%edx
  4044a1:	88 10                	mov    %dl,(%rax)
  4044a3:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  4044a7:	4c 8d 60 31          	lea    0x31(%rax),%r12
  4044ab:	41 bf 00 00 00 00    	mov    $0x0,%r15d
  4044b1:	41 be 00 00 00 00    	mov    $0x0,%r14d
  4044b7:	44 89 fb             	mov    %r15d,%ebx
  4044ba:	c1 fb 03             	sar    $0x3,%ebx
  4044bd:	45 89 f5             	mov    %r14d,%r13d
  4044c0:	41 c1 fd 03          	sar    $0x3,%r13d
  4044c4:	44 39 eb             	cmp    %r13d,%ebx
  4044c7:	75 44                	jne    40450d <load_entry+0x68d>
  4044c9:	48 63 c3             	movslq %ebx,%rax
  4044cc:	4c 01 e0             	add    %r12,%rax
  4044cf:	48 63 d3             	movslq %ebx,%rdx
  4044d2:	4c 01 e2             	add    %r12,%rdx
  4044d5:	0f b6 12             	movzbl (%rdx),%edx
  4044d8:	41 89 d0             	mov    %edx,%r8d
  4044db:	44 89 fa             	mov    %r15d,%edx
  4044de:	83 e2 07             	and    $0x7,%edx
  4044e1:	be ff 00 00 00       	mov    $0xff,%esi
  4044e6:	89 d1                	mov    %edx,%ecx
  4044e8:	d3 e6                	shl    %cl,%esi
  4044ea:	89 f2                	mov    %esi,%edx
  4044ec:	89 d6                	mov    %edx,%esi
  4044ee:	44 89 f2             	mov    %r14d,%edx
  4044f1:	f7 d2                	not    %edx
  4044f3:	83 e2 07             	and    $0x7,%edx
  4044f6:	bf ff 00 00 00       	mov    $0xff,%edi
  4044fb:	89 d1                	mov    %edx,%ecx
  4044fd:	d3 ff                	sar    %cl,%edi
  4044ff:	89 fa                	mov    %edi,%edx
  404501:	21 f2                	and    %esi,%edx
  404503:	44 09 c2             	or     %r8d,%edx
  404506:	88 10                	mov    %dl,(%rax)
  404508:	e9 5d 08 00 00       	jmpq   404d6a <load_entry+0xeea>
  40450d:	48 63 c3             	movslq %ebx,%rax
  404510:	4c 01 e0             	add    %r12,%rax
  404513:	48 63 d3             	movslq %ebx,%rdx
  404516:	4c 01 e2             	add    %r12,%rdx
  404519:	0f b6 12             	movzbl (%rdx),%edx
  40451c:	89 d7                	mov    %edx,%edi
  40451e:	44 89 fa             	mov    %r15d,%edx
  404521:	83 e2 07             	and    $0x7,%edx
  404524:	be ff 00 00 00       	mov    $0xff,%esi
  404529:	89 d1                	mov    %edx,%ecx
  40452b:	d3 e6                	shl    %cl,%esi
  40452d:	89 f2                	mov    %esi,%edx
  40452f:	09 fa                	or     %edi,%edx
  404531:	88 10                	mov    %dl,(%rax)
  404533:	eb 09                	jmp    40453e <load_entry+0x6be>
  404535:	48 63 c3             	movslq %ebx,%rax
  404538:	4c 01 e0             	add    %r12,%rax
  40453b:	c6 00 ff             	movb   $0xff,(%rax)
  40453e:	83 c3 01             	add    $0x1,%ebx
  404541:	44 39 eb             	cmp    %r13d,%ebx
  404544:	7c ef                	jl     404535 <load_entry+0x6b5>
  404546:	49 63 c5             	movslq %r13d,%rax
  404549:	4c 01 e0             	add    %r12,%rax
  40454c:	49 63 d5             	movslq %r13d,%rdx
  40454f:	4c 01 e2             	add    %r12,%rdx
  404552:	0f b6 12             	movzbl (%rdx),%edx
  404555:	89 d7                	mov    %edx,%edi
  404557:	44 89 f2             	mov    %r14d,%edx
  40455a:	f7 d2                	not    %edx
  40455c:	83 e2 07             	and    $0x7,%edx
  40455f:	be ff 00 00 00       	mov    $0xff,%esi
  404564:	89 d1                	mov    %edx,%ecx
  404566:	d3 fe                	sar    %cl,%esi
  404568:	89 f2                	mov    %esi,%edx
  40456a:	09 fa                	or     %edi,%edx
  40456c:	88 10                	mov    %dl,(%rax)
  40456e:	e9 f7 07 00 00       	jmpq   404d6a <load_entry+0xeea>
  404573:	48 8d 85 c0 fb ff ff 	lea    -0x440(%rbp),%rax
  40457a:	48 89 c6             	mov    %rax,%rsi
  40457d:	bf 2e 8d 40 00       	mov    $0x408d2e,%edi
  404582:	e8 79 d9 ff ff       	callq  401f00 <strcmp@plt>
  404587:	85 c0                	test   %eax,%eax
  404589:	74 1c                	je     4045a7 <load_entry+0x727>
  40458b:	48 8d 85 c0 fb ff ff 	lea    -0x440(%rbp),%rax
  404592:	48 89 c6             	mov    %rax,%rsi
  404595:	bf 34 8d 40 00       	mov    $0x408d34,%edi
  40459a:	e8 61 d9 ff ff       	callq  401f00 <strcmp@plt>
  40459f:	85 c0                	test   %eax,%eax
  4045a1:	0f 85 88 02 00 00    	jne    40482f <load_entry+0x9af>
  4045a7:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  4045ab:	0f b6 40 20          	movzbl 0x20(%rax),%eax
  4045af:	83 c8 01             	or     $0x1,%eax
  4045b2:	89 c2                	mov    %eax,%edx
  4045b4:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  4045b8:	88 50 20             	mov    %dl,0x20(%rax)
  4045bb:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  4045bf:	0f b6 40 28          	movzbl 0x28(%rax),%eax
  4045c3:	83 c8 01             	or     $0x1,%eax
  4045c6:	89 c2                	mov    %eax,%edx
  4045c8:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  4045cc:	88 50 28             	mov    %dl,0x28(%rax)
  4045cf:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  4045d3:	4c 8d 60 2b          	lea    0x2b(%rax),%r12
  4045d7:	41 bf 00 00 00 00    	mov    $0x0,%r15d
  4045dd:	41 be 1f 00 00 00    	mov    $0x1f,%r14d
  4045e3:	44 89 fb             	mov    %r15d,%ebx
  4045e6:	c1 fb 03             	sar    $0x3,%ebx
  4045e9:	45 89 f5             	mov    %r14d,%r13d
  4045ec:	41 c1 fd 03          	sar    $0x3,%r13d
  4045f0:	44 39 eb             	cmp    %r13d,%ebx
  4045f3:	75 41                	jne    404636 <load_entry+0x7b6>
  4045f5:	48 63 c3             	movslq %ebx,%rax
  4045f8:	4c 01 e0             	add    %r12,%rax
  4045fb:	48 63 d3             	movslq %ebx,%rdx
  4045fe:	4c 01 e2             	add    %r12,%rdx
  404601:	0f b6 12             	movzbl (%rdx),%edx
  404604:	41 89 d0             	mov    %edx,%r8d
  404607:	44 89 fa             	mov    %r15d,%edx
  40460a:	83 e2 07             	and    $0x7,%edx
  40460d:	be ff 00 00 00       	mov    $0xff,%esi
  404612:	89 d1                	mov    %edx,%ecx
  404614:	d3 e6                	shl    %cl,%esi
  404616:	89 f2                	mov    %esi,%edx
  404618:	89 d6                	mov    %edx,%esi
  40461a:	44 89 f2             	mov    %r14d,%edx
  40461d:	f7 d2                	not    %edx
  40461f:	83 e2 07             	and    $0x7,%edx
  404622:	bf ff 00 00 00       	mov    $0xff,%edi
  404627:	89 d1                	mov    %edx,%ecx
  404629:	d3 ff                	sar    %cl,%edi
  40462b:	89 fa                	mov    %edi,%edx
  40462d:	21 f2                	and    %esi,%edx
  40462f:	44 09 c2             	or     %r8d,%edx
  404632:	88 10                	mov    %dl,(%rax)
  404634:	eb 61                	jmp    404697 <load_entry+0x817>
  404636:	48 63 c3             	movslq %ebx,%rax
  404639:	4c 01 e0             	add    %r12,%rax
  40463c:	48 63 d3             	movslq %ebx,%rdx
  40463f:	4c 01 e2             	add    %r12,%rdx
  404642:	0f b6 12             	movzbl (%rdx),%edx
  404645:	89 d7                	mov    %edx,%edi
  404647:	44 89 fa             	mov    %r15d,%edx
  40464a:	83 e2 07             	and    $0x7,%edx
  40464d:	be ff 00 00 00       	mov    $0xff,%esi
  404652:	89 d1                	mov    %edx,%ecx
  404654:	d3 e6                	shl    %cl,%esi
  404656:	89 f2                	mov    %esi,%edx
  404658:	09 fa                	or     %edi,%edx
  40465a:	88 10                	mov    %dl,(%rax)
  40465c:	eb 09                	jmp    404667 <load_entry+0x7e7>
  40465e:	48 63 c3             	movslq %ebx,%rax
  404661:	4c 01 e0             	add    %r12,%rax
  404664:	c6 00 ff             	movb   $0xff,(%rax)
  404667:	83 c3 01             	add    $0x1,%ebx
  40466a:	44 39 eb             	cmp    %r13d,%ebx
  40466d:	7c ef                	jl     40465e <load_entry+0x7de>
  40466f:	49 63 c5             	movslq %r13d,%rax
  404672:	4c 01 e0             	add    %r12,%rax
  404675:	49 63 d5             	movslq %r13d,%rdx
  404678:	4c 01 e2             	add    %r12,%rdx
  40467b:	0f b6 12             	movzbl (%rdx),%edx
  40467e:	89 d7                	mov    %edx,%edi
  404680:	44 89 f2             	mov    %r14d,%edx
  404683:	f7 d2                	not    %edx
  404685:	83 e2 07             	and    $0x7,%edx
  404688:	be ff 00 00 00       	mov    $0xff,%esi
  40468d:	89 d1                	mov    %edx,%ecx
  40468f:	d3 fe                	sar    %cl,%esi
  404691:	89 f2                	mov    %esi,%edx
  404693:	09 fa                	or     %edi,%edx
  404695:	88 10                	mov    %dl,(%rax)
  404697:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  40469b:	4c 8d 60 2f          	lea    0x2f(%rax),%r12
  40469f:	41 bf 00 00 00 00    	mov    $0x0,%r15d
  4046a5:	41 be 0c 00 00 00    	mov    $0xc,%r14d
  4046ab:	44 89 fb             	mov    %r15d,%ebx
  4046ae:	c1 fb 03             	sar    $0x3,%ebx
  4046b1:	45 89 f5             	mov    %r14d,%r13d
  4046b4:	41 c1 fd 03          	sar    $0x3,%r13d
  4046b8:	44 39 eb             	cmp    %r13d,%ebx
  4046bb:	75 41                	jne    4046fe <load_entry+0x87e>
  4046bd:	48 63 c3             	movslq %ebx,%rax
  4046c0:	4c 01 e0             	add    %r12,%rax
  4046c3:	48 63 d3             	movslq %ebx,%rdx
  4046c6:	4c 01 e2             	add    %r12,%rdx
  4046c9:	0f b6 12             	movzbl (%rdx),%edx
  4046cc:	41 89 d0             	mov    %edx,%r8d
  4046cf:	44 89 fa             	mov    %r15d,%edx
  4046d2:	83 e2 07             	and    $0x7,%edx
  4046d5:	be ff 00 00 00       	mov    $0xff,%esi
  4046da:	89 d1                	mov    %edx,%ecx
  4046dc:	d3 e6                	shl    %cl,%esi
  4046de:	89 f2                	mov    %esi,%edx
  4046e0:	89 d6                	mov    %edx,%esi
  4046e2:	44 89 f2             	mov    %r14d,%edx
  4046e5:	f7 d2                	not    %edx
  4046e7:	83 e2 07             	and    $0x7,%edx
  4046ea:	bf ff 00 00 00       	mov    $0xff,%edi
  4046ef:	89 d1                	mov    %edx,%ecx
  4046f1:	d3 ff                	sar    %cl,%edi
  4046f3:	89 fa                	mov    %edi,%edx
  4046f5:	21 f2                	and    %esi,%edx
  4046f7:	44 09 c2             	or     %r8d,%edx
  4046fa:	88 10                	mov    %dl,(%rax)
  4046fc:	eb 61                	jmp    40475f <load_entry+0x8df>
  4046fe:	48 63 c3             	movslq %ebx,%rax
  404701:	4c 01 e0             	add    %r12,%rax
  404704:	48 63 d3             	movslq %ebx,%rdx
  404707:	4c 01 e2             	add    %r12,%rdx
  40470a:	0f b6 12             	movzbl (%rdx),%edx
  40470d:	89 d7                	mov    %edx,%edi
  40470f:	44 89 fa             	mov    %r15d,%edx
  404712:	83 e2 07             	and    $0x7,%edx
  404715:	be ff 00 00 00       	mov    $0xff,%esi
  40471a:	89 d1                	mov    %edx,%ecx
  40471c:	d3 e6                	shl    %cl,%esi
  40471e:	89 f2                	mov    %esi,%edx
  404720:	09 fa                	or     %edi,%edx
  404722:	88 10                	mov    %dl,(%rax)
  404724:	eb 09                	jmp    40472f <load_entry+0x8af>
  404726:	48 63 c3             	movslq %ebx,%rax
  404729:	4c 01 e0             	add    %r12,%rax
  40472c:	c6 00 ff             	movb   $0xff,(%rax)
  40472f:	83 c3 01             	add    $0x1,%ebx
  404732:	44 39 eb             	cmp    %r13d,%ebx
  404735:	7c ef                	jl     404726 <load_entry+0x8a6>
  404737:	49 63 c5             	movslq %r13d,%rax
  40473a:	4c 01 e0             	add    %r12,%rax
  40473d:	49 63 d5             	movslq %r13d,%rdx
  404740:	4c 01 e2             	add    %r12,%rdx
  404743:	0f b6 12             	movzbl (%rdx),%edx
  404746:	89 d7                	mov    %edx,%edi
  404748:	44 89 f2             	mov    %r14d,%edx
  40474b:	f7 d2                	not    %edx
  40474d:	83 e2 07             	and    $0x7,%edx
  404750:	be ff 00 00 00       	mov    $0xff,%esi
  404755:	89 d1                	mov    %edx,%ecx
  404757:	d3 fe                	sar    %cl,%esi
  404759:	89 f2                	mov    %esi,%edx
  40475b:	09 fa                	or     %edi,%edx
  40475d:	88 10                	mov    %dl,(%rax)
  40475f:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  404763:	4c 8d 60 31          	lea    0x31(%rax),%r12
  404767:	41 bf 00 00 00 00    	mov    $0x0,%r15d
  40476d:	41 be 08 00 00 00    	mov    $0x8,%r14d
  404773:	44 89 fb             	mov    %r15d,%ebx
  404776:	c1 fb 03             	sar    $0x3,%ebx
  404779:	45 89 f5             	mov    %r14d,%r13d
  40477c:	41 c1 fd 03          	sar    $0x3,%r13d
  404780:	44 39 eb             	cmp    %r13d,%ebx
  404783:	75 44                	jne    4047c9 <load_entry+0x949>
  404785:	48 63 c3             	movslq %ebx,%rax
  404788:	4c 01 e0             	add    %r12,%rax
  40478b:	48 63 d3             	movslq %ebx,%rdx
  40478e:	4c 01 e2             	add    %r12,%rdx
  404791:	0f b6 12             	movzbl (%rdx),%edx
  404794:	41 89 d0             	mov    %edx,%r8d
  404797:	44 89 fa             	mov    %r15d,%edx
  40479a:	83 e2 07             	and    $0x7,%edx
  40479d:	be ff 00 00 00       	mov    $0xff,%esi
  4047a2:	89 d1                	mov    %edx,%ecx
  4047a4:	d3 e6                	shl    %cl,%esi
  4047a6:	89 f2                	mov    %esi,%edx
  4047a8:	89 d6                	mov    %edx,%esi
  4047aa:	44 89 f2             	mov    %r14d,%edx
  4047ad:	f7 d2                	not    %edx
  4047af:	83 e2 07             	and    $0x7,%edx
  4047b2:	bf ff 00 00 00       	mov    $0xff,%edi
  4047b7:	89 d1                	mov    %edx,%ecx
  4047b9:	d3 ff                	sar    %cl,%edi
  4047bb:	89 fa                	mov    %edi,%edx
  4047bd:	21 f2                	and    %esi,%edx
  4047bf:	44 09 c2             	or     %r8d,%edx
  4047c2:	88 10                	mov    %dl,(%rax)
  4047c4:	e9 a1 05 00 00       	jmpq   404d6a <load_entry+0xeea>
  4047c9:	48 63 c3             	movslq %ebx,%rax
  4047cc:	4c 01 e0             	add    %r12,%rax
  4047cf:	48 63 d3             	movslq %ebx,%rdx
  4047d2:	4c 01 e2             	add    %r12,%rdx
  4047d5:	0f b6 12             	movzbl (%rdx),%edx
  4047d8:	89 d7                	mov    %edx,%edi
  4047da:	44 89 fa             	mov    %r15d,%edx
  4047dd:	83 e2 07             	and    $0x7,%edx
  4047e0:	be ff 00 00 00       	mov    $0xff,%esi
  4047e5:	89 d1                	mov    %edx,%ecx
  4047e7:	d3 e6                	shl    %cl,%esi
  4047e9:	89 f2                	mov    %esi,%edx
  4047eb:	09 fa                	or     %edi,%edx
  4047ed:	88 10                	mov    %dl,(%rax)
  4047ef:	eb 09                	jmp    4047fa <load_entry+0x97a>
  4047f1:	48 63 c3             	movslq %ebx,%rax
  4047f4:	4c 01 e0             	add    %r12,%rax
  4047f7:	c6 00 ff             	movb   $0xff,(%rax)
  4047fa:	83 c3 01             	add    $0x1,%ebx
  4047fd:	44 39 eb             	cmp    %r13d,%ebx
  404800:	7c ef                	jl     4047f1 <load_entry+0x971>
  404802:	49 63 c5             	movslq %r13d,%rax
  404805:	4c 01 e0             	add    %r12,%rax
  404808:	49 63 d5             	movslq %r13d,%rdx
  40480b:	4c 01 e2             	add    %r12,%rdx
  40480e:	0f b6 12             	movzbl (%rdx),%edx
  404811:	89 d7                	mov    %edx,%edi
  404813:	44 89 f2             	mov    %r14d,%edx
  404816:	f7 d2                	not    %edx
  404818:	83 e2 07             	and    $0x7,%edx
  40481b:	be ff 00 00 00       	mov    $0xff,%esi
  404820:	89 d1                	mov    %edx,%ecx
  404822:	d3 fe                	sar    %cl,%esi
  404824:	89 f2                	mov    %esi,%edx
  404826:	09 fa                	or     %edi,%edx
  404828:	88 10                	mov    %dl,(%rax)
  40482a:	e9 3b 05 00 00       	jmpq   404d6a <load_entry+0xeea>
  40482f:	48 8d 85 c0 fb ff ff 	lea    -0x440(%rbp),%rax
  404836:	48 89 c6             	mov    %rax,%rsi
  404839:	bf 3d 8d 40 00       	mov    $0x408d3d,%edi
  40483e:	e8 bd d6 ff ff       	callq  401f00 <strcmp@plt>
  404843:	85 c0                	test   %eax,%eax
  404845:	0f 85 4c 03 00 00    	jne    404b97 <load_entry+0xd17>
  40484b:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  40484f:	0f b6 40 20          	movzbl 0x20(%rax),%eax
  404853:	83 c8 01             	or     $0x1,%eax
  404856:	89 c2                	mov    %eax,%edx
  404858:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  40485c:	88 50 20             	mov    %dl,0x20(%rax)
  40485f:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  404863:	4c 8d 60 28          	lea    0x28(%rax),%r12
  404867:	41 bf 00 00 00 00    	mov    $0x0,%r15d
  40486d:	41 be 18 00 00 00    	mov    $0x18,%r14d
  404873:	44 89 fb             	mov    %r15d,%ebx
  404876:	c1 fb 03             	sar    $0x3,%ebx
  404879:	45 89 f5             	mov    %r14d,%r13d
  40487c:	41 c1 fd 03          	sar    $0x3,%r13d
  404880:	44 39 eb             	cmp    %r13d,%ebx
  404883:	75 41                	jne    4048c6 <load_entry+0xa46>
  404885:	48 63 c3             	movslq %ebx,%rax
  404888:	4c 01 e0             	add    %r12,%rax
  40488b:	48 63 d3             	movslq %ebx,%rdx
  40488e:	4c 01 e2             	add    %r12,%rdx
  404891:	0f b6 12             	movzbl (%rdx),%edx
  404894:	41 89 d0             	mov    %edx,%r8d
  404897:	44 89 fa             	mov    %r15d,%edx
  40489a:	83 e2 07             	and    $0x7,%edx
  40489d:	be ff 00 00 00       	mov    $0xff,%esi
  4048a2:	89 d1                	mov    %edx,%ecx
  4048a4:	d3 e6                	shl    %cl,%esi
  4048a6:	89 f2                	mov    %esi,%edx
  4048a8:	89 d6                	mov    %edx,%esi
  4048aa:	44 89 f2             	mov    %r14d,%edx
  4048ad:	f7 d2                	not    %edx
  4048af:	83 e2 07             	and    $0x7,%edx
  4048b2:	bf ff 00 00 00       	mov    $0xff,%edi
  4048b7:	89 d1                	mov    %edx,%ecx
  4048b9:	d3 ff                	sar    %cl,%edi
  4048bb:	89 fa                	mov    %edi,%edx
  4048bd:	21 f2                	and    %esi,%edx
  4048bf:	44 09 c2             	or     %r8d,%edx
  4048c2:	88 10                	mov    %dl,(%rax)
  4048c4:	eb 61                	jmp    404927 <load_entry+0xaa7>
  4048c6:	48 63 c3             	movslq %ebx,%rax
  4048c9:	4c 01 e0             	add    %r12,%rax
  4048cc:	48 63 d3             	movslq %ebx,%rdx
  4048cf:	4c 01 e2             	add    %r12,%rdx
  4048d2:	0f b6 12             	movzbl (%rdx),%edx
  4048d5:	89 d7                	mov    %edx,%edi
  4048d7:	44 89 fa             	mov    %r15d,%edx
  4048da:	83 e2 07             	and    $0x7,%edx
  4048dd:	be ff 00 00 00       	mov    $0xff,%esi
  4048e2:	89 d1                	mov    %edx,%ecx
  4048e4:	d3 e6                	shl    %cl,%esi
  4048e6:	89 f2                	mov    %esi,%edx
  4048e8:	09 fa                	or     %edi,%edx
  4048ea:	88 10                	mov    %dl,(%rax)
  4048ec:	eb 09                	jmp    4048f7 <load_entry+0xa77>
  4048ee:	48 63 c3             	movslq %ebx,%rax
  4048f1:	4c 01 e0             	add    %r12,%rax
  4048f4:	c6 00 ff             	movb   $0xff,(%rax)
  4048f7:	83 c3 01             	add    $0x1,%ebx
  4048fa:	44 39 eb             	cmp    %r13d,%ebx
  4048fd:	7c ef                	jl     4048ee <load_entry+0xa6e>
  4048ff:	49 63 c5             	movslq %r13d,%rax
  404902:	4c 01 e0             	add    %r12,%rax
  404905:	49 63 d5             	movslq %r13d,%rdx
  404908:	4c 01 e2             	add    %r12,%rdx
  40490b:	0f b6 12             	movzbl (%rdx),%edx
  40490e:	89 d7                	mov    %edx,%edi
  404910:	44 89 f2             	mov    %r14d,%edx
  404913:	f7 d2                	not    %edx
  404915:	83 e2 07             	and    $0x7,%edx
  404918:	be ff 00 00 00       	mov    $0xff,%esi
  40491d:	89 d1                	mov    %edx,%ecx
  40491f:	d3 fe                	sar    %cl,%esi
  404921:	89 f2                	mov    %esi,%edx
  404923:	09 fa                	or     %edi,%edx
  404925:	88 10                	mov    %dl,(%rax)
  404927:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  40492b:	4c 8d 60 2b          	lea    0x2b(%rax),%r12
  40492f:	41 bf 00 00 00 00    	mov    $0x0,%r15d
  404935:	41 be 1f 00 00 00    	mov    $0x1f,%r14d
  40493b:	44 89 fb             	mov    %r15d,%ebx
  40493e:	c1 fb 03             	sar    $0x3,%ebx
  404941:	45 89 f5             	mov    %r14d,%r13d
  404944:	41 c1 fd 03          	sar    $0x3,%r13d
  404948:	44 39 eb             	cmp    %r13d,%ebx
  40494b:	75 41                	jne    40498e <load_entry+0xb0e>
  40494d:	48 63 c3             	movslq %ebx,%rax
  404950:	4c 01 e0             	add    %r12,%rax
  404953:	48 63 d3             	movslq %ebx,%rdx
  404956:	4c 01 e2             	add    %r12,%rdx
  404959:	0f b6 12             	movzbl (%rdx),%edx
  40495c:	41 89 d0             	mov    %edx,%r8d
  40495f:	44 89 fa             	mov    %r15d,%edx
  404962:	83 e2 07             	and    $0x7,%edx
  404965:	be ff 00 00 00       	mov    $0xff,%esi
  40496a:	89 d1                	mov    %edx,%ecx
  40496c:	d3 e6                	shl    %cl,%esi
  40496e:	89 f2                	mov    %esi,%edx
  404970:	89 d6                	mov    %edx,%esi
  404972:	44 89 f2             	mov    %r14d,%edx
  404975:	f7 d2                	not    %edx
  404977:	83 e2 07             	and    $0x7,%edx
  40497a:	bf ff 00 00 00       	mov    $0xff,%edi
  40497f:	89 d1                	mov    %edx,%ecx
  404981:	d3 ff                	sar    %cl,%edi
  404983:	89 fa                	mov    %edi,%edx
  404985:	21 f2                	and    %esi,%edx
  404987:	44 09 c2             	or     %r8d,%edx
  40498a:	88 10                	mov    %dl,(%rax)
  40498c:	eb 61                	jmp    4049ef <load_entry+0xb6f>
  40498e:	48 63 c3             	movslq %ebx,%rax
  404991:	4c 01 e0             	add    %r12,%rax
  404994:	48 63 d3             	movslq %ebx,%rdx
  404997:	4c 01 e2             	add    %r12,%rdx
  40499a:	0f b6 12             	movzbl (%rdx),%edx
  40499d:	89 d7                	mov    %edx,%edi
  40499f:	44 89 fa             	mov    %r15d,%edx
  4049a2:	83 e2 07             	and    $0x7,%edx
  4049a5:	be ff 00 00 00       	mov    $0xff,%esi
  4049aa:	89 d1                	mov    %edx,%ecx
  4049ac:	d3 e6                	shl    %cl,%esi
  4049ae:	89 f2                	mov    %esi,%edx
  4049b0:	09 fa                	or     %edi,%edx
  4049b2:	88 10                	mov    %dl,(%rax)
  4049b4:	eb 09                	jmp    4049bf <load_entry+0xb3f>
  4049b6:	48 63 c3             	movslq %ebx,%rax
  4049b9:	4c 01 e0             	add    %r12,%rax
  4049bc:	c6 00 ff             	movb   $0xff,(%rax)
  4049bf:	83 c3 01             	add    $0x1,%ebx
  4049c2:	44 39 eb             	cmp    %r13d,%ebx
  4049c5:	7c ef                	jl     4049b6 <load_entry+0xb36>
  4049c7:	49 63 c5             	movslq %r13d,%rax
  4049ca:	4c 01 e0             	add    %r12,%rax
  4049cd:	49 63 d5             	movslq %r13d,%rdx
  4049d0:	4c 01 e2             	add    %r12,%rdx
  4049d3:	0f b6 12             	movzbl (%rdx),%edx
  4049d6:	89 d7                	mov    %edx,%edi
  4049d8:	44 89 f2             	mov    %r14d,%edx
  4049db:	f7 d2                	not    %edx
  4049dd:	83 e2 07             	and    $0x7,%edx
  4049e0:	be ff 00 00 00       	mov    $0xff,%esi
  4049e5:	89 d1                	mov    %edx,%ecx
  4049e7:	d3 fe                	sar    %cl,%esi
  4049e9:	89 f2                	mov    %esi,%edx
  4049eb:	09 fa                	or     %edi,%edx
  4049ed:	88 10                	mov    %dl,(%rax)
  4049ef:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  4049f3:	4c 8d 60 2f          	lea    0x2f(%rax),%r12
  4049f7:	41 bf 00 00 00 00    	mov    $0x0,%r15d
  4049fd:	41 be 0c 00 00 00    	mov    $0xc,%r14d
  404a03:	44 89 fb             	mov    %r15d,%ebx
  404a06:	c1 fb 03             	sar    $0x3,%ebx
  404a09:	45 89 f5             	mov    %r14d,%r13d
  404a0c:	41 c1 fd 03          	sar    $0x3,%r13d
  404a10:	44 39 eb             	cmp    %r13d,%ebx
  404a13:	75 41                	jne    404a56 <load_entry+0xbd6>
  404a15:	48 63 c3             	movslq %ebx,%rax
  404a18:	4c 01 e0             	add    %r12,%rax
  404a1b:	48 63 d3             	movslq %ebx,%rdx
  404a1e:	4c 01 e2             	add    %r12,%rdx
  404a21:	0f b6 12             	movzbl (%rdx),%edx
  404a24:	41 89 d0             	mov    %edx,%r8d
  404a27:	44 89 fa             	mov    %r15d,%edx
  404a2a:	83 e2 07             	and    $0x7,%edx
  404a2d:	be ff 00 00 00       	mov    $0xff,%esi
  404a32:	89 d1                	mov    %edx,%ecx
  404a34:	d3 e6                	shl    %cl,%esi
  404a36:	89 f2                	mov    %esi,%edx
  404a38:	89 d6                	mov    %edx,%esi
  404a3a:	44 89 f2             	mov    %r14d,%edx
  404a3d:	f7 d2                	not    %edx
  404a3f:	83 e2 07             	and    $0x7,%edx
  404a42:	bf ff 00 00 00       	mov    $0xff,%edi
  404a47:	89 d1                	mov    %edx,%ecx
  404a49:	d3 ff                	sar    %cl,%edi
  404a4b:	89 fa                	mov    %edi,%edx
  404a4d:	21 f2                	and    %esi,%edx
  404a4f:	44 09 c2             	or     %r8d,%edx
  404a52:	88 10                	mov    %dl,(%rax)
  404a54:	eb 61                	jmp    404ab7 <load_entry+0xc37>
  404a56:	48 63 c3             	movslq %ebx,%rax
  404a59:	4c 01 e0             	add    %r12,%rax
  404a5c:	48 63 d3             	movslq %ebx,%rdx
  404a5f:	4c 01 e2             	add    %r12,%rdx
  404a62:	0f b6 12             	movzbl (%rdx),%edx
  404a65:	89 d7                	mov    %edx,%edi
  404a67:	44 89 fa             	mov    %r15d,%edx
  404a6a:	83 e2 07             	and    $0x7,%edx
  404a6d:	be ff 00 00 00       	mov    $0xff,%esi
  404a72:	89 d1                	mov    %edx,%ecx
  404a74:	d3 e6                	shl    %cl,%esi
  404a76:	89 f2                	mov    %esi,%edx
  404a78:	09 fa                	or     %edi,%edx
  404a7a:	88 10                	mov    %dl,(%rax)
  404a7c:	eb 09                	jmp    404a87 <load_entry+0xc07>
  404a7e:	48 63 c3             	movslq %ebx,%rax
  404a81:	4c 01 e0             	add    %r12,%rax
  404a84:	c6 00 ff             	movb   $0xff,(%rax)
  404a87:	83 c3 01             	add    $0x1,%ebx
  404a8a:	44 39 eb             	cmp    %r13d,%ebx
  404a8d:	7c ef                	jl     404a7e <load_entry+0xbfe>
  404a8f:	49 63 c5             	movslq %r13d,%rax
  404a92:	4c 01 e0             	add    %r12,%rax
  404a95:	49 63 d5             	movslq %r13d,%rdx
  404a98:	4c 01 e2             	add    %r12,%rdx
  404a9b:	0f b6 12             	movzbl (%rdx),%edx
  404a9e:	89 d7                	mov    %edx,%edi
  404aa0:	44 89 f2             	mov    %r14d,%edx
  404aa3:	f7 d2                	not    %edx
  404aa5:	83 e2 07             	and    $0x7,%edx
  404aa8:	be ff 00 00 00       	mov    $0xff,%esi
  404aad:	89 d1                	mov    %edx,%ecx
  404aaf:	d3 fe                	sar    %cl,%esi
  404ab1:	89 f2                	mov    %esi,%edx
  404ab3:	09 fa                	or     %edi,%edx
  404ab5:	88 10                	mov    %dl,(%rax)
  404ab7:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  404abb:	4c 8d 60 31          	lea    0x31(%rax),%r12
  404abf:	41 bf 00 00 00 00    	mov    $0x0,%r15d
  404ac5:	41 be 08 00 00 00    	mov    $0x8,%r14d
  404acb:	44 89 fb             	mov    %r15d,%ebx
  404ace:	c1 fb 03             	sar    $0x3,%ebx
  404ad1:	45 89 f5             	mov    %r14d,%r13d
  404ad4:	41 c1 fd 03          	sar    $0x3,%r13d
  404ad8:	44 39 eb             	cmp    %r13d,%ebx
  404adb:	75 41                	jne    404b1e <load_entry+0xc9e>
  404add:	48 63 c3             	movslq %ebx,%rax
  404ae0:	4c 01 e0             	add    %r12,%rax
  404ae3:	48 63 d3             	movslq %ebx,%rdx
  404ae6:	4c 01 e2             	add    %r12,%rdx
  404ae9:	0f b6 12             	movzbl (%rdx),%edx
  404aec:	41 89 d0             	mov    %edx,%r8d
  404aef:	44 89 fa             	mov    %r15d,%edx
  404af2:	83 e2 07             	and    $0x7,%edx
  404af5:	be ff 00 00 00       	mov    $0xff,%esi
  404afa:	89 d1                	mov    %edx,%ecx
  404afc:	d3 e6                	shl    %cl,%esi
  404afe:	89 f2                	mov    %esi,%edx
  404b00:	89 d6                	mov    %edx,%esi
  404b02:	44 89 f2             	mov    %r14d,%edx
  404b05:	f7 d2                	not    %edx
  404b07:	83 e2 07             	and    $0x7,%edx
  404b0a:	bf ff 00 00 00       	mov    $0xff,%edi
  404b0f:	89 d1                	mov    %edx,%ecx
  404b11:	d3 ff                	sar    %cl,%edi
  404b13:	89 fa                	mov    %edi,%edx
  404b15:	21 f2                	and    %esi,%edx
  404b17:	44 09 c2             	or     %r8d,%edx
  404b1a:	88 10                	mov    %dl,(%rax)
  404b1c:	eb 61                	jmp    404b7f <load_entry+0xcff>
  404b1e:	48 63 c3             	movslq %ebx,%rax
  404b21:	4c 01 e0             	add    %r12,%rax
  404b24:	48 63 d3             	movslq %ebx,%rdx
  404b27:	4c 01 e2             	add    %r12,%rdx
  404b2a:	0f b6 12             	movzbl (%rdx),%edx
  404b2d:	89 d7                	mov    %edx,%edi
  404b2f:	44 89 fa             	mov    %r15d,%edx
  404b32:	83 e2 07             	and    $0x7,%edx
  404b35:	be ff 00 00 00       	mov    $0xff,%esi
  404b3a:	89 d1                	mov    %edx,%ecx
  404b3c:	d3 e6                	shl    %cl,%esi
  404b3e:	89 f2                	mov    %esi,%edx
  404b40:	09 fa                	or     %edi,%edx
  404b42:	88 10                	mov    %dl,(%rax)
  404b44:	eb 09                	jmp    404b4f <load_entry+0xccf>
  404b46:	48 63 c3             	movslq %ebx,%rax
  404b49:	4c 01 e0             	add    %r12,%rax
  404b4c:	c6 00 ff             	movb   $0xff,(%rax)
  404b4f:	83 c3 01             	add    $0x1,%ebx
  404b52:	44 39 eb             	cmp    %r13d,%ebx
  404b55:	7c ef                	jl     404b46 <load_entry+0xcc6>
  404b57:	49 63 c5             	movslq %r13d,%rax
  404b5a:	4c 01 e0             	add    %r12,%rax
  404b5d:	49 63 d5             	movslq %r13d,%rdx
  404b60:	4c 01 e2             	add    %r12,%rdx
  404b63:	0f b6 12             	movzbl (%rdx),%edx
  404b66:	89 d7                	mov    %edx,%edi
  404b68:	44 89 f2             	mov    %r14d,%edx
  404b6b:	f7 d2                	not    %edx
  404b6d:	83 e2 07             	and    $0x7,%edx
  404b70:	be ff 00 00 00       	mov    $0xff,%esi
  404b75:	89 d1                	mov    %edx,%ecx
  404b77:	d3 fe                	sar    %cl,%esi
  404b79:	89 f2                	mov    %esi,%edx
  404b7b:	09 fa                	or     %edi,%edx
  404b7d:	88 10                	mov    %dl,(%rax)
  404b7f:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  404b83:	8b 40 34             	mov    0x34(%rax),%eax
  404b86:	83 c8 10             	or     $0x10,%eax
  404b89:	89 c2                	mov    %eax,%edx
  404b8b:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  404b8f:	89 50 34             	mov    %edx,0x34(%rax)
  404b92:	e9 d3 01 00 00       	jmpq   404d6a <load_entry+0xeea>
  404b97:	c7 45 cc 07 00 00 00 	movl   $0x7,-0x34(%rbp)
  404b9e:	e9 6c 05 00 00       	jmpq   40510f <load_entry+0x128f>
  404ba3:	83 7d c8 2a          	cmpl   $0x2a,-0x38(%rbp)
  404ba7:	75 13                	jne    404bbc <load_entry+0xd3c>
  404ba9:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  404bad:	8b 40 34             	mov    0x34(%rax),%eax
  404bb0:	83 c8 08             	or     $0x8,%eax
  404bb3:	89 c2                	mov    %eax,%edx
  404bb5:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  404bb9:	89 50 34             	mov    %edx,0x34(%rax)
  404bbc:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  404bc0:	48 83 c0 20          	add    $0x20,%rax
  404bc4:	48 8b 8d c8 f7 ff ff 	mov    -0x838(%rbp),%rcx
  404bcb:	8b 55 c8             	mov    -0x38(%rbp),%edx
  404bce:	49 89 c9             	mov    %rcx,%r9
  404bd1:	41 89 d0             	mov    %edx,%r8d
  404bd4:	b9 00 00 00 00       	mov    $0x0,%ecx
  404bd9:	ba 3b 00 00 00       	mov    $0x3b,%edx
  404bde:	be 00 00 00 00       	mov    $0x0,%esi
  404be3:	48 89 c7             	mov    %rax,%rdi
  404be6:	e8 cf 05 00 00       	callq  4051ba <get_list>
  404beb:	0f be c0             	movsbl %al,%eax
  404bee:	89 45 c8             	mov    %eax,-0x38(%rbp)
  404bf1:	83 7d c8 ff          	cmpl   $0xffffffff,-0x38(%rbp)
  404bf5:	75 0c                	jne    404c03 <load_entry+0xd83>
  404bf7:	c7 45 cc 01 00 00 00 	movl   $0x1,-0x34(%rbp)
  404bfe:	e9 0c 05 00 00       	jmpq   40510f <load_entry+0x128f>
  404c03:	83 7d c8 2a          	cmpl   $0x2a,-0x38(%rbp)
  404c07:	75 13                	jne    404c1c <load_entry+0xd9c>
  404c09:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  404c0d:	8b 40 34             	mov    0x34(%rax),%eax
  404c10:	83 c8 10             	or     $0x10,%eax
  404c13:	89 c2                	mov    %eax,%edx
  404c15:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  404c19:	89 50 34             	mov    %edx,0x34(%rax)
  404c1c:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  404c20:	48 83 c0 28          	add    $0x28,%rax
  404c24:	48 8b 8d c8 f7 ff ff 	mov    -0x838(%rbp),%rcx
  404c2b:	8b 55 c8             	mov    -0x38(%rbp),%edx
  404c2e:	49 89 c9             	mov    %rcx,%r9
  404c31:	41 89 d0             	mov    %edx,%r8d
  404c34:	b9 00 00 00 00       	mov    $0x0,%ecx
  404c39:	ba 17 00 00 00       	mov    $0x17,%edx
  404c3e:	be 00 00 00 00       	mov    $0x0,%esi
  404c43:	48 89 c7             	mov    %rax,%rdi
  404c46:	e8 6f 05 00 00       	callq  4051ba <get_list>
  404c4b:	0f be c0             	movsbl %al,%eax
  404c4e:	89 45 c8             	mov    %eax,-0x38(%rbp)
  404c51:	83 7d c8 ff          	cmpl   $0xffffffff,-0x38(%rbp)
  404c55:	75 0c                	jne    404c63 <load_entry+0xde3>
  404c57:	c7 45 cc 02 00 00 00 	movl   $0x2,-0x34(%rbp)
  404c5e:	e9 ac 04 00 00       	jmpq   40510f <load_entry+0x128f>
  404c63:	83 7d c8 2a          	cmpl   $0x2a,-0x38(%rbp)
  404c67:	75 13                	jne    404c7c <load_entry+0xdfc>
  404c69:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  404c6d:	8b 40 34             	mov    0x34(%rax),%eax
  404c70:	83 c8 01             	or     $0x1,%eax
  404c73:	89 c2                	mov    %eax,%edx
  404c75:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  404c79:	89 50 34             	mov    %edx,0x34(%rax)
  404c7c:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  404c80:	48 83 c0 2b          	add    $0x2b,%rax
  404c84:	48 8b 8d c8 f7 ff ff 	mov    -0x838(%rbp),%rcx
  404c8b:	8b 55 c8             	mov    -0x38(%rbp),%edx
  404c8e:	49 89 c9             	mov    %rcx,%r9
  404c91:	41 89 d0             	mov    %edx,%r8d
  404c94:	b9 00 00 00 00       	mov    $0x0,%ecx
  404c99:	ba 1f 00 00 00       	mov    $0x1f,%edx
  404c9e:	be 01 00 00 00       	mov    $0x1,%esi
  404ca3:	48 89 c7             	mov    %rax,%rdi
  404ca6:	e8 0f 05 00 00       	callq  4051ba <get_list>
  404cab:	0f be c0             	movsbl %al,%eax
  404cae:	89 45 c8             	mov    %eax,-0x38(%rbp)
  404cb1:	83 7d c8 ff          	cmpl   $0xffffffff,-0x38(%rbp)
  404cb5:	75 0c                	jne    404cc3 <load_entry+0xe43>
  404cb7:	c7 45 cc 03 00 00 00 	movl   $0x3,-0x34(%rbp)
  404cbe:	e9 4c 04 00 00       	jmpq   40510f <load_entry+0x128f>
  404cc3:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  404cc7:	48 83 c0 2f          	add    $0x2f,%rax
  404ccb:	48 8b 8d c8 f7 ff ff 	mov    -0x838(%rbp),%rcx
  404cd2:	8b 55 c8             	mov    -0x38(%rbp),%edx
  404cd5:	49 89 c9             	mov    %rcx,%r9
  404cd8:	41 89 d0             	mov    %edx,%r8d
  404cdb:	b9 00 b4 60 00       	mov    $0x60b400,%ecx
  404ce0:	ba 0c 00 00 00       	mov    $0xc,%edx
  404ce5:	be 01 00 00 00       	mov    $0x1,%esi
  404cea:	48 89 c7             	mov    %rax,%rdi
  404ced:	e8 c8 04 00 00       	callq  4051ba <get_list>
  404cf2:	0f be c0             	movsbl %al,%eax
  404cf5:	89 45 c8             	mov    %eax,-0x38(%rbp)
  404cf8:	83 7d c8 ff          	cmpl   $0xffffffff,-0x38(%rbp)
  404cfc:	75 0c                	jne    404d0a <load_entry+0xe8a>
  404cfe:	c7 45 cc 04 00 00 00 	movl   $0x4,-0x34(%rbp)
  404d05:	e9 05 04 00 00       	jmpq   40510f <load_entry+0x128f>
  404d0a:	83 7d c8 2a          	cmpl   $0x2a,-0x38(%rbp)
  404d0e:	75 13                	jne    404d23 <load_entry+0xea3>
  404d10:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  404d14:	8b 40 34             	mov    0x34(%rax),%eax
  404d17:	83 c8 02             	or     $0x2,%eax
  404d1a:	89 c2                	mov    %eax,%edx
  404d1c:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  404d20:	89 50 34             	mov    %edx,0x34(%rax)
  404d23:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  404d27:	48 83 c0 31          	add    $0x31,%rax
  404d2b:	48 8b 8d c8 f7 ff ff 	mov    -0x838(%rbp),%rcx
  404d32:	8b 55 c8             	mov    -0x38(%rbp),%edx
  404d35:	49 89 c9             	mov    %rcx,%r9
  404d38:	41 89 d0             	mov    %edx,%r8d
  404d3b:	b9 80 b4 60 00       	mov    $0x60b480,%ecx
  404d40:	ba 07 00 00 00       	mov    $0x7,%edx
  404d45:	be 00 00 00 00       	mov    $0x0,%esi
  404d4a:	48 89 c7             	mov    %rax,%rdi
  404d4d:	e8 68 04 00 00       	callq  4051ba <get_list>
  404d52:	0f be c0             	movsbl %al,%eax
  404d55:	89 45 c8             	mov    %eax,-0x38(%rbp)
  404d58:	83 7d c8 ff          	cmpl   $0xffffffff,-0x38(%rbp)
  404d5c:	75 0c                	jne    404d6a <load_entry+0xeea>
  404d5e:	c7 45 cc 05 00 00 00 	movl   $0x5,-0x34(%rbp)
  404d65:	e9 a5 03 00 00       	jmpq   40510f <load_entry+0x128f>
  404d6a:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  404d6e:	0f b6 40 31          	movzbl 0x31(%rax),%eax
  404d72:	0f b6 c0             	movzbl %al,%eax
  404d75:	83 e0 01             	and    $0x1,%eax
  404d78:	85 c0                	test   %eax,%eax
  404d7a:	75 0c                	jne    404d88 <load_entry+0xf08>
  404d7c:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  404d80:	0f b6 40 31          	movzbl 0x31(%rax),%eax
  404d84:	84 c0                	test   %al,%al
  404d86:	79 3c                	jns    404dc4 <load_entry+0xf44>
  404d88:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  404d8c:	0f b6 40 31          	movzbl 0x31(%rax),%eax
  404d90:	83 c8 01             	or     $0x1,%eax
  404d93:	89 c2                	mov    %eax,%edx
  404d95:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  404d99:	88 50 31             	mov    %dl,0x31(%rax)
  404d9c:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  404da0:	0f b6 40 31          	movzbl 0x31(%rax),%eax
  404da4:	83 c8 80             	or     $0xffffff80,%eax
  404da7:	89 c2                	mov    %eax,%edx
  404da9:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  404dad:	88 50 31             	mov    %dl,0x31(%rax)
  404db0:	eb 12                	jmp    404dc4 <load_entry+0xf44>
  404db2:	48 8b 85 c8 f7 ff ff 	mov    -0x838(%rbp),%rax
  404db9:	48 89 c7             	mov    %rax,%rdi
  404dbc:	e8 2a 24 00 00       	callq  4071eb <get_char>
  404dc1:	89 45 c8             	mov    %eax,-0x38(%rbp)
  404dc4:	83 7d c8 09          	cmpl   $0x9,-0x38(%rbp)
  404dc8:	74 e8                	je     404db2 <load_entry+0xf32>
  404dca:	83 7d c8 20          	cmpl   $0x20,-0x38(%rbp)
  404dce:	74 e2                	je     404db2 <load_entry+0xf32>
  404dd0:	48 8b 95 c8 f7 ff ff 	mov    -0x838(%rbp),%rdx
  404dd7:	8b 45 c8             	mov    -0x38(%rbp),%eax
  404dda:	48 89 d6             	mov    %rdx,%rsi
  404ddd:	89 c7                	mov    %eax,%edi
  404ddf:	e8 3c 24 00 00       	callq  407220 <unget_char>
  404de4:	48 83 bd b8 f7 ff ff 	cmpq   $0x0,-0x848(%rbp)
  404deb:	00 
  404dec:	75 66                	jne    404e54 <load_entry+0xfd4>
  404dee:	48 8d 85 c0 fb ff ff 	lea    -0x440(%rbp),%rax
  404df5:	48 89 45 b8          	mov    %rax,-0x48(%rbp)
  404df9:	48 8b 95 c8 f7 ff ff 	mov    -0x838(%rbp),%rdx
  404e00:	48 8b 45 b8          	mov    -0x48(%rbp),%rax
  404e04:	b9 44 8d 40 00       	mov    $0x408d44,%ecx
  404e09:	be e8 03 00 00       	mov    $0x3e8,%esi
  404e0e:	48 89 c7             	mov    %rax,%rdi
  404e11:	e8 42 24 00 00       	callq  407258 <get_string>
  404e16:	89 45 c8             	mov    %eax,-0x38(%rbp)
  404e19:	83 7d c8 ff          	cmpl   $0xffffffff,-0x38(%rbp)
  404e1d:	75 0c                	jne    404e2b <load_entry+0xfab>
  404e1f:	c7 45 cc 06 00 00 00 	movl   $0x6,-0x34(%rbp)
  404e26:	e9 e4 02 00 00       	jmpq   40510f <load_entry+0x128f>
  404e2b:	48 8b 45 b8          	mov    -0x48(%rbp),%rax
  404e2f:	48 89 c7             	mov    %rax,%rdi
  404e32:	e8 e9 d0 ff ff       	callq  401f20 <getpwnam@plt>
  404e37:	48 89 85 b8 f7 ff ff 	mov    %rax,-0x848(%rbp)
  404e3e:	48 83 bd b8 f7 ff ff 	cmpq   $0x0,-0x848(%rbp)
  404e45:	00 
  404e46:	75 1e                	jne    404e66 <load_entry+0xfe6>
  404e48:	c7 45 cc 08 00 00 00 	movl   $0x8,-0x34(%rbp)
  404e4f:	e9 bb 02 00 00       	jmpq   40510f <load_entry+0x128f>
  404e54:	83 7d c8 2a          	cmpl   $0x2a,-0x38(%rbp)
  404e58:	75 0c                	jne    404e66 <load_entry+0xfe6>
  404e5a:	c7 45 cc 06 00 00 00 	movl   $0x6,-0x34(%rbp)
  404e61:	e9 a9 02 00 00       	jmpq   40510f <load_entry+0x128f>
  404e66:	48 8b 85 b8 f7 ff ff 	mov    -0x848(%rbp),%rax
  404e6d:	8b 50 10             	mov    0x10(%rax),%edx
  404e70:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  404e74:	89 50 08             	mov    %edx,0x8(%rax)
  404e77:	48 8b 85 b8 f7 ff ff 	mov    -0x848(%rbp),%rax
  404e7e:	8b 50 14             	mov    0x14(%rax),%edx
  404e81:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  404e85:	89 50 0c             	mov    %edx,0xc(%rax)
  404e88:	48 8b 85 b0 f7 ff ff 	mov    -0x850(%rbp),%rax
  404e8f:	48 89 c7             	mov    %rax,%rdi
  404e92:	e8 66 2a 00 00       	callq  4078fd <env_copy>
  404e97:	48 89 c2             	mov    %rax,%rdx
  404e9a:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  404e9e:	48 89 50 10          	mov    %rdx,0x10(%rax)
  404ea2:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  404ea6:	48 8b 40 10          	mov    0x10(%rax),%rax
  404eaa:	48 85 c0             	test   %rax,%rax
  404ead:	75 0c                	jne    404ebb <load_entry+0x103b>
  404eaf:	c7 45 cc 00 00 00 00 	movl   $0x0,-0x34(%rbp)
  404eb6:	e9 54 02 00 00       	jmpq   40510f <load_entry+0x128f>
  404ebb:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  404ebf:	48 8b 40 10          	mov    0x10(%rax),%rax
  404ec3:	48 89 c6             	mov    %rax,%rsi
  404ec6:	bf 47 8d 40 00       	mov    $0x408d47,%edi
  404ecb:	e8 a2 30 00 00       	callq  407f72 <env_get>
  404ed0:	48 85 c0             	test   %rax,%rax
  404ed3:	75 62                	jne    404f37 <load_entry+0x10b7>
  404ed5:	48 8d 85 d0 f7 ff ff 	lea    -0x830(%rbp),%rax
  404edc:	b9 4d 8d 40 00       	mov    $0x408d4d,%ecx
  404ee1:	ba 55 8d 40 00       	mov    $0x408d55,%edx
  404ee6:	be e8 03 00 00       	mov    $0x3e8,%esi
  404eeb:	48 89 c7             	mov    %rax,%rdi
  404eee:	b8 00 00 00 00       	mov    $0x0,%eax
  404ef3:	e8 f8 ce ff ff       	callq  401df0 <snprintf@plt>
  404ef8:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  404efc:	48 8b 40 10          	mov    0x10(%rax),%rax
  404f00:	48 8d 95 d0 f7 ff ff 	lea    -0x830(%rbp),%rdx
  404f07:	48 89 d6             	mov    %rdx,%rsi
  404f0a:	48 89 c7             	mov    %rax,%rdi
  404f0d:	e8 e1 2a 00 00       	callq  4079f3 <env_set>
  404f12:	48 89 45 b0          	mov    %rax,-0x50(%rbp)
  404f16:	48 83 7d b0 00       	cmpq   $0x0,-0x50(%rbp)
  404f1b:	74 0e                	je     404f2b <load_entry+0x10ab>
  404f1d:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  404f21:	48 8b 55 b0          	mov    -0x50(%rbp),%rdx
  404f25:	48 89 50 10          	mov    %rdx,0x10(%rax)
  404f29:	eb 0c                	jmp    404f37 <load_entry+0x10b7>
  404f2b:	c7 45 cc 00 00 00 00 	movl   $0x0,-0x34(%rbp)
  404f32:	e9 d8 01 00 00       	jmpq   40510f <load_entry+0x128f>
  404f37:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  404f3b:	48 8b 40 10          	mov    0x10(%rax),%rax
  404f3f:	48 89 c6             	mov    %rax,%rsi
  404f42:	bf 5e 8d 40 00       	mov    $0x408d5e,%edi
  404f47:	e8 26 30 00 00       	callq  407f72 <env_get>
  404f4c:	48 85 c0             	test   %rax,%rax
  404f4f:	75 6b                	jne    404fbc <load_entry+0x113c>
  404f51:	48 8b 85 b8 f7 ff ff 	mov    -0x848(%rbp),%rax
  404f58:	48 8b 50 20          	mov    0x20(%rax),%rdx
  404f5c:	48 8d 85 d0 f7 ff ff 	lea    -0x830(%rbp),%rax
  404f63:	48 89 d1             	mov    %rdx,%rcx
  404f66:	ba 63 8d 40 00       	mov    $0x408d63,%edx
  404f6b:	be e8 03 00 00       	mov    $0x3e8,%esi
  404f70:	48 89 c7             	mov    %rax,%rdi
  404f73:	b8 00 00 00 00       	mov    $0x0,%eax
  404f78:	e8 73 ce ff ff       	callq  401df0 <snprintf@plt>
  404f7d:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  404f81:	48 8b 40 10          	mov    0x10(%rax),%rax
  404f85:	48 8d 95 d0 f7 ff ff 	lea    -0x830(%rbp),%rdx
  404f8c:	48 89 d6             	mov    %rdx,%rsi
  404f8f:	48 89 c7             	mov    %rax,%rdi
  404f92:	e8 5c 2a 00 00       	callq  4079f3 <env_set>
  404f97:	48 89 45 b0          	mov    %rax,-0x50(%rbp)
  404f9b:	48 83 7d b0 00       	cmpq   $0x0,-0x50(%rbp)
  404fa0:	74 0e                	je     404fb0 <load_entry+0x1130>
  404fa2:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  404fa6:	48 8b 55 b0          	mov    -0x50(%rbp),%rdx
  404faa:	48 89 50 10          	mov    %rdx,0x10(%rax)
  404fae:	eb 0c                	jmp    404fbc <load_entry+0x113c>
  404fb0:	c7 45 cc 00 00 00 00 	movl   $0x0,-0x34(%rbp)
  404fb7:	e9 53 01 00 00       	jmpq   40510f <load_entry+0x128f>
  404fbc:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  404fc0:	48 8b 40 10          	mov    0x10(%rax),%rax
  404fc4:	48 89 c6             	mov    %rax,%rsi
  404fc7:	bf 6b 8d 40 00       	mov    $0x408d6b,%edi
  404fcc:	e8 a1 2f 00 00       	callq  407f72 <env_get>
  404fd1:	48 85 c0             	test   %rax,%rax
  404fd4:	75 62                	jne    405038 <load_entry+0x11b8>
  404fd6:	48 8d 85 d0 f7 ff ff 	lea    -0x830(%rbp),%rax
  404fdd:	b9 70 8d 40 00       	mov    $0x408d70,%ecx
  404fe2:	ba 7e 8d 40 00       	mov    $0x408d7e,%edx
  404fe7:	be e8 03 00 00       	mov    $0x3e8,%esi
  404fec:	48 89 c7             	mov    %rax,%rdi
  404fef:	b8 00 00 00 00       	mov    $0x0,%eax
  404ff4:	e8 f7 cd ff ff       	callq  401df0 <snprintf@plt>
  404ff9:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  404ffd:	48 8b 40 10          	mov    0x10(%rax),%rax
  405001:	48 8d 95 d0 f7 ff ff 	lea    -0x830(%rbp),%rdx
  405008:	48 89 d6             	mov    %rdx,%rsi
  40500b:	48 89 c7             	mov    %rax,%rdi
  40500e:	e8 e0 29 00 00       	callq  4079f3 <env_set>
  405013:	48 89 45 b0          	mov    %rax,-0x50(%rbp)
  405017:	48 83 7d b0 00       	cmpq   $0x0,-0x50(%rbp)
  40501c:	74 0e                	je     40502c <load_entry+0x11ac>
  40501e:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  405022:	48 8b 55 b0          	mov    -0x50(%rbp),%rdx
  405026:	48 89 50 10          	mov    %rdx,0x10(%rax)
  40502a:	eb 0c                	jmp    405038 <load_entry+0x11b8>
  40502c:	c7 45 cc 00 00 00 00 	movl   $0x0,-0x34(%rbp)
  405033:	e9 d7 00 00 00       	jmpq   40510f <load_entry+0x128f>
  405038:	48 8b 85 b8 f7 ff ff 	mov    -0x848(%rbp),%rax
  40503f:	48 8b 10             	mov    (%rax),%rdx
  405042:	48 8d 85 d0 f7 ff ff 	lea    -0x830(%rbp),%rax
  405049:	49 89 d0             	mov    %rdx,%r8
  40504c:	b9 86 8d 40 00       	mov    $0x408d86,%ecx
  405051:	ba 8e 8d 40 00       	mov    $0x408d8e,%edx
  405056:	be e8 03 00 00       	mov    $0x3e8,%esi
  40505b:	48 89 c7             	mov    %rax,%rdi
  40505e:	b8 00 00 00 00       	mov    $0x0,%eax
  405063:	e8 88 cd ff ff       	callq  401df0 <snprintf@plt>
  405068:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  40506c:	48 8b 40 10          	mov    0x10(%rax),%rax
  405070:	48 8d 95 d0 f7 ff ff 	lea    -0x830(%rbp),%rdx
  405077:	48 89 d6             	mov    %rdx,%rsi
  40507a:	48 89 c7             	mov    %rax,%rdi
  40507d:	e8 71 29 00 00       	callq  4079f3 <env_set>
  405082:	48 89 45 b0          	mov    %rax,-0x50(%rbp)
  405086:	48 83 7d b0 00       	cmpq   $0x0,-0x50(%rbp)
  40508b:	74 37                	je     4050c4 <load_entry+0x1244>
  40508d:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  405091:	48 8b 55 b0          	mov    -0x50(%rbp),%rdx
  405095:	48 89 50 10          	mov    %rdx,0x10(%rax)
  405099:	48 8b 95 c8 f7 ff ff 	mov    -0x838(%rbp),%rdx
  4050a0:	48 8d 85 c0 fb ff ff 	lea    -0x440(%rbp),%rax
  4050a7:	b9 94 8d 40 00       	mov    $0x408d94,%ecx
  4050ac:	be e8 03 00 00       	mov    $0x3e8,%esi
  4050b1:	48 89 c7             	mov    %rax,%rdi
  4050b4:	e8 9f 21 00 00       	callq  407258 <get_string>
  4050b9:	89 45 c8             	mov    %eax,-0x38(%rbp)
  4050bc:	83 7d c8 ff          	cmpl   $0xffffffff,-0x38(%rbp)
  4050c0:	75 14                	jne    4050d6 <load_entry+0x1256>
  4050c2:	eb 09                	jmp    4050cd <load_entry+0x124d>
  4050c4:	c7 45 cc 00 00 00 00 	movl   $0x0,-0x34(%rbp)
  4050cb:	eb 42                	jmp    40510f <load_entry+0x128f>
  4050cd:	c7 45 cc 06 00 00 00 	movl   $0x6,-0x34(%rbp)
  4050d4:	eb 39                	jmp    40510f <load_entry+0x128f>
  4050d6:	48 8d 85 c0 fb ff ff 	lea    -0x440(%rbp),%rax
  4050dd:	48 89 c7             	mov    %rax,%rdi
  4050e0:	e8 1b d1 ff ff       	callq  402200 <strdup@plt>
  4050e5:	48 89 c2             	mov    %rax,%rdx
  4050e8:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  4050ec:	48 89 50 18          	mov    %rdx,0x18(%rax)
  4050f0:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  4050f4:	48 8b 40 18          	mov    0x18(%rax),%rax
  4050f8:	48 85 c0             	test   %rax,%rax
  4050fb:	75 09                	jne    405106 <load_entry+0x1286>
  4050fd:	c7 45 cc 00 00 00 00 	movl   $0x0,-0x34(%rbp)
  405104:	eb 09                	jmp    40510f <load_entry+0x128f>
  405106:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  40510a:	e9 99 00 00 00       	jmpq   4051a8 <load_entry+0x1328>
  40510f:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  405113:	48 8b 40 10          	mov    0x10(%rax),%rax
  405117:	48 85 c0             	test   %rax,%rax
  40511a:	74 10                	je     40512c <load_entry+0x12ac>
  40511c:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  405120:	48 8b 40 10          	mov    0x10(%rax),%rax
  405124:	48 89 c7             	mov    %rax,%rdi
  405127:	e8 83 27 00 00       	callq  4078af <env_free>
  40512c:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  405130:	48 8b 40 18          	mov    0x18(%rax),%rax
  405134:	48 85 c0             	test   %rax,%rax
  405137:	74 10                	je     405149 <load_entry+0x12c9>
  405139:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  40513d:	48 8b 40 18          	mov    0x18(%rax),%rax
  405141:	48 89 c7             	mov    %rax,%rdi
  405144:	e8 e7 ca ff ff       	callq  401c30 <free@plt>
  405149:	48 8b 45 c0          	mov    -0x40(%rbp),%rax
  40514d:	48 89 c7             	mov    %rax,%rdi
  405150:	e8 db ca ff ff       	callq  401c30 <free@plt>
  405155:	83 7d cc 00          	cmpl   $0x0,-0x34(%rbp)
  405159:	74 3c                	je     405197 <load_entry+0x1317>
  40515b:	48 83 bd c0 f7 ff ff 	cmpq   $0x0,-0x840(%rbp)
  405162:	00 
  405163:	74 32                	je     405197 <load_entry+0x1317>
  405165:	8b 45 cc             	mov    -0x34(%rbp),%eax
  405168:	48 98                	cltq   
  40516a:	48 8b 04 c5 a0 b5 60 	mov    0x60b5a0(,%rax,8),%rax
  405171:	00 
  405172:	48 8b 95 c0 f7 ff ff 	mov    -0x840(%rbp),%rdx
  405179:	48 89 c7             	mov    %rax,%rdi
  40517c:	b8 00 00 00 00       	mov    $0x0,%eax
  405181:	ff d2                	callq  *%rdx
  405183:	eb 12                	jmp    405197 <load_entry+0x1317>
  405185:	48 8b 85 c8 f7 ff ff 	mov    -0x838(%rbp),%rax
  40518c:	48 89 c7             	mov    %rax,%rdi
  40518f:	e8 57 20 00 00       	callq  4071eb <get_char>
  405194:	89 45 c8             	mov    %eax,-0x38(%rbp)
  405197:	83 7d c8 ff          	cmpl   $0xffffffff,-0x38(%rbp)
  40519b:	74 06                	je     4051a3 <load_entry+0x1323>
  40519d:	83 7d c8 0a          	cmpl   $0xa,-0x38(%rbp)
  4051a1:	75 e2                	jne    405185 <load_entry+0x1305>
  4051a3:	b8 00 00 00 00       	mov    $0x0,%eax
  4051a8:	48 81 c4 28 08 00 00 	add    $0x828,%rsp
  4051af:	5b                   	pop    %rbx
  4051b0:	41 5c                	pop    %r12
  4051b2:	41 5d                	pop    %r13
  4051b4:	41 5e                	pop    %r14
  4051b6:	41 5f                	pop    %r15
  4051b8:	5d                   	pop    %rbp
  4051b9:	c3                   	retq   

00000000004051ba <get_list>:
  4051ba:	55                   	push   %rbp
  4051bb:	48 89 e5             	mov    %rsp,%rbp
  4051be:	41 57                	push   %r15
  4051c0:	41 56                	push   %r14
  4051c2:	41 55                	push   %r13
  4051c4:	41 54                	push   %r12
  4051c6:	53                   	push   %rbx
  4051c7:	48 83 ec 38          	sub    $0x38,%rsp
  4051cb:	48 89 7d c8          	mov    %rdi,-0x38(%rbp)
  4051cf:	89 75 c4             	mov    %esi,-0x3c(%rbp)
  4051d2:	89 55 c0             	mov    %edx,-0x40(%rbp)
  4051d5:	48 89 4d b8          	mov    %rcx,-0x48(%rbp)
  4051d9:	44 89 45 b4          	mov    %r8d,-0x4c(%rbp)
  4051dd:	4c 89 4d a8          	mov    %r9,-0x58(%rbp)
  4051e1:	4c 8b 65 c8          	mov    -0x38(%rbp),%r12
  4051e5:	41 bf 00 00 00 00    	mov    $0x0,%r15d
  4051eb:	8b 45 c0             	mov    -0x40(%rbp),%eax
  4051ee:	2b 45 c4             	sub    -0x3c(%rbp),%eax
  4051f1:	44 8d 70 01          	lea    0x1(%rax),%r14d
  4051f5:	44 89 fb             	mov    %r15d,%ebx
  4051f8:	c1 fb 03             	sar    $0x3,%ebx
  4051fb:	45 89 f5             	mov    %r14d,%r13d
  4051fe:	41 c1 fd 03          	sar    $0x3,%r13d
  405202:	44 39 eb             	cmp    %r13d,%ebx
  405205:	75 4b                	jne    405252 <get_list+0x98>
  405207:	48 63 c3             	movslq %ebx,%rax
  40520a:	4c 01 e0             	add    %r12,%rax
  40520d:	48 63 d3             	movslq %ebx,%rdx
  405210:	4c 01 e2             	add    %r12,%rdx
  405213:	0f b6 12             	movzbl (%rdx),%edx
  405216:	89 d6                	mov    %edx,%esi
  405218:	44 89 fa             	mov    %r15d,%edx
  40521b:	83 e2 07             	and    $0x7,%edx
  40521e:	b9 08 00 00 00       	mov    $0x8,%ecx
  405223:	29 d1                	sub    %edx,%ecx
  405225:	89 ca                	mov    %ecx,%edx
  405227:	bf ff 00 00 00       	mov    $0xff,%edi
  40522c:	89 d1                	mov    %edx,%ecx
  40522e:	d3 ff                	sar    %cl,%edi
  405230:	89 fa                	mov    %edi,%edx
  405232:	41 89 d0             	mov    %edx,%r8d
  405235:	44 89 f2             	mov    %r14d,%edx
  405238:	83 e2 07             	and    $0x7,%edx
  40523b:	83 c2 01             	add    $0x1,%edx
  40523e:	bf ff 00 00 00       	mov    $0xff,%edi
  405243:	89 d1                	mov    %edx,%ecx
  405245:	d3 e7                	shl    %cl,%edi
  405247:	89 fa                	mov    %edi,%edx
  405249:	44 09 c2             	or     %r8d,%edx
  40524c:	21 f2                	and    %esi,%edx
  40524e:	88 10                	mov    %dl,(%rax)
  405250:	eb 6b                	jmp    4052bd <get_list+0x103>
  405252:	48 63 c3             	movslq %ebx,%rax
  405255:	4c 01 e0             	add    %r12,%rax
  405258:	48 63 d3             	movslq %ebx,%rdx
  40525b:	4c 01 e2             	add    %r12,%rdx
  40525e:	0f b6 12             	movzbl (%rdx),%edx
  405261:	89 d6                	mov    %edx,%esi
  405263:	44 89 fa             	mov    %r15d,%edx
  405266:	83 e2 07             	and    $0x7,%edx
  405269:	b9 08 00 00 00       	mov    $0x8,%ecx
  40526e:	29 d1                	sub    %edx,%ecx
  405270:	89 ca                	mov    %ecx,%edx
  405272:	bf ff 00 00 00       	mov    $0xff,%edi
  405277:	89 d1                	mov    %edx,%ecx
  405279:	d3 ff                	sar    %cl,%edi
  40527b:	89 fa                	mov    %edi,%edx
  40527d:	21 f2                	and    %esi,%edx
  40527f:	88 10                	mov    %dl,(%rax)
  405281:	eb 09                	jmp    40528c <get_list+0xd2>
  405283:	48 63 c3             	movslq %ebx,%rax
  405286:	4c 01 e0             	add    %r12,%rax
  405289:	c6 00 00             	movb   $0x0,(%rax)
  40528c:	83 c3 01             	add    $0x1,%ebx
  40528f:	44 39 eb             	cmp    %r13d,%ebx
  405292:	7c ef                	jl     405283 <get_list+0xc9>
  405294:	49 63 c5             	movslq %r13d,%rax
  405297:	4c 01 e0             	add    %r12,%rax
  40529a:	49 63 d5             	movslq %r13d,%rdx
  40529d:	4c 01 e2             	add    %r12,%rdx
  4052a0:	0f b6 12             	movzbl (%rdx),%edx
  4052a3:	89 d6                	mov    %edx,%esi
  4052a5:	44 89 f2             	mov    %r14d,%edx
  4052a8:	83 e2 07             	and    $0x7,%edx
  4052ab:	83 c2 01             	add    $0x1,%edx
  4052ae:	bf ff 00 00 00       	mov    $0xff,%edi
  4052b3:	89 d1                	mov    %edx,%ecx
  4052b5:	d3 e7                	shl    %cl,%edi
  4052b7:	89 fa                	mov    %edi,%edx
  4052b9:	21 f2                	and    %esi,%edx
  4052bb:	88 10                	mov    %dl,(%rax)
  4052bd:	bb 00 00 00 00       	mov    $0x0,%ebx
  4052c2:	eb 45                	jmp    405309 <get_list+0x14f>
  4052c4:	4c 8b 45 a8          	mov    -0x58(%rbp),%r8
  4052c8:	8b 7d b4             	mov    -0x4c(%rbp),%edi
  4052cb:	48 8b 4d b8          	mov    -0x48(%rbp),%rcx
  4052cf:	8b 55 c0             	mov    -0x40(%rbp),%edx
  4052d2:	8b 75 c4             	mov    -0x3c(%rbp),%esi
  4052d5:	48 8b 45 c8          	mov    -0x38(%rbp),%rax
  4052d9:	4d 89 c1             	mov    %r8,%r9
  4052dc:	41 89 f8             	mov    %edi,%r8d
  4052df:	48 89 c7             	mov    %rax,%rdi
  4052e2:	e8 7e 00 00 00       	callq  405365 <get_range>
  4052e7:	0f be c0             	movsbl %al,%eax
  4052ea:	89 45 b4             	mov    %eax,-0x4c(%rbp)
  4052ed:	83 7d b4 2c          	cmpl   $0x2c,-0x4c(%rbp)
  4052f1:	75 11                	jne    405304 <get_list+0x14a>
  4052f3:	48 8b 45 a8          	mov    -0x58(%rbp),%rax
  4052f7:	48 89 c7             	mov    %rax,%rdi
  4052fa:	e8 ec 1e 00 00       	callq  4071eb <get_char>
  4052ff:	89 45 b4             	mov    %eax,-0x4c(%rbp)
  405302:	eb 05                	jmp    405309 <get_list+0x14f>
  405304:	bb 01 00 00 00       	mov    $0x1,%ebx
  405309:	85 db                	test   %ebx,%ebx
  40530b:	74 b7                	je     4052c4 <get_list+0x10a>
  40530d:	eb 0f                	jmp    40531e <get_list+0x164>
  40530f:	48 8b 45 a8          	mov    -0x58(%rbp),%rax
  405313:	48 89 c7             	mov    %rax,%rdi
  405316:	e8 d0 1e 00 00       	callq  4071eb <get_char>
  40531b:	89 45 b4             	mov    %eax,-0x4c(%rbp)
  40531e:	83 7d b4 09          	cmpl   $0x9,-0x4c(%rbp)
  405322:	74 23                	je     405347 <get_list+0x18d>
  405324:	83 7d b4 20          	cmpl   $0x20,-0x4c(%rbp)
  405328:	74 1d                	je     405347 <get_list+0x18d>
  40532a:	83 7d b4 0a          	cmpl   $0xa,-0x4c(%rbp)
  40532e:	74 17                	je     405347 <get_list+0x18d>
  405330:	83 7d b4 ff          	cmpl   $0xffffffff,-0x4c(%rbp)
  405334:	75 d9                	jne    40530f <get_list+0x155>
  405336:	eb 0f                	jmp    405347 <get_list+0x18d>
  405338:	48 8b 45 a8          	mov    -0x58(%rbp),%rax
  40533c:	48 89 c7             	mov    %rax,%rdi
  40533f:	e8 a7 1e 00 00       	callq  4071eb <get_char>
  405344:	89 45 b4             	mov    %eax,-0x4c(%rbp)
  405347:	83 7d b4 09          	cmpl   $0x9,-0x4c(%rbp)
  40534b:	74 eb                	je     405338 <get_list+0x17e>
  40534d:	83 7d b4 20          	cmpl   $0x20,-0x4c(%rbp)
  405351:	74 e5                	je     405338 <get_list+0x17e>
  405353:	8b 45 b4             	mov    -0x4c(%rbp),%eax
  405356:	48 83 c4 38          	add    $0x38,%rsp
  40535a:	5b                   	pop    %rbx
  40535b:	41 5c                	pop    %r12
  40535d:	41 5d                	pop    %r13
  40535f:	41 5e                	pop    %r14
  405361:	41 5f                	pop    %r15
  405363:	5d                   	pop    %rbp
  405364:	c3                   	retq   

0000000000405365 <get_range>:
  405365:	55                   	push   %rbp
  405366:	48 89 e5             	mov    %rsp,%rbp
  405369:	53                   	push   %rbx
  40536a:	48 83 ec 48          	sub    $0x48,%rsp
  40536e:	48 89 7d d8          	mov    %rdi,-0x28(%rbp)
  405372:	89 75 d4             	mov    %esi,-0x2c(%rbp)
  405375:	89 55 d0             	mov    %edx,-0x30(%rbp)
  405378:	48 89 4d c8          	mov    %rcx,-0x38(%rbp)
  40537c:	44 89 45 c4          	mov    %r8d,-0x3c(%rbp)
  405380:	4c 89 4d b8          	mov    %r9,-0x48(%rbp)
  405384:	83 7d c4 2a          	cmpl   $0x2a,-0x3c(%rbp)
  405388:	75 2f                	jne    4053b9 <get_range+0x54>
  40538a:	8b 45 d4             	mov    -0x2c(%rbp),%eax
  40538d:	89 45 ec             	mov    %eax,-0x14(%rbp)
  405390:	8b 45 d0             	mov    -0x30(%rbp),%eax
  405393:	89 45 e8             	mov    %eax,-0x18(%rbp)
  405396:	48 8b 45 b8          	mov    -0x48(%rbp),%rax
  40539a:	48 89 c7             	mov    %rax,%rdi
  40539d:	e8 49 1e 00 00       	callq  4071eb <get_char>
  4053a2:	89 45 c4             	mov    %eax,-0x3c(%rbp)
  4053a5:	83 7d c4 ff          	cmpl   $0xffffffff,-0x3c(%rbp)
  4053a9:	0f 85 c1 00 00 00    	jne    405470 <get_range+0x10b>
  4053af:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  4053b4:	e9 7a 01 00 00       	jmpq   405533 <get_range+0x1ce>
  4053b9:	48 8b 7d b8          	mov    -0x48(%rbp),%rdi
  4053bd:	8b 4d c4             	mov    -0x3c(%rbp),%ecx
  4053c0:	48 8b 55 c8          	mov    -0x38(%rbp),%rdx
  4053c4:	8b 75 d4             	mov    -0x2c(%rbp),%esi
  4053c7:	48 8d 45 ec          	lea    -0x14(%rbp),%rax
  4053cb:	49 89 f8             	mov    %rdi,%r8
  4053ce:	48 89 c7             	mov    %rax,%rdi
  4053d1:	e8 64 01 00 00       	callq  40553a <get_number>
  4053d6:	0f be c0             	movsbl %al,%eax
  4053d9:	89 45 c4             	mov    %eax,-0x3c(%rbp)
  4053dc:	83 7d c4 ff          	cmpl   $0xffffffff,-0x3c(%rbp)
  4053e0:	75 0a                	jne    4053ec <get_range+0x87>
  4053e2:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  4053e7:	e9 47 01 00 00       	jmpq   405533 <get_range+0x1ce>
  4053ec:	83 7d c4 2d          	cmpl   $0x2d,-0x3c(%rbp)
  4053f0:	74 2c                	je     40541e <get_range+0xb9>
  4053f2:	8b 4d ec             	mov    -0x14(%rbp),%ecx
  4053f5:	8b 55 d0             	mov    -0x30(%rbp),%edx
  4053f8:	8b 75 d4             	mov    -0x2c(%rbp),%esi
  4053fb:	48 8b 45 d8          	mov    -0x28(%rbp),%rax
  4053ff:	48 89 c7             	mov    %rax,%rdi
  405402:	e8 d7 02 00 00       	callq  4056de <set_element>
  405407:	83 f8 ff             	cmp    $0xffffffff,%eax
  40540a:	75 0a                	jne    405416 <get_range+0xb1>
  40540c:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  405411:	e9 1d 01 00 00       	jmpq   405533 <get_range+0x1ce>
  405416:	8b 45 c4             	mov    -0x3c(%rbp),%eax
  405419:	e9 15 01 00 00       	jmpq   405533 <get_range+0x1ce>
  40541e:	48 8b 45 b8          	mov    -0x48(%rbp),%rax
  405422:	48 89 c7             	mov    %rax,%rdi
  405425:	e8 c1 1d 00 00       	callq  4071eb <get_char>
  40542a:	89 45 c4             	mov    %eax,-0x3c(%rbp)
  40542d:	83 7d c4 ff          	cmpl   $0xffffffff,-0x3c(%rbp)
  405431:	75 0a                	jne    40543d <get_range+0xd8>
  405433:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  405438:	e9 f6 00 00 00       	jmpq   405533 <get_range+0x1ce>
  40543d:	48 8b 7d b8          	mov    -0x48(%rbp),%rdi
  405441:	8b 4d c4             	mov    -0x3c(%rbp),%ecx
  405444:	48 8b 55 c8          	mov    -0x38(%rbp),%rdx
  405448:	8b 75 d4             	mov    -0x2c(%rbp),%esi
  40544b:	48 8d 45 e8          	lea    -0x18(%rbp),%rax
  40544f:	49 89 f8             	mov    %rdi,%r8
  405452:	48 89 c7             	mov    %rax,%rdi
  405455:	e8 e0 00 00 00       	callq  40553a <get_number>
  40545a:	0f be c0             	movsbl %al,%eax
  40545d:	89 45 c4             	mov    %eax,-0x3c(%rbp)
  405460:	83 7d c4 ff          	cmpl   $0xffffffff,-0x3c(%rbp)
  405464:	75 0a                	jne    405470 <get_range+0x10b>
  405466:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  40546b:	e9 c3 00 00 00       	jmpq   405533 <get_range+0x1ce>
  405470:	83 7d c4 2f          	cmpl   $0x2f,-0x3c(%rbp)
  405474:	75 5b                	jne    4054d1 <get_range+0x16c>
  405476:	48 8b 45 b8          	mov    -0x48(%rbp),%rax
  40547a:	48 89 c7             	mov    %rax,%rdi
  40547d:	e8 69 1d 00 00       	callq  4071eb <get_char>
  405482:	89 45 c4             	mov    %eax,-0x3c(%rbp)
  405485:	83 7d c4 ff          	cmpl   $0xffffffff,-0x3c(%rbp)
  405489:	75 0a                	jne    405495 <get_range+0x130>
  40548b:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  405490:	e9 9e 00 00 00       	jmpq   405533 <get_range+0x1ce>
  405495:	48 8b 4d b8          	mov    -0x48(%rbp),%rcx
  405499:	8b 55 c4             	mov    -0x3c(%rbp),%edx
  40549c:	48 8d 45 e4          	lea    -0x1c(%rbp),%rax
  4054a0:	49 89 c8             	mov    %rcx,%r8
  4054a3:	89 d1                	mov    %edx,%ecx
  4054a5:	ba 00 00 00 00       	mov    $0x0,%edx
  4054aa:	be 00 00 00 00       	mov    $0x0,%esi
  4054af:	48 89 c7             	mov    %rax,%rdi
  4054b2:	e8 83 00 00 00       	callq  40553a <get_number>
  4054b7:	0f be c0             	movsbl %al,%eax
  4054ba:	89 45 c4             	mov    %eax,-0x3c(%rbp)
  4054bd:	83 7d c4 ff          	cmpl   $0xffffffff,-0x3c(%rbp)
  4054c1:	74 07                	je     4054ca <get_range+0x165>
  4054c3:	8b 45 e4             	mov    -0x1c(%rbp),%eax
  4054c6:	85 c0                	test   %eax,%eax
  4054c8:	7f 0e                	jg     4054d8 <get_range+0x173>
  4054ca:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  4054cf:	eb 62                	jmp    405533 <get_range+0x1ce>
  4054d1:	c7 45 e4 01 00 00 00 	movl   $0x1,-0x1c(%rbp)
  4054d8:	8b 45 ec             	mov    -0x14(%rbp),%eax
  4054db:	3b 45 d4             	cmp    -0x2c(%rbp),%eax
  4054de:	7c 18                	jl     4054f8 <get_range+0x193>
  4054e0:	8b 45 ec             	mov    -0x14(%rbp),%eax
  4054e3:	3b 45 d0             	cmp    -0x30(%rbp),%eax
  4054e6:	7f 10                	jg     4054f8 <get_range+0x193>
  4054e8:	8b 45 e8             	mov    -0x18(%rbp),%eax
  4054eb:	3b 45 d4             	cmp    -0x2c(%rbp),%eax
  4054ee:	7c 08                	jl     4054f8 <get_range+0x193>
  4054f0:	8b 45 e8             	mov    -0x18(%rbp),%eax
  4054f3:	3b 45 d0             	cmp    -0x30(%rbp),%eax
  4054f6:	7e 07                	jle    4054ff <get_range+0x19a>
  4054f8:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  4054fd:	eb 34                	jmp    405533 <get_range+0x1ce>
  4054ff:	8b 5d ec             	mov    -0x14(%rbp),%ebx
  405502:	eb 25                	jmp    405529 <get_range+0x1c4>
  405504:	8b 55 d0             	mov    -0x30(%rbp),%edx
  405507:	8b 75 d4             	mov    -0x2c(%rbp),%esi
  40550a:	48 8b 45 d8          	mov    -0x28(%rbp),%rax
  40550e:	89 d9                	mov    %ebx,%ecx
  405510:	48 89 c7             	mov    %rax,%rdi
  405513:	e8 c6 01 00 00       	callq  4056de <set_element>
  405518:	83 f8 ff             	cmp    $0xffffffff,%eax
  40551b:	75 07                	jne    405524 <get_range+0x1bf>
  40551d:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  405522:	eb 0f                	jmp    405533 <get_range+0x1ce>
  405524:	8b 45 e4             	mov    -0x1c(%rbp),%eax
  405527:	01 c3                	add    %eax,%ebx
  405529:	8b 45 e8             	mov    -0x18(%rbp),%eax
  40552c:	39 c3                	cmp    %eax,%ebx
  40552e:	7e d4                	jle    405504 <get_range+0x19f>
  405530:	8b 45 c4             	mov    -0x3c(%rbp),%eax
  405533:	48 83 c4 48          	add    $0x48,%rsp
  405537:	5b                   	pop    %rbx
  405538:	5d                   	pop    %rbp
  405539:	c3                   	retq   

000000000040553a <get_number>:
  40553a:	55                   	push   %rbp
  40553b:	48 89 e5             	mov    %rsp,%rbp
  40553e:	48 81 ec 20 04 00 00 	sub    $0x420,%rsp
  405545:	48 89 bd f8 fb ff ff 	mov    %rdi,-0x408(%rbp)
  40554c:	89 b5 f4 fb ff ff    	mov    %esi,-0x40c(%rbp)
  405552:	48 89 95 e8 fb ff ff 	mov    %rdx,-0x418(%rbp)
  405559:	89 8d f0 fb ff ff    	mov    %ecx,-0x410(%rbp)
  40555f:	4c 89 85 e0 fb ff ff 	mov    %r8,-0x420(%rbp)
  405566:	48 8d 85 00 fc ff ff 	lea    -0x400(%rbp),%rax
  40556d:	48 89 45 f8          	mov    %rax,-0x8(%rbp)
  405571:	c7 45 f4 00 00 00 00 	movl   $0x0,-0xc(%rbp)
  405578:	c7 45 ec 01 00 00 00 	movl   $0x1,-0x14(%rbp)
  40557f:	eb 6d                	jmp    4055ee <get_number+0xb4>
  405581:	83 45 f4 01          	addl   $0x1,-0xc(%rbp)
  405585:	81 7d f4 e7 03 00 00 	cmpl   $0x3e7,-0xc(%rbp)
  40558c:	7e 0a                	jle    405598 <get_number+0x5e>
  40558e:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  405593:	e9 44 01 00 00       	jmpq   4056dc <get_number+0x1a2>
  405598:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  40559c:	48 8d 50 01          	lea    0x1(%rax),%rdx
  4055a0:	48 89 55 f8          	mov    %rdx,-0x8(%rbp)
  4055a4:	8b 95 f0 fb ff ff    	mov    -0x410(%rbp),%edx
  4055aa:	88 10                	mov    %dl,(%rax)
  4055ac:	e8 ef cc ff ff       	callq  4022a0 <__ctype_b_loc@plt>
  4055b1:	48 8b 00             	mov    (%rax),%rax
  4055b4:	8b 95 f0 fb ff ff    	mov    -0x410(%rbp),%edx
  4055ba:	48 63 d2             	movslq %edx,%rdx
  4055bd:	48 01 d2             	add    %rdx,%rdx
  4055c0:	48 01 d0             	add    %rdx,%rax
  4055c3:	0f b7 00             	movzwl (%rax),%eax
  4055c6:	0f b7 c0             	movzwl %ax,%eax
  4055c9:	25 00 08 00 00       	and    $0x800,%eax
  4055ce:	85 c0                	test   %eax,%eax
  4055d0:	75 07                	jne    4055d9 <get_number+0x9f>
  4055d2:	c7 45 ec 00 00 00 00 	movl   $0x0,-0x14(%rbp)
  4055d9:	48 8b 85 e0 fb ff ff 	mov    -0x420(%rbp),%rax
  4055e0:	48 89 c7             	mov    %rax,%rdi
  4055e3:	e8 03 1c 00 00       	callq  4071eb <get_char>
  4055e8:	89 85 f0 fb ff ff    	mov    %eax,-0x410(%rbp)
  4055ee:	e8 ad cc ff ff       	callq  4022a0 <__ctype_b_loc@plt>
  4055f3:	48 8b 00             	mov    (%rax),%rax
  4055f6:	8b 95 f0 fb ff ff    	mov    -0x410(%rbp),%edx
  4055fc:	48 63 d2             	movslq %edx,%rdx
  4055ff:	48 01 d2             	add    %rdx,%rdx
  405602:	48 01 d0             	add    %rdx,%rax
  405605:	0f b7 00             	movzwl (%rax),%eax
  405608:	0f b7 c0             	movzwl %ax,%eax
  40560b:	83 e0 08             	and    $0x8,%eax
  40560e:	85 c0                	test   %eax,%eax
  405610:	0f 85 6b ff ff ff    	jne    405581 <get_number+0x47>
  405616:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  40561a:	c6 00 00             	movb   $0x0,(%rax)
  40561d:	83 7d f4 00          	cmpl   $0x0,-0xc(%rbp)
  405621:	75 0a                	jne    40562d <get_number+0xf3>
  405623:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  405628:	e9 af 00 00 00       	jmpq   4056dc <get_number+0x1a2>
  40562d:	48 83 bd e8 fb ff ff 	cmpq   $0x0,-0x418(%rbp)
  405634:	00 
  405635:	74 78                	je     4056af <get_number+0x175>
  405637:	c7 45 f0 00 00 00 00 	movl   $0x0,-0x10(%rbp)
  40563e:	eb 50                	jmp    405690 <get_number+0x156>
  405640:	8b 45 f0             	mov    -0x10(%rbp),%eax
  405643:	48 98                	cltq   
  405645:	48 8d 14 c5 00 00 00 	lea    0x0(,%rax,8),%rdx
  40564c:	00 
  40564d:	48 8b 85 e8 fb ff ff 	mov    -0x418(%rbp),%rax
  405654:	48 01 d0             	add    %rdx,%rax
  405657:	48 8b 00             	mov    (%rax),%rax
  40565a:	48 8d 95 00 fc ff ff 	lea    -0x400(%rbp),%rdx
  405661:	48 89 d6             	mov    %rdx,%rsi
  405664:	48 89 c7             	mov    %rax,%rdi
  405667:	e8 d4 c5 ff ff       	callq  401c40 <strcasecmp@plt>
  40566c:	85 c0                	test   %eax,%eax
  40566e:	75 1c                	jne    40568c <get_number+0x152>
  405670:	8b 55 f0             	mov    -0x10(%rbp),%edx
  405673:	8b 85 f4 fb ff ff    	mov    -0x40c(%rbp),%eax
  405679:	01 c2                	add    %eax,%edx
  40567b:	48 8b 85 f8 fb ff ff 	mov    -0x408(%rbp),%rax
  405682:	89 10                	mov    %edx,(%rax)
  405684:	8b 85 f0 fb ff ff    	mov    -0x410(%rbp),%eax
  40568a:	eb 50                	jmp    4056dc <get_number+0x1a2>
  40568c:	83 45 f0 01          	addl   $0x1,-0x10(%rbp)
  405690:	8b 45 f0             	mov    -0x10(%rbp),%eax
  405693:	48 98                	cltq   
  405695:	48 8d 14 c5 00 00 00 	lea    0x0(,%rax,8),%rdx
  40569c:	00 
  40569d:	48 8b 85 e8 fb ff ff 	mov    -0x418(%rbp),%rax
  4056a4:	48 01 d0             	add    %rdx,%rax
  4056a7:	48 8b 00             	mov    (%rax),%rax
  4056aa:	48 85 c0             	test   %rax,%rax
  4056ad:	75 91                	jne    405640 <get_number+0x106>
  4056af:	83 7d ec 00          	cmpl   $0x0,-0x14(%rbp)
  4056b3:	74 22                	je     4056d7 <get_number+0x19d>
  4056b5:	48 8d 85 00 fc ff ff 	lea    -0x400(%rbp),%rax
  4056bc:	48 89 c7             	mov    %rax,%rdi
  4056bf:	e8 9c ca ff ff       	callq  402160 <atoi@plt>
  4056c4:	89 c2                	mov    %eax,%edx
  4056c6:	48 8b 85 f8 fb ff ff 	mov    -0x408(%rbp),%rax
  4056cd:	89 10                	mov    %edx,(%rax)
  4056cf:	8b 85 f0 fb ff ff    	mov    -0x410(%rbp),%eax
  4056d5:	eb 05                	jmp    4056dc <get_number+0x1a2>
  4056d7:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  4056dc:	c9                   	leaveq 
  4056dd:	c3                   	retq   

00000000004056de <set_element>:
  4056de:	55                   	push   %rbp
  4056df:	48 89 e5             	mov    %rsp,%rbp
  4056e2:	48 89 7d f8          	mov    %rdi,-0x8(%rbp)
  4056e6:	89 75 f4             	mov    %esi,-0xc(%rbp)
  4056e9:	89 55 f0             	mov    %edx,-0x10(%rbp)
  4056ec:	89 4d ec             	mov    %ecx,-0x14(%rbp)
  4056ef:	8b 45 ec             	mov    -0x14(%rbp),%eax
  4056f2:	3b 45 f4             	cmp    -0xc(%rbp),%eax
  4056f5:	7c 08                	jl     4056ff <set_element+0x21>
  4056f7:	8b 45 ec             	mov    -0x14(%rbp),%eax
  4056fa:	3b 45 f0             	cmp    -0x10(%rbp),%eax
  4056fd:	7e 07                	jle    405706 <set_element+0x28>
  4056ff:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  405704:	eb 48                	jmp    40574e <set_element+0x70>
  405706:	8b 45 ec             	mov    -0x14(%rbp),%eax
  405709:	2b 45 f4             	sub    -0xc(%rbp),%eax
  40570c:	c1 f8 03             	sar    $0x3,%eax
  40570f:	48 63 d0             	movslq %eax,%rdx
  405712:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  405716:	48 01 c2             	add    %rax,%rdx
  405719:	8b 45 ec             	mov    -0x14(%rbp),%eax
  40571c:	2b 45 f4             	sub    -0xc(%rbp),%eax
  40571f:	c1 f8 03             	sar    $0x3,%eax
  405722:	48 63 c8             	movslq %eax,%rcx
  405725:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  405729:	48 01 c8             	add    %rcx,%rax
  40572c:	0f b6 00             	movzbl (%rax),%eax
  40572f:	89 c7                	mov    %eax,%edi
  405731:	8b 45 ec             	mov    -0x14(%rbp),%eax
  405734:	2b 45 f4             	sub    -0xc(%rbp),%eax
  405737:	83 e0 07             	and    $0x7,%eax
  40573a:	be 01 00 00 00       	mov    $0x1,%esi
  40573f:	89 c1                	mov    %eax,%ecx
  405741:	d3 e6                	shl    %cl,%esi
  405743:	89 f0                	mov    %esi,%eax
  405745:	09 f8                	or     %edi,%eax
  405747:	88 02                	mov    %al,(%rdx)
  405749:	b8 00 00 00 00       	mov    $0x0,%eax
  40574e:	5d                   	pop    %rbp
  40574f:	c3                   	retq   

0000000000405750 <job_add>:
  405750:	55                   	push   %rbp
  405751:	48 89 e5             	mov    %rsp,%rbp
  405754:	41 55                	push   %r13
  405756:	41 54                	push   %r12
  405758:	53                   	push   %rbx
  405759:	48 83 ec 08          	sub    $0x8,%rsp
  40575d:	49 89 fd             	mov    %rdi,%r13
  405760:	49 89 f4             	mov    %rsi,%r12
  405763:	48 8b 1d be 65 20 00 	mov    0x2065be(%rip),%rbx        # 60bd28 <jhead>
  40576a:	eb 15                	jmp    405781 <job_add+0x31>
  40576c:	48 8b 43 08          	mov    0x8(%rbx),%rax
  405770:	4c 39 e8             	cmp    %r13,%rax
  405773:	75 09                	jne    40577e <job_add+0x2e>
  405775:	48 8b 43 10          	mov    0x10(%rbx),%rax
  405779:	4c 39 e0             	cmp    %r12,%rax
  40577c:	74 51                	je     4057cf <job_add+0x7f>
  40577e:	48 8b 1b             	mov    (%rbx),%rbx
  405781:	48 85 db             	test   %rbx,%rbx
  405784:	75 e6                	jne    40576c <job_add+0x1c>
  405786:	bf 18 00 00 00       	mov    $0x18,%edi
  40578b:	e8 40 c8 ff ff       	callq  401fd0 <malloc@plt>
  405790:	48 89 c3             	mov    %rax,%rbx
  405793:	48 85 db             	test   %rbx,%rbx
  405796:	74 3a                	je     4057d2 <job_add+0x82>
  405798:	48 c7 03 00 00 00 00 	movq   $0x0,(%rbx)
  40579f:	4c 89 6b 08          	mov    %r13,0x8(%rbx)
  4057a3:	4c 89 63 10          	mov    %r12,0x10(%rbx)
  4057a7:	48 8b 05 7a 65 20 00 	mov    0x20657a(%rip),%rax        # 60bd28 <jhead>
  4057ae:	48 85 c0             	test   %rax,%rax
  4057b1:	75 09                	jne    4057bc <job_add+0x6c>
  4057b3:	48 89 1d 6e 65 20 00 	mov    %rbx,0x20656e(%rip)        # 60bd28 <jhead>
  4057ba:	eb 0a                	jmp    4057c6 <job_add+0x76>
  4057bc:	48 8b 05 6d 65 20 00 	mov    0x20656d(%rip),%rax        # 60bd30 <jtail>
  4057c3:	48 89 18             	mov    %rbx,(%rax)
  4057c6:	48 89 1d 63 65 20 00 	mov    %rbx,0x206563(%rip)        # 60bd30 <jtail>
  4057cd:	eb 04                	jmp    4057d3 <job_add+0x83>
  4057cf:	90                   	nop
  4057d0:	eb 01                	jmp    4057d3 <job_add+0x83>
  4057d2:	90                   	nop
  4057d3:	48 83 c4 08          	add    $0x8,%rsp
  4057d7:	5b                   	pop    %rbx
  4057d8:	41 5c                	pop    %r12
  4057da:	41 5d                	pop    %r13
  4057dc:	5d                   	pop    %rbp
  4057dd:	c3                   	retq   

00000000004057de <job_runqueue>:
  4057de:	55                   	push   %rbp
  4057df:	48 89 e5             	mov    %rsp,%rbp
  4057e2:	41 55                	push   %r13
  4057e4:	41 54                	push   %r12
  4057e6:	53                   	push   %rbx
  4057e7:	48 83 ec 08          	sub    $0x8,%rsp
  4057eb:	41 bc 00 00 00 00    	mov    $0x0,%r12d
  4057f1:	48 8b 1d 30 65 20 00 	mov    0x206530(%rip),%rbx        # 60bd28 <jhead>
  4057f8:	eb 25                	jmp    40581f <job_runqueue+0x41>
  4057fa:	48 8b 53 10          	mov    0x10(%rbx),%rdx
  4057fe:	48 8b 43 08          	mov    0x8(%rbx),%rax
  405802:	48 89 d6             	mov    %rdx,%rsi
  405805:	48 89 c7             	mov    %rax,%rdi
  405808:	e8 54 00 00 00       	callq  405861 <do_command>
  40580d:	4c 8b 2b             	mov    (%rbx),%r13
  405810:	48 89 df             	mov    %rbx,%rdi
  405813:	e8 18 c4 ff ff       	callq  401c30 <free@plt>
  405818:	41 83 c4 01          	add    $0x1,%r12d
  40581c:	4c 89 eb             	mov    %r13,%rbx
  40581f:	48 85 db             	test   %rbx,%rbx
  405822:	75 d6                	jne    4057fa <job_runqueue+0x1c>
  405824:	48 c7 05 01 65 20 00 	movq   $0x0,0x206501(%rip)        # 60bd30 <jtail>
  40582b:	00 00 00 00 
  40582f:	48 8b 05 fa 64 20 00 	mov    0x2064fa(%rip),%rax        # 60bd30 <jtail>
  405836:	48 89 05 eb 64 20 00 	mov    %rax,0x2064eb(%rip)        # 60bd28 <jhead>
  40583d:	44 89 e0             	mov    %r12d,%eax
  405840:	48 83 c4 08          	add    $0x8,%rsp
  405844:	5b                   	pop    %rbx
  405845:	41 5c                	pop    %r12
  405847:	41 5d                	pop    %r13
  405849:	5d                   	pop    %rbp
  40584a:	c3                   	retq   

000000000040584b <build_env>:
  40584b:	55                   	push   %rbp
  40584c:	48 89 e5             	mov    %rsp,%rbp
  40584f:	48 89 7d e8          	mov    %rdi,-0x18(%rbp)
  405853:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  405857:	48 89 45 f8          	mov    %rax,-0x8(%rbp)
  40585b:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  40585f:	5d                   	pop    %rbp
  405860:	c3                   	retq   

0000000000405861 <do_command>:
  405861:	55                   	push   %rbp
  405862:	48 89 e5             	mov    %rsp,%rbp
  405865:	48 83 ec 10          	sub    $0x10,%rsp
  405869:	48 89 7d f8          	mov    %rdi,-0x8(%rbp)
  40586d:	48 89 75 f0          	mov    %rsi,-0x10(%rbp)
  405871:	e8 fa c9 ff ff       	callq  402270 <fork@plt>
  405876:	83 f8 ff             	cmp    $0xffffffff,%eax
  405879:	74 06                	je     405881 <do_command+0x20>
  40587b:	85 c0                	test   %eax,%eax
  40587d:	74 1f                	je     40589e <do_command+0x3d>
  40587f:	eb 44                	jmp    4058c5 <do_command+0x64>
  405881:	e8 ba c4 ff ff       	callq  401d40 <getpid@plt>
  405886:	b9 98 8d 40 00       	mov    $0x408d98,%ecx
  40588b:	ba a3 8d 40 00       	mov    $0x408da3,%edx
  405890:	89 c6                	mov    %eax,%esi
  405892:	bf a9 8d 40 00       	mov    $0x408da9,%edi
  405897:	e8 3d 1c 00 00       	callq  4074d9 <log_it>
  40589c:	eb 27                	jmp    4058c5 <do_command+0x64>
  40589e:	bf 01 00 00 00       	mov    $0x1,%edi
  4058a3:	e8 54 16 00 00       	callq  406efc <acquire_daemonlock>
  4058a8:	48 8b 55 f0          	mov    -0x10(%rbp),%rdx
  4058ac:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  4058b0:	48 89 d6             	mov    %rdx,%rsi
  4058b3:	48 89 c7             	mov    %rax,%rdi
  4058b6:	e8 0d 00 00 00       	callq  4058c8 <child_process>
  4058bb:	bf 00 00 00 00       	mov    $0x0,%edi
  4058c0:	e8 eb c3 ff ff       	callq  401cb0 <_exit@plt>
  4058c5:	90                   	nop
  4058c6:	c9                   	leaveq 
  4058c7:	c3                   	retq   

00000000004058c8 <child_process>:
  4058c8:	55                   	push   %rbp
  4058c9:	48 89 e5             	mov    %rsp,%rbp
  4058cc:	41 57                	push   %r15
  4058ce:	41 56                	push   %r14
  4058d0:	41 55                	push   %r13
  4058d2:	41 54                	push   %r12
  4058d4:	53                   	push   %rbx
  4058d5:	48 81 ec d8 25 00 00 	sub    $0x25d8,%rsp
  4058dc:	48 89 bd 08 da ff ff 	mov    %rdi,-0x25f8(%rbp)
  4058e3:	48 89 b5 00 da ff ff 	mov    %rsi,-0x2600(%rbp)
  4058ea:	c7 45 c4 00 00 00 00 	movl   $0x0,-0x3c(%rbp)
  4058f1:	48 8b 1d 90 6c 20 00 	mov    0x206c90(%rip),%rbx        # 60c588 <ProgramName>
  4058f8:	eb 3c                	jmp    405936 <child_process+0x6e>
  4058fa:	e8 a1 c9 ff ff       	callq  4022a0 <__ctype_b_loc@plt>
  4058ff:	48 8b 00             	mov    (%rax),%rax
  405902:	0f b6 13             	movzbl (%rbx),%edx
  405905:	48 0f be d2          	movsbq %dl,%rdx
  405909:	48 01 d2             	add    %rdx,%rdx
  40590c:	48 01 d0             	add    %rdx,%rax
  40590f:	0f b7 00             	movzwl (%rax),%eax
  405912:	0f b7 c0             	movzwl %ax,%eax
  405915:	25 00 02 00 00       	and    $0x200,%eax
  40591a:	85 c0                	test   %eax,%eax
  40591c:	74 0f                	je     40592d <child_process+0x65>
  40591e:	0f b6 03             	movzbl (%rbx),%eax
  405921:	0f be c0             	movsbl %al,%eax
  405924:	89 c7                	mov    %eax,%edi
  405926:	e8 c5 c3 ff ff       	callq  401cf0 <toupper@plt>
  40592b:	eb 03                	jmp    405930 <child_process+0x68>
  40592d:	0f b6 03             	movzbl (%rbx),%eax
  405930:	88 03                	mov    %al,(%rbx)
  405932:	48 83 c3 01          	add    $0x1,%rbx
  405936:	0f b6 03             	movzbl (%rbx),%eax
  405939:	84 c0                	test   %al,%al
  40593b:	75 bd                	jne    4058fa <child_process+0x32>
  40593d:	48 8b 85 08 da ff ff 	mov    -0x25f8(%rbp),%rax
  405944:	48 8b 40 10          	mov    0x10(%rax),%rax
  405948:	48 89 c6             	mov    %rax,%rsi
  40594b:	bf ae 8d 40 00       	mov    $0x408dae,%edi
  405950:	e8 1d 26 00 00       	callq  407f72 <env_get>
  405955:	48 89 45 90          	mov    %rax,-0x70(%rbp)
  405959:	48 8b 85 08 da ff ff 	mov    -0x25f8(%rbp),%rax
  405960:	48 8b 40 10          	mov    0x10(%rax),%rax
  405964:	48 89 c6             	mov    %rax,%rsi
  405967:	bf b6 8d 40 00       	mov    $0x408db6,%edi
  40596c:	e8 01 26 00 00       	callq  407f72 <env_get>
  405971:	48 89 45 c8          	mov    %rax,-0x38(%rbp)
  405975:	48 8b 85 08 da ff ff 	mov    -0x25f8(%rbp),%rax
  40597c:	48 8b 40 10          	mov    0x10(%rax),%rax
  405980:	48 89 c6             	mov    %rax,%rsi
  405983:	bf bd 8d 40 00       	mov    $0x408dbd,%edi
  405988:	e8 e5 25 00 00       	callq  407f72 <env_get>
  40598d:	48 89 45 88          	mov    %rax,-0x78(%rbp)
  405991:	48 83 7d c8 00       	cmpq   $0x0,-0x38(%rbp)
  405996:	74 77                	je     405a0f <child_process+0x147>
  405998:	48 8b 45 c8          	mov    -0x38(%rbp),%rax
  40599c:	be c9 8d 40 00       	mov    $0x408dc9,%esi
  4059a1:	48 89 c7             	mov    %rax,%rdi
  4059a4:	e8 c7 c4 ff ff       	callq  401e70 <strspn@plt>
  4059a9:	48 01 45 c8          	add    %rax,-0x38(%rbp)
  4059ad:	48 8b 45 c8          	mov    -0x38(%rbp),%rax
  4059b1:	be c9 8d 40 00       	mov    $0x408dc9,%esi
  4059b6:	48 89 c7             	mov    %rax,%rdi
  4059b9:	e8 f2 c4 ff ff       	callq  401eb0 <strcspn@plt>
  4059be:	48 89 c2             	mov    %rax,%rdx
  4059c1:	48 8b 45 c8          	mov    -0x38(%rbp),%rax
  4059c5:	48 01 d0             	add    %rdx,%rax
  4059c8:	48 89 45 80          	mov    %rax,-0x80(%rbp)
  4059cc:	48 8b 45 c8          	mov    -0x38(%rbp),%rax
  4059d0:	0f b6 00             	movzbl (%rax),%eax
  4059d3:	3c 2d                	cmp    $0x2d,%al
  4059d5:	74 0b                	je     4059e2 <child_process+0x11a>
  4059d7:	48 8b 45 80          	mov    -0x80(%rbp),%rax
  4059db:	0f b6 00             	movzbl (%rax),%eax
  4059de:	84 c0                	test   %al,%al
  4059e0:	74 2d                	je     405a0f <child_process+0x147>
  4059e2:	bf cd 8d 40 00       	mov    $0x408dcd,%edi
  4059e7:	e8 14 c3 ff ff       	callq  401d00 <puts@plt>
  4059ec:	e8 4f c3 ff ff       	callq  401d40 <getpid@plt>
  4059f1:	b9 df 8d 40 00       	mov    $0x408ddf,%ecx
  4059f6:	ba a3 8d 40 00       	mov    $0x408da3,%edx
  4059fb:	89 c6                	mov    %eax,%esi
  4059fd:	bf a9 8d 40 00       	mov    $0x408da9,%edi
  405a02:	e8 d2 1a 00 00       	callq  4074d9 <log_it>
  405a07:	48 c7 45 c8 00 00 00 	movq   $0x0,-0x38(%rbp)
  405a0e:	00 
  405a0f:	be 00 00 00 00       	mov    $0x0,%esi
  405a14:	bf 11 00 00 00       	mov    $0x11,%edi
  405a19:	e8 f2 c4 ff ff       	callq  401f10 <signal@plt>
  405a1e:	48 8d 85 90 fe ff ff 	lea    -0x170(%rbp),%rax
  405a25:	48 89 c7             	mov    %rax,%rdi
  405a28:	e8 23 c4 ff ff       	callq  401e50 <pipe@plt>
  405a2d:	e8 be c4 ff ff       	callq  401ef0 <tmpfile@plt>
  405a32:	48 89 85 78 ff ff ff 	mov    %rax,-0x88(%rbp)
  405a39:	48 83 bd 78 ff ff ff 	cmpq   $0x0,-0x88(%rbp)
  405a40:	00 
  405a41:	75 25                	jne    405a68 <child_process+0x1a0>
  405a43:	e8 f8 c2 ff ff       	callq  401d40 <getpid@plt>
  405a48:	b9 ea 8d 40 00       	mov    $0x408dea,%ecx
  405a4d:	ba a3 8d 40 00       	mov    $0x408da3,%edx
  405a52:	89 c6                	mov    %eax,%esi
  405a54:	bf a9 8d 40 00       	mov    $0x408da9,%edi
  405a59:	e8 7b 1a 00 00       	callq  4074d9 <log_it>
  405a5e:	bf 01 00 00 00       	mov    $0x1,%edi
  405a63:	e8 58 c7 ff ff       	callq  4021c0 <exit@plt>
  405a68:	41 be 00 00 00 00    	mov    $0x0,%r14d
  405a6e:	48 8b 85 08 da ff ff 	mov    -0x25f8(%rbp),%rax
  405a75:	48 8b 58 18          	mov    0x18(%rax),%rbx
  405a79:	49 89 dc             	mov    %rbx,%r12
  405a7c:	eb 54                	jmp    405ad2 <child_process+0x20a>
  405a7e:	4c 39 e3             	cmp    %r12,%rbx
  405a81:	74 05                	je     405a88 <child_process+0x1c0>
  405a83:	44 89 e8             	mov    %r13d,%eax
  405a86:	88 03                	mov    %al,(%rbx)
  405a88:	45 85 f6             	test   %r14d,%r14d
  405a8b:	74 1d                	je     405aaa <child_process+0x1e2>
  405a8d:	41 83 fd 25          	cmp    $0x25,%r13d
  405a91:	74 06                	je     405a99 <child_process+0x1d1>
  405a93:	41 83 fd 5c          	cmp    $0x5c,%r13d
  405a97:	75 09                	jne    405aa2 <child_process+0x1da>
  405a99:	48 83 eb 01          	sub    $0x1,%rbx
  405a9d:	44 89 e8             	mov    %r13d,%eax
  405aa0:	88 03                	mov    %al,(%rbx)
  405aa2:	41 be 00 00 00 00    	mov    $0x0,%r14d
  405aa8:	eb 20                	jmp    405aca <child_process+0x202>
  405aaa:	41 83 fd 5c          	cmp    $0x5c,%r13d
  405aae:	75 08                	jne    405ab8 <child_process+0x1f0>
  405ab0:	41 be 01 00 00 00    	mov    $0x1,%r14d
  405ab6:	eb 12                	jmp    405aca <child_process+0x202>
  405ab8:	41 83 fd 25          	cmp    $0x25,%r13d
  405abc:	75 0c                	jne    405aca <child_process+0x202>
  405abe:	4c 89 e0             	mov    %r12,%rax
  405ac1:	4c 8d 60 01          	lea    0x1(%rax),%r12
  405ac5:	c6 00 00             	movb   $0x0,(%rax)
  405ac8:	eb 16                	jmp    405ae0 <child_process+0x218>
  405aca:	49 83 c4 01          	add    $0x1,%r12
  405ace:	48 83 c3 01          	add    $0x1,%rbx
  405ad2:	41 0f b6 04 24       	movzbl (%r12),%eax
  405ad7:	44 0f be e8          	movsbl %al,%r13d
  405adb:	45 85 ed             	test   %r13d,%r13d
  405ade:	75 9e                	jne    405a7e <child_process+0x1b6>
  405ae0:	c6 03 00             	movb   $0x0,(%rbx)
  405ae3:	e8 88 c7 ff ff       	callq  402270 <fork@plt>
  405ae8:	89 85 74 ff ff ff    	mov    %eax,-0x8c(%rbp)
  405aee:	8b 85 74 ff ff ff    	mov    -0x8c(%rbp),%eax
  405af4:	83 f8 ff             	cmp    $0xffffffff,%eax
  405af7:	74 09                	je     405b02 <child_process+0x23a>
  405af9:	85 c0                	test   %eax,%eax
  405afb:	74 2a                	je     405b27 <child_process+0x25f>
  405afd:	e9 88 03 00 00       	jmpq   405e8a <child_process+0x5c2>
  405b02:	e8 39 c2 ff ff       	callq  401d40 <getpid@plt>
  405b07:	b9 98 8d 40 00       	mov    $0x408d98,%ecx
  405b0c:	ba a3 8d 40 00       	mov    $0x408da3,%edx
  405b11:	89 c6                	mov    %eax,%esi
  405b13:	bf a9 8d 40 00       	mov    $0x408da9,%edi
  405b18:	e8 bc 19 00 00       	callq  4074d9 <log_it>
  405b1d:	bf 01 00 00 00       	mov    $0x1,%edi
  405b22:	e8 99 c6 ff ff       	callq  4021c0 <exit@plt>
  405b27:	8b 05 9b 59 20 00    	mov    0x20599b(%rip),%eax        # 60b4c8 <log_level>
  405b2d:	83 e0 01             	and    $0x1,%eax
  405b30:	85 c0                	test   %eax,%eax
  405b32:	74 6f                	je     405ba3 <child_process+0x2db>
  405b34:	8b 05 8e 59 20 00    	mov    0x20598e(%rip),%eax        # 60b4c8 <log_level>
  405b3a:	83 e0 08             	and    $0x8,%eax
  405b3d:	85 c0                	test   %eax,%eax
  405b3f:	75 62                	jne    405ba3 <child_process+0x2db>
  405b41:	48 8b 85 08 da ff ff 	mov    -0x25f8(%rbp),%rax
  405b48:	48 8b 40 18          	mov    0x18(%rax),%rax
  405b4c:	48 89 c7             	mov    %rax,%rdi
  405b4f:	e8 1c c2 ff ff       	callq  401d70 <strlen@plt>
  405b54:	89 c2                	mov    %eax,%edx
  405b56:	48 8b 85 08 da ff ff 	mov    -0x25f8(%rbp),%rax
  405b5d:	48 8b 40 18          	mov    0x18(%rax),%rax
  405b61:	89 d6                	mov    %edx,%esi
  405b63:	48 89 c7             	mov    %rax,%rdi
  405b66:	e8 38 1b 00 00       	callq  4076a3 <mkprints>
  405b6b:	48 89 85 18 ff ff ff 	mov    %rax,-0xe8(%rbp)
  405b72:	e8 c9 c1 ff ff       	callq  401d40 <getpid@plt>
  405b77:	89 c6                	mov    %eax,%esi
  405b79:	48 8b 95 18 ff ff ff 	mov    -0xe8(%rbp),%rdx
  405b80:	48 8b 45 90          	mov    -0x70(%rbp),%rax
  405b84:	48 89 d1             	mov    %rdx,%rcx
  405b87:	ba f9 8d 40 00       	mov    $0x408df9,%edx
  405b8c:	48 89 c7             	mov    %rax,%rdi
  405b8f:	e8 45 19 00 00       	callq  4074d9 <log_it>
  405b94:	48 8b 85 18 ff ff ff 	mov    -0xe8(%rbp),%rax
  405b9b:	48 89 c7             	mov    %rax,%rdi
  405b9e:	e8 8d c0 ff ff       	callq  401c30 <free@plt>
  405ba3:	e8 92 19 00 00       	callq  40753a <log_close>
  405ba8:	e8 b3 c2 ff ff       	callq  401e60 <setsid@plt>
  405bad:	8b 85 94 fe ff ff    	mov    -0x16c(%rbp),%eax
  405bb3:	89 c7                	mov    %eax,%edi
  405bb5:	e8 86 c2 ff ff       	callq  401e40 <close@plt>
  405bba:	8b 85 90 fe ff ff    	mov    -0x170(%rbp),%eax
  405bc0:	be 00 00 00 00       	mov    $0x0,%esi
  405bc5:	89 c7                	mov    %eax,%edi
  405bc7:	e8 e4 c1 ff ff       	callq  401db0 <dup2@plt>
  405bcc:	48 8b 85 78 ff ff ff 	mov    -0x88(%rbp),%rax
  405bd3:	48 89 c7             	mov    %rax,%rdi
  405bd6:	e8 b5 c3 ff ff       	callq  401f90 <fileno@plt>
  405bdb:	be 01 00 00 00       	mov    $0x1,%esi
  405be0:	89 c7                	mov    %eax,%edi
  405be2:	e8 c9 c1 ff ff       	callq  401db0 <dup2@plt>
  405be7:	be 02 00 00 00       	mov    $0x2,%esi
  405bec:	bf 01 00 00 00       	mov    $0x1,%edi
  405bf1:	e8 ba c1 ff ff       	callq  401db0 <dup2@plt>
  405bf6:	8b 85 90 fe ff ff    	mov    -0x170(%rbp),%eax
  405bfc:	89 c7                	mov    %eax,%edi
  405bfe:	e8 3d c2 ff ff       	callq  401e40 <close@plt>
  405c03:	48 8b 85 78 ff ff ff 	mov    -0x88(%rbp),%rax
  405c0a:	48 89 c7             	mov    %rax,%rdi
  405c0d:	e8 3e c1 ff ff       	callq  401d50 <fclose@plt>
  405c12:	48 8b 85 00 da ff ff 	mov    -0x2600(%rbp),%rax
  405c19:	48 89 c7             	mov    %rax,%rdi
  405c1c:	e8 1c 0e 00 00       	callq  406a3d <do_univ>
  405c21:	48 8b 85 08 da ff ff 	mov    -0x25f8(%rbp),%rax
  405c28:	8b 40 0c             	mov    0xc(%rax),%eax
  405c2b:	89 c7                	mov    %eax,%edi
  405c2d:	e8 6e c4 ff ff       	callq  4020a0 <setgid@plt>
  405c32:	85 c0                	test   %eax,%eax
  405c34:	74 6b                	je     405ca1 <child_process+0x3d9>
  405c36:	e8 45 c0 ff ff       	callq  401c80 <__errno_location@plt>
  405c3b:	8b 00                	mov    (%rax),%eax
  405c3d:	89 c7                	mov    %eax,%edi
  405c3f:	e8 cc c5 ff ff       	callq  402210 <strerror@plt>
  405c44:	48 89 c1             	mov    %rax,%rcx
  405c47:	48 8b 85 08 da ff ff 	mov    -0x25f8(%rbp),%rax
  405c4e:	8b 40 0c             	mov    0xc(%rax),%eax
  405c51:	89 c2                	mov    %eax,%edx
  405c53:	48 8d 85 10 da ff ff 	lea    -0x25f0(%rbp),%rax
  405c5a:	49 89 c8             	mov    %rcx,%r8
  405c5d:	48 89 d1             	mov    %rdx,%rcx
  405c60:	ba 00 8e 40 00       	mov    $0x408e00,%edx
  405c65:	be 00 01 00 00       	mov    $0x100,%esi
  405c6a:	48 89 c7             	mov    %rax,%rdi
  405c6d:	b8 00 00 00 00       	mov    $0x0,%eax
  405c72:	e8 79 c1 ff ff       	callq  401df0 <snprintf@plt>
  405c77:	e8 c4 c0 ff ff       	callq  401d40 <getpid@plt>
  405c7c:	89 c6                	mov    %eax,%esi
  405c7e:	48 8d 85 10 da ff ff 	lea    -0x25f0(%rbp),%rax
  405c85:	48 89 c1             	mov    %rax,%rcx
  405c88:	ba a3 8d 40 00       	mov    $0x408da3,%edx
  405c8d:	bf a9 8d 40 00       	mov    $0x408da9,%edi
  405c92:	e8 42 18 00 00       	callq  4074d9 <log_it>
  405c97:	bf 01 00 00 00       	mov    $0x1,%edi
  405c9c:	e8 1f c5 ff ff       	callq  4021c0 <exit@plt>
  405ca1:	48 8b 85 08 da ff ff 	mov    -0x25f8(%rbp),%rax
  405ca8:	8b 58 0c             	mov    0xc(%rax),%ebx
  405cab:	48 8b 85 08 da ff ff 	mov    -0x25f8(%rbp),%rax
  405cb2:	48 8b 40 10          	mov    0x10(%rax),%rax
  405cb6:	48 89 c6             	mov    %rax,%rsi
  405cb9:	bf ae 8d 40 00       	mov    $0x408dae,%edi
  405cbe:	e8 af 22 00 00       	callq  407f72 <env_get>
  405cc3:	89 de                	mov    %ebx,%esi
  405cc5:	48 89 c7             	mov    %rax,%rdi
  405cc8:	e8 53 c5 ff ff       	callq  402220 <initgroups@plt>
  405ccd:	85 c0                	test   %eax,%eax
  405ccf:	74 6b                	je     405d3c <child_process+0x474>
  405cd1:	e8 aa bf ff ff       	callq  401c80 <__errno_location@plt>
  405cd6:	8b 00                	mov    (%rax),%eax
  405cd8:	89 c7                	mov    %eax,%edi
  405cda:	e8 31 c5 ff ff       	callq  402210 <strerror@plt>
  405cdf:	48 89 c1             	mov    %rax,%rcx
  405ce2:	48 8b 85 08 da ff ff 	mov    -0x25f8(%rbp),%rax
  405ce9:	8b 40 0c             	mov    0xc(%rax),%eax
  405cec:	89 c2                	mov    %eax,%edx
  405cee:	48 8d 85 10 da ff ff 	lea    -0x25f0(%rbp),%rax
  405cf5:	49 89 c8             	mov    %rcx,%r8
  405cf8:	48 89 d1             	mov    %rdx,%rcx
  405cfb:	ba 28 8e 40 00       	mov    $0x408e28,%edx
  405d00:	be 00 01 00 00       	mov    $0x100,%esi
  405d05:	48 89 c7             	mov    %rax,%rdi
  405d08:	b8 00 00 00 00       	mov    $0x0,%eax
  405d0d:	e8 de c0 ff ff       	callq  401df0 <snprintf@plt>
  405d12:	e8 29 c0 ff ff       	callq  401d40 <getpid@plt>
  405d17:	89 c6                	mov    %eax,%esi
  405d19:	48 8d 85 10 da ff ff 	lea    -0x25f0(%rbp),%rax
  405d20:	48 89 c1             	mov    %rax,%rcx
  405d23:	ba a3 8d 40 00       	mov    $0x408da3,%edx
  405d28:	bf a9 8d 40 00       	mov    $0x408da9,%edi
  405d2d:	e8 a7 17 00 00       	callq  4074d9 <log_it>
  405d32:	bf 01 00 00 00       	mov    $0x1,%edi
  405d37:	e8 84 c4 ff ff       	callq  4021c0 <exit@plt>
  405d3c:	48 8b 85 08 da ff ff 	mov    -0x25f8(%rbp),%rax
  405d43:	8b 40 08             	mov    0x8(%rax),%eax
  405d46:	89 c7                	mov    %eax,%edi
  405d48:	e8 a3 c4 ff ff       	callq  4021f0 <setuid@plt>
  405d4d:	85 c0                	test   %eax,%eax
  405d4f:	74 6b                	je     405dbc <child_process+0x4f4>
  405d51:	e8 2a bf ff ff       	callq  401c80 <__errno_location@plt>
  405d56:	8b 00                	mov    (%rax),%eax
  405d58:	89 c7                	mov    %eax,%edi
  405d5a:	e8 b1 c4 ff ff       	callq  402210 <strerror@plt>
  405d5f:	48 89 c1             	mov    %rax,%rcx
  405d62:	48 8b 85 08 da ff ff 	mov    -0x25f8(%rbp),%rax
  405d69:	8b 40 08             	mov    0x8(%rax),%eax
  405d6c:	89 c2                	mov    %eax,%edx
  405d6e:	48 8d 85 10 da ff ff 	lea    -0x25f0(%rbp),%rax
  405d75:	49 89 c8             	mov    %rcx,%r8
  405d78:	48 89 d1             	mov    %rdx,%rcx
  405d7b:	ba 50 8e 40 00       	mov    $0x408e50,%edx
  405d80:	be 00 01 00 00       	mov    $0x100,%esi
  405d85:	48 89 c7             	mov    %rax,%rdi
  405d88:	b8 00 00 00 00       	mov    $0x0,%eax
  405d8d:	e8 5e c0 ff ff       	callq  401df0 <snprintf@plt>
  405d92:	e8 a9 bf ff ff       	callq  401d40 <getpid@plt>
  405d97:	89 c6                	mov    %eax,%esi
  405d99:	48 8d 85 10 da ff ff 	lea    -0x25f0(%rbp),%rax
  405da0:	48 89 c1             	mov    %rax,%rcx
  405da3:	ba a3 8d 40 00       	mov    $0x408da3,%edx
  405da8:	bf a9 8d 40 00       	mov    $0x408da9,%edi
  405dad:	e8 27 17 00 00       	callq  4074d9 <log_it>
  405db2:	bf 01 00 00 00       	mov    $0x1,%edi
  405db7:	e8 04 c4 ff ff       	callq  4021c0 <exit@plt>
  405dbc:	48 8b 85 08 da ff ff 	mov    -0x25f8(%rbp),%rax
  405dc3:	48 8b 40 10          	mov    0x10(%rax),%rax
  405dc7:	48 89 c6             	mov    %rax,%rsi
  405dca:	bf 72 8e 40 00       	mov    $0x408e72,%edi
  405dcf:	e8 9e 21 00 00       	callq  407f72 <env_get>
  405dd4:	48 89 c7             	mov    %rax,%rdi
  405dd7:	e8 b4 bf ff ff       	callq  401d90 <chdir@plt>
  405ddc:	48 8b 85 08 da ff ff 	mov    -0x25f8(%rbp),%rax
  405de3:	48 8b 40 10          	mov    0x10(%rax),%rax
  405de7:	48 89 c7             	mov    %rax,%rdi
  405dea:	e8 5c fa ff ff       	callq  40584b <build_env>
  405def:	48 89 85 10 ff ff ff 	mov    %rax,-0xf0(%rbp)
  405df6:	48 8b 85 10 ff ff ff 	mov    -0xf0(%rbp),%rax
  405dfd:	48 89 c6             	mov    %rax,%rsi
  405e00:	bf 77 8e 40 00       	mov    $0x408e77,%edi
  405e05:	e8 68 21 00 00       	callq  407f72 <env_get>
  405e0a:	48 89 85 08 ff ff ff 	mov    %rax,-0xf8(%rbp)
  405e11:	48 8b 85 08 da ff ff 	mov    -0x25f8(%rbp),%rax
  405e18:	48 8b 50 18          	mov    0x18(%rax),%rdx
  405e1c:	48 8b 8d 10 ff ff ff 	mov    -0xf0(%rbp),%rcx
  405e23:	48 8b b5 08 ff ff ff 	mov    -0xf8(%rbp),%rsi
  405e2a:	48 8b 85 08 ff ff ff 	mov    -0xf8(%rbp),%rax
  405e31:	49 89 c9             	mov    %rcx,%r9
  405e34:	41 b8 00 00 00 00    	mov    $0x0,%r8d
  405e3a:	48 89 d1             	mov    %rdx,%rcx
  405e3d:	ba 7d 8e 40 00       	mov    $0x408e7d,%edx
  405e42:	48 89 c7             	mov    %rax,%rdi
  405e45:	b8 00 00 00 00       	mov    $0x0,%eax
  405e4a:	e8 81 c3 ff ff       	callq  4021d0 <execle@plt>
  405e4f:	e8 2c be ff ff       	callq  401c80 <__errno_location@plt>
  405e54:	8b 00                	mov    (%rax),%eax
  405e56:	89 c7                	mov    %eax,%edi
  405e58:	e8 b3 c3 ff ff       	callq  402210 <strerror@plt>
  405e5d:	48 89 c1             	mov    %rax,%rcx
  405e60:	48 8b 05 79 59 20 00 	mov    0x205979(%rip),%rax        # 60b7e0 <stderr@@GLIBC_2.2.5>
  405e67:	48 8b 95 08 ff ff ff 	mov    -0xf8(%rbp),%rdx
  405e6e:	be 80 8e 40 00       	mov    $0x408e80,%esi
  405e73:	48 89 c7             	mov    %rax,%rdi
  405e76:	b8 00 00 00 00       	mov    $0x0,%eax
  405e7b:	e8 b0 c0 ff ff       	callq  401f30 <fprintf@plt>
  405e80:	bf 01 00 00 00       	mov    $0x1,%edi
  405e85:	e8 26 be ff ff       	callq  401cb0 <_exit@plt>
  405e8a:	8b 05 38 56 20 00    	mov    0x205638(%rip),%eax        # 60b4c8 <log_level>
  405e90:	83 e0 01             	and    $0x1,%eax
  405e93:	85 c0                	test   %eax,%eax
  405e95:	0f 84 9f 00 00 00    	je     405f3a <child_process+0x672>
  405e9b:	8b 05 27 56 20 00    	mov    0x205627(%rip),%eax        # 60b4c8 <log_level>
  405ea1:	83 e0 08             	and    $0x8,%eax
  405ea4:	85 c0                	test   %eax,%eax
  405ea6:	0f 84 8e 00 00 00    	je     405f3a <child_process+0x672>
  405eac:	48 8b 85 08 da ff ff 	mov    -0x25f8(%rbp),%rax
  405eb3:	48 8b 48 18          	mov    0x18(%rax),%rcx
  405eb7:	8b 95 74 ff ff ff    	mov    -0x8c(%rbp),%edx
  405ebd:	48 8d 85 10 da ff ff 	lea    -0x25f0(%rbp),%rax
  405ec4:	49 89 c8             	mov    %rcx,%r8
  405ec7:	89 d1                	mov    %edx,%ecx
  405ec9:	ba 90 8e 40 00       	mov    $0x408e90,%edx
  405ece:	be f0 03 00 00       	mov    $0x3f0,%esi
  405ed3:	48 89 c7             	mov    %rax,%rdi
  405ed6:	b8 00 00 00 00       	mov    $0x0,%eax
  405edb:	e8 10 bf ff ff       	callq  401df0 <snprintf@plt>
  405ee0:	48 8d 85 10 da ff ff 	lea    -0x25f0(%rbp),%rax
  405ee7:	48 89 c7             	mov    %rax,%rdi
  405eea:	e8 81 be ff ff       	callq  401d70 <strlen@plt>
  405eef:	89 c2                	mov    %eax,%edx
  405ef1:	48 8d 85 10 da ff ff 	lea    -0x25f0(%rbp),%rax
  405ef8:	89 d6                	mov    %edx,%esi
  405efa:	48 89 c7             	mov    %rax,%rdi
  405efd:	e8 a1 17 00 00       	callq  4076a3 <mkprints>
  405f02:	48 89 85 68 ff ff ff 	mov    %rax,-0x98(%rbp)
  405f09:	e8 32 be ff ff       	callq  401d40 <getpid@plt>
  405f0e:	89 c6                	mov    %eax,%esi
  405f10:	48 8b 95 68 ff ff ff 	mov    -0x98(%rbp),%rdx
  405f17:	48 8b 45 90          	mov    -0x70(%rbp),%rax
  405f1b:	48 89 d1             	mov    %rdx,%rcx
  405f1e:	ba f9 8d 40 00       	mov    $0x408df9,%edx
  405f23:	48 89 c7             	mov    %rax,%rdi
  405f26:	e8 ae 15 00 00       	callq  4074d9 <log_it>
  405f2b:	48 8b 85 68 ff ff ff 	mov    -0x98(%rbp),%rax
  405f32:	48 89 c7             	mov    %rax,%rdi
  405f35:	e8 f6 bc ff ff       	callq  401c30 <free@plt>
  405f3a:	90                   	nop
  405f3b:	83 45 c4 01          	addl   $0x1,-0x3c(%rbp)
  405f3f:	8b 85 90 fe ff ff    	mov    -0x170(%rbp),%eax
  405f45:	89 c7                	mov    %eax,%edi
  405f47:	e8 f4 be ff ff       	callq  401e40 <close@plt>
  405f4c:	41 0f b6 04 24       	movzbl (%r12),%eax
  405f51:	84 c0                	test   %al,%al
  405f53:	0f 84 bd 00 00 00    	je     406016 <child_process+0x74e>
  405f59:	e8 12 c3 ff ff       	callq  402270 <fork@plt>
  405f5e:	85 c0                	test   %eax,%eax
  405f60:	0f 85 b0 00 00 00    	jne    406016 <child_process+0x74e>
  405f66:	8b 85 94 fe ff ff    	mov    -0x16c(%rbp),%eax
  405f6c:	be 98 8e 40 00       	mov    $0x408e98,%esi
  405f71:	89 c7                	mov    %eax,%edi
  405f73:	e8 18 c1 ff ff       	callq  402090 <fdopen@plt>
  405f78:	49 89 c5             	mov    %rax,%r13
  405f7b:	41 bf 00 00 00 00    	mov    $0x0,%r15d
  405f81:	41 be 00 00 00 00    	mov    $0x0,%r14d
  405f87:	eb 46                	jmp    405fcf <child_process+0x707>
  405f89:	45 85 f6             	test   %r14d,%r14d
  405f8c:	74 14                	je     405fa2 <child_process+0x6da>
  405f8e:	83 fb 25             	cmp    $0x25,%ebx
  405f91:	74 19                	je     405fac <child_process+0x6e4>
  405f93:	4c 89 ee             	mov    %r13,%rsi
  405f96:	bf 5c 00 00 00       	mov    $0x5c,%edi
  405f9b:	e8 40 be ff ff       	callq  401de0 <_IO_putc@plt>
  405fa0:	eb 0a                	jmp    405fac <child_process+0x6e4>
  405fa2:	83 fb 25             	cmp    $0x25,%ebx
  405fa5:	75 05                	jne    405fac <child_process+0x6e4>
  405fa7:	bb 0a 00 00 00       	mov    $0xa,%ebx
  405fac:	83 fb 5c             	cmp    $0x5c,%ebx
  405faf:	0f 94 c0             	sete   %al
  405fb2:	44 0f b6 f0          	movzbl %al,%r14d
  405fb6:	45 85 f6             	test   %r14d,%r14d
  405fb9:	75 14                	jne    405fcf <child_process+0x707>
  405fbb:	4c 89 ee             	mov    %r13,%rsi
  405fbe:	89 df                	mov    %ebx,%edi
  405fc0:	e8 1b be ff ff       	callq  401de0 <_IO_putc@plt>
  405fc5:	83 fb 0a             	cmp    $0xa,%ebx
  405fc8:	0f 95 c0             	setne  %al
  405fcb:	44 0f b6 f8          	movzbl %al,%r15d
  405fcf:	4c 89 e0             	mov    %r12,%rax
  405fd2:	4c 8d 60 01          	lea    0x1(%rax),%r12
  405fd6:	0f b6 00             	movzbl (%rax),%eax
  405fd9:	0f be d8             	movsbl %al,%ebx
  405fdc:	85 db                	test   %ebx,%ebx
  405fde:	75 a9                	jne    405f89 <child_process+0x6c1>
  405fe0:	45 85 f6             	test   %r14d,%r14d
  405fe3:	74 0d                	je     405ff2 <child_process+0x72a>
  405fe5:	4c 89 ee             	mov    %r13,%rsi
  405fe8:	bf 5c 00 00 00       	mov    $0x5c,%edi
  405fed:	e8 ee bd ff ff       	callq  401de0 <_IO_putc@plt>
  405ff2:	45 85 ff             	test   %r15d,%r15d
  405ff5:	74 0d                	je     406004 <child_process+0x73c>
  405ff7:	4c 89 ee             	mov    %r13,%rsi
  405ffa:	bf 0a 00 00 00       	mov    $0xa,%edi
  405fff:	e8 dc bd ff ff       	callq  401de0 <_IO_putc@plt>
  406004:	4c 89 ef             	mov    %r13,%rdi
  406007:	e8 44 bd ff ff       	callq  401d50 <fclose@plt>
  40600c:	bf 00 00 00 00       	mov    $0x0,%edi
  406011:	e8 aa c1 ff ff       	callq  4021c0 <exit@plt>
  406016:	8b 85 94 fe ff ff    	mov    -0x16c(%rbp),%eax
  40601c:	89 c7                	mov    %eax,%edi
  40601e:	e8 1d be ff ff       	callq  401e40 <close@plt>
  406023:	83 45 c4 01          	addl   $0x1,-0x3c(%rbp)
  406027:	c7 45 c0 00 00 00 00 	movl   $0x0,-0x40(%rbp)
  40602e:	c7 45 bc 00 00 00 00 	movl   $0x0,-0x44(%rbp)
  406035:	e9 b7 01 00 00       	jmpq   4061f1 <child_process+0x929>
  40603a:	48 8d 85 cc e9 ff ff 	lea    -0x1634(%rbp),%rax
  406041:	48 89 c7             	mov    %rax,%rdi
  406044:	e8 f7 c1 ff ff       	callq  402240 <wait@plt>
  406049:	89 85 64 ff ff ff    	mov    %eax,-0x9c(%rbp)
  40604f:	83 bd 64 ff ff ff 00 	cmpl   $0x0,-0x9c(%rbp)
  406056:	0f 88 a1 01 00 00    	js     4061fd <child_process+0x935>
  40605c:	8b 85 cc e9 ff ff    	mov    -0x1634(%rbp),%eax
  406062:	89 85 a0 fe ff ff    	mov    %eax,-0x160(%rbp)
  406068:	8b 85 a0 fe ff ff    	mov    -0x160(%rbp),%eax
  40606e:	25 00 ff 00 00       	and    $0xff00,%eax
  406073:	c1 f8 08             	sar    $0x8,%eax
  406076:	89 45 bc             	mov    %eax,-0x44(%rbp)
  406079:	8b 05 49 54 20 00    	mov    0x205449(%rip),%eax        # 60b4c8 <log_level>
  40607f:	83 e0 04             	and    $0x4,%eax
  406082:	85 c0                	test   %eax,%eax
  406084:	0f 84 63 01 00 00    	je     4061ed <child_process+0x925>
  40608a:	8b 85 cc e9 ff ff    	mov    -0x1634(%rbp),%eax
  406090:	89 85 b0 fe ff ff    	mov    %eax,-0x150(%rbp)
  406096:	8b 85 b0 fe ff ff    	mov    -0x150(%rbp),%eax
  40609c:	83 e0 7f             	and    $0x7f,%eax
  40609f:	85 c0                	test   %eax,%eax
  4060a1:	0f 85 91 00 00 00    	jne    406138 <child_process+0x870>
  4060a7:	8b 85 cc e9 ff ff    	mov    -0x1634(%rbp),%eax
  4060ad:	89 85 c0 fe ff ff    	mov    %eax,-0x140(%rbp)
  4060b3:	8b 85 c0 fe ff ff    	mov    -0x140(%rbp),%eax
  4060b9:	25 00 ff 00 00       	and    $0xff00,%eax
  4060be:	c1 f8 08             	sar    $0x8,%eax
  4060c1:	85 c0                	test   %eax,%eax
  4060c3:	74 73                	je     406138 <child_process+0x870>
  4060c5:	8b 85 cc e9 ff ff    	mov    -0x1634(%rbp),%eax
  4060cb:	89 45 c0             	mov    %eax,-0x40(%rbp)
  4060ce:	8b 85 cc e9 ff ff    	mov    -0x1634(%rbp),%eax
  4060d4:	89 85 d0 fe ff ff    	mov    %eax,-0x130(%rbp)
  4060da:	8b 85 d0 fe ff ff    	mov    -0x130(%rbp),%eax
  4060e0:	25 00 ff 00 00       	and    $0xff00,%eax
  4060e5:	c1 f8 08             	sar    $0x8,%eax
  4060e8:	89 c1                	mov    %eax,%ecx
  4060ea:	8b 95 64 ff ff ff    	mov    -0x9c(%rbp),%edx
  4060f0:	48 8d 85 10 da ff ff 	lea    -0x25f0(%rbp),%rax
  4060f7:	41 89 c8             	mov    %ecx,%r8d
  4060fa:	89 d1                	mov    %edx,%ecx
  4060fc:	ba a0 8e 40 00       	mov    $0x408ea0,%edx
  406101:	be 00 01 00 00       	mov    $0x100,%esi
  406106:	48 89 c7             	mov    %rax,%rdi
  406109:	b8 00 00 00 00       	mov    $0x0,%eax
  40610e:	e8 dd bc ff ff       	callq  401df0 <snprintf@plt>
  406113:	e8 28 bc ff ff       	callq  401d40 <getpid@plt>
  406118:	89 c6                	mov    %eax,%esi
  40611a:	48 8d 85 10 da ff ff 	lea    -0x25f0(%rbp),%rax
  406121:	48 89 c1             	mov    %rax,%rcx
  406124:	ba a3 8d 40 00       	mov    $0x408da3,%edx
  406129:	bf a9 8d 40 00       	mov    $0x408da9,%edi
  40612e:	e8 a6 13 00 00       	callq  4074d9 <log_it>
  406133:	e9 b5 00 00 00       	jmpq   4061ed <child_process+0x925>
  406138:	8b 85 cc e9 ff ff    	mov    -0x1634(%rbp),%eax
  40613e:	89 85 e0 fe ff ff    	mov    %eax,-0x120(%rbp)
  406144:	8b 85 e0 fe ff ff    	mov    -0x120(%rbp),%eax
  40614a:	83 e0 7f             	and    $0x7f,%eax
  40614d:	83 c0 01             	add    $0x1,%eax
  406150:	d0 f8                	sar    %al
  406152:	84 c0                	test   %al,%al
  406154:	0f 8e 93 00 00 00    	jle    4061ed <child_process+0x925>
  40615a:	8b 85 cc e9 ff ff    	mov    -0x1634(%rbp),%eax
  406160:	89 45 c0             	mov    %eax,-0x40(%rbp)
  406163:	8b 85 cc e9 ff ff    	mov    -0x1634(%rbp),%eax
  406169:	89 85 f0 fe ff ff    	mov    %eax,-0x110(%rbp)
  40616f:	8b 85 f0 fe ff ff    	mov    -0x110(%rbp),%eax
  406175:	25 80 00 00 00       	and    $0x80,%eax
  40617a:	85 c0                	test   %eax,%eax
  40617c:	74 07                	je     406185 <child_process+0x8bd>
  40617e:	be ca 8e 40 00       	mov    $0x408eca,%esi
  406183:	eb 05                	jmp    40618a <child_process+0x8c2>
  406185:	be d8 8e 40 00       	mov    $0x408ed8,%esi
  40618a:	8b 85 cc e9 ff ff    	mov    -0x1634(%rbp),%eax
  406190:	89 85 00 ff ff ff    	mov    %eax,-0x100(%rbp)
  406196:	8b 85 00 ff ff ff    	mov    -0x100(%rbp),%eax
  40619c:	83 e0 7f             	and    $0x7f,%eax
  40619f:	89 c1                	mov    %eax,%ecx
  4061a1:	8b 95 64 ff ff ff    	mov    -0x9c(%rbp),%edx
  4061a7:	48 8d 85 10 da ff ff 	lea    -0x25f0(%rbp),%rax
  4061ae:	49 89 f1             	mov    %rsi,%r9
  4061b1:	41 89 c8             	mov    %ecx,%r8d
  4061b4:	89 d1                	mov    %edx,%ecx
  4061b6:	ba e0 8e 40 00       	mov    $0x408ee0,%edx
  4061bb:	be 00 01 00 00       	mov    $0x100,%esi
  4061c0:	48 89 c7             	mov    %rax,%rdi
  4061c3:	b8 00 00 00 00       	mov    $0x0,%eax
  4061c8:	e8 23 bc ff ff       	callq  401df0 <snprintf@plt>
  4061cd:	e8 6e bb ff ff       	callq  401d40 <getpid@plt>
  4061d2:	89 c6                	mov    %eax,%esi
  4061d4:	48 8d 85 10 da ff ff 	lea    -0x25f0(%rbp),%rax
  4061db:	48 89 c1             	mov    %rax,%rcx
  4061de:	ba a3 8d 40 00       	mov    $0x408da3,%edx
  4061e3:	bf a9 8d 40 00       	mov    $0x408da9,%edi
  4061e8:	e8 ec 12 00 00       	callq  4074d9 <log_it>
  4061ed:	83 6d c4 01          	subl   $0x1,-0x3c(%rbp)
  4061f1:	83 7d c4 00          	cmpl   $0x0,-0x3c(%rbp)
  4061f5:	0f 8f 3f fe ff ff    	jg     40603a <child_process+0x772>
  4061fb:	eb 01                	jmp    4061fe <child_process+0x936>
  4061fd:	90                   	nop
  4061fe:	48 8b 85 78 ff ff ff 	mov    -0x88(%rbp),%rax
  406205:	ba 02 00 00 00       	mov    $0x2,%edx
  40620a:	be 00 00 00 00       	mov    $0x0,%esi
  40620f:	48 89 c7             	mov    %rax,%rdi
  406212:	e8 49 be ff ff       	callq  402060 <fseek@plt>
  406217:	48 8b 85 78 ff ff ff 	mov    -0x88(%rbp),%rax
  40621e:	48 89 c7             	mov    %rax,%rdi
  406221:	e8 2a bd ff ff       	callq  401f50 <ftell@plt>
  406226:	48 89 85 58 ff ff ff 	mov    %rax,-0xa8(%rbp)
  40622d:	48 8b 85 78 ff ff ff 	mov    -0x88(%rbp),%rax
  406234:	ba 00 00 00 00       	mov    $0x0,%edx
  406239:	be 00 00 00 00       	mov    $0x0,%esi
  40623e:	48 89 c7             	mov    %rax,%rdi
  406241:	e8 1a be ff ff       	callq  402060 <fseek@plt>
  406246:	48 83 bd 58 ff ff ff 	cmpq   $0x0,-0xa8(%rbp)
  40624d:	00 
  40624e:	0f 84 ee 06 00 00    	je     406942 <child_process+0x107a>
  406254:	48 83 7d c8 00       	cmpq   $0x0,-0x38(%rbp)
  406259:	75 0a                	jne    406265 <child_process+0x99d>
  40625b:	48 8b 45 90          	mov    -0x70(%rbp),%rax
  40625f:	48 89 45 c8          	mov    %rax,-0x38(%rbp)
  406263:	eb 0f                	jmp    406274 <child_process+0x9ac>
  406265:	48 8b 45 c8          	mov    -0x38(%rbp),%rax
  406269:	0f b6 00             	movzbl (%rax),%eax
  40626c:	84 c0                	test   %al,%al
  40626e:	0f 84 d1 06 00 00    	je     406945 <child_process+0x107d>
  406274:	48 8d 85 00 fe ff ff 	lea    -0x200(%rbp),%rax
  40627b:	48 89 c6             	mov    %rax,%rsi
  40627e:	bf 09 8f 40 00       	mov    $0x408f09,%edi
  406283:	e8 78 25 00 00       	callq  408800 <__stat>
  406288:	89 85 54 ff ff ff    	mov    %eax,-0xac(%rbp)
  40628e:	83 bd 54 ff ff ff 00 	cmpl   $0x0,-0xac(%rbp)
  406295:	74 2e                	je     4062c5 <child_process+0x9fd>
  406297:	48 83 bd 58 ff ff ff 	cmpq   $0x0,-0xa8(%rbp)
  40629e:	00 
  40629f:	0f 8e a3 06 00 00    	jle    406948 <child_process+0x1080>
  4062a5:	e8 96 ba ff ff       	callq  401d40 <getpid@plt>
  4062aa:	b9 20 8f 40 00       	mov    $0x408f20,%ecx
  4062af:	ba 44 8f 40 00       	mov    $0x408f44,%edx
  4062b4:	89 c6                	mov    %eax,%esi
  4062b6:	bf a9 8d 40 00       	mov    $0x408da9,%edi
  4062bb:	e8 19 12 00 00       	callq  4074d9 <log_it>
  4062c0:	e9 83 06 00 00       	jmpq   406948 <child_process+0x1080>
  4062c5:	bb 00 00 00 00       	mov    $0x0,%ebx
  4062ca:	41 bd 01 00 00 00    	mov    $0x1,%r13d
  4062d0:	48 8b 85 08 da ff ff 	mov    -0x25f8(%rbp),%rax
  4062d7:	48 8b 40 10          	mov    0x10(%rax),%rax
  4062db:	48 89 c7             	mov    %rax,%rdi
  4062de:	e8 68 f5 ff ff       	callq  40584b <build_env>
  4062e3:	48 89 85 48 ff ff ff 	mov    %rax,-0xb8(%rbp)
  4062ea:	48 8b 85 48 ff ff ff 	mov    -0xb8(%rbp),%rax
  4062f1:	48 89 c6             	mov    %rax,%rsi
  4062f4:	bf 49 8f 40 00       	mov    $0x408f49,%edi
  4062f9:	e8 74 1c 00 00       	callq  407f72 <env_get>
  4062fe:	48 89 85 40 ff ff ff 	mov    %rax,-0xc0(%rbp)
  406305:	48 8b 85 48 ff ff ff 	mov    -0xb8(%rbp),%rax
  40630c:	48 89 c6             	mov    %rax,%rsi
  40630f:	bf 56 8f 40 00       	mov    $0x408f56,%edi
  406314:	e8 59 1c 00 00       	callq  407f72 <env_get>
  406319:	48 89 85 38 ff ff ff 	mov    %rax,-0xc8(%rbp)
  406320:	48 8d 85 d0 f9 ff ff 	lea    -0x630(%rbp),%rax
  406327:	be 40 00 00 00       	mov    $0x40,%esi
  40632c:	48 89 c7             	mov    %rax,%rdi
  40632f:	e8 6c be ff ff       	callq  4021a0 <gethostname@plt>
  406334:	48 8b 55 c8          	mov    -0x38(%rbp),%rdx
  406338:	48 8d 85 10 fa ff ff 	lea    -0x5f0(%rbp),%rax
  40633f:	49 89 d0             	mov    %rdx,%r8
  406342:	b9 09 8f 40 00       	mov    $0x408f09,%ecx
  406347:	ba 70 8f 40 00       	mov    $0x408f70,%edx
  40634c:	be e8 03 00 00       	mov    $0x3e8,%esi
  406351:	48 89 c7             	mov    %rax,%rdi
  406354:	b8 00 00 00 00       	mov    $0x0,%eax
  406359:	e8 92 ba ff ff       	callq  401df0 <snprintf@plt>
  40635e:	48 8b 95 08 da ff ff 	mov    -0x25f8(%rbp),%rdx
  406365:	48 8d 85 10 fa ff ff 	lea    -0x5f0(%rbp),%rax
  40636c:	be 98 8e 40 00       	mov    $0x408e98,%esi
  406371:	48 89 c7             	mov    %rax,%rdi
  406374:	e8 84 1c 00 00       	callq  407ffd <cron_popen>
  406379:	48 89 c3             	mov    %rax,%rbx
  40637c:	48 85 db             	test   %rbx,%rbx
  40637f:	75 14                	jne    406395 <child_process+0xacd>
  406381:	bf 09 8f 40 00       	mov    $0x408f09,%edi
  406386:	e8 65 bd ff ff       	callq  4020f0 <perror@plt>
  40638b:	bf 01 00 00 00       	mov    $0x1,%edi
  406390:	e8 1b b9 ff ff       	callq  401cb0 <_exit@plt>
  406395:	48 89 d9             	mov    %rbx,%rcx
  406398:	ba 19 00 00 00       	mov    $0x19,%edx
  40639d:	be 01 00 00 00       	mov    $0x1,%esi
  4063a2:	bf 8c 8f 40 00       	mov    $0x408f8c,%edi
  4063a7:	e8 34 be ff ff       	callq  4021e0 <fwrite@plt>
  4063ac:	48 8b 45 c8          	mov    -0x38(%rbp),%rax
  4063b0:	48 89 c2             	mov    %rax,%rdx
  4063b3:	be a6 8f 40 00       	mov    $0x408fa6,%esi
  4063b8:	48 89 df             	mov    %rbx,%rdi
  4063bb:	b8 00 00 00 00       	mov    $0x0,%eax
  4063c0:	e8 6b bb ff ff       	callq  401f30 <fprintf@plt>
  4063c5:	48 83 7d 88 00       	cmpq   $0x0,-0x78(%rbp)
  4063ca:	75 5d                	jne    406429 <child_process+0xb61>
  4063cc:	83 7d c0 00          	cmpl   $0x0,-0x40(%rbp)
  4063d0:	74 08                	je     4063da <child_process+0xb12>
  4063d2:	41 be ae 8f 40 00    	mov    $0x408fae,%r14d
  4063d8:	eb 06                	jmp    4063e0 <child_process+0xb18>
  4063da:	41 be d8 8e 40 00    	mov    $0x408ed8,%r14d
  4063e0:	48 8b 85 08 da ff ff 	mov    -0x25f8(%rbp),%rax
  4063e7:	4c 8b 60 18          	mov    0x18(%rax),%r12
  4063eb:	48 8d 85 d0 f9 ff ff 	lea    -0x630(%rbp),%rax
  4063f2:	be b8 8f 40 00       	mov    $0x408fb8,%esi
  4063f7:	48 89 c7             	mov    %rax,%rdi
  4063fa:	e8 47 11 00 00       	callq  407546 <first_word>
  4063ff:	48 89 c2             	mov    %rax,%rdx
  406402:	48 8b 45 90          	mov    -0x70(%rbp),%rax
  406406:	4d 89 f1             	mov    %r14,%r9
  406409:	4d 89 e0             	mov    %r12,%r8
  40640c:	48 89 d1             	mov    %rdx,%rcx
  40640f:	48 89 c2             	mov    %rax,%rdx
  406412:	be ba 8f 40 00       	mov    $0x408fba,%esi
  406417:	48 89 df             	mov    %rbx,%rdi
  40641a:	b8 00 00 00 00       	mov    $0x0,%eax
  40641f:	e8 0c bb ff ff       	callq  401f30 <fprintf@plt>
  406424:	e9 7d 02 00 00       	jmpq   4066a6 <child_process+0xdde>
  406429:	48 8d 85 10 da ff ff 	lea    -0x25f0(%rbp),%rax
  406430:	ba bc 0f 00 00       	mov    $0xfbc,%edx
  406435:	be 00 00 00 00       	mov    $0x0,%esi
  40643a:	48 89 c7             	mov    %rax,%rdi
  40643d:	e8 ce b9 ff ff       	callq  401e10 <memset@plt>
  406442:	48 8b 45 88          	mov    -0x78(%rbp),%rax
  406446:	be bd 0f 00 00       	mov    $0xfbd,%esi
  40644b:	48 89 c7             	mov    %rax,%rdi
  40644e:	e8 dd b9 ff ff       	callq  401e30 <strnlen@plt>
  406453:	48 3d bc 0f 00 00    	cmp    $0xfbc,%rax
  406459:	76 5d                	jbe    4064b8 <child_process+0xbf0>
  40645b:	83 7d c0 00          	cmpl   $0x0,-0x40(%rbp)
  40645f:	74 08                	je     406469 <child_process+0xba1>
  406461:	41 be ae 8f 40 00    	mov    $0x408fae,%r14d
  406467:	eb 06                	jmp    40646f <child_process+0xba7>
  406469:	41 be d8 8e 40 00    	mov    $0x408ed8,%r14d
  40646f:	48 8b 85 08 da ff ff 	mov    -0x25f8(%rbp),%rax
  406476:	4c 8b 60 18          	mov    0x18(%rax),%r12
  40647a:	48 8d 85 d0 f9 ff ff 	lea    -0x630(%rbp),%rax
  406481:	be b8 8f 40 00       	mov    $0x408fb8,%esi
  406486:	48 89 c7             	mov    %rax,%rdi
  406489:	e8 b8 10 00 00       	callq  407546 <first_word>
  40648e:	48 89 c2             	mov    %rax,%rdx
  406491:	48 8b 45 90          	mov    -0x70(%rbp),%rax
  406495:	4d 89 f1             	mov    %r14,%r9
  406498:	4d 89 e0             	mov    %r12,%r8
  40649b:	48 89 d1             	mov    %rdx,%rcx
  40649e:	48 89 c2             	mov    %rax,%rdx
  4064a1:	be ba 8f 40 00       	mov    $0x408fba,%esi
  4064a6:	48 89 df             	mov    %rbx,%rdi
  4064a9:	b8 00 00 00 00       	mov    $0x0,%eax
  4064ae:	e8 7d ba ff ff       	callq  401f30 <fprintf@plt>
  4064b3:	e9 ee 01 00 00       	jmpq   4066a6 <child_process+0xdde>
  4064b8:	48 8b 45 88          	mov    -0x78(%rbp),%rax
  4064bc:	48 89 c7             	mov    %rax,%rdi
  4064bf:	e8 ac b8 ff ff       	callq  401d70 <strlen@plt>
  4064c4:	48 89 c2             	mov    %rax,%rdx
  4064c7:	48 8b 4d 88          	mov    -0x78(%rbp),%rcx
  4064cb:	48 8d 85 10 da ff ff 	lea    -0x25f0(%rbp),%rax
  4064d2:	48 89 ce             	mov    %rcx,%rsi
  4064d5:	48 89 c7             	mov    %rax,%rdi
  4064d8:	e8 b3 b7 ff ff       	callq  401c90 <strncpy@plt>
  4064dd:	48 8d 85 10 da ff ff 	lea    -0x25f0(%rbp),%rax
  4064e4:	be d6 8f 40 00       	mov    $0x408fd6,%esi
  4064e9:	48 89 c7             	mov    %rax,%rdi
  4064ec:	e8 8f bd ff ff       	callq  402280 <strstr@plt>
  4064f1:	48 85 c0             	test   %rax,%rax
  4064f4:	74 25                	je     40651b <child_process+0xc53>
  4064f6:	48 8b 55 90          	mov    -0x70(%rbp),%rdx
  4064fa:	48 8d 85 10 da ff ff 	lea    -0x25f0(%rbp),%rax
  406501:	48 89 d1             	mov    %rdx,%rcx
  406504:	ba d6 8f 40 00       	mov    $0x408fd6,%edx
  406509:	be bc 0f 00 00       	mov    $0xfbc,%esi
  40650e:	48 89 c7             	mov    %rax,%rdi
  406511:	b8 00 00 00 00       	mov    $0x0,%eax
  406516:	e8 a7 20 00 00       	callq  4085c2 <replace_str>
  40651b:	48 8d 85 10 da ff ff 	lea    -0x25f0(%rbp),%rax
  406522:	be dd 8f 40 00       	mov    $0x408fdd,%esi
  406527:	48 89 c7             	mov    %rax,%rdi
  40652a:	e8 51 bd ff ff       	callq  402280 <strstr@plt>
  40652f:	48 85 c0             	test   %rax,%rax
  406532:	74 38                	je     40656c <child_process+0xca4>
  406534:	48 8d 85 d0 f9 ff ff 	lea    -0x630(%rbp),%rax
  40653b:	be b8 8f 40 00       	mov    $0x408fb8,%esi
  406540:	48 89 c7             	mov    %rax,%rdi
  406543:	e8 fe 0f 00 00       	callq  407546 <first_word>
  406548:	48 89 c2             	mov    %rax,%rdx
  40654b:	48 8d 85 10 da ff ff 	lea    -0x25f0(%rbp),%rax
  406552:	48 89 d1             	mov    %rdx,%rcx
  406555:	ba dd 8f 40 00       	mov    $0x408fdd,%edx
  40655a:	be bc 0f 00 00       	mov    $0xfbc,%esi
  40655f:	48 89 c7             	mov    %rax,%rdi
  406562:	b8 00 00 00 00       	mov    $0x0,%eax
  406567:	e8 56 20 00 00       	callq  4085c2 <replace_str>
  40656c:	48 8d 85 10 da ff ff 	lea    -0x25f0(%rbp),%rax
  406573:	be e8 8f 40 00       	mov    $0x408fe8,%esi
  406578:	48 89 c7             	mov    %rax,%rdi
  40657b:	e8 00 bd ff ff       	callq  402280 <strstr@plt>
  406580:	48 85 c0             	test   %rax,%rax
  406583:	74 2c                	je     4065b1 <child_process+0xce9>
  406585:	48 8b 85 08 da ff ff 	mov    -0x25f8(%rbp),%rax
  40658c:	48 8b 50 18          	mov    0x18(%rax),%rdx
  406590:	48 8d 85 10 da ff ff 	lea    -0x25f0(%rbp),%rax
  406597:	48 89 d1             	mov    %rdx,%rcx
  40659a:	ba e8 8f 40 00       	mov    $0x408fe8,%edx
  40659f:	be bc 0f 00 00       	mov    $0xfbc,%esi
  4065a4:	48 89 c7             	mov    %rax,%rdi
  4065a7:	b8 00 00 00 00       	mov    $0x0,%eax
  4065ac:	e8 11 20 00 00       	callq  4085c2 <replace_str>
  4065b1:	48 8d 85 10 da ff ff 	lea    -0x25f0(%rbp),%rax
  4065b8:	be ee 8f 40 00       	mov    $0x408fee,%esi
  4065bd:	48 89 c7             	mov    %rax,%rdi
  4065c0:	e8 bb bc ff ff       	callq  402280 <strstr@plt>
  4065c5:	48 85 c0             	test   %rax,%rax
  4065c8:	74 33                	je     4065fd <child_process+0xd35>
  4065ca:	83 7d bc 00          	cmpl   $0x0,-0x44(%rbp)
  4065ce:	74 07                	je     4065d7 <child_process+0xd0f>
  4065d0:	ba f7 8f 40 00       	mov    $0x408ff7,%edx
  4065d5:	eb 05                	jmp    4065dc <child_process+0xd14>
  4065d7:	ba fe 8f 40 00       	mov    $0x408ffe,%edx
  4065dc:	48 8d 85 10 da ff ff 	lea    -0x25f0(%rbp),%rax
  4065e3:	48 89 d1             	mov    %rdx,%rcx
  4065e6:	ba ee 8f 40 00       	mov    $0x408fee,%edx
  4065eb:	be bc 0f 00 00       	mov    $0xfbc,%esi
  4065f0:	48 89 c7             	mov    %rax,%rdi
  4065f3:	b8 00 00 00 00       	mov    $0x0,%eax
  4065f8:	e8 c5 1f 00 00       	callq  4085c2 <replace_str>
  4065fd:	48 8d 85 10 da ff ff 	lea    -0x25f0(%rbp),%rax
  406604:	be 06 90 40 00       	mov    $0x409006,%esi
  406609:	48 89 c7             	mov    %rax,%rdi
  40660c:	e8 6f bc ff ff       	callq  402280 <strstr@plt>
  406611:	48 85 c0             	test   %rax,%rax
  406614:	74 33                	je     406649 <child_process+0xd81>
  406616:	83 7d c0 00          	cmpl   $0x0,-0x40(%rbp)
  40661a:	74 07                	je     406623 <child_process+0xd5b>
  40661c:	ba f7 8f 40 00       	mov    $0x408ff7,%edx
  406621:	eb 05                	jmp    406628 <child_process+0xd60>
  406623:	ba fe 8f 40 00       	mov    $0x408ffe,%edx
  406628:	48 8d 85 10 da ff ff 	lea    -0x25f0(%rbp),%rax
  40662f:	48 89 d1             	mov    %rdx,%rcx
  406632:	ba 06 90 40 00       	mov    $0x409006,%edx
  406637:	be bc 0f 00 00       	mov    $0xfbc,%esi
  40663c:	48 89 c7             	mov    %rax,%rdi
  40663f:	b8 00 00 00 00       	mov    $0x0,%eax
  406644:	e8 79 1f 00 00       	callq  4085c2 <replace_str>
  406649:	48 8d 85 10 da ff ff 	lea    -0x25f0(%rbp),%rax
  406650:	be 13 90 40 00       	mov    $0x409013,%esi
  406655:	48 89 c7             	mov    %rax,%rdi
  406658:	e8 23 bc ff ff       	callq  402280 <strstr@plt>
  40665d:	48 85 c0             	test   %rax,%rax
  406660:	74 28                	je     40668a <child_process+0xdc2>
  406662:	48 8d 95 d0 f9 ff ff 	lea    -0x630(%rbp),%rdx
  406669:	48 8d 85 10 da ff ff 	lea    -0x25f0(%rbp),%rax
  406670:	48 89 d1             	mov    %rdx,%rcx
  406673:	ba 13 90 40 00       	mov    $0x409013,%edx
  406678:	be bc 0f 00 00       	mov    $0xfbc,%esi
  40667d:	48 89 c7             	mov    %rax,%rdi
  406680:	b8 00 00 00 00       	mov    $0x0,%eax
  406685:	e8 38 1f 00 00       	callq  4085c2 <replace_str>
  40668a:	48 8d 85 10 da ff ff 	lea    -0x25f0(%rbp),%rax
  406691:	48 89 c2             	mov    %rax,%rdx
  406694:	be 1a 90 40 00       	mov    $0x40901a,%esi
  406699:	48 89 df             	mov    %rbx,%rdi
  40669c:	b8 00 00 00 00       	mov    $0x0,%eax
  4066a1:	e8 8a b8 ff ff       	callq  401f30 <fprintf@plt>
  4066a6:	48 83 bd 40 ff ff ff 	cmpq   $0x0,-0xc0(%rbp)
  4066ad:	00 
  4066ae:	75 1c                	jne    4066cc <child_process+0xe04>
  4066b0:	ba 00 b8 60 00       	mov    $0x60b800,%edx
  4066b5:	be 28 90 40 00       	mov    $0x409028,%esi
  4066ba:	48 89 df             	mov    %rbx,%rdi
  4066bd:	b8 00 00 00 00       	mov    $0x0,%eax
  4066c2:	e8 69 b8 ff ff       	callq  401f30 <fprintf@plt>
  4066c7:	e9 84 00 00 00       	jmpq   406750 <child_process+0xe88>
  4066cc:	48 8b 85 40 ff ff ff 	mov    -0xc0(%rbp),%rax
  4066d3:	48 89 45 b0          	mov    %rax,-0x50(%rbp)
  4066d7:	48 8b 85 40 ff ff ff 	mov    -0xc0(%rbp),%rax
  4066de:	48 89 c7             	mov    %rax,%rdi
  4066e1:	e8 8a b6 ff ff       	callq  401d70 <strlen@plt>
  4066e6:	48 89 85 30 ff ff ff 	mov    %rax,-0xd0(%rbp)
  4066ed:	eb 07                	jmp    4066f6 <child_process+0xe2e>
  4066ef:	48 8b 45 b0          	mov    -0x50(%rbp),%rax
  4066f3:	c6 00 20             	movb   $0x20,(%rax)
  4066f6:	48 8b 45 b0          	mov    -0x50(%rbp),%rax
  4066fa:	0f b6 00             	movzbl (%rax),%eax
  4066fd:	84 c0                	test   %al,%al
  4066ff:	74 33                	je     406734 <child_process+0xe6c>
  406701:	48 8b 45 b0          	mov    -0x50(%rbp),%rax
  406705:	be 0a 00 00 00       	mov    $0xa,%esi
  40670a:	48 89 c7             	mov    %rax,%rdi
  40670d:	e8 ae b6 ff ff       	callq  401dc0 <strchr@plt>
  406712:	48 89 45 b0          	mov    %rax,-0x50(%rbp)
  406716:	48 83 7d b0 00       	cmpq   $0x0,-0x50(%rbp)
  40671b:	74 17                	je     406734 <child_process+0xe6c>
  40671d:	48 8b 95 40 ff ff ff 	mov    -0xc0(%rbp),%rdx
  406724:	48 8b 85 30 ff ff ff 	mov    -0xd0(%rbp),%rax
  40672b:	48 01 d0             	add    %rdx,%rax
  40672e:	48 3b 45 b0          	cmp    -0x50(%rbp),%rax
  406732:	77 bb                	ja     4066ef <child_process+0xe27>
  406734:	48 8b 85 40 ff ff ff 	mov    -0xc0(%rbp),%rax
  40673b:	48 89 c2             	mov    %rax,%rdx
  40673e:	be 4e 90 40 00       	mov    $0x40904e,%esi
  406743:	48 89 df             	mov    %rbx,%rdi
  406746:	b8 00 00 00 00       	mov    $0x0,%eax
  40674b:	e8 e0 b7 ff ff       	callq  401f30 <fprintf@plt>
  406750:	48 83 bd 38 ff ff ff 	cmpq   $0x0,-0xc8(%rbp)
  406757:	00 
  406758:	0f 84 84 00 00 00    	je     4067e2 <child_process+0xf1a>
  40675e:	48 8b 85 38 ff ff ff 	mov    -0xc8(%rbp),%rax
  406765:	48 89 45 a8          	mov    %rax,-0x58(%rbp)
  406769:	48 8b 85 38 ff ff ff 	mov    -0xc8(%rbp),%rax
  406770:	48 89 c7             	mov    %rax,%rdi
  406773:	e8 f8 b5 ff ff       	callq  401d70 <strlen@plt>
  406778:	48 89 85 28 ff ff ff 	mov    %rax,-0xd8(%rbp)
  40677f:	eb 07                	jmp    406788 <child_process+0xec0>
  406781:	48 8b 45 a8          	mov    -0x58(%rbp),%rax
  406785:	c6 00 20             	movb   $0x20,(%rax)
  406788:	48 8b 45 a8          	mov    -0x58(%rbp),%rax
  40678c:	0f b6 00             	movzbl (%rax),%eax
  40678f:	84 c0                	test   %al,%al
  406791:	74 33                	je     4067c6 <child_process+0xefe>
  406793:	48 8b 45 a8          	mov    -0x58(%rbp),%rax
  406797:	be 0a 00 00 00       	mov    $0xa,%esi
  40679c:	48 89 c7             	mov    %rax,%rdi
  40679f:	e8 1c b6 ff ff       	callq  401dc0 <strchr@plt>
  4067a4:	48 89 45 a8          	mov    %rax,-0x58(%rbp)
  4067a8:	48 83 7d a8 00       	cmpq   $0x0,-0x58(%rbp)
  4067ad:	74 17                	je     4067c6 <child_process+0xefe>
  4067af:	48 8b 95 38 ff ff ff 	mov    -0xc8(%rbp),%rdx
  4067b6:	48 8b 85 28 ff ff ff 	mov    -0xd8(%rbp),%rax
  4067bd:	48 01 d0             	add    %rdx,%rax
  4067c0:	48 3b 45 a8          	cmp    -0x58(%rbp),%rax
  4067c4:	77 bb                	ja     406781 <child_process+0xeb9>
  4067c6:	48 8b 85 38 ff ff ff 	mov    -0xc8(%rbp),%rax
  4067cd:	48 89 c2             	mov    %rax,%rdx
  4067d0:	be 60 90 40 00       	mov    $0x409060,%esi
  4067d5:	48 89 df             	mov    %rbx,%rdi
  4067d8:	b8 00 00 00 00       	mov    $0x0,%eax
  4067dd:	e8 4e b7 ff ff       	callq  401f30 <fprintf@plt>
  4067e2:	48 8b 85 08 da ff ff 	mov    -0x25f8(%rbp),%rax
  4067e9:	4c 8b 60 10          	mov    0x10(%rax),%r12
  4067ed:	eb 1d                	jmp    40680c <child_process+0xf44>
  4067ef:	49 8b 04 24          	mov    (%r12),%rax
  4067f3:	48 89 c2             	mov    %rax,%rdx
  4067f6:	be 7f 90 40 00       	mov    $0x40907f,%esi
  4067fb:	48 89 df             	mov    %rbx,%rdi
  4067fe:	b8 00 00 00 00       	mov    $0x0,%eax
  406803:	e8 28 b7 ff ff       	callq  401f30 <fprintf@plt>
  406808:	49 83 c4 08          	add    $0x8,%r12
  40680c:	49 8b 04 24          	mov    (%r12),%rax
  406810:	48 85 c0             	test   %rax,%rax
  406813:	75 da                	jne    4067ef <child_process+0xf27>
  406815:	48 89 de             	mov    %rbx,%rsi
  406818:	bf 0a 00 00 00       	mov    $0xa,%edi
  40681d:	e8 7e b6 ff ff       	callq  401ea0 <fputc@plt>
  406822:	48 8b 95 78 ff ff ff 	mov    -0x88(%rbp),%rdx
  406829:	48 8d 85 d0 e9 ff ff 	lea    -0x1630(%rbp),%rax
  406830:	48 89 d1             	mov    %rdx,%rcx
  406833:	ba 00 10 00 00       	mov    $0x1000,%edx
  406838:	be 01 00 00 00       	mov    $0x1,%esi
  40683d:	48 89 c7             	mov    %rax,%rdi
  406840:	e8 db b4 ff ff       	callq  401d20 <fread@plt>
  406845:	89 85 24 ff ff ff    	mov    %eax,-0xdc(%rbp)
  40684b:	83 bd 24 ff ff ff 00 	cmpl   $0x0,-0xdc(%rbp)
  406852:	74 4c                	je     4068a0 <child_process+0xfd8>
  406854:	8b 85 24 ff ff ff    	mov    -0xdc(%rbp),%eax
  40685a:	89 45 a4             	mov    %eax,-0x5c(%rbp)
  40685d:	eb 36                	jmp    406895 <child_process+0xfcd>
  40685f:	8b 45 a4             	mov    -0x5c(%rbp),%eax
  406862:	48 63 d0             	movslq %eax,%rdx
  406865:	48 8d 85 d0 e9 ff ff 	lea    -0x1630(%rbp),%rax
  40686c:	48 89 d9             	mov    %rbx,%rcx
  40686f:	be 01 00 00 00       	mov    $0x1,%esi
  406874:	48 89 c7             	mov    %rax,%rdi
  406877:	e8 64 b9 ff ff       	callq  4021e0 <fwrite@plt>
  40687c:	89 85 24 ff ff ff    	mov    %eax,-0xdc(%rbp)
  406882:	83 bd 24 ff ff ff 00 	cmpl   $0x0,-0xdc(%rbp)
  406889:	7e 12                	jle    40689d <child_process+0xfd5>
  40688b:	8b 85 24 ff ff ff    	mov    -0xdc(%rbp),%eax
  406891:	29 45 a4             	sub    %eax,-0x5c(%rbp)
  406894:	90                   	nop
  406895:	83 7d a4 00          	cmpl   $0x0,-0x5c(%rbp)
  406899:	75 c4                	jne    40685f <child_process+0xf97>
  40689b:	eb 85                	jmp    406822 <child_process+0xf5a>
  40689d:	90                   	nop
  40689e:	eb 82                	jmp    406822 <child_process+0xf5a>
  4068a0:	90                   	nop
  4068a1:	48 89 df             	mov    %rbx,%rdi
  4068a4:	e8 b5 1b 00 00       	callq  40845e <cron_pclose>
  4068a9:	89 45 c0             	mov    %eax,-0x40(%rbp)
  4068ac:	83 7d c0 00          	cmpl   $0x0,-0x40(%rbp)
  4068b0:	74 5e                	je     406910 <child_process+0x1048>
  4068b2:	41 83 fd 01          	cmp    $0x1,%r13d
  4068b6:	75 07                	jne    4068bf <child_process+0xff7>
  4068b8:	b9 d8 8e 40 00       	mov    $0x408ed8,%ecx
  4068bd:	eb 05                	jmp    4068c4 <child_process+0xffc>
  4068bf:	b9 91 90 40 00       	mov    $0x409091,%ecx
  4068c4:	8b 55 c0             	mov    -0x40(%rbp),%edx
  4068c7:	48 8d 85 10 da ff ff 	lea    -0x25f0(%rbp),%rax
  4068ce:	41 89 d1             	mov    %edx,%r9d
  4068d1:	49 89 c8             	mov    %rcx,%r8
  4068d4:	44 89 e9             	mov    %r13d,%ecx
  4068d7:	ba 98 90 40 00       	mov    $0x409098,%edx
  4068dc:	be e8 03 00 00       	mov    $0x3e8,%esi
  4068e1:	48 89 c7             	mov    %rax,%rdi
  4068e4:	b8 00 00 00 00       	mov    $0x0,%eax
  4068e9:	e8 02 b5 ff ff       	callq  401df0 <snprintf@plt>
  4068ee:	e8 4d b4 ff ff       	callq  401d40 <getpid@plt>
  4068f3:	89 c6                	mov    %eax,%esi
  4068f5:	48 8d 95 10 da ff ff 	lea    -0x25f0(%rbp),%rdx
  4068fc:	48 8b 45 90          	mov    -0x70(%rbp),%rax
  406900:	48 89 d1             	mov    %rdx,%rcx
  406903:	ba cd 90 40 00       	mov    $0x4090cd,%edx
  406908:	48 89 c7             	mov    %rax,%rdi
  40690b:	e8 c9 0b 00 00       	callq  4074d9 <log_it>
  406910:	48 8b 85 78 ff ff ff 	mov    -0x88(%rbp),%rax
  406917:	48 89 c7             	mov    %rax,%rdi
  40691a:	e8 f1 b3 ff ff       	callq  401d10 <ferror@plt>
  40691f:	85 c0                	test   %eax,%eax
  406921:	74 26                	je     406949 <child_process+0x1081>
  406923:	e8 18 b4 ff ff       	callq  401d40 <getpid@plt>
  406928:	89 c6                	mov    %eax,%esi
  40692a:	48 8b 45 90          	mov    -0x70(%rbp),%rax
  40692e:	b9 d2 90 40 00       	mov    $0x4090d2,%ecx
  406933:	ba cd 90 40 00       	mov    $0x4090cd,%edx
  406938:	48 89 c7             	mov    %rax,%rdi
  40693b:	e8 99 0b 00 00       	callq  4074d9 <log_it>
  406940:	eb 07                	jmp    406949 <child_process+0x1081>
  406942:	90                   	nop
  406943:	eb 04                	jmp    406949 <child_process+0x1081>
  406945:	90                   	nop
  406946:	eb 01                	jmp    406949 <child_process+0x1081>
  406948:	90                   	nop
  406949:	48 8b 85 78 ff ff ff 	mov    -0x88(%rbp),%rax
  406950:	48 89 c7             	mov    %rax,%rdi
  406953:	e8 f8 b3 ff ff       	callq  401d50 <fclose@plt>
  406958:	8b 05 6a 4b 20 00    	mov    0x204b6a(%rip),%eax        # 60b4c8 <log_level>
  40695e:	83 e0 02             	and    $0x2,%eax
  406961:	85 c0                	test   %eax,%eax
  406963:	0f 84 c1 00 00 00    	je     406a2a <child_process+0x1162>
  406969:	8b 05 59 4b 20 00    	mov    0x204b59(%rip),%eax        # 60b4c8 <log_level>
  40696f:	83 e0 08             	and    $0x8,%eax
  406972:	85 c0                	test   %eax,%eax
  406974:	74 5c                	je     4069d2 <child_process+0x110a>
  406976:	48 8b 85 08 da ff ff 	mov    -0x25f8(%rbp),%rax
  40697d:	48 8b 48 18          	mov    0x18(%rax),%rcx
  406981:	8b 95 74 ff ff ff    	mov    -0x8c(%rbp),%edx
  406987:	48 8d 85 10 da ff ff 	lea    -0x25f0(%rbp),%rax
  40698e:	49 89 c8             	mov    %rcx,%r8
  406991:	89 d1                	mov    %edx,%ecx
  406993:	ba 90 8e 40 00       	mov    $0x408e90,%edx
  406998:	be f0 03 00 00       	mov    $0x3f0,%esi
  40699d:	48 89 c7             	mov    %rax,%rdi
  4069a0:	b8 00 00 00 00       	mov    $0x0,%eax
  4069a5:	e8 46 b4 ff ff       	callq  401df0 <snprintf@plt>
  4069aa:	48 8d 85 10 da ff ff 	lea    -0x25f0(%rbp),%rax
  4069b1:	48 89 c7             	mov    %rax,%rdi
  4069b4:	e8 b7 b3 ff ff       	callq  401d70 <strlen@plt>
  4069b9:	89 c2                	mov    %eax,%edx
  4069bb:	48 8d 85 10 da ff ff 	lea    -0x25f0(%rbp),%rax
  4069c2:	89 d6                	mov    %edx,%esi
  4069c4:	48 89 c7             	mov    %rax,%rdi
  4069c7:	e8 d7 0c 00 00       	callq  4076a3 <mkprints>
  4069cc:	48 89 45 98          	mov    %rax,-0x68(%rbp)
  4069d0:	eb 2e                	jmp    406a00 <child_process+0x1138>
  4069d2:	48 8b 85 08 da ff ff 	mov    -0x25f8(%rbp),%rax
  4069d9:	48 8b 40 18          	mov    0x18(%rax),%rax
  4069dd:	48 89 c7             	mov    %rax,%rdi
  4069e0:	e8 8b b3 ff ff       	callq  401d70 <strlen@plt>
  4069e5:	89 c2                	mov    %eax,%edx
  4069e7:	48 8b 85 08 da ff ff 	mov    -0x25f8(%rbp),%rax
  4069ee:	48 8b 40 18          	mov    0x18(%rax),%rax
  4069f2:	89 d6                	mov    %edx,%esi
  4069f4:	48 89 c7             	mov    %rax,%rdi
  4069f7:	e8 a7 0c 00 00       	callq  4076a3 <mkprints>
  4069fc:	48 89 45 98          	mov    %rax,-0x68(%rbp)
  406a00:	48 8b 55 98          	mov    -0x68(%rbp),%rdx
  406a04:	8b b5 74 ff ff ff    	mov    -0x8c(%rbp),%esi
  406a0a:	48 8b 45 90          	mov    -0x70(%rbp),%rax
  406a0e:	48 89 d1             	mov    %rdx,%rcx
  406a11:	ba ee 90 40 00       	mov    $0x4090ee,%edx
  406a16:	48 89 c7             	mov    %rax,%rdi
  406a19:	e8 bb 0a 00 00       	callq  4074d9 <log_it>
  406a1e:	48 8b 45 98          	mov    -0x68(%rbp),%rax
  406a22:	48 89 c7             	mov    %rax,%rdi
  406a25:	e8 06 b2 ff ff       	callq  401c30 <free@plt>
  406a2a:	90                   	nop
  406a2b:	48 81 c4 d8 25 00 00 	add    $0x25d8,%rsp
  406a32:	5b                   	pop    %rbx
  406a33:	41 5c                	pop    %r12
  406a35:	41 5d                	pop    %r13
  406a37:	41 5e                	pop    %r14
  406a39:	41 5f                	pop    %r15
  406a3b:	5d                   	pop    %rbp
  406a3c:	c3                   	retq   

0000000000406a3d <do_univ>:
  406a3d:	55                   	push   %rbp
  406a3e:	48 89 e5             	mov    %rsp,%rbp
  406a41:	48 89 7d f8          	mov    %rdi,-0x8(%rbp)
  406a45:	90                   	nop
  406a46:	5d                   	pop    %rbp
  406a47:	c3                   	retq   

0000000000406a48 <strcmp_until>:
  406a48:	55                   	push   %rbp
  406a49:	48 89 e5             	mov    %rsp,%rbp
  406a4c:	53                   	push   %rbx
  406a4d:	48 89 7d f0          	mov    %rdi,-0x10(%rbp)
  406a51:	48 89 75 e8          	mov    %rsi,-0x18(%rbp)
  406a55:	89 55 e4             	mov    %edx,-0x1c(%rbp)
  406a58:	eb 0a                	jmp    406a64 <strcmp_until+0x1c>
  406a5a:	48 83 45 f0 01       	addq   $0x1,-0x10(%rbp)
  406a5f:	48 83 45 e8 01       	addq   $0x1,-0x18(%rbp)
  406a64:	48 8b 45 f0          	mov    -0x10(%rbp),%rax
  406a68:	0f b6 00             	movzbl (%rax),%eax
  406a6b:	84 c0                	test   %al,%al
  406a6d:	74 21                	je     406a90 <strcmp_until+0x48>
  406a6f:	48 8b 45 f0          	mov    -0x10(%rbp),%rax
  406a73:	0f b6 00             	movzbl (%rax),%eax
  406a76:	0f be c0             	movsbl %al,%eax
  406a79:	3b 45 e4             	cmp    -0x1c(%rbp),%eax
  406a7c:	74 12                	je     406a90 <strcmp_until+0x48>
  406a7e:	48 8b 45 f0          	mov    -0x10(%rbp),%rax
  406a82:	0f b6 10             	movzbl (%rax),%edx
  406a85:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  406a89:	0f b6 00             	movzbl (%rax),%eax
  406a8c:	38 c2                	cmp    %al,%dl
  406a8e:	74 ca                	je     406a5a <strcmp_until+0x12>
  406a90:	48 8b 45 f0          	mov    -0x10(%rbp),%rax
  406a94:	0f b6 00             	movzbl (%rax),%eax
  406a97:	84 c0                	test   %al,%al
  406a99:	74 0f                	je     406aaa <strcmp_until+0x62>
  406a9b:	48 8b 45 f0          	mov    -0x10(%rbp),%rax
  406a9f:	0f b6 00             	movzbl (%rax),%eax
  406aa2:	0f be c0             	movsbl %al,%eax
  406aa5:	3b 45 e4             	cmp    -0x1c(%rbp),%eax
  406aa8:	75 21                	jne    406acb <strcmp_until+0x83>
  406aaa:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  406aae:	0f b6 00             	movzbl (%rax),%eax
  406ab1:	84 c0                	test   %al,%al
  406ab3:	74 0f                	je     406ac4 <strcmp_until+0x7c>
  406ab5:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  406ab9:	0f b6 00             	movzbl (%rax),%eax
  406abc:	0f be c0             	movsbl %al,%eax
  406abf:	3b 45 e4             	cmp    -0x1c(%rbp),%eax
  406ac2:	75 07                	jne    406acb <strcmp_until+0x83>
  406ac4:	bb 00 00 00 00       	mov    $0x0,%ebx
  406ac9:	eb 18                	jmp    406ae3 <strcmp_until+0x9b>
  406acb:	48 8b 45 f0          	mov    -0x10(%rbp),%rax
  406acf:	0f b6 00             	movzbl (%rax),%eax
  406ad2:	0f be d0             	movsbl %al,%edx
  406ad5:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  406ad9:	0f b6 00             	movzbl (%rax),%eax
  406adc:	0f be c0             	movsbl %al,%eax
  406adf:	89 d3                	mov    %edx,%ebx
  406ae1:	29 c3                	sub    %eax,%ebx
  406ae3:	89 d8                	mov    %ebx,%eax
  406ae5:	5b                   	pop    %rbx
  406ae6:	5d                   	pop    %rbp
  406ae7:	c3                   	retq   

0000000000406ae8 <strdtb>:
  406ae8:	55                   	push   %rbp
  406ae9:	48 89 e5             	mov    %rsp,%rbp
  406aec:	48 83 ec 20          	sub    $0x20,%rsp
  406af0:	48 89 7d e8          	mov    %rdi,-0x18(%rbp)
  406af4:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  406af8:	48 89 45 f8          	mov    %rax,-0x8(%rbp)
  406afc:	eb 05                	jmp    406b03 <strdtb+0x1b>
  406afe:	48 83 45 f8 01       	addq   $0x1,-0x8(%rbp)
  406b03:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  406b07:	0f b6 00             	movzbl (%rax),%eax
  406b0a:	84 c0                	test   %al,%al
  406b0c:	75 f0                	jne    406afe <strdtb+0x16>
  406b0e:	48 83 6d f8 01       	subq   $0x1,-0x8(%rbp)
  406b13:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  406b17:	48 3b 45 e8          	cmp    -0x18(%rbp),%rax
  406b1b:	72 28                	jb     406b45 <strdtb+0x5d>
  406b1d:	e8 7e b7 ff ff       	callq  4022a0 <__ctype_b_loc@plt>
  406b22:	48 8b 10             	mov    (%rax),%rdx
  406b25:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  406b29:	0f b6 00             	movzbl (%rax),%eax
  406b2c:	48 0f be c0          	movsbq %al,%rax
  406b30:	48 01 c0             	add    %rax,%rax
  406b33:	48 01 d0             	add    %rdx,%rax
  406b36:	0f b7 00             	movzwl (%rax),%eax
  406b39:	0f b7 c0             	movzwl %ax,%eax
  406b3c:	25 00 20 00 00       	and    $0x2000,%eax
  406b41:	85 c0                	test   %eax,%eax
  406b43:	75 c9                	jne    406b0e <strdtb+0x26>
  406b45:	48 83 45 f8 01       	addq   $0x1,-0x8(%rbp)
  406b4a:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  406b4e:	c6 00 00             	movb   $0x0,(%rax)
  406b51:	48 8b 55 f8          	mov    -0x8(%rbp),%rdx
  406b55:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  406b59:	48 29 c2             	sub    %rax,%rdx
  406b5c:	48 89 d0             	mov    %rdx,%rax
  406b5f:	c9                   	leaveq 
  406b60:	c3                   	retq   

0000000000406b61 <set_debug_flags>:
  406b61:	55                   	push   %rbp
  406b62:	48 89 e5             	mov    %rsp,%rbp
  406b65:	48 83 ec 10          	sub    $0x10,%rsp
  406b69:	48 89 7d f8          	mov    %rdi,-0x8(%rbp)
  406b6d:	bf f8 90 40 00       	mov    $0x4090f8,%edi
  406b72:	e8 89 b1 ff ff       	callq  401d00 <puts@plt>
  406b77:	b8 00 00 00 00       	mov    $0x0,%eax
  406b7c:	c9                   	leaveq 
  406b7d:	c3                   	retq   

0000000000406b7e <set_cron_uid>:
  406b7e:	55                   	push   %rbp
  406b7f:	48 89 e5             	mov    %rsp,%rbp
  406b82:	bf 00 00 00 00       	mov    $0x0,%edi
  406b87:	e8 24 b7 ff ff       	callq  4022b0 <seteuid@plt>
  406b8c:	85 c0                	test   %eax,%eax
  406b8e:	79 14                	jns    406ba4 <set_cron_uid+0x26>
  406b90:	bf 2c 91 40 00       	mov    $0x40912c,%edi
  406b95:	e8 56 b5 ff ff       	callq  4020f0 <perror@plt>
  406b9a:	bf 01 00 00 00       	mov    $0x1,%edi
  406b9f:	e8 1c b6 ff ff       	callq  4021c0 <exit@plt>
  406ba4:	90                   	nop
  406ba5:	5d                   	pop    %rbp
  406ba6:	c3                   	retq   

0000000000406ba7 <set_cron_cwd>:
  406ba7:	55                   	push   %rbp
  406ba8:	48 89 e5             	mov    %rsp,%rbp
  406bab:	48 81 ec a0 00 00 00 	sub    $0xa0,%rsp
  406bb2:	48 8d 85 60 ff ff ff 	lea    -0xa0(%rbp),%rax
  406bb9:	48 89 c6             	mov    %rax,%rsi
  406bbc:	bf 34 91 40 00       	mov    $0x409134,%edi
  406bc1:	e8 3a 1c 00 00       	callq  408800 <__stat>
  406bc6:	85 c0                	test   %eax,%eax
  406bc8:	0f 89 c7 00 00 00    	jns    406c95 <set_cron_cwd+0xee>
  406bce:	e8 ad b0 ff ff       	callq  401c80 <__errno_location@plt>
  406bd3:	8b 00                	mov    (%rax),%eax
  406bd5:	83 f8 02             	cmp    $0x2,%eax
  406bd8:	0f 85 b7 00 00 00    	jne    406c95 <set_cron_cwd+0xee>
  406bde:	bf 34 91 40 00       	mov    $0x409134,%edi
  406be3:	e8 08 b5 ff ff       	callq  4020f0 <perror@plt>
  406be8:	e8 b3 b1 ff ff       	callq  401da0 <getuid@plt>
  406bed:	85 c0                	test   %eax,%eax
  406bef:	74 0a                	je     406bfb <set_cron_cwd+0x54>
  406bf1:	bf 01 00 00 00       	mov    $0x1,%edi
  406bf6:	e8 c5 b5 ff ff       	callq  4021c0 <exit@plt>
  406bfb:	bf 00 00 00 00       	mov    $0x0,%edi
  406c00:	e8 5b b3 ff ff       	callq  401f60 <umask@plt>
  406c05:	89 45 fc             	mov    %eax,-0x4(%rbp)
  406c08:	be ed 01 00 00       	mov    $0x1ed,%esi
  406c0d:	bf 34 91 40 00       	mov    $0x409134,%edi
  406c12:	e8 c9 b0 ff ff       	callq  401ce0 <mkdir@plt>
  406c17:	85 c0                	test   %eax,%eax
  406c19:	75 3e                	jne    406c59 <set_cron_cwd+0xb2>
  406c1b:	48 8b 05 be 4b 20 00 	mov    0x204bbe(%rip),%rax        # 60b7e0 <stderr@@GLIBC_2.2.5>
  406c22:	ba 34 91 40 00       	mov    $0x409134,%edx
  406c27:	be 44 91 40 00       	mov    $0x409144,%esi
  406c2c:	48 89 c7             	mov    %rax,%rdi
  406c2f:	b8 00 00 00 00       	mov    $0x0,%eax
  406c34:	e8 f7 b2 ff ff       	callq  401f30 <fprintf@plt>
  406c39:	48 8d 85 60 ff ff ff 	lea    -0xa0(%rbp),%rax
  406c40:	48 89 c6             	mov    %rax,%rsi
  406c43:	bf 34 91 40 00       	mov    $0x409134,%edi
  406c48:	e8 b3 1b 00 00       	callq  408800 <__stat>
  406c4d:	8b 45 fc             	mov    -0x4(%rbp),%eax
  406c50:	89 c7                	mov    %eax,%edi
  406c52:	e8 09 b3 ff ff       	callq  401f60 <umask@plt>
  406c57:	eb 3c                	jmp    406c95 <set_cron_cwd+0xee>
  406c59:	e8 22 b0 ff ff       	callq  401c80 <__errno_location@plt>
  406c5e:	8b 00                	mov    (%rax),%eax
  406c60:	89 c7                	mov    %eax,%edi
  406c62:	e8 a9 b5 ff ff       	callq  402210 <strerror@plt>
  406c67:	48 89 c2             	mov    %rax,%rdx
  406c6a:	48 8b 05 6f 4b 20 00 	mov    0x204b6f(%rip),%rax        # 60b7e0 <stderr@@GLIBC_2.2.5>
  406c71:	48 89 d1             	mov    %rdx,%rcx
  406c74:	ba 34 91 40 00       	mov    $0x409134,%edx
  406c79:	be 51 91 40 00       	mov    $0x409151,%esi
  406c7e:	48 89 c7             	mov    %rax,%rdi
  406c81:	b8 00 00 00 00       	mov    $0x0,%eax
  406c86:	e8 a5 b2 ff ff       	callq  401f30 <fprintf@plt>
  406c8b:	bf 01 00 00 00       	mov    $0x1,%edi
  406c90:	e8 2b b5 ff ff       	callq  4021c0 <exit@plt>
  406c95:	8b 85 78 ff ff ff    	mov    -0x88(%rbp),%eax
  406c9b:	25 00 40 00 00       	and    $0x4000,%eax
  406ca0:	85 c0                	test   %eax,%eax
  406ca2:	75 28                	jne    406ccc <set_cron_cwd+0x125>
  406ca4:	48 8b 05 35 4b 20 00 	mov    0x204b35(%rip),%rax        # 60b7e0 <stderr@@GLIBC_2.2.5>
  406cab:	ba 34 91 40 00       	mov    $0x409134,%edx
  406cb0:	be 60 91 40 00       	mov    $0x409160,%esi
  406cb5:	48 89 c7             	mov    %rax,%rdi
  406cb8:	b8 00 00 00 00       	mov    $0x0,%eax
  406cbd:	e8 6e b2 ff ff       	callq  401f30 <fprintf@plt>
  406cc2:	bf 01 00 00 00       	mov    $0x1,%edi
  406cc7:	e8 f4 b4 ff ff       	callq  4021c0 <exit@plt>
  406ccc:	bf 34 91 40 00       	mov    $0x409134,%edi
  406cd1:	e8 ba b0 ff ff       	callq  401d90 <chdir@plt>
  406cd6:	85 c0                	test   %eax,%eax
  406cd8:	79 3c                	jns    406d16 <set_cron_cwd+0x16f>
  406cda:	e8 a1 af ff ff       	callq  401c80 <__errno_location@plt>
  406cdf:	8b 00                	mov    (%rax),%eax
  406ce1:	89 c7                	mov    %eax,%edi
  406ce3:	e8 28 b5 ff ff       	callq  402210 <strerror@plt>
  406ce8:	48 89 c2             	mov    %rax,%rdx
  406ceb:	48 8b 05 ee 4a 20 00 	mov    0x204aee(%rip),%rax        # 60b7e0 <stderr@@GLIBC_2.2.5>
  406cf2:	48 89 d1             	mov    %rdx,%rcx
  406cf5:	ba 34 91 40 00       	mov    $0x409134,%edx
  406cfa:	be 87 91 40 00       	mov    $0x409187,%esi
  406cff:	48 89 c7             	mov    %rax,%rdi
  406d02:	b8 00 00 00 00       	mov    $0x0,%eax
  406d07:	e8 24 b2 ff ff       	callq  401f30 <fprintf@plt>
  406d0c:	bf 01 00 00 00       	mov    $0x1,%edi
  406d11:	e8 aa b4 ff ff       	callq  4021c0 <exit@plt>
  406d16:	48 8d 85 60 ff ff ff 	lea    -0xa0(%rbp),%rax
  406d1d:	48 89 c6             	mov    %rax,%rsi
  406d20:	bf 96 91 40 00       	mov    $0x409196,%edi
  406d25:	e8 d6 1a 00 00       	callq  408800 <__stat>
  406d2a:	85 c0                	test   %eax,%eax
  406d2c:	0f 89 90 01 00 00    	jns    406ec2 <set_cron_cwd+0x31b>
  406d32:	e8 49 af ff ff       	callq  401c80 <__errno_location@plt>
  406d37:	8b 00                	mov    (%rax),%eax
  406d39:	83 f8 02             	cmp    $0x2,%eax
  406d3c:	0f 85 80 01 00 00    	jne    406ec2 <set_cron_cwd+0x31b>
  406d42:	bf 96 91 40 00       	mov    $0x409196,%edi
  406d47:	e8 a4 b3 ff ff       	callq  4020f0 <perror@plt>
  406d4c:	e8 4f b0 ff ff       	callq  401da0 <getuid@plt>
  406d51:	85 c0                	test   %eax,%eax
  406d53:	74 0a                	je     406d5f <set_cron_cwd+0x1b8>
  406d55:	bf 01 00 00 00       	mov    $0x1,%edi
  406d5a:	e8 61 b4 ff ff       	callq  4021c0 <exit@plt>
  406d5f:	bf 00 00 00 00       	mov    $0x0,%edi
  406d64:	e8 f7 b1 ff ff       	callq  401f60 <umask@plt>
  406d69:	89 45 fc             	mov    %eax,-0x4(%rbp)
  406d6c:	be d8 03 00 00       	mov    $0x3d8,%esi
  406d71:	bf 96 91 40 00       	mov    $0x409196,%edi
  406d76:	e8 65 af ff ff       	callq  401ce0 <mkdir@plt>
  406d7b:	85 c0                	test   %eax,%eax
  406d7d:	75 3f                	jne    406dbe <set_cron_cwd+0x217>
  406d7f:	48 8b 05 5a 4a 20 00 	mov    0x204a5a(%rip),%rax        # 60b7e0 <stderr@@GLIBC_2.2.5>
  406d86:	ba 96 91 40 00       	mov    $0x409196,%edx
  406d8b:	be 44 91 40 00       	mov    $0x409144,%esi
  406d90:	48 89 c7             	mov    %rax,%rdi
  406d93:	b8 00 00 00 00       	mov    $0x0,%eax
  406d98:	e8 93 b1 ff ff       	callq  401f30 <fprintf@plt>
  406d9d:	8b 45 fc             	mov    -0x4(%rbp),%eax
  406da0:	89 c7                	mov    %eax,%edi
  406da2:	e8 b9 b1 ff ff       	callq  401f60 <umask@plt>
  406da7:	bf 9f 91 40 00       	mov    $0x40919f,%edi
  406dac:	e8 bf b1 ff ff       	callq  401f70 <getgrnam@plt>
  406db1:	48 89 45 f0          	mov    %rax,-0x10(%rbp)
  406db5:	48 83 7d f0 00       	cmpq   $0x0,-0x10(%rbp)
  406dba:	75 7a                	jne    406e36 <set_cron_cwd+0x28f>
  406dbc:	eb 3c                	jmp    406dfa <set_cron_cwd+0x253>
  406dbe:	e8 bd ae ff ff       	callq  401c80 <__errno_location@plt>
  406dc3:	8b 00                	mov    (%rax),%eax
  406dc5:	89 c7                	mov    %eax,%edi
  406dc7:	e8 44 b4 ff ff       	callq  402210 <strerror@plt>
  406dcc:	48 89 c2             	mov    %rax,%rdx
  406dcf:	48 8b 05 0a 4a 20 00 	mov    0x204a0a(%rip),%rax        # 60b7e0 <stderr@@GLIBC_2.2.5>
  406dd6:	48 89 d1             	mov    %rdx,%rcx
  406dd9:	ba 96 91 40 00       	mov    $0x409196,%edx
  406dde:	be 51 91 40 00       	mov    $0x409151,%esi
  406de3:	48 89 c7             	mov    %rax,%rdi
  406de6:	b8 00 00 00 00       	mov    $0x0,%eax
  406deb:	e8 40 b1 ff ff       	callq  401f30 <fprintf@plt>
  406df0:	bf 01 00 00 00       	mov    $0x1,%edi
  406df5:	e8 c6 b3 ff ff       	callq  4021c0 <exit@plt>
  406dfa:	e8 81 ae ff ff       	callq  401c80 <__errno_location@plt>
  406dff:	8b 00                	mov    (%rax),%eax
  406e01:	89 c7                	mov    %eax,%edi
  406e03:	e8 08 b4 ff ff       	callq  402210 <strerror@plt>
  406e08:	48 89 c2             	mov    %rax,%rdx
  406e0b:	48 8b 05 ce 49 20 00 	mov    0x2049ce(%rip),%rax        # 60b7e0 <stderr@@GLIBC_2.2.5>
  406e12:	48 89 d1             	mov    %rdx,%rcx
  406e15:	ba 96 91 40 00       	mov    $0x409196,%edx
  406e1a:	be a7 91 40 00       	mov    $0x4091a7,%esi
  406e1f:	48 89 c7             	mov    %rax,%rdi
  406e22:	b8 00 00 00 00       	mov    $0x0,%eax
  406e27:	e8 04 b1 ff ff       	callq  401f30 <fprintf@plt>
  406e2c:	bf 01 00 00 00       	mov    $0x1,%edi
  406e31:	e8 8a b3 ff ff       	callq  4021c0 <exit@plt>
  406e36:	48 8b 45 f0          	mov    -0x10(%rbp),%rax
  406e3a:	8b 40 10             	mov    0x10(%rax),%eax
  406e3d:	89 c2                	mov    %eax,%edx
  406e3f:	be ff ff ff ff       	mov    $0xffffffff,%esi
  406e44:	bf 96 91 40 00       	mov    $0x409196,%edi
  406e49:	e8 22 b2 ff ff       	callq  402070 <chown@plt>
  406e4e:	85 c0                	test   %eax,%eax
  406e50:	75 34                	jne    406e86 <set_cron_cwd+0x2df>
  406e52:	48 8b 05 87 49 20 00 	mov    0x204987(%rip),%rax        # 60b7e0 <stderr@@GLIBC_2.2.5>
  406e59:	ba 96 91 40 00       	mov    $0x409196,%edx
  406e5e:	be b9 91 40 00       	mov    $0x4091b9,%esi
  406e63:	48 89 c7             	mov    %rax,%rdi
  406e66:	b8 00 00 00 00       	mov    $0x0,%eax
  406e6b:	e8 c0 b0 ff ff       	callq  401f30 <fprintf@plt>
  406e70:	48 8d 85 60 ff ff ff 	lea    -0xa0(%rbp),%rax
  406e77:	48 89 c6             	mov    %rax,%rsi
  406e7a:	bf 96 91 40 00       	mov    $0x409196,%edi
  406e7f:	e8 7c 19 00 00       	callq  408800 <__stat>
  406e84:	eb 3c                	jmp    406ec2 <set_cron_cwd+0x31b>
  406e86:	e8 f5 ad ff ff       	callq  401c80 <__errno_location@plt>
  406e8b:	8b 00                	mov    (%rax),%eax
  406e8d:	89 c7                	mov    %eax,%edi
  406e8f:	e8 7c b3 ff ff       	callq  402210 <strerror@plt>
  406e94:	48 89 c2             	mov    %rax,%rdx
  406e97:	48 8b 05 42 49 20 00 	mov    0x204942(%rip),%rax        # 60b7e0 <stderr@@GLIBC_2.2.5>
  406e9e:	48 89 d1             	mov    %rdx,%rcx
  406ea1:	ba 96 91 40 00       	mov    $0x409196,%edx
  406ea6:	be c6 91 40 00       	mov    $0x4091c6,%esi
  406eab:	48 89 c7             	mov    %rax,%rdi
  406eae:	b8 00 00 00 00       	mov    $0x0,%eax
  406eb3:	e8 78 b0 ff ff       	callq  401f30 <fprintf@plt>
  406eb8:	bf 01 00 00 00       	mov    $0x1,%edi
  406ebd:	e8 fe b2 ff ff       	callq  4021c0 <exit@plt>
  406ec2:	8b 85 78 ff ff ff    	mov    -0x88(%rbp),%eax
  406ec8:	25 00 40 00 00       	and    $0x4000,%eax
  406ecd:	85 c0                	test   %eax,%eax
  406ecf:	75 28                	jne    406ef9 <set_cron_cwd+0x352>
  406ed1:	48 8b 05 08 49 20 00 	mov    0x204908(%rip),%rax        # 60b7e0 <stderr@@GLIBC_2.2.5>
  406ed8:	ba 96 91 40 00       	mov    $0x409196,%edx
  406edd:	be 60 91 40 00       	mov    $0x409160,%esi
  406ee2:	48 89 c7             	mov    %rax,%rdi
  406ee5:	b8 00 00 00 00       	mov    $0x0,%eax
  406eea:	e8 41 b0 ff ff       	callq  401f30 <fprintf@plt>
  406eef:	bf 01 00 00 00       	mov    $0x1,%edi
  406ef4:	e8 c7 b2 ff ff       	callq  4021c0 <exit@plt>
  406ef9:	90                   	nop
  406efa:	c9                   	leaveq 
  406efb:	c3                   	retq   

0000000000406efc <acquire_daemonlock>:
  406efc:	55                   	push   %rbp
  406efd:	48 89 e5             	mov    %rsp,%rbp
  406f00:	53                   	push   %rbx
  406f01:	48 81 ec 88 04 00 00 	sub    $0x488,%rsp
  406f08:	89 bd 7c fb ff ff    	mov    %edi,-0x484(%rbp)
  406f0e:	83 bd 7c fb ff ff 00 	cmpl   $0x0,-0x484(%rbp)
  406f15:	74 2b                	je     406f42 <acquire_daemonlock+0x46>
  406f17:	48 8b 05 2a 4e 20 00 	mov    0x204e2a(%rip),%rax        # 60bd48 <fp.5051>
  406f1e:	48 85 c0             	test   %rax,%rax
  406f21:	74 1f                	je     406f42 <acquire_daemonlock+0x46>
  406f23:	48 8b 05 1e 4e 20 00 	mov    0x204e1e(%rip),%rax        # 60bd48 <fp.5051>
  406f2a:	48 89 c7             	mov    %rax,%rdi
  406f2d:	e8 1e ae ff ff       	callq  401d50 <fclose@plt>
  406f32:	48 c7 05 0b 4e 20 00 	movq   $0x0,0x204e0b(%rip)        # 60bd48 <fp.5051>
  406f39:	00 00 00 00 
  406f3d:	e9 9f 02 00 00       	jmpq   4071e1 <acquire_daemonlock+0x2e5>
  406f42:	48 8b 05 ff 4d 20 00 	mov    0x204dff(%rip),%rax        # 60bd48 <fp.5051>
  406f49:	48 85 c0             	test   %rax,%rax
  406f4c:	0f 85 26 02 00 00    	jne    407178 <acquire_daemonlock+0x27c>
  406f52:	48 8d 85 80 fb ff ff 	lea    -0x480(%rbp),%rax
  406f59:	b9 d5 91 40 00       	mov    $0x4091d5,%ecx
  406f5e:	ba df 91 40 00       	mov    $0x4091df,%edx
  406f63:	be 64 00 00 00       	mov    $0x64,%esi
  406f68:	48 89 c7             	mov    %rax,%rdi
  406f6b:	b8 00 00 00 00       	mov    $0x0,%eax
  406f70:	e8 7b ae ff ff       	callq  401df0 <snprintf@plt>
  406f75:	48 8d 85 80 fb ff ff 	lea    -0x480(%rbp),%rax
  406f7c:	ba a4 01 00 00       	mov    $0x1a4,%edx
  406f81:	be 42 00 00 00       	mov    $0x42,%esi
  406f86:	48 89 c7             	mov    %rax,%rdi
  406f89:	b8 00 00 00 00       	mov    $0x0,%eax
  406f8e:	e8 2d b1 ff ff       	callq  4020c0 <open@plt>
  406f93:	89 45 ec             	mov    %eax,-0x14(%rbp)
  406f96:	83 7d ec ff          	cmpl   $0xffffffff,-0x14(%rbp)
  406f9a:	74 26                	je     406fc2 <acquire_daemonlock+0xc6>
  406f9c:	8b 45 ec             	mov    -0x14(%rbp),%eax
  406f9f:	be eb 91 40 00       	mov    $0x4091eb,%esi
  406fa4:	89 c7                	mov    %eax,%edi
  406fa6:	e8 e5 b0 ff ff       	callq  402090 <fdopen@plt>
  406fab:	48 89 05 96 4d 20 00 	mov    %rax,0x204d96(%rip)        # 60bd48 <fp.5051>
  406fb2:	48 8b 05 8f 4d 20 00 	mov    0x204d8f(%rip),%rax        # 60bd48 <fp.5051>
  406fb9:	48 85 c0             	test   %rax,%rax
  406fbc:	0f 85 8d 00 00 00    	jne    40704f <acquire_daemonlock+0x153>
  406fc2:	e8 b9 ac ff ff       	callq  401c80 <__errno_location@plt>
  406fc7:	8b 00                	mov    (%rax),%eax
  406fc9:	89 c7                	mov    %eax,%edi
  406fcb:	e8 40 b2 ff ff       	callq  402210 <strerror@plt>
  406fd0:	48 89 c1             	mov    %rax,%rcx
  406fd3:	48 8d 95 80 fb ff ff 	lea    -0x480(%rbp),%rdx
  406fda:	48 8d 85 f0 fb ff ff 	lea    -0x410(%rbp),%rax
  406fe1:	49 89 c8             	mov    %rcx,%r8
  406fe4:	48 89 d1             	mov    %rdx,%rcx
  406fe7:	ba ee 91 40 00       	mov    $0x4091ee,%edx
  406fec:	be e8 03 00 00       	mov    $0x3e8,%esi
  406ff1:	48 89 c7             	mov    %rax,%rdi
  406ff4:	b8 00 00 00 00       	mov    $0x0,%eax
  406ff9:	e8 f2 ad ff ff       	callq  401df0 <snprintf@plt>
  406ffe:	48 8b 15 83 55 20 00 	mov    0x205583(%rip),%rdx        # 60c588 <ProgramName>
  407005:	48 8b 05 d4 47 20 00 	mov    0x2047d4(%rip),%rax        # 60b7e0 <stderr@@GLIBC_2.2.5>
  40700c:	48 8d 8d f0 fb ff ff 	lea    -0x410(%rbp),%rcx
  407013:	be 0a 92 40 00       	mov    $0x40920a,%esi
  407018:	48 89 c7             	mov    %rax,%rdi
  40701b:	b8 00 00 00 00       	mov    $0x0,%eax
  407020:	e8 0b af ff ff       	callq  401f30 <fprintf@plt>
  407025:	e8 16 ad ff ff       	callq  401d40 <getpid@plt>
  40702a:	89 c6                	mov    %eax,%esi
  40702c:	48 8d 85 f0 fb ff ff 	lea    -0x410(%rbp),%rax
  407033:	48 89 c1             	mov    %rax,%rcx
  407036:	ba 12 92 40 00       	mov    $0x409212,%edx
  40703b:	bf 18 92 40 00       	mov    $0x409218,%edi
  407040:	e8 94 04 00 00       	callq  4074d9 <log_it>
  407045:	bf 01 00 00 00       	mov    $0x1,%edi
  40704a:	e8 71 b1 ff ff       	callq  4021c0 <exit@plt>
  40704f:	8b 45 ec             	mov    -0x14(%rbp),%eax
  407052:	be 06 00 00 00       	mov    $0x6,%esi
  407057:	89 c7                	mov    %eax,%edi
  407059:	e8 f2 b0 ff ff       	callq  402150 <flock@plt>
  40705e:	85 c0                	test   %eax,%eax
  407060:	0f 89 b6 00 00 00    	jns    40711c <acquire_daemonlock+0x220>
  407066:	e8 15 ac ff ff       	callq  401c80 <__errno_location@plt>
  40706b:	8b 00                	mov    (%rax),%eax
  40706d:	89 45 e8             	mov    %eax,-0x18(%rbp)
  407070:	48 8b 05 d1 4c 20 00 	mov    0x204cd1(%rip),%rax        # 60bd48 <fp.5051>
  407077:	48 8d 55 e4          	lea    -0x1c(%rbp),%rdx
  40707b:	be 1d 92 40 00       	mov    $0x40921d,%esi
  407080:	48 89 c7             	mov    %rax,%rdi
  407083:	b8 00 00 00 00       	mov    $0x0,%eax
  407088:	e8 43 ac ff ff       	callq  401cd0 <__isoc99_fscanf@plt>
  40708d:	8b 45 e8             	mov    -0x18(%rbp),%eax
  407090:	89 c7                	mov    %eax,%edi
  407092:	e8 79 b1 ff ff       	callq  402210 <strerror@plt>
  407097:	48 89 c6             	mov    %rax,%rsi
  40709a:	8b 4d e4             	mov    -0x1c(%rbp),%ecx
  40709d:	48 8d 95 80 fb ff ff 	lea    -0x480(%rbp),%rdx
  4070a4:	48 8d 85 f0 fb ff ff 	lea    -0x410(%rbp),%rax
  4070ab:	49 89 f1             	mov    %rsi,%r9
  4070ae:	41 89 c8             	mov    %ecx,%r8d
  4070b1:	48 89 d1             	mov    %rdx,%rcx
  4070b4:	ba 20 92 40 00       	mov    $0x409220,%edx
  4070b9:	be e8 03 00 00       	mov    $0x3e8,%esi
  4070be:	48 89 c7             	mov    %rax,%rdi
  4070c1:	b8 00 00 00 00       	mov    $0x0,%eax
  4070c6:	e8 25 ad ff ff       	callq  401df0 <snprintf@plt>
  4070cb:	48 8b 15 b6 54 20 00 	mov    0x2054b6(%rip),%rdx        # 60c588 <ProgramName>
  4070d2:	48 8b 05 07 47 20 00 	mov    0x204707(%rip),%rax        # 60b7e0 <stderr@@GLIBC_2.2.5>
  4070d9:	48 8d 8d f0 fb ff ff 	lea    -0x410(%rbp),%rcx
  4070e0:	be 0a 92 40 00       	mov    $0x40920a,%esi
  4070e5:	48 89 c7             	mov    %rax,%rdi
  4070e8:	b8 00 00 00 00       	mov    $0x0,%eax
  4070ed:	e8 3e ae ff ff       	callq  401f30 <fprintf@plt>
  4070f2:	e8 49 ac ff ff       	callq  401d40 <getpid@plt>
  4070f7:	89 c6                	mov    %eax,%esi
  4070f9:	48 8d 85 f0 fb ff ff 	lea    -0x410(%rbp),%rax
  407100:	48 89 c1             	mov    %rax,%rcx
  407103:	ba 12 92 40 00       	mov    $0x409212,%edx
  407108:	bf 18 92 40 00       	mov    $0x409218,%edi
  40710d:	e8 c7 03 00 00       	callq  4074d9 <log_it>
  407112:	bf 01 00 00 00       	mov    $0x1,%edi
  407117:	e8 a4 b0 ff ff       	callq  4021c0 <exit@plt>
  40711c:	8b 55 ec             	mov    -0x14(%rbp),%edx
  40711f:	48 8d 85 f0 fb ff ff 	lea    -0x410(%rbp),%rax
  407126:	89 d1                	mov    %edx,%ecx
  407128:	ba 46 92 40 00       	mov    $0x409246,%edx
  40712d:	be e8 03 00 00       	mov    $0x3e8,%esi
  407132:	48 89 c7             	mov    %rax,%rdi
  407135:	b8 00 00 00 00       	mov    $0x0,%eax
  40713a:	e8 b1 ac ff ff       	callq  401df0 <snprintf@plt>
  40713f:	e8 fc ab ff ff       	callq  401d40 <getpid@plt>
  407144:	89 c6                	mov    %eax,%esi
  407146:	48 8d 85 f0 fb ff ff 	lea    -0x410(%rbp),%rax
  40714d:	48 89 c1             	mov    %rax,%rcx
  407150:	ba 56 92 40 00       	mov    $0x409256,%edx
  407155:	bf 18 92 40 00       	mov    $0x409218,%edi
  40715a:	e8 7a 03 00 00       	callq  4074d9 <log_it>
  40715f:	8b 45 ec             	mov    -0x14(%rbp),%eax
  407162:	ba 01 00 00 00       	mov    $0x1,%edx
  407167:	be 02 00 00 00       	mov    $0x2,%esi
  40716c:	89 c7                	mov    %eax,%edi
  40716e:	b8 00 00 00 00       	mov    $0x0,%eax
  407173:	e8 b8 ab ff ff       	callq  401d30 <fcntl@plt>
  407178:	48 8b 05 c9 4b 20 00 	mov    0x204bc9(%rip),%rax        # 60bd48 <fp.5051>
  40717f:	48 89 c7             	mov    %rax,%rdi
  407182:	e8 49 ac ff ff       	callq  401dd0 <rewind@plt>
  407187:	e8 b4 ab ff ff       	callq  401d40 <getpid@plt>
  40718c:	89 c2                	mov    %eax,%edx
  40718e:	48 8b 05 b3 4b 20 00 	mov    0x204bb3(%rip),%rax        # 60bd48 <fp.5051>
  407195:	be 5b 92 40 00       	mov    $0x40925b,%esi
  40719a:	48 89 c7             	mov    %rax,%rdi
  40719d:	b8 00 00 00 00       	mov    $0x0,%eax
  4071a2:	e8 89 ad ff ff       	callq  401f30 <fprintf@plt>
  4071a7:	48 8b 05 9a 4b 20 00 	mov    0x204b9a(%rip),%rax        # 60bd48 <fp.5051>
  4071ae:	48 89 c7             	mov    %rax,%rdi
  4071b1:	e8 2a ae ff ff       	callq  401fe0 <fflush@plt>
  4071b6:	48 8b 05 8b 4b 20 00 	mov    0x204b8b(%rip),%rax        # 60bd48 <fp.5051>
  4071bd:	48 89 c7             	mov    %rax,%rdi
  4071c0:	e8 8b ad ff ff       	callq  401f50 <ftell@plt>
  4071c5:	48 89 c3             	mov    %rax,%rbx
  4071c8:	48 8b 05 79 4b 20 00 	mov    0x204b79(%rip),%rax        # 60bd48 <fp.5051>
  4071cf:	48 89 c7             	mov    %rax,%rdi
  4071d2:	e8 b9 ad ff ff       	callq  401f90 <fileno@plt>
  4071d7:	48 89 de             	mov    %rbx,%rsi
  4071da:	89 c7                	mov    %eax,%edi
  4071dc:	e8 1f ac ff ff       	callq  401e00 <ftruncate@plt>
  4071e1:	48 81 c4 88 04 00 00 	add    $0x488,%rsp
  4071e8:	5b                   	pop    %rbx
  4071e9:	5d                   	pop    %rbp
  4071ea:	c3                   	retq   

00000000004071eb <get_char>:
  4071eb:	55                   	push   %rbp
  4071ec:	48 89 e5             	mov    %rsp,%rbp
  4071ef:	48 83 ec 20          	sub    $0x20,%rsp
  4071f3:	48 89 7d e8          	mov    %rdi,-0x18(%rbp)
  4071f7:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  4071fb:	48 89 c7             	mov    %rax,%rdi
  4071fe:	e8 ed ad ff ff       	callq  401ff0 <_IO_getc@plt>
  407203:	89 45 fc             	mov    %eax,-0x4(%rbp)
  407206:	83 7d fc 0a          	cmpl   $0xa,-0x4(%rbp)
  40720a:	75 0f                	jne    40721b <get_char+0x30>
  40720c:	8b 05 7e 53 20 00    	mov    0x20537e(%rip),%eax        # 60c590 <LineNumber>
  407212:	83 c0 01             	add    $0x1,%eax
  407215:	89 05 75 53 20 00    	mov    %eax,0x205375(%rip)        # 60c590 <LineNumber>
  40721b:	8b 45 fc             	mov    -0x4(%rbp),%eax
  40721e:	c9                   	leaveq 
  40721f:	c3                   	retq   

0000000000407220 <unget_char>:
  407220:	55                   	push   %rbp
  407221:	48 89 e5             	mov    %rsp,%rbp
  407224:	48 83 ec 10          	sub    $0x10,%rsp
  407228:	89 7d fc             	mov    %edi,-0x4(%rbp)
  40722b:	48 89 75 f0          	mov    %rsi,-0x10(%rbp)
  40722f:	48 8b 55 f0          	mov    -0x10(%rbp),%rdx
  407233:	8b 45 fc             	mov    -0x4(%rbp),%eax
  407236:	48 89 d6             	mov    %rdx,%rsi
  407239:	89 c7                	mov    %eax,%edi
  40723b:	e8 c0 ad ff ff       	callq  402000 <ungetc@plt>
  407240:	83 7d fc 0a          	cmpl   $0xa,-0x4(%rbp)
  407244:	75 0f                	jne    407255 <unget_char+0x35>
  407246:	8b 05 44 53 20 00    	mov    0x205344(%rip),%eax        # 60c590 <LineNumber>
  40724c:	83 e8 01             	sub    $0x1,%eax
  40724f:	89 05 3b 53 20 00    	mov    %eax,0x20533b(%rip)        # 60c590 <LineNumber>
  407255:	90                   	nop
  407256:	c9                   	leaveq 
  407257:	c3                   	retq   

0000000000407258 <get_string>:
  407258:	55                   	push   %rbp
  407259:	48 89 e5             	mov    %rsp,%rbp
  40725c:	48 83 ec 30          	sub    $0x30,%rsp
  407260:	48 89 7d e8          	mov    %rdi,-0x18(%rbp)
  407264:	89 75 e4             	mov    %esi,-0x1c(%rbp)
  407267:	48 89 55 d8          	mov    %rdx,-0x28(%rbp)
  40726b:	48 89 4d d0          	mov    %rcx,-0x30(%rbp)
  40726f:	eb 1b                	jmp    40728c <get_string+0x34>
  407271:	83 7d e4 01          	cmpl   $0x1,-0x1c(%rbp)
  407275:	7e 15                	jle    40728c <get_string+0x34>
  407277:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  40727b:	48 8d 50 01          	lea    0x1(%rax),%rdx
  40727f:	48 89 55 e8          	mov    %rdx,-0x18(%rbp)
  407283:	8b 55 fc             	mov    -0x4(%rbp),%edx
  407286:	88 10                	mov    %dl,(%rax)
  407288:	83 6d e4 01          	subl   $0x1,-0x1c(%rbp)
  40728c:	48 8b 45 d8          	mov    -0x28(%rbp),%rax
  407290:	48 89 c7             	mov    %rax,%rdi
  407293:	e8 53 ff ff ff       	callq  4071eb <get_char>
  407298:	89 45 fc             	mov    %eax,-0x4(%rbp)
  40729b:	83 7d fc ff          	cmpl   $0xffffffff,-0x4(%rbp)
  40729f:	74 16                	je     4072b7 <get_string+0x5f>
  4072a1:	8b 55 fc             	mov    -0x4(%rbp),%edx
  4072a4:	48 8b 45 d0          	mov    -0x30(%rbp),%rax
  4072a8:	89 d6                	mov    %edx,%esi
  4072aa:	48 89 c7             	mov    %rax,%rdi
  4072ad:	e8 0e ab ff ff       	callq  401dc0 <strchr@plt>
  4072b2:	48 85 c0             	test   %rax,%rax
  4072b5:	74 ba                	je     407271 <get_string+0x19>
  4072b7:	83 7d e4 00          	cmpl   $0x0,-0x1c(%rbp)
  4072bb:	7e 07                	jle    4072c4 <get_string+0x6c>
  4072bd:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  4072c1:	c6 00 00             	movb   $0x0,(%rax)
  4072c4:	8b 45 fc             	mov    -0x4(%rbp),%eax
  4072c7:	c9                   	leaveq 
  4072c8:	c3                   	retq   

00000000004072c9 <skip_comments>:
  4072c9:	55                   	push   %rbp
  4072ca:	48 89 e5             	mov    %rsp,%rbp
  4072cd:	48 83 ec 20          	sub    $0x20,%rsp
  4072d1:	48 89 7d e8          	mov    %rdi,-0x18(%rbp)
  4072d5:	eb 4a                	jmp    407321 <skip_comments+0x58>
  4072d7:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  4072db:	48 89 c7             	mov    %rax,%rdi
  4072de:	e8 08 ff ff ff       	callq  4071eb <get_char>
  4072e3:	89 45 fc             	mov    %eax,-0x4(%rbp)
  4072e6:	83 7d fc 20          	cmpl   $0x20,-0x4(%rbp)
  4072ea:	74 eb                	je     4072d7 <skip_comments+0xe>
  4072ec:	83 7d fc 09          	cmpl   $0x9,-0x4(%rbp)
  4072f0:	74 e5                	je     4072d7 <skip_comments+0xe>
  4072f2:	83 7d fc ff          	cmpl   $0xffffffff,-0x4(%rbp)
  4072f6:	74 40                	je     407338 <skip_comments+0x6f>
  4072f8:	83 7d fc 0a          	cmpl   $0xa,-0x4(%rbp)
  4072fc:	74 17                	je     407315 <skip_comments+0x4c>
  4072fe:	83 7d fc 23          	cmpl   $0x23,-0x4(%rbp)
  407302:	75 37                	jne    40733b <skip_comments+0x72>
  407304:	eb 0f                	jmp    407315 <skip_comments+0x4c>
  407306:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  40730a:	48 89 c7             	mov    %rax,%rdi
  40730d:	e8 d9 fe ff ff       	callq  4071eb <get_char>
  407312:	89 45 fc             	mov    %eax,-0x4(%rbp)
  407315:	83 7d fc 0a          	cmpl   $0xa,-0x4(%rbp)
  407319:	74 06                	je     407321 <skip_comments+0x58>
  40731b:	83 7d fc ff          	cmpl   $0xffffffff,-0x4(%rbp)
  40731f:	75 e5                	jne    407306 <skip_comments+0x3d>
  407321:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  407325:	48 89 c7             	mov    %rax,%rdi
  407328:	e8 be fe ff ff       	callq  4071eb <get_char>
  40732d:	89 45 fc             	mov    %eax,-0x4(%rbp)
  407330:	83 7d fc ff          	cmpl   $0xffffffff,-0x4(%rbp)
  407334:	75 b0                	jne    4072e6 <skip_comments+0x1d>
  407336:	eb 04                	jmp    40733c <skip_comments+0x73>
  407338:	90                   	nop
  407339:	eb 01                	jmp    40733c <skip_comments+0x73>
  40733b:	90                   	nop
  40733c:	83 7d fc ff          	cmpl   $0xffffffff,-0x4(%rbp)
  407340:	74 11                	je     407353 <skip_comments+0x8a>
  407342:	48 8b 55 e8          	mov    -0x18(%rbp),%rdx
  407346:	8b 45 fc             	mov    -0x4(%rbp),%eax
  407349:	48 89 d6             	mov    %rdx,%rsi
  40734c:	89 c7                	mov    %eax,%edi
  40734e:	e8 cd fe ff ff       	callq  407220 <unget_char>
  407353:	90                   	nop
  407354:	c9                   	leaveq 
  407355:	c3                   	retq   

0000000000407356 <in_file>:
  407356:	55                   	push   %rbp
  407357:	48 89 e5             	mov    %rsp,%rbp
  40735a:	48 81 ec 00 04 00 00 	sub    $0x400,%rsp
  407361:	48 89 bd 08 fc ff ff 	mov    %rdi,-0x3f8(%rbp)
  407368:	48 89 b5 00 fc ff ff 	mov    %rsi,-0x400(%rbp)
  40736f:	48 8b 85 00 fc ff ff 	mov    -0x400(%rbp),%rax
  407376:	48 89 c7             	mov    %rax,%rdi
  407379:	e8 52 aa ff ff       	callq  401dd0 <rewind@plt>
  40737e:	eb 4a                	jmp    4073ca <in_file+0x74>
  407380:	0f b6 85 10 fc ff ff 	movzbl -0x3f0(%rbp),%eax
  407387:	84 c0                	test   %al,%al
  407389:	74 1b                	je     4073a6 <in_file+0x50>
  40738b:	48 8d 85 10 fc ff ff 	lea    -0x3f0(%rbp),%rax
  407392:	48 89 c7             	mov    %rax,%rdi
  407395:	e8 d6 a9 ff ff       	callq  401d70 <strlen@plt>
  40739a:	48 83 e8 01          	sub    $0x1,%rax
  40739e:	c6 84 05 10 fc ff ff 	movb   $0x0,-0x3f0(%rbp,%rax,1)
  4073a5:	00 
  4073a6:	48 8b 95 08 fc ff ff 	mov    -0x3f8(%rbp),%rdx
  4073ad:	48 8d 85 10 fc ff ff 	lea    -0x3f0(%rbp),%rax
  4073b4:	48 89 d6             	mov    %rdx,%rsi
  4073b7:	48 89 c7             	mov    %rax,%rdi
  4073ba:	e8 41 ab ff ff       	callq  401f00 <strcmp@plt>
  4073bf:	85 c0                	test   %eax,%eax
  4073c1:	75 07                	jne    4073ca <in_file+0x74>
  4073c3:	b8 01 00 00 00       	mov    $0x1,%eax
  4073c8:	eb 25                	jmp    4073ef <in_file+0x99>
  4073ca:	48 8b 95 00 fc ff ff 	mov    -0x400(%rbp),%rdx
  4073d1:	48 8d 85 10 fc ff ff 	lea    -0x3f0(%rbp),%rax
  4073d8:	be e8 03 00 00       	mov    $0x3e8,%esi
  4073dd:	48 89 c7             	mov    %rax,%rdi
  4073e0:	e8 eb aa ff ff       	callq  401ed0 <fgets@plt>
  4073e5:	48 85 c0             	test   %rax,%rax
  4073e8:	75 96                	jne    407380 <in_file+0x2a>
  4073ea:	b8 00 00 00 00       	mov    $0x0,%eax
  4073ef:	c9                   	leaveq 
  4073f0:	c3                   	retq   

00000000004073f1 <allowed>:
  4073f1:	55                   	push   %rbp
  4073f2:	48 89 e5             	mov    %rsp,%rbp
  4073f5:	48 83 ec 20          	sub    $0x20,%rsp
  4073f9:	48 89 7d e8          	mov    %rdi,-0x18(%rbp)
  4073fd:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  407401:	be 5f 92 40 00       	mov    $0x40925f,%esi
  407406:	48 89 c7             	mov    %rax,%rdi
  407409:	e8 f2 aa ff ff       	callq  401f00 <strcmp@plt>
  40740e:	85 c0                	test   %eax,%eax
  407410:	75 0a                	jne    40741c <allowed+0x2b>
  407412:	b8 01 00 00 00       	mov    $0x1,%eax
  407417:	e9 bb 00 00 00       	jmpq   4074d7 <allowed+0xe6>
  40741c:	c7 45 fc 00 00 00 00 	movl   $0x0,-0x4(%rbp)
  407423:	8b 05 27 49 20 00    	mov    0x204927(%rip),%eax        # 60bd50 <init.5099>
  407429:	85 c0                	test   %eax,%eax
  40742b:	75 36                	jne    407463 <allowed+0x72>
  40742d:	c7 05 19 49 20 00 01 	movl   $0x1,0x204919(%rip)        # 60bd50 <init.5099>
  407434:	00 00 00 
  407437:	be 64 92 40 00       	mov    $0x409264,%esi
  40743c:	bf 66 92 40 00       	mov    $0x409266,%edi
  407441:	e8 9a ac ff ff       	callq  4020e0 <fopen@plt>
  407446:	48 89 05 0b 49 20 00 	mov    %rax,0x20490b(%rip)        # 60bd58 <allow.5100>
  40744d:	be 64 92 40 00       	mov    $0x409264,%esi
  407452:	bf 76 92 40 00       	mov    $0x409276,%edi
  407457:	e8 84 ac ff ff       	callq  4020e0 <fopen@plt>
  40745c:	48 89 05 fd 48 20 00 	mov    %rax,0x2048fd(%rip)        # 60bd60 <deny.5101>
  407463:	48 8b 05 ee 48 20 00 	mov    0x2048ee(%rip),%rax        # 60bd58 <allow.5100>
  40746a:	48 85 c0             	test   %rax,%rax
  40746d:	74 20                	je     40748f <allowed+0x9e>
  40746f:	48 8b 15 e2 48 20 00 	mov    0x2048e2(%rip),%rdx        # 60bd58 <allow.5100>
  407476:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  40747a:	48 89 d6             	mov    %rdx,%rsi
  40747d:	48 89 c7             	mov    %rax,%rdi
  407480:	b8 00 00 00 00       	mov    $0x0,%eax
  407485:	e8 cc fe ff ff       	callq  407356 <in_file>
  40748a:	89 45 fc             	mov    %eax,-0x4(%rbp)
  40748d:	eb 07                	jmp    407496 <allowed+0xa5>
  40748f:	c7 45 fc 01 00 00 00 	movl   $0x1,-0x4(%rbp)
  407496:	48 8b 05 c3 48 20 00 	mov    0x2048c3(%rip),%rax        # 60bd60 <deny.5101>
  40749d:	48 85 c0             	test   %rax,%rax
  4074a0:	74 32                	je     4074d4 <allowed+0xe3>
  4074a2:	48 8b 05 af 48 20 00 	mov    0x2048af(%rip),%rax        # 60bd58 <allow.5100>
  4074a9:	48 85 c0             	test   %rax,%rax
  4074ac:	75 26                	jne    4074d4 <allowed+0xe3>
  4074ae:	48 8b 15 ab 48 20 00 	mov    0x2048ab(%rip),%rdx        # 60bd60 <deny.5101>
  4074b5:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  4074b9:	48 89 d6             	mov    %rdx,%rsi
  4074bc:	48 89 c7             	mov    %rax,%rdi
  4074bf:	b8 00 00 00 00       	mov    $0x0,%eax
  4074c4:	e8 8d fe ff ff       	callq  407356 <in_file>
  4074c9:	85 c0                	test   %eax,%eax
  4074cb:	0f 94 c0             	sete   %al
  4074ce:	0f b6 c0             	movzbl %al,%eax
  4074d1:	89 45 fc             	mov    %eax,-0x4(%rbp)
  4074d4:	8b 45 fc             	mov    -0x4(%rbp),%eax
  4074d7:	c9                   	leaveq 
  4074d8:	c3                   	retq   

00000000004074d9 <log_it>:
  4074d9:	55                   	push   %rbp
  4074da:	48 89 e5             	mov    %rsp,%rbp
  4074dd:	48 83 ec 20          	sub    $0x20,%rsp
  4074e1:	48 89 7d f8          	mov    %rdi,-0x8(%rbp)
  4074e5:	89 75 f4             	mov    %esi,-0xc(%rbp)
  4074e8:	48 89 55 e8          	mov    %rdx,-0x18(%rbp)
  4074ec:	48 89 4d e0          	mov    %rcx,-0x20(%rbp)
  4074f0:	48 8b 05 91 50 20 00 	mov    0x205091(%rip),%rax        # 60c588 <ProgramName>
  4074f7:	ba 48 00 00 00       	mov    $0x48,%edx
  4074fc:	be 01 00 00 00       	mov    $0x1,%esi
  407501:	48 89 c7             	mov    %rax,%rdi
  407504:	e8 77 ac ff ff       	callq  402180 <openlog@plt>
  407509:	48 8b 4d e0          	mov    -0x20(%rbp),%rcx
  40750d:	48 8b 55 e8          	mov    -0x18(%rbp),%rdx
  407511:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  407515:	49 89 c8             	mov    %rcx,%r8
  407518:	48 89 d1             	mov    %rdx,%rcx
  40751b:	48 89 c2             	mov    %rax,%rdx
  40751e:	be 85 92 40 00       	mov    $0x409285,%esi
  407523:	bf 06 00 00 00       	mov    $0x6,%edi
  407528:	b8 00 00 00 00       	mov    $0x0,%eax
  40752d:	e8 de aa ff ff       	callq  402010 <syslog@plt>
  407532:	e8 19 a7 ff ff       	callq  401c50 <closelog@plt>
  407537:	90                   	nop
  407538:	c9                   	leaveq 
  407539:	c3                   	retq   

000000000040753a <log_close>:
  40753a:	55                   	push   %rbp
  40753b:	48 89 e5             	mov    %rsp,%rbp
  40753e:	e8 0d a7 ff ff       	callq  401c50 <closelog@plt>
  407543:	90                   	nop
  407544:	5d                   	pop    %rbp
  407545:	c3                   	retq   

0000000000407546 <first_word>:
  407546:	55                   	push   %rbp
  407547:	48 89 e5             	mov    %rsp,%rbp
  40754a:	41 56                	push   %r14
  40754c:	41 55                	push   %r13
  40754e:	41 54                	push   %r12
  407550:	53                   	push   %rbx
  407551:	48 89 fb             	mov    %rdi,%rbx
  407554:	49 89 f6             	mov    %rsi,%r14
  407557:	8b 05 0b 48 20 00    	mov    0x20480b(%rip),%eax        # 60bd68 <retsel.5116>
  40755d:	ba 01 00 00 00       	mov    $0x1,%edx
  407562:	29 c2                	sub    %eax,%edx
  407564:	89 d0                	mov    %edx,%eax
  407566:	89 05 fc 47 20 00    	mov    %eax,0x2047fc(%rip)        # 60bd68 <retsel.5116>
  40756c:	8b 05 f6 47 20 00    	mov    0x2047f6(%rip),%eax        # 60bd68 <retsel.5116>
  407572:	48 98                	cltq   
  407574:	48 69 c0 e9 03 00 00 	imul   $0x3e9,%rax,%rax
  40757b:	4c 8d a8 80 bd 60 00 	lea    0x60bd80(%rax),%r13
  407582:	4d 89 ec             	mov    %r13,%r12
  407585:	eb 04                	jmp    40758b <first_word+0x45>
  407587:	48 83 c3 01          	add    $0x1,%rbx
  40758b:	0f b6 03             	movzbl (%rbx),%eax
  40758e:	84 c0                	test   %al,%al
  407590:	74 2a                	je     4075bc <first_word+0x76>
  407592:	0f b6 03             	movzbl (%rbx),%eax
  407595:	0f be c0             	movsbl %al,%eax
  407598:	89 c6                	mov    %eax,%esi
  40759a:	4c 89 f7             	mov    %r14,%rdi
  40759d:	e8 1e a8 ff ff       	callq  401dc0 <strchr@plt>
  4075a2:	48 85 c0             	test   %rax,%rax
  4075a5:	75 e0                	jne    407587 <first_word+0x41>
  4075a7:	eb 13                	jmp    4075bc <first_word+0x76>
  4075a9:	4c 89 e0             	mov    %r12,%rax
  4075ac:	4c 8d 60 01          	lea    0x1(%rax),%r12
  4075b0:	48 89 da             	mov    %rbx,%rdx
  4075b3:	48 8d 5a 01          	lea    0x1(%rdx),%rbx
  4075b7:	0f b6 12             	movzbl (%rdx),%edx
  4075ba:	88 10                	mov    %dl,(%rax)
  4075bc:	0f b6 03             	movzbl (%rbx),%eax
  4075bf:	84 c0                	test   %al,%al
  4075c1:	74 21                	je     4075e4 <first_word+0x9e>
  4075c3:	0f b6 03             	movzbl (%rbx),%eax
  4075c6:	0f be c0             	movsbl %al,%eax
  4075c9:	89 c6                	mov    %eax,%esi
  4075cb:	4c 89 f7             	mov    %r14,%rdi
  4075ce:	e8 ed a7 ff ff       	callq  401dc0 <strchr@plt>
  4075d3:	48 85 c0             	test   %rax,%rax
  4075d6:	75 0c                	jne    4075e4 <first_word+0x9e>
  4075d8:	49 8d 85 e8 03 00 00 	lea    0x3e8(%r13),%rax
  4075df:	4c 39 e0             	cmp    %r12,%rax
  4075e2:	77 c5                	ja     4075a9 <first_word+0x63>
  4075e4:	41 c6 04 24 00       	movb   $0x0,(%r12)
  4075e9:	4c 89 e8             	mov    %r13,%rax
  4075ec:	5b                   	pop    %rbx
  4075ed:	41 5c                	pop    %r12
  4075ef:	41 5d                	pop    %r13
  4075f1:	41 5e                	pop    %r14
  4075f3:	5d                   	pop    %rbp
  4075f4:	c3                   	retq   

00000000004075f5 <mkprint>:
  4075f5:	55                   	push   %rbp
  4075f6:	48 89 e5             	mov    %rsp,%rbp
  4075f9:	41 56                	push   %r14
  4075fb:	41 55                	push   %r13
  4075fd:	41 54                	push   %r12
  4075ff:	53                   	push   %rbx
  407600:	48 89 fb             	mov    %rdi,%rbx
  407603:	49 89 f6             	mov    %rsi,%r14
  407606:	41 89 d5             	mov    %edx,%r13d
  407609:	eb 7c                	jmp    407687 <mkprint+0x92>
  40760b:	4c 89 f0             	mov    %r14,%rax
  40760e:	4c 8d 70 01          	lea    0x1(%rax),%r14
  407612:	44 0f b6 20          	movzbl (%rax),%r12d
  407616:	41 80 fc 1f          	cmp    $0x1f,%r12b
  40761a:	77 1a                	ja     407636 <mkprint+0x41>
  40761c:	48 89 d8             	mov    %rbx,%rax
  40761f:	48 8d 58 01          	lea    0x1(%rax),%rbx
  407623:	c6 00 5e             	movb   $0x5e,(%rax)
  407626:	48 89 d8             	mov    %rbx,%rax
  407629:	48 8d 58 01          	lea    0x1(%rax),%rbx
  40762d:	41 8d 54 24 40       	lea    0x40(%r12),%edx
  407632:	88 10                	mov    %dl,(%rax)
  407634:	eb 51                	jmp    407687 <mkprint+0x92>
  407636:	41 80 fc 7e          	cmp    $0x7e,%r12b
  40763a:	77 0e                	ja     40764a <mkprint+0x55>
  40763c:	48 89 d8             	mov    %rbx,%rax
  40763f:	48 8d 58 01          	lea    0x1(%rax),%rbx
  407643:	44 89 e2             	mov    %r12d,%edx
  407646:	88 10                	mov    %dl,(%rax)
  407648:	eb 3d                	jmp    407687 <mkprint+0x92>
  40764a:	41 80 fc 7f          	cmp    $0x7f,%r12b
  40764e:	75 16                	jne    407666 <mkprint+0x71>
  407650:	48 89 d8             	mov    %rbx,%rax
  407653:	48 8d 58 01          	lea    0x1(%rax),%rbx
  407657:	c6 00 5e             	movb   $0x5e,(%rax)
  40765a:	48 89 d8             	mov    %rbx,%rax
  40765d:	48 8d 58 01          	lea    0x1(%rax),%rbx
  407661:	c6 00 3f             	movb   $0x3f,(%rax)
  407664:	eb 21                	jmp    407687 <mkprint+0x92>
  407666:	41 0f b6 c4          	movzbl %r12b,%eax
  40766a:	89 c1                	mov    %eax,%ecx
  40766c:	ba 92 92 40 00       	mov    $0x409292,%edx
  407671:	be 05 00 00 00       	mov    $0x5,%esi
  407676:	48 89 df             	mov    %rbx,%rdi
  407679:	b8 00 00 00 00       	mov    $0x0,%eax
  40767e:	e8 6d a7 ff ff       	callq  401df0 <snprintf@plt>
  407683:	48 83 c3 04          	add    $0x4,%rbx
  407687:	44 89 e8             	mov    %r13d,%eax
  40768a:	44 8d 68 ff          	lea    -0x1(%rax),%r13d
  40768e:	85 c0                	test   %eax,%eax
  407690:	0f 8f 75 ff ff ff    	jg     40760b <mkprint+0x16>
  407696:	c6 03 00             	movb   $0x0,(%rbx)
  407699:	90                   	nop
  40769a:	5b                   	pop    %rbx
  40769b:	41 5c                	pop    %r12
  40769d:	41 5d                	pop    %r13
  40769f:	41 5e                	pop    %r14
  4076a1:	5d                   	pop    %rbp
  4076a2:	c3                   	retq   

00000000004076a3 <mkprints>:
  4076a3:	55                   	push   %rbp
  4076a4:	48 89 e5             	mov    %rsp,%rbp
  4076a7:	41 55                	push   %r13
  4076a9:	41 54                	push   %r12
  4076ab:	53                   	push   %rbx
  4076ac:	48 83 ec 08          	sub    $0x8,%rsp
  4076b0:	49 89 fd             	mov    %rdi,%r13
  4076b3:	41 89 f4             	mov    %esi,%r12d
  4076b6:	42 8d 04 a5 00 00 00 	lea    0x0(,%r12,4),%eax
  4076bd:	00 
  4076be:	83 c0 01             	add    $0x1,%eax
  4076c1:	89 c0                	mov    %eax,%eax
  4076c3:	48 89 c7             	mov    %rax,%rdi
  4076c6:	e8 05 a9 ff ff       	callq  401fd0 <malloc@plt>
  4076cb:	48 89 c3             	mov    %rax,%rbx
  4076ce:	48 85 db             	test   %rbx,%rbx
  4076d1:	74 13                	je     4076e6 <mkprints+0x43>
  4076d3:	44 89 e2             	mov    %r12d,%edx
  4076d6:	4c 89 ee             	mov    %r13,%rsi
  4076d9:	48 89 df             	mov    %rbx,%rdi
  4076dc:	b8 00 00 00 00       	mov    $0x0,%eax
  4076e1:	e8 0f ff ff ff       	callq  4075f5 <mkprint>
  4076e6:	48 89 d8             	mov    %rbx,%rax
  4076e9:	48 83 c4 08          	add    $0x8,%rsp
  4076ed:	5b                   	pop    %rbx
  4076ee:	41 5c                	pop    %r12
  4076f0:	41 5d                	pop    %r13
  4076f2:	5d                   	pop    %rbp
  4076f3:	c3                   	retq   

00000000004076f4 <swap_uids>:
  4076f4:	55                   	push   %rbp
  4076f5:	48 89 e5             	mov    %rsp,%rbp
  4076f8:	e8 23 a7 ff ff       	callq  401e20 <geteuid@plt>
  4076fd:	89 05 3d 46 20 00    	mov    %eax,0x20463d(%rip)        # 60bd40 <save_euid>
  407703:	e8 48 a9 ff ff       	callq  402050 <getegid@plt>
  407708:	89 05 36 46 20 00    	mov    %eax,0x204636(%rip)        # 60bd44 <save_egid>
  40770e:	e8 8d a8 ff ff       	callq  401fa0 <getgid@plt>
  407713:	89 c7                	mov    %eax,%edi
  407715:	e8 46 ab ff ff       	callq  402260 <setegid@plt>
  40771a:	85 c0                	test   %eax,%eax
  40771c:	75 10                	jne    40772e <swap_uids+0x3a>
  40771e:	e8 7d a6 ff ff       	callq  401da0 <getuid@plt>
  407723:	89 c7                	mov    %eax,%edi
  407725:	e8 86 ab ff ff       	callq  4022b0 <seteuid@plt>
  40772a:	85 c0                	test   %eax,%eax
  40772c:	74 07                	je     407735 <swap_uids+0x41>
  40772e:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  407733:	eb 05                	jmp    40773a <swap_uids+0x46>
  407735:	b8 00 00 00 00       	mov    $0x0,%eax
  40773a:	5d                   	pop    %rbp
  40773b:	c3                   	retq   

000000000040773c <swap_uids_back>:
  40773c:	55                   	push   %rbp
  40773d:	48 89 e5             	mov    %rsp,%rbp
  407740:	8b 05 fe 45 20 00    	mov    0x2045fe(%rip),%eax        # 60bd44 <save_egid>
  407746:	89 c7                	mov    %eax,%edi
  407748:	e8 13 ab ff ff       	callq  402260 <setegid@plt>
  40774d:	85 c0                	test   %eax,%eax
  40774f:	75 11                	jne    407762 <swap_uids_back+0x26>
  407751:	8b 05 e9 45 20 00    	mov    0x2045e9(%rip),%eax        # 60bd40 <save_euid>
  407757:	89 c7                	mov    %eax,%edi
  407759:	e8 52 ab ff ff       	callq  4022b0 <seteuid@plt>
  40775e:	85 c0                	test   %eax,%eax
  407760:	74 07                	je     407769 <swap_uids_back+0x2d>
  407762:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  407767:	eb 05                	jmp    40776e <swap_uids_back+0x32>
  407769:	b8 00 00 00 00       	mov    $0x0,%eax
  40776e:	5d                   	pop    %rbp
  40776f:	c3                   	retq   

0000000000407770 <get_gmtoff>:
  407770:	55                   	push   %rbp
  407771:	48 89 e5             	mov    %rsp,%rbp
  407774:	48 83 ec 50          	sub    $0x50,%rsp
  407778:	48 89 7d b8          	mov    %rdi,-0x48(%rbp)
  40777c:	48 89 75 b0          	mov    %rsi,-0x50(%rbp)
  407780:	48 8b 45 b8          	mov    -0x48(%rbp),%rax
  407784:	48 89 c7             	mov    %rax,%rdi
  407787:	e8 94 a9 ff ff       	callq  402120 <gmtime@plt>
  40778c:	48 8b 10             	mov    (%rax),%rdx
  40778f:	48 89 55 c0          	mov    %rdx,-0x40(%rbp)
  407793:	48 8b 50 08          	mov    0x8(%rax),%rdx
  407797:	48 89 55 c8          	mov    %rdx,-0x38(%rbp)
  40779b:	48 8b 50 10          	mov    0x10(%rax),%rdx
  40779f:	48 89 55 d0          	mov    %rdx,-0x30(%rbp)
  4077a3:	48 8b 50 18          	mov    0x18(%rax),%rdx
  4077a7:	48 89 55 d8          	mov    %rdx,-0x28(%rbp)
  4077ab:	48 8b 50 20          	mov    0x20(%rax),%rdx
  4077af:	48 89 55 e0          	mov    %rdx,-0x20(%rbp)
  4077b3:	48 8b 50 28          	mov    0x28(%rax),%rdx
  4077b7:	48 89 55 e8          	mov    %rdx,-0x18(%rbp)
  4077bb:	48 8b 40 30          	mov    0x30(%rax),%rax
  4077bf:	48 89 45 f0          	mov    %rax,-0x10(%rbp)
  4077c3:	48 83 7d b0 00       	cmpq   $0x0,-0x50(%rbp)
  4077c8:	75 10                	jne    4077da <get_gmtoff+0x6a>
  4077ca:	48 8b 45 b8          	mov    -0x48(%rbp),%rax
  4077ce:	48 89 c7             	mov    %rax,%rdi
  4077d1:	e8 8a a4 ff ff       	callq  401c60 <localtime@plt>
  4077d6:	48 89 45 b0          	mov    %rax,-0x50(%rbp)
  4077da:	48 8b 45 b0          	mov    -0x50(%rbp),%rax
  4077de:	8b 10                	mov    (%rax),%edx
  4077e0:	8b 45 c0             	mov    -0x40(%rbp),%eax
  4077e3:	89 d1                	mov    %edx,%ecx
  4077e5:	29 c1                	sub    %eax,%ecx
  4077e7:	48 8b 45 b0          	mov    -0x50(%rbp),%rax
  4077eb:	8b 50 04             	mov    0x4(%rax),%edx
  4077ee:	8b 45 c4             	mov    -0x3c(%rbp),%eax
  4077f1:	29 c2                	sub    %eax,%edx
  4077f3:	89 d0                	mov    %edx,%eax
  4077f5:	c1 e0 02             	shl    $0x2,%eax
  4077f8:	89 c2                	mov    %eax,%edx
  4077fa:	c1 e2 04             	shl    $0x4,%edx
  4077fd:	29 c2                	sub    %eax,%edx
  4077ff:	89 d0                	mov    %edx,%eax
  407801:	01 c1                	add    %eax,%ecx
  407803:	48 8b 45 b0          	mov    -0x50(%rbp),%rax
  407807:	8b 50 08             	mov    0x8(%rax),%edx
  40780a:	8b 45 c8             	mov    -0x38(%rbp),%eax
  40780d:	29 c2                	sub    %eax,%edx
  40780f:	89 d0                	mov    %edx,%eax
  407811:	69 c0 10 0e 00 00    	imul   $0xe10,%eax,%eax
  407817:	01 c8                	add    %ecx,%eax
  407819:	48 98                	cltq   
  40781b:	48 89 45 f8          	mov    %rax,-0x8(%rbp)
  40781f:	48 8b 45 b0          	mov    -0x50(%rbp),%rax
  407823:	8b 50 14             	mov    0x14(%rax),%edx
  407826:	8b 45 d4             	mov    -0x2c(%rbp),%eax
  407829:	39 c2                	cmp    %eax,%edx
  40782b:	7d 0a                	jge    407837 <get_gmtoff+0xc7>
  40782d:	48 81 6d f8 80 51 01 	subq   $0x15180,-0x8(%rbp)
  407834:	00 
  407835:	eb 46                	jmp    40787d <get_gmtoff+0x10d>
  407837:	48 8b 45 b0          	mov    -0x50(%rbp),%rax
  40783b:	8b 50 14             	mov    0x14(%rax),%edx
  40783e:	8b 45 d4             	mov    -0x2c(%rbp),%eax
  407841:	39 c2                	cmp    %eax,%edx
  407843:	7e 0a                	jle    40784f <get_gmtoff+0xdf>
  407845:	48 81 45 f8 80 51 01 	addq   $0x15180,-0x8(%rbp)
  40784c:	00 
  40784d:	eb 2e                	jmp    40787d <get_gmtoff+0x10d>
  40784f:	48 8b 45 b0          	mov    -0x50(%rbp),%rax
  407853:	8b 50 1c             	mov    0x1c(%rax),%edx
  407856:	8b 45 dc             	mov    -0x24(%rbp),%eax
  407859:	39 c2                	cmp    %eax,%edx
  40785b:	7d 0a                	jge    407867 <get_gmtoff+0xf7>
  40785d:	48 81 6d f8 80 51 01 	subq   $0x15180,-0x8(%rbp)
  407864:	00 
  407865:	eb 16                	jmp    40787d <get_gmtoff+0x10d>
  407867:	48 8b 45 b0          	mov    -0x50(%rbp),%rax
  40786b:	8b 50 1c             	mov    0x1c(%rax),%edx
  40786e:	8b 45 dc             	mov    -0x24(%rbp),%eax
  407871:	39 c2                	cmp    %eax,%edx
  407873:	7e 08                	jle    40787d <get_gmtoff+0x10d>
  407875:	48 81 45 f8 80 51 01 	addq   $0x15180,-0x8(%rbp)
  40787c:	00 
  40787d:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  407881:	c9                   	leaveq 
  407882:	c3                   	retq   

0000000000407883 <env_init>:
  407883:	55                   	push   %rbp
  407884:	48 89 e5             	mov    %rsp,%rbp
  407887:	53                   	push   %rbx
  407888:	48 83 ec 08          	sub    $0x8,%rsp
  40788c:	bf 08 00 00 00       	mov    $0x8,%edi
  407891:	e8 3a a7 ff ff       	callq  401fd0 <malloc@plt>
  407896:	48 89 c3             	mov    %rax,%rbx
  407899:	48 85 db             	test   %rbx,%rbx
  40789c:	74 07                	je     4078a5 <env_init+0x22>
  40789e:	48 c7 03 00 00 00 00 	movq   $0x0,(%rbx)
  4078a5:	48 89 d8             	mov    %rbx,%rax
  4078a8:	48 83 c4 08          	add    $0x8,%rsp
  4078ac:	5b                   	pop    %rbx
  4078ad:	5d                   	pop    %rbp
  4078ae:	c3                   	retq   

00000000004078af <env_free>:
  4078af:	55                   	push   %rbp
  4078b0:	48 89 e5             	mov    %rsp,%rbp
  4078b3:	48 83 ec 20          	sub    $0x20,%rsp
  4078b7:	48 89 7d e8          	mov    %rdi,-0x18(%rbp)
  4078bb:	48 83 7d e8 00       	cmpq   $0x0,-0x18(%rbp)
  4078c0:	74 38                	je     4078fa <env_free+0x4b>
  4078c2:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  4078c6:	48 89 45 f8          	mov    %rax,-0x8(%rbp)
  4078ca:	eb 14                	jmp    4078e0 <env_free+0x31>
  4078cc:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  4078d0:	48 8b 00             	mov    (%rax),%rax
  4078d3:	48 89 c7             	mov    %rax,%rdi
  4078d6:	e8 55 a3 ff ff       	callq  401c30 <free@plt>
  4078db:	48 83 45 f8 08       	addq   $0x8,-0x8(%rbp)
  4078e0:	48 8b 45 f8          	mov    -0x8(%rbp),%rax
  4078e4:	48 8b 00             	mov    (%rax),%rax
  4078e7:	48 85 c0             	test   %rax,%rax
  4078ea:	75 e0                	jne    4078cc <env_free+0x1d>
  4078ec:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  4078f0:	48 89 c7             	mov    %rax,%rdi
  4078f3:	e8 38 a3 ff ff       	callq  401c30 <free@plt>
  4078f8:	eb 01                	jmp    4078fb <env_free+0x4c>
  4078fa:	90                   	nop
  4078fb:	c9                   	leaveq 
  4078fc:	c3                   	retq   

00000000004078fd <env_copy>:
  4078fd:	55                   	push   %rbp
  4078fe:	48 89 e5             	mov    %rsp,%rbp
  407901:	41 57                	push   %r15
  407903:	41 56                	push   %r14
  407905:	41 55                	push   %r13
  407907:	41 54                	push   %r12
  407909:	53                   	push   %rbx
  40790a:	48 83 ec 08          	sub    $0x8,%rsp
  40790e:	49 89 ff             	mov    %rdi,%r15
  407911:	41 bd 00 00 00 00    	mov    $0x0,%r13d
  407917:	eb 04                	jmp    40791d <env_copy+0x20>
  407919:	41 83 c5 01          	add    $0x1,%r13d
  40791d:	49 63 c5             	movslq %r13d,%rax
  407920:	48 c1 e0 03          	shl    $0x3,%rax
  407924:	4c 01 f8             	add    %r15,%rax
  407927:	48 8b 00             	mov    (%rax),%rax
  40792a:	48 85 c0             	test   %rax,%rax
  40792d:	75 ea                	jne    407919 <env_copy+0x1c>
  40792f:	41 8d 45 01          	lea    0x1(%r13),%eax
  407933:	48 98                	cltq   
  407935:	48 c1 e0 03          	shl    $0x3,%rax
  407939:	48 89 c7             	mov    %rax,%rdi
  40793c:	e8 8f a6 ff ff       	callq  401fd0 <malloc@plt>
  407941:	49 89 c4             	mov    %rax,%r12
  407944:	4d 85 e4             	test   %r12,%r12
  407947:	75 15                	jne    40795e <env_copy+0x61>
  407949:	e8 32 a3 ff ff       	callq  401c80 <__errno_location@plt>
  40794e:	c7 00 0c 00 00 00    	movl   $0xc,(%rax)
  407954:	b8 00 00 00 00       	mov    $0x0,%eax
  407959:	e9 86 00 00 00       	jmpq   4079e4 <env_copy+0xe7>
  40795e:	bb 00 00 00 00       	mov    $0x0,%ebx
  407963:	eb 66                	jmp    4079cb <env_copy+0xce>
  407965:	48 63 c3             	movslq %ebx,%rax
  407968:	48 c1 e0 03          	shl    $0x3,%rax
  40796c:	4d 8d 34 04          	lea    (%r12,%rax,1),%r14
  407970:	48 63 c3             	movslq %ebx,%rax
  407973:	48 c1 e0 03          	shl    $0x3,%rax
  407977:	4c 01 f8             	add    %r15,%rax
  40797a:	48 8b 00             	mov    (%rax),%rax
  40797d:	48 89 c7             	mov    %rax,%rdi
  407980:	e8 7b a8 ff ff       	callq  402200 <strdup@plt>
  407985:	49 89 06             	mov    %rax,(%r14)
  407988:	49 8b 06             	mov    (%r14),%rax
  40798b:	48 85 c0             	test   %rax,%rax
  40798e:	75 38                	jne    4079c8 <env_copy+0xcb>
  407990:	eb 15                	jmp    4079a7 <env_copy+0xaa>
  407992:	48 63 c3             	movslq %ebx,%rax
  407995:	48 c1 e0 03          	shl    $0x3,%rax
  407999:	4c 01 e0             	add    %r12,%rax
  40799c:	48 8b 00             	mov    (%rax),%rax
  40799f:	48 89 c7             	mov    %rax,%rdi
  4079a2:	e8 89 a2 ff ff       	callq  401c30 <free@plt>
  4079a7:	83 eb 01             	sub    $0x1,%ebx
  4079aa:	85 db                	test   %ebx,%ebx
  4079ac:	79 e4                	jns    407992 <env_copy+0x95>
  4079ae:	4c 89 e7             	mov    %r12,%rdi
  4079b1:	e8 7a a2 ff ff       	callq  401c30 <free@plt>
  4079b6:	e8 c5 a2 ff ff       	callq  401c80 <__errno_location@plt>
  4079bb:	c7 00 0c 00 00 00    	movl   $0xc,(%rax)
  4079c1:	b8 00 00 00 00       	mov    $0x0,%eax
  4079c6:	eb 1c                	jmp    4079e4 <env_copy+0xe7>
  4079c8:	83 c3 01             	add    $0x1,%ebx
  4079cb:	44 39 eb             	cmp    %r13d,%ebx
  4079ce:	7c 95                	jl     407965 <env_copy+0x68>
  4079d0:	49 63 c5             	movslq %r13d,%rax
  4079d3:	48 c1 e0 03          	shl    $0x3,%rax
  4079d7:	4c 01 e0             	add    %r12,%rax
  4079da:	48 c7 00 00 00 00 00 	movq   $0x0,(%rax)
  4079e1:	4c 89 e0             	mov    %r12,%rax
  4079e4:	48 83 c4 08          	add    $0x8,%rsp
  4079e8:	5b                   	pop    %rbx
  4079e9:	41 5c                	pop    %r12
  4079eb:	41 5d                	pop    %r13
  4079ed:	41 5e                	pop    %r14
  4079ef:	41 5f                	pop    %r15
  4079f1:	5d                   	pop    %rbp
  4079f2:	c3                   	retq   

00000000004079f3 <env_set>:
  4079f3:	55                   	push   %rbp
  4079f4:	48 89 e5             	mov    %rsp,%rbp
  4079f7:	41 54                	push   %r12
  4079f9:	53                   	push   %rbx
  4079fa:	48 83 ec 10          	sub    $0x10,%rsp
  4079fe:	48 89 7d e8          	mov    %rdi,-0x18(%rbp)
  407a02:	48 89 75 e0          	mov    %rsi,-0x20(%rbp)
  407a06:	41 bc ff ff ff ff    	mov    $0xffffffff,%r12d
  407a0c:	bb 00 00 00 00       	mov    $0x0,%ebx
  407a11:	eb 33                	jmp    407a46 <env_set+0x53>
  407a13:	48 63 c3             	movslq %ebx,%rax
  407a16:	48 8d 14 c5 00 00 00 	lea    0x0(,%rax,8),%rdx
  407a1d:	00 
  407a1e:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  407a22:	48 01 d0             	add    %rdx,%rax
  407a25:	48 8b 00             	mov    (%rax),%rax
  407a28:	48 8b 4d e0          	mov    -0x20(%rbp),%rcx
  407a2c:	ba 3d 00 00 00       	mov    $0x3d,%edx
  407a31:	48 89 ce             	mov    %rcx,%rsi
  407a34:	48 89 c7             	mov    %rax,%rdi
  407a37:	e8 0c f0 ff ff       	callq  406a48 <strcmp_until>
  407a3c:	85 c0                	test   %eax,%eax
  407a3e:	75 03                	jne    407a43 <env_set+0x50>
  407a40:	41 89 dc             	mov    %ebx,%r12d
  407a43:	83 c3 01             	add    $0x1,%ebx
  407a46:	48 63 c3             	movslq %ebx,%rax
  407a49:	48 8d 14 c5 00 00 00 	lea    0x0(,%rax,8),%rdx
  407a50:	00 
  407a51:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  407a55:	48 01 d0             	add    %rdx,%rax
  407a58:	48 8b 00             	mov    (%rax),%rax
  407a5b:	48 85 c0             	test   %rax,%rax
  407a5e:	75 b3                	jne    407a13 <env_set+0x20>
  407a60:	83 c3 01             	add    $0x1,%ebx
  407a63:	41 83 fc ff          	cmp    $0xffffffff,%r12d
  407a67:	74 7e                	je     407ae7 <env_set+0xf4>
  407a69:	49 63 c4             	movslq %r12d,%rax
  407a6c:	48 8d 14 c5 00 00 00 	lea    0x0(,%rax,8),%rdx
  407a73:	00 
  407a74:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  407a78:	48 01 d0             	add    %rdx,%rax
  407a7b:	48 8b 00             	mov    (%rax),%rax
  407a7e:	48 89 c7             	mov    %rax,%rdi
  407a81:	e8 aa a1 ff ff       	callq  401c30 <free@plt>
  407a86:	49 63 c4             	movslq %r12d,%rax
  407a89:	48 8d 14 c5 00 00 00 	lea    0x0(,%rax,8),%rdx
  407a90:	00 
  407a91:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  407a95:	48 8d 1c 02          	lea    (%rdx,%rax,1),%rbx
  407a99:	48 8b 45 e0          	mov    -0x20(%rbp),%rax
  407a9d:	48 89 c7             	mov    %rax,%rdi
  407aa0:	e8 5b a7 ff ff       	callq  402200 <strdup@plt>
  407aa5:	48 89 03             	mov    %rax,(%rbx)
  407aa8:	48 8b 03             	mov    (%rbx),%rax
  407aab:	48 85 c0             	test   %rax,%rax
  407aae:	75 2e                	jne    407ade <env_set+0xeb>
  407ab0:	49 63 c4             	movslq %r12d,%rax
  407ab3:	48 8d 14 c5 00 00 00 	lea    0x0(,%rax,8),%rdx
  407aba:	00 
  407abb:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  407abf:	48 01 d0             	add    %rdx,%rax
  407ac2:	48 c7 00 98 92 40 00 	movq   $0x409298,(%rax)
  407ac9:	e8 b2 a1 ff ff       	callq  401c80 <__errno_location@plt>
  407ace:	c7 00 0c 00 00 00    	movl   $0xc,(%rax)
  407ad4:	b8 00 00 00 00       	mov    $0x0,%eax
  407ad9:	e9 96 00 00 00       	jmpq   407b74 <env_set+0x181>
  407ade:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  407ae2:	e9 8d 00 00 00       	jmpq   407b74 <env_set+0x181>
  407ae7:	8d 43 01             	lea    0x1(%rbx),%eax
  407aea:	48 98                	cltq   
  407aec:	c1 e0 03             	shl    $0x3,%eax
  407aef:	89 c2                	mov    %eax,%edx
  407af1:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  407af5:	48 89 d6             	mov    %rdx,%rsi
  407af8:	48 89 c7             	mov    %rax,%rdi
  407afb:	e8 80 a5 ff ff       	callq  402080 <realloc@plt>
  407b00:	49 89 c4             	mov    %rax,%r12
  407b03:	4d 85 e4             	test   %r12,%r12
  407b06:	75 12                	jne    407b1a <env_set+0x127>
  407b08:	e8 73 a1 ff ff       	callq  401c80 <__errno_location@plt>
  407b0d:	c7 00 0c 00 00 00    	movl   $0xc,(%rax)
  407b13:	b8 00 00 00 00       	mov    $0x0,%eax
  407b18:	eb 5a                	jmp    407b74 <env_set+0x181>
  407b1a:	48 63 c3             	movslq %ebx,%rax
  407b1d:	48 c1 e0 03          	shl    $0x3,%rax
  407b21:	49 8d 14 04          	lea    (%r12,%rax,1),%rdx
  407b25:	48 63 c3             	movslq %ebx,%rax
  407b28:	48 c1 e0 03          	shl    $0x3,%rax
  407b2c:	48 83 e8 08          	sub    $0x8,%rax
  407b30:	4c 01 e0             	add    %r12,%rax
  407b33:	48 8b 00             	mov    (%rax),%rax
  407b36:	48 89 02             	mov    %rax,(%rdx)
  407b39:	48 63 c3             	movslq %ebx,%rax
  407b3c:	48 c1 e0 03          	shl    $0x3,%rax
  407b40:	48 83 e8 08          	sub    $0x8,%rax
  407b44:	49 8d 1c 04          	lea    (%r12,%rax,1),%rbx
  407b48:	48 8b 45 e0          	mov    -0x20(%rbp),%rax
  407b4c:	48 89 c7             	mov    %rax,%rdi
  407b4f:	e8 ac a6 ff ff       	callq  402200 <strdup@plt>
  407b54:	48 89 03             	mov    %rax,(%rbx)
  407b57:	48 8b 03             	mov    (%rbx),%rax
  407b5a:	48 85 c0             	test   %rax,%rax
  407b5d:	75 12                	jne    407b71 <env_set+0x17e>
  407b5f:	e8 1c a1 ff ff       	callq  401c80 <__errno_location@plt>
  407b64:	c7 00 0c 00 00 00    	movl   $0xc,(%rax)
  407b6a:	b8 00 00 00 00       	mov    $0x0,%eax
  407b6f:	eb 03                	jmp    407b74 <env_set+0x181>
  407b71:	4c 89 e0             	mov    %r12,%rax
  407b74:	48 83 c4 10          	add    $0x10,%rsp
  407b78:	5b                   	pop    %rbx
  407b79:	41 5c                	pop    %r12
  407b7b:	5d                   	pop    %rbp
  407b7c:	c3                   	retq   

0000000000407b7d <load_env>:
  407b7d:	55                   	push   %rbp
  407b7e:	48 89 e5             	mov    %rsp,%rbp
  407b81:	53                   	push   %rbx
  407b82:	48 81 ec 18 08 00 00 	sub    $0x818,%rsp
  407b89:	48 89 bd e8 f7 ff ff 	mov    %rdi,-0x818(%rbp)
  407b90:	48 89 b5 e0 f7 ff ff 	mov    %rsi,-0x820(%rbp)
  407b97:	48 8b 85 e0 f7 ff ff 	mov    -0x820(%rbp),%rax
  407b9e:	48 89 c7             	mov    %rax,%rdi
  407ba1:	e8 aa a3 ff ff       	callq  401f50 <ftell@plt>
  407ba6:	48 89 45 d0          	mov    %rax,-0x30(%rbp)
  407baa:	8b 05 e0 49 20 00    	mov    0x2049e0(%rip),%eax        # 60c590 <LineNumber>
  407bb0:	89 45 cc             	mov    %eax,-0x34(%rbp)
  407bb3:	48 8b 85 e0 f7 ff ff 	mov    -0x820(%rbp),%rax
  407bba:	48 89 c7             	mov    %rax,%rdi
  407bbd:	e8 07 f7 ff ff       	callq  4072c9 <skip_comments>
  407bc2:	48 8b 95 e0 f7 ff ff 	mov    -0x820(%rbp),%rdx
  407bc9:	48 8b 85 e8 f7 ff ff 	mov    -0x818(%rbp),%rax
  407bd0:	b9 99 92 40 00       	mov    $0x409299,%ecx
  407bd5:	be e7 03 00 00       	mov    $0x3e7,%esi
  407bda:	48 89 c7             	mov    %rax,%rdi
  407bdd:	e8 76 f6 ff ff       	callq  407258 <get_string>
  407be2:	83 f8 ff             	cmp    $0xffffffff,%eax
  407be5:	75 0a                	jne    407bf1 <load_env+0x74>
  407be7:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  407bec:	e9 77 03 00 00       	jmpq   407f68 <load_env+0x3eb>
  407bf1:	48 8b 85 e8 f7 ff ff 	mov    -0x818(%rbp),%rax
  407bf8:	48 05 e7 03 00 00    	add    $0x3e7,%rax
  407bfe:	c6 00 00             	movb   $0x0,(%rax)
  407c01:	48 8d 85 e0 fb ff ff 	lea    -0x420(%rbp),%rax
  407c08:	be e8 03 00 00       	mov    $0x3e8,%esi
  407c0d:	48 89 c7             	mov    %rax,%rdi
  407c10:	e8 fb a4 ff ff       	callq  402110 <bzero@plt>
  407c15:	48 8d 85 f0 f7 ff ff 	lea    -0x810(%rbp),%rax
  407c1c:	be e8 03 00 00       	mov    $0x3e8,%esi
  407c21:	48 89 c7             	mov    %rax,%rdi
  407c24:	e8 e7 a4 ff ff       	callq  402110 <bzero@plt>
  407c29:	48 8d 85 e0 fb ff ff 	lea    -0x420(%rbp),%rax
  407c30:	48 89 45 d8          	mov    %rax,-0x28(%rbp)
  407c34:	c7 45 ec 00 00 00 00 	movl   $0x0,-0x14(%rbp)
  407c3b:	c6 45 eb 00          	movb   $0x0,-0x15(%rbp)
  407c3f:	48 8b 85 e8 f7 ff ff 	mov    -0x818(%rbp),%rax
  407c46:	48 89 45 e0          	mov    %rax,-0x20(%rbp)
  407c4a:	e9 7f 01 00 00       	jmpq   407dce <load_env+0x251>
  407c4f:	83 7d ec 06          	cmpl   $0x6,-0x14(%rbp)
  407c53:	0f 87 70 01 00 00    	ja     407dc9 <load_env+0x24c>
  407c59:	8b 45 ec             	mov    -0x14(%rbp),%eax
  407c5c:	48 8b 04 c5 a8 92 40 	mov    0x4092a8(,%rax,8),%rax
  407c63:	00 
  407c64:	ff e0                	jmpq   *%rax
  407c66:	48 8b 45 e0          	mov    -0x20(%rbp),%rax
  407c6a:	0f b6 00             	movzbl (%rax),%eax
  407c6d:	3c 27                	cmp    $0x27,%al
  407c6f:	74 0b                	je     407c7c <load_env+0xff>
  407c71:	48 8b 45 e0          	mov    -0x20(%rbp),%rax
  407c75:	0f b6 00             	movzbl (%rax),%eax
  407c78:	3c 22                	cmp    $0x22,%al
  407c7a:	75 12                	jne    407c8e <load_env+0x111>
  407c7c:	48 8b 45 e0          	mov    -0x20(%rbp),%rax
  407c80:	48 8d 50 01          	lea    0x1(%rax),%rdx
  407c84:	48 89 55 e0          	mov    %rdx,-0x20(%rbp)
  407c88:	0f b6 00             	movzbl (%rax),%eax
  407c8b:	88 45 eb             	mov    %al,-0x15(%rbp)
  407c8e:	83 45 ec 01          	addl   $0x1,-0x14(%rbp)
  407c92:	80 7d eb 00          	cmpb   $0x0,-0x15(%rbp)
  407c96:	74 37                	je     407ccf <load_env+0x152>
  407c98:	48 8b 45 e0          	mov    -0x20(%rbp),%rax
  407c9c:	0f b6 00             	movzbl (%rax),%eax
  407c9f:	3a 45 eb             	cmp    -0x15(%rbp),%al
  407ca2:	75 0e                	jne    407cb2 <load_env+0x135>
  407ca4:	83 45 ec 01          	addl   $0x1,-0x14(%rbp)
  407ca8:	48 83 45 e0 01       	addq   $0x1,-0x20(%rbp)
  407cad:	e9 1c 01 00 00       	jmpq   407dce <load_env+0x251>
  407cb2:	83 7d ec 01          	cmpl   $0x1,-0x14(%rbp)
  407cb6:	75 66                	jne    407d1e <load_env+0x1a1>
  407cb8:	48 8b 45 e0          	mov    -0x20(%rbp),%rax
  407cbc:	0f b6 00             	movzbl (%rax),%eax
  407cbf:	3c 3d                	cmp    $0x3d,%al
  407cc1:	75 5b                	jne    407d1e <load_env+0x1a1>
  407cc3:	c7 45 ec 07 00 00 00 	movl   $0x7,-0x14(%rbp)
  407cca:	e9 ff 00 00 00       	jmpq   407dce <load_env+0x251>
  407ccf:	83 7d ec 01          	cmpl   $0x1,-0x14(%rbp)
  407cd3:	75 49                	jne    407d1e <load_env+0x1a1>
  407cd5:	e8 c6 a5 ff ff       	callq  4022a0 <__ctype_b_loc@plt>
  407cda:	48 8b 10             	mov    (%rax),%rdx
  407cdd:	48 8b 45 e0          	mov    -0x20(%rbp),%rax
  407ce1:	0f b6 00             	movzbl (%rax),%eax
  407ce4:	0f b6 c0             	movzbl %al,%eax
  407ce7:	48 01 c0             	add    %rax,%rax
  407cea:	48 01 d0             	add    %rdx,%rax
  407ced:	0f b7 00             	movzwl (%rax),%eax
  407cf0:	0f b7 c0             	movzwl %ax,%eax
  407cf3:	25 00 20 00 00       	and    $0x2000,%eax
  407cf8:	85 c0                	test   %eax,%eax
  407cfa:	74 0e                	je     407d0a <load_env+0x18d>
  407cfc:	48 83 45 e0 01       	addq   $0x1,-0x20(%rbp)
  407d01:	83 45 ec 01          	addl   $0x1,-0x14(%rbp)
  407d05:	e9 c4 00 00 00       	jmpq   407dce <load_env+0x251>
  407d0a:	48 8b 45 e0          	mov    -0x20(%rbp),%rax
  407d0e:	0f b6 00             	movzbl (%rax),%eax
  407d11:	3c 3d                	cmp    $0x3d,%al
  407d13:	75 09                	jne    407d1e <load_env+0x1a1>
  407d15:	83 45 ec 01          	addl   $0x1,-0x14(%rbp)
  407d19:	e9 b0 00 00 00       	jmpq   407dce <load_env+0x251>
  407d1e:	48 8b 45 d8          	mov    -0x28(%rbp),%rax
  407d22:	48 8d 50 01          	lea    0x1(%rax),%rdx
  407d26:	48 89 55 d8          	mov    %rdx,-0x28(%rbp)
  407d2a:	48 8b 55 e0          	mov    -0x20(%rbp),%rdx
  407d2e:	48 8d 4a 01          	lea    0x1(%rdx),%rcx
  407d32:	48 89 4d e0          	mov    %rcx,-0x20(%rbp)
  407d36:	0f b6 12             	movzbl (%rdx),%edx
  407d39:	88 10                	mov    %dl,(%rax)
  407d3b:	e9 8e 00 00 00       	jmpq   407dce <load_env+0x251>
  407d40:	48 8b 45 e0          	mov    -0x20(%rbp),%rax
  407d44:	0f b6 00             	movzbl (%rax),%eax
  407d47:	3c 3d                	cmp    $0x3d,%al
  407d49:	75 15                	jne    407d60 <load_env+0x1e3>
  407d4b:	83 45 ec 01          	addl   $0x1,-0x14(%rbp)
  407d4f:	48 8d 85 f0 f7 ff ff 	lea    -0x810(%rbp),%rax
  407d56:	48 89 45 d8          	mov    %rax,-0x28(%rbp)
  407d5a:	c6 45 eb 00          	movb   $0x0,-0x15(%rbp)
  407d5e:	eb 2e                	jmp    407d8e <load_env+0x211>
  407d60:	e8 3b a5 ff ff       	callq  4022a0 <__ctype_b_loc@plt>
  407d65:	48 8b 10             	mov    (%rax),%rdx
  407d68:	48 8b 45 e0          	mov    -0x20(%rbp),%rax
  407d6c:	0f b6 00             	movzbl (%rax),%eax
  407d6f:	0f b6 c0             	movzbl %al,%eax
  407d72:	48 01 c0             	add    %rax,%rax
  407d75:	48 01 d0             	add    %rdx,%rax
  407d78:	0f b7 00             	movzwl (%rax),%eax
  407d7b:	0f b7 c0             	movzwl %ax,%eax
  407d7e:	25 00 20 00 00       	and    $0x2000,%eax
  407d83:	85 c0                	test   %eax,%eax
  407d85:	75 07                	jne    407d8e <load_env+0x211>
  407d87:	c7 45 ec 07 00 00 00 	movl   $0x7,-0x14(%rbp)
  407d8e:	48 83 45 e0 01       	addq   $0x1,-0x20(%rbp)
  407d93:	eb 39                	jmp    407dce <load_env+0x251>
  407d95:	e8 06 a5 ff ff       	callq  4022a0 <__ctype_b_loc@plt>
  407d9a:	48 8b 10             	mov    (%rax),%rdx
  407d9d:	48 8b 45 e0          	mov    -0x20(%rbp),%rax
  407da1:	0f b6 00             	movzbl (%rax),%eax
  407da4:	0f b6 c0             	movzbl %al,%eax
  407da7:	48 01 c0             	add    %rax,%rax
  407daa:	48 01 d0             	add    %rdx,%rax
  407dad:	0f b7 00             	movzwl (%rax),%eax
  407db0:	0f b7 c0             	movzwl %ax,%eax
  407db3:	25 00 20 00 00       	and    $0x2000,%eax
  407db8:	85 c0                	test   %eax,%eax
  407dba:	74 07                	je     407dc3 <load_env+0x246>
  407dbc:	48 83 45 e0 01       	addq   $0x1,-0x20(%rbp)
  407dc1:	eb 0b                	jmp    407dce <load_env+0x251>
  407dc3:	83 45 ec 01          	addl   $0x1,-0x14(%rbp)
  407dc7:	eb 05                	jmp    407dce <load_env+0x251>
  407dc9:	e8 a2 9e ff ff       	callq  401c70 <abort@plt>
  407dce:	83 7d ec 07          	cmpl   $0x7,-0x14(%rbp)
  407dd2:	74 0f                	je     407de3 <load_env+0x266>
  407dd4:	48 8b 45 e0          	mov    -0x20(%rbp),%rax
  407dd8:	0f b6 00             	movzbl (%rax),%eax
  407ddb:	84 c0                	test   %al,%al
  407ddd:	0f 85 6c fe ff ff    	jne    407c4f <load_env+0xd2>
  407de3:	83 7d ec 06          	cmpl   $0x6,-0x14(%rbp)
  407de7:	74 3a                	je     407e23 <load_env+0x2a6>
  407de9:	83 7d ec 05          	cmpl   $0x5,-0x14(%rbp)
  407ded:	75 06                	jne    407df5 <load_env+0x278>
  407def:	80 7d eb 00          	cmpb   $0x0,-0x15(%rbp)
  407df3:	74 2e                	je     407e23 <load_env+0x2a6>
  407df5:	48 8b 4d d0          	mov    -0x30(%rbp),%rcx
  407df9:	48 8b 85 e0 f7 ff ff 	mov    -0x820(%rbp),%rax
  407e00:	ba 00 00 00 00       	mov    $0x0,%edx
  407e05:	48 89 ce             	mov    %rcx,%rsi
  407e08:	48 89 c7             	mov    %rax,%rdi
  407e0b:	e8 50 a2 ff ff       	callq  402060 <fseek@plt>
  407e10:	8b 45 cc             	mov    -0x34(%rbp),%eax
  407e13:	89 05 77 47 20 00    	mov    %eax,0x204777(%rip)        # 60c590 <LineNumber>
  407e19:	b8 00 00 00 00       	mov    $0x0,%eax
  407e1e:	e9 45 01 00 00       	jmpq   407f68 <load_env+0x3eb>
  407e23:	83 7d ec 05          	cmpl   $0x5,-0x14(%rbp)
  407e27:	75 66                	jne    407e8f <load_env+0x312>
  407e29:	48 8d 85 f0 f7 ff ff 	lea    -0x810(%rbp),%rax
  407e30:	48 89 c7             	mov    %rax,%rdi
  407e33:	e8 38 9f ff ff       	callq  401d70 <strlen@plt>
  407e38:	48 89 c2             	mov    %rax,%rdx
  407e3b:	48 8d 85 f0 f7 ff ff 	lea    -0x810(%rbp),%rax
  407e42:	48 01 d0             	add    %rdx,%rax
  407e45:	48 89 45 e0          	mov    %rax,-0x20(%rbp)
  407e49:	eb 0c                	jmp    407e57 <load_env+0x2da>
  407e4b:	48 83 6d e0 01       	subq   $0x1,-0x20(%rbp)
  407e50:	48 8b 45 e0          	mov    -0x20(%rbp),%rax
  407e54:	c6 00 00             	movb   $0x0,(%rax)
  407e57:	48 8d 85 f0 f7 ff ff 	lea    -0x810(%rbp),%rax
  407e5e:	48 39 45 e0          	cmp    %rax,-0x20(%rbp)
  407e62:	76 2b                	jbe    407e8f <load_env+0x312>
  407e64:	e8 37 a4 ff ff       	callq  4022a0 <__ctype_b_loc@plt>
  407e69:	48 8b 00             	mov    (%rax),%rax
  407e6c:	48 8b 55 e0          	mov    -0x20(%rbp),%rdx
  407e70:	48 83 ea 01          	sub    $0x1,%rdx
  407e74:	0f b6 12             	movzbl (%rdx),%edx
  407e77:	0f b6 d2             	movzbl %dl,%edx
  407e7a:	48 01 d2             	add    %rdx,%rdx
  407e7d:	48 01 d0             	add    %rdx,%rax
  407e80:	0f b7 00             	movzwl (%rax),%eax
  407e83:	0f b7 c0             	movzwl %ax,%eax
  407e86:	25 00 20 00 00       	and    $0x2000,%eax
  407e8b:	85 c0                	test   %eax,%eax
  407e8d:	75 bc                	jne    407e4b <load_env+0x2ce>
  407e8f:	48 8d 85 f0 f7 ff ff 	lea    -0x810(%rbp),%rax
  407e96:	48 89 c7             	mov    %rax,%rdi
  407e99:	e8 4a ec ff ff       	callq  406ae8 <strdtb>
  407e9e:	89 45 c8             	mov    %eax,-0x38(%rbp)
  407ea1:	83 7d c8 01          	cmpl   $0x1,-0x38(%rbp)
  407ea5:	7e 5e                	jle    407f05 <load_env+0x388>
  407ea7:	0f b6 85 f0 f7 ff ff 	movzbl -0x810(%rbp),%eax
  407eae:	3c 27                	cmp    $0x27,%al
  407eb0:	74 0b                	je     407ebd <load_env+0x340>
  407eb2:	0f b6 85 f0 f7 ff ff 	movzbl -0x810(%rbp),%eax
  407eb9:	3c 22                	cmp    $0x22,%al
  407ebb:	75 48                	jne    407f05 <load_env+0x388>
  407ebd:	8b 45 c8             	mov    -0x38(%rbp),%eax
  407ec0:	83 e8 01             	sub    $0x1,%eax
  407ec3:	48 98                	cltq   
  407ec5:	0f b6 94 05 f0 f7 ff 	movzbl -0x810(%rbp,%rax,1),%edx
  407ecc:	ff 
  407ecd:	0f b6 85 f0 f7 ff ff 	movzbl -0x810(%rbp),%eax
  407ed4:	38 c2                	cmp    %al,%dl
  407ed6:	75 2d                	jne    407f05 <load_env+0x388>
  407ed8:	8b 45 c8             	mov    -0x38(%rbp),%eax
  407edb:	83 e8 01             	sub    $0x1,%eax
  407ede:	48 98                	cltq   
  407ee0:	c6 84 05 f0 f7 ff ff 	movb   $0x0,-0x810(%rbp,%rax,1)
  407ee7:	00 
  407ee8:	48 8d 85 f0 f7 ff ff 	lea    -0x810(%rbp),%rax
  407eef:	48 83 c0 01          	add    $0x1,%rax
  407ef3:	48 8d 95 f0 f7 ff ff 	lea    -0x810(%rbp),%rdx
  407efa:	48 89 c6             	mov    %rax,%rsi
  407efd:	48 89 d7             	mov    %rdx,%rdi
  407f00:	e8 bb 9d ff ff       	callq  401cc0 <strcpy@plt>
  407f05:	48 8d 85 e0 fb ff ff 	lea    -0x420(%rbp),%rax
  407f0c:	48 89 c7             	mov    %rax,%rdi
  407f0f:	e8 5c 9e ff ff       	callq  401d70 <strlen@plt>
  407f14:	48 89 c3             	mov    %rax,%rbx
  407f17:	48 8d 85 f0 f7 ff ff 	lea    -0x810(%rbp),%rax
  407f1e:	48 89 c7             	mov    %rax,%rdi
  407f21:	e8 4a 9e ff ff       	callq  401d70 <strlen@plt>
  407f26:	48 01 d8             	add    %rbx,%rax
  407f29:	48 83 c0 01          	add    $0x1,%rax
  407f2d:	48 3d e6 03 00 00    	cmp    $0x3e6,%rax
  407f33:	76 07                	jbe    407f3c <load_env+0x3bf>
  407f35:	b8 00 00 00 00       	mov    $0x0,%eax
  407f3a:	eb 2c                	jmp    407f68 <load_env+0x3eb>
  407f3c:	48 8d 8d f0 f7 ff ff 	lea    -0x810(%rbp),%rcx
  407f43:	48 8d 95 e0 fb ff ff 	lea    -0x420(%rbp),%rdx
  407f4a:	48 8b 85 e8 f7 ff ff 	mov    -0x818(%rbp),%rax
  407f51:	be 9b 92 40 00       	mov    $0x40929b,%esi
  407f56:	48 89 c7             	mov    %rax,%rdi
  407f59:	b8 00 00 00 00       	mov    $0x0,%eax
  407f5e:	e8 4d a2 ff ff       	callq  4021b0 <sprintf@plt>
  407f63:	b8 01 00 00 00       	mov    $0x1,%eax
  407f68:	48 81 c4 18 08 00 00 	add    $0x818,%rsp
  407f6f:	5b                   	pop    %rbx
  407f70:	5d                   	pop    %rbp
  407f71:	c3                   	retq   

0000000000407f72 <env_get>:
  407f72:	55                   	push   %rbp
  407f73:	48 89 e5             	mov    %rsp,%rbp
  407f76:	41 57                	push   %r15
  407f78:	41 56                	push   %r14
  407f7a:	41 55                	push   %r13
  407f7c:	41 54                	push   %r12
  407f7e:	53                   	push   %rbx
  407f7f:	48 83 ec 08          	sub    $0x8,%rsp
  407f83:	49 89 fe             	mov    %rdi,%r14
  407f86:	49 89 f5             	mov    %rsi,%r13
  407f89:	4c 89 f7             	mov    %r14,%rdi
  407f8c:	e8 df 9d ff ff       	callq  401d70 <strlen@plt>
  407f91:	41 89 c7             	mov    %eax,%r15d
  407f94:	eb 44                	jmp    407fda <env_get+0x68>
  407f96:	be 3d 00 00 00       	mov    $0x3d,%esi
  407f9b:	48 89 df             	mov    %rbx,%rdi
  407f9e:	e8 1d 9e ff ff       	callq  401dc0 <strchr@plt>
  407fa3:	49 89 c4             	mov    %rax,%r12
  407fa6:	4d 85 e4             	test   %r12,%r12
  407fa9:	75 02                	jne    407fad <env_get+0x3b>
  407fab:	eb 2d                	jmp    407fda <env_get+0x68>
  407fad:	4c 89 e2             	mov    %r12,%rdx
  407fb0:	48 89 d8             	mov    %rbx,%rax
  407fb3:	48 29 c2             	sub    %rax,%rdx
  407fb6:	49 63 c7             	movslq %r15d,%rax
  407fb9:	48 39 c2             	cmp    %rax,%rdx
  407fbc:	75 1c                	jne    407fda <env_get+0x68>
  407fbe:	49 63 c7             	movslq %r15d,%rax
  407fc1:	48 89 c2             	mov    %rax,%rdx
  407fc4:	4c 89 f6             	mov    %r14,%rsi
  407fc7:	48 89 df             	mov    %rbx,%rdi
  407fca:	e8 d1 9c ff ff       	callq  401ca0 <strncmp@plt>
  407fcf:	85 c0                	test   %eax,%eax
  407fd1:	75 07                	jne    407fda <env_get+0x68>
  407fd3:	49 8d 44 24 01       	lea    0x1(%r12),%rax
  407fd8:	eb 14                	jmp    407fee <env_get+0x7c>
  407fda:	4c 89 e8             	mov    %r13,%rax
  407fdd:	4c 8d 68 08          	lea    0x8(%rax),%r13
  407fe1:	48 8b 18             	mov    (%rax),%rbx
  407fe4:	48 85 db             	test   %rbx,%rbx
  407fe7:	75 ad                	jne    407f96 <env_get+0x24>
  407fe9:	b8 00 00 00 00       	mov    $0x0,%eax
  407fee:	48 83 c4 08          	add    $0x8,%rsp
  407ff2:	5b                   	pop    %rbx
  407ff3:	41 5c                	pop    %r12
  407ff5:	41 5d                	pop    %r13
  407ff7:	41 5e                	pop    %r14
  407ff9:	41 5f                	pop    %r15
  407ffb:	5d                   	pop    %rbp
  407ffc:	c3                   	retq   

0000000000407ffd <cron_popen>:
  407ffd:	55                   	push   %rbp
  407ffe:	48 89 e5             	mov    %rsp,%rbp
  408001:	41 54                	push   %r12
  408003:	53                   	push   %rbx
  408004:	48 81 ec 70 04 00 00 	sub    $0x470,%rsp
  40800b:	48 89 bd 98 fb ff ff 	mov    %rdi,-0x468(%rbp)
  408012:	48 89 b5 90 fb ff ff 	mov    %rsi,-0x470(%rbp)
  408019:	48 89 95 88 fb ff ff 	mov    %rdx,-0x478(%rbp)
  408020:	48 8b 85 90 fb ff ff 	mov    -0x470(%rbp),%rax
  408027:	0f b6 00             	movzbl (%rax),%eax
  40802a:	3c 72                	cmp    $0x72,%al
  40802c:	74 0e                	je     40803c <cron_popen+0x3f>
  40802e:	48 8b 85 90 fb ff ff 	mov    -0x470(%rbp),%rax
  408035:	0f b6 00             	movzbl (%rax),%eax
  408038:	3c 77                	cmp    $0x77,%al
  40803a:	75 12                	jne    40804e <cron_popen+0x51>
  40803c:	48 8b 85 90 fb ff ff 	mov    -0x470(%rbp),%rax
  408043:	48 83 c0 01          	add    $0x1,%rax
  408047:	0f b6 00             	movzbl (%rax),%eax
  40804a:	84 c0                	test   %al,%al
  40804c:	74 0a                	je     408058 <cron_popen+0x5b>
  40804e:	b8 00 00 00 00       	mov    $0x0,%eax
  408053:	e9 fa 03 00 00       	jmpq   408452 <cron_popen+0x455>
  408058:	48 8b 05 f9 44 20 00 	mov    0x2044f9(%rip),%rax        # 60c558 <pids>
  40805f:	48 85 c0             	test   %rax,%rax
  408062:	75 73                	jne    4080d7 <cron_popen+0xda>
  408064:	e8 27 a2 ff ff       	callq  402290 <getdtablesize@plt>
  408069:	89 05 f1 44 20 00    	mov    %eax,0x2044f1(%rip)        # 60c560 <fds>
  40806f:	8b 05 eb 44 20 00    	mov    0x2044eb(%rip),%eax        # 60c560 <fds>
  408075:	85 c0                	test   %eax,%eax
  408077:	7f 0a                	jg     408083 <cron_popen+0x86>
  408079:	b8 00 00 00 00       	mov    $0x0,%eax
  40807e:	e9 cf 03 00 00       	jmpq   408452 <cron_popen+0x455>
  408083:	8b 05 d7 44 20 00    	mov    0x2044d7(%rip),%eax        # 60c560 <fds>
  408089:	48 98                	cltq   
  40808b:	c1 e0 02             	shl    $0x2,%eax
  40808e:	89 c0                	mov    %eax,%eax
  408090:	48 89 c7             	mov    %rax,%rdi
  408093:	e8 38 9f ff ff       	callq  401fd0 <malloc@plt>
  408098:	48 89 05 b9 44 20 00 	mov    %rax,0x2044b9(%rip)        # 60c558 <pids>
  40809f:	48 8b 05 b2 44 20 00 	mov    0x2044b2(%rip),%rax        # 60c558 <pids>
  4080a6:	48 85 c0             	test   %rax,%rax
  4080a9:	75 0a                	jne    4080b5 <cron_popen+0xb8>
  4080ab:	b8 00 00 00 00       	mov    $0x0,%eax
  4080b0:	e9 9d 03 00 00       	jmpq   408452 <cron_popen+0x455>
  4080b5:	8b 05 a5 44 20 00    	mov    0x2044a5(%rip),%eax        # 60c560 <fds>
  4080bb:	48 98                	cltq   
  4080bd:	48 8d 14 85 00 00 00 	lea    0x0(,%rax,4),%rdx
  4080c4:	00 
  4080c5:	48 8b 05 8c 44 20 00 	mov    0x20448c(%rip),%rax        # 60c558 <pids>
  4080cc:	48 89 d6             	mov    %rdx,%rsi
  4080cf:	48 89 c7             	mov    %rax,%rdi
  4080d2:	e8 39 a0 ff ff       	callq  402110 <bzero@plt>
  4080d7:	48 8d 45 d0          	lea    -0x30(%rbp),%rax
  4080db:	48 89 c7             	mov    %rax,%rdi
  4080de:	e8 6d 9d ff ff       	callq  401e50 <pipe@plt>
  4080e3:	85 c0                	test   %eax,%eax
  4080e5:	79 0a                	jns    4080f1 <cron_popen+0xf4>
  4080e7:	b8 00 00 00 00       	mov    $0x0,%eax
  4080ec:	e9 61 03 00 00       	jmpq   408452 <cron_popen+0x455>
  4080f1:	c7 45 e4 00 00 00 00 	movl   $0x0,-0x1c(%rbp)
  4080f8:	4c 8b a5 98 fb ff ff 	mov    -0x468(%rbp),%r12
  4080ff:	eb 3a                	jmp    40813b <cron_popen+0x13e>
  408101:	8b 5d e4             	mov    -0x1c(%rbp),%ebx
  408104:	8d 43 01             	lea    0x1(%rbx),%eax
  408107:	89 45 e4             	mov    %eax,-0x1c(%rbp)
  40810a:	be e0 92 40 00       	mov    $0x4092e0,%esi
  40810f:	4c 89 e7             	mov    %r12,%rdi
  408112:	e8 e9 9f ff ff       	callq  402100 <strtok@plt>
  408117:	48 89 c2             	mov    %rax,%rdx
  40811a:	48 63 c3             	movslq %ebx,%rax
  40811d:	48 89 94 c5 a0 fc ff 	mov    %rdx,-0x360(%rbp,%rax,8)
  408124:	ff 
  408125:	48 63 c3             	movslq %ebx,%rax
  408128:	48 8b 84 c5 a0 fc ff 	mov    -0x360(%rbp,%rax,8),%rax
  40812f:	ff 
  408130:	48 85 c0             	test   %rax,%rax
  408133:	74 0e                	je     408143 <cron_popen+0x146>
  408135:	41 bc 00 00 00 00    	mov    $0x0,%r12d
  40813b:	83 7d e4 63          	cmpl   $0x63,-0x1c(%rbp)
  40813f:	7e c0                	jle    408101 <cron_popen+0x104>
  408141:	eb 01                	jmp    408144 <cron_popen+0x147>
  408143:	90                   	nop
  408144:	48 c7 45 c0 00 00 00 	movq   $0x0,-0x40(%rbp)
  40814b:	00 
  40814c:	48 c7 45 e8 00 00 00 	movq   $0x0,-0x18(%rbp)
  408153:	00 
  408154:	e8 17 a1 ff ff       	callq  402270 <fork@plt>
  408159:	89 45 e0             	mov    %eax,-0x20(%rbp)
  40815c:	8b 45 e0             	mov    -0x20(%rbp),%eax
  40815f:	83 f8 ff             	cmp    $0xffffffff,%eax
  408162:	74 09                	je     40816d <cron_popen+0x170>
  408164:	85 c0                	test   %eax,%eax
  408166:	74 1e                	je     408186 <cron_popen+0x189>
  408168:	e9 6b 02 00 00       	jmpq   4083d8 <cron_popen+0x3db>
  40816d:	8b 45 d0             	mov    -0x30(%rbp),%eax
  408170:	89 c7                	mov    %eax,%edi
  408172:	e8 c9 9c ff ff       	callq  401e40 <close@plt>
  408177:	8b 45 d4             	mov    -0x2c(%rbp),%eax
  40817a:	89 c7                	mov    %eax,%edi
  40817c:	e8 bf 9c ff ff       	callq  401e40 <close@plt>
  408181:	e9 c8 02 00 00       	jmpq   40844e <cron_popen+0x451>
  408186:	48 8b 85 90 fb ff ff 	mov    -0x470(%rbp),%rax
  40818d:	0f b6 00             	movzbl (%rax),%eax
  408190:	3c 72                	cmp    $0x72,%al
  408192:	75 3c                	jne    4081d0 <cron_popen+0x1d3>
  408194:	8b 45 d4             	mov    -0x2c(%rbp),%eax
  408197:	83 f8 01             	cmp    $0x1,%eax
  40819a:	74 28                	je     4081c4 <cron_popen+0x1c7>
  40819c:	8b 45 d4             	mov    -0x2c(%rbp),%eax
  40819f:	be 01 00 00 00       	mov    $0x1,%esi
  4081a4:	89 c7                	mov    %eax,%edi
  4081a6:	e8 05 9c ff ff       	callq  401db0 <dup2@plt>
  4081ab:	8b 45 d4             	mov    -0x2c(%rbp),%eax
  4081ae:	be 02 00 00 00       	mov    $0x2,%esi
  4081b3:	89 c7                	mov    %eax,%edi
  4081b5:	e8 f6 9b ff ff       	callq  401db0 <dup2@plt>
  4081ba:	8b 45 d4             	mov    -0x2c(%rbp),%eax
  4081bd:	89 c7                	mov    %eax,%edi
  4081bf:	e8 7c 9c ff ff       	callq  401e40 <close@plt>
  4081c4:	8b 45 d0             	mov    -0x30(%rbp),%eax
  4081c7:	89 c7                	mov    %eax,%edi
  4081c9:	e8 72 9c ff ff       	callq  401e40 <close@plt>
  4081ce:	eb 2a                	jmp    4081fa <cron_popen+0x1fd>
  4081d0:	8b 45 d0             	mov    -0x30(%rbp),%eax
  4081d3:	85 c0                	test   %eax,%eax
  4081d5:	74 19                	je     4081f0 <cron_popen+0x1f3>
  4081d7:	8b 45 d0             	mov    -0x30(%rbp),%eax
  4081da:	be 00 00 00 00       	mov    $0x0,%esi
  4081df:	89 c7                	mov    %eax,%edi
  4081e1:	e8 ca 9b ff ff       	callq  401db0 <dup2@plt>
  4081e6:	8b 45 d0             	mov    -0x30(%rbp),%eax
  4081e9:	89 c7                	mov    %eax,%edi
  4081eb:	e8 50 9c ff ff       	callq  401e40 <close@plt>
  4081f0:	8b 45 d4             	mov    -0x2c(%rbp),%eax
  4081f3:	89 c7                	mov    %eax,%edi
  4081f5:	e8 46 9c ff ff       	callq  401e40 <close@plt>
  4081fa:	48 8b 85 88 fb ff ff 	mov    -0x478(%rbp),%rax
  408201:	8b 40 0c             	mov    0xc(%rax),%eax
  408204:	89 c7                	mov    %eax,%edi
  408206:	e8 95 9e ff ff       	callq  4020a0 <setgid@plt>
  40820b:	85 c0                	test   %eax,%eax
  40820d:	74 6b                	je     40827a <cron_popen+0x27d>
  40820f:	e8 6c 9a ff ff       	callq  401c80 <__errno_location@plt>
  408214:	8b 00                	mov    (%rax),%eax
  408216:	89 c7                	mov    %eax,%edi
  408218:	e8 f3 9f ff ff       	callq  402210 <strerror@plt>
  40821d:	48 89 c1             	mov    %rax,%rcx
  408220:	48 8b 85 88 fb ff ff 	mov    -0x478(%rbp),%rax
  408227:	8b 40 0c             	mov    0xc(%rax),%eax
  40822a:	89 c2                	mov    %eax,%edx
  40822c:	48 8d 85 a0 fb ff ff 	lea    -0x460(%rbp),%rax
  408233:	49 89 c8             	mov    %rcx,%r8
  408236:	48 89 d1             	mov    %rdx,%rcx
  408239:	ba e4 92 40 00       	mov    $0x4092e4,%edx
  40823e:	be 00 01 00 00       	mov    $0x100,%esi
  408243:	48 89 c7             	mov    %rax,%rdi
  408246:	b8 00 00 00 00       	mov    $0x0,%eax
  40824b:	e8 a0 9b ff ff       	callq  401df0 <snprintf@plt>
  408250:	e8 eb 9a ff ff       	callq  401d40 <getpid@plt>
  408255:	89 c6                	mov    %eax,%esi
  408257:	48 8d 85 a0 fb ff ff 	lea    -0x460(%rbp),%rax
  40825e:	48 89 c1             	mov    %rax,%rcx
  408261:	ba 01 93 40 00       	mov    $0x409301,%edx
  408266:	bf 07 93 40 00       	mov    $0x409307,%edi
  40826b:	e8 69 f2 ff ff       	callq  4074d9 <log_it>
  408270:	bf 01 00 00 00       	mov    $0x1,%edi
  408275:	e8 46 9f ff ff       	callq  4021c0 <exit@plt>
  40827a:	48 8b 85 88 fb ff ff 	mov    -0x478(%rbp),%rax
  408281:	8b 58 0c             	mov    0xc(%rax),%ebx
  408284:	48 8b 85 88 fb ff ff 	mov    -0x478(%rbp),%rax
  40828b:	48 8b 40 10          	mov    0x10(%rax),%rax
  40828f:	48 89 c6             	mov    %rax,%rsi
  408292:	bf 0c 93 40 00       	mov    $0x40930c,%edi
  408297:	e8 d6 fc ff ff       	callq  407f72 <env_get>
  40829c:	89 de                	mov    %ebx,%esi
  40829e:	48 89 c7             	mov    %rax,%rdi
  4082a1:	e8 7a 9f ff ff       	callq  402220 <initgroups@plt>
  4082a6:	85 c0                	test   %eax,%eax
  4082a8:	74 6b                	je     408315 <cron_popen+0x318>
  4082aa:	e8 d1 99 ff ff       	callq  401c80 <__errno_location@plt>
  4082af:	8b 00                	mov    (%rax),%eax
  4082b1:	89 c7                	mov    %eax,%edi
  4082b3:	e8 58 9f ff ff       	callq  402210 <strerror@plt>
  4082b8:	48 89 c1             	mov    %rax,%rcx
  4082bb:	48 8b 85 88 fb ff ff 	mov    -0x478(%rbp),%rax
  4082c2:	8b 40 0c             	mov    0xc(%rax),%eax
  4082c5:	89 c2                	mov    %eax,%edx
  4082c7:	48 8d 85 a0 fb ff ff 	lea    -0x460(%rbp),%rax
  4082ce:	49 89 c8             	mov    %rcx,%r8
  4082d1:	48 89 d1             	mov    %rdx,%rcx
  4082d4:	ba 18 93 40 00       	mov    $0x409318,%edx
  4082d9:	be 00 01 00 00       	mov    $0x100,%esi
  4082de:	48 89 c7             	mov    %rax,%rdi
  4082e1:	b8 00 00 00 00       	mov    $0x0,%eax
  4082e6:	e8 05 9b ff ff       	callq  401df0 <snprintf@plt>
  4082eb:	e8 50 9a ff ff       	callq  401d40 <getpid@plt>
  4082f0:	89 c6                	mov    %eax,%esi
  4082f2:	48 8d 85 a0 fb ff ff 	lea    -0x460(%rbp),%rax
  4082f9:	48 89 c1             	mov    %rax,%rcx
  4082fc:	ba 01 93 40 00       	mov    $0x409301,%edx
  408301:	bf 07 93 40 00       	mov    $0x409307,%edi
  408306:	e8 ce f1 ff ff       	callq  4074d9 <log_it>
  40830b:	bf 01 00 00 00       	mov    $0x1,%edi
  408310:	e8 ab 9e ff ff       	callq  4021c0 <exit@plt>
  408315:	48 8b 85 88 fb ff ff 	mov    -0x478(%rbp),%rax
  40831c:	8b 40 08             	mov    0x8(%rax),%eax
  40831f:	89 c7                	mov    %eax,%edi
  408321:	e8 ca 9e ff ff       	callq  4021f0 <setuid@plt>
  408326:	85 c0                	test   %eax,%eax
  408328:	74 6b                	je     408395 <cron_popen+0x398>
  40832a:	e8 51 99 ff ff       	callq  401c80 <__errno_location@plt>
  40832f:	8b 00                	mov    (%rax),%eax
  408331:	89 c7                	mov    %eax,%edi
  408333:	e8 d8 9e ff ff       	callq  402210 <strerror@plt>
  408338:	48 89 c1             	mov    %rax,%rcx
  40833b:	48 8b 85 88 fb ff ff 	mov    -0x478(%rbp),%rax
  408342:	8b 40 08             	mov    0x8(%rax),%eax
  408345:	89 c2                	mov    %eax,%edx
  408347:	48 8d 85 a0 fb ff ff 	lea    -0x460(%rbp),%rax
  40834e:	49 89 c8             	mov    %rcx,%r8
  408351:	48 89 d1             	mov    %rdx,%rcx
  408354:	ba 39 93 40 00       	mov    $0x409339,%edx
  408359:	be 00 01 00 00       	mov    $0x100,%esi
  40835e:	48 89 c7             	mov    %rax,%rdi
  408361:	b8 00 00 00 00       	mov    $0x0,%eax
  408366:	e8 85 9a ff ff       	callq  401df0 <snprintf@plt>
  40836b:	e8 d0 99 ff ff       	callq  401d40 <getpid@plt>
  408370:	89 c6                	mov    %eax,%esi
  408372:	48 8d 85 a0 fb ff ff 	lea    -0x460(%rbp),%rax
  408379:	48 89 c1             	mov    %rax,%rcx
  40837c:	ba 01 93 40 00       	mov    $0x409301,%edx
  408381:	bf 07 93 40 00       	mov    $0x409307,%edi
  408386:	e8 4e f1 ff ff       	callq  4074d9 <log_it>
  40838b:	bf 01 00 00 00       	mov    $0x1,%edi
  408390:	e8 2b 9e ff ff       	callq  4021c0 <exit@plt>
  408395:	48 8b 85 88 fb ff ff 	mov    -0x478(%rbp),%rax
  40839c:	48 8b 40 10          	mov    0x10(%rax),%rax
  4083a0:	48 89 c6             	mov    %rax,%rsi
  4083a3:	bf 57 93 40 00       	mov    $0x409357,%edi
  4083a8:	e8 c5 fb ff ff       	callq  407f72 <env_get>
  4083ad:	48 89 c7             	mov    %rax,%rdi
  4083b0:	e8 db 99 ff ff       	callq  401d90 <chdir@plt>
  4083b5:	48 8b 85 a0 fc ff ff 	mov    -0x360(%rbp),%rax
  4083bc:	48 8d 95 a0 fc ff ff 	lea    -0x360(%rbp),%rdx
  4083c3:	48 89 d6             	mov    %rdx,%rsi
  4083c6:	48 89 c7             	mov    %rax,%rdi
  4083c9:	e8 72 9d ff ff       	callq  402140 <execvp@plt>
  4083ce:	bf 01 00 00 00       	mov    $0x1,%edi
  4083d3:	e8 d8 98 ff ff       	callq  401cb0 <_exit@plt>
  4083d8:	48 8b 85 90 fb ff ff 	mov    -0x470(%rbp),%rax
  4083df:	0f b6 00             	movzbl (%rax),%eax
  4083e2:	3c 72                	cmp    $0x72,%al
  4083e4:	75 24                	jne    40840a <cron_popen+0x40d>
  4083e6:	8b 45 d0             	mov    -0x30(%rbp),%eax
  4083e9:	48 8b 95 90 fb ff ff 	mov    -0x470(%rbp),%rdx
  4083f0:	48 89 d6             	mov    %rdx,%rsi
  4083f3:	89 c7                	mov    %eax,%edi
  4083f5:	e8 96 9c ff ff       	callq  402090 <fdopen@plt>
  4083fa:	48 89 45 e8          	mov    %rax,-0x18(%rbp)
  4083fe:	8b 45 d4             	mov    -0x2c(%rbp),%eax
  408401:	89 c7                	mov    %eax,%edi
  408403:	e8 38 9a ff ff       	callq  401e40 <close@plt>
  408408:	eb 22                	jmp    40842c <cron_popen+0x42f>
  40840a:	8b 45 d4             	mov    -0x2c(%rbp),%eax
  40840d:	48 8b 95 90 fb ff ff 	mov    -0x470(%rbp),%rdx
  408414:	48 89 d6             	mov    %rdx,%rsi
  408417:	89 c7                	mov    %eax,%edi
  408419:	e8 72 9c ff ff       	callq  402090 <fdopen@plt>
  40841e:	48 89 45 e8          	mov    %rax,-0x18(%rbp)
  408422:	8b 45 d0             	mov    -0x30(%rbp),%eax
  408425:	89 c7                	mov    %eax,%edi
  408427:	e8 14 9a ff ff       	callq  401e40 <close@plt>
  40842c:	48 8b 1d 25 41 20 00 	mov    0x204125(%rip),%rbx        # 60c558 <pids>
  408433:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  408437:	48 89 c7             	mov    %rax,%rdi
  40843a:	e8 51 9b ff ff       	callq  401f90 <fileno@plt>
  40843f:	48 98                	cltq   
  408441:	48 c1 e0 02          	shl    $0x2,%rax
  408445:	48 8d 14 03          	lea    (%rbx,%rax,1),%rdx
  408449:	8b 45 e0             	mov    -0x20(%rbp),%eax
  40844c:	89 02                	mov    %eax,(%rdx)
  40844e:	48 8b 45 e8          	mov    -0x18(%rbp),%rax
  408452:	48 81 c4 70 04 00 00 	add    $0x470,%rsp
  408459:	5b                   	pop    %rbx
  40845a:	41 5c                	pop    %r12
  40845c:	5d                   	pop    %rbp
  40845d:	c3                   	retq   

000000000040845e <cron_pclose>:
  40845e:	55                   	push   %rbp
  40845f:	48 89 e5             	mov    %rsp,%rbp
  408462:	41 54                	push   %r12
  408464:	53                   	push   %rbx
  408465:	48 81 ec 40 01 00 00 	sub    $0x140,%rsp
  40846c:	48 89 bd b8 fe ff ff 	mov    %rdi,-0x148(%rbp)
  408473:	48 8b 05 de 40 20 00 	mov    0x2040de(%rip),%rax        # 60c558 <pids>
  40847a:	48 85 c0             	test   %rax,%rax
  40847d:	74 28                	je     4084a7 <cron_pclose+0x49>
  40847f:	4c 8b 25 d2 40 20 00 	mov    0x2040d2(%rip),%r12        # 60c558 <pids>
  408486:	48 8b 85 b8 fe ff ff 	mov    -0x148(%rbp),%rax
  40848d:	48 89 c7             	mov    %rax,%rdi
  408490:	e8 fb 9a ff ff       	callq  401f90 <fileno@plt>
  408495:	89 c3                	mov    %eax,%ebx
  408497:	48 63 c3             	movslq %ebx,%rax
  40849a:	48 c1 e0 02          	shl    $0x2,%rax
  40849e:	4c 01 e0             	add    %r12,%rax
  4084a1:	8b 00                	mov    (%rax),%eax
  4084a3:	85 c0                	test   %eax,%eax
  4084a5:	75 0a                	jne    4084b1 <cron_pclose+0x53>
  4084a7:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  4084ac:	e9 05 01 00 00       	jmpq   4085b6 <cron_pclose+0x158>
  4084b1:	48 8b 85 b8 fe ff ff 	mov    -0x148(%rbp),%rax
  4084b8:	48 89 c7             	mov    %rax,%rdi
  4084bb:	e8 90 98 ff ff       	callq  401d50 <fclose@plt>
  4084c0:	48 8d 85 d0 fe ff ff 	lea    -0x130(%rbp),%rax
  4084c7:	48 89 c7             	mov    %rax,%rdi
  4084ca:	e8 71 9a ff ff       	callq  401f40 <sigemptyset@plt>
  4084cf:	48 8d 85 d0 fe ff ff 	lea    -0x130(%rbp),%rax
  4084d6:	be 03 00 00 00       	mov    $0x3,%esi
  4084db:	48 89 c7             	mov    %rax,%rdi
  4084de:	e8 6d 9d ff ff       	callq  402250 <sigaddset@plt>
  4084e3:	48 8d 85 d0 fe ff ff 	lea    -0x130(%rbp),%rax
  4084ea:	be 02 00 00 00       	mov    $0x2,%esi
  4084ef:	48 89 c7             	mov    %rax,%rdi
  4084f2:	e8 59 9d ff ff       	callq  402250 <sigaddset@plt>
  4084f7:	48 8d 85 d0 fe ff ff 	lea    -0x130(%rbp),%rax
  4084fe:	be 01 00 00 00       	mov    $0x1,%esi
  408503:	48 89 c7             	mov    %rax,%rdi
  408506:	e8 45 9d ff ff       	callq  402250 <sigaddset@plt>
  40850b:	48 8d 95 50 ff ff ff 	lea    -0xb0(%rbp),%rdx
  408512:	48 8d 85 d0 fe ff ff 	lea    -0x130(%rbp),%rax
  408519:	48 89 c6             	mov    %rax,%rsi
  40851c:	bf 00 00 00 00       	mov    $0x0,%edi
  408521:	e8 fa 96 ff ff       	callq  401c20 <sigprocmask@plt>
  408526:	48 8b 05 2b 40 20 00 	mov    0x20402b(%rip),%rax        # 60c558 <pids>
  40852d:	48 63 d3             	movslq %ebx,%rdx
  408530:	48 c1 e2 02          	shl    $0x2,%rdx
  408534:	48 01 d0             	add    %rdx,%rax
  408537:	8b 00                	mov    (%rax),%eax
  408539:	48 8d 8d cc fe ff ff 	lea    -0x134(%rbp),%rcx
  408540:	ba 00 00 00 00       	mov    $0x0,%edx
  408545:	48 89 ce             	mov    %rcx,%rsi
  408548:	89 c7                	mov    %eax,%edi
  40854a:	e8 61 9b ff ff       	callq  4020b0 <waitpid@plt>
  40854f:	89 45 ec             	mov    %eax,-0x14(%rbp)
  408552:	48 8d 85 50 ff ff ff 	lea    -0xb0(%rbp),%rax
  408559:	ba 00 00 00 00       	mov    $0x0,%edx
  40855e:	48 89 c6             	mov    %rax,%rsi
  408561:	bf 02 00 00 00       	mov    $0x2,%edi
  408566:	e8 b5 96 ff ff       	callq  401c20 <sigprocmask@plt>
  40856b:	48 8b 05 e6 3f 20 00 	mov    0x203fe6(%rip),%rax        # 60c558 <pids>
  408572:	48 63 d3             	movslq %ebx,%rdx
  408575:	48 c1 e2 02          	shl    $0x2,%rdx
  408579:	48 01 d0             	add    %rdx,%rax
  40857c:	c7 00 00 00 00 00    	movl   $0x0,(%rax)
  408582:	83 7d ec ff          	cmpl   $0xffffffff,-0x14(%rbp)
  408586:	74 13                	je     40859b <cron_pclose+0x13d>
  408588:	8b 85 cc fe ff ff    	mov    -0x134(%rbp),%eax
  40858e:	89 45 d0             	mov    %eax,-0x30(%rbp)
  408591:	8b 45 d0             	mov    -0x30(%rbp),%eax
  408594:	83 e0 7f             	and    $0x7f,%eax
  408597:	85 c0                	test   %eax,%eax
  408599:	74 07                	je     4085a2 <cron_pclose+0x144>
  40859b:	b8 ff ff ff ff       	mov    $0xffffffff,%eax
  4085a0:	eb 14                	jmp    4085b6 <cron_pclose+0x158>
  4085a2:	8b 85 cc fe ff ff    	mov    -0x134(%rbp),%eax
  4085a8:	89 45 e0             	mov    %eax,-0x20(%rbp)
  4085ab:	8b 45 e0             	mov    -0x20(%rbp),%eax
  4085ae:	25 00 ff 00 00       	and    $0xff00,%eax
  4085b3:	c1 f8 08             	sar    $0x8,%eax
  4085b6:	48 81 c4 40 01 00 00 	add    $0x140,%rsp
  4085bd:	5b                   	pop    %rbx
  4085be:	41 5c                	pop    %r12
  4085c0:	5d                   	pop    %rbp
  4085c1:	c3                   	retq   

00000000004085c2 <replace_str>:
  4085c2:	55                   	push   %rbp
  4085c3:	48 89 e5             	mov    %rsp,%rbp
  4085c6:	41 54                	push   %r12
  4085c8:	53                   	push   %rbx
  4085c9:	48 83 ec 40          	sub    $0x40,%rsp
  4085cd:	48 89 7d c8          	mov    %rdi,-0x38(%rbp)
  4085d1:	89 75 c4             	mov    %esi,-0x3c(%rbp)
  4085d4:	48 89 55 b8          	mov    %rdx,-0x48(%rbp)
  4085d8:	48 89 4d b0          	mov    %rcx,-0x50(%rbp)
  4085dc:	48 89 e0             	mov    %rsp,%rax
  4085df:	49 89 c4             	mov    %rax,%r12
  4085e2:	8b 45 c4             	mov    -0x3c(%rbp),%eax
  4085e5:	48 63 d0             	movslq %eax,%rdx
  4085e8:	48 83 ea 01          	sub    $0x1,%rdx
  4085ec:	48 89 55 e8          	mov    %rdx,-0x18(%rbp)
  4085f0:	48 63 d0             	movslq %eax,%rdx
  4085f3:	49 89 d2             	mov    %rdx,%r10
  4085f6:	41 bb 00 00 00 00    	mov    $0x0,%r11d
  4085fc:	48 63 d0             	movslq %eax,%rdx
  4085ff:	49 89 d0             	mov    %rdx,%r8
  408602:	41 b9 00 00 00 00    	mov    $0x0,%r9d
  408608:	48 98                	cltq   
  40860a:	ba 10 00 00 00       	mov    $0x10,%edx
  40860f:	48 83 ea 01          	sub    $0x1,%rdx
  408613:	48 01 d0             	add    %rdx,%rax
  408616:	bb 10 00 00 00       	mov    $0x10,%ebx
  40861b:	ba 00 00 00 00       	mov    $0x0,%edx
  408620:	48 f7 f3             	div    %rbx
  408623:	48 6b c0 10          	imul   $0x10,%rax,%rax
  408627:	48 29 c4             	sub    %rax,%rsp
  40862a:	48 89 e0             	mov    %rsp,%rax
  40862d:	48 83 c0 00          	add    $0x0,%rax
  408631:	48 89 45 e0          	mov    %rax,-0x20(%rbp)
  408635:	8b 45 c4             	mov    -0x3c(%rbp),%eax
  408638:	48 63 d0             	movslq %eax,%rdx
  40863b:	48 8b 45 e0          	mov    -0x20(%rbp),%rax
  40863f:	be 00 00 00 00       	mov    $0x0,%esi
  408644:	48 89 c7             	mov    %rax,%rdi
  408647:	e8 c4 97 ff ff       	callq  401e10 <memset@plt>
  40864c:	8b 45 c4             	mov    -0x3c(%rbp),%eax
  40864f:	83 c0 01             	add    $0x1,%eax
  408652:	48 63 d0             	movslq %eax,%rdx
  408655:	48 8b 45 c8          	mov    -0x38(%rbp),%rax
  408659:	48 89 d6             	mov    %rdx,%rsi
  40865c:	48 89 c7             	mov    %rax,%rdi
  40865f:	e8 cc 97 ff ff       	callq  401e30 <strnlen@plt>
  408664:	48 89 c2             	mov    %rax,%rdx
  408667:	8b 45 c4             	mov    -0x3c(%rbp),%eax
  40866a:	48 98                	cltq   
  40866c:	48 39 c2             	cmp    %rax,%rdx
  40866f:	76 0a                	jbe    40867b <replace_str+0xb9>
  408671:	b8 00 00 00 00       	mov    $0x0,%eax
  408676:	e9 f3 00 00 00       	jmpq   40876e <replace_str+0x1ac>
  40867b:	48 8b 55 b8          	mov    -0x48(%rbp),%rdx
  40867f:	48 8b 45 c8          	mov    -0x38(%rbp),%rax
  408683:	48 89 d6             	mov    %rdx,%rsi
  408686:	48 89 c7             	mov    %rax,%rdi
  408689:	e8 f2 9b ff ff       	callq  402280 <strstr@plt>
  40868e:	48 89 45 d8          	mov    %rax,-0x28(%rbp)
  408692:	48 83 7d d8 00       	cmpq   $0x0,-0x28(%rbp)
  408697:	75 0a                	jne    4086a3 <replace_str+0xe1>
  408699:	b8 00 00 00 00       	mov    $0x0,%eax
  40869e:	e9 cb 00 00 00       	jmpq   40876e <replace_str+0x1ac>
  4086a3:	48 8b 55 d8          	mov    -0x28(%rbp),%rdx
  4086a7:	48 8b 45 c8          	mov    -0x38(%rbp),%rax
  4086ab:	48 29 c2             	sub    %rax,%rdx
  4086ae:	48 89 d0             	mov    %rdx,%rax
  4086b1:	48 89 c2             	mov    %rax,%rdx
  4086b4:	48 8b 45 e0          	mov    -0x20(%rbp),%rax
  4086b8:	48 8b 4d c8          	mov    -0x38(%rbp),%rcx
  4086bc:	48 89 ce             	mov    %rcx,%rsi
  4086bf:	48 89 c7             	mov    %rax,%rdi
  4086c2:	e8 c9 95 ff ff       	callq  401c90 <strncpy@plt>
  4086c7:	48 8b 45 b8          	mov    -0x48(%rbp),%rax
  4086cb:	48 89 c7             	mov    %rax,%rdi
  4086ce:	e8 9d 96 ff ff       	callq  401d70 <strlen@plt>
  4086d3:	48 89 c2             	mov    %rax,%rdx
  4086d6:	48 8b 45 d8          	mov    -0x28(%rbp),%rax
  4086da:	48 01 c2             	add    %rax,%rdx
  4086dd:	48 8b 45 e0          	mov    -0x20(%rbp),%rax
  4086e1:	48 8b 75 d8          	mov    -0x28(%rbp),%rsi
  4086e5:	48 8b 4d c8          	mov    -0x38(%rbp),%rcx
  4086e9:	48 29 ce             	sub    %rcx,%rsi
  4086ec:	48 89 f1             	mov    %rsi,%rcx
  4086ef:	48 8d 3c 08          	lea    (%rax,%rcx,1),%rdi
  4086f3:	48 8b 45 b0          	mov    -0x50(%rbp),%rax
  4086f7:	48 89 d1             	mov    %rdx,%rcx
  4086fa:	48 89 c2             	mov    %rax,%rdx
  4086fd:	be 5c 93 40 00       	mov    $0x40935c,%esi
  408702:	b8 00 00 00 00       	mov    $0x0,%eax
  408707:	e8 a4 9a ff ff       	callq  4021b0 <sprintf@plt>
  40870c:	48 8b 45 e0          	mov    -0x20(%rbp),%rax
  408710:	48 89 c7             	mov    %rax,%rdi
  408713:	e8 58 96 ff ff       	callq  401d70 <strlen@plt>
  408718:	48 89 c2             	mov    %rax,%rdx
  40871b:	48 8b 4d e0          	mov    -0x20(%rbp),%rcx
  40871f:	48 8b 45 c8          	mov    -0x38(%rbp),%rax
  408723:	48 89 ce             	mov    %rcx,%rsi
  408726:	48 89 c7             	mov    %rax,%rdi
  408729:	e8 62 95 ff ff       	callq  401c90 <strncpy@plt>
  40872e:	8b 45 c4             	mov    -0x3c(%rbp),%eax
  408731:	48 63 d8             	movslq %eax,%rbx
  408734:	48 8b 45 e0          	mov    -0x20(%rbp),%rax
  408738:	48 89 c7             	mov    %rax,%rdi
  40873b:	e8 30 96 ff ff       	callq  401d70 <strlen@plt>
  408740:	48 29 c3             	sub    %rax,%rbx
  408743:	48 8b 45 e0          	mov    -0x20(%rbp),%rax
  408747:	48 89 c7             	mov    %rax,%rdi
  40874a:	e8 21 96 ff ff       	callq  401d70 <strlen@plt>
  40874f:	48 89 c2             	mov    %rax,%rdx
  408752:	48 8b 45 c8          	mov    -0x38(%rbp),%rax
  408756:	48 01 d0             	add    %rdx,%rax
  408759:	48 89 da             	mov    %rbx,%rdx
  40875c:	be 00 00 00 00       	mov    $0x0,%esi
  408761:	48 89 c7             	mov    %rax,%rdi
  408764:	e8 a7 96 ff ff       	callq  401e10 <memset@plt>
  408769:	b8 01 00 00 00       	mov    $0x1,%eax
  40876e:	4c 89 e4             	mov    %r12,%rsp
  408771:	83 f8 01             	cmp    $0x1,%eax
  408774:	48 8d 65 f0          	lea    -0x10(%rbp),%rsp
  408778:	5b                   	pop    %rbx
  408779:	41 5c                	pop    %r12
  40877b:	5d                   	pop    %rbp
  40877c:	c3                   	retq   
  40877d:	0f 1f 00             	nopl   (%rax)

0000000000408780 <__libc_csu_init>:
  408780:	41 57                	push   %r15
  408782:	41 56                	push   %r14
  408784:	41 89 ff             	mov    %edi,%r15d
  408787:	41 55                	push   %r13
  408789:	41 54                	push   %r12
  40878b:	4c 8d 25 7e 26 20 00 	lea    0x20267e(%rip),%r12        # 60ae10 <__frame_dummy_init_array_entry>
  408792:	55                   	push   %rbp
  408793:	48 8d 2d 7e 26 20 00 	lea    0x20267e(%rip),%rbp        # 60ae18 <__init_array_end>
  40879a:	53                   	push   %rbx
  40879b:	49 89 f6             	mov    %rsi,%r14
  40879e:	49 89 d5             	mov    %rdx,%r13
  4087a1:	4c 29 e5             	sub    %r12,%rbp
  4087a4:	48 83 ec 08          	sub    $0x8,%rsp
  4087a8:	48 c1 fd 03          	sar    $0x3,%rbp
  4087ac:	e8 3f 94 ff ff       	callq  401bf0 <_init>
  4087b1:	48 85 ed             	test   %rbp,%rbp
  4087b4:	74 20                	je     4087d6 <__libc_csu_init+0x56>
  4087b6:	31 db                	xor    %ebx,%ebx
  4087b8:	0f 1f 84 00 00 00 00 	nopl   0x0(%rax,%rax,1)
  4087bf:	00 
  4087c0:	4c 89 ea             	mov    %r13,%rdx
  4087c3:	4c 89 f6             	mov    %r14,%rsi
  4087c6:	44 89 ff             	mov    %r15d,%edi
  4087c9:	41 ff 14 dc          	callq  *(%r12,%rbx,8)
  4087cd:	48 83 c3 01          	add    $0x1,%rbx
  4087d1:	48 39 eb             	cmp    %rbp,%rbx
  4087d4:	75 ea                	jne    4087c0 <__libc_csu_init+0x40>
  4087d6:	48 83 c4 08          	add    $0x8,%rsp
  4087da:	5b                   	pop    %rbx
  4087db:	5d                   	pop    %rbp
  4087dc:	41 5c                	pop    %r12
  4087de:	41 5d                	pop    %r13
  4087e0:	41 5e                	pop    %r14
  4087e2:	41 5f                	pop    %r15
  4087e4:	c3                   	retq   
  4087e5:	90                   	nop
  4087e6:	66 2e 0f 1f 84 00 00 	nopw   %cs:0x0(%rax,%rax,1)
  4087ed:	00 00 00 

00000000004087f0 <__libc_csu_fini>:
  4087f0:	f3 c3                	repz retq 
  4087f2:	66 2e 0f 1f 84 00 00 	nopw   %cs:0x0(%rax,%rax,1)
  4087f9:	00 00 00 
  4087fc:	0f 1f 40 00          	nopl   0x0(%rax)

0000000000408800 <__stat>:
  408800:	48 89 f2             	mov    %rsi,%rdx
  408803:	48 89 fe             	mov    %rdi,%rsi
  408806:	bf 01 00 00 00       	mov    $0x1,%edi
  40880b:	e9 a0 97 ff ff       	jmpq   401fb0 <__xstat@plt>

0000000000408810 <__fstat>:
  408810:	48 89 f2             	mov    %rsi,%rdx
  408813:	89 fe                	mov    %edi,%esi
  408815:	bf 01 00 00 00       	mov    $0x1,%edi
  40881a:	e9 01 98 ff ff       	jmpq   402020 <__fxstat@plt>
  40881f:	90                   	nop

0000000000408820 <__lstat>:
  408820:	48 89 f2             	mov    %rsi,%rdx
  408823:	48 89 fe             	mov    %rdi,%rsi
  408826:	bf 01 00 00 00       	mov    $0x1,%edi
  40882b:	e9 50 95 ff ff       	jmpq   401d80 <__lxstat@plt>

Disassembly of section .fini:

0000000000408830 <_fini>:
  408830:	48 83 ec 08          	sub    $0x8,%rsp
  408834:	48 83 c4 08          	add    $0x8,%rsp
  408838:	c3                   	retq   
