# vixie-cron
Debian's research crond (vixie cron)

Added:
- new env support in cron: MAILSUBJECT - for change default mail's subject which sending by cron. Added some macros for generate mail's subject. Example: 

```
MAILSUBJECT="CRON at the %hostname% (fqdn: %fqdn%): User %user% ran command %cmd% which was executed with status %status%. Cron fork status: %forkstatus%"
* * * * * root test
```
