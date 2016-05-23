-----------------------------------------------------------------------
OpenSSL v1.0.2h Win32 for ICS, http://www.overbyte.be
-----------------------------------------------------------------------

Previously built-in default engines became external DLLs by default.
As long as ICS doesn't support OpenSSL engines just use libeay32.dll 
and ssleay32.dll.

Built with:       Visual Studio Community 2013
                  The Netwide Assembler (NASM) v2.11.05
                  Strawberry Perl v5.20.3.1

Build Commands:   perl configure VC-WIN32 no-ssl2-method
                  ms\do_nasm
                  adjusted ms\ntdll.mak       (replaced "/MD" by "/MT")
                  nmake -f ms\ntdll.mak
                  nmake -f ms\ntdll.mak test
                  editbin.exe /rebase:base=0x11000000 libeay32.dll
                  editbin.exe /rebase:base=0x12000000 ssleay32.dll
