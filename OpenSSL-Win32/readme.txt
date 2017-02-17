-----------------------------------------------------------------------
OpenSSL v1.1.0e Win32 for ICS, http://www.overbyte.be
-----------------------------------------------------------------------

Built with:       Visual Studio Community 2013
                  The Netwide Assembler (NASM) v2.11.05
                  Strawberry Perl v5.20.3.1

Build Commands:   perl configure VC-WIN32
                  nmake
                  editbin.exe /rebase:base=0x11000000 libcrypto-1_1.dll
                  editbin.exe /rebase:base=0x12000000 libssl-1_1.dll
