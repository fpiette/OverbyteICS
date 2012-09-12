ICS - Internet Component Suite - V7 - Delphi 7 to RAD Studio XE3
================================================================
(Aka FPIETTE's Components)


Revised: September 12, 2012
http://www.overbyte.be

Table of content:
-----------------

- Legal issues
- Donate
- Register
- Contributions
- Installation
- Version Control repository
- Sample applications
- About SSL
- Support
- Release notes
- Midware
- Known problems
- Special thanks


Legal issues:
-------------
              Copyright (C) 1997-2012 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium
              <francois.piette@overbyte.be>

              SSL implementation includes code written by Arno Garrels,
              Berlin, Germany, contact: <arno.garrels@gmx.de>

              This software is provided 'as-is', without any express or
              implied warranty. In no event will the author be held liable
              for any damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

              5. As this code make use of OpenSSL, your rights are restricted
                 by OpenSSL license as soon as you use any SSL feature.
                 See http://www.openssl.org for details.



Donate
------

ICS is freeware. You can use it without paying anything except the registration
postcard (see "register" below). But of course donations are welcome. You can
send cash (Euro currency or US Dollars) in an envelop to my street address or
buy a gift certificate at Amazon in the UK. I will then use it to buy books.
Here is the direct URL at Amazon UK (nearest to my home, please don't use another):
http://www.amazon.co.uk/exec/obidos/gc-email-order1/ref=g_gc_email/202-6198323-6681414
For more generous amount, contact me by email.


Register
--------

ICS is freeware. If you use the components, you must register by sending a
picture postcard showing the area you live in and some beautiful stamps for
my kids who are stamp collectors. Do not use an envelop, I collect USED
postcards sent to me. Write on the postcard that it is your ICS registration.

Address your card to: Francois PIETTE, rue de Grady 24, 4053 Embourg, Belgium.
Don't forget to mention your name, street address, EMail and website.


Contributions:
--------------

ICS has been designed by François PIETTE but many other peoples are working on the
components and sample programs. The history of changes in each source file list
all developers having contributed (When no name is given, the change is by F. Piette).
I can't list all contributors here but I want to specially thanks two specially active
contributors:
    - Arno Garrels <arno.garrels@gmx.de>
    - Angus Robertson <angus@magsys.co.uk>


Latest versions:
---------------

The latest versions of ICS can be downloaded from the ICS Wiki web site:

http://wiki.overbyte.be/wiki/index.php/ICS_Download

V5, V6 and V7 are stable releases that are only ever updated for major bugs, but
not for new releases of Delphi. V8 Beta is the current beta development release
which is held in a public Version Control repository that is zipped each night
for easy download.  The download page above also includes the OpenSSL binaries
needed to support SSL.  V8 Beta is used by the developers in live production
applications.


Version Control repository:
---------------------------

svn://svn.overbyte.be/ics or http://svn.overbyte.be:8443/svn/ics
(Usercode = ics, password = ics)


Installation:
-------------

ICS-V7 has been designed for Delphi 2009 and up, and BCB 2009 and up, but is fully
compatible with Delphi 7, 2006 and 2007. Embarcadero RAD Studio includes Delphi and
C++ Builder.

With Delphi XE2 and later, VCL 64-bit Windows targets are supported for Delphi only.
FireMonkey is only supported by ICS-V8.

The zip file has subdirectories in it. You must use the WinZip "Use folder
names" option to restore this directory tree or you will have problems
because the files would not be in their proper subdirectories.

This is the subdirectory layout:

.\                            Info directory
.\delphi\Internet             Delphi.W32 sample internet applications (all Win32 Delphi versions)
.\delphi\Internet\Browser     Delphi.W32 sample browser application (all Win32 Delphi versions)
.\delphi\internet\WebServData Directory for WebServ demo data files
.\delphi\internet\WebAppServData Directory for WebAppServ demo data files
.\delphi\MiscDemos            Delphi.W32 sample non-internet applications (all Win32 Delphi versions)
.\delphi\sslinternet          Delphi.W32 SSL-enabled sample applications (all Win32 Delphi versions)
.\delphi\internet\WebServData Directory for WebServ demo data files
.\cpp\internet                C++Builder sample applications
.\cpp\internet\cb2006         C++Builder 2006 projects
.\cpp\internet\cb2007         C++Builder 2007 projects
.\cpp\internet\cb2009         C++Builder 2009 projects
.\cpp\internet\cb2010         C++Builder 2010 projects
.\cpp\internet\cbXE           C++Builder XE projects
.\cpp\internet\cbXE2          C++Builder XE2 projects
.\delphi\vc32                 Delphi (7 and up) and C++Builder (2006 and up) components
.\Install                     Component packages project groups for all versions


UPGRADING and REINSTALLING
Uninstall an existing ICS package (Menu | Component | Install Packages, select
the component package and click Remove).
Rename the old ICS directory and unzip to a new or empty directory, remove the
old path from the library path and add the new VC32 directory to the library
path under Tools | Options |...

All DELPHI and C++ BUILDER VERSIONS/WIN32
Always upgrade your compiler with the latest update available from Borland.
Always update your system with http://windowsupdate.microsoft.com

SSL or not SSL?
By default the SSL code is compiled into the run-time package and additional SSL-
enabled components are installed. In order to not compile the SSL code into the
run-time package and to not install the SSL-Enabled components you need to remove
the conditional define USE_SSL from both the run-time and design-time package.
However if you do not build your applications with run-time packages it is
recommended to build the packages with default settings. The SSL code will the
be compiled into your applications depending on whether the conditional define
USE_SSL is set in the project options or not.
Actual use of SSL in your applications also requires LIBEAY32.DLL and SSLEAY32.DLL
being available somewhere in the path, more details in IcsSslHowTo.txt.

INSTALLATION USING THE INSTALL PROJECT GROUPS
For each Delphi and C++ Builder version one project group is provided in directory
.\Install:

Delphi 7         :  D7Install.bpg
Delphi 2006      :  D2006Install.bdsgroup
Delphi 2007      :  D2007Install.groupproj
Delphi 2009      :  D2009Install.groupproj
Delphi 2010      :  D2010Install.groupproj
Delphi XE        :  DXeInstall.groupproj
Delphi XE2       :  DXe2Install.groupproj
Delphi XE3       :  DXe3Install.groupproj
C++ Builder 2006 :  CB2006Install.bdsgroup
C++ Builder 2007 :  CB2007Install.groupproj
C++ Builder 2009 :  CB2009Install.groupproj
C++ Builder 2010 :  CB2010Install.groupproj
C++ Builder XE   :  CBXeInstall.groupproj
C++ Builder XE2  :  CBXe2Install.groupproj
C++ Builder XE3  :  CBXe3Install.groupproj

1 - Do a File/Open Project, navigate to the Install directory, select the correct
file and open it. The project manager view should now display two package
projects, one run-time and one design-time package. The run-time package name
contains the "Run" suffix. The design-time package name contains the "Design"
suffix.
2 - Select and Build the run-time package (do not install).
3 - Select and Install the design-time package.

After a few seconds, you should have a dialog box telling you the package has
been installed with a bunch of new components registered in the Tool Palette
under "Overbyte ICS" and "Overbyte ICS SSL". Then do a "Save All" and a "Close All".

ALTERNATE INSTALLATION USING THE PACKAGE PROJECT FILES:
For each Delphi and C++ Builder version two package project files exist in the
VC32 directory. One run-time and one design-time package project file.
The run-time file name contains the "Run" suffix. The design-time file name
contains the "Design" suffix.

PACKAGE PROJECT FILE NAMES:
Delphi 7         :  OverbyteIcsD7Run.dpk, OverbyteIcsD7Design.dpk
Delphi 2006      :  OverbyteIcsD2006Run.bdsproj, OverbyteIcsD2006Design.bdsproj
Delphi 2007      :  OverbyteIcsD2007Run.dproj, OverbyteIcsD2007Design.dproj
Delphi 2009      :  OverbyteIcsD2009Run.dproj, OverbyteIcsD2009Design.dproj
Delphi 2010      :  OverbyteIcsD2010Run.dproj, OverbyteIcsD2010Design.dproj
Delphi XE        :  OverbyteIcsDXeRun.dproj, OverbyteIcsDXeDesign.dproj
Delphi XE2       :  OverbyteIcsDXe2Run.dproj, OverbyteIcsDXe2Design.dproj
Delphi XE3       :  OverbyteIcsDXe3Run.dproj, OverbyteIcsDXe3Design.dproj
C++ Builder 2006 :  OverbyteIcsCB2006Run.bdsproj, OverbyteIcsCB2006Design.bdsproj
C++ Builder 2007 :  OverbyteIcsCB2007Run.cbproj, OverbyteIcsCB2007Design.cbproj
C++ Builder 2009 :  OverbyteIcsCB2009Run.cbproj, OverbyteIcsCB2009Design.cbproj
C++ Builder 2010 :  OverbyteIcsCB2010Run.cbproj, OverbyteIcsCB2010Design.cbproj
C++ Builder XE   :  OverbyteIcsCBXeRun.cbproj, OverbyteIcsCBXeDesign.cbproj
C++ Builder XE2  :  OverbyteIcsCBXe2Run.cbproj, OverbyteIcsCBXe2Design.cbproj
C++ Builder XE3  :  OverbyteIcsCBXe3Run.cbproj, OverbyteIcsCBXe3Design.cbproj

1 - Open and Build the run-time package project (do not install!).

2 - Open and Install the design-time package project.
(Do a File/Open Project, browse to the VC32 directory. Select the correct file
and open it. Then in the project manager view, right-click on the package,
then click on either the Build or Install button.)

3 - For Delphi XE2, a 64-bit run-time package can be built by changing the
package target platform to 64-bit Windows. This has the same name as the 32-bit
package, so a different package output directory needs to be specified in
Tools / Options / Delphi Options for 64-bit Windows.

After a few seconds, you should have a dialog box telling you the package has
been installed with a bunch of new components registered in the Tool Palette
under "Overbyte ICS" and "Overbyte ICS SSL". Then do a "Save All" and a "Close All".

DELPHI 2006/WIN32, 2007/WIN32, 2009/WIN32, 2010/WIN32, XE/WIN32:
Having installed the package, verify that the VC32 directory has been added to
the Win32 Library Path (Tools / Options / Delphi Options / Library - Win32 /
Library Path). If not, add it manually. It is not mandatory to add vc32 to the global
Delphi path, but it will be much easier for you because otherwise you'll have to
add it to each project.

DELPHI XE2/XE3/WIN32 and XE2/XE3/WIN64: Similar to above, but the Library path is
specified separately for 32-bit and 64-bit Windows Platforms.

DELPHI 7: Add VC32 directory path to your library path (Tools menu / Environment
Options / Library / Library Path. Add VC32 path at the end of the existing path).

Once the package is installed, you may open the sample projects. There is a single
project group called OverByteIcsDemo.bpg for all versions (7, 2006, 2007, 2009, 2010,
XE and XE2) which has all sample programs. Open it with file/open project (Ctrl-F11),
browse to the Internet directory, select and open OverByteIcsDemo.bpg. You might get
some dialog box telling you that resource files are missing (they have not been
included in the zip file to save space) and are recreated by Delphi. It is OK.
Any other error message is a problem you should fix. After all resource files have
been recreated, you should see in the project manager a group of projects called
OverByteIcsDemo. In this group, you'll find all sample programs.

To compile all samples at once, do Project / Build all projects. This will
take some time to compile all sample programs. Delphi may run out of
memory if you don't have enough RAM installed in your computer.
If this happended, just build the projects one by one.

Note 1: Delphi may run out of memory if you ask to compile all projects at
once. If you have not enough RAM, then compile each project individually.

Note 2: Delphi has warnings which triggers a lot of messages for 100% OK
code. You can turn those warnings off in the project/ options / Compiler messages
and deselecting: "Deprecated symbol", "Platform symbol", "unsafe type", "unsafe code",
"unsafe typecast". Those are intended for .NET and Linux portability. You can
safely ignore them if you run windows. For you facility, I included a utility
SetProjectOptions (source code, you must compile it) in the internet directory.
This utility will update project options to disable the warnings.

Once the components are all installed, you may open the sample projects
each one after the other and compile them. For each project, do file/open
and select the dpr file in the internet directory. Then Project/Build All.

CBUILDER 2006, 2007, 2009, 2010, XE, XE2, XE3:
Follow the installation procedure described for Delphi 2006. Just change
the project group and package name: replace "del" by "bcb" in their names.
You can't have Delphi 2006 and CBuilder 2006 packages installed at the
same time in the IDE. So when switching from one to the other, be sure to
remove the one you don't need.
If you need both BCB and Delphi personalities ate the same time, then
use Delphi 2006 package (OverbyteIcsDel100.bpl) and change his options to make it
a dual mode Delphi/CPP package. See Borland documentation.

Once the components are all installed, you may open the sample projects
each one after the other and compile them. For each project, do file/open
and select the dpr file in the internet directory. Then Project/Build All.
Projects are located in CPP/INTERNET/BCB6. There is also a project group
OverbyteBcb60Sam.bpg which contains all sample projects.
It is likely that for each project, Bcb complains about a missing .res
file. This is not a problem, Bcb will recreate it as needed. They have not
been included to save space in the zip file.

Once the components are all installed, you may open the sample projects
each one after the other and compile them. For each project, do file/open
and select the dpr file in the internet directory. Then Project/Build All.
Projects are located in CPP/INTERNET/BCB1.


NOTES:
- You may have an error message, using Delphi or BCB, complaining about
Font.Charset, OldCreateOrder and other properties. Those are new properties
in newer Delphi or BCB versions, newer than the version you use.
You can safely ignore those errors because those properties are not
used by the components nor sample programs. You may encounter this
error at run time. To avoid it, you must open each form at design time
and ignore the error. Then recompile. If you don't ignore the error
at design time, you'll have it at runtime !

- If you have Delphi or BCB complaining about a file not found, add VC32
directory to your library path.

- If you are using BCB you may encounter an error at link time
such as "Unable to open file MWBCB30.LIB" (or other libs). This is a bug
in BCB. To solve it, you can edit project option file (right click in
project manager) and remove any reference to the missing libraries.

- Don't forget that the C++Builder components are located in .\delphi\vc32
which is object pascal source code (not a problem for C++Builder, just
indicate that the *.pas files are displayed when installing). C++Builder
will create the *.hpp files. There are some on-line help files in the VC32
directory.

- The following is a list of the files that should be installed in order to
properly add all of the available components in this collection:

> OverbyteIcsCharsetComboBox.pas Provides easy MIME charset selection
> OverbyteIcsDnsQuery          DNS lookup component - useful for getting MX records
> OverbyteIcsDprUpdFix.pas     IDE plugin for Delphi 2009 and 2010 to update old projects
> OverbyteIcsEmulVT.pas        ANSI terminal emulation in a control
> OverbyteIcsFingCli.pas       FINGER client protocol - Find information about user
> OverbyteIcsFtpCli.pas        FTP client protocol - file transfer
> OverbyteIcsFtpSrv.pas        FTP server protocol - file transfer
> OverbyteIcsFtpSrvT.pas       FTP server protocol - helpers
> OverbyteIcsHttpAppServer.pas HTTP server protocol - used to build advanced web servers
> OverbyteIcsHttpProt.pas      HTTP client protocol - used by the web
> OverbyteIcsHttpSrv.pas       HTTP server protocol - used to build web servers
> OverbyteIcsLogger.pas        A component to log information
> OverbyteIcsMimeDec.pas       MIME component - decode file attach, use with POP3
> OverbyteIcsMultiProgressBar.pas A segmented progress bar
> OverbyteIcsMultipartFtpDownloader.pas
        FTP client protocol - download one file using simultaneous connections to speedup download
> OverbyteIcsMultipartHttpDownloader.pas
        HTTP client protocol - download one file using simultaneous connections to speedup download
> OverbyteIcsNntpCli.pas       NNTP client protocol - send and receive newsgroups messages
> OverbyteIcsPing.pas          ICMP echo protocol - ping a host
> OverbyteIcsPop3Prot.pas      POP3 client protocol - get mail from mail server
> OverbyteIcsReg.pas           Register design components
> OverbyteIcsSmtpProt.pas      SMTP client protocol - send mail to server
> OverbyteIcsSnmpCli.pas       SNMP client protocol - network management
> OverbyteIcsSnmpMsgs.pas      SNMP client protocol - message helper
> OverbyteIcsSysLogClient.pas  Syslog Client Protocol - receive syslog messages
> OverbyteIcsSysLogDefs.pas    Syslog Protocol - helpers
> OverbyteIcsSysLogServer.pas  Syslog Server Protocol - send syslog messages
> OverbyteIcsTnCnx.pas         TELNET client protocol - terminal emulation protocol
> OverbyteIcsTnEmulVT.pas      TELNET and ANSI terminal emulation combined
> OverbyteIcsTnOptFrm.pas      TELNET Client configuration form
> OverbyteIcsTnScript.pas      TELNET client protocol - with automation
> OverbyteIcsWSocket.pas       Winsock component - TCP, UDP, DNS,...
> OverbyteIcsWSocketE.pas      Register procedure and property editor for TWSocket
> OverbyteIcsWSocketS.pas      Winsock component for building servers
> OverbyteIcsWSocketTS.pas     Winsock component for building multithreaded servers

- The following list support and utilities units:
> OverbyteIcsAsn1Utils.pas     ASN1 utilities (for TSnmpClient component)
> OverbyteIcsAvlTrees.pas      Implements a fast cache-like data storage
> OverbyteIcsCharsetUtils.pas  MIME-charset functions
> OverbyteIcsCookies.pas       Client Cookie Handling
> OverbyteIcsCRC.pas           32 bit CRC computation
> OverbyteIcsCsc.pas           character set routines
> OverbyteIcsDES.pas           Implementation of the Data Encryption Standard (DES)
> OverbyteIcsDigestAuth.pas    HTTP Digest Access Authentication
> OverbyteIcsFormDataDecoder.pas Decode a MIME data block as generated by a HTML form
> OverbyteIcsHttpCCodZLib.pas  Supports GZIP coding for HttpContCod
> OverbyteIcsHttpContCod.pas   HTTP Content Coding support, uses extra units
> OverbyteIcsIcmp.pas          ICMP protocol support, used by the PING component
> OverbyteIcsIconv.pas         Headers for iconv library (LGPL)
> OverbyteIcsLIBEAY.pas        Delphi encapsulation for LIBEAY32.DLL (OpenSSL)
> OverbyteIcsMD4.pas           Implementation of the MD4 Message-Digest Algorithm
> OverbyteIcsMD5.pas           Implementation of the MD5 Message-Digest Algorithm
> OverbyteIcsMimeUtil.pas      Support routines for MIME standard
> OverbyteIcsMLang.pas         A few header translations from MS mlang.h
> OverbyteIcsNtlmMsgs.pas      Client NTLM authentification messages used within HTTP protocol
> OverbyteIcsNtlmSsp.pas       Server NTLM authentification of user credentials using Windows SSPI
> OverbyteIcsOneTimePw.pas     One Time Password support functions, used by FTP
> OverbyteIcsSHA1.pas          Implementation of US Secure Hash Algorithm 1 (SHA1)
> OverbyteIcsSocketUtils.pas   Cross platform socket utilities for ICS
> OverbyteIcsSSLEAY.pas        Delphi encapsulation for SSLEAY32.DLL (OpenSSL)
> OverbyteIcsSslSessionCache.pas  A very fast external SSL-session-cache component
> OverbyteIcsSslThrdLock.pas   Implementation of OpenSsl thread locking (Windows);
> OverbyteIcsSspi.pas          A few header translations from MS sspi.h and security.h
> OverbyteIcsStreams.pas       Fast streams for ICS
> OverbyteIcsThreadTimer.pas   A custom timer class using custom timer messages from one or more threads
> OverbyteIcsTicks64.pas       GetTickCount64 support for all versions of Windows
> OverbyteIcsTimeList.pas      List of items with expiry times, used for WebSessions
> OverbyteIcsTypes.pas         Common types, mainly for backward compiler compatibility
> OverbyteIcsURL.pas           Support routines for URL handling
> OverbyteIcsUtils.pas         Vast number of common utilities, many supporting Unicode for D7/2007
> OverbyteIcsWSockBuf.pas      FIFO buffers for TWSocket
> OverbyteIcsWebSession.pas    Web session support for THttpAppSrv and MidWare
> OverbyteIcsWinnls.pas        A few header translations for Unicode Normalization in winnls.h
> OverbyteIcsWinsock.pas       Some Winsock initialisations
> OverbyteIcsWndControl.pas    A class that encapsulates a windows message queue and a message map
> OverbyteIcsZLibDll.pas       Zlib support, interface to external zlib.dll functions
> OverbyteIcsZlibHigh.pas      Zlib support, high level interface for compression and decompression
> OverbyteIcsZLibObj.pas       Zlib support, interface to zlib linked C OBJ functions


Sample applications:
--------------------

Note: All sample file names begins with prefix "OverbyteIcs".

Many samples are similar. When searching for something, always look at the date
the demos where created. The most recent is always the best code!  In the lists
below, ACTIVE!! indicates applications that are actively maintained to test and
support new functionality in the ICS components.  These may not be simplest
samples, but are usually the first to try when learning about a component.

Delphi Win32/Win64 Web Browser sample application
-------------------------------------------------
.\delphi\Internet\Broswer
> FrameBrowserIcs.dpr           Web Browser using HtmlViewer component - ACTIVE!!
Note this sample needs HtmlViewer component installed

Delphi Win32/Win64 FTP sample applications
------------------------------------------
.\delphi\Internet\OverbyteIcsDemo.bpg - Project group
> OverbyteIcsBasFtp.dpr         Basic FTP client program
> OverbyteIcsConFtp.dpr         Basic console mode FTP client
> OverbyteIcsFtpAsy.dpr         Example of asynchronous FTP client
> OverbyteIcsFtpMulti.dpr       Demo to do several FTP downloads in parallel to get a list of files
> OverbyteIcsFtpMultipartDownload.dpr Demo to FTP download a single large file in several parts in parallel
> OverbyteIcsFtpServ.dpr        General purpose FTP server, uses TSocketServer - ACTIVE!!
> OverbyteIcsFtpThrd.dpr        Demo of multithreaded FTP client, see also FTPASY
> OverbyteIcsFtpTst.dpr         Basic graphical FTP client - ACTIVE!!

Delphi Win32/Win64 SMTP, POP3, NNTP sample applications
-------------------------------------------------------
.\delphi\Internet\OverbyteIcsDemo.bpg - Project group
> OverbyteIcsBasNntp.dpr        Basic NNTP client program
> OverbyteIcsConPop3.dpr        Basic console mode demo for POP3 (mail receive)
> OverbyteIcsConSmtp.dpr        Basic console mode demo for SMTP (mail send)
> OverbyteIcsMailHtml.dpr       Example of HTML formatted EMail sending, including embedded images - ACTIVE!!
> OverbyteIcsMailRcv.dpr        Internet EMail access using POP3 protocol - ACTIVE!!
> OverbyteIcsMailSnd.dpr        Example of EMail sending using SMTP, including file attach - ACTIVE!!
> OverbyteIcsMailSndAsync.dpr   Example of parallel EMail sending with multiple connections
> OverbyteIcsMimeDemo.dpr       Example of EMail decoding (attached files are extracted) - ACTIVE!!
> OverbyteIcsNewsReader.dpr     Example of TNntpCli component (Send/receive newsgroups) - ACTIVE!!

Delphi Win32/Win64 Miscellaneous applications
---------------------------------------------
.\delphi\MiscDemos\OverbyteIcsMiscDemos.bpg - Project group
> OverbyteIcsBufStrmTst.dpr     Test of buffered stream classes
> OverbyteIcsCacheTest.dpr      Test of TCacheTree class used in TSslAvlSessionCache
> OverbyteIcsMD4Test.dpr        Test program for MD4 unit
> OverbyteIcsMD5File.dpr        Example of MD5 unit: computer MD5 checksum for files
> OverbyteIcsMD5Test.dpr        Test program for MD5 unit
> OverbyteIcsOneTimePassword.dpr One Time Password test routines for OverByteIcsOneTimePw unit
> OverbyteIcsSHA1Test.dpr       Test program for SHA unit
> OverbyteIcsThreadTimerDemo.dpr Demo for TIcsThreadTimer
> OverbyteIcsTicks64Demo.dpr    GetTickCount64 test routines for OverbyteIcsTicks64 unit
> OverbyteIcsTimerDemo.dpr      Very simple demo for TIcsTimer
> OverByteIcsWndControlTest.dpr Test program for windows and threads

Delphi Win32/Win64 DNS, Ping, SNMP, Syslog sample applications
--------------------------------------------------------------
.\delphi\Internet\OverbyteIcsDemo.bpg - Project group
> OverbyteIcsBatchDnsLookup.dpr Batch async DNS lookup using DnsLookup (IPv6 and IPv4)
> OverbyteIcsConPing.dpr        Basic console mode demo for ping component
> OverbyteIcsDll1.dpr           Demo showing how to use a TWSocket component in a DLL
> OverbyteIcsDll2.dpr           Demo showing how to use a THttpCli component in a DLL
> OverbyteIcsDllTst.dpr         Test program calling ICSDLL1 and ICSDLL2
> OverbyteIcsDnsLook.dpr        Example of name resolution (IPv6 and IPv4)
> OverbyteIcsDnsResolver.dpr    Batch async DNS lookup event driven using DnsQuery
> OverbyteIcsFinger.dpr         Example of TFingerCli component
> OverbyteIcsNsLookup.dpr       Demo for the DnsQuery component
> OverbyteIcsPingTst.dpr        Demo for the ping component
> OverbyteIcsSnmpCliTst.dpr     Demo for SNMP (simple network management protocol) component
> OverbyteIcsSysLogClientDemo.dpr Demo for SysLog client component
> OverbyteIcsSysLogServerDemo.dpr Demo for SysLog server component

 Delphi Win32/Win64 Socket sample applications
 ---------------------------------------------
.\delphi\Internet\OverbyteIcsDemo.bpg - Project group
> OverbyteIcsBinCliDemo.dpr       Client program to receive binary and delimited text data. Works with BinTcpSrv demo.
> OverbyteIcsCliDemo.dpr          Example of client for SRVDEMO, IPV4 only  - ACTIVE!!
> OverbyteIcsClient5.dpr          Basic client GUI applications
> OverbyteIcsClient7.dpr          Simple client application demonstrating TWSocket
> OverbyteIcsConCli1.dpr          Basic client/server console applications
> OverbyteIcsConCli2.dpr          Basic client/server console applications with thread
> OverbyteIcsConSrv1.dpr          Basic server application in console mode
> OverbyteIcsConUdpLstn.dpr       Console application to listen for UDP messages
> OverbyteIcsDynCli.dpr           Demo of dynamically created TWSocket components
> OverbyteIcsMtSrv.dpr            Basic server, multi-threaded, see THRDSRV for better code
> OverbyteIcsRecv.dpr             Simple file receive (server), use with SENDER demo (client)
> OverbyteIcsSender.dpr           Simple file send (client), use with RECV demo (server)
> OverbyteIcsServer5.dpr          Basic server GUI applications
> OverbyteIcsSocksTst.dpr         How to use TWSocket with SOCKS protocol (firewall traversing)
> OverbyteIcsSrvDemo.dpr          Example of server using a TTable - ACTIVE!!
> OverbyteIcsSrvTcp.dpr           Basic server without client forms, event-driven
> OverbyteIcsSvcTcp.dpr           Same as SRVTCP but as an NT/2K/XP service
> OverbyteIcsTWSChat.dpr          Chat program (both client and server in a single program)
> OverbyteIcsTcpSrv.dpr           Basic server without client forms, event-driven, IPv4 only - ACTIVE!!
> OverbyteIcsTcpSrvIPv6.dpr       Basic server without client forms, event-driven, IPv4/IPV6 - ACTIVE!!
> OverbyteIcsTelnetClient.dpr     Telnet client using a TnEmulVT
> OverbyteIcsThrdSrv.dpr          Basic multithreaded TCP server, banner sent in main thread
> OverbyteIcsThrdSrvV2.dpr        Basic multithreaded TCP server, banner sent in worker thread
> OverbyteIcsThrdSrvV3.dpr        Basic TCP server showing how to use TWSocketThrdServer
> OverbyteIcsTnDemo.dpr           Telnet client using a TMemo
> OverbyteIcsTnSrv.dpr            Basic TCP server with client forms, event-driven
> OverbyteIcsUdpLstn.dpr          UDP listen demo
> OverbyteIcsUdpSend.dpr          UDP send demo

Delphi Win32/Win64 SSL-enabled sample applications
--------------------------------------------------
.\delphi\sslinternet\OverbyteIcsSslDemo.bpg - Project Group
> OverbyteIcsHttpsTst.dpr         Example of TSslHttpCli component (GET) - ACTIVE!!
> OverbyteIcsMsVerify.dpr         Demo of HTTPs server, static and dynamic pages, template for HTML - ACTIVE!!
> OverbyteIcsPemTool.dpr          ICS Pem Certificate Tool - Create and import certificates in OpenSLL PEM format
> OverbyteIcsSimpleSslCli.dpr     Example of simple SSL client using TSslWSocket
> OverbyteIcsSimpleSslServer.dpr  Example of SSL server using TSslWSocket
> OverbyteIcsSslFtpServ.dpr       General purpose FTP SSL server, uses TSocketServer - ACTIVE!!
> OverbyteIcsSslFtpTst.dpr        Basic graphical FTP SSL client - ACTIVE!!
> OverbyteIcsSslMailRcv.dpr       Internet EMail access using POP3 protocol and SSL - ACTIVE!!
> OverbyteIcsSslMailSnd.dpr       Example of EMail sending using SMTP and SSL - ACTIVE!!
> OverbyteIcsSslNewsRdr.dpr       Example of TSslNntpCli component (Send/receive newsgroups) - ACTIVE!!
> OverbyteIcsSslSniSrv.dpr`       Test of Server Name Indication (SNI) in server mode.
> OverbyteIcsSslWebServ.dpr       Demo of HTTPS server, uses TSocketServer - ACTIVE!!

Delphi Win32/Win64 HTTP sample applications
-------------------------------------------
.\delphi\Internet\OverbyteIcsDemo.bpg - Project group
> OverbyteIcsConHttp.dpr          Basic console mode HTTP client
> OverbyteIcsHttpAsp.dpr          Example of THttpCli component with cookie (POST to an ASP page)
> OverbyteIcsHttpAsy.dpr          Example of THttpCli component with multiple async requests (GET)
> OverbyteIcsHttpChk.dpr          Example of THttpCli to check for valid URL using HEAD request
> OverbyteIcsHttpDmo.dpr          Simple HTTP client demo with proxy
> OverbyteIcsHttpGet.dpr          Example of THttpCli component (GET into a file)
> OverbyteIcsHttpMultipartDownload.dpr  Demo application for TMultipartHttpDownloader to download files using simultaneous connections
> OverbyteIcsHttpPg.dpr           Example of THttpCli component (POST to CGI script)
> OverbyteIcsHttpPost.dpr         Example of THttpCli component (POST), work with WebServ sample - ACTIVE!!
> OverbyteIcsHttpThrd.dpr         Example of THttpCli component (multi-threaded GET)
> OverbyteIcsHttpTst.dpr          Example of THttpCli component (GET), show many features - ACTIVE!!
> OverbyteIcsIsapi.dll            Example of FTP client component within an ISAPI extension
> OverbyteIcsWebAppServer.dpr     Advanced HTTP server demo, uses WebServ, adds sessions - ACTIVE!!
> OverbyteIcsWebServ.dpr          Demo of HTTP server, uses TSocketServer - ACTIVE!!

Two samples are not in the project group since they need extra components installed
> OverbyteIcsRestDemo.drp         Demo program showing how to use REST API from Google and Yahoo
> OverbyteIcsRestJsonDemo.drp     Demo program showing how to use REST API from Google Search and JSON

Sample Notes
------------
Note 1: Not all samples have been rewritten in C++ for C++ Builder. And those rewritten are
        frequently much simpler. So C++ Builder user: have a look at the Delphi sample too !
Note 2: Follow "UserMade" link on ICS web site to find more sample programs written by
        ICS users.

As explained in the component installation, you may encounter an error loading
a sample application or running it. This may be because the last time I
loaded the form, I was using another Delphi or BCB version which has new properties.
You can safely ignore messages related to those new properties. They are not used
in the samples. (The properties are CharSet, OldCreateOrder and others).
You can also encounter error about duplicate resources. You can ignore them
safely. If you have those errors, open each form in the IDE, ignore the error
then recompile. If you don't open the form in the IDE, you'll get the errors
at runtime and your program will abort.

When installing a new version, always delete old dcu, obj, dcpil and always
recompile everything !
Close everything before recompiling the library or packages.
When installing a new version, be sure to unzip it in the same directory
tree as the old one or you'll mess both versions.

It is possible to use several Delphi or BCB versions at the same time, but
before switching from one to the other, you MUST delete all DCU, OBJ, ILS,
ILF, TDS, ILC and ILD files.For BCB, you sometimes need to delete HPP files
as well. They will be recreated when you reinstall ICS components.

About SSL:
----------
TSslWSocket and TSslWSocketServer component are derived from the standard
TWSocket and TWSocketServer component. The SSL code is compiled into the
component only if you define USE_SSL symbol to your packages and projects.
Just add USE_SSL to the defines in the project or package options and
recompile everything.

The components make use of LIBEAY32.DLL and SSLEAY32.DLL to handle SSL
protocol stuff. The DLLs are dynamically loaded at runtime. It means that
the DLLs will only be required at runtime when you first make use of a SSL
function. Your applications will run on systems without OpenSSL DLLs as long
as you don't call any SSL function.

This version requires OpenSsl version 0.98e! If you need to support
older OpenSsl versions as well (not recommended) define symbol
BEFORE_OSSL_098E in OverbyteIcsSslDefs.inc and rebuild all.

Most ICS components have their SSL enabled counter part. They work exactly
the same way as the regular component except when SSL specific stuff is needed,
for example certificates. To support SSL stuff, the SSL-enabled version use
some new properties, events and methods. Many sample programs have their
SSL-enabled counter part in a separate sources located in SslInternet folder.

SSL certificates:
To make use of SSL, you frequently need certificates. I provide some demo
certificates I built using command line OpenSSL tool. PEM certificates can
be opened by a text editor, LF as well as CRLF are allowed as line breaks.

CACERT.PEM :   A demo certificate for "Example CA"
01CERT.PEM :   A demo certificate which is signed by CACERT.PEM
01KEY.PEM :    A demo private key for 01CERT.PEM
               Passphrase is "password".
CLIENT.PEM :   A demo certificate and private key.
               Passphrase is "password".
SERVER.PEM :   A demo certificate and private key.
               Passphrase is "password".
ROOT.PEM :     A demo CA certificate.
               Passphrase is "password".
TRUSTEDCABUNDLE.PEM :
               A demo CA file in PEM format containing multiple
               well known root CA certificates to be specified in
               property CA Path of the demo applications. Read
               the comments included in this file.
6F6359FC.0 :   Located in sub directory SslInternet\TrustedCaStore,
               it's the file CACERT.PEM stored with a hashed file
               name. Directory TrustedCaStore can be spezified in
               property CA Path of the demo applications.

For details about certificate, see the excellent book:
  "Network security with OpenSSL", O'Reilly, ISBN 10: 0-596-00270-X

You will find more information in IcsSslHowTo.txt file.


Commercial SSL certificates:
To avoid browsers giving certificate warning messages, you need to purchase
a SSL certificate from one of numerous companies, such as Verisign, Thawte
GeoTrust or RapidSSL.  Prices vary dramatically and are often cheaper from
resellers such as Servertastic than from the main issuing companies.

The main purpose of an SSL certificate is to prove the identity of the owner
of a web site, ideally the company behind the web site.  That usually requires
paper work identifying the company is submitted and also proof the domain being
protected is owned by that company, it usually also involves telephone calls.
Such certificates are usually called fully validated and cost $120 or more each
year for a single domain, ie secure.website.com.  Wild card certificates cost
$350 or more, but protect multiple sub-domains, ie web.website.com as well.
Extended Validation certificates cost from $450 a year, and show the company name
in green in the address bar.  For testing and simple use, instant issued SSL
certificates cost from $15 per year and protect a single domain only with automated
checking reducing the cost (an email to admin@website.com to prove you receive
email for the domain, perhaps a telephone call as well).  Note these instant
certificates do not include a company name.

To buy and install an SSL certificate for use with ICS and OpenSSL follow these
steps:

1 - Build the SSL demo project OverbyteIcsPemTool. Take Extras, Create Certificate
Requests, fill in the various fields (check other certificates if uncertain, the
Common Name is the domain to protect, ie secure.website.com and E-Mail should be
an email address at the than domain, ideally admin or administrator, 2048 bits.
Click Create, and specify two file names, first for the private key (mykey.pem)
then the certificate request file (myreq.pem).  The request can also be done using
OpenSSL command line arguments, or you can build it into your own application.

2 - Choose you SSL supplier and certificate type, at some point during the ordering
process you will be asked for the certificate request, so open the PEM file you
saved with a text editor and copy the base64 encoded block starting
-BEGIN CERTIFICATE REQUEST- into the web form.  It should be decoded and displayed
so you check it's correct.  The private key is not needed for the certificate to
be issued.  At this point the validation process starts as mentioned above, which
might take hours or weeks to complete.

3 - Eventually the SSL certificate should be issued, either by email or made
available to download from the supplier's web site.  It should be in X.509 format
in a base64 encoded block starting -BEGIN CERTIFICATE- which should be saved as
a PEM file (mycert.pem).  There should also be an Intermediate CA certificate,
with which your new certificate was signed, which should also be saved as a file
(mycacert.pem). This may also be downloadable from the supplier as a bundle file
and should be common to any certificates they issue, ie RapidSSL_CA_bundle.pem.

4 - The OverbyteIcsPemTool tool has a View PEM button that allows examination of
your new PEM files.

5 - The three PEM files now need to be attached to the SslContext component in
your application, with properties SslCertFile, SslPrivKeyFile and SslCAFile.
The request certificate file has no further use.


Support:
--------
There is a mailing list to discuss F. Piette's components and applications.
To subscribe surf to http://lists.elists.org/mailman/listinfo/twsocket.
Do not use an aliased EMail address, use your real EMail address, the one
you'll use to post messages. After asking for subscription, you'll receive a
confirmation email you must reply to it or you will _not_ be added to the
subscriber's list (this is to check for email path and also make sure
someone doesn't subscribe you without your consent).

Once you have been registered with the mailing list processor, you can
send messages to twsocket@elists.org. Every subscriber will receive a copy of
your message. I will respond, but anybody is welcome to respond to each
other's messages. So every body can share his expertise. There are many other
useful mailing lists at http://www.elists.org !

Before asking a question, browse the message archive you can download from
the support page on the website (click the "support" button from main page)
and from the mailing list website http://lists.elists.org/mailman/listinfo/twsocket.
Google is also archiving the list with some delay.

If you found a bug, please make a short program that reproduces the problem
attach it to a message addressed to me. If I can reproduce the problem, I
can find a fix ! Do not send exe file but just source code and instructions.
Always use the latest version (beta if any) before reporting any bug.

You are also encouraged to use the support mailing list to ask for
enhancements. You are welcome to post your own code.

The support mailing list has sometimes a heavy traffic. If
it is too much for you, you can select "digest" mode in which mailing list
processor will mail you only one big message per day. To select digest mode
goto http://lists.elists.org/mailman/listinfo/twsocket.

You can also subscribe to another mailing list called twsocket-announce which
will receive only very few messages when major bug fixes or updates are done.
The subscription process is the same as for the other mailing list.
See above procedure.


Release notes
-------------

There is no global release notes. Each component and sample has his own history.
You can find those histories in the comment in the beginning of each source file.
There are also a bunch of useful comments in the source code. You should at least
browse the source for the components you are interested in.


MidWare
-------
If you wants to build client/server applications using TCP/IP protocol, you
can do it easily with ICS. But you can do it much more easily using another
freeware product from François Piette: MidWare. Available from the same web
site http://www.overbyte.be.


francois.piette@overbyte.be
francois.piette@swing.be
http://www.overbyte.be
