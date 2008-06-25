ICS-SSL COMPONENTS AND DEMOS (BETA)
==========================================


* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
*                                                                         *
*              THIS IS NOT FREEWARE NOR OPEN SOURCE SOFTWARE              *
*                DO NOT DISTRIBUTE OR SHARE IT WITH OTHERS                *
*      YOU MUST FINANCIALLY CONTRIBUTE TO THE ICS-SSL EFFORT TO USE IT    *
*                                                                         *
* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


LEGAL ISSUES:
-------------
Copyright (C) 2003-2007 by François PIETTE
Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
mailto:francois.piette@overbyte.be
http://www.overbyte.be

SSL implementation includes code written by Arno Garrels,
Berlin, Germany, contact: <arno.garrels@gmx.de>

This software is provided 'as-is', without any express or
implied warranty.  In no event will the author be held liable
for any  damages arising from the use of this software.

This code is _NOT_ freeware nor Open Source.
To use it, you must financially contribute to the development.
See SSL page at http://www.overbyte.be for details.

Once you got the right to use this software, you can use in your
own applications only. Distributing the source code or compiled
units or packages is prohibed.

As this code make use of OpenSSL, your rights are restricted by
OpenSSL license. See http://www.openssl.org for details.

Further, the following restrictions applies:

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


CERTIFICATES:
-------------

To make use of SSL, you need certificate. I provide some demo
certificate I built using command line OpenSSL tool. PEM 
certificates can opened by a text editor, LF as well as CRLF
are allowed as line breaks.

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
               wellknown root CA certificates to be spezified in
               property CA Path of the demo applications. Read
               the comments included in this file.
6F6359FC.0 :   Located in sub directory SslInternet\TrustedCaStore,
               it's the file CACERT.PEM stored with a hashed file
               name. Directory TrustedCaStore can be spezified in
               property CA Path of the demo applications.                                   

For details about certificate, see the book:
  "Network security with OpenSSL", O'Reilly
  

INSTALLATION:
-------------

This is beta software. Backup your disk before using it !

If any previous version of ICS-V6 is installed it's recommended to first
uninstall it from the IDE and to rename its install directory to some name
not in any search path or the library path.

Unzip the archive to a new directory, restoring the directory tree present
in the zip file. If there was a previous version installed rename the new
directory to the old name. 

In the global options make sure the library path includes an entry pointing 
to <install directory>\Delphi\Vc32. Any entry pointing to different 
locations containing ICS-V6 files should be removed from the library path.  


DELPHI

There are project group files for Delphi 7, BDS2006 Delphi personality
and Delphi 2007. The SSL demo projects and project group files are located
in sub directory SslInternet. In this project groups, you'll find a package
you must first compile and install before opening the demos, further more
define USE_SSL in each SSL demo's project options before building it (D7).


BDS2006 BCB PERSONALITY

Build and install the BCB package Delphi\Vc32\OverbyteIcsSslBcb100Package.bdsproj.
Currently there are no C++ SSL demos included. 
  


HOW TO USE:
-----------

To use the SSL enabled components, you must have LIBEAY32.DLL and SSLEAY32.DLL
available somewhere in your path. See http://www.openssl.org for instructions,
licensing and source code.

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

If you need help, please avoid mailing me directly. Instead, use the ICS-SSL
mailing list to ask for your questions. You can subscribe by pointing your
browser to http://www.elists.org/mailman/listinfo/ics-ssl


NOTES:
------

This version requires OpenSsl version 0.98e! If you need to support 
older OpenSsl versions as well (not recommended) define symbol 
BEFORE_OSSL_098E in OverbyteIcsSslDefs.inc and rebuild all.


François PIETTE
mailto:francois.piette@overbyte.be
Revised: June 24, 2007
