{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  Automatically download SSL X509 certifcates from various
              issuers, including free certificates from Let's Encrypt, and
              commercial certificates from CertCentre AG and Servertastic.
              Supports and ACME V1 and V2 protocols, and REST protocols
              for specific vendors.  Domain validated certificates should
              generally be issued without internvention, other commercial
              certificates may take days to be approved.
Creation:     Apr 2018
Updated:      Apr 2018
Version:      8.53
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
Legal issues: Copyright (C) 1997-2018 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
              <francois.piette@overbyte.be>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

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


Overview
--------






Updates:
Apr 05, 2018  - 8.53 - baseline



}

unit OverbyteIcsSslX509Certs;

{$I Include\OverbyteIcsDefs.inc}

{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}
{$B-}             { Enable partial boolean evaluation   }
{$T-}             { Untyped pointers                    }
{$X+}             { Enable extended syntax              }
{$H+}             { Use long strings                    }
{$IFDEF BCB}
    {$ObjExportAll On}
{$ENDIF}

interface

uses
{$IFDEF MSWINDOWS}
    {$IFDEF RTL_NAMESPACES}Winapi.Messages{$ELSE}Messages{$ENDIF},
    {$IFDEF RTL_NAMESPACES}Winapi.Windows{$ELSE}Windows{$ENDIF},
//    {$IFDEF RTL_NAMESPACES}System.IniFiles{$ELSE}IniFiles{$ENDIF},
{$ENDIF}
{$IFDEF POSIX}
    Posix.Time,
    Ics.Posix.WinTypes,
    Ics.Posix.Messages,
{$ENDIF}
    {$Ifdef Rtl_Namespaces}System.Classes{$Else}Classes{$Endif},
    {$Ifdef Rtl_Namespaces}System.Sysutils{$Else}Sysutils{$Endif},
    {$IFDEF RTL_NAMESPACES}System.TypInfo{$ELSE}TypInfo{$ENDIF},
//    {$IFDEF Rtl_Namespaces}System.StrUtils{$ELSE}StrUtils{$ENDIF},
    Overbyteicsssleay, Overbyteicslibeay,
{$IFDEF FMX}
//    Ics.Fmx.OverbyteIcsWndControl,
    Ics.Fmx.OverbyteIcsWSocket,
{$ELSE}
//    OverbyteIcsWndControl,
    OverbyteIcsWSocket,
{$ENDIF FMX}
    OverbyteIcsTypes,
    OverbyteIcsUtils,
    OverbyteIcsSslX509Utils,
    OverbyteIcsHttpProt,
    OverbyteIcsSslHttpRest,
    OverbyteIcsUrl,
    OverbyteIcsMimeUtils,
//    OverbyteIcsCharsetUtils,
    OverbyteIcsSuperObject,
    OverbyteIcsSslJose;

{ NOTE - these components only build with SSL, there is no non-SSL option }

{$IFDEF USE_SSL}

implementation


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$ENDIF USE_SSL}

end.
