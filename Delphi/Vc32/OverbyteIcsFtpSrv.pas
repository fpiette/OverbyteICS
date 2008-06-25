{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  TFtpServer class encapsulate the FTP protocol (server side)
              See RFC-959 for a complete protocol description.
Creation:     April 21, 1998
Version:      6.01
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1998-2007 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@overbyte.be>
              SSL implementation includes code written by Arno Garrels,
              Berlin, Germany, contact: <arno.garrels@gmx.de>

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

History:
Apr 29, 1998  V0.90 released for beta testing.
May 01, 1998  V0.92 Adapted for Delphi 1.0
May 03, 1998  V0.93 Adapted for Delphi 2.0 and C++Builder
May 04, 1998  V0.94 Use '/' or '\' as path delimiter. Expose only '/' to the
              outside. Stripped any telnet options (IE send two !). Handled
              absolute path. Implemented SIZE and REST commands.
              Added support for UNC (not finished !)
May 06, 1998  V0.95 Corrected spurious 226 message on PASV mode STOR.
              Made GetInteger retunrs a LongInt.
              Use a LongInt for N in CommandPORT (needed for 16 bits)
              Added slash substitution in BuildFilePath command.
Jul 09, 1998  V1.00 Adapted for Delphi 4, removed beta status.
Jul 21, 1998  V1.01 Added OnValidateDele event
              Changed function to get file size (do not open the file)
Feb 14, 1999  V1.02 Replaced straight winsock call by indirect calls thru
              wsocket (this provide runtime link to winsock DLL).
Mar 06, 1999  V1.03 Added code from  Plegge, Steve <jsp@nciinc.com> to add
              APPE, XMKD, KRMD and STRU commands support.
Jul 24, 1999  V1.04 Replaced msgStorDisabled value from '500 Cannot STOR.' to
              '501 Permission Denied' because CuteFTP doesn't like error 500.
              Suggested by Cedric Veilleux <webmaster@smashweb.com>.
Aug 20, 1999  V1.05 Added compile time options. Revised for BCB4.
              Added Addr property to select interface in multihomed computers.
Oct 02, 1999  V1.06 Added OnValidateRnFr and OnValidateRnTo events.
              Initialized Allowed variable to TRUE before triggerValidateDele.
Nov 24, 1999  V1.07 Added MTDM support. Thanks to Bruce Christensen
              <bkc51831234@hotmail.com> for his code.
Jan 24, 2000  V1.08 Patch IE5 bug in file names. Thanks to <dsnake@infonie.fr>
Jun 08, 2000  V1.09 Added 'A N' type for type command for AIX systems.
Oct 25, 2000  V1.10 Exposed list of clients thru Client[] property.
Oct 29, 2000  V1.11 Added IsClient() method.
              Implemented OnValidateRmd event.
Nov 01, 2000  V1.12 Implemented proposals from Carl@Smotricz.com:
              (1) support for MODE command, but only the default do-nothing
              option S. (2) binding the data socket to the local host address
              and port 20 ('ftp-data'). (3) detection of failure to open the
              data connection for STOR or RETR.
              Added option wsoNoReceiveLoop to sockets. See comments in TWSocket
              about this option. Help in very fast LAN.
Nov 11, 2000  V1.13 Checked for DOS attack. Close connection when buffer
              overflow occured. Thanks to Lester <les@lester.co.uk> for finding
              this security hole.
Jun 18, 2001  V1.14 Fixed file left open when storing and client broken data
              connection. Thanks to Davie <smatters@smatters.com>
Jul 27, 2001  V1.15 I fixed a race condition between WMFtpSrvClientClosed and
              WMFtpSrvCloseData found by Matthew Comb <matt@filesafe.co.nz> who
              worked with Davie <smatters@smatters.com>. Now WMFtpSrvCloseData
              receive Client in LParam and check if client is still in client
              list.
              Fixed a but with resumed put. Thanks Yvan Turkan iturcan@gamo.sk !
              Added a procedure to disconnect a single client.
              Changed all Exception by FtpServerException.
              Changed all "Error" by "AError" to avoid conflict with global var.
              Added Client.ID property to uniquely indentify the client. Pass
              this ID along with all posted messages and verify if the correct
              client still exists when message is processed.
Jul 30, 2001  V1.16 Added same check as above for WMFtpSrvCloseData.
Sep 09, 2001  V1.17 Eric Pascual <e.pascual@cstb.fr> added Store Unique (STOU)
              command.
Feb 26, 2002  V1.18 Fastream Technologies (http://www.fastream.com) found a bug
              in Disconnect and DisconnectAll which prevented data connection
              to be closed and client component to be destroyed.
Jul 06, 2002  V1.19 Fastream Technologies (http://www.fastream.com) fixed
              CommandXPWD and CommandPWD to make the path in answer as
              "/c:/windows" instead of "c:/windows" which is more compatible
              with the UNIX standard that most clients expect.
Sep 16, 2002  V1.20 Added OnValidateSize event.
              Allowed "REST 0" as a valid command.
Sep 17, 2002  V1.21 Sven Schmidts <sven.schmidts@nusec.de> added partional FEAT
              command, must extended, because I doesn't know what commands are
              special-featured.
Oct 26, 2002  V1.22 Introduced OnBuildFilePath to allow component use to change
              the file path on the fly.
              Thanks to Serge Chelli <serge@aceinformatique.com> who proposed
              this change.
Nov 01, 2002  V1.23 When client request passive mode, select a port from a
              range of ports instead of letting the OS choose one. This ease the
              use of a FTP server behind a firewall. Passive mode transferts
              will use port in the specified range.
              Also implemented fixed IP for passive mode.
              Thanks to Ian Tuck <ituck@noglobalborders.com> for code base.
Nov 06, 2002  V1.24 Added definition for PBoolean which is missing in some
              older Delphi version and in BCB.
Nov 11, 2002  V1.25 Revised for Delphi 1
Jan 26, 2003  V1.26 ByteCount fix. Thanks to wilfried@mestdagh.biz and
              fastream@fastream.com for the fix.
Sep 15, 2003  V1.27 Added ICSDEF feature to the source code. Thanks to Marco
              van de Voort <marcov@stack.nl> for his help.
Nov 01, 2003  V1.28 Corrected FormatUnixDirEntry for files greater than 2GB.
Dec 15, 2003  V1.29 Changed ClientRetrSessionConnected to check if file exists
              to avoid TStream exception opening a non existant file.
Jan 15, 2004  V1.30 Made BuildFilePath virtual.
Feb 16, 2004  V1.31 Andreas Mueller <Amueller@Nord-Vision.de> updated
              CommandRNFR and CommandRNTO to handle directories.
Feb 24, 2004  V1.32 Wilfried changed Close by Shutdown(1) in WMFtpSrvCloseData.
Mar 06, 2004  V1.33 Added DirectoryExists function for Delphi below V5
May 26, 2004  V1.34 Added support for hidden files. Thanks to Martin Koberstein
              <MKoberstein@nord-vision.de>.
Jun 07, 2004  V1.35 Fixed DirExists to "see" hidden directories. This
              permit deletion of hidden directories
Jun 08, 2004  V1.36 Removed DirectoryExists function and used DirExists instead.
Jul 23, 2004  V1.37 Added type keyword to "TFtpString = type String;"
Aug 6, 2004   V1.38 Angus Robertson, angus@magsys.co.uk added new Options property
              added MDTM YYYYMMDDHHMMSS support (set file mod date)
              added MLST and MLSD commands for better file listings
              CWD now returns 550 if new directory does not exist and Options=ftpsCWDCheck
              changing to a higher level directory than HomeDir is blocked if Options=ftpsCdupHome
              corrected DirExists to strip trailing backslash so it works
Aug 19, 2004  V1.39 Angus Robertson, corrected Options=ftpsCWDCheck to allow
              root (c:\)
              Options passed to Client as ftpCwdCheck, ftpCdupHome so they
              can be changed per client
              MDTM checks logged-in, new trigger before changing file time stamp
              Added MFMT modify file modification time (same as
              MDTM YYYYMMDDHHMMSS but draft RFC'd)
              (not yet supporting MFCT create time or MFF file facts commands)
              Added MD5 command which returns hash of file content to allow
              corruption check
              (not yet supporting MMD5 multiple file or XMD5 file range commands)
Sep 08, 2004 V1.40 MD5 has been renamed to IcsMD5
Oct 20, 2004 V1.41 Angus Robertson, MLSD command failed in passive mode
Mar 11, 2005 V1.42 Marco van de Voort <marcov@stack.nl> updated the component
             to be compatible with NOFORMS concept.
             He implemented FtpSrvAllocateHWnd and FtpSrvDeallocateHWnd based
             on TWSocket versions.
             Angus Robertson, using ftpCwdCheck and ftpcUNC allow CWD to change
             to root
Sept 6, 2005 V1.43 64-bit support for Delphi 6 and later, for transfers larger
             than 2 gigs, added error handling for failed seeks and TStream issues
             by Angus Robertson, angus@magsys.co.uk
Oct 21, 2005 V1.44 Arno Garrels added SSL features.
Dec 29, 2005 V1.45 Peter Feldbaumer feldbaumer@feldtech.com fixed excessive
             226-response for cancelled passive data-connections. He also
             fixed ByteCount handling, unified passive data-connection-setup.
Dec 30, 2005 V1.46 Arno Garrels added IcsLogger.
Jan 18, 2006 V1.47 TLS/SSL related changes.
Aug 6, 2006  V1.48 using GetWinsockErr in wsocket to give consistent textual and
             numeric winsock errors, by Angus (some constant literals changed from %d to %s)
             wsocket fix for 64-bit transfers with range checking enabled
             for address in use error, report port in proper decimal
             SSL check Self = TSslFtpServer before accessing SSL properties (Arno)
Aug 31, 2006 V1.49 A.Garrels reworked 64-bit streams support.
Sep 20, 2006 V1.50 A.Garrels implemented smarter MD5 calculation.
             How it works: On new uploads new option ftpsCalcMD5OnTheFly forces
             calculation of the MD5 sum in chunks in ClientStorDataAvailable.
             New property Md5UseThreadFileSize determines whether the checksum
             is calculated in a blocking manner or inside a worker thread
             when FTP command MD5 is processed. Therefore I introduced a new
             ProcessingThread in TFtpCtrlSocket that may be used for any
             lengthy processing inside the component.
             New event OnMD5Calculated triggers either when the internal MD5
             calculation finished or when the sum needs to be updated, an
             empty parameter Md5Sum signals to delete a possibly cached entry
             except the file has been renamed.
Oct 27, 2006 V1.51 A.Garrels made the command table a dynamic array. Some
             improvements with PasvIpAddr: New options ftpsNoPasvIpAddrInLan
             and ftpsNoPasvIpAddrSameSubnet. New event OnPasvIpAddr.
Dec 05, 2006 Fixed FreeCurrentPasvPort
May 09, 2007 V1.52 changes by A.Garrels. Added two new events (sponsored by
             Fastream Technologies). OnEnterSecurityContext and
             OnLeaveSecurityContext make it possible to switch Windows'
             security context to the context of the logged client. New option
             ftpsHidePhysicalPath. Changed/fixed the STOU command. Fixed
             some security issues: If ftpCdUphome is in the options it's no
             longer possible to change, list, remove, rename or create
             directories above the home directory. Removed trailing
             slash from response-paths except upon root directories.
             Note: ftpsHidePhysicalPath is ignored unless ftpsCdUphome is also
             set.
June 11, 2007 V1.53 MDTM command failed with a directory name.
             Andreas Haas <andreas.haas@ops.de>
             Angus Robertson, MFMT command now supports millisecs when updating
             file time stamp (because MFMD command already returned millisecs)
             Note: sysutils FileAge functions used only supports round seconds
             Angus Robertson, Passive IP 0.0.0.0 now raises exception.
Dec 04, 2007 V1.54 added more FEAT extensions, by Angus Robertson, angus@magsys.co.uk
               Note: some FEATs only reported if new events are created
             added support for One Time Passwords (aka S/Key), otp-md5, otp-md4 and otp-sha1,
               see RFC2289, uses new events OnOtpMethodEvent and OnOtpGetPasswordEvent
               (see OverbyteIcsFtpServ1 demo for OTP usage, ignore events for no OTP)
             added timeouts to close sockets on inactivity, new properties
               TimeoutSecsLogin (default 60 seconds), TimeoutSecsIdle (300) and
               TimeoutSecsXfer (900, same as IIS/4) before client is closed with
               421 answer. Note timeout is checked using a timer event once every
               five seconds so not second accurate. This event could be used for other jobs.
               Uses new event OnTimeout which can reset the timeout if needed
             added Clnt command to accept client information, calls new event
                onClntStr and sets Client.ClntStr
             added Allo command to return disk space allocation (before upload)
               checks and returns space on user's volume, but only says OK if
               AlloExtraSpace (default 1 Byte) still left as well, calls OnValidateAllo
               which may check space allocated for a specific account
             added Comb command to allow multiple upload files to be combined,
               used new event onCombine (where the actual file handling should be added)
             added Site Pswd command to change the account password, uses new
               event onSitePswd where the old and new passwords may be parsed
             added Site Exec command to execute a progam, uses new event
               onSiteExec which can decide whether the program should be run, and do it
             added Site Index command to generate a recusive directory and file name
               listing (no date or size) on the control channel, supported by Serv-U
             added Site Zone command to return the server time zone difference from
               UTC (GMT) which the client may use to adjust file listing time stamps
             added SiteMsg command to accept a message to the server, uses
               new event onSiteMsg
             added Site Dmlsd command, similar to MLSD but optional argument
               -SUBDIRS or -R for recursive directories, the path may be quoted if
               it includes spaces, the listing file names includes paths
             added Site Cmlsd command, similar to Site Dmlsd but uses control channel
               to avoid lots of small data channels sessions
             NOTE: Site Dmlsd and SiteCmlsd are new commands supported only by ICS
                FTP Server and may be disabled by removing the option ftpsSiteXmlsd
             the LIST, NLST, MLSD commands now also support -R for recursive
               sub-directores and a quoted file name
             recursive subdirectories are processsed in a thread unless option
                ftpsThreadRecurDirs is removed, ftpsThreadAllDirs similarly for all lists
             added Xcrc command to generate hash optional start and end positions,
               uses new events OnCalculateCrc and OnCrcCalculated, uses
               MD5UseThreadFileSize to check if a thread is used
             added Xmd5 command to generate hash optional start and end positions,
               used existing events OnCalculateMd5 and OnMd5Calculated
             added Mode Z and Opts commands and support for ZLIB compression if
                option ftpsModeZCompress set for server and ftpModeZCompress not disabled
                for client, ZlibMinLevel (1) and ZlibMaxLevel (9) properties restrict
                min and max compress stategies (max takes longer), ZlibNoCompExt is
                list of file extensions which compress with level 0 means no compress,
                defaults to '.zip;.rar;.7z;.cab;.lzh;.gz;.avi;.wmv;.mpg;.mp3;.jpg;.png;',
                ZlibWorkDir runtime property is path where temporary ZLIB files are written
                  defaults to '(systmppath)\icsftpsrv\'
                ZlinMinSpace property is minimum space on ZlibWorkDir for Mode Z to
                  be allowed, defaults to 50MByte
             added onDisplay event for server to log special information
             now reporting upload and download performance via OnDisplay event
             added various Client counters that may be used in the events to track
               server and account usage and performance, TotGetBytes and TotPutBytes
               are cumulative totals for the client which should be read in
               onClientDisconnect, SessStartTick is when client started, SessIdInfo
               may be set with client account info during logon, ReqStartTick is when
               the last request started and ReqDurMilliSecs is set to it's duration
               when the request finishes, XferStartTick is when an upload or download
               connected and starting sending data.
               Note: OverbyteIcsFtpSrvT has new functions for tick processing and timing
             moved building file directory functions to OverbyteIcsFtpSrvC so they
               can be used from client thread
             moved slash/backslash functions to OverbyteIcsFtpSrvT
             using Arno's TBufferedFileStream for improved performance
09 Dec 2007 V1.55 mode z bug fix for resumed transfers, by Angus Robertson, angus@magsys.co.uk
             added ftpsModeZNoResume option to disable resume while in Mode Z
             added ZlibMaxSize property to restrict maximum size of file that can compressed
             added callback in zlib funcs to update LastTick
06 Jan 2008 V1.56 corrected timer interval, timeout improvements
             passive port pool now issues incrementing ports instead of the sames ones
03 Mar 2008 V1.57 added SrvFileModeRead and SrvFileModeWrite as public so share locking
               can be changed, use SrvFileModeRead for MD5SUM (not locked)
            ensure file stream closed if session terminates unexpectedly  
Mar 24, 2008 V6.01 Bumped version number to 6.01
             Francois Piette made some changes to prepare code for Unicode.
Angus pending -
CRC on the fly
MD5 on the fly for downloads if not cached already
bandwidth restrictions
test app - cache zlib files and CRCs and lock updates



 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsFtpSrv;

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$I OverbyteIcsDefs.inc}
{$DEFINE USE_BUFFERED_STREAM} { V1.54 }
{$DEFINE USE_MODEZ}           { V1.54 }
{$IFDEF DELPHI6_UP}
    {$WARN SYMBOL_PLATFORM   OFF}
    {$WARN SYMBOL_LIBRARY    OFF}
    {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
{$IFNDEF VER80}   { Not for Delphi 1                    }
    {$H+}         { Use long strings                    }
    {$J+}         { Allow typed constant to be modified }
{$ENDIF}
{$IFDEF BCB3_UP}
    {$ObjExportAll On}
{$ENDIF}

{$DEFINE BIND_FTP_DATA}

interface

uses
    Messages,
{$IFDEF USEWINDOWS}
    Windows,
{$ELSE}
    WinTypes, WinProcs,
{$ENDIF}
{$IFNDEF NOFORMS}
    Forms,
{$ENDIF}
{$IFNDEF NO_DEBUG_LOG}
    OverbyteIcsLogger,
{$ENDIF}
{$IFDEF USE_SSL}
    OverByteIcsSSLEAY,
{$ENDIF}
    SysUtils, Classes,
{$IFDEF WIN32}
{$IFDEF BCB}
    Winsock,
{$ENDIF}
{$ENDIF}
{$IFDEF USE_BUFFERED_STREAM}    { angus V1.54 }
    OverbyteIcsStreams,
{$ENDIF}
{$IFDEF USE_MODEZ}              { angus V1.54 }
    {$I OverbyteIcsZlib.inc}
    OverbyteIcsZlibHigh,
    {$IFDEF USE_ZLIB_OBJ}
        OverbyteIcsZLibObj,     {interface to access ZLIB C OBJ files}
    {$ELSE}
        OverbyteIcsZLibDll,     {interface to access zLib1.dll}
    {$ENDIF}
{$ENDIF}
    OverbyteIcsWndControl,
    OverbyteIcsWinsock,
    OverbyteIcsWinsock2, { AG V1.51 }
    OverbyteIcsWSocket,
    OverbyteIcsFtpSrvC,
    OverbyteIcsFtpSrvT,
    OverbyteIcsOneTimePw,  { angus V1.54 }
    OverbyteIcsCRC,        { angus V1.54 }
    OverbyteIcsMD5;

const
    FtpServerVersion         = 601;
    CopyRight : String       = ' TFtpServer (c) 1998-2008 F. Piette V6.01 ';
    UtcDateMaskPacked        = 'yyyymmddhhnnss';         { angus V1.38 }

type
    TFtpsOption      = (ftpsCwdCheck, ftpsCdupHome,      { angus V1.38 }
                        ftpsCalcMD5OnTheFly,             { AG V1.50 }
                        ftpsCalcCRCOnTheFly,             { angus V1.54 }
                        ftpsNoPasvIpAddrInLan,           { AG V1.51 }
                        ftpsNoPasvIpAddrSameSubnet,      { AG V1.51 }
                        ftpsHidePhysicalPath,            { AG V1.52 }
                        ftpsModeZCompress,               { angus V1.54 }
                        ftpsSiteXmlsd,                   { angus V1.54 }
                        ftpsThreadRecurDirs,             { angus V1.54 }
                        ftpsThreadAllDirs,               { angus V1.54 }
                        ftpsModeZNoResume                { angus V1.55 }
                         );
    TFtpsOptions     = set of TFtpsOption;               { angus V1.38 }

    PBoolean = ^Boolean;
    FtpServerException  = class(Exception);
{ Various Delphi and C++Builder version handle string parameter passed as var }
{ differently. To get application code compatible across all versions, we     }
{ need to define our own string type. We use the larger we can with the given }
{ compiler version. btw: the 255 limit is not a problem because it applies to }
{ the command lines sent to the server and 255 should be enough except if     }
{ you use incredibly long file names.                                         }
{$IFDEF DELPHI3_UP}
    TFtpString = type String;
{$ELSE}
    TFtpString = String[255];
{$ENDIF}
{$IFDEF VER80}
    WPARAM = WORD;
    LPARAM = DWORD;
{$ENDIF}
    TFtpCtrlSocketClass = class of TFtpCtrlSocket;
    TFtpSrvAuthenticateEvent  =  procedure (Sender   : TObject;
                                            Client   : TFtpCtrlSocket;
                                            UserName : TFtpString;
                                            Password : TFtpString;
                                            var Authenticated : Boolean) of object;
    TFtpSrvOtpMethodEvent  =  procedure (Sender   : TObject;                      { angus V1.54 }
                                         Client   : TFtpCtrlSocket;
                                         UserName : TFtpString;
                                         var OtpMethod : TOtpMethod) of object;
    TFtpSrvOtpGetPasswordEvent =  procedure (Sender           : TObject;
                                             Client           : TFtpCtrlSocket;
                                             UserName         : TFtpString;
                                             var UserPassword : String) of object; { angus V1.54 }
    TFtpSrvChangeDirectoryEvent =  procedure (Sender      : TObject;
                                              Client      : TFtpCtrlSocket;
                                              Directory   : TFtpString;
                                              var Allowed : Boolean) of object;
    TFtpSrvBuildDirectoryEvent =  procedure (Sender        : TObject;
                                             Client        : TFtpCtrlSocket;
                                             var Directory : TFtpString;
                                             Detailed      : Boolean) of object;
    TFtpSrvClientConnectEvent = procedure (Sender  : TObject;
                                           Client  : TFtpCtrlSocket;
                                           AError  : Word) of object;
    TFtpSrvDataSessionConnectedEvent = procedure (Sender  : TObject;
                                                  Client  : TFtpCtrlSocket;
                                                  Data    : TWSocket;
                                                  AError  : Word) of object;
    TFtpSrvClientCommandEvent = procedure (Sender        : TObject;
                                           Client        : TFtpCtrlSocket;
                                           var Keyword   : TFtpString;
                                           var Params    : TFtpString;
                                           var Answer    : TFtpString) of object;
    TFtpSrvAnswerToClientEvent = procedure (Sender        : TObject;
                                            Client        : TFtpCtrlSocket;
                                            var Answer    : TFtpString) of object;
    TFtpSrvValidateXferEvent  = procedure (Sender        : TObject;
                                           Client        : TFtpCtrlSocket;
                                           var FilePath  : TFtpString;
                                           var Allowed   : Boolean) of object;
    TFtpSrvCalculateMd5Event  = procedure (Sender        : TObject;        { angus V1.39 }
                                           Client        : TFtpCtrlSocket;
                                           var FilePath  : TFtpString;
                                           var Md5Sum    : TFtpString;
                                           var Allowed   : Boolean) of object;
    TFtpSrvMd5CalculatedEvent = procedure (Sender         : TObject;       { AG V1.50 }
                                           Client         : TFtpCtrlSocket;
                                           const FilePath : TFtpString;
                                           const Md5Sum   : TFtpString) of object;
    TFtpSrvOnPasvIpAddrEvent = procedure  (Sender : TObject;               { AG V1.51 }
                                           Client : TFtpCtrlSocket;
                                           var APasvIpAddr: TFtpString;
                                           var SetPasvIpAddr : Boolean) of object;
    TFtpSrvBuildFilePathEvent = procedure (Sender        : TObject;
                                           Client        : TFtpCtrlSocket;
                                           const Directory   : String;
                                           const FileName    : String;
                                           var   NewFileName : String) of object;
    TFtpSrvDataAvailableEvent = procedure (Sender : TObject;
                                           Client : TFtpCtrlSocket;
                                           Data   : TWSocket;
                                           Buf    : PChar;
                                           Len    : LongInt;
                                           AError : Word) of object;
    TFtpSrvRetrDataSentEvent  = procedure (Sender : TObject;
                                           Client : TFtpCtrlSocket;
                                           Data   : TWSocket;
                                           AError : Word) of object;
    TFtpSrvGetUniqueFileNameEvent = procedure (Sender       : TObject;
                                               Client       : TFtpCtrlSocket;
                                               var FileName : TFtpString) of object;
    TFtpSrvGetProcessingEvent     = procedure (Sender          : TObject;
                                               Client          : TFtpCtrlSocket;
                                               var DelayedSend : Boolean) of object;
    TFtpSrvCommandProc        = procedure (Client        : TFtpCtrlSocket;
                                           var Keyword   : TFtpString;
                                           var Params    : TFtpString;
                                           var Answer    : TFtpString) of object;
    TFtpSrvCommandTableItem   = record
                                    KeyWord : String;
                                    Proc    : TFtpSrvCommandProc;
                                end;
    TFtpSecurityContextEvent  = procedure (Sender : TObject;     { AG V1.52 }
                                           Client : TFtpCtrlSocket) of object;
    TFtpSrvGeneralEvent = procedure (Sender        : TObject;      { angus V1.54 }
                                     Client        : TFtpCtrlSocket;
                                     var Params    : TFtpString;
                                     var Answer    : TFtpString) of object;
    TFtpSrvTimeoutEvent =  procedure (Sender      : TObject;               { angus V1.54 }
                                      Client      : TFtpCtrlSocket;
                                      Duration    : Integer;
                                      var Abort   : Boolean) of object;
    TFtpSrvCompressFileEvent  = procedure (Sender        : TObject;        { angus V1.54 }
                                           Client        : TFtpCtrlSocket;
                                           var Done      : Boolean) of object;
    TFtpSrvCompressedFileEvent = procedure (Sender       : TObject;        { angus V1.54 }
                                            Client       : TFtpCtrlSocket) of object;
    TFtpSrvDisplayEvent = procedure (Sender        : TObject;      { angus V1.54 }
                                     Client        : TFtpCtrlSocket;
                                     Msg           : TFtpString) of object;

    TFtpServer = class(TIcsWndControl)
    protected
        FAddr                   : String;
        FPort                   : String;
        FBanner                 : String;
        FServSocket             : TWSocket;
        FClientClass            : TFtpCtrlSocketClass;
        FClientList             : TList;
        FClientNum              : LongInt;
        FMaxClients             : LongInt;
        FCmdTable               : array of TFtpSrvCommandTableItem;  { AG V1.51 }
        FLastCmd                : Integer;
        FUserData               : LongInt;      { Reserved for component user }
        FPasvPortRangeStart     : Integer;
        FPasvPortRangeSize      : Integer;
        FPasvPortTable          : PBoolean;
        FPasvPortTableSize      : Integer;
        FPasvIpAddr             : String;
        FPasvNextNr             : Integer;      { angus V1.56 }
        FMd5UseThreadFileSize   : Integer;      { AG V1.50 }
        FTimeoutSecsLogin       : Integer;      { angus V1.54 }
        FTimeoutSecsIdle        : Integer;      { angus V1.54 }
        FTimeoutSecsXfer        : Integer;      { angus V1.54 }
        FEventTimer             : TIcsTimer;    { angus V1.54 }
        FZlibMinLevel           : Integer;      { angus V1.54 }
        FZlibMaxLevel           : Integer;      { angus V1.54 }
        FZlibNoCompExt          : String;       { angus V1.54 }
        FZlibWorkDir            : String;       { angus V1.54 }
        FZlibMinSpace           : Integer;      { angus V1.54 }
        FAlloExtraSpace         : Integer;      { angus V1.54 }
        FZlibMaxSize            : Int64;        { angus V1.55 }
        FMsg_WM_FTPSRV_CLOSE_REQUEST  : UINT;
        FMsg_WM_FTPSRV_CLIENT_CLOSED  : UINT;
        FMsg_WM_FTPSRV_ABORT_TRANSFER : UINT;
        FMsg_WM_FTPSRV_CLOSE_DATA     : UINT;
        FMsg_WM_FTPSRV_START_SEND     : UINT;
        FOnStart                : TNotifyEvent;
        FOnStop                 : TNotifyEvent;
        FOnAuthenticate         : TFtpSrvAuthenticateEvent;
        FOnOtpMethod            : TFtpSrvOtpMethodEvent;           { angus V1.54 }
        FOnOtpGetPassword       : TFtpSrvOtpGetPasswordEvent;      { angus V1.54 }
        FOnClientConnect        : TFtpSrvClientConnectEvent;
        FOnClientDisconnect     : TFtpSrvClientConnectEvent;
        FOnClientCommand        : TFtpSrvClientCommandEvent;
        FOnAnswerToClient       : TFtpSrvAnswerToClientEvent;
        FOnChangeDirectory      : TFtpSrvChangeDirectoryEvent;
        FOnMakeDirectory        : TFtpSrvChangeDirectoryEvent;
        FOnBuildDirectory       : TFtpSrvBuildDirectoryEvent;
        FOnAlterDirectory       : TFtpSrvBuildDirectoryEvent;
        FOnValidatePut          : TFtpSrvValidateXferEvent;
        FOnValidateSize         : TFtpSrvValidateXferEvent;
        FOnValidateDele         : TFtpSrvValidateXferEvent;
        FOnValidateRmd          : TFtpSrvValidateXferEvent;
        FOnValidateRnFr         : TFtpSrvValidateXferEvent;
        FOnValidateRnTo         : TFtpSrvValidateXferEvent;
        FOnStorSessionConnected : TFtpSrvDataSessionConnectedEvent;
        FOnStorSessionClosed    : TFtpSrvDataSessionConnectedEvent;
        FOnStorDataAvailable    : TFtpSrvDataAvailableEvent;
        FOnValidateGet          : TFtpSrvValidateXferEvent;
        FOnRetrSessionConnected : TFtpSrvDataSessionConnectedEvent;
        FOnRetrSessionClosed    : TFtpSrvDataSessionConnectedEvent;
        FOnRetrDataSent         : TFtpSrvRetrDataSentEvent;
        FOnGetUniqueFileName    : TFtpSrvGetUniqueFileNameEvent;
        FOnGetProcessing        : TFtpSrvGetProcessingEvent;
        FOnBuildFilePath        : TFtpSrvBuildFilePathEvent; { serge le 5/10/2002 }
        FOnValidateMfmt         : TFtpSrvValidateXferEvent;  { angus V1.39 }
        FOnCalculateMd5         : TFtpSrvCalculateMd5Event;  { angus V1.39 }
        FOnCalculateCrc         : TFtpSrvCalculateMd5Event;  { angus V1.54 }
        FOptions                : TFtpsOptions;
        FOnMd5Calculated        : TFtpSrvMd5CalculatedEvent; { AG V1.50 }
        FOnCrcCalculated        : TFtpSrvMd5CalculatedEvent; { angus V1.54 }
        FOnPasvIpAddr           : TFtpSrvOnPasvIpAddrEvent;  { AG V1.51 }
        FOnEnterSecurityContext : TFtpSecurityContextEvent;  { AG V1.52 }
        FOnLeaveSecurityContext : TFtpSecurityContextEvent;  { AG V1.52 }
        FOnValidateAllo         : TFtpSrvGeneralEvent;       { angus V1.54 }
        FOnClntStr              : TFtpSrvGeneralEvent;       { angus V1.54 }
        FOnSiteMsg              : TFtpSrvGeneralEvent;       { angus V1.54 }
        FOnSiteExec             : TFtpSrvGeneralEvent;       { angus V1.54 }
        FOnSitePaswd            : TFtpSrvGeneralEvent;       { angus V1.54 }
        FOnCombine              : TFtpSrvGeneralEvent;       { angus V1.54 }
        FOnTimeout              : TFtpSrvTimeoutEvent;       { angus V1.54 }
        FOnDownCompressFile     : TFtpSrvCompressFileEvent;  { angus V1.54 }
        FOnUpCompressFile       : TFtpSrvCompressFileEvent;  { angus V1.54 }
        FOnUpCompressedFile     : TFtpSrvCompressedFileEvent; { angus V1.54 }
        FOnDisplay              : TFtpSrvDisplayEvent;       { angus V1.54 }
{$IFNDEF NO_DEBUG_LOG}
        function  GetIcsLogger: TIcsLogger;                                      { V1.46 }
        procedure SetIcsLogger(const Value: TIcsLogger);                         { V1.46 }
        procedure DebugLog(LogOption: TLogOption; const Msg : string); virtual;  { V1.46 }
        function  CheckLogOptions(const LogOption: TLogOption): Boolean; virtual;{ V1.46 }
{$ENDIF}
        procedure ClientProcessingThreadTerminate(Sender : TObject); { AG V1.50 }
        procedure Notification(AComponent: TComponent; operation: TOperation); override;
        procedure ServSocketSessionAvailable(Sender : TObject; AError  : Word);
        procedure ServSocketStateChange(Sender : TObject; OldState, NewState : TSocketState);
        procedure ClientSessionClosed(Sender : TObject; AError  : Word);
        procedure ClientDataSent(Sender : TObject; AError  : Word); virtual; { V1.47 }
        procedure ClientCommand(Sender : TObject; CmdBuf : PChar; CmdLen : Integer);
        procedure ClientPassiveSessionAvailable(Sender : TObject; AError  : Word); virtual; {AG SSL}
        procedure ClientStorSessionConnected(Sender : TObject; AError  : Word);
        procedure ClientStorSessionClosed(Sender : TObject; AError  : Word);
        procedure ClientStorDataAvailable(Sender: TObject; AError  : word); virtual;
        procedure ClientRetrSessionConnected(Sender : TObject; AError  : Word); virtual;
        procedure ClientRetrSessionClosed(Sender : TObject; AError  : Word);
        procedure ClientRetrDataSent(Sender : TObject; AError  : Word);
        procedure SendAnswer(Client : TFtpCtrlSocket; Answer : TFtpString);  virtual; {AG SSL}
        procedure SendNextDataChunk(Client : TFtpCtrlSocket; Data : TWSocket); virtual;
        procedure StartSendData(Client : TFtpCtrlSocket);
        procedure PrepareStorDataSocket(Client : TFtpCtrlSocket);
        procedure PreparePassiveStorDataSocket(Client : TFtpCtrlSocket);
        procedure PreparePassiveRetrDataSocket(Client : TFtpCtrlSocket);
        function  IsPathAllowed(Client : TFtpCtrlSocket; const Path : String;
                                ExcludeBackslash : Boolean = FALSE): Boolean; { V1.52 AG}
        procedure BuildDirectory(Client : TFtpCtrlSocket; var Path : TFtpString); { angus V1.54 }
        procedure EventTimerOnTimer(Sender : TObject);                            { angus V1.54 }

        procedure TriggerServerStart; virtual;
        procedure TriggerServerStop; virtual;
        procedure TriggerAuthenticate(Client            : TFtpCtrlSocket;
                                      UserName          : String;
                                      PassWord          : String;
                                      var Authenticated : Boolean); virtual;
        procedure TriggerOtpMethod   (Client   : TFtpCtrlSocket;
                                      UserName : TFtpString;
                                      var OtpMethod : TOtpMethod); virtual; { angus V1.54 }
        procedure TriggerOtpGetPassword(Client           : TFtpCtrlSocket;
                                        UserName         : TFtpString;
                                        var UserPassword : String); virtual; { angus V1.54 }
        procedure TriggerChangeDirectory(Client         : TFtpCtrlSocket;
                                         Directory      : String;
                                         var Allowed    : Boolean); virtual;
        procedure TriggerMakeDirectory(Client         : TFtpCtrlSocket;
                                       Directory      : String;
                                       var Allowed    : Boolean); virtual;
        procedure TriggerBuildDirectory(Client        : TFtpCtrlSocket;
                                        var Params    : TFtpString;
                                        Detailed      : Boolean); virtual;
        procedure TriggerAlterDirectory(Client        : TFtpCtrlSocket;
                                        var Params    : TFtpString;
                                        Detailed      : Boolean); virtual;
        procedure TriggerSendAnswer(Client : TFtpCtrlSocket;
                                    var Answer : TFtpString); virtual;
        procedure TriggerClientConnect(Client : TFtpCtrlSocket; AError  : Word); virtual;
        procedure TriggerClientDisconnect(Client : TFtpCtrlSocket; AError  : Word); virtual;
        procedure TriggerClientCommand(Client      : TFtpCtrlSocket;
                                       var Keyword : TFtpString;
                                       var Params  : TFtpString;
                                       var Answer  : TFtpString); virtual;
        procedure TriggerStorSessionConnected(Client : TFtpCtrlSocket;
                                              Data   : TWSocket;
                                              AError : Word); virtual;
        procedure TriggerStorSessionClosed(Client : TFtpCtrlSocket;
                                           Data   : TWSocket;
                                           AError : Word); virtual;
        procedure TriggerValidatePut(Client        : TFtpCtrlSocket;
                                     var FilePath  : TFtpString;
                                     var Allowed   : Boolean); virtual;
        procedure TriggerValidateSize(Client        : TFtpCtrlSocket;
                                      var FilePath  : TFtpString;
                                      var Allowed   : Boolean); virtual;
        procedure TriggerValidateDele(Client        : TFtpCtrlSocket;
                                      var FilePath  : TFtpString;
                                      var Allowed   : Boolean); virtual;
        procedure TriggerValidateRmd(Client        : TFtpCtrlSocket;
                                     var FilePath  : TFtpString;
                                     var Allowed   : Boolean); virtual;
        procedure TriggerValidateRnFr(Client        : TFtpCtrlSocket;
                                      var FilePath  : TFtpString;
                                      var Allowed   : Boolean); virtual;
        procedure TriggerValidateRnTo(Client        : TFtpCtrlSocket;
                                      var FilePath  : TFtpString;
                                      var Allowed   : Boolean); virtual;
        procedure TriggerRetrSessionConnected(Client : TFtpCtrlSocket;
                                              Data   : TWSocket;
                                              AError : Word); virtual;
        procedure TriggerRetrSessionClosed(Client : TFtpCtrlSocket;
                                           Data   : TWSocket;
                                           AError : Word); virtual;
        procedure TriggerValidateGet(Client        : TFtpCtrlSocket;
                                     var FilePath  : TFtpString;
                                     var Allowed   : Boolean); virtual;
        procedure TriggerStorDataAvailable(Client : TFtpCtrlSocket;
                                       Data   : TWSocket;
                                       Buf    : PChar;
                                       Len    : LongInt;
                                       AError : Word); virtual;
        procedure TriggerRetrDataSent(Client : TFtpCtrlSocket;
                                      Data   : TWSocket;
                                      AError : Word); virtual;
        procedure TriggerGetUniqueFileName(Client       : TFtpCtrlSocket;
                                           var FileName : TFtpString); virtual;
        procedure TriggerBuildFilePath(Client            : TFtpCtrlSocket;
                                       const Directory   : String;
                                       const FileName    : String;
                                       var   NewFileName : String); virtual;
        procedure TriggerValidateMfmt(Client        : TFtpCtrlSocket;   { angus V1.39 }
                                      var FilePath  : TFtpString;
                                      var Allowed   : Boolean); virtual;
        procedure TriggerCalculateMd5 (Client        : TFtpCtrlSocket;   { angus V1.39 }
                                      var FilePath  : TFtpString;
                                      var Md5Sum    : TFtpString;
                                      var Allowed   : Boolean); virtual;
        procedure TriggerMd5Calculated(Client         : TFtpCtrlSocket;  { AG V1.50 }
                                      const FilePath  : TFtpString;
                                      const Md5Sum    : TFtpString); virtual;
        procedure TriggerCalculateCrc (Client        : TFtpCtrlSocket;   { angus V1.54 }
                                      var FilePath  : TFtpString;
                                      var Md5Sum    : TFtpString;
                                      var Allowed   : Boolean); virtual;
        procedure TriggerCrcCalculated(Client         : TFtpCtrlSocket;  { angus V1.54 }
                                      const FilePath  : TFtpString;
                                      const Md5Sum    : TFtpString); virtual;
        procedure TriggerEnterSecurityContext(Client : TFtpCtrlSocket); virtual; { AG V1.52 }
        procedure TriggerLeaveSecurityContext(Client : TFtpCtrlSocket); virtual; { AG V1.52 }
        procedure TriggerValidateAllo (Client      : TFtpCtrlSocket;           { angus V1.54 }
                                       var Params  : TFtpString;
                                       var Answer  : TFtpString); virtual;
        procedure TriggerClntStr      (Client      : TFtpCtrlSocket;           { angus V1.54 }
                                       var Params  : TFtpString;
                                       var Answer  : TFtpString); virtual;
        procedure TriggerSiteMsg      (Client      : TFtpCtrlSocket;           { angus V1.54 }
                                       var Params  : TFtpString;
                                       var Answer  : TFtpString); virtual;
        procedure TriggerSiteExec     (Client      : TFtpCtrlSocket;           { angus V1.54 }
                                       var Params  : TFtpString;
                                       var Answer  : TFtpString); virtual;
        procedure TriggerSitePaswd    (Client      : TFtpCtrlSocket;           { angus V1.54 }
                                       var Params  : TFtpString;
                                       var Answer  : TFtpString); virtual;
        procedure TriggerCombine      (Client      : TFtpCtrlSocket;           { angus V1.54 }
                                       var Params  : TFtpString;
                                       var Answer  : TFtpString); virtual;
        procedure TriggerTimeout      (Client      : TFtpCtrlSocket;            { angus V1.54 }
                                       Duration    : Integer;
                                       var Abort   : Boolean); virtual;
        procedure TriggerDownCompressFile (Client    : TFtpCtrlSocket;          { angus V1.54 }
                                           var Done  : Boolean); virtual;
        procedure TriggerUpCompressFile (Client    : TFtpCtrlSocket;            { angus V1.54 }
                                         var Done  : Boolean); virtual;
        procedure TriggerUpCompressedFile (Client  : TFtpCtrlSocket); virtual;  { angus V1.54 }
        procedure TriggerDisplay      (Client      : TFtpCtrlSocket;
                                       Msg         : TFtpString); virtual;  { angus V1.54 }

        function BuildFilePath(Client      : TFtpCtrlSocket;
                               Directory   : String;
                               FileName    : String) : String; virtual;
        function  GetClientCount : Integer; virtual;
        function  GetClient(nIndex : Integer) : TFtpCtrlSocket; virtual;
{ !!!!!!!!!!!!!!!! NGB: Added next two lines }
        procedure FreeCurrentPasvPort(AClient : TFtpCtrlSocket);
        function  GetNextAvailablePasvPort : String;
{ !!!!!!!!!!!!!!!! NGB: Added last two lines }
        function  GetActive : Boolean;
        procedure SetActive(newValue : Boolean);
        procedure SetPasvPortRangeSize(const NewValue: Integer);
        procedure SetPasvPortRangeStart(const NewValue: Integer);
        procedure AddCommand(const Keyword : String;
                             const Proc : TFtpSrvCommandProc); virtual;
        procedure WMFtpSrvCloseRequest(var msg: TMessage); virtual;
        procedure WMFtpSrvClientClosed(var msg: TMessage); virtual;
        procedure WMFtpSrvAbortTransfer(var msg: TMessage); virtual;
        procedure WMFtpSrvCloseData(var msg: TMessage); virtual;
        procedure WMFtpSrvStartSend(var msg: TMessage); virtual;
        procedure CommandDirectory(Client      : TFtpCtrlSocket;
                                   var Keyword : TFtpString;
                                   var Params  : TFtpString;
                                   var Answer  : TFtpString;
                                   Detailed    : Boolean); virtual;
        procedure CommandDirectory2(Client      : TFtpCtrlSocket;
                                   var Keyword : TFtpString;
                                   var Params  : TFtpString;
                                   var Answer  : TFtpString;
                                   ListType    : TListType);
        procedure CommandUSER(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandPASS(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandQUIT(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandNOOP(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandLIST(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandNLST(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandDELE(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandSIZE(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandREST(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandRNFR(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandRNTO(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandPORT(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandSTOR(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandRETR(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandTYPE(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandCWD (Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandChangeDir(Client : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandMKD (Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandRMD (Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandCDUP(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandXPWD(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandPWD (Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandSYST(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandABOR(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandPASV(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandAPPE(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandSTRU(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandMDTM(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandMODE(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandOverflow(Client  : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandSTOU(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandFEAT(Client      : TFtpCtrlSocket;
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandMLST(Client      : TFtpCtrlSocket;   { angus V1.38 }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandMLSD(Client      : TFtpCtrlSocket;   { angus V1.38 }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandMD5 (Client      : TFtpCtrlSocket;   { angus V1.39 }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandXCRC (Client     : TFtpCtrlSocket;   { angus V1.54 }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandALLO (Client     : TFtpCtrlSocket;   { angus V1.54 }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandCLNT (Client     : TFtpCtrlSocket;   { angus V1.54 }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandOPTS (Client     : TFtpCtrlSocket;   { angus V1.54 }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandSitePaswd (Client : TFtpCtrlSocket;   { angus V1.54 }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandSiteExec (Client : TFtpCtrlSocket;   { angus V1.54 }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandSiteIndex (Client : TFtpCtrlSocket;   { angus V1.54 }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandSiteZone (Client : TFtpCtrlSocket;   { angus V1.54 }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandSiteMsg (Client  : TFtpCtrlSocket;   { angus V1.54 }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandSiteCmlsd (Client : TFtpCtrlSocket;   { angus V1.54 }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandSiteDmlsd (Client : TFtpCtrlSocket;   { angus V1.54 }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandComb (Client     : TFtpCtrlSocket;   { angus V1.54 }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;

    public
        SrvFileModeRead     : Word;   { angus V1.57 }
        SrvFileModeWrite    : Word;   { angus V1.57 }
        constructor Create(AOwner: TComponent); override;
        destructor  Destroy; override;
        procedure   Start;
        procedure   Stop;
        procedure   Disconnect(Client : TFtpCtrlSocket);
        procedure   DisconnectAll;
        procedure   DoStartSendData(Client: TFtpCtrlSocket; var Answer : TFtpString); virtual;
        procedure   AllocateMsgHandlers; override;
        procedure   FreeMsgHandlers; override;
        function    MsgHandlersCount: Integer; override;
        procedure   WndProc(var MsgRec: TMessage); override;
        { Check  if a given object is one of our clients }
        function    IsClient(SomeThing : TObject) : Boolean;
        function    OpenFileStream(const FileName: string; Mode: Word): TStream;    { angus V1.54 }
        procedure   CloseFileStreams(Client : TFtpCtrlSocket);                      { angus V1.54 }
        property  ServSocket    : TWSocket            read  FServSocket;
        property  ClientCount   : Integer             read  GetClientCount;
        property  Active        : Boolean             read  GetActive
                                                      write SetActive;
        property  ClientClass            : TFtpCtrlSocketClass
                                                      read  FClientClass
                                                      write FClientClass;
        { Client[] give direct access to anyone of our clients }
        property  Client[nIndex : Integer] : TFtpCtrlSocket
                                                      read  GetClient;
        property  ZlibWorkDir            : String     read  FZlibWorkDir    { angus V1.54 }
                                                      write FZlibWorkDir;
    published
{$IFNDEF NO_DEBUG_LOG}
        property IcsLogger              : TIcsLogger  read  GetIcsLogger  { V1.46 }
                                                      write SetIcsLogger;
{$ENDIF}
        property  Addr                   : String     read  FAddr
                                                      write FAddr;
        property  Port                   : String     read  FPort
                                                      write FPort;
        property  Banner                 : String     read  FBanner
                                                      write FBanner;
        property  UserData               : LongInt    read  FUserData
                                                      write FUserData;
        property  MaxClients             : LongInt    read  FMaxClients
                                                      write FMaxClients;
        property  PasvIpAddr             : String     read  FPasvIpAddr
                                                      write FPasvIpAddr;
        property  PasvPortRangeStart     : Integer    read  FPasvPortRangeStart
                                                      write SetPasvPortRangeStart;
        property  PasvPortRangeSize      : Integer    read  FPasvPortRangeSize
                                                      write SetPasvPortRangeSize;
        property  Options                : TFtpsOptions
                                                      read  FOptions
                                                      write FOptions;
        property  MD5UseThreadFileSize   : Integer    read  FMd5UseThreadFileSize
                                                      write FMd5UseThreadFileSize;
        property  TimeoutSecsLogin       : Integer    read FTimeoutSecsLogin
                                                      write FTimeoutSecsLogin; { angus V1.54 }
        property  TimeoutSecsIdle        : Integer    read FTimeoutSecsIdle
                                                      write FTimeoutSecsIdle;  { angus V1.54 }
        property  TimeoutSecsXfer        : Integer    read FTimeoutSecsXfer
                                                      write FTimeoutSecsXfer;  { angus V1.54 }
        property  ZlibMinLevel           : Integer    read FZlibMinLevel
                                                      write FZlibMinLevel;   { angus V1.54 }
        property  ZlibMaxLevel           : Integer    read FZlibMaxLevel
                                                      write FZlibMaxLevel;   { angus V1.54 }
        property  ZlibNoCompExt          : String     read  FZlibNoCompExt
                                                      write FZlibNoCompExt;  { angus V1.54 }
        property  AlloExtraSpace         : Integer    read  FAlloExtraSpace
                                                      write FAlloExtraSpace; { angus V1.54 }
        property  ZlibMinSpace           : Integer    read  FZlibMinSpace
                                                      write FZlibMinSpace;   { angus V1.54 }
        property  ZlibMaxSize            : Int64      read  FZlibMaxSize
                                                      write FZlibMaxSize ;   { angus V1.55 }
        property  OnStart                : TNotifyEvent
                                                      read  FOnStart
                                                      write FOnStart;
        property  OnStop                 : TNotifyEvent
                                                      read  FOnStop
                                                      write FOnStop;
        property  OnAuthenticate         : TFtpSrvAuthenticateEvent
                                                      read  FOnAuthenticate
                                                      write FOnAuthenticate;
        property  OnOtpMethod            : TFtpSrvOtpMethodEvent     { angus V1.54 }
                                                      read FOnOtpMethod
                                                      write FOnOtpMethod;
        property  OnOtpGetPassword       : TFtpSrvOtpGetPasswordEvent     { angus V1.54 }
                                                      read FOnOtpGetPassword
                                                      write FOnOtpGetPassword;
        property  OnClientDisconnect     : TFtpSrvClientConnectEvent
                                                      read  FOnClientDisconnect
                                                      write FOnClientDisconnect;
        property  OnClientConnect        : TFtpSrvClientConnectEvent
                                                      read  FOnClientConnect
                                                      write FOnClientConnect;
        property  OnClientCommand        : TFtpSrvClientCommandEvent
                                                      read  FOnClientCommand
                                                      write FOnClientCommand;
        property  OnAnswerToClient       : TFtpSrvAnswerToClientEvent
                                                      read  FOnAnswerToClient
                                                      write FOnAnswerToClient;
        property  OnChangeDirectory      : TFtpSrvChangeDirectoryEvent
                                                      read  FOnChangeDirectory
                                                      write FOnChangeDirectory;
        property  OnMakeDirectory        : TFtpSrvChangeDirectoryEvent
                                                      read  FOnMakeDirectory
                                                      write FOnMakeDirectory;
        property  OnBuildDirectory       : TFtpSrvBuildDirectoryEvent
                                                      read  FOnBuildDirectory
                                                      write FOnBuildDirectory;
        property  OnAlterDirectory       : TFtpSrvBuildDirectoryEvent
                                                      read  FOnAlterDirectory
                                                      write FOnAlterDirectory;
        property  OnStorSessionConnected : TFtpSrvDataSessionConnectedEvent
                                                      read  FOnStorSessionConnected
                                                      write FOnStorSessionConnected;
        property  OnRetrSessionConnected : TFtpSrvDataSessionConnectedEvent
                                                      read  FOnRetrSessionConnected
                                                      write FOnRetrSessionConnected;
        property  OnStorSessionClosed    : TFtpSrvDataSessionConnectedEvent
                                                      read  FOnStorSessionClosed
                                                      write FOnStorSessionClosed;
        property  OnRetrSessionClosed    : TFtpSrvDataSessionConnectedEvent
                                                      read  FOnRetrSessionClosed
                                                      write FOnRetrSessionClosed;
        property  OnRetrDataSent         : TFtpSrvRetrDataSentEvent
                                                      read  FOnRetrDataSent
                                                      write FOnRetrDataSent;
        property  OnValidatePut          : TFtpSrvValidateXferEvent
                                                      read  FOnValidatePut
                                                      write FOnValidatePut;
        property  OnValidateSize         : TFtpSrvValidateXferEvent
                                                      read  FOnValidateSize
                                                      write FOnValidateSize;
        property  OnValidateDele         : TFtpSrvValidateXferEvent
                                                      read  FOnValidateDele
                                                      write FOnValidateDele;
        property  OnValidateRmd          : TFtpSrvValidateXferEvent
                                                      read  FOnValidateRmd
                                                      write FOnValidateRmd;
        property  OnValidateRnFr         : TFtpSrvValidateXferEvent
                                                      read  FOnValidateRnFr
                                                      write FOnValidateRnFr;
        property  OnValidateRnTo         : TFtpSrvValidateXferEvent
                                                      read  FOnValidateRnTo
                                                      write FOnValidateRnTo;
        property  OnValidateGet          : TFtpSrvValidateXferEvent
                                                      read  FOnValidateGet
                                                      write FOnValidateGet;
        property  OnStorDataAvailable    : TFtpSrvDataAvailableEvent
                                                      read  FOnStorDataAvailable
                                                      write FOnStorDataAvailable;
        property  OnGetUniqueFileName    : TFtpSrvGetUniqueFileNameEvent
                                                      read  FOnGetUniqueFileName
                                                      write FOnGetUniqueFileName;
        property  OnGetProcessing        : TFtpSrvGetProcessingEvent
                                                      read  FOnGetProcessing
                                                      write FOnGetProcessing;
        property  OnBuildFilePath        : TFtpSrvBuildFilePathEvent
                                                      read  FOnBuildFilePath
                                                      write FOnBuildFilePath;
        property  OnValidateMfmt         : TFtpSrvValidateXferEvent        { angus V1.39 }
                                                      read  FOnValidateMfmt
                                                      write FOnValidateMfmt;
        property  OnCalculateMd5         : TFtpSrvCalculateMd5Event        { angus V1.39 }
                                                      read  FOnCalculateMd5
                                                      write FOnCalculateMd5;
        property  OnMd5Calculated        : TFtpSrvMd5CalculatedEvent       { AG V1.50 }
                                                      read  FOnMd5Calculated
                                                      write FOnMd5Calculated;
        property  OnCalculateCrc         : TFtpSrvCalculateMd5Event        { angus V1.54 }
                                                      read  FOnCalculateCrc
                                                      write FOnCalculateCrc;
        property  OnCrcCalculated        : TFtpSrvMd5CalculatedEvent       { angus V1.54 }
                                                      read  FOnCrcCalculated
                                                      write FOnCrcCalculated;
        property  OnPasvIpAddr           : TFtpSrvOnPasvIpAddrEvent       { AG V1.51 }
                                                      read  FOnPasvIpAddr
                                                      write FOnPasvIpAddr;
        property  OnEnterSecurityContext : TFtpSecurityContextEvent { AG V1.52 }
                                                      read  FOnEnterSecurityContext
                                                      write FOnEnterSecurityContext;
        property  OnLeaveSecurityContext : TFtpSecurityContextEvent { AG V1.52 }
                                                      read  FOnLeaveSecurityContext
                                                      write FOnLeaveSecurityContext;
        property  OnValidateAllo         : TFtpSrvGeneralEvent          { angus V1.54 }
                                                      read  FOnValidateAllo
                                                      write FOnValidateAllo;
        property  OnClntStr              : TFtpSrvGeneralEvent          { angus V1.54 }
                                                      read  FOnClntStr
                                                      write FOnClntStr;
        property  OnSiteMsg              : TFtpSrvGeneralEvent          { angus V1.54 }
                                                      read  FOnSiteMsg
                                                      write FOnSiteMsg;
        property  OnSiteExec             : TFtpSrvGeneralEvent          { angus V1.54 }
                                                      read  FOnSiteExec
                                                      write FOnSiteExec;
        property  OnSitePaswd            : TFtpSrvGeneralEvent          { angus V1.54 }
                                                      read  FOnSitePaswd
                                                      write FOnSitePaswd;
        property  OnCombine              : TFtpSrvGeneralEvent          { angus V1.54 }
                                                      read  FOnCombine
                                                      write FOnCombine;
        property  OnTimeout              : TFtpSrvTimeoutEvent          { angus V1.54 }
                                                      read  FOnTimeout
                                                      write FOnTimeout;
        property  OnDownCompressFile     : TFtpSrvCompressFileEvent          { angus V1.54 }
                                                      read  FOnDownCompressFile
                                                      write FOnDownCompressFile;
        property  OnUpCompressFile       : TFtpSrvCompressFileEvent          { angus V1.54 }
                                                      read  FOnUpCompressFile
                                                      write FOnUpCompressFile;
        property  OnUpCompressedFile     : TFtpSrvCompressedFileEvent        { angus V1.54 }
                                                      read  FOnUpCompressedFile
                                                      write FOnUpCompressedFile;
        property  OnDisplay              : TFtpSrvDisplayEvent               { angus V1.54 }
                                                      read  FOnDisplay
                                                      write FOnDisplay;
    end;

{$IFDEF USE_SSL}
{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels
Description:  A component adding TLS/SSL support to TFtpServer.
              This unit contains the implementation for the component.
              It is included in FtpSrv.pas.pas unit when USE_SSL is defined.
              The interface part is in FtpSrvIntfSsl.inc.
              Make use of OpenSSL (http://www.openssl.org).              
              Make use of freeware TWSocket component from ICS.
Creation:     Oct 21, 2005
Version:      1.03
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
              francois.piette@rtfm.be      http://www.rtfm.be/fpiette
              francois.piette@pophost.eunet.be              
Support:      Use the mailing list ics-ssl@elists.org
              Follow "SSL" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2003 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@overbyte.be>
              SSL implementation includes code written by Arno Garrels,
              Berlin, Germany, contact: <arno.garrels@gmx.de>
              
              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              This code is _NOT_ freeware nor Open Source.
              To use it, you must financially contribute to the development.
              See SSL page on the author website for details.

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

History:


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}    
    TFtpSslType  = (ftpAuthSsl,      ftpAuthTls,     ftpAuthTlsP,
                    ftpAuthTlsC ,    ftpImplicitSsl);
    TFtpSslTypes = set of TFtpSslType;
    

    TSslFtpServer = class(TFtpServer)
    protected
        FFtpSslTypes                        : TFtpSslTypes;
        FOnSslHandshakeDone                 : TSslHandshakeDoneEvent;
        FOnSslVerifyPeer                    : TSslVerifyPeerEvent;
        FOnSslSvrGetSession                 : TSslSvrGetSession;
        FOnSslSvrNewSession                 : TSslSvrNewSession;
        FOnSslSetSessionIDContext           : TSslSetSessionIDContext;
        FMsg_WM_FTPSRV_ABORT_TRANSFER       : UINT;
        FMsg_WM_FTPSRV_Close_Data           : UINT;

        procedure ClientPassiveSessionAvailable(Sender : TObject;
                                                AError : Word); override;
        procedure ClientDataSent(Sender : TObject; AError : Word); override; { 1.03 }
        procedure TriggerClientConnect(Client        : TFtpCtrlSocket;
                                       AError        : Word); override;
        procedure SendAnswer(Client                  : TFtpCtrlSocket;
                             Answer                  : TFtpString); override;
        procedure TriggerStorSessionConnected(Client : TFtpCtrlSocket;
                                              Data   : TWSocket;
                                              AError : Word); override;
        procedure TriggerRetrSessionConnected(Client : TFtpCtrlSocket;
                                              Data   : TWSocket;
                                              AError : Word); override;
        function  GetSslContext : TSslContext;
        procedure SetSslContext(Value : TSslContext);
        procedure CommandCCC(Client       : TFtpCtrlSocket;   { 1.03 }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandPBSZ(Client      : TFtpCtrlSocket;   { 1.03 }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandAUTH(Client      : TFtpCtrlSocket;   { AG }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure CommandPROT(Client      : TFtpCtrlSocket;   { AG }
                              var Keyword : TFtpString;
                              var Params  : TFtpString;
                              var Answer  : TFtpString); virtual;
        procedure TransferSslVerifyPeer(Sender        : TObject;
                                        var Ok        : Integer;
                                        Cert          : TX509Base); virtual;
        procedure TransferSslHandshakeDone(Sender         : TObject;
                                           ErrCode        : Word;
                                           PeerCert       : TX509Base;
                                           var Disconnect : Boolean); virtual;
        procedure TransferSslSetSessionIDContext(Sender : TObject;
                                   var SessionIDContext : TSslSessionIdContext); virtual;
        procedure TransferSslSvrNewSession(Sender       : TObject;
                                        SslSession      : Pointer;
                                        SessId          : Pointer;
                                        Idlen           : Integer;
                                 var AddToInternalCache : Boolean); virtual;
        procedure TransferSslSvrGetSession(Sender          : TObject;
                                         var SslSession : Pointer;
                                         SessId         : Pointer;
                                         Idlen          : Integer;
                                         var IncRefCount: Boolean); virtual;
        procedure SetFtpSslTypes(const Value: TFtpSslTypes); { 1.04 }
    public
        constructor Create(AOwner: TComponent); override;
        function  MsgHandlersCount : Integer; override;
        procedure AllocateMsgHandlers; override;
        procedure FreeMsgHandlers; override;
        //destructor  Destroy; override;
    published
        property  SslContext         : TSslContext         read  GetSslContext
                                                           write SetSslContext;
        property  OnSslVerifyPeer    : TSslVerifyPeerEvent read  FOnSslVerifyPeer
                                                           write FOnSslVerifyPeer;
        property  OnSslSetSessionIDContext : TSslSetSessionIDContext
                                                           read  FOnSslSetSessionIDContext
                                                           write FOnSslSetSessionIDContext;
        property  OnSslSvrNewSession : TSslSvrNewSession   read  FOnSslSvrNewSession
                                                           write FOnSslSvrNewSession;
        property  OnSslSvrGetSession : TSslSvrGetSession   read  FOnSslSvrGetSession
                                                           write FOnSslSvrGetSession;
        property  OnSslHandshakeDone : TSslHandshakeDoneEvent
                                                           read  FOnSslHandshakeDone
                                                           write FOnSslHandshakeDone;
        property  FtpSslTypes        : TFtpSslTypes        read  FFtpSslTypes  { 1.03 }
                                                           write SetFtpSslTypes; { 1.04 }
    end;
{$ENDIF} // USE_SSL

function GetZlibCacheFileName(const S : String) : String;  { angus V1.54 }

procedure Register;

implementation

const
    msgSyntaxParam    = '501 Syntax error in parameter.';        { V1.52 AG }
    msgSyntaxParamFmt = '501 Syntax error in parameter: %s.';    { V1.52 AG }
    msgDftBanner      = '220 ICS FTP Server ready.';
    msgTooMuchClients = '421 Too many users connected.';
    msgCmdUnknown     = '500 ''%s'': command not understood.';
    msgLoginFailed    = '530 Login incorrect.';
    msgNotLogged      = '530 Please login with USER and PASS.';
    msgNoUser         = '503 Login with USER first.';
    msgLogged         = '230 User %s logged in.';
    msgPassRequired   = '331 Password required for %s.';
    msgOptRespRequired = '331 Response to %s required for %s.';   { angus V1.54 }
    msgCWDSuccess     = '250 CWD command successful. "%s" is current directory.';
    msgCWDFailed      = '501 CWD failed. %s';
    msgPWDSuccess     = '257 "%s" is current directory.';
    msgQuit           = '221 Goodbye.';
    msgPortSuccess    = '200 Port command successful.';
    msgPortFailed     = '501 Invalid PORT command.';
    msgStorDisabled   = '501 Permission Denied'; {'500 Cannot STOR.';}
    msgStorSuccess    = '150 Opening data connection for %s.';
    msgStorFailed     = '501 Cannot STOR. %s';
    msgStorAborted    = '426 Connection closed; %s.';
    msgStorOk         = '226 File received ok';
{   msgStorOk         = '226-Multiple lines answer'#13#10'  Test'#13#10#13#10'226 File received OK'; }
    msgStorError      = '426 Connection closed; transfer aborted. Error %s';
    msgRetrDisabled   = '500 Cannot RETR.';
    msgRetrSuccess    = '150 Opening data connection for %s.';
    msgRetrFailed     = '501 Cannot RETR. %s';
    msgRetrAborted    = '426 Connection closed; %s.';
    msgRetrOk         = '226 File sent ok';
    msgRetrError      = '426 Connection closed; transfer aborted. Error %s';
    msgRetrNotExists  = '550 ''%s'': no such file or directory.';     { angus V1.54 }
    msgRetrFileErr    = '451 Cannot open file: %s.';                  { angus V1.54 }
    msgSystem         = '215 UNIX Type: L8 Internet Component Suite';
    msgDirOpen        = '150 Opening data connection for directory list.';
    msgDirFailed      = '451 Failed: %s.';
    msgTypeOk         = '200 Type set to %s.';
    msgTypeFailed     = '500 ''TYPE %s'': command not understood.';
    msgDeleNotExists  = '550 ''%s'': no such file or directory.';
    msgDeleOk         = '250 File ''%s'' deleted.';
    msgDeleFailed     = '450 File ''%s'' can''t be deleted.';
    msgDeleSyntax     = msgSyntaxParam;//'501 Syntax error in parameter.';  { V1.52 AG }
    msgDeleDisabled   = '550 Cannot delete.';
    msgRnfrNotExists  = '550 ''%s'': no such file or directory.';
    msgRnfrSyntax     = msgSyntaxParam;//'501 Syntax error in parameter.';  { V1.52 AG }
    msgRnfrOk         = '350 File exists, ready for destination name.';
    msgRnFrDisabled   = '500 Cannot RNFR.';
    msgRntoNotExists  = '550 ''%s'': no such file or directory.';
    msgRntoAlready    = '553 ''%s'': file already exists.';
    msgRntoOk         = '250 File ''%s'' renamed to ''%s''.';
    msgRntoFailed     = '450 File ''%s'' can''t be renamed.';
    msgRntoSyntax     = msgSyntaxParam;//'501 Syntax error in parameter.';  { V1.52 AG }
    msgRnToDisabled   = '500 Cannot RNTO.';
    msgMkdOk          = '257 ''%s'': directory created.';
    msgMkdAlready     = '550 ''%s'': file or directory already exists.';
    msgMkdFailed      = '550 ''%s'': can''t create directory.';
    msgMkdSyntax      = msgSyntaxParam;//'501 Syntax error in parameter.';  { V1.52 AG }
    msgRmdOk          = '250 ''%s'': directory removed.';
    msgRmdNotExists   = '550 ''%s'': no such directory.';
    msgRmdFailed      = '550 ''%s'': can''t remove directory.';
    msgRmdDisabled    = '500 Cannot remove directory.';
    msgRmdSyntax      = msgSyntaxParam;//'501 Syntax error in parameter.';  { V1.52 AG }
    msgNoopOk         = '200 Ok. Parameter was ''%s''.';
    msgAborOk         = '225 ABOR command successful.';
    msgPasvLocal      = '227 Entering Passive Mode (127,0,0,1,%d,%d).';
    msgPasvRemote     = '227 Entering Passive Mode (%d,%d,%d,%d,%d,%d).';
    msgPasvExcept     = '500 PASV exception: ''%s''.';
    msgSizeOk         = '213 %d';
    msgSizeDisabled   = '501 Permission Denied';
    msgSizeFailed     = '550 Command failed: %s.';
    msgSizeSyntax     = msgSyntaxParam;//'501 Syntax error in parameter.';  { V1.52 AG }
    msgRestOk         = '350 REST supported. Ready to resume at byte offset %d.';
    msgRestZero       = '501 Required byte offset parameter bad or missing.';
    msgRestFailed     = msgSyntaxParamFmt;//'501 Syntax error in parameter: %s.'; { V1.52 AG }
    msgRestNotModeZ   = '501 REST not supported while using Mode Z';    { angus V1.55 } 
    msgAppeFailed     = '550 APPE failed.';
    msgAppeSuccess    = '150 Opening data connection for %s (append).';
    msgAppeDisabled   = '500 Cannot APPE.';
    msgAppeAborted    = '426 Connection closed; %s.';
    msgAppeOk         = '226 File received ok';
    msgAppeError      = '426 Connection closed; transfer aborted. Error %s';
    msgAppeReady      = '150 APPE supported.  Ready to append file "%s" at offset %d.';
    msgStruOk         = '200 Ok. STRU parameter ''%s'' ignored.';
    msgMdtmOk         = '213 %s';
    msgMdtmFailed     = '550 %s';
    msgMdtmSyntax     = '501 Syntax error in MDTM/MFMT parameter.';
    msgMdtmNotExists  = '550 ''%s'': no such file or directory.';
    msgModeOK         = '200 MODE %s Ok';                               { angus V1.54 add param }
    msgModeSyntax     = '501 Missing argument for MODE';
    msgModeNotS       = '502 MODE %s not supported';                    { angus V1.54 add param }
    msgOverflow       = '500 Command too long';
    msgStouOk         = '250 ''%s'': file created.';
    msgStouSuccess    = msgStorSuccess;
    msgStouFailed     = '501 Cannot STOU. %s';
    msgStouAborted    = msgStorAborted;
    msgStouError      = msgStorError;
    msgFeatFollows    = '211-Extensions supported:';
    msgFeatFollowDone = '211 END';
    msgFeatFailed     = '211 No-Features';
    msgMdtmChangeOK   = '253 Date/time changed OK';                  { angus V1.38 }
    msgMfmtChangeOK   = '213 Date/time changed OK';                  { angus V1.39 }
    msgMdtmChangeFail = '550 MDTM/MFMT cannot change date/time on this server';  { angus V1.38 }
    msgCWDNoDir       = '550 CWD Failed to change directory to %s';  { angus V1.38 }
    msgMlstFollows    = '250-Listing ';                              { angus V1.38 }
    msgMlstFollowDone = '250 END';                                   { angus V1.38 }
    msgMlstNotExists  = '550 ''%s'': no such file or directory.';    { angus V1.38 }
    msgMlstDenied     = '550 Access denied';                         { AG V1.52 }
    msgMd5NotFound    = '550 ''%s'': no such file.';                 { angus V1.39 }
    msgMd5Failed      = '550 MD5 SUM failed : ''%s''.';              { angus V1.39 }
    msgMd5Ok          = '251 "%s" %s';                               { angus V1.39 }
    msgTimeout        = '421 Connection closed, timed out after %d secs.'; { angus V1.54 }
    msgNotedOK        = '200 Noted OK.';                             { angus V1.54 }
    msgSiteZone       = '210 UTC%s';                                 { angus V1.54 }
    msgCrcOk          = '250 %s';                                    { angus V1.54 }
    msgCrcFailed      = '550 CRC failed : ''%s''.';                  { angus V1.54 }
    msgSiteFailed     = '550 SITE command failed.';                  { angus V1.54 }
    msgIndexFollows   = '200-Index %s';                              { angus V1.54 }
    msgIndexDone      = '200 END Index';                             { angus V1.54 }
    msgOtpsOK         = '200 %s Ok';                                 { angus V1.54 }
    msgOptsFailed     = '501 %s is invalid';                         { angus V1.54 }
    msgAlloOK         = '200 ALLO OK, %d bytes available';           { angus V1.54 }
    msgAlloFail       = '501 Invalid size parameter';                { angus V1.54 }
    msgAlloFull       = '501 Insufficient disk space, only %d bytes available';  { angus V1.54 }

{$IFDEF USE_SSL}
    msgAuthOk         = '234 Using authentication type %s';
    msgAuthDenied     = '502 %s authentication not allowed'; // SSL/TLS
    msgAuthYetSetOkV2 = '234 Auth type already set.';
    msgAuthYetSetOkV3 = msgAuthYetSetOkV2 + ' SSL re-negotiation allowed';
    //msgAuthYetSetErr  = '534 Auth type already set to %s';
    msgAuthInitError  = '431 Could not initialize %s connection';
    msgAuthNoSupport  = '504 Auth type ''%s'' not supported';
                        
    msgErrInSslOnly   = '533 %s requires a secure connection';
    msgProtOk         = '200 Protection level set to %s';
    msgProtNoSupport  = '504 Protection level ''%s'' not supported';
    msgProtUnknown    = '504 Protection level ''%s'' not recognized';
    msgErrSslInit     = 'Fatal error on initializing SSL';
    msgPbszOk         = '200 PBSZ set to 0';
    msgCccOk          = '200 CCC OK Continue using plaintext commands';
{$ENDIF}

{ 1.54 moved to FtpSrvT
function SlashesToBackSlashes(const S : String) : String; forward;
function BackSlashesToSlashes(const S : String) : String; forward;  }
{ function BuildFilePath(const Directory : String; serge le 5/10/2002
                         FileName        : String) : String; forward; }

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Register;
begin
    RegisterComponents('FPiette',
                      [TFtpServer
                  {$IFDEF USE_SSL}
                      , TSslFtpServer
                  {$ENDIF} 
                      ]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF VER80}
procedure SetLength(var S: String; NewLength: Integer);
begin
    S[0] := chr(NewLength);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TrimRight(Str : String) : String;
var
    i : Integer;
begin
    i := Length(Str);
    while (i > 0) and (Str[i] = ' ') do
        i := i - 1;
    Result := Copy(Str, 1, i);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TrimLeft(Str : String) : String;
var
    i : Integer;
begin
    if Str[1] <> ' ' then
        Result := Str
    else begin
        i := 1;
        while (i <= Length(Str)) and (Str[i] = ' ') do
            i := i + 1;
        Result := Copy(Str, i, Length(Str) - i + 1);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Trim(Str : String) : String;
begin
    Result := TrimLeft(TrimRight(Str));
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetFileSize(const FileName : String) : TFtpBigInt;           { V1.49 }
var
    SR : TSearchRec;
{$IFDEF STREAM64}   { V1.49 }
    TempSize: TULargeInteger ;  // 64-bit integer record
{$ENDIF}
begin
{$IFNDEF VER80}{$WARNINGS OFF}{$ENDIF}
    if FindFirst(FileName, faReadOnly or faHidden or
                 faSysFile or faArchive, SR) = 0 then begin
{$IFDEF STREAM64}         { V1.49 }
        TempSize.LowPart  := SR.FindData.nFileSizeLow;
        TempSize.HighPart := SR.FindData.nFileSizeHigh;
        Result := TempSize.QuadPart;
{$ELSE}
        Result := SR.Size;
{$ENDIF}
        FindClose(SR);     { V1.49 }
    end
    else
        Result := -1;
{$IFNDEF VER80}{$WARNINGS ON}{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function DirExists(const Dir : String) : Boolean;                 { V1.52 AG}
{ INVALID_HANDLE_VALUE = INVALID_FILE_ATTRIBUTES = DWORD(-1) }
var
    Res : DWORD;
begin
    Res := GetFileAttributes(PChar(Dir));
    Result := (Res <> INVALID_HANDLE_VALUE) and
              ((Res and FILE_ATTRIBUTE_DIRECTORY) <> 0);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function CreateUniqueFile(Dir, Prefix, Extension: String): String; { V1.52 AG}
var
    FileName  : String;
    I         : Integer;
    hFile     : THandle;
    Err       : DWord;
begin
    Result := '';
    Dir := Trim(Dir);
    if (Length(Dir) = 0) or (not DirExists(Dir)) then
        Exit;
    Dir := IncludeTrailingPathDelimiter(Dir);
    Prefix := Trim(Prefix);
    if Length(Prefix) > 3 then
        SetLength(Prefix, 3);
    Extension := Trim(Extension);
    Dir := Dir + Prefix + FormatDateTime('yymdh', Now);
    I   := 0;
    Err := ERROR_FILE_EXISTS; 
    while (Err = ERROR_FILE_EXISTS) and (I < MaxInt) do begin
        FileName := Dir + IntToStr(I) + Extension;
        if Length(FileName) > MAX_PATH then
            Break;
        hFile := CreateFile(PChar(FileName), GENERIC_READ or GENERIC_WRITE,
                            0, nil, CREATE_NEW, FILE_ATTRIBUTE_NORMAL, 0);
        if hFile <> INVALID_HANDLE_VALUE then begin
            CloseHandle(hFile);
            Result := FileName;
            Break;
        end
        else
            Err := GetLastError;
        Inc(I);
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetZlibCacheFileName(const S : String) : String;  { angus V1.54 }
var
    I : Integer;
    Ticks: String;
begin
    Result := Lowercase (S);
    for I := 1 to Length(Result) do begin
        if (Result [I] = '\') or (Result [I] = '.') or
                           (Result [I] = ':') then Result[I] := '_';
    end;
    Ticks := IntToStr(IcsGetTickCountX);  { now make it unique by adding some ms }
    I := Length(Ticks);
    if I < 6 then Ticks := '123' + Ticks; { if windows running short }
    Result := Result + '_' + Copy (Ticks, I-6, 6) + '.zlib';
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function FormatResponsePath(                                       { AG V1.52 }
    Client       : TFtpCtrlSocket;
    const InPath : TFtpString): String;
const
    Slash = '/';
var
    Home : String;
begin
    Result := InPath;
    if (ftpHidePhysicalPath in Client.Options) and
       (ftpCdUpHome in Client.Options) then begin
        Home := ExcludeTrailingPathDelimiter(Client.HomeDir);
        if Pos(LowerCase(Home), LowerCase(InPath)) = 1 then
            Result := Copy(InPath, Length(Home) + 1, Length(InPath));
    end;
    while (Length(Result) > 0) and (Result[Length(Result)] = '\') do
        SetLength(Result, Length(Result) - 1);
    if (Length(Result) = 0) then
        Result := Slash
    else begin
        Result := BackSlashesToSlashes(Result);
        if Result[Length(Result)] = ':' then
            Result := Result + Slash;
        if Result[1] <> Slash then
            Result := Slash + Result;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsSpace(Ch : Char) : Boolean;
begin
    Result := (Ch = ' ') or (Ch = #9);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsLetterOrDigit(Ch : Char) : Boolean;
begin
    Result := ((Ch >= 'a') and (Ch <= 'z')) or
              ((Ch >= 'A') and (Ch <= 'Z')) or
              ((Ch >= '0') and (Ch <= '9'));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function atosi(const value : String) : Integer;  { angus V1.38 signed integer, added "const", AG }
var
    i, j : Integer;
begin
    Result := 0;
    i := 1;
    while (i <= Length(Value)) and (Value[i] = ' ') do
        i := i + 1;
    j := i;
    while (i <= Length(Value)) and ((Value[i] = '+') or (Value[i] = '-')) do
        i := i + 1;
    while (i <= Length(Value)) and (Value[i] >= '0') and (Value[i] <= '9')do begin
        Result := Result * 10 + ord(Value[i]) - ord('0');
        i := i + 1;
    end;
    if j < Length(Value) then begin
        if value[j] = '-' then
            Result := -Result;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TFtpServer.Create(AOwner: TComponent);
var
    Buffer: array [0..1023] of Char ;
begin
    inherited Create(AOwner);
    //FWindowHandle       := ftpsrvAllocateHWnd(WndProc);
    AllocateHWnd;
    FServSocket         := TWSocket.Create(Self);
    FServSocket.Name    := 'ServerWSocket';
    FClientList         := TList.Create;
    FPort               := 'ftp';
    FAddr               := '0.0.0.0';
    FBanner             := msgDftBanner;
    FClientClass        := TFtpCtrlSocket;
    FOptions            := [ftpsThreadRecurDirs, ftpsSiteXmlsd] ;   { angus V1.54 }
    FMd5UseThreadFileSize   := 0;  { AG V1.50 }
    FTimeoutSecsLogin   := 60;      { angus V1.54 }
    FTimeoutSecsIdle    := 300;     { angus V1.54 }
    FTimeoutSecsXfer    := 900;     { angus V1.54 }
    FZlibMinLevel       := 1;       { angus V1.54 }
    FZlibMaxLevel       := 9;       { angus V1.54 }
    FZlibNoCompExt      := '.zip;.rar;.7z;.cab;.lzh;.gz;.avi;.wmv;.mpg;.mp3;.jpg;.png;'; { angus V1.54 }
    SetString (FZlibWorkDir, Buffer, GetTempPath (Sizeof (Buffer) - 1, Buffer)) ;        { angus V1.54 }
    FZlibWorkDir        := IncludeTrailingPathDelimiter (FZlibWorkDir) + 'icsftpsrv\' ;  { angus V1.54 }
    FZlibMinSpace       := 50000000;               { angus V1.54 50 Mbyte }
    FZlibMaxSize        := 500000000;              { angus V1.55 - 500 meg }
    FAlloExtraSpace     := 1000000;                { angus V1.54 1 Mbyte }
    FEventTimer         := TIcsTimer.Create(Self); { angus V1.54 }
    FEventTimer.Enabled := false;                  { angus V1.54 }
    FEventTimer.OnTimer := EventTimerOnTimer;      { angus V1.54 }
    FEventTimer.Interval := 5000;     { angus V1.56 only used for timeouts, slow }
    SrvFileModeRead     := fmOpenRead + fmShareDenyNone;         { angus V1.57 }
    SrvFileModeWrite    := fmOpenReadWrite or fmShareDenyWrite;  { angus V1.57 }

{ !!!!!!!!!!! NGB: Added next five lines }
    FPasvIpAddr         := '';
    FPasvPortRangeStart := 0;
    FPasvPortRangeSize  := 0;
    FPasvPortTable      := nil;
    FPasvPortTableSize  := 0;
{ !!!!!!!!!!! NGB: Added previous five lines }
    FPasvNextNr         := 0;  { angus V1.56 }
    SetLength(FCmdTable, ftpcLast + 1 + 5);
    AddCommand('PORT', CommandPORT);
    AddCommand('STOR', CommandSTOR);
    AddCommand('RETR', CommandRETR);
    AddCommand('CWD',  CommandCWD);
    AddCommand('XPWD', CommandXPWD);
    AddCommand('PWD',  CommandPWD);
    AddCommand('USER', CommandUSER);
    AddCommand('PASS', CommandPASS);
    AddCommand('LIST', CommandLIST);
    AddCommand('NLST', CommandNLST);
    AddCommand('TYPE', CommandTYPE);
    AddCommand('SYST', CommandSYST);
    AddCommand('QUIT', CommandQUIT);
    AddCommand('DELE', CommandDELE);
    AddCommand('SIZE', CommandSIZE);
    AddCommand('REST', CommandREST);
    AddCommand('RNFR', CommandRNFR);
    AddCommand('RNTO', CommandRNTO);
    AddCommand('MKD',  CommandMKD);
    AddCommand('RMD',  CommandRMD);
    AddCommand('ABOR', CommandABOR);
    AddCommand('PASV', CommandPASV);
    AddCommand('NOOP', CommandNOOP);
    AddCommand('CDUP', CommandCDUP);
    AddCommand('APPE', CommandAPPE);
    AddCommand('STRU', CommandSTRU);
    AddCommand('XMKD', CommandMKD);
    AddCommand('XRMD', CommandRMD);
    AddCommand('MDTM', CommandMDTM);
    AddCommand('MODE', CommandMODE);
    AddCommand('OVER', CommandOverflow);
    AddCommand('STOU', CommandSTOU);
    AddCommand('FEAT', CommandFEAT);
    AddCommand('MLST', CommandMLST);  { angus V1.38 }
    AddCommand('MLSD', CommandMLSD);  { angus V1.38 }
    AddCommand('MFMT', CommandMDTM);  { angus V1.39 }
    AddCommand('MD5', CommandMD5);    { angus V1.39 }
    AddCommand('XCRC', CommandXCRC);  { angus V1.54 }
    AddCommand('XMD5', CommandMD5);   { angus V1.54 note same handler as MD5 }
    AddCommand('ALLO', CommandALLO);  { angus V1.54 }
    AddCommand('CLNT', CommandCLNT);  { angus V1.54 }
    AddCommand('OPTS', CommandOPTS);  { angus V1.54 }
    AddCommand('SITE PSWD', CommandSitePaswd);   { angus V1.54 }
    AddCommand('SITE EXEC', CommandSiteExec);    { angus V1.54 }
    AddCommand('SITE INDEX', CommandSiteIndex);  { angus V1.54 }
    AddCommand('SITE ZONE', CommandSiteZone);    { angus V1.54 }
    AddCommand('SITE MSG', CommandSiteMsg);      { angus V1.54 }
    AddCommand('SITE CMLSD', CommandSiteCmlsd);  { angus V1.54 }
    AddCommand('SITE DMLSD', CommandSiteDmlsd);  { angus V1.54 }
    AddCommand('COMB', CommandCOMB);  { angus V1.54 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TFtpServer.Destroy;
begin
    if Assigned(FEventTimer) then begin  { angus V1.54 }
        FEventTimer.Destroy;
        FEventTimer := nil;
    end;
    if Assigned(FServSocket) then begin
        FServSocket.Destroy;
        FServSocket := nil;
    end;
    if Assigned(FClientList) then begin
        FClientList.Destroy;
        FClientList := nil;
    end;
    if Assigned(FPasvPortTable) then begin
        FreeMem(FPasvPortTable, FPasvPortTableSize);
        FPasvPortTable     := nil;
        FPasvPortTableSize := 0;
    end;
    SetLength(FCmdTable, 0);
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpServer.MsgHandlersCount : Integer;
begin
    Result := 5 + inherited MsgHandlersCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.AllocateMsgHandlers;
begin
    inherited AllocateMsgHandlers;
    FMsg_WM_FTPSRV_CLOSE_REQUEST  := FWndHandler.AllocateMsgHandler(Self);
    FMsg_WM_FTPSRV_CLIENT_CLOSED  := FWndHandler.AllocateMsgHandler(Self);
    FMsg_WM_FTPSRV_ABORT_TRANSFER := FWndHandler.AllocateMsgHandler(Self);
    FMsg_WM_FTPSRV_CLOSE_DATA     := FWndHandler.AllocateMsgHandler(Self);
    FMsg_WM_FTPSRV_START_SEND     := FWndHandler.AllocateMsgHandler(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.FreeMsgHandlers;
begin
    if Assigned(FWndHandler) then begin
        FWndHandler.UnregisterMessage(FMsg_WM_FTPSRV_CLOSE_REQUEST);
        FWndHandler.UnregisterMessage(FMsg_WM_FTPSRV_CLIENT_CLOSED);
        FWndHandler.UnregisterMessage(FMsg_WM_FTPSRV_ABORT_TRANSFER);
        FWndHandler.UnregisterMessage(FMsg_WM_FTPSRV_CLOSE_DATA);
        FWndHandler.UnregisterMessage(FMsg_WM_FTPSRV_START_SEND);
    end;
    inherited FreeMsgHandlers;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.WndProc(var MsgRec: TMessage);
begin
    try
        with MsgRec do begin
            if      Msg = FMsg_WM_FTPSRV_CLOSE_REQUEST  then
                WMFtpSrvCloseRequest(MsgRec)
            else if Msg = FMsg_WM_FTPSRV_CLIENT_CLOSED then
                WMFtpSrvClientClosed(MsgRec)
            else if Msg = FMsg_WM_FTPSRV_ABORT_TRANSFER then
                WMFtpSrvAbortTransfer(MsgRec)
            else if Msg = FMsg_WM_FTPSRV_CLOSE_DATA then
                WMFtpSrvCloseData(MsgRec)
            else if Msg = FMsg_WM_FTPSRV_START_SEND then
                WMFtpSrvStartSend(MsgRec)
            else
                inherited WndProc(MsgRec);
        end;
    except
        on E:Exception do
            HandleBackGroundException(E);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.WMFtpSrvCloseRequest(var msg: TMessage);
var
    Client : TFtpCtrlSocket;
    I      : Integer;
begin
    Client := TFtpCtrlSocket(msg.LParam);
    I := FClientList.IndexOf(Client);
    if I >= 0 then begin
        { Check if client.ID is still the same as when message where posted }
        if WPARAM(TFtpCtrlSocket(FClientList.Items[I]).ID) = Msg.WParam then begin
            if Client.AllSent then
                Client.Close
            else
                Client.CloseRequest := TRUE;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.Notification(AComponent: TComponent; operation: TOperation);
begin
    inherited Notification(AComponent, operation);
    if operation = opRemove then begin
        if AComponent = FServSocket then
            FServSocket := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpServer.OpenFileStream (const FileName: string; Mode: Word): TStream;   { V1.54 }
begin
{$IFDEF USE_BUFFERED_STREAM}
    Result := TBufferedFileStream.Create(FileName, Mode, MAX_BUFSIZE);
{$ELSE}
    Result := TFileStream.Create(FileName, Mode);
{$ENDIF}
end ;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CloseFileStreams(Client : TFtpCtrlSocket);    { V1.54 }
begin
{$IFDEF USE_MODEZ}   { V1.54 }
  { delete temporary ZLIB file if not from cache }
    try
        if Assigned (Client.ZFileStream) then Client.ZFileStream.Destroy;
        Client.ZFileStream := Nil;
        if (Client.ZStreamState > ftpzStateNone) and
                                        Client.ZCompFileDelete then begin
            try
                if FileExists(Client.ZCompFileName) then
                                           DeleteFile (Client.ZCompFileName);
            except
            end;
        end;
    except
    end;
    Client.ZStreamState := ftpZStateNone;
{$ENDIF}
    if Client.HasOpenedFile then begin
        if Assigned(Client.DataStream) then Client.DataStream.Destroy;
        Client.DataStream    := nil;
        Client.HasOpenedFile := FALSE;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.AddCommand(
    const Keyword : String;
    const Proc    : TFtpSrvCommandProc);
begin
    if FLastCmd > High(FCmdTable) then
        raise FtpServerException.Create('Too many command');
    FCmdTable[FLastCmd].KeyWord := KeyWord;
    FCmdTable[FLastCmd].Proc    := Proc;
    Inc(FLastCmd);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.Start;
begin
{$IFNDEF USE_MODEZ}              { angus V1.54 }
    FOptions := FOptions - ftpsModeZCompress;
{$ENDIF}
    if FServSocket.State = wsListening then
        Exit;             { Server is already running }
    FServSocket.Port  := Port;
    FServSocket.Proto := 'tcp';
    FServSocket.Addr  := FAddr;
    FServSocket.OnSessionAvailable := ServSocketSessionAvailable;
    FServSocket.OnChangeState      := ServSocketStateChange;
    FServSocket.ComponentOptions   := [wsoNoReceiveLoop];
    FServSocket.Listen;
    FEventTimer.Enabled := true;                  { angus V1.54 }
{$IFNDEF NO_DEBUG_LOG}
    if CheckLogOptions(loProtSpecInfo) then                            { V1.46 }
        DebugLog(loProtSpecInfo, Name + ' started');                   { V1.46 }
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.Stop;
begin
    FEventTimer.Enabled := false;                  { angus V1.54 }
    FServSocket.Close;
{$IFNDEF NO_DEBUG_LOG}                                                 
    if CheckLogOptions(loProtSpecInfo) then                            { V1.46 }
        DebugLog(loProtSpecInfo, Name + ' stopped');                   { V1.46 }
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.DisconnectAll;
var
    Client : TFtpCtrlSocket;
    Msg    : TMessage;
begin
    while FClientList.Count > 0 do begin
        Client := TFtpCtrlSocket(FClientList.Items[0]);
        FillChar(Msg, SizeOf(Msg), 0);
        Msg.LParam := Integer(Client);
        Msg.WParam := Client.ID;
        WMFtpSrvClientClosed(Msg);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.Disconnect(Client : TFtpCtrlSocket);
var
    I   : Integer;
    Msg : TMessage;
begin
    I := FClientList.IndexOf(Client);
    if I < 0 then
        raise FtpServerException.Create('Disconnect: Not one of our clients');

    FillChar(Msg, SizeOf(Msg), 0);
    Msg.LParam := Integer(Client);
    Msg.WParam := Client.ID;
    WMFtpSrvClientClosed(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpServer.GetActive : Boolean;
begin
    Result := (FServSocket.State = wsListening);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.SetActive(newValue : Boolean);
begin
    if newValue then
        Start
    else
        Stop;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.ServSocketStateChange(Sender : TObject; OldState, NewState : TSocketState);
begin
    if csDestroying in ComponentState then
        Exit;
    if NewState = wsListening then
        TriggerServerStart
    else if NewState = wsClosed then
        TriggerServerStop;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.ServSocketSessionAvailable(Sender : TObject; AError  : Word);
var
    Client : TFtpCtrlSocket;
begin
    if AError <> 0 then
        raise FtpServerException.Create('Session Available Error - ' +
                                        GetWinsockErr(AError));
    if FClientNum >= $7FFFFF then FClientNum := 0;  { angus V1.54 prevent overflow }
    Inc(FClientNum);
    Client                 := FClientClass.Create(Self);
    FClientList.Add(Client);
    Client.Name            := 'ClientWSocket' + IntToStr(FClientNum);
    Client.DataSocket.Name := 'DataWSocket' + IntToStr(FClientNum);
    Client.ID              := FClientNum;
    Client.Banner          := FBanner;
    Client.HSocket         := ServSocket.Accept;
    Client.OnCommand       := ClientCommand;
    Client.OnSessionClosed := ClientSessionClosed;
    Client.OnDataSent      := ClientDataSent;
{$IFNDEF NO_DEBUG_LOG}
    Client.IcsLogger       := IcsLogger;                     { V1.46 }
    Client.DataSocket.IcsLogger := IcsLogger;                    //<= 01/01/06 AG
{$ENDIF}
{$IFDEF USE_SSL}
    if Self is TSslFtpServer then begin     {  V1.48 }
        if ftpImplicitSsl in TSslFtpserver(Self).FFtpSslTypes then   { V1.47 }
            Client.CurFtpSslType := curftpImplicitSsl;               { V1.47 }
    end;
{$ENDIF}
    if ftpsCdupHome in FOptions then
        Client.Options := Client.Options + [ftpCdupHome];   { angus V1.39 }
    if ftpsCwdCheck in FOptions then
        Client.Options := Client.Options + [ftpCwdCheck];   { angus V1.39 }
    if ftpsHidePhysicalPath in FOptions then                
        Client.Options := Client.Options + [ftpHidePhysicalPath]; { AG V1.52 }
{$IFDEF USE_MODEZ}              { angus V1.54 }
    if ftpsModeZCompress in FOptions then
        Client.Options := Client.Options + [ftpModeZCompress];
{$ENDIF}

{$IFNDEF NO_DEBUG_LOG}                                       { V1.46 }
    if CheckLogOptions(loProtSpecDump) then
        DebugLog(loProtSpecDump,  IntToHex(Integer(Client), 8) +
                 ' Client Connect Error - ' + GetWinsockErr(AError) + ' ' +
                  IntToStr(Client.HSocket));
{$ENDIF}
    Client.SessIdInfo      := Client.GetPeerAddr;  { angus V1.54 may be changed during event }
    Client.CurrTransMode   := FtpTransModeStream ; { angus V1.54 current zlib transfer mode }
    Client.ZReqLevel       := FZlibMinLevel;       { angus V1.54 initial compression level, minimum }
    Client.FileModeRead    := SrvFileModeRead;     { angus V1.57 }
    Client.FileModeWrite   := SrvFileModeWrite;    { angus V1.57 }
    TriggerClientConnect(Client, AError);
    { The event handler may have destroyed the client ! }
    if FClientList.IndexOf(Client) < 0 then
        Exit;
    { The event handler may have closed the connection }
    if Client.State <> wsConnected then
        Exit;
    { Ok, the client is still there, process with the connection }
    if (FMaxClients > 0) and (FMaxClients < ClientCount) then begin
        { Sorry, toomuch clients }
        Client.Banner := msgTooMuchClients;
        Client.StartConnection;
        Client.Close;
    end
    else
        Client.StartConnection;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.SendAnswer(Client : TFtpCtrlSocket; Answer : TFtpString);
begin
    try
        Client.ReqDurMilliSecs := IcsElapsedMsecs (Client.ReqStartTick);
        TriggerSendAnswer(Client, Answer);
        Client.SendAnswer(Answer);
    except
        { Just ignore any exception here }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.ClientCommand(
    Sender : TObject;
    CmdBuf : PChar;
    CmdLen : Integer);
const
    TELNET_IAC       = #255;
    TELNET_IP        = #244;
    TELNET_DATA_MARK = #242;
var
    Client  : TFtpCtrlSocket;
    Answer  : TFtpString;
    Params  : TFtpString;
    KeyWord : TFtpString;
    I       : Integer;
begin
    Client := Sender as TFtpCtrlSocket;
    Answer := '';

    { Copy the command received, removing any telnet option }
    try
        Client.ReqStartTick := IcsGetTickCountX;    { angus V1.54 tick when request started }
        Client.ReqDurMilliSecs := 0;                { angus V1.54 how long last request took, in ticks }
        Params := '';
        I      := 0;
        while I < CmdLen do begin
            if CmdBuf[I] <> TELNET_IAC then begin
                Params := Params + CmdBuf[I];
                Inc(I);
            end
            else begin
                Inc(I);
                if CmdBuf[I] = TELNET_IAC then
                    Params := Params + CmdBuf[I];
                Inc(I);
            end;
        end;

        { Extract keyword, ignoring leading spaces and tabs }
        I := 1; { angus V1.54 moved argument parsing code to FtpSrvT to avoid duplication }
        KeyWord := UpperCase(ScanGetAsciiArg (Params, I));
        if KeyWord = 'SITE' then begin  { angus 1.54 special case for two word command }
            KeyWord := 'SITE ' + UpperCase(ScanGetAsciiArg (Params, I));
        end ;
        ScanFindArg (Params, I);

        { Extract parameters, ignoring leading spaces and tabs }
        Params := Copy(Params, I, Length(Params));

        { Pass the command to the component user to let him a chance to }
        { handle it. If it does, he must return the answer.             }
        TriggerClientCommand(Client, Keyword, Params, Answer);
        if Answer <> '' then begin
            { Event handler has processed the client command, send the answer }
            SendAnswer(Client, Answer);
            Exit;
        end;

        { The command has not been processed, we'll process it }
        if Keyword = '' then begin
            { Empty keyword (should never occurs) }
            SendAnswer(Client, Format(msgCmdUnknown, [Params]));
            Exit;
        end;

        { We need to process the client command, search our command table }
        I := 0;
        while I <= High(FCmdTable) do begin
            if FCmdTable[I].KeyWord = KeyWord then begin
                Client.CurCmdType := I;             { angus V1.54 }
                Client.AnswerDelayed := FALSE; { AG V1.50 }
                FCmdTable[I].Proc(Client, KeyWord, Params, Answer);
                if not Client.AnswerDelayed then  { AG V1.50 }
                            SendAnswer(Client, Answer);
                Exit;
            end;
            Inc(I);
        end;
        SendAnswer(Client, Format(msgCmdUnknown, [KeyWord]));
    except
        on E:Exception do begin
            SendAnswer(Client, '501 ' + E.Message);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.ClientDataSent(Sender : TObject; AError  : Word);
var
    Client  : TFtpCtrlSocket;
begin
    Client := Sender as TFtpCtrlSocket;
    if Client.CloseRequest then begin
        Client.CloseRequest := FALSE;
        PostMessage(Handle, FMsg_WM_FTPSRV_CLOSE_REQUEST,
                    WPARAM(Client.ID), LPARAM(Client));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.ClientSessionClosed(Sender : TObject; AError  : Word);
var
    Client  : TFtpCtrlSocket;
begin
    Client := Sender as TFtpCtrlSocket;
    PostMessage(Handle, FMsg_WM_FTPSRV_CLIENT_CLOSED,
                WPARAM(Client.ID), LPARAM(Client));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.WMFtpSrvClientClosed(var msg: TMessage);
var
    Client    : TFtpCtrlSocket;
    I         : Integer;
    SesClosed : TSessionClosed;
begin
    Client := TFtpCtrlSocket(Msg.LParam);
    { Check if client still in our client list }
    I := FClientList.IndexOf(Client);
    if I >= 0 then begin
        { Check if client.ID is still the same as when message where posted }
        if WPARAM(TFtpCtrlSocket(FClientList.Items[I]).ID) = Msg.WParam then begin
            try
                SesClosed := Client.DataSocket.OnSessionClosed;
                if Client.DataSessionActive and Assigned(SesClosed) then
                    Client.DataSocket.OnSessionClosed(Client.DataSocket, WSAENOTCONN);
                CloseFileStreams(Client);      { angus V1.57 }
                if Client.PassiveMode then // FLD 17.1.06
                    FreeCurrentPasvPort(Client);
                FClientList.Remove(Client);
                TriggerClientDisconnect(Client, 0);
            finally
                Client.Destroy;
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.WMFtpSrvAbortTransfer(var msg: TMessage);
var
    Client : TFtpCtrlSocket;
    Data   : TWSocket;
    I      : Integer;
begin
    Client := TFtpCtrlSocket(Msg.LParam);
    { Check if client still in our client list }
    I := FClientList.IndexOf(Client);
    if I >= 0 then begin
        { Check if client.ID is still the same as when message where posted }
        if WPARAM(TFtpCtrlSocket(FClientList.Items[I]).ID) = Msg.WParam then begin
            Data := Client.DataSocket;
            { make sure to free PasvPort even on aborted connections ! }
            if Assigned(Data) then begin
                if Client.PassiveMode then // FLD 29.12.05
                    FreeCurrentPasvPort(Client);

                Data.ShutDown(2);
                Data.Close;
            end;
            CloseFileStreams(Client);      { angus V1.57 }
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.WMFtpSrvCloseData(var msg: TMessage);
var
    Client : TFtpCtrlSocket;
    Data   : TWSocket;
    I      : Integer;
{ !!!!!!!!!!! NGB: next line changed }
    {PortNumber : String;}
{ !!!!!!!!!!! NGB: previous line changed }
begin
    Client := TFtpCtrlSocket(Msg.LParam);
    { Check if client still in our client list }
    I := FClientList.IndexOf(Client);
    if I >= 0 then begin
        { Check if client.ID is still the same as when message where posted }
        if WPARAM(TFtpCtrlSocket(FClientList.Items[I]).ID) = Msg.WParam then begin
            Data := Client.DataSocket;
{ !!!!!!!!!!! NGB: Free Up Current Port - next 5 lines changed }
            if Assigned(Data) then begin
                if Client.PassiveMode then // FLD 29.12.05
                    FreeCurrentPasvPort(Client);
                Data.ShutDown(1);    {  Wilfried 24/02/04 }
            end;
{ !!!!!!!!!!! NGB: previous 5 lines changed }
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpServer.GetClient(nIndex : Integer) : TFtpCtrlSocket;
begin
    if not Assigned(FClientList) then begin
        Result := nil;
        Exit;
    end;
    if (nIndex < 0) or (nIndex >= FClientList.Count) then begin
        Result := nil;
        Exit;
    end;
    Result := TFtpCtrlSocket(FClientList.Items[nIndex]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Check  if a given object is one of our clients }
function TFtpServer.IsClient(SomeThing : TObject) : Boolean;
begin
    if not Assigned(FClientList) then
        Result := FALSE
    else
        Result := (FClientList.IndexOf(Pointer(SomeThing)) >= 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpServer.GetClientCount : Integer;
begin
    if Assigned(FClientList) then
        Result := FClientList.Count
    else
        Result := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerServerStart;
begin
    if Assigned(FOnStart) then
        FOnStart(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerServerStop;
begin
    if Assigned(FOnStop) then
        FOnStop(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerAuthenticate(
    Client            : TFtpCtrlSocket;
    UserName          : String;
    PassWord          : String;
    var Authenticated : Boolean);
begin
    if Assigned(FOnAuthenticate) then
        FOnAuthenticate(Self, Client, UserName, Password, Authenticated);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerOtpMethod(           { angus V1.54 }
    Client          : TFtpCtrlSocket;
    UserName        : TFtpString;
    var OtpMethod   : TOtpMethod);
begin
    if Assigned(FOnOtpMethod) then
        FOnOtpMethod(Self, Client, UserName, OtpMethod);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerOtpGetPassword(      { angus V1.54 }
    Client           : TFtpCtrlSocket;
    UserName         : TFtpString;
    var UserPassword : String);
begin
    if Assigned(FOnOtpGetPassword) then
        FOnOtpGetPassword(Self, Client, UserName, UserPassword);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerChangeDirectory(
    Client         : TFtpCtrlSocket;
    Directory      : String;
    var Allowed    : Boolean);
begin
    if Assigned(FOnChangeDirectory) then
        FOnChangeDirectory(Self, Client, Directory, Allowed);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerMakeDirectory(
    Client         : TFtpCtrlSocket;
    Directory      : String;
    var Allowed    : Boolean);
begin
    if Assigned(FOnMakeDirectory) then
        FOnMakeDirectory(Self, Client, Directory, Allowed);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerBuildDirectory(
    Client        : TFtpCtrlSocket;
    var Params    : TFtpString;
    Detailed      : Boolean);
begin
    if Assigned(FOnBuildDirectory) then
        FOnBuildDirectory(Self, Client, Params, Detailed);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerAlterDirectory(
    Client        : TFtpCtrlSocket;
    var Params    : TFtpString;
    Detailed      : Boolean);
begin
    if Assigned(FOnAlterDirectory) then
        FOnAlterDirectory(Self, Client, Params, Detailed);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerSendAnswer(
    Client     : TFtpCtrlSocket;
    var Answer : TFtpString);
begin
    if Assigned(FOnAnswerToClient) then
        FOnAnswerToClient(Self, Client, Answer);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerClientDisconnect(Client : TFtpCtrlSocket; AError  : Word);
begin
    if Assigned(FOnClientDisconnect) then
        FOnClientDisconnect(Self, Client, AError);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerClientConnect(Client : TFtpCtrlSocket; AError  : Word);
begin
    if Assigned(FOnClientConnect) then
        FOnClientConnect(Self, Client, AError);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerStorSessionConnected(
    Client : TFtpCtrlSocket; Data : TWSocket; AError  : Word);
begin
    if Assigned(FOnStorSessionConnected) then
        FOnStorSessionConnected(Self, Client, Data, AError);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerRetrSessionConnected(
    Client : TFtpCtrlSocket; Data : TWSocket; AError  : Word);
begin
    if Assigned(FOnRetrSessionConnected) then
        FOnRetrSessionConnected(Self, Client, Data, AError);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerStorSessionClosed(
    Client : TFtpCtrlSocket; Data : TWSocket; AError  : Word);
begin
    if Assigned(FOnStorSessionClosed) then
        FOnStorSessionClosed(Self, Client, Data, AError);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerRetrSessionClosed(
    Client : TFtpCtrlSocket; Data : TWSocket; AError  : Word);
begin
    if Assigned(FOnRetrSessionClosed) then
        FOnRetrSessionClosed(Self, Client, Data, AError);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerClientCommand(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Assigned(FOnClientCommand) then
        FOnClientCommand(Self, Client, KeyWord, Params, Answer);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerValidatePut(
    Client        : TFtpCtrlSocket;
    var FilePath  : TFtpString;
    var Allowed   : Boolean);
begin
    if Assigned(FOnValidatePut) then
        FOnValidatePut(Self, Client, FilePath, Allowed);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerValidateSize(
    Client        : TFtpCtrlSocket;
    var FilePath  : TFtpString;
    var Allowed   : Boolean);
begin
    if Assigned(FOnValidateSize) then
        FOnValidateSize(Self, Client, FilePath, Allowed);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerValidateDele(
    Client        : TFtpCtrlSocket;
    var FilePath  : TFtpString;
    var Allowed   : Boolean);
begin
    if Assigned(FOnValidateDele) then
        FOnValidateDele(Self, Client, FilePath, Allowed);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerValidateRmd(
    Client        : TFtpCtrlSocket;
    var FilePath  : TFtpString;
    var Allowed   : Boolean);
begin
    if Assigned(FOnValidateRmd) then
        FOnValidateRmd(Self, Client, FilePath, Allowed);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerValidateRnFr(
    Client        : TFtpCtrlSocket;
    var FilePath  : TFtpString;
    var Allowed   : Boolean);
begin
    if Assigned(FOnValidateRnFr) then
        FOnValidateRnFr(Self, Client, FilePath, Allowed);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerValidateRnTo(
    Client        : TFtpCtrlSocket;
    var FilePath  : TFtpString;
    var Allowed   : Boolean);
begin
    if Assigned(FOnValidateRnTo) then
        FOnValidateRnTo(Self, Client, FilePath, Allowed);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerValidateGet(
    Client        : TFtpCtrlSocket;
    var FilePath  : TFtpString;
    var Allowed   : Boolean);
begin
    if Assigned(FOnValidateGet) then
        FOnValidateGet(Self, Client, FilePath, Allowed);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerStorDataAvailable(
    Client : TFtpCtrlSocket;
    Data   : TWSocket;
    Buf    : PChar;
    Len    : LongInt;
    AError : Word);
begin
    if Assigned(FOnStorDataAvailable) then
        FOnStorDataAvailable(Self, Client, Data, Buf, Len, AError);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerRetrDataSent(
    Client : TFtpCtrlSocket;
    Data   : TWSocket;
    AError : Word);
begin
    if Assigned(FOnRetrDataSent) then
        FOnRetrDataSent(Self, Client, Data, AError);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerGetUniqueFileName(
    Client       : TFtpCtrlSocket;
    var FileName : TFtpString);
begin
    if Assigned (FOnGetUniqueFileName) then
        FOnGetUniqueFileName (Self, Client, FileName);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerValidateMfmt(  { angus V1.39 }
    Client        : TFtpCtrlSocket;
    var FilePath  : TFtpString;
    var Allowed   : Boolean);
begin
    if Assigned (FOnValidateMfmt) then
        FOnValidateMfmt (Self, Client, FilePath, Allowed);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerCalculateMd5(
    Client        : TFtpCtrlSocket;
    var FilePath  : TFtpString;
    var Md5Sum    : TFtpString;
    var Allowed   : Boolean);
begin
    if Assigned(FOnCalculateMd5) then
        FOnCalculateMd5(Self, Client, FilePath, Md5Sum, Allowed);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerMd5Calculated(Client: TFtpCtrlSocket; { AG V1.50 }
  const FilePath, Md5Sum: TFtpString);
begin
    if Assigned(FOnMd5Calculated) then
        FOnMd5Calculated(Self, Client, FilePath, Md5Sum);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerCalculateCrc(                           { angus V1.54 }
    Client        : TFtpCtrlSocket;
    var FilePath  : TFtpString;
    var Md5Sum    : TFtpString;
    var Allowed   : Boolean);
begin
    if Assigned(FOnCalculateCrc) then
        FOnCalculateCrc(Self, Client, FilePath, Md5Sum, Allowed);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerCrcCalculated(Client: TFtpCtrlSocket;  { angus V1.54 }
  const FilePath, Md5Sum: TFtpString);
begin
    if Assigned(FOnCrcCalculated) then
        FOnCrcCalculated(Self, Client, FilePath, Md5Sum);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerEnterSecurityContext(                  { AG V1.52 }
    Client : TFtpCtrlSocket);
begin
    if Assigned(FOnEnterSecurityContext) then
        FOnEnterSecurityContext(Self, Client);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerLeaveSecurityContext(                  { AG V1.52 }
    Client : TFtpCtrlSocket);
begin
    if Assigned(FOnLeaveSecurityContext) then
        FOnLeaveSecurityContext(Self, Client);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerValidateAllo(                          { angus V1.54 }
    Client      : TFtpCtrlSocket;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Assigned(FOnValidateAllo) then
        FOnValidateAllo(Self, Client, Params, Answer);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerClntStr (                            { angus V1.54 }
    Client      : TFtpCtrlSocket;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Assigned(FOnClntStr) then
        FOnClntStr(Self, Client, Params, Answer);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerSiteMsg (                            { angus V1.54 }
    Client      : TFtpCtrlSocket;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Assigned(FOnSiteMsg ) then
        FOnSiteMsg (Self, Client, Params, Answer);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerSiteExec (                            { angus V1.54 }
    Client      : TFtpCtrlSocket;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Assigned(FOnSiteExec) then
        FOnSiteExec(Self, Client, Params, Answer);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerSitePaswd (                            { angus V1.54 }
    Client      : TFtpCtrlSocket;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Assigned(FOnSitePaswd) then
        FOnSitePaswd(Self, Client, Params, Answer);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerCombine (                            { angus V1.54 }
    Client      : TFtpCtrlSocket;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Assigned(FOnCombine) then
        FOnCombine(Self, Client, Params, Answer);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerTimeout(
    Client      : TFtpCtrlSocket;            { angus V1.54 }
    Duration    : Integer;
    var Abort   : Boolean);
begin
    if Assigned(FOnTimeout) then
        FOnTimeout(Self, Client, Duration, Abort);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerDownCompressFile(
    Client    : TFtpCtrlSocket;          { angus V1.54 }
    var Done  : Boolean);
begin
    if Assigned(FOnDownCompressFile) then
        FOnDownCompressFile(Self, Client, Done);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerUpCompressFile(
    Client    : TFtpCtrlSocket;            { angus V1.54 }
    var Done  : Boolean);
begin
    if Assigned(FOnUpCompressFile) then
        FOnUpCompressFile(Self, Client, Done);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerUpCompressedFile(
      Client  : TFtpCtrlSocket);   { angus V1.54 }
begin
    if Assigned(FOnUpCompressedFile) then
        FOnUpCompressedFile(Self, Client);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerDisplay(
       Client      : TFtpCtrlSocket;
       Msg         : TFtpString);  { angus V1.54 }
begin
    if Assigned(FOnDisplay) then
        FOnDisplay(Self, Client, Msg);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandUSER(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    Challenge: string;
begin
    Client.CurCmdType := ftpcUSER;
    Client.UserName   := Trim(Params);
    Client.FtpState   := ftpcWaitingPassword;
  { angus V1.54 - check if user account is set-up for authentication using a
    one time password. If so, OtpMethod is changed to the method and
    Client.OtpSequence and Client.OtpSeed set to the last values saved for
    the account, or OtpSequence set to -1 to generate a new seed }
    TriggerOtpMethod(Client, Client.UserName, Client.OtpMethod);
    if Client.OtpMethod = OtpKeyNone then
        Answer := Format(msgPassRequired, [Client.UserName])
    else begin
        Challenge := OtpCreateChallenge(Client.OtpMethod,
                                        Client.OtpSequence, Client.OtpSeed);
        Answer := Format(msgOptRespRequired, [Challenge, Client.UserName])
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandPASS(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    Authenticated : Boolean;
    UserPassword : String ;
begin
    if Client.FtpState <> ftpcWaitingPassword then
        Answer := msgNoUser
    else begin
        Client.CurCmdType    := ftpcPASS;
        Client.PassWord      := Trim(Params);
        Authenticated        := TRUE;
     {  angus V1.54 - if authenticating using a one time password, we need to get
       the user account password so that it can tested against the hashed OTP
       password created by the client from the sequence and seed sent in the challenge.
       Note the TriggerAuthenticate event is still called but with Authenticated set
       false if the OTP password failed, allowing the client to check for a clear
       password if required, or log the failure.  If OTP is successful, the new
       Client.OtpSequence should be saved in the user account details }
        if Client.OtpMethod > OtpKeyNone then begin
            UserPassword := '' ;
            TriggerOtpGetPassword(Client, Client.UserName, UserPassword);
            Authenticated := OtpTestPassword(Client.PassWord, UserPassword,
                            Client.OtpMethod, Client.OtpSequence, Client.OtpSeed);
        end;
        TriggerAuthenticate(Client, Client.UserName, Client.PassWord, Authenticated);
        if Authenticated then begin
            Client.FtpState  := ftpcReady;
            Client.Directory := Client.HomeDir;
            Answer           := Format(msgLogged, [Client.UserName])
        end
        else begin
            Client.FtpState  := ftpcWaitingUserCode;
            Answer           := msgLoginFailed;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandCDUP(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    Client.CurCmdType := ftpcCDUP;
    Params := '..';
    CommandChangeDir(Client, Keyword, Params, Answer);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandCWD(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    Client.CurCmdType    := ftpcCWD;
    CommandChangeDir(Client, Keyword, Params, Answer);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
(*
function SlashesToBackSlashes(const S : String) : String;
var
    I : Integer;
begin
    Result := S;
    for I := 1 to Length(Result) do begin
        if Result [I] = '/' then
            Result[I] := '\';
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function BackSlashesToSlashes(const S : String) : String;
var
    I : Integer;
begin
    Result := S;
    for I := 1 to Length(Result) do begin
        if Result [I] = '\' then
            Result[I] := '/';
    end;
end;
  *)

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandChangeDir(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    Allowed : Boolean;
    OldDir  : String;
    DExists : Boolean;
begin
    OldDir := Client.Directory;
    try
        Params := SlashesToBackSlashes(Params);
        Client.Directory := Trim(Params);
        //Allowed          := TRUE;
        Allowed := IsPathAllowed(Client, Client.Directory);  { V1.52 AG }
        (*
        { angus V1.38  ensure not changing below root directory }  { V1.52 AG }
        if ftpCdupHome in Client.Options then begin    { angus V1.39 }
            if Pos(LowerCase(Client.HomeDir), LowerCase(Client.Directory)) <> 1 then begin
                Answer := Format(msgCWDFailed, ['No permission']);
                Client.Directory := OldDir;
                Exit;
            end;
        end;
        *)
        { should this event be before the ftpsCdupHome test??? }
        TriggerChangeDirectory(Client, Client.Directory, Allowed);
        if Allowed then begin
            TriggerEnterSecurityContext(Client);             { V1.52 AG }
            try
                DExists := DirExists(Client.Directory);
            finally
                TriggerLeaveSecurityContext(Client);         { V1.52 AG }
            end;
            { angus V1.38 make sure windows path exists }
            if (not (ftpCwdCheck in Client.Options)) or DExists or
                    (DExists and (Length(Client.Directory) <= 3)) or  { angus V1.39 }
                    (LowerCase(Client.HomeDir) = LowerCase(Client.Directory)) then { angus V1.42 }
                Answer := Format(msgCWDSuccess, [FormatResponsePath(Client, Client.Directory)])
            else begin
                Answer := Format(msgCWDNoDir, [FormatResponsePath(Client, Client.Directory)]);   { angus V1.38 }
                Client.Directory := OldDir;        { angus V1.38 }
            end;
        end
        else begin
            Client.Directory := OldDir;
            Answer           := Format(msgCWDFailed, ['No permission']);
        end;
    except
        on E:Exception do begin
            Client.Directory := OldDir;
            Answer           := Format(msgCWDFailed, [E.Message]);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandXPWD(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    Client.CurCmdType := ftpcXPWD;
    Answer := Format(msgPWDSuccess,
                   [FormatResponsePath(Client, Client.Directory)]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandPWD(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    
    Client.CurCmdType := ftpcPWD;
    Answer := Format(msgPWDSuccess,
                   [FormatResponsePath(Client, Client.Directory)]); { AG V1.52 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandQUIT(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    Client.CurCmdType := ftpcQUIT;
    Answer            := msgQuit;
    PostMessage(Handle, FMsg_WM_FTPSRV_CLOSE_REQUEST,
                WPARAM(Client.ID), LPARAM(Client));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetInteger(var I : Integer; const Src : String) : LongInt;
begin
    { Skip leading white spaces }
    while (I <= Length(Src)) and IsSpace(Src[I]) do
        Inc(I);
    Result := 0;
    while (I <= Length(Src)) and IsSpace(Src[I]) do begin
        Result := Result * 10 + Ord(Src[I]) - Ord('0');
        Inc(I);
    end;
    { Skip trailing white spaces }
    while (I <= Length(Src)) and IsSpace(Src[I]) do
        Inc(I);
    { Check if end of string of comma. If not, error, returns -1 }
    if I <= Length(Src) then begin
        if Src[I] = ',' then
            Inc(I)        { skip comma           }
        else
            raise FtpServerException.Create('GetInteger: unexpected char'); { error, must be comma }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandPORT(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    I : Integer;
    N : LongInt;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    try
        Client.CurCmdType := ftpcPORT;
        I                 := 1;
        Client.DataAddr   := IntToStr(GetInteger(I, Params));
        Client.DataAddr   := Client.DataAddr + '.' + IntToStr(GetInteger(I, Params));
        Client.DataAddr   := Client.DataAddr + '.' + IntToStr(GetInteger(I, Params));
        Client.DataAddr   := Client.DataAddr + '.' + IntToStr(GetInteger(I, Params));
        N := GetInteger(I, Params);
        N := (N shl 8) + GetInteger(I, Params);
        Client.DataPort := IntToStr(N);
        Answer := msgPortSuccess;
    except
        Answer := msgPortFailed;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandSTOR(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    Allowed  : Boolean;
    FilePath : TFtpString;
begin
    try
        if Client.FtpState <> ftpcReady then begin
            Answer := msgNotLogged;
            Exit;
        end;
        if Params = '' then begin                                { V1.52 AG }
            Answer := Format(msgStorFailed, ['File name not specified']);
            Exit;
        end;
        try
            Client.CurCmdType       := ftpcSTOR;
            Client.FileName         := SlashesToBackSlashes(Params);
            Client.HasOpenedFile    := FALSE;
            //Allowed                 := TRUE;  { AG V1.52 }
            FilePath                := BuildFilePath(Client, Client.Directory, Client.FileName);
            Allowed := IsPathAllowed(Client, FilePath); { AG V1.52 }
            TriggerValidatePut(Client, FilePath, Allowed);
            if not Allowed then begin
                Answer := msgStorDisabled;
                Exit;
            end;
            Client.FilePath := FilePath;
            PrepareStorDataSocket(Client);
            Answer := Format(msgStorSuccess, [Params]);
        except
            on E:Exception do begin
                Answer := Format(msgStorFailed, [E.Message]);
            end;
        end;
    finally
        { check for success 150..159 in passive mode              }
        if Client.PassiveMode and (Copy(Answer, 1, 2) <> '15') then begin
            { flag for ClientStorSessionClosed that the error-message was already sent! }
            Client.TransferError    := '';
            Client.AbortingTransfer := TRUE;
            { set up Passive DataSocket.EventHandlers }
            { otherwise FreeCurrentPasvPort won't be called ! }
            PreparePassiveStorDataSocket(Client);
        end;
    end;
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ New setup fuction for all STOR-based connections                            }
{ Performes the same task as StartSendData for RETR-based connections         }
procedure TFtpServer.PrepareStorDataSocket(Client : TFtpCtrlSocket);
begin
    Client.AbortingTransfer := FALSE;
    Client.TransferError    := 'Transfer Ok';

    if Client.PassiveMode then begin
        PreparePassiveStorDataSocket(Client);
    end
    else begin
        Client.DataSocket.Proto               := 'tcp';
        Client.DataSocket.Addr                := Client.DataAddr;
        Client.DataSocket.Port                := Client.DataPort;
        Client.DataSocket.OnSessionConnected  := ClientStorSessionConnected;
        Client.DataSocket.OnSessionClosed     := ClientStorSessionClosed;
        Client.DataSocket.OnDataAvailable     := ClientStorDataAvailable;
        Client.DataSocket.OnDataSent          := nil;
        Client.DataSocket.LingerOnOff         := wsLingerOff;
        Client.DataSocket.LingerTimeout       := 0;
{$IFDEF BIND_FTP_DATA}
        Client.DataSocket.LocalAddr           := Client.GetXAddr;
        Client.DataSocket.LocalPort           := 'ftp-data'; {20}
{$ENDIF}
        Client.DataSocket.ComponentOptions    := [wsoNoReceiveLoop];
        Client.DataSocket.Connect;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ New setup fuction for all Passive STOR-based data connections               }
procedure TFtpServer.PreparePassiveStorDataSocket(Client : TFtpCtrlSocket);
begin
    Client.DataSocket.OnSessionConnected  := ClientStorSessionConnected;
    Client.DataSocket.OnSessionClosed     := ClientStorSessionClosed;
    Client.DataSocket.OnDataAvailable     := ClientStorDataAvailable;
    Client.DataSocket.OnDataSent          := nil;
    if Client.PassiveConnected then
        Client.DataSocket.OnSessionConnected(Client.DataSocket, 0)
    else
        Client.PassiveStart := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.ClientStorSessionConnected(Sender : TObject; AError  : Word);
var
    Client      : TFtpCtrlSocket;
    Data        : TWSocket;
begin
    Data                     := TWSocket(Sender);
    Client                   := TFtpCtrlSocket(Data.Owner);
    Client.DataSessionActive := TRUE;
    Client.ByteCount         := 0;
    Client.XferStartTick     := IcsGetTickCountX; { angus V1.54 tick when last xfer started, for performance check }
    Client.LastTick          := IcsGetTickCountX;      { angus V1.54 last tick for time out checking }
    Client.ZStreamState      := ftpZStateNone;

    if Client.AbortingTransfer then
        Exit; // primary command (e.g. STOR) failed - don't trigger StorSessionConnected
    TriggerStorSessionConnected(Client, Data, AError);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.ClientStorSessionClosed(Sender : TObject; AError  : Word);
var
    Client      : TFtpCtrlSocket;
    Data        : TWSocket;
    Md5Sum      : String;  { AG V1.50 }
    I           : Integer; { AG V1.50 }
    Duration    : Integer;
    S           : String;
    BytesSec    : Int64;
    Answer      : String;
begin
    Data                     := TWSocket(Sender);
    Client                   := TFtpCtrlSocket(Data.Owner);
{ !!!!!!!! NGB: Free Up Current Port - next 2 lines added }
    if Client.PassiveMode then // FLD 29.12.05
        FreeCurrentPasvPort(Client);
{ !!!!!!!! NGB: previous 2 lines added }

    Client.DataSessionActive := FALSE;
    Client.PassiveStart      := FALSE;
    Client.PassiveConnected  := FALSE;
    Client.RestartPos        := 0;
    { Reset data port to standard value }
    Client.DataPort          := 'ftp-data';

{ angus V1.54 report performance }
    if Assigned(FOnDisplay) then begin
        Duration := IcsElapsedMsecs (Client.XferStartTick);
        S := Client.FilePath + ' ' +
                IntToKbyte(Client.ByteCount) + 'bytes received in ';
        if Duration < 2000 then
            S := S + IntToStr(Duration) + ' milliseconds'
        else begin
            S := S + IntToStr(Duration div 1000) + ' seconds';
            if Client.ByteCount > 32767 then
                BytesSec := 1000 * (Client.ByteCount div Duration)
            else
                BytesSec := (1000 * Client.ByteCount) div Duration;
            S := S + ' (' + IntToKbyte(BytesSec) + 'bytes/sec)';
        end;
        TriggerDisplay (Client, S);
    end;

    if Client.AbortingTransfer and (Client.TransferError = '') then
        Exit; { This happens when the Command itself was failed - do not      }
              { reply on command channel and don't trigger StorSessionClosed! }

    Answer := '';  { angus V1.54 don't send answer yet }
    case Client.CurCmdType of
    ftpcSTOR :
        begin
            if Client.AbortingTransfer then
                Answer := Format(msgStorAborted, [Client.TransferError])
            else if AError = 0 then
                Answer := msgStorOk
            else
                Answer := Format(msgStorError, [GetWinsockErr(AError)]);
        end;
    ftpcAPPE :
        begin
            if Client.AbortingTransfer then
                Answer := Format(msgAppeAborted, [Client.TransferError])
            else if AError = 0 then
                Answer := msgAppeOk
            else
                Answer := Format(msgAppeError, [GetWinsockErr(AError)]);
        end;
    ftpcSTOU :
        begin
            if Client.AbortingTransfer then
                Answer := Format(msgStouAborted, [Client.TransferError])
            else if AError = 0 then
                Answer := Format (msgStouOk, [Client.FileName])
            else
                Answer := Format(msgStouError, [GetWinsockErr(AError)]);
        end;
    else { Should never comes here }
        raise Exception.Create('Program error in ClientStorSessionClosed');
        exit;
    end;

{$IFDEF USE_MODEZ}   { V1.54 }
    if (Client.ZStreamState = ftpZStateSaveDecom) and
         (Client.ZFileStream.Size > 0) and Assigned(Client.DataStream) and
                     (NOT Client.AbortingTransfer) and (AError = 0) then begin
        try
            TriggerDisplay(Client, 'Using thread to decompress download file: ' +
                                         Client.ZCompFileName);
            Client.ProcessingThread := TClientProcessingThread.Create(TRUE);
            Client.ProcessingThread.Client := Client;
            Client.ProcessingThread.Sender := Data;
            Client.ProcessingThread.InData := Answer;
            Client.ProcessingThread.Keyword := 'DECOMPRESS';
            Client.ProcessingThread.OnTerminate := ClientProcessingThreadTerminate;
            Client.ProcessingThread.FreeOnTerminate := TRUE;
            Client.ProcessingThread.Resume;
            Client.AnswerDelayed := TRUE;
            exit;
        except
            on E:Exception do begin
                Answer := Format(msgStouError, ['Failed to start decompress - ' + E.Message]);
    end;
end;
    end;
{$ENDIF}

    { If we had opened a data stream ourself, then close it }
    CloseFileStreams(Client);      { angus V1.54 }
 {   if Client.HasOpenedFile then begin
        if Assigned(Client.DataStream) then
            Client.DataStream.Destroy;
        Client.DataStream    := nil;
        Client.HasOpenedFile := FALSE;
    end;   }

    TriggerStorSessionClosed(Client, Data, AError);

    if Client.MD5OnTheFlyFlag then begin { AG V1.50 }
        MD5Final(Client.MD5Digest, Client.MD5Context);
        Md5Sum := '';
        for I := 0 to 15 do
            Md5Sum := Md5Sum + IntToHex(Byte(Client.MD5Digest[I]), 2);
        TriggerMd5Calculated(Client, Client.FilePath, UpperCase(Md5Sum));
    end;
    SendAnswer(Client, Answer);  { angus V1.54 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.ClientStorDataAvailable(Sender: TObject; AError  : word);
var
    Len    : Integer;
    Client : TFtpCtrlSocket;
    Data   : TWSocket;
    NewPos : TFtpBigInt;
    I      : Integer; { AG V1.50 }
begin
    Data   := TWSocket(Sender);
    Client := TFtpCtrlSocket(Data.Owner);
    Len    := Data.Receive(Client.RcvBuf, Client.RcvSize);
    if Len <= 0 then
        Exit;

    if Client.AbortingTransfer then
        Exit;
    Client.LastTick := IcsGetTickCountX;  { angus V1.54 last tick for time out checking }

    try
        { Trigger the user event for the received data }
        TriggerStorDataAvailable(Client, Data, Client.RcvBuf, Len, AError);

        { We need to open a datastream if not already done and a FilePath }
        { exists (the component user can have nullified the FilePath      }
        if (not Client.HasOpenedFile) and
           (Length(Client.FilePath) > 0) and
           (not Assigned(Client.DataStream)) then begin
            { Store the file size temporarily }
            NewPos := GetFileSize(Client.FilePath); { V1.49 }
            { Use different file modes for APPE vs STOR }
            if (Client.CurCmdType = ftpcAPPE) and (NewPos > -1) then begin
                TriggerEnterSecurityContext(Client);  { AG V1.52 }
                try
                  {  Client.DataStream := TFileStream.Create(Client.FilePath,
                                            fmOpenReadWrite or fmShareDenyWrite);  }
                    Client.DataStream := OpenFileStream(Client.FilePath,
                                                Client.FileModeWrite); { angus V1.57 }
                finally
                    TriggerLeaveSecurityContext(Client); { AG V1.52 }
                end;
                { Cached Md5Sum should be deleted } { AG V1.50 }
                if (ftpsCalcMD5OnTheFly in FOptions) then
                    TriggerMd5Calculated(Client, Client.FilePath, '');
            end
            else if (Client.RestartPos > 0) and (NewPos > -1) then begin // check file exists!
                TriggerEnterSecurityContext(Client); { AG V1.52 }
                try
                 {   Client.DataStream := TFileStream.Create(Client.FilePath,
                                            fmOpenWrite or fmShareDenyWrite);   }
                    Client.DataStream := OpenFileStream(Client.FilePath,
                                                 Client.FileModeWrite); { angus V1.57 }
                finally
                    TriggerLeaveSecurityContext(Client); { AG V1.52 }
                end;
                { Cached Md5Sum should be deleted } { AG V1.50 }
                if (ftpsCalcMD5OnTheFly in FOptions) then
                    TriggerMd5Calculated(Client, Client.FilePath, '');
            end
            else begin
                TriggerEnterSecurityContext(Client); { AG V1.52 }
                try
                  {  Client.DataStream := TFileStream.Create(Client.FilePath, fmCreate);    }
                    Client.DataStream := OpenFileStream(Client.FilePath, fmCreate);  { angus V1.54 }
                finally
                    TriggerLeaveSecurityContext(Client); { AG V1.52 }
                end;
                NewPos := 0;

              { Calcutate MD5 checksum on the fly, when a new file is uploaded } { AG V1.50 }
                Client.MD5OnTheFlyFlag := ftpsCalcMD5OnTheFly in FOptions;
                if (Client.CurrTransMode = ftpTransModeZDeflate) then
                                             Client.MD5OnTheFlyFlag := false; { angus 1.54 }
                if Client.MD5OnTheFlyFlag then begin
                    for I := 0 to 15 do
                        Byte(Client.MD5Digest[I]) := I + 1;
                    MD5Init(Client.MD5Context);
                    Client.HashStartPos := 0;   { angus 1.54 }
                    Client.HashEndPos := 0;
                end;
            end;
            { We MUST check for file size >= RestartPos since Seek in any      } { V1.49 }
            { write-mode may write to the stream returning always the correct  }
            { new position.                                                    }
            if Client.RestartPos <= NewPos then begin
                TriggerEnterSecurityContext(Client); { AG V1.52 }
                try
                    NewPos := Client.DataStream.Seek(Client.RestartPos,
                    {$IFDEF STREAM64} soBeginning {$ELSE} sofromBeginning {$ENDIF});  { V1.49 }
                finally
                    TriggerLeaveSecurityContext(Client); { AG V1.52 }
                end;
            end;
            if NewPos <> Client.RestartPos then begin
                Client.TransferError    := 'Unable to set resume position in local file';
                Client.AbortingTransfer := TRUE;
                PostMessage(Handle, FMsg_WM_FTPSRV_ABORT_TRANSFER,
                            WPARAM(Client.ID), LPARAM(Client));
                Exit;
            end;
            Client.HasOpenedFile := TRUE;
        end;

        { If we have a DataStream, then we need to write the data }
        if Assigned(Client.DataStream) then begin
            Client.ByteCount := Client.ByteCount + Len;
            Client.TotPutBytes := Client.TotPutBytes + Len;    { angus V1.54 }
            TriggerEnterSecurityContext(Client);{ AG V1.52 }
            try
{$IFDEF USE_MODEZ}     { angus V1.54 }
                if (Client.CurrTransMode = ftpTransModeZDeflate) and
                         (Client.ZStreamState = ftpZStateNone) then begin
                 { save compressed data into temp file, decompress on close  }
                    zlibProblemString := '';
                    Client.ZCompFileName := FZlibWorkDir +
                                            GetZlibCacheFileName(Client.FilePath);
                    Client.ZCompFileDelete := True;
                    Client.ZFileStream := OpenFileStream(Client.ZCompFileName, fmCreate);
                    Client.ZStreamState := ftpZStateSaveDecom;
                end;
                if Client.ZStreamState = ftpZStateSaveDecom then
                    Client.ZFileStream.WriteBuffer(Client.RcvBuf^, Len)
                else
{$ENDIF}
                Client.DataStream.WriteBuffer(Client.RcvBuf^, Len);
            finally
                TriggerLeaveSecurityContext(Client); { AG V1.52 }
            end;
            if Client.MD5OnTheFlyFlag then { AG V1.50 }
                MD5UpdateBuffer(Client.MD5Context, Client.RcvBuf, Len);
        end;
    except
        { An exception occured, so we abort the transfer }
        on E:Exception do begin
            Client.TransferError    := E.Message;
            Client.AbortingTransfer := TRUE;
            PostMessage(Handle, FMsg_WM_FTPSRV_ABORT_TRANSFER,
                        WPARAM(Client.ID), LPARAM(Client));
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.TriggerBuildFilePath(
    Client            : TFtpCtrlSocket;
    const Directory   : String;
    const FileName    : String;
    var   NewFileName : String);
begin
    if Assigned(FOnBuildFilePath) then
         FOnBuildFilePath(Self, Client, Directory, FileName, NewFileName);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpServer.IsPathAllowed(                                 { AG V1.52 }
    Client           : TFtpCtrlSocket;
    const Path       : String;
    ExcludeBackslash : Boolean) : Boolean;
begin
    if (ftpCdUpHome in Client.Options) then begin
       if ExcludeBackslash then
          Result := (Pos(LowerCase(ExcludeTrailingPathDelimiter(Client.HomeDir)),
                         LowerCase(Path)) = 1)
       else
          Result := (Pos(LowerCase(Client.HomeDir), LowerCase(Path)) = 1);
    end
    else
        Result := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ serge le 5/10/2002 }
function TFtpServer.BuildFilePath(
    Client      : TFtpCtrlSocket;
    Directory   : String;
    FileName    : String) : String;
var
    Drive : String;
    Path  : String;
begin
    FileName := SlashesToBackSlashes(FileName);
    PatchIE5(FileName);

    { Gives the application a chance to do the work for us }
    Result := '';
    TriggerBuildFilePath(Client, Directory, FileName, Result);
    if Length(Result) > 0 then
        Exit;                     { Work is done at the app level, done }

    if IsUNC(FileName) then
        Result := FileName
    else if IsUNC(Directory) then begin
        if (Length(FileName) > 0) and (FileName[1] = '\') then begin
            if (ftpCdUpHome in Client.Options) then              { AG V1.52 }
                { absolute path, HomeDir }
                Result := Client.HomeDir + Copy(FileName, 2, Length(FileName))
            else
                Result := ExtractFileDrive(Directory) + FileName;
        end
        else
            Result := Directory + FileName;
    end
    else begin
        if (Length(FileName) > 1) and (FileName[2] = ':') then begin
            Drive := UpperCase(Copy(FileName, 1, 2));
            Path  := Copy(FileName, 3, Length(FileName));
        end
        else if (ftpCdUpHome in Client.Options) and              { AG V1.52 }
                (Length(FileName) > 0) and (FileName[1] = '\') then begin
                { absolute path, HomeDir }
                Drive := ExtractFileDrive(Client.HomeDir);
                Path  := Copy(Client.HomeDir, Length(Drive) + 1, Length(Client.HomeDir)) +
                              Copy(FileName, 2, Length(FileName));
        end
        else begin
            Drive := Copy(Directory, 1, 2);
            Path  := FileName;
        end;
        if (Length(Path) > 0) and (Path[1] = '\') then
            Result := Drive + Path
        else begin
            if Drive <> Copy(Directory, 1, 2) then
                raise FtpServerException.Create('No current dir for ''' + Drive + '''');
            Result := Directory + Path;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandRETR(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    Allowed     : Boolean;
    FilePath    : TFtpString;
    DelayedSend : Boolean;
begin
    try
        if Client.FtpState <> ftpcReady then begin
            Answer := msgNotLogged;
            Exit;
        end;

        try
            Client.CurCmdType    := ftpcRETR;
            Client.HasOpenedFile := FALSE;
            Client.ZStreamState  := ftpZStateNone;
            Client.FileName      := SlashesToBackSlashes(Params);
            //Allowed              := TRUE;
            FilePath             := BuildFilePath(Client, Client.Directory, Client.FileName);
            Allowed := IsPathAllowed(Client, FilePath); { AG V1.52 }
            TriggerValidateGet(Client, FilePath, Allowed);
            if not Allowed then begin
                Answer := msgRetrDisabled;
                Exit;
            end;
            Client.FilePath := FilePath;
            Answer          := Format(msgRetrSuccess, [Params]);
            DelayedSend     := FALSE;
            if Assigned(FOnGetProcessing) then
                FOnGetProcessing(Self, Client, DelayedSend);
            if not DelayedSend then
                DoStartSendData(Client, Answer);  { angus V1.54 added Answer }
        except
            on E:Exception do begin
                Answer := Format(msgRetrFailed, [E.Message]);
            end;
        end;
    finally
        { check for success 150..159 in passive mode }
        if Client.PassiveMode and (Copy(Answer, 1, 2) <> '15') then begin
            { flag for ClientStorSessionClosed that the error-message was already sent! }
            Client.TransferError    := '';
            Client.AbortingTransfer := TRUE;
            { set up Passive DataSocket.EventHandlers        }
            { otherwise FreeCurrentPasvPort won't be called! }
            PreparePassiveRetrDataSocket(Client);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.DoStartSendData(Client : TFtpCtrlSocket; var Answer : TFtpString);
var
    NewPos  : TFtpBigInt;
    FileExt : String;
    Done    : Boolean;
    FreeSpace: Int64;
begin
 { angus V1.54 moved main file opening here from ClientRetrSessionConnected so it's
   done before opening the data connection so 451 answer can be given for a missing
   file, and also check if thread needs to be started to compress file }
    Client.HashStartPos := 0;
    Client.HashEndPos := 0;
    Client.ZStreamState := ftpZStateNone;
    Client.ZCompInfo := '';  { text added to 226 OK answer }

{ We need to open a datastream if not already done and a FilePath }
{ exists the component user can have nullified the FilePath or    }
{ created his own data stream (virtual file feature)              }
    try
        if (not Client.HasOpenedFile) and (Length(Client.FilePath) > 0) and
                                       (not Assigned(Client.DataStream)) then begin
            TriggerEnterSecurityContext(Client);
            try
                if not FileExists(Client.FilePath) then begin
                    Answer := Format(msgRetrNotExists,
                                     [FormatResponsePath(Client, Client.FilePath)]);
                    Exit;
                end;
              {  Client.DataStream := TFileStream.Create(Client.FilePath,
                                                 fmOpenRead + fmShareDenyNone);  }
                Client.DataStream := OpenFileStream(Client.FilePath,
                                                Client.FileModeRead); { angus V1.57 }
                NewPos := Client.DataStream.Seek(Client.RestartPos,
                {$IFDEF STREAM64} soBeginning {$ELSE} sofromBeginning {$ENDIF});
            finally
                TriggerLeaveSecurityContext(Client);
            end;
            if NewPos <> Client.RestartPos then begin
                Answer := Format(msgRetrFailed, ['Unable to set resume position in local file']);
                CloseFileStreams(Client);      { angus V1.54 }
                Exit;
            end;
            Client.HasOpenedFile := TRUE;
        end;
        if (not Assigned(Client.DataStream)) then begin
            Answer := Format(msgRetrFailed, ['Failed to open local file']);
            Exit;
        end;
        Client.LastTick := IcsGetTickCountX;  { angus V1.54 last tick for time out checking }

{$IFDEF USE_MODEZ}     { angus V1.54 see if compressing the file with zlib }
        if (Client.CurrTransMode = ftpTransModeZDeflate) then begin
            Client.ZStreamState := ftpZStateSaveComp;
            Client.ZCurLevel := Client.ZReqLevel;
            Done := false;
            FreeAndNil (Client.ZFileStream);
            if Client.FilePath <> '' then begin { directory listings don't have file name }
             { don't try to compress certain files any further }
                FileExt := ExtractFileExt(LowerCase(Client.FilePath));
                if Pos (FileExt, FZlibNoCompExt) > 0 then
                                               Client.ZCurLevel := Z_NO_COMPRESSION;
                if Client.DataStream.Size > FZlibMaxSize then            { angus V1.55 }
                                               Client.ZCurLevel := Z_NO_COMPRESSION;
             { check sufficient space on work volume for compressed file }
                FreeSpace := GetFreeSpacePath (FZlibWorkDir);
                if (Client.DataStream.Size + 100000) > FreeSpace then begin   { don't fill volume!! }
                    TriggerDisplay(Client, 'Insufficient space on ' + FZlibWorkDir +
                        ', need ' + IntToKByte (Client.DataStream.Size) + ', free ' + IntToKByte (FreeSpace));
                    Answer := Format(msgRetrFailed, ['Failed to compress file, insufficient space']);
                    Exit;
                end;
                Client.ZCompFileName := FZlibWorkDir + GetZlibCacheFileName(Client.FilePath);
                Client.ZCompFileDelete := True;
                TriggerUpCompressFile (Client, Done);
                if Done then begin
                    if NOT Assigned (Client.ZFileStream) then begin
                        Done := false;
                        TriggerDisplay(Client, 'Error: no cache file set in UpCompressFile event');
                    end;
                    Client.ZCompInfo := ' compressed size ' + IntToKbyte
                             (Client.ZFileStream.Size) + 'bytes, uncompressed size ' +
                                         IntToKbyte (Client.DataStream.Size) + 'bytes' ;
                end;
            end
            else begin
                Client.ZCurLevel := Z_BEST_SPEED;
                Client.ZCompFileName := 'Directory: ' + Client.DirListPath ;
                Client.ZCompFileDelete := False;
            end;
            if NOT Done then begin
                if (Client.ProcessingThread <> nil) then begin
                    Answer := Format(msgRetrFailed, ['Failed to compress file, busy']);
                    CloseFileStreams(Client);
                    Exit;
                end;
                TriggerEnterSecurityContext(Client);
                try
                    if Client.FilePath <> '' then begin
                        if FileExists(Client.ZCompFileName) then begin
                                                 DeleteFile (Client.ZCompFileName);
                        end;
                        Client.ZFileStream := OpenFileStream(Client.ZCompFileName, fmCreate);
                    end
                    else
                        Client.ZFileStream := TMemoryStream.Create;
                except
                    Answer := Format(msgRetrFailed, ['Failed to create compress file']);
                    CloseFileStreams(Client);
                    TriggerLeaveSecurityContext(Client);
                    Exit;
                end ;
                TriggerDisplay(Client, 'Using thread to compress upload file: ' +
                         Client.ZCompFileName + ', Level ' + IntToStr (Client.ZCurLevel));
                Client.ProcessingThread := TClientProcessingThread.Create(TRUE);
                Client.ProcessingThread.Client := Client;
                Client.ProcessingThread.InData := Answer;
                Client.ProcessingThread.Keyword := 'COMPRESS';
                Client.ProcessingThread.OnTerminate := ClientProcessingThreadTerminate;
                Client.ProcessingThread.FreeOnTerminate := TRUE;
                Client.ProcessingThread.Resume;
                { Since answer is sent later when the thread returns we need }
                { to set this flag!                                          }
                Client.AnswerDelayed := TRUE;
                exit;
            end;
        end;
{$ENDIF}
        PostMessage(Handle, FMsg_WM_FTPSRV_START_SEND, 0, LongInt(Client));
    except
        on E: Exception do begin
            Answer := Format(msgRetrFailed, [E.Message]);
            CloseFileStreams(Client);      { angus V1.54 }
            Exit;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.WMFtpSrvStartSend(var msg: TMessage);
var
    Client      : TFtpCtrlSocket;
begin
    Client := TObject(Msg.LParam) as TFtpCtrlSocket;
    StartSendData(Client);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.ClientPassiveSessionAvailable(Sender : TObject; AError  : Word);
var
    HSocket : TSocket;
    Client  : TFtpCtrlSocket;
    Data    : TWSocket;
begin
    Data    := TWSocket(Sender);
    Client  := TFtpCtrlSocket(Data.Owner);
    HSocket := Data.Accept;
    Data.OnSessionClosed := nil;
    Data.Close;   { We don't need to listen any more }

    if Client.CurCmdType in [ftpcSTOR, ftpcAPPE, ftpcSTOU] then begin { FLD V1.45 fixed ftpcSTOU }
        Client.DataSocket.OnSessionConnected  := ClientStorSessionConnected;
        Client.DataSocket.OnSessionClosed     := ClientStorSessionClosed;
        Client.DataSocket.OnDataAvailable     := ClientStorDataAvailable;
        Client.DataSocket.OnDataSent          := nil;
    end
    else if Client.CurCmdType in [ftpcRETR, ftpcLIST, ftpcNLST, ftpcMLSD, ftpcSiteDMLSD] then begin  { angus V1.41, V1.54 }
        Client.DataSocket.OnSessionConnected  := ClientRetrSessionConnected;
        Client.DataSocket.OnSessionClosed     := ClientRetrSessionClosed;
        Client.DataSocket.OnDataAvailable     := nil;
        Client.DataSocket.OnDataSent          := ClientRetrDataSent;
    end
    else begin
        Client.DataSocket.OnSessionConnected  := nil;
        Client.DataSocket.OnSessionClosed     := nil;
        Client.DataSocket.OnDataAvailable     := nil;
        Client.DataSocket.OnDataSent          := nil;
    end;
    Client.DataSocket.LingerOnOff             := wsLingerOff;
    Client.DataSocket.LingerTimeout           := 0;
    Client.DataSocket.HSocket                 := HSocket;
    Client.PassiveConnected                   := TRUE;
    if Client.PassiveStart then
        Client.DataSocket.OnSessionConnected(Client.DataSocket, 0);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.StartSendData(Client : TFtpCtrlSocket);
begin
    Client.AbortingTransfer              := FALSE;
    Client.DataSent                      := FALSE;
    Client.TransferError                 := 'Transfer Ok';
    if Client.PassiveMode then begin
        PreparePassiveRetrDataSocket(Client);
    end
    else begin
        Client.DataSocket.Close;
        Client.DataSocket.Proto              := 'tcp';
        Client.DataSocket.Addr               := Client.DataAddr;
        Client.DataSocket.Port               := Client.DataPort;
        Client.DataSocket.OnSessionConnected := ClientRetrSessionConnected;
        Client.DataSocket.OnSessionClosed    := ClientRetrSessionClosed;
        Client.DataSocket.OnDataAvailable    := nil;
        Client.DataSocket.OnDataSent         := ClientRetrDataSent;
        Client.DataSocket.LingerOnOff        := wsLingerOff;
        Client.DataSocket.LingerTimeout      := 0;
{$IFDEF BIND_FTP_DATA}
        Client.DataSocket.LocalAddr           := Client.GetXAddr;
        Client.DataSocket.LocalPort           := 'ftp-data'; {20}
{$ENDIF}
        Client.DataSocket.ComponentOptions    := [wsoNoReceiveLoop];
        Client.DataSocket.Connect;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ New setup fuction for all Passive RETR-based data connections               }
procedure TFtpServer.PreparePassiveRetrDataSocket(Client : TFtpCtrlSocket);
begin
    Client.DataSocket.OnSessionConnected  := ClientRetrSessionConnected;
    Client.DataSocket.OnSessionClosed     := ClientRetrSessionClosed;
    Client.DataSocket.OnDataAvailable     := nil;
    Client.DataSocket.OnDataSent          := ClientRetrDataSent;
    if Client.PassiveConnected then
        Client.DataSocket.OnSessionConnected(Client.DataSocket, 0)
    else
        Client.PassiveStart := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.ClientRetrSessionConnected(Sender : TObject; AError  : Word);
var
    Client      : TFtpCtrlSocket;
    Data        : TWSocket;
{    NewPos      : TFtpBigInt;   }
begin
    Data                     := TWSocket(Sender);
    Client                   := TFtpCtrlSocket(Data.Owner);
    Client.DataSessionActive := (AError = 0);

    if Client.AbortingTransfer then
        Exit; { primary command (e.g. RETR) failed - don't trigger }
              { RetrSessionConnected or prepare any data/stream    }

    try
        TriggerRetrSessionConnected(Client, Data, AError);
        if AError <> 0 then
        begin
            raise FtpServerException.Create('Client data socket connection Error - ' +
               GetWinsockErr(AError) + ' - ' + Client.DataAddr + ':' + Client.DataPort); { V1.48 report port in proper decimal }
        end;
        { We need to open a datastream if not already done and a FilePath }
        { exists the component user can have nullified the FilePath or    }
        { created his own data stream (virtual file feature)              }
    (*    if (not Client.HasOpenedFile) and
           (Length(Client.FilePath) > 0) and
           (not Assigned(Client.DataStream)) then begin
            TriggerEnterSecurityContext(Client); { AG V1.52 }
            try
                if not FileExists(Client.FilePath) then begin
                    { Avoid unnecessary exception here }
                    Client.AbortingTransfer := TRUE;
                    Client.TransferError    := 'File not found: "' +    { AG V1.52 }
                               FormatResponsePath(Client, Client.FilePath) + '"';
                    PostMessage(Handle, FMsg_WM_FTPSRV_ABORT_TRANSFER,
                            WPARAM(Client.ID), LPARAM(Client));
                    Exit;
                end;
              {  Client.DataStream := TFileStream.Create(Client.FilePath,
                                                 fmOpenRead + fmShareDenyNone);  }
                Client.DataStream := OpenFileStream(Client.FilePath,
                                                 fmOpenRead + fmShareDenyNone);  { angus 1.54 }
                NewPos := Client.DataStream.Seek(Client.RestartPos,
                {$IFDEF STREAM64} soBeginning {$ELSE} sofromBeginning {$ENDIF});  { V1.49 }
            finally
                TriggerLeaveSecurityContext(Client); { AG V1.52 }
            end;
            if NewPos <> Client.RestartPos then begin
                Client.TransferError    := 'Unable to set resume position in local file';
                Client.AbortingTransfer := TRUE;
                PostMessage(Handle, FMsg_WM_FTPSRV_ABORT_TRANSFER,
                            WPARAM(Client.ID), LPARAM(Client));
                Exit;
            end;
            Client.HasOpenedFile := TRUE;
        end;     *)

    except
        on E: Exception do begin
            Client.AbortingTransfer := TRUE;
            Client.TransferError    := E.Message;
            PostMessage(Handle, FMsg_WM_FTPSRV_ABORT_TRANSFER,
                        WPARAM(Client.ID), LPARAM(Client));
            Exit;
        end;
    end;
  { now start sending data stream }
    Client.ByteCount := 0;
    Client.XferStartTick := IcsGetTickCountX; { angus V1.54 tick when last xfer started, for performance check }
    Client.LastTick := IcsGetTickCountX;      { angus V1.54 last tick for time out checking }
    SendNextDataChunk(Client, Data);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.ClientRetrSessionClosed(Sender : TObject; AError  : Word);
var
    Client      : TFtpCtrlSocket;
    Data        : TWSocket;
    Duration    : Integer;
    S           : String;
    BytesSec    : Int64;
begin
    Data                     := TWSocket(Sender);
    Client                   := TFtpCtrlSocket(Data.Owner);

{ !!!!!!!! NGB: Free Up Current Port - next 2 lines added }
    if Client.PassiveMode then // FLD 29.12.05
        FreeCurrentPasvPort(Client);
{ !!!!!!!! NGB: previous 2 lines added }

    Client.DataSessionActive := FALSE;
    Client.PassiveStart      := FALSE;
    Client.PassiveConnected  := FALSE;
    Client.RestartPos        := 0;
    { Reset data port to standard value }
    Client.DataPort          := 'ftp-data';

    { If we had opened a data stream ourself, then close it }
    CloseFileStreams(Client);      { angus V1.54 }
{    if Client.HasOpenedFile then begin
        if Assigned(Client.DataStream) then begin
            Client.DataStream.Destroy;
        end;
        Client.DataStream    := nil;
        Client.HasOpenedFile := FALSE;
    end;  }

{ angus V1.54 report performance }
    if Assigned(FOnDisplay) then begin
        Duration := IcsElapsedMsecs (Client.XferStartTick);
        S := Client.FilePath;
        if S = '' then S := 'Directory';
        S := S + ' ' + IntToKbyte(Client.ByteCount) + 'bytes sent in ';
        if Duration < 2000 then
            S := S + IntToStr(Duration) + ' milliseconds'
        else begin
            S := S + IntToStr(Duration div 1000) + ' seconds';
            if Client.ByteCount > 32767 then
                BytesSec := 1000 * (Client.ByteCount div Duration)
            else
                BytesSec := (1000 * Client.ByteCount) div Duration;
            S := S + ' (' + IntToKbyte(BytesSec) + 'bytes/sec)';
        end;
        TriggerDisplay (Client, S);
    end;

    if Client.AbortingTransfer and (Client.TransferError = '') then
        Exit; { This happens when the command itself was failed - do not      }
              { reply on command channel and don't trigger RetrSessionClosed! }

    if Client.AbortingTransfer then
        SendAnswer(Client, Format(msgRetrFailed, [Client.TransferError]))
    else if AError <> 0 then
        SendAnswer(Client, Format(msgRetrFailed, ['Error - ' + GetWinsockErr(AError)]))
    else
        SendAnswer(Client, msgRetrOk + Client.ZCompInfo);  { angus V1.54 }

    TriggerRetrSessionClosed(Client, Data, AError);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.SendNextDataChunk(
    Client : TFtpCtrlSocket;
    Data   : TWSocket);
var
    Count : LongInt;
begin
    try
        Count := 0;
            TriggerEnterSecurityContext(Client);           { AG V1.52 }
            try
{$IFDEF USE_MODEZ}         { angus V1.54 }
            if Client.ZStreamState = ftpZStateSaveComp then begin
                if Assigned(Client.ZFileStream) then
                    Count := Client.ZFileStream.Read(Client.RcvBuf^, Client.RcvSize);
            end
            else begin
{$ENDIF}
                if Assigned(Client.DataStream) then
                Count := Client.DataStream.Read(Client.RcvBuf^, Client.RcvSize);
            end;
            finally
                TriggerLeaveSecurityContext(Client);       { AG V1.52 }
            end;
        Client.LastTick := IcsGetTickCountX;  { angus V1.54 last tick for time out checking }

        if Count > 0 then begin
            Client.ByteCount := Client.ByteCount + Count;
            Client.TotGetBytes := Client.TotGetBytes + Count;    { angus V1.54 }
            Data.Send(Client.RcvBuf, Count);
        end
        else begin { EOF }
            if not Client.DataSent then begin
                Client.DataSent := TRUE;
                PostMessage(Handle, FMsg_WM_FTPSRV_CLOSE_DATA,
                            WPARAM(Client.ID), LPARAM(Client));
            end;
        end;
    except
        { An exception occured, so we abort the transfer }
        on E:Exception do begin
            Client.TransferError    := E.Message;
            Client.AbortingTransfer := TRUE;
            PostMessage(Handle, FMsg_WM_FTPSRV_ABORT_TRANSFER,
                        WPARAM(Client.ID), LPARAM(Client));
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.ClientRetrDataSent(Sender : TObject; AError : Word);
var
    Client : TFtpCtrlSocket;
    Data   : TWSocket;
begin
    Data   := TWSocket(Sender);
    Client := TFtpCtrlSocket(Data.Owner);

    if Client.AbortingTransfer then
        Exit;

    try
        { Trigger the user event for the received data }
        TriggerRetrDataSent(Client, Data, AError);
        if AError <> 0 then
            raise FtpServerException.Create('Send Error - ' + GetWinsockErr(AError));
        SendNextDataChunk(Client, Data);
    except
        { An exception occured, so we abort the transfer }
        on E:Exception do begin
            Client.TransferError    := E.Message;
            Client.AbortingTransfer := TRUE;
            SendAnswer(Client, Format(msgRetrAborted, [Client.TransferError]));
            PostMessage(Handle, FMsg_WM_FTPSRV_ABORT_TRANSFER,
                        WPARAM(Client.ID), LPARAM(Client));
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandSYST(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    Client.CurCmdType := ftpcSYST;
    Answer            := msgSystem;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandDirectory(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString;
    Detailed    : Boolean);
var
    ListType: TListType;              { angus V1.38 }
begin
    if Detailed then
        ListType := ListTypeUnix
    else
        ListType := ListTypeName;
    CommandDirectory2(Client, Keyword, Params, Answer, ListType); { angus V1.38 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandDirectory2(     { angus V1.38 }
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString;
    ListType    : TListType);               { angus V1.38 }
var
    Path       : TFtpString;
    Args       : String;
    Offset     : Integer;
begin
    try
        CloseFileStreams(Client);      { angus V1.54 }

        try
{ angus 1.54  parse optional file and directory parameters, ie
    (blank)                    ( list all files in working directory set by CWD, shown by PWD )
    -AL                        ( list all files including hidden )
    -R                         ( list all files recursively, include sub-directories )
    -SUBDIRS                   ( list all files recursively, include sub-directories )
    *.zip                      ( list all files with zip extension )
    index.html                 ( list a single file )
    "my index.html"            ( list a single file )
    temp                       ( list all files in specified directory )
    /temp                      ( list all files in specified directory )
    /temp/ -R                  ( list all files in specified directory and sub-directory )
    '/program files' -R
    "/program files/*.zip" -R

        NOTE1: we currently support all parameters for DIR, LIST, NLST, MLSD, SITE INDEX,
              SITE CMLSD, SITE DMLSD which is not really RFC compliant, but useful
        NOTE2: we don't yet support multiple arguments, ie only -R or -AL, not both
        NOTE3: -R or -SUBDIRS recursive listings have a file name with path and leading /, ie
               /download/ALLDEPOTS/all/30=page-022864.zip  }

            Params := SlashesToBackSlashes(Params);
            Path := '';
            Args := '';
            Client.DirListHidden := FALSE;
            Client.DirListSubDir := FALSE;
            Client.DirListType := ListType;

         { angus 1.54  parse parameter for file/path and one argument }
            if Length (Params) > 0 then begin
                if Params [1] = '-' then   { just found a argument }
                    Args := Params
                else begin                 { otherwise filename and option argument }
                   Offset := 1;
                   Path := ScanGetNextArg (Params, Offset);  { keep path or file name }
                   Args := ScanGetNextArg (Params, Offset);  { and argument, if any }
                end;
            end;

         { angus 1.54  check directory arguments }
            if (UpperCase(Args) = '-LA') or (UpperCase(Args)= '-AL') then
                                                         Client.DirListHidden := TRUE;
            if (Args = '-R') or (UpperCase(Args) = '-SUBDIRS') then
                                                         Client.DirListSubDir := TRUE;
            if (Client.CurCmdType = ftpcSiteIndex) then Client.DirListSubDir := TRUE;

         { see if application wants to build listing, if not we do it }
            TriggerBuildDirectory(Client, Path, (ListType <> ListTypeName));      { angus V1.38 }
            Client.FilePath := '';       { make sure no file open attempt }
            if not Assigned(Client.DataStream) then begin
                Client.DataStream    := TMemoryStream.Create;
                Client.HasOpenedFile := TRUE;
                BuildDirectory(Client, Path);          { angus V1.54  }
                if Client.AnswerDelayed then exit ;    { angus V1.54 using a thread }
                TriggerAlterDirectory(Client, Path, (ListType <> ListTypeName));  { angus V1.38 }
                Client.DataStream.Seek(0, 0);
            end;

         { angus V1.54 see if returning listing on control socket instead of data socket }
            if Client.CurCmdType in [ftpcSiteIndex, ftpcSiteCmlsd] then begin
                SetLength (Answer, Client.DataStream.Size) ;
                Client.DataStream.Read (Answer [1], Client.DataStream.Size) ;
                if Client.CurCmdType = ftpcSiteIndex then
                     Answer := Format (msgIndexFollows, [Params]) +
                                                     #13#10 + Answer + msgIndexDone;
                if Client.CurCmdType = ftpcSiteCmlsd then
                     Answer := msgMlstFollows + #13#10 + Answer + msgMlstFollowDone;
                CloseFileStreams(Client);
            end
            else
            begin
            Answer          := msgDirOpen;
                DoStartSendData(Client, Answer);  { angus V1.54 added Answer }
            end;
        except
            on E:Exception do begin
                Answer := Format(msgDirFailed, [E.Message])
            end;
        end;
    finally
        { check for success 150..159 in passive mode }
        if (Client.HasOpenedFile) and (Client.PassiveMode) and
                                            (Copy(Answer, 1, 2) <> '15') then begin
            { flag for ClientRetrSessionClosed that the error-message was already sent! }
            Client.TransferError    := '';
            Client.AbortingTransfer := TRUE;
            { set up Passive DataSocket.EventHandlers        }
            { otherwise FreeCurrentPasvPort won't be called! }
            PreparePassiveRetrDataSocket(Client);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandLIST(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    Client.CurCmdType := ftpcLIST;
    CommandDirectory(Client, KeyWord, Params, Answer, TRUE);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandNLST(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    Client.CurCmdType := ftpcNLST;
    CommandDirectory(Client, KeyWord, Params, Answer, FALSE);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.BuildDirectory(
    Client     : TFtpCtrlSocket;
    var Path   : TFtpString);    { angus 1.54 now Client.Stream and Client.DirListType }
var
    Buf        : String;
    Allowed    : Boolean;
begin

 {  angus 1.54 hidden argument now parsed in CommandDirectory2,
               params is now only path or file name }

 { angus 1.54 remove leading / to keep BuildFilePath happy, probably not backward compatible!! }
    if (Length (Path) >= 1) and (Path [1] = '\') then Path := Copy (Path, 2, 999);
    if Path = '' then
        Client.DirListPath := Client.Directory + '*.*'
    else begin
        if Path[Length(Path)] = '\' then Path := Path + '*.*';
        Client.DirListPath := BuildFilePath(Client, Client.Directory, Path);
    end;

(*    if Path[Length(Path)] = '\' then begin
        Path := Path + '*.*';
        Allowed := IsPathAllowed(Client, Path);                 { AG V1.52 }
    end
    else
        { single file or single directory }
        { This check is not 100% secure. Since we check against the home   }
        { directory path w/o a trailing backslash. It's anyway better than }
        { nothing.                                                         }
        Allowed := IsPathAllowed(Client, Path, TRUE);           { AG V1.52 }   *)

    Allowed := IsPathAllowed(Client, Client.DirListPath);                 { AG V1.52 }
    if not Allowed then { AG V1.52 }
    begin
        Buf := FormatResponsePath(Client, Client.DirListPath) +
                                                 ' Permission denied' + #13#10;
        if Length (Path) >= 1 then
                 Client.DataStream.Write(Path[1], Length(Path));   { angus 1.54 }
        Exit; //***
    end;

 { angus 1.54 see if using a thread to list directory }
    if (((ftpsThreadRecurDirs in Options) and (Client.DirListSubDir)) OR
               (ftpsThreadAllDirs in Options)) and
                        (Client.ProcessingThread = nil) then begin
    TriggerEnterSecurityContext(Client);                  { AG V1.52 }
        TriggerDisplay(Client, 'Using thread to list directory');
        Client.ProcessingThread := TClientProcessingThread.Create(TRUE);
        Client.ProcessingThread.Client := Client;
        Client.ProcessingThread.InData := Path;
        Client.ProcessingThread.Keyword := 'DIRECTORY';
        Client.ProcessingThread.OnTerminate := ClientProcessingThreadTerminate;
        Client.ProcessingThread.FreeOnTerminate := TRUE;
        Client.ProcessingThread.Resume;
        { Since answer is sent later when the thread returns we need }
        { to set this flag!                                          }
        Client.AnswerDelayed := TRUE;
        exit;
        end;
    TriggerEnterSecurityContext(Client);                  { AG V1.52 }
    try
     { angus 1.54 moved all listing code to FtpSrvC }
        Client.BuildDirectory(Path);
    finally
        TriggerLeaveSecurityContext(Client);              { AG V1.52 }
    end;

    if Client.DataStream.Size = 0 then begin
        Buf := FormatResponsePath(Client, Client.DirListPath) + ' not found' + #13#10; { AG V1.52 }
        Client.DataStream.Write(Buf[1], Length(Buf));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandTYPE(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    Buf : String;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    Client.CurCmdType := ftpcTYPE;
    Buf := UpperCase(Trim(Params));
    if (Buf = 'A') or (Buf = 'A N') or (Buf = 'I') then begin
        Answer            := Format(msgTypeOk, [Params]);
        Client.BinaryMode := (Buf = 'I');
    end
    else
        Answer := Format(msgTypeFailed, [Params]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandDELE(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    FileName : TFtpString;
    Allowed  : Boolean;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    Client.CurCmdType := ftpcDELE;
    FileName          := BuildFilePath(Client, Client.Directory, Params);
    //Allowed           := TRUE;
    Allowed := IsPathAllowed(Client, FileName); { AG V1.52 }
    TriggerValidateDele(Client, FileName, Allowed);
    if not Allowed then begin
        Answer := msgDeleDisabled;
        Exit;
    end;
    if Params = '' then begin
        Answer := msgDeleSyntax;                             { V1.52 AG}
        Exit;
    end;
    Allowed := FALSE;
    TriggerEnterSecurityContext(Client);
    try
        if FileExists(FileName) then begin
            if DeleteFile(FileName) then begin
                Answer := Format(msgDeleOk, [FormatResponsePath(Client, FileName)]); 
                Allowed := TRUE;
            end
            else
                Answer := Format(msgDeleFailed, [FormatResponsePath(Client, FileName)]);
        end
        else
            Answer := Format(msgDeleNotExists, [FormatResponsePath(Client, FileName)]);
    finally
        TriggerLeaveSecurityContext(Client);
    end;
    if Allowed then
        { Cached Md5Sum should be deleted }
        TriggerMd5Calculated(Client, FileName, ''); { AG V1.50 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandSIZE(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    FilePath : TFtpString;
    Allowed  : Boolean;
    Size     : TFtpBigInt;   
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    Client.CurCmdType := ftpcSIZE;
    //Allowed           := TRUE;  { AG V1.52 }
    FilePath          := BuildFilePath(Client, Client.Directory, Params);
    Allowed := IsPathAllowed(Client, FilePath); { AG V1.52 }
    TriggerValidateSize(Client, FilePath, Allowed);
    if not Allowed then begin
        Answer := msgSizeDisabled;
        Exit;
    end;

    if Params = '' then
        Answer := msgSizeSyntax                               { V1.52 AG}
    else begin
        try
            TriggerEnterSecurityContext(Client);               { V1.52 AG }
            try
                Size := GetFileSize(FilePath);
                if Size >= 0 then
                    Answer := Format(msgSizeOk, [Size])
                else
                    Answer := Format(msgSizeFailed, ['File not found']);
            finally
                TriggerLeaveSecurityContext(Client);           { V1.52 AG }
            end;
        except
            on E:Exception do begin
                Answer := Format(msgSizeFailed, [E.Message])
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandREST(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    Client.CurCmdType := ftpcREST;
    try
{$IFDEF STREAM64}                { V1.43 }
        Client.RestartPos := atoi64(Params);
{$ELSE}
        Client.RestartPos := atoi(Params);
{$ENDIF}
        if Client.RestartPos < 0 then begin        { 20020916 }
            Answer            := msgRestZero;
            Client.RestartPos := 0;
        end
        else begin
            if (ftpsModeZNoResume in Options) and
                    (Client.CurrTransMode = ftpTransModeZDeflate) then   { angus V1.55 }
                Answer := msgRestNotModeZ
        else
            Answer := Format(msgRestOk, [Client.RestartPos]);
        end;
    except
        on E:Exception do begin
            Answer            := Format(msgRestFailed, [E.Message]);
            Client.RestartPos := 0;
        end;
    end;
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandRNFR(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    FileName : TFtpString;
    Allowed  : Boolean;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    Client.CurCmdType := ftpcRNFR;
    FileName          := BuildFilePath(Client, Client.Directory, Params);
    //Allowed           := TRUE; { AG V1.52 }
    Allowed := IsPathAllowed(Client, FileName); { AG V1.52 }
    TriggerValidateRnFr(Client, FileName, Allowed);
    if not Allowed then begin
        Answer := msgRnFrDisabled;
        Exit;
    end;
    if Params = '' then
        Answer := msgRnfrSyntax                              { V1.52 AG}
    else begin
        TriggerEnterSecurityContext(Client);                 { V1.52 AG }
        try
            if FileExists(FileName) or DirExists(Filename) then begin
                Client.FromFileName := FileName;
                Answer              := msgRnfrOk;            { V1.52 AG }
            end
            else
                Answer := Format(msgRnfrNotExists, [FormatResponsePath(Client, FileName)]);
        finally
            TriggerLeaveSecurityContext(Client);             { V1.52 AG }
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandRNTO(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    FileName : TFtpString;
    Allowed  : Boolean;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    Client.CurCmdType := ftpcRNTO;
    FileName          := BuildFilePath(Client, Client.Directory, Params);
    //Allowed           := TRUE;  { AG V1.52 }
    Allowed := IsPathAllowed(Client, FileName); { AG V1.52 }
    TriggerValidateRnTo(Client, FileName, Allowed);
    if not Allowed then begin
        Answer := msgRnToDisabled;
        Exit;
    end;
    if Params = '' then begin
        Answer := msgRntoSyntax;                              { V1.52 AG}
        Exit;
    end;
    Allowed := FALSE;                                         { V1.52 AG }
    TriggerEnterSecurityContext(Client);                      { V1.52 AG }
    try
        if FileExists(FileName) or DirExists(Filename) then
            Answer := Format(msgRntoAlready, [FormatResponsePath(Client, FileName)])
        else if (not FileExists(Client.FromFileName)) and
           (not DirExists(Client.FromFileName)) then
            Answer := Format(msgRntoNotExists, [FormatResponsePath(Client, Client.FromFileName)])
        else begin
            Client.ToFileName := FileName;
            Allowed := RenameFile(Client.FromFileName, Client.ToFileName);
        end;
    finally
        TriggerLeaveSecurityContext(Client);                  { V1.52 AG }
    end;
    if Allowed then begin
        Answer := Format(msgRntoOk, [FormatResponsePath(Client, Client.FromFileName),
                                    FormatResponsePath(Client, Client.ToFileName)]);
        { Cached Md5Sum should be updated with a new key } { AG V1.50 }
        TriggerMd5Calculated(Client, FileName, '');
    end
    else
        Answer := Format(msgRntoFailed, [FormatResponsePath(Client, Client.FromFileName)]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandNOOP(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    Client.CurCmdType := ftpcNOOP;
    Answer            := Format(MsgNoopOk, [Params]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandMKD(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    Dir : TFtpString;                                    { V1.52 AG}
    Allowed  : Boolean;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    
    try
        Client.CurCmdType := ftpcMKD;
        Dir               := BuildFilePath(Client, Client.Directory, Params);
        //Allowed           := TRUE;  { AG V1.52 }
        Allowed := IsPathAllowed(Client, Dir); { AG V1.52 }
        TriggerMakeDirectory(Client, Dir, Allowed);
        if not Allowed then
            Answer := Format(msgMkdFailed, [FormatResponsePath(Client, Dir)]) { V1.52 AG }
        else if Params = '' then
            Answer := msgMkdSyntax                              { V1.52 AG}
        else begin
            TriggerEnterSecurityContext(Client);                { V1.52 AG }
            try
                if DirExists(Dir) or FileExists(Dir) then       { V1.52 AG }
                    Answer := Format(msgMkdAlready, [FormatResponsePath(Client, Dir)]) { V1.52 AG }
                else begin
                    {$I-}
                    MkDir(Dir);
                    if IOResult = 0 then
                        Answer := Format(msgMkdOk, [FormatResponsePath(Client, Dir)]) { V1.52 AG }
                    else
                        Answer := Format(msgMkdFailed, [FormatResponsePath(Client, Dir)]); { V1.52 AG }
                    {$I+}
                end;
            finally
                TriggerLeaveSecurityContext(Client);            { V1.52 AG }
            end;
        end;
    except
        on E:Exception do begin
            Answer := Format(msgMkdFailed, [E.Message])
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandAPPE(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    Allowed  : Boolean;
    FilePath : TFtpString;
begin
    try
        if Client.FtpState <> ftpcReady then begin
            Answer := msgNotLogged;
            Exit;
        end;

        try
            Client.CurCmdType       := ftpcAPPE;
            Client.FileName         := SlashesToBackSlashes(Params);
            Client.HasOpenedFile    := FALSE;
            //Allowed           := TRUE;  { AG V1.52 }
            FilePath                := BuildFilePath(Client, Client.Directory, Client.FileName);
            Allowed := IsPathAllowed(Client, FilePath); { AG V1.52 }
            TriggerValidatePut(Client, FilePath, Allowed);
            if not Allowed then begin
                Answer := msgAppeDisabled;
                Exit;
            end;
            Client.FilePath := FilePath;
            PrepareStorDataSocket(Client);
            Client.RestartPos := GetFileSize(Client.FilePath);
            if Client.RestartPos < 0 then
                Client.RestartPos := 0;
            Answer := Format(msgAppeReady, [Params,Client.RestartPos]);
        except
            on E:Exception do begin
                Answer := Format(msgAppeFailed, [E.Message]);
            end;
        end;
    finally
        { check for success 150..159 in passive mode }
        if Client.PassiveMode and (Copy(Answer, 1, 2) <> '15') then begin
            { flag for ClientStorSessionClosed that the error-message was already sent! }
            Client.TransferError    := '';
            Client.AbortingTransfer := TRUE;

            { set up Passive DataSocket.EventHandlers         }
            {  otherwise FreeCurrentPasvPort won't be called! }
            PreparePassiveStorDataSocket(Client);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandSTRU(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    Client.CurCmdType := ftpcSTRU;
    Answer            := Format(MsgStruOk, [Params]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandRMD(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    Dir      : TFtpString;   { V1.52 AG }
    Allowed  : Boolean;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    Client.CurCmdType := ftpcRMD;
    Dir               := BuildFilePath(Client, Client.Directory, Params);
    //Allowed           := TRUE;  { AG V1.52 }
    Allowed := IsPathAllowed(Client, Dir); { AG V1.52 }
    TriggerValidateRmd(Client, Dir, Allowed);
    if not Allowed then begin
        Answer := msgRmdDisabled;
        Exit;
    end;
    if Params = '' then begin
        Answer := msgMkdSyntax;                            { V1.52 AG}
        Exit;
    end;
    TriggerEnterSecurityContext(Client);                    { V1.52 AG }
    try
        if not DirExists(Dir) then
            Answer := Format(msgRmdNotExists, [FormatResponsePath(Client, Dir)])
        else begin
            {$I-}
            RmDir(Dir);
            if IOResult = 0 then
                Answer := Format(msgRmdOk, [FormatResponsePath(Client, Dir)])
            else
                Answer := Format(msgRmdFailed, [FormatResponsePath(Client, Dir)]);
            {$I+}
        end;
    finally
        TriggerLeaveSecurityContext(Client);               { V1.52 AG }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandABOR(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Client.DataSocket.State = wsConnected then begin
        Client.TransferError    := 'ABORT requested by client';
        Client.AbortingTransfer := TRUE;
        Client.DataSocket.Close;
    end;
    Answer := msgAborOk;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpServer.GetNextAvailablePasvPort : String;
var
    I        : Integer;
    NewPort  : Integer;
    TablePtr : PBoolean;
begin
    if (FPasvPortRangeSize = 0) or (FPasvPortRangeStart = 0) then
        Result := '0'
    else begin
        Result := '0';
        I := 0;
  { angus V1.56 - allocate sequential ports within range instead of same low ports }
        if FPasvNextNr >= FPasvPortRangeSize then FPasvNextNr := 0;      { angus V1.56 }
        while TRUE do begin
            TablePtr := PBoolean(PChar(FPasvPortTable) + (SizeOf(Boolean) * FPasvNextNr));
            if TablePtr^ = FALSE then begin
                TablePtr^ := TRUE;
             //   NewPort   := FPasvPortRangeStart + I;
                NewPort   := FPasvPortRangeStart + FPasvNextNr;          { angus V1.56 }
                Inc(FPasvNextNr);                                        { angus V1.56 }
                Result    := IntToStr(NewPort);
                break;
            end;
            Inc(FPasvNextNr);                                            { angus V1.56 }
            if FPasvNextNr >= FPasvPortRangeSize then FPasvNextNr := 0;  { angus V1.56 }
            Inc(I);
            if I >= FPasvPortRangeSize then
                break;  { no free ports in range - angus V1.56 }
            // Never used ! Inc(TablePtr);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.FreeCurrentPasvPort(AClient : TFtpCtrlSocket);
var
    CurrentPort : Integer;
    ErrorCode   : Integer;
begin
    if (FPasvPortRangeSize = 0) or (FPasvPortRangeStart = 0) then
        Exit;          
    { FLD changed following lines, because                                   }
    { FreeCurrentPasvPort might be called when the socket is already closed! }
    if AClient.DataSocket.State = wsClosed then
        Val(AClient.DataSocket.Port, CurrentPort, ErrorCode)
    else
        Val(AClient.DataSocket.GetXPort, CurrentPort, ErrorCode);
    if (CurrentPort >= FPasvPortRangeStart) and
       (CurrentPort <= (FPasvPortRangeStart + FPasvPortRangeSize)) then begin
        PBoolean(PChar(FPasvPortTable) +
                 SizeOf(Boolean) * (CurrentPort - FPasvPortRangeStart))^ := FALSE;
    end;
    AClient.PassiveMode := FALSE;  // FLD 29.12.05
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsIpPrivate(saddr : TInAddr): Boolean;                    { AG V1.51 }
begin
    Result := (Byte(saddr.S_un_b.s_b1) = 10) or   // private class A
              (saddr.S_un_w.s_w1       = 4268) or // private class B
              (saddr.S_un_w.s_w1       = 43200);  // private class C
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandPASV(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    saddr     : TSockAddrIn;
    saddrlen  : Integer;
    DataPort  : Integer;
    IPAddr    : TInAddr;
    PASVAddr  : TInAddr;
    APasvIp   : TFtpString;
    SetPasvIp : Boolean;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    try
        { Get our IP address from our control socket }
        saddrlen := SizeOf(saddr);
        Client.GetSockName(saddr, saddrlen);
        IPAddr   := saddr.sin_addr;

        { FLD Make sure to free up a previous connected passive data-socket!! }
        { can happen if a PASV-command is issued, but a passive connection is }
        { never connected, and then a subsequent PASV-command is issued.      }
        if Client.PassiveMode then // FLD 29.12.05
            FreeCurrentPasvPort(Client);

        Client.DataSocket.Close;
        Client.DataSocket.Addr  := '0.0.0.0';   { Any addr }

        Client.DataSocket.Port  := GetNextAvailablePasvPort; { '0';          Any port  }
        if Client.DataSocket.Port = '' then
            raise Exception.Create('No available PASV Ports');

        Client.DataSocket.Proto := 'tcp';
        Client.DataSocket.OnSessionAvailable := ClientPassiveSessionAvailable;
        Client.DataSocket.OnSessionConnected := nil;
        Client.DataSocket.OnSessionClosed    := nil;
        Client.DataSocket.OnDataAvailable    := nil;
        Client.DataSocket.ComponentOptions   := [wsoNoReceiveLoop];
        Client.DataSocket.Listen;
{        if Client.DataSocket.Listen <> 0 then
            raise Exception.Create('Listen failed'); 18/11/98 }

        { Get the port assigned by winsock }
        saddrlen := SizeOf(saddr);
        Client.DataSocket.GetSockName(saddr, saddrlen);
        DataPort := WSocket_ntohs(saddr.sin_port);

        if Client.sin.sin_addr.s_addr = WSocket_htonl($7F000001) then
            Answer := Format(msgPasvLocal,
                          [HiByte(DataPort),
                           LoByte(DataPort)])
        else begin
            APasvIp := FPasvIpAddr;
            SetPasvIp := (APasvIp <> '') and (not
                         (((ftpsNoPasvIpAddrInLan in FOptions) and
                           IsIpPrivate(Client.PeerSAddr.sin_addr)) or
                          ((ftpsNoPasvIpAddrSameSubnet in FOptions) and
                           WSocket2IsAddrInSubNet(Client.PeerSAddr.sin_addr))));

            if Assigned(FOnPasvIpAddr) then begin
                FOnPasvIpAddr(Self, Client, APasvIp, SetPasvIp);
                SetPasvIp := SetPasvIp and (APasvIp <> '');
            end;
             
            if not SetPasvIp then
                Answer := Format(msgPasvRemote,
                          [ord(IPAddr.S_un_b.s_b1),
                           ord(IPAddr.S_un_b.s_b2),
                           ord(IPAddr.S_un_b.s_b3),
                           ord(IPAddr.S_un_b.s_b4),
                           HiByte(DataPort),
                           LoByte(DataPort)])
            else begin
                PASVAddr.S_addr := WSocket_inet_addr(APasvIp);
                if (PASVAddr.S_addr = u_long(INADDR_NONE)) or
                            (PASVAddr.S_addr = 0) then { angus v1.53 0.0.0.0 not allowed }
                        raise Exception.Create('Invalid PASV IP Address')
                else
                        Answer := Format(msgPasvRemote,
                              [ord(PASVAddr.S_un_b.s_b1),
                               ord(PASVAddr.S_un_b.s_b2),
                               ord(PASVAddr.S_un_b.s_b3),
                               ord(PASVAddr.S_un_b.s_b4),
                               HiByte(DataPort),
                               LoByte(DataPort)]);
            end;
        end;

        Client.PassiveMode      := TRUE;
        Client.PassiveStart     := FALSE;
        Client.PassiveConnected := FALSE;
    except
        on E:Exception do begin
            Answer := Format(msgPasvExcept, [E.Message]);
            try
                Client.DataSocket.Close;
            except
                { Ignore any exception here }
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ angus V1.38  added set modification date and time version                        }
{ angus v1.53  support fractional seconds, usually milliseconds, updating time
{ MDTM default.asp                    get modification date and time               }
{ MFMT 20040804102811 default.asp     set modification date and time UTC time      }
{ MDTM 20040804102811 default.asp     set modification date and time local time    }
{ MDTM 20040804102811+60 default.asp  set modification date and time UTC + 60 mins }
{ MDTM 20040804102811-60 default.asp  set modification date and time UTC - 60 mins }
{ MFMT 20040804102811.1 default.asp   set modification date and time UTC time      }
{ MFMT 20040804102811.12 default.asp  set modification date and time UTC time      }
{ MFMT 20040804102811.123 default.asp set modification date and time UTC time      }
procedure TFtpServer.CommandMDTM(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    FileTime : String;
    FileName : TFtpString;
    I, J     : Integer;
    UtcFlag  : Boolean;
    SuccFlag : Boolean;
    FileDT   : TDateTime;
    Bias     : Integer;
    Allowed  : Boolean;         { angus V1.39 }
    FExists  : Boolean;
begin
    if Client.FtpState <> ftpcReady then begin   { angus V1.39 }
        Answer := msgNotLogged;
        Exit;
    end;

    try
        if Keyword = 'MFMT' then            { angus V1.39 else assume MDTM }
            Client.CurCmdType := ftpcMFMT
        else
            Client.CurCmdType := ftpcMDTM;
        J                 := 1;
        FileDT            := 0;
        UtcFlag           := FALSE;
        Allowed           := TRUE;

        { look for numeric date and time - angus V1.53 with or without millisecs }
        while (J <= Length(Params)) and
              (((Params[J] >= '0') and (Params[J] <= '9')) or (Params[J] = '.')) do
           Inc(J);
        if (J >= 15) and (J <= 19) then begin  { found date and time so we are setting it, not getting it }
            FileDT := MDTM2Date (Copy (Params, 1, J - 1));
            if FileDT < 10 then begin
                Answer := msgMdtmSyntax;
                Exit;
            end;
            I := J;

            { see if UTC time offset in minutes is passed }
            while (J <= Length(Params)) and
                  ((Params[J] = '+') or (Params[J] = '-') or
                   ((Params[J] >= '0') and (Params[J] <= '9'))) do
                Inc(J);
            if Client.CurCmdType = ftpcMFMT then
                UtcFlag := TRUE
            else begin
                if I <> J then begin
                    UtcFlag := TRUE;
                    Bias := atosi(Copy (Params, I, 4));   { signed integer, +60, -120, +0 }
                    if Bias <> 0 then FileDT := FileDT + (Bias / (60.0 * 24.0));
                end;
            end;
        end
        else
            J := 1;
        while (J <= Length(Params)) and (Params[J] = ' ') do
           Inc(J);
        FileName := BuildFilePath(Client, Client.Directory , Copy (Params, J, 999));
        if Params = '' then begin
            Answer := msgMdtmSyntax;
            Exit;
        end;
        TriggerEnterSecurityContext(Client);                  { V1.52 AG }
        try
            FExists := FileExists(FileName) OR DirExists(FileName);  { A. Haas, V1.53 }
        finally
            TriggerLeaveSecurityContext(Client);              { V1.52 AG }
        end;
        if not FExists then
            Answer := Format(msgMdtmNotExists, [FormatResponsePath(Client, FileName)])
        else if FileDT <> 0 then begin     { set file time stamp }
            Allowed := IsPathAllowed(Client, FileName); { AG V1.52 }
            TriggerValidateMfmt(Client, FileName, Allowed);   { angus V1.39 }
            if not Allowed then begin
                Answer := msgStorDisabled;
                Exit;
            end;
            TriggerEnterSecurityContext(Client);              { V1.52 AG }
            try
                if UtcFlag then
                    SuccFlag := UpdateUFileAge (FileName, FileDT)
                else
                    SuccFlag := UpdateFileAge (FileName, FileDT);
            finally
                TriggerLeaveSecurityContext(Client);          { V1.52 AG }
            end;
            if SuccFlag then begin
                if Client.CurCmdType = ftpcMFMT then    { angus V1.39 }
                    Answer := msgMfmtChangeOK
                else
                    Answer := msgMdtmChangeOK ;
            end
            else
                Answer := msgMdtmChangeFail;
        end
        else if Client.CurCmdType = ftpcMFMT then   { angus V1.39 never returns time }
            Answer := msgMdtmSyntax
        else begin
            TriggerEnterSecurityContext(Client);              { V1.52 AG }
            try
                FileTime := FileUtcStr(FileName);   { return file time stamp }
            finally
                TriggerLeaveSecurityContext(Client);          { V1.52 AG }
            end;
            if Length(FileTime) <> 0 then
                Answer := Format(msgMdtmOk, [FileTime])
            else
                Answer := Format(msgMdtmFailed,
                                 ['UTC File time retrieval failed']) ;
        end;
    except
        on E:Exception do begin
            Answer := Format(msgMdtmChangeFail, [E.Message])
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandMode(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    FreeSpace: Int64;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    if (Params = '') then begin
        Answer := msgModeSyntax;
        Exit;
    end;
    Params := Uppercase (Params);
    if (Params <> 'S') then begin
        Answer := Format (msgModeNotS, [Params]);
{$IFDEF USE_MODEZ}              { angus V1.54 }
        if (ftpModeZCompress in Client.Options) and (Params = 'Z') then begin
       { check sufficient space on work volume for compressed files }
            try
                ForceDirectories(FZlibWorkDir);
                FreeSpace := GetFreeSpacePath (FZlibWorkDir);
            except
                FreeSpace := -1;
            end;
            if FZlibMinSpace > FreeSpace then begin   { don't fill volume!! }
                if FreeSpace = -1 then
                    TriggerDisplay(Client, 'Error, working directory volume not available ' +
                                   FZlibWorkDir + ' - ' + GetWindowsErr (GetLastError))
                else
                    TriggerDisplay(Client, 'Insufficient space on ' + FZlibWorkDir +
                     ', need ' + IntToKByte(FZlibMinSpace) + ', free ' + IntToKByte(FreeSpace));
                Client.CurrTransMode := FtpTransModeStream;
            end
            else begin
                Client.CurrTransMode := FtpTransModeZDeflate;
                Answer := Format (msgModeOK, [Params]);
            end;
        end;
{$ENDIF}
        Exit;
    end;
    Client.CurrTransMode := FtpTransModeStream;
    Answer := Format (msgModeOK, [Params]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandOverflow(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    Buf : array [0..1023] of char;
begin
    Client.CurCmdType := ftpcOVER;
    { Disable receiving }
    Client.Shutdown(0);
    { Flush receive buffer }
    while (Client.Receive(@Buf, SizeOf(buf)) > 0) do;
    { Answer to client }
    Answer := msgOverflow;
    { Will close connection }
    PostMessage(Handle, FMsg_WM_FTPSRV_CLOSE_REQUEST,
                WPARAM(Client.ID), LPARAM(Client));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ [ep] STOU command support                                                   }
{ This code is more or less the same as CommandSTOR, with the addition of     }
{ GetUniqueFileName event triggering to let the user a chance to provide a    }
{ file name.                                                                  }
procedure TFtpServer.CommandSTOU(
    Client: TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    UniqueName : TFtpString;
    Allowed    : Boolean;
    FilePath   : TFtpString;
begin
    try
        if Client.FtpState <> ftpcReady then begin
            Answer := msgNotLogged;
            Exit;
        end;

        try
            Client.CurCmdType       := ftpcSTOU;
            Client.HasOpenedFile    := FALSE;
            //Allowed                 := TRUE;  { V1.52 AG }
            UniqueName              := '';//SlashesToBackSlashes(Params); { V1.52 AG }

            { Fire the GetUniqueFileName event to get the file name  }
            { to be used to store data                               }
            TriggerGetUniqueFileName (Client, UniqueName);

            TriggerEnterSecurityContext(Client);             { V1.52 AG }
            try
                { no file name has been provided, or provided one        }
                { already exists => create one                           }
                if (UniqueName = '') or
                   (FileExists(BuildFilePath(Client, Client.Directory,
                                              UniqueName))) then begin
                    UniqueName := ExtractFilename(CreateUniqueFile(
                                        Client.Directory, 'FTP', ''));
                    if UniqueName = '' then begin
                        Answer := Format(msgStouFailed, ['Error creating unique file']);
                        Exit;
                    end;
                end;
            finally
                TriggerLeaveSecurityContext(Client);         { V1.52 AG }
            end;

            Client.FileName   := UniqueName;
            FilePath          := BuildFilePath(Client, Client.Directory,
                                                     Client.FileName);
            Allowed := IsPathAllowed(Client, FilePath); { V1.52 AG }
            TriggerValidatePut(Client, FilePath, Allowed);
            if not Allowed then begin
                Answer := msgStorDisabled;
                DeleteFile(FilePath); // delete the created file { V1.52 AG }
                Exit;
            end;
            Client.FilePath := FilePath;
            PrepareStorDataSocket(Client);
            Answer := Format(msgStouSuccess, [UniqueName]);
        except
            on E:Exception do begin
                Answer := Format(msgStouFailed, [E.Message]);
            end;
        end;
    finally
        { check for success 150..159 in passive mode }
        if Client.PassiveMode and (Copy(Answer, 1, 2) <> '15') then begin
            { flag for ClientStorSessionClosed that the error-message was already sent! }
            Client.TransferError    := '';
            Client.AbortingTransfer := TRUE;
            { set up Passive DataSocket.EventHandlers        }
            { otherwise FreeCurrentPasvPort won't be called! }
            PreparePassiveStorDataSocket(Client);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandFEAT(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    try
        Client.CurCmdType := ftpcFEAT;
        Answer := msgFeatFollows + #13#10 +
                  '  SIZE'+ #13#10 +
                  '  REST STREAM'+ #13#10 +      { angus V1.39 (been supported for years) }
                  '  MDTM'+ #13#10 +
                  '  MDTM YYYYMMDDHHMMSS[+-TZ] filename'+ #13#10 +       { angus V1.38 }
                  '  MLST size*;type*;perm*;create*;modify*;'+ #13#10 +  { angus V1.38 }
                  '  MFMT'+ #13#10 +                                     { angus V1.39 }
                  '  MD5'+ #13#10 +                                      { angus V1.39 }
                  '  XCRC "filename" start end'+ #13#10 +                { angus V1.54 }
                  '  XMD5 "filename" start end'+ #13#10 +                { angus V1.54 }
                  '  CLNT'+ #13#10 +                                     { angus V1.54 }
                  '  SITE INDEX;ZONE';                                   { angus V1.54 }
        if Assigned (FOnSiteMsg) then Answer := Answer + ';MSG';         { angus V1.54 }
        if Assigned (FOnSiteExec) then Answer := Answer + ';EXEC';       { angus V1.54 }
        if Assigned (FOnSitePaswd) then Answer := Answer + ';PSWD';      { angus V1.54 }
        if ftpsSiteXmlsd in FOptions then
                                      Answer := Answer + ';CMLSD;DMLSD'; { angus V1.54 }
        Answer := Answer + #13#10;
        if Assigned (FOnCombine) then Answer := Answer + '  COMB'+ #13#10; { angus V1.54 }
    {$IFDEF USE_MODEZ}              { angus V1.54 }
        if ftpModeZCompress in Client.Options then
                                      Answer := Answer + '  MODE Z'+ #13#10;
    {$ENDIF}
    {$IFDEF USE_SSL}
        if Self is TSslFtpServer then begin     {  V1.48 }
        if TSslFtpserver(Self).FFtpSslTypes <> [] then begin             { V1.47 }
                if not (ftpImplicitSsl in TSslFtpserver(Self).FFtpSslTypes) then begin
                Answer := Answer + '  AUTH ';
                if ftpAuthTls in TSslFtpserver(Self).FFtpSslTypes then
                    Answer := Answer + 'TLS;';
                if ftpAuthSsl in TSslFtpserver(Self).FFtpSslTypes then
                    Answer := Answer + 'SSL;';
                if ftpAuthTlsP in TSslFtpserver(Self).FFtpSslTypes then
                    Answer := Answer + 'TLS-P;';
                if ftpAuthTlsC in TSslFtpserver(Self).FFtpSslTypes then
                    Answer := Answer + 'TLS-C;';
                Answer := Answer +  #13#10 +
                          '  CCC'+ #13#10;
            {if TSslFtpserver(Self).FFtpSslType = sslTypeAuthSsl then
                Answer := Answer + '  AUTH TLS;SSL;' + #13#10;}
            end;
            Answer := Answer + '  PROT C;P;' + #13#10 +
                               '  PBSZ'      + #13#10;
            end;
        end;
    {$ENDIF}
        Answer := Answer + msgFeatFollowDone;
    except
        on E:Exception do begin
            Answer := Format(msgFeatFailed, [E.Message]);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.SetPasvPortRangeSize(const NewValue: Integer);
var
    OldValue : Integer;
    TablePtr : PBoolean;
    I        : Integer;
begin
    if (NewValue < 0) or (NewValue > 65535) then
        raise ERangeError.CreateFmt('Invalid PasvPortRangeSize %d.', [NewValue]);
    if FPasvPortRangeSize = NewValue then
        Exit;
    OldValue := FPasvPortRangeSize;

    { If we reduce the range, we must be sure to not affect any port in use }
    if NewValue < OldValue then begin
        { Check if any port is used before changing }
        TablePtr := PBoolean(PChar(FPasvPortTable) + SizeOf(Boolean) * NewValue);
        I        := NewValue;
        while I < OldValue do begin
            if TablePtr^ then
                raise Exception.Create('Unable to change PasvPortRangeSize ' +
                                       'when port is in use.');
            Inc(I);
            Inc(TablePtr);
        end;
    end;

{$IFDEF VER80}
    FPasvPortTable := ReallocMem(FPasvPortTable, FPasvPortTableSize, NewValue);
{$ELSE}
    ReallocMem(FPasvPortTable, NewValue);
{$ENDIF}
    FPasvPortTableSize := NewValue;
    FPasvPortRangeSize := NewValue;
    if OldValue >= NewValue then
        Exit;

    TablePtr := PBoolean(PChar(FPasvPortTable) + SizeOf(Boolean) * OldValue);
    while OldValue < NewValue do begin
        TablePtr^ := FALSE;
        Inc(TablePtr);
        Inc(OldValue);
    end;
    FPasvNextNr := 0;  { angus V1.56 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.SetPasvPortRangeStart(const NewValue: Integer);
var
    TablePtr : PBoolean;
    I        : Integer;
begin
    if (NewValue < 0) or (NewValue > 65535) then
        raise ERangeError.CreateFmt('Invalid PasvPortRangeStart %d.', [NewValue]);
    if FPasvPortRangeStart = NewValue then
        Exit;
    { Check if any port is used before changing }
    TablePtr := FPasvPortTable;
    I        := 0;
    while I < FPasvPortRangeSize do begin
        if TablePtr^ then
            raise Exception.Create('Unable to change PasvPortRangeStart ' +
                                   'when port is in use.');
        Inc(I);
        Inc(TablePtr);
    end;

    { Now we can change PasvPortRangeStart }
    FPasvPortRangeStart := NewValue;
    FPasvNextNr := 0;  { angus V1.56 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandMLST(   { angus V1.38 }
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    F          : TSearchRec;
    FileName   : String;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    Client.CurCmdType := ftpcMLST;
    if Params = '' then Params := '*.*';   { current directory }
    FileName := BuildFilePath(Client, Client.Directory, Params);
    if not IsPathAllowed(Client, FileName) then begin  { V1.52 AG }
        Answer := msgMlstDenied;
        Exit;
    end;
    TriggerEnterSecurityContext(Client);                    { V1.52 AG }
    try
        if FindFirst(FileName, faArchive + faDirectory, F) = 0 then
            Answer := msgMlstFollows + Params + #13#10 +
                      ' ' + FormatFactsDirEntry(F, F.Name) + #13#10 + { angus 1.54 added name }
                      msgMlstFollowDone
        else
            Answer := Format(msgMlstNotExists, [Params]);
        FindClose(F);
    finally
        TriggerLeaveSecurityContext(Client);                { V1.52 AG }
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandMLSD(   { angus V1.38 }
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;

    Client.CurCmdType := ftpcMLSD;
    CommandDirectory2(Client, KeyWord, Params, Answer, ListTypeFacts);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *} { angus V1.54 }
procedure FileMD5OnProgress(
    Obj: TObject;
    Count: {$IFDEF STREAM64} Int64 {$ELSE} Integer {$ENDIF};
    var Cancel: Boolean);
begin
    Cancel := (Obj as TFtpCtrlSocket).AbortingTransfer;
    (Obj as TFtpCtrlSocket).LastTick := IcsGetTickCountX;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandMD5(   { angus V1.39 }
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    FileName  : TFtpString;
    Md5Sum    : TFtpString;
    Allowed   : Boolean;
    FileSize  : TFtpBigInt; { AG V1.50 }
    Offset    : Integer;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    // Allowed := TRUE; { AG V1.52 }
    Md5Sum  := '';
    Client.HashStartPos := 0;
    Client.HashEndPos := 0;

    try
        if Keyword = 'XMD5' then begin    { angus V1.54 }
            Client.CurCmdType := ftpcXMD5;
            Offset := 1;
            FileName := ScanGetNextArg(Params, Offset);                     { keep file name }
            Client.HashStartPos := atoi64(ScanGetNextArg (Params, Offset));  { start position, if any }
            Client.HashEndPos := atoi64(ScanGetNextArg (Params, Offset));    { end position, if any }
            if (Client.HashStartPos > 0) and (Client.HashEndPos = 0) then begin
                Client.HashEndPos := Client.HashStartPos;  { single argument is end position }
                Client.HashStartPos := 0;
            end ;
        end
        else begin
        Client.CurCmdType := ftpcMD5;
            FileName := Params;
        end;
        FileName := BuildFilePath(Client, Client.Directory, FileName);
        Allowed := IsPathAllowed(Client, FileName); { AG V1.52 }
        { Ideally the MD5 sum is being retrieved from a cache or file so it's not  }
        { done repeatedly, if left blank we'll do it here. MD5 may be used to check}
        { uploaded/downloaded files, so keep a timestamp with the sum.             }
        TriggerCalculateMd5(Client, FileName, Md5Sum, Allowed);
        if not Allowed then begin
             Answer := msgRetrDisabled;
             Exit;
        end;
        if Md5Sum = '' then begin
            FileSize := GetFileSize(FileName); { AG V1.50 }
            if FileSize = -1 then begin { AG V1.50 }
                TriggerMd5Calculated(Client, FileName, Md5Sum); { AG V1.50 }
                Answer := Format(msgMd5NotFound, [Params]);
                Exit;
            end ;
            { Calculate a 32-byte MD5 sum. If file size is small we may use }
            { a blocking function.                                 AG V1.50 }
            if (FMd5UseThreadFileSize = 0) or
               (FileSize < FMd5UseThreadFileSize) then begin
                Md5Sum := FileMD5(FileName, Client, FileMD5OnProgress,
                   Client.HashStartPos, Client.HashEndPos, Client.FileModeRead); { angus V1.57 }
                TriggerMd5Calculated(Client, FileName, UpperCase(Md5Sum));   
            end
            else begin
                { Use a thread to calculate MD5 checksum which otherwise }
                { would block the server.                       AG V1.50 }
                if Client.ProcessingThread <> nil then begin
                    //TriggerMd5Calculated(Client, FileName, '');
                    Answer := Format(msgMd5Failed, [Params]);
                    Exit;
                end ;
                { AG V1.50 }
                TriggerDisplay(Client, 'Using thread to calculate MD5Sum');  { angus V1.54 }
                Client.ProcessingThread := TClientProcessingThread.Create(TRUE);
                Client.ProcessingThread.Client := Client;
                Client.ProcessingThread.InData := FileName;
                Client.ProcessingThread.Params := Params;
                Client.ProcessingThread.Keyword := Keyword;
                Client.ProcessingThread.OnTerminate := ClientProcessingThreadTerminate;
                Client.ProcessingThread.FreeOnTerminate := TRUE;
                Client.ProcessingThread.Resume;
                { Since answer is sent later when the thread returns we need }
                { to set this flag!                                          }
                Client.AnswerDelayed := TRUE;
                exit;                                                { angus V1.54 }
            end;
            end;
        Client.LastTick := IcsGetTickCountX;                         { angus V1.54 }
        if Md5Sum = '' then                                          { angus V1.54 }
             Answer := Format(msgMd5Failed, [Params])
        else begin
            if Client.CurCmdType = ftpcXMD5 then
                Answer := Format(msgCrcOk , [Uppercase (Md5Sum)])
            else
                Answer := Format(msgMd5Ok, [Params, Uppercase (Md5Sum)]);
        end;
    except
        on E:Exception do begin
            Answer := Format(msgMd5Failed, [E.Message]);
        end;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandXCRC (  { angus V1.54 }
     Client      : TFtpCtrlSocket;
     var Keyword : TFtpString;
     var Params  : TFtpString;
     var Answer  : TFtpString);
var
    FileName  : TFtpString;
    Crc32b    : TFtpString;
    Allowed   : Boolean;
    FileSize  : TFtpBigInt;
    Offset    : Integer;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    Client.CurCmdType := ftpcXCRC;
    Crc32b  := '';

    try
      { get file name and optional start and end arguments }
        Offset := 1;
        FileName := ScanGetNextArg(Params, Offset);             { keep file name }
        Client.HashStartPos := atoi64(ScanGetNextArg (Params, Offset));  { start position, if any }
        Client.HashEndPos := atoi64(ScanGetNextArg (Params, Offset));    { end position, if any }
        if (Client.HashStartPos > 0) and (Client.HashEndPos = 0) then begin
            Client.HashEndPos := Client.HashStartPos;  { single argument is end position }
            Client.HashStartPos := 0;
        end ;
        FileName := BuildFilePath(Client, Client.Directory, FileName);
        Allowed := IsPathAllowed(Client, FileName);
        { Ideally the CRC sum is being retrieved from a cache or file so it's not  }
        { done repeatedly, if left blank we'll do it here. CRC may be used to check}
        { uploaded/downloaded files, so keep a timestamp with the sum.             }
        TriggerCalculateCrc(Client, FileName, Crc32b, Allowed);
        if not Allowed then begin
             Answer := msgRetrDisabled;
             Exit;
        end;
        if Crc32b = '' then begin
            FileSize := GetFileSize(FileName);
            if FileSize = -1 then begin
                TriggerCrcCalculated(Client, FileName, Crc32b);
                Answer := Format(msgMd5NotFound, [Params]);
                Exit;
            end ;
            { Calculate a 32-byte CRC sum. If file size is small we may use }
            { a blocking function.                                          }
            if (FMd5UseThreadFileSize = 0) or
                                   (FileSize < FMd5UseThreadFileSize) then begin
                Crc32b := FileCRC32B(FileName, Client, FileMD5OnProgress,
                      Client.HashStartPos, Client.HashEndPos, Client.FileModeRead); { angus V1.57 }
                TriggerCrcCalculated(Client, FileName, UpperCase(Crc32b));
        end
            else begin
                { Use a thread to calculate CRC checksum which otherwise }
                { would block the server.                        }
                if Client.ProcessingThread <> nil then begin
                    Answer := Format(msgCrcFailed, [Params]);
                    Exit;
                end ;
                TriggerDisplay(Client, 'Using thread to calculate CRC32B');
                Client.ProcessingThread := TClientProcessingThread.Create(TRUE);
                Client.ProcessingThread.Client := Client;
                Client.ProcessingThread.InData := FileName;
                Client.ProcessingThread.Params := Params;
                Client.ProcessingThread.Keyword := Keyword;
                Client.ProcessingThread.OnTerminate := ClientProcessingThreadTerminate;
                Client.ProcessingThread.FreeOnTerminate := TRUE;
                Client.ProcessingThread.Resume;
                { Since answer is sent later when the thread returns we need }
                { to set this flag!                                          }
                Client.AnswerDelayed := TRUE;
                exit;
            end;
        end;
        Client.LastTick := IcsGetTickCountX;
        if Crc32b = '' then
             Answer := Format(msgCrcFailed, [Params])
        else
             Answer := Format(msgCrcOk , [Uppercase (Crc32b)]);
    except
        on E:Exception do begin
            Answer := Format(msgMd5Failed, [E.Message]);
        end;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandALLO (  { angus V1.54 short for allocation }
     Client      : TFtpCtrlSocket;
     var Keyword : TFtpString;
     var Params  : TFtpString;
     var Answer  : TFtpString);
var
    Size, FreeSpace : Int64;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    Client.CurCmdType := ftpcALLO;
    Answer := '';
  { may need to check client account for disk space allocation remaining }
    TriggerValidateAllo (Client, Params, Answer);
    if Answer <> '' then exit;

  { otherwise check for free space on drive with working directory }
    try
        Size := atoi64(Params);
        FreeSpace := GetFreeSpacePath (Client.Directory);
        if FreeSpace < 0 then
           Answer := Format(msgAlloOk, [0])   { failed, but pretend Ok for backward compatibility }
        else if (Size = 0) then
            Answer := msgAlloFail             { invalid size }
        else begin
            if (Size + FAlloExtraSpace) < FreeSpace then  { don't allow files to fill drive }
                Answer := Format(msgAlloOk, [FreeSpace])
            else
                Answer := Format(msgAlloFull, [FreeSpace]);
        end;                
    except
        on E:Exception do begin
            Answer := msgAlloFail;
        end;
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandCLNT (  { angus V1.54 }
      Client      : TFtpCtrlSocket;
      var Keyword : TFtpString;
      var Params  : TFtpString;
      var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    Client.CurCmdType := ftpcCLNT;
    Client.ClntStr := Params;
    Answer := msgNotedOK;
    TriggerClntStr (Client, Params, Answer);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandOPTS (  { angus V1.54 }
      Client      : TFtpCtrlSocket;
      var Keyword : TFtpString;
      var Params  : TFtpString;
      var Answer  : TFtpString);
var
    Arg: string;
    Offset: Integer;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    Answer := Format(msgOptsFailed, [Params]);
    Params := Uppercase (Params);
    Offset := 1;
    Arg := ScanGetNextArg (Params, Offset);
    if Arg <> 'MODE' then exit;
    Arg := ScanGetNextArg (Params, Offset);
    if Arg <> 'Z' then exit;
    Arg := ScanGetNextArg (Params, Offset);
    if Arg <> 'LEVEL' then exit;
    Arg := ScanGetNextArg (Params, Offset);
    if Arg = '' then exit;
    Offset := atoi (Arg);
    if (Offset >= FZlibMinLevel) and (Offset <= FZlibMaxLevel) then begin
        Client.ZReqLevel := Offset;
        Answer := Format(msgOtpsOK, ['MODE Z LEVEL set to ' + Arg]);
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandSitePaswd (  { angus V1.54 }
      Client      : TFtpCtrlSocket;
      var Keyword : TFtpString;
      var Params  : TFtpString;
      var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    Client.CurCmdType := ftpcSitePaswd;
    Answer := msgSiteFailed;
    TriggerSitePaswd (Client, Params, Answer);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandSiteExec (  { angus V1.54 }
      Client      : TFtpCtrlSocket;
      var Keyword : TFtpString;
      var Params  : TFtpString;
      var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    Client.CurCmdType := ftpcSiteExec;
    Answer := msgSiteFailed;
    TriggerSiteExec (Client, Params, Answer);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandSiteIndex (  { angus V1.54 }
      Client      : TFtpCtrlSocket;
      var Keyword : TFtpString;
      var Params  : TFtpString;
      var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    Client.CurCmdType := ftpcSiteIndex;
    CommandDirectory2(Client, Keyword, Params, Answer, ListTypeName);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandSiteZone (  { angus V1.54 }
      Client      : TFtpCtrlSocket;
      var Keyword : TFtpString;
      var Params  : TFtpString;
      var Answer  : TFtpString);
var
    mins: integer;
    S: string ;
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    Client.CurCmdType := ftpcSiteZone;
    mins := GetLocalBiasUTC;
    S := IntToStr (mins);
    if mins >= 0 then S := '+' + S;
    Answer := Format(msgSiteZone, [S])
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandSiteMsg (  { angus V1.54 }
      Client      : TFtpCtrlSocket;
      var Keyword : TFtpString;
      var Params  : TFtpString;
      var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    Client.CurCmdType := ftpcSiteMsg;
    Answer := msgSiteFailed;
    TriggerSiteMsg (Client, Params, Answer);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandSiteCmlsd (  { angus V1.54 }
      Client      : TFtpCtrlSocket;
      var Keyword : TFtpString;
      var Params  : TFtpString;
      var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    Client.CurCmdType := ftpcSiteCmlsd;
    CommandDirectory2(Client, KeyWord, Params, Answer, ListTypeFacts);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandSiteDmlsd (  { angus V1.54 }
      Client      : TFtpCtrlSocket;
      var Keyword : TFtpString;
      var Params  : TFtpString;
      var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    Client.CurCmdType := ftpcSiteDmlsd;
    CommandDirectory2(Client, KeyWord, Params, Answer, ListTypeFacts);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.CommandComb (  { angus V1.54 }
      Client      : TFtpCtrlSocket;
      var Keyword : TFtpString;
      var Params  : TFtpString;
      var Answer  : TFtpString);
begin
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    Client.CurCmdType := ftpcComb;
    Answer := Format(msgCmdUnknown, ['COMB']);
    TriggerCombine (Client, Params, Answer);
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF NOFORMS}
{ This function is a callback function. It means that it is called by       }
{ windows. This is the very low level message handler procedure setup to    }
{ handle the message sent by windows (winsock) to handle messages.          }
function FtpSrvWindowProc(
    ahWnd   : HWND;
    auMsg   : Integer;
    awParam : WPARAM;
    alParam : LPARAM): Integer; stdcall;
var
    Obj    : TObject;
    MsgRec : TMessage;
begin
    { At window creation asked windows to store a pointer to our object     }
    Obj := TObject(GetWindowLong(ahWnd, 0));

    { If the pointer doesn't represent a Tftpserver, just call the default procedure}
    if not (Obj is Tftpserver) then
        Result := DefWindowProc(ahWnd, auMsg, awParam, alParam)
    else begin
        { Delphi use a TMessage type to pass parameter to his own kind of   }
        { windows procedure. So we are doing the same...                    }
        MsgRec.Msg    := auMsg;
        MsgRec.wParam := awParam;
        MsgRec.lParam := alParam;
        { May be a try/except around next line is needed. Not sure ! }
        TFtpServer(Obj).WndProc(MsgRec);
        Result := MsgRec.Result;
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF NO_DEBUG_LOG}
function TFTPServer.GetIcsLogger: TIcsLogger;                         { V1.46 }
begin
    Result := FServSocket.IcsLogger;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFTPServer.SetIcsLogger(const Value: TIcsLogger);           { V1.46 }
begin
    FServSocket.IcsLogger := Value;
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFTPServer.CheckLogOptions(const LogOption: TLogOption): Boolean; { V1.46 }
begin
    Result := Assigned(IcsLogger) and (LogOption in IcsLogger.LogOptions);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFTPServer.DebugLog(LogOption: TLogOption; const Msg: string);  { V1.46 }
begin
    if Assigned(IcsLogger) then
        IcsLogger.DoDebugLog(Self, LogOption, Msg);
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.ClientProcessingThreadTerminate(Sender: TObject); { AG V1.50 }
var
    Answer  : TFtpString;
    AThread : TClientProcessingThread;
    Params  : TFtpString;
    Data    : TWSocket;
begin
    AThread := TClientProcessingThread(Sender);
    if IsClient(AThread.Client) and
       (AThread.Client.ID = AThread.ClientID) then begin
        AThread.Client.ProcessingThread := nil;
        if AThread.Client.State <> wsConnected then
            Exit;

        AThread.Client.LastTick := IcsGetTickCountX;                          { angus V1.54 }
        if (AThread.Keyword = 'MD5') or (AThread.Keyword = 'XMD5') then begin { angus V1.54 }
            if AThread.OutData = '' then
                Answer := Format(msgMd5Failed, [AThread.Params])
            else begin
                if (AThread.Keyword = 'XMD5') then                            { angus V1.54 }
                    Answer := Format(msgCrcOk, [Uppercase (AThread.OutData)])
            else
                    Answer := Format(msgMd5Ok, [AThread.Params,
                                                Uppercase (AThread.OutData)]);
            end;
            if Assigned(FOnMd5Calculated) then
                FOnMd5Calculated(Self, AThread.Client,
                                 AThread.InData, UpperCase(AThread.OutData));
        end
        else if (AThread.Keyword = 'XCRC') then begin { angus V1.54 }
            if AThread.OutData = '' then
                Answer := Format(msgCrcFailed, [AThread.Params])
            else
                Answer := Format(msgCrcOk, [Uppercase (AThread.OutData)]);
            if Assigned(FOnCrcCalculated) then
                FOnCrcCalculated(Self, AThread.Client,
                                 AThread.InData, UpperCase(AThread.OutData));
        end
        else if (AThread.Keyword = 'DIRECTORY') then begin { angus V1.54 }
            with AThread.Client do begin
                Params := AThread.InData;
                try
                    TriggerAlterDirectory(AThread.Client, Params,
                                            (DirListType <> ListTypeName));
                    DataStream.Seek(0, 0);
                    FilePath := '';
                    TriggerLeaveSecurityContext(AThread.Client);         { AG V1.52 }
                    if AThread.OutData <> AThread.Keyword then
                    begin
                        Answer := Format(msgDirFailed, ['Thread Processing Error']);
                        CloseFileStreams(AThread.Client);      { angus V1.54 }
                    end
                 { see if returning listing on control socket instead of data socket }
                    else if CurCmdType in [ftpcSiteIndex, ftpcSiteCmlsd] then begin
                        SetLength(Answer, DataStream.Size) ;
                        DataStream.Read(Answer [1], DataStream.Size) ;
                        if CurCmdType = ftpcSiteIndex then
                             Answer := Format (msgIndexFollows, [Params]) +
                                                             #13#10 + Answer + msgIndexDone;
                        if CurCmdType = ftpcSiteCmlsd then
                             Answer := msgMlstFollows + #13#10 + Answer + msgMlstFollowDone;
                        CloseFileStreams(AThread.Client);      { angus V1.54 }
                    end
                    else
                    begin
                        Answer := msgDirOpen;
                        AThread.Client.AnswerDelayed := FALSE;
                        DoStartSendData(AThread.Client, Answer);   { angus V1.54 added Answer }
                        if AThread.Client.AnswerDelayed then Exit; { about to compress stream }
                    end;
                except
                    on E:Exception do begin
                        Answer := Format(msgDirFailed, [E.Message])
                    end;
                end;

             { check for success 150..159 in passive mode }
                if (HasOpenedFile) and (PassiveMode) and
                                                    (Copy(Answer, 1, 2) <> '15') then begin
                    { flag for ClientRetrSessionClosed that the error-message was already sent! }
                    TransferError    := '';
                    AbortingTransfer := TRUE;
                    { set up Passive DataSocket.EventHandlers        }
                    { otherwise FreeCurrentPasvPort won't be called! }
                    PreparePassiveRetrDataSocket(AThread.Client);
                end;
            end;
        end
{$IFDEF USE_MODEZ}              { angus V1.54 }
        else if (AThread.Keyword = 'COMPRESS') then begin
            TriggerLeaveSecurityContext(AThread.Client);
             if AThread.OutData = '' then begin
                TriggerDisplay(AThread.Client, AThread.Client.FilePath + ' took ' +
                         IntToStr(IcsElapsedMsecs(AThread.StartTick)) + 'ms,' +
                                                         AThread.Client.ZCompInfo);
                if AThread.Client.ZCompFileDelete then
                                    TriggerUpCompressedFile(AThread.Client);
                Answer := AThread.InData;
                AThread.Client.AnswerDelayed := FALSE;
                PostMessage(Handle, FMsg_WM_FTPSRV_START_SEND, 0,
                                                     LongInt(AThread.Client));
            end
            else begin  { compress failed }
                CloseFileStreams(AThread.Client);
                Answer := AThread.OutData;
            end;
        end
        else if (AThread.Keyword = 'DECOMPRESS') then begin
            if AThread.OutData = '' then begin
                TriggerDisplay(AThread.Client, AThread.Client.FilePath + ' took ' +
                    IntToStr(IcsElapsedMsecs(AThread.StartTick)) + 'ms,' +
                                                         AThread.Client.ZCompInfo);
                Answer := AThread.InData + AThread.Client.ZCompInfo;
                CloseFileStreams(AThread.Client);
                Data := TWSocket(AThread.Sender);
                TriggerStorSessionClosed(AThread.Client, Data, 0);
            end
            else begin  { decompress failed }
                CloseFileStreams(AThread.Client);
                Answer := AThread.OutData;
            end;
        end
{$ENDIF}
        else
            Answer := Format('500 Executing command %s failed', [AThread.Keyword]);
        AThread.Client.AnswerDelayed := FALSE;  { angus V1.54 }
        SendAnswer(AThread.Client, Answer);
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpServer.EventTimerOnTimer (Sender : TObject); { angus V1.54 }
var
    Client   : TFtpCtrlSocket;
    I        : integer;
    Timeout  : integer;
    Duration : integer;
    Abort    : boolean ;
    CurTicks : LongWord;
begin
    FEventTimer.Enabled := false;
    try
        if not Assigned(FClientList) then exit;
        if (FTimeoutSecsLogin <= 0) and (FTimeoutSecsIdle <= 0) and
                                     (FTimeoutSecsXfer <= 0) then exit;  { no timeouts }
        if FClientList.Count = 0 then exit;                              { no clients }
        CurTicks := IcsGetTickCountX; { V1.56 AG }
        for I := 0 to Pred (FClientList.Count) do begin
            Client := TFtpCtrlSocket(FClientList.Items[I]);
         { different length timeouts depending on what's happening }
            Timeout := 0;
            case Client.FtpState of
                ftpcWaitingUserCode, ftpcWaitingPassword: Timeout := FTimeoutSecsLogin;
                ftpcReady, ftpcWaitingAnswer: Timeout := FTimeoutSecsIdle;
            end;
            if Client.DataSocket.State = wsConnected then Timeout := FTimeoutSecsXfer;
            if Timeout > 0 then begin
                Duration :=  IcsDiffTicks(Client.LastTick, CurTicks) div TicksPerSecond; { V1.56 AG}
                if Duration >= Timeout then begin   { seconds }
                    Abort := true;
                    TriggerTimeout(Client, Duration, Abort);
                    if NOT Abort then
                        Client.LastTick := IcsGetTickCountX  { extend timeout }
                    else begin
                      { close data channel }
                        if Client.DataSocket.State = wsConnected then begin
                            Client.TransferError    := 'ABORT on Timeout';
                            Client.AbortingTransfer := TRUE;
                            Client.DataSocket.Close;
                        end;
                        SendAnswer(Client, Format(msgTimeout, [Duration]));
                      { close control channel }
                        Client.Close;
                    end;
                end;
            end;
        end;
    finally
        FEventTimer.Enabled := true;
    end ;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF USE_SSL}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServer.SetFtpSslTypes(const Value: TFtpSslTypes); { 1.04 }
begin
    { Implizit SSL cannot be combined with explizit SSL }
    if Value <> FFtpSslTypes then begin
        if (ftpImplicitSsl in Value) and
           ((ftpAuthSsl in Value) or
           (ftpAuthTls in Value) or
           (ftpAuthTlsP in Value) or
           (ftpAuthTlsC in Value)) then begin
            FFtpSslTypes := [];
            raise Exception.Create('Option ftpImplicitSsl cannot be combined ' +
                                   'with explizit SSL types.');
         end
         else
            FFtpSslTypes := Value;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServer.ClientDataSent(Sender : TObject; AError : Word);    { 1.03 }
var
    Client : TFtpCtrlSocket;
begin
    Client := Sender as TFtpCtrlSocket;
    (*
    if Client.AuthFlag then begin
        Client.AuthFlag := FALSE;
        try
            if AError = 0 then
                Client.AcceptSslHandshake
            else begin
                Client.CurFtpSslType            := curftpSslNone;
                Client.SslEnable                := FALSE;
                Client.OnSslVerifyPeer          := nil;
                Client.OnSslHandshakeDone       := nil;
                Client.OnSslSvrNewSession       := nil;
                Client.OnSslSvrGetSession       := nil;
                Client.OnSslSetSessionIDContext := nil;
            end;
        except
            Client.CurFtpSslType            := curftpSslNone;
            Client.SslEnable                := FALSE;
            Client.OnSslVerifyPeer          := nil;
            Client.OnSslHandshakeDone       := nil;
            Client.OnSslSvrNewSession       := nil;
            Client.OnSslSvrGetSession       := nil;
            Client.OnSslSetSessionIDContext := nil;
            Client.Close;
        end;
    end
    else *)
    if Client.CccFlag then begin
        if AError = 0 then begin
            Client.SslBiShutDownAsync;
            Client.CurFtpSslType := curftpSslNone;
        end;
        Client.CccFlag := FALSE;
    end;
    inherited ClientDataSent(Sender, AError);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServer.CommandCCC(                                           { 1.03 }
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    if (FFtpSslTypes = []) or (ftpImplicitSsl in FFtpSslTypes) then begin
        Answer := Format(msgCmdUnknown, [Keyword]);
        Exit;
    end;
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    Client.CurCmdType := ftpcCCC;
    if (Client.SslState <> sslEstablished) then begin
        Answer := Format(msgErrInSslOnly, ['CCC']);
        Exit;
    end;
    if (Client.CurFtpSslType = curftpSslNone) then begin
        Answer := Format(msgAuthNoSupport, [Params]);
        Exit;
    end;
    Answer := msgCccOk;
    Client.CccFlag := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServer.CommandAUTH(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
var
    PlainAnswer : TFtpString;
    TmpCurFtpSslType : TCurFtpSslType;
begin
    {
    msgAuthOk         = '234 Using authentication type %s';
    msgAuthDenied     = '502 %s authentication not allowed'; // SSL/TLS
    msgAuthInitError  = '431 Could not initialize %s connection';
    msgAuthNoSupport  = '504 Auth type "%s" not supported';

    // AUTH TLS-P = AUTH SSL + PROT P
    // AUTH TLS-C = AUTH SSL + PROT C
    }
    if (FFtpSslTypes = []) or (ftpImplicitSsl in FFtpSslTypes) then begin
        Answer := Format(msgCmdUnknown, [Keyword]);
        Exit;
    end;

    Client.CurCmdType := ftpcAUTH;
    TmpCurFtpSslType  := curftpSslNone;
    if      (Params = 'TLS')   and (ftpAuthTls  in FFtpSslTypes) then TmpCurFtpSslType := curftpAuthTls
    else if (Params = 'SSL')   and (ftpAuthSsl  in FFtpSslTypes) then TmpCurFtpSslType := curftpAuthSsl
    else if (Params = 'TLS-C') and (ftpAuthTlsC in FFtpSslTypes) then TmpCurFtpSslType := curftpAuthTlsC
    else if (Params = 'TLS-P') and (ftpAuthTlsP in FFtpSslTypes) then TmpCurFtpSslType := curftpAuthTlsP;

    if (TmpCurFtpSslType = curftpSslNone) then begin
        Answer := Format(msgAuthNoSupport, [Params]);
        Exit;
    end;

    try
        Client.SslEnable                := True;
        Client.SslMode                  := sslModeServer;
        Client.SslContext               := FServSocket.SslContext;
        Client.OnSslVerifyPeer          := TransferSslVerifyPeer;
        Client.OnSslHandshakeDone       := TransferSslHandshakeDone;
        Client.OnSslSvrNewSession       := TransferSslSvrNewSession;
        Client.OnSslSvrGetSession       := TransferSslSvrGetSession;
        Client.OnSslSetSessionIDContext := TransferSslSetSessionIDContext;
        { AUTH in plaintext mode }
        if (Client.SslState = sslNone) then begin
            Client.AcceptSslHandshake;
            PlainAnswer := Format(msgAuthOk, [Params]) ;
            TriggerSendAnswer(Client, PlainAnswer);
            PlainAnswer := PlainAnswer + #13#10;
            Client.SslSendPlain(@PlainAnswer[1], Length(PlainAnswer));
            Client.CurFtpSslType  := TmpCurFtpSslType;
        end  { AUTH in SSL mode, negotiates a new SSL session }
        else
            if (Client.SslState = sslEstablished) and Assigned(Client.Ssl) then begin
                if f_SSL_version(Client.SSL) >= SSL3_VERSION then
                    Answer := msgAuthYetSetOkV3
                else
                    Answer := msgAuthYetSetOkV2;
                Client.CurFtpSslType  := TmpCurFtpSslType;
        end
        else
            Answer := Format(msgAuthDenied, ['SSL/TLS']);
        Client.FtpState  := ftpcWaitingUserCode;     // Need to force re-login
        if Client.CurFtpSslType <> curftpAuthTlsP then
            Client.ProtP     := FALSE               // Need to reset prot-level as well
        else
            Client.ProtP     := TRUE;
    except
        Client.CurFtpSslType            := curftpSslNone;
        Client.SslEnable                := False;
        Client.OnSslVerifyPeer          := nil;
        Client.OnSslHandshakeDone       := nil;
        Client.OnSslSvrNewSession       := nil;
        Client.OnSslSvrGetSession       := nil;
        Client.OnSslSetSessionIDContext := nil;
        Answer := Format(msgAuthInitError, [Params]);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ DATA CHANNEL PROTECTION LEVEL }
procedure TSslFtpServer.CommandPROT(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    { Possible levels
    C - Clear
    S - Safe
    E - Confidential
    P - Private

    msgProtOk         = '200 Protection level set to %s';
    msgProtNotExists  = '504 Protection level ''%s'' not supported';
    msgProtUnknown    = '504 Protection level ''%s'' not recognized';
   }
    if (FFtpSslTypes = []) then begin
        Answer := Format(msgCmdUnknown, [Keyword]);
        Exit;
    end;
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    Client.CurCmdType := ftpcPROT;
    if (Client.SslState = sslEstablished) then
    begin
        if (Params = 'C') or (Params = 'P') then begin
            Client.ProtP := Params = 'P';
            Answer := Format(msgProtOK, [Params]);
        end else
        if (Params = 'S') or (Params = 'E') then
            Answer := Format(msgProtNoSupport, [Params])
        else
            Answer := Format(msgProtUnknown, [Params])
    end else
       Answer := Format(msgErrInSslOnly, ['PROT']);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServer.CommandPBSZ(
    Client      : TFtpCtrlSocket;
    var Keyword : TFtpString;
    var Params  : TFtpString;
    var Answer  : TFtpString);
begin
    { Dummy command to fullfill RFC4217 }
    if (FFtpSslTypes = []) then begin
        Answer := Format(msgCmdUnknown, [Keyword]);
        Exit;
    end;
    if Client.FtpState <> ftpcReady then begin
        Answer := msgNotLogged;
        Exit;
    end;
    if (Client.SslState = sslEstablished) then
    begin
        Client.CurCmdType := ftpcPBSZ;
        Answer            := msgPbszOk;
    end
    else
        Answer := Format(msgErrInSslOnly, ['PBSZ']);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TSslFtpServer.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FFtpSslTypes   := [];
    AddCommand('AUTH', CommandAUTH);
    AddCommand('PROT', CommandPROT);
    AddCommand('PBSZ', CommandPBSZ);
    AddCommand('CCC',  CommandCCC);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslFtpServer.GetSslContext: TSslContext;
begin
    Result := FServSocket.SslContext
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServer.SetSslContext(Value: TSslContext);
begin
    FServSocket.SslContext := Value
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServer.SendAnswer(Client: TFtpCtrlSocket;
  Answer: TFtpString);
begin
    if (Client.CurCmdType = ftpcAUTH) and (Answer = '') then
        Exit;
    inherited SendAnswer(Client, Answer);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServer.TransferSslHandshakeDone(Sender: TObject;
  ErrCode: Word; PeerCert: TX509Base; var Disconnect: Boolean);
var
    Client  : TFtpCtrlSocket;
begin
    if Assigned(FOnSslHandshakeDone) then
        FOnSslHandshakeDone(Sender, ErrCode, PeerCert, Disconnect);
    { If SSL handshake failed fatal the socket has been closed already. }
    { Then a "226 File OK" is sent anyway, even with code below.        } //fix needed?
    if (ErrCode <> 0) or Disconnect then begin
        if not (Sender is TFtpCtrlSocket) then begin
            Client := TFtpCtrlSocket((Sender as TWSocket).Owner);
            Client.AbortingTransfer := TRUE;
            Client.TransferError    := 'SSL handshake failed';
            PostMessage(FHandle, FMsg_WM_FTPSRV_Close_Data,
                        WPARAM(Client.ID), LPARAM(Client));
            Disconnect := FALSE;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServer.TransferSslSetSessionIDContext(Sender: TObject;
  var SessionIDContext: TSslSessionIdContext);
begin
    if Assigned(FOnSslSetSessionIDContext) then
        FOnSslSetSessionIDContext(Sender, SessionIDContext);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServer.TransferSslSvrGetSession(Sender: TObject;
  var SslSession: Pointer; SessId: Pointer; Idlen: Integer;
  var IncRefCount: Boolean);
begin
    if Assigned(FOnSslSvrGetSession) then
        FOnSslSvrGetSession(Sender, SslSession, SessId, IdLen, IncRefCount);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServer.TransferSslSvrNewSession(Sender: TObject;
  SslSession, SessId: Pointer; Idlen: Integer;
  var AddToInternalCache: Boolean);
begin
    if Assigned(FOnSslSvrNewSession) then
        FOnSslSvrNewSession(Sender, SslSession, SessID, IDLen, AddToInternalCache);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServer.TransferSslVerifyPeer(Sender: TObject;
  var Ok: Integer; Cert: TX509Base);
begin
    if Assigned(FOnSslVerifyPeer) then
        FOnSslVerifyPeer(Sender, Ok, Cert);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServer.TriggerClientConnect(
    Client  : TFtpCtrlSocket;
    AError  : Word);
begin
    inherited TriggerClientConnect(Client, AError);
    if FClientList.IndexOf(Client) < 0 then
        Exit;
    { The event handler may have closed the connection }
    if Client.State <> wsConnected then
        Exit;
    Client.SslEnable  := ftpImplicitSsl in TSslFtpServer(Self).FFtpSslTypes;
    if Client.SslEnable then begin
        Client.CurFtpSslType            := curftpImplicitSsl;
        Client.SslMode                  := sslModeServer;
        Client.SslContext               := FServSocket.SslContext;
        Client.OnSslVerifyPeer          := TransferSslVerifyPeer;
        Client.OnSslHandshakeDone       := TransferSslHandshakeDone;
        Client.OnSslSvrNewSession       := TransferSslSvrNewSession;
        Client.OnSslSvrGetSession       := TransferSslSvrGetSession;
        Client.OnSslSetSessionIDContext := TransferSslSetSessionIDContext;
        try
            Client.AcceptSslHandshake;
        except
            Client.SslEnable := False;
            Client.Banner := msgErrSslInit;
            Client.StartConnection;
            Client.Close;
        end;
    end;
end;



{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServer.ClientPassiveSessionAvailable(Sender : TObject; AError : Word);
var
    Client  : TFtpCtrlSocket;
    Data    : TWSocket;
begin
    Data    := TWSocket(Sender);
    Client  := TFtpCtrlSocket(Data.Owner);
    Client.DataSocket.SslEnable := False; // we need to start SSL by ourself

    inherited ClientPassiveSessionAvailable(Sender, AError);

    if (not Client.PassiveStart) and Client.ProtP and
       (Client.DataSocket.SslState = sslNone) then
    begin
        Client.DataSocket.SslEnable                 := TRUE;
        Client.DataSocket.SslMode                   := sslModeServer;
        Client.DataSocket.SslContext                := SslContext;
        Client.DataSocket.OnSslVerifyPeer           := TransferSslVerifyPeer;
        Client.DataSocket.OnSslHandshakeDone        := TransferSslHandshakeDone;
        Client.DataSocket.OnSslSvrNewSession        := TransferSslSvrNewSession;
        Client.DataSocket.OnSslSvrGetSession        := TransferSslSvrGetSession;
        Client.DataSocket.OnSslSetSessionIDContext  := TransferSslSetSessionIDContext;
        try
            Client.DataSocket.AcceptSslHandshake;
        except
            Client.DataSocket.SslEnable                := False;
            Client.DataSocket.OnSslVerifyPeer          := nil;
            Client.DataSocket.OnSslHandshakeDone       := nil;
            Client.DataSocket.OnSslSvrNewSession       := nil;
            Client.DataSocket.OnSslSvrGetSession       := nil;
            Client.DataSocket.OnSslSetSessionIDContext := nil;
            SendAnswer(Client, Format(msgAuthInitError, ['SSL']));
            PostMessage(FHandle, FMsg_WM_FTPSRV_Close_Data,
                        WPARAM(Client.ID), LPARAM(Client));
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServer.TriggerStorSessionConnected(Client: TFtpCtrlSocket;
  Data: TWSocket; AError: Word);
begin
    inherited TriggerStorSessionConnected(Client, Data, AError);
    if ((not Client.PassiveStart) and Client.PassiveConnected) or
       (AError <> 0) then
        Exit;
    Client.DataSocket.SslEnable := False;
    if (Client.DataSocket.State = wsConnected) then
    begin
        if Client.ProtP and (Client.DataSocket.SslState = sslNone) then
        begin
            Client.DataSocket.SslEnable                 := True;
            Client.DataSocket.SslMode                   := sslModeServer;
            Client.DataSocket.SslContext                := SslContext;
            Client.DataSocket.OnSslVerifyPeer           := TransferSslVerifyPeer;
            Client.DataSocket.OnSslHandshakeDone        := TransferSslHandshakeDone;
            Client.DataSocket.OnSslSvrNewSession        := TransferSslSvrNewSession;
            Client.DataSocket.OnSslSvrGetSession        := TransferSslSvrGetSession;
            Client.DataSocket.OnSslSetSessionIDContext  := TransferSslSetSessionIDContext;
            try
                Client.DataSocket.AcceptSslHandshake;
            except
                Client.DataSocket.SslEnable := False;
                Client.DataSocket.OnSslVerifyPeer           := nil;
                Client.DataSocket.OnSslHandshakeDone        := nil;
                Client.DataSocket.OnSslSvrNewSession        := nil;
                Client.DataSocket.OnSslSvrGetSession        := nil;
                Client.DataSocket.OnSslSetSessionIDContext  := nil;
                Client.AbortingTransfer := TRUE;
                Client.TransferError    := msgErrSslInit;
                PostMessage(FHandle, FMsg_WM_FTPSRV_ABORT_TRANSFER,
                        WPARAM(Client.ID), LPARAM(Client));
            end;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServer.TriggerRetrSessionConnected(Client: TFtpCtrlSocket;
  Data: TWSocket; AError: Word);
begin
    inherited TriggerRetrSessionConnected(Client, Data, AError);

    if ((not Client.PassiveStart) and Client.PassiveConnected) or
       (AError <> 0) then
        Exit;

    Client.DataSocket.SslEnable := False;
    if Client.ProtP and (Client.DataSocket.SslState = sslNone) then begin
        Client.DataSocket.SslEnable                 := True;
        Client.DataSocket.SslMode                   := sslModeServer;
        Client.DataSocket.SslContext                := SslContext;
        Client.DataSocket.OnSslVerifyPeer           := TransferSslVerifyPeer;
        Client.DataSocket.OnSslHandshakeDone        := TransferSslHandshakeDone;
        Client.DataSocket.OnSslSvrNewSession        := TransferSslSvrNewSession;
        Client.DataSocket.OnSslSvrGetSession        := TransferSslSvrGetSession;
        Client.DataSocket.OnSslSetSessionIDContext  := TransferSslSetSessionIDContext;
        try
            Client.DataSocket.AcceptSslHandshake;
        except
            Client.DataSocket.SslEnable                := False;
            Client.DataSocket.OnSslVerifyPeer          := nil;
            Client.DataSocket.OnSslHandshakeDone       := nil;
            Client.DataSocket.OnSslSvrNewSession       := nil;
            Client.DataSocket.OnSslSvrGetSession       := nil;
            Client.DataSocket.OnSslSetSessionIDContext := nil;
            Client.AbortingTransfer := TRUE;
            raise Exception.Create(msgErrSslInit);
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TSslFtpServer.MsgHandlersCount : Integer;
begin
    Result := 2 + inherited MsgHandlersCount;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServer.AllocateMsgHandlers;
begin
    inherited AllocateMsgHandlers;
    FMsg_WM_FTPSRV_ABORT_TRANSFER    := FWndHandler.AllocateMsgHandler(Self);
    FMsg_WM_FTPSRV_Close_Data     := FWndHandler.AllocateMsgHandler(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslFtpServer.FreeMsgHandlers;
begin
    if Assigned(FWndHandler) then begin
        FWndHandler.UnregisterMessage(FMsg_WM_FTPSRV_ABORT_TRANSFER);
        FWndHandler.UnregisterMessage(FMsg_WM_FTPSRV_Close_Data);
    end;
    inherited FreeMsgHandlers;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$ENDIF} // USE_SSL

end.

