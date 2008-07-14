{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  TFtpCtrlSocket component. It handle the client connection for
              the TFtpServer component.
Creation:     April 21, 1998
Version:      6.04
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1998-2008 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
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

History:
If not otherwise noted, changes are by Francois Piette
Apr 29, 1998  V0.90 released for beta testing.
May 03, 1998  V0.93 Adapted for Delphi 2.0 and C++Builder
May 04, 1998  V0.94 Added support for UNC (not finished !)
Jul 09, 1998  V1.00 Adapted for Delphi 4, removed beta status.
Jul 21, 1998  V1.01 Publised TrumpetCompatibility property.
Aug 06, 1998  V1.02 Verified that FRcvCnt was 0 in SetRcvSize. Suggested
              by Nick MacDonald <NickMacDonald@hotmail.com>
Mar 06, 1999  V1.03 Added code from  Plegge, Steve <jsp@nciinc.com> to add
              APPE and STRU support.
Aug 20, 1999  V1.04 Revised compile time options. Adapted for BCB4.
Nov 24, 1999  V1.05 Added MTDM support. Thanks to Bruce Christensen
              <bkc51831234@hotmail.com> for his code.
Jan 24, 2000  V1.06 Patch IE5 bug in file names. Thanks to <dsnake@infonie.fr>
Nov 11, 2000  V1.07 Checked for DOS attack. Close connection when buffer
              overflow occured. Thanks to Lester <les@lester.co.uk> for finding
              this security hole.
Jul 28, 2001  V1.08 Added ID property to uniquely indentify the client.
Sep 09, 2001  V1.09 Eric Pascual <e.pascual@cstb.fr> changed TFtpCmdType type
              to make it a Byte so that new values can be added by
              sub-components which add new commands.
Sep 15, 2003  V1.10 Added ICSDEF feature to the source code. Thanks to Marco
              van de Voort <marcov@stack.nl> for his help.
Jan 15, 2003  V1.11 Made SetDirectory virtual.
Aug 6, 2004   V1.38 Angus added MLST and MLSD
Aug 19, 2004  V1.39 Angus added MFMT, MD5, ftpCwdCheck, ftpCdupHome
Sept 6, 2005  V1.43 64-bit support for Delphi 6 and later, for transfers larger
              than 2 gigs, by Angus Robertson, angus@magsys.co.uk
Dec 30, 2005  V1.44 Arno Garrels added IcsLogger.
Jan 06, 2006  V1.45 A.Garrels added command PBSZ, SSL only.
Sep 20, 2006  V1.46 A.Garrels added ProcessingThread and some new variables
              for smarter MD5 handling.
Oct 27, 2006  V1.47 A. Garrels added property PeerSAddr
May 09, 2007  V1.52 changes by A.Garrels. ftpCdUphome and ftpHidePhysicalPath support
Dec 03, 2007  V1.54 added more FEAT extensions, by Angus Robertson, angus@magsys.co.uk
             added OTP, Clnt, Allo, SitePswd, SiteExec, SiteZone, SiteMsg, SiteDmlsd,
               SiteCmlsd, timeouts, Mode Z, Opts, Comb, Xmd5, XCrc, bandwidth counters,
               SessIdInfo,
             moved directory indexing code to client from server
             added various function to support Zlib compression for Mode Z
               (more details in OverbyteFtpSrv)
Dec 09, 2007 V1.55 mode z bug fix for resumed transfers
Jan 08, 2008 V1.57 added FileModeRead and FileModeWrite as public so share locking
               can be changed, use FileModeRead for MD5SUM (not locked)
Jul 10, 2008 V6.03 bumped version to match OverbyteFtpCli
Jul 13, 2008 V6.04 Made ReadCount a public property


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsFtpSrvC;

interface

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

uses
    Messages,
{$IFDEF USEWINDOWS}
    Windows,
{$ELSE}
    WinTypes, WinProcs,
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
    OverbyteIcsFtpSrvT,
    OverbyteIcsMd5, { AG V1.46}
    OverbyteIcsCRC,        { angus V1.54 }
    OverbyteIcsOneTimePw,  { V1.54 }
    OverbyteIcsWinsock, OverbyteIcsWSocket, OverbyteIcsWSockBuf;

const
    FtpCtrlSocketVersion = 604;
    CopyRight : String   = ' TFtpCtrlSocket  (c) 1998-2008 F. Piette V6.04 ';
    DefaultRcvSize       = 2048;
    UtcDateMaskPacked    = 'yyyymmddhhnnss';         { angus V1.38 }

type
    EFtpCtrlSocketException = class(Exception);
    TFtpCtrlState = (ftpcInvalid, ftpcWaitingUserCode, ftpcWaitingPassword,
                     ftpcReady, ftpcWaitingAnswer);

    { TFtpCmdType is now defined as a byte and enumerated items as constants, }
    { so that new values can be added by sub-components who add new commands  }
    TFtpCmdType   = Byte;

    { Angus Nov 2007 - previously the values in this table did not match the
      command table, which is why dimensioning was incorrect, now corrected  }
const
    ftpcPORT      = 0;
    ftpcSTOR      = 1;
    ftpcRETR      = 2;
    ftpcCWD       = 3;
    ftpcXPWD      = 4;
    ftpcPWD       = 5;
    ftpcUSER      = 6;
    ftpcPASS      = 7;
    ftpcLIST      = 8;
    ftpcNLST      = 9;
    ftpcTYPE      = 10;
    ftpcSYST      = 11;
    ftpcQUIT      = 12;
    ftpcDELE      = 13;
    ftpcSIZE      = 14;
    ftpcREST      = 15;
    ftpcRNFR      = 16;
    ftpcRNTO      = 17;
    ftpcMKD       = 18;
    ftpcRMD       = 19;
    ftpcABOR      = 20;
    ftpcPASV      = 21;
    ftpcNOOP      = 22;
    ftpcCDUP      = 23;
    ftpcAPPE      = 24;
    ftpcSTRU      = 25;   {jsp - Added APPE and STRU types }
    ftpcXMKD      = 26;
    ftpcXRMD      = 27;
    ftpcMDTM      = 28;   {bkc - Added MDTM type           }
    ftpcMODE      = 29;
    ftpcOVER      = 31;
    ftpcSTOU      = 32;   {ep  - Added STOU type           }
    ftpcFEAT      = 33;   {SSV - Added FEAT type           }
    ftpcMLST      = 34;   {angus Added MLST type           }
    ftpcMLSD      = 35;   {angus Added MLSD type           }
    ftpcMFMT      = 36;   {angus Added MFMT type           }
    ftpcMD5       = 37;   {angus Added MD5 type            }
    ftpcXCRC      = 38;   {angus Added XCRC type           }
    ftpcXMD5      = 39;   {angus Added XMD5 type           }
    ftpcALLO      = 40;   {angus Added ALLO type           }
    ftpcCLNT      = 41;   {angus Added CLNT type           }
    ftpcOPTS      = 42;   {angus Added OPTS type           }
    ftpcSitePaswd = 43;   {angus Added SITE PASWD type     }
    ftpcSiteExec  = 44;   {angus Added SITE EXEC type      }
    ftpcSiteIndex = 45;   {angus Added SITE INDEX type     }
    ftpcSiteZone  = 46;   {angus Added SITE ZONE type      }
    ftpcSiteMsg   = 47;   {angus Added SITE MSG type       }
    ftpcSiteCmlsd = 48;   {angus Added SITE CMLSD type     }
    ftpcSiteDmlsd = 49;   {angus Added SITE DMLSD type     }
    ftpcCOMB      = 50;   {angus Added COMB                }
{$IFNDEF USE_SSL}
    ftpcLast      = 50;   {angus used to dimension FCmdTable}
{$ELSE}
    ftpcAUTH      = 51;
    ftpcCCC       = 52;
    ftpcPBSZ      = 53;   {V1.45}
    ftpcPROT      = 54;
    ftpcLast      = 54;
{$ENDIF}

type
    TFtpOption    = (ftpcUNC, ftpCwdCheck, ftpCdupHome, ftpHidePhysicalPath,
                     ftpModeZCompress);  { angus V1.39 } { AG V1.52 } { angus V1.54 }
    TFtpOptions   = set of TFtpOption;
    TDisplayEvent = procedure (Sender : TObject; Msg : String) of object;
    TCommandEvent = procedure (Sender : TObject; CmdBuf : PChar; CmdLen : Integer) of object;
{$IFDEF USE_SSL}
    TCurFtpSslType  = (curftpSslNone,   curftpAuthSsl,      curftpAuthTls,
                       curftpAuthTlsP,  curftpAuthTlsC ,    curftpImplicitSsl);
{$ENDIF}

    TFtpTransMode   = (ftpTransModeStream, ftpTransModeZDeflate) ;  { angus V1.54 }
    TZStreamState   = (ftpZStateNone, ftpZStateSaveDecom, ftpZStateSaveComp{,
                     ftpZStateImmDecon, ftpZStateImmComp});         { angus V1.54 }
    TListType        = (ListTypeName,
                        ListTypeUnix, ListTypeFacts);    { angus V1.54 same as Server }

    TFtpCtrlSocket = class; //Forward

    TClientProcessingThread = class(TThread)  { AG V1.46}
    public
        Client    : TFtpCtrlSocket;
        Keyword   : String;
        Params    : String;
        InData    : String;
        OutData   : String;
        ClientID  : Integer;
        StartTick : LongWord;      { angus V1.54 }
        Sender    : TObject;       { angus V1.54 }
        procedure Execute; override;
    end; 

{$IFNDEF USE_SSL}
    TBaseFtpCtrlSocket = TCustomWSocket;
{$ELSE}
    TBaseFtpCtrlSocket = TCustomSslWSocket;
{$ENDIF}
    TFtpCtrlSocket = class(TBaseFtpCtrlSocket)
    protected
        FDataSocket        : TWSocket;
        FRcvBuf            : PChar;
        FRcvCnt            : Integer;
        FRcvSize           : Integer;
        FBusy              : Boolean;
        FConnectedSince    : TDateTime;
        FLastCommand       : TDateTime;
        FCommandCount      : LongInt;
        FBanner            : String;
        FUserName          : String;
        FPassWord          : String;
        FCloseRequest      : Boolean;
        FHomeDir           : String;
        FDirectory         : String;
        FFtpState          : TFtpCtrlState;
        FAbortingTransfer  : Boolean;
        FUserData          : LongInt;        { Reserved for component user }
        FPeerAddr          : String;
        FPeerSAddr         : TSockAddr;      { AG V1.47 }
        FID                : LongInt;
        FOnDisplay         : TDisplayEvent;
        FOnCommand         : TCommandEvent;
        procedure TriggerSessionConnected(Error : Word); override;
        procedure TriggerSessionClosed(Error : Word); override;
        function  TriggerDataAvailable(Error : Word) : boolean; override;
        procedure TriggerCommand(CmdBuf : PChar; CmdLen : Integer); virtual;
        procedure SetRcvSize(newValue : Integer);
        procedure SetHomeDir(const newValue: String);   { AG V1.52}
    public
        BinaryMode        : Boolean;
        DataAddr          : String;
        DataPort          : String;
        FileName          : String;
        FilePath          : String;
        DataSessionActive : Boolean;
        DataStream        : TStream;
        HasOpenedFile     : Boolean;
        TransferError     : String;
        DataSent          : Boolean;
        CurCmdType        : TFtpCmdType;
        MD5Digest         : TMD5Digest;  { AG V1.46}
        MD5Context        : TMD5Context; { AG V1.46}
        MD5OnTheFlyFlag   : Boolean;     { AG V1.46}
        ProcessingThread  : TClientProcessingThread; { AG V1.46}
        AnswerDelayed     : Boolean;     { AG V1.46}
{$IFDEF STREAM64}                        { V1.43 }
        ByteCount         : Int64;       { upload or download bytes for current data session }
        RestartPos        : Int64;
        HashStartPos      : Int64;       { angus V1.54 start for MD5/CRC }
        HashEndPos        : Int64;       { angus V1.54 start for MD5/CRC }
{$ELSE}
        ByteCount         : LongInt;
        RestartPos        : LongInt;
        HashStartPos      : LongInt;     { angus V1.54 start for MD5/CRC }
        HashEndPos        : LongInt;     { angus V1.54 start for MD5/CRC }
{$ENDIF}
        FromFileName      : String;
        ToFileName        : String;
        PassiveMode       : Boolean;
        PassiveStart      : Boolean;
        PassiveConnected  : Boolean;
        Options           : TFtpOptions;
        OtpMethod         : TOtpMethod;  { angus V1.54 One Time Password authentication method }
        OtpSequence       : Integer;     { angus V1.54 One Time Password current sequence }
        OtpSeed           : String;      { angus V1.54 One Time Password current seed }
        LastTick          : Longword;    { angus V1.54 last tick for time out checking }
        ClntStr           : String;      { angus V1.54 from clnt command }
        DirListPath       : String;      { angus V1.54 last parsed directory listing path }
        DirListSubDir     : Boolean;     { angus V1.54 did we list subdirs }
        DirListHidden     : Boolean;     { angus V1.54 did we list hidden files }
        DirListType       : TListType;   { angus V1.54 how we list files }
        CurrTransMode     : TFtpTransMode; {angus V1.54 current zlib transfer mode }
        ZStreamState      : TZStreamState; { angus V1.54 current Zlib stream state }
        ZReqLevel         : Integer;     { angus V1.54 requested Zlib compression level 1 to 9 }
        ZCurLevel         : Integer;     { angus V1.54 current Zlib compression level 0 to 9 }
   {    ZStreamRec        : TZStreamRec;   angus V1.54 Zlib stream control record for immediate mode }
        ZCompFileName     : String;      { angus V1.54 zlib file name of compressed file }
        ZFileStream       : TStream;     { angus V1.54 Zlib compressed file stream  }
        ZCompInfo         : String;      { angus V1.54 zlib compress information to return with 251 OK }
        ZCompFileDelete   : Boolean;     { angus V1.54 zlib delete compressed file when closing it }
        SessStartTick     : Longword;    { angus V1.54 tick when client session started, for duration check }
        ReqStartTick      : Longword;    { angus V1.54 tick when last request started, for duration check }
        XferStartTick     : Longword;    { angus V1.54 tick when last xfer started, for performance check }
        ReqDurMilliSecs   : Integer;     { angus V1.54 how long last request took, in ticks }
        TotGetBytes       : Int64;       { angus V1.54 how many bytes GET during session, data and control }
        TotPutBytes       : Int64;       { angus V1.54 how many bytes PUT during session, data and control }
        SessIdInfo        : String;      { angus V1.54 session identificaton information for application use }
        FileModeRead      : Word;        { angus V1.57 }
        FileModeWrite     : Word;        { angus V1.57 }
{$IFDEF USE_SSL}
        ProtP             : Boolean;
        AuthFlag          : Boolean;
        CccFlag           : Boolean;
        CurFtpSslType     : TCurFtpSslType;
{$ENDIF}
        constructor Create(AOwner: TComponent); override;
        destructor  Destroy; override;
        procedure   Dup(newHSocket : TSocket); override;
        procedure   StartConnection; virtual;
        procedure   SendAnswer(Answer : String);
        procedure   SetDirectory(newValue : String); virtual;
        procedure   SetAbortingTransfer(newValue : Boolean);
        procedure   BuildDirectory(const Path : String);
        function    GetPeerAddr: string; override;
{$IFDEF USE_SSL}
        function    SslSendPlain(Data : TWSocketData; Len : Integer) : Integer;
{$ENDIF}
        property    DataSocket     : TWSocket    read FDataSocket;
        property    ConnectedSince : TDateTime   read FConnectedSince;
        property    LastCommand    : TDateTime   read FLastCommand;
        property    CommandCount   : LongInt     read FCommandCount;
        property    RcvBuf         : PChar       read FRcvBuf;
        property    RcvdCount;
        property    CloseRequest   : Boolean     read  FCloseRequest
                                                 write FCloseRequest;
        property Directory : String              read  FDirectory
                                                 write SetDirectory;
        property HomeDir : String                read  FHomeDir
                                                 write SetHomeDir;  { AG V1.52}
        property AbortingTransfer   : Boolean    read  FAbortingTransfer
                                                 write SetAbortingTransfer;
        property ID                 : LongInt    read  FID
                                                 write FID;
        property PeerSAddr          : TSockAddr  read  FPeerSAddr;  { AG V1.47 }
{$IFDEF STREAM64}                      { V5.26 }
        property ReadCount          : Int64      read  FReadCount;
{$ELSE}
        property ReadCount          : LongInt    read  FReadCount;
{$ENDIF}
    published
        property FtpState : TFtpCtrlState  read  FFtpState
                                           write FFtpState;
        property Banner : String           read  FBanner
                                           write FBanner;
        property RcvSize : integer         read  FRcvSize
                                           write SetRcvSize;
        property Busy : Boolean            read  FBusy
                                           write FBusy;
        property UserName : String         read  FUserName
                                           write FUserName;
        property PassWord : String         read  FPassWord
                                           write FPassWord;
        property UserData  : LongInt       read  FUserData
                                           write FUserData;
        property OnDisplay : TDisplayEvent read  FOnDisplay
                                           write FOnDisplay;
        property OnCommand : TCommandEvent read  FOnCommand
                                           write FOnCommand;
        property OnSessionClosed;
        property OnDataSent;
        property HSocket;
        property AllSent;
        property State;
{$IFNDEF NO_DEBUG_LOG}
        property IcsLogger;
{$ENDIF}
{$IFDEF VER80}
        property TrumpetCompability;
{$ENDIF}
    end;

function  IsUNC(S : String) : Boolean;
procedure PatchIE5(var S : String);
function FormatFactsDirEntry(F : TSearchRec; const FileName: string) : String;  { angus 1.54  }
{$IFDEF VER80}
function ExtractFileDir(const FileName: String): String;
function ExtractFileDrive(const FileName: String): String;
{$ENDIF}

implementation

const
    DefaultBanner = '220-ICS FTP Server ready';
var
    ThisYear, ThisMonth, ThisDay : Word;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF VER80}
procedure SetLength(var S: string; NewLength: Integer);
begin
    S[0] := chr(NewLength);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ ExtractFileDir extracts the drive and directory parts of the given        }
{ filename. The resulting string is a directory name suitable for passing   }
{ to SetCurrentDir, CreateDir, etc. The resulting string is empty if        }
{ FileName contains no drive and directory parts.                           }
function ExtractFileDir(const FileName: String): String;
var
    I: Integer;
begin
    I := Length(FileName);
    while (I > 0) and (not (FileName[I] in ['\', ':'])) do
        Dec(I);
    if (I > 1) and (FileName[I] = '\') and
       (not (FileName[I - 1] in ['\', ':'])) then
        Dec(I);
    Result := Copy(FileName, 1, I);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ ExtractFileDrive extracts the drive part of the given filename.  For        }
{ filenames with drive letters, the resulting string is '<drive>:'.           }
{ For filenames with a UNC path, the resulting string is in the form          }
{ '\\<servername>\<sharename>'.  If the given path contains neither           }
{ style of filename, the result is an empty string.                           }
function ExtractFileDrive(const FileName: String): String;
var
    I : Integer;
begin
    if Length(FileName) <= 1 then
        Result := ''
    else begin
        if FileName[2] = ':' then
            Result := Copy(FileName, 1, 2)
        else if (FileName[2] = '\') and (FileName[1] = '\') then begin
            { UNC file name }
            I := 3;
            while (I <= Length(FileName)) and (FileName[I] <> '\') do
                Inc(I);
            Inc(I);
            while (I <= Length(FileName)) and (FileName[I] <> '\') do
                Inc(I);
            Result := Copy(FileName, 1, I - 1);
        end
        else
            Result := '';
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TFtpCtrlSocket.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FDataSocket      := TWSocket.Create(Self);
(* Moved to TFtpServer.ServSocketSessionAvailable         01/01/06 AG
{$IFNDEF NO_DEBUG_LOG}
    FDataSocket.IcsLogger := IcsLogger;
{$ENDIF}
*)
{$IFDEF USE_SSL}
    ProtP            := FALSE;
    AuthFlag         := FALSE;
    CccFlag          := FALSE;
    CurFtpSslType    := curftpSslNone;
{$ENDIF}
    FDataSocket.Name := 'DataWSocket';
    FBanner          := DefaultBanner;
    FFtpState        := ftpcInvalid;
    FHomeDir         := 'C:\TEMP\';  { Must include a trailing backslash !!}
    FDirectory       := FHomeDir;    { Must include a trailing backslash !!}
    SetRcvSize(DefaultRcvSize);
    OtpMethod        := OtpKeyNone;  { angus V1.54 One Time Password authentication method }
    OtpSequence      := -1;          { angus V1.54 One Time Password current sequence }
    OtpSeed          := '';          { angus V1.54 One Time Password current seed }
    LastTick         := IcsGetTickCountX;  { angus V1.54 last tick for time out checking }
    SessStartTick    := IcsGetTickCountX;     { angus V1.54 tick when client session started, for duration check }
    ReqStartTick     := 0;    { angus V1.54 tick when last request started, for duration check }
    ReqDurMilliSecs  := 0;    { angus V1.54 how long last request took, in ticks }
    TotGetBytes      := 0;    { angus V1.54 how many bytes GET during session, data and control }
    TotPutBytes      := 0;    { angus V1.54 how many bytes PUT during session, data and control }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TFtpCtrlSocket.Destroy;
begin
    FRcvCnt := 0;      { Clear received data }
    SetRcvSize(0);     { Free the buffer     }
    if Assigned(FDataSocket) then begin
        FDataSocket.Destroy;
        FDataSocket := nil;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpCtrlSocket.SetRcvSize(newValue : Integer);
begin
    if FRcvCnt <> 0 then
        raise EFtpCtrlSocketException.Create('Data in buffer, can''t change size');

    if FRcvSize < 0 then
        FRcvSize := 0;

    if FRcvSize = newValue then
        Exit; { No change, nothing to do }

    { Free previously allocated buffer }
    if FRcvBuf <> nil then begin
        FreeMem(FRcvBuf, FRcvSize);
        FRcvBuf := nil;
    end;

    { Allocate new buffer }
    FRcvSize := newValue;

    { If size is nul, then do not allocated the buffer }
    if newValue > 0 then
        GetMem(FRcvBuf, FRcvSize);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpCtrlSocket.StartConnection;
begin
    FConnectedSince := Now;
    FLastCommand    := 0;
    FCommandCount   := 0;
    FFtpState       := ftpcWaitingUserCode;
    SendStr(FBanner + #13#10);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpCtrlSocket.GetPeerAddr: String;
begin
    Result := FPeerAddr;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpCtrlSocket.Dup(newHSocket : TSocket);
var
    Len : Integer;
begin
    inherited Dup(newHSocket);
{$IFDEF CLR}
    if DesignMode then begin
        FPeerAddr := '';
        Exit;
    end;
{$ENDIF}
    //FPeerAddr := inherited GetPeerAddr;
    Len := SizeOf(TSockAddr);
    if WSocket_GetPeerName(newHSocket, FPeerSAddr, Len) = 0 then
        FPeerAddr := WSocket_inet_ntoa(FPeerSAddr.sin_addr)
    else begin
        SocketError('GetPeerName');
        Exit;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpCtrlSocket.TriggerSessionClosed(Error: Word);
begin
    if Assigned(ProcessingThread) then
        ProcessingThread.Terminate;
    inherited TriggerSessionClosed(Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpCtrlSocket.TriggerSessionConnected(Error : Word);
begin
    FPeerAddr := inherited GetPeerAddr;
    inherited TriggerSessionConnected(Error);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpCtrlSocket.TriggerCommand(CmdBuf : PChar; CmdLen : Integer);
begin
    if Assigned(FOnCommand) then
        FOnCommand(Self, CmdBuf, CmdLen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TFtpCtrlSocket.TriggerDataAvailable(Error : Word) : Boolean;
var
    Len  : Integer;
    I    : Integer;
begin
    Result := TRUE;                                { We read data }

    Len := Receive(@FRcvBuf[FRcvCnt], FRcvSize - FRcvCnt - 1);
    if Len <= 0 then
        Exit;

    FRcvCnt := FRcvCnt + Len;
    FRcvBuf[FRcvCnt] := #0;
    LastTick := IcsGetTickCountX;  { angus V1.54 last tick for time out checking }
    TotPutBytes := TotPutBytes + Len;    { angus V1.54 }

    while TRUE do begin
        I := 0;
        while (I < FRcvCnt) and (FRcvBuf[I] <> #10) do
            Inc(I);
        if I >= FRcvCnt then begin
            { Check line overflow. }
            if FRcvCnt >= (FRcvSize - 1) then begin
                StrPCopy(FRcvBuf, 'OVER' + #13#10);
                FRcvCnt := StrLen(FRcvBuf);
                I       := FRcvCnt - 1;
            end
            else
                Exit;
        end;
        FRcvBuf[I]   := #0;
        FLastCommand := Now;
        Inc(FCommandCount);
        if (I > 1) and (FRcvBuf[I - 1] = #13) then begin
            FRcvBuf[I - 1] := #0;
            TriggerCommand(FRcvBuf, I - 1);
            FRcvBuf[I - 1] := #13;
        end
        else
            TriggerCommand(FRcvBuf, I);

        FRcvBuf[I] := #10;
        if I >= (FRcvCnt - 1) then begin
            FRcvCnt    := 0;
            FRcvBuf[0] := #0;
            break;
        end;
        Move(FRcvBuf[I + 1], FRcvBuf^, FRcvCnt - I);
        FRcvCnt := FRcvCnt - I - 1;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpCtrlSocket.SendAnswer(Answer : String);
begin
    SendStr(Answer + #13#10);
    LastTick := IcsGetTickCountX;  { angus V1.54 last tick for time out checking }
    TotGetBytes := TotGetBytes + Length (Answer) + 2;    { angus V1.54 }
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsUNC(S : String) : Boolean;
begin
    Result := (Length(S) >= 2) and (S[2] = '\') and (S[1] = '\');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure PatchIE5(var S : String);
begin
    { \c:\Temp\ -> c:\Temp\ IE5 like this invalid syntax !}
    if (Length(S) >= 3) and (S[3] = ':') and (S[1] = '\') then
        Delete(S, 1, 1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpCtrlSocket.SetDirectory(newValue : String);
var
    newDrive : String;
    newPath  : String;
    I        : Integer;
begin
    if FDirectory = newValue then
        Exit;
    PatchIE5(newValue);
    newDrive := ExtractFileDrive(newValue);
    if IsUNC(newDrive) then begin
        if not (ftpcUNC in Options) then
            raise Exception.Create('Cannot accept UNC path');
        FDirectory := newValue;
        { Always terminate with a backslash }
        if (Length(FDirectory) > 0) and (FDirectory[Length(FDirectory)] <> '\') then
            FDirectory := FDirectory + '\';
        Exit;
    end;
    (*                                        { AG V1.52 }
    if Length(newDrive) = 0 then begin
        newDrive := ExtractFileDrive(FDirectory);
        newPath  := newValue;
    end
    else
        newPath := Copy(newValue, 3, Length(newValue));
    *)
    if Length(newDrive) = 0 then begin        { AG V1.52 }
        if (ftpCdUpHome in Options) then begin
            if (Length(newValue) > 0) and (newValue[1] = '\') then begin
                { absolute path, HomeDir }
                newDrive := ExtractFileDrive(FHomeDir);
                newPath  := Copy(FHomeDir, Length(newDrive) + 1, Length(FHomeDir)) +
                                 Copy(newValue, 2, Length(newValue))
            end
            else begin
                newDrive := ExtractFileDrive(FDirectory);
                newPath  := newValue;
            end;
        end
        else begin
          newDrive := ExtractFileDrive(FDirectory);
          newPath  := newValue;
        end;
    end
    else
        newPath := Copy(newValue, 3, Length(newValue));


    if Pos(':', newPath) <> 0 then
        raise Exception.Create('Invalid directory name syntax');

    if newPath = '..' then begin
        if IsUNC(FDirectory) then begin
            I := Length(FDirectory) - 1;
            while (I > 0) and (FDirectory[I] <> '\') do
                Dec(I);
            if I > Length(newDrive) then
                SetLength(FDirectory, I);
            Exit;
        end
        else begin
            newPath := Copy(FDirectory, 3, Length(FDirectory));
            I := Length(newPath) - 1;
            while (I > 0) and (newPath[I] <> '\') do
                Dec(I);
            SetLength(newPath, I);
        end;
    end;

    if (Length(newPath) > 0) and (newPath[1] <> '\') then begin
        { Relative path }
        if IsUNC(FDirectory) then begin
            FDirectory := FDirectory + newPath;
            { Always terminate with a backslash }
            if (Length(FDirectory) > 0) and (FDirectory[Length(FDirectory)] <> '\') then
                FDirectory := FDirectory + '\';
            Exit;
        end
        else begin
            if UpperCase(newDrive[1]) <> UpperCase(FDirectory[1]) then
                raise Exception.Create('Cannot accept path not relative to current directory');
            if Pos('.\', newPath) <> 0 then
                raise Exception.Create('Cannot accept relative path using dot notation');
            if newPath = '.' then
                newPath := Copy(FDirectory, 3, Length(FDirectory))
            else
                newPath := Copy(FDirectory, 3, Length(FDirectory)) + newPath;
        end;
    end
    else begin
        if Pos('.\', newPath) <> 0 then
            raise Exception.Create('Cannot accept relative path using dot notation');
    end;

    if Length(newPath) = 0 then begin
        if UpperCase(newDrive[1]) <> UpperCase(FDirectory[1]) then
            newPath := '\'
        else
            newPath := Copy(FDirectory, 3, Length(FDirectory));
    end;

    { Always terminate with a backslash }
    if (Length(newPath) > 0) and (newPath[Length(newPath)] <> '\') then
        newPath := newPath + '\';

    FDirectory := newDrive + newPath;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpCtrlSocket.SetHomeDir(const newValue: String);
begin
    if FHomeDir = newValue then
        Exit;
    if (Length(newValue) > 0) and (newValue[Length(newValue)] <> '\') then
        FHomeDir := newValue + '\'
    else
        FHomeDir := newValue;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpCtrlSocket.SetAbortingTransfer(newValue : Boolean);
begin
    FAbortingTransfer := newValue;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function FormatUnixDirEntry(F : TSearchRec; const FileName: string) : String;
var
    Attr             : String;
    Ext              : String;
    Day, Month, Year : Integer;
    Hour, Min        : Integer;
    SizeStr          : String;
    TimeStr          : String;
const
    StrMonth : array [1..12] of String =
        ('Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec');
begin
{$IFNDEF VER80}{$WARNINGS OFF}{$ENDIF}
    if ((F.Attr and faVolumeID) <> 0) {or
       ((F.Attr and faHidden)   <> 0)} then begin
        { Ignore hidden files and volume ID entries }
        Result := '';
        Exit;
    end;
    { Owner - Group - Others }
    Attr := '-rw-rw-rw-';
    if (F.Attr and faDirectory) <> 0 then
        Attr[1] := 'd';

    if (F.Attr and faReadOnly) <> 0 then begin
        Attr[3] := '-';
        Attr[6] := '-';
        Attr[9] := '-';
    end;
{$IFNDEF VER80}{$WARNINGS ON}{$ENDIF}

    Ext := UpperCase(ExtractFileExt(FileName));
    if (Ext = '.EXE') or (Ext = '.COM') or (Ext = '.BAT') then begin
        Attr[4]  := 'x';
        Attr[7]  := 'x';
        Attr[10] := 'x';
    end;

    Day   := (HIWORD(F.Time) and $1F);
    Month := ((HIWORD(F.Time) shr 5) and $0F);
    Year  := ((HIWORD(F.Time) shr 9) and $3F) + 1980;
{   Sec   := ((F.Time and $1F) shl 1); }
    Min   := ((F.Time shr 5) and $3F);
    Hour  := ((F.Time shr 11) and $1F);

{$IFDEF STREAM64} { Defined in Delphi 6 and up }
    if F.FindData.nFileSizeHigh = 0 then
        SizeStr := IntToStr(F.FindData.nFileSizeLow)
    else
        SizeStr := IntToStr(F.FindData.nFileSizeLow +
                           (Int64(F.FindData.nFileSizeHigh) shl 32));
{$ELSE}
    { WARNING: TSearchRec.Size is an integer which limit file size to 2GB }
    { Every file of greater size than 2GB will report an incorrect size   }
    SizeStr := IntToStr(F.Size and $7FFFFFFF);
{$ENDIF}

    if Year = ThisYear then
        TimeStr := Format('%2.2d:%2.2d', [Hour, Min])
    else
        TimeStr := Format('%5d', [Year]);

    Result := Attr + '   1 ftp      ftp  ' +
              Format('%11s %s %2.2d %5s ',
                     [SizeStr, StrMonth[Month], Day, TimeStr]) +
              FileName + #13#10;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF VER80}
function FileTimeToStr(const FileTime: TFileTime): String;     { angus V1.38 }
const
  FileTimeBase = -109205.0;   { days between years 1601 and 1900 }
  FileTimeStep: Extended = 24.0 * 60.0 * 60.0 * 1000.0 * 1000.0 * 10.0; { 100 nsec per Day }
var
    F64    : Comp absolute FileTime;
    TempDT : TDateTime;
begin
    TempDT := F64 / FileTimeStep;
    TempDT := TempDT + FileTimeBase;
    Result := FormatDateTime (UtcDateMaskPacked, TempDT);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{FTP MLSD command, same format for MSLT for a single file
much nice than LIST since it has a proper date with year, and seconds, and is much easier to parse
size=0;type=cdir;perm=fdelcmp;create=20020616151738;modify=20031002125810; .
size=0;type=pdir;perm=fdelcmp;create=20020616151738;modify=20031002125810; ..
size=17199;type=file;perm=fdrwa;create=20030616152030;modify=20031001190100; 00master.zip
size=182928;type=file;perm=fdrwa;create=20030922195127;modify=20030922190600; 12=page-004394.zip
size=134503;type=file;perm=fdrwa;create=20030923181732;modify=20030923170800; 12=page-004399.zip
size=225460;type=file;perm=fdrwa;create=20030923193147;modify=20030923185600; 12=page-004400.zip
size=205011;type=file;perm=fdrwa;create=20030923120836;modify=20030922225700; 12=page-004405.zip
size=191721;type=file;perm=fdrwa;create=20030905141821;modify=20030904181100; 20=page-004320.zip
size=183977;type=file;perm=fdrwa;create=20030905142247;modify=20030904181100; 20=page-004321.zip
size=0;type=dir;perm=fdelcmp;create=20030219123018;modify=20030305153855; errors
size=0;type=dir;perm=fdelcmp;create=20021217151845;modify=20030903193625; new software
size=0;type=dir;perm=fdelcmp;create=20020805160304;modify=20031002133003; sql logs
size=70806;type=file;perm=fdrwa;create=20030718113340;modify=20031001185600; vehinfiles.zip
size=0;type=dir;perm=fdelcmp;create=20020801100314;modify=20031004124403; zip logs  }

function FormatFactsDirEntry(F : TSearchRec; const FileName: string) : String;  { angus V1.38, 1.54 added FileName }
var
    SizeStr : String;
begin
{$IFNDEF VER80}{$WARNINGS OFF}{$ENDIF}
    if ((F.Attr and faVolumeID) <> 0)  then begin
        { Ignore volume ID entries }
        Result := '';
        Exit;
    end;

{$IFDEF STREAM64} { Defined in Delphi 6 and up }
    if F.FindData.nFileSizeHigh = 0 then
        SizeStr := IntToStr(F.FindData.nFileSizeLow)
    else
        SizeStr := IntToStr(F.FindData.nFileSizeLow +
                           (Int64(F.FindData.nFileSizeHigh) shl 32));
{$ELSE}
    { WARNING: TSearchRec.Size is an integer which limit file size to 2GB }
    { Every file of greater size than 2GB will report an incorrect size   }
    SizeStr := IntToStr(F.Size and $7FFFFFFF);
{$ENDIF}

    { PERMissions is advisory only, max 10 characters - not properly set here }
    { a - APPE allowed for a file                                             }
    { c - files may be created in this directory                              }
    { d - may be deleted                                                      }
    { e - directory entry allowed                                             }
    { f - may be renamed                                                      }
    { l - directory may be listed                                             }
    { m - new directories may be made                                         }
    { p - file may be deleted from the directory                              }
    { r - RETR allowed for a file                                             }
    { w - STOR allowed for a file                                             }
    if (F.Attr and faDirectory) <> 0 then begin
        if FileName = '.' then
            result := 'size=0;type=cdir;perm=fdelcmp;'
        else if FileName = '..' then
            result := 'size=0;type=pdir;perm=fdelcmp;'
        else
            result := 'size=0;type=dir;perm=fdelcmp;'
    end
    else begin
        result := 'size=' + SizeStr + ';type=file;perm=';
        if (F.Attr and faReadOnly) <> 0 then
            result := result + 'rw;'
        else
            result := result + 'fdrwa;';
    end;

{$IFDEF VER80}
    SizeStr := FormatDateTime (UtcDateMaskPacked, FileDateToDateTime (F.Time));
    result := result +
        'create=' + SizeStr + ';modify=' + SizeStr +
        '; ' + FileName;
{$ELSE}
    result := result +
        'create=' + FileTimeToStr (F.FindData.ftCreationTime) +
        ';modify=' + FileTimeToStr (F.FindData.ftLastWriteTime) +
        '; ' + FileName;    { note space before filename is delimiter }
{$ENDIF}
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TFtpCtrlSocket.BuildDirectory(const Path : String);     { angus 1.54  }
var
    F          : TSearchRec;
    Status     : Integer;
    Buf        : String;
    LocFiles   : TIcsFileRecs;  { angus 1.54 dynamic array of File Records }
    LocFileList: TList;         { angus 1.54 sorted pointers to File Records }
    I          : Integer;
    TotFiles   : Integer;
    FileRecX   : PTIcsFileRec;
begin
    DecodeDate(Now, ThisYear, ThisMonth, ThisDay);

 { angus 1.54 build sorted recursive directory }
    if DirListSubDir then begin
        SetLength (LocFiles, 250);   { initial expected number of files }
        LocFileList := TList.Create;
        try
         { fill LocFiles dynamic array with SearchRecs, sorted by LocFileList }
            TotFiles := IcsGetDirList (DirListPath, DirListSubDir,
                                            DirListHidden, LocFiles, LocFileList) ;
            if TotFiles > 0 then begin
              { need a descendent of TMemoryStream with SetCapacity }
              {  TMemoryStream (Stream).SetCapacity (TotFiles * 128);  }
                for I := 0 to Pred (TotFiles) do begin
                    if LocFileList [I] = Nil then continue ;
                    FileRecX := LocFileList [I] ;   { get file record pointer }
                    if DirListSubDir then   { add path before file name }
                        Buf := BackSlashesToSlashes(FileRecX^.FrSubDirs) +
                                                     FileRecX^.FrSearchRec.Name
                    else
                        Buf := FileRecX^.FrSearchRec.Name;

                { build single line according to listing style }
                    if DirListType = ListTypeUnix then
                        Buf := FormatUnixDirEntry(FileRecX^.FrSearchRec, Buf)
                    else if DirListType = ListTypeFacts then
                        Buf := FormatFactsDirEntry(FileRecX^.FrSearchRec, Buf) + #13#10
                    else
                        Buf := Buf + #13#10;
                    if Length(Buf) > 0 then begin
                        if CurCmdType = ftpcSiteIndex then Buf := '200-' + Buf;
                        if CurCmdType = ftpcSiteCmlsd then Buf := '250-' + Buf;
                        DataStream.Write(Buf[1], Length(Buf));
                    end;
                end;
            end;
        finally
            SetLength (LocFiles, 0);
            LocFileList.Free;
        end;
    end
    else begin
        if DirListHidden then
            Status := FindFirst(DirListPath, faAnyFile, F)
        else
            Status := FindFirst(DirListPath, faArchive + faDirectory, F);
        while Status = 0 do begin
            if DirListType = ListTypeUnix then               { angus V1.38 }
                Buf := FormatUnixDirEntry(F, F.Name)
            else if DirListType = ListTypeFacts then         { angus V1.38 }
                Buf := FormatFactsDirEntry(F, F.Name) + #13#10    { angus V1.38, V1.54 added Name }
            else
                Buf := F.Name + #13#10;
            if Length(Buf) > 0 then begin
                if CurCmdType = ftpcSiteIndex then Buf := '200-' + Buf; { angus 1.54 }
                if CurCmdType = ftpcSiteCmlsd then Buf := '250-' + Buf;    { angus 1.54 }
                DataStream.Write(Buf[1], Length(Buf));
            end;
            Status := FindNext(F);
        end;
        FindClose(F);
    end;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF USE_SSL}
function TFtpCtrlSocket.SslSendPlain(Data : TWSocketData; Len : Integer) : Integer;
begin
    Result := RealSend(Data, Len);
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *} { AG V1.46}
procedure FileMD5OnProgress(
    Obj: TObject;
    Count: {$IFDEF STREAM64} Int64 {$ELSE} Integer {$ENDIF};
    var Cancel: Boolean);
begin
    Cancel := (Obj as TClientProcessingThread).Terminated;
    (Obj as TClientProcessingThread).Client.LastTick := IcsGetTickCountX;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TClientProcessingThread.Execute;                        { AG V1.46}
var
    NewSize: Int64;
begin
    ClientID := Client.ID;
    try
        with Client.ProcessingThread do begin
            StartTick := IcsGetTickCountX;
            if (Keyword = 'MD5') or (Keyword = 'XMD5')  then    { angus V1.54 }
                OutData := FileMD5(InData, Self, FileMD5OnProgress,
                   Client.HashStartPos, Client.HashEndPos, Client.FileModeRead) { angus V1.57 }
            else if (Keyword = 'XCRC') then                                   { angus V1.54 }
                OutData := FileCRC32B(InData, Self, FileMD5OnProgress,
                   Client.HashStartPos, Client.HashEndPos, Client.FileModeRead) { angus V1.57 }
            else if (Keyword = 'DIRECTORY') then begin                  { angus V1.54 }
                OutData := Keyword;
                Client.BuildDirectory(InData);
                Client.DataStream.Seek(0, 0);
            end
{$IFDEF USE_MODEZ}              { angus V1.54 }
            else if (Keyword = 'COMPRESS') then begin { angus V1.54 }
                with Client do begin
                    try
                     { angus V1.55 data stream may be set to restart position, but check sensible }
                        NewSize := DataStream.Size - DataStream.Position;
                        if NewSize < 0 then begin
                            OutData := 'Failed to compress file - Invalid restart position or';
                            ZCompFileDelete := True;
                            Exit;
                        end;
                        ZlibCompressStreamEx(DataStream, ZFileStream, ZCurLevel,
                                         zsZLib, false, Self, FileMD5OnProgress);   { angus V1.55 }
                        ZFileStream.Position := 0 ;
                        ZCompInfo := ' compressed size ' + IntToKbyte(ZFileStream.Size) +
                            'bytes, uncompressed size ' + IntToKbyte(NewSize) + 'bytes' ;
                     { close data file now, not needed any more }
                        DataStream.Destroy;
                        DataStream := Nil;
                        OutData := ''; { OK }
                    except
                        on E:Exception do begin
                            OutData := 'Failed to compress file - ' + E.Message;
                            ZCompFileDelete := True;
                        end;
                    end;
                end;
            end
            else if (Keyword = 'DECOMPRESS') then begin { angus V1.54 }
                with Client do begin
                    try
                        ZFileStream.Position := 0;
                        NewSize := DataStream.Size ;
                        ZlibDecompressStreamEx(ZFileStream, DataStream,
                                                 Self, FileMD5OnProgress) ;   { angus V1.55 }
                        NewSize := DataStream.Size - NewSize ;
                        ZCompInfo := ' compressed size ' + IntToKbyte(Client.ZFileStream.Size) +
                             'bytes, uncompressed size ' + IntToKbyte(NewSize) + 'bytes' ;
                        OutData := ''; { OK }
                    except
                        on E:Exception do begin
                            OutData := 'Failed to decompress file - ' + E.Message;
                        end;
                    end;
                end;
            end
{$ENDIF}
        else
            OutData := '';
        end;
    except
        OutData := '';
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.


