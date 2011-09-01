{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Description:  This unit encapsulate the ICMP.DLL into an object of type TICMP.
              Using this object, you can easily ping any host on your network.
              Works only in 32 bits mode (no Delphi 1) under NT or 95.
              TICMP is perfect for a console mode program, but if you build a
              GUI program, you could use the TPing object wich is a true VCL
              encapsulating the TICMP object. Then you can use object inspector
              to change properties or event handler. This is much simpler to
              use for a GUI program.
Creation:     January 6, 1997
Version:      7.01
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2010 by François PIETTE
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

Updates:
Dec 13, 1997 V1.01 Added OnEchoRequest and OnEchoReply events and removed the
             corresponding OnDisplay event. This require to modify existing
             programs.
Mar 15, 1998 V1.02 Deplaced address resolution just before use
Sep 24, 1998 V1.02a Changed TIPAddr and others to LongInt to avoid range error
             problems with Delphi 4
Jan 24, 1999 V1.03 Surfaced Flags property to allow fragmentation check
             (Flags = IP_FLAG_DF to enable fragmentation check)
Jan 19, 2004 V1.04 Added property ICMPDLLHandle.
May 32, 2004 V1.05 Used ICSDEFS.INC
Mar 26, 2006 V6.00 Started new version 6
Mar 24, 2008 V6.01 Francois Piette made some changes to prepare code
                   for Unicode.
                   Use of AnsiString.
Aug 12, 2008 V7.00 Reverted from AnsiString to String for properties.
Jul 25, 2011 V7.01 Added directive "EXTERNALSYM"


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsIcmp;

interface

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$I OverbyteIcsDefs.inc}
{$IFDEF COMPILER14_UP}
  {$IFDEF NO_EXTENDED_RTTI}
    {$RTTI EXPLICIT METHODS([]) FIELDS([]) PROPERTIES([])}
  {$ENDIF}
{$ENDIF}
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
{$IFDEF VER80}
// This source file is *NOT* compatible with Delphi 1 because it uses
// Win 32 features.
{$ENDIF}

uses
{$IFDEF USEWINDOWS}
    Windows,
{$ELSE}
    WinTypes, WinProcs,
{$ENDIF}
    SysUtils, Classes, WinSock;

const
  IcmpVersion = 7.01;
  CopyRight : String   = ' TICMP (c) 1997-2010 F. Piette V7.01 ';
  IcmpDLL     = 'icmp.dll';

  // IP status codes returned to transports and user IOCTLs.
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_SUCCESS} {$ENDIF}
  IP_SUCCESS                  = 0;
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_STATUS_BASE} {$ENDIF}
  IP_STATUS_BASE              = 11000;
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_BUF_TOO_SMALL} {$ENDIF}
  IP_BUF_TOO_SMALL            = (IP_STATUS_BASE + 1);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_DEST_NET_UNREACHABLE} {$ENDIF}
  IP_DEST_NET_UNREACHABLE     = (IP_STATUS_BASE + 2);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_DEST_HOST_UNREACHABLE} {$ENDIF}
  IP_DEST_HOST_UNREACHABLE    = (IP_STATUS_BASE + 3);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_DEST_PROT_UNREACHABLE} {$ENDIF}
  IP_DEST_PROT_UNREACHABLE    = (IP_STATUS_BASE + 4);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_DEST_PORT_UNREACHABLE} {$ENDIF}
  IP_DEST_PORT_UNREACHABLE    = (IP_STATUS_BASE + 5);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_NO_RESOURCES} {$ENDIF}
  IP_NO_RESOURCES             = (IP_STATUS_BASE + 6);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_BAD_OPTION} {$ENDIF}
  IP_BAD_OPTION               = (IP_STATUS_BASE + 7);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_HW_ERROR} {$ENDIF}
  IP_HW_ERROR                 = (IP_STATUS_BASE + 8);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_PACKET_TOO_BIG} {$ENDIF}
  IP_PACKET_TOO_BIG           = (IP_STATUS_BASE + 9);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_REQ_TIMED_OUT} {$ENDIF}
  IP_REQ_TIMED_OUT            = (IP_STATUS_BASE + 10);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_BAD_REQ} {$ENDIF}
  IP_BAD_REQ                  = (IP_STATUS_BASE + 11);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_BAD_ROUTE} {$ENDIF}
  IP_BAD_ROUTE                = (IP_STATUS_BASE + 12);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_TTL_EXPIRED_TRANSIT} {$ENDIF}
  IP_TTL_EXPIRED_TRANSIT      = (IP_STATUS_BASE + 13);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_TTL_EXPIRED_REASSEM} {$ENDIF}
  IP_TTL_EXPIRED_REASSEM      = (IP_STATUS_BASE + 14);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_PARAM_PROBLEM} {$ENDIF}
  IP_PARAM_PROBLEM            = (IP_STATUS_BASE + 15);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_SOURCE_QUENCH} {$ENDIF}
  IP_SOURCE_QUENCH            = (IP_STATUS_BASE + 16);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_OPTION_TOO_BIG} {$ENDIF}
  IP_OPTION_TOO_BIG           = (IP_STATUS_BASE + 17);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_BAD_DESTINATION} {$ENDIF}
  IP_BAD_DESTINATION          = (IP_STATUS_BASE + 18);

  // status codes passed up on status indications.
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_ADDR_DELETED} {$ENDIF}
  IP_ADDR_DELETED             = (IP_STATUS_BASE + 19);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_SPEC_MTU_CHANGE} {$ENDIF}
  IP_SPEC_MTU_CHANGE          = (IP_STATUS_BASE + 20);
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_MTU_CHANGE} {$ENDIF}
  IP_MTU_CHANGE               = (IP_STATUS_BASE + 21);

  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_GENERAL_FAILURE} {$ENDIF}
  IP_GENERAL_FAILURE          = (IP_STATUS_BASE + 50);

  {$IFDEF COMPILER16_UP} {$EXTERNALSYM MAX_IP_STATUS} {$ENDIF}
  MAX_IP_STATUS               = IP_GENERAL_FAILURE;

  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_PENDING} {$ENDIF}
  IP_PENDING                  = (IP_STATUS_BASE + 255);

  // IP header flags
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_FLAG_DF} {$ENDIF}
  IP_FLAG_DF                  = $02;         // Don't fragment this packet.

  // IP Option Types
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_OPT_EOL} {$ENDIF}
  IP_OPT_EOL                  = $00;         // End of list option
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_OPT_NOP} {$ENDIF}
  IP_OPT_NOP                  = $01;         // No operation
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_OPT_SECURITY} {$ENDIF}
  IP_OPT_SECURITY             = $82;         // Security option.
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_OPT_LSRR} {$ENDIF}
  IP_OPT_LSRR                 = $83;         // Loose source route.
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_OPT_SSRR} {$ENDIF}
  IP_OPT_SSRR                 = $89;         // Strict source route.
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_OPT_RR} {$ENDIF}
  IP_OPT_RR                   = $07;         // Record route.
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_OPT_TS} {$ENDIF}
  IP_OPT_TS                   = $44;         // Timestamp.
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM IP_OPT_SID} {$ENDIF}
  IP_OPT_SID                  = $88;         // Stream ID (obsolete)
  {$IFDEF COMPILER16_UP} {$EXTERNALSYM MAX_OPT_SIZE} {$ENDIF}
  MAX_OPT_SIZE                = $40;

type
  // IP types
  TIPAddr   = LongInt;   // An IP address.
  TIPMask   = LongInt;   // An IP subnet mask.
  TIPStatus = LongInt;   // Status code returned from IP APIs.

  PIPOptionInformation = ^TIPOptionInformation;
  TIPOptionInformation = packed record
     TTL:         Byte;      // Time To Live (used for traceroute)
     TOS:         Byte;      // Type Of Service (usually 0)
     Flags:       Byte;      // IP header flags (usually 0)
     OptionsSize: Byte;      // Size of options data (usually 0, max 40)
     OptionsData: PAnsiChar; // Options data buffer
  end;

  PIcmpEchoReply = ^TIcmpEchoReply;
  TIcmpEchoReply = packed record
     Address:       TIPAddr;              // Replying address
     Status:        DWord;                // IP status value
     RTT:           DWord;                // Round Trip Time in milliseconds
     DataSize:      Word;                 // Reply data size
     Reserved:      Word;                 // Reserved
     Data:          Pointer;              // Pointer to reply data buffer
     Options:       TIPOptionInformation; // Reply options
  end;

  // IcmpCreateFile:
  //     Opens a handle on which ICMP Echo Requests can be issued.
  // Arguments:
  //     None.
  // Return Value:
  //     An open file handle or INVALID_HANDLE_VALUE. Extended error information
  //     is available by calling GetLastError().
  TIcmpCreateFile  = function: THandle; stdcall;

  // IcmpCloseHandle:
  //     Closes a handle opened by ICMPOpenFile.
  // Arguments:
  //     IcmpHandle  - The handle to close.
  // Return Value:
  //     TRUE if the handle was closed successfully, otherwise FALSE. Extended
  //     error information is available by calling GetLastError().
  TIcmpCloseHandle = function(IcmpHandle: THandle): Boolean; stdcall;

  // IcmpSendEcho:
  //     Sends an ICMP Echo request and returns one or more replies. The
  //     call returns when the timeout has expired or the reply buffer
  //     is filled.
  // Arguments:
  //     IcmpHandle         - An open handle returned by ICMPCreateFile.
  //     DestinationAddress - The destination of the echo request.
  //     RequestData        - A buffer containing the data to send in the
  //                          request.
  //     RequestSize        - The number of bytes in the request data buffer.
  //     RequestOptions     - Pointer to the IP header options for the request.
  //                          May be NULL.
  //     ReplyBuffer        - A buffer to hold any replies to the request.
  //                          On return, the buffer will contain an array of
  //                          ICMP_ECHO_REPLY structures followed by options
  //                          and data. The buffer should be large enough to
  //                          hold at least one ICMP_ECHO_REPLY structure
  //                          and 8 bytes of data - this is the size of
  //                          an ICMP error message.
  //     ReplySize          - The size in bytes of the reply buffer.
  //     Timeout            - The time in milliseconds to wait for replies.
  // Return Value:
  //     Returns the number of replies received and stored in ReplyBuffer. If
  //     the return value is zero, extended error information is available
  //     via GetLastError().
  TIcmpSendEcho    = function(IcmpHandle:          THandle;
                              DestinationAddress:  TIPAddr;
                              RequestData:         Pointer;
                              RequestSize:         Word;
                              RequestOptions:      PIPOptionInformation;
                              ReplyBuffer:         Pointer;
                              ReplySize:           DWord;
                              Timeout:             DWord
                             ): DWord; stdcall;

  // Event handler type declaration for TICMP.OnDisplay event.
  TICMPDisplay = procedure(Sender: TObject; Msg : String) of object;
  TICMPReply   = procedure(Sender: TObject; Error : Integer) of object;

  // The object wich encapsulate the ICMP.DLL
  TICMP = class(TObject)
  private
    hICMPdll :        HModule;                    // Handle for ICMP.DLL
    IcmpCreateFile :  TIcmpCreateFile;
    IcmpCloseHandle : TIcmpCloseHandle;
    IcmpSendEcho :    TIcmpSendEcho;
    hICMP :           THandle;                    // Handle for the ICMP Calls
    FReply :          TIcmpEchoReply;             // ICMP Echo reply buffer
    FAddress :        String;                     // Address given
    FHostName :       String;                     // Dotted IP of host (output)
    FHostIP :         String;                     // Name of host      (Output)
    FIPAddress :      TIPAddr;                    // Address of host to contact
    FSize :           Integer;                    // Packet size (default to 56)
    FTimeOut :        Integer;                    // Timeout (default to 4000mS)
    FTTL :            Integer;                    // Time To Live (for send)
    FFlags :          Integer;                    // Options flags
    FOnDisplay :      TICMPDisplay;               // Event handler to display
    FOnEchoRequest :  TNotifyEvent;
    FOnEchoReply :    TICMPReply;
    FLastError :      DWORD;                      // After sending ICMP packet
    FAddrResolved :   Boolean;
    procedure ResolveAddr;
  public
    constructor Create; virtual;
    destructor  Destroy; override;
    function    Ping : Integer;
    procedure   SetAddress(Value : String);
    function    GetErrorString : String;

    property Address       : String         read  FAddress   write SetAddress;
    property Size          : Integer        read  FSize      write FSize;
    property Timeout       : Integer        read  FTimeout   write FTimeout;
    property Reply         : TIcmpEchoReply read  FReply;
    property TTL           : Integer        read  FTTL       write FTTL;
    Property Flags         : Integer        read  FFlags     write FFlags;
    property ErrorCode     : DWORD          read  FLastError;
    property ErrorString   : String         read  GetErrorString;
    property HostName      : String         read  FHostName;
    property HostIP        : String         read  FHostIP;
    property ICMPDLLHandle : HModule        read  hICMPdll;
    property OnDisplay     : TICMPDisplay   read  FOnDisplay write FOnDisplay;
    property OnEchoRequest : TNotifyEvent   read  FOnEchoRequest
                                            write FOnEchoRequest;
    property OnEchoReply   : TICMPReply     read  FOnEchoReply
                                            write FOnEchoReply;
  end;

  TICMPException = class(Exception);

implementation

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TICMP.Create;
var
    WSAData: TWSAData;
begin
    hICMP    := INVALID_HANDLE_VALUE;
    FSize    := 56;
    FTTL     := 64;
    FTimeOut := 4000;

    // initialise winsock
    if WSAStartup($101, WSAData) <> 0 then
        raise TICMPException.Create('Error initialising Winsock');

    // register the icmp.dll stuff
    hICMPdll := LoadLibrary(icmpDLL);
    if hICMPdll = 0 then
        raise TICMPException.Create('Unable to register ' + icmpDLL);

    @ICMPCreateFile  := GetProcAddress(hICMPdll, 'IcmpCreateFile');
    @IcmpCloseHandle := GetProcAddress(hICMPdll, 'IcmpCloseHandle');
    @IcmpSendEcho    := GetProcAddress(hICMPdll, 'IcmpSendEcho');

    if (@ICMPCreateFile = Nil) or
       (@IcmpCloseHandle = Nil) or
       (@IcmpSendEcho = Nil) then
          raise TICMPException.Create('Error loading dll functions');

    hICMP := IcmpCreateFile;
    if hICMP = INVALID_HANDLE_VALUE then
        raise TICMPException.Create('Unable to get ping handle');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TICMP.Destroy;
begin
    if hICMP <> INVALID_HANDLE_VALUE then
        IcmpCloseHandle(hICMP);
    if hICMPdll <> 0 then
        FreeLibrary(hICMPdll);
    WSACleanup;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function MinInteger(X, Y: Integer): Integer;
begin
    if X >= Y then
        Result := Y
    else
        Result := X;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TICMP.ResolveAddr;
var
    Phe : PHostEnt;             // HostEntry buffer for name lookup
begin
    // Convert host address to IP address
    FIPAddress := inet_addr(PAnsiChar(AnsiString(FAddress)));
    if FIPAddress <> LongInt(INADDR_NONE) then
        // Was a numeric dotted address let it in this format
        FHostName := FAddress
    else begin
        // Not a numeric dotted address, try to resolve by name
        Phe := GetHostByName(PAnsiChar(AnsiString(FAddress)));
        if Phe = nil then begin
            FLastError := GetLastError;
            if Assigned(FOnDisplay) then
                FOnDisplay(Self, 'Unable to resolve ' + FAddress);
            Exit;
        end;

        FIPAddress := longint(plongint(Phe^.h_addr_list^)^);
        FHostName  := String(Phe^.h_name);
    end;

    FHostIP       := String(AnsiString(inet_ntoa(TInAddr(FIPAddress))));
    FAddrResolved := TRUE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TICMP.SetAddress(Value : String);
begin
    // Only change if needed (could take a long time)
    if FAddress = Value then
        Exit;
    FAddress      := Value;
    FAddrResolved := FALSE;
//    ResolveAddr;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TICMP.GetErrorString : String;
begin
    case FLastError of
    IP_SUCCESS:               Result := 'No error';
    IP_BUF_TOO_SMALL:         Result := 'Buffer too small';
    IP_DEST_NET_UNREACHABLE:  Result := 'Destination network unreachable';
    IP_DEST_HOST_UNREACHABLE: Result := 'Destination host unreachable';
    IP_DEST_PROT_UNREACHABLE: Result := 'Destination protocol unreachable';
    IP_DEST_PORT_UNREACHABLE: Result := 'Destination port unreachable';
    IP_NO_RESOURCES:          Result := 'No resources';
    IP_BAD_OPTION:            Result := 'Bad option';
    IP_HW_ERROR:              Result := 'Hardware error';
    IP_PACKET_TOO_BIG:        Result := 'Packet too big';
    IP_REQ_TIMED_OUT:         Result := 'Request timed out';
    IP_BAD_REQ:               Result := 'Bad request';
    IP_BAD_ROUTE:             Result := 'Bad route';
    IP_TTL_EXPIRED_TRANSIT:   Result := 'TTL expired in transit';
    IP_TTL_EXPIRED_REASSEM:   Result := 'TTL expired in reassembly';
    IP_PARAM_PROBLEM:         Result := 'Parameter problem';
    IP_SOURCE_QUENCH:         Result := 'Source quench';
    IP_OPTION_TOO_BIG:        Result := 'Option too big';
    IP_BAD_DESTINATION:       Result := 'Bad Destination';
    IP_ADDR_DELETED:          Result := 'Address deleted';
    IP_SPEC_MTU_CHANGE:       Result := 'Spec MTU change';
    IP_MTU_CHANGE:            Result := 'MTU change';
    IP_GENERAL_FAILURE:       Result := 'General failure';
    IP_PENDING:               Result := 'Pending';
    else
        Result := 'ICMP error #' + IntToStr(FLastError);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TICMP.Ping : Integer;
var
  BufferSize:        Integer;
  pReqData, pData:   Pointer;
  pIPE:              PIcmpEchoReply;       // ICMP Echo reply buffer
  IPOpt:             TIPOptionInformation; // IP Options for packet to send
  Msg:               String;
begin
    Result     := 0;
    FLastError := 0;

    if not FAddrResolved then
        ResolveAddr;

    if FIPAddress = LongInt(INADDR_NONE) then begin
        FLastError := IP_BAD_DESTINATION;
        if Assigned(FOnDisplay) then
            FOnDisplay(Self, 'Invalid host address');
        Exit;
    end;

    // Allocate space for data buffer space
    BufferSize := SizeOf(TICMPEchoReply) + FSize;
    GetMem(pReqData, FSize);
    GetMem(pData,    FSize);
    GetMem(pIPE,     BufferSize);

    try
        // Fill data buffer with some data bytes
        FillChar(pReqData^, FSize, $20);
        Msg := 'Pinging from Delphi code written by F. Piette';
        Move(Msg[1], pReqData^, MinInteger(FSize, Length(Msg)));

        pIPE^.Data := pData;
        FillChar(pIPE^, SizeOf(pIPE^), 0);

        if Assigned(FOnEchoRequest) then
            FOnEchoRequest(Self);

        FillChar(IPOpt, SizeOf(IPOpt), 0);
        IPOpt.TTL   := FTTL;
        IPOpt.Flags := FFlags;
        Result      := IcmpSendEcho(hICMP, FIPAddress, pReqData, FSize,
                                    @IPOpt, pIPE, BufferSize, FTimeOut);
        FLastError  := GetLastError;
        FReply      := pIPE^;

        if Assigned(FOnEchoReply) then
            FOnEchoReply(Self, Result);
    finally
        // Free those buffers
        FreeMem(pIPE);
        FreeMem(pData);
        FreeMem(pReqData);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

