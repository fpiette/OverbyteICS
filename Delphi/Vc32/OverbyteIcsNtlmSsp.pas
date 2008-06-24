{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *
Author:       Arno Garrels <arno.garrels@gmx.de>
Description:  Server-side NTLM, validation of user credentials using Windows SSPI.
Creation:     Sep 04, 2006
Version:      1.01
Legal issues: Copyright (C) 2005 by Arno Garrels, Berlin, Germany,
              contact: <arno.garrels@gmx.de>

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

              Development of this unit has been sponsored by Fastream
              Technologies (www.fastream.com) and donated to ICS, thanks.

              **If you compile with BCB personality define SECURITY_WIN32
             in the project options**.
History:
Sep 11, 2006 V1.01 A. Garrels added func ValidateUserCredentials() which allows
             validation of user credentials locally. Also added func
             ImpersonateContext as well as RevertContext. ImpersonateContext
             will make the calling thread run in the security context of the
             authenticated user.

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsNtlmSsp;

{$I OverbyteIcsDefs.inc}
{$B-}                                 { Enable partial Boolean evaluation   }
{$T-}                                 { Untyped Pointers                    }
{$X+}                                 { Enable extended syntax              }
{$H+}                                 { Use long Strings                    }

{#$DEFINE UNICODE}
{#$DEFINE DEBUG_EXCEPTIONS}

interface

uses
    Windows, SysUtils, OverbyteIcsSspi, OverbyteIcsMimeUtils,
    OverbyteIcsNtlmMsgs;

const
    {$EXTERNALSYM WC_NO_BEST_FIT_CHARS}
    WC_NO_BEST_FIT_CHARS = $00000400; // do not use best fit chars

type
    UCS2String = String;
    PNTLM_Message3  = ^TNTLM_Message3;
    PNTLM_Message1  = ^TNTLM_Message1;
    TNtlmState  = (lsNone, lsInAuth, lsDoneOK, lsDoneErr);
    TNtlmSessionBeforeValidate = procedure(Sender: TObject; var Allow: Boolean) of object;
    TNtlmAuthSession = class(TObject)
    private
        FPSFT              : PSecurityFunctionTable;
        FHCred             : TCredHandle;
        FHCtx              : TSecHandle;
        FHaveCredHandle    : Boolean;
        FHaveCtxHandle     : Boolean;
        FState             : TNtlmState;
        FUsername          : String;
        FDomain            : String;
        FHost              : String;
        FNtlmMessage       : String;
        FAuthError         : Integer;
        FOnBeforeValidate  : TNtlmSessionBeforeValidate;
   protected
        procedure   NtlmMsg3GetAttributes(const NtlmMsg3: String);
        function    NtlmMsgGetType(const NtlmMsg: String): Integer;
        function    NtlmAccept(const InBuffer: String): String;
        function    NtlmErrorDesc(ErrCode: Integer): String;
   public
        constructor Create;
        destructor  Destroy; override;
        function    AuthErrorDesc: String;
        function    GetUserFromContext: {$IFNDEF UNICODE} String {$ELSE} WideString {$ENDIF};
        function    GetAuthorityFromContext: {$IFNDEF UNICODE} String {$ELSE} WideString {$ENDIF};
        function    ProcessNtlmMsg(const InBuffer: String): Boolean;
        function    ValidateUserCredentials(const AUser, APassword, ADomain: String;
                                            CleanUpSession: Boolean): Boolean;
        function    ImpersonateContext: Boolean;
        function    RevertContext: Boolean;
        procedure   CleanUpLogonSession;
        property    NtlmMessage : String read FNtlmMessage write FNtlmMessage;
        property    Username : String read FUsername;
        property    Domain : String read FDomain;
        property    Host : String read FHost;
        property    HCread : TCredHandle read FHCred;
        property    HCtx  : TSecHandle  read  FHCtx;
        property    State : TNtlmState read FState;
        property    OnBeforeValidate: TNtlmSessionBeforeValidate read FOnBeforeValidate write FOnBeforeValidate;
    end;

//procedure LoadSecPackage;
//procedure UnloadSecPackage;

implementation

var
    SecPackageLock      : TRtlCriticalSection;
    cbMaxMessage        : Cardinal;
    PSFT                : PSecurityFunctionTable;
    SecPackageHandle    : THandle;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Get rid of some ntdll.DbgBreakPoints M$ forgot to remove from their DLLs  }
{ popping up the CPU window. Author: Matze, no e-mail available.            }
procedure PatchINT3;
var
    NOP: Byte;
    NTDLL: THandle;
    BytesWritten: DWORD;
    Address: Pointer;
begin
    if Win32Platform <> VER_PLATFORM_WIN32_NT then Exit;
    NTDLL := GetModuleHandle('NTDLL.DLL');
    if NTDLL = 0 then Exit;
    Address := GetProcAddress(NTDLL, 'DbgBreakPoint');
    if Address = nil then Exit;
    try
        if Char(Address^) <> #$CC then Exit;
        NOP := $90;
        if WriteProcessMemory(GetCurrentProcess, Address, @NOP, 1, BytesWritten) and
          (BytesWritten = 1) then
            FlushInstructionCache(GetCurrentProcess, Address, 1);
    except
        //Do not panic if you see an EAccessViolation here, it is perfectly harmless!
        on EAccessViolation do ;
        else
            raise;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure LoadSecPackage;
type
    INIT_SECURITY_ENTRYPOINT_FN = function : PSecurityFunctionTable;
var
    pInit   : INIT_SECURITY_ENTRYPOINT_FN;
    pkgInfo : PSecPkgInfo;
    SS      : TSecurityStatus;
begin
    EnterCriticalSection(SecPackageLock);
    try
        if SecPackageHandle <> 0 then
            Exit;

        SecPackageHandle := LoadLibrary('security.dll');     // Winnt up
        if SecPackageHandle = 0 then
            SecPackageHandle := LoadLibrary('secur32.dll');  // Win9x
        if SecPackageHandle <> 0 then
        try
            pInit := GetProcAddress(SecPackageHandle, SECURITY_ENTRYPOINT);

            if not Assigned(pInit) then
                raise Exception.Create('Couldn''t init security package');

            PSFT := pInit;
{$IFNDEF UNICODE}
            SS := PSFT^.QuerySecurityPackageInfoA(NTLMSP_NAME_A, pkgInfo);
{$ELSE}
            SS := PSFT^.QuerySecurityPackageInfoW(PWideChar(NTLMSP_NAME),
                                                  pkgInfo);
{$ENDIF}
            if SS < 0 then
                raise Exception.CreateFmt('Couldn''t find package info for ' +
                                          'NTLM, error 0x%x', [SS]);

            cbMaxMessage := pkgInfo^.cbMaxToken;

            PSFT^.FreeContextBuffer(pkgInfo);
        except
            if SecPackageHandle <> 0 then
                FreeLibrary(SecPackageHandle);
            raise
        end
    finally
        LeaveCriticalSection(SecPackageLock);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure UnloadSecPackage;
begin
    if SecPackageHandle = 0 then
        Exit;
    EnterCriticalSection(SecPackageLock);
    try
        FreeLibrary(SecPackageHandle);
        SecPackageHandle := 0;
    finally
        LeaveCriticalSection(SecPackageLock);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TNtlmAuthSession.Create;
begin
    inherited Create;
    FPSFT := PSFT;
    CleanUpLogonSession;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TNtlmAuthSession.Destroy;
begin
    CleanUpLogonSession;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TNtlmAuthSession.ValidateUserCredentials(
  const AUser, APassword, ADomain: String;
  CleanUpSession: Boolean): Boolean;
var
    NtlmMsg2Info : TNTLM_Msg2_Info;
begin
    Result := ProcessNtlmMsg(NtlmGetMessage1('', ADomain));
    if not (FState in [lsDoneOk, lsDoneErr]) then begin
        NtlmMsg2Info := NtlmGetMessage2(FNtlmMessage);
        Result := ProcessNtlmMsg(NtlmGetMessage3(ADomain, '', AUser,
                                                 APassword,
                                                 NtlmMsg2Info.Challenge));
        if CleanUpSession then
            CleanUpLogonSession;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNtlmAuthSession.CleanUpLogonSession;
begin
    if FHaveCtxHandle then
        FPSFT^.DeleteSecurityContext(@FHCtx);
    if FHaveCredHandle then
        FPSFT^.FreeCredentialHandle(@FHCred);
    FHaveCredHandle := FALSE;
    FHaveCtxHandle  := FALSE;
    FUserName       := '';
    FDomain         := '';
    FHost           := '';
    FNtlmMessage    := '';
    FAuthError      := 0;
    FState          := lsNone;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{function StringToUCS2(const S: String): UCS2String;
begin
    if Length(S) = 0 then
        Result := ''
    else begin
        SetLength(Result, Length(S) * 2);
        if MultiByteToWideChar(CP_ACP, MB_PRECOMPOSED, @S[1], Length(S),
                               @Result[1], Length(Result)) = 0 then
            RaiseLastOsError;
    end;
end;}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function UCS2ToString(const S: UCS2String): String;
begin
    if Length(S) < 2 then
        Result := ''
    else begin
        SetLength(Result, Length(S));
        if WideCharToMultiByte(CP_ACP, 0, @S[1], Length(S),
                               @Result[1], Length(Result),
                               nil, nil) = 0 then
            RaiseLastOsError
        else
            SetLength(Result, StrLen(PChar(Result)));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{function UCS2ToWideString(const S: UCS2String): WideString;
begin
    if Length(S) < 2 then
        Result := ''
    else begin
        SetLength(Result, Length(S) div 2);
        Move(S[1], Result[1], Length(S));
    end;
end;}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{function WideStringToString(const WS: WideString): String;
begin
    if Length(WS) = 0 then
        Result := ''
    else begin
        SetLength(Result, Length(WS));
        if WideCharToMultiByte(CP_ACP, 0,
                               //WC_COMPOSITECHECK or WC_DISCARDNS or
                               //WC_SEPCHARS or WC_DEFAULTCHAR,
                               @Ws[1], Length(WS), @Result[1], Length(Result),
                               nil, nil) = 0 then
            RaiseLastOsError;
    end;
end;}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ The name of a server or domain that authenticated the connection.         }
function TNtlmAuthSession.GetAuthorityFromContext: {$IFNDEF UNICODE} String {$ELSE} WideString {$ENDIF};
var
    CtxAutority: TSecPkgContextAuthority;
    Sec: Integer;
begin
    if not FHaveCtxHandle then
        Exit;
{$IFNDEF UNICODE}
    Sec := FPSFT^.QueryContextAttributesA(@FHCtx, SECPKG_ATTR_AUTHORITY, @CtxAutority);
{$ELSE}
    Sec := FPSFT^.QueryContextAttributesW(@FHCtx, SECPKG_ATTR_AUTHORITY, @CtxAutority);
{$ENDIF}
    if Sec = 0 then
        Result := CtxAutority.sAuthorityName;
    FPSFT^.FreeContextBuffer(@CtxAutority);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TNtlmAuthSession.GetUserFromContext: {$IFNDEF UNICODE} String {$ELSE} WideString {$ENDIF};
var
    ContextNames : TSecPkgContextNames;
    Sec: Integer;
begin
    if not FHaveCtxHandle then
        Exit;
{$IFNDEF UNICODE}
    Sec := FPSFT^.QueryContextAttributesA(@FHCtx, SECPKG_ATTR_NAMES, @ContextNames);
{$ELSE}
    Sec := FPSFT^.QueryContextAttributesW(@FHCtx, SECPKG_ATTR_NAMES, @ContextNames);
{$ENDIF}
    if Sec = 0 then
        Result := ContextNames.sUserName;
    FPSFT^.FreeContextBuffer(@ContextNames);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TNtlmAuthSession.NtlmMsg3GetAttributes(const NtlmMsg3: String);
var
    PMsg : PNTLM_Message3;
begin
    if Length(NtlmMsg3) < SizeOf(TNTLM_Message3) then
        Exit;
    PMsg := @NtlmMsg3[1];
    if PMsg^.User.Length <= 256 * 2 then begin // ? Max. length Win logon names = 104
        FUsername := Copy(NtlmMsg3, PMsg^.User.Offset + 1, PMsg^.User.Length);
        if Pmsg^.Flags and Flags_Negotiate_Unicode <> 0 then
            FUsername := UCS2ToString(FUsername);
    end;
    if PMsg^.Host.Length <= 256 * 2 then begin// Max. host name length ?
        FHost := Copy(NtlmMsg3, PMsg^.Host.Offset + 1, PMsg^.Host.Length);
        if Pmsg^.Flags and Flags_Negotiate_Unicode <> 0 then
            FHost := UCS2ToString(FHost);
    end;
    if PMsg^.Domain.Length <= 256 * 2 then begin // Max. domain name length ?
        FDomain := Copy(NtlmMsg3, PMsg^.Domain.Offset + 1, PMsg^.Domain.Length);
        if Pmsg^.Flags and Flags_Negotiate_Unicode <> 0 then
            FDomain := UCS2ToString(FDomain);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TNtlmAuthSession.NtlmMsgGetType(const NtlmMsg: String): Integer;
var
    PMsg : PNTLM_Message1;
begin
    Result := -1;
    if Length(NtlmMsg) < SizeOf(TNTLM_Message1) then
        Exit;
    PMsg   := @NtlmMsg[1];
    Result := PMsg^.MsgType;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TNtlmAuthSession.ProcessNtlmMsg(const InBuffer: String): Boolean;
begin
    Result := False;
    if Length(InBuffer) = 0 then begin
        CleanupLogonSession;
        Exit;
    end;
    FNtlmMessage := NtlmAccept(InBuffer);
    Result       := FState = lsDoneOk;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TNtlmAuthSession.NtlmAccept(const InBuffer: String): String;
var
    Sec                 : TSecurityStatus;
    Lifetime            : LARGE_INTEGER;
    OutBuffDesc         : TSecBufferDesc;
    InBuffDesc          : TSecBufferDesc;
    InSecBuff           : TSecBuffer;
    OutSecBuff          : TSecBuffer;
    ContextAttr         : Cardinal;
    pHCtx               : PCtxtHandle;
    InBufDec            : String;
    Allow               : Boolean;
    MsgType             : Integer;
begin
    try
        InBufDec := Base64Decode(InBuffer);
        if Length(InBufDec) = 0 then begin
            FAuthError := Integer(SEC_E_INVALID_TOKEN);
            {$IFDEF DEBUG_EXCEPTIONS}
                raise Exception.Create('InBuffer empty');
            {$ELSE}
                FState := lsDoneErr;
                Exit;
            {$ENDIF}
        end;
        MsgType := NtlmMsgGetType(InBufDec);
        if not ((MsgType = 3) and (FState = lsInAuth)) then
            CleanupLogonSession;

        if FState = lsNone then begin
{$IFNDEF UNICODE}
            Sec := FPSFT^.AcquireCredentialsHandleA(nil,
                                                   PChar(NTLMSP_NAME_A),
                                                   SECPKG_CRED_INBOUND,
                                                   nil,
                                                   nil,
                                                   nil,
                                                   nil,
                                                   FHCred,
                                                   Lifetime);
{$ELSE}
            Sec := FPSFT^.AcquireCredentialsHandleW(nil,
                                                   PWideChar(NTLMSP_NAME),
                                                   SECPKG_CRED_INBOUND,
                                                   nil,
                                                   nil,
                                                   nil,
                                                   nil,
                                                   FHCred,
                                                   Lifetime);
{$ENDIF}
            if Sec < 0 then begin
                FAuthError := Sec;
            {$IFDEF DEBUG_EXCEPTIONS}
                raise Exception.CreateFmt('AcquireCredentials failed 0x%x', [Sec]);
            {$ELSE}
                FState := lsDoneErr;
                Exit;
            {$ENDIF}
            end;
            FHaveCredHandle := TRUE
        end;

        // prepare output buffer
        OutBuffDesc.ulVersion := 0;
        OutBuffDesc.cBuffers  := 1;
        OutBuffDesc.pBuffers  := @OutSecBuff;

        SetLength(Result, cbMaxMessage);
        OutSecBuff.cbBuffer   := Length(Result);
        OutSecBuff.BufferType := SECBUFFER_TOKEN;
        OutSecBuff.pvBuffer   := @Result[1];

        // prepare input buffer
        InBuffDesc.ulVersion := 0;
        InBuffDesc.cBuffers  := 1;
        InBuffDesc.pBuffers  := @InSecBuff;

        InSecBuff.cbBuffer   := Length(InBufDec);
        InSecBuff.BufferType := SECBUFFER_TOKEN;
        InSecBuff.pvBuffer   := @InBufDec[1];

        if FState = lsNone then
            pHCtx := nil
        else begin
            pHCtx := @FHCtx;
            { We received NTLMMsg3 }
            NtlmMsg3GetAttributes(InBufDec);
            Allow := TRUE;
            if Assigned(FOnBeforeValidate) then
                FOnBeforeValidate(Self, Allow);
            if not Allow then begin
            {$IFDEF DEBUG_EXCEPTIONS}
                raise Exception.Create('User canceled the session');
            {$ELSE}
                FState := lsDoneErr;
                Exit;
            {$ENDIF}
            end;
        end;
        Sec := FPSFT^.AcceptSecurityContext(@FHCred,
                                           pHCtx,
                                           @InBuffDesc,
                                           ASC_REQ_SEQUENCE_DETECT{ASC_REQ_DELEGATE}, // context requirements
                                           SECURITY_NATIVE_DREP,
                                           @FHCtx,
                                           @OutBuffDesc,
                                           ContextAttr,
                                           Lifetime);
        if Sec < 0 then begin
            FAuthError := Sec;
        {$IFDEF DEBUG_EXCEPTIONS}
            raise Exception.CreateFmt('Init context failed: 0x%x', [Sec]);
        {$ELSE}
            Result := '';
            FState := lsDoneErr;
            Exit;
        {$ENDIF}
        end;

        FHaveCtxHandle := TRUE;
        if(Sec = SEC_I_COMPLETE_NEEDED) or
          (Sec = SEC_I_COMPLETE_AND_CONTINUE) then begin
            if Assigned(FPSFT^.CompleteAuthToken) then begin
                Sec := FPSFT^.CompleteAuthToken(@FHCtx, @OutBuffDesc);
                if Sec < 0 then begin
                    FAuthError := Sec;
                {$IFDEF DEBUG_EXCEPTIONS}
                    raise Exception.CreateFmt('Complete failed: 0x%x', [Sec]);
                {$ELSE}
                    Result := '';
                    FState := lsDoneErr;
                    Exit;
                {$ENDIF}
                end;
            end
            else begin
            {$IFDEF DEBUG_EXCEPTIONS}
                raise Exception.Create('CompleteAuthToken not supported.');
            {$ELSE}
                Result := '';
                FState := lsDoneErr;
                Exit;
            {$ENDIF}
            end;
        end;

        if (Sec <> SEC_I_CONTINUE_NEEDED) and
                   (Sec <> SEC_I_COMPLETE_AND_CONTINUE) then
            FState := lsDoneOK
        else
            if FState = lsNone then
                FState := lsInAuth;

        if FState = lsInAuth then begin
            SetLength(Result, OutSecBuff.cbBuffer);
            Result := Base64Encode(Result);
        end;
    except
        FState := lsDoneErr;
        Result := '';
    end
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TNtlmAuthSession.AuthErrorDesc: String;
begin
    Result := NtlmErrorDesc(FAuthError) + 'Error: 0x' + IntToHex(FAuthError, 8);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TNtlmAuthSession.NtlmErrorDesc(ErrCode: Integer): String;
begin
    case Cardinal(ErrCode) of
        SEC_E_INCOMPLETE_MESSAGE : Result := 'The supplied message is incomplete.  The signature was not verified.';
        SEC_E_INSUFFICIENT_MEMORY: Result := 'Not enough memory is available to complete this request';
        SEC_E_INTERNAL_ERROR : Result := 'The Local Security Authority cannot be contacted';
        SEC_E_INVALID_HANDLE : Result := 'The handle specified is invalid';
        SEC_E_INVALID_TOKEN : Result := 'The token supplied to the function is invalid';
        SEC_E_LOGON_DENIED : Result := 'The logon attempt failed';
        SEC_E_NO_AUTHENTICATING_AUTHORITY : Result := 'No authority could be contacted for authentication';
        SEC_E_NO_CREDENTIALS : Result := 'No credentials are available in the security package';
        SEC_E_OK : Result := 'Call completed successfully';
        SEC_E_SECURITY_QOS_FAILED : Result := 'The security context could not be established due to a failure in the requested quality of service (e.g. mutual authentication or delegation).';
        SEC_E_UNSUPPORTED_FUNCTION : Result := 'The function requested is not supported';
        else
            Result := 'Unknow error';
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TNtlmAuthSession.ImpersonateContext: Boolean;
begin
    if FHaveCtxHandle then
        Result := FPSFT^.ImpersonateSecurityContext(@FHCtx) = 0
    else
        Result := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TNtlmAuthSession.RevertContext: Boolean;
begin
    if FHaveCtxHandle then
        Result := FPSFT^.RevertSecurityContext(@FHCtx) = 0
    else
        Result := FALSE;    
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
initialization
    InitializeCriticalSection(SecPackageLock);
    SecPackageHandle := 0;
    LoadSecPackage;
    if DebugHook <> 0 then
        PatchINT3;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
finalization
    UnloadSecPackage;
    DeleteCriticalSection(SecPackageLock);


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}    
end.
