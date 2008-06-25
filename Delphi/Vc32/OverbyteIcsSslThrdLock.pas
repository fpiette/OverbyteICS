{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels
Creation:     Nov 01, 2005
Description:  Implementation of OpenSsl thread locking (Windows);
Version:      1.01
EMail:        arno.garrels@gmx.de
Support:      Use the mailing list ics-ssl@elists.org
              Follow "SSL" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2005 by Arno Garrels, Berlin, Germany,
              <arno.garrels@gmx.de> contributed to the ICS-SSL project.

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

How to use:  TSslStaticLock and TSslDynamicLock implement the locking callbacks
             required for OpenSSL to be thread-safe. Currently (v0.98a)
             only static locking is used by OpenSSL, dynamic locking is
             for future OSSL versions, thus TSslDynamicLock is untested so far.
             Simply drop one of the components per application on the form or
             create it from within your foreground thread at runtime. Set
             property Enabled to TRUE before any of the OpenSSL functions are
             called, that's it. Note that setting Enabled to TRUE also loads
             and initializes the OpenSSL libraries. Any multi-threaded OpenSSL
             application MUST provide a single, enabled locking component,
             otherwise random errors in OpenSSL will crash the application.

History:
March 03, 2006 Version 1.01, new property Enabled, OpenSSL is now loaded
          when Enabled is set to TRUE. 

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsSslThrdLock;
{$IFDEF VER80}
    Bomb('This unit require a 32 bit compiler !');
{$ENDIF} 
{$IFNDEF USE_SSL}
    Bomb('Add USE_SSL in the define section in project options');
{$ENDIF}

{$B-}              { Enable partial boolean evaluation   }
{$T-}              { Untyped pointers                    }
{$X+}              { Enable extended syntax              }
{$I OverbyteIcsDefs.inc}
{$IFDEF DELPHI6_UP}
    {$WARN SYMBOL_PLATFORM   OFF}
    {$WARN SYMBOL_LIBRARY    OFF}
    {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
{#$DEFINE NO_DYNLOCK}

interface

uses
    Windows,
    Classes,
    SysUtils,
    OverbyteIcsLIBEAY,
    OverbyteIcsWSocket;

type
    ESslLockException = class(Exception);
    TMutexBuf  = array of THandle;
    TSslStaticLock = class(TComponent)
    protected
        FSslInitialized : Boolean;
        FEnabled    : Boolean;
        procedure   InitializeSsl; virtual;
        procedure   FinalizeSsl; virtual;
        procedure   MutexSetup(var Mutex : THandle);
        procedure   SetEnabled(const Value: Boolean); virtual;
    public
        constructor Create(AOwner: TComponent); override;
        destructor  Destroy; override;
    published
        property    Enabled : Boolean read FEnabled write SetEnabled;
    end;
{$IFNDEF NO_DYNLOCK}
    { Suggested for use with future openssl versions, not yet tested }
    TSslDynamicLock = class(TSslStaticLock)
    protected
        procedure SetEnabled(const Value: Boolean); override;
    end;
{$ENDIF}
    procedure Register;

implementation

var
   MutexBuf : TMutexBuf;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

procedure Register;
begin
    RegisterComponents('FPiette', [TSslStaticLock
                                  {$IFNDEF NO_DYNLOCK}
                                  , TSslDynamicLock
                                  {$ENDIF}]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TSslStaticLock.Create(AOwner: TComponent);
begin
    inherited Create(AOwner);
    FSslInitialized := FALSE;
    FEnabled        := FALSE;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TSslStaticLock.Destroy;
begin
    if Enabled then
        SetEnabled(FALSE);
    FinalizeSsl;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslStaticLock.FinalizeSsl;
begin
    if not FSslInitialized then
        Exit;
    UnloadSsl;
    FSslInitialized := False;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslStaticLock.InitializeSsl;
begin
    if FSslInitialized then
        Exit;
    LoadSsl;
    FSslInitialized := True;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslStaticLock.MutexSetup(var Mutex : THandle);
begin
    Mutex := CreateMutex(nil, False, nil);
    if Mutex = 0 then
        raise ESslLockException.Create('MutexSetup ' +
                                       SysErrorMessage(GetLastError));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure MutexCleanup(var Mutex : THandle);
begin
    if Mutex <> 0 then
    try
        CloseHandle(Mutex);
    except
    end;    
    Mutex := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure MutexLock(Mutex : THandle);
begin
    if WaitForSingleObject(Mutex, Infinite) = WAIT_FAILED then
        raise ESslLockException.Create('MutexLock ' +
                                       SysErrorMessage(GetLastError));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure MutexUnlock(Mutex : THandle);
begin
    if not ReleaseMutex(Mutex) then
        raise ESslLockException.Create('MutexUnlock ' +
                                       SysErrorMessage(GetLastError));
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure StatLockCallback(Mode : Integer; N : Integer;
    const _File : PChar; Line : Integer); cdecl;
begin
    if Mode and Crypto_Lock <> 0 then
        MutexLock(MutexBuf[n])
    else
        MutexUnLock(MutexBuf[n])
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IDCallback : Longword; cdecl;
begin
    Result := GetCurrentThreadID;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslStaticLock.SetEnabled(const Value: Boolean);
var
    I : Integer;
begin
    if Value = FEnabled then
        Exit;
    if not (csDesigning in ComponentState) then
    begin
        InitializeSsl;
        if Value then
        begin
            SetLength(MutexBuf, f_CRYPTO_num_locks);
            try
                for I := 0 to f_CRYPTO_num_locks -1 do
                    MutexSetup(MutexBuf[I]);
            except
                on E : Exception do begin
                    for I := 0 to f_CRYPTO_num_locks -1 do
                        MutexCleanup(MutexBuf[I]);
                    SetLength(MutexBuf, 0);
                    raise;
                end;
            end;
            f_CRYPTO_set_id_callback(IDCallback);
            f_CRYPTO_set_locking_callback(StatLockCallback);
        end
        else begin
            f_CRYPTO_set_locking_callback(nil);
            f_CRYPTO_set_id_callback(nil);
            for I := 0 to f_CRYPTO_num_locks -1 do
                MutexCleanup(MutexBuf[I]);
            SetLength(MutexBuf, 0);
        end;
    end;
    FEnabled := Value;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF NO_DYNLOCK}
function DynCreateCallBack(const _file : PChar;
                           Line: Integer): PCRYPTO_dynlock_value; cdecl;
begin
    New(Result);
    Result^.Mutex := CreateMutex(nil, False, nil);
    if Result^.Mutex = 0 then
    begin
        Dispose(Result);
        Result := nil;
    end;    
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure DynDestroyCallBack(L : PCRYPTO_dynlock_value; _File : PChar;
    Line: Integer); cdecl;
begin
    if Assigned(L) then
    begin
        MutexCleanUp(L^.Mutex);
        Dispose(L);
        //L := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure DynLockCallback(Mode : Integer; L : PCRYPTO_dynlock_value;
    _File : PChar; Line: Integer); cdecl;
begin
    if Assigned(L) then
    begin
        if Mode and CRYPTO_LOCK <> 0 then
            MutexLock(L^.Mutex)
        else
            MutexUnlock(L^.Mutex);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TSslDynamicLock.SetEnabled(const Value: Boolean);
var
    OldValue : Boolean;
begin
    OldValue := FEnabled;
    inherited SetEnabled(Value);
    if OldValue = FEnabled then
        Exit;
    if not (csDesigning in ComponentState) then begin
        if Value then begin
            f_CRYPTO_set_dynlock_create_callback(DynCreateCallBack);
            f_CRYPTO_set_dynlock_lock_callback(DynLockCallback);
            f_CRYPTO_set_dynlock_destroy_callback(DynDestroyCallBack);
        end
        else begin
            f_CRYPTO_set_dynlock_create_callback(nil);
            f_CRYPTO_set_dynlock_lock_callback(nil);
            f_CRYPTO_set_dynlock_destroy_callback(nil);
        end;

    end;
    FEnabled := Value;
end;

{$ENDIF}
{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
end.
