unit OverbyteIcsReg;

{$I OverbyteIcsDefs.inc}
{$IFDEF USE_SSL}
    {$I OverbyteIcsSslDefs.inc}
{$ENDIF}

interface

uses
    SysUtils, Classes, Controls,
    OverbyteIcsWSocket,
    OverbyteIcsDnsQuery,
    OverbyteIcsEmulVT,
    OverbyteIcsMimeDec,
    OverbyteIcsMultiProgressBar,
    OverbyteIcsTnCnx, OverbyteIcsTnEmulVT, OverbyteIcsTnScript,
    OverbyteIcsFtpCli, OverbyteIcsFtpSrv, OverbyteIcsMultipartFtpDownloader,
    OverbyteIcsHttpProt, OverbyteIcsHttpSrv, OverbyteIcsMultipartHttpDownloader,
    OverbyteIcsHttpAppServer,
    OverbyteIcsTimeList,
    OverbyteIcsCharsetComboBox,
    OverbyteIcsPop3Prot,
    OverbyteIcsSmtpProt,
    OverbyteIcsNntpCli,
    OverbyteIcsFingCli,
  {$IFNDEF BCB}
    OverbyteIcsWSocketTS,
  {$ENDIF}
    OverbyteIcsPing
  {$IFDEF USE_SSL}
    , OverbyteIcsSslSessionCache
    , OverbyteIcsSslThrdLock
  {$ENDIF}
  {$IFDEF VCL}
    , OverbyteIcsLogger
  {$ENDIF}
  {$IFDEF WIN32}
    , OverbyteIcsWSocketE
    , OverbyteIcsWSocketS
  {$ENDIF}
    ;

procedure Register;

implementation

uses
{$IFDEF WIN32}
  {$IFDEF COMPILER10_UP}
    Windows,
    ToolsApi,
  {$ENDIF}
  {$IFDEF COMPILER6_UP}
    DesignIntf, DesignEditors;
  {$ELSE}
    DsgnIntf;
  {$ENDIF}
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Register;
begin
{$IFDEF COMPILER16_UP}
    StartClassGroup(TControl);
    ActivateClassGroup(TControl);
{$ENDIF}
    RegisterComponents('Overbyte ICS', [
      TWSocket, TWSocketServer,
      TDnsQuery, TEmulVT, TFingerCli, TPing,
      TMimeDecode, TMimeDecodeEx, TMimeDecodeW,
      TMultiProgressBar,
      TTimeList,
      THttpAppSrv,
      TTnCnx, TTnEmulVT, TTnScript,
      TFtpClient, TFtpServer, TMultipartFtpDownloader,
      THttpCli, THttpServer, TMultipartHttpDownloader,
      TPop3Cli, TSyncPop3Cli,
      TSmtpCli, TSyncSmtpCli, THtmlSmtpCli,
      TNntpCli, THtmlNntpCli,
  {$IFNDEF BCB}
      TWSocketThrdServer,
  {$ENDIF}
      TIcsCharsetComboBox
  {$IFDEF VCL}
      ,TIcsLogger
  {$ENDIF}
    ]);

{$IFDEF COMPILER16_UP}
    { For now disable everything for non VCL }
    GroupDescendentsWith(TWSocket, TControl);
    GroupDescendentsWith(TWSocketServer, TControl);
    GroupDescendentsWith(TDnsQuery, TControl);
    GroupDescendentsWith(TFingerCli, TControl);
    GroupDescendentsWith(TPing, TControl);
    GroupDescendentsWith(TMimeDecode, TControl);
    GroupDescendentsWith(TMimeDecodeEx, TControl);
    GroupDescendentsWith(TMimeDecodeW, TControl);
    GroupDescendentsWith(TTimeList, TControl);
    GroupDescendentsWith(THttpAppSrv, TControl);
    GroupDescendentsWith(TTnCnx, TControl);
    GroupDescendentsWith(TFtpClient, TControl);
    GroupDescendentsWith(TFtpServer, TControl);
    GroupDescendentsWith(TMultipartFtpDownloader, TControl);
    GroupDescendentsWith(THttpCli, TControl);
    GroupDescendentsWith(THttpServer, TControl);
    GroupDescendentsWith(TMultipartHttpDownloader, TControl);
    GroupDescendentsWith(TPop3Cli, TControl);
    GroupDescendentsWith(TSyncPop3Cli, TControl);
    GroupDescendentsWith(TSmtpCli, TControl);
    GroupDescendentsWith(TSyncSmtpCli, TControl);
    GroupDescendentsWith(THtmlSmtpCli, TControl);
    GroupDescendentsWith(TNntpCli, TControl);
    GroupDescendentsWith(THtmlNntpCli, TControl);
  {$IFNDEF BCB}
    GroupDescendentsWith(TWSocketThrdServer, TControl);
  {$ENDIF}
  {$IFDEF VCL}
    GroupDescendentsWith(TIcsLogger, TControl);
  {$ENDIF}
{$ENDIF}

{$IFDEF USE_SSL}
    RegisterComponents('Overbyte ICS SSL', [
      TSslWSocket, TSslWSocketServer,
      TSslContext,
      TSslFtpClient, TSslFtpServer,
      TSslHttpCli, TSslHttpServer,
      TSslPop3Cli,
      TSslSmtpCli,
      TSslNntpCli,
      TSslAvlSessionCache,
  {$IFNDEF BCB}
      TSslWSocketThrdServer,
  {$ENDIF}
      TSslStaticLock
    {$IFNDEF NO_DYNLOCK}
      ,TSslDynamicLock
    {$ENDIF}
    {$IFNDEF OPENSSL_NO_ENGINE}
      ,TSslEngine
    {$ENDIF}
    ]);
  {$IFDEF COMPILER16_UP}
    { For now disable everything for non VCL }
    GroupDescendentsWith(TSslWSocket, TControl);
    GroupDescendentsWith(TSslWSocketServer, TControl);
    GroupDescendentsWith(TSslContext, TControl);
    GroupDescendentsWith(TSslFtpClient, TControl);
    GroupDescendentsWith(TSslFtpServer, TControl);
    GroupDescendentsWith(TSslHttpCli, TControl);
    GroupDescendentsWith(TSslHttpServer, TControl);
    GroupDescendentsWith(TSslPop3Cli, TControl);
    GroupDescendentsWith(TSslSmtpCli, TControl);
    GroupDescendentsWith(TSslNntpCli, TControl);
    GroupDescendentsWith(TSslAvlSessionCache, TControl);
  {$IFNDEF BCB}
    GroupDescendentsWith(TSslWSocketThrdServer, TControl);
  {$ENDIF}
    GroupDescendentsWith(TSslStaticLock, TControl);
  {$IFNDEF NO_DYNLOCK}
    GroupDescendentsWith(TSslDynamicLock, TControl);
  {$ENDIF}
  {$IFNDEF OPENSSL_NO_ENGINE}
    GroupDescendentsWith(TSslEngine, TControl);
  {$ENDIF}
  {$ENDIF}
{$ENDIF}

    RegisterPropertyEditor(TypeInfo(AnsiString), TWSocket, 'LineEnd',
      TWSocketLineEndProperty);
    
{$IFDEF COMPILER10_UP}
    ForceDemandLoadState(dlDisable); // Required to show our product icon on splash screen
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

{$IFDEF COMPILER10_UP}
{$R OverbyteIcsProductIcon.res}
const
{$IFDEF COMPILER14_UP}
    sIcsSplashImg       = 'ICSPRODUCTICONBLACK';
{$ELSE}
    {$IFDEF COMPILER10}
        sIcsSplashImg   = 'ICSPRODUCTICONBLACK';
    {$ELSE}
        sIcsSplashImg   = 'ICSPRODUCTICON';
    {$ENDIF}
{$ENDIF}
    sIcsLongProductName = 'Internet Component Suite V7';
    sIcsFreeware        = 'Freeware';
    sIcsDescription     = sIcsLongProductName + #13#10 +
                          //'Copyright (C) 1996-2011 by François PIETTE'+ #13#10 +
                          // Actually there's source included with different
                          // copyright, so either all or none should be mentioned
                          // here.
                          'http://www.overbyte.be/' + #13#10 +
                          'svn://svn.overbyte.be/ics/trunk' + #13#10 +
                          'http://svn.overbyte.be:8443/svn/ics/trunk' + #13#10 +
                          'User and password = "ics"';

var
    AboutBoxServices: IOTAAboutBoxServices = nil;
    AboutBoxIndex: Integer = -1;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure PutIcsIconOnSplashScreen;
var
    hImage: HBITMAP;
begin
    if Assigned(SplashScreenServices) then begin
        hImage := LoadBitmap(FindResourceHInstance(HInstance), sIcsSplashImg);
        SplashScreenServices.AddPluginBitmap(sIcsLongProductName, hImage,
                                             FALSE, sIcsFreeware);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure RegisterAboutBox;
begin
    if Supports(BorlandIDEServices, IOTAAboutBoxServices, AboutBoxServices) then begin
        AboutBoxIndex := AboutBoxServices.AddPluginInfo(sIcsLongProductName,
          sIcsDescription, 0, FALSE, sIcsFreeware);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure UnregisterAboutBox;
begin
    if (AboutBoxIndex <> -1) and Assigned(AboutBoxServices) then begin
        AboutBoxServices.RemovePluginInfo(AboutBoxIndex);
        AboutBoxIndex := -1;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

initialization
    PutIcsIconOnSplashScreen;
    RegisterAboutBox;

finalization
    UnregisterAboutBox;
{$ENDIF COMPILER10_UP}

end.

