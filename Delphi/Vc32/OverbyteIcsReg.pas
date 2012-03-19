unit OverbyteIcsReg;
{$DEFINE ICS_COMMON}
{$DEFINE VCL}
{$IFDEF FMX}
  {$UNDEF FMX}
{$ENDIF}

{ Feb 15, 2012 Angus - added OverbyteIcsMimeUtils }
{ Mar 15, 2012 Angus - added OverbyteIcsCookies }

{$I OverbyteIcsDefs.inc}
{$IFDEF USE_SSL}
    {$I OverbyteIcsSslDefs.inc}
{$ENDIF}

{$IFDEF VCL}
  {$DEFINE VCL_OR_FMX}
{$ELSE}
  {$IFDEF FMX}
    {$DEFINE VCL_OR_FMX}
  {$ENDIF}
{$ENDIF}

interface

uses
  {$IFDEF VCL}
    Controls,
    OverbyteIcsWndControl,
    OverbyteIcsWSocket,
    OverbyteIcsDnsQuery,
    OverbyteIcsFtpCli,
    OverbyteIcsFtpSrv,
    OverbyteIcsMultipartFtpDownloader,
    OverbyteIcsHttpProt,
    OverbyteIcsHttpSrv,
    OverbyteIcsMultipartHttpDownloader,
    OverbyteIcsHttpAppServer,
    OverbyteIcsCharsetComboBox,
    OverbyteIcsPop3Prot,
    OverbyteIcsSmtpProt,
    OverbyteIcsNntpCli,
    OverbyteIcsFingCli,
    OverbyteIcsPing,
    {$IFDEF USE_SSL}
      OverbyteIcsSslSessionCache,
      OverbyteIcsSslThrdLock,
    {$ENDIF}
    OverbyteIcsWSocketE,
    OverbyteIcsWSocketS,

    // VCL only
    OverbyteIcsMultiProgressBar,
    OverbyteIcsEmulVT, OverbyteIcsTnCnx, OverbyteIcsTnEmulVT, OverbyteIcsTnScript,
    {$IFNDEF BCB}
      OverbyteIcsWSocketTS,
    {$ENDIF}
  {$ENDIF VCL}
  {$IFDEF ICS_COMMON}
    OverbyteIcsMimeDec,
    OverbyteIcsMimeUtils,
    OverbyteIcsTimeList,
    OverbyteIcsLogger,
    OverbyteIcsCookies,
  {$ENDIF}
    SysUtils, Classes;

procedure Register;

implementation

uses
{$IFDEF MSWINDOWS}
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
  {$IFDEF VCL}
    GroupDescendentsWith(TIcsWndControl, TControl);
    GroupDescendentsWith(TDnsQuery, TControl);
    GroupDescendentsWith(TFingerCli, TControl);
    GroupDescendentsWith(THttpAppSrv, TControl);
    GroupDescendentsWith(TFtpClient, TControl);
    GroupDescendentsWith(TPop3Cli, TControl);
    GroupDescendentsWith(TSmtpCli, TControl);
    GroupDescendentsWith(TMimeDecode, TControl);
    GroupDescendentsWith(TMimeDecodeEx, TControl);
    GroupDescendentsWith(TMimeTypesList, TControl);
    GroupDescendentsWith(TTimeList, TControl);
    GroupDescendentsWith(TIcsLogger, TControl);
    GroupDescendentsWith(TCookies, TControl);
  {$ENDIF VCL}
{$ENDIF COMPILER16_UP}

{$IFDEF VCL_OR_FMX}
    RegisterComponents('Overbyte ICS', [
      TWSocket, TWSocketServer,
      THttpCli, THttpServer, THttpAppSrv, TMultipartHttpDownloader,
      TFtpClient, TFtpServer, TMultipartFtpDownloader,
      TSmtpCli, TSyncSmtpCli, THtmlSmtpCli,
      TPop3Cli, TSyncPop3Cli,
      TNntpCli, THtmlNntpCli,
      TDnsQuery, TFingerCli, TPing,
      TIcsCharsetComboBox
    ]);
{$ENDIF}
{$IFDEF VCL}
    RegisterComponents('Overbyte ICS', [
      { Not yet ported to FMX }
      TEmulVT, TTnCnx, TTnEmulVT, TTnScript,
      {$IFNDEF BCB}
        TWSocketThrdServer,
      {$ENDIF}
      TMultiProgressBar
    ]);
{$ENDIF VCL}
{$IFDEF ICS_COMMON}
    RegisterComponents('Overbyte ICS', [
      { Components neither depending on the FMX nor on the VCL package }
      TMimeDecode, TMimeDecodeEx, TMimeDecodeW, TMimeTypesList, TTimeList, TIcsLogger, TIcsCookies
    ]);
{$ENDIF}

{$IFDEF USE_SSL}
  {$IFDEF COMPILER16_UP}
  {$IFDEF VCL}
    GroupDescendentsWith(TSslBaseComponent, TControl);
    GroupDescendentsWith(TSslStaticLock, TControl);
  {$ENDIF VCL}
  {$ENDIF COMPILER16_UP}
  {$IFDEF VCL_OR_FMX}
    RegisterComponents('Overbyte ICS SSL', [
      TSslWSocket, TSslWSocketServer,
      TSslContext,
      TSslFtpClient, TSslFtpServer,
      TSslHttpCli, TSslHttpServer,
      TSslPop3Cli,
      TSslSmtpCli,
      TSslNntpCli,
      TSslAvlSessionCache,
    {$IFDEF VCL}
      {$IFNDEF BCB}
        TSslWSocketThrdServer,
      {$ENDIF}
    {$ENDIF VCL}
    {$IFNDEF NO_DYNLOCK}
      TSslDynamicLock,
    {$ENDIF}
    {$IFNDEF OPENSSL_NO_ENGINE}
      TSslEngine,
    {$ENDIF}
      TSslStaticLock
    ]);
  {$ENDIF VCL_OR_FMX}
{$ENDIF USE_SSL}
//{$IFNDEF ICS_COMMON}
    RegisterPropertyEditor(TypeInfo(AnsiString), TWSocket, 'LineEnd',
      TWSocketLineEndProperty);
//{$ENDIF}

{$IFDEF COMPILER10_UP}
  {$IFNDEF COMPILER16_UP}
    {$IFDEF ICS_COMMON}
      ForceDemandLoadState(dlDisable); // Required to show our product icon on splash screen
    {$ENDIF}
  {$ENDIF}
{$ENDIF}

end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF COMPILER10_UP}
{$IFDEF VCL}
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
{$ENDIF VCL}
{$ENDIF COMPILER10_UP}
end.

