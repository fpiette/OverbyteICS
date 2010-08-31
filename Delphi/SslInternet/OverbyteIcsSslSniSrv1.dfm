object MainForm: TMainForm
  Left = 298
  Top = 146
  Width = 461
  Height = 300
  Caption = 'ICS SSL SNI Server Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object DisplayMemo: TMemo
    Left = 0
    Top = 0
    Width = 453
    Height = 266
    Align = alClient
    Lines.Strings = (
      'DisplayMemo')
    ScrollBars = ssVertical
    TabOrder = 0
    WordWrap = False
  end
  object SslWSocketServer1: TSslWSocketServer
    LineMode = False
    LineLimit = 65536
    LineEnd = #13#10
    LineEcho = False
    LineEdit = False
    Addr = '0.0.0.0'
    Port = '443'
    Proto = 'tcp'
    LocalAddr = '0.0.0.0'
    LocalPort = '0'
    MultiThreaded = False
    MultiCast = False
    MultiCastIpTTL = 1
    FlushTimeout = 60
    SendFlags = wsSendNormal
    LingerOnOff = wsLingerOn
    LingerTimeout = 0
    KeepAliveOnOff = wsKeepAliveOff
    KeepAliveTime = 0
    KeepAliveInterval = 0
    SocksLevel = '5'
    SocksAuthentication = socksNoAuthentication
    LastError = 0
    ReuseAddr = False
    ComponentOptions = []
    ListenBacklog = 5
    ReqVerLow = 1
    ReqVerHigh = 1
    Banner = 'Welcome to OverByte ICS TcpSrv'
    BannerTooBusy = 'Sorry, too many clients'
    MaxClients = 0
    OnClientDisconnect = ClientDisconnect
    OnClientConnect = ClientConnect
    SslContext = SslContext1
    SslEnable = True
    Left = 70
    Top = 104
  end
  object SslContext1: TSslContext
    SslCertFile = 'SelfSignedServer1.pem'
    SslPrivKeyFile = 'SelfSignedServer1.pem'
    SslVerifyPeer = False
    SslVerifyDepth = 9
    SslOptions = [sslOpt_CIPHER_SERVER_PREFERENCE, sslOpt_NETSCAPE_CHALLENGE_BUG, sslOpt_NETSCAPE_REUSE_CIPHER_CHANGE_BUG, sslOpt_NO_SSLv2]
    SslVerifyPeerModes = [SslVerifyMode_PEER]
    SslSessionCacheModes = []
    SslCipherList = 'ALL:!ADH:RC4+RSA:+SSLv2:@STRENGTH'
    SslVersionMethod = sslV23_SERVER
    SslSessionTimeout = 0
    SslSessionCacheSize = 20480
    Left = 160
    Top = 104
  end
  object SslContext2: TSslContext
    SslCertFile = 'SelfSignedServer2.pem'
    SslPrivKeyFile = 'SelfSignedServer2.pem'
    SslVerifyPeer = False
    SslVerifyDepth = 9
    SslOptions = [sslOpt_CIPHER_SERVER_PREFERENCE, sslOpt_MICROSOFT_SESS_ID_BUG, sslOpt_NETSCAPE_CHALLENGE_BUG, sslOpt_NO_SSLv2]
    SslVerifyPeerModes = [SslVerifyMode_PEER]
    SslSessionCacheModes = []
    SslCipherList = 'ALL:!ADH:RC4+RSA:+SSLv2:@STRENGTH'
    SslVersionMethod = sslV23_SERVER
    SslSessionTimeout = 0
    SslSessionCacheSize = 20480
    Left = 240
    Top = 104
  end
end
