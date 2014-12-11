object SmtpSslSrvForm: TSmtpSslSrvForm
  Left = 192
  Top = 107
  Caption = 'Test SSL SMTP Server - http://www.overbyte.be'
  ClientHeight = 621
  ClientWidth = 707
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ButtonPanel: TPanel
    Left = 0
    Top = 0
    Width = 707
    Height = 161
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    object Label1: TLabel
      Left = 10
      Top = 35
      Width = 57
      Height = 13
      Caption = 'DNS Server'
    end
    object Label2: TLabel
      Left = 233
      Top = 38
      Width = 125
      Height = 13
      Caption = 'Email Accounts Accepted:'
    end
    object Label3: TLabel
      Left = 10
      Top = 10
      Width = 72
      Height = 13
      Caption = 'Spool Directory'
    end
    object Label4: TLabel
      Left = 369
      Top = 38
      Width = 334
      Height = 13
      Caption = 
        'Alias Email Accounts (alias=account, *@domain=account for catch-' +
        'all):'
    end
    object PrefDnsServer: TEdit
      Left = 98
      Top = 30
      Width = 121
      Height = 21
      TabOrder = 0
      Text = '8.8.8.8'
    end
    object PrefEmailAccs: TMemo
      Left = 173
      Top = 57
      Width = 203
      Height = 94
      Lines.Strings = (
        'PrefEmailAccs')
      ScrollBars = ssVertical
      TabOrder = 1
      WordWrap = False
    end
    object PrefSpoolDir: TEdit
      Left = 98
      Top = 5
      Width = 248
      Height = 21
      TabOrder = 2
      Text = 'c:\mailspool\'
    end
    object PrefAddRecvHdrs: TCheckBox
      Left = 10
      Top = 60
      Width = 131
      Height = 17
      Caption = 'Add Received Header'
      TabOrder = 3
    end
    object PrefAddEnvHdrs: TCheckBox
      Left = 10
      Top = 80
      Width = 141
      Height = 17
      Caption = 'Add Envelope Headers'
      TabOrder = 4
    end
    object doStart: TButton
      Left = 369
      Top = 7
      Width = 75
      Height = 25
      Caption = 'Start Server'
      TabOrder = 5
      OnClick = doStartClick
    end
    object doStop: TButton
      Left = 460
      Top = 7
      Width = 75
      Height = 25
      Caption = 'Stop Server'
      Enabled = False
      TabOrder = 6
      OnClick = doStopClick
    end
    object doExit: TButton
      Left = 550
      Top = 7
      Width = 75
      Height = 25
      Caption = 'Exit'
      TabOrder = 7
      OnClick = doExitClick
    end
    object PrefAllowRelay: TCheckBox
      Left = 10
      Top = 120
      Width = 131
      Height = 17
      Caption = 'Allow Relaying'
      TabOrder = 8
    end
    object PrefAliasAccs: TMemo
      Left = 388
      Top = 57
      Width = 298
      Height = 94
      Lines.Strings = (
        'PrefAliasAccs')
      ScrollBars = ssVertical
      TabOrder = 9
      WordWrap = False
    end
    object PrefAddReplayHdrs: TCheckBox
      Left = 10
      Top = 100
      Width = 141
      Height = 17
      Caption = 'Add Replay Headers'
      TabOrder = 10
    end
    object PrefAuthTls: TCheckBox
      Left = 10
      Top = 140
      Width = 146
      Height = 17
      Caption = 'Auth Requires TLS/SSL'
      TabOrder = 11
    end
  end
  object Log: TMemo
    Left = 0
    Top = 241
    Width = 707
    Height = 380
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'Log')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
  end
  object ToolsPanel: TPanel
    Left = 0
    Top = 161
    Width = 707
    Height = 80
    Align = alTop
    TabOrder = 2
    object Label5: TLabel
      Left = 39
      Top = 10
      Width = 35
      Height = 13
      Caption = 'CertFile'
    end
    object Label11: TLabel
      Left = 264
      Top = 10
      Width = 33
      Height = 13
      Caption = 'CA File'
    end
    object Label10: TLabel
      Left = 256
      Top = 35
      Width = 39
      Height = 13
      Caption = 'CA Path'
    end
    object Label7: TLabel
      Left = 20
      Top = 60
      Width = 56
      Height = 13
      Caption = 'PassPhrase'
    end
    object Label6: TLabel
      Left = 23
      Top = 35
      Width = 51
      Height = 13
      Caption = 'PrivateKey'
    end
    object CertFileEdit: TEdit
      Left = 80
      Top = 5
      Width = 153
      Height = 21
      TabOrder = 0
      Text = 'CertFileEdit'
    end
    object CAFileEdit: TEdit
      Left = 300
      Top = 5
      Width = 153
      Height = 21
      TabOrder = 1
      Text = 'CAFileEdit'
    end
    object CAPathEdit: TEdit
      Left = 300
      Top = 30
      Width = 153
      Height = 21
      TabOrder = 2
      Text = 'CAPathEdit'
    end
    object PrivKeyFileEdit: TEdit
      Left = 80
      Top = 30
      Width = 153
      Height = 21
      TabOrder = 3
      Text = 'PrivKeyFileEdit'
    end
    object PassPhraseEdit: TEdit
      Left = 80
      Top = 55
      Width = 153
      Height = 21
      TabOrder = 4
      Text = 'PassPhraseEdit'
    end
    object VerifyPeerCheckBox: TCheckBox
      Left = 300
      Top = 57
      Width = 71
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Verify Peer'
      TabOrder = 5
    end
  end
  object SmtpServer1: TSslSmtpServer
    MultiListenSockets = <
      item
        Addr = '0.0.0.0'
        Port = '587'
      end>
    Addr = '0.0.0.0'
    Port = 'smtp'
    SocketFamily = sfAnyIPv4
    ServerHost = 'pc19-web'
    ServerDesc = 'SMTP Server (c) 1997-2013 Francois Piette V8.00'
    MaxClients = 0
    MultiThreaded = False
    MaxMessageSize = 0
    ClientTimeout = 60
    GreyDelaySecs = 0
    Options = [smtpsAddIpAddrHdr]
    OnException = SmtpServer1Exception
    OnServerStarted = SmtpServer1ServerStarted
    OnServerStopped = SmtpServer1ServerStopped
    OnConnect = SmtpServer1Connect
    OnDisconnect = SmtpServer1Disconnect
    OnCommand = SmtpServer1Command
    OnResponse = SmtpServer1Response
    OnMailFrom = SmtpServer1MailFrom
    OnRcptTo = SmtpServer1RcptTo
    OnAuth = SmtpServer1Auth
    OnAuthPW = SmtpServer1AuthPW
    OnDataStart = SmtpServer1DataStart
    OnDataEnd = SmtpServer1DataEnd
    SslContext = SslContext1
    OnSslVerifyPeer = SmtpServer1SslVerifyPeer
    OnSslHandshakeDone = SmtpServer1SslHandshakeDone
    Left = 35
    Top = 325
  end
  object SslContext1: TSslContext
    SslVerifyPeer = False
    SslVerifyDepth = 9
    SslVerifyFlags = []
    SslOptions = [sslOpt_NO_SSLv2, sslOpt_NO_SSLv3]
    SslVerifyPeerModes = [SslVerifyMode_PEER]
    SslSessionCacheModes = []
    SslCipherList = 'ALL:!ADH:RC4+RSA:+SSLv2:@STRENGTH'
    SslVersionMethod = sslV23_SERVER
    SslSessionTimeout = 0
    SslSessionCacheSize = 20480
    Left = 85
    Top = 325
  end
end
