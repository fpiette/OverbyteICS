object MsVerifyForm: TMsVerifyForm
  Left = 69
  Top = 0
  Width = 530
  Height = 385
  ActiveControl = ConnectButton
  Caption = 'Test MS Crypto API Certificate Verification'
  Color = clBtnFace
  Constraints.MinHeight = 150
  Constraints.MinWidth = 530
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Shell Dlg 2'
  Font.Style = []
  OldCreateOrder = False
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ToolsPanel: TPanel
    Left = 0
    Top = 0
    Width = 522
    Height = 65
    Align = alTop
    TabOrder = 0
    DesignSize = (
      522
      65)
    object Label1: TLabel
      Left = 24
      Top = 13
      Width = 26
      Height = 13
      Caption = 'Host:'
    end
    object Label2: TLabel
      Left = 359
      Top = 14
      Width = 24
      Height = 13
      Anchors = [akTop, akRight]
      Caption = 'Port:'
    end
    object Label3: TLabel
      Left = 173
      Top = 40
      Width = 105
      Height = 13
      Caption = 'URL retrieval timeout:'
    end
    object HostEdit: TEdit
      Left = 56
      Top = 10
      Width = 297
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
      Text = 'www.embarcadero.com'
    end
    object PortEdit: TEdit
      Left = 385
      Top = 10
      Width = 43
      Height = 21
      Anchors = [akTop, akRight]
      TabOrder = 1
      Text = '443'
    end
    object ConnectButton: TButton
      Left = 437
      Top = 8
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Connect'
      TabOrder = 2
      OnClick = ConnectButtonClick
    end
    object RevocationCheckBox: TCheckBox
      Left = 56
      Top = 37
      Width = 117
      Height = 17
      Caption = 'Revocation check'
      TabOrder = 3
    end
    object TimeoutEdit: TEdit
      Left = 284
      Top = 37
      Width = 69
      Height = 21
      TabOrder = 4
      Text = 'TimeoutEdit'
    end
    object ShowCertButton: TButton
      Left = 56
      Top = 10
      Width = 75
      Height = 21
      Caption = 'ShowCertButton'
      TabOrder = 5
      OnClick = ShowCertButtonClick
    end
    object DisconnectButton: TButton
      Left = 437
      Top = 36
      Width = 75
      Height = 25
      Anchors = [akTop, akRight]
      Caption = 'Disconnect'
      TabOrder = 6
      OnClick = DisconnectButtonClick
    end
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 65
    Width = 522
    Height = 286
    Align = alClient
    Lines.Strings = (
      'DisplayMemo')
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object SslWSocket1: TSslWSocket
    LineMode = False
    LineLimit = 65536
    LineEnd = #13#10
    LineEcho = False
    LineEdit = False
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
    OnDataAvailable = SslWSocket1DataAvailable
    OnSessionClosed = SslWSocket1SessionClosed
    OnSessionConnected = SslWSocket1SessionConnected
    SslContext = SslContext1
    SslEnable = True
    SslMode = sslModeClient
    OnSslVerifyPeer = SslWSocket1SslVerifyPeer
    OnSslHandshakeDone = SslWSocket1SslHandshakeDone
    OnSslCliGetSession = SslWSocket1SslCliGetSession
    OnSslCliNewSession = SslWSocket1SslCliNewSession
    Left = 36
    Top = 86
  end
  object SslContext1: TSslContext
    SslVerifyPeer = False
    SslVerifyDepth = 9
    SslVerifyFlags = []
    SslOptions = [sslOpt_NO_SSLv2]
    SslVerifyPeerModes = [SslVerifyMode_PEER]
    SslSessionCacheModes = [sslSESS_CACHE_CLIENT, sslSESS_CACHE_NO_INTERNAL_LOOKUP, sslSESS_CACHE_NO_INTERNAL_STORE]
    SslCipherList = 'ALL:!ADH:RC4+RSA:+SSLv2:@STRENGTH'
    SslVersionMethod = sslV23_CLIENT
    SslSessionTimeout = 5000
    SslSessionCacheSize = 20480
    Left = 112
    Top = 86
  end
  object SslAvlSessionCache1: TSslAvlSessionCache
    IdleTimeout = 30
    FlushInterval = 10000
    MaxCacheSize = 1000
    Left = 206
    Top = 86
  end
end
