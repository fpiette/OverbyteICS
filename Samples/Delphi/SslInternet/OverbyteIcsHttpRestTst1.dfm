object HttpRestForm: THttpRestForm
  Left = 86
  Top = 176
  Caption = 
    'ICS HTTPS REST and OAuth Demo - http://www.overbyte.be - V8.61 -' +
    ' 24th April 2019'
  ClientHeight = 636
  ClientWidth = 823
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 823
    Height = 315
    ActivePage = TabREST
    Align = alTop
    TabOrder = 0
    object TabREST: TTabSheet
      Caption = 'HTTPS REST'
      object Label1: TLabel
        Left = 10
        Top = 5
        Width = 87
        Height = 14
        Caption = 'REST Paramaters '
      end
      object Label3: TLabel
        Left = 10
        Top = 155
        Width = 271
        Height = 14
        Caption = 'Raw Parameters (not blank overrides REST Parameters)'
      end
      object Label5: TLabel
        Left = 10
        Top = 200
        Width = 104
        Height = 14
        Caption = 'URL (no ? or Params)'
      end
      object RestURL: TComboBox
        Left = 10
        Top = 220
        Width = 478
        Height = 22
        ItemHeight = 14
        TabOrder = 5
        Text = 'https://jsonplaceholder.typicode.com/posts/1'
        Items.Strings = (
          'https://jsonplaceholder.typicode.com/posts/1'
          'https://jsonplaceholder.typicode.com/posts'
          'https://jsonplaceholder.typicode.com/users'
          'https://reqres.in/api/users/2'
          'https://reqres.in/api/users'
          'https://fakerestapi.azurewebsites.net/api/Activities'
          'https://www.telecom-tariffs.co.uk/serverinfo.htm'
          'https://api.cix.uk/.well-known/openid-configuration'
          'https://accounts.google.com/.well-known/openid-configuration')
      end
      object GridParams: TStringGrid
        Left = 10
        Top = 20
        Width = 453
        Height = 120
        ColCount = 3
        DefaultRowHeight = 20
        FixedCols = 0
        RowCount = 10
        Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing]
        TabOrder = 0
        ColWidths = (
          133
          215
          70)
      end
      object ParamContent: TRadioGroup
        Left = 480
        Top = 20
        Width = 109
        Height = 56
        Caption = 'REST Content'
        ItemIndex = 0
        Items.Strings = (
          'URL Eencoded'
          'Json')
        TabOrder = 1
      end
      object ReqMode: TRadioGroup
        Left = 480
        Top = 94
        Width = 109
        Height = 56
        Caption = 'Request Mode'
        ItemIndex = 0
        Items.Strings = (
          'Sync Request'
          'Async Request')
        TabOrder = 2
      end
      object RawParams: TEdit
        Left = 10
        Top = 170
        Width = 773
        Height = 22
        TabOrder = 4
      end
      object ReqType: TRadioGroup
        Left = 607
        Top = 20
        Width = 109
        Height = 106
        Caption = 'Request Type'
        ItemIndex = 0
        Items.Strings = (
          'GET'
          'POST'
          'HEAD'
          'PUT'
          'DELETE')
        TabOrder = 3
      end
      object doStartReq: TButton
        Left = 10
        Top = 255
        Width = 122
        Height = 25
        Caption = 'Start REST Request'
        TabOrder = 6
        OnClick = doStartReqClick
      end
      object doClear: TButton
        Left = 250
        Top = 255
        Width = 75
        Height = 25
        Caption = 'Clear Log'
        TabOrder = 8
        OnClick = doClearClick
      end
      object doAbort: TButton
        Left = 154
        Top = 255
        Width = 75
        Height = 25
        Caption = 'Abort'
        TabOrder = 7
        OnClick = doAbortClick
      end
    end
    object TabSettings: TTabSheet
      Caption = 'Settings'
      ImageIndex = 1
      object Label2: TLabel
        Left = 10
        Top = 240
        Width = 136
        Height = 14
        Caption = 'Optional Root CA Bundle File'
      end
      object Label4: TLabel
        Left = 490
        Top = 5
        Width = 115
        Height = 14
        Caption = 'Special Extra Header(s)'
      end
      object Label6: TLabel
        Left = 460
        Top = 105
        Width = 52
        Height = 14
        Caption = 'Auth Login'
      end
      object Label7: TLabel
        Left = 460
        Top = 135
        Width = 76
        Height = 14
        Caption = 'Auth Password'
      end
      object Label8: TLabel
        Left = 460
        Top = 165
        Width = 67
        Height = 14
        Caption = ' Bearer Token'
      end
      object Label9: TLabel
        Left = 405
        Top = 240
        Width = 253
        Height = 14
        Caption = 'Optional Client SSL Certificate (if required by server)'
      end
      object Label22: TLabel
        Left = 460
        Top = 185
        Width = 65
        Height = 14
        Caption = 'Log Directory'
      end
      object DebugLogging: TRadioGroup
        Left = 10
        Top = 3
        Width = 121
        Height = 136
        Caption = 'Debug Logging'
        ItemIndex = 3
        Items.Strings = (
          'None'
          'Connections'
          'Parameters'
          'SSL Negotiation'
          'HTTP Headers'
          'HTML Body'
          'Ssl Low Level')
        TabOrder = 0
      end
      object SslSecurity: TRadioGroup
        Left = 152
        Top = 3
        Width = 169
        Height = 246
        Caption = 'SSL Security Level'
        ItemIndex = 6
        Items.Strings = (
          'Ignore'
          'None'
          'SSLv3 Only'
          'TLSv1 Only'
          'TLSv1.1 Only'
          'TLSv1.2 Only'
          'TLSv1.3 Only'
          'TLSv1 or Better'
          'TLSv1.1 or Better'
          'TLSv1.2 or Better'
          'Backward Ciphers'
          'Intermediate Ciphers'
          'High Ciphers, 2048 keys'
          'High Ciphers, 3072 keys'
          'High Ciphers, 7680 keys')
        TabOrder = 2
        OnClick = SettingsChange
      end
      object CertVerMethod: TRadioGroup
        Left = 337
        Top = 5
        Width = 134
        Height = 69
        Caption = 'SSL Certificate Check'
        ItemIndex = 0
        Items.Strings = (
          'None'
          'PEM Bundle File'
          'Windows Cert Store')
        TabOrder = 3
      end
      object ReportCertChain: TCheckBox
        Left = 337
        Top = 80
        Width = 111
        Height = 29
        Caption = 'Report SSL Certificate Chain'
        TabOrder = 4
        WordWrap = True
      end
      object SslClientCertFile: TEdit
        Left = 405
        Top = 255
        Width = 341
        Height = 22
        TabOrder = 13
      end
      object ExtraHeaders: TMemo
        Left = 490
        Top = 30
        Width = 256
        Height = 59
        Lines.Strings = (
          'ExtraHeaders')
        TabOrder = 6
      end
      object AuthType: TRadioGroup
        Left = 337
        Top = 115
        Width = 104
        Height = 119
        Caption = 'Authentication'
        ItemIndex = 0
        Items.Strings = (
          'None'
          'Basic'
          'NTLM'
          'Digest'
          'Bearer'
          'XAuth Token')
        TabOrder = 5
      end
      object AuthLogin: TEdit
        Left = 545
        Top = 100
        Width = 141
        Height = 22
        TabOrder = 7
      end
      object AuthPassword: TEdit
        Left = 545
        Top = 130
        Width = 141
        Height = 22
        PasswordChar = '*'
        TabOrder = 8
      end
      object AuthBearer: TEdit
        Left = 545
        Top = 160
        Width = 241
        Height = 22
        TabOrder = 9
      end
      object SslRootBundleFile: TEdit
        Left = 10
        Top = 255
        Width = 341
        Height = 22
        TabOrder = 12
      end
      object IpSockFamily: TRadioGroup
        Left = 10
        Top = 145
        Width = 121
        Height = 92
        Caption = 'Socket Family'
        ItemIndex = 0
        Items.Strings = (
          'Any'
          'Prefer IPv4'
          'Prefer IPv6'
          'Only IPv4 '
          'Only IPv6')
        TabOrder = 1
      end
      object SelDirLogs: TBitBtn
        Left = 769
        Top = 233
        Width = 31
        Height = 25
        TabOrder = 11
        OnClick = SelDirLogsClick
        Glyph.Data = {
          76010000424D7601000000000000760000002800000020000000100000000100
          04000000000000010000120B0000120B00001000000000000000000000000000
          800000800000008080008000000080008000808000007F7F7F00BFBFBF000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00303333333333
          333337F3333333333333303333333333333337F33FFFFF3FF3FF303300000300
          300337FF77777F77377330000BBB0333333337777F337F33333330330BB00333
          333337F373F773333333303330033333333337F3377333333333303333333333
          333337F33FFFFF3FF3FF303300000300300337FF77777F77377330000BBB0333
          333337777F337F33333330330BB00333333337F373F773333333303330033333
          333337F3377333333333303333333333333337FFFF3FF3FFF333000003003000
          333377777F77377733330BBB0333333333337F337F33333333330BB003333333
          333373F773333333333330033333333333333773333333333333}
        NumGlyphs = 2
      end
      object DirLogs: TEdit
        Left = 460
        Top = 205
        Width = 326
        Height = 22
        TabOrder = 10
      end
    end
    object TabOAuth: TTabSheet
      Caption = 'OAuth'
      ImageIndex = 2
      object Label19: TLabel
        Left = 300
        Top = 220
        Width = 69
        Height = 14
        Caption = 'Web Server IP'
      end
      object Label20: TLabel
        Left = 300
        Top = 250
        Width = 80
        Height = 14
        Caption = 'Web Server Port'
      end
      object OAuthProtoType: TRadioGroup
        Left = 10
        Top = 212
        Width = 101
        Height = 71
        Caption = 'OAuth Protocol'
        Enabled = False
        ItemIndex = 2
        Items.Strings = (
          'OAuth1'
          'OAuth1A'
          'OAuth2')
        TabOrder = 2
      end
      object GroupBox1: TGroupBox
        Left = 10
        Top = 5
        Width = 381
        Height = 202
        Caption = 'Permanent Web Server OAuth App'
        TabOrder = 0
        object Label10: TLabel
          Left = 5
          Top = 25
          Width = 68
          Height = 14
          Caption = 'App Auth URL'
        end
        object Label11: TLabel
          Left = 5
          Top = 55
          Width = 37
          Height = 14
          Caption = 'Client Id'
        end
        object Label12: TLabel
          Left = 5
          Top = 85
          Width = 61
          Height = 14
          Caption = 'Client Secret'
        end
        object Label13: TLabel
          Left = 5
          Top = 115
          Width = 60
          Height = 14
          Caption = 'Redirect-URI'
        end
        object Label14: TLabel
          Left = 5
          Top = 145
          Width = 74
          Height = 14
          Caption = 'App Token URL'
        end
        object Label21: TLabel
          Left = 5
          Top = 175
          Width = 31
          Height = 14
          Caption = 'Scope'
        end
        object OAuthAppUrl: TEdit
          Left = 90
          Top = 20
          Width = 281
          Height = 22
          TabOrder = 0
        end
        object OAuthClientId: TEdit
          Left = 90
          Top = 50
          Width = 281
          Height = 22
          TabOrder = 1
        end
        object OAuthClientSecret: TEdit
          Left = 90
          Top = 80
          Width = 146
          Height = 22
          TabOrder = 2
        end
        object OAuthRedirectUrl: TEdit
          Left = 90
          Top = 110
          Width = 281
          Height = 22
          TabOrder = 4
        end
        object OAuthTokenUrl: TEdit
          Left = 90
          Top = 140
          Width = 281
          Height = 22
          TabOrder = 5
        end
        object OAuthScope: TEdit
          Left = 90
          Top = 170
          Width = 281
          Height = 22
          TabOrder = 6
        end
        object OAuthOptNoRedir: TCheckBox
          Left = 250
          Top = 80
          Width = 121
          Height = 17
          Caption = 'Auth No Redirect'
          TabOrder = 3
        end
      end
      object GroupBox2: TGroupBox
        Left = 410
        Top = 5
        Width = 371
        Height = 185
        Caption = 'Short Lived Codes and Tokens'
        TabOrder = 1
        object Label15: TLabel
          Left = 5
          Top = 25
          Width = 92
          Height = 14
          Caption = 'Authorization Code'
        end
        object Label16: TLabel
          Left = 5
          Top = 55
          Width = 69
          Height = 14
          Caption = 'Access Token'
        end
        object Label17: TLabel
          Left = 5
          Top = 85
          Width = 70
          Height = 14
          Caption = 'Refresh Token'
        end
        object Label18: TLabel
          Left = 5
          Top = 115
          Width = 67
          Height = 14
          Caption = 'Tokens Expire'
        end
        object LabelResult: TLabel
          Left = 10
          Top = 165
          Width = 36
          Height = 14
          Caption = 'Result: '
          Color = clBtnFace
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentColor = False
          ParentFont = False
        end
        object OAuthAuthCode: TEdit
          Left = 110
          Top = 20
          Width = 231
          Height = 22
          TabOrder = 0
        end
        object OAuthAccToken: TEdit
          Left = 110
          Top = 50
          Width = 231
          Height = 22
          Enabled = False
          TabOrder = 1
        end
        object OAuthRefToken: TEdit
          Left = 110
          Top = 80
          Width = 231
          Height = 22
          TabOrder = 2
        end
        object OAuthExpire: TEdit
          Left = 110
          Top = 110
          Width = 181
          Height = 22
          Enabled = False
          TabOrder = 3
        end
        object OAuthAutoRefresh: TCheckBox
          Left = 5
          Top = 142
          Width = 231
          Height = 17
          Caption = 'Automatic Refresh, Minutes Before Expiry'
          TabOrder = 4
        end
        object OAuthRefrMins: TEdit
          Left = 257
          Top = 140
          Width = 64
          Height = 22
          TabOrder = 5
          Text = '120'
        end
      end
      object OAuthAuthType: TRadioGroup
        Left = 130
        Top = 212
        Width = 135
        Height = 70
        Caption = 'OAuth Authentication'
        ItemIndex = 0
        Items.Strings = (
          'Local Web Server'
          'Manual Code'
          'Embedded Browser')
        TabOrder = 3
      end
      object OAuthWebIP: TComboBox
        Left = 390
        Top = 215
        Width = 106
        Height = 22
        ItemHeight = 14
        TabOrder = 4
        Text = '127.0.0.1'
      end
      object OAuthWebPort: TEdit
        Left = 390
        Top = 245
        Width = 56
        Height = 22
        MaxLength = 5
        TabOrder = 5
        Text = '8080'
      end
      object doOAuthLogin: TButton
        Left = 527
        Top = 227
        Width = 106
        Height = 25
        Caption = 'Login to App'
        TabOrder = 7
        OnClick = doOAuthLoginClick
      end
      object doOAuthToken: TButton
        Left = 675
        Top = 197
        Width = 106
        Height = 25
        Caption = 'Get New Token'
        TabOrder = 8
        OnClick = doOAuthTokenClick
      end
      object doOAuthRefresh: TButton
        Left = 675
        Top = 228
        Width = 106
        Height = 25
        Caption = 'Refresh Token'
        TabOrder = 9
        OnClick = doOAuthRefreshClick
      end
      object doTestRedir: TButton
        Left = 527
        Top = 196
        Width = 106
        Height = 25
        Caption = 'Test Redirect'
        TabOrder = 6
        OnClick = doTestRedirClick
      end
      object doGrantCred: TButton
        Left = 527
        Top = 258
        Width = 106
        Height = 25
        Caption = 'Grant Credentials'
        TabOrder = 10
        OnClick = doGrantCredClick
      end
      object doGrantPassword: TButton
        Left = 675
        Top = 259
        Width = 106
        Height = 25
        Caption = 'Grant Password'
        TabOrder = 11
        OnClick = doGrantPasswordClick
      end
    end
    object TabDNSHTTPS: TTabSheet
      Caption = 'DNS over HTTPS'
      ImageIndex = 3
      object Label23: TLabel
        Left = 10
        Top = 15
        Width = 104
        Height = 14
        Caption = 'DNS over HTTPS URL'
      end
      object Label222: TLabel
        Left = 10
        Top = 45
        Width = 115
        Height = 14
        Caption = 'Domain Name to Lookup'
      end
      object Label25: TLabel
        Left = 10
        Top = 75
        Width = 94
        Height = 14
        Caption = 'Lookup Query Type'
      end
      object DnsHttpsUrl: TComboBox
        Left = 150
        Top = 10
        Width = 346
        Height = 22
        ItemHeight = 14
        TabOrder = 0
        Text = 'https://cloudflare-dns.com/dns-query'
        Items.Strings = (
          'https://cloudflare-dns.com/dns-query'
          'https://dns.google.com/resolve'
          'https://dns.quad9.net/dns-query'
          'https://doh.powerdns.org'
          'https://doh.securedns.eu/dns-query'
          'https://doh.appliedprivacy.net/query')
      end
      object DnsDomainName: TComboBox
        Left = 150
        Top = 40
        Width = 201
        Height = 22
        ItemHeight = 14
        TabOrder = 1
        Text = 'pool.ntp.org'
        Items.Strings = (
          'pool.ntp.org'
          'www.google.com'
          'google.com'
          'www.overbyte.eu'
          'overbyte.eu'
          'wiki.overbyte.eu'
          'magsys.co.uk'
          'www.magsys.co.uk'
          'ftp.magsys.co.uk'
          'mail.magsys.co.uk'
          'embarcadero.com'
          'www.embarcadero.com')
      end
      object DnsQueryType: TComboBox
        Left = 150
        Top = 68
        Width = 201
        Height = 22
        Style = csDropDownList
        ItemHeight = 14
        TabOrder = 2
      end
      object doDNSJson: TButton
        Left = 10
        Top = 115
        Width = 104
        Height = 25
        Caption = 'DNS Using Json'
        TabOrder = 5
        OnClick = doDNSJsonClick
      end
      object DnsDnssec: TCheckBox
        Left = 370
        Top = 45
        Width = 106
        Height = 17
        Caption = 'DNSSEC Data'
        TabOrder = 3
      end
      object DnsNoValidation: TCheckBox
        Left = 370
        Top = 75
        Width = 116
        Height = 17
        Caption = 'Disable Validation'
        TabOrder = 4
      end
      object doDnsQuery1: TButton
        Left = 127
        Top = 115
        Width = 138
        Height = 25
        Caption = 'Single Query (Wire Format)'
        TabOrder = 6
        OnClick = doDnsQuery1Click
      end
      object doDnsQueryAll: TButton
        Left = 278
        Top = 115
        Width = 138
        Height = 25
        Caption = 'All Queries (Wire Format)'
        TabOrder = 7
        OnClick = doDnsQueryAllClick
      end
    end
    object TabTwitter: TTabSheet
      Caption = 'Twiiter'
      ImageIndex = 4
    end
    object TabSms: TTabSheet
      Caption = 'Send SMS'
      ImageIndex = 5
      ExplicitTop = 23
      object BoxKapow: TGroupBox
        Left = 5
        Top = 10
        Width = 426
        Height = 261
        Caption = 'https://www.kapow.co.uk/'
        TabOrder = 0
        object Label24: TLabel
          Left = 220
          Top = 25
          Width = 50
          Height = 14
          Caption = 'Password'
        end
        object Label26: TLabel
          Left = 5
          Top = 55
          Width = 75
          Height = 14
          Caption = 'Sender Number'
        end
        object Label27: TLabel
          Left = 5
          Top = 25
          Width = 74
          Height = 14
          Caption = 'Account: Name'
        end
        object Label28: TLabel
          Left = 220
          Top = 58
          Width = 62
          Height = 14
          Caption = 'SMS Number'
        end
        object Label29: TLabel
          Left = 5
          Top = 85
          Width = 44
          Height = 14
          Caption = 'Message'
        end
        object LabelKapowCredit: TLabel
          Left = 298
          Top = 194
          Width = 78
          Height = 14
          Caption = 'Kapow Credits: '
          Font.Charset = DEFAULT_CHARSET
          Font.Color = clRed
          Font.Height = -11
          Font.Name = 'Arial'
          Font.Style = []
          ParentFont = False
        end
        object KapowAccName: TEdit
          Left = 85
          Top = 20
          Width = 126
          Height = 22
          TabOrder = 0
        end
        object KapowAccPw: TEdit
          Left = 290
          Top = 22
          Width = 126
          Height = 22
          PasswordChar = '*'
          TabOrder = 1
        end
        object KapowAccSender: TEdit
          Left = 85
          Top = 50
          Width = 126
          Height = 22
          TabOrder = 2
        end
        object KapowSmsNum: TEdit
          Left = 290
          Top = 50
          Width = 126
          Height = 22
          TabOrder = 3
        end
        object KapowMsg: TMemo
          Left = 85
          Top = 80
          Width = 331
          Height = 91
          Lines.Strings = (
            'KapowMsg')
          ScrollBars = ssVertical
          TabOrder = 4
        end
        object doKapowSend: TButton
          Left = 14
          Top = 190
          Width = 75
          Height = 25
          Caption = 'Send SMS'
          TabOrder = 5
          OnClick = doKapowSendClick
        end
        object doKapowCheck: TButton
          Left = 106
          Top = 190
          Width = 85
          Height = 25
          Caption = 'Check Delivery'
          Enabled = False
          TabOrder = 6
          OnClick = doKapowCheckClick
        end
        object doKapowCredit: TButton
          Left = 207
          Top = 190
          Width = 75
          Height = 25
          Caption = 'Check Credit'
          TabOrder = 7
          OnClick = doKapowCreditClick
        end
      end
    end
  end
  object LogWin: TMemo
    Left = 0
    Top = 434
    Width = 823
    Height = 202
    Align = alBottom
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
  end
  object RespList: TListView
    Left = 0
    Top = 315
    Width = 823
    Height = 119
    Align = alClient
    Columns = <
      item
        Caption = 'Name'
        Width = 100
      end
      item
        Caption = 'Type'
        Width = 70
      end
      item
        Caption = 'Value'
        Width = 400
      end
      item
        Width = 100
      end>
    GridLines = True
    ReadOnly = True
    TabOrder = 1
    ViewStyle = vsReport
  end
  object HttpRest1: TSslHttpRest
    LocalAddr = '0.0.0.0'
    LocalAddr6 = '::'
    ProxyPort = '80'
    Agent = 'Mozilla/4.0'
    Accept = 'image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, */*'
    NoCache = False
    ResponseNoException = False
    ContentTypePost = 'application/x-www-form-urlencoded'
    RequestVer = '1.1'
    FollowRelocation = True
    LocationChangeMaxCount = 5
    ServerAuth = httpAuthNone
    ProxyAuth = httpAuthNone
    BandwidthLimit = 10000
    BandwidthSampling = 1000
    Options = [httpoEnableContentCoding]
    Timeout = 30
    SocksAuthentication = socksNoAuthentication
    SocketFamily = sfAny
    SocketErrs = wsErrTech
    RestParams.PContent = PContUrlencoded
    RestParams = <>
    DebugLevel = DebugSsl
    MaxBodySize = 1000000
    SslCliSecurity = sslCliSecTls1
    SslSessCache = True
    CertVerMethod = CertVerNone
    SslRootFile = 'RootCaCertsBundle.pem'
    SslRevocation = False
    SslReportChain = False
    OnHttpRestProg = HttpRest1HttpRestProg
    OnRestRequestDone = HttpRest1RestRequestDone
    Left = 60
    Top = 375
  end
  object RestOAuth1: TRestOAuth
    DebugLevel = DebugConn
    AuthType = OAuthTypeWeb
    OAOptions = []
    ProtoType = OAuthv2
    RefreshAuto = False
    RefrMinsPrior = 0
    WebSrvIP = '127.0.0.1'
    WebSrvPort = '8080'
    OnOAuthAuthUrl = RestOAuth1OAuthAuthUrl
    OnOAuthNewCode = RestOAuth1OAuthNewCode
    OnOAuthNewToken = RestOAuth1OAuthNewToken
    Left = 110
    Top = 375
  end
  object OpenDirDiag: TOpenDialog
    Options = [ofHideReadOnly, ofNoValidate, ofPathMustExist, ofNoTestFileCreate, ofEnableSizing]
    Title = 'Select Directory'
    Left = 215
    Top = 750
  end
  object DnsQueryHttps1: TDnsQueryHttps
    Port = '53'
    Proto = 'udp'
    MultiThreaded = False
    OnRequestDone = DnsQueryHttps1RequestDone
    DnsSrvUrl = 'https://cloudflare-dns.com/dns-query'
    DebugLevel = DebugNone
    OnDnsProg = DnsQueryHttps1DnsProg
    Left = 160
    Top = 375
  end
  object TimerLog: TTimer
    Interval = 2000
    OnTimer = TimerLogTimer
    Left = 210
    Top = 375
  end
  object IcsSMS1: TIcsSMS
    SmsProvider = SmsProvKapow
    DebugLevel = DebugNone
    OnSmsProg = IcsSMS1SmsProg
    OnSmsDone = IcsSMS1SmsDone
    Left = 255
    Top = 375
  end
end
