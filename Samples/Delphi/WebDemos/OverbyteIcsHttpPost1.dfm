object HttpPostForm: THttpPostForm
  Left = 250
  Top = 147
  Caption = 'Http Post - http://www.orverbyte.be'
  ClientHeight = 352
  ClientWidth = 372
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ToolsPanel: TPanel
    Left = 0
    Top = 0
    Width = 372
    Height = 201
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 12
      Top = 12
      Width = 50
      Height = 13
      Caption = 'First Name'
    end
    object Label2: TLabel
      Left = 12
      Top = 40
      Width = 51
      Height = 13
      Caption = 'Last Name'
    end
    object Label3: TLabel
      Left = 8
      Top = 64
      Width = 55
      Height = 13
      Caption = 'Action URL'
    end
    object Label4: TLabel
      Left = 164
      Top = 88
      Width = 177
      Height = 13
      Caption = 'Use WebServ demo as server !'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentFont = False
    end
    object Label5: TLabel
      Left = 24
      Top = 123
      Width = 38
      Height = 13
      Caption = 'FilePath'
    end
    object Shape1: TShape
      Left = 12
      Top = 112
      Width = 349
      Height = 2
    end
    object Label6: TLabel
      Left = 4
      Top = 147
      Width = 59
      Height = 13
      Caption = 'Upload URL'
    end
    object Label10: TLabel
      Left = 6
      Top = 174
      Width = 172
      Height = 16
      Caption = 'Maximum Bandwidth (bytes/sec)'
      WordWrap = True
    end
    object FirstNameEdit: TEdit
      Left = 72
      Top = 8
      Width = 289
      Height = 21
      TabOrder = 0
      Text = 'FirstNameEdit'
    end
    object LastNameEdit: TEdit
      Left = 72
      Top = 36
      Width = 289
      Height = 21
      TabOrder = 1
      Text = 'LastNameEdit'
    end
    object ActionURLEdit: TEdit
      Left = 72
      Top = 60
      Width = 289
      Height = 21
      TabOrder = 2
      Text = 'ActionURLEdit'
    end
    object PostButton: TButton
      Left = 69
      Top = 86
      Width = 75
      Height = 21
      Caption = '&Post'
      TabOrder = 3
      OnClick = PostButtonClick
    end
    object FileNameEdit: TEdit
      Left = 70
      Top = 121
      Width = 290
      Height = 21
      TabOrder = 4
      Text = 'FileNameEdit'
    end
    object UploadButton: TButton
      Left = 259
      Top = 170
      Width = 75
      Height = 21
      Caption = '&Upload'
      TabOrder = 5
      OnClick = UploadButtonClick
    end
    object UploadURLEdit: TEdit
      Left = 69
      Top = 144
      Width = 294
      Height = 21
      TabOrder = 6
      Text = 'UploadURLEdit'
    end
    object BandwidthLimitEdit: TEdit
      Left = 171
      Top = 170
      Width = 65
      Height = 21
      TabOrder = 7
      Text = 'BandwidthLimitEdit'
    end
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 201
    Width = 372
    Height = 151
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'DisplayMemo')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
    ExplicitTop = 194
    ExplicitHeight = 158
  end
  object HttpCli1: THttpCli
    LocalAddr = '0.0.0.0'
    ProxyPort = '80'
    Agent = 'Mozilla/4.0 (compatible; ICS)'
    Accept = 'image/gif, image/x-xbitmap, image/jpeg, image/pjpeg, */*'
    NoCache = False
    ContentTypePost = 'application/x-www-form-urlencoded'
    MultiThreaded = False
    RequestVer = '1.0'
    FollowRelocation = True
    LocationChangeMaxCount = 5
    ServerAuth = httpAuthNone
    ProxyAuth = httpAuthNone
    BandwidthLimit = 10000
    BandwidthSampling = 1000
    Options = []
    Timeout = 30
    OnRequestDone = HttpCli1RequestDone
    SocksLevel = '5'
    SocksAuthentication = socksNoAuthentication
    Left = 88
    Top = 204
  end
end
