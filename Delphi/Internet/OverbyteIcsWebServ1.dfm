object WebServForm: TWebServForm
  Left = 130
  Top = 386
  Caption = 'ICS WebServer Demo - http://www.overbyte.be'
  ClientHeight = 281
  ClientWidth = 501
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ToolsPanel: TPanel
    Left = 0
    Top = 0
    Width = 501
    Height = 105
    Align = alTop
    TabOrder = 0
    object Label1: TLabel
      Left = 36
      Top = 8
      Width = 33
      Height = 13
      Caption = 'DocDir'
    end
    object Label2: TLabel
      Left = 16
      Top = 32
      Width = 54
      Height = 13
      Caption = 'DefaultDoc'
    end
    object Label3: TLabel
      Left = 259
      Top = 7
      Width = 19
      Height = 13
      Caption = 'Port'
    end
    object ClientCountLabel: TLabel
      Left = 348
      Top = 87
      Width = 80
      Height = 13
      Caption = 'ClientCountLabel'
    end
    object Label5: TLabel
      Left = 312
      Top = 87
      Width = 31
      Height = 13
      Caption = 'Clients'
    end
    object Label4: TLabel
      Left = 16
      Top = 83
      Width = 50
      Height = 13
      Caption = 'Redir URL'
    end
    object Label6: TLabel
      Left = 14
      Top = 55
      Width = 57
      Height = 13
      Caption = 'TemplateDir'
    end
    object DocDirEdit: TEdit
      Left = 80
      Top = 4
      Width = 165
      Height = 21
      TabOrder = 0
      Text = 'DocDirEdit'
    end
    object DefaultDocEdit: TEdit
      Left = 80
      Top = 28
      Width = 165
      Height = 21
      TabOrder = 1
      Text = 'DefaultDocEdit'
    end
    object StartButton: TButton
      Left = 251
      Top = 52
      Width = 53
      Height = 21
      Caption = '&Start'
      TabOrder = 3
      OnClick = StartButtonClick
    end
    object StopButton: TButton
      Left = 251
      Top = 79
      Width = 53
      Height = 21
      Caption = 'St&op'
      TabOrder = 4
      OnClick = StopButtonClick
    end
    object PortEdit: TEdit
      Left = 283
      Top = 3
      Width = 53
      Height = 21
      TabOrder = 5
      Text = 'PortEdit'
    end
    object ClearButton: TButton
      Left = 311
      Top = 52
      Width = 53
      Height = 21
      Caption = '&Clear'
      TabOrder = 6
      OnClick = ClearButtonClick
    end
    object DisplayHeaderCheckBox: TCheckBox
      Left = 389
      Top = 68
      Width = 97
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Display Header'
      TabOrder = 7
    end
    object WriteLogFileCheckBox: TCheckBox
      Left = 389
      Top = 45
      Width = 97
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Write to log file'
      TabOrder = 8
      OnClick = WriteLogFileCheckBoxClick
    end
    object DirListCheckBox: TCheckBox
      Left = 401
      Top = 25
      Width = 85
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Allow Dir List'
      TabOrder = 2
    end
    object OutsideRootCheckBox: TCheckBox
      Left = 373
      Top = 6
      Width = 113
      Height = 17
      Alignment = taLeftJustify
      Caption = 'Allow Outside Root'
      TabOrder = 9
    end
    object RedirURLEdit: TEdit
      Left = 80
      Top = 79
      Width = 165
      Height = 21
      Hint = 'Enter here the URL used for the redir.htm virtual page.'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 10
      Text = 'RedirURLEdit'
    end
    object TemplateDirEdit: TEdit
      Left = 80
      Top = 52
      Width = 165
      Height = 21
      TabOrder = 11
      Text = 'TemplateDirEdit'
    end
  end
  object DisplayMemo: TMemo
    Left = 0
    Top = 105
    Width = 501
    Height = 176
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    Lines.Strings = (
      'DisplayMemo')
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object HttpServer1: THttpServer
    ListenBacklog = 5
    Port = '80'
    Addr = '0.0.0.0'
    MaxClients = 0
    DocDir = '\WebShare'
    TemplateDir = 'c:\wwwroot\templates'
    DefaultDoc = 'index.html'
    LingerOnOff = wsLingerNoSet
    LingerTimeout = 1
    Options = []
    OnServerStarted = HttpServer1ServerStarted
    OnServerStopped = HttpServer1ServerStopped
    OnClientConnect = HttpServer1ClientConnect
    OnClientDisconnect = HttpServer1ClientDisconnect
    OnGetDocument = HttpServer1GetDocument
    OnHeadDocument = HttpServer1HeadDocument
    OnPostDocument = HttpServer1PostDocument
    OnPostedData = HttpServer1PostedData
    OnAuthGetPassword = HttpServer1AuthGetPassword
    OnAuthResult = HttpServer1AuthResult
    OnAuthGetType = HttpServer1AuthGetType
    OnAuthNtlmBeforeValidate = HttpServer1AuthNtlmBeforeValidate
    AuthTypes = []
    AuthRealm = 'ics'
    Left = 32
    Top = 131
  end
end
