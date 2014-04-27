object frmPemTool1: TfrmPemTool1
  Left = 212
  Top = 124
  Width = 851
  Height = 607
  Color = clBtnFace
  Constraints.MinHeight = 379
  Constraints.MinWidth = 527
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Position = poScreenCenter
  Scaled = False
  ShowHint = True
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    843
    553)
  PixelsPerInch = 96
  TextHeight = 14
  object btnShowCert: TButton
    Left = 764
    Top = 24
    Width = 75
    Height = 21
    Anchors = [akTop, akRight]
    Caption = '&View PEM'
    TabOrder = 1
    OnClick = btnShowCertClick
  end
  object PageControl1: TPageControl
    Left = 2
    Top = 2
    Width = 760
    Height = 496
    ActivePage = TabCertLv
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    OnChange = PageControl1Change
    object TabCertLv: TTabSheet
      Caption = 'Certificates'
      DesignSize = (
        752
        467)
      object Label4: TLabel
        Left = 4
        Top = 446
        Width = 47
        Height = 14
        Anchors = [akLeft, akBottom]
        Caption = 'Directory:'
      end
      object LvCerts: TListView
        Left = 4
        Top = 6
        Width = 744
        Height = 430
        Anchors = [akLeft, akTop, akRight, akBottom]
        Columns = <
          item
            AutoSize = True
            Caption = 'Common Name'
          end
          item
            AutoSize = True
            Caption = 'Issued to'
          end
          item
            AutoSize = True
            Caption = 'Issuer'
          end
          item
            Caption = 'Expires at'
            Width = 70
          end
          item
            Caption = 'File Name'
            Width = 140
          end>
        ReadOnly = True
        RowSelect = True
        PopupMenu = pmLv
        SmallImages = ImageList1
        SortType = stData
        TabOrder = 0
        ViewStyle = vsReport
        OnColumnClick = LvCertsColumnClick
        OnCompare = LvCertsCompare
        OnCustomDraw = LvCertsCustomDraw
        OnDblClick = LvCertsDblClick
      end
      object btnRefresh: TButton
        Left = 677
        Top = 443
        Width = 71
        Height = 21
        Anchors = [akRight, akBottom]
        Caption = '&Refresh'
        TabOrder = 4
        OnClick = btnRefreshClick
      end
      object CurrentCertDirEdit: TEdit
        Left = 54
        Top = 442
        Width = 420
        Height = 22
        Anchors = [akLeft, akRight, akBottom]
        TabOrder = 1
        Text = 'CurrentCertDirEdit'
        OnChange = CurrentCertDirEditChange
      end
      object btnDeleteCert: TButton
        Left = 597
        Top = 443
        Width = 75
        Height = 21
        Anchors = [akRight, akBottom]
        Caption = '&Delete'
        TabOrder = 3
        OnClick = btnDeleteCertClick
      end
      object btnCopyCert: TButton
        Left = 517
        Top = 442
        Width = 75
        Height = 21
        Anchors = [akRight, akBottom]
        Caption = '&Copy'
        TabOrder = 2
        OnClick = btnCopyCertClick
      end
      object SelCurrDir: TBitBtn
        Left = 480
        Top = 441
        Width = 31
        Height = 25
        Anchors = [akRight, akBottom]
        TabOrder = 5
        OnClick = SelCurrDirClick
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
    end
    object TabImport: TTabSheet
      Caption = 'Import Certificates'
      ImageIndex = 1
      DesignSize = (
        752
        467)
      object Bevel2: TBevel
        Left = 5
        Top = 305
        Width = 744
        Height = 64
        Anchors = [akLeft, akTop, akRight]
        Shape = bsFrame
      end
      object Bevel1: TBevel
        Left = 5
        Top = 3
        Width = 744
        Height = 283
        Anchors = [akLeft, akTop, akRight]
        Shape = bsFrame
      end
      object Label1: TLabel
        Left = 20
        Top = 28
        Width = 327
        Height = 56
        Caption = 
          'Current user'#39's Windows-System-Certificate-Store is opened.'#13#10'Then' +
          ' the DER formated certs are read and translated to PEM format.'#13#10 +
          'Certs are stored to the specified folder in the form of Hash.0.'#13 +
          #10'The '#39'Cert. Store Type'#39' box has static values: CA, ROOT, MY.'
      end
      object Label3: TLabel
        Left = 22
        Top = 94
        Width = 82
        Height = 14
        Caption = 'Cert. Store Type:'
      end
      object Label2: TLabel
        Left = 22
        Top = 118
        Width = 75
        Height = 14
        Caption = 'Destination Dir.:'
      end
      object Label5: TLabel
        Left = 20
        Top = 12
        Width = 338
        Height = 14
        Caption = 'Import a Windows Ceritificate Store to a Folder in PEM Format'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object Label6: TLabel
        Left = 20
        Top = 319
        Width = 26
        Height = 14
        Caption = 'Misc'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Arial'
        Font.Style = [fsBold]
        ParentFont = False
      end
      object ComboBoxStoreType: TComboBox
        Left = 106
        Top = 90
        Width = 227
        Height = 22
        Hint = 'Select a Windows store type'
        Style = csDropDownList
        ItemHeight = 14
        TabOrder = 0
        Items.Strings = (
          'Certificate Authorities'
          'Root Certificate Authorities'
          'My Own Certificates')
      end
      object DestDirEdit: TEdit
        Left = 106
        Top = 114
        Width = 265
        Height = 22
        Hint = 'Existing destination directory '
        TabOrder = 1
        Text = 'DestDirEdit'
        OnChange = DestDirEditChange
      end
      object CheckBoxWarnDestNotEmpty: TCheckBox
        Left = 106
        Top = 145
        Width = 243
        Height = 17
        Caption = 'Warn me if destination folder is not empty'
        Checked = True
        State = cbChecked
        TabOrder = 2
      end
      object CheckBoxOverwriteExisting: TCheckBox
        Left = 106
        Top = 165
        Width = 243
        Height = 17
        Hint = 
          'If enabled, existing certs with the same name are overwritten.'#13#10 +
          'If not enabled, file extensions are changed. '#13#10'(e.g. 9d66eef0.0,' +
          ' 9d66eef0.1 etc)'
        Caption = 'Overwrite existing files, don'#39't change file ext.'
        TabOrder = 3
      end
      object CheckBoxEmptyDestDir: TCheckBox
        Left = 106
        Top = 185
        Width = 243
        Height = 17
        Hint = 'Warning! - deletes any file in destination folder '
        Caption = 'Empty destination directory'
        TabOrder = 4
      end
      object btnImport: TButton
        Left = 104
        Top = 252
        Width = 229
        Height = 21
        Caption = 'Start import from Windows'
        TabOrder = 6
        OnClick = btnImportClick
      end
      object CheckBoxWriteToBundle: TCheckBox
        Left = 106
        Top = 205
        Width = 145
        Height = 17
        Caption = 'Create a CA bundle file'
        TabOrder = 5
      end
      object SelImpDir: TBitBtn
        Left = 388
        Top = 113
        Width = 31
        Height = 25
        TabOrder = 7
        OnClick = SelImpDirClick
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
      object btnImportPemFile: TButton
        Left = 16
        Top = 339
        Width = 231
        Height = 21
        Caption = 'Import/Hash a PEM Cert File to Destination Dir.'
        TabOrder = 8
        OnClick = btnImportPemFileClick
      end
      object CheckBoxComment: TCheckBox
        Left = 106
        Top = 225
        Width = 145
        Height = 17
        Caption = 'Add Comments to file'
        TabOrder = 9
      end
    end
  end
  object About: TButton
    Left = 764
    Top = 477
    Width = 75
    Height = 21
    Anchors = [akRight, akBottom]
    Caption = '&About'
    TabOrder = 2
    OnClick = AboutClick
  end
  object ProgressBar1: TProgressBar
    Left = 764
    Top = 4
    Width = 73
    Height = 16
    Anchors = [akTop, akRight]
    TabOrder = 3
    Visible = False
  end
  object pmLv: TPopupMenu
    Left = 74
    Top = 188
    object pmShowDetails: TMenuItem
      Caption = 'Show Details'
      OnClick = LvCertsDblClick
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object pmCopy: TMenuItem
      Caption = 'Copy Certificate'
      OnClick = btnCopyCertClick
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object pmDelete: TMenuItem
      Caption = 'Delete Certificate'
      OnClick = btnDeleteCertClick
    end
  end
  object ImageList1: TImageList
    Left = 40
    Top = 186
    Bitmap = {
      494C010103001400140010001000FFFFFFFFFF10FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001001000000000000008
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000002000200020002000200020002
      0002000200020002000200020002000200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000002104218631863186310421863
      1863186318631042186318631042000200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000002186300000000E07F00000000
      000000000000E07F000000001863000200000000000000000000000000000000
      1042FF7F00000000000000000000000000000000000000000000FF7FFF7FFF7F
      FF7FFF7FFF7FFF7FFF7F00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000002186300000000100010001000
      1000100010001000100010001863000200000000000000000000000000001042
      00000000FF7F0000000000000000000000000000000000000000104200000000
      0000000000000000FF7F00000000000000000000000000000000000000000000
      00000000000000000000000000000000000000021042E07F0000000000000000
      0000E07F00000000000000001042000200000000000000000000000000001042
      00000000FF7F0000000000000000000000000000000000000000000010420000
      000000000000FF7F000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000002186300000000E07F00000000
      1000000000001000E07F00001863000200000000000000000000000010420000
      000000000000FF7F000000000000000000000000000000000000000010420000
      000000000000FF7F000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000002186300001863004218631000
      E07F186310000000100000001863000200000000000000000000000010420000
      000000000000FF7F000000000000000000000000000000000000000000001042
      00000000FF7F0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000002104200000042104200420000
      1000000000001863000000001042000200000000000000000000104200000000
      0000000000000000FF7F00000000000000000000000000000000000000001042
      00000000FF7F0000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000002186300001863004218630000
      0000000000000000E07F00001863000200000000000000000000104210421042
      1042104210421042FF7F00000000000000000000000000000000000000000000
      1042FF7F00000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000021863E07F000000000000E07F
      00000000E07F0000000000001863000200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000002104218631863186318631042
      1863186318631042186318631042000200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000002000200020002000200020002
      0002000200020002000200020002000200000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF00FFFFFFFFFFFF0000FFFFFFFFFFFF0000
      FFFFFFFFFFFF00008000FFFFFFFF00000000FFFFFFFF00000000FFFFFFFF0000
      37D8FE7FF00F00003000FDBFF7EF00001F78FDBFFBDF000036C8FBDFFBDF0000
      2028FBDFFDBF000022D8F7EFFDBF000023E8F00FFE7F00001DB8FFFFFFFF0000
      0000FFFFFFFF00000001FFFFFFFF000000000000000000000000000000000000
      000000000000}
  end
  object OpenDlg: TOpenDialog
    Filter = 'All Files *.*|*.*|PEM Files *.pem|*.pem'
    Options = [ofHideReadOnly, ofNoChangeDir, ofEnableSizing]
    Left = 48
    Top = 232
  end
  object MainMenu1: TMainMenu
    Left = 254
    Top = 2
    object MMFile: TMenuItem
      Caption = '&File'
      object MMFileExit: TMenuItem
        Caption = '&Exit'
        OnClick = MMFileExitClick
      end
    end
    object MMExtras: TMenuItem
      Caption = '&Extras'
      object MMExtrasCreateSelfSignedCert: TMenuItem
        Caption = 'Create a self-signed certificate..'
        OnClick = MMExtrasCreateSelfSignedCertClick
      end
      object MMExtrasCreateCertRequest: TMenuItem
        Caption = 'Create a certificate request..'
        OnClick = MMExtrasCreateCertRequestClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object MMExtrasEncryptStringRSA: TMenuItem
        Caption = 'RSA encrypt/decrypt..'
        OnClick = MMExtrasEncryptStringRSAClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object MMExtrasEncryptStringBlowfish: TMenuItem
        Caption = 'Blowfish encrypt/decrypt string'
        OnClick = MMExtrasEncryptStringBlowfishClick
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object MMExtrasEncryptStreamBlowfish: TMenuItem
        Caption = 'Blowfish encrypt/decrypt stream'
        OnClick = MMExtrasEncryptStreamBlowfishClick
      end
      object N6: TMenuItem
        Caption = '-'
      end
      object MMExtrasEncryptFileBlowfish: TMenuItem
        Caption = 'Blowfish encrypt file..'
        OnClick = MMExtrasEncryptFileBlowfishClick
      end
      object MMExtrasDecryptFileBlowfish: TMenuItem
        Caption = 'Blowfish decrypt file..'
        OnClick = MMExtrasDecryptFileBlowfishClick
      end
    end
  end
  object OpenDirDiag: TOpenDialog
    Options = [ofHideReadOnly, ofNoValidate, ofPathMustExist, ofNoTestFileCreate, ofEnableSizing]
    Title = 'Select Certificate Directory'
    Left = 85
    Top = 230
  end
end
