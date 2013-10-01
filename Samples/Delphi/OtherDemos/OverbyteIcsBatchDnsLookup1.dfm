object BatchDnsLookupForm: TBatchDnsLookupForm
  Left = 202
  Top = 118
  BorderStyle = bsToolWindow
  Caption = 'ICS batch async DNS lookup new API (IPv6 and IPv4)'
  ClientHeight = 363
  ClientWidth = 311
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    311
    363)
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 298
    Width = 31
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Min. #'
  end
  object Label2: TLabel
    Left = 69
    Top = 298
    Width = 35
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Max. #'
  end
  object Label3: TLabel
    Left = 8
    Top = 279
    Width = 295
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Lookup threads shared by all instances in this thread context '
  end
  object Label4: TLabel
    Left = 8
    Top = 259
    Width = 149
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Number of TWSocket instances'
  end
  object Label5: TLabel
    Left = 8
    Top = 318
    Width = 62
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'SocketFamily'
  end
  object Label6: TLabel
    Left = 132
    Top = 298
    Width = 157
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '(Max - Min time out after 60 sec)'
  end
  object StartButton: TButton
    Left = 223
    Top = 332
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = 'Start'
    TabOrder = 6
    OnClick = StartButtonClick
  end
  object DnsNamesMemo: TMemo
    Left = 8
    Top = 8
    Width = 145
    Height = 240
    Anchors = [akLeft, akTop, akBottom]
    Lines.Strings = (
      'DnsNamesMemo')
    TabOrder = 0
    WordWrap = False
  end
  object ResultMemo: TMemo
    Left = 157
    Top = 8
    Width = 145
    Height = 240
    Anchors = [akLeft, akTop, akBottom]
    Lines.Strings = (
      'ResultMemo')
    ReadOnly = True
    TabOrder = 1
    WordWrap = False
  end
  object MinEdit: TEdit
    Left = 40
    Top = 295
    Width = 23
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 3
    Text = '0'
  end
  object MaxEdit: TEdit
    Left = 105
    Top = 295
    Width = 23
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 4
    Text = '2'
  end
  object InstancesEdit: TEdit
    Left = 159
    Top = 256
    Width = 36
    Height = 21
    Anchors = [akLeft, akBottom]
    TabOrder = 2
    Text = '3'
  end
  object SocketFamilyComboBox: TComboBox
    Left = 8
    Top = 334
    Width = 157
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akBottom]
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 5
    Text = 'sfAny'
    OnChange = SocketFamilyComboBoxChange
    Items.Strings = (
      'sfAny'
      'sfAnyIPv4'
      'sfAnyIPv6'
      'sfIPv4 (old API)'
      'sfIPv6')
  end
end
