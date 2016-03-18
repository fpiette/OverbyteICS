unit ProxyDlg;

interface

uses
    WinTypes, WinProcs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
    Dialogs, StdCtrls, Buttons, ExtCtrls;

type
    TProxyForm = class(TForm)
        ProxyEdit : TEdit;
        PortEdit : TEdit;
        Label1 : TLabel;
        Label2 : TLabel;
        OKBurron : TBitBtn;
        CancelButton : TBitBtn;
        ProxyUsername : TEdit;
        ProxyPassword : TEdit;
        Label3 : TLabel;
        Label4 : TLabel;
        Label5: TLabel;
        UserAgent: TEdit;
        lbl1: TLabel;
        lbl2: TLabel;
        SslVersionList: TComboBox;
        SslAcceptableHostsEdit: TEdit;
        SslVerifyCertMode: TRadioGroup;
        SslRevokeCheck: TCheckBox;
        SslReportChain: TCheckBox;
    private
        { Private declarations }
    public
        { Public declarations }
    end;

var
    ProxyForm : TProxyForm;

implementation

{$R *.DFM}

end.
