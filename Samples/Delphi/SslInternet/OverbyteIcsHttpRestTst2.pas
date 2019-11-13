unit OverbyteIcsHttpRestTst2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, TypInfo, OverbyteIcsSuperObject;

type
  TFormObject = class(TForm)
    SubRespList: TListView;
    procedure SubRespListDblClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure DispJson(const JsonStr: WideString);
  end;

var
  FormObject: TFormObject;

implementation

{$R *.dfm}

Uses OverbyteIcsHttpRestTst1;

procedure TFormObject.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    Action := caHide;
end;

procedure TFormObject.SubRespListDblClick(Sender: TObject);
begin
    if SubRespList.ItemIndex < 0 then Exit;
    with SubRespList.Items[SubRespList.ItemIndex] do begin
        if SubItems.Count < 2 then Exit;
        if (SubItems[0] = 'stArray') or (SubItems[0] = 'stObject') then
            DispJson(SubItems[1]);
    end;
end;

procedure TFormObject.DispJson(const JsonStr: WideString);
var
    CVal: String;
    JsonObj, JsonRow: ISuperObject;
    JsonEnum: TSuperAvlIterator;
    JsonItem: TSuperAvlEntry;
    FirstCol, FirstRow: Boolean;
    I, tot: Integer;
begin
    try
        if (Pos ('{', JsonStr) <> 1) and (Pos ('[', JsonStr) <> 1) then Exit;
        JsonObj := TSuperObject.ParseString(PWideChar(JsonStr), True);
        SubRespList.Items.Clear;
        Visible := True;
        BringToFront; 
        if JsonObj.DataType = stArray then begin
            tot := JsonObj.AsArray.Length;
            if tot = 0 then Exit;
            with SubRespList do begin
                Items.BeginUpdate;
                Columns.Clear;
                FirstRow := True;
                for I := 0 to tot - 1 do begin
                    JsonRow := JsonObj.AsArray[I];
                    FirstCol := True;
                    with Items.Add do begin
                        if JsonRow.DataType = stObject then begin
                            JsonEnum := JsonRow.AsObject.GetEnumerator;
                            while JsonEnum.MoveNext do begin
                                JsonItem := JsonEnum.GetIter;
                                if NOT Assigned(JsonItem) then continue;
                                CVal := JsonItem.Value.AsString;
                                if FirstRow then begin
                                    with Columns.Add do begin
                                        Caption := JsonItem.Name;
                                        Width := 250;
                                    end;
                                end;
                                if FirstCol then
                                    Caption := CVal
                                else
                                    SubItems.Add(CVal);
                                FirstCol := False;
                            end;
                        end

                     // not Json object, single column
                        else begin
                            if FirstRow then begin
                                with Columns.Add do begin
                                    Caption := 'Value';
                                    Width := 600;
                                end;
                            end;
                            Caption := JsonRow.AsString;
                        end;
                    end;
                    FirstRow := False;
                end;
                Items.EndUpdate;
            end;
        end;

        if JsonObj.DataType = stObject then begin
         // note that values containing objects are displayed as raw Json
            with SubRespList do begin
                Columns.Clear;
                with Columns.Add do begin
                    Caption := 'Name';
                    Width := 100;
                end;
                with Columns.Add do begin
                    Caption := 'Type';
                    Width := 70;
                end;
                with Columns.Add do begin
                    Caption := 'Value';
                    Width := 600;
                end;
                with Columns.Add do begin
                    Caption := '';
                    Width := 2100;
                end;
                JsonEnum := JsonObj.AsObject.GetEnumerator;
                try
                    while JsonEnum.MoveNext do begin
                        JsonItem := JsonEnum.GetIter;
                        with Items.Add do begin
                            Caption := JsonItem.Name;
                            SubItems.Add(GetEnumName(TypeInfo(TSuperType),
                                                    Ord(JsonItem.Value.DataType)));
                            SubItems.Add(JsonItem.Value.AsString);
                        end;
                    end;
                finally
                    JsonEnum.Free;
                end;
            end;
        end;
    except
        on E:Exception do
             HttpRestForm.AddLog('Error parsing Json: ' + E.Message);
    end;
end;


end.
