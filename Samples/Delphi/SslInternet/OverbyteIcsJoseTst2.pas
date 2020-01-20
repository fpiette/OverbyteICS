{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Angus Robertson, Magenta Systems Ltd
Description:  ICS HTTPS REST functions demo display form.
Creation:     Nov 2019
Updated:      Dec 2019
Version:      8.64
Support:      Use the mailing list ics-ssl@elists.org
Legal issues: Copyright (C) 2003-2019 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium.
              <francois.piette@overbyte.be>
              SSL implementation includes code written by Arno Garrels,
              Berlin, Germany, contact: <arno.garrels@gmx.de>

              This software is provided 'as-is', without any express or
              implied warranty.  In no event will the author be held liable
              for any  damages arising from the use of this software.

              Permission is granted to anyone to use this software for any
              purpose, including commercial applications, and to alter it
              and redistribute it freely, subject to the following
              restrictions:

              1. The origin of this software must not be misrepresented,
                 you must not claim that you wrote the original software.
                 If you use this software in a product, an acknowledgment
                 in the product documentation would be appreciated but is
                 not required.

              2. Altered source versions must be plainly marked as such, and
                 must not be misrepresented as being the original software.

              3. This notice may not be removed or altered from any source
                 distribution.

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

History:
Nov 11, 2019 - V8.63 Basline
Dec 09, 2019 - V8.64 Allow clicking on nested arrays.



 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsJoseTst2;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, TypInfo, OverbyteIcsSuperObject;

type
  TFormObject = class(TForm)
    SubJsonGrid: TListView;
    procedure SubJsonGridDblClick(Sender: TObject);
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

Uses OverbyteIcsJoseTst1;

procedure TFormObject.FormClose(Sender: TObject; var Action: TCloseAction);
begin
    Action := caHide;
end;

procedure TFormObject.SubJsonGridDblClick(Sender: TObject);
begin
    if SubJsonGrid.ItemIndex < 0 then Exit;
    with SubJsonGrid.Items[SubJsonGrid.ItemIndex] do begin
        if SubItems.Count >= 2 then begin
            if (SubItems[0] = 'stArray') or (SubItems[0] = 'stObject') then { objects }
                DispJson(SubItems[1])
        end;
        if (Pos ('{', Caption) = 1) or (Pos ('[', Caption) = 1) then   { V8.64 arrays }
            DispJson(Caption);
    end;
end;

procedure TFormObject.DispJson(const JsonStr: WideString);
var
    CVal: String;
    JsonObj, JsonRow: ISuperObject;
    JsonEnum: TSuperAvlIterator;
    JsonItem: TSuperAvlEntry;
    FirstCol, FirstRow: Boolean;
    I, tot, CWid: Integer;
begin
    try
        if (Pos ('{', JsonStr) <> 1) and (Pos ('[', JsonStr) <> 1) then Exit;
        JsonObj := TSuperObject.ParseString(PWideChar(JsonStr), False);
        SubJsonGrid.Items.Clear;
        if NOT Assigned(JsonObj) then begin
            JsonDemoForm.AddLog('Failed to parse Json');
            Exit;
        end;
        Visible := True;
        BringToFront;
        if JsonObj.DataType = stArray then begin
            tot := JsonObj.AsArray.Length;
            if tot = 0 then Exit;
            with SubJsonGrid do begin
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
                                CWid := (Length(CVal) * 5) + 30;
                                if CWid > 400 then CWid := 400;
                                if FirstRow then begin
                                    with Columns.Add do begin
                                        Caption := JsonItem.Name;
                                        Width := CWid;
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
                            CVal := JsonRow.AsString;
                            if FirstRow then begin
                                with Columns.Add do begin
                                    Caption := 'Value';
                                    Width := 1000;
                                end;
                            end;
                            Caption := CVal;
                        end;
                    end;
                    FirstRow := False;
                end;
                Items.EndUpdate;
            end;
        end;

        if JsonObj.DataType = stObject then begin
         // note that values containing objects are displayed as raw Json
            with SubJsonGrid do begin
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
                    Width := 1000;
                end;
                with Columns.Add do begin
                    Caption := '';
                    Width := 100;
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
             JsonDemoForm.AddLog('Error parsing Json: ' + E.Message);
    end;
end;


end.
