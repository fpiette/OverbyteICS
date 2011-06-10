{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       Arno Garrels <arno.garrels@gmx.de>
Description:  Delphi 2009 and 2010 do not update old projects correctly.
              This IDE plugin reads OutputDir, UnitOutputDir, SearchPath and
              Conditionals from the .dof file on project updates from .dpr if
              no .dproj file already exists and sets these in new project's
              Base configuration.
Creation:     June 2011
Version:      1.00
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2011 by Arno Garrels
              Berlin, Germany <arno.garrels@gmx.de>

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
                 to François PIETTE. Use a nice stamp and mention your name,
                 street address, EMail address and any comment you like to say.

History:


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsDprUpdFix;

{.$DEFINE DEBUGLOG}
{$I OverbyteIcsDefs.inc}

interface

uses
    SysUtils, Classes, Forms, IniFiles, TypInfo, ToolsApi;

type
    TIdeNotifier = class(TNotifierObject, IOTAIDENotifier)
    private
        FIni : TIniFile;
        FLastDpr : string;
        FPossibleUpd: Boolean;
    protected
        procedure AfterCompile(Succeeded: Boolean);
        procedure BeforeCompile(const Project: IOTAProject; var Cancel: Boolean);
        procedure FileNotification(NotifyCode: TOTAFileNotification;
            const FileName: string; var Cancel: Boolean);
    end;

procedure Register;

implementation

uses
    DCCStrs;

var
    IDENotifierIndex : Integer = -1;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure DebugLog(const Msg: string);
begin
    (BorlandIDEServices as IOTAMessageServices).AddTitleMessage(Msg);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Register;
var
    Services : IOTAServices;
begin
{$IFDEF COMPILER12_UP}
{$IFNDEF COMPILER15_UP}
    Services := BorlandIDEServices as IOTAServices;
    if (Services <> nil) and (IDENotifierIndex = -1) then
    begin
        IDENotifierIndex := Services.AddNotifier(TIdeNotifier.Create);
    {$IFDEF DEBUGLOG}
        DebugLog('OverbyteIcsDprUpdFix Installed NotifierIndex: #' + IntToStr(IDENotifierIndex));
    {$ENDIF}
    end;
{$ENDIF}
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsProjectFile(const FileName: string;
  var Project : IOTAProject): Boolean;
var
    Module  : IOTAModule;
    ProjectGroup : IOTAProjectGroup;
begin
    Module := (BorlandIDEServices as IOTAModuleServices).FindModule(FileName);
    Result := Supports(Module, IOTAProject, Project) and
              not Supports(Module, IOTAProjectGroup, ProjectGroup);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIdeNotifier.AfterCompile(Succeeded: Boolean);
begin
{$IFDEF DEBUGLOG}
    DebugLog('After Compile');
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIdeNotifier.BeforeCompile(const Project: IOTAProject;
  var Cancel: Boolean);
begin
{$IFDEF DEBUGLOG}
    DebugLog('Before Compile');
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TIdeNotifier.FileNotification(NotifyCode: TOTAFileNotification;
  const FileName: string; var Cancel: Boolean);
const
    sDofSectDir = 'Directories';
var
    Project               : IOTAProject;
    OptionsConfigurations : IOTAProjectOptionsConfigurations;
    BaseConfig            : IOTABuildConfiguration;
    IniValues             : TStringList;
    Values                : TStringList;
    RS                    : array of string;
    S1                    : string;
    S2                    : string;
    Dirty                 : Boolean;
    I                     : Integer;
    DofFile               : string;
begin
    try
    {$IFDEF DEBUGLOG}
        DebugLog(Format('%s: %s',
         [GetEnumName(TypeInfo(TOTAFileNotification), Ord(NotifyCode)), FileName]));
    {$ENDIF}
        case NotifyCode of
            ofnFileOpening :
                begin
                    if (not FPossibleUpd) and (ExtractFileExt(FileName) = '.dpr') then
                    begin
                        FLastDpr := ChangeFileExt(FileName, '.dproj');
                        if FileExists(FLastDpr) then
                        begin
                            FLastDpr     := '';
                            FPossibleUpd := False;
                        end
                        else
                            FPossibleUpd := True;
                    end;
                end;

            ofnFileOpened :
                begin
                    if FPossibleUpd and (FLastDpr = FileName) then
                    begin
                        FLastDpr      := '';
                        FPossibleUpd  := False;
                        DofFile       := ChangeFileExt(FileName, '.dof');
                        Dirty         := False;
                        Values        := nil;
                        IniValues     := nil;
                        FIni          := nil;
                        if FileExists(DofFile) and IsProjectFile(FileName, Project) and
                          Supports(Project.ProjectOptions,
                                   IOTAProjectOptionsConfigurations,
                                   OptionsConfigurations) then
                        try
                            BaseConfig  := OptionsConfigurations.BaseConfiguration;
                            if BaseConfig = nil then // Should never happen
                                Exit;
                            FIni := TIniFile.Create(DofFile);
                            IniValues := TStringList.Create;
                            IniValues.Delimiter := ';';
                            IniValues.StrictDelimiter := True;
                            Values := TStringList.Create;

                            //-----------------------------------
                            IniValues.DelimitedText := FIni.ReadString(sDofSectDir, 'SearchPath', '');
                            if IniValues.Count > 0 then
                            begin
                                BaseConfig.GetValues(sUnitSearchPath, Values, False);
                                for I := IniValues.Count - 1 downto 0 do
                                begin
                                    if Values.IndexOf(Trim(IniValues[I])) > 0 then
                                        IniValues.Delete(I);
                                end;
                                if IniValues.Count > 0 then
                                begin
                                    SetLength(RS, IniValues.Count);
                                    for I := 0 to IniValues.Count - 1 do
                                        RS[I] := Trim(IniValues[I]);
                                    BaseConfig.InsertValues(sUnitSearchPath, RS);
                                    Dirty := True;
                                    (BorlandIDEServices as IOTAMessageServices).AddWideTitleMessage(
                                      'ICS UpdateFix from .dof: Base SearchPath');
                                end;
                            end;

                            //-----------------------------------
                            IniValues.DelimitedText := FIni.ReadString(sDofSectDir, 'Conditionals', '');
                            if IniValues.Count > 0 then
                            begin
                                Values.Clear;
                                BaseConfig.GetValues(sDefine, Values, False);
                                for I := IniValues.Count - 1 downto 0 do
                                begin
                                    if Values.IndexOf(Trim(IniValues[I])) > 0 then
                                        IniValues.Delete(I);
                                end;
                                if IniValues.Count > 0 then
                                begin
                                    SetLength(RS, IniValues.Count);
                                    for I := 0 to IniValues.Count - 1 do
                                        RS[I] := Trim(IniValues[I]);
                                    BaseConfig.InsertValues(sDefine, RS);
                                    Dirty := True;
                                    (BorlandIDEServices as IOTAMessageServices).AddWideTitleMessage(
                                      'ICS UpdateFix from .dof: Base Conditionals');
                                end;
                            end;

                            //-----------------------------------
                            S2 := Trim(FIni.ReadString(sDofSectDir, 'OutputDir', ''));
                            if S2 <> '' then
                            begin
                                S1 := BaseConfig.GetValue(sExeOutput, False);
                                if S1 <> S2 then
                                begin
                                    BaseConfig.Value[sExeOutput] := S2;
                                    Dirty := True;
                                    (BorlandIDEServices as IOTAMessageServices).AddWideTitleMessage(
                                      'ICS UpdateFix from .dof: Base OutputDir');
                                end;
                            end;

                            //-----------------------------------
                            S2 := Trim(FIni.ReadString(sDofSectDir, 'UnitOutputDir', ''));
                            if S2 <> '' then
                            begin
                                S1 := BaseConfig.GetValue(sDcuOutput, False);
                                if S1 <> S2 then
                                begin
                                    BaseConfig.Value[sDcuOutput] := S2;
                                    Dirty := True;
                                    (BorlandIDEServices as IOTAMessageServices).AddWideTitleMessage(
                                      'ICS UpdateFix from .dof: Base UnitOutputDir');
                                end;
                            end;

                            //-----------------------------------
                            if Dirty then
                                Project.Save(False, True);

                        finally
                            FIni.Free;
                            Values.Free;
                            IniValues.Free;
                        end;
                    end;
                end;
        end;
    except
        FLastDpr := '';
        Application.HandleException(Self);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure RemoveIDENotifier;
var
    Services : IOTAServices;
begin
    if IDENotifierIndex > -1 then
    begin
        Services := BorlandIDEServices as IOTAServices;
        if Services <> nil then
            Services.RemoveNotifier(IDENotifierIndex);
        IDENotifierIndex := -1;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
initialization

finalization
    RemoveIDENotifier;

end.

