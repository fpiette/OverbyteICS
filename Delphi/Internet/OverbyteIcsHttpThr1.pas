{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE (From a work done by Ed Hochman <ed@mbhsys.com>)
Creation:     Jan 13, 1998
Version:      1.00
Description:  HttpThrd is a demo program showing how to use THttpCli component
              in a multi-threaded program.
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1997-2010 by François PIETTE
              Rue de Grady 24, 4053 Embourg, Belgium. Fax: +32-4-365.74.56
              <francois.piette@overbyte.be>

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

Updates:

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsHttpThr1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  OverbyteIcsHttpProt, StdCtrls, OverbyteIcsIniFiles;


type
  TThreadState = (tsInexistant, tsReady, tsInUse);

  THttpThreadForm = class(TForm)
    URLEdit: TEdit;
    ResultsMemo: TMemo;
    DoItButton: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Thread0Label: TLabel;
    Thread1Label: TLabel;
    Thread2Label: TLabel;
    Thread3Label: TLabel;
    Thread4Label: TLabel;
    Thread5Label: TLabel;
    ProgressListBox: TListBox;
    Label14: TLabel;
    ProxyEdit: TEdit;
    Label15: TLabel;
    procedure DoItButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FInitialized : Boolean;
    FIniFileName : String;
    procedure CreateThread(Sender: TObject);
    procedure SetThreadState(Which : Integer; State: TThreadState);
  public
    procedure ProcessResults(ThreadNumber: Integer; Success : Boolean);
  end;

const
    SectionData   = 'Data';
    KeyURL        = 'URL';
    KeyProxy      = 'Proxy';
    SectionWindow = 'Window';
    KeyTop        = 'Top';
    KeyLeft       = 'Left';
    KeyWidth      = 'Width';
    KeyHeight     = 'Height';

var
    HttpThreadForm: THttpThreadForm;

implementation

{$R *.DFM}

uses
    OverbyteIcsHttpThr2,             // The thread class is defined there
    OverbyteIcsStreams;

const
    MaxThreads    = 6;    // If you change this, change labels on the form
var
    // The array with all our threads components
    ThreadsObjects : array [0..MaxThreads - 1] of THTTPThread;
    // The array with all thread states
    ThreadsState   : array [0..MaxThreads - 1] of TThreadState;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpThreadForm.FormCreate(Sender: TObject);
var
    i: Integer;
begin
    FIniFileName := GetIcsIniFileName;
    for i := Low(ThreadsObjects) to High(ThreadsObjects) do
        SetThreadState(i, tsInexistant);  //Not created.
    ResultsMemo.Clear;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpThreadForm.FormShow(Sender: TObject);
var
    IniFile : TIcsIniFile;
begin
    if not FInitialized then begin
        FInitialized   := TRUE;
        IniFile        := TIcsIniFile.Create(FIniFileName);
        try
            URLEdit.Text   := IniFile.ReadString(SectionData, KeyURL,
                                             'http://www.rtfm.be/fpiette');
            ProxyEdit.Text := IniFile.ReadString(SectionData, KeyProxy, '');
            Top            := IniFile.ReadInteger(SectionWindow, KeyTop,    Top);
            Left           := IniFile.ReadInteger(SectionWindow, KeyLeft,   Left);
            Width          := IniFile.ReadInteger(SectionWindow, KeyWidth,  Width);
            Height         := IniFile.ReadInteger(SectionWindow, KeyHeight, Height);
        finally
            IniFile.Free;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpThreadForm.FormClose(Sender: TObject;
  var Action: TCloseAction);
var
    IniFile : TIcsIniFile;
begin
    IniFile := TIcsIniFile.Create(FIniFileName);
    try
        IniFile.WriteString(SectionData, KeyURL,       URLEdit.Text);
        IniFile.WriteString(SectionData, KeyProxy,     proxyEdit.Text);
        IniFile.WriteInteger(SectionWindow, KeyTop,    Top);
        IniFile.WriteInteger(SectionWindow, KeyLeft,   Left);
        IniFile.WriteInteger(SectionWindow, KeyWidth,  Width);
        IniFile.WriteInteger(SectionWindow, KeyHeight, Height);
        IniFile.UpdateFile;
    finally
        IniFile.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpThreadForm.DoItButtonClick(Sender: TObject);
var
    i: Integer;
begin
    for i := Low(ThreadsObjects) to High(ThreadsObjects) do begin
        if ThreadsState[i] = tsInexistant then  //Thread has not been created yet
            CreateThread(Self);

        if ThreadsState[i] = tsReady then begin  //Thread is ready for use
            with ThreadsObjects[i] do begin
                FURL     := UrlEdit.Text;
                FProxy   := ProxyEdit.Text;
                SetThreadState(i, tsInUse);   // In use
                //get the page
            {$if RTLVersion >= 21}
                Start;
            {$else}
                Resume;
            {$ifend}
                Exit;    //For now, only start one thread for each click of DoIt
            end;
        end;
    end;
    MessageBeep(MB_OK);
    ShowMessage('No more threads available');
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpThreadForm.CreateThread(Sender: TObject);
var
    i: Integer;
begin
    for i := Low(ThreadsObjects) to High(ThreadsObjects) do begin
        if ThreadsObjects[i] = nil then begin
            ThreadsObjects[i] := THTTPThread.Create(True);
            ThreadsObjects[i].Setup(i);   //Create the HTTP object
            SetThreadState(i, tsReady);
            Exit;  //Found and activated an unused thread
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure THttpThreadForm.SetThreadState(Which : Integer; State: TThreadState);
begin
    ThreadsState[Which] := State;
    case which of
        0: Thread0Label.Caption := IntToStr(Ord(State));
        1: Thread1Label.Caption := IntToStr(Ord(State));
        2: Thread2Label.Caption := IntToStr(Ord(State));
        3: Thread3Label.Caption := IntToStr(Ord(State));
        4: Thread4Label.Caption := IntToStr(Ord(State));
        5: Thread5Label.Caption := IntToStr(Ord(State));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure LoadMemoFromMemoryStream(Memo : TMemo; Stream : TMemoryStream);
var
    Reader : TIcsStreamReader;
    Line : String;
begin
    Stream.Position := 0;
    Reader := TIcsStreamReader.Create(Stream, TRUE, CP_ACP);
    try
        while Reader.ReadLine(Line) do
            Memo.Lines.Add(Line);
    finally
        Reader.Free;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
// To be called by each thread as it completes (using Synchronize !)
{$WARN SYMBOL_DEPRECATED OFF} // TThread.Suspend is deprecated, don't use it !!
procedure THttpThreadForm.ProcessResults
   (ThreadNumber: Integer; Success : Boolean);
var
    Stream : TMemoryStream;
begin
    ThreadsObjects[ThreadNumber].Suspend;
    if Success then begin
        ResultsMemo.Lines.Add('* * * * * * THREAD ' +
                        IntToStr(ThreadNumber) + ' * * * * * *');
        Stream := ThreadsObjects[ThreadNumber].FHttpCli.RcvdStream
                  as TMemoryStream;
        LoadMemoFromMemoryStream(ResultsMemo, Stream);
        ResultsMemo.Lines.Add('');
    end
    else begin
        // There was an error getting data.
        ResultsMemo.Lines.Add('Nothing returned by thread: ' +
                              IntToStr(ThreadNumber));
    end;
    ResultsMemo.Lines.Add('* * * * * * * * * * * * * * * * * * * *');
    SetThreadState(ThreadNumber, tsReady);
    //Waiting for something to do (get next url here)
end;
{$WARN SYMBOL_DEPRECATED ON}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

