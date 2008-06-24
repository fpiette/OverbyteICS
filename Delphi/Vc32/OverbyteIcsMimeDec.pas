{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *

Author:       François PIETTE
Object:       TMimeDecode is a component whose job is to decode MIME encoded
              EMail messages (file attach). You can use it for example to
              decode messages received with a POP3 or NNTP component.
              MIME is described in RFC-1521. Headers are described if RFC-822.
Creation:     March 08, 1998
Version:      6.03
EMail:        francois.piette@overbyte.be  http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 1998-2007 by François PIETTE
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

              4. You must register this software by sending a picture postcard
                 to the author. Use a nice stamp and mention your name, street
                 address, EMail address and any comment you like to say.

QUICK REFERENCE:
----------------
TMimeDecode take a file or a stream as input and produce several event when
the message is parsed. each event can be used to display or to save to a file
the message parts.

Two methods can be called to decode either a file or a stream:
procedure DecodeFile(FileName : String);
procedure DecodeStream(aStream : TStream);

During the decode process, the component trigger several events. You have to
use those events to save data to a file or to display somehow on the
user interface.

Events are organized by groups of three for message header, part header and
part data:
Message header events: OnHeaderBegin      OnHeaderLine      OnHeaderEnd
Part header events:    OnPartHeaderBegin  OnPartHeaderLine  OnPartHeaderEnd
Part data events:      OnPartDataBegin    OnPartDataLine    OnPartDataEnd

The 'Begin' event is triggered once just before the first item will occur.
The 'Line' event is triggered for each item of the given type.
The 'End' event is triggered once after the last item.

For a multi-part message, we have this sequence:
a) The message header
OnHeaderBegin, then many OnHeaderLine, one for each line in the header. Lines
can be continuated in the message. The event here is triggered with continuated
lines concatenated (so it can be quite large !). After the last header line
has been processed, the OnHeaderEnd is triggered once.
b) The non-significant message part which can be empty. This is part 0. We
get OnPartBegin once, then OnPartLine for each line and finally OnPartEnd once.
c) The first significant part header with his three events, just like the
message header: OnPartHeaderBegin, OnPartHeaderLine and OnPartHeaderEnd.
d) The first significant part data with his three events: OnPartBegin once,
OnPartLine for each line and OnPartEnd once at the end of the part.
It's possible to have an empty part. This gives the OnPartBegin and OnPartEnd
events and NO OnPartLine event.
e) We can have many other parts. The sequence is always the same. We restart
at point (b) here above for each part (header, then data). Note that there is
often en empty part at the end of a message.

TMimeDecode decode encoded parts using 'base64' and 'quoted-printable' methods.
For those parts, the OnPartLine event will gives DECODED data. Other methods
are passed not decoded. You can use the property ContentTransferEncoding to
know which encoding method is used and add your own decoding mechanism.

For each OnHeaderLine, OnPartHeaderLine and OnPartLine, you can find the
actual data at the address pointed by the property CurrentData (a PChar).
The reason for a PChar is that the data can be quite large. The data pointed
is a null terminated string. You can get the length using StrLen, or convert
to a string with StrPas. It is more efficient to process the data using a
pointer. Using strings tends to copy the data several times.
The OnPartLine event passes a PChar and a length to the handler. This actully
point to the internal buffer and overwrite the original data (base64 and
quote-printable method produce decoded data smaller tha encoded one).

>From the message header, the component extract the following values:
>From         The message author. Not necessary the real author...
             Looks like "Francois Piette" <francois.piette@overbyte.be>
Dest         The message destination (To field, but To is a reserved word)
             Looks like "Francois Piette" <francois.piette@overbyte.be>
Subject      The message subject. Free text.
Date         The message date.
             Look like: Mon, 16 Feb 1998 12:45:11 -0800
ContentType  'multipart/mixed' or empty.
For details about those header fields and others, read RFC-822

For each part, we have the following properties updated (the header is parsed
on the fly):
PartNumber     Starting from 0 for the non-significant part
PartLine                  Starting 1 for the first line of each part or header
PartContentType           Such as 'text/plain' or 'application/x-zip-compressed'
PartCharset               This is a complement for the PartContentType.
ApplicationType           When PartContentType is 'application/something', we
                          get the 'something' extracted
PartName                  This is the value for 'name=something' in the
                          Content-Type header line.
PartEncoding              Encoding method (Content-Transfer-Encoding).
                          Can be used to decode unsupported
                          methods (supported methods are 'base64' and
                          'quoted-printable'. '7bit' and '8bit' does'nt
                          generally require processing.
PartDisposition           Can be 'inline' or 'attachement' and is generally
                          followed by a 'filename=something'
PartFileName              The specified filename in Content-Disposition header
                          line. Be aware that the file name is not necessary
                          suitable for windows ! Use it with caution...
For details about those header fields and others, read RFC-1521.

To write part data to files, you can either implement your own writing in
the OnPartLine event handler, or use the DestStream property. If assigned,
this property will be used to write the data. If not assigned, it will be
ignore.

To select a file name for each part, you can use the PartFileName property or
the 'PartName' property or a comnination of both. But be aware that those value
can be either missing or even invalid as a filename because the message was
generated with another opertaing system which has different filename
conventions.

Updates:
Apr 13, 1998  V1.01 Corrected a bug in ProcessLineBase64 which decoded one
              byte too much. Thanks to Rune Fredriksen <runefr@mail.link.no>.
Apr 15, 1998  V1.02 Corrected bug in ProcessHeaderLine which retreived only
              the first word for each item.
              Added the ReturnPath property.
Apr 24, 1998  V1.03 Removed the modification made in version 1.01 !
Apr 26, 1998  V1.04 Corrected a bug in ReallocMem with Delphi 1
Aug 27, 1998  V1.05 Corrected a bug in decoding which incorrectly merge
              the first message line with the header when the line begon
              by a space. Thanks to Mitch Cant <mitchcant@hotmail.com> for
              finding the bug and correction.
Sep 13, 1998  V1.06 Correctly handled unterminated messages.
              Correctly handled parts without header.
Dec 26, 1998  V1.07 Added features coded by Eric Fortier <efortier@videotron.ca>
              (Embedded mime parts, UUDecode).
Dec 30, 1998  V1.08 Check for header end when a header line begin with a
              space or tab character. (Normally a header end with a blank
              line, we also accept invalid header line).
Feb 01, 1999  V1.09 Corrected a bug ProcessLineUUDecode where 'end' was not
              checked. Thanks to Eric Fortier.
Feb 16, 1999  V1.10 Added UUEncoded embedded parts. Thanks to Eric Fortier.
              Corrected a line termination problem in ProcessLineBase64.
Jul 21, 1999  V1.11 Added support for encoded message without multipart.
              Added Encoding property with the encoding value.
              Thanks to Marcelo S Massuda <massuda@4web.com.br> for pinting this
              lack of feature.
Aug 20, 1999  V1.12 Added compile time options. Revised for BCB4.
Nov 25, 1999  V1.13 Changed continuation line character for quoted printable
              encoding. By Ken Petersen <KPT@edbgruppen.dk>.
              Created GetTokenEx function to take care of comments in header
              lines. This affect ProcessPartHeaderLine and ProcessHeaderLine.
              Thanks to Boris Daljevic <biber@eunet.yu> for his code.
              Added CharSet property related to main part charset (see also
              existing PartCharset property). Thanks to Boris Daljevic
              <biber@eunet.yu> for his code.
Jun 20, 2000  V1.14 Poessler Thomas <Thomas.Poessler@uta.at> corrected a bug in
              ProcessLineQuotedPrintable.
Jul 02, 2000  V1.15 Added OnMessageEnd event
Jul 15, 2000  V1.16 Added code from Wolfgang Baron <Wolfgang.Baron@gwtel.de>
              to support content-description header line.
              Changed GetToken and GetTokenEx so that a space before a delimiter
              will not break token parsing. Outlook generate such invalid
              formatting thanks for Arno van Rossum <a.van.rossum@mmp-obec.nl>
              for finding this bug.
              Revised code to handle inline UUEncoded messages.
Jul 21, 2000  V1.17 Use GetValue instead of GetToken to solve problem with
              boundaries of embbeded parts.
              With help of Jan Bartak <bart@seznam.cz>.
              As suggested by Sebastien Gariepy <beeper@globetrotter.net>, I
              added PartContentID.
Oct 29, 2000  V1.18 Checked for missing content-type before calling
              UUProcessLine. Without the check, a part with a line beginning
              with 'begin 666' will be wrongly decoded.
Feb 17, 2001  V1.19 Top of the messages with a field multipart was incorrectly
              processed.Property FCharset was not initialized in procedure
              MessageBegin. Thanks to Bayanov <bayanov@alt.ru>
Jul 26, 2001  V1.20 Cleared FEncoding in MessageBegin. Thanks to Joel
              lauvinerie <joel.lauvinerie@wanadoo.fr> who found this bug.
              Poessler Thomas <Thomas.Poessler@uta.at> added new properties:
              HeaderName, FileName, HeaderLines, Disposition, EndOfMime,
              IsMultipart.
Jul 29, 2001  V1.21 Moved ProcessLineBase64 to public section. Made CurrentData
              property read/write. This permit to use Base64 decoding from
              outside of the component.
              Corrected a glitche with Delphi 1
May 04, 2002  V1.23 Added "Len" argument to OnInlineDecodeLine event.
              Corrected UUDec so that nul is handled as space. Thanks to
              arnaud.mesnews@free.fr who provided a test case.
              Made UUOutDec a little bit faster.
May 10, 2002  V1.24 Accept 'begin 644' as well as 'begin 666' for UUEncoding
              start. arnaud.mesnews@free.fr found that OE does that.
Nov 01, 2002  V1.25 Changed PChar arguments to Pointer to work around Delphi 7
              bug with PAnsiChar<->PChar.
              This will require small changes in your application code: change
              PChar args to Pointer and add a PChar cast when using the arg.
              Changed Base64 decoding so that is doesn't crash even if input
              data is malformed (corrupted message).
              Changed UUEncoded detection procedure. Thanks to Arnaud
              <arnaud.mesnews@free.fr> for providing his code.
Apr 22, 2003  V1.26 Corrected ProcessLineQuotedPrintable which overflowed input
              data when an empty line was given. Thanks to Dmitry Andreev for
              finding a test case.
              V1.27 Christophe Thiaux <tophet@free.fr> added PartFormat and
              format properties.
Jul 20, 2003  V1.28 <arnaud.mesnews@free.fr> added yEnc decoding and fixed
              uudecode when "begin" has to be lower case.
Aug 06, 2003  V1.29 Dmitry Andreev <advadvadv@mailgate.ru> and Arnaud
              <arnaud.mesnews@free.fr> corrected a bug with continuation
              lines in ProcessLineQuotedPrintable.
Aug 10, 2003  V1.30 Reformatted the source line to make Arnaud and Dmitry
              changes looking like my own code. Translated all comments to
              english. Englicized identificators.
Jan 03, 2004  V1.31 Replaced private section by protected.
              Moved procedure ProcessLineQuotedPrintable to public section.
May 31, 2004  V1.32 John Bridgwater <jbridgwater@goodyear.com> fixed GetTokenEx
              to allow a space around delimiter.
Jul 24, 2004  V1.33 arnaud.mesnews@free.fr added TriggerInlineDecodeBegin,
              TriggerInlineDecodeLine and TriggerInlineDecodeEnd and called
              them where needed. He also added InlineDecodeLine and
              LengthHeader properties
Nov 3, 2006   V6.00 New version 6.00 started
Nov 13, 2007  V6.01 Fixed TMimeDecode.ProcessPartLine to avoid adding a CRLF
              at the end of attached text file.
Nov 14, 2007  V6.02 Added Cc decoding
Mar 10, 2008  V6.03 Francois Piette made some changes to prepare code
                    for Unicode.


 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsMimeDec;

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$I OverbyteIcsDefs.inc}
{$IFDEF DELPHI6_UP}
    {$WARN SYMBOL_PLATFORM   OFF}
    {$WARN SYMBOL_LIBRARY    OFF}
    {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}
{$IFNDEF VER80}   { Not for Delphi 1                    }
    {$H+}         { Use long strings                    }
    {$J+}         { Allow typed constant to be modified }
{$ENDIF}
{$IFDEF BCB3_UP}
    {$ObjExportAll On}
{$ENDIF}

interface

uses
{$IFDEF USEWINDOWS}
    Windows,
{$ELSE}
    WinTypes, WinProcs,
{$ENDIF}
    SysUtils, Classes;

const
    MimeDecodeVersion  = 603;
    CopyRight : String = ' TMimeDecode (c) 1998-2008 Francois Piette V6.03 ';

type
    TMimeDecodePartLine = procedure (Sender  : TObject;
                                     Data    : Pointer;
                                     DataLen : Integer) of object;

    TInlineDecodeBegin = procedure (Sender: TObject; Filename: String) of object;
    TInlineDecodeLine  = procedure (Sender: TObject; Line: Pointer; Len : Integer) of object;
    TInlineDecodeEnd   = procedure (Sender: TObject; Filename: String) of object;

    TMimeDecode = class(TComponent)
    protected
        FFrom                     : String;
        FDest                     : String;
        FCc                       : String;
        FSubject                  : String;
        FDate                     : String;
        FReturnPath               : String;
        FEncoding                 : String;
        FCharSet                  : String;
        FContentType              : String;
        FMimeVersion              : String;
        FHeaderName               : String;
        FDisposition              : String;
        FFileName                 : String;
        FFormat                   : String;
        FHeaderLines              : TStrings;
        FIsMultipart              : Boolean;
        FEndOfMime                : Boolean;
        FPartContentType          : String;
        FPartEncoding             : String;
        FPartNumber               : Integer;
        FPartHeaderBeginSignaled  : Boolean;
        FPartName                 : String;
        FPartDisposition          : String;
        FPartContentID            : String;
        FPartFileName             : String;
        FPartFormat               : String;
        FPartCharset              : String;
        FApplicationType          : String;
        FPartOpened               : Boolean;
        FHeaderFlag               : Boolean;
        FLineNum                  : Integer;
        FBuffer                   : PChar;
        FBufferSize               : Integer;
        FCurrentData              : PChar;
        FBoundary                 : String;
        FUUProcessFlag            : Boolean;
        FProcessFlagYBegin        : Boolean;   { AS: YEnc handling }
        FSizeFileY                : Integer;   { AS: YEnc handling }
        FSizeBlocY                : Integer;   { AS: YEnc handling }
        FSizeLeftY                : Integer;   { AS: YEnc handling }
        FNext                     : procedure of object;
        FDestStream               : TStream;
        cUUFilename               : String;             { ##ERIC }
        FEmbeddedBoundary         : TStringList;        { ##ERIC }
        cIsEmbedded               : Boolean;            { ##ERIC }
        FOnHeaderBegin            : TNotifyEvent;
        FOnHeaderLine             : TNotifyEvent;
        FOnHeaderEnd              : TNotifyEvent;
        FOnPartHeaderBegin        : TNotifyEvent;
        FOnPartHeaderLine         : TNotifyEvent;
        FOnPartHeaderEnd          : TNotifyEvent;
        FOnPartBegin              : TNotifyEvent;
        FOnPartLine               : TMimeDecodePartLine;
        FOnPartEnd                : TNotifyEvent;
        FOnMessageEnd             : TNotifyEvent;
        FOnInlineDecodeBegin      : TInlineDecodeBegin;
        FOnInlineDecodeLine       : TInlineDecodeLine;
        FOnInlineDecodeEnd        : TInlineDecodeEnd;
        { Used to force InLine decoding even if there was no OnInlineDecodeLine
          event. See UUProcessLine }
        FInlineDecodeLine         : Boolean;
        FLengthHeader             : Integer;
        FPartFirstLine            : Boolean;
        procedure TriggerHeaderBegin; virtual;
        procedure TriggerHeaderLine; virtual;
        procedure TriggerHeaderEnd; virtual;
        procedure TriggerPartHeaderBegin; virtual;
        procedure TriggerPartHeaderLine; virtual;
        procedure TriggerPartHeaderEnd; virtual;
        procedure TriggerPartBegin; virtual;
        procedure TriggerPartLine(Data : Pointer; DataLen : Integer); virtual;
        procedure TriggerPartEnd; virtual;
        procedure TriggerMessageEnd; virtual;
        procedure TriggerInlineDecodeBegin(Filename: String); virtual;
        procedure TriggerInlineDecodeLine(Line: Pointer; Len : Integer); virtual;
        procedure TriggerInlineDecodeEnd(Filename: String); virtual;
        procedure ProcessLineUUDecode;
        function  UUProcessLine(FCurrentData: PChar): boolean;
        procedure ProcessHeaderLine;
        procedure ProcessPartHeaderLine;
        procedure ProcessPartLine;
        procedure ProcessWaitBoundary;
        procedure ProcessMessageLine;
        procedure PreparePart;
        procedure PrepareNextPart;
        procedure ProcessDecodedLine(Line : Pointer; Len : Integer);
        procedure InternalDecodeStream(aStream : TStream);
        procedure MessageBegin;
        procedure MessageEnd;
        procedure ParseYBegin(const Ch : String);
    public
        constructor Create(AOwner : TComponent); override;
        destructor  Destroy;                     override;

        procedure DecodeFile(FileName : String);
        procedure DecodeStream(aStream : TStream);
        procedure ProcessLineBase64;
        procedure ProcessLineQuotedPrintable;
        property From             : String           read  FFrom;
        property Dest             : String           read  FDest;
        property Cc               : String           read  FCc;
        property Subject          : String           read  FSubject;
        property Date             : String           read  FDate;
        property ReturnPath       : String           read  FReturnPath;
        property ContentType      : String           read  FContentType;
        property Encoding         : String           read  FEncoding;
        property Charset          : String           read  FCharset;
        property MimeVersion      : String           read  FMimeVersion;
        property HeaderName       : String           read  FHeaderName;
        property Disposition      : String           read  FDisposition;
        property FileName         : String           read  FFileName;
        property Format           : String           read  FFormat;
        property HeaderLines      : TStrings         read  FHeaderLines;
        property IsMultipart      : Boolean          read  FIsMultipart;
        property EndOfMime        : Boolean          read  FEndOfMime;
        property PartContentType  : String           read  FPartContentType;
        property PartEncoding     : String           read  FPartEncoding;
        property PartName         : String           read  FPartName;
        property PartDisposition  : String           read  FPartDisposition;
        property PartContentID    : String           read  FPartContentID;
        property PartFileName     : String           read  FPartFileName;
        property PartFormat       : String           read  FPartFormat;
        property PartCharset      : String           read  FPartCharset;
        property ApplicationType  : String           read  FApplicationType;
        property PartNumber       : Integer          read  FPartNumber;
        property CurrentData      : PChar            read  FCurrentData
                                                     write FCurrentData;
        property DestStream       : TStream          read  FDestStream
                                                     write FDestStream;
        property InlineDecodeLine : boolean          read  FInlineDecodeLine
                                                     write FInlineDecodeLine
                                                     default FALSE;
        property LengthHeader     : Integer          read  FLengthHeader;
    published
        property OnHeaderBegin : TNotifyEvent        read  FOnHeaderBegin
                                                     write FOnHeaderBegin;
        property OnHeaderLine : TNotifyEvent         read  FOnHeaderLine
                                                     write FOnHeaderLine;
        property OnHeaderEnd : TNotifyEvent          read  FOnHeaderEnd
                                                     write FOnHeaderEnd;
        property OnPartHeaderBegin : TNotifyEvent    read  FOnPartHeaderBegin
                                                     write FOnPartHeaderBegin;
        property OnPartHeaderLine : TNotifyEvent     read  FOnPartHeaderLine
                                                     write FOnPartHeaderLine;
        property OnPartHeaderEnd : TNotifyEvent      read  FOnPartHeaderEnd
                                                     write FOnPartHeaderEnd;
        property OnPartBegin : TNotifyEvent          read  FOnPartBegin
                                                     write FOnPartBegin;
        property OnPartLine : TMimeDecodePartLine    read  FOnPartLine
                                                     write FOnPartLine;
        property OnPartEnd : TNotifyEvent            read  FOnPartEnd
                                                     write FOnPartEnd;
        property OnMessageEnd : TNotifyEvent         read  FOnMessageEnd
                                                     write FOnMessageEnd;
        property OnInlineDecodeBegin : TInlineDecodeBegin
                                                     read  FOnInlineDecodeBegin
                                                     write FOnInlineDecodeBegin;
        property OnInlineDecodeLine  : TInlineDecodeLine
                                                     read  FOnInlineDecodeLine
                                                     write FOnInlineDecodeLine;
        property OnInlineDecodeEnd   : TInlineDecodeEnd
                                                     read  FOnInlineDecodeEnd
                                                     write FOnInlineDecodeEnd;
    end;

procedure Register;

function GetToken(Src : PChar; var Dst : String; var Delim : Char) : PChar;
function GetHeaderValue(X : PChar) : String;
function IsSpaceChar(Ch : Char) : Boolean;
function IsCrLfChar(Ch : Char) : Boolean;
function IsCrLf1Char(Ch : Char) : Boolean;
function IsCrLf1OrSpaceChar(Ch : Char) : Boolean;
function IsCharInSysCharSet(Ch : Char; const MySet : TSysCharSet) : Boolean;

implementation

type
  TLookup = array [0..127] of Byte;

const
  Base64In: TLookup = (
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255, 255,
    255, 255, 255, 255,  62, 255, 255, 255,  63,  52,  53,  54,  55,
     56,  57,  58,  59,  60,  61, 255, 255, 255,  64, 255, 255, 255,
      0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,
     13,  14,  15,  16,  17,  18,  19,  20,  21,  22,  23,  24,  25,
    255, 255, 255, 255, 255, 255,  26,  27,  28,  29,  30,  31,  32,
     33,  34,  35,  36,  37,  38,  39,  40,  41,  42,  43,  44,  45,
     46,  47,  48,  49,  50,  51, 255, 255, 255, 255, 255
  );


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure Register;
begin
    RegisterComponents('FPiette', [TMimeDecode]);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF VER80}
{ Delphi 1 miss the SetLength procedure. So we rewrite it. }
procedure SetLength(var S: string; NewLength: Integer);
begin
    S[0] := chr(NewLength);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF VER80}
function TrimRight(Str : String) : String;
var
    i : Integer;
begin
    i := Length(Str);
    while (i > 0) and IsSpaceChar(Str[i]) do
        i := i - 1;
    Result := Copy(Str, 1, i);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TrimLeft(Str : String) : String;
var
    i : Integer;
begin
    if Str[1] <> ' ' then
        Result := Str
    else begin
        i := 1;
        while (i <= Length(Str)) and (Str[i] = ' ') do
            i := i + 1;
        Result := Copy(Str, i, Length(Str) - i + 1);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Trim(Str : String) : String;
begin
    Result := TrimLeft(TrimRight(Str));
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function HexConv(Ch : Char) : Integer;
begin
    if IsCharInSysCharSet(Ch, ['0'..'9']) then
        Result := Ord(Ch) - Ord('0')
    else
        Result := (Ord(Ch) and 15) + 9;
end;

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
constructor TMimeDecode.Create(AOwner : TComponent);
begin
    inherited Create(AOwner);
    FHeaderLines      := TStringList.Create;
    FIsMultipart      := FALSE;
    FEndOfMime        := FALSE;
    FInlineDecodeLine := false;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
destructor TMimeDecode.Destroy;
begin
    if Assigned(FHeaderLines) then begin
        FHeaderLines.Destroy;
        FHeaderLines := nil;
    end;
    inherited Destroy;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.TriggerHeaderBegin;
begin
    if Assigned(FOnHeaderBegin) then
        FOnHeaderBegin(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.TriggerHeaderLine;
begin
    if Assigned(FOnHeaderLine) then
        FOnHeaderLine(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.TriggerHeaderEnd;
begin
    if Assigned(FOnHeaderEnd) then
        FOnHeaderEnd(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.TriggerPartHeaderBegin;
begin
    if Assigned(FOnPartHeaderBegin) then
        FOnPartHeaderBegin(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.TriggerPartHeaderLine;
begin
    if Assigned(FOnPartHeaderLine) then
        FOnPartHeaderLine(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.TriggerPartHeaderEnd;
begin
    if Assigned(FOnPartHeaderEnd) then
        FOnPartHeaderEnd(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.TriggerPartBegin;
begin
    if Assigned(FOnPartBegin) then
        FOnPartBegin(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.TriggerPartLine(Data : Pointer; DataLen : Integer);
begin
    if Assigned(FOnPartLine) then
        FOnPartLine(Self, Data, DataLen);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.TriggerPartEnd;
begin
    if Assigned(FOnPartEnd) then
        FOnPartEnd(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.TriggerMessageEnd;
begin
    if Assigned(FOnMessageEnd) then
        FOnMessageEnd(Self);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.TriggerInlineDecodeBegin(Filename: String);
begin
    if Assigned(FOnInlineDecodeBegin) then
        FOnInlineDecodeBegin(self, Filename);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.TriggerInlineDecodeLine(Line: Pointer; Len : Integer);
begin
    if Assigned(FOnInlineDecodeLine) then
        FOnInlineDecodeLine(self, Line, Len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.TriggerInlineDecodeEnd(Filename: String);
begin
    if Assigned(FOnInlineDecodeEnd) then
        FOnInlineDecodeEnd(self, Filename);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.ProcessDecodedLine(Line : Pointer; Len : Integer);
begin
    if Len > 0 then begin
        if (FPartContentType = '')  { Not sure it is always OK !              }
                                    { As such we can't have a MIME part which }
                                    { is uu-encoded.                          }
           and uuprocessline(line) then
                Exit;
    end;
    TriggerPartLine(Line, Len);

    { Write decoded characters to the destination stream }
    if Assigned(FDestStream) and (Len > 0) then
        FDestStream.WriteBuffer(Line^, Len);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This works if charset="iso-8859-1" !                                        }
procedure TMimeDecode.ProcessLineQuotedPrintable;
var
    SourceIndex         : Integer;
    DecodedIndex        : Integer;
    Ch                  : Char;
    Code                : Integer;
    DecodedBuf          : String;
const
    EmptyLine : array [0..2] of char = (#13, #10, #0);
begin
    if FCurrentData = nil then
        Exit;

    { Allocate a buffer for decode line. At most the length of encoded data }
    { plus 2 bytes for CRLF                                                 }
    SetLength(DecodedBuf, StrLen(FCurrentData) + 2);
    SourceIndex  := 0; { It's a PChar so index start at 0   }
    DecodedIndex := 1; { It's a String, so index start at 1 }
    while TRUE do begin
        Ch := FCurrentData[SourceIndex];
        if Ch = #0 then begin
            { End of line, add CRLF and let's go }
            DecodedBuf[DecodedIndex] := #13;
            Inc(DecodedIndex);
            DecodedBuf[DecodedIndex] := #10;
            ProcessDecodedLine(@DecodedBuf[1], DecodedIndex);
            break;
        end;
        if Ch = '=' then begin
            { Encoded character. Next two chars should be hex code }
            Inc(SourceIndex);
            Ch := FCurrentData[SourceIndex];
            if Ch = #0 then begin
{*** Changed 20030806 ***}
                { process without #13#10 adding }
                ProcessDecodedLine(@DecodedBuf[1], DecodedIndex-1);
                break;
{***         ***}
            end;
            Code := HexConv(Ch);
            Inc(SourceIndex);
            Ch := FCurrentData[SourceIndex];
            if Ch = #0 then begin
                { Should not occur: code truncated, ignore }
                continue;
            end;
            Code := (Code shl 4) + HexConv(Ch);
            DecodedBuf[DecodedIndex] := Chr(Code);
        end
        else
            DecodedBuf[DecodedIndex] := FCurrentData[SourceIndex];
        Inc(SourceIndex);
        Inc(DecodedIndex);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.ProcessLineBase64;
var
    SourceIndex         : Integer;
    DataIn0             : Byte;
    DataIn1             : Byte;
    DataIn2             : Byte;
    DataIn3             : Byte;
    DecodedIndex        : Integer;
    Len                 : Integer;
begin
    SourceIndex  := 0;
    DecodedIndex := 0;
    Len          := StrLen(FCurrentData);

    { Remove spaces at the end of line }
    while (Len > 0) and IsSpaceChar(FCurrentData[Len - 1]) do
        Dec(Len);

    { Skip white spaces at the start of line }
    while (SourceIndex < Len) and IsSpaceChar(FCurrentData[SourceIndex]) do
        Inc(SourceIndex);

    { Decode until end of line. Replace coded chars by decoded ones       }
    { Protect agains malformed messages. Normally we have a length which  }
    { is multiple of four. But this may be corrupted !                    }
    while SourceIndex < Len do begin
        { "And $7F" will clear 8th bit and avoid range error. If found in }
        { a message, it is probably a corrupted message !                 }
        DataIn0 := Base64In[Byte(FCurrentData[SourceIndex]) and $7F];
        Inc(SourceIndex);
        if SourceIndex >= Len then begin
            DataIn1 := $40;
            DataIn2 := $40;
            DataIn3 := $40;
        end
        else begin
            DataIn1 := Base64In[Byte(FCurrentData[SourceIndex]) and $7F];
            Inc(SourceIndex);
            if SourceIndex >= Len then begin
                DataIn2 := $40;
                DataIn3 := $40;
            end
            else begin
                DataIn2 := Base64In[Byte(FCurrentData[SourceIndex]) and $7F];
                Inc(SourceIndex);
                if SourceIndex >= Len then
                    DataIn3 := $40
                else begin
                    DataIn3 := Base64In[Byte(FCurrentData[SourceIndex]) and $7F];
                    Inc(SourceIndex);
                end;
            end;
        end;

        FCurrentData[DecodedIndex] := Char((DataIn0 and $3F) shl 2 + (DataIn1 and $30) shr 4);
        if DataIn2 <> $40 then begin
            FCurrentData[DecodedIndex + 1] := Char((DataIn1 and $0F) shl 4 + (DataIn2 and $3C) shr 2);
            if DataIn3 <> $40 then begin
                FCurrentData[DecodedIndex + 2] := Char((DataIn2 and $03) shl 6 + (DataIn3 and $3F));
                Inc(DecodedIndex, 3);
            end
            else
                Inc(DecodedIndex, 2);
        end
        else
            Inc(DecodedIndex, 1);
    end;

    { Nul terminate decoded line }
    FCurrentData[DecodedIndex] := #0; { 16/02/99 }
    ProcessDecodedLine(FCurrentData, DecodedIndex);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function UUDec(Sym : Char): Byte;
begin
    if Sym = #0 then
        Result := 0
    else
        Result := (Ord(Sym) - Ord(' ')) and $3F;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure UUOutDec(buf: PChar; n: Integer; var out1 : String);
begin
    case n of
    0:   ;
    1:   out1 := out1 + Char((UUDec(buf[0]) SHL 2) + (UUDec(buf[1]) SHR 4));
    2:   out1 := out1 + Char((UUDec(buf[0]) SHL 2) + (UUDec(buf[1]) SHR 4)) +
                        Char((UUDec(buf[1]) SHL 4) + (UUDec(buf[2]) SHR 2));
    else out1 := out1 + Char((UUDec(buf[0]) SHL 2) + (UUDec(buf[1]) SHR 4)) +
                        Char((UUDec(buf[1]) SHL 4) + (UUDec(buf[2]) SHR 2)) +
                        Char((UUDec(buf[2]) SHL 6) + (UUDec(buf[3])));
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF NEVER}
procedure UUOutDec(buf: PChar; n: Integer; var out1 : String);
var
    c1, c2, c3: Char;
begin
    c1 := Chr((word(UUDec(buf[0])) SHL 2) or (word(UUDec(buf[1])) SHR 4));
    c2 := Chr((word(UUDec(buf[1])) SHL 4) or (word(UUDec(buf[2])) SHR 2));
    c3 := Chr((word(UUDec(buf[2])) SHL 6) or (word(UUDec(buf[3]))));
    if n >= 1 then
        out1 := out1 + c1;
    if n >= 2 then
        out1 := out1 + c2;
    if n >= 3 then
        out1 := out1 + c3;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ AS: Get a value from a header line. Support multiline header.               }
{ When a second line is present, make sure only ONE space is taken.           }
{ InternalDecodeStream has replaced CR, LF and TAB by #1 character.           }
function GetHeaderValue(X : PChar) : String;
var
    I, J : Integer;
begin
    Result := SysUtils.StrPas(X);
    I      := Length(Result);
    while I >= 1 do begin
        if IsCrLf1Char(Result[I]) then begin
            { Make sure we preserve a single space }
            J := I;
            while (I >= 1) and IsCrLf1OrSpaceChar(Result[I - 1]) do
                Dec(i);
            while (J < Length(Result)) and
                  IsCrLf1OrSpaceChar(Result[J + 1]) do
                Inc(J);
            Delete(Result, I, J - I);
            Result[I] := ' ';
        end;
        Dec(I);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.ProcessLineUUDecode; { ##ERIC }
var
    count, Size : Integer;
    s           : String;
    out1        : String;
    bp          : PChar;
    pos1        : Integer;
begin
    if FCurrentData^ = #0 then
        exit;
    s := StrPas(FCurrentData);

    if LowerCase(copy(s, 1, 6)) = 'begin ' then begin
        out1:=lowercase(s);
        if (Pos('--', out1) > 0) and (Pos('cut here', out1) > 0) then
            Exit;
        pos1 := Pos(' ', s);
        s    := Copy(s, pos1 + 1, 255);
        pos1 := Pos(' ', s);
        s    := Copy(s, pos1 + 1, 255);
        cUUFilename := s;
        exit;
    end
    else if LowerCase(Copy(s, 1, 3)) = 'end' then begin
        out1 := LowerCase(s);
        if (Pos('--', out1) > 0) and (Pos('cut here', out1) > 0) then
            Exit;
        cUUFilename := '';
        exit;
    end;

    { if no filename defined yet, exit }
    if cUUFilename = '' then
        exit;

    { decode the line }
    count := UUDec(s[1]);
    Size  := Count;
    if count > 0 then begin
        bp := @s[2];
        repeat
            UUOutDec(bp, count, out1);
            count := count - 3;
            bp    := bp + 4;
        until count <= 0;
    end;

    { we're done. copy and leave }
    Move(Out1[1], FCurrentData[0], Size);
    ProcessDecodedLine(FCurrentData, Size);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function UUSectionBegin(
    const Line     : String;
    var   FileName : String) : Boolean;
var
    I : Integer;
begin
    { A UUEncoded section begins by a line having the syntax:               }
    {   "begin nnn filename" with xxx being a number (unix file permission) }
    { We accept xxx with at least 2 digits. Filename is optional.           }
    Result   := FALSE;
    FileName := '';
    { AS: "begin" _must_ be in lower case !                                 }
    if Copy(Line, 1, 6) = 'begin ' then begin
        I := 7;
        while I <= Length(Line) do begin
            if Line[I] = ' ' then begin
                Result := (I > 8);
                if Result then
                    FileName := Copy(Line, I + 1, Length(Line));
                break
            end;
            if not IsCharInSysCharSet(Line[I], ['0'..'9']) then
                break;
            Inc(I)
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ AS: YEnc support routine                                                  }
procedure TMimeDecode.ParseYBegin(const Ch : String);
var
    I, J : Integer;
begin
    { AS: Line format is "=ybegin line=128 size=XXXX name=YYYY";            }
    FSizeFileY := 0;
    FSizeBlocY := 0;
    I          := 9;
    while I < Length(Ch) do begin
        if Copy(Ch, I, 5) = 'line=' then begin
            I := I + 5;
            while IsCharInSysCharSet(Ch[I], ['0'..'9']) do begin
                FSizeBlocY := 10 * FSizeBlocY + (Ord(Ch[I]) - Ord('0'));
                Inc(I);
            end;
        end
        else if Copy(Ch, I, 5) = 'size=' then begin
            I := I + 5;
            while IsCharInSysCharSet(Ch[I], ['0'..'9']) do begin
                FSizeFileY := 10 * FSizeFileY + (Ord(Ch[I]) - Ord('0'));
                Inc(I);
            end;
        end
        else if Copy(Ch, I, 5) = 'name=' then begin
            I := I + 5;
            J := I;
            repeat
                while (J <= Length(Ch)) and (Ch[J] <> ' ') do
                    Inc(J);
                if (J >= Length(Ch)) or (Ch[J + 1] = '=') then
                    break
                else
                    Inc(J);
            until FALSE;
            cUUFilename := Copy(Ch, I, J - I);
            I           := J;
        end;
        Inc(I);
    end;
    FSizeLeftY := FSizeFileY;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TMimeDecode.UUProcessLine(FCurrentData: PChar): Boolean;
var
    S           : String;
    out1        : String;
    count       : Integer;
    bp          : PChar;
    chName      : String;
    I, C        : Integer;
begin
    Result := TRUE;
    S := StrPas(FCurrentData); { AS }
    if Trim(S) = '' then begin
        Result := FALSE;
        Exit;
    end;

    if (not FUUProcessFlag) and UUSectionBegin(S, chName) then begin { AS }
        if chName <> '' then                                         { AS }
            cUUFilename := chName;                                   { AS }
        out1 := LowerCase(S);
        if (Pos('--', out1) > 0) and (Pos('cut here', out1) > 0) then
            Exit;
        FUUProcessFlag := TRUE;
        FProcessFlagYBegin := false;
        TriggerInlineDecodeBegin(cUUFilename);
        Exit;
    end;
    { AS: Handle YEnc }
    if (not FUUProcessFlag) and (Copy(S, 1, 8) = '=ybegin ') then begin
        { Line format : "=ybegin line=128 size=XXXX name=YYYY"; }
        ParseYBegin(S);
        FUUProcessFlag     := TRUE;
        FProcessFlagYBegin := TRUE;
        TriggerInlineDecodeBegin(cUUFilename);
        Exit;
    end;

    if not FUUProcessFlag then begin
        Result := FALSE;
        Exit;
    end;

    if CompareText(Copy(S, 1, 3), 'end') = 0 then begin
        out1 := LowerCase(S);
        if (Pos('--', out1) > 0) and (Pos('cut here', out1) > 0) then
            Exit;
        FUUProcessFlag := FALSE;
        { I also use the filename here in case the client prefer to save   }
        { data to a stream and save to a file when the decoding is complete }
        TriggerInlineDecodeEnd(cUUFileName);
        cUUFilename := '';
        Exit;
    end;

    { AS: Handle YEnc }
    if CompareText(Copy(S, 1, 6), '=yend ') = 0 then begin
        FUUProcessFlag := FALSE;
        FProcessFlagYBegin := false;
        { I also use the filename here in case the client prefer to save   }
        { data to a stream and save to a file when the decoding is complete }
        TriggerInlineDecodeEnd(cUUFilename);
        cUUFilename := '';
        Exit;
    end;

    if CompareText(Copy(S, 1, 7), '=ypart ') = 0 then begin
        { The message is in several parts. Something to do ? }
        Exit;
    end;

    if FInlineDecodeLine or Assigned(FOnInlineDecodeLine) then begin
        { decode the line }
        { AS: Handle YEnc }
        if not FProcessFlagYBegin then begin
            Count := UUDec(S[1]);
            out1  := ''; { AS: 25/11/2002 }

           {AS : new method to ignore wrongly coded lines }
           I := Length(S) - 1;
           if (Count > 0) and (Length(S) > 1) then begin
               bp := @S[2];
               repeat
                   UUOutDec(bp, Count, out1);
                   if Count >= 3 then begin
                       Count := Count - 3;
                       I     := I - 4;
                   end
                   else begin
                       if I >= 4 then
                           I := I - 4
                       else if I > 0 then
                           I := 0;
                       Count := 0;
                   end;
                   bp    := bp + 4;
               until Count <= 0;
               if I <> 0 then
                   out1 := '';
           end;
{ Old code
           if (Count > 0) and (Length(S) > 1) then begin
               bp := @S[2];
               repeat
                   UUOutDec(bp, Count, out1);
                   Count := Count - 3;
                   bp    := bp + 4;
               until Count <= 0;
           end;
}
        end
        else begin { AS: Handle YEnc }
            out1 := '';
            I    := 0;
            bp   := FCurrentData;
            while (I < FSizeBlocY) and (bp[I] <> #0) do begin
                if bp[I] = '=' then begin
                    C := Byte(bp[I + 1]) - 64 - 42;
                    Inc(I);
                end
                else
                    C := byte(bp[I]) - 42;
                if C < 0 then
                    C := C + 256;
                out1 := out1 + Char(C);
                Inc(I);
            end;
        end;

        {$IFDEF VER80}
        if Length(Out1) = 0 then
            FOnInlineDecodeLine(Self, nil, 0)
        else
            FOnInlineDecodeLine(Self, @Out1[1], Length(Out1));
        {$ELSE}
        TriggerInlineDecodeLine(PChar(Out1), Length(Out1));
        {$ENDIF}
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function stpblk(PValue : PChar) : PChar;
begin
    Result := PValue;
    { AS: Add #1 which is used to handle header lines }
    while IsCrLf1OrSpaceChar(Result^) do
        Inc(Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetValue(Src : PChar; var Dst : String; var Delim : Char) : PChar;
begin
    Result := StpBlk(Src);
    Dst    := '';
    Delim  := Result^;
    if Delim = '"' then begin
        Inc(Result);
        while TRUE do begin
            Delim  := Result^;
            if Delim = #0 then
                break;
            if Delim = '"' then begin
                Inc(Result);
                Delim := Result^;
                break;
            end;
            Dst := Dst + Delim;
            Inc(Result);
        end;
    end
    else begin
        while TRUE do begin
            Delim  := Result^;
            if IsCharInSysCharSet(Delim, [':', ' ', ';', '=', #9, #0]) then
                break;
            Dst := Dst + LowerCase(Result^);
            Inc(Result);
        end;
    end;
    if IsSpaceChar(Delim) then begin
        Result := stpblk(Result);
        if IsCharInSysCharSet(Result^, [':', ';', '=', #9]) then
            Inc(Result);
    end
    else if Delim <> #0 then
        Inc(Result);
    Result := stpblk(Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetToken(Src : PChar; var Dst : String; var Delim : Char) : PChar;
begin
    Result := StpBlk(Src);
    Dst    := '';
    while TRUE do begin
        Delim := Result^;
        if IsCharInSysCharSet(Delim, [':', ' ', ';', '=', #9, #0]) then
                break;
        Dst := Dst + LowerCase(Result^);
        Inc(Result);
    end;
    if IsSpaceChar(Delim) then begin
        Result := stpblk(Result);
        if IsCharInSysCharSet(Result^, [':', ';', '=', #9]) then begin
            {AS: Take delimiter after space }
            Delim := Result^;
            Inc(Result);
        end;
    end
    else if Delim <> #0 then
        Inc(Result);
    Result := stpblk(Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Same as GetToken, but take be aware of comments                           }
function GetTokenEx(Src : PChar; var Dst : String; var Delim : Char) : PChar;
var
    Comment: Integer;
begin
    Result  := StpBlk(Src);
    Dst     := '';
    Comment := 0;
    while TRUE do begin
        Delim := Result^;
        if Delim = #0 then
            break;
        if Delim = '(' then begin
            Inc(comment); { Comments can be nested }
            Inc(Result);
            Continue;
        end
        else if Delim = ')' then begin
            Dec(Comment);
            Inc(Result);
            Continue;
        end
        else if (Comment = 0) and
                IsCharInSysCharSet(Delim, [':', ' ', ';', '=', #9]) then
            break;
        Dst := Dst + LowerCase(Result^);
        Inc(Result);
    end;
    if IsSpaceChar(Delim) then begin
        Result := stpblk(Result);
        if IsCharInSysCharSet(Result^, [':', ';', '=', #9]) then begin
            Delim := Result^;
            Inc(Result);
        end;
    end
    else if Delim <> #0 then
    Inc(Result);
    Result := StpBlk(Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function GetQuoted(Src : PChar; var Dst : String) : PChar;
var
    Quote : Char;
begin
    Result := StpBlk(Src);
    Dst    := '';
    Quote  := Result^;
    if Quote <> #34 then begin  { ##ERIC }
        Dst := StrPas(Src);     { ##ERIC }
        Exit;                   { ##ERIC }
    end;                        { ##ERIC }

    Inc(Result);
    while (Result^ <> #0) and (Result^ <> Quote) do begin
        Dst := Dst + Result^;
        Inc(Result);
    end;
    Result := stpblk(Result);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.PreparePart;
begin
    FPartOpened    := FALSE;
    TriggerPartEnd;
    PrepareNextPart;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.ProcessWaitBoundary;       { ##ERIC }
var
    T : Integer;
    S : String;
begin
    S := LowerCase(StrPas(FCurrentData));
    if S = FBoundary then begin
        PreparePart;
        Exit;
    end
    else begin
        { are we in the embedded boundaries ? }
        for T := 0 to FEmbeddedBoundary.Count - 1 do begin
            if FEmbeddedBoundary[T] = S then begin
                cIsEmbedded := true;
                PreparePart;
                Exit;
            end;
        end;
       { if not in primary boundary or embedded boundaries, then process it.}
       ProcessDecodedLine(FCurrentData, StrLen(FCurrentData));
   end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.ProcessMessageLine;
begin
    Inc(FLineNum);
    if FLineNum = 1 then begin
        FPartFirstLine := TRUE;
        TriggerPartBegin;
    end;
    if FEncoding = 'base64' then
        ProcessLineBase64
    else if FEncoding = 'quoted-printable' then
        ProcessLineQuotedPrintable
    else if FEncoding = 'x-uuencode' then
        ProcessLineUUDecode                       { ##ERIC }
    else begin {tap}
        ProcessDecodedLine(FCurrentData, StrLen(FCurrentData));
        ProcessDecodedLine(PChar(#13#10), 2); {tap: add \r\n to other encodings}
    end; {tap}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.PrepareNextPart;
begin
    FPartEncoding            := '';
    FPartContentType         := '';
    FPartDisposition         := '';
    FPartContentID           := '';
    FPartName                := '';
    FPartFileName            := '';
    FPartFormat              := '';
    FHeaderFlag              := TRUE;  { We begin by a header }
    FLineNum                 := 0;
    FUUProcessFlag           := FALSE;
    FProcessFlagYBegin       := FALSE;  { AS: Handle YEnc }
    FPartHeaderBeginSignaled := FALSE;
    FNext                    := ProcessPartHeaderLine;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.ProcessPartLine; { ##ERIC }
var
    Len : Integer;
    t   : Integer;
    s   : String;            { ##ERIC }
begin
    { Check if end of part (boundary line found) }
    if (FCurrentData <> nil) and (FCurrentData^ <> #0) then begin
        s := LowerCase(StrPas(FCurrentData));
        if (s = FBoundary) then begin
            PreparePart;
            exit;
        end
        else if (s = (FBoundary + '--')) then begin
            FEndOfMime := TRUE;
            PreparePart;
            exit;
        end
        else begin
            for t := 0 to FEmbeddedBoundary.Count - 1 do begin
                if (s = FEmbeddedBoundary[t]) or
                   (s = (FEmbeddedBoundary[t] + '--')) then begin
                    { we now have to wait for the next part }
                    PreparePart;
                    exit;
                end
            end;
        end;
    end;

    if not FPartOpened then begin
        FPartOpened    := TRUE;
        FPartFirstLine := TRUE;
        TriggerPartBegin;
    end;

    if FPartEncoding = 'base64' then
        ProcessLineBase64
    else if FPartEncoding = 'quoted-printable' then
        ProcessLineQuotedPrintable
    else if FPartEncoding = 'x-uuencode' then   { ##ERIC }
        ProcessLineUUDecode                     { ##ERIC }
    else begin
        if FCurrentData = nil then
            Len := 0
        else
            Len := StrLen(FCurrentData);
        if FPartFirstLine then               { FP Nov 13, 2007 }
            FPartFirstLine := FALSE
        else
            ProcessDecodedLine(PChar(#13#10), 2);

        ProcessDecodedLine(FCurrentData, Len);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.ProcessPartHeaderLine;
var
    p       : PChar;
    Delim   : Char;
    Token   : String;
    KeyWord : String;
    Value   : String;
{   Value1  : String; }
begin
    if (FCurrentData = nil) or (FCurrentData^ = #0) then begin
        { End of part header }
        if not FPartHeaderBeginSignaled then begin
            Inc(FPartNumber);
            TriggerPartHeaderBegin;
        end;
        TriggerPartHeaderEnd;
        FHeaderFlag        := FALSE;  { Remember we are no more in a header }
        FLineNum           := 0;
        FUUProcessFlag     := FALSE;
        FProcessFlagYBegin := FALSE;
        FNext              := ProcessPartLine;
        Exit;
    end;

    Inc(FLineNum);
    if FLineNum = 1 then begin
        Inc(FPartNumber);
        FPartHeaderBeginSignaled := TRUE;
        TriggerPartHeaderBegin;
{       FEmbeddedBoundary.clear; }
    end;

    { A header line can't begin with a space nor tab char. If we got that }
    { then we consider the header as begin finished and process line      }
    if FHeaderFlag and IsSpaceChar(FCurrentData[0]) then begin
        TriggerPartHeaderEnd;
        FHeaderFlag        := FALSE;
        FLineNum           := 0;
        FUUProcessFlag     := FALSE;
        FProcessFlagYBegin := FALSE;
        FNext              := ProcessPartLine;
        ProcessPartLine;
        Exit;
    end;

    p := GetToken(FCurrentData, KeyWord, Delim);
    if KeyWord = 'content-type' then begin
        p := GetTokenEx(p, FPartContentType, Delim);
        while Delim = ';' do begin
            p := GetToken(p, Token, Delim);
            if Delim = '=' then begin
                p := GetValue(p, Value, Delim);
                if Token = 'name' then
                    FPartName     := Value
                else if Token = 'charset' then
                    FPartCharset := Value
                else if Token = 'format' then
                    FPartFormat := Value
                else if Token = 'boundary' then begin
                    { we have an embedded boundary }
                    FEmbeddedBoundary.Add('--' + LowerCase(Value));
{                   Value := Value + #0;  }{ NUL terminate string for Delphi 1 }
{                   GetQuoted(@Value[1], Value1);}                    { ##ERIC }
{                   FEmbeddedBoundary.Add('--' + LowerCase(Value1));} { ##ERIC }
                end;                                                  { ##ERIC }
            end;
        end;
    end
    else if KeyWord = 'content-transfer-encoding' then begin
        GetTokenEx(p, FPartEncoding, Delim);
    end
    else if KeyWord = 'content-id' then begin
        FPartContentID := StrPas(p);
        if (Length(FPartContentID) >= 2) and
           (FPartContentID[1] = '<') and
           (FPartContentID[Length(FPartContentID)] = '>') then
               FPartContentID := Copy(FPartContentID, 2, Length(FPartContentID) - 2);
    end
    else if KeyWord = 'content-disposition' then begin
        p := GetTokenEx(p, FPartDisposition, Delim);
        while Delim = ';' do begin
            p := GetToken(p, Token, Delim);
            if Delim = '=' then begin
                p := GetQuoted(p, Value);
                if Token = 'filename' then
                    FPartFileName := Value;
            end;
        end;
    end
    else if (KeyWord = 'content-description') and (FPartFileName = '') then begin
        Delim:= ';';
        while Delim = ';' do begin
            p := GetToken(p, Token, Delim);
            if Delim = '=' then begin
                p := GetQuoted(p, Value);
                if Token = 'filename' then
                    FPartFileName := Value;
            end;
        end;
    end;

    TriggerPartHeaderLine;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.ProcessHeaderLine;
var
    p     : PChar;
    pVal  : PChar;
    Delim : Char;
    Token : String;
    Value : String;
begin
    if (FCurrentData = nil) or (FCurrentData^ = #0) then begin
        FHeaderFlag        := FALSE;  { We are no more in a header }
        TriggerHeaderEnd;
        FLineNum           := 0;
        FUUProcessFlag     := FALSE;
        FProcessFlagYBegin := FALSE;
        if FBoundary = '' then
            FNext := ProcessMessageLine
        else begin
            TriggerPartBegin;
            FPartFirstLine := TRUE;
            FNext          := ProcessWaitBoundary;
        end;
        Exit;
    end;

    Inc(FLineNum);
    if FLineNum = 1 then
        TriggerHeaderBegin;

    p    := GetToken(FCurrentData, Token, Delim);
    pVal := StpBlk(p);
    if Delim = ':' then begin
        p := GetTokenEx(p, Value, Delim);
        if Token = 'from' then
            FFrom := GetHeaderValue(pVal)
        else if Token = 'to' then
            FDest := GetHeaderValue(pVal)
        else if Token = 'cc' then
            FCc := GetHeaderValue(pVal)
        else if Token = 'subject' then
            FSubject := GetHeaderValue(pVal)
        else if Token = 'return-path' then begin
            FReturnPath := GetHeaderValue(pVal);
            if (Length(FReturnPath) >= 2) and
               (FReturnPath[1] = '<') and
               (FReturnPath[Length(FReturnPath)] = '>') then
                FReturnPath := Copy(FReturnPath, 2, Length(FReturnPath) - 2);
        end
        else if Token = 'date' then
            FDate := GetHeaderValue(pVal)
        else if Token = 'mime-version' then
            FMimeVersion := GetHeaderValue(pVal)
        else if Token = 'content-type' then begin
            FContentType := Value;
            while Delim = ';' do begin
                p := GetToken(p, Token, Delim);
                if Delim = '=' then begin
                    p := GetValue(p, Value, Delim);
                    if Token = 'name' then
                        FHeaderName := Value
                    else if Token = 'charset' then
                        FCharset := Value
                    else if Token = 'format' then
                        FFormat := Value
                    else if Token = 'boundary' then begin
                        FBoundary := '--' + LowerCase(Value);
                        FIsMultipart := TRUE;
                    end;             { ##ERIC }
                end;
            end;
        end
        else if Token = 'content-transfer-encoding' then
            FEncoding := Value
        else if Token = 'content-disposition' then begin
            FDisposition := Value;
            while Delim = ';' do begin
                p := GetToken(p, Token, Delim);
                if Delim = '=' then begin
                    p := GetValue(p, Value, Delim);
{                   p := GetQuoted(p, Value);}
                    if Token = 'filename' then
                        FFileName := Value;
                end
            end
        end
    end;
    FLengthHeader := FLengthHeader + Integer(StrLen(FCurrentData)) + 2;
    FHeaderLines.Add(GetHeaderValue(FCurrentData));
    TriggerHeaderLine;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.MessageEnd;
begin
    if (FBoundary = '') or FPartOpened then
        TriggerPartEnd;
    TriggerMessageEnd;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.MessageBegin;
begin
    FApplicationType         := '';
    FBoundary                := '';
    FCharset                 := '';
    FContentType             := '';
    FCurrentData             := nil;
    FDate                    := '';
    FDest                    := '';
    FDisposition             := '';
    FEncoding                := '';
    FEndOfMime               := FALSE;
    FFileName                := '';
    FFormat                  := '';
    FFrom                    := '';
    FCc                      := '';
    FHeaderFlag              := TRUE;
    FHeaderName              := '';
    FIsMultiPart             := FALSE;
    FLineNum                 := 0;
    FMimeVersion             := '';
    FNext                    := ProcessHeaderLine;
    FPartContentType         := '';
    FPartCharset             := '';
    FPartContentID           := '';
    FPartDisposition         := '';
    FPartEncoding            := '';
    FPartFileName            := '';
    FPartFormat              := '';
    FPartHeaderBeginSignaled := FALSE;
    FPartName                := '';
    FPartNumber              := 0;
    FPartOpened              := FALSE;
    FReturnPath              := '';
    FSubject                 := '';
    FUUProcessFlag           := FALSE;
    FProcessFlagYBegin       := FALSE;
    FHeaderLines.Clear;
    FEmbeddedBoundary.Clear;
    FLengthHeader := 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.DecodeFile(FileName : String);
var
    aStream  : TStream;
begin
    aStream  := TFileStream.Create(FileName, fmOpenRead);
    try
        DecodeStream(aStream);
    finally
        aStream.Destroy;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure TMimeDecode.DecodeStream(aStream : TStream);
begin
    FBufferSize := 2048;      { Start with a reasonable FBuffer }
    GetMem(FBuffer, FBufferSize);
    try
        cUUFilename       := '';                    { ##ERIC }
        FEmbeddedBoundary := TStringList.Create;    { ##ERIC }
        try
            InternalDecodeStream(aStream);
        finally
            FEmbeddedBoundary.Free;                 { ##ERIC }
        end;
    finally
        FreeMem(FBuffer, FBufferSize);
        FBuffer     := nil;
        FBufferSize := 0;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This routine use an intelligent buffer management, trying to move data    }
{ the less possible times. The buffer is enlarged as necessary to contains  }
{ the largest line we encounter.                                            }
procedure TMimeDecode.InternalDecodeStream(aStream : TStream);
var
    RdCnt   : LongInt;
    nUsed   : Integer;
    nStart  : Integer;
    nLast   : Integer;
    nSearch : Integer;
    I, J    : Integer;
begin
    nUsed  := 0;
    nStart := 0;
    MessageBegin;
    while TRUE do begin
        nSearch := nStart + nUsed;
        RdCnt   := aStream.Read(FBuffer[nSearch],
                                FBufferSize - nUsed - nStart -
                                2);  { for next char and #0 }
        if RdCnt <= 0 then begin
            break;
        end;

        nUsed  := nUsed + RdCnt;
        nLast  := nStart + nUsed;

        { Nul terminate the FBuffer }
        FBuffer[nLast] := #0;

        { Search for terminating line feed }
        while TRUE do begin
            I := nSearch;
            while (I < nLast) and (FBuffer[I] <> #10) do
                Inc(I);
            if I >= nLast then begin
                { We did'nt find any LF in the FBuffer, need to read more ! }
                if nStart > (3 * (FBufferSize div 4)) then begin
                    { Reuse start of FBuffer because 3/4 buffer is unused   }
                    Move(FBuffer[nStart], FBuffer[0], nUsed + 1);
                    nStart := 0;
                end
                else begin
                    { Makes FBuffer larger }
                    {$IFDEF VER80}
                    FBuffer := ReallocMem(FBuffer, FBufferSize, FBufferSize + 32);
                    {$ELSE}
                    ReallocMem(FBuffer, FBufferSize + 32);
                    {$ENDIF}
                    FBufferSize := FBufferSize + 32;
                end;
                break;
            end;

            { We found a line feed, process FBuffer up to this point }
            { Remove any preceding CR                               }
            if (I > nStart) and (FBuffer[I - 1] = #13) then
                J := I - 1
            else
                J := I;

            { We found a LF, if we are processing a header, we must     }
            { have the next character to see if the line is continuated }
            if FHeaderFlag then begin
                if I >= (nLast - 1) then begin
                    { We don't have the next character in our FBuffer, }
                    { we need to read more data                        }
                    { Read a single byte at the end of the FBuffer     }
                    { We have room because we preserved it previously  }
                    RdCnt := aStream.Read(FBuffer[I + 1], 1);
                    if RdCnt > 0 then begin
                        { We have read the next char }
                        Inc(nLast);
                        Inc(nUsed);
                        FBuffer[I + 2] := #0;
                    end;
                end;

                if I < nLast then begin
                    if (not IsCrLfChar(FBuffer[nStart])) and  { 27/08/98 }
                       IsSpaceChar(FBuffer[I + 1]) then begin
                        { We have a continuation line, replace CR, LF, TAB }
                        { by #1 which will be handled in GetHeaderValue    }
                        FBuffer[I] := #1;
                        FBuffer[J] := #1;
                        if FBuffer[I + 1]= #9 then
                            FBuffer[I + 1] := #1;
                        nSearch   := I;
                        { and search new end of line }
                        continue;
                    end;
                end;
            end;

            FBuffer[J]   := #0;
            FCurrentData := FBuffer + nStart;

            FNext;
            FBuffer[J] := #10;         {tap: ERROR ? #13}
            nStart     := I + 1;
            nUsed      := nLast - nStart;
            nSearch    := nStart;
        end;
    end;
    { Process the last line }
    if nUsed > 0 then begin
        FCurrentData := FBuffer + nStart;
        FNext;
    end;

    MessageEnd;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsSpaceChar(Ch : Char) : Boolean;
begin
    Result := (Ch = ' ') or (Ch = #9);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsCrLfChar(Ch : Char) : Boolean;
begin
    Result := (Ch = #10) or (Ch = #13);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsCrLf1Char(Ch : Char) : Boolean;
begin
    Result := (Ch = #10) or (Ch = #13) or
              (Ch = #1);                     // #1 is used to handle line break
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsCrLf1OrSpaceChar(Ch : Char) : Boolean;
begin
    Result := (Ch = #10) or (Ch = #13) or
              (Ch = #1) or                  // #1 is used to handle line break
              (Ch = ' ') or (Ch = #9);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsCharInSysCharSet(Ch : Char; const MySet : TSysCharSet) : Boolean;
begin
{$IF SIZEOF(CHAR) > 1}
    if Ord(Ch) > 255 then
        Result := FALSE
    else
{$IFEND}
        Result := AnsiChar(Ch) in MySet;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}

end.

