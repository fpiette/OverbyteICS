{*_* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *


Author:       François PIETTE
Object:       Mime support routines (RFC2045).
Creation:     May 03, 2003  (Extracted from SmtpProt unit)
Version:      6.06
EMail:        francois.piette@overbyte.be   http://www.overbyte.be
Support:      Use the mailing list twsocket@elists.org
              Follow "support" link at http://www.overbyte.be for subscription.
Legal issues: Copyright (C) 2003-2009 by François PIETTE
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

History:
May 03, 2003  V1.00 Initial release
Jun 19, 2003  V1.01 Fixed SplitQuotedPrintableString. Thanks to Arno Garrels
                    <arno.garrels@gmx.de>
Jan 12, 2004  V1.02 Marc HUBAUT <mhu@wanadoo.fr> fixed DoFileEncBase64 in case
                    of file size is a multple of 3.
May 31, 2004  V1.03 Used ICSDEFS.INC, added const with version and copyright
May 28, 2005  V1.04 Piotr Hellrayzer Dalek <enigmatical@interia.pl>
              added a fast quoted-printable encoder. Arno Garrels
              <arno.garrels@gmx.de> added some routines and fixed a bug in
              func. SplitQuotedPrintableString.
Jan 28, 2006  V1.05 Gerhard Rattinger fixed TSysCharSet for Delphi 3
Mar 26, 2006  V6.00 New version 6.00 started
Dec 14, 2006  V6.01 Updated Base64Decode to ignore CR and LF
Jul 28, 2007  V6.02 Updated for DotNET
Aug 29, 2007  V6.03 A. Garrels added functions DoFileEncQuotedPrintable and
                    DoTextFileReadNoEncoding.
Mar 10, 2008  V6.04 Francois Piette made some changes to prepare code
                    for Unicode.
                    Call StrPas with appropriate typecast
Jan 03, 2009 V6.05  A. Garrels added a PAnsiChar overload to Base64Encode().
May 02, 2009 V6.06  A. Garrels fixed a bug in IcsWrapTextEx that could break
                    multi-byte characters (back port from V7).

 * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
unit OverbyteIcsMimeUtils;

{$B-}           { Enable partial boolean evaluation   }
{$T-}           { Untyped pointers                    }
{$X+}           { Enable extended syntax              }
{$I OverbyteIcsDefs.inc}
{$IFDEF DELPHI6_UP}
    {$WARN SYMBOL_PLATFORM   OFF}
    {$WARN SYMBOL_LIBRARY    OFF}
    {$WARN SYMBOL_DEPRECATED OFF}
    {$DEFINE USE_BUFFERED_STREAM}
{$ENDIF}
{$IFNDEF VER80}   { Not for Delphi 1                    }
    {$H+}         { Use long strings                    }
    {$J+}         { Allow typed constant to be modified }
{$ENDIF}
{$IFDEF BCB3_UP}
    {$ObjExportAll On}
{$ENDIF}

interface

{$R-}

uses
{$IFDEF CLR}
    System.Text,
    System.IO,
{$ENDIF}
{$IFDEF USE_BUFFERED_STREAM}
    OverbyteIcsStreams,
{$ENDIF}
    SysUtils, Classes;

type
{$IFDEF CLR}
    // DotNet has 16 bits characters which are not allowed in sets
    // So we must use bytes
    TSetType    = Byte;
    TSysCharSet = set of Byte;
{$ELSE}
    TSetType    = Char;
{$IFNDEF DELPHI4_UP}
    TSysCharSet = set of Char;
{$ENDIF}
{$ENDIF}

const
    TMimeUtilsVersion = 606;
    CopyRight : String = ' MimeUtils (c) 2003-2009 F. Piette V6.06 ';

{$IFDEF CLR}
    SpecialsRFC822 : TSysCharSet = [Ord('('), Ord(')'), Ord('<'), Ord('>'), Ord('@'), Ord(','), Ord(';'), Ord(':'),
                                    Ord('\'), Ord('"'), Ord('['), Ord(']'), Ord('.')];
    CrLfSet : TSysCharSet = [13, 10];
    QuotedCharSet : TSysCharSet = [Ord('?'), Ord('='), Ord(' '), Ord('_')];
    BreakCharsSet : TSysCharSet = [9, 32, Ord(';'), Ord(','), Ord('>'), Ord(']')];
{$ELSE}
    SpecialsRFC822 : TSysCharSet = ['(', ')', '<', '>', '@', ',', ';', ':',
                                    '\', '"', '[', ']', '.'];
    CrLfSet : TSysCharSet = [#13, #10];
    QuotedCharSet : TSysCharSet = ['?', '=', ' ', '_'];
    BreakCharsSet : TSysCharSet = [#9, #32, ';', ',', '>', ']'];
{$ENDIF}

    HexTable : array[0..15] of Char = ('0','1','2','3','4','5','6','7','8','9',
                                       'A','B','C','D','E','F');      {HLX}


{ Functions to encode/decode string as a "quoted-printable" string RFC2045}
function  EncodeQuotedPrintable(const S: String) : String;
function  DecodeQuotedPrintable(const S: String) : String;
function  SplitQuotedPrintableString(const S : String) : String;
{ Find a Content-Type from a file name                                   }
function  FilenameToContentType(FileName : String) : String;
{ Base 64 encoding }
function  Base64Encode(const Input : String) : String; overload;
{$IFNDEF CLR}
function  Base64Encode(const Input : PAnsiChar; Len: Integer) : AnsiString; overload;
{$ENDIF}
{ Similar to Base64Encode, returns just a coded line                      }
function Base64EncodeEx(const Input : String;
                        MaxCol      : Integer;
                        var cPos    : Integer) : String;
function  Base64Decode(const Input : String) : String; overload;
{$IFDEF CLR}
function Base64Encode(Input : StringBuilder) : StringBuilder; overload;
function Base64Decode(Input : StringBuilder) : StringBuilder; overload;
{$ENDIF}
function  InitFileEncBase64(const FileName : String;
                            ShareMode      : Word) : TStream;
function  DoFileEncBase64(var Stream     : TStream;
                          var More       : Boolean) : String;
function  DoFileEncQuotedPrintable(var Stream     : TStream;                {AG}
                          var More       : Boolean) : String;
function  DoTextFileReadNoEncoding(var Stream     : TStream;                {AG}
                          var More       : Boolean) : String;
function  DoFileLoadNoEncoding(var Stream     : TStream;                    {Bjørnar}
                          var More       : Boolean) : String;
procedure EndFileEncBase64(var Stream : TStream);
{ Dot at start of line escaping for SMTP and NNTP (double the dot)        }
procedure DotEscape(var S : String; OnlyAfterCrLf : Boolean = False); {AG 11/04/07}
{ Text wrap and folding                                                   } {AG}
{function  IcsWrapText(const Line,
                      BreakStr   : String;
                      BreakCharsSet : TSysCharSet;
                      MaxCol     : Integer;
                      QuoteChars : TSysCharSet): String;}
{ Similar to IcsWrapText, returns just a single line                      } {AG}
function IcsWrapTextEx(const Line : String;
                       const BreakStr : String;
                       const BreakingChars: TSysCharSet;
                       MaxCol       : Integer;
                       QuoteChars   : TSysCharSet;
                       var cPos: Integer;
                       ForceBreak: Boolean = False): String;
{ Unfolds folded headers                                                  } {AG}
function UnFoldHdrLine(const S : String): String;
{Helper function                                                          }
function NeedsEncoding(const S : String) : Boolean;                         {AG}
{$IFDEF WIN32}
function NeedsEncodingPChar(S : PChar) : Boolean;                           {FP}
{$ENDIF}
{ MIME In-Line-Encoding plus Folding, see comments in function source     } {AG}
function HdrEncodeInLine(const Input   : String;
                         Specials      : TSysCharSet; { Try const SpecialsRFC822 }
                         EncType       : Char;        { Either 'Q' or 'B'        }
                         const CharSet : String;      { e.g. 'iso-8859-1'        }
                         MaxCol        : Integer;
                         DoFold         : Boolean): String;
{ Alternate to functions
{ EncodeQuotedPrintable + SplitQuotedPrintableString + DotEscape          }
function StrEncodeQP(const Input : String;                                  {HLX, AG}
                     MaxCol      : Integer;
                     Specials    : TSysCharSet): String;
{ Similar to StrEncodeQP, returns just a single line                      } {AG}
function StrEncodeQPEx(const Buf   : String;
                       MaxCol      : Integer;
                       Specials    : TSysCharSet;
                       ShortSpace  : Boolean; {'_' e.g. for in-line}
                       var cPos    : Integer;
                       DoFold      : Boolean) : String;

procedure FoldHdrLine(HdrLines      : TStrings;                             {AG}
                      const HdrLine : String);

function FoldString(const Input : String;                                {AG}
                    BreakCharsSet : TSysCharSet;
                    MaxCol      : Integer): String;

function IsCharInSysCharSet(Ch : TSetType; const MySet : TSysCharSet) : Boolean;

implementation

{$IFDEF DELPHI1}
{ LeadBytes is a char set that indicates which char values are lead bytes
  in multibyte character sets (Japanese, Chinese, etc).
  This set is always empty for western locales. }
const
  LeadBytes: set of Char = [];
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF DELPHI1}
{ Delphi 1 miss the SetLength procedure. So we rewrite it. }
procedure SetLength(var S: string; NewLength: Integer);
begin
    S[0] := chr(NewLength);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TrimRight(Str : String) : String;
var
    I : Integer;
begin
    I := Length(Str);
    while (I > 0) and (Str[I] in [' ', #9]) do
        I := I - 1;
    Result := Copy(Str, 1, I);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function TrimLeft(Str : String) : String;
var
    I : Integer;
begin
    if Str[1] <> ' ' then
        Result := Str
    else begin
        I := 1;
        while (I <= Length(Str)) and (Str[I] = ' ') do
            I := I + 1;
        Result := Copy(Str, I, Length(Str) - I + 1);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Trim(Str : String) : String;
begin
    Result := TrimLeft(TrimRight(Str));
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ See also SplitQuotedPrintableString !                                     }
function EncodeQuotedPrintable(const S: String) : String;
var
    I, J : Integer;
begin
    Result := '';
    I      := 1;
    while I <= Length(S) do begin
        J := I;
        while (I <= Length(S)) and
              (S[I] <> '=') and
              (S[I] >= ' ') and
              (Ord(S[I]) <= 126) do
            Inc(I);
        if I > Length(S) then begin
            if J = 1 then
                Result := S     { Optimisation }
            else
                Result := Result + Copy(S, J, I - J);
            Exit;
        end;
        Result := Result + Copy(S, J, I - J) + '=' +
                  UpperCase(IntToHex(Ord(S[I]), 2));
        Inc(I);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ A line ending with an equal sign is continued on the next line. This is   }
{ what RFC2045 refers as a "soft line break".                               }
{ This routine doesn't take care of the equal sign at the end of string.    }
{ It is simply ignored. The caller must check that condition and merge      }
{ successives lines. But the routine handle embedded soft line break.       }
function DecodeQuotedPrintable(const S: String) : String;
var
    I, J : Integer;
begin
    Result := '';
    I      := 1;
    while I <= Length(S) do begin
        J := I;
        while (I <= Length(S)) and (S[I] <> '=') do
            Inc(I);
        Result := Result + Copy(S, J, I - J);
        if I >= Length(S) then
            break;
        if S[I + 1] = #13 then  { Could also check for #10 }
            { Soft line break, nothing to do except continuing }
        else
            Result := Result + Char(StrToInt('$' + Copy(S, I + 1, 2)));
        Inc(I, 3);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function SplitQuotedPrintableString(const S : String) : String;
var
    I, J : Integer;
begin
    if Length(S) <= 76 then begin
        { No need to split }
        Result := S;
        Exit;
    end;
    Result := '';
    J      := 1;
    I      := 76;
    while TRUE do begin
        if S[I - 1] = '=' then
            Dec(I)
        else if S[I - 2] = '=' then
            Dec(I, 2);
        Result := Result + Copy(S, J, I - J) + '=' + #13#10;
        J      := I;
        Inc(I, 75);
        if I > Length(S) then begin
            Result := Result + Copy(S, J, I - J);
            break;
        end;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure DotEscape(var S : String; OnlyAfterCrLf : Boolean = False);  {AG 11/04/07}
var
    I : Integer;
begin
    if S = '' then
        Exit;
    if (S[1] = '.') and not OnlyAfterCrLf then begin  {AG 11/04/07}
        Insert('.', S, 1);
        I := 3;
    end
    else
        I := 1;
    while I <= (Length(S) - 2) do begin  // {AG 10/29/07}
        if (S[I] = #13) and (S[I + 1] = #10) and (S[I + 2] = '.') then begin
            Insert('.', S, I + 2);
            Inc(I, 4);
            continue;
        end;
        Inc(I);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function FilenameToContentType(FileName : String) : String;
var
    Ext : String;
begin
    { We probably should the registry to find MIME type for known file types }
    Ext := LowerCase(ExtractFileExt(FileName));
    if Length(Ext) > 1 then
        Ext := Copy(Ext, 2, Length(Ext));
    if (Ext = 'htm') or (Ext = 'html') then
        Result := 'text/html'
    else if Ext = 'gif' then
        Result := 'image/gif'
    else if Ext = 'bmp' then
        Result := 'image/bmp'
    else if (Ext = 'jpg') or (Ext = 'jpeg') then
        Result := 'image/jpeg'
    else if Ext = 'txt' then
        Result := 'text/plain'
    else
        Result := 'application/octet-stream';
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function  InitFileEncBase64(
    const FileName : String;
    ShareMode      : Word) : TStream;
begin
{$IFNDEF USE_BUFFERED_STREAM}
    Result := TFileStream.Create(FileName, fmOpenRead or ShareMode);
{$ELSE}
    Result := TBufferedFileStream.Create(FileName, fmOpenRead or ShareMode, 4096);
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
const
    Base64Out: array [0..64] of Char = (
        'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I', 'J', 'K', 'L', 'M',
        'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X', 'Y', 'Z',
        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm',
        'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
        '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '+', '/', '='
    );
    Base64In: array[0..127] of Byte = (
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

{Bjørnar}
function DoFileLoadNoEncoding(
    var Stream     : TStream;
    var More       : Boolean) : String;
const
    MAX_LENGTH            = 76; {HLX: Longer lines = less CRLF's, RFC does allow lines *that* long}
    MULTIPLIER            = 4;
    MAX_READ              = MAX_LENGTH * MULTIPLIER;
var
    DataOut      : array [0..MAX_READ]  of Byte;
    ByteCount    : Integer;
    //I          : Integer;
    //Lines      : Integer;
begin

    ByteCount := Stream.Read(DataOut, MAX_READ);
    //Lines := ByteCount div MAX_READ;
    //Insert(#09, Result, 1);
    SetLength(Result, ByteCount);// + (Lines * 2));
    Move(DataOut[0], Result[1], Length(Result));
    { Splitting lines
    I := MAX_LENGTH + 1;
    while I < Lines do begin;
        Insert(#13#10, Result, I);
        Inc(I, MAX_LENGTH + 2);
        Inc(Lines);
    end;}
    More := (ByteCount = MAX_READ);
end;
{Bjørnar}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This is a slow function, it realy should be used with TBufferedFileStream }
function DoTextFileReadNoEncoding(                                       {AG}
    var Stream : TStream;
    var More   : Boolean) : String;
const
    LINE_LENGTH  = 1022;
var
    Cnt  : Integer;
    I    : Integer;
    Buf  : Char;
begin
    I   := 0;
    Cnt := 1;
    SetLength(Result, LINE_LENGTH);
    while (I < LINE_LENGTH + 2) and (Cnt = 1) do begin
        Cnt := Stream.Read(Buf, 1);
        if (Cnt = 1) then begin
            if not IsCharInSysCharSet(Buf, CrLfSet) then begin
                if I >= LINE_LENGTH then begin
                    Stream.Seek(-1, sofromCurrent);
                    Break;
                end
                else
                Result[I + 1] := Buf
            end
            else begin
                if (Buf = #13) then
                  Continue
                else
                  Break;
            end;
            Inc(I);
        end
    end;
    if I <> Length(Result) then
        SetLength(Result, I);
    More := Stream.Position <> Stream.Size;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function DoFileEncQuotedPrintable(                                       {AG}
    var Stream : TStream;
    var More   : Boolean) : String;
const
    LINE_LENGTH = 73; // + 2 = 75 + trailing '=' = 76
var
    Cnt  : Integer;
    I    : Integer;
    Buf  : Char;
begin
    I   := 0;
    Cnt := 1;
    SetLength(Result, LINE_LENGTH + 3);
    while (I < LINE_LENGTH) and (Cnt = 1) do begin
        Cnt := Stream.Read(Buf, 1);
        if (Cnt = 1) then begin
            if (Ord(Buf) > 126)  or
               (Ord(Buf) < 32)   or
               IsCharInSysCharSet(Buf, ['=', '.']) then begin
                Inc(I);
                Result[I] := '=';
                Inc(I);
                Result[I] := HexTable[(Ord(Buf) shr 4) and 15];
                Inc(I);
                Result[I] := HexTable[Ord(Buf) and 15];
            end
            else begin
                Inc(I);
                Result[I] := Buf;
            end;
        end;
    end;
    if I > 0 then begin
        Inc(I);
        Result[I] := '=';
    end;
    if I <> Length(Result) then
        Setlength(Result, I);
    More := Cnt <> 0;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF OLD_VERSION}
function DoFileEncBase64(
    var Stream     : TStream;
    var More       : Boolean) : String;
const
    HLX_MULTIPLIER        = 3;  { for three lines at once }
    MAX_LENGTH            = 76; {HLX: Longer lines = less CRLF's, RFC does allow lines *that* long}
    MAX_LENGTH_MULTIPLIED = (MAX_LENGTH + 2) * HLX_MULTIPLIER;
    MAX_READ              = ((MAX_LENGTH * 3)div 4) * HLX_MULTIPLIER;
    MAX_READ_MOD          = (MAX_LENGTH * 3) div 4;
var
    Count, Place : Integer;
    DataIn       : array [0..MAX_READ]  of Byte;
    DataOut      : array [0..MAX_LENGTH_MULTIPLIED + 8] of Byte;
    ByteCount    : Integer;
    I            : Integer;
{ HLX: The following code is rewritten, so it loads data in MAX_READ chunks and
  encodes all loaded data. The trick relies on the fact that TriggerGetData's
  MsgLine buffer can hold up to 1024 chars. We'll encode 3 lines at once,
  add CRLF's, and return all three as one: component will see it as one,
  server will still see it as three.
  I've noticed a strange behavior: having HLX_MULTIPLIER > 3, data aren't
  sent completely, although it shouldn't occur
  (see: TCustomSmtpClient.DataNext) }
begin
    Count     := 0;
    Place     := 0;
    ByteCount := Stream.Read(DataIn, MAX_READ);
    while Place < ByteCount do begin
        DataOut[Count] := (DataIn[Place] and $FC) shr 2;
        Inc(Count);
        DataOut[Count] := (DataIn[Place] and $03) shl 4;
        Inc(Place);
        if Place < ByteCount then begin
            DataOut[Count] := DataOut[Count] + (DataIn[Place] and $F0) shr 4;
            Inc(Count);
            DataOut[Count] := (DataIn[Place] and $0F) shl 2;
            Inc(Place);
            if Place < ByteCount then begin
                DataOut[Count] := DataOut[Count] + (DataIn[Place] and $C0) shr 6;
                Inc(Count);
                DataOut[Count] := (DataIn[Place] and $3F);
                Inc(Place);
                Inc(Count);
            end
            else begin
                Inc(Count);
                DataOut[Count] := $40;
                Inc(Count);
            end;
        end
        else begin
            Inc(Count);
            DataOut[Count] := $40;
            Inc(Count);
            DataOut[Count] := $40;
            Inc(Count);
        end;
    end;
    { Moved out of the main loop, so it has the chance to work in the }
    { processor's L1 Cache                                            }
    SetLength(Result, Count);
    for I := 0 to Count - 1 do
        DataOut[I] := Byte(Base64Out[DataOut[I]]);
    Move(DataOut[0], Result[1], Count);
    { Splitting lines }
    I := MAX_LENGTH + 1;
    while I < Count do begin;
        Insert(#13#10, Result, I);
        Inc(I, MAX_LENGTH + 2);
        Inc(Count);
    end;
    More := (ByteCount = MAX_READ);
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function DoFileEncBase64(
    var Stream     : TStream;
    var More       : Boolean) : String;
const
    MAX_LENGTH = 72;
var
    Count     : Integer;
    DataIn    : array [0..2]  of Byte;
    DataOut   : array [0..MAX_LENGTH + 8] of Byte;
    ByteCount : Integer;
    I         : Integer;
{$IFDEF CLR}
    SB        : StringBuilder;
{$ENDIF}
begin
    Count     := 0;
    ByteCount := 0;
    while Count < MAX_LENGTH do begin
        ByteCount          := Stream.Read(DataIn, 3);
        if ByteCount = 0 then                            {<=MHU}
           Break;                                        {<=MHU}
        DataOut[Count]     := (DataIn[0] and $FC) shr 2;
        DataOut[Count + 1] := (DataIn[0] and $03) shl 4;
        if ByteCount > 1 then begin
            DataOut[Count + 1] := DataOut[Count + 1] +
                                  (DataIn[1] and $F0) shr 4;
            DataOut[Count + 2] := (DataIn[1] and $0F) shl 2;
            if ByteCount > 2 then begin
                DataOut[Count + 2] := DataOut[Count + 2] +
                                      (DataIn[2] and $C0) shr 6;
                DataOut[Count + 3] := (DataIn[2] and $3F);
            end
            else begin
                DataOut[Count + 3] := $40;
            end;
        end
        else begin
            DataOut[Count + 2] := $40;
            DataOut[Count + 3] := $40;
        end;

        for I := 0 to 3 do
            DataOut[Count + I] := Byte(Base64Out[DataOut[Count + I]]);

        Count := Count + 4;
        if (Count > MAX_LENGTH) or (ByteCount < 3) then
            break;
    end;

    DataOut[Count] := $0;
    More           := (ByteCount = 3);
{$IFDEF CLR}
    SB := StringBuilder.Create(Count);
    for I := 0 to Count - 1 do
        SB[I] := Char(DataOut[I]);
    Result := SB.ToString;
{$ELSE}
    Result := StrPas(PAnsiChar(@DataOut[0]));
{$ENDIF}
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
procedure EndFileEncBase64(var Stream : TStream);
begin
    if Assigned(Stream) then begin
        Stream.Free;
        Stream := nil;
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFNDEF CLR}
function Base64Encode(const Input : PAnsiChar; Len: Integer) : AnsiString;
var
    Count : Integer;
begin
    Result := '';
    Count  := 0;
    while Count < Len do begin
        Result := Result + Base64Out[(Byte(Input[Count]) and $FC) shr 2];
        if (Count + 1) < Len then begin
            Result := Result + Base64Out[((Byte(Input[Count]) and $03) shl 4) +
                                         ((Byte(Input[Count + 1]) and $F0) shr 4)];
            if (Count + 2) < Len then begin
                Result := Result + Base64Out[((Byte(Input[Count + 1]) and $0F) shl 2) +
                                             ((Byte(Input[Count + 2]) and $C0) shr 6)];
                Result := Result + Base64Out[(Byte(Input[Count + 2]) and $3F)];
            end
            else begin
                Result := Result + Base64Out[(Byte(Input[Count + 1]) and $0F) shl 2];
                Result := Result + '=';
            end
        end
        else begin
            Result := Result + Base64Out[(Byte(Input[Count]) and $03) shl 4];
            Result := Result + '==';
        end;
        Count := Count + 3;
    end;
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Base64Encode(const Input : String) : String;
{$IFNDEF CLR}
begin
    Result := Base64Encode(PAnsiChar(Input), Length(Input));
end;
{$ELSE}
var
    Count : Integer;
    Len   : Integer;
begin
    Result := '';
    Count  := 1;
    Len    := Length(Input);
    while Count <= Len do begin
        Result := Result + Base64Out[(Byte(Input[Count]) and $FC) shr 2];
        if (Count + 1) <= Len then begin
            Result := Result + Base64Out[((Byte(Input[Count]) and $03) shl 4) +
                                         ((Byte(Input[Count + 1]) and $F0) shr 4)];
            if (Count + 2) <= Len then begin
                Result := Result + Base64Out[((Byte(Input[Count + 1]) and $0F) shl 2) +
                                             ((Byte(Input[Count + 2]) and $C0) shr 6)];
                Result := Result + Base64Out[(Byte(Input[Count + 2]) and $3F)];
            end
            else begin
                Result := Result + Base64Out[(Byte(Input[Count + 1]) and $0F) shl 2];
                Result := Result + '=';
            end
        end
        else begin
            Result := Result + Base64Out[(Byte(Input[Count]) and $03) shl 4];
            Result := Result + '==';
        end;
        Count := Count + 3;
    end;
end;
{$ENDIF}

{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF CLR}
function Base64Encode(Input : StringBuilder) : StringBuilder;
var
    Count : Integer;
    Len   : Integer;
begin
    Result := StringBuilder.Create;
    Count  := 1;
    Len    := Input.Length;
    while Count <= Len do begin
        Result.Append(Base64Out[(Byte(Input[Count]) and $FC) shr 2]);
        if (Count + 1) <= Len then begin
            Result.Append(Base64Out[((Byte(Input[Count]) and $03) shl 4) +
                                    ((Byte(Input[Count + 1]) and $F0) shr 4)]);
            if (Count + 2) <= Len then begin
                Result.Append(Base64Out[((Byte(Input[Count + 1]) and $0F) shl 2) +
                                        ((Byte(Input[Count + 2]) and $C0) shr 6)]);
                Result.Append(Base64Out[(Byte(Input[Count + 2]) and $3F)]);
            end
            else begin
                Result.Append(Base64Out[(Byte(Input[Count + 1]) and $0F) shl 2]);
                Result.Append('=');
            end
        end
        else begin
            Result.Append(Base64Out[(Byte(Input[Count]) and $03) shl 4]);
            Result.Append('==');
        end;
        Count := Count + 3;
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Similar to Base64Encode, returns just a coded line                        }
function Base64EncodeEx(const Input : String;
                        MaxCol      : Integer;
                        var cPos    : Integer) : String;
var
    Len : Integer;
begin
    Len := Length(Input);
    while cPos <= Len do begin
        if Length(Result) >= MaxCol then
            Exit;
        Result := Result + Base64Out[(Byte(Input[cPos]) and $FC) shr 2];
        if (cPos + 1) <= Len  then begin
            Result := Result + Base64Out[((Byte(Input[cPos]) and $03) shl 4) +
                                   ((Byte(Input[cPos + 1]) and $F0) shr 4)];
            if (cPos + 2) <= Len then begin
                Result := Result + Base64Out[((Byte(Input[cPos + 1]) and $0F) shl 2) +
                                       ((Byte(Input[cPos + 2]) and $C0) shr 6)];
                Result := Result + Base64Out[(Byte(Input[cPos + 2]) and $3F)];
            end
            else begin
                Result := Result + Base64Out[(Byte(Input[cPos + 1]) and $0F) shl 2];
                Result := Result + '=';
            end
        end
        else begin
             Result := Result + Base64Out[(Byte(Input[cPos]) and $03) shl 4];
             Result := Result + '==';
        end;
        Inc(cPos, 3);
    end;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function Base64Decode(const Input : String) : String;
var
    Final   : String;
    Count   : Integer;
    Len     : Integer;
    DataIn0 : Byte;
    DataIn1 : Byte;
    DataIn2 : Byte;
    DataIn3 : Byte;
begin
    Final := '';
    Count := 1;
    Len   := Length(Input);
    while Count <= Len do begin
        if Byte(Input[Count]) in [13, 10] then
            Inc(Count)
        else begin
            DataIn0 := Base64In[Byte(Input[Count])];
            DataIn1 := Base64In[Byte(Input[Count+1])];
            DataIn2 := Base64In[Byte(Input[Count+2])];
            DataIn3 := Base64In[Byte(Input[Count+3])];

            Final := Final + Char(((DataIn0 and $3F) shl 2) +
                                  ((DataIn1 and $30) shr 4));
            if DataIn2 <> $40 then begin
                Final := Final + Char(((DataIn1 and $0F) shl 4) +
                                      ((DataIn2 and $3C) shr 2));
                if DataIn3 <> $40 then
                    Final := Final + Char(((DataIn2 and $03) shl 6) +
                                          (DataIn3 and $3F));
            end;
            Count := Count + 4;
        end;
    end;
    Result := Final;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF CLR}
function Base64Decode(Input : StringBuilder) : StringBuilder;
var
    Count   : Integer;
    Len     : Integer;
    DataIn0 : Byte;
    DataIn1 : Byte;
    DataIn2 : Byte;
    DataIn3 : Byte;
begin
    Result := StringBuilder.Create;
    Count  := 1;
    Len    := Input.Length;
    while Count <= Len do begin
        DataIn0 := Base64In[Byte(Input[Count])];
        DataIn1 := Base64In[Byte(Input[Count+1])];
        DataIn2 := Base64In[Byte(Input[Count+2])];
        DataIn3 := Base64In[Byte(Input[Count+3])];

        Result.Append(Char(((DataIn0 and $3F) shl 2) +
                           ((DataIn1 and $30) shr 4)));
        if DataIn2 <> $40 then begin
            Result.Append(Char(((DataIn1 and $0F) shl 4) +
                               ((DataIn2 and $3C) shr 2)));
            if DataIn3 <> $40 then
                Result.Append(Char(((DataIn2 and $03) shl 6) +
                                   (DataIn3 and $3F)));
        end;
        Count := Count + 4;
    end;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ This function takes the QuoteChars as a parameter and it returns just a   }
{ line.                                                                     }
(*
function IcsWrapTextEx(
    const Line, BreakStr : String;
    BreakCharsSet : TSysCharSet;
                       MaxCol       : Integer;
                       QuoteChars   : TSysCharSet;
                       var cPos     : Integer): String;
var
    Col                : Integer;
    LinePos, LineLen   : Integer;
    BreakLen, BreakPos : Integer;
    QuoteChar, CurChar : Char;
    ExistingBreak      : Boolean;
begin
    Col := 1;
    LinePos := cPos;
    BreakPos := 0;
    QuoteChar := ' ';
    ExistingBreak := False;
    LineLen := Length(Line);
    BreakLen := Length(BreakStr);
    Result := '';
    while cPos <= LineLen do begin
        CurChar := Line[cPos];
        if CurChar in LeadBytes then begin
            Inc(cPos);
            Inc(Col);
        end
        else if CurChar = BreakStr[1] then begin
            if QuoteChar = ' ' then begin
                ExistingBreak := CompareText(BreakStr, Copy(Line, cPos, BreakLen)) = 0;
                if ExistingBreak then begin
                    Inc(cPos, BreakLen-1);
                    BreakPos := cPos;
                end;
            end
        end
        else if TSetType(CurChar) in BreakCharsSet then begin
            if QuoteChar = ' ' then
                BreakPos := cPos
        end
        else if TSetType(CurChar) in QuoteChars then
            if CurChar = QuoteChar then
                QuoteChar := ' '
            else if QuoteChar = ' ' then
                QuoteChar := CurChar;
        Inc(cPos);
        Inc(Col);
        if not (TSetType(QuoteChar) in QuoteChars) and
               (ExistingBreak or ((Col > MaxCol) and (BreakPos > LinePos))) then begin
            { Col := cPos - BreakPos; }
            Result := Result + Copy(Line, LinePos, BreakPos - LinePos + 1);
            if not (TSetType(CurChar) in QuoteChars) then
                while (cPos <= LineLen) and
                      (TSetType(Line[cPos]) in (BreakCharsSet + CrLfSet)) do
                    Inc(cPos);
                if ExistingBreak then
                    Result := Copy(Result, 1, Length(Result) - BreakLen);
            Inc(BreakPos);
            cPos := BreakPos;
            Exit;
        end;
    end;
    Result := Result + Copy(Line, LinePos, MaxInt);
    cPos   := MaxInt;
end;
*)
{ Any BreakStr not in a quoted string is truncated and the function returns!  }
{ i.e. when BreakStr #13#10#9 is found cPos returned is pos of char #9 + 1.   }
{ Breaking chars appear at the end of a line. ForceBreak works outside quoted }
{ strings only and forces a break at MaxCol if no breaking char has been found.}
function IcsWrapTextEx(
  const Line : String;
  const BreakStr: String;
    const BreakingChars  : TSysCharSet;
    MaxCol               : Integer;
    QuoteChars           : TSysCharSet;
    var cPos             : Integer;
    ForceBreak           : Boolean): String;
var
    Col                : Integer;
    LinePos, LineLen   : Integer;
    BreakLen, BreakPos : Integer;
    QuoteChar, CurChar : Char;
    ExistingBreak      : Boolean;
    L                  : Integer;
begin
    Col           := 1;
    LinePos       := cPos;
    BreakPos      := 0;
    QuoteChar     := #0;
    ExistingBreak := False;
    LineLen       := Length(Line);
    BreakLen      := Length(BreakStr);
    Result        := '';
    while cPos <= LineLen do begin
        CurChar := Line[cPos];
        if CurChar in LeadBytes then begin
            L := CharLength(Line, cPos) -1;
            Inc(cPos, L);
            Inc(Col, L);
            CurChar := Line[cPos];
        end;
        //else begin
            if CurChar = BreakStr[1] then begin
                if QuoteChar = #0 then begin
                    ExistingBreak := StrLComp(PChar(BreakStr),
                                              PChar(@Line[cPos]),
                                              BreakLen) = 0;
                    if ExistingBreak then begin
                        Inc(cPos, BreakLen - 1);
                        BreakPos := cPos;
                    end;
                end
            end
            else if IsCharInSysCharSet(TSetType(CurChar),
                                       BreakingChars) then begin
                if QuoteChar = #0 then
                    BreakPos := cPos;
            end
            else if IsCharInSysCharSet(TSetType(CurChar), QuoteChars) then begin
                if CurChar = QuoteChar then begin
                    QuoteChar := #0;
                    if ForceBreak and (Col >= MaxCol) and (BreakPos = 0) then
                        BreakPos := cPos;
                end
                else if QuoteChar = #0 then begin
                    QuoteChar := CurChar;
                    if ForceBreak and (cPos > LinePos) then
                        BreakPos := cPos -1; // Break before the QuoteChar
                end;
            end
            else if ForceBreak and (QuoteChar = #0) and (Col >= MaxCol) and
                    (BreakPos = 0) then begin
                BreakPos := cPos;
            end;
        //end;
        Inc(cPos);
        Inc(Col);

        if (not IsCharInSysCharSet(TSetType(QuoteChar), QuoteChars)) and
           (ExistingBreak or
           ((Col > MaxCol) and (BreakPos >= LinePos))) then begin
            if ExistingBreak then
                Result := Copy(Line, LinePos, BreakPos - LinePos + 1 - BreakLen)
            else
                Result := Copy(Line, LinePos, BreakPos - LinePos + 1);
            if (not IsCharInSysCharSet(TSetType(CurChar), QuoteChars)) or
               (ExistingBreak) then begin
                if cPos <= LineLen then begin
                    if StrLComp(PChar(@Line[cPos]), #13#10, 2) = 0 then begin
                        if not ExistingBreak then begin
                            { Break due to one of the breaking chars found and CRLF follows }
                            Inc(cPos, 2);
                            Exit;
                        end;
                        { Empty line follows }
                        BreakPos := cPos - 1;
                    end;
                end;
            end;
            Inc(BreakPos);
            cPos := BreakPos;
            Exit;
        end;
    end;
    Result := Copy(Line, LinePos, MaxInt);
    cPos := MaxInt;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Unfold header lines                                                       } {AG}
{ RFC822 says "Unfolding is accomplished by regarding CRLF immediately      }
{ followed by a LWSP-char as equivalent to the LWSP-char."                  }
function UnFoldHdrLine(const S : String): String;
var
    I, J : Integer;
begin
    SetLength(Result, Length(S));
    J := 1;
    I := 1;
    while I <= Length(S) do begin
        if S[I] = #13 then begin
            if (I + 2 <= Length(S)) and
               (S[I + 1] = #10)     and
               (Byte(S[I + 2]) in [9, 32]) then begin
                Result[J] := #32;
                Inc(J);
                Inc(I, 2);
            end;
        end
        else begin
            Result[J] := S[I];
            Inc(J);
        end;
        Inc(I);
    end;
    SetLength(Result, J - 1);
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *} {AG}
function NeedsEncoding(const S : String) : Boolean;
var
    I : Integer;
begin
    for I := 1 to Length(S) do
        if Byte(S[I]) in [Byte(0)..Byte(8), Byte(11), Byte(12), Byte(14)..Byte(31), Byte(127)..Byte(255)] then begin
            Result := True;
            Exit;
        end;
    Result := False;
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{$IFDEF WIN32}
function NeedsEncodingPChar(S : PChar) : Boolean;
begin
    while S^ <> #0 do begin
        if IsCharInSysCharSet(S^, [#0..#8, #11, #12, #14..#31, #127..#255]) then begin
            Result := True;
            Exit;
        end;
        Inc(S);
    end;
    Result := False;
end;
{$ENDIF}


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function HdrEncodeInLine(const Input    : String;
                         Specials       : TSysCharSet;
                         EncType        : Char;        { Either 'Q' or 'B' }
                         const CharSet  : String;      { e.g. 'iso-8859-1' }
                         MaxCol         : Integer;
                         DoFold         : Boolean): String;
const
    Suffix = '?=';
var
    Len,
    lPos,
    LenRes      : Integer;
    Prefix,
    Res         : String;
begin
    Result := '';
    if DoFold and (MaxCol < 25) then
        MaxCol := 25;

    if Length(CharSet) < 4 then
        raise Exception.Create('Function ''HdrEncodeInLine'', invalid CharSet: ' +
                                '' + Charset + '');
    if not (Byte(EncType) in [Ord('Q'), Ord('B')]) then
        raise Exception.Create('Function ''HdrEncodeInLine'', invalid EncType: ' +
                                '' + EncType + '');
    Res    := '';
    Prefix := '=?' + LowerCase(CharSet) + '?' + EncType + '?';
    Len    := Length(Input);
    lPos   := 1;

    if EncType = 'Q' then begin
        if lPos <= Len then
        begin
            Res :=  StrEncodeQPEx(Input,
                                  MaxCol - Length(Prefix) - 2,
                                  Specials + QuotedCharSet,
                                  True,
                                  lPos,
                                  DoFold);
            if Length(Res) = 0 then
                Exit;
            if Res[Length(Res)] = '=' then
                SetLength(Res, Length(Res) - 1);
            Result := Prefix + Res  + Suffix;
        end;
        while lPos <= Length(Input) do begin
            Res :=  StrEncodeQPEx(Input,
                                  MaxCol - Length(Prefix) - 2,
                                  Specials + QuotedCharSet,
                                  True,
                                  lPos,
                                  DoFold);
            if (Length(Res) > 0) then begin
                if Res[Length(Res)] = '=' then
                    SetLength(Res, Length(Res) - 1);
                Result := Result + #13#10#09 + Prefix + Res  + Suffix;
            end;
        end;
    end
    else begin
        { Base64 }
        { taken from function B64Encode and modified slightly }
        if not DoFold then
            MaxCol := MaxInt;

        Res    := Res + Prefix;
        LenRes := Length(Prefix) + 2;

        while lPos <= Len do begin
            if (LenRes + 4 > MaxCol) then begin
                Res := Res + Suffix + #13#10#09 + Prefix;
                LenRes := Length(Prefix) + 2;
            end;
            Res := Res + Base64Out[(Byte(Input[lPos]) and $FC) shr 2];
            if (lPos + 1) <= Len  then begin
                Res := Res + Base64Out[((Byte(Input[lPos]) and $03) shl 4) +
                                       ((Byte(Input[lPos + 1]) and $F0) shr 4)];
                if (lPos + 2) <= Len then begin
                    Res := Res + Base64Out[((Byte(Input[lPos + 1]) and $0F) shl 2) +
                                           ((Byte(Input[lPos + 2]) and $C0) shr 6)];
                    Res := Res + Base64Out[(Byte(Input[lPos + 2]) and $3F)];
                end
                else begin
                    Res := Res + Base64Out[(Byte(Input[lPos + 1]) and $0F) shl 2];
                    Res := Res + '=';
                end
            end
            else begin
                 Res := Res + Base64Out[(Byte(Input[lPos]) and $03) shl 4];
                 Res := Res + '==';
            end;
            Inc(LenRes, 4);
            Inc(lPos, 3);
        end;
        Result := Res + Suffix;
    end;
end;




{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Piotr Hellrayzer Dalek <enigmatical@interia.pl>, AG                                                                    }
{ Use it to code message text that includes extended ASCII chars, passing    }
{ empty Specials '[]' will work mostly.                                      }
{ Param MaxCol should be set to 1 below max. line length                     }

function StrEncodeQP(const Input : String;
                     MaxCol      : Integer;
                     Specials    : TSysCharSet) : String;
var
    cPos, rPos, lPos :Integer;
begin;
    SetLength(Result, Length(Input) * 2);
    cPos   := 1;
    lPos   := 1;
    for rPos := 1 to Length(Input) do begin
        if (Ord(Input[rPos]) > 126)  or
           (Ord(Input[rPos]) < 32)   or
           (Input[rPos]      = '=')  or
           IsCharInSysCharSet(TSetType(Input[rPos]), Specials) then begin
            Result[cPos] := '=';
            Inc(cPos);
            Result[cPos] := HexTable[(Ord(Input[rPos]) shr 4) and 15];
            Inc(cPos);
            Result[cPos] := HexTable[Ord(Input[rPos]) and 15];
            Inc(cPos);
            Inc(lPos, 3);
            if lPos >= MaxCol then begin
                Result[cPos] := '=';
                Inc(cPos);
                Result[cPos] := #13;
                Inc(cPos);
                Result[cPos] := #10;
                Inc(cPos);
                lPos := 1;
            end;
        end
        else begin
            Result[cPos] := Input[rPos];
            Inc(cPos);
            Inc(lPos);
            if lPos >= MaxCol then begin
                Result[cPos] := '=';
                Inc(cPos);
                Result[cPos] := #13;
                Inc(cPos);
                Result[cPos] := #10;
                Inc(cPos);
                lPos := 1;
            end;
        end;
        { Grow }
        if cPos > Length(Result) - 3 then
            SetLength(Result, Length(Result) + MaxCol);
    end;
    Setlength(Result, cPos - 1);
end;


{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ Similar to StrEncodeQP, returns just a coded line                          }
function StrEncodeQPEx(const Buf   : String;
                       MaxCol      : Integer;
                       Specials    : TSysCharSet;
                       ShortSpace  : Boolean;
                       var cPos    : Integer;
                       DoFold      : Boolean) : String;
var
    lPosRes : Integer;
begin
    lPosRes := 1;
    if not DoFold then
        MaxCol := Length(Buf);
    SetLength(Result, MaxCol);
    while cPos <= Length(Buf) do begin
        if (Ord(Buf[cPos]) > 126)  or
           (Ord(Buf[cPos]) < 32)   or
           IsCharInSysCharSet(TSetType(Buf[cPos]), Specials) or
           (Buf[cPos] = '=') then begin
            if (Buf[cPos] = ' ') and ShortSpace then begin
                Result[lPosRes] := '_';
                Inc(lPosRes);
                Inc(cPos);
            end
            else
            if lPosRes < MaxCol - 2 then begin
                Result[lPosRes] := '=';
                Inc(lPosRes);
                Result[lPosRes] := HexTable[(Ord(Buf[cPos]) shr 4) and 15];
                Inc(lPosRes);
                Result[lPosRes] := HexTable[Ord(Buf[cPos]) and 15];
                Inc(lPosRes);
                Inc(cPos);
            end
            else begin
                     Result[lPosRes] := '=';
                     Inc(lPosRes);
                     Break;
            end;
        end
        else
                if lPosRes < MaxCol then begin
                    Result[lPosRes] := Buf[cPos];
                    Inc(lPosRes);
                    Inc(cPos);
                end
                else begin
                    Result[lPosRes] := '=';
                    Inc(lPosRes);
                    Break;
                end;
    end;
    SetLength(Result, lPosRes - 1);
end;


{ * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
{ RFC822 - 3.1.1.  LONG HEADER FIELDS                                       }
{ This is just my (AG) interpretation of folding long header lines.         }
{ Probably further BreakCharsSet are possible here.                          }
{ However before you modify this procedure you should refer to RFC822.       }
{ Also note that header lines may be encoded 'in-line' as described in       }
{ RFC2047. The passed HdrLine String *MUST not include CRLF except they      }
{ are followed by one of the space chars, means that a already folded        }
{ line should work. If a string doesn't include one of the BreakChars        }
{ it won't fold to the next line!                                            }
procedure FoldHdrLine(
    HdrLines      : TStrings;
                      const HdrLine : String);
var
    rPos : Integer;
begin
    rPos := 1;
    if rPos <= Length(HdrLine) then
        HdrLines.Add(Trim(IcsWrapTextEx(HdrLine, #13#10#09,
                          BreakCharsSet, 76, [], rPos)));
    while rPos <= Length(HdrLine) do
        HdrLines.Add(#09 + Trim(IcsWrapTextEx(HdrLine, #13#10#09,
                                BreakCharsSet, 76, [], rPos)))
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *} {AG}
function FoldString(const Input : String;
                    BreakCharsSet : TSysCharSet;
                    MaxCol      : Integer): String;
var
    rPos : Integer;
begin
    rPos := 1;
    if rPos <= Length(Input) then
        Result := Trim(IcsWrapTextEx(Input, #13#10#09,
                       BreakCharsSet, MaxCol, [], rPos));
    while rPos <= Length(Input) do
        Result := Result + #13#10#09 + Trim(IcsWrapTextEx(Input, #13#10#09,
                                                          BreakCharsSet,
                                                          MaxCol,
                                                          [], rPos))
end;


{* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *}
function IsCharInSysCharSet(Ch : TSetType; const MySet : TSysCharSet) : Boolean;
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
