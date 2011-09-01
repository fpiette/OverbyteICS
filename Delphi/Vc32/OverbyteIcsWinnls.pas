unit OverbyteIcsWinnls;

// From Winnls.h
{ Normalization requires Vista+ or XP SP2 with additional install of:
  Microsoft Internationalized Domain Names (IDN) Mitigation APIs 1.1 }

{$ALIGN ON}
{$MINENUMSIZE 4}
{$WEAKPACKAGEUNIT}

{.$DEFINE CPP_WINHEADER_VISTA}

interface

uses
  Windows;

type
//#if (WINVER >= 0x0600)
  {$IFDEF CPP_WINHEADER_VISTA} {$EXTERNALSYM _NORM_FORM} {$ENDIF}
  _NORM_FORM = (
    NormalizationOther,
    NormalizationC,
    NormalizationD,
    NormalizationKC = 5,
    NormalizationKD);
  {$IFDEF CPP_WINHEADER_VISTA} {$EXTERNALSYM NORM_FORM} {$ENDIF}
  NORM_FORM = _NORM_FORM;
  TNormForm = NORM_FORM;

{$IFDEF CPP_WINHEADER_VISTA} {$EXTERNALSYM IsNormalizedString} {$ENDIF}
function IsNormalizedString(NormForm: TNormForm; lpString: LPCWSTR;
  cwLength: Integer): BOOL; stdcall;

{$IFDEF CPP_WINHEADER_VISTA} {$EXTERNALSYM NormalizeString} {$ENDIF}
function NormalizeString(NormForm: TNormForm; lpSrcString: LPCWSTR;
  cwSrcLength: Integer; lpDstString: LPWSTR; cwDstLength: Integer): Integer; stdcall;
//#endif (WINVER >= 0x0600)

function LoadNormalizeLib: Boolean;

implementation

const
  LibNormaliz = 'Normaliz.dll';
  _IsNormalizedString : function (NormForm: TNormForm; lpString: LPCWSTR;
    cwLength: Integer): BOOL; stdcall = nil;
  _NormalizeString : function(NormForm: TNormForm; lpSrcString: LPCWSTR;
    cwSrcLength: Integer; lpDstString: LPWSTR; cwDstLength: Integer): Integer; stdcall = nil;
var
  hNormaliz: THandle = 0;

function LoadNormalizeLib: Boolean;
begin
  if (@_IsNormalizedString = nil) or (@_NormalizeString = nil) then
  begin
    if hNormaliz = 0 then
      hNormaliz := LoadLibrary(LibNormaliz);
    if hNormaliz <> 0 then
    begin
      @_IsNormalizedString := GetProcAddress(hNormaliz, 'IsNormalizedString');
      if @_IsNormalizedString <> nil then
        @_NormalizeString := GetProcAddress(hNormaliz, 'NormalizeString');
    end;
  end;
  Result := (@_IsNormalizedString <> nil) and (@_NormalizeString <> nil);
end;

function IsNormalizedString(NormForm: TNormForm; lpString: LPCWSTR;
  cwLength: Integer): BOOL;
begin
  if (@_IsNormalizedString = nil) then
    LoadNormalizeLib;
  Result := _IsNormalizedString(NormForm, lpString, cwLength);
end;

function NormalizeString(NormForm: TNormForm; lpSrcString: LPCWSTR;
  cwSrcLength: Integer; lpDstString: LPWSTR;
  cwDstLength: Integer): Integer;
begin
  if (@_NormalizeString = nil) then
    LoadNormalizeLib;
  Result := _NormalizeString(NormForm, lpSrcString, cwSrcLength, lpDstString,
                             cwDstLength);
end;


end.
