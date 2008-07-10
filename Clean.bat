@echo off
if exist delphi\vc32\OverbyteIcsWSocket.pas goto clean
echo.
echo.
echo Not an ICS repository
goto end
:clean
echo Hit return to clean temporary files, of CTRL+C to abort...
pause
if exist delphi\vc32\OverbyteIcs*.dcu del delphi\vc32\OverbyteIcs*.dcu
if exist delphi\vc32\OverbyteIcs*.~*  del delphi\vc32\OverbyteIcs*.~*
if exist delphi\vc32\OverbyteIcs*.bak del delphi\vc32\OverbyteIcs*.bak
if exist delphi\vc32\OverbyteIcs*.obj del delphi\vc32\OverbyteIcs*.obj
if exist delphi\vc32\OverbyteIcs*.dproj.local del delphi\vc32\OverbyteIcs*.dproj.local

if exist delphi\internet\OverbyteIcs*.dcu del delphi\internet\OverbyteIcs*.dcu
if exist delphi\internet\OverbyteIcs*.~*  del delphi\internet\OverbyteIcs*.~*
if exist delphi\internet\OverbyteIcs*.bak del delphi\internet\OverbyteIcs*.bak
if exist delphi\internet\OverbyteIcs*.obj del delphi\internet\OverbyteIcs*.obj

if exist cpp\internet\OverbyteIcs*.dcu del cpp\internet\OverbyteIcs*.dcu
if exist cpp\internet\OverbyteIcs*.~*  del cpp\internet\OverbyteIcs*.~*
if exist cpp\internet\OverbyteIcs*.ba  del cpp\internet\OverbyteIcs*.bak
if exist cpp\internet\OverbyteIcs*.obj del cpp\internet\OverbyteIcs*.obj

if exist cpp\internet\bcb6\OverbyteIcs*.dcu del cpp\internet\bcb6\OverbyteIcs*.dcu
if exist cpp\internet\bcb6\OverbyteIcs*.~*  del cpp\internet\bcb6\OverbyteIcs*.~*
if exist cpp\internet\bcb6\OverbyteIcs*.bak del cpp\internet\bcb6\OverbyteIcs*.bak
if exist cpp\internet\bcb6\OverbyteIcs*.obj del cpp\internet\bcb6\OverbyteIcs*.obj

echo Done !
:end
pause
