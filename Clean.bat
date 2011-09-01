@echo off
if exist delphi\vc32\OverbyteIcsWSocket.pas goto clean
echo.
echo.
echo Not an ICS repository
goto end
:clean
echo Hit return to clean temporary and all D2006+ project files, of CTRL+C to abort...
pause
if exist *.bak del *.bak

if exist Install\*.local del Install\*.local
if exist Install\*.~* del Install\*.~*

if exist delphi\vc32\Debug_Build\*.* del delphi\vc32\Debug_Build\*.*
if exist delphi\vc32\OverbyteIcs*.hpp del delphi\vc32\OverbyteIcs*.hpp
if exist delphi\vc32\OverbyteIcs*.dcu del delphi\vc32\OverbyteIcs*.dcu
if exist delphi\vc32\OverbyteIcs*.~*  del delphi\vc32\OverbyteIcs*.~*
if exist delphi\vc32\OverbyteIcs*.bak del delphi\vc32\OverbyteIcs*.bak
if exist delphi\vc32\OverbyteIcs*.obj del delphi\vc32\OverbyteIcs*.obj
if exist delphi\vc32\OverbyteIcs*.dproj.local del delphi\vc32\OverbyteIcs*.dproj.local
if exist delphi\vc32\OverbyteIcs*.cbproj.local del delphi\vc32\OverbyteIcs*.cbproj.local
if exist delphi\vc32\OverbyteIcs*.bdsproj.local del delphi\vc32\OverbyteIcs*.bdsproj.local
if exist delphi\vc32\OverbyteIcs*.identcache del delphi\vc32\OverbyteIcs*.identcache

if exist delphi\internet\OverbyteIcs*.rsm del delphi\internet\OverbyteIcs*.rsm
if exist delphi\internet\OverbyteIcs*_Icon.ico del delphi\internet\OverbyteIcs*_Icon.ico
if exist delphi\internet\OverbyteIcs*.exe del delphi\internet\OverbyteIcs*.exe
if exist delphi\internet\OverbyteIcs*.dll del delphi\internet\OverbyteIcs*.dll
if exist delphi\internet\OverbyteIcs*.ddp del delphi\internet\OverbyteIcs*.ddp
if exist delphi\internet\Dcu\*.dcu del delphi\internet\Dcu\*.dcu
if exist delphi\internet\OverbyteIcs*.dproj del delphi\internet\OverbyteIcs*.dproj
if exist delphi\internet\OverbyteIcs*.dproj.2007 del delphi\internet\OverbyteIcs*.dproj.2007
if exist delphi\internet\OverbyteIcs*.bdsproj del delphi\internet\OverbyteIcs*.bdsproj
if exist delphi\internet\OverbyteIcs*.bdsgroup del delphi\internet\OverbyteIcs*.bdsgroup
if exist delphi\internet\OverbyteIcs*.groupproj del delphi\internet\OverbyteIcs*.groupproj
if exist delphi\internet\dcu\OverbyteIcs*.dcu del delphi\internet\dcu\OverbyteIcs*.dcu
if exist delphi\internet\OverbyteIcs*.cfg del delphi\internet\OverbyteIcs*.cfg
if exist delphi\internet\OverbyteIcs*.dcu del delphi\internet\OverbyteIcs*.dcu
if exist delphi\internet\OverbyteIcs*.~*  del delphi\internet\OverbyteIcs*.~*
if exist delphi\internet\OverbyteIcs*.bak del delphi\internet\OverbyteIcs*.bak
if exist delphi\internet\OverbyteIcs*.obj del delphi\internet\OverbyteIcs*.obj
if exist delphi\internet\OverbyteIcs*.dproj.local del delphi\internet\OverbyteIcs*.dproj.local
if exist delphi\internet\OverbyteIcs*.groupproj.local del delphi\internet\OverbyteIcs*.groupproj.local
if exist delphi\internet\OverbyteIcs*.bdsproj.local del delphi\internet\OverbyteIcs*.bdsproj.local
if exist delphi\internet\OverbyteIcs*.bdsgroup.local del delphi\internet\OverbyteIcs*.bdsgroup.local
if exist delphi\internet\OverbyteIcs*.identcache del delphi\internet\OverbyteIcs*.identcache

if exist delphi\sslinternet\OverbyteIcs*.rsm del delphi\sslinternet\OverbyteIcs*.rsm
if exist delphi\sslinternet\OverbyteIcs*_Icon.ico del delphi\sslinternet\OverbyteIcs*_Icon.ico
if exist delphi\sslinternet\OverbyteIcs*.exe del delphi\sslinternet\OverbyteIcs*.exe
if exist delphi\sslinternet\OverbyteIcs*.ddp del delphi\sslinternet\OverbyteIcs*.ddp
if exist delphi\sslinternet\Dcu\*.dcu del delphi\sslinternet\Dcu\*.dcu
if exist delphi\sslinternet\OverbyteIcs*.dproj del delphi\sslinternet\OverbyteIcs*.dproj
if exist delphi\sslinternet\OverbyteIcs*.dproj.2007 del delphi\sslinternet\OverbyteIcs*.dproj.2007
if exist delphi\sslinternet\OverbyteIcs*.bdsproj del delphi\sslinternet\OverbyteIcs*.bdsproj
if exist delphi\sslinternet\OverbyteIcs*.bdsgroup del delphi\sslinternet\OverbyteIcs*.bdsgroup
if exist delphi\sslinternet\OverbyteIcs*.groupproj del delphi\sslinternet\OverbyteIcs*.groupproj
if exist delphi\sslinternet\dcu\OverbyteIcs*.dcu del delphi\sslinternet\dcu\OverbyteIcs*.dcu
if exist delphi\sslinternet\OverbyteIcs*.cfg del delphi\sslinternet\OverbyteIcs*.cfg
if exist delphi\sslinternet\OverbyteIcs*.dcu del delphi\sslinternet\OverbyteIcs*.dcu
if exist delphi\sslinternet\OverbyteIcs*.~*  del delphi\sslinternet\OverbyteIcs*.~*
if exist delphi\sslinternet\OverbyteIcs*.bak del delphi\sslinternet\OverbyteIcs*.bak
if exist delphi\sslinternet\OverbyteIcs*.obj del delphi\sslinternet\OverbyteIcs*.obj
if exist delphi\sslinternet\OverbyteIcs*.dproj.local del delphi\sslinternet\OverbyteIcs*.dproj.local
if exist delphi\sslinternet\OverbyteIcs*.groupproj.local del delphi\sslinternet\OverbyteIcs*.groupproj.local
if exist delphi\sslinternet\OverbyteIcs*.bdsproj.local del delphi\sslinternet\OverbyteIcs*.bdsproj.local
if exist delphi\sslinternet\OverbyteIcs*.bdsgroup.local del delphi\sslinternet\OverbyteIcs*.bdsgroup.local
if exist delphi\sslinternet\OverbyteIcs*.identcache del delphi\sslinternet\OverbyteIcs*.identcache

if exist delphi\miscdemos\OverbyteIcs*.rsm del delphi\miscdemos\OverbyteIcs*.rsm
if exist delphi\miscdemos\OverbyteIcs*_Icon.icon del delphi\miscdemos\OverbyteIcs*_Icon.icon
if exist delphi\miscdemos\OverbyteIcs*.exe del delphi\miscdemos\OverbyteIcs*.exe
if exist delphi\miscdemos\OverbyteIcs*.ddp del delphi\miscdemos\OverbyteIcs*.ddp
if exist delphi\miscdemos\Dcu\*.dcu del delphi\miscdemos\Dcu\*.dcu
if exist delphi\miscdemos\OverbyteIcs*.dproj del delphi\miscdemos\OverbyteIcs*.dproj
if exist delphi\miscdemos\OverbyteIcs*.dproj.2007 del delphi\miscdemos\OverbyteIcs*.dproj.2007
if exist delphi\miscdemos\OverbyteIcs*.bdsproj del delphi\miscdemos\OverbyteIcs*.bdsproj
if exist delphi\miscdemos\OverbyteIcs*.bdsgroup del delphi\miscdemos\OverbyteIcs*.bdsgroup
if exist delphi\miscdemos\OverbyteIcs*.groupproj del delphi\miscdemos\OverbyteIcs*.groupproj
if exist delphi\miscdemos\dcu\OverbyteIcs*.dcu del delphi\miscdemos\dcu\OverbyteIcs*.dcu
if exist delphi\miscdemos\OverbyteIcs*.cfg del delphi\miscdemos\OverbyteIcs*.cfg
if exist delphi\miscdemos\OverbyteIcs*.dcu del delphi\miscdemos\OverbyteIcs*.dcu
if exist delphi\miscdemos\OverbyteIcs*.~*  del delphi\miscdemos\OverbyteIcs*.~*
if exist delphi\miscdemos\OverbyteIcs*.bak del delphi\miscdemos\OverbyteIcs*.bak
if exist delphi\miscdemos\OverbyteIcs*.obj del delphi\miscdemos\OverbyteIcs*.obj
if exist delphi\miscdemos\OverbyteIcs*.dproj.local del delphi\miscdemos\OverbyteIcs*.dproj.local
if exist delphi\miscdemos\OverbyteIcs*.groupproj.local del delphi\miscdemos\OverbyteIcs*.groupproj.local
if exist delphi\miscdemos\OverbyteIcs*.bdsproj.local del delphi\miscdemos\OverbyteIcs*.bdsproj.local
if exist delphi\miscdemos\OverbyteIcs*.bdsgroup.local del delphi\miscdemos\OverbyteIcs*.bdsgroup.local
if exist delphi\miscdemos\OverbyteIcs*.identcache del delphi\miscdemos\OverbyteIcs*.identcache

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
