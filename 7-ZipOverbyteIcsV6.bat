@echo off
mode con lines=50
if not exist OverbyteIcsV6.zip goto ZipIt
if exist OverbyteIcsV6_21.zip del OverbyteIcsV6_21.zip
if exist OverbyteIcsV6_20.zip ren OverbyteIcsV6_20.zip OverbyteIcsV6_21.zip
if exist OverbyteIcsV6_19.zip ren OverbyteIcsV6_19.zip OverbyteIcsV6_20.zip
if exist OverbyteIcsV6_18.zip ren OverbyteIcsV6_18.zip OverbyteIcsV6_19.zip
if exist OverbyteIcsV6_17.zip ren OverbyteIcsV6_17.zip OverbyteIcsV6_18.zip
if exist OverbyteIcsV6_16.zip ren OverbyteIcsV6_16.zip OverbyteIcsV6_17.zip
if exist OverbyteIcsV6_15.zip ren OverbyteIcsV6_15.zip OverbyteIcsV6_16.zip
if exist OverbyteIcsV6_14.zip ren OverbyteIcsV6_14.zip OverbyteIcsV6_15.zip
if exist OverbyteIcsV6_13.zip ren OverbyteIcsV6_13.zip OverbyteIcsV6_14.zip
if exist OverbyteIcsV6_12.zip ren OverbyteIcsV6_12.zip OverbyteIcsV6_13.zip
if exist OverbyteIcsV6_11.zip ren OverbyteIcsV6_11.zip OverbyteIcsV6_12.zip
if exist OverbyteIcsV6_10.zip ren OverbyteIcsV6_10.zip OverbyteIcsV6_11.zip
if exist OverbyteIcsV6_09.zip ren OverbyteIcsV6_09.zip OverbyteIcsV6_10.zip
if exist OverbyteIcsV6_08.zip ren OverbyteIcsV6_08.zip OverbyteIcsV6_09.zip
if exist OverbyteIcsV6_07.zip ren OverbyteIcsV6_07.zip OverbyteIcsV6_08.zip
if exist OverbyteIcsV6_06.zip ren OverbyteIcsV6_06.zip OverbyteIcsV6_07.zip
if exist OverbyteIcsV6_05.zip ren OverbyteIcsV6_05.zip OverbyteIcsV6_06.zip
if exist OverbyteIcsV6_04.zip ren OverbyteIcsV6_04.zip OverbyteIcsV6_05.zip
if exist OverbyteIcsV6_03.zip ren OverbyteIcsV6_03.zip OverbyteIcsV6_04.zip
if exist OverbyteIcsV6_02.zip ren OverbyteIcsV6_02.zip OverbyteIcsV6_03.zip
if exist OverbyteIcsV6_01.zip ren OverbyteIcsV6_01.zip OverbyteIcsV6_02.zip
if exist OverbyteIcsV6.zip   ren OverbyteIcsV6.zip OverbyteIcsV6_01.zip
:ZipIt
7z a -tzip OverbyteIcsV6.zip @ZipOverbyteIcsV6.lst
REM 7z a -t7z OverbyteIcsV6.7z @ZipOverbyteIcsV6.lst
ren OverbyteIcsV6.zip OverbyteIcsV6.zip
pause
