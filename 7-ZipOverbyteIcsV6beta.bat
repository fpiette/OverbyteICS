@echo off
mode con lines=50
if not exist OverbyteIcsV6beta.zip goto ZipIt
if exist OverbyteIcsV6beta_21.zip del OverbyteIcsV6beta_21.zip
if exist OverbyteIcsV6beta_20.zip ren OverbyteIcsV6beta_20.zip OverbyteIcsV6beta_21.zip
if exist OverbyteIcsV6beta_19.zip ren OverbyteIcsV6beta_19.zip OverbyteIcsV6beta_20.zip
if exist OverbyteIcsV6beta_18.zip ren OverbyteIcsV6beta_18.zip OverbyteIcsV6beta_19.zip
if exist OverbyteIcsV6beta_17.zip ren OverbyteIcsV6beta_17.zip OverbyteIcsV6beta_18.zip
if exist OverbyteIcsV6beta_16.zip ren OverbyteIcsV6beta_16.zip OverbyteIcsV6beta_17.zip
if exist OverbyteIcsV6beta_15.zip ren OverbyteIcsV6beta_15.zip OverbyteIcsV6beta_16.zip
if exist OverbyteIcsV6beta_14.zip ren OverbyteIcsV6beta_14.zip OverbyteIcsV6beta_15.zip
if exist OverbyteIcsV6beta_13.zip ren OverbyteIcsV6beta_13.zip OverbyteIcsV6beta_14.zip
if exist OverbyteIcsV6beta_12.zip ren OverbyteIcsV6beta_12.zip OverbyteIcsV6beta_13.zip
if exist OverbyteIcsV6beta_11.zip ren OverbyteIcsV6beta_11.zip OverbyteIcsV6beta_12.zip
if exist OverbyteIcsV6beta_10.zip ren OverbyteIcsV6beta_10.zip OverbyteIcsV6beta_11.zip
if exist OverbyteIcsV6beta_09.zip ren OverbyteIcsV6beta_09.zip OverbyteIcsV6beta_10.zip
if exist OverbyteIcsV6beta_08.zip ren OverbyteIcsV6beta_08.zip OverbyteIcsV6beta_09.zip
if exist OverbyteIcsV6beta_07.zip ren OverbyteIcsV6beta_07.zip OverbyteIcsV6beta_08.zip
if exist OverbyteIcsV6beta_06.zip ren OverbyteIcsV6beta_06.zip OverbyteIcsV6beta_07.zip
if exist OverbyteIcsV6beta_05.zip ren OverbyteIcsV6beta_05.zip OverbyteIcsV6beta_06.zip
if exist OverbyteIcsV6beta_04.zip ren OverbyteIcsV6beta_04.zip OverbyteIcsV6beta_05.zip
if exist OverbyteIcsV6beta_03.zip ren OverbyteIcsV6beta_03.zip OverbyteIcsV6beta_04.zip
if exist OverbyteIcsV6beta_02.zip ren OverbyteIcsV6beta_02.zip OverbyteIcsV6beta_03.zip
if exist OverbyteIcsV6beta_01.zip ren OverbyteIcsV6beta_01.zip OverbyteIcsV6beta_02.zip
if exist OverbyteIcsV6beta.zip   ren OverbyteIcsV6beta.zip OverbyteIcsV6beta_01.zip
:ZipIt
7z a -tzip OverbyteIcsV6beta.zip @ZipOverbyteIcsV6beta.lst
REM 7z a -t7z OverbyteIcsV6beta.7z @ZipOverbyteIcsV6beta.lst
ren OverbyteIcsV6beta.zip OverbyteIcsV6beta.zip
pause
