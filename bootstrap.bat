@echo off
setlocal enabledelayedexpansion

set "DL_url=https://raw.githubusercontent.com/matortheeternal/TES5Edit/mator-zedit/"
set "folder=lib\xedit\"
set nocommit=false
::*********************************************************************************
:main
if "x%1"=="x" goto usage
if "x%1"=="xhelp" goto usage
if "x%1"=="xnocommit" (
    set nocommit=true
    shift
)
if "x%1"=="xall" (
    call :all_files
) else (
    call :single_file %1
)
if "%nocommit%"=="false" goto commit
exit /b
::*********************************************************************************
:single_file
set file=%1
call :Download %1
exit /b
::*********************************************************************************
:all_files
for /f "tokens=*" %%G in ('dir /B /s  /a:-d "lib\xedit\*"') do (
    set file_path=%%G
	call :Download !file_path:%CD%\=!
)
exit /b
::*********************************************************************************
:Download <File_with_path>
set "file=%1"
set "file=%file:lib\xedit\=%"
set "URL=%DL_url%!file:\=/!"
echo    Downloading file "%file%" from URL : "%URL%" to : "%1
rem Powershell.exe -command "(New-Object System.Net.WebClient).DownloadFile('%URL%','%1')"
exit /b
::*************************************************************************
:commit
echo.
echo    Auto committing new files
git add lib\xedit\*
git commit -m "Update TES5Edit library"
exit /b
::*************************************************************************
:usage
echo Usage : %0 [nocommit] ^<all^|file^> 
echo nocommit : By default, %0 will auto commit all new added files, this option let the user with uncommitted files
echo all : get all files from lib\xedit
echo file : get one file, give the path from git root i.e. lib\xedit\lz4\lz4.pas or lib\xedit\wbBAS.pas
exit /b