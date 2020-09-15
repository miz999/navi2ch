@echo off
setlocal enabledelayedexpansion

rem chcp 437 > nul

set LOCK_PREFIX=lock
rem set LOCK_FILE=%TEMP%\%LOCK_PREFIX%.curl
rem echo %1 %2 %3 %4

rem :CHECK
rem echo check lockfile %LOCK_FILE% (%~n0)

rem if exist %LOCK_FILE% (
rem     rem echo locking 
rem     rem timeout /t 2 /nobreak 
rem     waitfor dummy /t 1 >nul 2>&1
rem     goto :CHECK
rem ) else (
rem     copy nul > %LOCK_FILE%
rem )

:MAIN
if exist "c:/Program Files/Git/mingw64/bin/curl.exe" (
    "c:/Program Files/Git/mingw64/bin/curl.exe" -s %1 --create-dirs -o %2
) else (
    curl -s %1 --create-dirs -o %2
)

rem if not exist %2 (
rem     rem echo "file exist"
rem 	rem
rem ) else (
rem     if exist %LOCK_FILE% del %LOCK_FILE%
rem 	echo "file not exist"
rem 	goto :END2
rem )

if %~z2==0 (
    rem if %~z2 EQU 0 (
	set delfile=%2
	set escaped_delfile=!delfile:/=\!
	echo zero file !escaped_delfile!
	del !escaped_delfile!
) else (
    if exist "c:/Program Files/Git/mingw64/bin/curl.exe" (
	   "c:/Program Files/Git/mingw64/bin/curl.exe" -s %3 -o %4  "-H" "Authorization:Client-ID ebe3ee4157ab24a"
	) else (
	  curl.exe -s %3 -o %4  "-H" "Authorization:Client-ID ebe3ee4157ab24a"
	)
	echo done %2
)

rem :END
rem if exist %LOCK_FILE% del %LOCK_FILE%

rem :END2
