@echo off
setlocal enabledelayedexpansion

mkdir %~dp5 > NUL 2>&1

if exist "c:/Program Files/Git/mingw64/bin/curl.exe" (
"c:/Program Files/Git/mingw64/bin/curl.exe" -s --create-dirs --dump-header "%5".header "%1?url=%2&w=%3&h=%4" --create-dirs -o "%5".jpg
) else (
curl.exe -s --create-dirs --dump-header "%5".header "%1?url=%2&w=%3&h=%4" --create-dirs -o "%5".jpg
)

if %~z5.jpg EQU 0 (
   set delfile=%5.jpg
   echo zero file del !delfile!
   set escaped_delfile=!delfile:/=\\!
   del !escaped_delfile!
) else (
  echo done %2
)
