@echo off
rem echo "1:" %1
rem echo "2:" %2
rem echo "3:" %3
if exist "c:/Program Files/Git/mingw64/bin/curl.exe" (
"c:/Program Files/Git/mingw64/bin/curl.exe" -s "%1" --create-dirs -o %2
) else (
curl -s "%1" --create-dirs -o %2
)

if %~z2 EQU 0 (
 echo zero file %2
del "%2"
) else (
echo done %1
)

