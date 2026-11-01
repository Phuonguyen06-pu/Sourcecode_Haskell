@echo off
rem 
stack build
if errorlevel 1 exit /b 1
for /f "delims=" %%i in ('stack path --local-install-root') do set "BIN=%%i\bin"
if not exist "%BIN%\freeglut.dll" copy /Y "lib\freeglut.dll" "%BIN%\" >nul
stack exec tank-battle-exe -- %*