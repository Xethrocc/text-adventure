@echo off
rem play.bat -- start a compiled adventure.
rem Usage:
rem   play.bat            lists the compiled worlds, asks for one
rem   play.bat mygame     starts worlds\mygame\world.json directly
setlocal enabledelayedexpansion
chcp 65001 >nul 2>&1
cd /d "%~dp0"

if not "%~1"=="" (
    call :play "%~1"
    goto :eof
)

echo Compiled worlds:
set "any=0"
for /d %%D in (worlds\*) do (
    if exist "%%D\world.json" (
        set "any=1"
        echo   %%~nxD
    )
)
if "!any!"=="0" (
    echo No compiled worlds yet. Run check.bat first:
    echo   check.bat          ^(checks and compiles everything in adventures\^)
    goto :eof
)

set "pick="
set /p pick="Type the world name and press Enter: "
if "!pick!"=="" goto :eof
call :play "!pick!"
goto :eof

:play
if not exist "worlds\%~1\world.json" (
    echo No compiled world called "%~1". Run check.bat first:
    echo   check.bat %~1
    goto :eof
)
bin\text-adventure.exe --world "worlds\%~1\world.json" --save "worlds\%~1\save.json"
goto :eof
