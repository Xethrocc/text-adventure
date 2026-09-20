@echo off
rem check.bat -- validate and compile an adventure.
rem Usage:
rem   check.bat            checks every adventure in adventures\ (all of them)
rem   check.bat mygame.yml checks one file
setlocal enabledelayedexpansion
chcp 65001 >nul 2>&1
cd /d "%~dp0"

if "%~1"=="" (
    set "any=0"
    for %%F in (adventures\*.yaml adventures\*.yml adventures\*.json) do (
        set "any=1"
        call :check %%F
    )
    if "!any!"=="0" (
        echo No adventures found in the adventures\ folder.
        echo Put a .yaml file there ^(see examples\ for templates^), then run check.bat again.
    )
    goto :eof
)

call :check "%~1"
goto :eof

:check
echo.
echo === %~nx1 ===
bin\worldbuilder.exe validate "%~1"
if errorlevel 1 (
    echo ^> Fix the errors above, then run check.bat again.
    goto :eof
)
bin\worldbuilder.exe compile "%~1" -o worlds\%~n1
if errorlevel 1 (
    echo ^> The adventure checks out but could not be compiled. Fix the errors above.
    goto :eof
)
echo OK: worlds\%~n1\world.json is ready. Run play.bat %~n1 to start it.
goto :eof
