FASMARM_win32\FASMARM.EXE program-5-Arm32.fasm
@if ERRORLEVEL 1 goto :exit

COPY /Y program-5-Arm32.bin ..\CppHosting\RudimentaryHostForARM32

:exit