<p align="center">
<img src="Assets\image.png"/>
</p>

# PsyloDbg

PsyloDbg is a very simple **Windows Debugger** that currently only monitor for debug events:

* Exception
* Create Thread
* Create Process
* Exit Thread
* Exit Process
* Load DLL
* Unload DLL
* Debug String
* RIP

It supports x86-32 and x86-64 application debugging.

You can create a new process (and optionnaly monitor all child process) or attach to a compatible and existing process.

I started recently this project to learn a bit more about how Debuggers works on Windows. This project is expected to grow in functionalities throughout my learning experience.

This application uses Windows API:

* [WaitForDebugEvent](https://learn.microsoft.com/en-us/windows/win32/api/debugapi/nf-debugapi-waitfordebugevent)
* [WaitForDebugEventEx](https://learn.microsoft.com/en-us/windows/win32/api/debugapi/nf-debugapi-waitfordebugeventex) (Unicode)
* [DebugActiveProcess](https://learn.microsoft.com/en-us/windows/win32/api/debugapi/nf-debugapi-debugactiveprocess)
* [DebugActiveProcessStop](https://learn.microsoft.com/en-us/windows/win32/api/debugapi/nf-debugapi-debugactiveprocessstop)
* [ContinueDebugEvent](https://learn.microsoft.com/en-us/windows/win32/api/debugapi/nf-debugapi-continuedebugevent)

To receive debug events from debugged process (including child process if possible).

## Changelog

- (2022/10/13) : Initial Release

## TODO List

* Show Loaded Modules
* Memory String / File String
* Windows API Call Tracing
* Dump Mounted Module Image
* Show Memory Map


