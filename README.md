<p align="center">
<img src="Assets\header.png"/>
</p>

# PsyloDbg

PsyloDbg is a fast-growing and user-friendly open-source Windows Debugger entirely coded in Delphi.

The main goal of this project is to offer to Malware analysts another tool to short their response time during their analysis process.

The project is still in very early stage, it is expected to grow in feature progressively.

# Images

## Debug Events

<p align="center">
<img src="Assets\events.png"/>
</p>

## Memory Map

<p align="center">
<img src="Assets\memmap.png"/>
</p>

## Loaded Modules

<p align="center">
<img src="Assets\modules.png"/>
</p>

## Debugged Process Tree

<p align="center">
<img src="Assets\proctree.png"/>
</p>

## Dump and Reconstruct PE Image (Beta)

<p align="center">
<img src="Assets\imagedump.png"/>
</p>

## Debugger Features

- [x] = Implemented
- [ ] = Not yet implemented

---

### Loaded Modules

- [ ] List.
- [x] Dump Reconstructed Module Image.
- [ ] List Exported Functions.


### ANSI / WIDE String

- [ ] List file / memory strings.
- [ ] Advanced search features.


### Memory Map

- [x] List.
- [x] Identify PE Images and Sections.
- [ ] Identify Thread Stacks.
- [ ] Identify Heaps.
- [x] Dump Region(s) / Page(s) Memory.

### Extra Tools

- [ ] File Merger.
- [ ] Hash Utils.

### Misc

- [x] Catch Debug Events.
- [x] Attach running process to debugger.
- [ ] Enumerate Threads.
- [x] Dump and reconstruct image(s) from process.
- [ ] List open handles.
- [ ] API Tracing.
- [ ] Import Module and Functions.
- [ ] Internationalization.
- [x] PsyloDbg Logging (Ex: Psylo Exception).


## Changelog

### 2022/10/13 : v0.1 - Initial Release

- First Release, Supporting Debug Events.


### 2022/10/21 : v0.2

- Memory Map List Added.
- Memory Dymp.
- Better Exception Handling.
- Logging.
- Themes Supported.

### 2022/11/15 : v0.3

- Option to keep session information when debug process stop.
- PsyloDbg Window Title is now dynamic.
- Better thread system (with thread manager) - previous feature needs to implement new thread system.
- Enumerate loaded modules.
- Certain features now support child process (Ex: loaded modules, memory map etc..)
- Memory Map now support child process.
- Dump and reconstruct module image (Reconstruct PE - Beta)

