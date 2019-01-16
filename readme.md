# TurboPack SysTools

Updated for **10.3 Rio** / VER330 / PKG 260

You can still access [10.2 Tokyo]() and [10.1 Berlin]() versions too.


## Table of contents

1.  Introduction
2.  Package names
3.  Installation

-----

## 1. Introduction


SysTools is a library of utility routines & classes for Embarcadero
Delphi and C++Builder. It includes 1-D & 2-D bar codes, sorting, 
money routines, logging, high-precision math, a run-time 
math expression analyzer, & much more.

This is a source-only release of TurboPack SysTools. It includes
designtime and runtime packages for Delphi and CBuilder and supports Win32 and Win64.

-----

## 2. Package names


TurboPack SysTools package names have the following form:

### Delphi
* SysToolsDR.bpl   (Delphi Runtime)
* SysToolsDD.bpl   (Delphi Designtime)
* SysToolsDBDR.bpl (Delphi data aware Runtime)
* SysToolsDBDD.bpl (Delphi data aware Designtime)

### C++Builder
* SysToolsCR.bpl   (C++Builder Runtime)
* SysToolsCD.bpl   (C++Builder Designtime)
* SysToolsDBCR.bpl (C++Builder data aware Runtime)
* SysToolsDBCD.bpl (C++Builder data aware Designtime)

------

## 3. Installation

SysTools is available via the [GetIt Package Manager](http://docwiki.embarcadero.com/RADStudio/en/Installing_a_Package_Using_GetIt_Package_Manager) where you can quickly and easily install and uninstall it.

To manually install TurboPack SysTools into your IDE, take the following
steps:

  1. Unzip the release files into a directory (e.g., d:\systools).

  2. Start RAD Studio.

  3. Add the source subdirectory (e.g., d:\systools\source) to the
     IDE's library path. For CBuilder, add the hpp subdirectory
     (e.g., d:\systools\source\hpp\Win32\Release) to the IDE's system include path.

  4. Open & install the designtime package specific to the IDE being
     used. The IDE should notify you the components have been
     installed.

     
-----

## 4. Issue with C++Builder under Win64

If you compile with C++Builder under Win64 and receive an error like:

`[ilink64 Error] Error: Unresolved external 'vtable for...`

you should add the following lines to your cpp file:

```cpp
#ifdef _WIN64
#pragma comment(lib, "%packagename%")
#endif
```
