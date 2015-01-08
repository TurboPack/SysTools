TurboPower SysTools


Table of contents

1.  Introduction
2.  Package names
3.  Installation
4.  Version history
4.1   Release 4.03

==============================================


1. Introduction


SysTools is a library of utility routines & classes for Borland
Delphi, C++Builder, & environments that support COM. It includes 1-D &
2-D bar codes, sorting, money routines, logging, high-precision math,
a run-time math expression analyzer, & much more.

This is a source-only release of TurboPower SysTools. It includes
designtime and runtime packages for Delphi 3 through 7 and C++Builder
3 through 6.

==============================================

2. Package names


TurboPower SysTools package names have the following form:

  SNNN_KVV.*
   |   ||
   |   |+------ VV  VCL version (30=Delphi 3, 40=Delphi 4, 70=Delphi 7)
   |   +------- K   Kind of package (R=runtime, D=designtime)
   |
   +----------- NNN Product version number (e.g., 403=version 4.03)


For example, the SysTools designtime package files for Delphi 7 have
the filename S403_D70.*.

==============================================

3. Installation


To install TurboPower SysTools into your IDE, take the following
steps:

  1. Unzip the release files into a directory (e.g., d:\systools).

  2. Start Delphi or C++Builder.

  3. Add the source subdirectory (e.g., d:\systools\source) to the
     IDE's library path.

  4. Open & install the designtime package specific to the IDE being
     used. The IDE should notify you the components have been
     installed.

  5. Make sure the PATH environmental variable contains the directory
     in which the compiled packages (i.e., BPL or DPL files) were
     placed.

==============================================

4. Version history


4.1 Release 4.03

    Bug fixes
    -------------------------------------------------------------
    - Range error in TimeToTimeStringPrim Routine
    - Added empty string case handling to TrimLeadS
