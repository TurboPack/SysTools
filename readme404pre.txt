TurboPower SysTools -- Version 4.04 Pre-Release


Table of contents

1.  Introduction
2.  Package names
3.  Installation


==============================================


1. Introduction

This is a "pre-release" of SysTools version 4.04. It contains only the 
Delphi packages for Delphi 2005 and Delphi 2006. It does not contain any
other enhancements or bug fixes that may have been made since the release of
version 4.03. To compile the Delphi 2005 and Delphi 2006 packages, you will
still need all of the source files from version 4.03.

==============================================

2. Package names


The Delphi 2005 runtime packages are S404_R90.dpk and S404BR90.dpk. The 
designtime packages are S404_D90.dpk and S404BD90.dpk.

The Delphi 2006 runtime packages are S404_R100.dpk and S404BR100.dpk. The 
designtime packages are S404_D100.dpk and S404BD100.dpk.

==============================================


3. Installation


To install TurboPower SysTools into your Delphi 2005 or 2006 IDE, take the following steps:

  1. Unzip the release files into the packages directory of your SysTools 4.03 installation
     (e.g., c:\turbopower\systools\packages).

  2. Start Delphi 2005 or 2006.

  3. Add the source subdirectory (e.g., c:\turbopower\systools\source) to the IDE's
     library path.

  4. Open & compile the runtime packages specific to the IDE being used. 
     (S404_R90.bdsproj and S404BR90.bdsproj for Delphi 2005, and S404_R100.bdsproj
     and S404BR100.bdsproj for Delphi 2006.)

  5. Open & compile the designtime packages specific to the IDE being used. 
     (S404_D90.bdsproj and S404BD90.bdsproj for Delphi 2005, and S404_D100.bdsproj
     and S404BD100.bdsproj for Delphi 2006.)

  6. Install the designtime packages into the IDE by using the Component/Install
     Packages menu option. From there, click the "Add" button and then navigate
     to the location of your newly-compiled designtime packages. (S404_D90.bpl and
     S404BD90.bpl for Delphi 2005, and S404_D100.bpl and S404BD100.bpl for Delphi 
     2006.) By default, the Delphi compiler places the compiled packages in the 
     C:\Documents and Settings\<Your User Name>\My Documents\Borland Studio Projects\Bpl
     directory.

