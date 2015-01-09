(* ***** BEGIN LICENSE BLOCK *****
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 * http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License
 * for the specific language governing rights and limitations under the
 * License.
 *
 * The Original Code is TurboPower SysTools
 *
 * The Initial Developer of the Original Code is
 * TurboPower Software
 *
 * Portions created by the Initial Developer are Copyright (C) 1996-2002
 * the Initial Developer. All Rights Reserved.
 *
 * Contributor(s):
 *
 * ***** END LICENSE BLOCK ***** *)

{*********************************************************}
{* SysTools: StDefine.inc 4.03                           *}
{*********************************************************}
{* SysTools: Compiler options/directives include file    *}
{*********************************************************}

{ Because StDefine.inc is included in all SysTools units, you can specify   }
{ global compiler options here. STDEFINE is included *before* each units'   }
{ required compiler options, so options specified here might be overridden  }
{ by hardcoded options in the source file.                                  }

{$IFNDEF LINUX}
{---Global compiler defines for 32-bit OS's---}
{$A+} {Word Align Data}
{$H+} {Huge string support}
{$Q+} {Overflow check}
{$R+} {Range check}
{$S-} {Stack check}
{$T-} {Typed @ check}
{$V-} {Var strings}
{$B-} {Incomplete boolean evaluation}
{$J+} {Writeable Typed Constants}
{$ENDIF LINUX}

{$DEFINE ThreadSafe}
{ This define determines whether SysTools operations are safe in a 32-bit   }
{ multi-threaded application. Leaving it undefined will improved performance}
{ slightly in a single-threaded 32-bit application.                         }

{$DEFINE Median}
{.$DEFINE MidPoint}
{.$DEFINE Random}
{ The three partitioning methods used by the merge sort algorithm in STSORT.}
{ Choose one, and one only. Median is on average fastest, and quite safe.   }
{ Midpoint is the simplest, but may degrade on some data sets. Random is    }
{ the safest, but on average the slowest. Safety in this context means the  }
{ prevention of the underlying quicksort degenerating into a sort of O(N^2) }
{ The default is Median.                                                    }

{$IFDEF Median}
 {$IFDEF MidPoint}
   !! ERROR: you must define only one of Median, MidPoint, Random
 {$ENDIF}
 {$IFDEF Random}
   !! ERROR: you must define only one of Median, MidPoint, Random
 {$ENDIF}
{$ENDIF}
{$IFDEF MidPoint}
 {$IFDEF Random}
   !! ERROR: you must define only one of Median, MidPoint, Random
 {$ENDIF}
{$ENDIF}
{$IFNDEF Median}
 {$IFNDEF MidPoint}
   {$IFNDEF Random}
     !! ERROR: you must define at least one of Median, MidPoint, Random
   {$ENDIF}
 {$ENDIF}
{$ENDIF}

