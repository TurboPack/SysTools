// Upgraded to Delphi 2009: Sebastian Zierer

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
{* SysTools: StBarC.pas 4.04                             *}
{*********************************************************}
{* SysTools: bar code components                         *}
{*********************************************************}

{$I StDefine.inc}

unit StBarC;

interface

uses
  Windows,
  Classes, ClipBrd, Controls, Graphics, Messages, SysUtils,
  StBase, StConst;

const
  {.Z+}
  bcMaxBarCodeLen = 255;
  bcGuardBarAbove = True;
  bcGuardBarBelow = True;
  bcDefNarrowToWideRatio = 2;
  {.Z-}

type
  TStBarKind = (bkSpace, bkBar, bkThreeQuarterBar, bkHalfBar, bkGuard, bkSupplement, bkBlankSpace);
  {.Z+}
  TStBarKindSet = set of TStBarKind;
  TStDigitArray = array[1..bcMaxBarCodeLen] of Byte;
  {.Z-}

  {.Z+}
  TStBarData = class
    FKind    : TStBarKindSet;
    FModules : Integer;
  public
    property Kind : TStBarKindSet
      read FKind
      write FKind;
    property Modules : Integer
      read FModules
      write FModules;
  end;
  {.Z-}

  {.Z+}
  TStBarCodeInfo = class
  private
    FBars       : TList;

    function GetBars(Index : Integer) : TStBarData;
    function GetCount : Integer;

  public
    constructor Create;
      virtual;
    destructor Destroy;
      override;
    procedure Add(ModuleCount : Integer; BarKind : TStBarKindSet);
    procedure Clear;

    property Bars[Index : Integer] : TStBarData
      read GetBars;
      default;

    property Count : Integer
      read GetCount;
  end;
  {.Z-}

  TStBarCodeType = (bcUPC_A, bcUPC_E, bcEAN_8, bcEAN_13,
                    bcInterleaved2of5, bcCodabar, bcCode11,
                    bcCode39, bcCode93, bcCode128);
  TStCode128CodeSubset = (csCodeA, csCodeB, csCodeC);

  TStBarCode = class(TGraphicControl)
  protected {private}
    {property variables}
    {.Z+}
    FAddCheckChar     : Boolean;
    FBarCodeType      : TStBarCodeType;
    FBarColor         : TColor;
    FBarToSpaceRatio  : Double;
    FBarNarrowToWideRatio : Integer;
    FBarWidth         : Double;         {in mils}
    FCode128Subset    : TStCode128CodeSubset;
    FBearerBars       : Boolean;
    FShowCode         : Boolean;
    FShowGuardChars   : Boolean;
    FSupplementalCode : string;
    FTallGuardBars    : Boolean;
    FExtendedSyntax   : Boolean;

    {internal variables}
    bcBarInfo        : TStBarCodeInfo;
    bcBarModWidth    : Integer; {width of single bar}
    bcCheckK         : Integer; {"K" check character for use by Code11}
    bcDigits         : TStDigitArray;
    bcDigitCount     : Integer;
    bcSpaceModWidth  : Integer; {width of empty space between bars}
    bcNormalWidth    : Integer;
    bcSpaceWidth     : Integer;
    bcSupplementWidth: Integer;

    {property methods}
    function GetCode : string;
    function GetVersion : string;
    procedure SetAddCheckChar(Value : Boolean);
    procedure SetBarCodeType(Value : TStBarCodeType);
    procedure SetBarColor(Value : TColor);
    procedure SetBarToSpaceRatio(Value : Double);
    procedure SetBarNarrowToWideRatio(Value: Integer);
    procedure SetBarWidth(Value : Double);
    procedure SetBearerBars(Value : Boolean);
    procedure SetCode(const Value : string);
    procedure SetCode128Subset(Value : TStCode128CodeSubset);
    procedure SetExtendedSyntax (const v : Boolean);
    procedure SetShowCode(Value : Boolean);
    procedure SetShowGuardChars(Value : Boolean);
    procedure SetSupplementalCode(const Value : string);
    procedure SetTallGuardBars(Value : Boolean);
    procedure SetVersion(const Value : string);

    {internal methods}
    procedure CalcBarCode;
    procedure CalcBarCodeWidth;
    function DrawBar(XPos, YPos, AWidth, AHeight : Integer) : Integer;
    procedure DrawBarCode(const R : TRect);
    function GetDigits(Characters : string) : Integer;
    procedure PaintPrim(const R : TRect);
    function SmallestLineWidth(PixelsPerInch : Integer) : Double;

    {VCL message methods}
    procedure CMTextChanged(var Msg : TMessage);
      message CM_TEXTCHANGED;

  protected
   procedure Loaded;
     override;
    procedure Paint;
      override;
  public
    constructor Create(AOwner : TComponent);
      override;
    destructor Destroy;
      override;
    {.Z-}

    procedure CopyToClipboard;
    procedure GetCheckCharacters(const S : string; var C, K : Integer);
    function GetBarCodeWidth(ACanvas : TCanvas) : Double;
    procedure PaintToCanvas(ACanvas : TCanvas; ARect : TRect);
    procedure PaintToCanvasSize(ACanvas : TCanvas; X, Y, H : Double);
    procedure PaintToDC(DC : hDC; ARect : TRect);
    procedure PaintToDCSize(DC : hDC; X, Y, W, H : Double);
    procedure SaveToFile(const FileName : string);
    function Validate(DisplayError : Boolean) : Boolean;

  published
    {properties}
    property Align;
    property Color;
    property Cursor;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property Visible;

    property AddCheckChar : Boolean
      read FAddCheckChar
      write SetAddCheckChar;

    property BarCodeType : TStBarCodeType
      read FBarCodeType
      write SetBarCodeType;

    property BarColor : TColor
      read FBarColor
      write SetBarColor;

    property BarToSpaceRatio : Double
      read FBarToSpaceRatio
      write SetBarToSpaceRatio;

    property BarNarrowToWideRatio : Integer
      read FBarNarrowToWideRatio
      write SetBarNarrowToWideRatio
      default bcDefNarrowToWideRatio;

    property BarWidth : Double
      read FBarWidth
      write SetBarWidth;

    property BearerBars : Boolean
      read FBearerBars
      write SetBearerBars;

    property Code : string
      read GetCode
      write SetCode;

    property Code128Subset : TStCode128CodeSubset
      read FCode128Subset
      write SetCode128Subset;

    property ExtendedSyntax : Boolean
             read FExtendedSyntax write SetExtendedSyntax
             default False;

    property ShowCode : Boolean
      read FShowCode
      write SetShowCode;

    property ShowGuardChars : Boolean
      read FShowGuardChars
      write SetShowGuardChars;

    property SupplementalCode : string
      read FSupplementalCode
      write SetSupplementalCode;

    property TallGuardBars : Boolean
      read FTallGuardBars
      write SetTallGuardBars;

    property Version : string
      read GetVersion
      write SetVersion
      stored False;

    {events}
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
  end;


implementation
{$IFDEF FPC}
  uses mymetafile
  , graphics_delphi
  ;
{$ENDIF}

const
  {left and right codes for UPC_A}
  UPC_A_LeftHand : array[0..9] of string =
    ('0001101', {0}
     '0011001', {1}
     '0010011', {2}
     '0111101', {3}
     '0100011', {4}
     '0110001', {5}
     '0101111', {6}
     '0111011', {7}
     '0110111', {8}
     '0001011'  {9} );

  UPC_A_RightHand : array[0..9] of string =
    ('1110010', {0}
     '1100110', {1}
     '1101100', {2}
     '1000010', {3}
     '1011100', {4}
     '1001110', {5}
     '1010000', {6}
     '1000100', {7}
     '1001000', {8}
     '1110100'  {9} );

const
  UPC_E_OddParity : array[0..9] of string =
    ('0001101', {0}
     '0011001', {1}
     '0010011', {2}
     '0111101', {3}
     '0100011', {4}
     '0110001', {5}
     '0101111', {6}
     '0111011', {7}
     '0110111', {8}
     '0001011'  {9} );

  UPC_E_EvenParity : array[0..9] of string =
    ('0100111', {0}
     '0110011', {1}
     '0011011', {2}
     '0100001', {3}
     '0011101', {4}
     '0111001', {5}
     '0000101', {6}
     '0010001', {7}
     '0001001', {8}
     '0010111'  {9} );

const
  EAN_LeftHandA : array[0..9] of string =
    ('0001101', {0}
     '0011001', {1}
     '0010011', {2}
     '0111101', {3}
     '0100011', {4}
     '0110001', {5}
     '0101111', {6}
     '0111011', {7}
     '0110111', {8}
     '0001011'  {9} );

  EAN_LeftHandB : array[0..9] of string =
    ('0100111', {0}
     '0110011', {1}
     '0011011', {2}
     '0100001', {3}
     '0011101', {4}
     '0111001', {5}
     '0000101', {6}
     '0010001', {7}
     '0001001', {8}
     '0010111'  {9} );

const
  Interleaved_2of5 : array[0..9] of string =
    ('00110', {0}
     '10001', {1}
     '01001', {2}
     '11000', {3}
     '00101', {4}
     '10100', {5}
     '01100', {6}
     '00011', {7}
     '10010', {8}
     '01010'  {9} );

const
  Codabar : array[0..19] of string =
     {BSBSBSB}      {bar-space-bar-space-bar...}
    ('0000011', {0}
     '0000110', {1}
     '0001001', {2}
     '1100000', {3}
     '0010010', {4}
     '1000010', {5}
     '0100001', {6}
     '0100100', {7}
     '0110000', {8}
     '1001000', {9}
     '0001100', {-}
     '0011000', { $}
     '1000101', {:}
     '1010001', {/}
     '1010100', {.}
     '0010101', {+}
     '0011010', {A}
     '0101001', {B}
     '0001011', {C}
     '0001110'  {D});

const
  Code11 :  array[0..11] of string =
     {BSBSB}    {bar-space-bar-space-bar...} {0-narrow, 1-wide}
    ('00001',   {0}
     '10001',   {1}
     '01001',   {2}
     '11000',   {3}
     '00101',   {4}
     '10100',   {5}
     '01100',   {6}
     '00011',   {7}
     '10010',   {8}
     '10000',   {9}
     '00100',   {-}
     '00110');  {stop character}

const
  Code39 : array[0..43] of string =
     {BSBSBSBSB}      {bar-space-bar-space-bar...} {0-narrow, 1-wide}
    ('000110100',   {0}
     '100100001',   {1}
     '001100001',   {2}
     '101100000',   {3}
     '000110001',   {4}
     '100110000',   {5}
     '001110000',   {6}
     '000100101',   {7}
     '100100100',   {8}
     '001100100',   {9}
     '100001001',   {A}
     '001001001',   {B}
     '101001000',   {C}
     '000011001',   {D}
     '100011000',   {E}
     '001011000',   {F}
     '000001101',   {G}
     '100001100',   {H}
     '001001100',   {I}
     '000011100',   {J}
     '100000011',   {K}
     '001000011',   {L}
     '101000010',   {M}
     '000010011',   {N}
     '100010010',   {O}
     '001010010',   {P}
     '000000111',   {Q}
     '100000110',   {R}
     '001000110',   {S}
     '000010110',   {T}
     '110000001',   {U}
     '011000001',   {V}
     '111000000',   {W}
     '010010001',   {X}
     '110010000',   {Y}
     '011010000',   {Z}
     '010000101',   {-}
     '110000100',   {.}
     '011000100',   {SPACE}
     '010101000',   { $}
     '010100010',   {/}
     '010001010',   {+}
     '000101010',   {%}
     '010010100');  {*}

const
  Code93 : array[0..46] of string =
     {BSBSBS}    {bar-space-bar-space-bar...} {0-narrow, 1-wide}
    ('131112',   {0}
     '111213',   {1}
     '111312',   {2}
     '111411',   {3}
     '121113',   {4}
     '121212',   {5}
     '121311',   {6}
     '111114',   {7}
     '131211',   {8}
     '141111',   {9}
     '211113',   {A}
     '211212',   {B}
     '211311',   {C}
     '221112',   {D}
     '221211',   {E}
     '231111',   {F}
     '112113',   {G}
     '112212',   {H}
     '112311',   {I}
     '122112',   {J}
     '132111',   {K}
     '111123',   {L}
     '111222',   {M}
     '111321',   {N}
     '121122',   {O}
     '131121',   {P}
     '212112',   {Q}
     '212211',   {R}
     '211122',   {S}
     '211221',   {T}
     '221121',   {U}
     '222111',   {V}
     '112122',   {W}
     '112221',   {X}
     '122121',   {Y}
     '123111',   {Z}
     '121131',   {-}
     '311112',   {.}
     '311211',   {SPACE}
     '321111',   { $}
     '112131',   {/}
     '113121',   {+}
     '211131',   {%}
     '121221',   {($)}
     '312111',   {(%)}
     '311121',   {(/)}
     '122211');  {(+)}

  Code93Map : array[#0..#127] of string =
  {Circle Code}   {ASCII Code 93 }
    ('%U',        {NL     (%)U   }
     '$A',        {SH     ($)A   }
     '$B',        {SX     ($)B   }
     '$C',        {EX     ($)C   }
     '$D',        {ET     ($)D   }
     '$E',        {EQ     ($)E   }
     '$F',        {AK     ($)F   }
     '$G',        {BL     ($)G   }
     '$H',        {BS     ($)H   }
     '$I',        {HT     ($)I   }
     '$J',        {LF     ($)J   }
     '$K',        {VT     ($)K   }
     '$L',        {FF     ($)L   }
     '$M',        {CR     ($)M   }
     '$N',        {SO     ($)N   }
     '$O',        {SI     ($)O   }
     '$P',        {DL     ($)P   }
     '$Q',        {D1     ($)Q   }
     '$R',        {D2     ($)R   }
     '$S',        {D3     ($)S   }
     '$T',        {D4     ($)T   }
     '$U',        {NK     ($)U   }
     '$V',        {SY     ($)V   }
     '$W',        {EB     ($)W   }
     '$X',        {CN     ($)X   }
     '$Y',        {EM     ($)Y   }
     '$Z',        {SB     ($)Z   }
     '%A',        {EC     (%)A   }
     '%B',        {FS     (%)B   }
     '%C',        {GS     (%)C   }
     '%D',        {RS     (%)D   }
     '%E',        {US     (%)E   }
      ' ',        {Space   Space }
     '/A',        {!      (/)A   }
     '/B',        {"      (/)B   }
     '/C',        {#      (/)C   }
      '$',        { $   (/)D or $}
      '%',        {%    (/)E or %}
     '/F',        {&      (/)F   }
     '/G',        {'      (/)G   }
     '/H',        {(      (/)H   }
     '/I',        {)      (/)I   }
     '/J',        {*      (/)J   }
     ' +',        {+    (/)K or +}
     '/L',        {,      (/)L   }
      '-',        {-    (/)M or -}
      '.',        {.    (/)N or .}
      '/',        {/    (/)O or /}
      '0',        {0    (/)P or 0}
      '1',        {1    (/)Q or 1}
      '2',        {2    (/)R or 2}
      '3',        {3    (/)S or 3}
      '4',        {4    (/)T or 4}
      '5',        {5    (/)U or 5}
      '6',        {6    (/)V or 6}
      '7',        {7    (/)W or 7}
      '8',        {8    (/)X or 8}
      '9',        {9    (/)Y or 9}
     '/Z',        {:      (/)Z   }
     '%F',        {;      (%)F   }
     '%G',        {<      (%)G   }
     '%H',        {=      (%)H   }
     '%I',        {>      (%)I   }
     '%J',        {?      (%)J   }
     '%V',        {       (%)V   }
      'A',        {A   	    A    }
      'B',        {B	    B    }
      'C',        {C	    C    }
      'D',        {D	    D    }
      'E',        {E	    E    }
      'F',        {F	    F    }
      'G',        {G	    G    }
      'H',        {H	    H    }
      'I',        {I	    I    }
      'J',        {J	    J    }
      'K',        {K	    K    }
      'L',        {L	    L    }
      'M',        {M	    M    }
      'N',        {N	    N    }
      'O',        {O	    O    }
      'P',        {P	    P    }
      'Q',        {Q	    Q    }
      'R',        {R	    R    }
      'S',        {S	    S    }
      'T',        {T        T    }
      'U',        {U	    U    }
      'V',        {V	    V    }
      'W',        {W	    W    }
      'X',        {X	    X    }
      'Y',        {Y	    Y    }
      'Z',        {Z	    Z    }
     '%K',        {[  	   (%)K  }
     '%L',        {\  	   (%)L  }
     '%M',        {]  	   (%)M  }
     '%N',        {^  	   (%)N  }
     '%O',        {_  	   (%)O  }
     '%W',        {`       (%)W  }
     '+A',        {a  	   (+)A  }
     '+B',        {b  	   (+)B  }
     '+C',        {c  	   (+)C  }
     '+D',        {d  	   (+)D  }
     '+E',        {e  	   (+)E  }
     '+F',        {f  	   (+)F  }
     '+G',        {g  	   (+)G  }
     '+H',        {h  	   (+)H  }
     '+I',        {i  	   (+)I  }
     '+J',        {j  	   (+)J  }
     '+K',        {k  	   (+)K  }
     '+L',        {l  	   (+)L  }
     '+M',        {m  	   (+)M  }
     '+N',        {n  	   (+)N  }
     '+O',        {o  	   (+)O  }
     '+P',        {p  	   (+)P  }
     '+Q',        {q  	   (+)Q  }
     '+R',        {r  	   (+)R  }
     '+S',        {s  	   (+)S  }
     '+T',        {t  	   (+)T  }
     '+U',        {u  	   (+)U  }
     '+V',        {v  	   (+)V  }
     '+W',        {w  	   (+)W  }
     '+X',        {x  	   (+)X  }
     '+Y',        {y  	   (+)Y  }
     '+Z',        {z  	   (+)Z  }
     '%P',        {{  	   (%)P  }
     '%Q',        {|  	   (%)Q  }
     '%R',        {}{	   (%)R  }
     '%S',        {~  	   (%)S  }
     '%T');       { DEL    (%)T  }

const
  Code128 : array[0..106] of string =
     {BSBSBS}   {Value  CodeA  CodeB   CodeC}
    ('212222',  {0	SPACE	SPACE	00}
     '222122',  {1	!	!	01}
     '222221',  {2	"	"	02}
     '121223',  {3	#	#	03}
     '121322',  {4	$	$	04}
     '131222',  {5	%	%	05}
     '122213',  {6	&	&	06}
     '122312',  {7	'	'	07}
     '132212',  {8	(	(	08}
     '221213',  {9	)	)	09}
     '221312',  {10	* 	*	10}
     '231212',  {11	+	+	11}
     '112232',  {12	,	,	12}
     '122132',  {13	-	-	13}
     '122231',  {14	.	.	14}
     '113222',  {15	/	/	15}
     '123122',  {16	0	0	16}
     '123221',  {17	1	1	17}
     '223211',  {18	2	2	18}
     '221132',  {19	3	3	19}
     '221231',  {20	4	4	20}
     '213212',  {21	5	5	21}
     '223112',  {22	6	6	22}
     '312131',  {23	7	7	23}
     '311222',  {24	8	8	24}
     '321122',  {25	9	9	25}
     '321221',  {26	:	:	26}
     '312212',  {27	;	;	27}
     '322112',  {28	<	<	28}
     '322211',  {29	= 	= 	29}
     '212123',  {30	>	>	30}
     '212321',  {31	?	?	31}
     '232121',  {32	@	@	32}
     '111323',  {33	A	A	33}
     '131123',  {34	B	B	34}
     '131321',  {35	C	C	35}
     '112313',  {36	D	D	36}
     '132113',  {37	E	E	37}
     '132311',  {38	F	F	38}
     '211313',  {39	G	G	39}
     '231113',  {40	H	H	40}
     '231311',  {41	I	I	41}
     '112133',  {42	J	J	42}
     '112331',  {43	K	K	43}
     '132131',  {44	L	L	44}
     '113123',  {45	M	M	45}
     '113321',  {46	N	N	46}
     '133121',  {47	O	O	47}
     '313121',  {48	P	P	48}
     '211331',  {49	Q	Q	49}
     '231131',  {50	R	R	50}
     '213113',  {51	S	S	51}
     '213311',  {52	T	T	52}
     '213131',  {53	U	U	53}
     '311123',  {54	V	V	54}
     '311321',  {55	W	W	55}
     '331121',  {56	X	X	56}
     '312113',  {57	Y	Y	57}
     '312311',  {58	Z	Z	58}
     '332111',  {59	[	[	59}
     '314111',  {60	\	\	60}
     '221411',  {61	]	]	61}
     '431111',  {62	^	^	62}
     '111224',  {63	_ 	_ 	63}
     '111422',  {64	NU	`	64}
     '121124',  {65	SH	a	65}
     '121421',  {66	SX	b	66}
     '141122',  {67	EX	c	67}
     '141221',  {68	ET	d	68}
     '112214',  {69	EQ	e	69}
     '112412',  {70	AK	f	70}
     '122114',  {71	BL	g	71}
     '122411',  {72	BS	h	72}
     '142112',  {73	HT	i	73}
     '142211',  {74	LF	j	74}
     '241211',  {75	VT	k	75}
     '221114',  {76	FF	l	76}
     '413111',  {77	CR	m	77}
     '241112',  {78	SO	n	78}
     '134111',  {79	SI	o	79}
     '111242',  {80	DL	p	80}
     '121142',  {81	D1	q	81}
     '121241',  {82	D2	r	82}
     '114212',  {83	D3	s	83}
     '124112',  {84	D4	t	84}
     '124211',  {85	NK	u	85}
     '411212',  {86	SY	v	86}
     '421112',  {87	EB	w	87}
     '421211',  {88	CN	x	88}
     '212141',  {89	EM	y	89}
     '214121',  {90	SB	z	90}
     '412121',  (*91	EC	{	91*)
     '111143',  {92	FS		92}
     '111341',  (*93	GS	}	93*)
     '131141',  {94	RS	~	94}
     '114113',  {95	US	DEL	95}
     '114311',  {96	FNC 3	FNC 3	96}      {use #132}
     '411113',  {97	FNC 2	FNC 2	97}      {use #131}
     '411311',  {98	SHIFT	SHIFT	98}      {use #130}
     '113141',  {99	CODE C	CODE C	99}      {use #135}
     '114131',  {100	CODE B	FNC 4	CODE B}  {use #134}
     '311141',  {101	FNC 4	CODE A	CODE A}  {use #133}
     '411131',  {102	FNC 1	FNC 1	FNC 1 }  {use #130}
     '211412',  {103	CODE A}                  {use #136}
     '211214',  {104	CODE B}                  {use #137}
     '211232',  {105	CODE C}                  {use #138}
     '2331112');{106    STOP}                    {use #139}


{*** helper routines ***}

function RectWidth(const R : TRect) : Integer;
begin
  Result := R.Right-R.Left;
end;

function RectHeight(const R : TRect) : Integer;
begin
  Result := R.Bottom-R.Top;
end;


{*** TStBarCodeInfo ***}

procedure TStBarCodeInfo.Add(ModuleCount : Integer; BarKind : TStBarKindSet);
var
  Bar : TStBarData;
begin
  Bar := TStBarData.Create;
  Bar.Modules := ModuleCount;
  Bar.Kind := BarKind;
  FBars.Add(Bar);
end;

procedure TStBarCodeInfo.Clear;
var
  I : Integer;
begin
  for I := 0 to FBars.Count-1 do
    TStBarData(FBars[I]).Free;
  FBars.Clear;
end;

constructor TStBarCodeInfo.Create;
begin
  inherited Create;

  FBars := TList.Create;
end;

destructor TStBarCodeInfo.Destroy;
begin
  Clear;
  FBars.Free;
  FBars := nil;

  inherited Destroy;
end;

function TStBarCodeInfo.GetBars(Index : Integer) : TStBarData;
begin
  Result := FBars[Index];
end;

function TStBarCodeInfo.GetCount : Integer;
begin
  Result := FBars.Count;
end;


{*** TStBarCode ***}

procedure TStBarCode.CalcBarCode;
var
  I, J, X : Integer;
  CheckC  : Integer;
  CheckK  : Integer;
  CSP     : string;
  C       : string;
  C1, C2  : string;

  procedure AddCode(const S : string; AKind : TStBarKindSet);
  var
    I : Integer;
  begin
    for I := 1 to Length(S) do
      if S[I] = '0' then
        bcBarInfo.Add(1, AKind - [bkBar, bkThreeQuarterBar, bkHalfBar] + [bkSpace])
      else
        bcBarInfo.Add(StrToInt(S[I]), AKind);
  end;

  procedure AddECode(const Parity : string);
  var
    I : Integer;
  begin
    for I := 1 to Length(Parity) do begin
      if Parity[I] = 'E' then
        AddCode(UPC_E_EvenParity[bcDigits[I]], [bkBar])
      else
        AddCode(UPC_E_OddParity[bcDigits[I]], [bkBar]);
    end;
  end;

  procedure AddSupCode(const Parity : string);
  var
    I : Integer;
  begin
    for I := 1 to Length(Parity) do begin
      if Parity[I] = 'E' then
        AddCode(UPC_E_EvenParity[bcDigits[I]], [bkThreeQuarterBar, bkSupplement])
      else
        AddCode(UPC_E_OddParity[bcDigits[I]], [bkThreeQuarterBar, bkSupplement]);
      if I < Length(Parity) then
        AddCode('01', [bkThreeQuarterBar, bkSupplement]);
    end;
  end;

  procedure AddCodeModules(const S : string);
  var
    K : Integer;
  begin
    for K := 1 to Length(S) do begin
      if Odd(K) then
        bcBarInfo.Add(StrToInt(S[K]), [bkBar])
      else
        bcBarInfo.Add(StrToInt(S[K]), [bkSpace]);
    end;
  end;

  procedure AddCodeWideNarrow(const S : string);
  var
    K : Integer;
  begin
    for K := 1 to Length(S) do begin
      case S[K] of
        '0' : if Odd(K) then
                bcBarInfo.Add(1, [bkBar])
              else
                bcBarInfo.Add(1, [bkSpace]);
        '1' : if Odd(K) then
                bcBarInfo.Add(FBarNarrowToWideRatio, [bkBar])
              else
                bcBarInfo.Add(FBarNarrowToWideRatio, [bkSpace]);
      end;
    end;
  end;

begin
  if csLoading in ComponentState then
    Exit;

  bcBarInfo.Clear;
  if Code = '' then
    Exit;

  {get copy of code}
  C := Code;

  {get digits}
  case FBarCodeType of
    bcUPC_A, bcUPC_E, bcEAN_8, bcEAN_13, bcCodabar, bcCode11, bcCode93 :
      begin
        bcDigitCount := GetDigits(C);
      end;
    bcInterleaved2of5 :
      begin
        {adjust odd length code}
        if FAddCheckChar then begin
          if not Odd(Length(C)) then
            C := '0' + C;
        end else begin
          if Odd(Length(C)) then
          C := '0' + C;
        end;
        bcDigitCount := GetDigits(C);
      end;
    bcCode39 :
      begin
        {add guard characters}
        if C[1] <> '*' then
          C := '*' + C;
        if C[Length(C)] <> '*' then
          C := C + '*';
        bcDigitCount := GetDigits(C);
      end;
    bcCode128 :
      begin
        {add start code}
        if not CharInSet(C[1], [#136, #137, #138]) then
          case FCode128Subset of
            csCodeA : C := #136 + C;
            csCodeB : C := #137 + C;
            csCodeC : C := #138 + C;
          end;
        bcDigitCount := GetDigits(C);
      end;
  end;

  case FBarCodeType of
    bcUPC_A :
      begin
        {get check digit}
        if Length(C) = 11 then
          GetCheckCharacters(C, CheckC, CheckK)
        else
          CheckC := bcDigits[12];

        {encode left hand guard bars}
        AddCode('101', [bkGuard, bkBar]);

        {first six characters as left hand characters}
        for I := 1 to 6 do
          AddCode(UPC_A_LeftHand[bcDigits[I]], [bkBar]);

        {center guard pattern}
        AddCode('01010', [bkGuard, bkBar]);

        {last five data characters as right hand characters}
        for I := 7 to 11 do
          AddCode(UPC_A_RightHand[bcDigits[I]], [bkBar]);

        {check character}
        AddCode(UPC_A_RightHand[CheckC], [bkBar]);

        {encode right hand guard bars}
        AddCode('101', [bkGuard, bkBar]);
      end;
    bcUPC_E :
      begin
        {encode left hand guard bars, 101}
        AddCode('101', [bkGuard, bkBar]);
        GetCheckCharacters(C, CheckC, CheckK);
        case CheckC of
          0 : AddECode('EEEOOO');
          1 : AddECode('EEOEOO');
          2 : AddECode('EEOOEO');
          3 : AddECode('EEOOOE');
          4 : AddECode('EOEEOO');
          5 : AddECode('EOOEEO');
          6 : AddECode('EOOOEE');
          7 : AddECode('EOEOEO');
          8 : AddECode('EOEOOE');
          9 : AddECode('EOOEOE');
        end;
        {encode right hand guard bars}
        AddCode('010101', [bkGuard, bkBar]);
      end;
    bcEAN_8   :
      begin
        {get check digit}
        if Length(C) = 7 then
          GetCheckCharacters(C, CheckC, CheckK)
        else
          CheckC := bcDigits[8];

        {encode left hand guard bars}
        AddCode('101', [bkGuard, bkBar]);
        {two flag two data characters, encoded as left hand A characters}
        for I := 1 to 4 do
          AddCode(EAN_LeftHandA[bcDigits[I]], [bkBar]);
        {encode center guard bars}
        AddCode('01010', [bkGuard, bkBar]);
        {last three data characters, encoded as right hand characters}
        for I := 5 to 7 do
          AddCode(UPC_A_RightHand[bcDigits[I]], [bkBar]);
        {check character}
        AddCode(UPC_A_RightHand[CheckC], [bkBar]);
        {encode right hand guard bars}
        AddCode('101', [bkGuard, bkBar]);
      end;
    bcEAN_13  :
      begin
        {get check digit}
        if Length(C) = 12 then
          GetCheckCharacters(C, CheckC, CheckK)
        else
          CheckC := bcDigits[13];

        {determine which left hand table to use based on first flag character}
        {EAN refers to this as the 13th digit - counting from the right}
        case bcDigits[1] of
                     { 12345}
          0 : CSP := 'AAAAAA';
          1 : CSP := 'AABABB';
          2 : CSP := 'AABBAB';
          3 : CSP := 'AABBBA';
          4 : CSP := 'ABAABB';
          5 : CSP := 'ABBAAB';
          6 : CSP := 'ABBBAA';
          7 : CSP := 'ABABAB';
          8 : CSP := 'ABABBA';
          9 : CSP := 'ABBABA';
        end;
        {encode left hand guard bars}
        AddCode('101', [bkGuard, bkBar]);
        {start with second flag character and next five data characters}
        for I := 2 to 7 do
          if CSP[I-1] = 'A' then
            AddCode(EAN_LeftHandA[bcDigits[I]], [bkBar])
          else
            AddCode(EAN_LeftHandB[bcDigits[I]], [bkBar]);
        {encode center guard bars}
        AddCode('01010', [bkGuard, bkBar]);
        {encode last five data characters}
        for I := 8 to 12 do
          AddCode(UPC_A_RightHand[bcDigits[I]], [bkBar]);
        {check character}
        AddCode(UPC_A_RightHand[CheckC], [bkBar]);
        {encode right hand guard bars}
        AddCode('101', [bkGuard, bkBar]);
      end;
    bcInterleaved2of5 :
      begin
        {add check character}
        if FAddCheckChar then begin
          {get check digit}
          GetCheckCharacters(C, CheckC, CheckK);
          Inc(bcDigitCount);
          bcDigits[bcDigitCount] := CheckC;
        end;

        {encode left guard pattern}
        bcBarInfo.Add(1, [bkGuard, bkBar]);
        bcBarInfo.Add(1, [bkGuard, bkSpace]);
        bcBarInfo.Add(1, [bkGuard, bkBar]);
        bcBarInfo.Add(1, [bkGuard, bkSpace]);

        I := 1;
        while I < bcDigitCount do begin
          {take two characters at a time - odd as bars, even as spaces}
          C1 := Interleaved_2of5[bcDigits[I]];
          C2 := Interleaved_2of5[bcDigits[I+1]];
          {interleave data}
          for J := 1 to 5 do begin
            if C1[J] = '1' then
              bcBarInfo.Add(FBarNarrowToWideRatio, [bkBar]) {wide bar}
            else
              bcBarInfo.Add(1, [bkBar]);   {narrow bar}
            if C2[J] = '1' then
              bcBarInfo.Add(FBarNarrowToWideRatio, [bkSpace]){wide space}
            else
              bcBarInfo.Add(1, [bkSpace]); {narrow space}
          end;
          Inc(I, 2);
        end;

        {encode right guard pattern}
        bcBarInfo.Add(FBarNarrowToWideRatio,
          [bkGuard, bkBar]); {double-width bar}
        bcBarInfo.Add(1, [bkGuard, bkSpace]);
        bcBarInfo.Add(1, [bkGuard, bkBar]);
      end;
    bcCodabar :
      begin
        for I := 1 to bcDigitCount do begin
          AddCodeWideNarrow(Codabar[bcDigits[I]]);
          if I < bcDigitCount then
            bcBarInfo.Add(1, [bkSpace]);
        end;
      end;
    bcCode11 :
      begin
        AddCodeWideNarrow(Code11[11]);  {start}
        bcBarInfo.Add(1, [bkSpace]);
        {add check characters}
        if FAddCheckChar then begin
          {get check digits}
          GetCheckCharacters(C, CheckC, CheckK);
          Inc(bcDigitCount);
          bcDigits[bcDigitCount] := CheckC;
          Inc(bcDigitCount);
          bcDigits[bcDigitCount] := CheckK;
        end;

        for I := 1 to bcDigitCount do begin
          AddCodeWideNarrow(Code11[bcDigits[I]]);
          bcBarInfo.Add(1, [bkSpace]);
        end;
        AddCodeWideNarrow(Code11[11]);  {stop}
      end;
    bcCode39 :
      begin
        for I := 1 to bcDigitCount do begin
          C1 := Code39[bcDigits[I]];
          for J := 1 to Length(C1) do begin
            case C1[J] of
              '0' : if Odd(J) then
                      bcBarInfo.Add(1, [bkBar])
                    else
                      bcBarInfo.Add(1, [bkSpace]);
              '1' : if Odd(J) then
                      bcBarInfo.Add(2, [bkBar])
                    else
                      bcBarInfo.Add(2, [bkSpace]);
            end;
          end;
          bcBarInfo.Add(1, [bkSpace]);
        end;
      end;
    bcCode93 :
      begin;
        {start character}
        AddCodeModules('111141');
        {add check characters}
        if FAddCheckChar then begin
          {get check digits}
          GetCheckCharacters(C, CheckC, CheckK);
          Inc(bcDigitCount);
          bcDigits[bcDigitCount] := CheckC;
          Inc(bcDigitCount);
          bcDigits[bcDigitCount] := CheckK;
        end;
        for I := 1 to bcDigitCount do
          AddCodeModules(Code93[bcDigits[I]]);
        {stop character}
        AddCodeModules('1111411');
      end;
    bcCode128 :
      begin
        {add check character}
        if FAddCheckChar then begin
          GetCheckCharacters(C, CheckC, CheckK);
          Inc(bcDigitCount);
          bcDigits[bcDigitCount] := CheckC;
        end;
        {add stop code}
        Inc(bcDigitCount);
        bcDigits[bcDigitCount] := 106;
        for I  := 1 to bcDigitCount do
          AddCodeModules(Code128[bcDigits[I]]);
      end;
  end;

  if FBarCodeType in [bcUPC_A, bcUPC_E, bcEAN_8, bcEAN_13] then begin
    {add supplemental encodings if requested}
    if Length(FSupplementalCode) in [2, 5] then begin
      {get digits}
      bcDigitCount := GetDigits(FSupplementalCode);
      {7 spaces after primary code - 0000000}
      AddCode('0000000', [bkThreeQuarterBar, bkBlankSpace]);
      {encode left hand guard bars, 1011}
      AddCode('1011', [bkThreeQuarterBar, bkSupplement]);

      if bcDigitCount = 2 then begin
        {two digit supplement}
        {determine parity table to use for each of the two characters}
        X := bcDigits[1] * 10 + bcDigits[2];
        case X mod 4 of
          0 : AddSupCode('OO');
          1 : AddSupCode('OE');
          2 : AddSupCode('EO');
          3 : AddSupCode('EE');
         end;
      end else begin
        {five digit supplement}
        {determine the parity pattern to use for each of the five}
        X := ((bcDigits[1] + bcDigits[3] + bcDigits[5])*3 + (bcDigits[2] + bcDigits[4])*9) mod 10;
        case X of
          0 : AddSupCode('EEOOO');
          1 : AddSupCode('EOEOO');
          2 : AddSupCode('EOOEO');
          3 : AddSupCode('EOOOE');
          4 : AddSupCode('OEEOO');
          5 : AddSupCode('OOEEO');
          6 : AddSupCode('OOOEE');
          7 : AddSupCode('OEOEO');
          8 : AddSupCode('OEOOE');
          9 : AddSupCode('OOEOE');
        end;
      end;
    end;
  end;
end;

procedure TStBarCode.CalcBarCodeWidth;
var
  I : Integer;
begin
  bcNormalWidth := 0;
  bcSpaceWidth := 0;
  bcSupplementWidth := 0;
  for I := 0 to bcBarInfo.Count-1 do begin
    if bkSpace in bcBarInfo[I].Kind then begin
      if bkBlankSpace in bcBarInfo[I].Kind then
        Inc(bcSpaceWidth, bcSpaceModWidth*bcBarInfo[I].Modules)
      else if bkSupplement in bcBarInfo[I].Kind then
        Inc(bcSupplementWidth, bcSpaceModWidth*bcBarInfo[I].Modules)
      else
        Inc(bcNormalWidth, bcSpaceModWidth*bcBarInfo[I].Modules)
    end else begin
      if bkBlankSpace in bcBarInfo[I].Kind then
        Inc(bcSpaceWidth, bcBarModWidth*bcBarInfo[I].Modules)
      else if bkSupplement in bcBarInfo[I].Kind then
        Inc(bcSupplementWidth, bcBarModWidth*bcBarInfo[I].Modules)
      else
        Inc(bcNormalWidth, bcBarModWidth*bcBarInfo[I].Modules)
    end;
  end;
end;

procedure TStBarCode.CMTextChanged(var Msg : TMessage);
begin
  CalcBarCode;
  Invalidate;
end;

procedure TStBarCode.CopyToClipboard;
var
  MetaFile       : TMetaFile;
  MetaFileCanvas : TMetaFileCanvas;
  Bitmap         : TBitmap;
begin
  Clipboard.Clear;
  Clipboard.Open;
  try
    {bitmap}
    Bitmap := TBitmap.Create;
    try
      Bitmap.Width := ClientWidth;
      Bitmap.Height := ClientHeight;
      PaintToDC(Bitmap.Canvas.Handle, ClientRect);
      Clipboard.Assign(Bitmap);

      {metafile}
      MetaFile := TMetaFile.Create;
      try
        MetaFileCanvas := TMetaFileCanvas.Create(MetaFile, 0);
        try
          MetaFile.Enhanced := True;
          MetaFile.Width := ClientWidth;
          MetaFile.Height := ClientHeight;
          MetaFileCanvas.Draw(0, 0, Bitmap);
        finally
          MetaFileCanvas.Free;
        end;
        Clipboard.Assign(MetaFile);
      finally
        MetaFile.Free;
      end;

    finally
      Bitmap.Free;
    end
  finally
    Clipboard.Close;
  end;
end;

constructor TStBarCode.Create(AOwner : TComponent);
begin
  inherited Create(AOwner);

  bcBarInfo := TStBarCodeInfo.Create;

  {defaults}
  Color := clWhite;
  Width := 200;
  Height := 75;
  Text := '123456789012';

  FAddCheckChar := True;
  FBarColor := clBlack;
  FBarToSpaceRatio := 1;
  FBarNarrowToWideRatio := bcDefNarrowToWideRatio;
  FBarWidth := 12;
  FShowCode := True;
  FShowGuardChars := False;
  FTallGuardBars := False;
  FExtendedSyntax := False;
end;

destructor TStBarCode.Destroy;
begin
  bcBarInfo.Free;
  bcBarInfo := nil;

  inherited Destroy;
end;

function TStBarCode.DrawBar(XPos, YPos, AWidth, AHeight : Integer) : Integer;
begin
  Canvas.Rectangle(XPos, YPos, XPos+AWidth, YPos+AHeight);
  Result := XPos + AWidth;
end;

procedure TStBarCode.DrawBarCode(const R : TRect);
var
  I, X, Y        : Integer;
  CheckC         : Integer;
  CheckK         : Integer;
  TH, GA, TQ, BB : Integer;
  BarCodeHeight  : Integer;
  BarCodeWidth   : Integer;
  PixelsPerInchX : Integer;
  TR             : TRect;
  SmallestWidth  : Double;
  C              : string;
  Buf            : array[0..512] of Char;
begin
  Canvas.Brush.Color := FBarColor;
  Canvas.Brush.Style := bsSolid;

  PixelsPerInchX := GetDeviceCaps(Canvas.Handle, LOGPIXELSX);

  {determine narrowest line width}
  SmallestWidth := SmallestLineWidth(PixelsPerInchX);

  {find sizes for the BarCode elements}
  bcBarModWidth := Round(FBarWidth/1000 * PixelsPerInchX);
  if bcBarModWidth < FBarToSpaceRatio then
    bcBarModWidth := Round(BarToSpaceRatio);
  if bcBarModWidth < SmallestWidth then
    bcBarModWidth := Round(SmallestWidth);
  bcSpaceModWidth := Round(bcBarModWidth / FBarToSpaceRatio);

  {total width of BarCode and position within rect}
  CalcBarCodeWidth;
  BarCodeWidth := bcNormalWidth + bcSpaceWidth + bcSupplementWidth;
  BarCodeHeight := RectHeight(R);
  if BarCodeWidth < RectWidth(R) then
    X := R.Left + (RectWidth(R)-BarCodeWidth) div 2
  else
    X := R.Left;
  Y := R.Top;

  {get text height}
  TH := Canvas.TextHeight('Yg0');

  {guard bar adjustment}
  GA := (BarCodeHeight*10) div 100; {10% of bar height}
  {but, not more than 1/4 of the font height}
  if FShowCode and (GA > TH div 4) then
    GA := TH div 4;

  {three quarter height bar adjustment}
  TQ := BarCodeHeight div 4;

  {draw the text}
  if FShowCode and (Code > '') then begin
    C := Code;
    {fill out invalid codes}
    case FBarCodeType of
      bcUPC_A  :
        begin
          C := Copy(C, 1, 12); {truncate}
          if Length(C) = 11 then begin
            GetCheckCharacters(C, CheckC, CheckK);
            C := C + IntToStr(CheckC);
          end;
          while Length(C) < 12 do
            C := C + '0';
        end;
      bcUPC_E  :
        begin
          C := Copy(C, 1, 6); {truncate}
          while Length(C) < 6 do
            C := C + '0';
        end;
      bcEAN_8  :
        begin
          C := Copy(C, 1, 8); {truncate}
          if Length(C) = 7 then begin
            GetCheckCharacters(C, CheckC, CheckK);
            C := C + IntToStr(CheckC);
          end;
          while Length(C) < 8 do
            C := C + '0';
        end;
      bcEAN_13 :
        begin
          C := Copy(C, 1, 13); {truncate}
          if Length(C) = 12 then begin
            GetCheckCharacters(C, CheckC, CheckK);
            C := C + IntToStr(CheckC);
          end;
          while Length(C) < 13 do
            C := C + '0';
        end;
      bcInterleaved2of5 :
        begin
          if Odd(Length(C)) then
            C := '0' + C;
        end;
      bcCodabar :
        begin
          if not FShowGuardChars then
            {strip leading and trailing characters}
            C := Copy(C, 2, Length(C)-2);
        end;
      bcCode11 :
        begin
        end;
      bcCode39 :
        begin
          {add guard characters}
          if C[1] <> '*' then
            C := '*' + C;
          if C[Length(C)] <> '*' then
            C := C + '*';
          if not FShowGuardChars then
            {strip leading and trailing characters}
            C := Copy(C, 2, Length(C)-2);
        end;
      bcCode93 :
        begin
          {remove non-printable characters}
          for I := 1 to Length(C) do
            if C[I] < ' ' then
              C[I] := ' ';
        end;
      bcCode128 :
        begin
          {remove non-printable characters}
          I := 1;
          while I <= Length (C) do begin
            if C[I] < ' ' then
              C[I] := ' ';
            if (i < Length (C)) and (ExtendedSyntax) then begin
              if (C[I] = '\') and
                 CharInSet(C[I + 1], ['A', 'B', 'C', 'a', 'b', 'c']) then begin
                C[I] := ' ';
                C[I + 1] := ' ';
                Inc (I);
              end else if (C[I] = '\') and (C[I+1] = '\') then begin
                C[I] := ' ';
                Inc (I);
              end;
            end;
            Inc (I);
          end;
        end;
    end;

    Dec(BarCodeHeight, TH + (TH div 4));
    Canvas.Brush.Style := bsClear;
    {guard bar adjustment - again}
    GA := (BarCodeHeight*10) div 100; {10% of bar height}
    {but, not more than 1/4 of the font height}
    if FShowCode and (GA > TH div 4) then
      GA := TH div 4;
    {three quarter height bar adjustment}
    TQ := BarCodeHeight div 4;

    if FBarCodeType = bcUPC_A then begin
      {print first and last character to sides of symbol}
      TR.Top := Y;
      TR.Bottom := TR.Top + BarCodeHeight;
      {left hand character}
      Buf[0] := C[1];
      TR.Right := X;
      TR.Left := X - 2 * Canvas.TextWidth(C[1]);
      DrawText(Canvas.Handle, @Buf, 1, TR, DT_BOTTOM or DT_CENTER or DT_SINGLELINE);
      {remove character from code to print}
      C := Copy(C, 2, Length(C)-1);

      {right hand character - if no supplemental code}
      if FSupplementalCode = '' then begin
        Buf[0] := C[Length(C)];
        TR.Left := X + bcNormalWidth;
        TR.Right := X + bcNormalWidth +  2 * Canvas.TextWidth(C[Length(C)]);
        DrawText(Canvas.Handle, @Buf, 1, TR, DT_BOTTOM or DT_CENTER or DT_SINGLELINE);
        {remove character from code to print}
        C := Copy(C, 1, Length(C)-1);
      end;
    end;

    if FSupplementalCode > '' then begin
      {draw supplemental code above the code}
      TR.Top := Y + TQ - TH;
      TR.Bottom := Y + BarCodeHeight;
      TR.Left := X + bcNormalWidth + bcSpaceWidth;
      TR.Right := TR.Left + bcSupplementWidth;
      StrPLCopy(Buf, FSupplementalCode, Length(Buf)-1);
      DrawText(Canvas.Handle, @Buf, StrLen(Buf), TR, DT_VCENTER or DT_CENTER);
    end;

    TR := R;
    TR.Top := R.Top + BarCodeHeight + (TH div 4);
    TR.Left := X;
    TR.Right := TR.Left + bcNormalWidth;
    Canvas.Brush.Style := bsClear;
    StrPLCopy(Buf, C, Length(Buf)-1);
    DrawText(Canvas.Handle, @Buf, StrLen(Buf), TR, DT_VCENTER or DT_CENTER);
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := FBarColor;
  end;

  if (FBarCodeType = bcInterleaved2of5) and FBearerBars then begin
    BB := 3 * bcBarModWidth;
    {reduce height to allow for bearer bars}
    Dec(BarCodeHeight, BB * 2);
    {draw the bearer bars}
    Canvas.Rectangle(X-bcBarModWidth, Y,
                     X+BarCodeWidth+bcBarModWidth, Y+BB);
    Canvas.Rectangle(X-bcBarModWidth, Y+BarCodeHeight+BB,
                     X+BarCodeWidth+bcBarModWidth, Y+BarCodeHeight+BB*2);
    {adjust top of BarCode}
    Inc(Y, BB);
  end;

  {draw the bar code}
  for I := 0 to bcBarInfo.Count-1 do begin
    if bkSpace in bcBarInfo[I].Kind then
      Inc(X, bcSpaceModWidth*bcBarInfo[I].Modules)
    else if (bkGuard in bcBarInfo[I].Kind) and FTallGuardBars then begin
      if bcGuardBarAbove and bcGuardBarBelow then
        X := DrawBar(X, Y-GA, bcBarModWidth*bcBarInfo[I].Modules, BarCodeHeight+2*GA)
      else if bcGuardBarAbove then
        X := DrawBar(X, Y-GA, bcBarModWidth*bcBarInfo[I].Modules, BarCodeHeight+GA)
      else if bcGuardBarBelow then
        X := DrawBar(X, Y, bcBarModWidth*bcBarInfo[I].Modules, BarCodeHeight+2*GA)
    end else if (bkBar in bcBarInfo[I].Kind) or (bkGuard in bcBarInfo[I].Kind) then
      X := DrawBar(X, Y, bcBarModWidth*bcBarInfo[I].Modules, BarCodeHeight)
    else if (bkThreeQuarterBar in bcBarInfo[I].Kind) then
      X := DrawBar(X, Y+TQ, bcBarModWidth*bcBarInfo[I].Modules, BarCodeHeight-TQ);
  end;
end;

 {added}
function TStBarCode.GetBarCodeWidth(ACanvas : TCanvas) : Double;
var
  PixelsPerInchX : Integer;
  SmallestWidth  : Double;
begin
  PixelsPerInchX := GetDeviceCaps(ACanvas.Handle, LOGPIXELSX);

  {determine narrowest line width}
  SmallestWidth := SmallestLineWidth(PixelsPerInchX);

  {find sizes for the BarCode elements}
  bcBarModWidth := Round(FBarWidth/1000 * PixelsPerInchX);
  if bcBarModWidth < FBarToSpaceRatio then
    bcBarModWidth := Round(BarToSpaceRatio);
  if bcBarModWidth < SmallestWidth then
    bcBarModWidth := Round(SmallestWidth);
  bcSpaceModWidth := Round(bcBarModWidth / FBarToSpaceRatio);

  CalcBarcodeWidth;

  {width in pixels (not counting text printed to left or right of code)}
  Result := bcNormalWidth + bcSpaceWidth + bcSupplementWidth;
  {return width of barcode in inches}
  Result := Result / PixelsPerInchX;
end;

procedure TStBarCode.GetCheckCharacters(const S : string; var C, K : Integer);
var
  I  : Integer;
  C1 : Integer;
  C2 : Integer;
  St : string;
begin
  C := -1;
  K := -1;
  St := S;
  case FBarCodeType of
    bcUPC_A :
      begin
        if Length(St) >= 11 then begin
          {get digits}
          GetDigits(St);
          {determine check character}
          C1 := (bcDigits[1] + bcDigits[3] + bcDigits[5] + bcDigits[7] +
                 bcDigits[9] + bcDigits[11]) * 3;
          C2 := bcDigits[2] + bcDigits[4] + bcDigits[6] +
                bcDigits[8] + bcDigits[10];
          C := 10 - ((C1 + C2) mod 10);
          if C = 10 then
            C := 0;
        end;
      end;
    bcUPC_E :
      begin
        {get digits}
        GetDigits(St);
        {determine check character}
        C1 := (bcDigits[2] + bcDigits[4] + bcDigits[6]) * 3;
        C2 := bcDigits[1] + bcDigits[3] + bcDigits[5];
        C := 10 - ((C1 + C2) mod 10);
        if C = 10 then
          C := 0;
      end;
    bcEAN_8 :
      begin
        if Length(St) >= 7 then begin
          {get digits}
          GetDigits(St);
          {determine check character}
          C1 := (bcDigits[1] + bcDigits[3] + bcDigits[5] + bcDigits[7]) * 3;
          C2 := bcDigits[2] + bcDigits[4] + bcDigits[6];
          C := 10 - ((C1 + C2) mod 10);
          if C = 10 then
            C := 0;
        end;
      end;
    bcEAN_13 :
      begin
        if Length(St) >= 12 then begin
          {get digits}
          GetDigits(St);
          {determine check character}
          C1 := (bcDigits[2] + bcDigits[4] + bcDigits[6] + bcDigits[8] +
                 bcDigits[10] + bcDigits[12]) * 3;
          C2 := bcDigits[1] + bcDigits[3] + bcDigits[5] + bcDigits[7] +
                bcDigits[9] + bcDigits[11];
          C := 10 - ((C1 + C2) mod 10);
          if C = 10 then
            C := 0;
        end;
      end;
    bcInterleaved2of5 :
      begin
        {get digits}
        bcDigitCount := GetDigits(St);

        C1 := 0;
        C2 := 0;
        for I := 1 to bcDigitCount do
          if Odd(I) then
            C1 := C1 + bcDigits[I]  {odd digits}
          else
            C2 := C2 + bcDigits[I]; {even digits}
        C2 := C2 * 3;

        C := 10 - ((C1 + C2) mod 10);
        if C = 10 then
          C := 0;
      end;
    bcCodabar :
      begin
        {get digits}
        bcDigitCount := GetDigits(St);

        C1 := 0;
        for I := 1 to bcDigitCount do
          C1 := C1 + bcDigits[I];

        C := 16 - (C1 mod 16);
        if C = 16 then
          C := 0;
      end;
    bcCode11 :
      begin
        {get digits}
        bcDigitCount := GetDigits(St);
        C1 := 0;
        for I := bcDigitCount downto 1 do
          C1 := C1 + bcDigits[I]*(bcDigitCount-I+1);
        C1 := C1 mod 11; {the "C" check character}
        C2 := C1;
        for I := bcDigitCount downto 1 do
          C2 := C2 + bcDigits[I]*(bcDigitCount-I+2);
        C2 := C2 mod 11; {the "K" check character}
        K := C2;
        C := C1;
      end;
    bcCode39 :
      begin
        {get digits}
        bcDigitCount := GetDigits(St);

        C1 := 0;
        for I := 1 to bcDigitCount do
          C1 := C1 + bcDigits[I];

        C := 43 - (C1 mod 43);
        if C = 43 then
          C := 0;
      end;
    bcCode93 :
      begin
        {get digits}
        bcDigitCount := GetDigits(St);
        C1 := 0;
        for I := bcDigitCount downto 1 do
          C1 := C1 + bcDigits[I]*(bcDigitCount-I+1);
        C1 := C1 mod 47; {the "C" check character}
        C2 := C1;
        for I := bcDigitCount downto 1 do
          C2 := C2 + bcDigits[I]*(bcDigitCount-I+2);
        C2 := C2 mod 47; {the "K" check character}
        K := C2;
        C := C1;
      end;
    bcCode128 :
      begin
        {get digits}
        bcDigitCount := GetDigits(St);

        C1 := bcDigits[1];
        for I := 2 to bcDigitCount do
          C1 := C1 + bcDigits[I]*(I-1);

        C := C1 mod 103;
        if C = 103 then
          C := 0;
      end;
  end;
end;

function TStBarCode.GetCode : string;
begin
  Result := Text;
end;

function TStBarCode.GetDigits(Characters : string) : Integer;

  procedure GetACode128CDigit (c : Char; var Index : Integer;
                               var bcDigitPos : Integer);
  var
    J : Integer;

  begin
    case Ord(c) of
      130     : bcDigits[bcDigitPos + 1] := 98;  {rest are manufactured characters}
      131     : bcDigits[bcDigitPos + 1] := 97;
      132     : bcDigits[bcDigitPos + 1] := 96;
      133     : bcDigits[bcDigitPos + 1] := 98;
      134     : bcDigits[bcDigitPos + 1] := 100;
      135     : bcDigits[bcDigitPos + 1] := 99;
      136     : bcDigits[bcDigitPos + 1] := 103;
      137     : bcDigits[bcDigitPos + 1] := 104;
      138     : bcDigits[bcDigitPos + 1] := 105;
      139     : bcDigits[bcDigitPos + 1] := 106;
    else
      try
        J := StrToInt (Copy (Characters, Index, 2));
        bcDigits[bcDigitPos + 1] := J;
        Inc (Index);
      except
        RaiseStError(EStBarCodeError, stscInvalidCharacter);
      end;
    end;
    Inc (Index);
    Inc (bcDigitPos);
  end;

  procedure GetACode128ABDigit (c : Char; var Index : Integer;
                                var bcDigitPos : Integer);
  begin
    case c of
      ' '      : bcDigits[bcDigitPos + 1] := 0;
      '!'      : bcDigits[bcDigitPos + 1] := 1;
      '"'      : bcDigits[bcDigitPos + 1] := 2;
      '#'      : bcDigits[bcDigitPos + 1] := 3;
      '$'      : bcDigits[bcDigitPos + 1] := 4;
      '%'      : bcDigits[bcDigitPos + 1] := 5;
      '&'      : bcDigits[bcDigitPos + 1] := 6;
      ''''     : bcDigits[bcDigitPos + 1] := 7;
      '('      : bcDigits[bcDigitPos + 1] := 8;
      ')'      : bcDigits[bcDigitPos + 1] := 9;
      '*'      : bcDigits[bcDigitPos + 1] := 10;
      '+'      : bcDigits[bcDigitPos + 1] := 11;
      ','      : bcDigits[bcDigitPos + 1] := 12;
      '-'      : bcDigits[bcDigitPos + 1] := 13;
      '.'      : bcDigits[bcDigitPos + 1] := 14;
      '/'      : bcDigits[bcDigitPos + 1] := 15;
      '0'..'9' : bcDigits[bcDigitPos + 1] := 16 + Ord(c)-Ord('0');
      ':'      : bcDigits[bcDigitPos + 1] := 26;
      ';'      : bcDigits[bcDigitPos + 1] := 27;
      '<'      : bcDigits[bcDigitPos + 1] := 28;
      '='      : bcDigits[bcDigitPos + 1] := 29;
      '>'      : bcDigits[bcDigitPos + 1] := 30;
      '?'      : bcDigits[bcDigitPos + 1] := 31;
      '@'      : bcDigits[bcDigitPos + 1] := 32;
      'A'..'Z' : bcDigits[bcDigitPos + 1] := 33 + Ord(c)-Ord('A');
      '['      : bcDigits[bcDigitPos + 1] := 59;
      '\'      : bcDigits[bcDigitPos + 1] := 60;
      ']'      : bcDigits[bcDigitPos + 1] := 61;
      '^'      : bcDigits[bcDigitPos + 1] := 62;
      '_'      : bcDigits[bcDigitPos + 1] := 63;
      #0, #31  : bcDigits[bcDigitPos + 1] := 64 + Ord(c);  {control characters}
      '`'      : bcDigits[bcDigitPos + 1] := 64;
      'a'..'z' : bcDigits[bcDigitPos + 1] := 65 + Ord(c)-Ord('a');
      '{'      : bcDigits[bcDigitPos + 1] := 91;
      '|'      : bcDigits[bcDigitPos + 1] := 92;
      '}'      : bcDigits[bcDigitPos + 1] := 93;
      '~'      : bcDigits[bcDigitPos + 1] := 94;
      else
      begin
        case Ord(C) of
          130     : bcDigits[bcDigitPos + 1] := 98; {rest are manufactured characters}
          131     : bcDigits[bcDigitPos + 1] := 97;
          132     : bcDigits[bcDigitPos + 1] := 96;
          133     : bcDigits[bcDigitPos + 1] := 98;
          134     : bcDigits[bcDigitPos + 1] := 100;
          135     : bcDigits[bcDigitPos + 1] := 99;
          136     : bcDigits[bcDigitPos + 1] := 103;
          137     : bcDigits[bcDigitPos + 1] := 104;
          138     : bcDigits[bcDigitPos + 1] := 105;
          139     : bcDigits[bcDigitPos + 1] := 106;
        else
          RaiseStError(EStBarCodeError, stscInvalidCharacter);
        end;
      end;
    end;
    Inc (Index);
    Inc (bcDigitPos);
  end;

  function CountCode128Digits (Index : Integer) : Integer;
  begin
    Result := 0;
    while (Index <= Length (Characters)) and
          (Characters[Index] >= '0') and (Characters[Index] <= '9') do begin
      Inc (Result);
      Inc (Index);
    end;
  end;

  function CheckCode128Digits (Index : Integer; CharsLen : Integer) : Boolean;
  var
    NumDigits : Integer;
  begin
    Result := False;
    NumDigits := CountCode128Digits (Index);
    if NumDigits mod 2 <> 0 then begin
      Characters := Copy (Characters, 1, Index - 1) +
                    '0' + Copy (Characters, Index, CharsLen - Index + 1);
      Result := True;
    end;
  end;

  function GetCode128Digits : Integer;
  var
    I             : Integer;
    RLen          : Integer;
    CurMode       : TStCode128CodeSubset;
    NeedCharCount : Boolean;
    Skip          : Boolean;

  begin
    I := 1;
    Result := Length (Characters);
    RLen := 0;
    CurMode := Self.Code128Subset;
    NeedCharCount := Self.Code128Subset = csCodeC;

    while I <= Result do begin
      if (NeedCharCount) and
         (Characters[I] >= '0') and (Characters[I] <= '9') then begin
        NeedCharCount := False;
        if CheckCode128Digits (I, Result) then
          Inc (Result);
      end;

      Skip := False;
      if (ExtendedSyntax) and (Characters[I] = '\')  and
         (I < Result) then begin
        if ((Characters[I + 1] = 'A') or (Characters[I + 1] = 'a')) and
           (CurMode <> csCodeA) then begin
          Inc (RLen);
          bcDigits[RLen] := 101;
          CurMode := csCodeA;
          Skip := True;
        end else if ((Characters[I + 1] = 'B') or (Characters[I + 1] = 'b')) and
                    (CurMode <> csCodeB) then begin
          Inc (RLen);
          bcDigits[RLen] := 100;
          CurMode :=csCodeB;
          Skip := True;
        end else if ((Characters[I + 1] = 'C') or (Characters[I + 1] = 'c')) and
                    (CurMode <> csCodeC) then begin
          NeedCharCount := True;
          Inc (RLen);
          bcDigits[RLen] := 99;
          CurMode := csCodeC;
          Skip := True;
        end else if (Characters[I + 1] = '\') then begin
          GetACode128ABDigit ('\', I, RLen);
          Skip := True;
        end;
        Inc (I);
      end;

      if not Skip then
        case CurMode of
          csCodeC :
            GetACode128CDigit (Characters[I], I, RLen);
          else
            GetACode128ABDigit (Characters[I], I, RLen);
        end
      else
        Inc (I);
    end;
    Result := RLen;
  end;

var
  I, J : Integer;
  S    : string;
begin
  Result := 0;

  FillChar(bcDigits, SizeOf(bcDigits), #0);

  case FBarCodeType of
    bcUPC_A, bcUPC_E, bcEAN_8, bcEAN_13, bcInterleaved2of5 :
      begin
        Result := Length(Characters);
        for I := 1 to Result do
          bcDigits[I] := StrToInt(Characters[I]);
      end;
    bcCodabar :
      begin
        Result := Length(Characters);
        for I := 1 to Result do begin
          case Characters[I] of
            '0'..'9' : bcDigits[I] := StrToInt(Characters[I]);
            '-'      : bcDigits[I] := 10;
            '$'      : bcDigits[I] := 11;
            ':'      : bcDigits[I] := 12;
            '/'      : bcDigits[I] := 13;
            '.'      : bcDigits[I] := 14;
            '+'      : bcDigits[I] := 15;
            'A', 'a' : bcDigits[I] := 16;
            'B', 'b' : bcDigits[I] := 17;
            'C', 'c' : bcDigits[I] := 18;
            'D', 'd' : bcDigits[I] := 19;
          else
            RaiseStError(EStBarCodeError, stscInvalidCharacter);
          end;
        end;
      end;
    bcCode11 :
      begin
        Result := Length(Characters);
        for I := 1 to Result do begin
          case Characters[I] of
            '0'..'9' : bcDigits[I] := StrToInt(Characters[I]);
            '-'      : bcDigits[I] := 10;
          else
            RaiseStError(EStBarCodeError, stscInvalidCharacter);
          end;
        end;
      end;
    bcCode39 :
      begin
        Result := Length(Characters);
        for I := 1 to Result do begin
          case Characters[I] of
            '0'..'9' : bcDigits[I] := StrToInt(Characters[I]);
            'A'..'Z' : bcDigits[I] := Ord(Characters[I]) - Ord('A') + 10;
            '-'      : bcDigits[I] := 36;
            '.'      : bcDigits[I] := 37;
            ' '      : bcDigits[I] := 38;
            '$'      : bcDigits[I] := 39;
            '/'      : bcDigits[I] := 40;
            '+'      : bcDigits[I] := 41;
            '%'      : bcDigits[I] := 42;
            '*'      : bcDigits[I] := 43;
          else
            RaiseStError(EStBarCodeError, stscInvalidCharacter);
          end;
        end;
      end;
    bcCode93 :
      begin
        Result := Length(Characters);
        J := 1;
        I := 1;
        while I <= Result do begin
          S := Code93Map[Characters[I]];
          if Length(S) > 1 then begin
            case S[1] of
              '$' : bcDigits[J] := 43; {(+)}
              '%' : bcDigits[J] := 44; {(%)}
              '/' : bcDigits[J] := 45; {(/)}
              '+' : bcDigits[J] := 46; {(+)}
            else
              RaiseStError(EStBarCodeError, stscInvalidCharacter);
            end;
            Inc(J);
            S := S[2];
          end;

          case S[1] of
            '0'..'9' : bcDigits[J] := Ord(S[1])-Ord('0');
            'A'..'Z' : bcDigits[J] := 10 + Ord(S[1])-Ord('A');
            '-'      : bcDigits[J] := 36;
            '.'      : bcDigits[J] := 37;
            ' '      : bcDigits[J] := 38;
            '$'      : bcDigits[J] := 39;
            '/'      : bcDigits[J] := 40;
            '+'      : bcDigits[J] := 41;
            '%'      : bcDigits[J] := 42;
          else
            RaiseStError(EStBarCodeError, stscInvalidCharacter);
          end;
          Inc(I);
          Inc(J);
        end;
        Result := J;
      end;
    bcCode128 :
      Result := GetCode128Digits;
  end;
end;

function TStBarCode.GetVersion : string;
begin
  Result := StVersionStr;
end;

procedure TStBarCode.Loaded;
begin
  inherited Loaded;

  CalcBarCode;
end;

procedure TStBarCode.Paint;
var
  Margin : Integer;
  R      : TRect;
begin
  {use our font}
  Canvas.Font := Font;

  {clear the canvas}
  Canvas.Brush.Color := Color;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(ClientRect);

  {adjust height of rect to provide top and bottom margin}
  R := ClientRect;
  Margin := RectHeight(R)*10 div 100;
  InflateRect(R, 0, -Margin);
  PaintPrim(R);
end;

procedure TStBarCode.PaintPrim(const R : TRect);
begin
  Canvas.Brush.Style := bsClear;
  Canvas.Brush.Color := FBarColor;
  Canvas.Pen.Color := FBarColor;
  DrawBarCode(R);
end;

procedure TStBarCode.PaintToCanvas(ACanvas : TCanvas; ARect : TRect);
var
  Margin  : Integer;
  SavedDC : Integer;
  R       : TRect;
begin
  Canvas.Handle := ACanvas.Handle;
  SavedDC := SaveDC(ACanvas.Handle);
  try
    {use our font}
    Canvas.Font := Font;

    {clear the specified area of the canvas}
    Canvas.Brush.Color := Color;
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(ARect);

    {adjust height of rect to provide top and bottom margin}
    R := ARect;
    Margin := RectHeight(R)*10 div 100;
    InflateRect(R, 0, -Margin);
    PaintPrim(R);
  finally
    Canvas.Handle := 0;
    RestoreDC(ACanvas.Handle, SavedDC);
  end;
end;

procedure TStBarCode.PaintToCanvasSize(ACanvas : TCanvas; X, Y, H : Double);
var
  TH             : Integer;
  PixelsPerInchX : Integer;
  PixelsPerInchY : Integer;
  OldPPI         : Integer;
  SavedDC        : Integer;
  R              : TRect;
  SmallestWidth  : Double;
begin
  Canvas.Handle := ACanvas.Handle;
  SavedDC := SaveDC(ACanvas.Handle);
  try
    {get some information about this device context}
    PixelsPerInchX := GetDeviceCaps(Canvas.Handle, LOGPIXELSX);
    PixelsPerInchY := GetDeviceCaps(Canvas.Handle, LOGPIXELSY);

    OldPPI := Canvas.Font.PixelsPerInch;
    {this is necessary because of a Delphi buglet}
    Canvas.Font.PixelsPerInch := PixelsPerInchY;

    {use our font}
    Canvas.Font := Font;

    {determine narrowest line width}
    SmallestWidth := SmallestLineWidth(PixelsPerInchX);

    {find sizes for the BarCode elements}
    bcBarModWidth := Round(FBarWidth/1000 * PixelsPerInchX);
    if bcBarModWidth < FBarToSpaceRatio then
      bcBarModWidth := Round(FBarToSpaceRatio);
    if bcBarModWidth < SmallestWidth then
      bcBarModWidth := Round(SmallestWidth);
    bcSpaceModWidth := Round(bcBarModWidth / FBarToSpaceRatio);
    CalcBarCodeWidth;

    {convert to a rect}
    R := Rect(Round(X * PixelsPerInchX),
              Round(Y * PixelsPerInchY),
              Round(X * PixelsPerInchX) + bcNormalWidth + bcSpaceWidth + bcSupplementWidth,
              Round((Y + H) * PixelsPerInchY));

    {increase height of rect to allow for text}
    if FShowCode and (Code > '') then begin
      TH :=Canvas.TextHeight(Code);
      Inc(R.Bottom, TH + (TH div 4));
    end;

    PaintPrim(R);
    Canvas.Font.PixelsPerInch := OldPPI;
    Invalidate;
  finally
    Canvas.Handle := 0;
    RestoreDC(ACanvas.Handle, SavedDC);
  end;
end;

procedure TStBarCode.PaintToDC(DC : hDC; ARect : TRect);
var
  Margin  : Integer;
  SavedDC : Integer;
  R       : TRect;
begin
  Canvas.Handle := DC;
  SavedDC := SaveDC(DC);
  try
    {use our font}
    Canvas.Font := Font;

    {clear the specified area of the canvas}
    Canvas.Brush.Color := Color;
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(ARect);

    {adjust height of rect to provide top and bottom margin}
    R := ARect;
    Margin := RectHeight(R)*10 div 100;
    InflateRect(R, 0, -Margin);
    PaintPrim(R);
  finally
    Canvas.Handle := 0;
    RestoreDC(DC, SavedDC);
  end;
end;

procedure TStBarCode.PaintToDCSize(DC : hDC; X, Y, W, H : Double);
begin
  Canvas.Handle := DC;
  PaintToCanvasSize(Canvas, X, Y, H);
end;

procedure TStBarCode.SaveToFile(const FileName : string);
var
  Bitmap : TBitmap;
begin
  Bitmap := TBitmap.Create;
  try
    Bitmap.Width := ClientWidth;
    Bitmap.Height := ClientHeight;
    PaintToDC(Bitmap.Canvas.Handle, ClientRect);
    Bitmap.SaveToFile(FileName);
  finally
    Bitmap.Free;
  end
end;

procedure TStBarCode.SetAddCheckChar(Value : Boolean);
begin
  if Value <> FAddCheckChar then begin
    FAddCheckChar := Value;
    CalcBarCode;
    Invalidate;
  end;
end;

procedure TStBarCode.SetBarCodeType(Value : TStBarCodeType);
begin
  if Value <> FBarCodeType then begin
    FBarCodeType := Value;
    CalcBarCode;
    Invalidate;
  end;
end;

procedure TStBarCode.SetBarColor(Value : TColor);
begin
  if Value <> FBarColor then begin
    FBarColor := Value;
    Invalidate;
  end;
end;

procedure TStBarCode.SetBarToSpaceRatio(Value : Double);
begin
  {always uses a bar to space ratio of 1}
  if FBarCodeType in [bcInterleaved2of5, bcCode11, bcCode39, bcCode93, bcCode128] then
    Value := 1;

  if Value <> FBarToSpaceRatio then begin
    FBarToSpaceRatio := Value;
    CalcBarCode;
    Invalidate;
  end;
end;

procedure TStBarCode.SetBarNarrowToWideRatio(Value : Integer);
begin
  if Value <> FBarNarrowToWideRatio then begin
    FBarNarrowToWideRatio := Value;
    CalcBarCode;
    Invalidate;
  end;
end;

procedure TStBarCode.SetBarWidth(Value : Double);
begin
  if Value <> FBarWidth then begin
    FBarWidth := Value;
    Invalidate;
  end;
end;

procedure TStBarCode.SetBearerBars(Value : Boolean);
begin
  if Value <> FBearerBars then begin
    FBearerBars := Value;
    Invalidate;
  end;
end;

procedure TStBarCode.SetCode(const Value : string);
begin
  if FBarCodeType in [bcCode39] then
    Text := UpperCase(Value)
  else if FBarCodeType in [bcCodabar] then
    Text := LowerCase(Value)
  else
    Text := Value;
end;

procedure TStBarCode.SetCode128Subset(Value : TStCode128CodeSubset);
begin
  if Value <> FCode128Subset then begin
    FCode128Subset := Value;
    CalcBarCode;
    Invalidate;
  end;
end;

procedure TStBarCode.SetExtendedSyntax (const v : Boolean);
begin
  if v <> FExtendedSyntax then begin
    FExtendedSyntax := v;
    CalcBarCode;
    Invalidate;
  end;
end;

procedure TStBarCode.SetShowCode(Value : Boolean);
begin
  if Value <> FShowCode then begin
    FShowCode := Value;
    Invalidate;
  end;
end;

procedure TStBarCode.SetShowGuardChars(Value : Boolean);
begin
  if Value <> FShowGuardChars then begin
    FShowGuardChars := Value;
    Invalidate;
  end;
end;

procedure TStBarCode.SetSupplementalCode(const Value : string);
begin
  if Value <> FSupplementalCode then begin
    FSupplementalCode := Value;
    CalcBarCode;
    Invalidate;
  end;
end;

procedure TStBarCode.SetTallGuardBars(Value : Boolean);
begin
  if Value <> FTallGuardBars then begin
    FTallGuardBars := Value;
    Invalidate;
  end;
end;

procedure TStBarCode.SetVersion(const Value : string);
begin
end;

function TStBarCode.SmallestLineWidth(PixelsPerInch : Integer) : Double;
begin
  Result := PixelsPerInch * 0.010; {10 mils}
  if Result < 1 then
    Result := 1;
end;

function TStBarCode.Validate(DisplayError : Boolean) : Boolean;
var
  I      : Integer;
  CheckC : Integer;
  CheckK : Integer;
begin
  Result := True;
  try
    case FBarCodeType of
      bcUPC_A :
        begin
          {11 or 12 characters}
          if not (Length(Code) in [11, 12]) then
            RaiseStError(EStBarCodeError, stscInvalidUPCACodeLen);
          try
            GetDigits(Code);
          except
            RaiseStError(EStBarCodeError, stscInvalidCharacter);
          end;

          GetCheckCharacters(Code, CheckC, CheckK);
          if (Length(Code) = 12) and (CheckC <> bcDigits[12]) then
            RaiseStError(EStBarCodeError, stscInvalidCheckCharacter);
        end;
      bcUPC_E :
        begin
          {6 characters}
          if not (Length(Code) = 6) then
            RaiseStError(EStBarCodeError, stscInvalidUPCACodeLen);
          try
            GetDigits(Code);
          except
            RaiseStError(EStBarCodeError, stscInvalidCharacter);
          end;
        end;
      bcEAN_8 :
        begin
          {7 or 8 characters}
          if not (Length(Code) in [7, 8]) then
            RaiseStError(EStBarCodeError, stscInvalidEAN8CodeLen);
          try
            GetDigits(Code);
          except
            RaiseStError(EStBarCodeError, stscInvalidCharacter);
          end;

          GetCheckCharacters(Code, CheckC, CheckK);
          if (Length(Code) = 8) and (CheckC <> bcDigits[8]) then
            RaiseStError(EStBarCodeError, stscInvalidCheckCharacter);
        end;
      bcEAN_13 :
        begin
          {12 or 13 characters}
          if not (Length(Code) in [12, 13]) then
            RaiseStError(EStBarCodeError, stscInvalidEAN13CodeLen);
          try
            GetDigits(Code);
          except
            RaiseStError(EStBarCodeError, stscInvalidCharacter);
          end;

          GetCheckCharacters(Code, CheckC, CheckK);
          if (Length(Code) = 13) and (CheckC <> bcDigits[13]) then
            RaiseStError(EStBarCodeError, stscInvalidCheckCharacter);
        end;
      bcInterleaved2of5 :
        begin
          try
            GetDigits(Code);
          except
            RaiseStError(EStBarCodeError, stscInvalidCharacter);
          end;
        end;
      bcCodabar :
        begin
          for I := 1 to Length(Code) do
            if not CharInSet(Code[I], ['0'..'9', '-', '$', ':', '/', '.', '+', 'a'..'d', 'A'..'D']) then
              RaiseStError(EStBarCodeError, stscInvalidCharacter);
        end;
      bcCode11 :
        begin
          for I := 1 to Length(Code) do
            if not CharInSet(Code[I], ['0'..'9', '-']) then
              RaiseStError(EStBarCodeError, stscInvalidCharacter);
          {test check characters}
          if not FAddCheckChar then begin
            GetCheckCharacters(Code, CheckC, CheckK);
            if (StrToInt(Code[Length(Code)-1]) <> CheckC) or
               (StrToInt(Code[Length(Code)]) <> CheckK) then
              RaiseStError(EStBarCodeError, stscInvalidCheckCharacter);
          end;
        end;
      bcCode39 :
        begin
          for I := 1 to Length(Code) do
            if not CharInSet(Code[I], ['0'..'9', 'A'..'Z', 'a'..'z',
            '-', '.', ' ', '$', '/', '+', '%', '*']) then
              RaiseStError(EStBarCodeError, stscInvalidCharacter);
          {check for embedded guard character}
          for I := 2 to Length(Code)-1 do
            if Code[I] = '*' then
              RaiseStError(EStBarCodeError, stscInvalidCharacter);
        end;
      bcCode93 :
        begin
          try
            GetCheckCharacters(Code, CheckC, CheckK);
          except
            RaiseStError(EStBarCodeError, stscInvalidCharacter);
          end;
        end;
      bcCode128 :
        begin
          try
            GetCheckCharacters(Code, CheckC, CheckK);
          except
            RaiseStError(EStBarCodeError, stscInvalidCharacter);
          end;
        end;
    end;
    {check supplemental code}
    if FSupplementalCode > '' then
      if not (Length(FSupplementalCode) in [2, 5]) then
        RaiseStError(EStBarCodeError, stscInvalidSupCodeLen);
  except
    Result := False;
    if DisplayError then
      raise;
  end;
end;


end.
