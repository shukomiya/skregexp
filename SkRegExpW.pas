﻿(* ***************************************************************************
  SkRegExpW.pas (SkRegExp regular expression library)
  **************************************************************************** *)
(*
  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in
  compliance with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/

  Software distributed under the License is distributed on an "AS IS"
  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
  License for the specific language governing rights and limitations
  under the License.

  The Original Code is SkRegExpW.pas(for SkRegExp Library).

  The Initial Developer of the Original Code is Shuichi Komiya.

  E-mail: shu AT k DOT email DOT ne DOT jp
  URL:    http://skregexp.komish.com/

  Portions created by Komiya Shuichi are
  Copyright (C) 2007-2013 Komiya Shuichi. All Rights Reserved.
*)

unit SkRegExpW;

interface

{ Jananese Extenstion Define.
  Undefine JapaneseExt if you do not use Japanese.

  日本語特有の処理を行う条件定義
  以下の定義を無効にすると、全角半角の同一視、カタカナひらがなの同一視を行わない。 }
{$DEFINE JapaneseExt}
{$IFDEF JapaneseExt}
{ (?k) と (?w) を正規表現パターン内で使えるようにする条件定義。
  無効にしても、IgnoreKana, IgnoreWidth プロパティで指定することはできる。 }
  {$DEFINE UseJapaneseOption}
{$ENDIF}

{.$DEFINE CHECK_MATCH_EXPLOSION}

{$DEFINE USE_UNICODE_PROPERTY}

uses
  SysUtils,
  Classes,
  Contnrs,
{$IFNDEF UNICODE}
  WideStrings,
  WideStrUtils,
{$ENDIF}
  SkRegExpConst
{$IFDEF USE_UNICODE_PROPERTY}
  ,
  UnicodeProp
{$ENDIF USE_UNICODE_PROPERTY}
  ;

const
{$IFDEF CONDITIONALEXPRESSIONS}
{$IF CompilerVersion >= 16.0}
  MaxListSize = Maxint div 16;
{$IFEND}
{$ENDIF}
  CONST_CharMapMax = 251;
  CONST_GroupNameHashMax = 15;
  CONST_TrieHashMax = 15;

type
  { Exception }
  { 例外 }
  ESkRegExp = class(Exception);

  ESkRegExpRuntime = class(ESkRegExp);

  ESkRegExpCompile = class(ESkRegExp)
  public
    ErrorPos: Integer
  end;

{$IFDEF UNICODE}
  TREStrings = TStrings;
  TREStringList = TStringList;
  REString = UnicodeString;
{$ELSE}
  TREStrings = TWideStrings;
  TREStringList = TWideStringList;
  REString = WideString;
{$ENDIF}
  UChar = UCS4Char;

  { String Compare Option }
  { 文字照合オプション }
  TRECompareOption = (coIgnoreCase, coIgnoreWidth, coIgnoreKana, coASCIIOnly);
  TRECompareOptions = set of TRECompareOption;

  { Regular Expression Option }
  { 正規表現オプション }
  TREOption = (roNone, roIgnoreCase, roMultiLine, roNamedGroupOnly,
    roSingleLine, roExtended, roIgnoreWidth, roIgnoreKana,
    roDefinedCharClassLegacy, roAutoCallout, roASCIICharClass, roASCIIOnly);
  TREOptions = set of TREOption;
  PREOptions = ^TREOptions;

  { Token }
  { トークン }
  TREToken = (tkEnd, tkChar, tkUnion, tkQuest, tkDot, tkRangeChar, tkLHead,
    tkLTail, tkEmpty, tkLPar, tkRPar, tkStar, tkPlus, tkBound,
    tkCharClassFirst, tkNegativeCharClassFirst, tkCharClassEnd, tkGroupBegin,
    tkReference, tkReferenceRelative, tkNamedReference, tkLParWithOption,
    tkWordChar, tkNEWordChar, tkDigitChar, tkNEDigitChar, tkSpaceChar,
    tkNESpaceChar, tkTHead, tkTTail, tkTTailEnd, tkWordBoundary,
    tkNEWordBoundary, tkOption, tkHorizontalSpaceChar,
    tkNEHorizontalSpaceChar, tkVerticalSpaceChar, tkNEVerticalSpaceChar,
    tkLineBreak, tkPosixBracket, tkNEPosixBracket, tkProperty, tkNEProperty,
    tkNoBackTrack, tkKeepPattern, tkAheadMatch, tkAheadNoMatch, tkBehindMatch,
    tkBehindNoMatch, tkCombiningSequence, tkGoSub, tkGoSubName,
    tkGoSubRelative, tkIfMatch, tkIfMatchRef, tkGlobalPos, tkBranchReset,
    tkExceptEOL,
    tkFail, tkCallout, tkInSubRef, tkDefine, tkbcPrune, tkbcSkip, tkbcMark,
    tkbcThen, tkbcCommit, tkbcAccept, tkEOLType);

  TREOperator = (opEmply, opConcat, opUnion, opGroup, opLHead, opLTail,
    opPlus, opStar, opBound, opLoop, opNoBackTrack, opKeepPattern,
    opAheadMatch, opAheadNoMatch, opBehindMatch, opBehindNoMatch, opGoSub,
    opIfMatch, opIfThen, opDefine, opFail, opPrune, opSkip, opMark, opThen,
    opCommint, opAccept);

  TRENFAKind = (nkNormal, nkChar, nkEmpty, nkStar, nkPlus, nkBound, nkQuest, nkLoop,
    nkLoopExit, nkLoopEnd, nkGroupBegin, nkGroupEnd, nkKeepPattern,
    nkSuspend, nkMatchEnd, nkEnd, nkGoSub, nkAheadMatch, nkAheadNoMatch,
    nkBehindMatch, nkBehindNoMatch, nkIfMatch, nkIfThen, nkCallout, nkDefine,
    nkFail, nkPrune, nkSkip, nkMark, nkThen, nkCommit, nkAccept);

  TRELoopKind = (lkNone, lkGreedy, lkReluctant, lkSimpleReluctant, lkPossessive,
    lkAny, lkCombiningSequence);

  TREPosixClassKind = (pckNone, pckAlnum, pckAlpha, pckAscii, pckBlank,
    pckCntrl, pckDigit, pckGraph, pckLower, pckPrint, pckUpper, pckPunct,
    pckPunctPerl, pckPunctVertical, pckPunctHorizontal,
    pckSpace, pckSpacePerl, pckSpaceVertical, pckSpaceHorizontal,
    pckXdigit, pckWord, pckAny, pckAssigned);

  TRELineBreakKind = (lbAnyCRLF, lbLF, lbCR, lCRLF, lbAny);

  TRETextPosRec = record
    Min, Max: Integer;
  end;

{$IFNDEF USE_UNICODE_PROPERTY}
  TUnicodeMultiChar = array[0..3] of Integer;
{$ENDIF NDEF USE_UNICODE_PROPERTY}

  TREQuickSearch = class
  private
    FPattern: PWideChar;
    FPatternLen: Integer;
    FSkipTable: array [0 .. 255] of Integer;
    FTextTopP, FTextEndP: PWideChar;
    FTextLen: Integer;
    FMatchLen: Integer;
    FCompiled: Boolean;
    FMatchP: PWideChar;
    FFindText: REString;
    FOptions: TRECompareOptions;
    FSkipP: PWideChar;
    procedure SetFindText(const Value: REString);
    procedure SetOptions(const Value: TRECompareOptions);
  protected
    function IsMatch(AStr: PWideChar; AOptions: TRECompareOptions): Boolean;
  public
    procedure Clear;
    procedure Compile;
    function Exec(AStr: PWideChar; ATextLen: Integer): Boolean;
    function ExecNext: Boolean;
    property FindText: REString read FFindText write SetFindText;
    property MatchLen: Integer read FMatchLen;
    property MatchP: PWideChar read FMatchP;
    property SkipP: PWideChar read FSkipP write FSkipP;
    property Options: TRECompareOptions read FOptions write SetOptions;
    property PatternLen: Integer read FPatternLen;
  end;

  TRECode = class;
  TRETrieList = class;

  TRETrieNode = class
  private
    FParent: TRETrieNode;
    FWChar: UChar;
    FChildren: TRETrieList;
    FFailure: TRETrieNode;
    FAccepted: Boolean;
    FOptions: TRECompareOptions;
    FIsHead: Boolean;
    FCode: TRECode;
    FSourceString: REString;
  public
    constructor Create(AParent: TRETrieNode; AWChar: UChar);
    destructor Destroy; override;
    procedure Clear;
    property Children: TRETrieList read FChildren;
    property WChar: UChar read FWChar write FWChar;
    property Parent: TRETrieNode read FParent write FParent;
    property Failure: TRETrieNode read FFailure write FFailure;
    property Accepted: Boolean read FAccepted write FAccepted;
    property Options: TRECompareOptions read FOptions write FOptions;
    property SourceString: REString read FSourceString write FSourceString;
    property IsHead: Boolean read FIsHead write FIsHead;
    property Code: TRECode read FCode write FCode;
  end;

  PRETrieHashData = ^TRETrieHashData;
  TRETrieHashData = record
    Index: Integer;
    Next: PRETrieHashData;
  end;

  TRETrieList = class
  private
    FList: TList;
    FBuckets: array [0..CONST_TrieHashMax - 1] of Pointer;
    FStartWChar, FLastWChar: UChar;
    FOptions: TRECompareOptions;
    function Get(Index: Integer): TRETrieNode;
  public
    constructor Create;
    destructor Destroy; override;
    function Add(const Value: TRETrieNode): Integer;
    function Count: Integer;
    function Find(const WChar: UChar): Integer; overload;
    procedure Clear;
    property Items[Index: Integer]: TRETrieNode read Get; default;
    property Options: TRECompareOptions read FOptions write FOptions;
  end;

  TREACSearch = class
  private
    FRoot: TRETrieNode;
    FCompiled: Boolean;
    FStartP: PWideChar;
    FEndP: PWideChar;
    FMatchTopP, FMatchEndP: PWideChar;
    FOptions: TRECompareOptions;
    FSkipP: PWideChar;
    FCodeList: TObjectList;
    function GetMatchString: REString;
    function GetMatchLength: Integer;
  protected
    procedure MakeFailure(ANode: TRETrieNode);
    function Go(ANode: TRETrieNode; Ch: UChar): TRETrieNode;
    function InternalAdd(const S: REString; AOptions: TRECompareOptions): TRETrieNode;
    function MatchCore(AStr: PWideChar): Boolean;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(const S: REString; AOptions: TRECompareOptions = []); overload;
    procedure Add(ACode: TRECode); overload;
    procedure Clear;
    procedure Compile;
    function Match(AStr: PWideChar): Boolean;
    function Exec(AStr: PWideChar; ATextLen: Integer): Boolean;
    function ExecNext: Boolean;
    property Root: TRETrieNode read FRoot;
    property MatchString: REString read GetMatchString;
    property MatchLength: Integer read GetMatchLength;
    property Compiled: Boolean read FCompiled;
    property StartP: PWideChar read FStartP;
    property EndP: PWideChar read FEndP;
    property SkipP: PWideChar read FSkipP write FSkipP;
  end;

  TRECharMapRec = record
    Ch: UChar;
    Next: Pointer;
  end;
  PRECharMapRec = ^TRECharMapRec;

  TRECharMap = class
  private
    FMap: array[0..CONST_CharMapMax - 1] of Pointer;
    FASCIIMap: array [0 .. 32] of Byte;
    FStartChar, FLastChar: UChar;
    FCount: Integer;
    FHasUnicode: Boolean;
    FIgnoreCase: Boolean;
    FASCIIOnly: Boolean;
  protected
    procedure InternalAdd(Ch: UChar);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TRECharMap);
    function Add(Ch: UChar; AOptions: TRECompareOptions): Integer; overload;
    procedure Add(AMap: TRECharMap); overload;
    procedure Clear;
    function Count: Integer;
    function IsExists(AStr: PWideChar): Boolean; overload;
{$IFDEF SKREGEXP_DEBUG}
    procedure MapToList(ADest: TREStrings);
    function GetDebugStr: REString;
{$ENDIF SKREGEXP_DEBUG}
  end;

  TCalloutData = record
    Version: Integer;
    CalloutNumber: Integer;
    CurrentPosition: Integer;
    StartMatch: Integer;
    PatternPosition: Integer;
    PatternLength: Integer;
  end;
  PCalloutData = ^TCalloutData;

  TSkRegExp = class;

  { 文字列の照合を行う基底クラス }
  TRECode = class
  private
    FRegExp: TSkRegExp;
  protected
    function GetLength: Integer; virtual;
    function GetCharLength: TRETextPosRec; virtual;
    function GetSearch: TREQuickSearch; virtual;
  public
    constructor Create(ARegExp: TSkRegExp);
    function CompareCode(Source: TRECode): Integer; virtual;
    function ExecRepeat(var AStr: PWideChar; IsStar: Boolean): Boolean;
      overload; virtual;
    function ExecRepeat(var AStr: PWideChar; AMin, AMax: Integer): Boolean;
      overload; virtual;
    function Find(AStr: PWideChar): PWideChar; virtual;
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; virtual;
    // 文字クラスの最適化用。重複した比較をしないため。
    function IsInclude(ACode: TRECode): Boolean; virtual;
    // この Code の末尾が ACode の先頭と一致すればTrue。繰り返しの最適化用
    function IsOverlap(ACode: TRECode): Boolean; virtual;
    function IsVariable: Boolean; virtual;
    function IsAny: Boolean; virtual;
{$IFDEF SKREGEXP_DEBUG}
    function GetDebugStr: REString; virtual;
{$ENDIF}
    // エレメント数
    property Length: Integer read GetLength;
    // 文字数
    property CharLength: TRETextPosRec read GetCharLength;
    property Search: TREQuickSearch read GetSearch;
  end;

  TRELiteralCode = class(TRECode)
  private
    FStrings: REString;
    FSubP: PWideChar;
    FLength: Integer;
    FCharLength: Integer;
    FOptions: TREOptions;
    FCompareOptions: TRECompareOptions;
    FSearch: TREQuickSearch;
  protected
    function GetCharLength: TRETextPosRec; override;
    function GetLength: Integer; override;
    function GetSearch: TREQuickSearch; override;
  public
    constructor Create(ARegExp: TSkRegExp; Str: UCS4String;
      AOptions: TREOptions); overload;
    constructor Create(ARegExp: TSkRegExp; Str: REString;
      AOptions: TREOptions); overload;
    constructor Create(ARegExp: TSkRegExp; AWChar: UChar;
      AOptions: TREOptions); overload;
    destructor Destroy; override;
    function ExecRepeat(var AStr: PWideChar; IsStar: Boolean): Boolean; override;
    function Find(AStr: PWideChar): PWideChar; override;
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
    function IsOverlap(ACode: TRECode): Boolean; override;
{$IFDEF SKREGEXP_DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TREAnyCharCode = class(TRECode)
  private
    FOptions: TREOptions;
    FIsMatchAll: Boolean;
  public
    constructor Create(ARegExp: TSkRegExp; AOptions: TREOptions);
    function ExecRepeat(var AStr: PWideChar; IsStar: Boolean): Boolean;
      override;
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
    function IsAny: Boolean; override;
{$IFDEF SKREGEXP_DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TREWordCharCode = class(TRECode)
  private
    FOptions: TREOptions;
    FNegative: Boolean;
    FIsASCII: Boolean;
  public
    constructor Create(ARegExp: TSkRegExp; AOptions: TREOptions;
      ANegative: Boolean);
    function CompareCode(Dest: TRECode): Integer; override;
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
    function IsOverlap(ACode: TRECode): Boolean; override;
{$IFDEF SKREGEXP_DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TREDigitCharCode = class(TRECode)
  private
    FOptions: TREOptions;
    FNegative: Boolean;
    FIsASCII: Boolean;
  public
    constructor Create(ARegExp: TSkRegExp; AOptions: TREOptions;
      ANegative: Boolean);
    function CompareCode(Dest: TRECode): Integer; override;
    function ExecRepeat(var AStr: PWideChar; IsStar: Boolean): Boolean;
      overload; override;
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
    function IsOverlap(ACode: TRECode): Boolean; override;
{$IFDEF SKREGEXP_DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TRESpaceCharCode = class(TRECode)
  private
    FOptions: TREOptions;
    FNegative: Boolean;
    FIsASCII: Boolean;
  public
    constructor Create(ARegExp: TSkRegExp; AOptions: TREOptions;
      ANegative: Boolean);
    function CompareCode(Dest: TRECode): Integer; override;
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
    function IsOverlap(ACode: TRECode): Boolean; override;
{$IFDEF SKREGEXP_DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TREHorizontalSpaceCharCode = class(TRECode)
  private
    FNegative: Boolean;
  public
    constructor Create(ARegExp: TSkRegExp; ANegative: Boolean);
    function CompareCode(Dest: TRECode): Integer; override;
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
    function IsOverlap(ACode: TRECode): Boolean; override;
{$IFDEF SKREGEXP_DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TREVerticalSpaceCharCode = class(TRECode)
  private
    FNegative: Boolean;
  public
    constructor Create(ARegExp: TSkRegExp; ANegative: Boolean);
    function CompareCode(Dest: TRECode): Integer; override;
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
    function IsOverlap(ACode: TRECode): Boolean; override;
{$IFDEF SKREGEXP_DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TRELineBreakCharCode = class(TRECode)
  protected
    function GetCharLength: TRETextPosRec; override;
    function GetLength: Integer; override;
  public
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
    function IsOverlap(ACode: TRECode): Boolean; override;
    function IsVariable: Boolean; override;
{$IFDEF SKREGEXP_DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TRECharClassCode = class(TRECode)
  private
    FMap: TRECharMap;
    FWChar: UChar;
    FNegative: Boolean;
    FCodeList: TObjectList;
    FOptions: TREOptions;
    FSimpleClass: Boolean;
  public
    constructor Create(ARegExp: TSkRegExp; ANegative: Boolean;
      AOptions: TREOptions);
    destructor Destroy; override;
    function Add(AWChar: UChar): Integer; overload;
    function Add(AStartWChar, ALastWChar: UChar): Integer; overload;
    function Add(Value: TRECode): Integer; overload;
    function Find(AStr: PWideChar): PWideChar; override;
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
    function IsOverlap(ACode: TRECode): Boolean; override;
    procedure Rebuild;
    procedure Sort;
{$IFDEF SKREGEXP_DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
    property SimpleClass: Boolean read FSimpleClass;
  end;

{$IFDEF USE_UNICODE_PROPERTY}
  TRECombiningSequence = class(TRECode)
  protected
    function GetCharLength: TRETextPosRec; override;
  public
    function ExecRepeat(var AStr: PWideChar; IsStar: Boolean): Boolean; override;
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
    function IsVariable: Boolean; override;
    function IsAny: Boolean; override;
{$IFDEF SKREGEXP_DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;
{$ENDIF USE_UNICODE_PROPERTY}

  TREBoundaryCode = class(TRECode)
  private
    FOptions: TREOptions;
    FNegative: Boolean;
    FIsASCII: Boolean;
  protected
    function GetCharLength: TRETextPosRec; override;
  public
    constructor Create(ARegExp: TSkRegExp; AOptions: TREOptions;
      ANegative: Boolean);
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
{$IFDEF SKREGEXP_DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TREReferenceCode = class(TRECode)
  private
    FGroupIndex: Integer;
    FOptions: TREOptions;
    FCompareOptions: TRECompareOptions;
  protected
    function GetCharLength: TRETextPosRec; override;
  public
    constructor Create(ARegExp: TSkRegExp; AGroupIndex: Integer;
      AOptions: TREOptions); overload;
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
    function IsVariable: Boolean; override;
{$IFDEF SKREGEXP_DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TRENamedReferenceCode = class(TRECode)
  private
    FGroupName: REString;
    FGroupIndex: Integer;
    FOptions: TREOptions;
    FCompareOptions: TRECompareOptions;
  protected
    function GetCharLength: TRETextPosRec; override;
  public
    constructor Create(ARegExp: TSkRegExp; AGroupName: REString;
      AGroupIndex: Integer; AOptions: TREOptions);
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
    procedure SetGroupIndex(AGroupIndex: Integer);
    function IsVariable: Boolean; override;
{$IFDEF SKREGEXP_DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TRELineHeadCode = class(TRECode)
  private
    FOptions: TREOptions;
  protected
    function GetCharLength: TRETextPosRec; override;
  public
    constructor Create(ARegExp: TSkRegExp; AOptions: TREOptions);
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
{$IFDEF SKREGEXP_DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TRELineTailCode = class(TRECode)
  private
    FOptions: TREOptions;
  protected
    function GetCharLength: TRETextPosRec; override;
  public
    constructor Create(ARegExp: TSkRegExp; AOptions: TREOptions);
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
{$IFDEF SKREGEXP_DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TRETextHeadCode = class(TRECode)
  protected
    function GetCharLength: TRETextPosRec; override;
  public
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
{$IFDEF SKREGEXP_DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TRETextTailCode = class(TRECode)
  protected
    function GetCharLength: TRETextPosRec; override;
  public
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
{$IFDEF SKREGEXP_DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TRETextEndCode = class(TRECode)
  protected
    function GetCharLength: TRETextPosRec; override;
  public
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
{$IFDEF SKREGEXP_DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TREPosixCharClassCode = class(TRECode)
  private
    FPosixClass: TREPosixClassKind;
    FOptions: TREOptions;
    FCompareOptions: TRECompareOptions;
    FNegative: Boolean;
    FIsASCII: Boolean;
  public
    constructor Create(ARegExp: TSkRegExp; APosixClass: TREPosixClassKind;
      AOptions: TREOptions; ANegative: Boolean);
    function CompareCode(Dest: TRECode): Integer; override;
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
    function IsOverlap(ACode: TRECode): Boolean; override;
{$IFDEF SKREGEXP_DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

{$IFDEF USE_UNICODE_PROPERTY}
  TREPropertyCode = class(TRECode)
  private
    FUniCodeProperty: TUnicodeProperty;
    FNegative: Boolean;
  public
    constructor Create(ARegExp: TSkRegExp; AUnicodeProperty: TUnicodeProperty;
      ANegative: Boolean);
    function CompareCode(Dest: TRECode): Integer; override;
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
    function IsOverlap(ACode: TRECode): Boolean; override;
{$IFDEF SKREGEXP_DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;
{$ENDIF USE_UNICODE_PROPERTY}

  TREGlobalPosCode = class(TRECode)
  protected
    function GetCharLength: TRETextPosRec; override;
  public
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
{$IFDEF SKREGEXP_DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TREIfThenReferenceCode = class(TRECode)
  private
    FGroupIndex: Integer;
  protected
    function GetCharLength: TRETextPosRec; override;
  public
    constructor Create(ARegExp: TSkRegExp; const AGroupIndex: Integer);
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
    function IsVariable: Boolean; override;
{$IFDEF SKREGEXP_DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TREIfThenNamedReferenceCode = class(TRECode)
  private
    FGroupName: REString;
  protected
    function GetCharLength: TRETextPosRec; override;
  public
    constructor Create(ARegExp: TSkRegExp; const AGroupName: REString);
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
    function IsVariable: Boolean; override;
{$IFDEF SKREGEXP_DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TREInSubReferenceCode = class(TRECode)
  private
    FGroupIndex: Integer;
  protected
    function GetCharLength: TRETextPosRec; override;
  public
    constructor Create(ARegExp: TSkRegExp; const AGroupIndex: Integer);
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
    function IsVariable: Boolean; override;
{$IFDEF SKREGEXP_DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TREInSubNameReferenceCode = class(TRECode)
  private
    FGroupName: REString;
  protected
    function GetCharLength: TRETextPosRec; override;
  public
    constructor Create(ARegExp: TSkRegExp; const AGroupName: REString);
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
    function IsVariable: Boolean; override;
{$IFDEF SKREGEXP_DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TRECalloutCode = class(TRECode)
  private
    FData: TCalloutData;
  protected
    function GetCharLength: TRETextPosRec; override;
  public
    constructor Create(ARegExp: TSkRegExp;
      const ACalloutNumber, APatternPosition, APatternLength: Integer);
    function IsEqual(AStr: PWideChar; var Len: Integer): Boolean; override;
    function IsInclude(ACode: TRECode): Boolean; override;
    function IsVariable: Boolean; override;
{$IFDEF SKREGEXP_DEBUG}
    function GetDebugStr: REString; override;
{$ENDIF}
  end;

  TREBinCode = class(TRECode)
  private
    FOp: TREOperator;
    FLeft: TRECode;
    FRight: TRECode;
    FGroupIndex, FMin, FMax: Integer;
    FMatchKind: TRELoopKind;
    FGroupName: REString;
  public
    constructor Create(ARegExp: TSkRegExp; AOp: TREOperator;
      ALeft, ARight: TRECode; AMin: Integer = 0; AMax: Integer = 0); overload;
    property Op: TREOperator read FOp;
    property Left: TRECode read FLeft;
    property Right: TRECode read FRight;
    property Min: Integer read FMin;
    property Max: Integer read FMax;
    property GroupIndex: Integer read FGroupIndex write FGroupIndex;
    property GroupName: REString read FGroupName write FGroupName;
    property MatchKind: TRELoopKind read FMatchKind write FMatchKind;
  end;

  TREContextKind = (ctNormal, ctCharClass, ctNegativeCharClass, ctQuote);

  { トークン先読み用バッファ }
  TRELexRec = record
    FStored: Boolean;
    FToken: TREToken;
    FOptions: TREOptions;
    FNewOptions: TREOptions;
    FP: PWideChar;
    FTokenStartP: PWideChar;
    FTopP: PWideChar;
    FWChar, FStartWChar, FLastWChar: UChar;
    FMin, FMax, FLevel: Integer;
    FContext: TREContextKind;
{$IFDEF USE_UNICODE_PROPERTY}
    FUniCodeProperty: TUnicodeProperty;
{$ENDIF USE_UNICODE_PROPERTY}
    FFold: TUnicodeMultiChar;
    FPosixClass: TREPosixClassKind;
    FNoBackTrack: Boolean;
    FGroupName: REString;
    FOptionList: TList;
    FIsQuote: Boolean;
    FNegativeCharClassFirst: Boolean;
    FLineBreakKind: TRELineBreakKind;
  end;

  { 字句解析を行うクラス }
  TRELex = class
  private
    FRegExp: TSkRegExp;
    FToken: TREToken;
    FP: PWideChar;
    FTokenStartP: PWideChar;
    FTopP, FLastP: PWideChar;
    FWChar, FStartWChar, FLastWChar: UChar;
    FMin, FMax, FLevel: Integer;
    FContext: TREContextKind;
{$IFDEF USE_UNICODE_PROPERTY}
    FUniodeProperty: TUnicodeProperty;
{$ENDIF USE_UNICODE_PROPERTY}
    FFold: TUnicodeMultiChar;
    FPosixClass: TREPosixClassKind;
    FIsQuote: Boolean;
    FGroupName: REString;
    FOptions: TREOptions;
    FNewOptions: TREOptions;
    FOptionList: TList;
    FNegativeCharClassFirst: Boolean;
    FLineBreakKind: TRELineBreakKind;
    FGroupCount: Integer;

    FPrevLex: array [0 .. 2] of TRELexRec;
    FPrevCount: Integer;
    function GetCurrentPosition: Integer;
    function GetTokenStartPosition: Integer;
    function GetTokenLength: Integer;
  protected
    procedure Error(const Msg: REString;
      const Prefix: REString = '...'); overload;
    procedure Error(const Msg: REString; APosition: Integer); overload;
    function GetErrorPositionString(APosition: Integer): REString;
    procedure SkipWhiteSpace;
    procedure LexCharClass;
    procedure LexPosixCharClass;
    procedure LexOption;
    function GetPosixType(const S: REString): TREPosixClassKind;
    function GetControlCode(var Len: Integer): UChar;
    function GetDigit(var Len: Integer): Integer;
    function GetHexDigit(var Len: Integer): UChar;
    function GetOctalDigit(Ch: WideChar; var Len: Integer): UChar;
    procedure LexVerb;
    procedure LexBrace;
    procedure LexCallout;
{$IFDEF USE_UNICODE_PROPERTY}
    procedure LexProperty(const CheckNegative: Boolean);
{$ENDIF USE_UNICODE_PROPERTY}
    procedure LexGroupName(const LastDelimiter: WideChar);
    procedure LexReference(const LastDelimiter: WideChar);
    procedure LexGoSub(const LastDelimiter: WideChar);
    procedure LexESCChar;
    procedure LexLeftPar;
    procedure PushOptions;
    procedure PopOptions;
    procedure UpdateOptions;
    procedure ClearOptionList;
    function GetRECompareOptions: TRECompareOptions;
  public
    constructor Create(ARegExp: TSkRegExp; const Expression: REString);
    destructor Destroy; override;
    function GetCompileErrorPos: Integer;
    procedure CharNext(var P: PWideChar; const Len: Integer = 1);
    procedure CharPrev(var P: PWideChar; const Len: Integer = 1);
    procedure GetToken(Skip: Boolean = False);
    procedure PushToken;
    procedure SaveToken;

    class function CountGroup(const S: REString): Integer;

    property TokenStartPosition: Integer read GetTokenStartPosition;
    property TokenLength: Integer read GetTokenLength;
    property CurrentPosition: Integer read GetCurrentPosition;
    property Token: TREToken read FToken;
    property Min: Integer read FMin;
    property Max: Integer read FMax;
    property Level: Integer read FLevel;
    property Options: TREOptions read FOptions;
    property WChar: UChar read FWChar;
    property StartWChar: UChar read FStartWChar;
    property LastWChar: UChar read FLastWChar;
{$IFDEF USE_UNICODE_PROPERTY}
    property UnicodeProperty: TUnicodeProperty read FUniodeProperty;
{$ENDIF USE_UNICODE_PROPERTY}
    property PosixClass: TREPosixClassKind read FPosixClass;
    property GroupName: REString read FGroupName;
    property LineBreakKind: TRELineBreakKind read  FLineBreakKind;
    property GroupCount: Integer read FGroupCount;
  end;

  // 後方参照のエラー処理用
  TReferenceErrorRec = record
    ErrorPos: Integer;
    AObject: Pointer;
    case IsRelative: Boolean of
      True:
        (RelativeGourpIndex: Integer);
      False:
        (GroupIndex: Integer);
  end;
  PReferenceErrorRec = ^TReferenceErrorRec;

  { 構文解析を行い構文木を作るクラス }
  TREParser = class
  private
    FRegExp: TSkRegExp;
    FLex: TRELex;
    FCurrentGroup: Integer;
    FGroupLevel: Integer;
    FGroupCount: Integer;
    FHasRecursion: Boolean;
    FReferenceErrorList: TREStrings;
    FGoSubErrorList: TREStrings;
    FGroupStack: TStack;
    FJoinMatchGroup: array of Boolean;
    FJoinMatchGroupSize: Integer;
    FHasGoSub: Boolean;
    FHasTailAnchor: Boolean;
    FHasReference: Boolean;
  protected
    function NewBinCode(AOperator: TREOperator; ALeft, ARight: TRECode;
      AMin: Integer = 0; AMax: Integer = 0): TRECode;
    function NewCharClassCode(ANegative: Boolean): TRECode;
    function Term: TRECode;
    function Factor: TRECode;
    function Primay: TRECode;
    function RegExpr: TRECode;
    procedure AddJoinMatchGroup(const Index: Integer);
  public
    constructor Create(ARegExp: TSkRegExp; const Expression: REString);
    destructor Destroy; override;
    procedure Parse;
    property HasGoSub: Boolean read FHasGoSub;
    property HasTailAnchor: Boolean read FHasTailAnchor;
    property HasReference: Boolean read FHasReference;
  end;

  TREOptimizeDataKind = (odkNormal, odkExist, odkLead, odkTail, odkLineHead, odkLineTail,
    odkTextHead, odkTextTail);

  TRELeadCharMode = (lcmNone, lcmFirstLiteral, lcmFirstBranch,
    lcmSimple, lcmSimpleBranch, lcmTextTop, lcmLineTop,
    lcmHasLead, lcmLeadMap, lcmFixedAnchor, lcmVariableAnchor,
    lcmFixedBranch, lcmVariableBranch);

  TREOptimizeData = class
  private
    FCode: TRECode;
    FBranchLevel: Integer;
    FOffset: TRETextPosRec;
    FKind: TREOptimizeDataKind;
  public
{$IFDEF SKREGEXP_DEBUG}
    function DebugOutput: REString;
{$ENDIF SKREGEXP_DEBUG}
    property Code: TRECode read FCode write FCode;
    property Offset: TRETextPosRec read FOffset write FOffset;
    property BranchLevel: Integer read FBranchLevel write FBranchLevel;
    property Kind: TREOptimizeDataKind read FKind write FKind;
  end;

  TREOptimizeDataCollection = class
  private
    FList: TObjectList;
    function GetItem(Index: Integer): TREOptimizeData;
    function GetCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function Add(Value: TRECode; AKind: TREOptimizeDataKind;
      ABranchLevel: Integer; AOffset: TRETextPosRec): Integer;
{$IFDEF SKREGEXP_DEBUG}
    procedure DebugOutput(ADest: TStrings);
{$ENDIF SKREGEXP_DEBUG}
    procedure Delete(Index: Integer);
    procedure GetAnchorCode(ABranchCount: Integer; ADest: TList);
    procedure GetLeadCode(ABranchCount: Integer; ADest: TList);
    procedure GetTailCode(var ACode: TREOptimizeData);
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TREOptimizeData read GetItem; default;
  end;

  TREOptimizeDataList = class(TList)
  private
    IsWork: Boolean;
    function Get(Index: Integer): TREOptimizeData;
    procedure Put(Index: Integer; const Value: TREOptimizeData);
  public
    function Add(Item: TREOptimizeData): Integer;
    function IsEqual(const AStr: PWideChar): Boolean;
    property Items[Index: Integer]: TREOptimizeData read Get write Put; default;
  end;

  { MFA の状態を保持するクラス }
  TRENFAState = class
  private
{$IFDEF SKREGEXP_DEBUG}
    FRegExp: TSkRegExp;
    FIndex: Integer;
{$ENDIF}
    FKind: TRENFAKind;
    FCode: TRECode;
    FTransitTo: Integer;
    FNext: TRENFAState;
    FGroupIndex: Integer;
    FMin: Integer;
    FMax: Integer;
    FMatchKind: TRELoopKind;
    FExtendTo: Integer;
    FLoopIndex: Integer;
    FBranchIndex: Integer;
  public
{$IFDEF SKREGEXP_DEBUG}
    constructor Create(ARegExp: TSkRegExp);
{$ENDIF}
    property Code: TRECode read FCode write FCode;
    property TransitTo: Integer read FTransitTo write FTransitTo;
    property Next: TRENFAState read FNext write FNext;
    property Kind: TRENFAKind read FKind write FKind;
    property GroupIndex: Integer read FGroupIndex write FGroupIndex;
    property MatchKind: TRELoopKind read FMatchKind write FMatchKind;
    property Min: Integer read FMin write FMin;
    property Max: Integer read FMax write FMax;
    property ExtendTo: Integer read FExtendTo write FExtendTo;
    property BranchIndex: Integer read FBranchIndex write FBranchIndex;
    property LoopIndex: Integer read FLoopIndex write FLoopIndex;
{$IFDEF SKREGEXP_DEBUG}
    property Index: Integer read FIndex write FIndex;
    function GetString: REString;
{$ENDIF}
  end;

  TRELoopStateRec = record
    Step: Integer;
  end;
  PRELoopStateRec = ^TRELoopStateRec;

  TRELoopStateItem = class
  private
    FPrevP: PWideChar;
    FState: TList;
    FCurIndex: Integer;
    FNFACode: TRENFAState;
    FMatchP: PWideChar;
    function GetState(Index: Integer): PRELoopStateRec;
    function GetStep: Integer;
    function GetNestLevel: Integer;
    procedure SetNestLevel(const Value: Integer);
    procedure SetStep(const Value: Integer);
  protected
    procedure InternalClear;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Up; inline;
    procedure Push;
    procedure Pop;
    procedure Reset;
    property NestLevel: Integer read GetNestLevel write SetNestLevel;
    property Step: Integer read GetStep write SetStep;
    property PrevP: PWideChar read FPrevP write FPrevP;
    property NFACode: TRENFAState read FNFACode write FNFACode;
    property MatchP: PWideChar read FMatchP write FMatchP;
  end;

  TRELoopStateList = class(TObjectList)
  private
    function GetItem(Index: Integer): TRELoopStateItem;
    procedure SetItem(Index: Integer; const Value: TRELoopStateItem);
  public
    procedure Clear; override;
    procedure Push;
    procedure Pop;
    procedure Reset;
    procedure SetLoopIndex(const Index: Integer);
    function Add(Value: TRELoopStateItem): Integer;
    property Items[Index: Integer]: TRELoopStateItem read GetItem write SetItem; default;
  end;

  TREBranchStateRec = record
    State, Count: Integer;
  end;
  PREBranchStateRec = ^TREBranchStateRec;

  TRENFAOptimizeState = record
    IsNullMatch, IsJoinMatch: Boolean;
  end;

  { NFA を生成するクラス }
  TRENFA = class
  private
    FRegExp: TSkRegExp;
    FParser: TREParser;
    FStateList: TList;
    FBEntryState, FBExitState: Integer;
    FEntryStack, FExitStack: TList;
    FEntryStackIndex, FExitStateIndex: Integer;
    FStateStack: TList;
    FStateStackIndex: Integer;
    FGroupCount: Integer;
    FBranchStack: TList;
    FBranchIndex: Integer;
    FOptimizeData: TREOptimizeDataCollection;
    FHasAccept: Boolean;
    FInBranch: Boolean;
  protected
    function GetNumber: Integer;
    function AddTransition(AKind: TRENFAKind; ATransFrom, ATransTo: Integer;
      ACode: TRECode; AGroupIndex: Integer; ABranchIndex: Integer;
      AMin: Integer = 0; AMax: Integer = 0): TRENFAState;
    procedure CalculateGroupLength(ACode: TRECode; var AMatchLen: TRETextPosRec;
      IsNullMatch, IsJoinMatch: Boolean);
    procedure GenerateStateList(ACode: TRECode; AEntry, AWayout: Integer;
      var ABranchLevel: Integer; var AMatchLen: TRETextPosRec;
      var AOffset: TRETextPosRec; AGroupIndex: Integer;
      AState: TRENFAOptimizeState);
    procedure PushState(AEntry, AWayout, ANewEntry, ANewWayout: Integer);
    procedure PopState;
  public
    constructor Create(ARegExp: TSkRegExp);
    destructor Destroy; override;
    procedure Compile(AParser: TREParser);
  end;

  TRESubExpression = class
  private
    FStartP: PWideChar;
    FEndP: PWideChar;
  public
    property StartP: PWideChar read FStartP write FStartP;
    property EndP: PWideChar read FEndP write FEndP;
  end;

  TRECapture = class
  private
    FRegExp: TSkRegExp;
    FStartP, FStartPBuf: PWideChar;
    FMatched: Boolean;
    FEndP: PWideChar;
    procedure SetEndP(const Value: PWideChar); inline;
    procedure SetStartP(const Value: PWideChar); inline;
    function GetIndex: Integer;
    function GetLength: Integer;
    function GetStrings: REString;
    function GetSuccess: Boolean;
  protected
    property StartP: PWideChar read FStartP write SetStartP;
    property EndP: PWideChar read FEndP write SetEndP;
    property Matched: Boolean read FMatched write FMatched;
  public
    constructor Create(ARegExp: TSkRegExp);
    procedure Assign(AOjbect: TObject);
    procedure Clear;
    property Strings: REString read GetStrings;
    property Index: Integer read GetIndex;
    property Length: Integer read GetLength;
    property Success: Boolean read GetSuccess;
  end;

  TRECaptureCollection = class
  private
    FRegExp: TSkRegExp;
    FItems: TObjectList;
    FCurIndex: Integer;
    function GetItem(Index: Integer): TRECapture; inline;
    function GetEndP: PWideChar; inline;
    function GetMatched: Boolean; inline;
    function GetStartP: PWideChar; inline;
    procedure SetEndP(const Value: PWideChar); inline;
    procedure SetStartP(const Value: PWideChar); inline;
    procedure SetMatched(const Value: Boolean); inline;
    function GetCount: Integer; inline;
    procedure SetCurIndex(const Value: Integer); inline;
    function GetLength: Integer; inline;
    function GetStrings: REString; inline;
    function GetSuccess: Boolean; inline;
    function GetIndex: Integer; inline;
    function GetStartPBuf: PWideChar; inline;
    procedure SetStartPBuf(const Value: PWideChar); inline;
  protected
    procedure SetData(ACapture: TRECapture);
    property StartP: PWideChar read GetStartP write SetStartP;
    property EndP: PWideChar read GetEndP write SetEndP;
    property StartPBuf: PWideChar read GetStartPBuf write SetStartPBuf;

    property Matched: Boolean read GetMatched write SetMatched;
  public
    constructor Create(ARegExp: TSkRegExp);
    destructor Destroy; override;
    function Add(ACapture: TRECapture): Integer;
    procedure Clear;
    procedure Push;
    procedure Pop;
    property Count: Integer read GetCount;
    property CurrentIndex: Integer read FCurIndex write SetCurIndex;
    property Items[Index: Integer]: TRECapture read GetItem; default;
    property Strings: REString read GetStrings;
    property Index: Integer read GetIndex;
    property Length: Integer read GetLength;
    property Success: Boolean read GetSuccess;
  end;

  { マッチ結果を保持するクラス }
  TGroup = class
  private
    FRegExp: TSkRegExp;
    FGroupName: REString;
    FGroupBegin, FGroupEnd: TRENFAState;
    FSubExp: PWideChar;
    FSameGroup: Integer;
    FJoinMatch: Boolean;
    FCharLength: TRETextPosRec;
    FStartP: PWideChar;
    FStartPBuf: PWideChar;
    FEndP: PWideChar;
    FSuccess: Boolean;
    function GetIndex: Integer;
    function GetLength: Integer;
    function GetStrings: REString;
    function GetSubExpression: REString;
    procedure SetEndP(const Value: PWideChar);
    procedure SetStartP(const Value: PWideChar);
  protected
    procedure Clear;
    property GroupBegin: TRENFAState read FGroupBegin write FGroupBegin;
    property GroupEnd: TRENFAState read FGroupEnd write FGroupEnd;
    property SameGroup: Integer read FSameGroup write FSameGroup;

    property StartP: PWideChar read FStartP write SetStartP;
    property EndP: PWideChar read FEndP write SetEndP;
    property StartPBuf: PWideChar read FStartPBuf;
  public
    constructor Create(ARegExp: TSkRegExp);
    procedure Assign(Source: TGroup);
    procedure Reset;
    property GroupName: REString read FGroupName write FGroupName;
    property Strings: REString read GetStrings;
    property Index: Integer read GetIndex;
    property Length: Integer read GetLength;
    property Success: Boolean read FSuccess;
    property SubExpression: REString read GetSubExpression;
    property JoinMatch: Boolean read FJoinMatch;
    property CharLength: TRETextPosRec read FCharLength write FCharLength;
  end;

  TGroupCollectionEnumerator = class
  private
    FIndex: Integer;
    FList: TObjectList;
  public
    constructor Create(AList: TObjectList);
    function GetCurrent: TGroup;
    function MoveNext: Boolean;
    property Current: TGroup read GetCurrent;
  end;

  TREHashItem = record
    Next: Pointer;
    Key: REString;
    Value: Integer;
  end;

  PREHashItem = ^TREHashItem;
  TREHashArray = array [0..CONST_GroupNameHashMax] of PREHashItem;

  TIntDynArray = array of Integer;
  PIntDynArray = ^TIntDynArray;

  { すべてのマッチ結果を保持するクラス }
  TGroupCollection = class
  private
    FRegExp: TSkRegExp;
    FItems: TObjectList;
    FBuckets: TREHashArray;
    function GetItems(Index: Integer): TGroup;
    function GetNames(AName: REString): TGroup;
    function GetCount: Integer; inline;
  protected
    function Add(const AGroupName: REString;
      AEntry, AWayout: TRENFAState): Integer;
    procedure Clear;
    function HashOf(const Key: REString): Cardinal;
    function IsDuplicateGroupName(const AGroupName: REString): Boolean;
    procedure Reset;
    procedure CheckSameGroupName;
  public
    constructor Create(ARegExp: TSkRegExp);
    destructor Destroy; override;
    procedure AddGroupName(const AGroupName: REString; Index: Integer);
    procedure Assign(Source: TGroupCollection);
    function EnumIndexOfName(const AGroupName: REString): TIntDynArray;
    function GetEnumerator: TGroupCollectionEnumerator;
    function IndexOfName(const AGroupName: REString): Integer;
    function IndexOfMatchedName(const AGroupName: REString): Integer;
    function NameExists(const AGroupName: REString): Boolean; inline;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TGroup read GetItems; default;
    property Names[AName: REString]: TGroup read GetNames;
  end;

  TREGroupStack = class
  private
    FRegExp: TSkRegExp;
    FCurIndex: Integer;
    FItems: TObjectList;
  public
    constructor Create(ARegExp: TSkRegExp);
    destructor Destroy; override;
    procedure Clear;
    procedure Push(AGroups: TGroupCollection);
    procedure Pop(var AGroups: TGroupCollection); overload;
    procedure Pop(var AGroups: TGroupCollection; const Index: Integer); overload;
{$IFDEF SKREGEXP_DEBUG}
    function GetDebugStr: REString;
{$ENDIF SKREGEXP_DEBUG}
  end;

  TREMatchExplosionStateRec = record
    NFACode: TRENFAState;
    Next: Pointer;
  end;
  PREMatchExplosionStateRec = ^TREMatchExplosionStateRec;

  TREBackTrackStateRec = record
    NFACode: TRENFAState;
    Str: PWideChar;
    NestLevel: Integer;
  end;
  PREBackTrackStateRec = ^TREBackTrackStateRec;

{$IFDEF CONDITIONALEXPRESSIONS}
{$IF CompilerVersion >= 16.0}
  PPointerList = ^TPointerList;
  TPointerList = array [0 .. MaxListSize - 1] of Pointer;
{$IFEND}
{$ENDIF}

  { バックトラック用のステートを保存するクラス }
  TREBackTrackStack = class
  private
    FRegExp: TSkRegExp;
    FCount, FSize: Integer;
    FStat, FGroup: PPointerList;
    FCheckMatchExplosion: Boolean;
    function GetIndex: Integer; inline;
  protected
    procedure Extend(ASize: Integer);
  public
    constructor Create(ARegExp: TSkRegExp;
      ACheckMatchExplosion: Boolean = True);
    destructor Destroy; override;
    procedure Clear;
    function Count: Integer; inline;
    function Peek: TRENFAState;
    procedure Push(NFACode: TRENFAState; AStr: PWideChar; IsPushGroup: Boolean);
    procedure Pop(var NFACode: TRENFAState; var AStr: PWideChar); overload;
    procedure Pop; overload;
    procedure Remove(const AIndex: Integer); overload;
    procedure Remove(BranchCode: TRENFAState); overload;
    procedure RemoveGoSub(const AGoSubIndex: Integer);
    property Index: Integer read GetIndex;
  end;

  TREGoSubStateRec = record
    Index: Integer;
    EndCode, NextCode: TRENFAState;
    PrevP: PWideChar;
  end;
  PREGoSubStateRec = ^TREGoSubStateRec;

  { 再帰パターン用のスタック }
  TREGoSubStack = class
  private
    FRegExp: TSkRegExp;
    FCurIndex, FSize: Integer;
    FState: TList;
    function GetState(Index: Integer): PREGoSubStateRec; inline;
    function GetGroupIndex: Integer; inline;
    function GetIndex: Integer; inline;
    procedure SetIndex(const Value: Integer); inline;
  public
    constructor Create(ARegExp: TSkRegExp);
    destructor Destroy; override;
    procedure Clear;
    function Count: Integer; inline;
    function Peek: PREGoSubStateRec;
    procedure Push(AGroupIndex: Integer; EndCode, NextCode: TRENFAState);
    procedure Pop;
    property Index: Integer read GetIndex write SetIndex;
    property GroupIndex: Integer read GetGroupIndex;
    property State[Index: Integer]: PREGoSubStateRec read GetState; default;
  end;

  TREIsLeadMatchMethod = function(AStr: PWideChar): Boolean of object;

  { 従来型NFAバックトラック照合エンジンクラス
    NFAと言っても状態機械ではなく、NFA状態をバイトコードとみなして処理している。 }
  TREMatchEngine = class
  private
    FRegExp: TSkRegExp;
    FBackTrackStack: TREBackTrackStack;
    FGroups: TGroupCollection;
    FLoopState: TRELoopStateList;
    FStateList: TList;
    FLeadCode: TREOptimizeDataList;
    FOptimizeData: TREOptimizeDataCollection;
    FLeadStrings: TRECode;
    FAnchorStrings: TRECode;
    FLeadCharMode: TRELeadCharMode;
    FLeadMap: TRECharMap;
    FACSearch: TREACSearch;
    FLeadCharOffset: TRETextPosRec;
    FAnchorOffset: TRETextPosRec;
    FSkipP: PWideChar;
    FSkipIndex: Integer;
    FHasSkip: Boolean;
  protected
    IsLeadMatch: TREIsLeadMatchMethod;
    function IsLeadCode(AStr: PWideChar): Boolean; inline;
    function IsLeadMap(AStr: PWideChar): Boolean; inline;
    function IsLeadAllMatch(AStr: PWideChar): Boolean; inline;
    function MatchAhead(var NFACode: TRENFAState; var AStr: PWideChar): Boolean; overload;
    function MatchCore(var NFACode: TRENFAState; Stack: TREBackTrackStack;
      var AStr: PWideChar): Boolean; overload;
    function MatchCore(var NFACode, EndCode: TRENFAState;
      var AStr: PWideChar): Boolean; overload;
    function MatchCore(var NFACode, EndCode: TRENFAState; Stack: TREBackTrackStack;
      var AStr: PWideChar): Boolean; overload;
    function MatchPrim(NFACode: TRENFAState; Stack: TREBackTrackStack;
      var AStr: PWideChar): TRENFAState;

    procedure OptimizeLoop;

    function MatchEntry(AStr: PWideChar): Boolean;

    procedure CreateLeadMap(NFACode: TRENFAState; var NoMap: Boolean);
    procedure SetupLeadStrings;
    procedure SetupPreMatchStrings;
    procedure SetupLeadMatch(ALeadCharMode: TRELeadCharMode);
{$IFDEF SKREGEXP_DEBUG}
    procedure MatchProcessAdd(NFACode: TRENFAState; AStr: PWideChar;
      Level: Integer);
{$ENDIF}
  public
    constructor Create(ARegExp: TSkRegExp);
    destructor Destroy; override;
    function Match(AStr: PWideChar): Boolean;
    procedure Optimize;
  end;

  TREIsLineBreakMethod = function(P: PWideChar): Integer of object;

  TSkRegExpReplaceFunction = function(ARegExp: TSkRegExp): REString of object;
  TSkRegExpReplaceEvent = procedure(Sender: TObject; var ReplaceWith: REString)
    of object;
  TCalloutEvent = procedure(ARegExp: TSkRegExp; AData: PCalloutData;
    var IsMatch: Boolean; var MatchLength: Integer) of object;

  TSkRegExp = class
  private
    FMatchEngine: TREMatchEngine;
    FCode: TRECode;
    FCodeList: TList;
    FBinCodeList: TList;
    FCompiled: Boolean;
    FTextTopP, FTextEndP: PWideChar;
    FMatchTopP, FMatchEndP, FMatchStartP: PWideChar;
    FGroups: TGroupCollection;
    FVerbNames: TREStrings;

    FGroupStack: TREGroupStack;
    FSubStack: TREGoSubStack;
    FOptimizeData: TREOptimizeDataCollection;
    FLoopState: TRELoopStateList;
    FBranchCount: Integer;
    FMinMatchLength: Integer;
    FMaxMatchLength: Integer;

    FInputString: REString;
    FExpression: REString;
    FOptions: TREOptions;

{$IFDEF CHECK_MATCH_EXPLOSION}
    FMatchExplosionState: array of PREMatchExplosionStateRec;
{$ENDIF}
    //
    FStateList: TList;
    FEntryState, FExitState: Integer;

    FHasReference: Boolean;
    FOnMatch: TNotifyEvent;

    FGlobalStartP, FGlobalEndP: PWideChar;
    FModified: Boolean;
    FSuccess: Boolean;

    FOnCallout: TCalloutEvent;
    FReplaceFunc: TSkRegExpReplaceFunction;
    FOnReplace: TSkRegExpReplaceEvent;
    FMatchOffset: Integer;
    FMatchLength: Integer;
    FStartMatch: Integer;
    FLastRegMarkIndex: Integer;
    FLastRegErrorIndex: Integer;

    FLineBreakKind: TRELineBreakKind;

{$IFDEF SKREGEXP_DEBUG}
    FMatchProcess: TREStrings;
    FShowMatchProcess: Boolean;
    FShowBackTrackStack: Boolean;
    FShowCaptureHistory: Boolean;
{$ENDIF}

    procedure SetExpression(const Value: REString);
    procedure SetInputString(const Value: REString);
    function GetGroupCount: Integer;
    function GetVersion: REString;
    function GetOptions(const Index: Integer): Boolean;
    procedure SetOptions(const Index: Integer; const Value: Boolean);
{$IFDEF JapaneseExt}
    procedure SetIgnoreZenHan(const Value: Boolean);
    function GetIgnoreZenHan: Boolean;
{$ENDIF}
    function GetDefineCharClassLegacy: Boolean;
    procedure SetDefineCharClassLegacy(const Value: Boolean);
    function GetGroupNameFromIndex(Index: Integer): REString;
    function GetIndexFromGroupName(Name: REString): Integer;
    function GetRegMark: REString;
    function GetRegError: REString;
    function GetIndex: Integer; inline;
    function GetLength: Integer; inline;
    function GetStrings: REString; inline;
    procedure SetLineBreakKind(const Value: TRELineBreakKind);
  protected
    procedure ClearCodeList;
    { FBinCodeListをクリアする。
      FBinCodeListはTREBinCodeのリスト。
      NFAを生成した後は不要なので、生成後呼び出してクリアする。 }
    procedure ClearBinCodeList;
    procedure ClearStateList;
{$IFDEF CHECK_MATCH_EXPLOSION}
    procedure ClearMatchExplosionState;
{$ENDIF}
    function IsCR(P: PWideChar): Integer;
    function IsLF(P: PWideChar): Integer;
    function IsCRLF(P: PWideChar): Integer;
    function IsAnyCRLF(P: PWideChar): Integer;
    function IsAnyEOL(P: PWideChar): Integer;

{$IFDEF CHECK_MATCH_EXPLOSION}
    function IsAlreadyTried(NFACode: TRENFAState; const AStr: PWideChar): Boolean;
{$ENDIF CHECK_MATCH_EXPLOSION}

    function MatchCore(AStr: PWideChar): Boolean;

    procedure DoReplaceFunc(Sender: TObject; var ReplaceWith: REString);
  public
    IsLineBreak: TREIsLineBreakMethod;
//
    constructor Create;
    destructor Destroy; override;

    { 正規表現を構文解析し、NFAを生成する }
    procedure Compile;

    procedure Error(const ErrorMes: REString);

    { 最初のマッチを実行する }
    function Exec(const AInputStr: REString): Boolean;
    { 次のマッチを実行する }
    function ExecNext: Boolean;
    { AOffsetの位置から AMaxLenght の範囲でマッチを実行する。AMaxLengthを指定した場合の動作に注意。ヘルプ参照 }
    function ExecPos(AOffset: Integer = 1; AMaxLength: Integer = 0): Boolean;

    function Substitute(const ATemplate: REString): REString;

    function Replace(const Input, Replacement: REString; Count: Integer = 0;
      AOffset: Integer = 1): REString; overload;
    function Replace(const Input: REString;
      AReplaceFunc: TSkRegExpReplaceFunction; Count: Integer = 0;
      AOffset: Integer = 1): REString; overload;

    class function RegReplace(const ARegExpStr, AInputStr, AReplaceStr: REString;
      AOptions: TREOptions = []): REString; overload;
    class function RegReplace(const ARegExpStr, AInputStr: REString;
      AReplaceFunc: TSkRegExpReplaceFunction; AOptions: TREOptions = []): REString; overload;

    procedure Split(const Input: REString; APieces: TREStrings;
      Count: Integer = 0; AOffset: Integer = 1); overload;

    class function RegIsMatch(const ARegExpStr, AInputStr: REString;
      AOptions: TREOptions = []): Boolean;
    class function RegMatch(const ARegExpStr, AInputStr: REString;
      AMatches: TREStrings; AOptions: TREOptions = []): Boolean;
    class procedure RegSplit(const ARegExpStr, AInputStr: REString;
      APieces: TREStrings; AOptions: TREOptions = []); overload;

    class function DecodeEscape(const S: REString): REString; overload;
    class function EncodeEscape(const Str: REString): REString;
    class function EscapeRegExChars(const S: REString): REString;

{$IFDEF SKREGEXP_DEBUG}
//    procedure DumpParse(ADest: TStrings);
    procedure DumpNFA(ADest: TStrings);
    function DumpLeadCode: REString;
    function DumpMatchProcess: REString;
    procedure DebugOutput(ADest: TStrings);
    procedure SKREGEXP_DEBUGPrint(const S: REString);
{$ENDIF}
    { 正規表現の文字列 }
    property Expression: REString read FExpression write SetExpression;
    { グループの数を返す。グループは 0 から GroupCount まで。 }
    property GroupCount: Integer read GetGroupCount;
    { 検索対象の文字列 }
    property InputString: REString read FInputString write SetInputString;

    // 正規表現オプション
    property Options: TREOptions read FOptions write FOptions;
    property IgnoreCase: Boolean index 0 read GetOptions write SetOptions;
    property MultiLine: Boolean index 1 read GetOptions write SetOptions;
    property NamedGroupOnly: Boolean index 2 read GetOptions write SetOptions;
    property SingleLine: Boolean index 3 read GetOptions write SetOptions;
    property Extended: Boolean index 4 read GetOptions write SetOptions;
{$IFDEF JapaneseExt}
    property IgnoreWidth: Boolean index 5 read GetOptions write SetOptions;
    property IgnoreKana: Boolean index 6 read GetOptions write SetOptions;
    property IgnoreZenHan: Boolean read GetIgnoreZenHan write SetIgnoreZenHan;
{$ENDIF}
    property AutoCallout: Boolean index 7 read GetOptions write SetOptions;
    property DefinedCharClassLegacy: Boolean read GetDefineCharClassLegacy
      write SetDefineCharClassLegacy;

    property LineBreakKind: TRELineBreakKind read FLineBreakKind write SetLineBreakKind;
    property Groups: TGroupCollection read FGroups;

    property Strings: REString read GetStrings;
    property Index: Integer read GetIndex;
    property Length: Integer read GetLength;

    property GroupNameFromIndex[Index: Integer]: REString
      read GetGroupNameFromIndex;
    property IndexFromGroupName[Name: REString]: Integer
      read GetIndexFromGroupName;

    property Version: REString read GetVersion;

    // マッチが成功したら True
    property Success: Boolean read FSuccess;

    property MatchOffset: Integer read FMatchOffset;
    property MatchLength: Integer read FMatchLength;

    property RegMark: REString read GetRegMark;
    property RegError: REString read GetRegError;

    // Event
    property OnCallout: TCalloutEvent read FOnCallout write FOnCallout;
    property OnMatch: TNotifyEvent read FOnMatch write FOnMatch;
    property OnReplace: TSkRegExpReplaceEvent read FOnReplace write FOnReplace;
{$IFDEF SKREGEXP_DEBUG}
    property ShowMatchProcess: Boolean read FShowMatchProcess
      write FShowMatchProcess;
    property ShowBackTrackStack: Boolean read FShowBackTrackStack
      write FShowBackTrackStack;
    property ShowCaptureHistory: Boolean read FShowCaptureHistory
      write FShowCaptureHistory;
    property MinMatchLength: Integer read FMinMatchLength;
    property MaxMatchLength: Integer read FMaxMatchLength;
{$ENDIF SKREGEXP_DEBUG}
  end;

function RegIsMatch(const ARegExpStr, AInputStr: REString;
  AOptions: TREOptions = []): Boolean; inline;
function RegMatch(const ARegExpStr, AInputStr: REString; AMatches: TREStrings;
  AOptions: TREOptions = []): Boolean; inline;
function RegReplace(const ARegExpStr, AInputStr, AReplaceStr: REString;
  AOptions: TREOptions = []): REString; inline; overload;
function RegReplace(const ARegExpStr, AInputStr: REString;
  AReplaceFunc: TSkRegExpReplaceFunction; AOptions: TREOptions = [])
  : REString;  inline; overload;
procedure RegSplit(const ARegExpStr, AInputStr: REString; APieces: TREStrings;
  AOptions: TREOptions = []); inline;

function DecodeEscape(const S: REString): REString; inline;
function EncodeEscape(const Str: REString): REString; inline;

{$IFDEF SKREGEXP_DEBUG}
function REStrLJComp(AStr, ASubStr: PWideChar; ALen: Integer;
  var MatchLen: Integer; AOptions: TRECompareOptions): Integer;
function RECompareString(SourceP: PWideChar; DestP: PWideChar; DestLen: Integer;
  var MatchLen: Integer; Options: TRECompareOptions): Integer;
function REStrPos(AStr: PWideChar; ALen: Integer; APattern: PWideChar;
  APatternLen: Integer; var MatchLen: Integer; AOptions: TRECompareOptions): PWideChar;
function REStrLComp(const Str1, Str2: PWideChar; MaxLen: Cardinal): Integer;
function REStrLIComp(const Str1, Str2: PWideChar; MaxLen: Cardinal;
  var MatchLen: Integer): Integer;
{$IFDEF UNICODE}
function REStrLIAComp(const Str1, Str2: PWideChar; MaxLen:Cardinal): Integer; inline;
{$ELSE UNICODE}
function REStrLIAComp(const Str1, Str2: PWideChar; MaxLen:Cardinal): Integer;
{$ENDIF UNICODE}
function ToUChar(AStr: PWideChar): UChar; inline; overload;
function ToUChar(const S: REString; const Index: Integer): UChar; inline; overload;
function UCharToString(AWChar: UChar): REString; inline;
function ToFoldCase(const S: REString; IsASCII: Boolean): REString;
{$IFDEF JapaneseExt}
function ToHalf(const S: REString): REString;
function ToWide(const S: REString): REString;
function ToHiragana(const S: REString): REString;
function ToKatakana(const S: REString): REString;
{$ENDIF JapaneseExt}
{$ENDIF SKREGEXP_DEBUG}

var
  SkRegExpDefaultOptions: TREOptions = [];
  SkRegExpDefaultLineBreakind: TRELineBreakKind = lbAnyCRLF;

implementation

const
  CONST_VERSION = '3.1.0';
  CONST_LoopMax = $7FFF;
  CONST_BackTrack_Stack_Default_Size = 128;
  CONST_Recursion_Stack_Default_Size = 16;
  CONST_CAPTURE_DEFAULT_SIZE = 20;
  CONST_Group_Stack_Default_Size = 16;

  CONST_Dakuten = #$FF9E;
  CONST_Handakuten = #$FF9F;
  CONST_Wide_Dakuten = #$309B;
  CONST_Wide_Handakuten = #$309C;
  CONST_Wide_Dakuten_CS = #$3099;
  CONST_Wide_Handakuten_CS = #$309A;

  CONST_AlnumAMap: array[0..15] of Byte = (
    $00, $00, $00, $00, $00, $00, $FF, $03, $FE, $FF, $FF, $07, $FE, $FF, $FF, $07
  );
  CONST_AlphaAMap: array[0..15] of Byte = (
    $00, $00, $00, $00, $00, $00, $00, $00, $FE, $FF, $FF, $07, $FE, $FF, $FF, $07
  );
  CONST_BlankAMap: array[0..15] of Byte = (
    $00, $02, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  );
  CONST_CntrlAMap: array[0..15] of Byte = (
    $FF, $FF, $FF, $FF, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $80
  );
  CONST_SpaceAMap: array[0..15] of Byte = (
    $00, $3E, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  );
  CONST_SpacePerlAMap: array[0..15] of Byte = (
    $00, $36, $00, $00, $01, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00
  );
  CONST_GraphAMap: array[0..15] of Byte = (
    $00, $00, $00, $00, $FE, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $7F
  );
  CONST_LowerAMap: array[0..15] of Byte = (
    $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $00, $FE, $FF, $FF, $07
  );
  CONST_PrintAMap: array[0..15] of Byte = (
    $00, $00, $00, $00, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $FF, $7F
  );
  CONST_PunctAMap: array[0..15] of Byte = (
    $00, $00, $00, $00, $FE, $FF, $00, $FC, $01, $00, $00, $F8, $01, $00, $00, $78
  );
  CONST_UpperAMap: array[0..15] of Byte = (
    $00, $00, $00, $00, $00, $00, $00, $00, $FE, $FF, $FF, $07, $00, $00, $00, $00
  );
  CONST_XDigitAMap: array[0..15] of Byte = (
    $00, $00, $00, $00, $00, $00, $FF, $03, $7E, $00, $00, $00, $7E, $00, $00, $00
  );
  CONST_WordAMap: array[0..15] of Byte = (
    $00, $00, $00, $00, $00, $00, $FF, $03, $FE, $FF, $FF, $87, $FE, $FF, $FF, $07
  );
  CONST_DigitAMap: array[0..15] of Byte = (
    $00, $00, $00, $00, $00, $00, $FF, $03, $00, $00, $00, $00, $00, $00, $00, $00
  );

{$IFDEF JapaneseExt}
  HalfToWideAnkTable: array[$0020 .. $007E] of UChar = (
    $3000, $FF01, $FF02, $FF03, $FF04, $FF05, $FF06, $FF07, $FF08, $FF09,
    $FF0A, $FF0B, $FF0C, $FF0D, $FF0E, $FF0F, $FF10, $FF11, $FF12, $FF13,
    $FF14, $FF15, $FF16, $FF17, $FF18, $FF19, $FF1A, $FF1B, $FF1C, $FF1D,
    $FF1E, $FF1F, $FF20, $FF21, $FF22, $FF23, $FF24, $FF25, $FF26, $FF27,
    $FF28, $FF29, $FF2A, $FF2B, $FF2C, $FF2D, $FF2E, $FF2F, $FF30, $FF31,
    $FF32, $FF33, $FF34, $FF35, $FF36, $FF37, $FF38, $FF39, $FF3A, $FF3B,
    $005C, $FF3D, $FF3E, $FF3F, $FF40, $FF41, $FF42, $FF43, $FF44, $FF45,
    $FF46, $FF47, $FF48, $FF49, $FF4A, $FF4B, $FF4C, $FF4D, $FF4E, $FF4F,
    $FF50, $FF51, $FF52, $FF53, $FF54, $FF55, $FF56, $FF57, $FF58, $FF59,
    $FF5A, $FF5B, $FF5C, $FF5D, $FF5E
  );

  WideToHalfAnkTable: array[$FF01 .. $FF5E] of UChar = (
    $0021, $0022, $0023, $0024, $0025, $0026, $0027, $0028, $0029, $002A,
    $002B, $002C, $002D, $002E, $002F, $0030, $0031, $0032, $0033, $0034,
    $0035, $0036, $0037, $0038, $0039, $003A, $003B, $003C, $003D, $003E,
    $003F, $0040, $0041, $0042, $0043, $0044, $0045, $0046, $0047, $0048,
    $0049, $004A, $004B, $004C, $004D, $004E, $004F, $0050, $0051, $0052,
    $0053, $0054, $0055, $0056, $0057, $0058, $0059, $005A, $005B, $FF3C,
    $005D, $005E, $005F, $0060, $0061, $0062, $0063, $0064, $0065, $0066,
    $0067, $0068, $0069, $006A, $006B, $006C, $006D, $006E, $006F, $0070,
    $0071, $0072, $0073, $0074, $0075, $0076, $0077, $0078, $0079, $007A,
    $007B, $007C, $007D, $007E
  );

  HalfToWideKatakanaTable: array[$FF61 .. $FF9F, 0..2] of UChar = (
    ($3002, $3002, $3002), ($300C, $300C, $300C), ($300D, $300D, $300D),
    ($3001, $3001, $3001), ($30FB, $30FB, $30FB), ($30F2, $30FA, $30F2),
    ($30A1, $30A1, $30A1), ($30A3, $30A3, $30A3), ($30A5, $30A5, $30A5),
    ($30A7, $30A7, $30A7), ($30A9, $30A9, $30A9), ($30E3, $30E3, $30E3),
    ($30E5, $30E5, $30E5), ($30E7, $30E7, $30E7), ($30C3, $30C3, $30C3),
    ($30FC, $30FC, $30FC), ($30A2, $30A2, $30A2), ($30A4, $30A4, $30A4),
    ($30A6, $30F4, $30A6), ($30A8, $30A8, $30A8), ($30AA, $30AA, $30AA),
    ($30AB, $30AC, $30AB), ($30AD, $30AE, $30AD), ($30AF, $30B0, $30AF),
    ($30B1, $30B2, $30B1), ($30B3, $30B4, $30B3), ($30B5, $30B6, $30B5),
    ($30B7, $30B8, $30B7), ($30B9, $30BA, $30B9), ($30BB, $30BC, $30BB),
    ($30BD, $30BE, $30BD), ($30BF, $30C0, $30BF), ($30C1, $30C2, $30C1),
    ($30C4, $30C5, $30C4), ($30C6, $30C7, $30C6), ($30C8, $30C9, $30C8),
    ($30CA, $30CA, $30CA), ($30CB, $30CB, $30CB), ($30CC, $30CC, $30CC),
    ($30CD, $30CD, $30CD), ($30CE, $30CE, $30CE), ($30CF, $30D0, $30D1),
    ($30D2, $30D3, $30D4), ($30D5, $30D6, $30D7), ($30D8, $30D9, $30DA),
    ($30DB, $30DC, $30DD), ($30DE, $30DE, $30DE), ($30DF, $30DF, $30DF),
    ($30E0, $30E0, $30E0), ($30E1, $30E1, $30E1), ($30E2, $30E2, $30E2),
    ($30E4, $30E4, $30E4), ($30E6, $30E6, $30E6), ($30E8, $30E8, $30E8),
    ($30E9, $30E9, $30E9), ($30EA, $30EA, $30EA), ($30EB, $30EB, $30EB),
    ($30EC, $30EC, $30EC), ($30ED, $30ED, $30ED), ($30EF, $30F7, $30EF),
    ($30F3, $30F3, $30F3), ($3099, $3099, $3099), ($309A, $309A, $309A)
  );

  WideToHalfKatakanaTable: array[$3000 .. $30FF, 0..1] of UChar = (
    ($0020, $0000), ($FF64, $0000), ($FF61, $0000), ($3003, $0000),
    ($3004, $0000), ($3005, $0000), ($3006, $0000), ($3007, $0000),
    ($3008, $0000), ($3009, $0000), ($300A, $0000), ($300B, $0000),
    ($FF62, $0000), ($FF63, $0000), ($300E, $0000), ($300F, $0000),
    ($3010, $0000), ($3011, $0000), ($3012, $0000), ($3013, $0000),
    ($3014, $0000), ($3015, $0000), ($3016, $0000), ($3017, $0000),
    ($3018, $0000), ($3019, $0000), ($301A, $0000), ($301B, $0000),
    ($301C, $0000), ($301D, $0000), ($301E, $0000), ($301F, $0000),
    ($3020, $0000), ($3021, $0000), ($3022, $0000), ($3023, $0000),
    ($3024, $0000), ($3025, $0000), ($3026, $0000), ($3027, $0000),
    ($3028, $0000), ($3029, $0000), ($302A, $0000), ($302B, $0000),
    ($302C, $0000), ($302D, $0000), ($302E, $0000), ($302F, $0000),
    ($3030, $0000), ($3031, $0000), ($3032, $0000), ($3033, $0000),
    ($3034, $0000), ($3035, $0000), ($3036, $0000), ($3037, $0000),
    ($3038, $0000), ($3039, $0000), ($303A, $0000), ($303B, $0000),
    ($303C, $0000), ($303D, $0000), ($303E, $0000), ($303F, $0000),
    ($3040, $0000), ($3041, $0000), ($3042, $0000), ($3043, $0000),
    ($3044, $0000), ($3045, $0000), ($3046, $0000), ($3047, $0000),
    ($3048, $0000), ($3049, $0000), ($304A, $0000), ($304B, $0000),
    ($304C, $0000), ($304D, $0000), ($304E, $0000), ($304F, $0000),
    ($3050, $0000), ($3051, $0000), ($3052, $0000), ($3053, $0000),
    ($3054, $0000), ($3055, $0000), ($3056, $0000), ($3057, $0000),
    ($3058, $0000), ($3059, $0000), ($305A, $0000), ($305B, $0000),
    ($305C, $0000), ($305D, $0000), ($305E, $0000), ($305F, $0000),
    ($3060, $0000), ($3061, $0000), ($3062, $0000), ($3063, $0000),
    ($3064, $0000), ($3065, $0000), ($3066, $0000), ($3067, $0000),
    ($3068, $0000), ($3069, $0000), ($306A, $0000), ($306B, $0000),
    ($306C, $0000), ($306D, $0000), ($306E, $0000), ($306F, $0000),
    ($3070, $0000), ($3071, $0000), ($3072, $0000), ($3073, $0000),
    ($3074, $0000), ($3075, $0000), ($3076, $0000), ($3077, $0000),
    ($3078, $0000), ($3079, $0000), ($307A, $0000), ($307B, $0000),
    ($307C, $0000), ($307D, $0000), ($307E, $0000), ($307F, $0000),
    ($3080, $0000), ($3081, $0000), ($3082, $0000), ($3083, $0000),
    ($3084, $0000), ($3085, $0000), ($3086, $0000), ($3087, $0000),
    ($3088, $0000), ($3089, $0000), ($308A, $0000), ($308B, $0000),
    ($308C, $0000), ($308D, $0000), ($308E, $0000), ($308F, $0000),
    ($3090, $0000), ($3091, $0000), ($3092, $0000), ($3093, $0000),
    ($3094, $0000), ($3095, $0000), ($3096, $0000), ($3097, $0000),
    ($3098, $0000), ($FF9E, $0000), ($FF9F, $0000), ($FF9E, $0000),
    ($FF9F, $0000), ($309D, $0000), ($309E, $0000), ($309F, $0000),
    ($30A0, $0000), ($FF67, $0000), ($FF71, $0000), ($FF68, $0000),
    ($FF72, $0000), ($FF69, $0000), ($FF73, $0000), ($FF6A, $0000),
    ($FF74, $0000), ($FF6B, $0000), ($FF75, $0000), ($FF76, $0000),
    ($FF76, $FF9E), ($FF77, $0000), ($FF77, $FF9E), ($FF78, $0000),
    ($FF78, $FF9E), ($FF79, $0000), ($FF79, $FF9E), ($FF7A, $0000),
    ($FF7A, $FF9E), ($FF7B, $0000), ($FF7B, $FF9E), ($FF7C, $0000),
    ($FF7C, $FF9E), ($FF7D, $0000), ($FF7D, $FF9E), ($FF7E, $0000),
    ($FF7E, $FF9E), ($FF7F, $0000), ($FF7F, $FF9E), ($FF80, $0000),
    ($FF80, $FF9E), ($FF81, $0000), ($FF81, $FF9E), ($FF6F, $0000),
    ($FF82, $0000), ($FF82, $FF9E), ($FF83, $0000), ($FF83, $FF9E),
    ($FF84, $0000), ($FF84, $FF9E), ($FF85, $0000), ($FF86, $0000),
    ($FF87, $0000), ($FF88, $0000), ($FF89, $0000), ($FF8A, $0000),
    ($FF8A, $FF9E), ($FF8A, $FF9F), ($FF8B, $0000), ($FF8B, $FF9E),
    ($FF8B, $FF9F), ($FF8C, $0000), ($FF8C, $FF9E), ($FF8C, $FF9F),
    ($FF8D, $0000), ($FF8D, $FF9E), ($FF8D, $FF9F), ($FF8E, $0000),
    ($FF8E, $FF9E), ($FF8E, $FF9F), ($FF8F, $0000), ($FF90, $0000),
    ($FF91, $0000), ($FF92, $0000), ($FF93, $0000), ($FF6C, $0000),
    ($FF94, $0000), ($FF6D, $0000), ($FF95, $0000), ($FF6E, $0000),
    ($FF96, $0000), ($FF97, $0000), ($FF98, $0000), ($FF99, $0000),
    ($FF9A, $0000), ($FF9B, $0000), ($30EE, $0000), ($FF9C, $0000),
    ($30F0, $0000), ($30F1, $0000), ($FF66, $0000), ($FF9D, $0000),
    ($FF73, $FF9E), ($30F5, $0000), ($30F6, $0000), ($FF9C, $FF9E),
    ($30F0, $FF9E), ($30F1, $FF9E), ($FF66, $FF9E), ($FF65, $0000),
    ($FF70, $0000), ($30FD, $0000), ($30FD, $FF9E), ($30FF, $0000)
  );

  HiraganaToKatakanaTable: array[$3041 .. $309F] of UChar = (
    $30A1, $30A2, $30A3, $30A4, $30A5, $30A6, $30A7, $30A8, $30A9, $30AA,
    $30AB, $30AC, $30AD, $30AE, $30AF, $30B0, $30B1, $30B2, $30B3, $30B4,
    $30B5, $30B6, $30B7, $30B8, $30B9, $30BA, $30BB, $30BC, $30BD, $30BE,
    $30BF, $30C0, $30C1, $30C2, $30C3, $30C4, $30C5, $30C6, $30C7, $30C8,
    $30C9, $30CA, $30CB, $30CC, $30CD, $30CE, $30CF, $30D0, $30D1, $30D2,
    $30D3, $30D4, $30D5, $30D6, $30D7, $30D8, $30D9, $30DA, $30DB, $30DC,
    $30DD, $30DE, $30DF, $30E0, $30E1, $30E2, $30E3, $30E4, $30E5, $30E6,
    $30E7, $30E8, $30E9, $30EA, $30EB, $30EC, $30ED, $30EE, $30EF, $30F0,
    $30F1, $30F2, $30F3, $30F4, $30F5, $30F6, $3097, $3098, $3099, $309A,
    $309B, $309C, $30FD, $30FE, $309F
  );

  KatakanaToHiraganaTable: array[$30A0 .. $30FF] of UChar = (
    $30A0, $3041, $3042, $3043, $3044, $3045, $3046, $3047, $3048, $3049,
    $304A, $304B, $304C, $304D, $304E, $304F, $3050, $3051, $3052, $3053,
    $3054, $3055, $3056, $3057, $3058, $3059, $305A, $305B, $305C, $305D,
    $305E, $305F, $3060, $3061, $3062, $3063, $3064, $3065, $3066, $3067,
    $3068, $3069, $306A, $306B, $306C, $306D, $306E, $306F, $3070, $3071,
    $3072, $3073, $3074, $3075, $3076, $3077, $3078, $3079, $307A, $307B,
    $307C, $307D, $307E, $307F, $3080, $3081, $3082, $3083, $3084, $3085,
    $3086, $3087, $3088, $3089, $308A, $308B, $308C, $308D, $308E, $308F,
    $3090, $3091, $3092, $3093, $3094, $3095, $3096, $30F7, $30F8, $30F9,
    $30FA, $30FB, $30FC, $309D, $309E, $30FF
  );
{$ENDIF JapaneseExt}

{$IFNDEF USE_UNICODE_PROPERTY}
procedure ClearUnicodeMultiChar(var AMultiChar: TUnicodeMultiChar);
var
  I: Integer;
begin
  for I := Low(AMultiChar) to High(AMultiChar) do
    AMultiChar[I] := 0;
end;
{$ENDIF USE_UNICODE_PROPERTY}

function RegIsMatch(const ARegExpStr, AInputStr: REString;
  AOptions: TREOptions): Boolean;
begin
  Result := TSkRegExp.RegIsMatch(ARegExpStr, AInputStr, AOptions);
end;

function RegMatch(const ARegExpStr, AInputStr: REString; AMatches: TREStrings;
  AOptions: TREOptions = []): Boolean;
begin
  Result := TSkRegExp.RegMatch(ARegExpStr, AInputStr, AMatches, AOptions);
end;

function RegReplace(const ARegExpStr, AInputStr, AReplaceStr: REString;
  AOptions: TREOptions = []): REString;
begin
  Result := TSkRegExp.RegReplace(ARegExpStr, AInputStr, AReplaceStr, AOptions);
end;

function RegReplace(const ARegExpStr, AInputStr: REString;
  AReplaceFunc: TSkRegExpReplaceFunction; AOptions: TREOptions = []): REString;
begin
  Result := TSkRegExp.RegReplace(ARegExpStr, AInputStr, AReplaceFunc, AOptions);
end;

procedure RegSplit(const ARegExpStr, AInputStr: REString; APieces: TREStrings;
  AOptions: TREOptions = []);
begin
  TSkRegExp.RegSplit(ARegExpStr, AInputStr, APieces, AOptions);
end;

function DecodeEscape(const S: REString): REString;
begin
  Result := TSkRegExp.DecodeEscape(S);
end;

function EncodeEscape(const Str: REString): REString;
begin
  Result := TSkRegExp.EncodeEscape(Str);
end;

// ==========Suport functions =========

function Max(a, b: Integer): Integer; inline;
begin
  if a > b then
    Result := a
  else
    Result := b;
end;

function Min(a, b: Integer): Integer; inline;
begin
  if a < b then
    Result := a
  else
    Result := b;
end;

function IsLeadChar(C: WideChar): Boolean; inline;
begin
  Result := (C >= #$D800) and (C <= #$DFFF);
end;

function IsNoLeadChar(C: WideChar): Boolean; inline;
begin
  Result := (C < #$D800) or (C > #$DFFF);
end;

function REOptionsToRECompareOptions(AREOptions: TREOptions): TRECompareOptions;
begin
  Result := [];
  if roIgnoreCase in AREOptions then
    Include(Result, coIgnoreCase);
  if roIgnoreKana in AREOptions then
    Include(Result, coIgnoreKana);
  if roIgnoreWidth in AREOptions then
    Include(Result, coIgnoreWidth);
  if roASCIIOnly in AREOptions then
    Include(Result, coASCIIOnly);
end;

function GetASCIIMode(AOptions: TREOptions): Boolean; inline;
begin
  Result := (roDefinedCharClassLegacy in AOptions) or
    (roASCIICharClass in AOptions);
end;

function ToFoldCase(const S: REString; IsASCII: Boolean): REString;
var
  I, J, L, K: Integer;
  Ch: UChar;
  IsSurrogate: Boolean;
  LFold: TUnicodeMultiChar;
begin
  I := 1;
  J := 1;
  IsSurrogate := False;
  L := System.Length(S);
  SetLength(Result, L);
  ClearUnicodeMultiChar(LFold);

  while I <= L do
  begin
    if S[I] < #$80 then
    begin
      case S[I] of
        'A'..'Z':
          Ch := Ord(S[I]) xor $0020;
        else
          Ch := Ord(S[I]);
      end;
    end
    else
    begin
      if IsNoLeadChar(S[I]) then
      begin
        Ch := Ord(S[I]);
        IsSurrogate := False;
      end
      else
      begin
        Ch := ((WORD(S[I]) and $03FF) shl 10) + ((WORD(S[I + 1]) and $03FF) + $10000);
        Inc(I);
        IsSurrogate := True;
      end;

{$IFDEF USE_UNICODE_PROPERTY}
      if not IsASCII then
      begin
        LFold := GetUnicodeFoldCase(Ch);

        if LFold[0] > 1 then
        begin
          SetLength(Result, System.Length(Result) + LFold[0] - 1);
        end;

        for K := LFold[0] downto 1 do
        begin
          if LFold[K] >= $10000 then
          begin
            // 変換元文字はサロゲートペアではないが変換後にサロゲートペアになる場合の処理
            if not IsSurrogate then
              SetLength(Result, System.Length(Result) + 1);

            Result[J] := WideChar($D800 + (LFold[K] - $10000) shr 10);
            Inc(J);
            Result[J] := WideChar($DC00 + (LFold[K] and $03FF));
          end
          else
            Result[J] := WideChar(LFold[K]);

          Inc(J);
        end;
        Inc(I);

        Continue;
      end;
{$ENDIF USE_UNICODE_PROPERTY}
    end;

    if Ch >= $10000 then
    begin
      // 変換元文字はサロゲートペアではないが変換後にサロゲートペアになる場合の処理
      if not IsSurrogate then
        SetLength(Result, System.Length(Result) + 1);

      Result[J] := WideChar($D800 + (Ch - $10000) shr 10);
      Inc(J);
      Result[J] := WideChar($DC00 + (Ch and $03FF));
    end
    else
      Result[J] := WideChar(Ch);

    Inc(I);
    Inc(J);
  end;
end;

// ==========日本語処理用ルーチン==========

{$IFDEF JapaneseExt}
function IsDaku(S: PWideChar): Boolean; inline;
begin
  Result := ((S + 1)^ = CONST_Dakuten) and (HalfToWideKatakanaTable[UChar(S^), 1] <> 0);
end;

function IsHanDaku(S: PWideChar): Boolean; inline;
begin
  Result := ((S + 1)^ = CONST_HanDakuten) and (HalfToWideKatakanaTable[UChar(S^), 2] <> 0);
end;

function IsWideKana(Ch: WideChar): Boolean; inline;
begin
  Result := (Ch >= #$3000) and (Ch <= #$30FF);
end;

function IsHalfKatakana(S: UChar): Boolean; inline; overload;
begin
  Result := (S >= $FF61) and (S <= $FF9F);
end;

function IsHalfKatakana(S: WideChar): Boolean; inline; overload;
begin
  Result := (S >= #$FF61) and (S <= #$FF9F);
end;

function IsWideHiragana(S: UChar): Boolean; inline; overload;
begin
  Result := (S >= $3041) and (S <= $309F);
end;

function IsWideHiragana(Ch: WideChar): Boolean; inline; overload;
begin
  Result := (Ch >= #$3041) and (Ch <= #$309F);
end;

function IsWideKatakana(Ch: WideChar): Boolean; inline;
begin
  Result := (Ch >= #$30A0) and (Ch <= #$30FF);
end;

function IsHalfAnk(S: UChar): Boolean; inline; overload;
begin
  Result := (S >= $20) and (S <= $7E);
end;

function IsHalfAnk(S: WideChar): Boolean; inline; overload;
begin
  Result := (S >= #$0020) and (S <= #$007E);
end;

function IsWideAnk(Ch: WideChar): Boolean; inline;
begin
  Result := (Ch = #$3000) or (Ch = #$FFE5) or
    ((Ch >= #$FF01) and (Ch <= #$FF5E))
end;

function ToHalf(const S: REString): REString;
var
  I, J, L: Integer;
begin
  I := 1;
  J := 1;
  L := Length(S);
  SetLength(Result, L);

  while I <= L do
  begin
    if IsWideAnk(S[I]) then
    begin
      if S[I] = #$3000 then
        Result[J] := #$0020
      else if S[I] = #$FFE5 then
        Result[J] := #$005C
      else
        Result[J] := WideChar(WideToHalfAnkTable[Ord(S[I])])
    end
    else if IsWideKana(S[I]) then
    begin
      Result[J] := WideChar(WideToHalfKatakanaTable[Ord(S[I]), 0]);
      if WideToHalfKatakanaTable[Ord(S[I]), 1] <> 0 then
      begin
        SetLength(Result, System.Length(Result) + 1);
        Inc(J);
        Result[J] := WideChar(WideToHalfKatakanaTable[Ord(S[I]), 1]);
      end;
    end
    else
      Result[J] := S[I];

    Inc(I);
    Inc(J);
  end;
end;

function ToWide(const S: REString): REString;
var
  I, J, L: Integer;
begin
  I := 1;
  J := 1;
  L := Length(S);
  SetLength(Result, L);

  while I <= L do
  begin
    if IsHalfAnk(S[I]) then
      Result[J] := WideChar(HalfToWideAnkTable[Ord(S[I])])
    else if IsHalfKatakana(S[I]) then
    begin
      if (I < L) and (S[I + 1] = CONST_Dakuten) then
      begin
        SetLength(Result, System.Length(Result) - 1);
        Result[J] := WideChar(HalfToWideKatakanaTable[Ord(S[I]), 1]);
        Inc(I);
      end
      else if (I < L) and (S[I + 1] = CONST_Handakuten) then
      begin
        SetLength(Result, System.Length(Result) - 1);
        Result[J] := WideChar(HalfToWideKatakanaTable[Ord(S[I]), 2]);
        Inc(I);
      end
      else
        Result[J] := WideChar(HalfToWideKatakanaTable[Ord(S[I]), 0]);
    end
    else if S[I] = CONST_Wide_Dakuten then
      Result[J] := CONST_Wide_Dakuten_CS
    else if S[I] = CONST_Wide_Handakuten then
      Result[J] := CONST_Wide_Handakuten_CS
    else
      Result[J] := S[I];

    Inc(I);
    Inc(J);
  end;
end;

function ToHiragana(const S: REString): REString;
var
  I, J, L: Integer;
begin
  I := 1;
  J := 1;
  L := Length(S);
  SetLength(Result, L);

  while I <= L do
  begin
    if IsWideKatakana(S[I]) then
      Result[J] := WideChar(KatakanaToHiraganaTable[Ord(S[I])])
    else
      Result[J] := S[I];

    Inc(I);
    Inc(J);
  end;
end;

function ToKatakana(const S: REString): REString;
var
  I, J, L: Integer;
begin
  I := 1;
  J := 1;
  L := Length(S);
  SetLength(Result, L);

  while I <= L do
  begin
    if IsWideHiragana(S[I]) then
      Result[J] := WideChar(HiraganaToKatakanaTable[Ord(S[I])])
    else
      Result[J] := S[I];

    Inc(I);
    Inc(J);
  end;
end;
{$ENDIF JapaneseExt}

// ==========文字入出力用ルーチン==========

function ToUChar(AStr: PWideChar): UChar; inline; overload;
begin
  if IsNoLeadChar(AStr^) then
    Result := UChar(AStr^)
  else
    Result := ((WORD(AStr^) and $03FF) shl 10) +
      ((WORD((AStr + 1)^) and $03FF) + $10000);
end;

function ToUChar(AStr: PWideChar; out Len: Integer): UChar; inline; overload;
begin
  if IsNoLeadChar(AStr^) then
  begin
    Result := UChar(AStr^);
    Len := 1;
  end
  else
  begin
    Result := ((WORD(AStr^) and $03FF) shl 10) +
      ((WORD((AStr + 1)^) and $03FF) + $10000);
    Len := 2;
  end;
end;

function ToUChar(const S: REString; const Index: Integer): UChar; inline; overload;
begin
  if IsNoLeadChar(S[Index]) then
    Result := UChar(S[Index])
  else
    Result := ((WORD(S[Index]) and $03FF) shl 10) +
      ((WORD(S[Index + 1]) and $03FF) + $10000);
end;

function UCharToString(AWChar: UChar): REString; inline;
var
  H, L: Cardinal;
begin
  if AWChar >= $10000 then
  begin
    H := $D800 + (AWChar - $10000) shr 10;
    L := $DC00 + (AWChar and $03FF);
    Result := WideChar(H) + WideChar(L);
  end
  else
    Result := WideChar(AWChar);
end;

function RECharArrayToString(CharArray: UCS4String): REString;
var
  I: Integer;
begin
  for I := 0 to Length(CharArray) - 1 do
    Result := Result + UCharToString(CharArray[I]);
end;

function GetREChar(AStr: PWideChar; var Len: Integer;
  Options: TRECompareOptions; var AFoldCase: TUnicodeMultiChar): UChar;
begin
  Len := 0;

  if AFoldCase[0] = 0 then
  begin
    if IsNoLeadChar(AStr^) then
    begin
      Result := UChar(AStr^);
      Len := 1;
    end
    else
    begin
      Result := ((WORD(AStr^) and $03FF) shl 10) +
        ((WORD((AStr + 1)^) and $03FF) + $10000);
      Len := 2;
    end;

    if Options = [] then
      Exit;

    if coIgnoreCase in Options then
    begin
      if Result < $80 then
      begin
        case Result of
          Ord('A')..Ord('Z'):
            Result := Result xor $0020;
        end;
{$IFDEF USE_UNICODE_PROPERTY}
      end
      else
      begin
        if not (coASCIIOnly in Options) then
        begin
          AFoldCase := GetUnicodeFoldCase(Result);
          Result := AFoldCase[AFoldCase[0]];
          Dec(AFoldCase[0]);
          if AFoldCase[0] > 0 then
            Len := 0;
        end;
{$ENDIF USE_UNICODE_PROPERTY}
      end;
    end;
  end
  else
  begin
    Result := AFoldCase[AFoldCase[0]];
    Dec(AFoldCase[0]);
    if AFoldCase[0] > 0 then
      Len := 0
    else
      Len := 1;
  end;

{$IFDEF JapaneseExt}
  if coIgnoreWidth in Options then
  begin
    if IsHalfAnk(Result) then
      Result := HalfToWideAnkTable[Result]
    else if IsHalfKatakana(Result) then
    begin
      if IsDaku(AStr) then
      begin
        Result := HalfToWideKatakanaTable[Result, 1];
        Len := 2;
      end
      else if IsHanDaku(AStr) then
      begin
        Result := HalfToWideKatakanaTable[Result, 2];
        Len := 2;
      end
      else
        Result := HalfToWideKatakanaTable[Result, 0];
    end;
  end;

  if coIgnoreKana in Options then
  begin
    if IsWideHiragana(AStr^) then
      Result := HiraganaToKatakanaTable[Result]
  end;
{$ENDIF}
end;

// ==========文字種判定用ルーチン==========

function IsAlnumA(Ch: UChar): Boolean; inline;
begin
  Result :=
    (CONST_AlnumAMap[Byte(Ch) div 8] and (1 shl (Byte(Ch) and 7)) <> 0);
end;

function IsAlnumU(Ch: UChar): Boolean;
var
  up, ug: TUnicodeProperty;
begin
  up := GetUnicodeCategory(Ch);
  if up = upNd then
    Result := True
  else
  begin
    ug := UnicodeGeneralCategoryTable[up];

    Result := ug in [upL, upM];
  end;
end;

function IsAlphaA(Ch: UChar): Boolean; inline;
begin
  Result :=
      (CONST_AlphaAMap[Byte(Ch) div 8] and (1 shl (Byte(Ch) and 7)) <> 0);
end;

function IsAlphaU(Ch: UChar): Boolean; inline;
var
  up: TUnicodeProperty;
begin
  up := GetUnicodeGeneralCategory(Ch);
  Result := up in [upL, upM];
end;

function IsAscii(Ch: UChar): Boolean; inline;
begin
  Result := Ch <= $7F;
end;

function IsBlankA(Ch: UChar): Boolean; inline;
begin
//    [ \t]
  Result :=
    (CONST_BlankAMap[Byte(Ch) div 8] and (1 shl (Byte(Ch) and 7)) <> 0);
end;

function IsBlankU(Ch: UChar): Boolean; inline;
begin
  Result := (Ch = 9) or
    (GetUnicodeCategory(Ch) = upZs);
end;

function IsCntrlA(Ch: UChar): Boolean; overload; inline;
begin
//  [\x00-\x1F\x7F]
  Result :=
    (CONST_CntrlAMap[Byte(Ch) div 8] and (1 shl (Byte(Ch) and 7)) <> 0);
end;

function IsCntrlU(Ch: UChar): Boolean; inline;
begin
  Result := GetUnicodeCategory(Ch) = upCc;
end;

function IsDigitA(Ch: UChar): Boolean; inline;
begin
  Result :=
    (CONST_DigitAMap[Byte(Ch) div 8] and (1 shl (Byte(Ch) and 7)) <> 0);
end;

function IsDigitU(Ch: UChar): Boolean; inline;
begin
  Result := GetUnicodeCategory(Ch) = upNd;
end;

function IsSpaceA(Ch: UChar): Boolean; inline;
begin
  Result :=
    (CONST_SpaceAMap[Byte(Ch) div 8] and (1 shl (Byte(Ch) and 7)) <> 0);
end;

function IsSpaceU(Ch: UChar): Boolean; inline;
begin
  if (Ch < 128) and IsSpaceA(Ch) then
    Result := True
  else
    Result := (GetUnicodeGeneralCategory(Ch) = upZ)
end;

function IsSpacePerlA(Ch: UChar): Boolean; inline; overload;
begin
  Result :=
    (CONST_SpacePerlAMap[Byte(Ch) div 8] and (1 shl (Byte(Ch) and 7)) <> 0);
end;

function IsSpacePerlU(Ch: UChar): Boolean; inline; overload;
begin
  if (Ch < 128) and IsSpacePerlA(Ch) then
  begin
    Result := True;
    Exit;
  end;

  case Ch of
    $0085, $2028, $2029:
      Result := True;
    else
      Result := GetUnicodeGeneralCategory(Ch) = upZ;
  end;
end;

function IsGraphA(Ch: UChar): Boolean; inline;
begin
//  [\x21-\x7E]
  Result :=
    (CONST_GraphAMap[Byte(Ch) div 8] and (1 shl (Byte(Ch) and 7)) <> 0);
end;

function IsGraphU(Ch: UChar): Boolean;
var
  up: TUnicodeProperty;
begin
//[\P{IsSpace}\P{Cc}\P{Cn}\P{Cs}]と同じ
  if IsSpaceU(Ch) then
    Result := False
  else
  begin
    up := GetUnicodeCategory(Ch);
    Result := (up <> upCc) and (up <> upCn) and (up <> upCs);
  end;
end;

function IsLowerA(Ch: UChar): Boolean; inline;
begin
  Result :=
    (CONST_LowerAMap[Ch div 8] and (1 shl (Ch and 7)) <> 0);
end;

function IsLowerU(Ch: UChar): Boolean; inline;
begin
  Result := GetUnicodeCategory(Ch) = upLl;
end;

function IsPrintA(Ch: UChar): Boolean; inline;
begin
//  [\x20-\x7E]
  Result :=
    (CONST_PrintAMap[Byte(Ch) div 8] and (1 shl (Byte(Ch) and 7)) <> 0);
end;

function IsPrintU(Ch: UChar): Boolean;
var
  up: TUnicodeProperty;
begin
  if Ch = $0085 then
    Result := True
  else
  begin
    up := GetUnicodeCategory(Ch);
    Result := (GetUnicodeGeneralCategory(Ch) = upZ) or
      ((up <> upCc) and (up <> upCn) and (up <> upCs));
  end;
end;

function IsPunctA(Ch: UChar): Boolean;  inline;
begin
//[!"#$%&'()*+,\-./:;<=>?@[\\\]^_`{|}~]
  Result :=
    (CONST_PunctAMap[Byte(Ch) div 8] and (1 shl (Byte(Ch) and 7)) <> 0);
end;

function IsPunctU(Ch: UChar): Boolean;  inline;
begin
  Result := GetUnicodeGeneralCategory(Ch) = upP;
end;

function IsUpperA(Ch: UChar): Boolean; inline;
begin
  Result :=
    (CONST_UpperAMap[Byte(Ch) div 8] and (1 shl (Byte(Ch) and 7)) <> 0);
end;

function IsUpperU(Ch: UChar): Boolean; inline;
begin
  Result := GetUnicodeCategory(Ch) = upLu;
end;

function IsXDigit(Ch: UChar): Boolean; inline;
begin
  Result :=
    (CONST_XDigitAMap[Byte(Ch) div 8] and (1 shl (Byte(Ch) and 7)) <> 0);
end;

function IsWordA(Ch: UChar): Boolean; inline; overload;
begin
  Result :=
    (CONST_WordAMap[Byte(Ch) div 8] and (1 shl (Byte(Ch) and 7)) <> 0);
end;

function IsWordU(Ch: UChar): Boolean; overload;
var
  up, ug: TUnicodeProperty;
begin
  //      (ug = upL) or (ug = upM) or (up = upNd) or (up = upPc);
  up := GetUnicodeCategory(Ch);
  if up in [upNd, upPc] then
    Result := True
  else
  begin
    ug := UnicodeGeneralCategoryTable[up];
    Result := (ug in [upL, upM]);
  end;
end;

function IsAny(Ch: UChar): Boolean; inline;
begin
  Result := True;
end;

function IsAssignedA(Ch: UChar): Boolean; inline;
begin
  Result := True;
end;

function IsAssignedU(Ch: UChar): Boolean; inline;
begin
  Result := GetUnicodeCategory(Ch) <> upCn;
end;

function IsSpaceHorizontal(Ch: UChar): Boolean; inline; overload;
begin
  case Ch of
    $0009, $0020, $00A0, $1680, $180E, $2000 .. $200A, $202F,
      $205F, $3000:
      Result := True
  else
    Result := False;
  end;
end;

function IsSpaceVertical(Ch: UChar): Boolean; inline; overload;
begin
  case Ch of
    $000A, $000B, $000C, $000D, $0085, $2028, $2029:
      Result := True
  else
    Result := False;
  end;
end;

function IsPosixClassA(Ch: UChar; AClass: TREPosixClassKind;
  IgnoreCase: Boolean): Boolean;
begin
  case AClass of
    pckAlnum:
      Result := IsAlnumA(Ch);
    pckAlpha:
      Result := IsAlphaA(Ch);
    pckAscii:
      Result := IsASCII(Ch);
    pckBlank:
      Result := IsBlankA(Ch);
    pckCntrl:
      Result := IsCntrlA(Ch);
    pckDigit:
      Result := IsDigitA(Ch);
    pckGraph:
      Result := IsGraphA(Ch);
    pckLower:
      begin
        if not IgnoreCase then
        begin
          Result := IsLowerA(Ch);
        end
        else
        begin
          case Ch of
            Ord('A')..Ord('Z'), Ord('a')..Ord('z'):
              Result := True;
            else
              Result := False;
          end;
        end;
      end;
    pckPrint:
      Result := IsPrintA(Ch);
    pckUpper:
      begin
        if not IgnoreCase then
        begin
          Result := IsUpperA(Ch);
        end
        else
        begin
          case Ch of
            Ord('A')..Ord('Z'), Ord('a')..Ord('z'):
              Result := True;
            else
              Result := False;
          end;
        end;
      end;
    pckPunct:
      Result := IsPunctA(Ch);
    pckSpace:
      Result := IsSpaceA(Ch);
    pckSpacePerl:
      Result := IsSpacePerlA(Ch);
    pckSpaceVertical:
      Result := IsSpaceVertical(Ch);
    pckSpaceHorizontal:
      Result := IsSpaceHorizontal(Ch);
    pckXdigit:
      Result := IsXDigit(Ch);
    pckWord:
      Result := IsWordA(Ch);
    pckAssigned:
      Result := IsAssignedA(Ch);
  else
    Result := IsAny(Ch);
  end;
end;

{$IFDEF USE_UNICODE_PROPERTY}
function IsPosixClassU(Ch: UChar; AClass: TREPosixClassKind;
  IgnoreCase: Boolean): Boolean;
var
  up: TUnicodeProperty;
begin
  case AClass of
    pckAlnum:
      Result := IsAlnumU(Ch);
    pckAlpha:
      Result := IsAlphaU(Ch);
    pckAscii:
      Result := IsASCII(Ch);
    pckBlank:
      Result := IsBlankU(Ch);
    pckCntrl:
      Result := IsCntrlU(Ch);
    pckDigit:
      Result := IsDigitU(Ch);
    pckGraph:
      Result := IsGraphU(Ch);
    pckLower:
      begin
        if not IgnoreCase then
        begin
          Result := IsLowerU(Ch);
        end
        else
        begin
          up := GetUnicodeCategory(Ch);
          Result := (up = upLu) or (up = upLl);
        end;
      end;
    pckPrint:
      Result := IsPrintU(Ch);
    pckUpper:
      begin
        if not IgnoreCase then
        begin
          Result := IsUpperU(Ch);
        end
        else
        begin
          up := GetUnicodeCategory(Ch);
          Result := (up = upLu) or (up = upLl);
        end;
      end;
    pckPunct:
      Result := IsPunctU(Ch);
    pckSpace:
      Result := IsSpaceU(Ch);
    pckSpacePerl:
      Result := IsSpacePerlU(Ch);
    pckPunctVertical:
      Result := IsSpaceVertical(Ch);
    pckPunctHorizontal:
      Result := IsSpaceHorizontal(Ch);
    pckXdigit:
      Result := (Ch < 128) and IsXDigit(Ch);
    pckWord:
      Result := IsWordU(Ch);
    pckAssigned:
      Result := IsAssignedU(Ch);
  else
    Result := IsAny(Ch);
  end;
end;
{$ENDIF USE_UNICODE_PROPERTY}

// ==========文字列検索用ルーチン==========

{$IFDEF UNICODE}
function REStrLComp(const Str1, Str2: PWideChar; MaxLen: Cardinal): Integer; inline;
begin
  Result := StrLComp(Str1, Str2, MaxLen);
end;
{$ELSE UNICODE}
function REStrLComp(const Str1, Str2: PWideChar; MaxLen: Cardinal): Integer;
var
  P1, P2: PWideChar;
  I: Cardinal;
  C1, C2: WideChar;
begin
  P1 := Str1;
  P2 := Str2;
  I := 0;
  while I < MaxLen do
  begin
    C1 := P1^;
    C2 := P2^;

    if (C1 <> C2) or (C1 = #0) then
    begin
      Result := Ord(C1) - Ord(C2);
      Exit;
    end;

    Inc(P1);
    Inc(P2);
    Inc(I);
  end;
  Result := 0;
end;
{$ENDIF UNICODE}

function REStrLJComp(AStr, ASubStr: PWideChar; ALen: Integer;
  var MatchLen: Integer; AOptions: TRECompareOptions): Integer;
var
  StartP: PWideChar;
  W1, W2: Integer;
  LFold1, LFold2: TUnicodeMultiChar;
  L1, L2: Integer;
  I: Integer;
begin
  Result := 0;
  MatchLen := 0;
  StartP := AStr;
  ClearUnicodeMultiChar(LFold1);
  ClearUnicodeMultiChar(LFold2);

  I := 1;
  while I <= ALen do
  begin
    W1 := GetREChar(AStr, L1, AOptions, LFold1);
    W2 := GetREChar(ASubStr, L2, AOptions, LFold2);
    if W1 <> W2 then
    begin
      Result := W1 - W2;
      MatchLen := 0;
      Exit;
    end;

    Inc(AStr, L1);
    Inc(ASubStr, L2);
    if L1 = 0 then
      Inc(I);
    Inc(I, L1);
  end;
  MatchLen := AStr - StartP;
end;

function REStrLIComp(const Str1, Str2: PWideChar; MaxLen: Cardinal;
  var MatchLen: Integer): Integer;
var
  C1, C2: Integer;
  P1, P2: PWideChar;
  FD1, FD2: TUnicodeMultiChar;
  I: Cardinal;
begin
  ClearUnicodeMultiChar(FD1);
  ClearUnicodeMultiChar(FD2);
  P1 := Str1;
  P2 := Str2;
  I := 0;
  MatchLen := 0;

  while I < MaxLen do
  begin
    if FD1[0] = 0 then
    begin
      if P1^ < #$80 then
      begin
        case P1^ of
          'A'..'Z':
            begin
              C1 := Integer(P1^) xor $0020;
            end;
          else
            C1 := Integer(P1^);
        end;
        Inc(P1);
      end
      else
      begin
        if IsLeadChar(P1^) then
        begin
          C1 := ((WORD(P1^) and $03FF) shl 10) +
            ((WORD((P1 + 1)^) and $03FF) + $10000);
          Inc(P1, 2);
        end
        else
        begin
          C1 := UChar(P1^);
          Inc(P1);
        end;
{$IFDEF USE_UNICODE_PROPERTY}
        FD1 := GetUnicodeFoldCase(C1);
        C1 := FD1[FD1[0]];
        Dec(FD1[0]);
{$ENDIF USE_UNICODE_PROPERTY}
      end;
      Inc(MatchLen);
    end
    else
    begin
      C1 := FD1[FD1[0]];
      Dec(FD1[0]);
    end;

    if FD2[0] = 0 then
    begin
      if P2^ < #$80 then
      begin
        case P2^ of
          'A'..'Z':
            C2 := Integer(P2^) xor $0020
        else
          C2 := Integer(P2^);
        end;
        Inc(P2);
      end
      else
      begin
        if IsLeadChar(P2^) then
        begin
          C2 := ((WORD(P2^) and $03FF) shl 10) +
            ((WORD((P2 + 1)^) and $03FF) + $10000);
          Inc(P2, 2);
        end
        else
        begin
          C2 := UChar(P2^);
          Inc(P2);
        end;
{$IFDEF USE_UNICODE_PROPERTY}
        FD2 := GetUnicodeFoldCase(C2);
        C2 := FD2[FD2[0]];
        Dec(FD2[0]);
{$ENDIF USE_UNICODE_PROPERTY}
      end;
    end
    else
    begin
      C2 := FD2[FD2[0]];
      Dec(FD2[0]);
    end;

    if (C1 <> C2) or (C1 = 0) then
    begin
      Result := C1 - C2;
      MatchLen := 0;
      Exit;
    end;

    Inc(I);
  end;
  Result := FD1[0] - FD2[0];
end;

function REStrLIAComp(const Str1, Str2: PWideChar; MaxLen:Cardinal): Integer;
{$IFDEF UNICODE}
begin
  Result := StrLIComp(Str1, Str2, MaxLen);
end;
{$ELSE UNICODE}
var
  P1, P2: PWideChar;
  I: Cardinal;
  C1, C2: Integer;
begin
  P1 := Str1;
  P2 := Str2;
  I := 0;
  while I < MaxLen do
  begin
    case P1^ of
      'A'..'Z':
        C1 := Integer(P1^) xor $0020
      else
        C1 := Integer(P1^);
    end;
    Inc(P1);

    case P2^ of
      'A'..'Z':
        C2 := Integer(P2^) xor $0020
      else
        C2 := Integer(P2^);
    end;
    Inc(P2);

    if (C1 <> C2) or (C1 = 0) then
    begin
      Result := C1 - C2;
      Exit;
    end;

    Inc(I);
  end;
  Result := 0;
end;
{$ENDIF UNICODE}

{
  AStr: 検索対象の文字列
  ASubStr: 検索文字列
  ALen: 検索文字列の文字数
  AOptions: 検索オプション

  大文字小文字を区別しない
  全角半角を区別しない
  ひらがなカタカナを区別しない

}
function RECompareString(SourceP: PWideChar; DestP: PWideChar; DestLen: Integer;
  var MatchLen: Integer; Options: TRECompareOptions): Integer; inline;
begin
  if (coIgnoreWidth in Options) or (coIgnoreKana in Options) then
    Result := REStrLJComp(SourceP, DestP, DestLen, MatchLen, Options)
  else if coIgnoreCase in Options then
  begin
    if coASCIIOnly in Options then
    begin
      Result := REStrLIAComp(SourceP, DestP, DestLen);
      if Result = 0 then
        MatchLen := DestLen
      else
        MatchLen := 0;
    end
    else
      Result := REStrLIComp(SourceP, DestP, DestLen, MatchLen);
  end
  else if DestLen = 1 then
  begin
    Result := Ord(SourceP^) - Ord(DestP^);
    if Result = 0 then
      MatchLen := DestLen
    else
      MatchLen := 0;
  end
  else
  begin
    Result := REStrLComp(SourceP, DestP, DestLen);
    if Result = 0 then
      MatchLen := DestLen
    else
      MatchLen := 0;
  end;
end;

function REStrPos(AStr: PWideChar; ALen: Integer; APattern: PWideChar;
  APatternLen: Integer; var MatchLen: Integer;
  AOptions: TRECompareOptions): PWideChar;
var
  TextEndP: PWideChar;
begin
  Result := nil;
  MatchLen := 0;

  TextEndP := AStr + ALen;

  if (coIgnoreWidth in AOptions) or (coIgnoreKana in AOptions) then
  begin
    while AStr <= TextEndP do
    begin
      if REStrLJComp(AStr, APattern, APatternLen, MatchLen, AOptions) = 0 then
      begin
        Result := AStr;
        Exit;
      end;
      Inc(AStr);
    end;
  end
  else if coIgnoreCase in AOptions then
  begin
    if coASCIIOnly in AOptions then
    begin
      while AStr <= TextEndP do
      begin
        if REStrLIAComp(AStr, APattern, APatternLen) = 0 then
        begin
          Result := AStr;
          MatchLen := APatternLen;
          Exit;
        end;
        Inc(AStr);
      end;
    end
    else
    begin
      while AStr <= TextEndP do
      begin
        if REStrLIComp(AStr, APattern, APatternLen, MatchLen) = 0 then
        begin
          Result := AStr;
          Exit;
        end;
        Inc(AStr);
      end;
    end;
  end
  else
  begin
    while AStr <= TextEndP do
    begin
      if REStrLComp(AStr, APattern, APatternLen) = 0 then
      begin
        MatchLen := APatternLen;
        Result := AStr;
        Exit;
      end;
      Inc(AStr);
    end;
  end;
end;

// ==========文字位置移動用ルーチン==========

procedure CharNext(var P: PWideChar; Len: Integer = 1); inline; overload;
var
  I: Integer;
begin
  for I := 1 to Len do
  begin
    if IsNoLeadChar(P^) then
      Inc(P)
    else
      Inc(P, 2);
  end;
end;

procedure CharNext(var P: PWideChar; EndP: PWideChar; Len: Integer = 1); inline; overload;
var
  I: Integer;
begin
  for I := 1 to Len do
  begin
    if IsNoLeadChar(P^) then
      Inc(P)
    else
      Inc(P, 2);
    if EndP = P then
      Exit;
  end;
end;

{$IFDEF USE_UNICODE_PROPERTY}
procedure CharNextForCombiningSequence(var P: PWideChar; Len: Integer = 1);
var
  I: Integer;
begin
  for I := 1 to Len do
  begin
    if not IsUnicodeProperty(ToUChar(P), upM) then
    begin
      Inc(P);

      while IsUnicodeProperty(ToUChar(P), upM) do
        Inc(P);
    end;
  end;
end;
{$ENDIF USE_UNICODE_PROPERTY}

procedure CharPrev(var P: PWideChar; Len: Integer = 1); inline; overload;
var
  I: Integer;
begin
  for I := 1 to Len do
  begin
    Dec(P);
    if IsLeadChar(P^) then
      Dec(P);
  end;
end;

procedure CharPrev(var P: PWideChar;  StartP: PWideChar; Len: Integer = 1); inline; overload;
var
  I: Integer;
begin
  for I := 1 to Len do
  begin
    if StartP = P then
      Exit;
    Dec(P);
    if IsLeadChar(P^) then
      Dec(P);
  end;
end;

{ TREQuickSearch }

procedure TREQuickSearch.Clear;
begin
  FFindText := '';
  FPattern := nil;
  FPatternLen := 0;
  FTextTopP := nil;
  FTextEndP := nil;
  FTextLen := 0;
  FOptions := [];
  FMatchP := nil;
  FCompiled := False;
end;

procedure TREQuickSearch.Compile;
var
  I, Low: Integer;
begin
  if FPatternLen > 2 then
  begin
    for I := 0 to 255 do
      FSkipTable[I] := FPatternLen;

    for I := 0 to FPatternLen - 1 do
    begin
      Low := Integer(FPattern[I]) and $FF;
      FSkipTable[ Low] := FPatternLen - I;
    end;
  end;
  FCompiled := True;
end;

function TREQuickSearch.Exec(AStr: PWideChar; ATextLen: Integer): Boolean;
begin
  if not FCompiled then
    Compile;

  FTextTopP := AStr;
  FTextLen := ATextLen;
  FTextEndP := AStr + ATextLen;

  Result := IsMatch(AStr, FOptions);
end;

function TREQuickSearch.ExecNext: Boolean;
var
  AStr: PWideChar;
begin
  if FSkipP = nil then
  begin
    AStr := FMatchP;
    CharNext(AStr);
  end
  else
  begin
    AStr := FSkipP;
    if IsLeadChar(AStr^) then
      Inc(AStr);
    Inc(AStr);
    FSkipP := nil;
  end;

  FTextLen := FTextEndP - AStr;

  Result := IsMatch(AStr, FOptions);
end;

function TREQuickSearch.IsMatch(AStr: PWideChar;
  AOptions: TRECompareOptions): Boolean;
var
  Index, L, Low: Integer;
  WChar, C1: UChar;
  F1: TUnicodeMultiChar;
  P: PWideChar;
  LFold: TUnicodeMultiChar;
  LMatchLen: Integer;
begin
  Result := False;
  FMatchP := nil;

  if not (coIgnoreCase in FOptions) or (coASCIIOnly in FOptions) then
    if FTextLen < FPatternLen then
      Exit;

  { パターンが1-2文字ならQuick Seachの意味がない }
  if FPatternLen = 1 then
  begin
    if (coIgnoreWidth in AOptions) or (coIgnoreKana in AOptions) then
    begin
      ClearUnicodeMultiChar(LFold);

      WChar := ToUChar(FPattern);
      while AStr <= FTextEndP do
      begin
        if WChar = GetREChar(AStr, L, AOptions, LFold) then
        begin
          FMatchP := AStr;
          Result := True;
          Exit;
        end;
        Inc(AStr, L);
      end;
    end
    else if coIgnoreCase in AOptions then
    begin
      if coASCIIOnly in AOptions then
      begin
        C1 := ToUChar(FPattern);

        while AStr <= FTextEndP do
        begin
          P := AStr;

          if AStr^ < #$80 then
          begin
            case AStr^ of
              'A'..'Z':
                WChar := Integer(AStr^) xor $0020
              else
                WChar := Integer(AStr^);
            end;
            Inc(AStr);
          end
          else
          begin
            if IsNoLeadChar(AStr^) then
            begin
              WChar := UChar(AStr^);
              Inc(AStr);
            end
            else
            begin
              WChar := ((WORD(AStr^) and $03FF) shl 10) +
                ((WORD((AStr + 1)^) and $03FF) + $10000);
              Inc(AStr, 2);
            end;
          end;

          if WChar = C1 then
          begin
            FMatchP := P;
            Result := True;
            Exit;
          end;
        end;
      end
      else
      begin
        C1 := ToUChar(FPattern);

        while AStr <= FTextEndP do
        begin
          P := AStr;

          if AStr^ < #$80 then
          begin
            case AStr^ of
              'A'..'Z':
                WChar := Integer(AStr^) xor $0020
              else
                WChar := Integer(AStr^);
            end;
            Inc(AStr);
          end
          else
          begin
            if IsNoLeadChar(AStr^) then
            begin
              WChar := UChar(AStr^);
              Inc(AStr);
            end
            else
            begin
              WChar := ((WORD(AStr^) and $03FF) shl 10) +
                ((WORD((AStr + 1)^) and $03FF) + $10000);
              Inc(AStr, 2);
            end;

{$IFDEF USE_UNICODE_PROPERTY}
            F1 := GetUnicodeFoldCase(WChar);
            if F1[0] = 1 then
              WChar := F1[1]
            else
              Exit;
{$ENDIF USE_UNICODE_PROPERTY}
          end;

          if WChar = C1 then
          begin
            FMatchP := P;
            Result := True;
            Exit;
          end;
        end;
      end;
    end
    else
    begin
      P := PWideChar(FPattern);
      while AStr <= FTextEndP do
      begin
        if AStr^ = P^ then
        begin
          FMatchP := AStr;
          Result := True;
          Exit;
        end;
        Inc(AStr);
      end;
    end;
  end
  else if FPatternLen = 2 then
  begin
    L := FTextEndP - AStr;
    P := REStrPos(AStr, L, PWideChar(FPattern), FPatternLen, LMatchLen, AOptions);
    Result := P <> nil;
    if Result then
    begin
      FMatchLen := LMatchLen;
      FMatchP := P;
      Exit;
    end;
  end
  else
  begin
    Index := 0;
    ClearUnicodeMultiChar(LFold);

    if (coIgnoreWidth in AOptions) or (coIgnoreKana in AOptions) then
    begin
      while Index <= FTextLen - FPatternLen do
      begin
        if REStrLJComp(AStr + Index, FPattern, FPatternLen, LMatchLen, AOptions) = 0 then
        begin
          FMatchP := AStr + Index;
          FMatchLen := LMatchLen;
          Result := True;
          Exit;
        end;

        Low := Integer(
          GetREChar(AStr + Index + FPatternLen, L, FOptions, LFold)) and $FF;
        Inc(Index, FSkipTable[Low]);
      end;
    end
    else if coIgnoreCase in AOptions then
    begin
      if coASCIIOnly in AOptions then
      begin
        while Index <= FTextLen - FPatternLen do
        begin
          if REStrLIAComp(AStr + Index, FPattern, FPatternLen) = 0 then
          begin
            FMatchP := AStr + Index;
            Result := True;
            Exit;
          end;

          P := AStr + Index + FPatternLen;
          case P^ of
            'A'..'Z':
              C1 := Integer(P^) xor $0020;
          else
            C1 := ToUChar(P);
          end;
          Low := C1 and $FF;
          Inc(Index, FSkipTable[ Low]);
        end;
      end
      else
      begin
        FMatchP :=
          REStrPos(AStr, FTextLen, FPattern, FPatternLen, LMatchLen, FOptions);
        if FMatchP <> nil then
        begin
          FMatchLen := LMatchLen;
          Result := True;
          Exit;
        end;
      end;
    end
    else
    begin
      while Index <= FTextLen - FPatternLen do
      begin
        if REStrLComp(AStr + Index, FPattern, FPatternLen) = 0 then
        begin
          FMatchP := AStr + Index;
          Result := True;
          Exit;
        end;

        Low := Integer(AStr[Index + FPatternLen]) and $FF;
        Inc(Index, FSkipTable[ Low]);
        Assert(Index <= FTextLen);
      end;
    end;
  end;
end;

procedure TREQuickSearch.SetFindText(const Value: REString);
begin
  if FFindText <> Value then
  begin
    FFindText := Value;
    FPattern := PWideChar(FFindText);
    FPatternLen := Length(FFindText);
    FMatchLen := Length(FFindText);
    FCompiled := False;
  end;
end;

procedure TREQuickSearch.SetOptions(const Value: TRECompareOptions);
begin
  if FOptions <> Value then
  begin
    FOptions := Value;
    FCompiled := False;
  end;
end;

{ TRETrieNode }

procedure TRETrieNode.Clear;
begin
  FChildren.Clear;
  FParent := nil;
  FWChar := 0;
  FOptions := [];
  FAccepted := False;
end;

constructor TRETrieNode.Create(AParent: TRETrieNode; AWChar: UChar);
begin
  inherited Create;
  FChildren := TRETrieList.Create;
  FParent := AParent;
  FWChar := AWChar;
end;

destructor TRETrieNode.Destroy;
begin
  FChildren.Free;
  inherited;
end;

{ TRETrieList }

function TRETrieList.Add(const Value: TRETrieNode): Integer;
var
  Hash: Integer;
  Data, SubData, PrevData: PRETrieHashData;
begin
  FStartWChar := Min(FStartWChar, Value.WChar);
  FLastWChar := Max(FLastWChar, Value.WChar);

  Result := FList.Add(Value);

  Hash := Value.WChar mod CONST_TrieHashMax;

  if FBuckets[Hash] = nil then
  begin
    New(Data);
    Data.Index := Result;
    Data.Next := nil;
    FBuckets[Hash] := Data;
  end
  else
  begin
    SubData := FBuckets[Hash];

    repeat
      if SubData^.Index = Result then
        Exit;
      PrevData := SubData;
      SubData := SubData^.Next;
    until SubData = nil;

    New(Data);
    Data.Index := Result;
    Data.Next := nil;
    PrevData.Next := Data;
  end;
end;

procedure TRETrieList.Clear;
var
  I: Integer;
  Data, SubData: PRETrieHashData;
  Node: TRETrieNode;
begin
  for I := 0 to CONST_TrieHashMax - 1 do
  begin
    Data := FBuckets[I];
    while Data <> nil do
    begin
      SubData := Data.Next;
      Dispose(Data);
      Data := SubData;
    end;
    FBuckets[I] := nil;
  end;
  for I := 0 to FList.Count - 1 do
  begin
    Node := FList[I];
    if Assigned(Node) then
      Node.Free;
  end;
  FList.Clear;

  FStartWChar := 0;
  FLastWChar := $10FFFF;
  FOptions := [];
  inherited;
end;

function TRETrieList.Count: Integer;
begin
  Result := FList.Count;
end;

constructor TRETrieList.Create;
begin
  inherited Create;
  FList := TList.Create;
  FStartWChar := 0;
  FLastWChar := $10FFFF;
end;

destructor TRETrieList.Destroy;
begin
  Clear;
  FList.Free;
  inherited;
end;

function TRETrieList.Find(const WChar: UChar): Integer;
var
  Hash: Integer;
  Data: PRETrieHashData;
begin
  Result := -1;
  if (WChar < FStartWChar) or (WChar > FLastWChar) then
    Exit;

  Hash := WChar mod CONST_TrieHashMax;

  Data := FBuckets[Hash];

  while Data <> nil do
  begin
    if TRETrieNode(FList[Data.Index]).WChar = WChar then
    begin
      Result := Data.Index;
      Exit;
    end;

    Data := Data.Next;
  end;
end;

function TRETrieList.Get(Index: Integer): TRETrieNode;
begin
  Result := FList[Index]
end;

{ TREACSearch }

procedure TREACSearch.Add(ACode: TRECode);
begin
  FCodeList.Add(ACode);
  if ACode is TRELiteralCode then
    FOptions := FOptions +
      (ACode as TRELiteralCode).FCompareOptions;
end;

procedure TREACSearch.Add(const S: REString; AOptions: TRECompareOptions);
var
  T: REString;
begin
  T := S;
  if coIgnoreCase in AOptions then
    T := ToFoldCase(T, coASCIIOnly in AOptions);
{$IFDEF JapaneseExt}
  if coIgnoreWidth in AOptions then
    T := ToWide(T);
  if coIgnoreKana in AOptions then
    T := ToKatakana(T);
{$ENDIF JapaneseExt}

  InternalAdd(T, AOptions);
end;

procedure TREACSearch.Clear;
begin
  FCodeList.Clear;
  FRoot.Clear;
  FOptions := [];
  FCompiled := False;
  FStartP := nil;
  FEndP := nil;
end;

procedure TREACSearch.Compile;
var
  I: Integer;
begin
  if not Compiled then
  begin
    for I := 0 to FCodeList.Count - 1 do
    begin
      if FCodeList[I] is TRELiteralCode then
        Add((FCodeList[I] as TRELiteralCode).FStrings,
          (FCodeList[I] as TRELiteralCode).FCompareOptions)
    end;

    MakeFailure(FRoot);
    FCompiled := True;
  end;
end;

constructor TREACSearch.Create;
begin
  inherited Create;
  FRoot := TRETrieNode.Create(nil, 0);
  FCodeList := TObjectList.Create;
  FCodeList.OwnsObjects := False;
end;

destructor TREACSearch.Destroy;
begin
  Clear;
  FCodeList.Free;
  FRoot.Free;
  inherited;
end;

function TREACSearch.Exec(AStr: PWideChar; ATextLen: Integer): Boolean;
begin
  if not Compiled then
    Compile;

  FMatchTopP := AStr;
  FMatchEndP := AStr + ATextLen;

  Result := Match(AStr);
end;


function TREACSearch.ExecNext: Boolean;
var
  P: PWideChar;
begin
  Assert(FStartP <> nil, 'bug: Not call TREACSearch.Exec function.');

  if FSkipP <> nil then
  begin
    P := FSkipP;
    FSkipP := nil;
  end
  else
  begin
    P := FStartP;
    CharNext(P);
  end;

  Result := Match(P);
end;

function TREACSearch.Go(ANode: TRETrieNode; Ch: UChar): TRETrieNode;
var
  Index: Integer;
begin
  Result := nil;
  if ANode = nil then
    Exit;

  Index := ANode.Children.Find(Ch);
  if Index <> -1 then
    Result := ANode.Children[Index];
end;

function TREACSearch.InternalAdd(const S: REString;
  AOptions: TRECompareOptions): TRETrieNode;
var
  I, L, Len, Index: Integer;
  Node, NewNode: TRETrieNode;
  LFold: TUnicodeMultiChar;
  Ch: UChar;
begin
  Node := FRoot;
  Node.Options := FOptions;

  I := 1;
  L := Length(S);

  while I <= L do
  begin
    ClearUnicodeMultiChar(LFold);
    Ch := GetREChar(@S[I], Len, Node.Options, LFold);
    Index := Node.Children.Find(Ch);
    if Index = -1 then
    begin
      NewNode := TRETrieNode.Create(Node, Ch);
      Node.Options := FOptions;
      Node.Children.Add(NewNode);
      Node := NewNode;
    end
    else
      Node := Node.Children[Index];

    if I = 1 then
      Node.IsHead := True;
    Inc(I, Len);
  end;
  Node.Accepted := True;
  Node.Options := AOptions;
  Node.SourceString := S;
  Result := Node;
end;

function TREACSearch.GetMatchString: REString;
begin
  SetString(Result, FStartP, FEndP - FStartP);
end;

procedure TREACSearch.MakeFailure(ANode: TRETrieNode);

  function GetFailure(ANode: TRETrieNode; const Ch: UChar): TRETrieNode;
  var
    SubNode: TRETrieNode;
  begin
    if Go(ANode.Parent, Ch) = nil then
      Result := GetFailure(ANode.Parent.Parent, Ch)
    else
    begin
      Result := ANode.Parent;

      SubNode := Go(FRoot, Ch);
      if (SubNode <> nil) and SubNode.Accepted then
      begin
//        ANode.Accepted := True;
        ANode.IsHead := SubNode.IsHead;
      end;
    end;
  end;

var
  I: Integer;
  CurNode: TRETrieNode;
  Ch: UChar;
begin
  for I := 0 to ANode.Children.Count - 1 do
  begin
    CurNode := ANode.Children[I];
    Ch := CurNode.WChar;
    CurNode.Failure := GetFailure(CurNode, Ch);
    MakeFailure(CurNode);
  end;
end;

function TREACSearch.Match(AStr: PWideChar): Boolean;
begin
  Result := False;
  FStartP := nil;
  FEndP := nil;
  while AStr < FMatchEndP do
  begin
    if MatchCore(AStr) then
    begin
      Result := True;
      Exit;
    end;
    CharNext(AStr);
  end;
end;

function TREACSearch.MatchCore(AStr: PWideChar): Boolean;
var
  L, K, Index: Integer;
  Node: TRETrieNode;
  Ch: UChar;
  LFold: TUnicodeMultiChar;
begin
  ClearUnicodeMultiChar(LFold);

  FStartP := AStr;
  FEndP := nil;
  Node := FRoot;

  while AStr < FMatchEndP do
  begin
    Ch := GetREChar(AStr, L, Node.Options, LFold);
    Index := -1;
    while (Node <> nil) do
    begin
      Index := Node.Children.Find(Ch);
      if Index = -1 then
        Node := Node.Failure
      else
        Break;
    end;

    if Node <> nil then
    begin
      if (FEndP <> nil) and Node.IsHead then
        Break;

      Node := Node.Children[Index];

      Inc(AStr, L);

      if Node.Accepted then
      begin
        if FRoot.Options <> [] then
        begin
          if RECompareString(FStartP,
              PWideChar(Node.SourceString),
              System.Length(Node.SourceString), K, Node.Options) = 0 then
          begin
            FEndP := AStr;
            Break;
          end;
        end
        else
          FEndP := AStr;
      end;
    end
    else
      Break;
  end;
  Result := FEndP <> nil;
end;

function TREACSearch.GetMatchLength: Integer;
begin
  Result := FEndP - FStartP;
end;

{ TRECharMap }

function TRECharMap.Add(Ch: UChar; AOptions: TRECompareOptions): Integer;
var
  LStr, SubStr: REString;
begin
  Result := 1;

  InternalAdd(Ch);

  LStr := UCharToString(Ch);

  if AOptions = [] then
  begin
    InternalAdd(ToUChar(@LStr[1]));
    Inc(FCount);
    Exit;
  end;

  if coIgnoreCase in AOptions then
  begin
    FIgnoreCase := True;
    FASCIIOnly := coASCIIOnly in AOptions;
    SubStr := ToFoldCase(LStr, FASCIIOnly);
    Result := Length(SubStr);
    InternalAdd(ToUChar(@SubStr[1]));
    Inc(FCount);
  end;

{$IFDEF JapaneseExt}
  if coIgnoreWidth in AOptions then
  begin
    SubStr := ToWide(LStr);
    if SubStr <> LStr then
    begin
      InternalAdd(ToUChar(@SubStr[1]));
      Inc(FCount);
    end;
    SubStr := ToHalf(LStr);
    if SubStr <> LStr then
    begin
      InternalAdd(ToUChar(@SubStr[1]));
      Inc(FCount);
    end;
  end;

  if coIgnoreKana in AOptions then
  begin
    SubStr := ToHiragana(LStr);
    if SubStr <> LStr then
    begin
      InternalAdd(ToUChar(@SubStr[1]));
      Inc(FCount);
    end;
    SubStr := ToKatakana(LStr);
    if SubStr <> LStr then
    begin
      InternalAdd(ToUChar(@SubStr[1]));
      Inc(FCount);
    end;
  end;
{$ENDIF JapaneseExt}
end;

procedure TRECharMap.Add(AMap: TRECharMap);
var
  I: Integer;
  Source, Next: PRECharMapRec;
begin
  for I := Low(AMap.FASCIIMap) to High(AMap.FASCIIMap) do
    FASCIIMap[I] := FASCIIMap[I] or AMap.FASCIIMap[I];

  for I := 0 to High(AMap.FMap) do
  begin
    if AMap.FMap[I] <> nil then
    begin
      Source := AMap.FMap[I];
      Add(Source.Ch, []);
      Next := Source.Next;
      while Next <> nil do
      begin
        Source := Next;
        Add(Source.Ch, []);
        Next := Source.Next;
      end;
    end;
  end;

  if AMap.FHasUnicode and not FHasUnicode then
    FHasUnicode := True;
  if AMap.FIgnoreCase and not FIgnoreCase then
    FIgnoreCase := True;
  if AMap.FASCIIOnly and not FASCIIOnly then
    FASCIIOnly := True;

  Inc(FCount, AMap.FCount);
end;

procedure TRECharMap.Assign(Source: TRECharMap);
begin
  Clear;
  FMap := Source.FMap;
  FASCIIMap := Source.FASCIIMap;
  FCount := Source.FCount;
end;

procedure TRECharMap.Clear;
var
  I: Integer;
  P, Next: PRECharMapRec;
begin
  for I := 0 to High(FMap) do
  begin
    if FMap[I] <> nil then
    begin
      P := FMap[I];
      Next := P.Next;
      Dispose(P);
      while Next <> nil do
      begin
        P := Next;
        Next := P.Next;
        Dispose(P);
      end;
    end;
    FMap[I] := nil;
  end;
  for I := 0 to High(FASCIIMap) do
    FASCIIMap[I] := 0;

  FHasUnicode := False;
  FIgnoreCase := False;
  FASCIIOnly := False;
  FCount := 0;
end;

function TRECharMap.Count: Integer;
begin
  Result := FCount;
end;

constructor TRECharMap.Create;
begin
  inherited Create;
  FCount := 0;
  FHasUnicode := False;
  FIgnoreCase := False;
  FASCIIOnly := False;
end;

{$IFDEF SKREGEXP_DEBUG}

function TRECharMap.GetDebugStr: REString;

  function BuidStr(Start, Last, Prev: UChar): REString;
  var
    StartS, LastS: REString;
  begin
    if Last - Start > 0 then
    begin
      if IsCntrlA(Start) then
        StartS := Format('($%x)', [Start])
      else
      begin
        StartS := Format('%s($%x)', [UCharToString(Start), Start]);
      end;

      if IsCntrlA(Last) then
        LastS := Format('($%x)', [Last])
      else
      begin
        LastS := Format('%s($%x)', [UCharToString(Last), Last]);
      end;

      Result := Format('%s-%s', [StartS, LastS]);
    end
    else
    begin
      if IsCntrlA(Prev) then
        StartS := Format('($%x)', [Prev])
      else
      begin
        StartS := Format('%s($%x)', [UCharToString(Prev), Prev]);
      end;

      Result := Format('%s', [StartS, LastS]);
    end;
  end;

var
  I: Integer;
  SL: TREStringList;
  C, Prev, Start, Last: UChar;
begin
  SL := TREStringList.Create;
  try
    MapToList(SL);

    if SL.Count > 0 then
    begin
      Start := ToUChar(PWideChar(SL[0]));
      Last := Start;
      Prev := Start;

      for I := 1 to SL.Count - 1 do
      begin
        C := ToUChar(PWideChar(SL[I]));
        if C = Prev + 1 then
        begin
          Last := C;
          Prev := C;
        end
        else
        begin
          Result := Result + BuidStr(Start, Last, Prev) + ' ';
          Start := C;
          Last := C;
          Prev := C;
        end;
      end;
      Result := Result + BuidStr(Start, Last, Prev) + ' ';
    end;

  finally
    SL.Free;
  end;
end;
{$ENDIF SKREGEXP_DEBUG}

destructor TRECharMap.Destroy;
begin
  Clear;
  inherited;
end;

function TRECharMap.IsExists(AStr: PWideChar): Boolean;
var
  Ch: Cardinal;
  P: PRECharMapRec;
  FD: TUnicodeMultiChar;
begin
  Result := False;

  if FIgnoreCase then
  begin
    if AStr^ < #$80 then
    begin
      if (AStr^ >= 'A') and (AStr^ <= 'Z') then
        Ch := UChar(AStr^) xor $20
      else
        Ch := UChar(AStr^);
{$IFDEF USE_UNICODE_PROPERTY}
    end
    else
    begin
      Ch := ToUChar(AStr);
      FD := GetUnicodeFoldCase(Ch);
      Ch := FD[FD[0]];
{$ENDIF USE_UNICODE_PROPERTY}
    end;
  end
  else
  begin
    if AStr^ < #$80 then
      Ch := UChar(AStr^)
    else
      if IsNoLeadChar(AStr^) then
        Ch := UChar(AStr^)
      else
        Ch := ToUChar(AStr);
  end;

  if Ch <= $FF then
  begin
    Result := (FASCIIMap[Ch div 8] and (1 shl (Ch mod 8))) <> 0;
  end
  else
  begin
    if not FHasUnicode then
      Exit;

    if (Ch < FStartChar) or (Ch > FLastChar) then
      Exit;

    P := FMap[Ord(Ch) mod CONST_CharMapMax];
    if P = nil then
      Exit;

    if P.Ch = Ch then
    begin
      Result := True;
      Exit;
    end;

    P := P.Next;
    while P <> nil do
    begin
      if P.Ch = Ch then
      begin
        Result := True;
        Exit;
      end;
      P := P.Next;
    end;
  end;
end;

procedure TRECharMap.InternalAdd(Ch: UChar);
var
  P, Prev: PRECharMapRec;
  Index: Integer;
begin
  FStartChar := Min(FStartChar, Ch);
  FLastChar := Max(FLastChar, Ch);

  if Ch <= $FF then
  begin
    FASCIIMap[Ch div 8] := FASCIIMap[Ch div 8] or (1 shl (Ch mod 8))
  end
  else
  begin
    if not FHasUnicode then
      FHasUnicode := True;

    Index := Ord(Ch) mod CONST_CharMapMax;

    P := FMap[Index];
    if P = nil then
    begin
      New(P);
      P^.Ch := Ch;
      P^.Next := nil;
      FMap[Index] := P;
    end
    else
    begin
      repeat
        if P^.Ch = Ch then
          Exit;
        Prev := P;
        P := P.Next;
      until P = nil;

      New(P);
      P^.Ch := Ch;
      P^.Next := nil;
      Prev.Next := P;
    end;
  end;
end;

{$IFDEF SKREGEXP_DEBUG}
procedure TRECharMap.MapToList(ADest: TREStrings);
var
  I: Integer;
  P, Next: PRECharMapRec;
begin
  ADest.Clear;

  for I := 0 to 255 do
  begin
    if (FASCIIMap[I div 8] and (1 shl (I mod 8))) <> 0 then
      ADest.Add(UCharToString(I));
  end;

  for I := 0 to CONST_CharMapMax - 1 do
  begin
    if FMap[I] <> nil then
    begin
      P := FMap[I];
      ADest.Add(UCharToString(P.Ch));
      Next := P.Next;
      while Next <> nil do
      begin
        P := Next;
        ADest.Add(UCharToString(P.Ch));
        Next := P.Next;
      end;
    end;
  end;
end;
{$ENDIF SKREGEXP_DEBUG}

{ TRECode }

function TRECode.CompareCode(Source: TRECode): Integer;
begin
  Result := 0;
end;

constructor TRECode.Create(ARegExp: TSkRegExp);
begin
  inherited Create;
  FRegExp := ARegExp;
end;

function TRECode.ExecRepeat(var AStr: PWideChar; AMin, AMax: Integer): Boolean;
var
  I: Integer;
  Len: Integer;
begin
  Result := False;

  for I := 1 to AMin do
  begin
    if (AStr < FRegExp.FMatchEndP) and IsEqual(AStr, Len) then
      Inc(AStr, Len)
    else
      Exit;
  end;

  Result := True;
  if AMin = AMax then
    Exit;

  AMax := AMax - AMin;

  for I := 1 to AMax do
  begin
    if (AStr < FRegExp.FMatchEndP) and IsEqual(AStr, Len) then
      Inc(AStr, Len)
    else
      Break;
  end;
end;

function TRECode.Find(AStr: PWideChar): PWideChar;
begin
  Result := nil;
end;

function TRECode.IsAny: Boolean;
begin
  Result := False;
end;

function TRECode.IsEqual(AStr: PWideChar; var Len: Integer): Boolean;
begin
  Result := False;
end;

function TRECode.ExecRepeat(var AStr: PWideChar; IsStar: Boolean): Boolean;
var
  StartP: PWideChar;
  Len: Integer;
begin
  Result := IsStar;
  StartP := AStr;

  while (AStr < FRegExp.FMatchEndP) and IsEqual(AStr, Len) do
    Inc(AStr, Len);

  if not Result then
    Result := AStr - StartP > 0;
end;

function TRECode.GetCharLength: TRETextPosRec;
begin
  Result.Min := 1;
  Result.Max := 1;
end;

{$IFDEF SKREGEXP_DEBUG}

function TRECode.GetDebugStr: REString;
begin

end;
{$ENDIF}

function TRECode.GetLength: Integer;
begin
  Result := 1;
end;

function TRECode.GetSearch: TREQuickSearch;
begin
  Result := nil;
end;

function TRECode.IsInclude(ACode: TRECode): Boolean;
begin
  Result := False;
end;

function TRECode.IsOverlap(ACode: TRECode): Boolean;
begin
  Result := True;
end;

function TRECode.IsVariable: Boolean;
begin
  Result := False;
end;

{ TRELiteralCode }

constructor TRELiteralCode.Create(ARegExp: TSkRegExp; Str: UCS4String;
  AOptions: TREOptions);
begin
  Create(ARegExp, RECharArrayToString(Str), AOptions);
end;

constructor TRELiteralCode.Create(ARegExp: TSkRegExp; Str: REString;
  AOptions: TREOptions);
var
  I: Integer;
begin
  inherited Create(ARegExp);
  FStrings := Str;
  FOptions := AOptions;
  FCompareOptions := REOptionsToRECompareOptions(FOptions);

  if coIgnoreCase in FCompareOptions then
    FStrings := ToFoldCase(FStrings, coASCIIOnly in FCompareOptions);

{$IFDEF JapaneseExt}
  if coIgnoreWidth in FCompareOptions then
    FStrings := ToWide(FStrings);

  if coIgnoreKana in FCompareOptions then
    FStrings := ToKatakana(FStrings);
{$ENDIF JapaneseExt}

  FSubP := PWideChar(FStrings);
  FLength := System.Length(FStrings);
  FSubP := PWideChar(FStrings);

  FSearch := TREQuickSearch.Create;
  FSearch.FindText := FStrings;
  FSearch.Options := FCompareOptions;

  FCharLength := 0;
  for I := 1 to System.Length(FStrings) do
  begin
    if IsLeadChar(FStrings[I]) then
      Dec(FCharLength);
    Inc(FCharLength);
  end;
end;

constructor TRELiteralCode.Create(ARegExp: TSkRegExp; AWChar: UChar;
  AOptions: TREOptions);
begin
  Create(ARegExp, UCharToString(AWChar), AOptions);
end;

destructor TRELiteralCode.Destroy;
begin
  FSearch.Free;
  inherited;
end;

function TRELiteralCode.ExecRepeat(var AStr: PWideChar;
  IsStar: Boolean): Boolean;
var
  StartP: PWideChar;
  Len, MatchLen: Integer;
begin
  Result := IsStar;
  StartP := AStr;

  if (roIgnoreKana in FOptions) or (roIgnoreWidth in FOptions) then
  begin
    while REStrLJComp(AStr, PWideChar(FStrings), FLength,
        MatchLen, FCompareOptions) = 0 do
      Inc(AStr, MatchLen);
  end
  else if roIgnoreCase in FOptions then
  begin
    if roASCIIOnly in FOptions then
    begin
      while REStrLIAComp(AStr, PWideChar(FStrings), FLength) = 0 do
        Inc(AStr, FLength);
    end
    else
    begin
      while REStrLIComp(AStr, PWideChar(FStrings), FLength, MatchLen) = 0 do
        Inc(AStr, MatchLen);
    end;
  end
  else
  begin
    if FLength = 1 then
    begin
      while AStr^ = PWideChar(FStrings)^ do
        Inc(AStr);
    end
    else
    begin
      while REStrLComp(AStr, PWideChar(FStrings), FLength) = 0 do
        Inc(AStr, FLength);
    end
  end;

  Len := AStr - StartP;
  if not Result then
    Result := Len > 0;
end;

function TRELiteralCode.Find(AStr: PWideChar): PWideChar;
var
  L: Integer;
begin
  L := FRegExp.FMatchEndP - AStr;

  if FSearch.Exec(AStr, L) then
    Result := FSearch.MatchP
  else
    Result := nil;
end;

function TRELiteralCode.GetCharLength: TRETextPosRec;
begin
  Result.Min := FCharLength;
  Result.Max := FCharLength;
end;

{$IFDEF SKREGEXP_DEBUG}

function TRELiteralCode.GetDebugStr: REString;
var
  I: Integer;
  S, T: REString;
  IsW: Boolean;
begin
  I := 1;
  while I <= System.Length(FStrings) do
  begin
    IsW := IsLeadChar(FStrings[I]);

    if not IsCntrlU(ToUChar(@FStrings[I])) then
    begin
      if IsW then
      begin
        S := S + FStrings[I];
        Inc(I);
      end;
      S := S + FStrings[I];
    end
    else
    begin
      if IsW then
      begin
        S := S + '\x' + IntToHex(Ord(FStrings[I]), 2);
        Inc(I);
      end;
      S := S + '\x' + IntToHex(Ord(FStrings[I]), 2);
    end;
    Inc(I);
  end;

  T := '';
  if roIgnoreCase in FOptions then
  begin
    T := T + 'F';
    if roASCIIOnly in FOptions then
      T := T + 'A';
  end;
  if roIgnoreWidth in FOptions then
    T := T + 'W';
  if roIgnoreKana in FOptions then
    T := T + 'K';

  Result := Format(sLiteral, [T, S]);
end;
{$ENDIF SKREGEXP_DEBUG}

function TRELiteralCode.GetLength: Integer;
begin
  Result := FLength;
end;

function TRELiteralCode.GetSearch: TREQuickSearch;
begin
  Result := FSearch;
end;

function TRELiteralCode.IsEqual(AStr: PWideChar; var Len: Integer): Boolean;
begin
  Result := False;
  Len := 0;

  if AStr = FRegExp.FMatchEndP then
    Exit;

  Result := RECompareString(AStr, FSubP, FLength, Len, FCompareOptions) = 0;
end;

function TRELiteralCode.IsInclude(ACode: TRECode): Boolean;
begin
  Result := False;

  if not (ACode is TRELiteralCode) then
    Exit;

  Result := (ACode as TRELiteralCode).FStrings = FStrings;
end;

function TRELiteralCode.IsOverlap(ACode: TRECode): Boolean;
var
  S: REString;
  LOptions: TRECompareOptions;
  L, MatchLen: Integer;
begin
  if ACode.CharLength.Min <= 0 then
    Result := True
  else
  begin
    if ACode is TRELiteralCode then
    begin
      S := (ACode as TRELiteralCode).FStrings;
      LOptions := (ACode as TRELiteralCode).FCompareOptions;
      L := Min(System.Length(S), FLength);
      Result := RECompareString(FSubP, PWideChar(S), L, MatchLen, LOptions) = 0;
    end
    else
    begin
      Result := ACode.IsEqual(FSubP, L);
    end
  end;
end;

{ TREAnyCharCode }

constructor TREAnyCharCode.Create(ARegExp: TSkRegExp; AOptions: TREOptions);
begin
  inherited Create(ARegExp);
  FOptions := AOptions;
  FIsMatchAll := roSingleLine in FOptions;
end;

function TREAnyCharCode.IsAny: Boolean;
begin
  Result := True;
end;

function TREAnyCharCode.IsEqual(AStr: PWideChar; var Len: Integer): Boolean;
var
  StartP: PWideChar;
begin
  Result := False;
  Len := 0;

  if FRegExp.FMatchEndP = AStr then
    Exit;

  StartP := AStr;

  if FIsMatchAll then
  begin
    Result := True;

    if IsLeadChar(AStr^) then
      Inc(AStr, 2)
    else
      Inc(AStr);
  end
  else
  begin
    if FRegExp.IsLineBreak(AStr) = 0 then
    begin
      if IsLeadChar(AStr^) then
        Inc(AStr, 2)
      else
        Inc(AStr);

      Result := True;
    end;
  end;
  if Result then
    Len := AStr - StartP;
end;

function TREAnyCharCode.ExecRepeat(var AStr: PWideChar;
  IsStar: Boolean): Boolean;
var
  StartP: PWideChar;
begin
  Result := IsStar;

  StartP := AStr;

  if roSingleLine in FOptions then
  begin
    AStr := FRegExp.FMatchEndP;
  end
  else
  begin
    while (AStr <> FRegExp.FMatchEndP) and (FRegExp.IsLineBreak(AStr) = 0) do
      Inc(AStr);
  end;

  if not Result then
    Result := AStr - StartP > 0;
end;

{$IFDEF SKREGEXP_DEBUG}

function TREAnyCharCode.GetDebugStr: REString;
begin
  Result := sAnyChar;
end;
{$ENDIF}

function TREAnyCharCode.IsInclude(ACode: TRECode): Boolean;
begin
  if ACode is TREAnyCharCode then
    Result := True
  else
    Result := False;
end;

{ TREWordCharCode }

function TREWordCharCode.CompareCode(Dest: TRECode): Integer;
begin
  if Dest is TREWordCharCode then
  begin
    Result := 0;
  end
  else
    Result := -1;
end;

constructor TREWordCharCode.Create(ARegExp: TSkRegExp; AOptions: TREOptions;
  ANegative: Boolean);
begin
  inherited Create(ARegExp);
  FOptions := AOptions;
  FNegative := ANegative;
  FIsASCII := GetASCIIMode(FOptions);
end;

function TREWordCharCode.IsEqual(AStr: PWideChar; var Len: Integer): Boolean;
begin
  Result := False;
  Len := 0;

  if AStr = FRegExp.FMatchEndP then
    Exit;

  if AStr^ < #128 then
  begin
    Result := IsWordA(UChar(AStr^));
    if Result then
      Len := 1;
  end
  else
  begin
    if not FIsASCII then
      Result := IsWordU(ToUChar(AStr, Len));
  end;

  if FNegative then
    Result := not Result;

  if not Result then
    Len := 0;
end;

{$IFDEF SKREGEXP_DEBUG}

function TREWordCharCode.GetDebugStr: REString;
begin
  if not FNegative then
    Result := sWordChar
  else
    Result := sNegativeWordChar;
end;
{$ENDIF}

function TREWordCharCode.IsInclude(ACode: TRECode): Boolean;
begin
  if ACode is TREWordCharCode then
    Result := (ACode as TREWordCharCode).FNegative = FNegative
  else if ACode is TREDigitCharCode then
  begin
    if FNegative then
      Result := (ACode as TREDigitCharCode).FNegative
    else
      Result := not(ACode as TREDigitCharCode).FNegative;
  end
  else
    Result := False;
end;

function TREWordCharCode.IsOverlap(ACode: TRECode): Boolean;
begin
  if ACode.CharLength.Min <= 0 then
    Result := True
  else
  begin
    if (ACode is TRELiteralCode) then
    begin
      if FIsASCII then
        Result := (PWideChar((ACode as TRELiteralCode).FStrings) < #128) and
          IsWordA(ToUChar(PWideChar((ACode as TRELiteralCode).FStrings)))
      else
        Result := IsWordU(ToUChar(PWideChar((ACode as TRELiteralCode).FStrings)));

      if FNegative then
        Result := not Result;
    end
    else
      Result := True;
  end;
end;

{ TREDigitCharCode }

function TREDigitCharCode.CompareCode(Dest: TRECode): Integer;
begin
  if Dest is TREWordCharCode then
  begin
    Result := 1;
  end
  else if Dest is TREDigitCharCode then
  begin
    Result := 0;
  end
  else
    Result := -1;
end;

constructor TREDigitCharCode.Create(ARegExp: TSkRegExp; AOptions: TREOptions;
  ANegative: Boolean);
begin
  inherited Create(ARegExp);
  FOptions := AOptions;
  FNegative := ANegative;
  FIsASCII := GetASCIIMode(FOptions)
end;

function TREDigitCharCode.ExecRepeat(var AStr: PWideChar;
  IsStar: Boolean): Boolean;
var
  StartP: PWideChar;
  Len: Integer;
  Ch: UChar;
begin
  Result := IsStar;
  StartP := AStr;

  if not FNegative then
  begin
    if FIsASCII then
    begin
      while (AStr < FRegExp.FMatchEndP) do
      begin
        if (AStr^ < #128) and IsDigitA(UChar(AStr^)) then
          Inc(AStr)
        else
          Break;
      end
    end
    else
    begin
      Ch := ToUChar(AStr, Len);

      while (AStr < FRegExp.FMatchEndP) do
      begin
        if IsDigitU(Ch) then
        begin
          Inc(AStr, Len);
          Ch := ToUChar(AStr, Len);
        end
        else
          Break;
      end;
    end;
  end
  else
  begin
    if FIsASCII then
    begin
      while (AStr < FRegExp.FMatchEndP) do
      begin
        if (AStr^ < #128) and not IsDigitA(UChar(AStr^)) then
          Inc(AStr)
        else
          Break;
      end
    end
    else
    begin
      Ch := ToUChar(AStr, Len);

      while (AStr < FRegExp.FMatchEndP) do
      begin
        if not IsDigitU(Ch) then
        begin
          Inc(AStr, Len);
          Ch := ToUChar(AStr, Len);
        end
        else
          Break;
      end;
    end;
  end;

  if not Result then
    Result := AStr - StartP > 0;
end;

function TREDigitCharCode.IsEqual(AStr: PWideChar; var Len: Integer): Boolean;
begin
  Result := False;
  Len := 0;

  if AStr = FRegExp.FMatchEndP then
    Exit;

  if AStr^ < #128 then
  begin
    Result := IsDigitA(UChar(AStr^));
    if Result then
      Len := 1;
  end
  else
  begin
    if not FIsASCII then
      Result := IsDigitU(ToUChar(AStr, Len));
  end;

  if FNegative then
    Result := not Result;

  if not Result then
    Len := 0;
end;

{$IFDEF SKREGEXP_DEBUG}

function TREDigitCharCode.GetDebugStr: REString;
begin
  if not FNegative then
    Result := sDigitChar
  else
    Result := sNegativeDigitChar;
end;
{$ENDIF}

function TREDigitCharCode.IsInclude(ACode: TRECode): Boolean;
begin
  if ACode is TREDigitCharCode then
    Result := FNegative and (ACode as TREDigitCharCode).FNegative
  else
    Result := False;
end;

function TREDigitCharCode.IsOverlap(ACode: TRECode): Boolean;
begin
  if ACode.CharLength.Min <= 0 then
    Result := True
  else
  begin
    if (ACode is TRELiteralCode) then
    begin
      if FIsASCII then
        Result := (PWideChar((ACode as TRELiteralCode).FStrings) < #128) and
          IsDigitA(ToUChar(PWideChar((ACode as TRELiteralCode).FStrings)))
      else
        Result := IsDigitU(ToUChar(PWideChar((ACode as TRELiteralCode).FStrings)));

      if FNegative then
        Result := not Result;
    end
    else
      Result := True;
  end;
end;

{ TRESpaceCharCode }

function TRESpaceCharCode.CompareCode(Dest: TRECode): Integer;
begin
  if (Dest is TRESpaceCharCode) then
  begin
    Result := 0;
  end
  else
    Result := 1;
end;

constructor TRESpaceCharCode.Create(ARegExp: TSkRegExp; AOptions: TREOptions;
  ANegative: Boolean);
begin
  inherited Create(ARegExp);
  FOptions := AOptions;
  FNegative := ANegative;
  FIsASCII := GetASCIIMode(FOptions);
end;

function TRESpaceCharCode.IsEqual(AStr: PWideChar; var Len: Integer): Boolean;
begin
  Result := False;
  Len := 0;

  if AStr = FRegExp.FMatchEndP then
    Exit;

  if AStr^ < #128 then
  begin
    Result := IsSpacePerlA(UChar(AStr^));
    if Result then
      Len := 1;
  end
  else
  begin
    if not FIsASCII then
      Result := IsSpacePerlU(ToUChar(AStr, Len));
  end;

  if FNegative then
    Result := not Result;

  if not Result then
    Len := 0;
end;

{$IFDEF SKREGEXP_DEBUG}

function TRESpaceCharCode.GetDebugStr: REString;
begin
  if not FNegative then
    Result := sSpaceChar
  else
    Result := sNegativeSpaceChar;
end;
{$ENDIF}

function TRESpaceCharCode.IsInclude(ACode: TRECode): Boolean;
begin
  if ACode is TRESpaceCharCode then
    Result := FNegative = (ACode as TRESpaceCharCode).FNegative
  else
    Result := False;
end;

function TRESpaceCharCode.IsOverlap(ACode: TRECode): Boolean;
begin
  if ACode.CharLength.Min <= 0 then
    Result := True
  else
  begin
    if (ACode is TRELiteralCode) then
    begin
      if FIsASCII then
        Result := (PWideChar((ACode as TRELiteralCode).FStrings) < #128) and
          IsSpacePerlA(ToUChar(PWideChar((ACode as TRELiteralCode).FStrings)))
      else
        Result :=
          IsSpacePerlU(ToUChar(PWideChar((ACode as TRELiteralCode).FStrings)));

      if FNegative then
        Result := not Result;
    end
    else
      Result := True;
  end;
end;

{ TRHorizontalSpaceCharCode }

function TREHorizontalSpaceCharCode.CompareCode(Dest: TRECode): Integer;
begin
  if (Dest is TRESpaceCharCode) then
  begin
    Result := -1;
  end
  else if (Dest is TREHorizontalSpaceCharCode) then
  begin
    Result := 0
  end
  else
    Result := 1;
end;

constructor TREHorizontalSpaceCharCode.Create(ARegExp: TSkRegExp;
  ANegative: Boolean);
begin
  inherited Create(ARegExp);
  FNegative := ANegative;
end;

{$IFDEF SKREGEXP_DEBUG}

function TREHorizontalSpaceCharCode.GetDebugStr: REString;
begin
  if not FNegative then
    Result := sHorizontalSpaceChar
  else
    Result := sNegativeHorizontalSpaceChar;
end;
{$ENDIF}

function TREHorizontalSpaceCharCode.IsEqual(AStr: PWideChar;
  var Len: Integer): Boolean;
begin
  Len := 0;
  Result := False;

  if AStr = FRegExp.FMatchEndP then
    Exit;

  Result := IsSpaceHorizontal(UChar(AStr^));
  if FNegative then
    Result := not Result;

  if Result then
    Len := 1;
end;

function TREHorizontalSpaceCharCode.IsInclude(ACode: TRECode): Boolean;
begin
  Result := ACode is TREHorizontalSpaceCharCode;
end;

function TREHorizontalSpaceCharCode.IsOverlap(ACode: TRECode): Boolean;
begin
  if ACode.CharLength.Min <= 0 then
    Result := True
  else
  begin
    if (ACode is TRELiteralCode) then
    begin
      Result := IsSpaceHorizontal
        (ToUChar(PWideChar((ACode as TRELiteralCode).FStrings)));
      if FNegative then
        Result := not Result;
    end
    else
      Result := True;
  end;
end;

{ TREVerticalSpaceCharCode }

function TREVerticalSpaceCharCode.CompareCode(Dest: TRECode): Integer;
begin
  if (Dest is TRESpaceCharCode) then
  begin
    Result := -1;
  end
  else if (Dest is TREVerticalSpaceCharCode) then
  begin
    Result := 0
  end
  else
    Result := 1;
end;

constructor TREVerticalSpaceCharCode.Create(ARegExp: TSkRegExp;
  ANegative: Boolean);
begin
  inherited Create(ARegExp);
  FNegative := ANegative;
end;

{$IFDEF SKREGEXP_DEBUG}

function TREVerticalSpaceCharCode.GetDebugStr: REString;
begin
  if not FNegative then
    Result := sVerticalSpaceChar
  else
    Result := sNegativeVerticalSpaceChar;
end;
{$ENDIF}

function TREVerticalSpaceCharCode.IsEqual(AStr: PWideChar;
  var Len: Integer): Boolean;
begin
  Len := 0;
  Result := False;

  if AStr = FRegExp.FMatchEndP then
    Exit;

  Result := IsSpaceVertical(UChar(AStr^));
  if FNegative then
    Result := not Result;

  if Result then
    Len := 1;
end;

function TREVerticalSpaceCharCode.IsInclude(ACode: TRECode): Boolean;
begin
  Result := ACode is TREVerticalSpaceCharCode;
end;

function TREVerticalSpaceCharCode.IsOverlap(ACode: TRECode): Boolean;
begin
  if ACode.CharLength.Min <= 0 then
    Result := True
  else
  begin
    if (ACode is TRELiteralCode) then
    begin
      Result := IsSpaceVertical
        (ToUChar(PWideChar((ACode as TRELiteralCode).FStrings)));
      if FNegative then
        Result := not Result;
    end
    else
      Result := True;
  end;
end;

{ TRELineBreakCharCode }

function TRELineBreakCharCode.GetCharLength: TRETextPosRec;
begin
  Result.Min := 1;
  Result.Max := 2;
end;

{$IFDEF SKREGEXP_DEBUG}

function TRELineBreakCharCode.GetDebugStr: REString;
begin
  Result := sLineBreakChar;
end;
{$ENDIF}

function TRELineBreakCharCode.GetLength: Integer;
begin
  Result := 2;
end;

function TRELineBreakCharCode.IsEqual(AStr: PWideChar;
  var Len: Integer): Boolean;
begin
  Len := 0;
  Result := False;

  if FRegExp.FMatchEndP = AStr then
    Exit;

  Len := FRegExp.IsAnyEOL(AStr);
  Result := Len > 0;
end;

function TRELineBreakCharCode.IsInclude(ACode: TRECode): Boolean;
begin
  Result := ACode is TRELineBreakCharCode;
end;

function TRELineBreakCharCode.IsOverlap(ACode: TRECode): Boolean;
begin
  Result := ACode is TRELineBreakCharCode;
end;

function TRELineBreakCharCode.IsVariable: Boolean;
begin
  Result := True;
end;

{ TRECharClassCode }

function TRECharClassCode.Add(AStartWChar, ALastWChar: UChar): Integer;
var
  C: UChar;
begin
  Result := 0;
  for C := AStartWChar to ALastWChar do
    Add(C);
end;

function TRECharClassCode.Add(AWChar: UChar): Integer;
begin
  Result := 0;
  FWChar := AWChar;

  FMap.Add(AWChar, REOptionsToRECompareOptions(FOptions));
end;

function TRECharClassCode.Add(Value: TRECode): Integer;
begin
  Result := FCodeList.Add(Value);
  if FSimpleClass then
    FSimpleClass := False;
end;

constructor TRECharClassCode.Create(ARegExp: TSkRegExp; ANegative: Boolean;
  AOptions: TREOptions);
begin
  inherited Create(ARegExp);
  FMap := TRECharMap.Create;
  FCodeList := TObjectList.Create;
  FNegative := ANegative;
  FOptions := AOptions;
  FSimpleClass := True;
end;

destructor TRECharClassCode.Destroy;
begin
  FCodeList.Free;
  FMap.Free;
  inherited;
end;

function TRECharClassCode.Find(AStr: PWideChar): PWideChar;
var
  L: Integer;
begin
  Result := nil;

  while AStr <= FRegExp.FMatchEndP do
  begin
    if IsEqual(AStr, L) then
    begin
      Result := AStr;
      Exit;
    end;
    CharNext(AStr);
  end;
end;

function TRECharClassCode.IsEqual(AStr: PWideChar; var Len: Integer): Boolean;
label
  Final;
var
  I: Integer;
begin
  Len := 0;
  Result := FNegative;

  if AStr = FRegExp.FMatchEndP then
  begin
    Result := False;
    Exit;
  end;

  if FSimpleClass then
  begin
    if FMap.IsExists(AStr) then
    begin
      Result := not Result;
      goto Final;
    end;
  end
  else
  begin
    if FMap.IsExists(AStr) then
    begin
      Result := not Result;
      goto Final;
    end;

    for I := 0 to FCodeList.Count - 1 do
    begin
      if TRECode(FCodeList[I]).IsEqual(AStr, Len) then
      begin
        Result := not Result;
        Break;
      end
    end;
  end;

Final:
  if Result then
  begin
    if IsNoLeadChar(AStr^) then
      Len := 1
    else
      Len := 2;
  end;
end;

{$IFDEF SKREGEXP_DEBUG}

function TRECharClassCode.GetDebugStr: REString;
var
  I: Integer;
begin
  if not FNegative then
    Result := sCharClass + FMap.GetDebugStr
  else
    Result := sNEGCharClass + FMap.GetDebugStr;

  for I := 0 to FCodeList.Count - 1 do
    if I > 0 then
      Result := Result + ' ' + TRECode(FCodeList[I]).GetDebugStr
    else
      Result := Result + TRECode(FCodeList[I]).GetDebugStr;

  Result := Result + ']';
end;
{$ENDIF} // SKREGEXP_DEBUG

function TRECharClassCode.IsInclude(ACode: TRECode): Boolean;
begin
  Result := False;
end;

function TRECharClassCode.IsOverlap(ACode: TRECode): Boolean;
var
  LCode: TRECode;
  I: Integer;
begin
  Result := FNegative;

  if ACode is TRELiteralCode then
  begin
    if FMap.IsExists((ACode as TRELiteralCode).FSubP) then
    begin
      Result := not Result;
      Exit;
    end;
  end;

  for I := 0 to FCodeList.Count - 1 do
  begin
    LCode := TRECode(FCodeList[I]);
    if LCode.IsOverlap(ACode) then
    begin
      Result := not Result;
      Exit;
    end;
  end;
end;

procedure TRECharClassCode.Rebuild;

  procedure RebuildSub(Index: Integer);
  var
    Source, Dest: TRECode;
    I: Integer;
  begin
    Source := TRECode(FCodeList[Index]);
    for I := FCodeList.Count - 1 downto 0 do
    begin
      if I <> Index then
      begin
        Dest := TRECode(FCodeList[I]);
        if Source.IsInclude(Dest) then
          FCodeList.Delete(I);
      end;
    end;
  end;

var
  I: Integer;
begin
  I := 0;

  while I < FCodeList.Count do
  begin
    RebuildSub(I);
    Inc(I);
  end;
  Sort;
end;

function RECompareCode(Item1, Item2: Pointer): Integer;
begin
  Result := TRECode(Item1).CompareCode(TRECode(Item2));
end;

procedure TRECharClassCode.Sort;
begin
  FCodeList.Sort(@RECompareCode);
end;

{ TRECombiningSequence }

{$IFDEF USE_UNICODE_PROPERTY}
function TRECombiningSequence.IsAny: Boolean;
begin
  Result := True;
end;

function TRECombiningSequence.IsEqual(AStr: PWideChar;
  var Len: Integer): Boolean;
var
  StartP: PWideChar;
begin
  Result := False;
  StartP := AStr;
  Len := 0;

  if AStr >= FRegExp.FMatchEndP then
    Exit;

  if not IsUnicodeProperty(ToUChar(AStr), upM) then
  begin
    Inc(AStr);
    Result := True;

    while (AStr < FRegExp.FMatchEndP) and
      IsUnicodeProperty(ToUChar(AStr), upM) do
      Inc(AStr);
  end;

  if Result then
    Len := AStr - StartP;
end;

function TRECombiningSequence.ExecRepeat(var AStr: PWideChar;
  IsStar: Boolean): Boolean;
begin
  Result := True;
  AStr := FRegExp.FMatchEndP;
end;

function TRECombiningSequence.GetCharLength: TRETextPosRec;
begin
  Result.Min := 1;
  Result.Max := -1;
end;

{$IFDEF SKREGEXP_DEBUG}

function TRECombiningSequence.GetDebugStr: REString;
begin
  Result := sCombiningSequence;
end;
{$ENDIF}

function TRECombiningSequence.IsInclude(ACode: TRECode): Boolean;
begin
  Result := ACode is TRECombiningSequence;
end;

function TRECombiningSequence.IsVariable: Boolean;
begin
  Result := True;
end;

{$ENDIF USE_UNICODE_PROPERTY}

{ TREBoundaryCode }

constructor TREBoundaryCode.Create(ARegExp: TSkRegExp; AOptions: TREOptions;
  ANegative: Boolean);
begin
  inherited Create(ARegExp);
  FOptions := AOptions;
  FNegative := ANegative;
  FIsASCII := GetASCIIMode(FOptions);
end;

function TREBoundaryCode.IsEqual(AStr: PWideChar; var Len: Integer): Boolean;
var
  PrevType, CurType: Boolean;
begin
  Len := 0;

  if not FNegative then
  begin
    if AStr = FRegExp.FMatchTopP then
      PrevType := False
    else
    begin
      Dec(AStr);

      if AStr^ < #128 then
        PrevType := IsWordA(UChar(AStr^))
      else
      begin
        if not FIsASCII then
        begin
          if IsNoLeadChar(AStr^) then
            PrevType := IsWordU(UChar(AStr^))
          else
            PrevType := IsWordU(ToUChar(AStr))
        end
        else
          PrevType := False;
      end;

      Inc(AStr);
    end;

    if AStr = FRegExp.FMatchEndP then
      CurType := False
    else
    begin
      if AStr^ < #128 then
        CurType := IsWordA(UChar(AStr^))
      else
      begin
        if not FIsASCII then
        begin
          if IsNoLeadChar(AStr^) then
            CurType := IsWordU(UChar(AStr^))
          else
            CurType := IsWordU(ToUChar(AStr))
        end
        else
          CurType := False;
      end;
    end;

    Result := PrevType <> CurType;
  end
  else
  begin
    if AStr <> FRegExp.FMatchTopP then
    begin
      Dec(AStr);

      if AStr^ < #128 then
        PrevType := not IsWordA(UChar(AStr^))
      else
      begin
        if not FIsASCII then
        begin
          if IsNoLeadChar(AStr^) then
            PrevType := not IsWordU(UChar(AStr^))
          else
            PrevType := not IsWordU(ToUChar(AStr));
        end
        else
          PrevType := True;
      end;

      Inc(AStr);
    end
    else
      PrevType := True;

    if AStr <> FRegExp.FMatchEndP then
    begin
      if AStr^ < #128 then
        CurType := not IsWordA(UChar(AStr^))
      else
      begin
        if not FIsASCII then
        begin
          if IsNoLeadChar(AStr^) then
            CurType := not IsWordU(UChar(AStr^))
          else
            CurType := not IsWordU(ToUChar(AStr))
        end
        else
          CurType := True;
      end;
    end
    else
      CurType := True;

    Result := PrevType = CurType;
  end;
end;

function TREBoundaryCode.GetCharLength: TRETextPosRec;
begin
  Result.Min := 0;
  Result.Max := 0;
end;

{$IFDEF SKREGEXP_DEBUG}

function TREBoundaryCode.GetDebugStr: REString;
begin
  if not FNegative then
    Result := sBoundaryCode
  else
    Result := sNegativeBoundaryCode;
end;
{$ENDIF}

function TREBoundaryCode.IsInclude(ACode: TRECode): Boolean;
begin
  Result := ACode is TREBoundaryCode;
end;

{ TREReferenceCode }

constructor TREReferenceCode.Create(ARegExp: TSkRegExp; AGroupIndex: Integer;
  AOptions: TREOptions);
begin
  inherited Create(ARegExp);
  FGroupIndex := AGroupIndex;
  FOptions := AOptions;
  FCompareOptions := REOptionsToRECompareOptions(FOptions);
end;

function TREReferenceCode.IsEqual(AStr: PWideChar; var Len: Integer): Boolean;
var
  S: REString;
  ARefTagNo: Integer;
begin
  Result := False;
  Len := 0;

  ARefTagNo := FGroupIndex;

  if not FRegExp.FGroups[ARefTagNo].Success then
    Exit;

  S := FRegExp.FGroups[ARefTagNo].Strings;

  Result := RECompareString(AStr, PWideChar(S), System.Length(S),
    Len, FCompareOptions) = 0;
  if Result then
    Len := System.Length(S);
end;

function TREReferenceCode.GetCharLength: TRETextPosRec;
begin
  Result.Min := FRegExp.FGroups[FGroupIndex].FCharLength.Min;
  Result.Max := FRegExp.FGroups[FGroupIndex].FCharLength.Max;
end;

{$IFDEF SKREGEXP_DEBUG}

function TREReferenceCode.GetDebugStr: REString;
begin
  Result := Format(sFmtGroupReference, [FGroupIndex]);
end;
{$ENDIF}

function TREReferenceCode.IsInclude(ACode: TRECode): Boolean;
begin
  Result := ACode is TREReferenceCode;
  if Result then
    Result := (ACode as TREReferenceCode).FGroupIndex = FGroupIndex;
end;

function TREReferenceCode.IsVariable: Boolean;
begin
  Result := True;
end;

{ TRENamedReferenceCode }

constructor TRENamedReferenceCode.Create(ARegExp: TSkRegExp;
  AGroupName: REString; AGroupIndex: Integer; AOptions: TREOptions);
begin
  inherited Create(ARegExp);
  FGroupName := AGroupName;
  FGroupIndex := AGroupIndex;
  FOptions := AOptions;
  FCompareOptions := REOptionsToRECompareOptions(FOptions);
end;

function TRENamedReferenceCode.IsEqual(AStr: PWideChar;
  var Len: Integer): Boolean;
var
  S: REString;
  MatchLen: Integer;
  LGroup: TGroup;
begin
  Result := False;
  Len := 0;

  LGroup := FRegExp.FGroups.Names[FGroupName];

  if LGroup.Success then
  begin
    S := LGroup.Strings;

    if (RECompareString(AStr, PWideChar(S), System.Length(S),
        MatchLen, FCompareOptions) = 0) then
    begin
      Result := True;
      Len := MatchLen;
    end;
  end;
end;

function TRENamedReferenceCode.GetCharLength: TRETextPosRec;
begin
  Result.Min := FRegExp.FGroups.Names[FGroupName].FCharLength.Min;
  Result.Max := FRegExp.FGroups.Names[FGroupName].FCharLength.Max;
end;

{$IFDEF SKREGEXP_DEBUG}

function TRENamedReferenceCode.GetDebugStr: REString;
var
  ARefTagNo: Integer;
  S: REString;
begin
  ARefTagNo := FGroupIndex;
  S := Format('"%s":%d', [FGroupName, ARefTagNo]);
  ARefTagNo := FRegExp.FGroups[ARefTagNo].SameGroup;

  repeat
    S := S + Format(',%d', [ARefTagNo]);
    ARefTagNo := FRegExp.FGroups[ARefTagNo].SameGroup;
  until ARefTagNo = 0;

  Result := Format(sFmtGroupNameReference, [S]);
end;
{$ENDIF}

function TRENamedReferenceCode.IsInclude(ACode: TRECode): Boolean;
begin
  Result := ACode is TRENamedReferenceCode;
  if Result then
    Result := (ACode as TRENamedReferenceCode).FGroupIndex = FGroupIndex;
end;

function TRENamedReferenceCode.IsVariable: Boolean;
begin
  Result := True;
end;

procedure TRENamedReferenceCode.SetGroupIndex(AGroupIndex: Integer);
begin
  FGroupIndex := AGroupIndex;
end;

{ TRELineHeadCode }

constructor TRELineHeadCode.Create(ARegExp: TSkRegExp; AOptions: TREOptions);
begin
  inherited Create(ARegExp);
  FOptions := AOptions;
end;

function TRELineHeadCode.IsEqual(AStr: PWideChar; var Len: Integer): Boolean;
begin
  Len := 0;
  Result := False;

  if roMultiLine in FOptions then
  begin
    if (AStr = FRegExp.FMatchTopP) then
      Result := True
    else
    begin
      if AStr = FRegExp.FMatchEndP then
        Exit;
      if FRegExp.LineBreakKind = lCRLF then
        Dec(AStr, 2)
      else
        Dec(AStr);
      Result := FRegExp.IsLineBreak(AStr) > 0;
    end;
  end
  else
    Result := AStr = FRegExp.FMatchTopP;
end;

function TRELineHeadCode.GetCharLength: TRETextPosRec;
begin
  Result.Min := 0;
  Result.Max := 0;
end;

{$IFDEF SKREGEXP_DEBUG}

function TRELineHeadCode.GetDebugStr: REString;
begin
  Result := sHeadOfLineCode
end;
{$ENDIF}

function TRELineHeadCode.IsInclude(ACode: TRECode): Boolean;
begin
  Result := ACode is TRELineHeadCode;
end;

{ TRELineTailCode }

constructor TRELineTailCode.Create(ARegExp: TSkRegExp; AOptions: TREOptions);
begin
  inherited Create(ARegExp);
  FOptions := AOptions;
end;

function TRELineTailCode.IsEqual(AStr: PWideChar; var Len: Integer): Boolean;
var
  L: Integer;
begin
  Len := 0;
  if roMultiLine in FOptions then
  begin
    if (AStr = FRegExp.FMatchEndP) then
      Result := True
    else
      Result := FRegExp.IsLineBreak(AStr) > 0;
  end
  else
  begin
    L := FRegExp.IsLineBreak(AStr);
    if L > 0 then
      Inc(AStr, L);

    Result := AStr = FRegExp.FMatchEndP;
  end;
end;

function TRELineTailCode.GetCharLength: TRETextPosRec;
begin
  Result.Min := 0;
  Result.Max := 0;
end;

{$IFDEF SKREGEXP_DEBUG}

function TRELineTailCode.GetDebugStr: REString;
begin
  Result := sEndOfLineCode;
end;
{$ENDIF}

function TRELineTailCode.IsInclude(ACode: TRECode): Boolean;
begin
  Result := ACode is TRELineTailCode;
end;

{ TRETextHeadCode }

function TRETextHeadCode.IsEqual(AStr: PWideChar; var Len: Integer): Boolean;
begin
  Len := 0;
  Result := FRegExp.FMatchTopP = AStr;
end;

function TRETextHeadCode.GetCharLength: TRETextPosRec;
begin
  Result.Min := 0;
  Result.Max := 0;
end;

{$IFDEF SKREGEXP_DEBUG}

function TRETextHeadCode.GetDebugStr: REString;
begin
  Result := sTextHeadCode;
end;
{$ENDIF}

function TRETextHeadCode.IsInclude(ACode: TRECode): Boolean;
begin
  Result := ACode is TRETextHeadCode;
end;

{ TRETextTailCode }

function TRETextTailCode.IsEqual(AStr: PWideChar; var Len: Integer): Boolean;
var
  L: Integer;
begin
  Len := 0;
  L := FRegExp.IsLineBreak(AStr);
  if L > 0 then
    Inc(AStr, L);

  Result := FRegExp.FMatchEndP = AStr;
end;

function TRETextTailCode.GetCharLength: TRETextPosRec;
begin
  Result.Min := 0;
  Result.Max := 0;
end;

{$IFDEF SKREGEXP_DEBUG}

function TRETextTailCode.GetDebugStr: REString;
begin
  Result := sTextTailCode;
end;
{$ENDIF}

function TRETextTailCode.IsInclude(ACode: TRECode): Boolean;
begin
  Result := ACode is TRETextTailCode;
end;

{ TRETextEndCode }

function TRETextEndCode.IsEqual(AStr: PWideChar; var Len: Integer): Boolean;
begin
  Len := 0;
  Result := FRegExp.FMatchEndP = AStr;
end;

function TRETextEndCode.GetCharLength: TRETextPosRec;
begin
  Result.Min := 0;
  Result.Max := 0;
end;

{$IFDEF SKREGEXP_DEBUG}

function TRETextEndCode.GetDebugStr: REString;
begin
  Result := sTextEndCode;
end;
{$ENDIF}

function TRETextEndCode.IsInclude(ACode: TRECode): Boolean;
begin
  Result := ACode is TRETextEndCode;
end;

{ TREPropertyCode }

{$IFDEF USE_UNICODE_PROPERTY}
function TREPropertyCode.CompareCode(Dest: TRECode): Integer;
begin
  if (Dest is TREPropertyCode) then
  begin
    if ((Dest as TREPropertyCode).FNegative = FNegative) and
      (FUniCodeProperty = (Dest as TREPropertyCode).FUniCodeProperty) then
      Result := 0
    else
      Result := 1;
  end
  else
    Result := 1;
end;

constructor TREPropertyCode.Create(ARegExp: TSkRegExp;
  AUnicodeProperty: TUnicodeProperty; ANegative: Boolean);
begin
  inherited Create(ARegExp);
  FUniCodeProperty := AUnicodeProperty;
  FNegative := ANegative;
end;

function TREPropertyCode.IsEqual(AStr: PWideChar; var Len: Integer): Boolean;
begin
  Result := False;
  Len := 0;

  if FRegExp.FMatchEndP = AStr then
    Exit;

  Result := IsUnicodeProperty(ToUChar(AStr), FUniCodeProperty);
  if FNegative then
    Result := not Result;

  if not Result then
    Len := 0
  else
  begin
    if IsNoLeadChar(AStr^) then
      Len := 1
    else
      Len := 2;
  end;
end;

{$IFDEF SKREGEXP_DEBUG}

function TREPropertyCode.GetDebugStr: REString;

  function GetPropertyName(P: TUnicodeProperty): REString;
  var
    I: Integer;
  begin
    for I := 0 to PropertyNames.Count - 1 do
    begin
      if TUnicodeProperty(PropertyNames.Objects[I]) = P then
      begin
        Result := PropertyNames[I];
        Exit;
      end;
    end;
  end;

begin
  if not FNegative then
    Result := Format(sPropertyCode, [GetPropertyName(FUniCodeProperty)])
  else
    Result := Format(sNegativePropertyCode,
      [GetPropertyName(FUniCodeProperty)]);
end;
{$ENDIF SKREGEXP_DEBUG}

function TREPropertyCode.IsInclude(ACode: TRECode): Boolean;
begin
  Result := (ACode is TREPropertyCode) and
    ((ACode as TREPropertyCode).FUniCodeProperty = FUniCodeProperty) and
    ((ACode as TREPropertyCode).FNegative = FNegative);
end;

function TREPropertyCode.IsOverlap(ACode: TRECode): Boolean;
begin
  if ACode.CharLength.Min <= 0 then
    Result := True
  else
  begin
    if (ACode is TRELiteralCode) then
    begin
      Result := IsUnicodeProperty
        (ToUChar(PWideChar((ACode as TRELiteralCode).FStrings)),
        FUniCodeProperty);
      if FNegative then
        Result := not Result;
    end
    else
      Result := True;
  end;
end;
{$ENDIF USE_UNICODE_PROPERTY}

{ TREPosixCharClassCode }

function TREPosixCharClassCode.CompareCode(Dest: TRECode): Integer;
begin
  if Dest is TREPosixCharClassCode then
  begin
    if ((Dest as TREPosixCharClassCode).FNegative = FNegative) and
      ((Dest as TREPosixCharClassCode).FPosixClass = FPosixClass) then
      Result := 0
    else
      Result := 1;
  end
{$IFDEF USE_UNICODE_PROPERTY}
  else if Dest is TREPropertyCode then
  begin
    Result := -1;
  end
{$ENDIF USE_UNICODE_PROPERTY}
  else
  begin
    Result := 1;
  end;
end;

constructor TREPosixCharClassCode.Create(ARegExp: TSkRegExp;
  APosixClass: TREPosixClassKind; AOptions: TREOptions; ANegative: Boolean);
begin
  inherited Create(ARegExp);
  FPosixClass := APosixClass;
  FOptions := AOptions;
  FCompareOptions := REOptionsToRECompareOptions(FOptions);
  FNegative := ANegative;
  FIsASCII := GetASCIIMode(FOptions)
end;

{$IFDEF SKREGEXP_DEBUG}

function TREPosixCharClassCode.GetDebugStr: REString;
begin
  case FPosixClass of
    pckAlnum:
      Result := 'alnum';
    pckAlpha:
      Result := 'alpha';
    pckAscii:
      Result := 'ascii';
    pckBlank:
      Result := 'blank';
    pckCntrl:
      Result := 'cntrl';
    pckDigit:
      Result := 'digit';
    pckGraph:
      Result := 'graph';
    pckLower:
      Result := 'lower';
    pckPrint:
      Result := 'print';
    pckUpper:
      Result := 'upper';
    pckPunct:
      Result := 'punct';
    pckSpace:
      Result := 'space';
    pckSpacePerl:
      Result := 'spaceperl';
    pckXdigit:
      Result := 'xdigit';
    pckWord:
      Result := 'word';
    pckAssigned:
      Result := 'assigned';
  else
    Result := 'any';
  end;

  if not FNegative then
    Result := Format(sPosixCharClassCode, [Result])
  else
    Result := Format(sNegativePosixCharClassCode, [Result]);
end;
{$ENDIF SKREGEXP_DEBUG}

function TREPosixCharClassCode.IsEqual(AStr: PWideChar;
  var Len: Integer): Boolean;
var
  Ch: UChar;
  L: Integer;
  LFold: TUnicodeMultiChar;
begin
  Result := False;
  Len := 0;

  if FRegExp.FMatchEndP = AStr then
    Exit;

  ClearUnicodeMultiChar(LFold);

  Ch := GetREChar(AStr, L, FCompareOptions, LFold);

  if FIsASCII and (FPosixClass <> pckAny) then
    Result := (Ch < 128) and
      IsPosixClassA(Ch, FPosixClass, roIgnoreCase in FOptions)
  else
    Result := IsPosixClassU(Ch, FPosixClass, roIgnoreCase in FOptions);

  if FNegative then
    Result := not Result;

  if Result then
  begin
    if IsNoLeadChar(AStr^) then
      Len := 1
    else
      Len := 2;
  end;
end;

function TREPosixCharClassCode.IsInclude(ACode: TRECode): Boolean;
begin
  Result := (ACode is TREPosixCharClassCode) and
    ((ACode as TREPosixCharClassCode).FPosixClass = FPosixClass) and
    ((ACode as TREPosixCharClassCode).FNegative = FNegative);
end;

function TREPosixCharClassCode.IsOverlap(ACode: TRECode): Boolean;
begin
  if ACode.CharLength.Min <= 0 then
    Result := True
  else
  begin
    if (ACode is TRELiteralCode) then
    begin
      if FIsASCII then
        Result := IsPosixClassA(
          ToUChar(PWideChar((ACode as TRELiteralCode).FStrings)),
          FPosixClass, roIgnoreCase in FOptions)
      else
        Result := IsPosixClassU(
          ToUChar(PWideChar((ACode as TRELiteralCode).FStrings)),
          FPosixClass, roIgnoreCase in FOptions);

      if FNegative then
        Result := not Result;
    end
    else
      Result := True;
  end;
end;

{ TREGlobalPosCode }

function TREGlobalPosCode.GetCharLength: TRETextPosRec;
begin
  Result.Min := 0;
  Result.Max := 0;
end;

{$IFDEF SKREGEXP_DEBUG}

function TREGlobalPosCode.GetDebugStr: REString;
begin
  Result := sGlobalPos;
end;
{$ENDIF}

function TREGlobalPosCode.IsEqual(AStr: PWideChar; var Len: Integer): Boolean;
var
  P: PWideChar;
begin
  Len := 0;
  P := FRegExp.FGlobalEndP;
  if P = nil then
    P := FRegExp.FMatchTopP;

  Result := AStr = P;
end;

function TREGlobalPosCode.IsInclude(ACode: TRECode): Boolean;
begin
  Result := ACode is TREGlobalPosCode;
end;

{ TREIfMatchReferenceCode }

constructor TREIfThenReferenceCode.Create(ARegExp: TSkRegExp;
  const AGroupIndex: Integer);
begin
  inherited Create(ARegExp);
  FGroupIndex := AGroupIndex;
end;

function TREIfThenReferenceCode.GetCharLength: TRETextPosRec;
begin
  Result.Min := 0;
  Result.Max := 0;
end;

{$IFDEF SKREGEXP_DEBUG}

function TREIfThenReferenceCode.GetDebugStr: REString;
begin
  Result := Format(sIfThenReference, [FGroupIndex]);
end;
{$ENDIF}

function TREIfThenReferenceCode.IsEqual(AStr: PWideChar;
  var Len: Integer): Boolean;
begin
  Len := 0;
  Result := False;
  if FGroupIndex <= FRegExp.GroupCount then
    Result := FRegExp.FGroups[FGroupIndex].Success;
end;

function TREIfThenReferenceCode.IsInclude(ACode: TRECode): Boolean;
begin
  Result := (ACode is TREIfThenReferenceCode) and
    ((ACode as TREIfThenReferenceCode).FGroupIndex = FGroupIndex);
end;

function TREIfThenReferenceCode.IsVariable: Boolean;
begin
  Result := True;
end;

{ TREIfThenNamedReferenceCode }

constructor TREIfThenNamedReferenceCode.Create(ARegExp: TSkRegExp;
  const AGroupName: REString);
begin
  inherited Create(ARegExp);
  FGroupName := AGroupName;
end;

function TREIfThenNamedReferenceCode.GetCharLength: TRETextPosRec;
begin
  Result.Min := 0;
  Result.Max := 0;
end;

{$IFDEF SKREGEXP_DEBUG}

function TREIfThenNamedReferenceCode.GetDebugStr: REString;
begin
  Result := Format(sIfThenNamedReference, [FGroupName]);
end;
{$ENDIF}

function TREIfThenNamedReferenceCode.IsEqual(AStr: PWideChar;
  var Len: Integer): Boolean;
begin
  Len := 0;

  Result := FRegExp.FGroups.Names[FGroupName].Success;
end;

function TREIfThenNamedReferenceCode.IsInclude(ACode: TRECode): Boolean;
begin
  Result := (ACode is TREIfThenNamedReferenceCode) and
    ((ACode as TREIfThenNamedReferenceCode).FGroupName = FGroupName);
end;

function TREIfThenNamedReferenceCode.IsVariable: Boolean;
begin
  Result := True;
end;

{ TREInSubReferenceCode }

constructor TREInSubReferenceCode.Create(ARegExp: TSkRegExp;
  const AGroupIndex: Integer);
begin
  inherited Create(ARegExp);
  FGroupIndex := AGroupIndex;
end;

function TREInSubReferenceCode.GetCharLength: TRETextPosRec;
begin
  Result.Min := 0;
  Result.Max := 0;
end;

{$IFDEF SKREGEXP_DEBUG}

function TREInSubReferenceCode.GetDebugStr: REString;
begin
  Result := Format(sInSubR, [FGroupIndex]);
end;
{$ENDIF SKREGEXP_DEBUG}

function TREInSubReferenceCode.IsEqual(AStr: PWideChar;
  var Len: Integer): Boolean;
begin
  Result := False;
  Len := 0;
  if FRegExp.FSubStack.Count > 0 then
    if FGroupIndex > 0 then
      Result := FGroupIndex = FRegExp.FSubStack.GroupIndex
    else
      Result := True;
end;

function TREInSubReferenceCode.IsInclude(ACode: TRECode): Boolean;
begin
  if (ACode is TREInSubReferenceCode) then
    Result := (ACode as TREInSubReferenceCode).FGroupIndex = FGroupIndex
  else
    Result := False;
end;

function TREInSubReferenceCode.IsVariable: Boolean;
begin
  Result := True;
end;

{ TREInSubNameReferenceCode }

constructor TREInSubNameReferenceCode.Create(ARegExp: TSkRegExp;
  const AGroupName: REString);
begin
  inherited Create(ARegExp);
  FGroupName := AGroupName;
end;

function TREInSubNameReferenceCode.GetCharLength: TRETextPosRec;
begin
  Result.Min := 0;
  Result.Max := 0;
end;

{$IFDEF SKREGEXP_DEBUG}

function TREInSubNameReferenceCode.GetDebugStr: REString;
begin
  Result := Format(sInSubN, [FGroupName]);
end;
{$ENDIF SKREGEXP_DEBUG}

function TREInSubNameReferenceCode.IsEqual(AStr: PWideChar;
  var Len: Integer): Boolean;
begin
  Result := False;
  Len := 0;
  if FRegExp.FSubStack.Count > 0 then
    Result := FRegExp.FGroups[FRegExp.FSubStack.GroupIndex].GroupName = FGroupName;
end;

function TREInSubNameReferenceCode.IsInclude(ACode: TRECode): Boolean;
begin
  if ACode is TREInSubNameReferenceCode then
    Result := (ACode as TREInSubNameReferenceCode).FGroupName = FGroupName
  else
    Result := False;
end;

function TREInSubNameReferenceCode.IsVariable: Boolean;
begin
  Result := True;
end;

{ TRECalloutCode }

constructor TRECalloutCode.Create(ARegExp: TSkRegExp;
  const ACalloutNumber, APatternPosition, APatternLength: Integer);
begin
  inherited Create(ARegExp);
  FData.Version := 1;
  FData.CalloutNumber := ACalloutNumber;
  FData.CurrentPosition := 1;
  FData.StartMatch := 1;
  FData.PatternPosition := APatternPosition;
  FData.PatternLength := APatternLength;
end;

function TRECalloutCode.GetCharLength: TRETextPosRec;
begin
  Result.Min := 0;
  Result.Max := -1;
end;

{$IFDEF SKREGEXP_DEBUG}
function TRECalloutCode.GetDebugStr: REString;
begin
  Result := Format(sCallout, [FData.CalloutNumber]);
end;
{$ENDIF}

function TRECalloutCode.IsEqual(AStr: PWideChar; var Len: Integer): Boolean;
begin
  Result := False;
  Len := 0;

  if not Assigned(FRegExp.FOnCallout) then
    Exit;

  FData.CurrentPosition := AStr - FRegExp.FTextTopP + 1;
  FData.StartMatch := FRegExp.FStartMatch;

  FRegExp.FOnCallout(FRegExp, @FData, Result, Len);
end;

function TRECalloutCode.IsInclude(ACode: TRECode): Boolean;
begin
  Result := (ACode is TRECalloutCode) and
    ((ACode as TRECalloutCode).FData.CalloutNumber = FData.CalloutNumber);
end;

function TRECalloutCode.IsVariable: Boolean;
begin
  Result := True;
end;

{ TREBinCode }

constructor TREBinCode.Create(ARegExp: TSkRegExp; AOp: TREOperator;
  ALeft, ARight: TRECode; AMin, AMax: Integer);
begin
  inherited Create(ARegExp);
  FOp := AOp;
  FLeft := ALeft;
  FRight := ARight;
  FMin := AMin;
  FMax := AMax;
  FMatchKind := lkGreedy;
end;

{ TRELex }

procedure TRELex.CharNext(var P: PWideChar; const Len: Integer);
begin
  // TRELexでは文字の読み出しにGetRECharを使っているため、
  // ここでサロゲートを処理する必要はない。
  if P^ <> #0000 then
    Inc(P, Len);
end;

procedure TRELex.CharPrev(var P: PWideChar; const Len: Integer);
begin
  // TRELexでは文字の読み出しにGetRECharを使っているため、
  // ここでサロゲートを処理する必要はない。
  if P > FTopP then
    Dec(P, Len);
end;

procedure TRELex.ClearOptionList;
var
  I: Integer;
  P: PREOptions;
begin
  for I := 0 to FOptionList.Count - 1 do
    if FOptionList[I] <> nil then
    begin
      P := FOptionList[I];
      Dispose(P);
    end;
  FOptionList.Clear;
end;

class function TRELex.CountGroup(const S: REString): Integer;
var
  P, LastP: PWideChar;
begin
  Result := 0;
  P := PWideChar(S);
  LastP := P + System.Length(S);

  while P < LastP do
  begin
    if P^ = '(' then
    begin
      Inc(P);
      if P^ = '?' then
      begin
        Inc(P);
        if (P^ = '<') or (P^ = '''') then
        begin
          Inc(Result);
        end
        else if P^ = 'P' then
        begin
          Inc(P);
          if P^ = '<' then
            Inc(Result);
        end;
      end
      else
      begin
        Inc(Result);
        Dec(P);
      end;
    end
    else if P^ = '\' then
      Inc(P);

    Inc(P);
  end;
end;

constructor TRELex.Create(ARegExp: TSkRegExp; const Expression: REString);
begin
  inherited Create;
  FOptionList := TList.Create;
  FRegExp := ARegExp;
  FOptions := FRegExp.FOptions;
  FP := PWideChar(Expression);
  FTopP := FP;
  FLastP := FP + Length(Expression);

  FPrevCount := 0;
  FPrevLex[0].FOptionList := TList.Create;
  FPrevLex[1].FOptionList := TList.Create;
  FPrevLex[2].FOptionList := TList.Create;
  ClearUnicodeMultiChar(FFold);
  FGroupCount := CountGroup(Expression);
end;

destructor TRELex.Destroy;
begin
  FPrevLex[0].FOptionList.Free;
  FPrevLex[1].FOptionList.Free;
  FPrevLex[2].FOptionList.Free;

  ClearOptionList;
  FOptionList.Free;
  inherited;
end;

procedure TRELex.Error(const Msg, Prefix: REString);
var
  S, T: REString;
begin
  T := GetErrorPositionString(GetCompileErrorPos);
  CharNext(FP);
  SetString(S, FTokenStartP, FP - FTokenStartP);
  if S <> '' then
    S := S + Prefix;
  S := Format(Msg, [S]);

  raise ESkRegExpCompile.CreateFmt('%s; %s', [S, T]);
end;

procedure TRELex.Error(const Msg: REString; APosition: Integer);
var
  S, T: REString;
begin
  T := GetErrorPositionString(APosition);
  CharNext(FP);
  SetString(S, FTokenStartP, FP - FTokenStartP);
  S := Format(Msg, [S]);
  raise ESkRegExpCompile.Create(Format('%s; %s', [S, T]));
end;

function TRELex.GetCompileErrorPos: Integer;
begin
  if FP <> nil then
    Result := FP - FTopP + 1
  else
    Result := 0;
end;

function TRELex.GetControlCode(var Len: Integer): UChar;
var
  P: PWideChar;
begin
  Result := 0;
  CharNext(FP);

  P := FP;
  if ((P^ >= '@') and (P^ <= '_')) or ((P^ >= 'a') and (P^ <= 'z')) then
  begin
    if P^ = '\' then
    begin
      Inc(P);
      if P^ <> '\' then
        Error(sInvalidEscapeCharacterSyntax, GetCompileErrorPos);
      Inc(Len);
    end;

    Result := UChar(P^);
    if (Result >= UChar('a')) and (Result <= UChar('z')) then
      Dec(Result, $20);
    Result := Result xor $40;
  end
  else
    Error(sInvalidEscapeCharacterSyntax, GetCompileErrorPos);
end;

function TRELex.GetCurrentPosition: Integer;
begin
  Result := FP - FTopP + 1;
end;

function TRELex.GetDigit(var Len: Integer): Integer;
var
  P: PWideChar;
begin
  P := FP;
  Result := 0;

  while (P^ >= '0') and (P^ <= '9') do
  begin
    Result := Result * 10 + (Integer(P^) - Integer('0'));
    CharNext(P);
  end;
  Len := P - FP;
end;

function TRELex.GetErrorPositionString(APosition: Integer): REString;
begin
  if APosition > 0 then
    Result := Copy(FRegExp.Expression, 1, APosition) + ' <-- ' +
      Copy(FRegExp.Expression, APosition + 1, Maxint);
end;

function TRELex.GetHexDigit(var Len: Integer): UChar;
var
  P: PWideChar;
  I, L: Integer;
  IsBrace, Is6: Boolean;
begin
  Result := 0;
  CharNext(FP);

  if FP^ = '{' then
  begin
    CharNext(FP);
    IsBrace := True;
    L := 6;
  end
  else
  begin
    IsBrace := False;
    L := 2;
  end;

  P := FP;
  Len := 0;
  Is6 := True;

  for I := 1 to L do
  begin
    case P^ of
      '0' .. '9':
        Result := (Result shl 4) + (UChar(P^) - UChar('0'));
      'A' .. 'F':
        Result := (Result shl 4) + (UChar(P^) - UChar('7'));
      'a' .. 'f':
        Result := (Result shl 4) + (UChar(P^) - UChar('W'));
      '}':
        begin
          if IsBrace then
          begin
            Is6 := False;
            Inc(Len);
            Break;
          end;
        end
    else
      begin
        FP := P;
        Error(sHexDigitIsRequired);
      end;
    end;
    CharNext(P);
    Inc(Len);
  end;

  if IsBrace and Is6 then
  begin
    if P^ <> '}' then
    begin
      FP := P;
      Error(sMissingRightBraceOnEscx);
    end;
    Inc(Len);
  end;

  if Len = 0 then
  begin
    FP := P;
    Error(sHexDigitIsRequired);
  end;
  if (LongWord(Result) > $10FFFF) then
  begin
    FP := P;
    Error(sCodePointRangeOver);
  end;
end;

function TRELex.GetOctalDigit(Ch: WideChar; var Len: Integer): UChar;
var
  P: PWideChar;
  I, L: Integer;
  IsBrace: Boolean;
begin
  Result := 0;
  L := 3;
  IsBrace := False;

  if Ch = 'o' then
  begin
    CharNext(FP);

    if FP^ = '{' then
    begin
      CharNext(FP);
      IsBrace := True;
      L := 7;
    end
    else
      Error(sMissingLeftBraceOnESCo);
  end;

  P := FP;
  Len := 0;

  for I := 1 to L do
  begin
    case P^ of
      '0' .. '7':
        Result := (Result shl 3) + (UChar(P^) - UChar('0'));
    else
      Break;
    end;
    CharNext(P);
    Inc(Len);
  end;

  if IsBrace then
  begin
    if P^ <> '}' then
    begin
      FP := P;
      Error(sMissingRightBraceOnEsco);
    end;
    Inc(Len);
  end;
  if Len = 0 then
  begin
    FP := P;
    Error(sOctalDigitIsRequired);
  end;
  if (LongWord(Result) > $10FFFF) then
  begin
    FP := P;
    Error(sCodePointRangeOver);
  end;
end;

function TRELex.GetPosixType(const S: REString): TREPosixClassKind;
begin
  if SameText(S, 'alnum') then
    Result := pckAlnum
  else if SameText(S, 'alpha') then
    Result := pckAlpha
  else if SameText(S, 'ascii') then
    Result := pckAscii
  else if SameText(S, 'blank') then
    Result := pckBlank
  else if SameText(S, 'cntrl') then
    Result := pckCntrl
  else if SameText(S, 'digit') then
    Result := pckDigit
  else if SameText(S, 'graph') then
    Result := pckGraph
  else if SameText(S, 'lower') then
    Result := pckLower
  else if SameText(S, 'print') then
    Result := pckPrint
  else if SameText(S, 'punct') then
    Result := pckPunct
  else if SameText(S, 'space') then
    Result := pckSpace
  else if SameText(S, 'spaceperl') then
    Result := pckSpacePerl
  else if SameText(S, 'upper') then
    Result := pckUpper
  else if SameText(S, 'xdigit') then
    Result := pckXdigit
  else if SameText(S, 'word') then
    Result := pckWord
  else if SameText(S, 'any') then
    Result := pckAny
  else if SameText(S, 'assigned') then
    Result := pckAssigned
  else
    Result := pckNone;
end;

function TRELex.GetRECompareOptions: TRECompareOptions;
begin
  Result := REOptionsToRECompareOptions(FOptions);
end;

procedure TRELex.GetToken(Skip: Boolean);
var
  L: Integer;
begin
  if not Skip then
    SaveToken;

  FWChar := 0;
  FStartWChar := 0;
  FLastWChar := 0;
  FMin := 0;
  FMax := 0;
  FLevel := 0;
  FGroupName := '';
  FTokenStartP := FP;

  if roExtended in FOptions then
    SkipWhiteSpace;

  if FP^ = #0 then
  begin
    if FContext <> ctNormal then
      Error(sUnmatchedBigPar);

    FToken := tkEnd;
    Exit;
  end;

  L := 1;

  if FContext = ctCharClass then
  begin
    if FP^ = ']' then
    begin
      if (FP + 1)^ <> ']' then
      begin
        FToken := tkCharClassEnd;
        FContext := ctNormal;
        CharNext(FP, 1);
      end
      else
      begin
        FWChar := UChar(']');
        FToken := tkChar;
        CharNext(FP, 1);
      end;
    end
    else
      LexCharClass;
  end
  else if FContext = ctNegativeCharClass then
  begin
    if FP^ = ']' then
    begin
      if FNegativeCharClassFirst then
      begin
        FWChar := UChar(']');
        FToken := tkChar;
        CharNext(FP, 1);
      end
      else
      begin
        if (FP + 1)^ <> ']' then
        begin
          FToken := tkCharClassEnd;
          FContext := ctNormal;
          CharNext(FP, 1);
        end
        else
        begin
          FWChar := UChar(']');
          FToken := tkChar;
          CharNext(FP, 1);
        end;
      end;
    end
    else
      LexCharClass;

    if FNegativeCharClassFirst then
      FNegativeCharClassFirst := False;
  end
  else if FContext = ctQuote then
  begin
    if REStrLComp(FP, '\E', 2) = 0 then
    begin
      FContext := ctNormal;
      CharNext(FP, 2);
      GetToken(True);
      Exit;
    end;
    FWChar := GetREChar(FP, L, GetRECompareOptions, FFold);
    FToken := tkChar;
    CharNext(FP, L);
  end
  else
  begin
    if not FIsQuote then
    begin
      if REStrLComp(FP, '(*', 2) = 0 then
      begin
        if REStrLComp(FP, '(*CR)', 5) = 0 then
        begin
          FRegExp.LineBreakKind := lbCR;
          CharNext(FP, 5);
        end
        else if REStrLComp(FP, '(*LF)', 5) = 0 then
        begin
          FRegExp.LineBreakKind := lbLF;
          CharNext(FP, 5);
        end
        else if REStrLComp(FP, '(*CRLF)', 7) = 0 then
        begin
          FRegExp.LineBreakKind := lCRLF;
          CharNext(FP, 7);
        end
        else if REStrLComp(FP, '(*ANYCRLF)', 10) = 0 then
        begin
          FRegExp.LineBreakKind := lbAnyCRLF;
          CharNext(FP, 10);
        end
        else if REStrLComp(FP, '(*ANY)', 6) = 0 then
        begin
          FRegExp.LineBreakKind := lbAny;
          CharNext(FP, 6);
        end;
      end;

      case FP^ of
        '|':
          FToken := tkUnion;
        '(':
          begin
            CharNext(FP);
            LexLeftPar;

            Exit;
          end;
        ')':
          FToken := tkRPar;
        '*':
          begin
            FToken := tkStar;
            FMin := 0;
            FMax := 0;
          end;
        '+':
          begin
            FToken := tkPlus;
            FMin := 1;
            FMax := 0;
          end;
        '?':
          FToken := tkQuest;
        '.':
          FToken := tkDot;
        '\':
          begin
            LexESCChar;
            Exit;
          end;
        '[':
          begin
            CharNext(FP);
            if FP^ = '^' then
            begin
              FContext := ctNegativeCharClass;
              FToken := tkNegativeCharClassFirst;
              FNegativeCharClassFirst := True;
            end
            else if FP^ = ':' then
              LexPosixCharClass
            else
            begin
              CharPrev(FP);
              FContext := ctCharClass;
              FToken := tkCharClassFirst;
            end;
          end;
        '{':
          begin
            CharNext(FP);
            if (FP^ >= '0') and (FP^ <= '9') then
            begin
              LexBrace;
              Exit;
            end
            else
            begin
              CharPrev(FP);
              FWChar := GetREChar(FP, L, GetRECompareOptions, FFold);
              FToken := tkChar;
            end;
          end;
        '^':
            FToken := tkLHead;
        '$':
            FToken := tkLTail;
      else
        begin
          FWChar := GetREChar(FP, L, GetRECompareOptions, FFold);
          FToken := tkChar;
        end;
      end;
    end
    else
    begin
      FWChar := GetREChar(FP, L, GetRECompareOptions, FFold);
      FToken := tkChar;
    end;
    CharNext(FP, L);
  end;
end;

function TRELex.GetTokenLength: Integer;
begin
  Result := FP - FTokenStartP;
end;

function TRELex.GetTokenStartPosition: Integer;
begin
  Result := FTokenStartP - FTopP + 1;
end;

procedure TRELex.LexVerb;
var
  S: REString;
  StartP: PWideChar;
begin
  CharNext(FP);

  if REStrLComp(FP, 'PRUNE', 5) = 0 then
  begin
    FToken := tkbcPrune;
    CharNext(FP, 5);
  end
  else if REStrLComp(FP, 'SKIP', 4) = 0 then
  begin
    FToken := tkbcSkip;
    CharNext(FP, 4);
  end
  else if REStrLComp(FP, 'MARK', 4) = 0 then
  begin
    FToken := tkbcMark;
    CharNext(FP, 4);
  end
  else if REStrLComp(FP, 'THEN', 4) = 0 then
  begin
    FToken := tkbcThen;
    CharNext(FP, 4);
  end
  else if REStrLComp(FP, 'COMMIT', 6) = 0 then
  begin
    FToken := tkbcCommit;
    CharNext(FP, 6);
  end
  else if REStrLComp(FP, 'ACCEPT', 6) = 0 then
  begin
    FToken := tkbcAccept;
    CharNext(FP, 6);
    Exit;
  end
  else if REStrLComp(FP, 'FAIL', 4) = 0 then
  begin
    FToken := tkFail;
    CharNext(FP, 4);
    Exit;
  end
  else if FP^ = 'F' then
  begin
    FToken := tkFail;
    CharNext(FP);
    Exit;
  end
  else
    FToken := tkbcMark;

  if FP^ = ':' then
  begin
    CharNext(FP);
    StartP := FP;

    while (FP^ <> #0000) do
    begin
      if FP^ = ')' then
      begin
        SetString(S, StartP, FP - StartP);
        FGroupName := S;
        Exit;
      end;
      Inc(FP);
    end;
  end;
end;

procedure TRELex.LexBrace;
var
  I, L: Integer;
begin
  SkipWhiteSpace;

  I := GetDigit(L);
  CharNext(FP, L);

  if I > CONST_LoopMax then
    Error(sQuantifierIsTooLarge);

  FMin := I;

  if FP^ = ',' then
  begin
    CharNext(FP);
    SkipWhiteSpace;
    if (FP^ >= '0') and (FP^ <= '9') then
    begin
      I := GetDigit(L);
      CharNext(FP, L);

      if I > CONST_LoopMax then
        Error(sQuantifierIsTooLarge);

      if I < FMin then
        Error(sCantDoMaxLessMin);

      FMax := I;
    end
    else
      FMax := CONST_LoopMax;
  end
  else
    FMax := FMin;

  if FP^ <> '}' then
    Error(sUnmatchedCurlyBracket);

  CharNext(FP);
  FToken := tkBound;
end;

procedure TRELex.LexCallout;
var
  L: Integer;
begin
  CharNext(FP);

  L := 0;

  if not ((FP^ < #128) and IsDigitA(UChar(FP^))) then
    FMin := 0
  else
  begin
    FMin := GetDigit(L);
    if FMin > $FF then
    begin
      CharNext(FP, L);
      Error(sRangeOverCalloutNumber);
    end;
  end;

  FToken := tkCallout;
  CharNext(FP, L);

  if FP^ <> ')' then
    Error(sUnmatchedSmallPar);

  CharNext(FP);
end;

procedure TRELex.LexCharClass;
var
  L: Integer;
  BFP: PWideChar;
begin
  if FP^ = '\' then
  begin
    LexESCChar;
    L := 0;
  end
  else if REStrLComp(FP, '[:', 2) = 0 then
  begin
    CharNext(FP);
    LexPosixCharClass;
    L := 0;
  end
  else
  begin
    FWChar := GetREChar(FP, L, [], FFold);
    FToken := tkChar;
  end;

  CharNext(FP, L);
  if (FToken = tkChar) and (FP^ = '-') then
  begin
    BFP := FP;

    CharNext(FP);
    FStartWChar := FWChar;

    if FP^ <> ']' then
    begin
      if FP^ = '\' then
      begin
        LexESCChar;

        if FToken <> tkChar then
        begin
          FToken := tkChar;
          FWChar := FStartWChar;
          FP := BFP;
          Exit;
        end;

        if FToken <> tkChar then
          Error(sInvalideBigParRange, '');
      end
      else if REStrLComp(FP, '[:', 2) = 0 then
      begin
        FToken := tkChar;
        FWChar := FStartWChar;
        FP := BFP;
        Exit;
      end
      else
      begin
        FWChar := GetREChar(FP, L, [], FFold);
      end;

      if FStartWChar > FWChar then
        Error(sInvalideBigParRange, '');

      CharNext(FP, L);

      FLastWChar := FWChar;

      FToken := tkRangeChar;
    end
    else
    begin
      CharPrev(FP);
      FToken := tkChar;
      Exit;
    end;
  end;
end;

procedure TRELex.LexESCChar;
var
  L, LTagNo: Integer;
  SaveP: PWideChar;
begin
  L := 1;
  CharNext(FP);

  if FP^ = #0 then
    Error(sRegExpNotCompleted, GetCompileErrorPos);

  case FP^ of
    'A', 'B', 'E', 'G', 'K', 'N', 'Q', 'X', 'R', 'Z', 'b', 'g', 'k', 'z':
      begin
        if FContext = ctNormal then
        begin
          case FP^ of
            'A':
              FToken := tkTHead;
            'B':
              FToken := tkNEWordBoundary;
            'Q':
              begin
                FContext := ctQuote;
                CharNext(FP);
                GetToken(True);
                Exit;
              end;
            'N':
              FToken := tkExceptEOL;
            'Z':
              FToken := tkTTail;
            'b':
              FToken := tkWordBoundary;
            'k':
              begin
                SaveP := FP;
                CharNext(FP);
                case FP^ of
                  '<':
                    LexReference('>');
                  '''':
                    LexReference('''');
                  '{':
                    LexReference('}');
                  '1' .. '9', '+', '-':
                    LexReference(#0000);
                else
                  Error(sGroupNumberIsEmpty);
                end;
                if (FToken = tkNamedReference) and
                    (FGroupName = '') then
                begin
                  FP := SaveP;
                  FWChar := GetREChar(FP, L, GetRECompareOptions, FFold);
                  FToken := tkChar;
                end
                else
                  Exit;
              end;
            'R':
              FToken := tkLineBreak;
            'z':
              FToken := tkTTailEnd;
            'K':
              FToken := tkKeepPattern;
            'X':
              FToken := tkCombiningSequence;
            'G':
              FToken := tkGlobalPos;
            'g':
              begin
                CharNext(FP);
                case FP^ of
                  '<':
                    LexGoSub('>');
                  '''':
                    LexGoSub('''');
                  '{':
                    LexReference('}');
                  '1' .. '9', '-':
                    LexReference(#0000);
                else
                  Error(sGroupNumberIsEmpty);
                end;
                Exit;
              end;
          end;
        end
        else
        begin
          FWChar := GetREChar(FP, L, GetRECompareOptions, FFold);
          FToken := tkChar;
        end;
      end;
    '1' .. '9':
      begin
        if FContext = ctNormal then
        begin
          LTagNo := GetDigit(L);

          if FGroupCount < LTagNo then
          begin
            if (FP^ >= '1') and (FP^ <= '7') then
              FWChar := GetOctalDigit('0', L)
            else
              FWChar := GetREChar(FP, L, GetRECompareOptions, FFold);
            FToken := tkChar;
          end
          else
          begin
            FMin := LTagNo;
            FToken := tkReference;
          end;
        end
        else
        begin
          if (FP^ >= '0') and (FP^ <= '7') then
            FWChar := GetOctalDigit('0', L)
          else
            FWChar := GetREChar(FP, L, GetRECompareOptions, FFold);

          FToken := tkChar;
        end;
      end;
{$IFDEF USE_UNICODE_PROPERTY}
    'p', 'P':
      begin
        if FP^ = 'P' then
          FToken := tkNEProperty
        else
          FToken := tkProperty;

        CharNext(FP);
        if FP^ = '{' then
        begin
          LexProperty(FToken = tkProperty);
          Exit;
        end
        else
        begin
          case FP^ of
            'L':
              FUniodeProperty := upL;
            'M':
              FUniodeProperty := upM;
            'N':
              FUniodeProperty := upN;
            'P':
              FUniodeProperty := upP;
            'S':
              FUniodeProperty := upS;
            'Z':
              FUniodeProperty := upZ;
          else
            Error(sInvalidProperty);
          end;
        end;
      end;
{$ENDIF USE_UNICODE_PROPERTY}
    'D':
      FToken := tkNEDigitChar;
    'H':
      FToken := tkNEHorizontalSpaceChar;
    'S':
      FToken := tkNESpaceChar;
    'V':
      FToken := tkNEVerticalSpaceChar;
    'W':
      FToken := tkNEWordChar;
    'd':
      FToken := tkDigitChar;
    'h':
      FToken := tkHorizontalSpaceChar;
    's':
      FToken := tkSpaceChar;
    'v':
      FToken := tkVerticalSpaceChar;
    'w':
      FToken := tkWordChar;
  else
    begin
      case FP^ of
        'n':
          FWChar := $A;
        'c':
          FWChar := GetControlCode(L);
        'x':
          FWChar := GetHexDigit(L);
        '0'..'3', 'o':
          FWChar := GetOctalDigit(FP^, L);
        't':
          FWChar := 9;
        'r':
          FWChar := $D;
        'f':
          FWChar := $C;
        'a':
          FWChar := 7;
        'e':
          FWChar := $1B;
      else
        FWChar := GetREChar(FP, L, GetRECompareOptions, FFold);
      end;
      FToken := tkChar;
    end;
  end;
  CharNext(FP, L);
end;

procedure TRELex.LexGoSub(const LastDelimiter: WideChar);
var
  ATag, L: Integer;
  StartP: PWideChar;
  S: REString;
  IsMinus, IsPlus: Boolean;
begin
  if LastDelimiter = #0000 then
  begin
    if (FP^ = '-') or (FP^ = '+') then
    begin
      IsMinus := FP^ = '-';

      CharNext(FP);

      FToken := tkGoSubRelative;
    end
    else
    begin
      IsMinus := False;
      FToken := tkGoSub;
    end;

    ATag := GetDigit(L);

    if IsMinus then
      FMin := 0 - ATag
    else
      FMin := ATag;

    if L = 0 then
      Error(sGroupNumberIsEmpty);

    CharNext(FP, L);
  end
  else
  begin
    CharNext(FP);

    StartP := FP;

    IsMinus := FP^ = '-';
    IsPlus := FP^ = '+';

    if IsMinus or IsPlus then
    begin
      FToken := tkGoSubRelative;
      CharNext(FP);
    end
    else
      FToken := tkGoSub;

    if (FP^ < #128) and IsDigitA(UChar(FP^)) then
    begin
      ATag := GetDigit(L);

      if IsMinus then
        FMin := 0 - ATag
      else
        FMin := ATag;

      if L = 0 then
        Error(sGroupNumberIsEmpty);

      CharNext(FP, L);
      if FP^ <> LastDelimiter then
        Error(sNotTerminated);

      CharNext(FP, L);
    end
    else
    begin
      while FP^ <> #0 do
      begin
        if FP^ = LastDelimiter then
        begin
          SetString(S, StartP, FP - StartP);
          FGroupName := S;
          FToken := tkGoSubName;
          CharNext(FP);
          Exit;
        end;
        if IsWordU(ToUChar(FP)) then
          CharNext(FP)
        else
          Error(sInvalidCharInGroupName);
      end;
      Error(sNoEndOfGroup);
    end;
  end;
end;

procedure TRELex.LexGroupName(const LastDelimiter: WideChar);
var
  S: REString;
  StartP: PWideChar;
begin
  if (FP^ >= '0') and (FP^ <= '9') then
    Error(sInvalidCharInGroupName);

  StartP := FP;

  while FP^ <> #0 do
  begin
    if FP^ = LastDelimiter then
    begin
      SetString(S, StartP, FP - StartP);
      FGroupName := S;
      CharNext(FP);
      Exit;
    end
    else
    begin
      if not IsWordU(ToUChar(FP)) then
        Error(sInvalidCharInGroupName);

      CharNext(FP);
    end;
  end;
  Error(sNoEndOfGroup);
end;

procedure TRELex.LexLeftPar;
var
  L: Integer;
  S: REString;
  StartP: PWideChar;
begin
  L := 1;
  if FP^ = '?' then
  begin
    CharNext(FP);
    case FP^ of
      '-', '^':
        begin
          CharNext(FP);
          if not ((FP^ < #128) and IsDigitA(UChar(FP^))) then
          begin
            CharPrev(FP);
            LexOption;
          end
          else
          begin
            FMin := 0 - GetDigit(L);
            CharNext(FP, L);
            if FP^ <> ')' then
              Error(sUnmatchedSmallPar);
            FToken := tkGoSubRelative;
          end;
        end;
      'a', 'd', 'i', 'l', 'm', 'n', 's', 'u', 'x', 'w', 'k':
        LexOption;
      '#':
        begin
          CharNext(FP);
          while FP^ <> #0 do
          begin
            if FP^ = ')' then
            begin
              CharNext(FP);
              GetToken(True);
              Exit;
            end;
            CharNext(FP);
          end;
        end;
      '>':
        FToken := tkNoBackTrack;
      ':':
        FToken := tkLPar;
      '''':
        begin
          CharNext(FP);
          LexGroupName('''');
          FToken := tkGroupBegin;
          Exit;
        end;
      '<':
        begin
          CharNext(FP);
          if FP^ = '=' then
            FToken := tkBehindMatch
          else if FP^ = '!' then
            FToken := tkBehindNoMatch
          else
          begin
            LexGroupName('>');
            FToken := tkGroupBegin;
            Exit;
          end;
        end;
      ')':
        begin
          FToken := tkEmpty;
        end;
      'P':
        begin
          CharNext(FP);
          if FP^ = '<' then
          begin
            CharNext(FP);
            LexGroupName('>');
            FToken := tkGroupBegin;
            Exit;
          end
          else if FP^ = '=' then
          begin
            CharNext(FP);
            LexGroupName(')');
            FToken := tkNamedReference;
            Exit;
          end
          else if FP^ = '>' then
          begin
            CharNext(FP);
            LexGroupName(')');
            FToken := tkGoSubName;
            Exit;
          end
          else
            Error(sGroupNameIsEmpty, '...)');
        end;
      '+':
        begin
          CharNext(FP);
          if (FP < #128) and IsDigitA(UChar(FP^)) then
          begin
            FMin := GetDigit(L);
            CharNext(FP, L);
            if FP^ <> ')' then
              Error(sUnmatchedSmallPar);
            FToken := tkGoSubRelative;
          end
          else
            Error(sGroupNumberIsEmpty);
        end;
      '=':
        FToken := tkAheadMatch;
      '!':
        begin
          CharNext(FP);
          if FP^ = ')' then
            FToken := tkFail
          else
          begin
            FToken := tkAheadNoMatch;
            Exit;
          end;
        end;
      '(':
        begin
          CharNext(FP);
          if (FP < #128) and IsDigitA(UChar(FP^)) then
          begin
            FMin := GetDigit(L);
            CharNext(FP, L);
            if FP^ = ')' then
            begin
              CharNext(FP);
              FToken := tkIfMatchRef;
              Exit;
            end;
          end
          else if FP^ = '<' then
          begin
            CharNext(FP);
            LexGroupName('>');
            if FP^ = ')' then
            begin
              CharNext(FP);
              FToken := tkIfMatchRef;
              Exit;
            end;
          end
          else if FP^ = '''' then
          begin
            CharNext(FP);
            LexGroupName('''');
            if FP^ = ')' then
            begin
              CharNext(FP);
              FToken := tkIfMatchRef;
              Exit;
            end;
          end
          else if FP^ = 'R' then
          begin
            CharNext(FP);
            if FP^ = ')' then
            begin
              CharNext(FP);
              FMin := 0;
              FToken := tkInSubRef;
              Exit;
            end
            else if (FP^ < #128) and IsDigitA(UChar(FP^)) then
            begin
              FMin := GetDigit(L);
              CharNext(FP, L);
              if FP^ = ')' then
              begin
                CharNext(FP);
                FToken := tkInSubRef;
                Exit;
              end;
            end
            else if FP^ = '&' then
            begin
              CharNext(FP);
              LexGroupName(')');
              FToken := tkInSubRef;
              Exit;
            end;
          end
          else if REStrLComp(FP, 'DEFINE', 6) = 0 then
          begin
            CharNext(FP, System.Length('DEFINE'));
            if FP^ = ')' then
            begin
              CharNext(FP);
              FToken := tkDefine;
              Exit;
            end;
          end
          else
          begin
            Dec(FP);
            FToken := tkIfMatch;
            Exit;
          end;
          Error(sConditionNotRecognized, '...)');
        end;
      'R':
        begin
          FMin := 0;
          CharNext(FP);
          if FP^ <> ')' then
            Error(sUnmatchedSmallPar);

          FToken := tkGoSub;
        end;
      '&':
        begin
          CharNext(FP);
          StartP := FP;
          while FP^ <> #0000 do
          begin
            if FP^ = ')' then
            begin
              SetString(S, StartP, FP - StartP);
              CharNext(FP);
              FGroupName := S;
              FToken := tkGoSubName;
              Exit;
            end;
            CharNext(FP);
          end;
          Error(sUnmatchedSmallPar);
        end;
      '0' .. '9':
        begin
          FMin := GetDigit(L);
          CharNext(FP, L);
          if FP^ <> ')' then
            Error(sUnmatchedSmallPar);

          FToken := tkGoSub;
        end;
      '|': // branch reset
        FToken := tkBranchReset;
      'C':
        begin
          LexCallout;
          Exit;
        end;
    else // case
      Error(sNotRecognized);
    end;
  end
  else if FP^ = '*' then
  begin
    LexVerb;
    if FP^ <> ')' then
      Error(sUnterminatedVerbPattern);
    L := 1;
  end
  else // if
  begin
    if not(roNamedGroupOnly in FOptions) then
      FToken := tkGroupBegin
    else
      FToken := tkLPar;
    Exit;
  end;
  CharNext(FP, L);
end;

{ .$WARNINGS OFF }
{$WARNINGS ON}

procedure TRELex.LexOption;
type
  TLanguageMode = (lmNone, lmASCII, lmUnicode, lmDefault, lmLocale);
var
  IsInclude, IsNotExlude, IsError: Boolean;
  LMode: TLanguageMode;
  ACount: Byte;
begin
  FNewOptions := FOptions;
  LMode := lmNone;
  ACount := 0;

  if FP^ = '^' then
  begin
    IsNotExlude := True;
    FNewOptions := [roMultiLine, roSingleLine, roExtended];
    CharNext(FP);
  end
  else
    IsNotExlude := False;

  while FP^ <> #0 do
  begin
    if (FP^ = '-') then
    begin
      if IsNotExlude then
        Error(sModifierCanNotBeDisabledInCaret);

      IsInclude := False;
      CharNext(FP);
    end
    else
      IsInclude := True;

    case FP^ of
      'a':
        if IsInclude then
        begin
          if ACount >= 2 then
            Error(sModifierAMayAppearAMaximumOfTwice);

          if (LMode <> lmASCII) and (LMode <> lmNone) then
            Error(sModifiersareMutuallyExclusive);

          Include(FNewOptions, roASCIICharClass);

          Inc(ACount);

          LMode := lmASCII;
        end
        else
          Error(sModifierCanNotBeDisabled);
      'i':
        if IsInclude then
          Include(FNewOptions, roIgnoreCase)
        else
          Exclude(FNewOptions, roIgnoreCase);
      'm':
        if IsInclude then
          Include(FNewOptions, roMultiLine)
        else
          Exclude(FNewOptions, roMultiLine);
      'n':
        if IsInclude then
          Include(FNewOptions, roNamedGroupOnly)
        else
          Exclude(FNewOptions, roNamedGroupOnly);
      's':
        if IsInclude then
          Include(FNewOptions, roSingleLine)
        else
          Exclude(FNewOptions, roSingleLine);
      'u', 'l', 'd':
        if IsInclude then
        begin
          if LMode <> lmNone then
          begin
            case FP^ of
              'l':
                IsError :=  LMode <> lmLocale;
              'd':
                IsError := LMode <> lmDefault;
              'u':
                IsError := LMode <> lmUnicode;
            else
              IsError := False;
            end;
          end
          else
            IsError := False;

          if IsError then
            Error(sModifiersareMutuallyExclusive)
          else
          begin
            Exclude(FNewOptions, roASCIICharClass);
            Exclude(FNewOptions, roASCIIOnly);
            case FP^ of
              'l':
                LMode := lmLocale;
              'd':
                LMode := lmDefault;
            else
              LMode := lmUnicode;
            end;
          end;
        end
        else
          Error(sModifierCanNotBeDisabled);
      'x':
        if IsInclude then
          Include(FNewOptions, roExtended)
        else
          Exclude(FNewOptions, roExtended);
{$IFDEF UseJapaneseOption}
      'w':
        if IsInclude then
          Include(FNewOptions, roIgnoreWidth)
        else
          Exclude(FNewOptions, roIgnoreWidth);
      'k':
        if IsInclude then
          Include(FNewOptions, roIgnoreKana)
        else
          Exclude(FNewOptions, roIgnoreKana);
{$ENDIF}
    else
      Error(sNotRecognized);
    end;

    CharNext(FP);

    if (FP^ = ')') or (FP^ = ':') then
    begin
      if (ACount = 2) and (roIgnoreCase in FNewOptions) then
        Include(FNewOptions, roASCIIOnly);

      if FP^ = ')' then
        FToken := tkOption
      else if FP^ = ':' then
        FToken := tkLParWithOption;
      Exit;
    end;
  end;
  Error(sOptionNotCompleted);
end;

procedure TRELex.LexPosixCharClass;
var
  IsNegative: Boolean;
  L: Integer;
  P: PWideChar;
  S: REString;
begin
  CharNext(FP);
  if FP^ = ']' then
  begin
    CharPrev(FP, 2);
    FWChar := GetREChar(FP, L, GetRECompareOptions, FFold);
    FToken := tkChar;
    CharNext(FP, L);
    Exit;
  end
  else if FP^ = '^' then
  begin
    IsNegative := True;
    CharNext(FP);
  end
  else
    IsNegative := False;

  P := FP;

  while P^ <> #0 do
  begin
    if (P^ = ']') or (P^ = #0) then
    begin
      FWChar := GetREChar(FP, L, [], FFold);
      FToken := tkChar;
      CharNext(FP, L);
      Exit;
    end
    else if P^ = ':' then
    begin
      SetString(S, FP, P - FP);
      FPosixClass := GetPosixType(S);

      if FPosixClass = pckNone then
      begin
        CharNext(FP);
        Error(Format(sPosixClassUnkown, [S]));
      end;

      if IsNegative then
        FToken := tkNEPosixBracket
      else
        FToken := tkPosixBracket;

      CharNext(P);
      if P^ <> ']' then
      begin
        FP := P;
        Error(sUnmatchedBigPar);
      end;

      CharNext(P);
      FP := P;

      Break;
    end;
    CharNext(P);
  end;

  if not(FContext in [ctCharClass, ctNegativeCharClass]) then
    Error(sPosixClassSupportedOnlyClass);
end;

{$IFDEF USE_UNICODE_PROPERTY}
procedure TRELex.LexProperty(const CheckNegative: Boolean);
var
  Index: Integer;
  S, S2, PreStr: REString;
  StartP: PWideChar;
  LPosix: TREPosixClassKind;
begin
  CharNext(FP);
  if CheckNegative and (FP^ = '^') then
  begin
    FToken := tkNEProperty;
    CharNext(FP);
  end;

  StartP := FP;

  while FP^ <> #0 do
  begin
    if FP^ = '}' then
    begin
      SetString(S, StartP, FP - StartP);
      S := PreStr + S;
      if PropertyNames.Find(S, Index) then
        FUniodeProperty := TUnicodeProperty(PropertyNames.Objects[Index])
      else
      begin
        if UpperCase(Copy(S, 1, 2)) = 'IS' then
          S2 := Copy(S, 3, MaxInt)
        else
          S2 := S;

        LPosix := GetPosixType(S2);
        if LPosix = pckNone then
        begin
          S2 := 'In' + Copy(S, 3, Maxint);
          if PropertyNames.Find(S2, Index) then
            FUniodeProperty := TUnicodeProperty(PropertyNames.Objects[Index])
          else
            Error(Format(sPropertyUnknown, [S]));
        end
        else
        begin
          if FToken = tkNEProperty then
            FToken := tkNEPosixBracket
          else
            FToken := tkPosixBracket;

            FPosixClass := LPosix;
        end;
      end;

      CharNext(FP);
      Exit;
    end
//    else if (FP^ = '-') or (FP^ = '_') or IsAnkSpace(FP) then
//    begin
//      SetString(S, StartP, FP - StartP);
//      PreStr := PreStr + S;
//      CharNext(FP);
//      StartP := FP;
//    end
    else if ((FP < #128) and IsWordA(UChar(FP^))) or (FP^ = '&') then
      CharNext(FP)
    else
      Error(sInvalidProperty);
  end;
  Error(sMissingRightBrace);
end;
{$ENDIF USE_UNICODE_PROPERTY}

procedure TRELex.LexReference(const LastDelimiter: WideChar);
var
  L: Integer;
  StartP: PWideChar;
  S: REString;
  IsMinus: Boolean;
begin
  if LastDelimiter = #0000 then
  begin
    if FP^ = '-' then
    begin
      CharNext(FP);

      FToken := tkReferenceRelative;
    end
    else
      FToken := tkReference;

    FMin := GetDigit(L);

    if L = 0 then
      Error(sGroupNumberIsEmpty);

    CharNext(FP, L);
  end
  else
  begin
    CharNext(FP);

    StartP := FP;

    IsMinus := FP^ = '-';

    if IsMinus then
    begin
      FToken := tkReferenceRelative;
      CharNext(FP);
    end
    else
      FToken := tkReference;

    if (FP^ < #128) and IsDigitA(UChar(FP^)) then
    begin
      FMin := GetDigit(L);

      if L = 0 then
        Error(sGroupNumberIsEmpty);

      CharNext(FP, L);
      if FP^ <> LastDelimiter then
        Error(sNotTerminated);

      CharNext(FP, L);
    end
    else
    begin
      if LastDelimiter = '>' then
      begin
        while FP^ <> #0 do
        begin
          if (FP^ = '>') then
          begin
            SetString(S, StartP, FP - StartP);
            FGroupName := S;
            FToken := tkNamedReference;
            if FP^ = '>' then
            begin
              CharNext(FP);
              Exit;
            end;
          end;
          if IsWordU(ToUChar(FP)) then
            CharNext(FP)
          else
            Error(sInvalidCharInGroupName);
        end;
      end
      else
      begin
        while FP^ <> #0 do
        begin
          if FP^ = LastDelimiter then
          begin
            SetString(S, StartP, FP - StartP);
            FGroupName := S;
            FToken := tkNamedReference;
            CharNext(FP);
            Exit;
          end;
          if IsWordU(ToUChar(FP)) then
            CharNext(FP)
          else
            Error(sInvalidCharInGroupName);
        end;
      end;
      Error(sNoEndOfGroup);
    end;
  end;
end;

procedure TRELex.PopOptions;
var
  AOptions: PREOptions;
begin
  if FOptionList.Count = 0 then
    Exit;
  AOptions := PREOptions(FOptionList[FOptionList.Count - 1]);
  FOptions := AOptions^;
  FOptionList.Delete(FOptionList.Count - 1);
  Dispose(AOptions);
end;

procedure TRELex.PushOptions;
var
  AOptions: PREOptions;
begin
  New(AOptions);
  AOptions^ := FOptions;
  FOptionList.Add(AOptions);
end;

procedure TRELex.PushToken;
var
  I: Integer;
begin
  case FPrevCount of
    0:
      I := 2;
    1:
      I := 0;
  else
    I := 1;
  end;

  if not FPrevLex[I].FStored then
    Exit;

  FPrevLex[I].FStored := False;

  FToken := FPrevLex[I].FToken;
  FOptions := FPrevLex[I].FOptions;
  FNewOptions := FPrevLex[I].FNewOptions;
  FP := FPrevLex[I].FP;
  FTokenStartP := FPrevLex[I].FTokenStartP;
  FTopP := FPrevLex[I].FTopP;
  FWChar := FPrevLex[I].FWChar;
  FStartWChar := FPrevLex[I].FStartWChar;
  FLastWChar := FPrevLex[I].FLastWChar;
  FMin := FPrevLex[I].FMin;
  FMax := FPrevLex[I].FMax;
  FLevel := FPrevLex[I].FLevel;
  FContext := FPrevLex[I].FContext;
{$IFDEF USE_UNICODE_PROPERTY}
  FUniodeProperty := FPrevLex[I].FUniCodeProperty;
{$ENDIF USE_UNICODE_PROPERTY}
  FPosixClass := FPrevLex[I].FPosixClass;
  FGroupName := FPrevLex[I].FGroupName;
  FOptionList.Assign(FPrevLex[I].FOptionList);
  FIsQuote := FPrevLex[I].FIsQuote;
  FNegativeCharClassFirst := FPrevLex[I].FNegativeCharClassFirst;
  FFold := FPrevLex[I].FFold;
  FLineBreakKind := FPrevLex[I].FLineBreakKind;

  case FPrevCount of
    0:
      FPrevCount := 2;
    1:
      FPrevCount := 0;
    2:
      FPrevCount := 1;
  end;
end;

procedure TRELex.SaveToken;
var
  I: Integer;
begin
  case FPrevCount of
    0:
      I := 0;
    1:
      I := 1;
  else
    I := 2;
  end;

  FPrevLex[I].FStored := True;

  FPrevLex[I].FToken := FToken;
  FPrevLex[I].FOptions := FOptions;
  FPrevLex[I].FNewOptions := FNewOptions;
  FPrevLex[I].FP := FP;
  FPrevLex[I].FTokenStartP := FTokenStartP;
  FPrevLex[I].FTopP := FTopP;
  FPrevLex[I].FWChar := FWChar;
  FPrevLex[I].FStartWChar := FStartWChar;
  FPrevLex[I].FLastWChar := FLastWChar;
  FPrevLex[I].FMin := FMin;
  FPrevLex[I].FMax := FMax;
  FPrevLex[I].FLevel := FLevel;
  FPrevLex[I].FContext := FContext;
{$IFDEF USE_UNICODE_PROPERTY}
  FPrevLex[I].FUniCodeProperty := FUniodeProperty;
{$ENDIF USE_UNICODE_PROPERTY}
  FPrevLex[I].FPosixClass := FPosixClass;
  FPrevLex[I].FGroupName := FGroupName;
  FPrevLex[I].FOptionList.Assign(FOptionList);
  FPrevLex[I].FIsQuote := FIsQuote;
  FPrevLex[I].FNegativeCharClassFirst := FNegativeCharClassFirst;
  FPrevLex[I].FFold := FFold;
  FPrevLex[I].FLineBreakKind := FLineBreakKind;

  case FPrevCount of
    0:
      FPrevCount := 1;
    1:
      FPrevCount := 2;
    2:
      FPrevCount := 0;
  end;
end;

procedure TRELex.SkipWhiteSpace;
begin
  while (FP^ <> #0) and ((FP^ = ' ') or (FP^ = #9) or (FP^ = #10) or
    (FP^ = #13)) do
    Inc(FP);
end;

procedure TRELex.UpdateOptions;
begin
  FOptions := FNewOptions;
end;

{ TREParser }

procedure TREParser.AddJoinMatchGroup(const Index: Integer);
begin
  if Index >= FJoinMatchGroupSize then
  begin
    FJoinMatchGroupSize := Index + (Index div 4) + 1;
    SetLength(FJoinMatchGroup, FJoinMatchGroupSize);
  end;
  FJoinMatchGroup[Index] := True;
end;

constructor TREParser.Create(ARegExp: TSkRegExp; const Expression: REString);
begin
  inherited Create;
  FRegExp := ARegExp;
  FLex := TRELex.Create(ARegExp, Expression);
  FGroupStack := TStack.Create;
  FReferenceErrorList := TREStringList.Create;
  FGoSubErrorList := TREStringList.Create;
  FJoinMatchGroupSize := CONST_CAPTURE_DEFAULT_SIZE;
  SetLength(FJoinMatchGroup, FJoinMatchGroupSize);
  FHasRecursion := False;
  FHasGoSub := False;
  FHasTailAnchor := False;
end;

destructor TREParser.Destroy;
var
  I: Integer;
  P: PReferenceErrorRec;
begin
  for I := 0 to FReferenceErrorList.Count - 1 do
  begin
    P := PReferenceErrorRec(FReferenceErrorList.Objects[I]);
    Dispose(P);
  end;
  FReferenceErrorList.Free;

  for I := 0 to FGoSubErrorList.Count - 1 do
  begin
    P := PReferenceErrorRec(FGoSubErrorList.Objects[I]);
    Dispose(P);
  end;
  FGoSubErrorList.Free;

  FLex.Free;
  FGroupStack.Free;
  inherited;
end;

function TREParser.Factor: TRECode;

  procedure SetMinMatch(BinCode: TRECode);
  begin
    if not(BinCode is TREBinCode) then
      FLex.Error(sQuestPosInaccurate);

    (BinCode as TREBinCode).MatchKind := lkReluctant;
  end;

  procedure SetMaxMatch(BinCode: TRECode);
  begin
    if not(BinCode is TREBinCode) then
      FLex.Error(sQuestPosInaccurate);

    (BinCode as TREBinCode).MatchKind := lkPossessive;
  end;

  procedure CheckAheadMatch(ACode: TRECode);
  begin
    if ACode is TREBinCode then
    begin
      if ((ACode as TREBinCode).FOp = opAheadMatch) or
          ((ACode as TREBinCode).FOp = opAheadNoMatch) then
        FLex.Error(sLoopOfMatchAheadNotSpecified);
    end;
  end;

  procedure CheckEmptyLoop(ACode: TRECode);
  begin
    if (ACode is TREBoundaryCode) or (ACode is TRETextHeadCode) or
      (ACode is TRETextTailCode) or (ACode is TRETextEndCode) then
      FLex.Error(sLoopOfLengthZeroCannotSpecified);
  end;

var
  LMin, LMax: Integer;
  LToken: TREToken;
  LMatchKind: TRELoopKind;
  CalloutCode: TRECalloutCode;
begin
  CalloutCode := nil;

  Result := Primay;

  if FLex.Token in [tkStar, tkPlus, tkQuest, tkBound] then
  begin
    if Result = nil then
      FLex.Error(sNothingToRepeat);

    if roAutoCallout in FLex.Options then
    begin
      CalloutCode := TRECalloutCode.Create(FRegExp, 255,
        FLex.TokenStartPosition, FLex.TokenLength);
      FRegExp.FCodeList.Add(CalloutCode);
    end;

    LToken := FLex.Token;
    LMin := FLex.Min;
    LMax := FLex.Max;

    CheckAheadMatch(Result);
    CheckEmptyLoop(Result);

    FLex.GetToken;

    if FLex.Token = tkQuest then
    begin
      LMatchKind := lkReluctant;
      FLex.GetToken;
    end
    else if FLex.Token = tkPlus then
    begin
      LMatchKind := lkPossessive;
      FLex.GetToken;
    end
    else
    begin
      LMatchKind := lkGreedy;
    end;

    case LToken of
      tkStar:
        begin
{$IFDEF NotOptimizeCompile}
          Result := NewBinCode(opLoop, Result, nil, 0, CONST_LoopMax);
          TREBinCode(Result).MatchKind := LMatchKind;
{$ELSE NotOptimizeCompile}
          if (Result is TREAnyCharCode) and (LMatchKind in [lkGreedy, lkPossessive]) then
          begin
            Result := NewBinCode(opStar, Result, nil, 0, CONST_LoopMax);
            TREBinCode(Result).MatchKind := lkAny;
          end
{$IFDEF USE_UNICODE_PROPERTY}
          else if (Result is TRECombiningSequence) and (LMatchKind in [lkGreedy, lkPossessive]) then
          begin
            Result := NewBinCode(opStar, Result, nil, 0, CONST_LoopMax);
            TREBinCode(Result).MatchKind := lkCombiningSequence;
          end
{$ENDIF USE_UNICODE_PROPERTY}
          else if (not(Result is TREBinCode)) and (LMatchKind in [lkGreedy, lkPossessive]) then
          begin
            Result := NewBinCode(opStar, Result, nil, 0, CONST_LoopMax);
            TREBinCode(Result).MatchKind := LMatchKind;
          end
          else
          begin
            Result := NewBinCode(opLoop, Result, nil, 0, CONST_LoopMax);
            TREBinCode(Result).MatchKind := LMatchKind;
          end;
{$ENDIF NotOptimizeCompile}
        end;
      tkPlus:
        begin
{$IFDEF NotOptimizeCompile}
          Result := NewBinCode(opLoop, Result, nil, 1, CONST_LoopMax);
          TREBinCode(Result).MatchKind := LMatchKind;
{$ELSE NotOptimizeCompile}
          if (not(Result is TREBinCode)) and (LMatchKind in [lkGreedy, lkPossessive]) then
          begin
            Result := NewBinCode(opPlus, Result, nil, 1, CONST_LoopMax);
            TREBinCode(Result).MatchKind := LMatchKind;
          end
          else
          begin
            Result := NewBinCode(opLoop, Result, nil, 1, CONST_LoopMax);
            TREBinCode(Result).MatchKind := LMatchKind;
          end;
{$ENDIF NotOptimizeCompile}
        end;
      tkQuest:
        begin
          Result := NewBinCode(opLoop, Result, nil, 0, 1);
          TREBinCode(Result).MatchKind := LMatchKind;
        end;
      tkBound:
        begin
{$IFDEF NotOptimizeCompile}
          Result := NewBinCode(opLoop, Result, nil, LMin, LMax);
          TREBinCode(Result).MatchKind := LMatchKind;
{$ELSE NotOptimizeCompile}
          if (not(Result is TREBinCode)) and (LMatchKind in [lkGreedy, lkPossessive]) then
          begin
            Result := NewBinCode(opBound, Result, nil, LMin, LMax);
            TREBinCode(Result).MatchKind := LMatchKind;
          end
          else
          begin
            Result := NewBinCode(opLoop, Result, nil, LMin, LMax);
            TREBinCode(Result).MatchKind := LMatchKind;
          end;
{$ENDIF NotOptimizeCompile}
        end;
    end;
    if Assigned(CalloutCode) then
    begin
      Result := NewBinCode(opConcat, CalloutCode, Result);
    end;
  end
  else
  begin
    if Result = nil then
      FLex.Error(sNotRecognized);
  end;
end;

function TREParser.NewBinCode(AOperator: TREOperator; ALeft, ARight: TRECode;
  AMin, AMax: Integer): TRECode;
begin
  Result := TREBinCode.Create(FRegExp, AOperator, ALeft, ARight, AMin, AMax);
  FRegExp.FBinCodeList.Add(Result);
end;

function TREParser.NewCharClassCode(ANegative: Boolean): TRECode;
var
  I: Integer;
  CharClass: TRECharClassCode;
begin
  CharClass := TRECharClassCode.Create(FRegExp, ANegative, FLex.Options);
  FRegExp.FCodeList.Add(CharClass);

  FLex.GetToken;
  case FLex.Token of
    tkChar:
      CharClass.Add(FLex.FWChar);
    tkRangeChar:
      CharClass.Add(FLex.StartWChar, FLex.LastWChar);
    tkWordChar:
      CharClass.Add(TREWordCharCode.Create(FRegExp, FLex.Options, False));
    tkDigitChar:
      CharClass.Add(TREDigitCharCode.Create(FRegExp, FLex.Options, False));
    tkSpaceChar:
      CharClass.Add(TRESpaceCharCode.Create(FRegExp, FLex.Options, False));
    tkNEWordChar:
      CharClass.Add(TREWordCharCode.Create(FRegExp, FLex.Options, True));
    tkNEDigitChar:
      CharClass.Add(TREDigitCharCode.Create(FRegExp, FLex.Options, True));
    tkNESpaceChar:
      CharClass.Add(TRESpaceCharCode.Create(FRegExp, FLex.Options, True));
    tkPosixBracket:
      CharClass.Add(TREPosixCharClassCode.Create(FRegExp,
        FLex.PosixClass, FLex.Options, False));
    tkNEPosixBracket:
      CharClass.Add(TREPosixCharClassCode.Create(FRegExp,
        FLex.PosixClass, FLex.Options, True));
{$IFDEF USE_UNICODE_PROPERTY}
    tkProperty:
      CharClass.Add(TREPropertyCode.Create(FRegExp,
        FLex.UnicodeProperty, False));
    tkNEProperty:
      CharClass.Add(TREPropertyCode.Create(FRegExp,
        FLex.UnicodeProperty, True));
{$ENDIF USE_UNICODE_PROPERTY}
    tkHorizontalSpaceChar:
      CharClass.Add(TREHorizontalSpaceCharCode.Create(FRegExp, False));
    tkNEHorizontalSpaceChar:
      CharClass.Add(TREHorizontalSpaceCharCode.Create(FRegExp, True));
    tkVerticalSpaceChar:
      CharClass.Add(TREVerticalSpaceCharCode.Create(FRegExp, False));
    tkNEVerticalSpaceChar:
      CharClass.Add(TREVerticalSpaceCharCode.Create(FRegExp, True));
  else
    FLex.Error(sInvalidCharactorClass);
  end;

  FLex.GetToken;
  while (FLex.Token = tkRangeChar) or (FLex.Token = tkChar) or
    (FLex.Token = tkWordChar) or (FLex.Token = tkNEWordChar) or
    (FLex.Token = tkDigitChar) or (FLex.Token = tkNEDigitChar) or
    (FLex.Token = tkSpaceChar) or (FLex.Token = tkNESpaceChar) or
    (FLex.Token = tkPosixBracket) or (FLex.Token = tkNEPosixBracket) or
    (FLex.Token = tkProperty) or (FLex.Token = tkNEProperty) or
    (FLex.Token = tkHorizontalSpaceChar) or
    (FLex.Token = tkNEHorizontalSpaceChar) or (FLex.Token = tkVerticalSpaceChar)
    or (FLex.Token = tkNEVerticalSpaceChar) do
  begin
    case FLex.Token of
      tkChar:
        CharClass.Add(FLex.WChar);
      tkRangeChar:
        CharClass.Add(FLex.StartWChar, FLex.LastWChar);
      tkWordChar:
        CharClass.Add(TREWordCharCode.Create(FRegExp, FLex.Options, False));
      tkDigitChar:
        CharClass.Add(TREDigitCharCode.Create(FRegExp, FLex.Options, False));
      tkSpaceChar:
        CharClass.Add(TRESpaceCharCode.Create(FRegExp, FLex.Options, False));
      tkNEWordChar:
        CharClass.Add(TREWordCharCode.Create(FRegExp, FLex.Options, True));
      tkNEDigitChar:
        CharClass.Add(TREDigitCharCode.Create(FRegExp, FLex.Options, True));
      tkNESpaceChar:
        CharClass.Add(TRESpaceCharCode.Create(FRegExp, FLex.Options, True));
      tkPosixBracket:
        CharClass.Add(TREPosixCharClassCode.Create(FRegExp,
          FLex.PosixClass, FLex.Options, False));
      tkNEPosixBracket:
        CharClass.Add(TREPosixCharClassCode.Create(FRegExp,
          FLex.PosixClass, FLex.Options, True));
{$IFDEF USE_UNICODE_PROPERTY}
      tkProperty:
        CharClass.Add(TREPropertyCode.Create(FRegExp,
          FLex.UnicodeProperty, False));
      tkNEProperty:
        CharClass.Add(TREPropertyCode.Create(FRegExp,
          FLex.UnicodeProperty, True));
{$ENDIF USE_UNICODE_PROPERTY}
      tkHorizontalSpaceChar:
        CharClass.Add(TREHorizontalSpaceCharCode.Create(FRegExp, False));
      tkNEHorizontalSpaceChar:
        CharClass.Add(TREHorizontalSpaceCharCode.Create(FRegExp, True));
      tkVerticalSpaceChar:
        CharClass.Add(TREVerticalSpaceCharCode.Create(FRegExp, False));
      tkNEVerticalSpaceChar:
        CharClass.Add(TREVerticalSpaceCharCode.Create(FRegExp, True));
    else
      FLex.Error(sInvalidCharactorClass);
    end;
    FLex.GetToken;
  end;
  if FLex.Token <> tkCharClassEnd then
    FLex.Error(sUnmatchedBigPar);

  // Ⅰ文字だけの文字クラスなら解除
  if not CharClass.FNegative and (CharClass.FCodeList.Count = 0) and
    (CharClass.FMap.Count = 1) then
  begin
    Result := TRELiteralCode.Create(
      FRegExp, CharClass.FWChar, CharClass.FOptions);
    I := FRegExp.FCodeList.IndexOf(CharClass);
    Assert(I <> -1, 'bug?: Error at NewCharClassCode');

    TRECode(FRegExp.FCodeList[I]).Free;
    FRegExp.FCodeList[I] := Result;
  end
  else
  begin
    CharClass.Rebuild;

    Result := CharClass;
  end;

end;

procedure TREParser.Parse;
var
  I, P, LGroupNo: Integer;
  RefError: PReferenceErrorRec;
begin
  FGroupCount := 0;
  FCurrentGroup := 0;
  FGroupLevel := 0;
  FHasRecursion := False;
  FHasReference := False;

  FLex.GetToken;
  FRegExp.FCode := RegExpr;

  if FLex.Token <> tkEnd then
  begin
    if FLex.Token = tkRPar then
      FLex.Error(sUnmatchedSmallPar)
    else
      FLex.Error(sRegExpNotCompleted);
  end;

  for I := 0 to FReferenceErrorList.Count - 1 do
  begin
    RefError := PReferenceErrorRec(FReferenceErrorList.Objects[I]);
    if FReferenceErrorList[I] <> '' then
    begin
      LGroupNo := FRegExp.FGroups.IndexOfName(FReferenceErrorList[I]);
      if LGroupNo = -1 then
      begin
        FLex.Error(Format(sReferenceToNonexistentNamedGroup, [FReferenceErrorList[I]]),
          RefError.ErrorPos);
      end
      else
      begin
        TRENamedReferenceCode(RefError.AObject).SetGroupIndex(LGroupNo);
        AddJoinMatchGroup(LGroupNo);
      end;
    end
    else
      raise ESkRegExpCompile.Create('bug?: not set reference group name on TREParser.Parse');
  end;

  for I := 0 to FGoSubErrorList.Count - 1 do
  begin
    RefError := PReferenceErrorRec(FGoSubErrorList.Objects[I]);
    if FGoSubErrorList[I] <> '' then
    begin
      P := FRegExp.FGroups.IndexOfName(FGoSubErrorList[I]);
      if P = -1 then
        FLex.Error(Format(sReferenceToNonexistentNamedGroup, [FGoSubErrorList[I]]),
          RefError.ErrorPos);

      if FRegExp.FGroups.IsDuplicateGroupName(FGoSubErrorList[I]) then
        FLex.Error(Format(sCannotCallMultipleDefineGroupName,
          [FGoSubErrorList[I]]), RefError.ErrorPos);
    end
    else
      raise ESkRegExpCompile.Create('bug?: not set gosub group name TREParser.Parse');
  end;

  SetLength(FJoinMatchGroup, FRegExp.FGroups.Count);

  for I := 1 to FRegExp.FGroups.Count - 1 do
  begin
    if FJoinMatchGroup[I] then
    begin
      FRegExp.FGroups[I].FJoinMatch := True;
      if not FHasReference then
        FHasReference := True;
    end;
  end;
end;

function TREParser.Primay: TRECode;

  procedure CheckVariableLength(ACode: TRECode);
  begin
    if ACode is TREBinCode then
    begin
      case (ACode as TREBinCode).FOp of
        opStar, opPlus, opBound, opLoop:
            FLex.Error(sBehindMatchNotVariableLength);
        else
          begin
            if (ACode as TREBinCode).Left <> nil then
              CheckVariableLength((ACode as TREBinCode).Left);
            if (ACode as TREBinCode).Right <> nil then
              CheckVariableLength((ACode as TREBinCode).Right);
          end;
      end;
    end
    else if ACode.IsVariable then
      FLex.Error(sBehindMatchNotVariableLength);

  end;

  procedure CheckRecursion(SubCode: TRECode);
  begin
    if SubCode is TREBinCode then
    begin
      if (SubCode as TREBinCode).Op = opGoSub then
        FLex.Error(sNeverEndingRecursion)
      else if (SubCode as TREBinCode).Op = opUnion then
      begin
        CheckRecursion((SubCode as TREBinCode).Left);
        CheckRecursion((SubCode as TREBinCode).Right);
      end;
    end
  end;

// 10進表記の数値を8進数に変換。あくまでも10進表記
  function ToOctal(const ADigit: Integer): Integer;
  var
    I: Integer;
    S: REString;
  begin
    Result := 0;
    S := IntToStr(ADigit);

    for I := 1 to Length(S) do
    begin
      case S[I] of
        '0' .. '7':
          Result := (Result shl 3) + (Integer(S[I]) - Integer('0'));
      else
        Break;
      end;
    end;
  end;

var
  LGroupNo, LGroupBuffer: Integer;
  LGroupName: REString;
  SubCode, CondCode: TRECode;
  CalloutCode: TRECalloutCode;
  RefError: PReferenceErrorRec;

  LOptions: TREOptions;
  LWChar: UChar;
  Str: UCS4String;
  Len: Integer;
begin
  Result := nil;

  if roAutoCallout in FLex.Options then
  begin
    CalloutCode := TRECalloutCode.Create(FRegExp, 255, FLex.TokenStartPosition,
      FLex.TokenLength);
    FRegExp.FCodeList.Add(CalloutCode);
  end
  else
    CalloutCode := nil;

  case FLex.Token of
    tkChar:
      begin
        Len := 1;
        LWChar := FLex.WChar;
        LOptions := FLex.Options;

        SetLength(Str, Len);
        Str[Len - 1] := FLex.FWChar;
        Inc(Len);

        FLex.GetToken;
        while (FLex.Token = tkChar) and (FLex.Options = LOptions) do
        begin
          FLex.GetToken;
          if FLex.Token in [tkQuest, tkStar, tkBound, tkPlus] then
          begin
            FLex.PushToken;
            Break;
          end;

          FLex.PushToken;

          SetLength(Str, Len);
          Str[Len - 1] := FLex.FWChar;
          Inc(Len);
          FLex.GetToken;
        end;

        if Length(Str) > 1 then
        begin
          Result := TRELiteralCode.Create(FRegExp, Str, FLex.Options);
          if Assigned(CalloutCode) then
            CalloutCode.FData.PatternLength := Length(Str);
        end
        else
          Result := TRELiteralCode.Create(FRegExp, LWChar, LOptions);

        FRegExp.FCodeList.Add(Result);
      end;
    tkLHead:
      begin
        Result := TRELineHeadCode.Create(FRegExp, FLex.Options);
        FRegExp.FCodeList.Add(Result);
        FLex.GetToken;
      end;
    tkLTail:
      begin
        Result := TRELineTailCode.Create(FRegExp, FLex.Options);
        FRegExp.FCodeList.Add(Result);
        FLex.GetToken;
        if not FHasTailAnchor then
          FHasTailAnchor := True;
      end;
    tkDot:
      begin
        Result := TREAnyCharCode.Create(FRegExp, FLex.Options);
        FRegExp.FCodeList.Add(Result);
        FLex.GetToken;
      end;
    tkExceptEOL:
      begin
        Result := TREAnyCharCode.Create(FRegExp, []);
        FRegExp.FCodeList.Add(Result);
        FLex.GetToken;
      end;
    tkWordChar, tkNEWordChar:
      begin
        Result := TREWordCharCode.Create(FRegExp, FLex.Options,
          FLex.Token = tkNEWordChar);
        FRegExp.FCodeList.Add(Result);
        FLex.GetToken;
      end;
    tkDigitChar, tkNEDigitChar:
      begin
        Result := TREDigitCharCode.Create(FRegExp, FLex.Options,
          FLex.Token = tkNEDigitChar);
        FRegExp.FCodeList.Add(Result);
        FLex.GetToken;
      end;
    tkSpaceChar, tkNESpaceChar:
      begin
        Result := TRESpaceCharCode.Create(FRegExp, FLex.Options,
          FLex.Token = tkNESpaceChar);
        FRegExp.FCodeList.Add(Result);
        FLex.GetToken;
      end;
    tkCharClassFirst, tkNegativeCharClassFirst:
      begin
        Result := NewCharClassCode(FLex.Token = tkNegativeCharClassFirst);
        FLex.GetToken;
      end;
    tkTHead:
      begin
        Result := TRETextHeadCode.Create(FRegExp);
        FRegExp.FCodeList.Add(Result);
        FLex.GetToken;
      end;
    tkTTail:
      begin
        Result := TRETextTailCode.Create(FRegExp);
        FRegExp.FCodeList.Add(Result);
        FLex.GetToken;
        if not FHasTailAnchor then
          FHasTailAnchor := True;
      end;
    tkTTailEnd:
      begin
        Result := TRETextEndCode.Create(FRegExp);
        FRegExp.FCodeList.Add(Result);
        FLex.GetToken;
        if not FHasTailAnchor then
          FHasTailAnchor := True;
      end;
    tkReference, tkReferenceRelative:
      begin
        if (roNamedGroupOnly in FLex.Options) then
          FLex.Error(sUseNamedGroup, FLex.CurrentPosition);

        if FLex.Token = tkReference then
        begin
          LGroupNo := FLex.Min;
          if LGroupNo > FLex.FGroupCount then
            FLex.Error(Format(sInvalidGroupNumber, [LGroupNo]), FLex.GetCurrentPosition);
        end
        else
        begin
          if FGroupCount < FLex.Min then
            FLex.Error(Format(sInvalidGroupNumber, [0 - FLex.Min]));

          LGroupNo := FGroupCount - FLex.Min + 1;
        end;

        Result := TREReferenceCode.Create(FRegExp, LGroupNo, FLex.Options);
        FRegExp.FCodeList.Add(Result);

        AddJoinMatchGroup(LGroupNo);

        FLex.GetToken;
      end;
    tkNamedReference:
      begin
        LGroupNo := FRegExp.FGroups.IndexOfName(FLex.GroupName);
        if LGroupNo = -1 then
        begin
          New(RefError);
          RefError.GroupIndex := -1;
          RefError.AObject := nil;
          RefError.ErrorPos := FLex.GetCompileErrorPos;
          FReferenceErrorList.AddObject(FLex.GroupName, TObject(RefError));
        end;

        Result := TRENamedReferenceCode.Create(FRegExp, FLex.GroupName,
          LGroupNo, FLex.Options);

        if LGroupNo = -1 then
          PReferenceErrorRec(FReferenceErrorList.Objects
            [FReferenceErrorList.Count - 1]).AObject := Result
        else
          AddJoinMatchGroup(LGroupNo);

        FRegExp.FCodeList.Add(Result);
        FLex.GetToken;
      end;
    tkWordBoundary, tkNEWordBoundary:
      begin
        Result := TREBoundaryCode.Create(FRegExp, FLex.Options,
          FLex.Token = tkNEWordBoundary);
        FRegExp.FCodeList.Add(Result);
        FLex.GetToken;
      end;
    tkHorizontalSpaceChar, tkNEHorizontalSpaceChar:
      begin
        Result := TREHorizontalSpaceCharCode.Create(FRegExp,
          FLex.Token = tkNEHorizontalSpaceChar);
        FRegExp.FCodeList.Add(Result);
        FLex.GetToken;
      end;
    tkVerticalSpaceChar, tkNEVerticalSpaceChar:
      begin
        Result := TREVerticalSpaceCharCode.Create(FRegExp,
          FLex.Token = tkNEVerticalSpaceChar);
        FRegExp.FCodeList.Add(Result);
        FLex.GetToken;
      end;
    tkLineBreak:
      begin
        Result := TRELineBreakCharCode.Create(FRegExp);
        FRegExp.FCodeList.Add(Result);
        FLex.GetToken;
      end;
    tkGroupBegin:
      begin
        FLex.PushOptions;

        Inc(FGroupCount);
        Inc(FGroupLevel);
        FGroupStack.Push(Pointer(FCurrentGroup));
        FCurrentGroup := FGroupCount;

        LGroupName := FLex.FGroupName;

        if FGroupCount > FRegExp.FGroups.Count - 1 then
          LGroupNo := FRegExp.FGroups.Add(FLex.GroupName, nil, nil)
        else
        begin
          LGroupNo := FGroupCount;
          if (LGroupName <> '') and
              (FRegExp.Groups.IndexOfName(LGroupName) = -1) then
            FRegExp.FGroups.AddGroupName(LGroupName, LGroupNo);
        end;

        FRegExp.FGroups[LGroupNo].FSubExp := FLex.FP;

        FLex.GetToken;
        Result := NewBinCode(opGroup, RegExpr, nil);
        if FLex.Token <> tkRPar then
          FLex.Error(sUnmatchedSmallPar);

        FCurrentGroup := Integer(FGroupStack.Pop);

        Dec(FGroupLevel);

        if FHasRecursion then
          CheckRecursion(Result);

        (Result as TREBinCode).GroupIndex := LGroupNo;
        if LGroupName <> '' then
          (Result as TREBinCode).GroupName := LGroupName;

        FLex.PopOptions;
        FLex.GetToken;
      end;
    tkLPar:
      begin
        FLex.PushOptions;

        FLex.GetToken;
        Result := RegExpr;
        if FLex.Token <> tkRPar then
          FLex.Error(sUnmatchedSmallPar);
        FLex.PopOptions;
        FLex.GetToken;
      end;
    tkLParWithOption:
      begin
        FLex.PushOptions;
        FLex.UpdateOptions;

        FLex.GetToken;
        Result := RegExpr;
        if FLex.Token <> tkRPar then
          FLex.Error(sUnmatchedSmallPar);
        FLex.PopOptions;
        FLex.GetToken;
      end;
    tkOption:
      begin
        FLex.UpdateOptions;
        if FGroupLevel = 0 then
          FRegExp.Options := FLex.Options;
        FLex.GetToken;
        Result := RegExpr;
      end;
{$IFDEF USE_UNICODE_PROPERTY}
    tkProperty:
      begin
        Result := TREPropertyCode.Create(FRegExp, FLex.UnicodeProperty, False);
        FRegExp.FCodeList.Add(Result);
        FLex.GetToken;
      end;
    tkNEProperty:
      begin
        Result := TREPropertyCode.Create(FRegExp, FLex.UnicodeProperty, True);
        FRegExp.FCodeList.Add(Result);
        FLex.GetToken;
      end;
{$ENDIF USE_UNICODE_PROPERTY}
    tkPosixBracket, tkNEPosixBracket:
      begin
        Result := TREPosixCharClassCode.Create(FRegExp, FLex.PosixClass,
          FLex.Options, FLex.Token = tkNEPosixBracket);
        FRegExp.FCodeList.Add(Result);
        FLex.GetToken;
      end;
    tkNoBackTrack:
      begin
        FLex.GetToken;
        Result := NewBinCode(opNoBackTrack, RegExpr, nil);
        if FLex.Token <> tkRPar then
          FLex.Error(sUnmatchedSmallPar);
        FLex.GetToken;
      end;
    tkKeepPattern:
      begin
        FLex.GetToken;
        Result := NewBinCode(opKeepPattern, RegExpr, nil);
      end;
{$IFDEF USE_UNICODE_PROPERTY}
    tkCombiningSequence:
      begin
        Result := TRECombiningSequence.Create(FRegExp);
        FRegExp.FCodeList.Add(Result);
        FLex.GetToken;
      end;
{$ENDIF USE_UNICODE_PROPERTY}
    tkGlobalPos:
      begin
        Result := TREGlobalPosCode.Create(FRegExp);
        FRegExp.FCodeList.Add(Result);
        FLex.GetToken;
      end;
    tkGoSub, tkGoSubRelative:
      begin
        if roNamedGroupOnly in FLex.Options then
          FLex.Error(sUseNamedGroup, FLex.CurrentPosition);

        if FLex.Token = tkGoSub then
        begin
          LGroupNo := FLex.Min;
        end
        else
        begin
          LGroupBuffer := FLex.Min;

          if LGroupBuffer < 0 then
            LGroupNo := FGroupCount - LGroupBuffer - 1
          else
            LGroupNo := FGroupCount + LGroupBuffer;

          if LGroupNo < 1 then
            FRegExp.Error(Format(sRangeOverGroupNumber, [LGroupBuffer]));

          if LGroupNo < 0 then
            LGroupNo := FGroupCount - LGroupNo - 1
          else
            LGroupNo := FGroupCount + LGroupNo - 1;
        end;

        Result := NewBinCode(opGoSub, nil, nil);
        (Result as TREBinCode).GroupIndex := LGroupNo;

        FHasRecursion := not FHasRecursion and (LGroupNo = FGroupLevel);

        if not FHasGoSub then
          FHasGoSub := True;

        FLex.GetToken;
      end;
    tkGoSubName:
      begin
        LGroupNo := FRegExp.FGroups.IndexOfName(FLex.GroupName);
        if LGroupNo = -1 then
        begin
          New(RefError);
          RefError.GroupIndex := -1;
          RefError.AObject := nil;
          RefError.ErrorPos := FLex.GetCompileErrorPos;
          FGoSubErrorList.AddObject(FLex.GroupName, TObject(RefError));
        end;

        Result := NewBinCode(opGoSub, nil, nil);

        (Result as TREBinCode).GroupName := FLex.GroupName;
        (Result as TREBinCode).GroupIndex := LGroupNo;

        if LGroupNo <> -1 then
          FHasRecursion := LGroupNo <= FGroupLevel
        else
          FHasRecursion := False;

        if not FHasGoSub then
          FHasGoSub := True;

        FLex.GetToken;
      end;
    tkAheadMatch:
      begin
        FLex.GetToken;
        Result := NewBinCode(opAheadMatch, RegExpr, nil);
        if FLex.Token <> tkRPar then
          FLex.Error(sUnmatchedSmallPar);
        FLex.GetToken;
      end;
    tkAheadNoMatch:
      begin
        FLex.GetToken;
        Result := NewBinCode(opAheadNoMatch, RegExpr, nil);
        if FLex.Token <> tkRPar then
          FLex.Error(sUnmatchedSmallPar);
        FLex.GetToken;
      end;
    tkBehindMatch:
      begin
        FLex.GetToken;
        SubCode := RegExpr;
        Result := NewBinCode(opBehindMatch, SubCode, nil);
        if FLex.Token <> tkRPar then
          FLex.Error(sUnmatchedSmallPar);

        CheckVariableLength(SubCode);

        FLex.GetToken;
      end;
    tkBehindNoMatch:
      begin
        FLex.GetToken;
        SubCode := RegExpr;
        Result := NewBinCode(opBehindNoMatch, SubCode, nil);
        if FLex.Token <> tkRPar then
          FLex.Error(sUnmatchedSmallPar);

        CheckVariableLength(SubCode);

        FLex.GetToken;
      end;
    tkIfMatchRef:
      begin
        FLex.PushOptions;

        if FLex.Min = 0 then
        begin
          CondCode := TREIfThenNamedReferenceCode.Create(FRegExp,
            FLex.GroupName);
          FRegExp.FCodeList.Add(CondCode);
        end
        else
        begin
          CondCode := TREIfThenReferenceCode.Create(FRegExp, FLex.Min);
          FRegExp.FCodeList.Add(CondCode);
        end;

        FLex.GetToken;
        Result := Term;
        if FLex.Token = tkUnion then
        begin
          FLex.GetToken;
          SubCode := Term;
        end
        else
          SubCode := nil;

        if FLex.Token <> tkRPar then
          FLex.Error(sUnmatchedSmallPar);

        Result := NewBinCode(opIfThen, Result, SubCode);
        Result := NewBinCode(opIfMatch, CondCode, Result);

        FLex.PopOptions;
        FLex.GetToken;
      end;
    tkInSubRef:
      begin
        FLex.PushOptions;

        if FLex.GroupName <> '' then
        begin
          CondCode := TREInSubNameReferenceCode.Create(FRegExp, FLex.GroupName);
          FRegExp.FCodeList.Add(CondCode);
        end
        else
        begin
          CondCode := TREInSubReferenceCode.Create(FRegExp, FLex.Min);
          FRegExp.FCodeList.Add(CondCode);
        end;

        FLex.GetToken;
        Result := Term;
        if FLex.Token = tkUnion then
        begin
          FLex.GetToken;
          SubCode := Term;
        end
        else
          SubCode := nil;

        if FLex.Token <> tkRPar then
          FLex.Error(sUnmatchedSmallPar);

        Result := NewBinCode(opIfThen, Result, SubCode);
        Result := NewBinCode(opIfMatch, CondCode, Result);

        FLex.PopOptions;
        FLex.GetToken;
      end;
    tkIfMatch:
      begin
        FLex.PushOptions;

        FLex.GetToken;
        if not(FLex.Token in [tkAheadMatch, tkAheadNoMatch, tkBehindMatch,
          tkBehindNoMatch, tkCallout]) then
          FLex.Error(sInvalideCondition);

        CondCode := Primay;

        if CondCode = nil then
          FLex.Error(sInvalideCondition);

        Result := Term;
        if FLex.Token = tkUnion then
        begin
          FLex.GetToken;
          SubCode := Term;
        end
        else
          SubCode := nil;

        if FLex.Token <> tkRPar then
          FLex.Error(sContainsTooManyBranches);

        Result := NewBinCode(opIfThen, Result, SubCode);
        Result := NewBinCode(opIfMatch, CondCode, Result);

        FLex.PopOptions;
        FLex.GetToken;
      end;
    tkDefine:
      begin
        FLex.PushOptions;

        FLex.GetToken;

        Result := NewBinCode(opIfThen, Term, nil);
        Result := NewBinCode(opDefine, Result, nil);

        if FLex.Token <> tkRPar then
          FLex.Error(sContainsTooManyBranches);

        FLex.PopOptions;
        FLex.GetToken;
      end;
    tkBranchReset:
      begin
        FLex.PushOptions;
        LGroupNo := FGroupCount;
        LGroupBuffer := LGroupNo;

        FLex.GetToken;

        Result := Term;
        while FLex.Token = tkUnion do
        begin
          LGroupBuffer := FGroupCount;
          FGroupCount := LGroupNo;
          FLex.GetToken;
          Result := NewBinCode(opUnion, Result, Term);
        end;

        if FLex.Token <> tkRPar then
          FLex.Error(sUnmatchedSmallPar);

        FGroupCount := LGroupBuffer;

        FLex.PopOptions;
        FLex.GetToken;
      end;
    tkCallout:
      begin
        Result := TRECalloutCode.Create(FRegExp, FLex.Min,
          FLex.CurrentPosition, 1);
        FRegExp.FCodeList.Add(Result);
        FLex.GetToken;
      end;
    tkEmpty:
      begin
        Result := NewBinCode(opEmply, nil, nil);
        FLex.GetToken;
      end;
    tkFail:
      begin
        Result := NewBinCode(opFail, nil, nil);
        FLex.GetToken;
      end;
    tkbcPrune:
      begin
        Result := NewBinCode(opPrune, nil, nil);
        if FLex.GroupName <> '' then
        begin
          LGroupNo := FRegExp.FVerbNames.IndexOf(FLex.GroupName);
          if LGroupNo = -1 then
            LGroupNo := FRegExp.FVerbNames.Add(FLex.GroupName);
        end
        else
          LGroupNo := -1;

        (Result as TREBinCode).GroupIndex := LGroupNo;

        FLex.GetToken;
      end;
    tkbcSkip:
      begin
        Result := NewBinCode(opSkip, nil, nil);
        if FLex.GroupName <> '' then
        begin
          LGroupNo := FRegExp.FVerbNames.IndexOf(FLex.GroupName);
          if LGroupNo = -1 then
            LGroupNo := FRegExp.FVerbNames.Add(FLex.GroupName);
        end
        else
          LGroupNo := -1;

        (Result as TREBinCode).GroupIndex := LGroupNo;

        FLex.GetToken;
      end;
    tkbcMark:
      begin
        Result := NewBinCode(opMark, nil, nil);
        LGroupNo := FRegExp.FVerbNames.IndexOf(FLex.GroupName);
        if LGroupNo = -1 then
          LGroupNo := FRegExp.FVerbNames.Add(FLex.GroupName);

        (Result as TREBinCode).GroupIndex := LGroupNo;

        FLex.GetToken;
      end;
    tkbcThen:
      begin
        Result := NewBinCode(opThen, nil, nil);
        if FLex.GroupName <> '' then
        begin
          LGroupNo := FRegExp.FVerbNames.IndexOf(FLex.GroupName);
          if LGroupNo = -1 then
            LGroupNo := FRegExp.FVerbNames.Add(FLex.GroupName);
        end
        else
          LGroupNo := -1;

        (Result as TREBinCode).GroupIndex := LGroupNo;

        FLex.GetToken;
      end;
    tkbcCommit:
      begin
        Result := NewBinCode(opCommint, nil, nil);
        if FLex.GroupName <> '' then
        begin
          LGroupNo := FRegExp.FVerbNames.IndexOf(FLex.GroupName);
          if LGroupNo = -1 then
            LGroupNo := FRegExp.FVerbNames.Add(FLex.GroupName);
        end
        else
          LGroupNo := -1;

        (Result as TREBinCode).GroupIndex := LGroupNo;

        FLex.GetToken;
      end;
    tkbcAccept:
      begin
        Result := NewBinCode(opAccept, nil, nil);
        FLex.GetToken;
      end;
    tkEOLType:
      begin
        FRegExp.LineBreakKind := FLex.LineBreakKind;
        FLex.GetToken;
      end;
  end;

  if Assigned(CalloutCode) then
  begin
    Result := NewBinCode(opConcat, CalloutCode, Result);
  end;
end;

function TREParser.RegExpr: TRECode;
begin
  Result := Term;
  while FLex.Token = tkUnion do
  begin
    FLex.GetToken;
    Result := NewBinCode(opUnion, Result, Term);
  end;
end;

function TREParser.Term: TRECode;
begin
  if (FLex.Token = tkUnion) or (FLex.Token = tkRPar) or
    (FLex.Token = tkEnd) then
    Result := NewBinCode(opEmply, nil, nil)
  else
  begin
    Result := Factor;
    while (FLex.Token <> tkUnion) and (FLex.Token <> tkRPar) and
      (FLex.Token <> tkEnd) do
      Result := NewBinCode(opConcat, Result, Factor);
  end;
end;

{ TREOptimizeData }

{$IFDEF SKREGEXP_DEBUG}

function TREOptimizeData.DebugOutput: REString;

  function GetKindStr(AKind: TREOptimizeDataKind): REString;
  begin
    case AKind of
      odkLead:
        Result := 'Lead';
      odkTail:
        Result := 'Tail';
      odkLineHead:
        Result := 'Line Head';
      odkLineTail:
        Result := 'Line Tail';
      odkTextHead:
        Result := 'Text Head';
      odkTextTail:
        Result := 'Text Tail';
    else
      Result := 'Anchor';
    end;
  end;

begin
  Result := Format('(%s) "%s" at (%d, %d), branch:%d',
    [GetKindStr(FKind), FCode.GetDebugStr,
      FOffset.Min, FOffset.Max, FBranchLevel])
end;
{$ENDIF SKREGEXP_DEBUG}

{ TREOptimizeDataCollection }

function TREOptimizeDataCollection.Add(Value: TRECode;
  AKind: TREOptimizeDataKind; ABranchLevel: Integer; AOffset: TRETextPosRec): Integer;
var
  Item: TREOptimizeData;
begin
  Item := TREOptimizeData.Create;
  Item.Code := Value;
  Item.Kind := AKind;
  Item.BranchLevel := ABranchLevel;
  Item.Offset := AOffset;
  Result := FList.Add(Item);
end;

procedure TREOptimizeDataCollection.Clear;
begin
  FList.Clear;
end;

constructor TREOptimizeDataCollection.Create;
begin
  inherited;
  FList := TObjectList.Create;
end;

{$IFDEF SKREGEXP_DEBUG}
procedure TREOptimizeDataCollection.DebugOutput(ADest: TStrings);
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    ADest.Add(TREOptimizeData(FList[I]).DebugOutput);
end;
{$ENDIF SKREGEXP_DEBUG}

procedure TREOptimizeDataCollection.Delete(Index: Integer);
begin
  FList.Delete(Index);
end;

destructor TREOptimizeDataCollection.Destroy;
begin
  FList.Free;
  inherited;
end;

procedure TREOptimizeDataCollection.GetAnchorCode(ABranchCount: Integer;
  ADest: TList);
var
  I: Integer;
  BranchBuf: array of TREOptimizeData;
begin
  ADest.Clear;

  SetLength(BranchBuf, ABranchCount + 1);

  for I := 0 to FList.Count - 1 do
  begin
    if (GetItem(I).Kind = odkNormal) or (GetItem(I).Kind = odkExist) or
      ((GetItem(I).Kind = odkTail) and (GetItem(I).BranchLevel > 0)) then
    begin
      if BranchBuf[GetItem(I).BranchLevel] = nil then
      begin
        BranchBuf[GetItem(I).BranchLevel] := GetItem(I);
      end
      else
      begin
        if (BranchBuf[GetItem(I).BranchLevel].Offset.Min = -1) and
            (GetItem(I).Offset.Min > -1) then
        begin
          //offset.min が -1 でなければマッチ位置を特定できる
          BranchBuf[GetItem(I).BranchLevel] := GetItem(I);
        end
        else if (BranchBuf[GetItem(I).BranchLevel].Code is TRELiteralCode) and
            (GetItem(I).Code is TRELiteralCode) then
        begin
          //文字列が長い方がマッチ位置の候補を絞り込める
          if (BranchBuf[GetItem(I).BranchLevel].Code.Length < GetItem(I).Code.Length) then
            BranchBuf[GetItem(I).BranchLevel] := GetItem(I)
          else if (((BranchBuf[GetItem(I).BranchLevel].Code as TRELiteralCode).FSubP^ < #128) and
            IsWordA(UChar(
              (BranchBuf[GetItem(I).BranchLevel].Code as TRELiteralCode).FSubP^))) and
              (not ((GetItem(I).Code as TRELiteralCode).FSubP^ < #128) and
              IsWordA(UChar((GetItem(I).Code as TRELiteralCode).FSubP^))) then
            //アルファベット以外の文字の方がマッチ位置の候補を絞り込める
            BranchBuf[GetItem(I).BranchLevel] := GetItem(I);
        end
        else
        begin
          if (not (BranchBuf[GetItem(I).BranchLevel].Code is TRELiteralCode)) and
              (GetItem(I).Code is TRELiteralCode) then
            BranchBuf[GetItem(I).BranchLevel] := GetItem(I);
        end;
      end;
    end;
  end;

  if Length(BranchBuf) > 1 then
  begin
    for I := 1 to High(BranchBuf) do
    begin
      if BranchBuf[I] = nil then
      begin
        ADest.Clear;
        Break;
      end;
      ADest.Add(BranchBuf[I]);
    end;

    if BranchBuf[0] <> nil then
      ADest.Add(BranchBuf[0]);
  end
  else if (Length(BranchBuf) = 1) and (BranchBuf[0] <> nil) then
    ADest.Add(BranchBuf[0]);
end;

function TREOptimizeDataCollection.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TREOptimizeDataCollection.GetItem(Index: Integer): TREOptimizeData;
begin
  Result := FList[Index] as TREOptimizeData;
end;

procedure TREOptimizeDataCollection.GetLeadCode(ABranchCount: Integer;
  ADest: TList);
var
  I: Integer;
  BranchBuf: array of Boolean;
begin
  ADest.Clear;

  SetLength(BranchBuf, ABranchCount + 1);

  for I := 0 to FList.Count - 1 do
  begin
    if GetItem(I).Kind = odkLead then
    begin
      if not BranchBuf[GetItem(I).BranchLevel] then
        BranchBuf[GetItem(I).BranchLevel] := True;

      ADest.Add(GetItem(I));
    end;
  end;

  if Length(BranchBuf) > 1 then
  begin
    if not BranchBuf[0] then
    begin
      for I := 1 to High(BranchBuf) do
      begin
        if not BranchBuf[I] then
        begin
          ADest.Clear;
          Exit;
        end;
      end;
    end;
  end;
end;

procedure TREOptimizeDataCollection.GetTailCode(var ACode: TREOptimizeData);
var
  I, Len, P: Integer;
begin
  ACode := nil;
  P := -1;
  Len := 0;

  for I := 0 to FList.Count - 1 do
  begin
    if (GetItem(I).Kind = odkTail) and (GetItem(I).BranchLevel = 0) then
    begin
      if (P = -1) or (GetItem(I).Code.Length > Len) then
      begin
        P := I;
        Len := GetItem(I).Code.Length;
      end;
    end;
  end;

  if P <> -1 then
    ACode := GetItem(P);
end;

{ TREOptimizeDataList }

function TREOptimizeDataList.Add(Item: TREOptimizeData): Integer;
begin
  IsWork := False;
  Result := inherited Add(Item);
end;

function TREOptimizeDataList.Get(Index: Integer): TREOptimizeData;
begin
  Result := inherited Get(Index);
end;

function TREOptimizeDataList.IsEqual(const AStr: PWideChar): Boolean;
var
  I, L: Integer;
begin
  for I := 0 to Count - 1 do
    if Get(I).Code.IsEqual(AStr, L) then
    begin
      Result := True;
      Exit;
    end;
  Result := False;
end;

procedure TREOptimizeDataList.Put(Index: Integer; const Value: TREOptimizeData);
begin
  inherited Put(Index, Value);
end;

{ TRENFAState }

{$IFDEF SKREGEXP_DEBUG}

constructor TRENFAState.Create(ARegExp: TSkRegExp);
begin
  inherited Create;
  FRegExp := ARegExp;
end;

function TRENFAState.GetString: REString;

  function GetVerbName(Index: Integer): REString;
  begin
    if Index <> -1 then
      Result := FRegExp.FVerbNames[Index];
  end;

var
  MatchTypeStr: REString;
begin
  if FKind = nkEmpty then
    Result := Format(sFmtDumpNFA_Empty, [FTransitTo])
  else if FKind = nkLoop then
  begin
    case FMatchKind of
      lkReluctant:
        MatchTypeStr := 'R';
      lkPossessive:
        MatchTypeStr := 'P';
    else
      MatchTypeStr := 'G';
    end;
    Result := Format(sFmtDumpNFA_Loop, [MatchTypeStr, FMin, FMax, FTransitTo]);
  end
  else if FKind = nkEnd then
    Result := Format(sFmtDumpNFA_EndStr, [FTransitTo])
  else if FKind = nkLoopExit then
    Result := Format(sFmtDumpNFA_LoopExit, [FTransitTo])
  else if FKind = nkLoopEnd then
    Result := Format(sFmtDumpNFA_LoopEnd, [FTransitTo])
  else if FKind = nkStar then
  begin
    case FMatchKind of
      lkReluctant:
        MatchTypeStr := 'R';
      lkPossessive:
        MatchTypeStr := 'P';
    else
      MatchTypeStr := 'G';
    end;
    Result := Format(sFmtDumpNFA_Star, [MatchTypeStr, FTransitTo, FCode.GetDebugStr]);
  end
  else if FKind = nkPlus then
  begin
    case FMatchKind of
      lkReluctant:
        MatchTypeStr := 'R';
      lkPossessive:
        MatchTypeStr := 'P';
    else
      MatchTypeStr := 'G';
    end;
    Result := Format(sFmtDumpNFA_Plus, [MatchTypeStr, FTransitTo, FCode.GetDebugStr]);
  end
  else if FKind = nkBound then
  begin
    case FMatchKind of
      lkReluctant:
        MatchTypeStr := 'R';
      lkPossessive:
        MatchTypeStr := 'P';
    else
      MatchTypeStr := 'G';
    end;
    Result := Format(sFmtDumpNFA_Bound, [MatchTypeStr, FMin, FMax, FTransitTo, FCode.GetDebugStr]);
  end
  else if FKind = nkMatchEnd then
    Result := Format(sFmtDumpNFA_MatchEnd, [FTransitTo])
  else if FKind = nkGroupBegin then
    Result := Format(sFmtDumpNFA_GroupBegin, [FGroupIndex, FTransitTo])
  else if FKind = nkGroupEnd then
    Result := Format(sFmtDumpNFA_GroupEnd, [FGroupIndex, FTransitTo])
  else if FKind = nkSuspend then
    Result := Format(sFmtDumpNFA_SuspendBegin, [FTransitTo])
  else if FKind = nkAheadMatch then
    Result := Format(sFmtDumpNFA_AheadMatch, [FTransitTo])
  else if FKind = nkAheadNoMatch then
    Result := Format(sFmtDumpNFA_AheadNoMatch, [FTransitTo])
  else if FKind = nkBehindMatch then
    Result := Format(sFmtDumpNFA_BehindMatch, [FTransitTo])
  else if FKind = nkBehindNoMatch then
    Result := Format(sFmtDumpNFA_BehindNoMatch, [FTransitTo])
  else if FKind = nkGoSub then
    Result := Format(sFmtDumpNFA_GoSub, [FGroupIndex, FTransitTo])
  else if FKind = nkIfMatch then
    Result := Format(sFmtDumpNFA_IfMatch, [FTransitTo])
  else if FKind = nkIfThen then
    Result := Format(sFmtDumpNFA_IfThen, [FTransitTo])
  else if FKind = nkKeepPattern then
    Result := Format(sFmtDumpNFA_KeepPattern, [FTransitTo])
  else if FKind = nkDefine then
    Result := Format(sFmtDumpNFA_Define, [FTransitTo])
  else if FKind = nkFail then
    Result := Format(sFmtDumpNFA_Fail, [FTransitTo])
  else if FKind = nkPrune then
    Result := Format(sFmtDumpNFA_Prune, [GetVerbName(FGroupIndex), FTransitTo])
  else if FKind = nkSkip then
    Result := Format(sFmtDumpNFA_Skip, [GetVerbName(FGroupIndex), FTransitTo])
  else if FKind = nkThen then
    Result := Format(sFmtDumpNFA_Then, [GetVerbName(FGroupIndex), FTransitTo])
  else if FKind = nkMark then
    Result := Format(sFmtDumpNFA_Mark, [GetVerbName(FGroupIndex), FTransitTo])
  else if FKind = nkCommit then
    Result := Format(sFmtDumpNFA_Commit, [GetVerbName(FGroupIndex), FTransitTo])
  else if FKind = nkAccept then
    Result := Format(sFmtDumpNFA_Accept, [FTransitTo])
  else
  begin
    if FCode <> nil then
      Result := Format(sFmtDumpNFA_Null, [(FCode as TRECode).GetDebugStr,
        FTransitTo]);
  end;

end;
{$ENDIF SKREGEXP_DEBUG}

{ TRENFA }

function TRENFA.AddTransition(AKind: TRENFAKind; ATransFrom, ATransTo: Integer;
  ACode: TRECode; AGroupIndex: Integer; ABranchIndex: Integer; AMin: Integer;
  AMax: Integer): TRENFAState;
var
  NFACode: TRENFAState;
begin
{$IFDEF SKREGEXP_DEBUG}
  NFACode := TRENFAState.Create(FRegExp);
{$ELSE}
  NFACode := TRENFAState.Create;
{$ENDIF SKREGEXP_DEBUG}
{$IFDEF SKREGEXP_DEBUG}
  NFACode.Index := ATransFrom;
{$ENDIF}
  NFACode.Kind := AKind;
  NFACode.Code := ACode;
  NFACode.TransitTo := ATransTo;
  NFACode.Next := TRENFAState(FStateList[ATransFrom]);
  NFACode.Min := AMin;
  NFACode.Max := AMax;
  NFACode.BranchIndex := ABranchIndex;
  NFACode.GroupIndex := AGroupIndex;

  FStateList[ATransFrom] := NFACode;
  Result := NFACode;
end;

procedure TRENFA.Compile(AParser: TREParser);
var
  BranchCount: Integer;
  Offset, MatchLen: TRETextPosRec;
  State: TRENFAOptimizeState;
begin
  FParser := AParser;
  FRegExp.ClearStateList;

  FRegExp.FEntryState := GetNumber;
  FBEntryState := FRegExp.FEntryState;
  FRegExp.FExitState := GetNumber;
  FBExitState := FRegExp.FExitState;

  FEntryStack.Clear;
  FExitStack.Clear;
  FEntryStackIndex := 0;
  FExitStateIndex := 0;
  FGroupCount := 0;
  FHasAccept := False;
  FBranchIndex := 0;
  FInBranch := False;

  FOptimizeData.Clear;
  FRegExp.FLoopState.Clear;
  FRegExp.FBranchCount := 0;
  FRegExp.FHasReference := AParser.HasReference;

  BranchCount := 0;
  Offset.Min := 0;
  Offset.Max := 0;

  if AParser.HasGoSub then
  begin
    MatchLen.Min := 0;
    MatchLen.Max := 0;
    CalculateGroupLength(FRegExp.FCode, MatchLen, False, True);
  end;

  MatchLen.Min := 0;
  MatchLen.Max := 0;
  State.IsNullMatch := False;
  State.IsJoinMatch := True;

  AddTransition(nkEnd, FRegExp.FExitState, -1, nil, 0, 0);
  GenerateStateList(FRegExp.FCode, FRegExp.FEntryState, FRegExp.FExitState,
    BranchCount, MatchLen, Offset, 0, State);

  FRegExp.FMinMatchLength := MatchLen.Min;
  FRegExp.FMaxMatchLength := MatchLen.Max;
  FRegExp.FGroups.CheckSameGroupName;
end;

constructor TRENFA.Create(ARegExp: TSkRegExp);
begin
  inherited Create;
  FRegExp := ARegExp;
  FStateList := ARegExp.FStateList;
  FOptimizeData := ARegExp.FOptimizeData;
  FEntryStack := TList.Create;
  FExitStack := TList.Create;
  FStateStack := TList.Create;
  FBranchStack := TList.Create;
  FEntryStackIndex := 0;
  FExitStateIndex := 0;
  FStateStackIndex := 0;
  FBranchIndex := 0;
end;

destructor TRENFA.Destroy;
begin
  FBranchStack.Free;
  FStateStack.Free;
  FEntryStack.Free;
  FExitStack.Free;
  inherited;
end;

procedure TRENFA.GenerateStateList(ACode: TRECode; AEntry, AWayout: Integer;
  var ABranchLevel: Integer; var AMatchLen: TRETextPosRec;
  var AOffset: TRETextPosRec; AGroupIndex: Integer;
  AState: TRENFAOptimizeState);

  function GetAnchorKind(AState: TRENFAOptimizeState): TREOptimizeDataKind;
  begin
    if AState.IsNullMatch then
      Result := odkExist
    else if FBEntryState = AEntry then
      Result := odkLead
    else if AWayout = FBExitState then
      Result := odkTail
    else
      Result := odkNormal;
  end;

var
  State1, State2, Index: Integer;
  LMin, LMax, LLoopIndex: Integer;
  LOffset, ROffset, LLen, RLen: TRETextPosRec;
  SubCode: TRECode;
  NFACode: TRENFAState;
  BranchState: PREBranchStateRec;
  IsPush: Boolean;
  SubOption: TREOptions;
  LLoopState: TRELoopStateItem;
begin
  if ACode is TREBinCode then
  begin
    with ACode as TREBinCode do
    begin
      case Op of
        opUnion:
          begin
            IsPush := False;
            if (FBranchStack.Count = 0) or
              (PREBranchStateRec(FBranchStack[FBranchStack.Count - 1]).State <> AEntry) then
            begin
              IsPush := True;
              New(BranchState);
              BranchState^.State := AEntry;
              BranchState^.Count := 0;
              FBranchStack.Add(BranchState);

              Inc(ABranchLevel);
              Inc(FBranchIndex);
              FInBranch := (FBranchIndex > 1) or (FBranchStack.Count > 1);
            end;

            RLen.Min := 0;
            RLen.Max := 0;
            LLen.Min := 0;
            LLen.Max := 0;

            ROffset := AOffset;
            LOffset := AOffset;

            GenerateStateList(Right, AEntry, AWayout, ABranchLevel, RLen,
              ROffset, AGroupIndex, AState);

            Inc(ABranchLevel);
            GenerateStateList(Left, AEntry, AWayout, ABranchLevel, LLen,
              LOffset, AGroupIndex, AState);

            if (AMatchLen.Min > -1) and (LLen.Min > -1) then
              Inc(AMatchLen.Min, SkRegExpW.Min(RLen.Min, LLen.Min))
            else
              AMatchLen.Min := -1;

            if (AMatchLen.Max > -1) and (LLen.Max > -1) then
              Inc(AMatchLen.Max, SkRegExpW.Max(RLen.Max, LLen.Max))
            else
              AMatchLen.Max := -1;

            if (AOffset.Min > -1) and (LOffset.Min > -1) and (ROffset.Min > -1) then
              Inc(AOffset.Min, SkRegExpW.Min(LOffset.Min, ROffset.Min))
            else
              AOffset.Min := -1;

            if (AOffset.Max > -1) and (LOffset.Max > -1) and (ROffset.Max > -1) then
              Inc(AOffset.Max, SkRegExpW.Max(LOffset.Max, ROffset.Max))
            else
              AOffset.Max := -1;

            FRegExp.FBranchCount :=
              SkRegExpW.Max(FRegExp.FBranchCount, ABranchLevel);
            Dec(ABranchLevel);

            if IsPush then
            begin
              Dec(ABranchLevel);

              BranchState := FBranchStack[FBranchStack.Count - 1];
              Dispose(BranchState);
              FBranchStack.Delete(FBranchStack.Count - 1);
            end;
          end;
        opConcat:
          begin
            State1 := GetNumber;
            GenerateStateList(Left, AEntry, State1, ABranchLevel, AMatchLen,
              AOffset, AGroupIndex, AState);
            GenerateStateList(Right, State1, AWayout, ABranchLevel, AMatchLen,
              AOffset, AGroupIndex, AState);
          end;
        opStar:
          begin
            if (FBEntryState = AEntry) and (MatchKind = lkAny) and
                (ABranchLevel = 0) and
                (not FRegExp.FGroups[AGroupIndex].JoinMatch) and
                not FParser.HasTailAnchor then
            begin
              State1 := GetNumber;

              SubOption := TREAnyCharCode(Left).FOptions;
              if not(roSingleLine in SubOption) then
                Include(SubOption, roMultiLine);

              SubCode := TRELineHeadCode.Create(FRegExp, SubOption);
              FRegExp.FCodeList.Add(SubCode);

              AddTransition(nkNormal, AEntry, State1, SubCode, AGroupIndex,
                ABranchLevel, 0, CONST_LoopMax);

              if not FInBranch then
                FOptimizeData.Add(SubCode, odkLead, ABranchLevel, AOffset);

              NFACode := AddTransition(nkStar, State1, AWayout, Left, AGroupIndex,
                ABranchLevel, 0, CONST_LoopMax);
            end
            else
              NFACode := AddTransition(nkStar, AEntry, AWayout, Left, AGroupIndex,
                ABranchLevel, 0, CONST_LoopMax);

            AOffset.Max := -1;
            AMatchLen.Max := -1;

            LLoopState := TRELoopStateItem.Create;
            LLoopIndex := FRegExp.FLoopState.Add(LLoopState);

            NFACode.MatchKind := MatchKind;
            NFACode.ExtendTo := AWayout;
            NFACode.LoopIndex := LLoopIndex;
            LLoopState.NFACode := NFACode;
          end;
        opPlus:
          begin
            NFACode := AddTransition(nkPlus, AEntry, AWayout, Left, AGroupIndex,
              ABranchLevel, 1, CONST_LoopMax);

            if not FHasAccept then
            begin
              if AState.IsJoinMatch and not Left.IsVariable and (FBEntryState = AEntry) then
              begin
                if not AState.IsNullMatch and not FInBranch then
                  FOptimizeData.Add(Left, odkLead, ABranchLevel, AOffset);
              end;
            end;

            if (AMatchLen.Min > -1) and (Left.CharLength.Min > -1) then
              Inc(AMatchLen.Min, Left.CharLength.Min)
            else
              AMatchLen.Min := -1;

            AMatchLen.Max := -1;

            if (AOffset.Min > -1) and (Left.CharLength.Min > -1) then
              Inc(AOffset.Min, Left.CharLength.Min)
            else
              AOffset.Min := -1;

            AOffset.Max := -1;

            LLoopState := TRELoopStateItem.Create;
            LLoopIndex := FRegExp.FLoopState.Add(LLoopState);

            NFACode.MatchKind := MatchKind;
            NFACode.ExtendTo := AWayout;
            NFACode.LoopIndex := LLoopIndex;
            LLoopState.NFACode := NFACode;
          end;
        opBound:
          begin
            NFACode := AddTransition(nkBound, AEntry, AWayout, Left, AGroupIndex,
              ABranchLevel, FMin, FMax);

            LLoopState := TRELoopStateItem.Create;
            LLoopIndex := FRegExp.FLoopState.Add(LLoopState);

            NFACode.MatchKind := MatchKind;
            NFACode.ExtendTo := AWayout;
            NFACode.LoopIndex := LLoopIndex;
            LLoopState.NFACode := NFACode;

            if not FHasAccept then
            begin
              if (FMin > 0) and AState.IsJoinMatch and not Left.IsVariable then
              begin
                if not AState.IsNullMatch and not FInBranch then
                  FOptimizeData.Add(Left, GetAnchorKind(AState), ABranchLevel, AOffset);
              end;
            end;

            if (AMatchLen.Min > -1) and (Left.CharLength.Min > -1) then
              Inc(AMatchLen.Min, FMin * Left.CharLength.Min)
            else
              AMatchLen.Min := -1;

            if FMax = CONST_LoopMax then
              AMatchLen.Max := -1
            else
            begin
              if (AMatchLen.Max > -1) and (Left.CharLength.Max > -1) then
                Inc(AMatchLen.Max, FMax * Left.CharLength.Max)
              else
                AMatchLen.Max := -1;
            end;

            if (AOffset.Min <> -1) and (FMin > 0) and (ACode.CharLength.Min > -1) then
              Inc(AOffset.Min, FMin * ACode.CharLength.Min)
            else
              AOffset.Min := -1;

            if (AOffset.Max <> -1) and (FMax < CONST_LoopMax) and (ACode.CharLength.Max > -1) then
              Inc(AOffset.Max, FMax * ACode.CharLength.Max)
            else
              AOffset.Max := -1;
          end;
        opLoop:
          begin
            State1 := GetNumber;
            State2 := GetNumber;

            NFACode := AddTransition(nkLoop, AEntry, State1, nil, AGroupIndex,
              ABranchLevel, FMin, FMax);

            LLoopState := TRELoopStateItem.Create;
            LLoopIndex := FRegExp.FLoopState.Add(LLoopState);

            NFACode.MatchKind := MatchKind;
            NFACode.ExtendTo := State2;
            NFACode.LoopIndex := LLoopIndex;
            LLoopState.NFACode := NFACode;

            LLen.Min := 0;
            LLen.Max := 0;
            LOffset := AOffset;

            PushState(AEntry, AWayout, State1, State2);
            AState.IsNullMatch := FMin = 0;
            GenerateStateList(Left, State1, State2, ABranchLevel, LLen, LOffset,
              AGroupIndex, AState);
            PopState;

            if (AMatchLen.Min > -1) and (LLen.Min > -1) then
              Inc(AMatchLen.Min, LLen.Min * FMin)
            else
              AMatchLen.Min := -1;

            if FMax = CONST_LoopMax then
              AMatchLen.Max := -1
            else if (AMatchLen.Max > -1) and (LLen.Max > -1) then
              Inc(AMatchLen.Max, LLen.Max * FMax);

            if (AOffset.Min <> -1) then
              Inc(AOffset.Min, LLen.Min * FMin)
            else
              AOffset.Min := -1;

            if (AOffset.Max <> -1) and (FMax < CONST_LoopMax) then
              Inc(AOffset.Max, LLen.Max * FMax)
            else
              AOffset.Max := -1;

            NFACode := AddTransition(nkLoopExit, State1, AWayout, nil, AGroupIndex,
              ABranchLevel);

            NFACode.LoopIndex := LLoopIndex;
            NFACode.ExtendTo := AEntry;
            NFACode.Min := FMin;
            NFACode.Max := FMax;
            NFACode.MatchKind := FMatchKind;

            NFACode := AddTransition(nkLoopEnd, State2, State1, nil, AGroupIndex,
              ABranchLevel);

            NFACode.LoopIndex := LLoopIndex;
            NFACode.Min := FMin;
            NFACode.Max := FMax;
            NFACode.MatchKind := FMatchKind;
          end;
        opGroup:
          begin
            State1 := GetNumber;
            State2 := GetNumber;

            AddTransition(nkGroupBegin, AEntry, State1, nil, AGroupIndex, ABranchLevel);
            NFACode := FStateList[AEntry];
            NFACode.GroupIndex := GroupIndex;
            if GroupIndex > 0 then
              FRegExp.FGroups[GroupIndex].GroupBegin := NFACode;

            FStateStack.Add(Pointer(AEntry));
            Inc(FGroupCount);

            LLen.Min := 0;
            LLen.Max := 0;

            PushState(AEntry, AWayout, State1, State2);
            GenerateStateList(Left, State1, State2, ABranchLevel, LLen, AOffset,
              GroupIndex, AState);
            PopState;

            FRegExp.FGroups[GroupIndex].CharLength := LLen;

            if (AMatchLen.Min > -1) and (LLen.Min > -1) then
              Inc(AMatchLen.Min, LLen.Min)
            else
              AMatchLen.Min := -1;

            if (AMatchLen.Max > -1) and (LLen.Max > -1) then
              Inc(AMatchLen.Max, LLen.Max)
            else
              AMatchLen.Max := -1;

            Dec(FGroupCount);
            FStateStack.Delete(FStateStack.Count - 1);

            NFACode := AddTransition(nkGroupEnd, State2, AWayout, nil, AGroupIndex,
              ABranchLevel);
            NFACode.GroupIndex := GroupIndex;
            if GroupIndex > 0 then
              FRegExp.FGroups[GroupIndex].GroupEnd := NFACode;
          end;
        opNoBackTrack:
          begin
            State1 := GetNumber;
            State2 := GetNumber;

            NFACode := AddTransition(nkSuspend, AEntry, State1, nil, AGroupIndex,
              ABranchLevel);
            NFACode.ExtendTo := State2;

            PushState(AEntry, AWayout, State1, State2);
            GenerateStateList(Left, State1, State2, ABranchLevel, AMatchLen,
              AOffset, AGroupIndex, AState);
            PopState;

            AddTransition(nkMatchEnd, State2, AWayout, nil, AGroupIndex,
              ABranchLevel);
          end;
        opKeepPattern:
          begin
            State1 := GetNumber;

            AddTransition(nkKeepPattern, AEntry, State1, nil, AGroupIndex,
              ABranchLevel);
            GenerateStateList(Left, State1, AWayout, ABranchLevel, AMatchLen,
              AOffset, AGroupIndex, AState);
          end;
        opEmply:
          begin
            AddTransition(nkEmpty, AEntry, AWayout, nil, AGroupIndex,
              ABranchLevel);
          end;
        opGoSub:
          begin
            NFACode := AddTransition(nkGoSub, AEntry, AWayout, nil, AGroupIndex,
              ABranchLevel);

            if GroupIndex = -1 then
            begin
              Index := FRegExp.FGroups.IndexOfName(GroupName);
              Assert(Index <> -1, 'bug?: Not Define Group Index');
              NFACode.GroupIndex := Index;
            end
            else
              NFACode.GroupIndex := GroupIndex;

            LMin := FRegExp.FGroups[NFACode.GroupIndex].CharLength.Min;
            LMax := FRegExp.FGroups[NFACode.GroupIndex].CharLength.Max;

            if (AMatchLen.Min > -1) and (LMin > -1) then
              Inc(AMatchLen.Min, LMin)
            else
              AMatchLen.Min := -1;

            if (AMatchLen.Max > -1) and (LMax > -1) then
              Inc(AMatchLen.Max, LMax)
            else
              AMatchLen.Max := -1;

            if (AOffset.Min <> -1) and (LMin > -1) then
              Inc(AOffset.Min,  LMin)
            else
              AOffset.Min := -1;

            if (AOffset.Max <> -1) and (LMax > -1) then
              Inc(AOffset.Max,  LMax)
            else
              AOffset.Max := -1;
          end;
        opAheadMatch:
          begin
            State1 := GetNumber;
            State2 := GetNumber;

            NFACode := AddTransition(nkAheadMatch, AEntry, State1, nil, AGroupIndex,
              ABranchLevel);
            NFACode.ExtendTo := State2;

            LLen.Min := 0;
            LLen.Max := 0;
            LOffset.Min := -1;
            LOffset.Max := -1;
            AState.IsJoinMatch := False;
            AState.IsNullMatch := True;

            PushState(AEntry, AWayout, State1, State2);
            GenerateStateList(Left, State1, State2, ABranchLevel, LLen, LOffset,
              AGroupIndex, AState);
            PopState;

            AddTransition(nkMatchEnd, State2, AWayout, nil, AGroupIndex,
              ABranchLevel)
          end;
        opAheadNoMatch:
          begin
            State1 := GetNumber;
            State2 := GetNumber;

            NFACode := AddTransition(nkAheadNoMatch, AEntry, State1, nil, AGroupIndex,
              ABranchLevel);
            NFACode.ExtendTo := State2;

            LLen.Min := 0;
            LLen.Max := 0;
            LOffset.Min := 0;
            LOffset.Max := 0;
            AState.IsNullMatch := True;
            AState.IsJoinMatch := False;

            PushState(AEntry, AWayout, State1, State2);
            GenerateStateList(Left, State1, State2, ABranchLevel, LLen, LOffset,
              AGroupIndex, AState);
            PopState;

            AddTransition(nkMatchEnd, State2, AWayout, nil, AGroupIndex,
              ABranchLevel);
          end;
        opBehindMatch:
          begin
            State1 := GetNumber;
            State2 := GetNumber;

            NFACode := AddTransition(nkBehindMatch, AEntry, State1, nil, AGroupIndex,
              ABranchLevel);
            NFACode.Max := FMax;
            NFACode.Min := FMin;
            NFACode.ExtendTo := State2;

            LLen.Min := 0;
            LLen.Max := 0;
            LOffset.Min := -1;
            LOffset.Max := -1;
            AState.IsJoinMatch := False;
            AState.IsNullMatch := True;

            PushState(AEntry, AWayout, State1, State2);
            GenerateStateList(Left, State1, State2, ABranchLevel, LLen, LOffset,
              AGroupIndex, AState);
            PopState;

            NFACode.Min := LLen.Min;
            NFACode.Max := LLen.Max;

            AddTransition(nkMatchEnd, State2, AWayout, nil, AGroupIndex,
              ABranchLevel)
          end;
        opBehindNoMatch:
          begin
            State1 := GetNumber;
            State2 := GetNumber;

            NFACode := AddTransition(nkBehindNoMatch, AEntry, State1, nil, AGroupIndex,
              ABranchLevel);
            NFACode.ExtendTo := State2;
            NFACode.Max := FMax;
            NFACode.Min := FMin;

            LLen.Min := 0;
            LLen.Max := 0;
            LOffset.Min := 0;
            LOffset.Max := 0;
            AState.IsNullMatch := True;
            AState.IsJoinMatch := False;

            PushState(AEntry, AWayout, State1, State2);
            GenerateStateList(Left, State1, State2, ABranchLevel, LLen, LOffset,
              AGroupIndex, AState);
            PopState;

            NFACode.Min := LLen.Min;
            NFACode.Max := LLen.Max;

            AddTransition(nkMatchEnd, State2, AWayout, nil, AGroupIndex,
              ABranchLevel);
          end;
        opIfMatch:
          begin
            State1 := GetNumber;
            State2 := GetNumber;

            NFACode := AddTransition(nkIfMatch, AEntry, State1, nil, AGroupIndex,
              ABranchLevel);

            AState.IsNullMatch := True;

            GenerateStateList(Left, State1, State2, ABranchLevel, AMatchLen,
              AOffset, AGroupIndex, AState);
            NFACode.ExtendTo := State2;

            LLen.Min := 0;
            LLen.Max := 0;
            AState.IsNullMatch := True;

            PushState(AEntry, AWayout, State1, State2);
            GenerateStateList(Right, State2, AWayout, ABranchLevel, LLen,
              AOffset, AGroupIndex, AState);
            PopState;

            AddTransition(nkMatchEnd, State2, AWayout, nil, AGroupIndex,
              ABranchLevel);
          end;
        opIfThen:
          begin
            if Right <> nil then
            begin
              State1 := GetNumber;

              AddTransition(nkIfThen, AEntry, State1, nil, AGroupIndex,
                ABranchLevel);
              RLen.Min := AMatchLen.Min;
              RLen.Max := AMatchLen.Max;
              ROffset := AOffset;
              GenerateStateList(Right, State1, AWayout, ABranchLevel, RLen,
                ROffset, AGroupIndex, AState);
            end
            else
            begin
              AddTransition(nkEmpty, AEntry, AWayout, nil, AGroupIndex,
                ABranchLevel);
            end;

            State2 := GetNumber;
            AddTransition(nkIfThen, AEntry, State2, nil, AGroupIndex,
              ABranchLevel);

            LLen.Min := 0;
            LLen.Max := 0;
            LOffset.Min := 0;
            LOffset.Max := 0;

            GenerateStateList(Left, State2, AWayout, ABranchLevel, LLen,
              LOffset, AGroupIndex, AState);

            AOffset.Min := -1;
            AOffset.Max := -1;

            if Right <> nil then
            begin
              if (AMatchLen.Min > -1) and (LLen.Min > -1) and (RLen.Min > -1) then
                Inc(AMatchLen.Min, SkRegExpW.Min(LLen.Min, RLen.Min))
              else
                AMatchLen.Min := -1;

              if (AMatchLen.Max > -1) and (LLen.Max > -1) and (RLen.Max > -1) then
                Inc(AMatchLen.Max, SkRegExpW.Max(LLen.Max, RLen.Max))
              else
                AMatchLen.Max := -1;
            end
            else
            begin
              if (AMatchLen.Min > -1) and (LLen.Min > -1) then
                Inc(AMatchLen.Min, LLen.Min)
              else
                AMatchLen.Min := -1;

              if (AMatchLen.Max > -1) and (LLen.Max > -1) then
                Inc(AMatchLen.Max, LLen.Max)
              else
                AMatchLen.Max := -1;
            end;
          end;
        opDefine:
          begin
            State1 := GetNumber;
            State2 := GetNumber;

            NFACode := AddTransition(nkDefine, AEntry, State1, nil, AGroupIndex,
              ABranchLevel);
            NFACode.ExtendTo := AWayout;

            LLen.Min := 0;
            LLen.Max := 0;

            GenerateStateList(Left, State1, State2, ABranchLevel, LLen,
              AOffset, AGroupIndex, AState);
            AddTransition(nkMatchEnd, State2, AWayout, nil, AGroupIndex,
              ABranchLevel);
          end;
        opFail:
          begin
            AddTransition(nkFail, AEntry, AWayout, nil, AGroupIndex,
              ABranchLevel);
          end;
        opPrune:
          begin
            NFACode := AddTransition(nkPrune, AEntry, AWayout, nil, AGroupIndex,
              ABranchLevel);
            NFACode.GroupIndex := GroupIndex;
          end;
        opSkip:
          begin
            NFACode := AddTransition(nkSkip, AEntry, AWayout, nil, AGroupIndex,
              ABranchLevel);
            NFACode.GroupIndex := GroupIndex;
          end;
        opMark:
          begin
            NFACode := AddTransition(nkMark, AEntry, AWayout, nil, AGroupIndex,
              ABranchLevel);
            NFACode.GroupIndex := GroupIndex;
          end;
        opThen:
          begin
            NFACode := AddTransition(nkThen, AEntry, AWayout, nil, AGroupIndex,
              ABranchLevel);
            NFACode.GroupIndex := GroupIndex;

            if FBranchStack.Count > 0 then
            begin
              NFACode.ExtendTo :=
                PREBranchStateRec(FBranchStack[FBranchStack.Count - 1]).State;
              NFACode.Min := PREBranchStateRec
                (FBranchStack[FBranchStack.Count - 1]).Count;
              PREBranchStateRec(FBranchStack[FBranchStack.Count - 1]).Count :=
                PREBranchStateRec(FBranchStack[FBranchStack.Count - 1]).Count + 1;
            end
            else
              NFACode.ExtendTo := -1;

          end;
        opCommint:
          begin
            NFACode := AddTransition(nkCommit, AEntry, AWayout, nil, AGroupIndex,
              ABranchLevel);
            NFACode.GroupIndex := GroupIndex;
          end;
        opAccept:
          begin
            AddTransition(nkAccept, AEntry, FRegExp.FExitState, nil,
              AGroupIndex, ABranchLevel);
            FHasAccept := True;
          end;
      end;
    end;
  end
  else
  begin
    if (FBEntryState = AEntry) and (ACode is TRELineHeadCode) then
    begin
      AddTransition(nkNormal, AEntry, AWayout, ACode, AGroupIndex,
        ABranchLevel);

      if not FHasAccept and AState.IsJoinMatch and not FInBranch then
        FOptimizeData.Add(ACode, odkLead, ABranchLevel, AOffset);
    end
    else if (AWayout = FBExitState) and (ACode is TRELineTailCode) then
    begin
      AddTransition(nkNormal, AEntry, AWayout, ACode, AGroupIndex,
        ABranchLevel);

      if not FHasAccept and AState.IsJoinMatch and FInBranch then
        FOptimizeData.Add(ACode, odkLineTail, ABranchLevel, AOffset);
    end
    else
    begin
      if (ACode is TRELiteralCode) then
      begin
        AddTransition(nkChar, AEntry, AWayout, ACode, AGroupIndex,
          ABranchLevel);

        if not FHasAccept then
        begin
          if not AState.IsNullMatch and not FInBranch then
            FOptimizeData.Add(ACode, GetAnchorKind(AState), ABranchLevel, AOffset);

          if AOffset.Min > -1 then
            Inc(AOffset.Min, ACode.CharLength.Min);

          if AOffset.Max > -1 then
            Inc(AOffset.Max, ACode.CharLength.Max);

          if AMatchLen.Min > -1 then
            Inc(AMatchLen.Min, ACode.CharLength.Min);

          if AMatchLen.Max > -1 then
            Inc(AMatchLen.Max, ACode.CharLength.Max);
        end;
      end
      else if (ACode is TRETextHeadCode) then
      begin
        AddTransition(nkNormal, AEntry, AWayout, ACode, AGroupIndex,
          ABranchLevel);

        if not FHasAccept then
          if AState.IsJoinMatch and (FBEntryState = AEntry) then
            if not AState.IsNullMatch and not FInBranch then
              FOptimizeData.Add(ACode, odkLead, ABranchLevel, AOffset);
      end
      else if (ACode is TRETextTailCode) or (ACode is TRETextEndCode) then
      begin
        AddTransition(nkNormal, AEntry, AWayout, ACode, AGroupIndex,
          ABranchLevel);

        if not FHasAccept then
          if AState.IsJoinMatch and (AEntry = FBEntryState) and not FInBranch then
            FOptimizeData.Add(ACode, odkTextTail, ABranchLevel, AOffset)
      end
      else if (ACode is TRECalloutCode) then
      begin
        AddTransition(nkCallout, AEntry, AWayout, ACode, AGroupIndex,
          ABranchLevel);
        AMatchLen.Min := -1;
        AMatchLen.Max := -1;
        AOffset.Min := -1;
        AOffset.Max := -1;
      end
      else
      begin
        AddTransition(nkNormal, AEntry, AWayout, ACode, AGroupIndex,
          ABranchLevel);

        if not FHasAccept then
        begin
          if (FBEntryState = AEntry) and AState.IsJoinMatch and
              not ACode.IsVariable and not ACode.IsAny then
          begin
            if not AState.IsNullMatch and not FInBranch then
              FOptimizeData.Add(ACode, odkLead, ABranchLevel, AOffset);
          end;

          if (AMatchLen.Min > -1) and (ACode.CharLength.Min > -1) then
            Inc(AMatchLen.Min,  ACode.CharLength.Min)
          else
            AMatchLen.Min := -1;

          if (AMatchLen.Max > -1) and (ACode.CharLength.Max > -1) then
            Inc(AMatchLen.Max, ACode.CharLength.Max)
          else
            AMatchLen.Max := -1;

          if (AOffset.Min > -1) and (ACode.CharLength.Min > -1) then
            Inc(AOffset.Min, ACode.CharLength.Min)
          else
            AOffset.Min := -1;

          if (AOffset.Max > -1) and (ACode.CharLength.Max > -1) then
            Inc(AOffset.Max, ACode.CharLength.Max)
          else
            AOffset.Max := -1;
        end;
      end;
    end;
  end;
end;

function TRENFA.GetNumber: Integer;
begin
  Result := FStateList.Add(nil);
end;

procedure TRENFA.PopState;
begin
  if FEntryStackIndex > 0 then
  begin
    Dec(FEntryStackIndex);
    FBEntryState := (Integer(FEntryStack[FEntryStackIndex]));
    FEntryStack.Delete(FEntryStackIndex);
  end;
  if FExitStateIndex > 0 then
  begin
    Dec(FExitStateIndex);
    FBExitState := Integer(FExitStack[FExitStateIndex]);
    FExitStack.Delete(FExitStateIndex);
  end;
end;

procedure TRENFA.CalculateGroupLength(ACode: TRECode;
  var AMatchLen: TRETextPosRec;
  IsNullMatch, IsJoinMatch: Boolean);
var
  LLen, RLen: TRETextPosRec;
begin
  if ACode is TREBinCode then
  begin
    with ACode as TREBinCode do
    begin
      case Op of
        opUnion:
          begin
            RLen.Min := 0;
            RLen.Max := 0;
            LLen.Min := 0;
            LLen.Max := 0;

            CalculateGroupLength(Right, RLen, IsNullMatch, IsJoinMatch);
            CalculateGroupLength(Left, LLen, IsNullMatch, IsJoinMatch);

            if (AMatchLen.Min > -1) and (RLen.Min > -1) and (LLen.Min > -1) then
              Inc(AMatchLen.Min, SkRegExpW.Min(RLen.Min, LLen.Min))
            else
              AMatchLen.Min := -1;

            if (AMatchLen.Max > -1) and (RLen.Max > -1) and (LLen.Max > -1) then
              Inc(AMatchLen.Max, SkRegExpW.Max(RLen.Max, LLen.Max))
            else
              AMatchLen.Max := -1;
          end;
        opConcat:
          begin
            CalculateGroupLength(Left, AMatchLen, IsNullMatch, IsJoinMatch);
            CalculateGroupLength(Right, AMatchLen, IsNullMatch, IsJoinMatch);
          end;
        opStar:
          begin
            AMatchLen.Max := -1;
          end;
        opPlus:
          begin
            if (AMatchLen.Min > -1) and (Left.CharLength.Min > -1) then
              Inc(AMatchLen.Min, Left.CharLength.Min)
            else
              AMatchLen.Min := -1;

            AMatchLen.Max := -1;
          end;
        opBound:
          begin
            if (AMatchLen.Min > -1) and (Left.CharLength.Min > -1) then
              Inc(AMatchLen.Min, FMin * Left.CharLength.Min)
            else
              AMatchLen.Min := -1;

            if FMax = CONST_LoopMax then
              AMatchLen.Max := -1
            else
            begin
              if (AMatchLen.Max > -1) and (Left.CharLength.Max > -1) then
                Inc(AMatchLen.Max, FMax * Left.CharLength.Max)
              else
                AMatchLen.Max := -1;
            end;
          end;
        opLoop:
          begin
            LLen.Min := 0;
            LLen.Max := 0;

            CalculateGroupLength(Left, LLen, FMin = 0, IsJoinMatch);

            if (AMatchLen.Min > -1) and (LLen.Min > -1) then
              Inc(AMatchLen.Min, LLen.Min * FMin)
            else
              AMatchLen.Min := -1;

            if FMax = CONST_LoopMax then
              AMatchLen.Max := -1
            else
              if (AMatchLen.Max > -1) and (LLen.Max > -1) then
                Inc(AMatchLen.Max, LLen.Max * FMax)
              else
                AMatchLen.Max := -1;

          end;
        opGroup:
          begin
            LLen.Min := 0;
            LLen.Max := 0;

            CalculateGroupLength(Left, LLen, IsNullMatch, IsJoinMatch);

            FRegExp.FGroups[GroupIndex].CharLength := LLen;

            if (AMatchLen.Min > -1) and (LLen.Min > -1) then
              Inc(AMatchLen.Min, LLen.Min)
            else
              AMatchLen.Min := -1;

            if (AMatchLen.Max > -1) and (LLen.Max > -1) then
              Inc(AMatchLen.Max, LLen.Max)
            else
              AMatchLen.Max := -1;
          end;
        opNoBackTrack:
          begin
            CalculateGroupLength(Left, AMatchLen, IsNullMatch, IsJoinMatch);
          end;
        opKeepPattern:
          begin
            CalculateGroupLength(Left, AMatchLen, IsNullMatch, IsJoinMatch);
          end;
        opAheadMatch:
          begin
            LLen.Min := 0;
            LLen.Max := 0;

            CalculateGroupLength(Left, LLen, IsNullMatch, False);
          end;
        opAheadNoMatch:
          begin
            LLen.Min := 0;
            LLen.Max := 0;

            CalculateGroupLength(Left, LLen, True, False);
          end;
        opBehindMatch:
          begin
            LLen.Min := 0;
            LLen.Max := 0;

            CalculateGroupLength(Left, LLen, True, False);
          end;
        opBehindNoMatch:
          begin
            LLen.Min := 0;
            LLen.Max := 0;

            CalculateGroupLength(Left, LLen, True, False);
          end;
        opIfMatch:
          begin
            CalculateGroupLength(Left, AMatchLen, IsNullMatch, IsJoinMatch);

            LLen.Min := 0;
            LLen.Max := 0;

            CalculateGroupLength(Right, LLen, True, IsJoinMatch);
          end;
        opIfThen:
          begin
            if Right <> nil then
            begin
              RLen.Min := AMatchLen.Min;
              RLen.Max := AMatchLen.Max;

              CalculateGroupLength(Right, RLen, IsNullMatch, IsJoinMatch);
            end;

            LLen := AMatchLen;

            CalculateGroupLength(Left, LLen, IsNullMatch, IsJoinMatch);

            if Right <> nil then
            begin
              if (AMatchLen.Min > -1) and (LLen.Min > -1) and (RLen.Min > -1) then
                Inc(AMatchLen.Min, SkRegExpW.Min(LLen.Min, RLen.Min))
              else
                AMatchLen.Min := -1;

              if (AMatchLen.Max > -1) and (LLen.Max > -1) and (RLen.Max > -1) then
                Inc(AMatchLen.Max, SkRegExpW.Max(LLen.Max, RLen.Max))
              else
                AMatchLen.Max := -1;
            end
            else
            begin
              if (AMatchLen.Min > -1) and (LLen.Min > -1) then
                Inc(AMatchLen.Min, LLen.Min)
              else
                AMatchLen.Min := -1;

              if (AMatchLen.Max > -1) and (LLen.Max > -1) then
                Inc(AMatchLen.Max, LLen.Max)
              else
                AMatchLen.Max := -1;
            end;
          end;
        opDefine:
          begin
            LLen.Min := 0;
            LLen.Max := 0;

            CalculateGroupLength(Left, LLen, IsNullMatch, IsJoinMatch);
          end;
      end;
    end;
  end
  else
  begin
    if (ACode is TRELiteralCode) then
    begin
      if not FHasAccept then
      begin
        if AMatchLen.Min > -1 then
          Inc(AMatchLen.Min, ACode.CharLength.Min);

        if AMatchLen.Max > -1 then
          Inc(AMatchLen.Max, ACode.CharLength.Max);
      end;
    end
    else
    begin
      if not FHasAccept then
      begin
        if (AMatchLen.Min > -1) and (ACode.CharLength.Min > -1) then
          Inc(AMatchLen.Min, ACode.CharLength.Min)
        else
          AMatchLen.Min := -1;

        if (AMatchLen.Max > -1) and (ACode.CharLength.Max > -1) then
          Inc(AMatchLen.Max, ACode.CharLength.Max)
        else
          AMatchLen.Max := -1;
      end;
    end;
  end;
end;

procedure TRENFA.PushState(AEntry, AWayout, ANewEntry, ANewWayout: Integer);
begin
  if FBEntryState = AEntry then
  begin
    FEntryStack.Add(Pointer(FBEntryState));
    FBEntryState := ANewEntry;
    Inc(FEntryStackIndex);
  end;
  if AWayout = FBExitState then
  begin
    FExitStack.Add(Pointer(FBExitState));
    FBExitState := ANewWayout;
    Inc(FExitStateIndex);
  end;
end;

{ TRELoopState }

procedure TRELoopStateItem.Clear;
var
  P: PRELoopStateRec;
begin
  InternalClear;

  New(P);
  P.Step := 0;
  FState.Add(P);
  FPrevP := nil;
  FMatchP := nil;
end;

constructor TRELoopStateItem.Create;
var
  P: PRELoopStateRec;
begin
  inherited;
  FState := TList.Create;

  New(P);
  P.Step := 0;
  FState.Add(P);
end;

destructor TRELoopStateItem.Destroy;
begin
  InternalClear;
  FState.Free;
  inherited;
end;

function TRELoopStateItem.GetNestLevel: Integer;
begin
  Result := FCurIndex;
end;

function TRELoopStateItem.GetState(Index: Integer): PRELoopStateRec;
begin
  Result := FState[Index];
end;

function TRELoopStateItem.GetStep: Integer;
begin
  Result := GetState(FCurIndex).Step;
end;

procedure TRELoopStateItem.InternalClear;
var
  I: Integer;
  P: PRELoopStateRec;
begin
  for I := FState.Count - 1 downto 0 do
  begin
    P := FState[I];
    if P <> nil then
      Dispose(P);
  end;
  FState.Clear;

  FCurIndex := 0;
end;

procedure TRELoopStateItem.Pop;
begin
  Dec(FCurIndex);
end;

procedure TRELoopStateItem.Push;
var
  Source, Dest: PRELoopStateRec;
begin
  Inc(FCurIndex);

  if FCurIndex = FState.Count then
  begin
    New(Dest);
    Dest.Step := 0;
    FState.Add(Dest);
  end
  else
  begin
    Source := FState[FCurIndex];
    Source.Step := 0;
  end;
end;

procedure TRELoopStateItem.Reset;
begin
  FCurIndex := 0;
  FPrevP := nil;
  GetState(FCurIndex).Step := 0;
  FMatchP := nil;
end;

procedure TRELoopStateItem.SetNestLevel(const Value: Integer);
begin
  FCurIndex := Value;
end;

procedure TRELoopStateItem.SetStep(const Value: Integer);
begin
  GetState(FCurIndex).Step := Value;
end;

procedure TRELoopStateItem.Up;
begin
  Inc(GetState(FCurIndex).Step);
end;

{ TRELoopStateList }

function TRELoopStateList.Add(Value: TRELoopStateItem): Integer;
begin
  Result := inherited Add(Value);
end;

procedure TRELoopStateList.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    GetItem(I).Clear;
  inherited;
end;

function TRELoopStateList.GetItem(Index: Integer): TRELoopStateItem;
begin
  Result := inherited GetItem(Index) as TRELoopStateItem;
end;

procedure TRELoopStateList.Pop;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    GetItem(I).Pop;
end;

procedure TRELoopStateList.Push;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    GetItem(I).Push;
end;

procedure TRELoopStateList.Reset;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    GetItem(I).Reset;
end;

procedure TRELoopStateList.SetItem(Index: Integer; const Value: TRELoopStateItem);
begin
  inherited SetItem(Index, Value);
end;

procedure TRELoopStateList.SetLoopIndex(const Index: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    GetItem(I).NestLevel := Index + 1;
end;

{ TRECapture }

procedure TRECapture.Assign(AOjbect: TObject);
begin
  if AOjbect is TRECapture then
  begin
    FStartP := (AOjbect as TRECapture).FStartP;
    FEndP := (AOjbect as TRECapture).FEndP;
    FMatched := (AOjbect as TRECapture).FMatched;
  end;
end;

procedure TRECapture.Clear;
begin
  FStartP := nil;
  FStartPBuf := nil;
  FEndP := nil;
  FMatched := False;
end;

constructor TRECapture.Create(ARegExp: TSkRegExp);
begin
  inherited Create;
  FRegExp := ARegExp;
end;

function TRECapture.GetIndex: Integer;
begin
  if FMatched then
    Result := FStartP - FRegExp.FTextTopP + 1
  else
    Result := 0;
end;

function TRECapture.GetLength: Integer;
begin
  if FMatched then
    Result := FEndP - FStartP
  else
    Result := 0;
end;

function TRECapture.GetStrings: REString;
begin
  if FMatched then
    SetString(Result, FStartP, FEndP - FStartP)
  else
    Result := '';
end;

function TRECapture.GetSuccess: Boolean;
begin
  Result := FMatched;
end;

procedure TRECapture.SetEndP(const Value: PWideChar);
begin
  if (FStartPBuf <> nil) and (not FMatched) then
    FMatched := True;
  FStartP := FStartPBuf;
  FEndP := Value;
end;

procedure TRECapture.SetStartP(const Value: PWideChar);
begin
  if (FStartP = nil) then
    FStartP := Value;
  FStartPBuf := Value;
end;

{ TRECaptureCollection }

function TRECaptureCollection.Add(ACapture: TRECapture): Integer;
begin
  Result := FItems.Add(ACapture);
end;

procedure TRECaptureCollection.Clear;
begin
  FItems.Clear;
  FCurIndex := 0;
end;

constructor TRECaptureCollection.Create(ARegExp: TSkRegExp);
begin
  inherited Create;
  FRegExp := ARegExp;
  FItems := TObjectList.Create;
  FCurIndex := 0;
end;

destructor TRECaptureCollection.Destroy;
begin
  FItems.Free;
  inherited;
end;

function TRECaptureCollection.GetCount: Integer;
begin
  Result := FCurIndex;
end;

procedure TRECaptureCollection.SetCurIndex(const Value: Integer);
begin
  FCurIndex := Value + 1;
end;

procedure TRECaptureCollection.SetData(ACapture: TRECapture);
var
  LCap: TRECapture;
begin
  LCap := GetItem(FCurIndex - 1);
  LCap.FStartP := ACapture.FStartP;
  LCap.FStartPBuf := ACapture.FStartPBuf;
  LCap.FEndP := ACapture.FEndP;
  LCap.FMatched := ACapture.FMatched;
end;

function TRECaptureCollection.GetEndP: PWideChar;
begin
  Result := GetItem(FCurIndex - 1).EndP;
end;

function TRECaptureCollection.GetIndex: Integer;
begin
  Result := GetItem(FCurIndex - 1).Index;
end;

function TRECaptureCollection.GetItem(Index: Integer): TRECapture;
begin
  Result := TRECapture(FItems[Index]);
end;

function TRECaptureCollection.GetLength: Integer;
begin
  Result := GetItem(FCurIndex - 1).Length;
end;

function TRECaptureCollection.GetMatched: Boolean;
begin
  Result := GetItem(FCurIndex - 1).Matched;
end;

function TRECaptureCollection.GetStartP: PWideChar;
begin
  Result := GetItem(FCurIndex - 1).StartP;
end;

function TRECaptureCollection.GetStartPBuf: PWideChar;
begin
  Result := GetItem(FCurIndex - 1).FStartPBuf;
end;

function TRECaptureCollection.GetStrings: REString;
begin
  Result := GetItem(FCurIndex - 1).Strings;
end;

function TRECaptureCollection.GetSuccess: Boolean;
begin
  Result := GetItem(FCurIndex - 1).Success;
end;

procedure TRECaptureCollection.Pop;
begin
  Dec(FCurIndex);
end;

procedure TRECaptureCollection.Push;
var
  Source, Dest: TRECapture;
begin
  if FCurIndex > FItems.Count - 1 then
  begin
    Source := FItems[FCurIndex - 1] as TRECapture;
    Dest := TRECapture.Create(FRegExp);
    Dest.Assign(Source);
    FItems.Add(Dest);
    Inc(FCurIndex);
  end
  else
  begin
    Source := FItems[FCurIndex - 1] as TRECapture;
    Inc(FCurIndex);
    (FItems[FCurIndex - 1] as TRECapture).Assign(Source);
  end;
end;

procedure TRECaptureCollection.SetEndP(const Value: PWideChar);
begin
  GetItem(FCurIndex - 1).EndP := Value;
end;

procedure TRECaptureCollection.SetMatched(const Value: Boolean);
begin
  GetItem(FCurIndex - 1).Matched := Value;
end;

procedure TRECaptureCollection.SetStartP(const Value: PWideChar);
begin
  GetItem(FCurIndex - 1).StartP := Value;
end;

procedure TRECaptureCollection.SetStartPBuf(const Value: PWideChar);
begin
  GetItem(FCurIndex - 1).FStartPBuf := Value;
end;

{ TGroup }

procedure TGroup.Assign(Source: TGroup);
begin
  FRegExp := Source.FRegExp;
  FGroupName := Source.FGroupName;
  FGroupBegin := Source.FGroupBegin;
  FGroupEnd := Source.FGroupEnd;
  FStartP := Source.FStartP;
  FEndP := Source.FEndP;
  FSuccess := Source.FSuccess;
  FSubExp := Source.FSubExp;
  FSameGroup := Source.FSameGroup;
  FJoinMatch := Source.FJoinMatch;
  FCharLength := Source.FCharLength;
end;

procedure TGroup.Clear;
begin
  FGroupName := '';
  FGroupBegin := nil;
  FGroupEnd := nil;
  FSameGroup := 0;
  FStartP := nil;
  FEndP := nil;
  FStartPBuf := nil;
  FSubExp := '';
  FSameGroup := 0;
  FJoinMatch := False;
  FCharLength.Min := 0;
  FCharLength.Max := 0;
end;

constructor TGroup.Create(ARegExp: TSkRegExp);
begin
  inherited Create;
  FRegExp := ARegExp;
  FStartP := nil;
  FEndP := nil;
  FStartPBuf := nil;
  FSubExp := nil;
  FGroupName := '';
  FGroupBegin := nil;
  FGroupEnd := nil;
  FSameGroup := 0;
  FJoinMatch := False;
  FCharLength.Min := 0;
  FCharLength.Max := 0;
end;

function TGroup.GetIndex: Integer;
begin
  if FSuccess then
    Result := FStartP - FRegExp.FTextTopP + 1
  else
    Result := 0;
end;

function TGroup.GetLength: Integer;
begin
  if FSuccess then
    Result := FEndP - FStartP
  else
    Result := 0;
end;

function TGroup.GetStrings: REString;
begin
  if FSuccess then
    SetString(Result, FStartP, FEndP - FStartP)
  else
    Result := '';
end;

function TGroup.GetSubExpression: REString;
var
  P: PWideChar;
  LCount: Integer;
begin
  if FSubExp = nil then
  begin
    Result := '';
    Exit;
  end;

  LCount := 1;
  P := FSubExp;

  while P^ <> #0 do
  begin
    if P^ = '(' then
    begin
      Inc(LCount);
    end
    else if P^ = ')' then
    begin
      Dec(LCount);
      if LCount = 0 then
      begin
        SetString(Result, FSubExp, P - FSubExp);
        Result := TrimRight(Result);
        Exit;
      end;
    end;
    Inc(P);
  end;
end;

procedure TGroup.Reset;
begin
  FStartP := nil;
  FEndP := nil;
  FStartPBuf := nil;
  FSuccess := False;
end;

procedure TGroup.SetEndP(const Value: PWideChar);
begin
  if (FStartPBuf <> nil) and (not FSuccess) then
    FSuccess := True;
  FStartP := FStartPBuf;
  FEndP := Value;
end;

procedure TGroup.SetStartP(const Value: PWideChar);
begin
  if (FStartP = nil) then
    FStartP := Value;
  FStartPBuf := Value;
end;

{ TGroupCollectionEnumerator }

constructor TGroupCollectionEnumerator.Create(AList: TObjectList);
begin
  inherited Create;
  FList := AList;
  FIndex := -1;
end;

function TGroupCollectionEnumerator.GetCurrent: TGroup;
begin
  Result := FList[FIndex] as TGroup;
end;

function TGroupCollectionEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FList.Count - 1;
  if Result then
    Inc(FIndex);
end;

{ TGroupCollection }

function TGroupCollection.Add(const AGroupName: REString;
  AEntry, AWayout: TRENFAState): Integer;
var
  Item: TGroup;
begin
  Item := TGroup.Create(FRegExp);
  Item.GroupName := AGroupName;
  Item.GroupBegin := AEntry;
  Item.GroupEnd := AWayout;
  Result := FItems.Add(Item);

  if AGroupName <> '' then
    AddGroupName(AGroupName, Result);
end;

procedure TGroupCollection.AddGroupName(const AGroupName: REString;
  Index: Integer);
var
  H: Cardinal;
  S, D: PREHashItem;
begin
  New(D);
  D.Key := AGroupName;
  D.Value := Index;
  D.Next := nil;

  H := HashOf(AGroupName);

  S := FBuckets[H];
  if S <> nil then
  begin
    while S.Next <> nil do
      S := S.Next;

    S.Next := D;
  end
  else
    FBuckets[H] := D;

end;

procedure TGroupCollection.Assign(Source: TGroupCollection);
var
  I: Integer;
  Item: TGroup;
begin
  Clear;

  FRegExp := Source.FRegExp;
  for I := 0 to Source.FItems.Count - 1 do
  begin
    Item := TGroup.Create(FRegExp);
    Item.Assign(Source.FItems[I] as TGroup);
    if I <> 0 then
    begin
      if Item.GroupName <> '' then
        AddGroupName(Item.GroupName, I);
      FItems.Add(Item);
    end
    else
      FItems[0] := Item;
  end;
end;

procedure TGroupCollection.Clear;
begin
  FItems.Clear;
  FItems.Add(TGroup.Create(FRegExp));
end;

constructor TGroupCollection.Create(ARegExp: TSkRegExp);
begin
  inherited Create;
  FRegExp := ARegExp;
  FItems := TObjectList.Create;
  FItems.Add(TGroup.Create(FRegExp));
end;

destructor TGroupCollection.Destroy;
var
  I: Integer;
  S, D: PREHashItem;
begin
  for I := 0 to CONST_GroupNameHashMax - 1 do
  begin
    S := FBuckets[I];
    while S <> nil do
    begin
      D := S.Next;
      Dispose(S);
      S := D;
    end;
  end;
  FItems.Free;
  inherited;
end;

function TGroupCollection.EnumIndexOfName(const AGroupName: REString): TIntDynArray;
var
  H: Cardinal;
  S: PREHashItem;
  LCount: Integer;
begin
  LCount := 0;
  SetLength(Result, Count);

  H := HashOf(AGroupName);
  S := FBuckets[H];

  while S <> nil do
  begin
    if S.Key = AGroupName then
    begin
      Result[LCount] := S.Value;
      Inc(LCount);
    end;
    S := S.Next;
  end;
  SetLength(Result, LCount);
end;

function TGroupCollection.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TGroupCollection.GetEnumerator: TGroupCollectionEnumerator;
begin
  Result := TGroupCollectionEnumerator.Create(FItems);
end;

function TGroupCollection.GetItems(Index: Integer): TGroup;
begin
  if (Index < 0) or (Index > FItems.Count - 1) then
    raise ESkRegExpRuntime.CreateFmt(sRangeOverGroupNumber, [Index]);

  Result := TGroup(FItems[Index]);
end;

function TGroupCollection.GetNames(AName: REString): TGroup;
var
  Index: Integer;
begin
  Index := IndexOfName(AName);
  if Index = -1 then
    raise ESkRegExpRuntime.CreateFmt(sMissingGroupName, [AName]);

  if GetItems(Index).SameGroup = 0 then
    Result := GetItems(Index)
  else
  begin
    while GetItems(Index).SameGroup <> 0 do
    begin
      if GetItems(Index).Success then
      begin
        Result := GetItems(Index);
        Exit;
      end;
      Index := GetItems(Index).SameGroup;
    end;

    Result := GetItems(Index);
  end;
end;

function TGroupCollection.HashOf(const Key: REString): Cardinal;
var
  I: Integer;
begin
  Result := 0;
  for I := 1 to System.Length(Key) do
    Result := ((Result shl 2) or (Result shr (Sizeof(Result) * 8 - 2)))
      xor Ord(Key[I]);
  Result := Result mod CONST_GroupNameHashMax;
end;

function TGroupCollection.IndexOfName(const AGroupName: REString): Integer;
var
  H: Cardinal;
  S: PREHashItem;
begin
  Result := -1;
  H := HashOf(AGroupName);

  S := FBuckets[H];

  while S <> nil do
  begin
    if S.Key = AGroupName then
    begin
      if (Result = -1) or (Result > S.Value) then
      begin
        Result := S.Value;
        Exit;
      end;
    end;
    S := S.Next;
  end;
end;

function TGroupCollection.IndexOfMatchedName(const AGroupName
  : REString): Integer;
var
  H: Cardinal;
  S: PREHashItem;
begin
  Result := -1;
  H := HashOf(AGroupName);

  S := FBuckets[H];

  while S <> nil do
  begin
    if (S.Key = AGroupName) and GetItems(S.Value).Success then
    begin
      Result := S.Value;
      Exit;
    end;
    S := S.Next;
  end;
end;

function TGroupCollection.IsDuplicateGroupName(const AGroupName
  : REString): Boolean;
var
  H: Cardinal;
  S: PREHashItem;
  M: Integer;
begin
  Result := False;
  M := 0;
  H := HashOf(AGroupName);

  S := FBuckets[H];

  while S <> nil do
  begin
    if S.Key = AGroupName then
    begin
      Inc(M);
      if M > 1 then
      begin
        Result := True;
        Exit;
      end;
    end;
    S := S.Next;
  end;
end;

function TGroupCollection.NameExists(const AGroupName: REString): Boolean;
begin
  Result := IndexOfName(AGroupName) <> -1;
end;

procedure TGroupCollection.Reset;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    TGroup(FItems[I]).Reset;
end;

procedure TGroupCollection.CheckSameGroupName;
var
  I: Integer;
  NFACode: TRENFAState;
  S1, S2: REString;
begin
  for I := 1 to FItems.Count - 1 do
  begin
    NFACode := FRegExp.FGroups[I].GroupBegin;

    if (NFACode.Next <> nil) and (NFACode.Next.Kind = nkGroupBegin) then
    begin
      S1 := GetItems(NFACode.GroupIndex).GroupName;
      S2 := GetItems(NFACode.Next.GroupIndex).GroupName;
      if (S1 <> '') and (S1 = S2) then
        GetItems(I).SameGroup := NFACode.Next.GroupIndex
      else
        GetItems(I).SameGroup := 0;
    end;
  end;
end;

{ TREGroupStack }

procedure TREGroupStack.Clear;
begin
  FItems.Clear;
  FCurIndex := 0;
end;

constructor TREGroupStack.Create(ARegExp: TSkRegExp);
begin
  inherited Create;
  FRegExp := ARegExp;
  FItems := TObjectList.Create;
  FCurIndex := 0;
end;

destructor TREGroupStack.Destroy;
begin
  FItems.Free;
  inherited;
end;

{$IFDEF SKREGEXP_DEBUG}
function TREGroupStack.GetDebugStr: REString;
var
  I, J, Index, Len: Integer;
  LCapture: TRECapture;
  LCollection: TObjectList;
  S: REString;
begin
  for I := 0 to FItems.Count - 1 do
  begin
    LCollection := TObjectList(FItems[I]);

    for J := 0 to LCollection.Count - 1 do
    begin
      LCapture := TRECapture(LCollection[J]);
      if LCapture.Matched then
      begin
        S := LCapture.Strings;
        S := FRegExp.EncodeEscape(S);

        Index := LCapture.Index;
        Len := LCapture.Length;
        Result := Result +
          Format('[Cap:%d-%d] %s (Index:%d, Length:%d)'#0013#0010,
          [I, J, S, Index, Len]);
      end
      else if LCapture.StartP <> nil then
      begin
        SetString(S, LCapture.StartP, 1);
        S := FRegExp.EncodeEscape(S);

        Index := LCapture.StartP - FRegExp.FTextTopP + 1;
        Len := 1;
        Result := Result +
          Format('[Cap:%d-%d] %s (Index:%d, Length:%d)'#0013#0010,
          [I, J, S, Index, Len]);
      end
      else
      begin
        Result := Result + Format('[Cap:%d-%d] NoMatch'#0013#0010, [I, J]);
      end;
    end;
  end;
end;
{$ENDIF SKREGEXP_DEBUG}

procedure TREGroupStack.Pop(var AGroups: TGroupCollection;
  const Index: Integer);
var
  I: Integer;
  LCapture: TRECapture;
  LCollection: TObjectList;
begin
  if FItems.Count = 0 then
    Exit;

  if Index = -1 then
    FCurIndex := 1
  else
    FCurIndex := Index + 1;

  LCollection := TObjectList(FItems[FCurIndex - 1]);

  for I := 0 to LCollection.Count - 1 do
  begin
    LCapture := TRECapture(LCollection[I]);
    AGroups[I].FStartP := LCapture.FStartP;
    AGroups[I].FEndP := LCapture.FEndP;
    AGroups[I].FStartPBuf := LCapture.FStartPBuf;
    AGroups[I].FSuccess := LCapture.FMatched;
  end;
end;

procedure TREGroupStack.Pop(var AGroups: TGroupCollection);
var
  I: Integer;
  LCapture: TRECapture;
  LCollection: TObjectList;
begin
  if FItems.Count = 0 then
    Exit;

  LCollection := TObjectList(FItems[FCurIndex - 1]);

  for I := 0 to LCollection.Count - 1 do
  begin
    LCapture := TRECapture(LCollection[I]);
    AGroups[I].FStartP := LCapture.FStartP;
    AGroups[I].FEndP := LCapture.FEndP;
    AGroups[I].FStartPBuf := LCapture.FStartPBuf;
    AGroups[I].FSuccess := LCapture.FMatched;
  end;
  Dec(FCurIndex);
end;

procedure TREGroupStack.Push(AGroups: TGroupCollection);
var
  I: Integer;
  LCapture: TRECapture;
  LCollection: TObjectList;
begin
  if FCurIndex > FItems.Count - 1 then
  begin
    LCollection := TObjectList.Create;

    for I := 0 to AGroups.Count - 1 do
    begin
      LCapture := TRECapture.Create(FRegExp);

      LCapture.FStartP := FRegExp.FGroups[I].StartP;
      LCapture.FStartPBuf := FRegExp.FGroups[I].StartPBuf;
      LCapture.FEndP := FRegExp.FGroups[I].EndP;
      LCapture.FMatched := FRegExp.FGroups[I].Success;

      LCollection.Add(LCapture);
    end;

    FItems.Add(LCollection);
  end
  else
  begin
    LCollection := TObjectList(FItems[FCurIndex]);

    for I := 0 to AGroups.Count - 1 do
    begin
      LCapture := TRECapture(LCollection[I]);

      LCapture.FStartP := FRegExp.FGroups[I].StartP;
      LCapture.FStartPBuf := FRegExp.FGroups[I].StartPBuf;
      LCapture.FEndP := FRegExp.FGroups[I].EndP;
      LCapture.FMatched := FRegExp.FGroups[I].Success;
    end;
  end;
  Inc(FCurIndex);
end;

{ TREBackTrackStack }

procedure TREBackTrackStack.Clear;
var
  I: Integer;
  MatchRecList: TList;
  LStat: PREBackTrackStateRec;
  P: TRECapture;
  J: Integer;
begin
  for I := FCount downto 0 do
  begin
    LStat := FStat^[I];
    if LStat <> nil then
    begin
      Dispose(LStat);
      FStat^[I] := nil;
    end;

    if FGroup^[I] <> nil then
    begin
      MatchRecList := FGroup^[I];
      for J := MatchRecList.Count - 1 downto 0 do
      begin
        P := MatchRecList[J];
        if Assigned(P) then
          P.Free;
      end;
      MatchRecList.Free;
      FGroup^[I] := nil;
    end;
  end;

  FCount := -1;
end;

function TREBackTrackStack.Count: Integer;
begin
  Result := FCount + 1;
end;

constructor TREBackTrackStack.Create(ARegExp: TSkRegExp;
  ACheckMatchExplosion: Boolean);
begin
  inherited Create;
  FRegExp := ARegExp;
  FSize := CONST_BackTrack_Stack_Default_Size;
  GetMem(FStat, FSize * Sizeof(Pointer));
  GetMem(FGroup, FSize * Sizeof(Pointer));
  FCount := -1;
  FCheckMatchExplosion := ACheckMatchExplosion;
end;

destructor TREBackTrackStack.Destroy;
begin
  Clear;

  FreeMem(FStat);
  FreeMem(FGroup);
  inherited;
end;

function TREBackTrackStack.Peek: TRENFAState;
begin
  if FCount = -1 then
  begin
    Result := nil;
    Exit;
  end;

  Result := PREBackTrackStateRec(FStat^[FCount]).NFACode;
end;

procedure TREBackTrackStack.Pop;
var
  I: Integer;
  MatchRecList: TList;
  P: TRECapture;
  LStat: PREBackTrackStateRec;
{$IFDEF SKREGEXP_DEBUG}
  AStr: PWideChar;
  NFACode: TRENFAState;
  T: REString;
{$ENDIF SKREGEXP_DEBUG}
begin
  if FCount = -1 then
    Exit;

  LStat := FStat^[FCount];

{$IFDEF SKREGEXP_DEBUG}
  if FRegExp.FShowMatchProcess and FRegExp.ShowBackTrackStack then
  begin
    AStr := LStat.Str;
    NFACode := LStat.NFACode;
    T := Copy(AStr, 1, 20);
    if NFACode <> nil then
      T := Format(#0009'[pop:%d][NestLevel:%d] %s "%s"',
        [FCount, LStat.NestLevel + 1, NFACode.GetString, T])
    else
      if LStat.Str <> nil then
        T := Format(#0009'[pop:%d][NestLevel:%d] Group pop"%s"',
          [FCount, LStat.NestLevel + 1, T])
      else
        T := Format(#0009'[pop:%d][NestLevel:%d] Gosub break"%s"',
          [FCount, LStat.NestLevel + 1, T]);
    FRegExp.FMatchProcess.Add(T);
  end;
{$ENDIF SKREGEXP_DEBUG}

  Dispose(LStat);
  FStat^[FCount] := nil;

  if FGroup^[FCount] <> nil then
  begin
    MatchRecList := FGroup^[FCount];
    for I := 0 to MatchRecList.Count - 1 do
    begin
      P := MatchRecList[I];
      FRegExp.FGroups[I + 1].FStartP := P.StartP;
      FRegExp.FGroups[I + 1].FStartPBuf := P.FStartPBuf;
      FRegExp.FGroups[I + 1].FEndP := P.FEndP;
      FRegExp.FGroups[I + 1].FSuccess := P.FMatched;
      P.Free;
    end;
    MatchRecList.Free;
  end;
  Dec(FCount);
end;

procedure TREBackTrackStack.Pop(var NFACode: TRENFAState; var AStr: PWideChar);
label
  ReStart;
var
  I: Integer;
  MatchRecList: TList;
  P: TRECapture;
  LStat: PREBackTrackStateRec;
  IsNilStr: Boolean;
{$IFDEF SKREGEXP_DEBUG}
  T: REString;
{$ENDIF SKREGEXP_DEBUG}
begin
ReStart:
  if FCount = -1 then
  begin
    NFACode := nil;
    Exit;
  end;

  LStat := FStat[FCount];

  NFACode := LStat.NFACode;
  IsNilStr := (NFACode = nil) and (LStat.Str = nil);
  if not IsNilStr then
    AStr := LStat.Str;

  if FRegExp.FSubStack.Index <> LStat.NestLevel then
  begin
    FRegExp.FSubStack.Index := LStat.NestLevel;
//@    FRegExp.FGroups.SetCaptureIndex(LStat.NestLevel);
    FRegExp.FGroupStack.Pop(FRegExp.FGroups, LStat.NestLevel);
    FRegExp.FLoopState.SetLoopIndex(LStat.NestLevel);
  end;

{$IFDEF SKREGEXP_DEBUG}
  if FRegExp.FShowMatchProcess and FRegExp.ShowBackTrackStack then
  begin
    T := Copy(AStr, 1, 20);
    if NFACode <> nil then
      T := Format(#0009'[pop:%d][NestLevel:%d] %s "%s"',
        [FCount, LStat.NestLevel + 1, NFACode.GetString, T])
    else
      if not IsNilStr then
        T := Format(#0009'[pop:%d][NestLevel:%d] Group pop"%s"',
          [FCount, LStat.NestLevel + 1, T])
      else
        T := Format(#0009'[pop:%d][NestLevel:%d] Gosub break"%s"',
          [FCount, LStat.NestLevel + 1, T]);
    FRegExp.FMatchProcess.Add(T);
  end;
{$ENDIF SKREGEXP_DEBUG}

  Dispose(LStat);
  FStat^[FCount] := nil;

  if FGroup^[FCount] <> nil then
  begin
    MatchRecList := FGroup^[FCount];
    for I := 0 to MatchRecList.Count - 1 do
    begin
      P := MatchRecList[I];
      FRegExp.FGroups[I + 1].FStartP := P.FStartP;
      FRegExp.FGroups[I + 1].FStartPBuf := P.FStartPBuf;
      FRegExp.FGroups[I + 1].FEndP := P.EndP;
      FRegExp.FGroups[I + 1].FSuccess := P.FMatched;
      P.Free;
    end;
    MatchRecList.Free;
  end;

  Dec(FCount);

  if (NFACode = nil) then
  begin
    if IsNilStr then
    begin
      if FCount = -1 then
        Exit;
      LStat := FStat[FCount];
      //GoSub 用のスタックが２回続いたら抜ける
      if (LStat.NFACode = nil) and (LStat.Str = nil) then
        Exit;
      goto ReStart;
    end
    else
      Exit;
  end;

{$IFDEF CHECK_MATCH_EXPLOSION}
  if FCheckMatchExplosion and (NFACode.Kind in [nkEmpty, nkLoop, nkStar, nkPlus,
    nkBound]) then
  begin
    if FRegExp.IsAlreadyTried(NFACode, AStr) then
      goto ReStart;
  end;
{$ENDIF}
end;

procedure TREBackTrackStack.Push(NFACode: TRENFAState; AStr: PWideChar;
  IsPushGroup: Boolean);
var
  I: Integer;
  MatchRecList: TList;
  LStat: PREBackTrackStateRec;
  P: TRECapture;
{$IFDEF SKREGEXP_DEBUG}
  S: REString;
{$ENDIF SKREGEXP_DEBUG}
begin
  Inc(FCount);

  if FCount >= FSize then
    Extend(FSize div 4);

  New(LStat);
  LStat.NFACode := NFACode;
  LStat.Str := AStr;
  LStat.NestLevel := FRegExp.FSubStack.Index;

  FStat^[FCount] := LStat;

  if IsPushGroup then
  begin
    MatchRecList := TList.Create;
    for I := 1 to FRegExp.FGroups.Count - 1 do
    begin
      P := TRECapture.Create(FRegExp);
      P.FStartP := FRegExp.FGroups[I].StartP;
      P.FStartPBuf := FRegExp.FGroups[I].StartPBuf;
      P.FEndP := FRegExp.FGroups[I].EndP;
      P.FMatched := FRegExp.FGroups[I].Success;
      MatchRecList.Add(P);
    end;
    FGroup^[FCount] := MatchRecList;
  end
  else
    FGroup^[FCount] := nil;

{$IFDEF SKREGEXP_DEBUG}
  if FRegExp.FShowMatchProcess and FRegExp.ShowBackTrackStack then
  begin
    S := Copy(AStr, 1, 20);
    if NFACode <> nil then
      S := Format(#0009'[push:%d][NestLevel:%d] %s "%s"',
        [FCount, LStat.NestLevel + 1, NFACode.GetString, S])
    else
      if AStr <> nil then
        S := Format(#0009'[push:%d][NestLevel:%d] Group push "%s"',
          [FCount, LStat.NestLevel + 1, S])
      else
        S := Format(#0009'[push:%d][NestLevel:%d] Gosub break "%s"',
          [FCount, LStat.NestLevel + 1, S]);
    FRegExp.FMatchProcess.Add(S);
  end;
{$ENDIF SKREGEXP_DEBUG}
end;

procedure TREBackTrackStack.Remove(BranchCode: TRENFAState);
var
  SubCode, NextCode: TRENFAState;
begin
  if FCount = -1 then
    Exit;

{$IFDEF SKREGEXP_DEBUG}
  if FRegExp.FShowMatchProcess and FRegExp.ShowBackTrackStack then
    FRegExp.FMatchProcess.Add(#0009'Stack remove begin');
{$ENDIF SKREGEXP_DEBUG}
  NextCode := BranchCode;
  SubCode := Peek;

  while SubCode <> nil do
  begin
    while NextCode <> nil do
    begin
      if SubCode = NextCode then
      begin
        Exit;
      end;
      NextCode := NextCode.Next;
    end;

    Pop;

    NextCode := BranchCode;
    SubCode := Peek;
  end;
{$IFDEF SKREGEXP_DEBUG}
  if FRegExp.FShowMatchProcess and FRegExp.ShowBackTrackStack then
    FRegExp.FMatchProcess.Add(#0009'Stack remove end');
{$ENDIF SKREGEXP_DEBUG}
end;

procedure TREBackTrackStack.RemoveGoSub(const AGoSubIndex: Integer);
var
  Stat: PREBackTrackStateRec;
begin
  while FCount >= 0 do
  begin
    Stat := FStat[FCount];
    if Stat.NestLevel = AGoSubIndex then
      Pop
    else
      Break;
  end;
end;

procedure TREBackTrackStack.Remove(const AIndex: Integer);
begin
{$IFDEF SKREGEXP_DEBUG}
  if FRegExp.FShowMatchProcess and FRegExp.ShowBackTrackStack then
    FRegExp.FMatchProcess.Add(#0009'Stack remove begin');
  try
{$ENDIF SKREGEXP_DEBUG}
    while AIndex < FCount do
      Pop;
{$IFDEF SKREGEXP_DEBUG}
  finally
    if FRegExp.FShowMatchProcess and FRegExp.ShowBackTrackStack then
      FRegExp.FMatchProcess.Add(#0009'Stack remove end');
  end;
{$ENDIF SKREGEXP_DEBUG}
end;

procedure TREBackTrackStack.Extend(ASize: Integer);
begin
  if ASize < CONST_BackTrack_Stack_Default_Size then
    ASize := CONST_BackTrack_Stack_Default_Size;

  FSize := FSize + ASize;

  ReallocMem(FStat, FSize * Sizeof(Pointer));
  ReallocMem(FGroup, FSize * Sizeof(Pointer));
end;

function TREBackTrackStack.GetIndex: Integer;
begin
  Result := FCount;
end;

{ TRERecursionStack }

procedure TREGoSubStack.Clear;
var
  I: Integer;
  LStat: PREGoSubStateRec;
begin
  for I := FState.Count - 1 downto 0 do
  begin
    LStat := FState[I];
    if LStat <> nil then
    begin
      Dispose(LStat);
      FState[I] := nil;
    end;
  end;

  FState.Clear;
  FCurIndex := 0;
end;

function TREGoSubStack.Count: Integer;
begin
  Result := FCurIndex;
end;

constructor TREGoSubStack.Create(ARegExp: TSkRegExp);
begin
  inherited Create;
  FRegExp := ARegExp;
  FSize := CONST_Recursion_Stack_Default_Size;
  FState := TList.Create;
  FCurIndex := 0;
end;

destructor TREGoSubStack.Destroy;
begin
  Clear;

  FState.Free;
  inherited;
end;

function TREGoSubStack.GetGroupIndex: Integer;
begin
  Result := PREGoSubStateRec(FState[FCurIndex - 1]).Index;
end;

function TREGoSubStack.GetIndex: Integer;
begin
  Result := FCurIndex - 1;
end;

function TREGoSubStack.GetState(Index: Integer): PREGoSubStateRec;
begin
  Result := PREGoSubStateRec(FState[Index]);
end;

function TREGoSubStack.Peek: PREGoSubStateRec;
begin
  Result := FState[FCurIndex - 1];
end;

procedure TREGoSubStack.Pop;
begin
  Dec(FCurIndex);
end;

procedure TREGoSubStack.Push(AGroupIndex: Integer;
  EndCode, NextCode: TRENFAState);
var
  LStat, LStatSub: PREGoSubStateRec;
begin
  New(LStat);

  LStat.Index := AGroupIndex;
  LStat.EndCode := EndCode;
  LStat.NextCode := NextCode;
  LStat.PrevP := nil;

  if FCurIndex > FState.Count - 1 then
    FState.Add(LStat)
  else
  begin
    LStatSub := FState[FCurIndex];
    if LStatSub <> nil then
      Dispose(LStatSub);

    FState[FCurIndex] := LStat;
  end;

  Inc(FCurIndex);
end;

procedure TREGoSubStack.SetIndex(const Value: Integer);
begin
  FCurIndex := Value + 1;
end;

{ TREMatchEngine }

constructor TREMatchEngine.Create(ARegExp: TSkRegExp);
begin
  inherited Create;
  FRegExp := ARegExp;
  FBackTrackStack := TREBackTrackStack.Create(FRegExp);
  FOptimizeData := FRegExp.FOptimizeData;
  FLeadCode := TREOptimizeDataList.Create;
  FLeadMap := TRECharMap.Create;
  FACSearch := TREACSearch.Create;
  IsLeadMatch := IsLeadAllMatch;

  FLeadCharMode := lcmNone;
  FSkipP := nil;
  FSkipIndex := -1;

  FStateList := FRegExp.FStateList;
  FGroups := FRegExp.FGroups;
  FLoopState := FRegExp.FLoopState;
end;

procedure TREMatchEngine.CreateLeadMap(NFACode: TRENFAState; var NoMap: Boolean);

  procedure AddMap(Code: TRECode; var NoMap: Boolean);
  begin
    if Code is TRELiteralCode then
    begin
      FLeadMap.Add(ToUChar((Code as TRELiteralCode).FSubP), (Code as TRELiteralCode).FCompareOptions);
    end
    else if (Code is TRECharClassCode) and (Code as TRECharClassCode).FSimpleClass then
    begin
      if not (Code as TRECharClassCode).FNegative then
        FLeadMap.Add((Code as TRECharClassCode).FMap)
      else
        NoMap := True;
    end
    else
      NoMap := True;
  end;

var
  SubCode: TRENFAState;
begin
  while NFACode.Kind = nkGroupBegin do
  begin
    if NFACode.Next <> nil then
      CreateLeadMap(NFACode.Next, NoMap);

    NFACode := FStateList[NFACode.TransitTo];
  end;

  while NFACode <> nil do
  begin
    case NFACode.Kind of
      nkChar, nkNormal:
        AddMap(NFACode.Code, NoMap);
      nkStar:
        begin
          AddMap(NFACode.Code, NoMap);
          CreateLeadMap(FStateList[NFACode.TransitTo], NoMap);
        end;
      nkPlus:
        begin
          AddMap(NFACode.Code, NoMap);
        end;
      nkBound:
        begin
          AddMap(NFACode.Code, NoMap);
          if NFACode.Min = 0 then
            CreateLeadMap(FStateList[NFACode.TransitTo], NoMap);
        end;
      nkLoop:
        begin
          SubCode := FStateList[NFACode.TransitTo];
          if NFACode.Min = 0 then
          begin
            CreateLeadMap(FStateList[SubCode.TransitTo], NoMap);
          end;

          CreateLeadMap(SubCode.Next, NoMap);
        end;
      nkAheadMatch, nkSuspend, nkDefine, nkEmpty:
        CreateLeadMap(FStateList[NFACode.TransitTo], NoMap);
    else
      begin
        NoMap := True;
      end;
    end;

    if NoMap then
      Exit;

    NFACode := NFACode.Next;
  end;
end;

destructor TREMatchEngine.Destroy;
begin
  FACSearch.Free;
  FLeadMap.Free;
  FLeadCode.Free;
  FBackTrackStack.Free;
  inherited;
end;

function TREMatchEngine.Match(AStr: PWideChar): Boolean;
var
  I, K, L: Integer;
  IsCheck: Boolean;
  P, HeadP, StartP, LastP: PWideChar;
begin
  Result := False;
  FSkipP := nil;

  if (not (roIgnoreCase in FRegExp.FOptions) or
      (roASCIIOnly in FRegExp.FOptions)) then
  begin
    if FRegExp.FMinMatchLength > System.Length(FRegExp.InputString) then
      Exit;
  end;

  FRegExp.FLastRegMarkIndex := -1;
  FRegExp.FLastRegErrorIndex := -1;

  case FLeadCharMode of
    lcmFirstLiteral:
      begin
        P := AStr;
        if FLeadStrings.Search.Exec(P, FRegExp.FMatchEndP - P) then
        begin
          repeat
            StartP := FLeadStrings.Search.MatchP;

            I := FLeadCharOffset.Max;

            while I >= FLeadCharOffset.Min do
            begin
              P := StartP;

              if I > 0 then
                CharPrev(P, FRegExp.FMatchStartP, I);

              if MatchEntry(P) then
              begin
                Result := True;
                Exit;
              end;

              if FHasSkip and (FSkipP <> nil) then
                Break;

              Dec(I);
            end;

            if FHasSkip and (FSkipP <> nil) then
              FLeadStrings.Search.SkipP := FSkipP;

          until not FLeadStrings.Search.ExecNext;
        end;
      end;
    lcmFirstBranch:
      begin
        P := AStr;
        if FACSearch.Exec(P, FRegExp.FMatchEndP - P) then
        begin
          repeat
            StartP := FACSearch.StartP;

            I := FLeadCharOffset.Max;

            while I >= FLeadCharOffset.Min do
            begin
              P := StartP;

              if I > 0 then
                CharPrev(P, FRegExp.FMatchStartP, I);

              if MatchEntry(P) then
              begin
                Result := True;
                Exit;
              end;

              if FHasSkip and (FSkipP <> nil) then
                Break;

              Dec(I);
            end;

            if FHasSkip and (FSkipP <> nil) then
              FACSearch.SkipP := FSkipP;

          until not FACSearch.ExecNext;
        end;
      end;
    lcmSimple:
      begin
        if FLeadStrings.Search.Exec(AStr, FRegExp.FMatchEndP - AStr) then
        begin
          Result := True;
          FGroups[0].StartP := FLeadStrings.Search.MatchP;
          FGroups[0].EndP := FLeadStrings.Search.MatchP + FLeadStrings.Search.MatchLen;
        end;
      end;
    lcmSimpleBranch:
      begin
        if FACSearch.Exec(AStr, FRegExp.FMatchEndP - AStr) then
        begin
          Result := True;
          FGroups[0].StartP := FACSearch.StartP;
          FGroups[0].EndP := FACSearch.EndP;
        end;
      end;
    lcmTextTop:
      begin
        if MatchEntry(AStr) then
          Result := True
        else
          Result := False;
        FSkipP := FRegExp.FMatchEndP;
        Exit;
      end;
    lcmLineTop:
      begin
        while AStr <= FRegExp.FMatchEndP do
        begin
          if MatchEntry(AStr) then
          begin
            Result := True;
            Exit;
          end;

          if FSkipP <> nil then
          begin
            AStr := FSkipP;
            if AStr = FRegExp.FMatchEndP then
              Exit;
            FSkipP := nil;
          end
          else
          begin
            while (FRegExp.IsLineBreak(AStr) = 0) and
                (AStr <= FRegExp.FMatchEndP) do
              Inc(AStr);

            L := FRegExp.IsLineBreak(AStr);
            if L > 0 then
            begin
              repeat
                Inc(AStr, L);
                L := FRegExp.IsLineBreak(AStr);
              until L = 0;
            end
            else
              Break;
          end;
        end;
      end;
    lcmFixedAnchor:
      begin
        if FAnchorStrings.Search.Exec(AStr, FRegExp.FMatchEndP - AStr) then
        begin
          repeat
            StartP := FAnchorStrings.Search.MatchP;

            I := FAnchorOffset.Max;

            while I >= FAnchorOffset.Min do
            begin
              P := StartP;
              if I > 0 then
                CharPrev(P, FRegExp.FMatchStartP, I);

              if MatchEntry(P) then
              begin
                Result := True;
                Exit;
              end;

              if (FSkipP <> nil) then
              begin
                FAnchorStrings.Search.SkipP := FSkipP;
                Break;
              end;

              Dec(I);
            end;

          until not FAnchorStrings.Search.ExecNext;
        end;
      end;
    lcmVariableAnchor:
      begin
        StartP := AStr;
        if FAnchorStrings.Search.Exec(AStr, FRegExp.FMatchEndP - AStr) then
        begin
          repeat
            LastP := FAnchorStrings.Search.MatchP;
            if FAnchorOffset.Min = -1 then
              CharNext(LastP, FRegExp.FMatchEndP, FAnchorStrings.CharLength.Max);

            P := StartP;

            while P <= LastP do
            begin
              if IsLeadMatch(P) then
              begin
                if MatchEntry(P) then
                begin
                  Result := True;
                  Exit;
                end;

                if (FSkipP <> nil) and (FSkipP <= LastP) then
                begin
                  P := FSkipP;
                end
                else
                begin
                  if IsLeadChar(P^) then
                    Inc(P);
                  Inc(P);
                end;

                FSkipP := nil;
              end
              else
              begin
                if IsLeadChar(P^) then
                  Inc(P);
                Inc(P);
              end;
            end;

            if FSkipP <> nil then
            begin
              StartP := FSkipP;
              FAnchorStrings.Search.SkipP := FSkipP;
              FSkipP := nil;
            end
            else
            begin
              StartP := P;
            end;

          until not FAnchorStrings.Search.ExecNext;
        end;
      end;
    lcmFixedBranch:
      begin
        if FACSearch.Exec(AStr, FRegExp.FMatchEndP - AStr) then
        begin
          repeat
            StartP := FACSearch.StartP;

            I := FAnchorOffset.Max;

            while I >= FAnchorOffset.Min do
            begin
              P := StartP;
              if I > 0 then
                CharPrev(P, FRegExp.FMatchStartP, I);

              if MatchEntry(P) then
              begin
                Result := True;
                Exit;
              end;

              if FSkipP <> nil then
              begin
                AStr := FSkipP;
                FACSearch.SkipP := FSkipP;
                FSkipP := nil;
                Break;
              end;

              Dec(I);
            end;

          until not FACSearch.ExecNext;
        end;
      end;
    lcmVariableBranch:
      begin
        StartP := AStr;
        if FACSearch.Exec(AStr, FRegExp.FMatchEndP - AStr) then
        begin
          repeat
            LastP := FACSearch.StartP;
            P := StartP;

            while P <= LastP do
            begin
              if IsLeadMatch(P) then
              begin
                if MatchEntry(P) then
                begin
                  Result := True;
                  Exit;
                end;

                if (FSkipP <> nil) and (FSkipP <= LastP) then
                  P := FSkipP
                else
                begin
                  if IsLeadChar(P^) then
                    Inc(P);
                  Inc(P);
                end;
              end
              else
              begin
                if IsLeadChar(P^) then
                  Inc(P);
                Inc(P);
              end;
            end;

            if FSkipP <> nil then
            begin
              StartP := FSkipP;
              FACSearch.SkipP := FSkipP;
              FSkipP := nil;
            end
            else
            begin
              StartP := P;
            end;

          until not FACSearch.ExecNext;
        end;
      end;
    lcmLeadMap:
      begin
        while AStr <= FRegExp.FMatchEndP do
        begin
          if FLeadMap.IsExists(AStr) then
          begin
            if MatchEntry(AStr) then
            begin
              Result := True;
              Exit;
            end;
          end;

          if FSkipP <> nil then
          begin
            AStr := FSkipP;
            if AStr = FRegExp.FMatchEndP then
              Exit;
            FSkipP := nil;
          end
          else
          begin
            if IsLeadChar(AStr^) then
              Inc(AStr);
            Inc(AStr);
          end;
        end;
      end;
    lcmHasLead:
      begin
        while AStr <= FRegExp.FMatchEndP do
        begin
          if FLeadCode.IsEqual(AStr) then
          begin
            if MatchEntry(AStr) then
            begin
              Result := True;
              Exit;
            end;
          end;

          if FHasSkip and (FSkipP <> nil) then
          begin
            AStr := FSkipP;
            if AStr = FRegExp.FMatchEndP then
              Exit;
            FSkipP := nil;
          end
          else
          begin
            if IsLeadChar(AStr^) then
              Inc(AStr);
            Inc(AStr);
          end;
        end;
      end
  else
    begin
      while AStr <= FRegExp.FMatchEndP do
      begin
        if MatchEntry(AStr) then
        begin
          Result := True;
          Exit;
        end;

        if FHasSkip and (FSkipP <> nil) then
        begin
          AStr := FSkipP;
          if AStr = FRegExp.FMatchEndP then
            Exit;
          FSkipP := nil;
        end
        else
        begin
          if IsLeadChar(AStr^) then
            Inc(AStr);
          Inc(AStr);
        end;
      end;
    end;
  end;
end;

function TREMatchEngine.MatchAhead(var NFACode: TRENFAState;
  var AStr: PWideChar): Boolean;
var
  Index: Integer;
  SaveP: PWideChar;
  Stack: TREBackTrackStack;
begin
  Stack := TREBackTrackStack.Create(FRegExp, False);
  try
    Result := False;
    SaveP := AStr;
    Index := Stack.Index;

    while NFACode <> nil do
    begin
      NFACode := MatchPrim(NFACode, Stack, AStr);

      if (NFACode = nil) then
      begin
        if FGroups[0].EndP <> nil then
        begin
          Result := True;
          Exit;
        end
        else if Stack.Index > Index then
          Stack.Pop(NFACode, AStr);
      end;

      if (NFACode <> nil) then
      begin
        if NFACode.Kind in [nkEnd, nkMatchEnd] then
        begin
          Result := True;
          Exit;
        end
      end;
    end;

    if not Result then
      AStr := SaveP;
  finally
    Stack.Free;
  end;
end;

function TREMatchEngine.MatchCore(var NFACode: TRENFAState;
  Stack: TREBackTrackStack; var AStr: PWideChar): Boolean;
begin
  Result := False;

  while NFACode <> nil do
  begin
    NFACode := MatchPrim(NFACode, Stack, AStr);

    if (NFACode = nil) then
    begin
      if (FGroups[0].EndP <> nil) then
      begin
        Result := True;
        Exit;
      end
      else
        Stack.Pop(NFACode, AStr);
    end;

    if (NFACode <> nil) and (NFACode.Kind = nkMatchEnd) then
    begin
      Result := True;
      Exit;
    end;
  end;
end;

function TREMatchEngine.MatchCore(var NFACode, EndCode: TRENFAState;
  var AStr: PWideChar): Boolean;
var
  Index: Integer;
  SaveP: PWideChar;
  Stack: TREBackTrackStack;
begin
  Stack := TREBackTrackStack.Create(FRegExp, False);
  try
    Result := False;
    SaveP := AStr;
    Index := Stack.Index;

    while NFACode <> nil do
    begin
      NFACode := MatchPrim(NFACode, Stack, AStr);

      if (NFACode = nil) then
      begin
        if FGroups[0].EndP <> nil then
        begin
          Result := True;
          Exit;
        end
        else if Stack.Index > Index then
          Stack.Pop(NFACode, AStr);
      end;

      if (NFACode <> nil) and
          ((NFACode = EndCode) or (NFACode.Kind = nkMatchEnd)) then
      begin
        Result := True;
        Exit;
      end;
    end;

    if not Result then
      AStr := SaveP;
  finally
    Stack.Free;
  end;
end;

function TREMatchEngine.MatchEntry(AStr: PWideChar): Boolean;
var
  NFACode: TRENFAState;
begin
  FGroups.Reset;
  FRegExp.FSubStack.Clear;
  FLoopState.Reset;
  FBackTrackStack.Clear;
  FHasSkip := False;
  FSkipP := nil;
  FSkipIndex := -1;

  NFACode := FStateList[FRegExp.FEntryState];
  FGroups[0].StartP := AStr;
  FRegExp.FStartMatch := AStr - FRegExp.FTextTopP + 1;
  Result := MatchCore(NFACode, FBackTrackStack, AStr);

  if not Result then
    FRegExp.FLastRegErrorIndex := FRegExp.FLastRegMarkIndex;
end;

function TREMatchEngine.MatchPrim(NFACode: TRENFAState;
  Stack: TREBackTrackStack; var AStr: PWideChar): TRENFAState;

  function MatchLoop(EntryCode, EndCode: TRENFAState;
    Stack: TREBackTrackStack; var AStr: PWideChar): Boolean;
  var
    NFACode: TRENFAState;
    Index: Integer;
    SaveP: PWideChar;
  begin
    Result := False;
    Index := Stack.Index;
    SaveP := AStr;
    NFACode := EntryCode;
    while NFACode <> nil do
    begin
      while NFACode <> nil do
      begin
        NFACode := MatchPrim(NFACode, Stack, AStr);

        if (NFACode <> nil) and
          ((NFACode = EndCode) or (NFACode.Kind = nkMatchEnd)) then
        begin
          Result := True;
          Break;
        end
      end;

      if (NFACode = nil) then
      begin
        if FGroups[0].EndP <> nil then
          Exit;

        if Stack.Index > Index then
        begin
          Stack.Pop(NFACode, AStr);

          if (NFACode <> nil) and
            ((NFACode = EndCode) or (NFACode.Kind = nkMatchEnd)) then
          begin
            Result := True;
            Break;
          end
        end
        else
          Break;
      end
      else
        Break;
    end;

    if (NFACode = nil) and Result then
      AStr := SaveP;
  end;

  procedure BranchSetup(NFACode: TRENFAState; AStr: PWideChar;
    IsPushGroup: Boolean);
  var
    Len: Integer;
  begin
    if NFACode <> nil then
    begin
      if NFACode.Kind in [nkChar, nkNormal] then
      begin
        if NFACode.Code.IsEqual(AStr, Len) then
          Stack.Push(NFACode, AStr, IsPushGroup)
        else if NFACode.Next <> nil then
          BranchSetup(NFACode.Next, AStr, IsPushGroup)
      end
      else
        Stack.Push(NFACode, AStr, IsPushGroup);
    end;
  end;

  function GetBranchCode(NextCode, LoopCode: TRENFAState): TRENFAState;
  var
    NFACode: TRENFAState;
  begin
    Result := nil;
    NFACode := FStateList[NextCode.TransitTo];

    if (NFACode.Kind = nkLoopExit) then
    begin
      NFACode := NFACode.Next;

      while (NFACode <> nil) do
      begin
        if NFACode <> LoopCode then
          Result := NFACode
        else if (Result <> nil) and (NFACode = LoopCode) then
          Result := nil;

        NFACode := NFACode.Next;
      end;
    end;
  end;

  function PreMatchLoopNext(ALoopIndex: Integer;
    NextCode: TRENFAState; AStr: PWideChar): Boolean;
  begin
    Result := True;

    while NextCode.Kind = nkGroupEnd do
      NextCode := FStateList[NextCode.TransitTo];

    if NextCode.Next <> nil then
      Exit;

    if not (NextCode.Kind in [nkEnd, nkStar, nkPlus, nkBound]) then
    begin
      if (NextCode.Code is TRELiteralCode) or
          ((NextCode.Code is TRECharClassCode) and
          (NextCode.Code as TRECharClassCode).SimpleClass) then
      begin
        FLoopState[ALoopIndex].MatchP := NextCode.Code.Find(AStr);
        Result := FLoopState[ALoopIndex].MatchP <> nil;
      end;
    end;
  end;

var
  I, Len, LMin, LMax, Index, BaseIndex, LLoopIndex: Integer;
  EntryCode, NextCode, EndCode, SubCode, LoopCode, BranchCode: TRENFAState;
  LMatchKind: TRELoopKind;
  SubP, BufP, SaveP:PWideChar;
  IsMatched, IsLoopMatched: Boolean;
{$IFDEF SKREGEXP_DEBUG}
  CurrentNFA: TRENFAState;
{$ENDIF}
begin
  Result := nil;
  BaseIndex := Stack.Index;

{$IFDEF SKREGEXP_DEBUG}
  IsLoopMatched := False;
  CurrentNFA := NFACode;
  try
{$ENDIF}
    if NFACode.Next <> nil then
    begin
      if NFACode.Kind in [nkIfThen] then
      begin
        // nkIfTheがここに入ってくることはありえない。
        // スタックへ退避せず次へ遷移
        NFACode := FStateList[NFACode.TransitTo];
{$IFDEF SKREGEXP_DEBUG}
        if FRegExp.FShowMatchProcess then
          CurrentNFA := NFACode;
{$ENDIF}
      end
      else
      begin
        if not (NFACode.Kind in [nkLoopExit, nkThen]) then
          // 分岐があればスタックへ退避
          BranchSetup(NFACode.Next, AStr, True);
      end;
    end;

{$IFDEF SKREGEXP_DEBUG}
    if FRegExp.FShowMatchProcess then
    begin
      MatchProcessAdd(NFACode, AStr, FRegExp.FSubStack.Index);
    end;
{$ENDIF}
    case NFACode.Kind of
      nkNormal, nkChar, nkCallout:
        begin
          if NFACode.Code.IsEqual(AStr, Len) then
          begin
            Inc(AStr, Len);
            Result := FStateList[NFACode.TransitTo];
          end
          else
            Result := nil;
        end;
      nkStar, nkPlus:
        begin
          SaveP := AStr;
          SubP := AStr;
          LMatchKind := NFACode.MatchKind;
          NextCode := FStateList[NFACode.TransitTo];
          EntryCode := NFACode;

          if NextCode.Kind = nkLoopEnd then
            BranchCode := GetBranchCode(NextCode, NFACode)
          else
            BranchCode := nil;

          if EntryCode.Code.ExecRepeat(AStr, NFACode.Kind = nkStar) then
          begin
            if LMatchKind = lkAny then
            begin
              Len := 1;
              // Plus でマッチしたときは SubP を１単位進める
              if NFACode.Kind = nkPlus then
                CharNext(SubP);

              while SubP <= AStr do
              begin
                BranchSetup(NextCode, SubP, True);
                if BranchCode <> nil then
                  BranchSetup(BranchCode, SubP, True);

                CharNext(SubP, Len);
              end;

              Result := NextCode;
            end
{$IFDEF USE_UNICODE_PROPERTY}
            else if LMatchKind = lkCombiningSequence then
            begin
              AStr := FRegExp.FMatchEndP;
              Len := 1;
              // Plus でマッチしたときは SubP を１単位進める
              if NFACode.Kind = nkPlus then
                CharNextForCombiningSequence(SubP);

              while SubP <= AStr do
              begin
                BranchSetup(NextCode, SubP, True);
                if BranchCode <> nil then
                  BranchSetup(BranchCode, SubP, True);

                CharNextForCombiningSequence(SubP, Len);
              end;

              Result := NextCode;
            end
{$ENDIF USE_UNICODE_PROPERTY}
            else
            begin
              if LMatchKind <> lkPossessive then
              begin
                // Plus でマッチしたときは SubP を１単位進める
                Len := EntryCode.Code.CharLength.Min;
                if NFACode.Kind = nkPlus then
                  CharNext(SubP, Len);

                while SubP < AStr do
                begin
                  BranchSetup(NextCode, SubP, True);
                  if BranchCode <> nil then
                    BranchSetup(BranchCode, SubP, True);

                  CharNext(SubP, Len);
                end;

                Result := NextCode;
              end
              else
              begin
                Stack.Remove(BaseIndex);
                Result := NextCode;
                if not FRegExp.FHasReference and (AStr - SaveP > 0) then
                  FSkipP := AStr;
              end;
            end;
          end
          else
            Result := nil;
        end;
      nkBound:
        begin
          SaveP := AStr;
          SubP := AStr;
          LMatchKind := NFACode.MatchKind;
          LMin := NFACode.Min;
          LMax := NFACode.Max;

          NextCode := FStateList[NFACode.TransitTo];
          EntryCode := NFACode;

          if NextCode.Kind = nkLoopEnd then
            BranchCode := GetBranchCode(NextCode, NFACode)
          else
            BranchCode := nil;

          if EntryCode.Code.ExecRepeat(AStr, LMin, LMax) then
          begin
            Len := EntryCode.Code.CharLength.Min;

            // LMin 分は確定
            if LMin > 0 then
              CharNext(SubP, LMin * Len);

            if LMatchKind <> lkPossessive then
            begin
              while SubP < AStr do
              begin
                BranchSetup(NextCode, SubP, True);
                if BranchCode <> nil then
                  BranchSetup(BranchCode, SubP, True);

                CharNext(SubP, Len);
              end;

              Result := NextCode;
            end //
            else
            begin
              Stack.Remove(BaseIndex);
              Result := NextCode;
            end;
          end
          else
            Result := nil;
          end;
      nkLoop:
        begin
          LoopCode := NFACode;
          LLoopIndex := NFACode.LoopIndex;
          BranchCode := NFACode.Next;
          SaveP := AStr;

          LMatchKind := NFACode.MatchKind;
          LMin := NFACode.Min;
          LMax := NFACode.Max;
          FLoopState[LLoopIndex].Step := 0;

          // ループの終了を登録
          EndCode := FStateList[NFACode.ExtendTo];
          NFACode := FStateList[NFACode.TransitTo];
          // ループの次を登録
          NextCode := FStateList[NFACode.TransitTo];
          // ループの入口を登録
          EntryCode := NFACode.Next;

          if (NextCode.Kind = nkLoopEnd) then
            BranchCode := GetBranchCode(NextCode, LoopCode);

          if LMin > 0 then
          begin
            if LMin = LMax then
            begin
              I := 0;
              Len := 0;
              NFACode := EntryCode;

              while I < LMin do
              begin
                SubP := AStr;
                if not MatchCore(NFACode, EndCode, Stack, AStr) then
                begin
                  if Stack.Index > BaseIndex then
                  begin
{$IFDEF SKREGEXP_DEBUG}
                    if FRegExp.FShowMatchProcess then
                    begin
                      FRegExp.FMatchProcess.Add(#0009'not matched, continue');
                    end;
{$ENDIF}
                    Stack.Pop(NFACode, AStr);
                    if NFACode = nil then
                    begin
                      AStr := SaveP;
                      Exit;
                    end;
                    Continue;
                  end
                  else
                  begin
                    AStr := SaveP;
                    Exit;
                  end;
                end
                else
                begin
                  NFACode := EntryCode;
                  if SubP = AStr then
                  begin
                    if BufP = AStr then
                    begin
                      Inc(I);
                      if I = LMin then
                        Result := NextCode;
                      Exit;
                    end;
{$IFDEF SKREGEXP_DEBUG}
                    if FRegExp.FShowMatchProcess then
                    begin
                      FRegExp.FMatchProcess.Add(#0009'empty matched, continue');
                    end;
{$ENDIF}
                    BufP := AStr;
                    Continue;
                  end;

                  Inc(I);
                  FLoopState[LLoopIndex].Up;
{$IFDEF SKREGEXP_DEBUG}
                  if FRegExp.FShowMatchProcess then
                  begin
                    FRegExp.FMatchProcess.Add(
                      Format(#0009'matched %d out of %d..%d',[FLoopState[LLoopIndex].Step, LMin, LMax]));
                  end;
{$ENDIF}
                end;
              end;
              Result := NextCode;
              Exit;
            end
            else
            begin
              for I := 1 to LMin do
              begin
                SubP := AStr;
                if not MatchLoop(EntryCode, EndCode, Stack, AStr) then
                begin
                  AStr := SaveP;
                  Exit;
                end;
                FLoopState[LLoopIndex].Up;
                FLoopState[LLoopIndex].PrevP := AStr;
{$IFDEF SKREGEXP_DEBUG}
                if FRegExp.FShowMatchProcess then
                begin
                  FRegExp.FMatchProcess.Add(
                    Format(#0009'matched %d out of %d..%d',[FLoopState[LLoopIndex].Step, LMin, LMax]));
                end;
{$ENDIF}
              end;

              if AStr = SubP then
              begin
                Result := NextCode;
                Exit;
              end;

            end;
          end;

          IsLoopMatched := True;

          if LMax <> CONST_LoopMax then
            LMax := LMax - LMin;

          if LMatchKind = lkGreedy then
          begin
            NFACode := EntryCode;
            IsMatched := True;

            for I := 1 to LMax do
            begin
              SubP := AStr;
              if (BranchCode <> nil) and (I > 1) then
                BranchSetup(BranchCode, AStr, True);
              BranchSetup(NextCode, AStr, True);

              IsMatched := MatchLoop(NFACode, EndCode, Stack, AStr);

              if not IsMatched or (SubP = AStr) then
              begin
{$IFDEF SKREGEXP_DEBUG}
                if FRegExp.FShowMatchProcess and IsMatched then
                begin
                  FRegExp.FMatchProcess.Add(#0009'empty matched, continue');
                end;
{$ENDIF}
                if IsMatched then
                begin
                  FLoopState[LLoopIndex].Up;
                  FLoopState[LLoopIndex].PrevP := AStr;
                end;
                Break;
              end;

              FLoopState[LLoopIndex].Up;
              FLoopState[LLoopIndex].PrevP := AStr;

{$IFDEF SKREGEXP_DEBUG}
              if FRegExp.FShowMatchProcess then
              begin
                FRegExp.FMatchProcess.Add(
                  Format(#0009'matched %d out of %d..%d',
                    [FLoopState[LLoopIndex].Step, LoopCode.Min, LoopCode.Max]));
              end;
{$ENDIF}
            end;

            if IsMatched then
              Result := NextCode
            else if not IsLoopMatched then
              AStr := SaveP;
          end
          else if LMatchKind = lkSimpleReluctant then
          begin
            if not PreMatchLoopNext(LLoopIndex, NextCode, AStr) then
            begin
              if LMin = 0 then
                Result := NextCode
              else if NextCode.BranchIndex = 0 then
                FSkipP := FRegExp.FMatchEndP;
              Exit;
            end;

            for I := 1 to LMax do
            begin
              if (FLoopState[LLoopIndex].MatchP <> nil) and
                  (AStr >= FLoopState[LLoopIndex].MatchP) then
              begin
                BranchSetup(EntryCode, AStr, True);
                Break;
              end;

              if not MatchLoop(EntryCode, EndCode, Stack, AStr) then
                Break;

              FLoopState[LLoopIndex].Up;
              FLoopState[LLoopIndex].PrevP := AStr;
            end;

            Result := NextCode;
          end
          else if LMatchKind = lkReluctant then
          begin
            NFACode := EntryCode;

            for I := 1 to LMax do
            begin
              SubP := AStr;
              Stack.Push(nil, AStr, True);
              Result := NextCode;

              if MatchAhead(Result, AStr) then
              begin
                BranchSetup(EntryCode, AStr, True);
                Exit;
              end
              else
              begin
                Stack.Pop(SubCode, AStr);
                AStr := SubP;
              end;

              IsMatched := MatchLoop(NFACode, EndCode, Stack, AStr);

              if not IsLoopMatched and IsMatched then
                IsLoopMatched := True;

              if not IsMatched or (SubP = AStr) then
                Break;

              FLoopState[LLoopIndex].Up;
              FLoopState[LLoopIndex].PrevP := AStr;
{$IFDEF SKREGEXP_DEBUG}
              if FRegExp.FShowMatchProcess then
              begin
                FRegExp.FMatchProcess.Add(
                  Format(#0009'matched %d out of %d..%d',
                    [FLoopState[LLoopIndex].Step, LoopCode.Min, LoopCode.Max]));
              end;
{$ENDIF}
            end;

            if IsLoopMatched then
              Result := NextCode
            else
              AStr := SaveP;
          end
          else if LMatchKind = lkPossessive then
          begin
            NFACode := EntryCode;

            for I := 1 to LMax do
            begin
              SubP := AStr;
              IsMatched := MatchLoop(NFACode, EndCode, Stack, AStr);

              if not IsLoopMatched and IsMatched then
                IsLoopMatched := True;

              if not IsMatched or (SubP = AStr) then
                Break;

              FLoopState[LLoopIndex].Up;
              FLoopState[LLoopIndex].PrevP := AStr;
{$IFDEF SKREGEXP_DEBUG}
              if FRegExp.FShowMatchProcess then
              begin
                FRegExp.FMatchProcess.Add(
                  Format(#0009'matched %d out of %d..%d',
                      [FLoopState[LLoopIndex].Step, LoopCode.Min, LoopCode.Max]));
              end;
{$ENDIF}
            end;
            Stack.Remove(BaseIndex);

            Result := NextCode;
            if not FRegExp.FHasReference and
                (AStr - SaveP > 0) and (LMin > 0) then
              FSkipP := AStr;
          end;
        end;
      nkEnd:
        begin
          if (FRegExp.FSubStack.Count > 0) and
            (FRegExp.FSubStack[FRegExp.FSubStack.Count - 1].EndCode = NFACode) then
          begin
            Result := FRegExp.FSubStack[FRegExp.FSubStack.Count - 1].NextCode;
//@            FGroups.Pop;
            FRegExp.FGroupStack.Pop(FGroups);
            FRegExp.FSubStack.Pop;
          end
          else
          begin
            FGroups[0].EndP := AStr;
            Result := nil;
          end;
        end;
      nkLoopExit:
        begin
          LLoopIndex := NFACode.LoopIndex;
          SubCode := FLoopState[LLoopIndex].NFACode;
          NextCode := FStateList[NFACode.TransitTo];

{$IFDEF CHECK_MATCH_EXPLOSTION}
          if Stack.FCheckMatchExplosion and
              FRegExp.IsAlreadyTried(SubCode, AStr) then
{$ELSE CHECK_MATCH_EXPLOSTION}
          if FLoopState[LLoopIndex].Step > System.Length(FRegExp.FInputString) * 2 then
{$ENDIF CHECK_MATCH_EXPLOSTION}
          begin
            Result := NextCode;
            Exit;
          end;

          if NFACode.MatchKind = lkGreedy then
          begin
            if ((FLoopState[LLoopIndex].PrevP <> nil) and
                (AStr = (FLoopState[LLoopIndex].PrevP))) or
                (FLoopState[LLoopIndex].Step >= NFACode.Max) then
            begin
              Result := NextCode;
            end
            else
            begin
              BranchSetup(NextCode, AStr, True);
              Result := NFACode.Next;
              FLoopState[LLoopIndex].PrevP := AStr;
            end;
          end
          else if NFACode.MatchKind = lkReluctant then
          begin
            BranchSetup(NFACode.Next, AStr, True);
            Result := NextCode;
          end
          else
            Result := NextCode;
        end;
      nkLoopEnd:
        begin
          LLoopIndex := NFACode.LoopIndex;
          FLoopState[LLoopIndex].Up;
          Result := FStateList[NFACode.TransitTo];
{$IFDEF SKREGEXP_DEBUG}
          if FRegExp.FShowMatchProcess then
          begin
            FRegExp.FMatchProcess.Add(
              Format(#0009'matched %d out of %d..%d',[FLoopState[LLoopIndex].Step, NFACode.Min, NFACode.Max]));
          end;
{$ENDIF}
        end;
      nkEmpty, nkIfThen:
        Result := FStateList[NFACode.TransitTo];
      nkGroupBegin:
        begin
          FGroups[NFACode.GroupIndex].StartP := AStr;
          Result := FStateList[NFACode.TransitTo];
{$IFDEF SKREGEXP_DEBUG}
          if FRegExp.FShowMatchProcess and FRegExp.ShowCaptureHistory then
          begin
            FRegExp.FMatchProcess.Add(Format(#0009#0009'Cap%d: [Index:%d]',
              [NFACode.GroupIndex, FGroups[NFACode.GroupIndex].StartPBuf - FRegExp.FTextTopP + 1]));
          end;
{$ENDIF SKREGEXP_DEBUG}
        end;
      nkGroupEnd:
        begin
          FGroups[NFACode.GroupIndex].EndP := AStr;
{$IFDEF SKREGEXP_DEBUG}
          if FRegExp.FShowMatchProcess and FRegExp.ShowCaptureHistory then
            FRegExp.FMatchProcess.Add(Format(#0009#0009'Cap%d:"%s" [Index:%d, Length:%d]',
              [NFACode.GroupIndex, FRegExp.EncodeEscape(FGroups[NFACode.GroupIndex].Strings),
              FGroups[NFACode.GroupIndex].Index, FGroups[NFACode.GroupIndex].Length]));
{$ENDIF SKREGEXP_DEBUG}
          if (FRegExp.FSubStack.Count > 0) and
            (FRegExp.FSubStack.Peek.EndCode = NFACode) then
          begin
{$IFDEF SKREGEXP_DEBUG}
            if FRegExp.FShowMatchProcess then
              FRegExp.FMatchProcess.Add(Format(#0009#0009'NestLevel: %d',
                [FRegExp.FSubStack.Index + 1]));
{$ENDIF SKREGEXP_DEBUG}
            Result := FRegExp.FSubStack[FRegExp.FSubStack.Count - 1].NextCode;
//@            FGroups.Pop;
            FRegExp.FGroupStack.Pop(FGroups);
            FLoopState.Pop;
            FRegExp.FSubStack.Pop;
          end
          else
          begin
            Result := FStateList[NFACode.TransitTo];
          end;
        end;
      nkSuspend:
        begin
          EndCode := FStateList[NFACode.ExtendTo];
          NFACode := FStateList[NFACode.TransitTo];
          SubP := AStr;
          if MatchCore(NFACode, EndCode, AStr) then
          begin
            Result := FStateList[EndCode.TransitTo];
          end
          else
          begin
            AStr := SubP;
            Result := nil;
          end;
        end;
      nkKeepPattern:
        begin
          Stack.Clear;
          FGroups[0].StartP := AStr;
          Result := FStateList[NFACode.TransitTo];
        end;
      nkGoSub:
        begin
          Index := NFACode.GroupIndex;

          if Index > 0 then
          begin
            Index := NFACode.GroupIndex;
            NextCode := FStateList[NFACode.TransitTo];

            EntryCode := FGroups[Index].GroupBegin;
            EndCode := FGroups[Index].GroupEnd;

            Stack.Push(nil, nil, False);
            FRegExp.FSubStack.Push(Index, EndCode, NextCode);
//@            FGroups.Push;
            FRegExp.FGroupStack.Push(FGroups);
            FLoopState.Push;
            Result := EntryCode;
          end
          else
          begin
            EntryCode := FStateList[FRegExp.FEntryState];
            EndCode := FStateList[FRegExp.FExitState];

            FRegExp.FSubStack.Push(Index, EndCode,
              FStateList[NFACode.TransitTo]);
//@            FGroups.Push;
            FRegExp.FGroupStack.Push(FGroups);
            FLoopState.Push;
            Result := EntryCode;
          end;
        end;
      nkMatchEnd:
        begin
          Result := NFACode;
          Exit;
        end;
      nkAheadMatch:
        begin
          EndCode := FStateList[NFACode.ExtendTo];
          NFACode := FStateList[NFACode.TransitTo];

          SubP := AStr;
          if MatchCore(NFACode, EndCode, SubP) then
            Result := FStateList[NFACode.TransitTo]
          else
            Result := nil;
        end;
      nkAheadNoMatch:
        begin
          EndCode := FStateList[NFACode.ExtendTo];
          NFACode := FStateList[NFACode.TransitTo];

          SubP := AStr;
          if MatchCore(NFACode, EndCode, SubP) then
            Result := nil
          else
            Result := FStateList[EndCode.TransitTo];
        end;
      nkBehindMatch:
        begin
          SubP := AStr;
          LMax := NFACode.Max;
          LMin := NFACode.Min;

          EndCode := FStateList[NFACode.ExtendTo];
          EntryCode := FStateList[NFACode.TransitTo];

          if LMin = LMax then
            Len := LMin
          else
            Len := LMax;

          if FRegExp.FMatchStartP > (SubP - Len) then
            Len := AStr - FRegExp.FMatchStartP;

          if Len >= LMin then
          begin
            CharPrev(SubP, Len);
            SaveP := SubP;
            NFACode := EntryCode;
            
            IsMatched := MatchCore(NFACode, EndCode, SubP);
            if IsMatched and (SubP <> AStr) then
              IsMatched := False;
          
            while not IsMatched do
            begin
              SubP := SaveP;
              CharNext(SubP);
              SaveP := SubP;
              if AStr - SubP >= LMin then
              begin
                NFACode := EntryCode;
                IsMatched := MatchCore(NFACode, EndCode, SubP);
                if IsMatched and (SubP <> AStr) then
                  IsMatched := False;
              end
              else
                Break;
            end;
            
            if not IsMatched then
              Result := nil
            else
            begin
              if SubP = AStr then
                Result := FStateList[NFACode.TransitTo]
              else
                Result := nil;
            end;
          end
          else
          begin
            Result := nil;
            Exit;
          end;
        end;
      nkBehindNoMatch:
        begin
          SubP := AStr;
          LMax := NFACode.Max;
          LMin := NFACode.Min;

          EndCode := FStateList[NFACode.ExtendTo];
          Result := FStateList[EndCode.TransitTo];
          EntryCode := FStateList[NFACode.TransitTo];

          if LMin = LMax then
            Len := LMin
          else
            Len := LMax - LMin + 1;

          if FRegExp.FMatchStartP > (SubP - Len) then
            Len := AStr - FRegExp.FMatchStartP;

          if Len >= LMin then
          begin
            CharPrev(SubP, Len);
            SaveP := SubP;
            NFACode := EntryCode;

            IsMatched := MatchCore(NFACode, EndCode, SubP);
            if IsMatched and (SubP <> AStr) then
              IsMatched := False;

            while not IsMatched do
            begin
              SubP := SaveP;
              CharNext(SubP);
              SaveP := SubP;
              if AStr - SubP >= LMin then
              begin
                NFACode := EntryCode;
                IsMatched := MatchCore(NFACode, EndCode, SubP);
                if IsMatched and (SubP <> AStr) then
                  IsMatched := False;
              end
              else
                Break;
            end;

            if IsMatched then
              Result := nil;
          end;
        end;
      nkIfMatch:
        begin
          SubP := AStr;
          EntryCode := FStateList[NFACode.TransitTo];
          EndCode := FStateList[NFACode.ExtendTo];
          NextCode := FStateList[EndCode.Next.TransitTo];
          if EndCode.Next.Next <> nil then
            SubCode := FStateList[EndCode.Next.Next.TransitTo];

          if MatchCore(EntryCode, EndCode, SubP) then
            Result := NextCode
          else
            Result := SubCode;
        end;
      nkDefine:
        begin
          Result := FStateList[NFACode.ExtendTo];
        end;
      nkFail:
        begin
          Result := nil;
        end;
      nkPrune:
        begin
          Stack.Clear;
          if NFACode.GroupIndex <> -1 then
            FRegExp.FLastRegMarkIndex := NFACode.GroupIndex;
          Result := FStateList[NFACode.TransitTo];
        end;
      nkSkip:
        begin
          if NFACode.GroupIndex = -1 then
          begin
            Stack.Clear;
          end
          else
          begin
            FSkipIndex := NFACode.GroupIndex;
            FRegExp.FLastRegMarkIndex := FSkipIndex;
          end;

          FSkipP := AStr;
          Result := FStateList[NFACode.TransitTo];
          FHasSkip := True;
        end;
      nkMark:
        begin
          if NFACode.GroupIndex = FSkipIndex then
          begin
            Stack.Clear;
            FSkipP := AStr;
          end
          else
          begin
            if NFACode.GroupIndex <> -1 then
              FRegExp.FLastRegMarkIndex := NFACode.GroupIndex;
          end;

          Result := FStateList[NFACode.TransitTo];
        end;
      nkThen:
        begin
          if NFACode.ExtendTo <> -1 then
          begin
            BranchCode := FStateList[NFACode.ExtendTo];
            Stack.Remove(BranchCode);
          end
          else
            Stack.Clear;

          if NFACode.GroupIndex <> -1 then
            FRegExp.FLastRegMarkIndex := NFACode.GroupIndex;

          Result := FStateList[NFACode.TransitTo];
        end;
      nkCommit:
        begin
          Stack.Clear;
          if NFACode.GroupIndex <> -1 then
            FRegExp.FLastRegMarkIndex := NFACode.GroupIndex;

          FSkipP := FRegExp.FMatchEndP;
          Result := FStateList[NFACode.TransitTo];
        end;
      nkAccept:
        begin
          //内包するキャプチャを設定
          for I := 0 to FGroups.Count - 1 do
          begin
            if (FGroups[I].EndP = nil) and
                (FGroups[I].StartPBuf <> nil) then
              FGroups[I].EndP := AStr;
          end;

          if FRegExp.FSubStack.Count > 0 then
          begin
            Stack.RemoveGoSub(FRegExp.FSubStack.Index);
            Result := FRegExp.FSubStack[FRegExp.FSubStack.Count - 1].EndCode;
          end
          else
          begin
            Stack.Clear;
            Result := FStateList[NFACode.TransitTo];
          end;
        end;
    end;

{$IFDEF SKREGEXP_DEBUG}
  finally
    if FRegExp.FShowMatchProcess then
    begin
      if (Result = nil) and (not IsLoopMatched) then
      begin
        if CurrentNFA.Kind <> nkEnd then
          FRegExp.FMatchProcess.Add('...fail')
        else
          FRegExp.FMatchProcess.Add('Match Success!');
      end;
    end;
  end;
{$ENDIF}
end;

{$IFDEF SKREGEXP_DEBUG}

procedure TREMatchEngine.MatchProcessAdd(NFACode: TRENFAState; AStr: PWideChar;
  Level: Integer);
var
  S: REString;
  StartMatch, CurrentPosition: Integer;
begin
  CurrentPosition := AStr - FRegExp.FTextTopP + 1;
  StartMatch := FGroups[0].StartP - FRegExp.FTextTopP + 1;

  S := FRegExp.InputString;
  if Length(S) > 40 then
    Exit;

  Insert('>##TAB##', S, CurrentPosition);
  Insert('<', S, StartMatch);

  S := FRegExp.EncodeEscape(S);

  S := StringReplace(S, '##TAB##', #0009, [rfReplaceAll]);

  FRegExp.FMatchProcess.Add(Format('%3d %s | %2d: %s[NestLevel:%d]',
    [CurrentPosition - 1, S, NFACode.Index, NFACode.GetString, Level + 1]));
end;
{$ENDIF SKREGEXP_DEBUG}

procedure TREMatchEngine.Optimize;
begin
  FLeadCharMode := lcmNone;
  FLeadCode.Clear;
  FLeadStrings := nil;
  FACSearch.Clear;
  FLeadMap.Clear;
  FLeadCharOffset.Min := 0;
  FLeadCharOffset.Max := 0;
  FAnchorOffset.Min := 0;
  FAnchorOffset.Max := 0;

{$IFDEF NotOptimizeRuntime}
{$ELSE NotOptimizeRuntime}
  OptimizeLoop;

  SetupLeadStrings;

  if (FLeadCharMode in [lcmNone, lcmLeadMap, lcmHasLead])  then
    SetupPreMatchStrings;

{$ENDIF NotOptimizeRuntime}
end;

procedure TREMatchEngine.OptimizeLoop;

  function HasOverlap(NFACode, NextCode: TRENFAState): Boolean;
  begin
    Result := True;

    while NFACode.Kind <> nkLoopEnd do
    begin
      if NFACode.Kind = nkChar then
      begin
        if NFACode.Code.IsOverlap(NextCode.Code) then
          Exit;

        NFACode := FStateList[NFACode.TransitTo];
      end
      else
        Exit;
    end;
    Result := False;
  end;

var
  I: Integer;
  NFACode, SubCode, NextCode: TRENFAState;
begin
  for I := 0 to FRegExp.FLoopState.Count - 1 do
  begin
    NFACode := FRegExp.FLoopState[I].NFACode;
    case NFACode.Kind of
      nkStar, nkPlus, nkBound:
        begin
          // 量指定子の次の部分式が、量指定子の対象となる文字を含むか？
          // 含まなければバックトラックの必要はないので強欲に変更。
          NextCode := FStateList[NFACode.TransitTo];

          while NextCode.Kind in [nkGroupEnd, nkGroupBegin] do
            NextCode := FStateList[NextCode.TransitTo];

          if (NextCode.Kind = nkEnd) or
              ((NFACode.BranchIndex = NextCode.BranchIndex) and
              ((NextCode.Code <> nil) and not NextCode.Code.IsVariable) and
              (NextCode.Code <> nil) and
              not NFACode.Code.IsOverlap(NextCode.Code)) then
            NFACode.MatchKind := lkPossessive
        end;
    else
      begin
        if NFACode.FMatchKind = lkReluctant then
        begin
          SubCode := FStateList[NFACode.TransitTo];
          NextCode := FStateList[SubCode.TransitTo];

          if (NextCode.Next = nil) and
              ((NextCode.Code is TRELiteralCode) or
              ((NextCode.Code is TRECharClassCode) and
              (NextCode.Code as TRECharClassCode).SimpleClass)) then
            NFACode.MatchKind := lkSimpleReluctant;
        end;
      end;
    end;
  end;
end;

function TREMatchEngine.IsLeadAllMatch(AStr: PWideChar): Boolean;
begin
  Result := True;
end;

function TREMatchEngine.IsLeadCode(AStr: PWideChar): Boolean;
begin
  Result := FLeadCode.IsEqual(AStr);
end;

function TREMatchEngine.IsLeadMap(AStr: PWideChar): Boolean;
begin
  Result := FLeadMap.IsExists(AStr);
end;

procedure TREMatchEngine.SetupPreMatchStrings;
var
  I: Integer;
  SubList: TREOptimizeDataList;
  Code: TREOptimizeData;
  IsBranch: Boolean;
begin
  IsBranch := False;

  FOptimizeData.GetTailCode(Code);

  if Code <> nil then
  begin
    if (Code.Code is TRELiteralCode) then
    begin
      FAnchorStrings := Code.Code;
      FAnchorOffset := Code.FOffset;

      if (FAnchorOffset.Min >= 0) and (FAnchorOffset.Max >= 0) then
      begin
        FLeadCharMode := lcmFixedAnchor;
        Exit;
      end
      else
      begin
        SetupLeadMatch(lcmVariableAnchor);
        Exit;
      end;
    end;
  end;

  if (FLeadCharMode in [lcmNone, lcmLeadMap, lcmHasLead]) then
  begin
    SubList := TREOptimizeDataList.Create;
    try
      FOptimizeData.GetAnchorCode(FRegExp.FBranchCount, SubList);

      if (SubList.Count = 1) then
      begin
        if (SubList[0].Code is TRELiteralCode) then
        begin
          FAnchorStrings := SubList[0].Code;
          FAnchorOffset := SubList[0].Offset;

          if (FAnchorOffset.Min >= 0) and (FAnchorOffset.Max >= 0) then
            FLeadCharMode := lcmFixedAnchor
          else
            SetupLeadMatch(lcmVariableAnchor);
        end;
      end
      else
      begin
        for I := 0 to SubList.Count - 1 do
        begin
          if (SubList[I].Code is TRELiteralCode) then
          begin
            FACSearch.Add(SubList[I].Code);
          end
          else
          begin
            FACSearch.Clear;
            Exit;
          end;

          if (not IsBranch) and (SubList[I].BranchLevel <> 0) then
            IsBranch := True;

          if SubList[I].Offset.Min <> -1 then
          begin
            if FAnchorOffset.Min > 0 then
              FAnchorOffset.Min := Min(SubList[I].Offset.Min, FAnchorOffset.Min)
            else
              FAnchorOffset.Min := SubList[I].Offset.Min;
          end
          else
            FAnchorOffset.Min := -1;

          if SubList[I].Offset.Max <> -1 then
            FAnchorOffset.Max := Max(SubList[I].Offset.Min, FAnchorOffset.Max)
          else
            FAnchorOffset.Max := -1;
        end;

        if IsBranch then
        begin
          if (FAnchorOffset.Min >= 0) and (FAnchorOffset.Max >= 0) then
            FLeadCharMode := lcmFixedBranch
          else
            SetupLeadMatch(lcmVariableBranch);
        end;
      end;
    finally
      SubList.Free;
    end;
  end;
end;


procedure TREMatchEngine.SetupLeadMatch(ALeadCharMode: TRELeadCharMode);
begin
  case FLeadCharMode of
    lcmHasLead: 
      IsLeadMatch := IsLeadCode;
    lcmLeadMap: 
      IsLeadMatch := IsLeadMap;
    lcmNone:
      IsLeadMatch := IsLeadAllMatch;
  end;
  FLeadCharMode := ALeadCharMode;
end;

procedure TREMatchEngine.SetupLeadStrings;
var
  I: Integer;
  NFACode, NextCode: TRENFAState;
  IsLiteral, NoMap: Boolean;
  LineHeadCount, TextHeadCount: Integer;
//  LiteralCount: Integer;
begin
  IsLiteral := False;

  FOptimizeData.GetLeadCode(FRegExp.FBranchCount, FLeadCode);

  if FLeadCode.Count = 1 then
  begin
    FLeadCharOffset := FLeadCode[0].Offset;

    if (FLeadCode[0].Code is TRELiteralCode) then
    begin
      FLeadStrings := FLeadCode[0].Code;
      IsLiteral := True;
    end
    else if (FLeadCode[0].Code is TRELineHeadCode) then
    begin
      if not(roMultiLine in (FLeadCode[0].Code as TRELineHeadCode).FOptions) then
        FLeadCharMode := lcmTextTop
      else
        FLeadCharMode := lcmLineTop;
      Exit;
    end
    else if (FLeadCode[0].Code is TRETextHeadCode) then
    begin
      FLeadCharMode := lcmTextTop;
      Exit;
    end;

    if IsLiteral then
    begin
      NFACode := FStateList[FRegExp.FEntryState];
      NextCode := FStateList[NFACode.TransitTo];

      if (NFACode.Kind = nkChar) and (NFACode.Next = nil) and
        (NextCode.Kind = nkEnd) then
        FLeadCharMode := lcmSimple
      else
        FLeadCharMode := lcmFirstLiteral;
    end;
  end
  else if FLeadCode.Count > 1 then
  begin
    LineHeadCount := 0;
    TextHeadCount := 0;
//    LiteralCount := 0;

    for I := 0 to FLeadCode.Count - 1 do
    begin
      if FLeadCode[I].Code is TRELineHeadCode then
      begin
        if not (roMultiLine in (FLeadCode[I].Code as TRELineHeadCode).FOptions) then
          Inc(TextHeadCount)
        else
          Inc(LineHeadCount)
      end
      else if FLeadCode[I].Code is TRETextHeadCode then
      begin
        Inc(TextHeadCount);
      end
//      else if (FLeadCode[I].Code is TRECharCode) or
//          (FLeadCode[I].Code is TRELiteralCode) then
//      begin
//        FACSearch.Add(FLeadCode[I].Code);
//        if I = 0 then
//        begin
//          FAnchorOffset.Min := FLeadCode[I].Offset.Min;
//          FAnchorOffset.Max := FLeadCode[I].Offset.Max;
//        end
//        else
//        begin
//          FAnchorOffset.Min := Min(FAnchorOffset.Min, FLeadCode[I].Offset.Min);
//          FAnchorOffset.Max := Min(FAnchorOffset.Max, FLeadCode[I].Offset.Max);
//        end;
//
//        Inc(LiteralCount);
//      end
      else
      begin
        FLeadCharMode := lcmNone;
        FACSearch.Clear;
        Break;
      end;
    end;

//    if LiteralCount = FLeadCode.Count then
//    begin
//      NFACode := FStateList[FRegExp.FEntryState];
//
//      while NFACode <> nil do
//      begin
//        NextCode := FStateList[NFACode.TransitTo];
//        if NextCode.Kind <> nkEnd then
//        begin
//          FLeadCharMode := lcmFirstBranch;
//          Exit;
//        end;
//        NFACode := NFACode.Next;
//      end;
//      FLeadCharMode := lcmSimpleBranch;
//    end
//    else
    if LineHeadCount = FLeadCode.Count then
      FLeadCharMode := lcmLineTop
    else if TextHeadCount = FLeadCode.Count then
      FLeadCharMode := lcmTextTop;
  end;

  if FLeadCharMode = lcmNone then
  begin
    NoMap := False;
    CreateLeadMap(FStateList[FRegExp.FEntryState], NoMap);
    if NoMap then
    begin
      FLeadMap.Clear;
      if (FLeadCharMode = lcmNone) and (FLeadCode.Count > 0) then
        FLeadCharMode := lcmHasLead
      else
        FLeadCharMode := lcmNone;
    end
    else
      FLeadCharMode := lcmLeadMap;
  end;
end;

function TREMatchEngine.MatchCore(var NFACode, EndCode: TRENFAState;
  Stack: TREBackTrackStack; var AStr: PWideChar): Boolean;
var
  Index: Integer;
  SaveP: PWideChar;
begin
  Result := False;
  SaveP := AStr;
  Index := Stack.Index;

  while NFACode <> nil do
  begin
    NFACode := MatchPrim(NFACode, Stack, AStr);

    if (NFACode = nil) then
    begin
      if FGroups[0].EndP <> nil then
      begin
        Result := True;
        Exit;
      end
      else if Stack.Index > Index then
        Stack.Pop(NFACode, AStr);
    end
    else
    begin
      if (NFACode = EndCode) or (NFACode.Kind in [nkEnd, nkMatchEnd, nkLoopExit]) then
      begin
        Result := True;
        Exit;
      end;
    end;
  end;

  if not Result then
    AStr := SaveP;
end;

{ TSkRegExp }

procedure TSkRegExp.ClearBinCodeList;
var
  I: Integer;
begin
  for I := 0 to FBinCodeList.Count - 1 do
    if FBinCodeList[I] <> nil then
      TRECode(FBinCodeList[I]).Free;
  FBinCodeList.Clear;
end;

procedure TSkRegExp.ClearCodeList;
var
  I: Integer;
begin
  for I := 0 to FCodeList.Count - 1 do
    if FCodeList[I] <> nil then
      TRECode(FCodeList[I]).Free;
  FCodeList.Clear;
end;

{$IFDEF CHECK_MATCH_EXPLOSION}

procedure TSkRegExp.ClearMatchExplosionState;
var
  I: Integer;
  S, D: PREMatchExplosionStateRec;
begin
  for I := 0 to System.Length(FMatchExplosionState) - 1 do
  begin
    S := FMatchExplosionState[I];
    while S <> nil do
    begin
      D := S.Next;
      Dispose(S);
      S := D;
    end;
    FMatchExplosionState[I] := nil;
  end;
end;
{$ENDIF}

procedure TSkRegExp.ClearStateList;
var
  I: Integer;
  Code, Next: TRENFAState;
begin
  if FStateList <> nil then
  begin
    for I := 0 to FStateList.Count - 1 do
    begin
      Code := FStateList[I];
      while Code <> nil do
      begin
        Next := Code.Next;
        Code.Free;
        Code := Next;
      end;
    end;
    FStateList.Clear;
  end;
end;

procedure TSkRegExp.Compile;
var
  Parser: TREParser;
  NFA: TRENFA;
begin
  if not FCompiled then
  begin
    ClearCodeList;
    ClearBinCodeList;
    FGroups.Clear;

    Parser := TREParser.Create(Self, FExpression);
    try
      Parser.Parse;
      NFA := TRENFA.Create(Self);
      try
        NFA.Compile(Parser);
        FMatchEngine.Optimize;
      finally
        NFA.Free;
      end;
    finally
      Parser.Free;
    end;
{$IFNDEF SKREGEXP_DEBUG}
    ClearBinCodeList;
{$ENDIF}
    FCompiled := True;
  end;
end;

constructor TSkRegExp.Create;
begin
  inherited Create;
  FGroups := TGroupCollection.Create(Self);
  FCodeList := TList.Create;
  FBinCodeList := TList.Create;
  FStateList := TList.Create;
  FVerbNames := TREStringList.Create;
  FGroupStack := TREGroupStack.Create(Self);
  FSubStack := TREGoSubStack.Create(Self);
  FOptimizeData := TREOptimizeDataCollection.Create;
  FLoopState := TRELoopStateList.Create;

  FOptions := SkRegExpDefaultOptions;

  if SkRegExpDefaultLineBreakind <> lbAnyCRLF then
    SetLineBreakKind(SkRegExpDefaultLineBreakind)
  else
    IsLineBreak := IsAnyCRLF;

  FMatchEngine := TREMatchEngine.Create(Self);

  FSuccess := False;
  FModified := False;
  FCompiled := False;

  FMatchOffset := 1;
  FMatchLength := 0;
  FStartMatch := 0;
  FLastRegMarkIndex := -1;
  FLastRegErrorIndex := -1;

{$IFDEF SKREGEXP_DEBUG}
  FMatchProcess := TREStringList.Create;
{$ENDIF}
end;

{$IFDEF SKREGEXP_DEBUG}

procedure TSkRegExp.DebugOutput(ADest: TStrings);
var
  I: Integer;
  S: REString;
begin
  ADest.Add(Format('Compiling Regex: %s', [FExpression]));
  ADest.Add('Final program:');
  ADest.Add('--------------------------');
  DumpNFA(ADest);

  ADest.Add('Dump anchor');
  ADest.Add('--------------------------');
  ADest.Add(DumpLeadCode);

  ADest.Add('Match process');
  ADest.Add('--------------------------');
  ADest.Add(DumpMatchProcess);

  ADest.Add('Group match history');
  ADest.Add('---------------------------');
  ADest.Add(FGroupStack.GetDebugStr);

  ADest.Add('Match result');
  ADest.Add('---------------------------');
  if Groups[0].Success then
    ADest.Add(Format('Match str: %s Pos: %d Len: %d at "%s"',
      [EncodeEscape(Groups[0].Strings), Groups[0].Index, Groups[0].Length, Expression]))
  else
    ADest.Add(Format('Match str: (NoMatch) at "%s"', [Expression]));

  ADest.Add('');
  ADest.Add('Groups result');
  ADest.Add('--------------------------');
  if Groups[0].Success then
    for I := 0 to GroupCount do
    begin
      if Groups[I].Success then
      begin
        S := EncodeEscape(Groups[I].Strings);
        if Groups[I].GroupName <> '' then
          ADest.Add(Format('%2.d<%s>: %s    [Pos: %d, Len: %d]',
            [I, Groups[I].GroupName, S, Groups[I].Index, Groups[I].Length]))
        else
          ADest.Add(Format('%2.d: %s    [Pos: %d, Len: %d]',
            [I, S, Groups[I].Index, Groups[I].Length]));
      end
      else
        ADest.Add(Format('%2.d: (NoMatch)', [I]));
    end;
end;
procedure TSkRegExp.SKREGEXP_DEBUGPrint(const S: REString);
begin
  FMatchProcess.Add(S);
end;

{$ENDIF SKREGEXP_DEBUG}

class function TSkRegExp.DecodeEscape(const S: REString): REString;
var
  StartP: PWideChar;
  LGroupCount: Integer;

  function GetErrorStopedString(const ErrMes: REString; P: PWideChar): REString;
  var
    S: REString;
  begin
    Inc(P);
    SetString(S, StartP, P - StartP);
    S := S + ' <-- ';
    Result := Format(ErrMes, [S]);
  end;

  function GetOctalDigit(var P: PWideChar): Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 1 to 3 do
    begin
      case P^ of
        '0' .. '7':
          Result := (Result shl 3) + Ord(P^) - Ord('0');
      else
        Break;
      end;
      Inc(P);
    end;
  end;

var
  P, SaveP: PWideChar;
  I, J, N: Integer;
  T: REString;
begin
  Result := '';
  if S = '' then
    Exit;

  LGroupCount := TRELex.CountGroup(S);

  SetLength(Result, System.Length(S));
  J := 1;

  P := PWideChar(S);
  StartP := P;

  while P^ <> #0 do
  begin
    if P^ = '\' then
    begin
      Inc(P);
      case P^ of
        '1'..'9':
          begin
            SaveP := P;
            N := 0;
            while P^ <> #0 do
            begin
              case P^ of
                '0'..'9':
                  N := N * 10 + (Integer(P^) - Integer('0'));
                else
                  Break;
              end;
              Inc(P);
            end;

            if N <= LGroupCount then
            begin
              Result[J] := '\';
              Inc(J);
              while SaveP < P do
              begin
                Result[J] := SaveP^;
                Inc(SaveP);
                Inc(J);
              end;
            end
            else
            begin
              if N > $10FFFF then
                raise ESkRegExpCompile.Create(
                  GetErrorStopedString(sCodePointRangeOver, P));

              P := SaveP;
              N := GetOctalDigit(P);
              if N > $FFFF then
              begin
                T := UCharToString(N);
                Result[J] := T[1];
                Inc(J);
                Result[J] := T[2];
              end
              else
                Result[J] := WideChar(N);
              Inc(J);
            end;
            Continue;
          end;
        'c':
          begin
            Inc(P);
            if ((P^ >= '@') and (P^ <= '_')) or
              ((P^ >= 'a') and (P^ <= 'z')) then
            begin
              if P^ = '\' then
              begin
                Inc(P);
                if P^ <> '\' then
                  raise ESkRegExpCompile.Create(
                    GetErrorStopedString(sInvalidEscapeCharacterSyntax, P));
              end;

              N := Ord(P^);
              if (P^ >= 'a') and (P^ <= 'z') then
                Dec(N, $20);
              N := N xor $40;
              Result[J] := WideChar(N);
            end;
          end;
        'x':
          begin
            N := 0;
            Inc(P);

            if P^ = '{' then
            begin
              Inc(P);

              for I := 1 to 6 do
              begin
                case P^ of
                  '0' .. '9':
                    N := (N shl 4) + Ord(P^) - Ord('0');
                  'A' .. 'F':
                    N := (N shl 4) + Ord(P^) - Ord('7');
                  'a' .. 'f':
                    N := (N shl 4) + Ord(P^) - Ord('W');
                  '}':
                    Break;
                else
                  raise ESkRegExpCompile.Create(
                    GetErrorStopedString(sHexDigitIsRequired, P));
                end;
                Inc(P);
              end;

              if N > $10FFFF then
                raise ESkRegExpCompile.Create(
                  GetErrorStopedString(sCodePointRangeOver, P));

              if P^ <> '}' then
                raise ESkRegExpCompile.Create(
                  GetErrorStopedString(sUnmatchedCurlyBracket, P));

              if N > $FFFF then
              begin
                T := UCharToString(N);
                Result[J] := T[1];
                Inc(J);
                Result[J] := T[2];
              end
              else
                Result[J] := WideChar(N);
            end
            else
            begin
              for I := 1 to 2 do
              begin
                case P^ of
                  '0' .. '9':
                    N := (N shl 4) + Ord(P^) - Ord('0');
                  'A' .. 'F':
                    N := (N shl 4) + Ord(P^) - Ord('7');
                  'a' .. 'f':
                    N := (N shl 4) + Ord(P^) - Ord('W');
                else
                  raise ESkRegExpCompile.Create(
                    GetErrorStopedString(sHexDigitIsRequired, P));
                end;
                Inc(P);
              end;

              Result[J] := WideChar(N);
              Inc(J);

              Continue;
            end;
          end;
        'o', '0':
          begin
            N := 0;
            if P^ = 'o' then
            begin
              Inc(P);
              if P^ <> '{' then
                raise ESkRegExpCompile.Create(
                  GetErrorStopedString(sMissingLeftBraceOnESCo, P ));
              Inc(P);

              for I := 1 to 7 do
              begin
                case P^ of
                  '0' .. '7':
                    N := (N shl 3) + (Integer(P^) - Integer('0'));
                  '}':
                    Break;
                else
                  raise ESkRegExpCompile.Create(
                    GetErrorStopedString(sOctalDigitIsRequired, P));
                end;
                Inc(P);
              end;

              if P^ <> '}' then
                raise ESkRegExpCompile.Create(
                  GetErrorStopedString(sMissingRightBraceOnEsco, P ));
              Inc(P);
            end
            else
            begin
              for I := 1 to 3 do
              begin
                case P^ of
                  '0' .. '7':
                    N := (N shl 3) + (Integer(P^) - Integer('0'));
                else
                  Break;
                end;
                Inc(P);
              end;
            end;

            if N > $10FFFF then
              raise ESkRegExpCompile.Create(
                GetErrorStopedString(sCodePointRangeOver, P));

            if N > $FFFF then
            begin
              T := UCharToString(N);
              Result[J] := T[1];
              Inc(J);
              Result[J] := T[2];
            end
            else
              Result[J] := WideChar(N);

            Inc(J);
            Continue;
          end;
        't':
          Result[J] := #0009;
        'n':
          Result[J] := #$000A;
        'r':
          Result[J] := #$000D;
        'f':
          Result[J] := #$000C;
        'a':
          Result[J] := #0007;
        'e':
          Result[J] := #$001B;
      else
        Result[J] := P^;
      end;
    end
    else
      Result[J] := P^;

    Inc(P);
    Inc(J);
  end;
  SetLength(Result, J - 1);
end;

destructor TSkRegExp.Destroy;
begin
{$IFDEF CHECK_MATCH_EXPLOSION}
  ClearMatchExplosionState;
{$ENDIF}
  ClearStateList;
  ClearBinCodeList;
  ClearCodeList;

  FLoopState.Free;
  FOptimizeData.Free;
  FGroupStack.Free;
  FSubStack.Free;
  FVerbNames.Free;
  FMatchEngine.Free;
  FStateList.Free;
  FBinCodeList.Free;
  FCodeList.Free;
  FGroups.Free;
{$IFDEF SKREGEXP_DEBUG}
  FMatchProcess.Free;
{$ENDIF}
  inherited;
end;

procedure TSkRegExp.DoReplaceFunc(Sender: TObject; var ReplaceWith: REString);
begin
  if Assigned(FReplaceFunc) then
    ReplaceWith := FReplaceFunc(Self);
end;

{$IFDEF SKREGEXP_DEBUG}

function TSkRegExp.DumpLeadCode: REString;

  function LeadCharModeToStr(LMode: TRELeadCharMode): REString;
  begin
    case LMode of
      lcmFirstLiteral:
        Result := 'FirstLiteral';
      lcmSimple:
        Result := 'Simple';
      lcmTextTop:
        Result := 'TextTop';
      lcmLineTop:
        Result := 'LineTop';
      lcmHasLead:
        Result := 'HasLead';
      lcmLeadMap:
        Result := 'Map';
      lcmFixedAnchor:
        Result := 'FixedAnchor';
      lcmVariableAnchor:
        Result := 'VariableAnchor';
      lcmFixedBranch:
        Result := 'FixedBranch';
      lcmVariableBranch:
        Result := 'VariableBranch';
      lcmFirstBranch:
        Result := 'FirstBranch';
      lcmSimpleBranch:
        Result := 'SimpleBranch';
      else
        Result := 'None';
    end;
  end;

var
  SL: TStrings;
begin
  SL := TStringList.Create;
  try
    FOptimizeData.DebugOutput(SL);

    SL.Add('');

    if FMinMatchLength > 0 then
      SL.Add(Format('minlen: %d', [FMinMatchLength]));
    if FMaxMatchLength > 0 then
      SL.Add(Format('maxlen: %d', [FMaxMatchLength]));

    SL.Add(Format('offset at (%d, %d)',
      [FMatchEngine.FAnchorOffset.Min, FMatchEngine.FAnchorOffset.Max]));

    if (FMatchEngine.FLeadStrings <> nil) and
        (FMatchEngine.FLeadStrings.Search.FindText <> '') then
      SL.Add(sFmtDumpLeadCodeExist + FMatchEngine.FLeadStrings.Search.FindText);

    if (FMatchEngine.FAnchorStrings <> nil) and
        (FMatchEngine.FAnchorStrings.Search.FindText <> '') then
      SL.Add(sFmtDumpAnchor + FMatchEngine.FAnchorStrings.Search.FindText);

    SL.Add(Format('LeadMap: %s', [FMatchEngine.FLeadMap.GetDebugStr]));

    SL.Add('');
    SL.Add(Format('LeadCharMode: %s', [LeadCharModeToStr(FMatchEngine.FLeadCharMode)]));

    Result := SL.Text;
  finally
    SL.Free;
  end;
end;

function TSkRegExp.DumpMatchProcess: REString;
begin
  if FShowMatchProcess then
    Result := FMatchProcess.Text;
end;

procedure TSkRegExp.DumpNFA(ADest: TStrings);
var
  I: Integer;
  Code: TRENFAState;
  Str: REString;
begin
  ADest.BeginUpDate;
  for I := 0 to FStateList.Count - 1 do
  begin
    Code := FStateList[I];
    if I = FEntryState then
      Str := Format(sFmtDumpNFA_Start, [I])
    else if I = FExitState then
      Str := Format(sFmtDumpNFA_End, [I])
    else
      Str := Format(sFmtDumpNFA_Status, [I]);
    while Code <> nil do
    begin
      Str := Str + Code.GetString;

      Code := Code.Next;
    end;
    ADest.Add(Str);
  end;
  ADest.Add('');
  ADest.EndUpDate;
end;

//procedure TSkRegExp.DumpParse(TreeView: TTreeView);
//
//  function Add(Node: TTreeNode; const S: REString): TTreeNode;
//  begin
//    Result := TreeView.Items.AddChild(Node, Format('%s', [S]));
//  end;
//
//  procedure DumpParseSub(Code: TRECode; Node: TTreeNode);
//  var
//    ANode: TTreeNode;
//  begin
//    if Code is TREBinCode then
//    begin
//      with Code as TREBinCode do
//      begin
//        case Op of
//          opUnion:
//            ANode := Add(Node, sBinCode_Union);
//          opConcat:
//            ANode := Add(Node, sBinCode_Concat);
//          opEmply:
//            ANode := Add(Node, sBinCode_Emply);
//          opLoop:
//            ANode := Add(Node, sBinCode_Loop);
//          opPlus:
//            ANode := Add(Node, sBinCode_Plus);
//          opStar:
//            ANode := Add(Node, sBinCode_Star);
//          opQuest:
//            ANode := Add(Node, sBinCode_Quest);
//          opBound:
//            ANode := Add(Node, sBinCode_Bound);
//          opLHead:
//            ANode := Add(Node, sBinCode_LHead);
//          opLTail:
//            ANode := Add(Node, sBinCode_LTail);
//          opGroup:
//            ANode := Add(Node, sBinCode_Group);
//          opNoBackTrack:
//            ANode := Add(Node, sBinCode_Suspend);
//          opKeepPattern:
//            ANode := Add(Node, sBinCode_KeepPattern);
//          opFail:
//            ANode := Add(Node, sBinCode_Fail);
//          opPrune:
//            ANode := Add(Node, sBinCode_Prune);
//          opSkip:
//            ANode := Add(Node, sBinCode_Skip);
//          opMark:
//            ANode := Add(Node, sBinCode_Mark);
//          opThen:
//            ANode := Add(Node, sBinCode_Then);
//          opCommint:
//            ANode := Add(Node, sBinCode_Commit);
//          opAccept:
//            ANode := Add(Node, sBinCode_Accept);
//          opAheadMatch:
//            ANode := Add(Node, sBinCode_AheadMatch);
//          opBehindMatch:
//            ANode := Add(Node, sBinCode_BehindMatch);
//          opAheadNoMatch:
//            ANode := Add(Node, sBinCode_AheadNoMatch);
//          opBehindNoMatch:
//            ANode := Add(Node, sBinCode_BehindNoMatch);
//          opGoSub:
//            ANode := Add(Node, sBinCode_GroupCall);
//          opIfMatch:
//            ANode := Add(Node, sBinCode_IfMatch);
//          opIfThen:
//            ANode := Add(Node, sBinCode_IfThen);
//          opDefine:
//            ANode := Add(Node, sBinCode_Define);
//        else
//          raise ESkRegExp.Create(sBinCode_Raise);
//        end;
//        if Left <> nil then
//          DumpParseSub(Left, ANode);
//        if Right <> nil then
//          DumpParseSub(Right, ANode);
//      end;
//    end
//    else
//      TreeView.Items.AddChild(Node, (Code as TRECode).GetDebugStr);
//  end;
//
//begin
//  TreeView.Items.Clear;
//  DumpParseSub(FCode, nil);
//  TreeView.FullExpand;
//end;

{$ENDIF}

function TSkRegExp.Exec(const AInputStr: REString): Boolean;
var
  P: PWideChar;
begin
  FMatchOffset := 1;
  FMatchLength := 0;

  if not FCompiled then
    Compile;

  SetInputString(AInputStr);

  FModified := False;

  FMatchTopP := FTextTopP;
  FMatchEndP := FTextEndP;

  FMatchStartP := FMatchTopP;

  P := FTextTopP;

  Result := MatchCore(P);
end;

function TSkRegExp.ExecNext: Boolean;
var
  P: PWideChar;
begin
  Result := False;

  if not FCompiled then
    Error(sExecFuncNotCall);

  if not FSuccess then
    Exit;

  if FGlobalEndP - FGlobalStartP > 0 then
    P := FGlobalEndP
  else
    P := FGlobalEndP + 1;

  FMatchStartP := P;

  FMatchOffset := P - FTextTopP + 1;

  Result := MatchCore(P);
end;

function TSkRegExp.ExecPos(AOffset, AMaxLength: Integer): Boolean;
var
  P: PWideChar;
begin
  Result := False;
  FMatchOffset := AOffset;
  FMatchLength := AMaxLength;

  if (AOffset < 1) then
    Exit;

  if FMatchTopP = nil then
    Exit;

  if not FCompiled then
  begin
    Compile;
{$IFDEF CHECK_MATCH_EXPLOSION}
    if not FModified then
      ClearMatchExplosionState
    else
{$ENDIF CHECK_MATCH_EXPLOSION}
      FModified := False;
  end;

  if AOffset > 1 then
    P := FTextTopP + AOffset - 1
  else
    P := FTextTopP;

  FMatchOffset := P - FTextTopP + 1;

  if AMaxLength > 0 then
  begin
    if FMatchOffset + AMaxLength > System.Length(FInputString) then
      FMatchEndP := FTextEndP
    else
      FMatchEndP := P + AMaxLength;

    // ALength が指定されたときは、指定範囲内が文字列全体だと扱う。
    FMatchTopP := P;
    FMatchLength := FMatchEndP - FMatchTopP;
  end
  else
  begin
    FMatchEndP := FTextEndP;
    FMatchTopP := FTextTopP;
  end;

  FMatchStartP := FMatchTopP;

  Result := MatchCore(P);
end;

{$IFDEF JapaneseExt}

function TSkRegExp.GetIgnoreZenHan: Boolean;
begin
  Result := (roIgnoreWidth in FOptions) and (roIgnoreKana in FOptions);
end;
{$ENDIF JapaneseExt}

function TSkRegExp.GetIndex: Integer;
begin
  Result := FGroups[0].Index;
end;

function TSkRegExp.GetIndexFromGroupName(Name: REString): Integer;
var
  LIntArray: TIntDynArray;
begin
  LIntArray := FGroups.EnumIndexOfName(Name);

  for Result in LIntArray do
    if FGroups[Result].Success then
      Exit;
  if System.Length(LIntArray) > 0 then
    Result := LIntArray[0]
  else
    Result := -1;
end;

function TSkRegExp.GetLength: Integer;
begin
  Result := FGroups[0].Length;
end;

function TSkRegExp.GetOptions(const Index: Integer): Boolean;
var
  LOption: TREOption;
begin
  case Index of
    0:
      LOption := roIgnoreCase;
    1:
      LOption := roMultiLine;
    2:
      LOption := roNamedGroupOnly;
    3:
      LOption := roSingleLine;
    4:
      LOption := roExtended;
    5:
      LOption := roIgnoreWidth;
    7:
      LOption := roAutoCallout;
  else
    LOption := roIgnoreKana;
  end;
  Result := LOption in FOptions;
end;

function TSkRegExp.GetRegError: REString;
begin
  if FLastRegErrorIndex <> -1 then
    Result := FVerbNames[FLastRegErrorIndex]
  else
    Result := '';
end;

function TSkRegExp.GetRegMark: REString;
begin
  if FLastRegMarkIndex <> -1 then
    Result := FVerbNames[FLastRegMarkIndex]
  else
    Result := '';
end;

function TSkRegExp.GetStrings: REString;
begin
  Result := FGroups[0].Strings;
end;

function TSkRegExp.GetDefineCharClassLegacy: Boolean;
begin
  Result := roDefinedCharClassLegacy in FOptions;
end;

function TSkRegExp.GetGroupCount: Integer;
begin
  Result := FGroups.Count - 1;
end;

function TSkRegExp.GetGroupNameFromIndex(Index: Integer): REString;
begin
  Result := FGroups[Index].GroupName;
end;

function TSkRegExp.GetVersion: REString;
begin
  Result := CONST_VERSION;
end;

{$IFDEF CHECK_MATCH_EXPLOSION}
function TSkRegExp.IsAlreadyTried(NFACode: TRENFAState;
  const AStr: PWideChar): Boolean;
var
  Index: Integer;
  S, D, N: PREMatchExplosionStateRec;
begin
  Index := AStr - FTextTopP;

  if FMatchExplosionState[Index] <> nil then
  begin
    S := FMatchExplosionState[Index];
    D := S;
    while S <> nil do
    begin
      if (S.NFACode = NFACode) then
      begin
        Result := True;
        Exit;
      end;

      D := S;
      S := S.Next;
    end;

    New(N);
    N.NFACode := NFACode;
    N.Next := nil;
    D.Next := N;
  end
  else
  begin
    New(N);
    N.NFACode := NFACode;
    N.Next := nil;

    FMatchExplosionState[Index] := N;
  end;
  Result := False;
end;
{$ENDIF CHECK_MATCH_EXPLOSION}

function TSkRegExp.IsAnyCRLF(P: PWideChar): Integer;
begin
  Result := 0;
  if P^ = #$000A then
    Result := 1
  else if P^ = #$000D then
  begin
    if (P + 1)^ = #$000A then
      Result := 2
    else
      Result := 1;
  end;
end;

function TSkRegExp.IsAnyEOL(P: PWideChar): Integer;
begin
  case P^ of
    #$000A, #$000B, #$000C, #$0085, #$2028, #$2029:
      Result := 1;
    #$000D:
      if (P + 1)^ = #$000A then
        Result := 2
      else
        Result := 1;
    else
      Result := 0;
  end;
end;

function TSkRegExp.IsCR(P: PWideChar): Integer;
begin
  if P^ = #$000D then
    Result := 1
  else
    Result := 0;
end;

function TSkRegExp.IsCRLF(P: PWideChar): Integer;
begin
  Result := 0;
  if P^ = #$000D then
    if (P + 1)^ = #$000A then
      Result := 2;
end;

function TSkRegExp.IsLF(P: PWideChar): Integer;
begin
  if P^ = #$000A then
    Result := 1
  else
    Result := 0;
end;

function TSkRegExp.MatchCore(AStr: PWideChar): Boolean;
begin
{$IFDEF SKREGEXP_DEBUG}
  if FShowMatchProcess then
    FMatchProcess.Clear;
{$ENDIF}
  FSuccess := FMatchEngine.Match(AStr);
  if FSuccess then
  begin
    FGlobalStartP := FGroups[0].StartP;
    FGlobalEndP := FGroups[0].EndP;

    if Assigned(FOnMatch) then
      FOnMatch(Self);
  end
  else
  begin
    FGlobalStartP := FMatchTopP;
    FGlobalEndP := FMatchTopP;
  end;
  Result := FSuccess;
end;

class function TSkRegExp.RegIsMatch(const ARegExpStr, AInputStr: REString;
  AOptions: TREOptions): Boolean;
var
  R: TSkRegExp;
begin
  R := TSkRegExp.Create;
  try
    R.FOptions := AOptions;
    R.Expression := ARegExpStr;
    R.NamedGroupOnly := True;
    Result := R.Exec(AInputStr);
  finally
    R.Free;
  end;
end;

class function TSkRegExp.RegMatch(const ARegExpStr, AInputStr: REString;
  AMatches: TREStrings; AOptions: TREOptions): Boolean;
var
  R: TSkRegExp;
  I: Integer;
begin
  R := TSkRegExp.Create;
  try
    AMatches.Clear;
    R.FOptions := AOptions;
    R.Expression := ARegExpStr;
    if R.Exec(AInputStr) then
    begin
      for I := 0 to R.GroupCount do
        AMatches.Add(R.Groups[I].Strings);
      Result := True;
    end
    else
      Result := False;
  finally
    R.Free;
  end;
end;

class procedure TSkRegExp.RegSplit(const ARegExpStr, AInputStr: REString;
  APieces: TREStrings; AOptions: TREOptions);
var
  R: TSkRegExp;
begin
  APieces.Clear;

  R := TSkRegExp.Create;
  try
    R.FOptions := AOptions;
    R.Expression := ARegExpStr;
    R.Split(AInputStr, APieces);
  finally
    R.Free;
  end;
end;

function TSkRegExp.Replace(const Input: REString;
  AReplaceFunc: TSkRegExpReplaceFunction; Count, AOffset: Integer): REString;
begin
  FReplaceFunc := AReplaceFunc;
  FOnReplace := DoReplaceFunc;
  try
    Result := Replace(Input, '', Count, AOffset);
  finally
    FOnReplace := nil;
    FReplaceFunc := nil;
  end;
end;

function TSkRegExp.Replace(const Input, Replacement: REString;
  Count, AOffset: Integer): REString;
var
  Index, LCount: Integer;
  RepStr: REString;
  LReplacement: REString;
begin
  Result := '';
  LCount := 0;
  InputString := Input;
  Index := 1;
  LReplacement := DecodeEscape(Replacement);

  if ExecPos(AOffset) then
  begin
    repeat
      if FGroups[0].Length > 0 then
      begin
        if (Count > 0) then
        begin
          Inc(LCount);
          if (LCount > Count) then
            Break;
        end;

        RepStr := Substitute(LReplacement);
        if Assigned(FOnReplace) then
          FOnReplace(Self, RepStr);

        Result := Result + Copy(Input, Index, FGroups[0].Index - Index)
          + RepStr;
        Index := FGroups[0].Index + FGroups[0].Length;
      end;
    until not ExecNext;
  end;

  Result := Result + Copy(Input, Index, Maxint);
end;

class function TSkRegExp.EncodeEscape(const Str: REString): REString;
var
  I, L: Integer;
begin
  Result := '';
  if Str = '' then
    Exit;

  I := 1;
  L := System.Length(Str);

  while I <= L do
  begin
    case Str[I] of
      #0009:
        Result := Result + '\t';
      #$000A:
        Result := Result + '\n';
      #$000D:
        Result := Result + '\r';
      #$000C:
        Result := Result + '\f';
      #$0007:
        Result := Result + '\a';
      #$001B:
        Result := Result + '\e';
    else
      begin
        if IsCntrlU(ToUChar(Str, I)) then
        begin
          Result := Result + Format('\x{%.2x}', [Ord(Str[I])])
        end
        else
          Result := Result + Str[I];
      end;
    end;
    Inc(I);
  end;
end;

procedure TSkRegExp.Error(const ErrorMes: REString);
begin
  raise ESkRegExpRuntime.Create(ErrorMes);
end;

class function TSkRegExp.EscapeRegExChars(const S: REString): REString;
var
  I: Integer;
begin
  Result := S;
  I := System.Length(Result);
  while I > 0 do
  begin
    case Result[I] of
      '.', '[', ']', '(', ')', '?', '*', '+', '{', '}', '^', '$', '|', '\':
        Insert('\', Result, I);
      #0:
        begin
          Result[I] := '0';
          Insert('\', Result, I);
        end;
    end;
    Dec(I);
  end;
end;

procedure TSkRegExp.SetDefineCharClassLegacy(const Value: Boolean);
begin
  if Value then
  begin
    if not(roDefinedCharClassLegacy in FOptions) then
    begin
      FCompiled := False;
      Include(FOptions, roDefinedCharClassLegacy);
    end;
  end
  else
  begin
    if roDefinedCharClassLegacy in FOptions then
    begin
      FCompiled := False;
      Exclude(FOptions, roDefinedCharClassLegacy);
    end;
  end;
end;

procedure TSkRegExp.SetLineBreakKind(const Value: TRELineBreakKind);
begin
  if FLineBreakKind <> Value then
  begin
    FLineBreakKind := Value;
    case FLineBreakKind of
      lbLF:
        IsLineBreak := IsLF;
      lbCR:
        IsLineBreak := IsCR;
      lCRLF:
        IsLineBreak := IsCRLF;
      lbAnyCRLF:
        IsLineBreak := IsAnyCRLF;
      else
        IsLineBreak := IsAnyEOL;
    end;
  end;
end;

procedure TSkRegExp.SetExpression(const Value: REString);
begin
  if FExpression <> Value then
  begin
    FExpression := Value;
    FCompiled := False;
  end;
end;

{$IFDEF JapaneseExt}
procedure TSkRegExp.SetIgnoreZenHan(const Value: Boolean);
begin
  IgnoreWidth := Value;
  IgnoreKana := Value;
end;
{$ENDIF}

procedure TSkRegExp.SetInputString(const Value: REString);
var
  L: Integer;
begin
  if FInputString <> Value then
  begin
    FInputString := Value;
    FModified := True;
    L := System.Length(FInputString);
  {$IFDEF CHECK_MATCH_EXPLOSION}
    ClearMatchExplosionState;
    SetLength(FMatchExplosionState, L + 1);
  {$ENDIF}
    FTextTopP := PWideChar(FInputString);
    FTextEndP := FTextTopP + L;
    FSuccess := False;
    FMatchTopP := FTextTopP;
    FMatchEndP := FTextEndP;
    FMatchStartP := FTextTopP;
    FGlobalStartP := nil;
    FGlobalEndP := nil;
  end;
end;

procedure TSkRegExp.SetOptions(const Index: Integer; const Value: Boolean);
var
  LOption: TREOption;
begin
  case Index of
    0:
      LOption := roIgnoreCase;
    1:
      LOption := roMultiLine;
    2:
      LOption := roNamedGroupOnly;
    3:
      LOption := roSingleLine;
    4:
      LOption := roExtended;
    5:
      LOption := roIgnoreWidth;
    7:
      LOption := roAutoCallout;
  else
    LOption := roIgnoreKana;
  end;

  if (LOption = roNone) or (Value and (LOption in FOptions)) or
    (not Value and not(LOption in FOptions)) then
    Exit;

  if Value then
    Include(FOptions, LOption)
  else
    Exclude(FOptions, LOption);

  FCompiled := False;
end;

procedure TSkRegExp.Split(const Input: REString; APieces: TREStrings;
  Count, AOffset: Integer);
var
  Index, LCount: Integer;
  S: REString;
begin
  Index := 1;
  LCount := 0;
  APieces.Clear;

  InputString := Input;

  if ExecPos(AOffset) then
  begin
    repeat
      if FGroups[0].Length > 0 then
      begin
        S := Copy(Input, Index, FGroups[0].Index - Index);
        APieces.Add(S);

        Index := FGroups[0].Index + FGroups[0].Length;

        Inc(LCount);
        if (Count > 0) and (LCount >= Count - 1) then
          Break;
      end
      until not ExecNext;

      APieces.Add(Copy(Input, Index, Maxint));
    end;
  end;

function TSkRegExp.Substitute(const ATemplate: REString): REString;
var
  K: Integer;
  LGroupName: REString;
  I, L: Integer;
begin
  Result := '';
  if ATemplate = '' then
    Exit;

  if not FSuccess then
    Exit;

  I := 1;
  L := System.Length(ATemplate);

  while I <= L do
  begin
    if ATemplate[I] = '$' then
    begin
      Inc(I);
      if (ATemplate[I] >= '0') and (ATemplate[I] <= '9') then
      begin
        K := (Integer(ATemplate[I]) - Integer('0'));
        if K <= FGroups.Count - 1 then
          Result := Result + FGroups[K].Strings;
      end
      else if ATemplate[I] = '{' then
      begin
        Inc(I);
        LGroupName := '';
        while (ATemplate[I] <> '}') and (ATemplate[I] <> #0000) do
        begin
          LGroupName := LGroupName + ATemplate[I];
          Inc(I);
        end;

        if FGroups.NameExists(LGroupName) then
          Result := Result + FGroups.Names[LGroupName].Strings;
      end
      else if ATemplate[I] = '&' then
        Result := Result + FGroups[0].Strings
      else if ATemplate[I] = '$' then
        Result := Result + '$'
      else if ATemplate[I] = '`' then
      begin
        Result := Result + Copy(FInputString, 1, FGroups[0].Index - 1);
      end
      else if ATemplate[I] = '''' then
      begin
        Result := Result + Copy(FInputString, FGroups[0].
          Index + FGroups[0].Length, Maxint);
      end
      else if ATemplate[I] = '_' then
      begin
        Result := Result + FInputString;
      end
      else if ATemplate[I] = '+' then
      begin
        for I := GroupCount downto 1 do
        begin
          if FGroups[I].Index > 0 then
          begin
            Result := Result + FGroups[I].Strings;
            Break;
          end;
        end;
      end
      else
        Result := Result + ATemplate[I];
    end
    else
      Result := Result + ATemplate[I];

    Inc(I);
  end;
end;

class function TSkRegExp.RegReplace(const ARegExpStr, AInputStr,
  AReplaceStr: REString; AOptions: TREOptions): REString;
var
  R: TSkRegExp;
begin
  R := TSkRegExp.Create;
  try
    R.FOptions := AOptions;
    R.Expression := ARegExpStr;
    Result := R.Replace(AInputStr, AReplaceStr);
  finally
    R.Free;
  end;
end;

class function TSkRegExp.RegReplace(const ARegExpStr, AInputStr: REString;
  AReplaceFunc: TSkRegExpReplaceFunction; AOptions: TREOptions): REString;
var
  R: TSkRegExp;
begin
  R := TSkRegExp.Create;
  try
    R.Options := AOptions;
    R.Expression := ARegExpStr;
    Result := R.Replace(AInputStr, AReplaceFunc)
  finally
    R.Free;
  end;
end;


end.
