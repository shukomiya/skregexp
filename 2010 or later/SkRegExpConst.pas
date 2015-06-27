(* ***************************************************************************
  SkRegExpConst.pas (SkRegExp regular expression library)
  **************************************************************************** *)
(*

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  This Source Code Form is “Incompatible With Secondary Licenses”, as
  defined by the Mozilla Public License, v. 2.0.

  Software distributed under the License is distributed on an "AS IS"
  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
  License for the specific language governing rights and limitations
  under the License.

  The Original Code is SkRegExpConst.pas(for SkRegExp Library).

  The Initial Developer of the Original Code is Komiya Shuichi.

  E-mail: shu AT komish DOT jp
  URL:    http://skregexp.komish.com/

  Portions created by Komiya Shuichi are
  Copyright (C) 2007-2015 Komiya Shuichi. All Rights Reserved.

*)

unit SkRegExpConst;

interface

{$DEFINE JapaneseMessage}

resourcestring

{$IFDEF JapaneseMessage}
  { Error Message Japanese }

  { Complile error messages }
  sNotRecognized                      = '%s認識できない正規表現です';
  sHexDigitIsRequired                 = '%s16進数が必要です';
  sCodePointRangeOver                 = '%sコードポイントの範囲を超えています';
  sLoopOfMatchAheadNotSpecified       = '先読みでの繰り返しは指定できません';
  sUnmatchedBigPar                    = '%s [ がマッチしません';
  sMissingRightBraceOnEscx            = '%s } がありません';
  sQuantifierIsTooLarge               = '%s繰り返しの数が大きすぎます';
  sCantDoMaxLessMin                   = '%s最大値より最小値のほうが大きいです';
  sPosixClassUnkown                   = '[:%s:] はPOSIXクラスではありません';
  sInvalideBigParRange                = '文字クラスの範囲指定が違います [%s]';
  sGroupNameIsEmpty                   = '%sグループ名がありません';
  sGroupNumberIsEmpty                 = '%sグループ番号がありません';
  sNoEndOfGroup                       = '%sグループが終了していません';
  sUnmatchedSmallPar                  = '%s ( がマッチしません';
  sOptionNotCompleted                 = '%sオプションの指定が終了していません';
  sPropertyUnknown                    = '{%s} はプロパティではありません';
  sInvalidProperty                    = '%sプロパティが不正です';
  sMissingRightBrace                  = '%s ] がありません';
  sQuestPosInaccurate                 = '?の位置が不正です';
  sLoopOfLengthZeroCannotSpecified    = '%s長さゼロの繰り返しはできません';
  sRegExpNotCompleted                 = '%s正規表現が正しく終了していません';
  sInvalidCharactorClass              = '%s不正な文字クラスです';
  sCannotCallMultipleDefineGroupName  = '多重定義されたグループ名<%s>の部分式は呼び出せません';
  sInvalidGroupNumber                 = 'グループ番号<%d>は参照できません';
  sReferenceToNonexistentNamedGroup   = '存在しないグループ名<%s>を参照しています';
  sInvalidCharInGroupName             = 'グループ名の中に不正な文字があります';
  sNeverEndingRecursion               = '%s終了しないかもしない再帰があります';
  sBehindMatchNotVariableLength       = '%s戻り読み内で可変長文字列は使えません';
  sInvalideCondition                  = '%s条件式が不正です';
  sContainsTooManyBranches            = '条件構文の分岐が多すぎます';
  sNotTerminated                      = '%s正規表現が終了していません';
  sInvalidEscapeCharacterSyntax       = '%sエスケープ文字の書式が間違っています';
  sPosixClassSupportedOnlyClass       = '%sPOSIX文字クラスが使えるのは文字クラスの中だけです';
  sRangeOverCalloutNumber             = '%sコールアウト番号は 0 から 255 までです';
  sNothingToRepeat                    = '%s繰り返す対象がありません';
  sConditionNotRecognized             = '%s条件式が認識できません';
  sUnterminatedVerbPattern            = '%sバックトラック制御記号が正しく終了していません';
  sUseNamedGroup                      = '%sグループ番号では参照できません。グループ名を使ってください';
  sMissingLeftBraceOnESCo             = '%s\o の次は { が必要です';
  sMissingRightBraceOnEsco            = '%s\o の最後には } が必要です';
  sOctalDigitIsRequired               = '%s8進数が必要です';
  sModifierCanNotBeDisabled           = '%s無効にできない修飾子です';
  sModifierCanNotBeDisabledInCaret    = '%s"(?^...)"内では修飾子を無効にできません';
  sModifiersareMutuallyExclusive      = '%s修飾子"adlu" は同時に設定できません';
  sModifierAMayAppearAMaximumOfTwice  = '%s修飾子"a"が設定できるのは2個までです';
  // end of compile error

  { Runtime error messages }
  sExecFuncNotCall                    = 'Execメソッドが実行されていません';
  sUnmatchedCurlyBracket              = '%s { がマッチしません';
  sRangeOverGroupNumber               = 'グループ番号<%d>は範囲を越えています';
  sMissingGroupName                   = 'グループ名<%s>は存在しません';

{$ELSE}
  { Error Message English }

  { Complile error messages }
  sNotRecognized                      = '%s not recognized';
  sHexDigitIsRequired                 = '%sHex-digit is required';
  sCodePointRangeOver                 = '%sCodepoint range over';
  sLoopOfMatchAheadNotSpecified       = 'Loop of match ahead not specified';
  sUnmatchedBigPar                    = '%sUnmatched [';
  sMissingRightBraceOnEscx            = '%sMissing right brace on \x{}';
  sQuantifierIsTooLarge               = '%sQuantifier is too large';
  sCantDoMaxLessMin                   = '%sCan''t do {n,m} with n > m';
  sPosixClassUnkown                   = 'Posix class [:%s:] unknown';
  sInvalideBigParRange                = 'Invalid [] range [%s]';
  sGroupNameIsEmpty                   = '%sgroup name is empty';
  sGroupNumberIsEmpty                 = '%sGroup number is empty';
  sNoEndOfGroup                       = '%sNo end of group';
  sUnmatchedSmallPar                  = '%sUnmatched (';
  sOptionNotCompleted                 = '%sOption not completed';
  sPropertyUnknown                    = 'Property {%s} unkown';
  sInvalidProperty                    = '%sInvalide property';
  sMissingRightBrace                  = '%sMissing right brace';
  sQuestPosInaccurate                 = '%s? position is inaccurate';
  sLoopOfLengthZeroCannotSpecified    = '%sLoop of Length zero cannot specified';
  sRegExpNotCompleted                 = '%sRegular expression not completed';
  sInvalidCharactorClass              = '%sInvalid charactor class';
  sCannotCallMultipleDefineGroupName  =
    'Multiple define group name <%s> can not call'; // check!
  sInvalidGroupNumber                 = 'Invalid group number <%d>';
  sInvalidCharInGroupName             = '%sInvalid char in group name';
  sReferenceToNonexistentNamedGroup   = 'Reference to nonexistent named group <%s>';
  sNeverEndingRecursion               = '%sNever ending recursion';
  sBehindMatchNotVariableLength       = '%sBehind match not variable length';
  sInvalideCondition                  = '%sInvalid Condition';
  sContainsTooManyBranches            = 'Contains too many Branches';
  sNotTerminated                      = '%sNot terminated';
  sInvalidEscapeCharacterSyntax       = '%sInvalid escape charactoer syntax';
  sPosixClassSupportedOnlyClass       = '%sPOSIX named classes are supported only within a class';
  sRangeOverCalloutNumber             = '%sCallout number is up to 255-0';
  sNothingToRepeat                    = '%sNothing to repeat';
  sConditionNotRecognized             = '%sCondition not recognized';
  sUnterminatedVerbPattern            = '%sUnterminated verb pattern';
  sUseNamedGroup                      = '%sNumberd backref/call is not allowed.(use name)';
  sMissingLeftBraceOnESCo             = '%sMissing left brace on \o{}';
  sMissingRightBraceOnEsco            = '%sMissing right braces on \o{}';
  sOctalDigitIsRequired               = '%sOctal-digit is required';
  sModifierCanNotBeDisabled           = '%sModifier can not be disable';
  sModifierCanNotBeDisabledInCaret    = '%sModifire can not be disable in "(?^...)"';
  sModifiersareMutuallyExclusive      = '%sModifiers "adlu" are mutually exclusive';
  sModifierAMayAppearAMaximumOfTwice  = '%sModifier "a" may appear a maximum of twice';
  // end of compile error

  { Runtime error messages }
  sExecFuncNotCall                    = 'Exec function not call';
  sUnmatchedCurlyBracket              = '%sUnmatched {';
  sRangeOverGroupNumber               = 'Range over group number <%d>';
  sMissingGroupName                   = 'Missing group number <%s>';

{$ENDIF}  // end of error message

{$IFDEF SKREGEXP_DEBUG}
  { for debug }
  sLiteral                            = 'EXACT%s <%s> ';
  sTrie                               = 'TRIE [%s] ';
  sAnyChar                            = 'ANY ';
  sWordChar                           = 'WORD ';
  sNegativeWordChar                   = 'NWORD ';
  sDigitChar                          = 'DIGIT ';
  sNegativeDigitChar                  = 'NDIGIT ';
  sSpaceChar                          = 'SPACE ';
  sNegativeSpaceChar                  = 'NSPACE ';
  sHorizontalSpaceChar                = 'HSPACE ';
  sNegativeHorizontalSpaceChar        = 'NHSPACE ';
  sVerticalSpaceChar                  = 'VSPACE ';
  sNegativeVerticalSpaceChar          = 'NVSPACE ';
  sLineBreakChar                      = 'LINEBREK ';
  sCharClass                          = 'CCLASS [';
  sNEGCharClass                       = 'NEG-CCLASS [';
  sCombiningSequence                  = 'COMBSQ ';
  sBoundaryCode                       = 'BOUNDA ';
  sNegativeBoundaryCode               = 'NBOUNDA ';
  sFmtGroupReference                  = 'REF%d ';
  sFmtGroupNameReference              = 'REF[%s]';
  sHeadOfLineCode                     = 'BOL';
  sEndOfLineCode                      = 'EOL';
  sTextHeadCode                       = 'SBOL';
  sTextTailCode                       = 'SEOL';
  sTextEndCode                        = 'EOS';
  sPropertyCode                       = 'UPROP(%s)';
  sNegativePropertyCode               = 'NUPROP(%s)';
  // 1.1.0 add
  sIfThenReference                    = 'Cond<%d>';
  sIfThenNamedReference               = 'CondName<%s>';
  sGlobalPos                          = 'GPOS';
  // 1.4.0
  sCallout                            = 'Callout[%d]';
  // 1.5 add
  sInSubR                             = 'INSUBR[%d]';
  sInSubN                             = 'INSUBN[%s]';
  // 1.6 add
  sPosixCharClassCode                 = 'POSIX(%s)';
  sNegativePosixCharClassCode         = 'NPOSIX(%s)';

  sFmtDumpLeadCodeExist               = 'LeadChar: ';
  sFmtDumpAnchor                      = 'Anchor: ';

  sBinCode_Raise                      = 'bug: not define operator';

  sFmtDumpNFA_Start                   = '%2d : ';
  sFmtDumpNFA_End                     = '%2d : ';
  sFmtDumpNFA_EndStr                  = 'END';
  sFmtDumpNFA_Status                  = '%2d : ';
  sFmtDumpNFA_Empty                   = 'NOTHING (%d) :';
  sFmtDumpNFA_LoopExit                = 'LOOPEXIT (%d) :';
  sFmtDumpNFA_LoopEnd                 = 'LOOPEND (%d) :';
  sFmtDumpNFA_Star                    = 'STAR%s (%d) %s :';
  sFmtDumpNFA_Plus                    = 'PLUS%s (%d) %s :';
  sFmtDumpNFA_Quest                   = 'QUEST%s (%d) %s :';
  sFmtDumpNFA_Bound                   = 'BOUND%s[%d..%d] (%d) %s :';

  sFmtDumpNFA_Loop                    = 'LOOP%s [%d..%d] (%d) :';
  sFmtDumpNFA_AheadMatch              = 'AMATCH (%d) :';
  sFmtDumpNFA_AheadNoMatch            = 'NAMATCH (%d) :';
  sFmtDumpNFA_BehindMatch             = 'BMATCH (%d) :';
  sFmtDumpNFA_BehindNoMatch           = 'NBMATCH (%d) :';
  sFmtDumpNFA_MatchEnd                = 'EOMATCH (%d) :';
  sFmtDumpNFA_GroupBegin              = 'OPEN%d (%d) :';
  sFmtDumpNFA_GroupEnd                = 'CLOSE%d (%d) :';
  sFmtDumpNFA_SuspendBegin            = 'SUSPEND (%d) :';
  sFmtDumpNFA_Null                    = '%s (%d)  :';
  // 0.9.4 add
  sFmtDumpNFA_GoSub                   = 'GOSUB[%d] (%d) :';
  // 1.1.0 add
  sFmtDumpNFA_IfMatch                 = 'IFMATCH (%d) :';
  // 1.1.0 add
  sFmtDumpNFA_IfThen                  = 'IFTHEN (%d) :';
  sFmtDumpNFA_KeepPattern             = 'KEEPL (%d) :';
  // 1.3.0 add
  sFmtDumpNFA_Fail                    = 'FAIL (%d) :';
  // 1.5.0 add
  sFmtDumpNFA_Define                  = 'DEFINE (%d) :';
  sFmtDumpNFA_Prune                   = 'PRUNE[%s] (%d) :';
  sFmtDumpNFA_Skip                    = 'SKIP[%s] (%d) :';
  sFmtDumpNFA_Mark                    = 'MARK[%s] (%d) :';
  sFmtDumpNFA_Then                    = 'THEN[%s] (%d) :';
  sFmtDumpNFA_Commit                  = 'COMMIT[%s] (%d) :';
  sFmtDumpNFA_Accept                  = 'ACCEPT (%d):';
  //
  sBinCode_Union                      = 'Union "|"';
  sBinCode_Concat                     = 'Concat';
  sBinCode_Emply                      = 'Empty';
  sBinCode_Plus                       = 'Plus "+"';
  sBinCode_Star                       = 'Star "*"';
  sBinCode_Loop                       = 'Loop';
  sBinCode_Quest                      = 'Quest "?"';
  sBinCode_Bound                      = 'Bound';
  sBinCode_LHead                      = 'Line head';
  sBinCode_LTail                      = 'Line tail';
  sBinCode_Group                      = 'Group';
  sBinCode_AheadMatch                 = 'Ahead match';
  sBinCode_BehindMatch                = 'Behind match';
  sBinCode_AheadNoMatch               = 'Ahead no match';
  sBinCode_BehindNoMatch              = 'Behind no match';
  sBinCode_Suspend                    = 'Suspend';

  // 0.9.4 add
  sBinCode_GroupCall                  = 'Call';
  // 1.1.0 add
  sBinCode_IfMatch                    = 'IfMatch';
  sBinCode_IfThen                     = 'IfThen';
  sBinCode_KeepPattern                = 'KeepLeft';
  // 1.3.0 add
  sBinCode_Fail                       = 'Fail';
  // 1.5.0 add
  sBinCode_Define                     = 'Define';
  sBinCode_Prune                      = 'Prune';
  sBinCode_Skip                       = 'Skip';
  sBinCode_Mark                       = 'Mark';
  sBinCode_Then                       = 'Then';
  sBinCode_Commit                     = 'Commit';
  sBinCode_Accept                     = 'Accept';

{$ENDIF}  // end of Debug;

implementation

end.
