(* ***************************************************************************
  SkRegExpConst.pas (SkRegExp regular expression library)
  **************************************************************************** *)
(*

  This Source Code Form is subject to the terms of the Mozilla Public
  License, v. 2.0. If a copy of the MPL was not distributed with this
  file, You can obtain one at http://mozilla.org/MPL/2.0/.

  This Source Code Form is �gIncompatible With Secondary Licenses�h, as
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
  sNotRecognized                      = '%s�F���ł��Ȃ����K�\���ł�';
  sHexDigitIsRequired                 = '%s16�i�����K�v�ł�';
  sCodePointRangeOver                 = '%s�R�[�h�|�C���g�͈̔͂𒴂��Ă��܂�';
  sLoopOfMatchAheadNotSpecified       = '��ǂ݂ł̌J��Ԃ��͎w��ł��܂���';
  sUnmatchedBigPar                    = '%s [ ���}�b�`���܂���';
  sMissingRightBraceOnEscx            = '%s } ������܂���';
  sQuantifierIsTooLarge               = '%s�J��Ԃ��̐����傫�����܂�';
  sCantDoMaxLessMin                   = '%s�ő�l���ŏ��l�̂ق����傫���ł�';
  sPosixClassUnkown                   = '[:%s:] ��POSIX�N���X�ł͂���܂���';
  sInvalideBigParRange                = '�����N���X�͈͎̔w�肪�Ⴂ�܂� [%s]';
  sGroupNameIsEmpty                   = '%s�O���[�v��������܂���';
  sGroupNumberIsEmpty                 = '%s�O���[�v�ԍ�������܂���';
  sNoEndOfGroup                       = '%s�O���[�v���I�����Ă��܂���';
  sUnmatchedSmallPar                  = '%s ( ���}�b�`���܂���';
  sOptionNotCompleted                 = '%s�I�v�V�����̎w�肪�I�����Ă��܂���';
  sPropertyUnknown                    = '{%s} �̓v���p�e�B�ł͂���܂���';
  sInvalidProperty                    = '%s�v���p�e�B���s���ł�';
  sMissingRightBrace                  = '%s ] ������܂���';
  sQuestPosInaccurate                 = '?�̈ʒu���s���ł�';
  sLoopOfLengthZeroCannotSpecified    = '%s�����[���̌J��Ԃ��͂ł��܂���';
  sRegExpNotCompleted                 = '%s���K�\�����������I�����Ă��܂���';
  sInvalidCharactorClass              = '%s�s���ȕ����N���X�ł�';
  sCannotCallMultipleDefineGroupName  = '���d��`���ꂽ�O���[�v��<%s>�̕������͌Ăяo���܂���';
  sInvalidGroupNumber                 = '�O���[�v�ԍ�<%d>�͎Q�Ƃł��܂���';
  sReferenceToNonexistentNamedGroup   = '���݂��Ȃ��O���[�v��<%s>���Q�Ƃ��Ă��܂�';
  sInvalidCharInGroupName             = '�O���[�v���̒��ɕs���ȕ���������܂�';
  sNeverEndingRecursion               = '%s�I�����Ȃ��������Ȃ��ċA������܂�';
  sBehindMatchNotVariableLength       = '%s�߂�ǂݓ��ŉϒ�������͎g���܂���';
  sInvalideCondition                  = '%s���������s���ł�';
  sContainsTooManyBranches            = '�����\���̕��򂪑������܂�';
  sNotTerminated                      = '%s���K�\�����I�����Ă��܂���';
  sInvalidEscapeCharacterSyntax       = '%s�G�X�P�[�v�����̏������Ԉ���Ă��܂�';
  sPosixClassSupportedOnlyClass       = '%sPOSIX�����N���X���g����͕̂����N���X�̒������ł�';
  sRangeOverCalloutNumber             = '%s�R�[���A�E�g�ԍ��� 0 ���� 255 �܂łł�';
  sNothingToRepeat                    = '%s�J��Ԃ��Ώۂ�����܂���';
  sConditionNotRecognized             = '%s���������F���ł��܂���';
  sUnterminatedVerbPattern            = '%s�o�b�N�g���b�N����L�����������I�����Ă��܂���';
  sUseNamedGroup                      = '%s�O���[�v�ԍ��ł͎Q�Ƃł��܂���B�O���[�v�����g���Ă�������';
  sMissingLeftBraceOnESCo             = '%s\o �̎��� { ���K�v�ł�';
  sMissingRightBraceOnEsco            = '%s\o �̍Ō�ɂ� } ���K�v�ł�';
  sOctalDigitIsRequired               = '%s8�i�����K�v�ł�';
  sModifierCanNotBeDisabled           = '%s�����ɂł��Ȃ��C���q�ł�';
  sModifierCanNotBeDisabledInCaret    = '%s"(?^...)"���ł͏C���q�𖳌��ɂł��܂���';
  sModifiersareMutuallyExclusive      = '%s�C���q"adlu" �͓����ɐݒ�ł��܂���';
  sModifierAMayAppearAMaximumOfTwice  = '%s�C���q"a"���ݒ�ł���̂�2�܂łł�';
  // end of compile error

  { Runtime error messages }
  sExecFuncNotCall                    = 'Exec���\�b�h�����s����Ă��܂���';
  sUnmatchedCurlyBracket              = '%s { ���}�b�`���܂���';
  sRangeOverGroupNumber               = '�O���[�v�ԍ�<%d>�͔͈͂��z���Ă��܂�';
  sMissingGroupName                   = '�O���[�v��<%s>�͑��݂��܂���';

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
