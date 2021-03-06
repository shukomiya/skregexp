﻿(****************************************************************************
  UnicodeProp.pas (SkRegExp regular expression library)
*****************************************************************************)
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

  The Original Code is UnicodeProp.pas(for SkRegExp Library).

  The Initial Developer of the Original Code is Komiya Shuichi.

  E-mail: shu AT komish DOT jp
  URL:    http://skregexp.komish.com/

  Portions created by Komiya Shuichi are
  Copyright (C) 2007-2015 Komiya Shuichi. All Rights Reserved.

*)

unit UnicodeProp;

{$DEFINE BLOCK}

interface

uses IniFiles;

type
  TUnicodeProperty = (
    //7
    upC,
    upL,
    upM,
    upN,
    upP,
    upS,
    upZ,
    //31
    upCc,
    upCf,
    upCn,
    upCo,
    upCs,
    upLC,
    upLl,
    upLm,
    upLo,
    upLt,
    upLu,
    upMc,
    upMe,
    upMn,
    upNd,
    upNl,
    upNo,
    upPc,
    upPd,
    upPe,
    upPf,
    upPi,
    upPo,
    upPs,
    upSc,
    upSk,
    upSm,
    upSo,
    upZl,
    upZp,
    upZs,
    //104
    upunKnown,
    upArabic,
    upImperialAramaic,
    upArmenian,
    upAvestan,
    upBalinese,
    upBamum,
    upBatak,
    upBengali,
    upBopomofo,
    upBrahmi,
    upBraille,
    upBuginese,
    upBuhid,
    upChakma,
    upCanadianAboriginal,
    upCarian,
    upCham,
    upCherokee,
    upCoptic,
    upCypriot,
    upCyrillic,
    upDevanagari,
    upDeseret,
    upEgyptianHieroglyphs,
    upEthiopic,
    upGeorgian,
    upGlagolitic,
    upGothic,
    upGreek,
    upGujarati,
    upGurmukhi,
    upHangul,
    upHan,
    upHanunoo,
    upHebrew,
    upHiragana,
    upKatakanaOrHiragana,
    upOldItalic,
    upJavanese,
    upKayahLi,
    upKatakana,
    upKharoshthi,
    upKhmer,
    upKannada,
    upKaithi,
    upTaiTham,
    upLao,
    upLatin,
    upLepcha,
    upLimbu,
    upLinearB,
    upLisu,
    upLycian,
    upLydian,
    upMandaic,
    upMeroiticCursive,
    upMeroiticHieroglyphs,
    upMalayalam,
    upMongolian,
    upMeeteiMayek,
    upMyanmar,
    upNko,
    upOgham,
    upOlChiki,
    upOldTurkic,
    upOriya,
    upOsmanya,
    upPhagsPa,
    upInscriptionalPahlavi,
    upPhoenician,
    upMiao,
    upInscriptionalParthian,
    upRejang,
    upRunic,
    upSamaritan,
    upOldSouthArabian,
    upSaurashtra,
    upShavian,
    upSharada,
    upSinhala,
    upSoraSompeng,
    upSundanese,
    upSylotiNagri,
    upSyriac,
    upTagbanwa,
    upTakri,
    upTaiLe,
    upNewTaiLue,
    upTamil,
    upTaiViet,
    upTelugu,
    upTifinagh,
    upTagalog,
    upThaana,
    upThai,
    upTibetan,
    upUgaritic,
    upVai,
    upOldPersian,
    upCuneiform,
    upYi,
    upInherited,
    upCommon
{$IFDEF BLOCK}
,
    //220
    upInBasicLatin,
    upInLatin1Supplement,
    upInLatinExtendedA,
    upInLatinExtendedB,
    upInIPAExtensions,
    upInSpacingModifierLetters,
    upInCombiningDiacriticalMarks,
    upInGreekandCoptic,
    upInCyrillic,
    upInCyrillicSupplement,
    upInArmenian,
    upInHebrew,
    upInArabic,
    upInSyriac,
    upInArabicSupplement,
    upInThaana,
    upInNKo,
    upInSamaritan,
    upInMandaic,
    upInArabicExtendedA,
    upInDevanagari,
    upInBengali,
    upInGurmukhi,
    upInGujarati,
    upInOriya,
    upInTamil,
    upInTelugu,
    upInKannada,
    upInMalayalam,
    upInSinhala,
    upInThai,
    upInLao,
    upInTibetan,
    upInMyanmar,
    upInGeorgian,
    upInHangulJamo,
    upInEthiopic,
    upInEthiopicSupplement,
    upInCherokee,
    upInUnifiedCanadianAboriginalSyllabics,
    upInOgham,
    upInRunic,
    upInTagalog,
    upInHanunoo,
    upInBuhid,
    upInTagbanwa,
    upInKhmer,
    upInMongolian,
    upInUnifiedCanadianAboriginalSyllabicsExtended,
    upInLimbu,
    upInTaiLe,
    upInNewTaiLue,
    upInKhmerSymbols,
    upInBuginese,
    upInTaiTham,
    upInBalinese,
    upInSundanese,
    upInBatak,
    upInLepcha,
    upInOlChiki,
    upInSundaneseSupplement,
    upInVedicExtensions,
    upInPhoneticExtensions,
    upInPhoneticExtensionsSupplement,
    upInCombiningDiacriticalMarksSupplement,
    upInLatinExtendedAdditional,
    upInGreekExtended,
    upInGeneralPunctuation,
    upInSuperscriptsandSubscripts,
    upInCurrencySymbols,
    upInCombiningDiacriticalMarksforSymbols,
    upInLetterlikeSymbols,
    upInNumberForms,
    upInArrows,
    upInMathematicalOperators,
    upInMiscellaneousTechnical,
    upInControlPictures,
    upInOpticalCharacterRecognition,
    upInEnclosedAlphanumerics,
    upInBoxDrawing,
    upInBlockElements,
    upInGeometricShapes,
    upInMiscellaneousSymbols,
    upInDingbats,
    upInMiscellaneousMathematicalSymbolsA,
    upInSupplementalArrowsA,
    upInBraillePatterns,
    upInSupplementalArrowsB,
    upInMiscellaneousMathematicalSymbolsB,
    upInSupplementalMathematicalOperators,
    upInMiscellaneousSymbolsandArrows,
    upInGlagolitic,
    upInLatinExtendedC,
    upInCoptic,
    upInGeorgianSupplement,
    upInTifinagh,
    upInEthiopicExtended,
    upInCyrillicExtendedA,
    upInSupplementalPunctuation,
    upInCJKRadicalsSupplement,
    upInKangxiRadicals,
    upInIdeographicDescriptionCharacters,
    upInCJKSymbolsandPunctuation,
    upInHiragana,
    upInKatakana,
    upInBopomofo,
    upInHangulCompatibilityJamo,
    upInKanbun,
    upInBopomofoExtended,
    upInCJKStrokes,
    upInKatakanaPhoneticExtensions,
    upInEnclosedCJKLettersandMonths,
    upInCJKCompatibility,
    upInCJKUnifiedIdeographsExtensionA,
    upInYijingHexagramSymbols,
    upInCJKUnifiedIdeographs,
    upInYiSyllables,
    upInYiRadicals,
    upInLisu,
    upInVai,
    upInCyrillicExtendedB,
    upInBamum,
    upInModifierToneLetters,
    upInLatinExtendedD,
    upInSylotiNagri,
    upInCommonIndicNumberForms,
    upInPhagspa,
    upInSaurashtra,
    upInDevanagariExtended,
    upInKayahLi,
    upInRejang,
    upInHangulJamoExtendedA,
    upInJavanese,
    upInCham,
    upInMyanmarExtendedA,
    upInTaiViet,
    upInMeeteiMayekExtensions,
    upInEthiopicExtendedA,
    upInMeeteiMayek,
    upInHangulSyllables,
    upInHangulJamoExtendedB,
    upInHighSurrogates,
    upInHighPrivateUseSurrogates,
    upInLowSurrogates,
    upInPrivateUseArea,
    upInCJKCompatibilityIdeographs,
    upInAlphabeticPresentationForms,
    upInArabicPresentationFormsA,
    upInVariationSelectors,
    upInVerticalForms,
    upInCombiningHalfMarks,
    upInCJKCompatibilityForms,
    upInSmallFormVariants,
    upInArabicPresentationFormsB,
    upInHalfwidthandFullwidthForms,
    upInSpecials,
    upInLinearBSyllabary,
    upInLinearBIdeograms,
    upInAegeanNumbers,
    upInAncientGreekNumbers,
    upInAncientSymbols,
    upInPhaistosDisc,
    upInLycian,
    upInCarian,
    upInOldItalic,
    upInGothic,
    upInUgaritic,
    upInOldPersian,
    upInDeseret,
    upInShavian,
    upInOsmanya,
    upInCypriotSyllabary,
    upInImperialAramaic,
    upInPhoenician,
    upInLydian,
    upInMeroiticHieroglyphs,
    upInMeroiticCursive,
    upInKharoshthi,
    upInOldSouthArabian,
    upInAvestan,
    upInInscriptionalParthian,
    upInInscriptionalPahlavi,
    upInOldTurkic,
    upInRumiNumeralSymbols,
    upInBrahmi,
    upInKaithi,
    upInSoraSompeng,
    upInChakma,
    upInSharada,
    upInTakri,
    upInCuneiform,
    upInCuneiformNumbersandPunctuation,
    upInEgyptianHieroglyphs,
    upInBamumSupplement,
    upInMiao,
    upInKanaSupplement,
    upInByzantineMusicalSymbols,
    upInMusicalSymbols,
    upInAncientGreekMusicalNotation,
    upInTaiXuanJingSymbols,
    upInCountingRodNumerals,
    upInMathematicalAlphanumericSymbols,
    upInArabicMathematicalAlphabeticSymbols,
    upInMahjongTiles,
    upInDominoTiles,
    upInPlayingCards,
    upInEnclosedAlphanumericSupplement,
    upInEnclosedIdeographicSupplement,
    upInMiscellaneousSymbolsAndPictographs,
    upInEmoticons,
    upInTransportAndMapSymbols,
    upInAlchemicalSymbols,
    upInCJKUnifiedIdeographsExtensionB,
    upInCJKUnifiedIdeographsExtensionC,
    upInCJKUnifiedIdeographsExtensionD,
    upInCJKCompatibilityIdeographsSupplement,
    upInTags,
    upInVariationSelectorsSupplement,
    upInSupplementaryPrivateUseAreaA,
    upInSupplementaryPrivateUseAreaB

{$ENDIF BLOCK}
  );

  TUnicodeMultiChar = array[0..3] of Integer;
  PUnicodeMultiChar = ^TUnicodeMultiChar;

  TUnicodeCharTypeRec = record
    Script, Category: Byte;
    FoldCase: PUnicodeMultiChar;
  end;



const
{$IFDEF BLOCK}
  UnicodeBlockTable: array [0..$DB, 0..1] of Cardinal = (
    ($0000, $007F),
    ($0080, $00FF),
    ($0100, $017F),
    ($0180, $024F),
    ($0250, $02AF),
    ($02B0, $02FF),
    ($0300, $036F),
    ($0370, $03FF),
    ($0400, $04FF),
    ($0500, $052F),
    ($0530, $058F),
    ($0590, $05FF),
    ($0600, $06FF),
    ($0700, $074F),
    ($0750, $077F),
    ($0780, $07BF),
    ($07C0, $07FF),
    ($0800, $083F),
    ($0840, $085F),
    ($08A0, $08FF),
    ($0900, $097F),
    ($0980, $09FF),
    ($0A00, $0A7F),
    ($0A80, $0AFF),
    ($0B00, $0B7F),
    ($0B80, $0BFF),
    ($0C00, $0C7F),
    ($0C80, $0CFF),
    ($0D00, $0D7F),
    ($0D80, $0DFF),
    ($0E00, $0E7F),
    ($0E80, $0EFF),
    ($0F00, $0FFF),
    ($1000, $109F),
    ($10A0, $10FF),
    ($1100, $11FF),
    ($1200, $137F),
    ($1380, $139F),
    ($13A0, $13FF),
    ($1400, $167F),
    ($1680, $169F),
    ($16A0, $16FF),
    ($1700, $171F),
    ($1720, $173F),
    ($1740, $175F),
    ($1760, $177F),
    ($1780, $17FF),
    ($1800, $18AF),
    ($18B0, $18FF),
    ($1900, $194F),
    ($1950, $197F),
    ($1980, $19DF),
    ($19E0, $19FF),
    ($1A00, $1A1F),
    ($1A20, $1AAF),
    ($1B00, $1B7F),
    ($1B80, $1BBF),
    ($1BC0, $1BFF),
    ($1C00, $1C4F),
    ($1C50, $1C7F),
    ($1CC0, $1CCF),
    ($1CD0, $1CFF),
    ($1D00, $1D7F),
    ($1D80, $1DBF),
    ($1DC0, $1DFF),
    ($1E00, $1EFF),
    ($1F00, $1FFF),
    ($2000, $206F),
    ($2070, $209F),
    ($20A0, $20CF),
    ($20D0, $20FF),
    ($2100, $214F),
    ($2150, $218F),
    ($2190, $21FF),
    ($2200, $22FF),
    ($2300, $23FF),
    ($2400, $243F),
    ($2440, $245F),
    ($2460, $24FF),
    ($2500, $257F),
    ($2580, $259F),
    ($25A0, $25FF),
    ($2600, $26FF),
    ($2700, $27BF),
    ($27C0, $27EF),
    ($27F0, $27FF),
    ($2800, $28FF),
    ($2900, $297F),
    ($2980, $29FF),
    ($2A00, $2AFF),
    ($2B00, $2BFF),
    ($2C00, $2C5F),
    ($2C60, $2C7F),
    ($2C80, $2CFF),
    ($2D00, $2D2F),
    ($2D30, $2D7F),
    ($2D80, $2DDF),
    ($2DE0, $2DFF),
    ($2E00, $2E7F),
    ($2E80, $2EFF),
    ($2F00, $2FDF),
    ($2FF0, $2FFF),
    ($3000, $303F),
    ($3040, $309F),
    ($30A0, $30FF),
    ($3100, $312F),
    ($3130, $318F),
    ($3190, $319F),
    ($31A0, $31BF),
    ($31C0, $31EF),
    ($31F0, $31FF),
    ($3200, $32FF),
    ($3300, $33FF),
    ($3400, $4DBF),
    ($4DC0, $4DFF),
    ($4E00, $9FFF),
    ($A000, $A48F),
    ($A490, $A4CF),
    ($A4D0, $A4FF),
    ($A500, $A63F),
    ($A640, $A69F),
    ($A6A0, $A6FF),
    ($A700, $A71F),
    ($A720, $A7FF),
    ($A800, $A82F),
    ($A830, $A83F),
    ($A840, $A87F),
    ($A880, $A8DF),
    ($A8E0, $A8FF),
    ($A900, $A92F),
    ($A930, $A95F),
    ($A960, $A97F),
    ($A980, $A9DF),
    ($AA00, $AA5F),
    ($AA60, $AA7F),
    ($AA80, $AADF),
    ($AAE0, $AAFF),
    ($AB00, $AB2F),
    ($ABC0, $ABFF),
    ($AC00, $D7AF),
    ($D7B0, $D7FF),
    ($D800, $DB7F),
    ($DB80, $DBFF),
    ($DC00, $DFFF),
    ($E000, $F8FF),
    ($F900, $FAFF),
    ($FB00, $FB4F),
    ($FB50, $FDFF),
    ($FE00, $FE0F),
    ($FE10, $FE1F),
    ($FE20, $FE2F),
    ($FE30, $FE4F),
    ($FE50, $FE6F),
    ($FE70, $FEFF),
    ($FF00, $FFEF),
    ($FFF0, $FFFF),
    ($10000, $1007F),
    ($10080, $100FF),
    ($10100, $1013F),
    ($10140, $1018F),
    ($10190, $101CF),
    ($101D0, $101FF),
    ($10280, $1029F),
    ($102A0, $102DF),
    ($10300, $1032F),
    ($10330, $1034F),
    ($10380, $1039F),
    ($103A0, $103DF),
    ($10400, $1044F),
    ($10450, $1047F),
    ($10480, $104AF),
    ($10800, $1083F),
    ($10840, $1085F),
    ($10900, $1091F),
    ($10920, $1093F),
    ($10980, $1099F),
    ($109A0, $109FF),
    ($10A00, $10A5F),
    ($10A60, $10A7F),
    ($10B00, $10B3F),
    ($10B40, $10B5F),
    ($10B60, $10B7F),
    ($10C00, $10C4F),
    ($10E60, $10E7F),
    ($11000, $1107F),
    ($11080, $110CF),
    ($110D0, $110FF),
    ($11100, $1114F),
    ($11180, $111DF),
    ($11680, $116CF),
    ($12000, $123FF),
    ($12400, $1247F),
    ($13000, $1342F),
    ($16800, $16A3F),
    ($16F00, $16F9F),
    ($1B000, $1B0FF),
    ($1D000, $1D0FF),
    ($1D100, $1D1FF),
    ($1D200, $1D24F),
    ($1D300, $1D35F),
    ($1D360, $1D37F),
    ($1D400, $1D7FF),
    ($1EE00, $1EEFF),
    ($1F000, $1F02F),
    ($1F030, $1F09F),
    ($1F0A0, $1F0FF),
    ($1F100, $1F1FF),
    ($1F200, $1F2FF),
    ($1F300, $1F5FF),
    ($1F600, $1F64F),
    ($1F680, $1F6FF),
    ($1F700, $1F77F),
    ($20000, $2A6DF),
    ($2A700, $2B73F),
    ($2B740, $2B81F),
    ($2F800, $2FA1F),
    ($E0000, $E007F),
    ($E0100, $E01EF),
    ($F0000, $FFFFF),
    ($100000, $10FFFF)
  );
{$ENDIF BLOCK}

  UnicodeGeneralCategoryTable: array[upCc..upZs] of TUnicodeProperty =(
    upC,upC,upC,upC,upC,
    upL,upL,upL,upL,upL,upL,
    upM,upM,upM,
    upN,upN,upN,
    upP,upP,upP,upP,upP,upP,upP,
    upS,upS,upS,upS,
    upZ,upZ,upZ
  );

  tfc: array[0..131] of TUnicodeMultiChar =( //132
    (0, 0, 0, 0),		//0
    (1, -32, 0, 0),		//1
    (1, -232, 0, 0),		//2
    (1, -775, 0, 0),		//3
    (2, 115, 115, 0),		//4
    (1, -1, 0, 0),		//5
    (1, 199, 105, 0),		//6
    (2, 110, 700, 0),		//7
    (1, 121, 0, 0),		//8
    (1, 268, 0, 0),		//9
    (1, -210, 0, 0),		//10
    (1, -206, 0, 0),		//11
    (1, -205, 0, 0),		//12
    (1, -79, 0, 0),		//13
    (1, -202, 0, 0),		//14
    (1, -203, 0, 0),		//15
    (1, -207, 0, 0),		//16
    (1, -211, 0, 0),		//17
    (1, -209, 0, 0),		//18
    (1, -213, 0, 0),		//19
    (1, -214, 0, 0),		//20
    (1, -218, 0, 0),		//21
    (1, -217, 0, 0),		//22
    (1, -219, 0, 0),		//23
    (1, -2, 0, 0),		//24
    (1, -116, 0, 0),		//25
    (1, -38, 0, 0),		//26
    (1, -37, 0, 0),		//27
    (1, -64, 0, 0),		//28
    (1, -63, 0, 0),		//29
    (3, 769, 776, 953),		//30
    (3, 769, 776, 965),		//31
    (1, -8, 0, 0),		//32
    (1, 30, 0, 0),		//33
    (1, 25, 0, 0),		//34
    (1, 15, 0, 0),		//35
    (1, 22, 0, 0),		//36
    (1, 54, 0, 0),		//37
    (1, 48, 0, 0),		//38
    (1, 60, 0, 0),		//39
    (1, 64, 0, 0),		//40
    (1, -80, 0, 0),		//41
    (1, -15, 0, 0),		//42
    (1, -48, 0, 0),		//43
    (2, 1410, 1381, 0),		//44
    (1, -7264, 0, 0),		//45
    (2, 817, 104, 0),		//46
    (2, 776, 116, 0),		//47
    (2, 778, 119, 0),		//48
    (2, 778, 121, 0),		//49
    (2, 702, 97, 0),		//50
    (1, 58, 0, 0),		//51
    (1, 8, 0, 0),		//52
    (2, 787, 965, 0),		//53
    (3, 768, 787, 965),		//54
    (3, 769, 787, 965),		//55
    (3, 834, 787, 965),		//56
    (2, 953, 7936, 0),		//57
    (2, 953, 7937, 0),		//58
    (2, 953, 7938, 0),		//59
    (2, 953, 7939, 0),		//60
    (2, 953, 7940, 0),		//61
    (2, 953, 7941, 0),		//62
    (2, 953, 7942, 0),		//63
    (2, 953, 7943, 0),		//64
    (2, 953, 7968, 0),		//65
    (2, 953, 7969, 0),		//66
    (2, 953, 7970, 0),		//67
    (2, 953, 7971, 0),		//68
    (2, 953, 7972, 0),		//69
    (2, 953, 7973, 0),		//70
    (2, 953, 7974, 0),		//71
    (2, 953, 7975, 0),		//72
    (2, 953, 8032, 0),		//73
    (2, 953, 8033, 0),		//74
    (2, 953, 8034, 0),		//75
    (2, 953, 8035, 0),		//76
    (2, 953, 8036, 0),		//77
    (2, 953, 8037, 0),		//78
    (2, 953, 8038, 0),		//79
    (2, 953, 8039, 0),		//80
    (2, 953, 8048, 0),		//81
    (2, 953, 945, 0),		//82
    (2, 953, 940, 0),		//83
    (2, 834, 945, 0),		//84
    (3, 953, 834, 945),		//85
    (1, 74, 0, 0),		//86
    (1, 7173, 0, 0),		//87
    (2, 953, 8052, 0),		//88
    (2, 953, 951, 0),		//89
    (2, 953, 942, 0),		//90
    (2, 834, 951, 0),		//91
    (3, 953, 834, 951),		//92
    (1, 86, 0, 0),		//93
    (3, 768, 776, 953),		//94
    (2, 834, 953, 0),		//95
    (3, 834, 776, 953),		//96
    (1, 100, 0, 0),		//97
    (2, 953, 8060, 0),		//98
    (2, 953, 969, 0),		//99
    (2, 953, 974, 0),		//100
    (2, 834, 969, 0),		//101
    (3, 953, 834, 969),		//102
    (1, 128, 0, 0),		//103
    (1, 126, 0, 0),		//104
    (1, 7517, 0, 0),		//105
    (1, 8383, 0, 0),		//106
    (1, 8262, 0, 0),		//107
    (1, -28, 0, 0),		//108
    (1, -16, 0, 0),		//109
    (1, 10743, 0, 0),		//110
    (1, 3814, 0, 0),		//111
    (1, 10727, 0, 0),		//112
    (1, 10780, 0, 0),		//113
    (1, 10749, 0, 0),		//114
    (1, 10783, 0, 0),		//115
    (1, 10782, 0, 0),		//116
    (1, 10815, 0, 0),		//117
    (1, 42280, 0, 0),		//118
    (1, 42308, 0, 0),		//119
    (2, 102, 102, 0),		//120
    (2, 105, 102, 0),		//121
    (2, 108, 102, 0),		//122
    (3, 105, 102, 102),		//123
    (3, 108, 102, 102),		//124
    (2, 116, 115, 0),		//125
    (2, 1398, 1396, 0),		//126
    (2, 1381, 1396, 0),		//127
    (2, 1387, 1396, 0),		//128
    (2, 1398, 1406, 0),		//129
    (2, 1389, 1396, 0),		//130
    (1, -40, 0, 0)		//131
  );

  UnicodeCharTypeTable: array[0..600] of TUnicodeCharTypeRec = (
    (Script: 103; Category:   0; FoldCase: @tfc[0]),    //   0
    (Script: 103; Category:  30; FoldCase: @tfc[0]),    //   1
    (Script: 103; Category:  22; FoldCase: @tfc[0]),    //   2
    (Script: 103; Category:  24; FoldCase: @tfc[0]),    //   3
    (Script: 103; Category:  23; FoldCase: @tfc[0]),    //   4
    (Script: 103; Category:  19; FoldCase: @tfc[0]),    //   5
    (Script: 103; Category:  26; FoldCase: @tfc[0]),    //   6
    (Script: 103; Category:  18; FoldCase: @tfc[0]),    //   7
    (Script: 103; Category:  14; FoldCase: @tfc[0]),    //   8
    (Script:  48; Category:  10; FoldCase: @tfc[1]),    //   9
    (Script:  48; Category:  10; FoldCase: @tfc[2]),    //  10
    (Script: 103; Category:  25; FoldCase: @tfc[0]),    //  11
    (Script: 103; Category:  17; FoldCase: @tfc[0]),    //  12
    (Script:  48; Category:   6; FoldCase: @tfc[0]),    //  13
    (Script: 103; Category:  27; FoldCase: @tfc[0]),    //  14
    (Script:  48; Category:   8; FoldCase: @tfc[0]),    //  15
    (Script: 103; Category:  21; FoldCase: @tfc[0]),    //  16
    (Script: 103; Category:   1; FoldCase: @tfc[0]),    //  17
    (Script: 103; Category:  16; FoldCase: @tfc[0]),    //  18
    (Script: 103; Category:   6; FoldCase: @tfc[3]),    //  19
    (Script: 103; Category:  20; FoldCase: @tfc[0]),    //  20
    (Script:  48; Category:   6; FoldCase: @tfc[4]),    //  21
    (Script:  48; Category:  10; FoldCase: @tfc[5]),    //  22
    (Script:  48; Category:  10; FoldCase: @tfc[6]),    //  23
    (Script:  48; Category:   6; FoldCase: @tfc[7]),    //  24
    (Script:  48; Category:  10; FoldCase: @tfc[8]),    //  25
    (Script:  48; Category:   6; FoldCase: @tfc[9]),    //  26
    (Script:  48; Category:  10; FoldCase: @tfc[10]),    //  27
    (Script:  48; Category:  10; FoldCase: @tfc[11]),    //  28
    (Script:  48; Category:  10; FoldCase: @tfc[12]),    //  29
    (Script:  48; Category:  10; FoldCase: @tfc[13]),    //  30
    (Script:  48; Category:  10; FoldCase: @tfc[14]),    //  31
    (Script:  48; Category:  10; FoldCase: @tfc[15]),    //  32
    (Script:  48; Category:  10; FoldCase: @tfc[16]),    //  33
    (Script:  48; Category:  10; FoldCase: @tfc[17]),    //  34
    (Script:  48; Category:  10; FoldCase: @tfc[18]),    //  35
    (Script:  48; Category:  10; FoldCase: @tfc[19]),    //  36
    (Script:  48; Category:  10; FoldCase: @tfc[20]),    //  37
    (Script:  48; Category:  10; FoldCase: @tfc[21]),    //  38
    (Script:  48; Category:  10; FoldCase: @tfc[22]),    //  39
    (Script:  48; Category:  10; FoldCase: @tfc[23]),    //  40
    (Script:  48; Category:  10; FoldCase: @tfc[24]),    //  41
    (Script:  48; Category:   7; FoldCase: @tfc[0]),    //  42
    (Script: 103; Category:   7; FoldCase: @tfc[0]),    //  43
    (Script:   9; Category:  25; FoldCase: @tfc[0]),    //  44
    (Script: 102; Category:  13; FoldCase: @tfc[0]),    //  45
    (Script: 102; Category:  13; FoldCase: @tfc[25]),    //  46
    (Script:  29; Category:  10; FoldCase: @tfc[5]),    //  47
    (Script:  29; Category:   6; FoldCase: @tfc[0]),    //  48
    (Script:  29; Category:  25; FoldCase: @tfc[0]),    //  49
    (Script:  29; Category:   7; FoldCase: @tfc[0]),    //  50
    (Script:  29; Category:  10; FoldCase: @tfc[26]),    //  51
    (Script:  29; Category:  10; FoldCase: @tfc[27]),    //  52
    (Script:  29; Category:  10; FoldCase: @tfc[28]),    //  53
    (Script:  29; Category:  10; FoldCase: @tfc[29]),    //  54
    (Script:  29; Category:   6; FoldCase: @tfc[30]),    //  55
    (Script:  29; Category:  10; FoldCase: @tfc[1]),    //  56
    (Script:  29; Category:   6; FoldCase: @tfc[31]),    //  57
    (Script:  29; Category:   6; FoldCase: @tfc[5]),    //  58
    (Script:  29; Category:  10; FoldCase: @tfc[32]),    //  59
    (Script:  29; Category:   6; FoldCase: @tfc[33]),    //  60
    (Script:  29; Category:   6; FoldCase: @tfc[34]),    //  61
    (Script:  29; Category:  10; FoldCase: @tfc[0]),    //  62
    (Script:  29; Category:   6; FoldCase: @tfc[35]),    //  63
    (Script:  29; Category:   6; FoldCase: @tfc[36]),    //  64
    (Script:  19; Category:  10; FoldCase: @tfc[5]),    //  65
    (Script:  19; Category:   6; FoldCase: @tfc[0]),    //  66
    (Script:  29; Category:   6; FoldCase: @tfc[37]),    //  67
    (Script:  29; Category:   6; FoldCase: @tfc[38]),    //  68
    (Script:  29; Category:  10; FoldCase: @tfc[39]),    //  69
    (Script:  29; Category:   6; FoldCase: @tfc[40]),    //  70
    (Script:  29; Category:  26; FoldCase: @tfc[0]),    //  71
    (Script:  21; Category:  10; FoldCase: @tfc[41]),    //  72
    (Script:  21; Category:  10; FoldCase: @tfc[1]),    //  73
    (Script:  21; Category:   6; FoldCase: @tfc[0]),    //  74
    (Script:  21; Category:  10; FoldCase: @tfc[5]),    //  75
    (Script:  21; Category:  27; FoldCase: @tfc[0]),    //  76
    (Script:  21; Category:  13; FoldCase: @tfc[0]),    //  77
    (Script:  21; Category:  12; FoldCase: @tfc[0]),    //  78
    (Script:  21; Category:  10; FoldCase: @tfc[42]),    //  79
    (Script:   3; Category:  10; FoldCase: @tfc[43]),    //  80
    (Script:   3; Category:   7; FoldCase: @tfc[0]),    //  81
    (Script:   3; Category:  22; FoldCase: @tfc[0]),    //  82
    (Script:   3; Category:   6; FoldCase: @tfc[0]),    //  83
    (Script:   3; Category:   6; FoldCase: @tfc[44]),    //  84
    (Script:   3; Category:  18; FoldCase: @tfc[0]),    //  85
    (Script:   3; Category:  24; FoldCase: @tfc[0]),    //  86
    (Script:  35; Category:  13; FoldCase: @tfc[0]),    //  87
    (Script:  35; Category:  18; FoldCase: @tfc[0]),    //  88
    (Script:  35; Category:  22; FoldCase: @tfc[0]),    //  89
    (Script:  35; Category:   8; FoldCase: @tfc[0]),    //  90
    (Script:   1; Category:   1; FoldCase: @tfc[0]),    //  91
    (Script:   1; Category:  26; FoldCase: @tfc[0]),    //  92
    (Script:   1; Category:  22; FoldCase: @tfc[0]),    //  93
    (Script:   1; Category:  24; FoldCase: @tfc[0]),    //  94
    (Script:   1; Category:  27; FoldCase: @tfc[0]),    //  95
    (Script:   1; Category:  13; FoldCase: @tfc[0]),    //  96
    (Script:   1; Category:   8; FoldCase: @tfc[0]),    //  97
    (Script:   1; Category:   7; FoldCase: @tfc[0]),    //  98
    (Script:   1; Category:  14; FoldCase: @tfc[0]),    //  99
    (Script:  84; Category:  22; FoldCase: @tfc[0]),    // 100
    (Script:  84; Category:   1; FoldCase: @tfc[0]),    // 101
    (Script:  84; Category:   8; FoldCase: @tfc[0]),    // 102
    (Script:  84; Category:  13; FoldCase: @tfc[0]),    // 103
    (Script:  94; Category:   8; FoldCase: @tfc[0]),    // 104
    (Script:  94; Category:  13; FoldCase: @tfc[0]),    // 105
    (Script:  62; Category:  14; FoldCase: @tfc[0]),    // 106
    (Script:  62; Category:   8; FoldCase: @tfc[0]),    // 107
    (Script:  62; Category:  13; FoldCase: @tfc[0]),    // 108
    (Script:  62; Category:   7; FoldCase: @tfc[0]),    // 109
    (Script:  62; Category:  27; FoldCase: @tfc[0]),    // 110
    (Script:  62; Category:  22; FoldCase: @tfc[0]),    // 111
    (Script:  75; Category:   8; FoldCase: @tfc[0]),    // 112
    (Script:  75; Category:  13; FoldCase: @tfc[0]),    // 113
    (Script:  75; Category:   7; FoldCase: @tfc[0]),    // 114
    (Script:  75; Category:  22; FoldCase: @tfc[0]),    // 115
    (Script:  55; Category:   8; FoldCase: @tfc[0]),    // 116
    (Script:  55; Category:  13; FoldCase: @tfc[0]),    // 117
    (Script:  55; Category:  22; FoldCase: @tfc[0]),    // 118
    (Script:  22; Category:  13; FoldCase: @tfc[0]),    // 119
    (Script:  22; Category:  11; FoldCase: @tfc[0]),    // 120
    (Script:  22; Category:   8; FoldCase: @tfc[0]),    // 121
    (Script:  22; Category:  14; FoldCase: @tfc[0]),    // 122
    (Script:  22; Category:  22; FoldCase: @tfc[0]),    // 123
    (Script:  22; Category:   7; FoldCase: @tfc[0]),    // 124
    (Script:   8; Category:  13; FoldCase: @tfc[0]),    // 125
    (Script:   8; Category:  11; FoldCase: @tfc[0]),    // 126
    (Script:   8; Category:   8; FoldCase: @tfc[0]),    // 127
    (Script:   8; Category:  14; FoldCase: @tfc[0]),    // 128
    (Script:   8; Category:  24; FoldCase: @tfc[0]),    // 129
    (Script:   8; Category:  16; FoldCase: @tfc[0]),    // 130
    (Script:   8; Category:  27; FoldCase: @tfc[0]),    // 131
    (Script:  31; Category:  13; FoldCase: @tfc[0]),    // 132
    (Script:  31; Category:  11; FoldCase: @tfc[0]),    // 133
    (Script:  31; Category:   8; FoldCase: @tfc[0]),    // 134
    (Script:  31; Category:  14; FoldCase: @tfc[0]),    // 135
    (Script:  30; Category:  13; FoldCase: @tfc[0]),    // 136
    (Script:  30; Category:  11; FoldCase: @tfc[0]),    // 137
    (Script:  30; Category:   8; FoldCase: @tfc[0]),    // 138
    (Script:  30; Category:  14; FoldCase: @tfc[0]),    // 139
    (Script:  30; Category:  22; FoldCase: @tfc[0]),    // 140
    (Script:  30; Category:  24; FoldCase: @tfc[0]),    // 141
    (Script:  66; Category:  13; FoldCase: @tfc[0]),    // 142
    (Script:  66; Category:  11; FoldCase: @tfc[0]),    // 143
    (Script:  66; Category:   8; FoldCase: @tfc[0]),    // 144
    (Script:  66; Category:  14; FoldCase: @tfc[0]),    // 145
    (Script:  66; Category:  27; FoldCase: @tfc[0]),    // 146
    (Script:  66; Category:  16; FoldCase: @tfc[0]),    // 147
    (Script:  89; Category:  13; FoldCase: @tfc[0]),    // 148
    (Script:  89; Category:   8; FoldCase: @tfc[0]),    // 149
    (Script:  89; Category:  11; FoldCase: @tfc[0]),    // 150
    (Script:  89; Category:  14; FoldCase: @tfc[0]),    // 151
    (Script:  89; Category:  16; FoldCase: @tfc[0]),    // 152
    (Script:  89; Category:  27; FoldCase: @tfc[0]),    // 153
    (Script:  89; Category:  24; FoldCase: @tfc[0]),    // 154
    (Script:  91; Category:  11; FoldCase: @tfc[0]),    // 155
    (Script:  91; Category:   8; FoldCase: @tfc[0]),    // 156
    (Script:  91; Category:  13; FoldCase: @tfc[0]),    // 157
    (Script:  91; Category:  14; FoldCase: @tfc[0]),    // 158
    (Script:  91; Category:  16; FoldCase: @tfc[0]),    // 159
    (Script:  91; Category:  27; FoldCase: @tfc[0]),    // 160
    (Script:  44; Category:  11; FoldCase: @tfc[0]),    // 161
    (Script:  44; Category:   8; FoldCase: @tfc[0]),    // 162
    (Script:  44; Category:  13; FoldCase: @tfc[0]),    // 163
    (Script:  44; Category:  14; FoldCase: @tfc[0]),    // 164
    (Script:  58; Category:  11; FoldCase: @tfc[0]),    // 165
    (Script:  58; Category:   8; FoldCase: @tfc[0]),    // 166
    (Script:  58; Category:  13; FoldCase: @tfc[0]),    // 167
    (Script:  58; Category:  14; FoldCase: @tfc[0]),    // 168
    (Script:  58; Category:  16; FoldCase: @tfc[0]),    // 169
    (Script:  58; Category:  27; FoldCase: @tfc[0]),    // 170
    (Script:  80; Category:  11; FoldCase: @tfc[0]),    // 171
    (Script:  80; Category:   8; FoldCase: @tfc[0]),    // 172
    (Script:  80; Category:  13; FoldCase: @tfc[0]),    // 173
    (Script:  80; Category:  22; FoldCase: @tfc[0]),    // 174
    (Script:  95; Category:   8; FoldCase: @tfc[0]),    // 175
    (Script:  95; Category:  13; FoldCase: @tfc[0]),    // 176
    (Script:  95; Category:   7; FoldCase: @tfc[0]),    // 177
    (Script:  95; Category:  22; FoldCase: @tfc[0]),    // 178
    (Script:  95; Category:  14; FoldCase: @tfc[0]),    // 179
    (Script:  47; Category:   8; FoldCase: @tfc[0]),    // 180
    (Script:  47; Category:  13; FoldCase: @tfc[0]),    // 181
    (Script:  47; Category:   7; FoldCase: @tfc[0]),    // 182
    (Script:  47; Category:  14; FoldCase: @tfc[0]),    // 183
    (Script:  96; Category:   8; FoldCase: @tfc[0]),    // 184
    (Script:  96; Category:  27; FoldCase: @tfc[0]),    // 185
    (Script:  96; Category:  22; FoldCase: @tfc[0]),    // 186
    (Script:  96; Category:  13; FoldCase: @tfc[0]),    // 187
    (Script:  96; Category:  14; FoldCase: @tfc[0]),    // 188
    (Script:  96; Category:  16; FoldCase: @tfc[0]),    // 189
    (Script:  96; Category:  23; FoldCase: @tfc[0]),    // 190
    (Script:  96; Category:  19; FoldCase: @tfc[0]),    // 191
    (Script:  96; Category:  11; FoldCase: @tfc[0]),    // 192
    (Script:  61; Category:   8; FoldCase: @tfc[0]),    // 193
    (Script:  61; Category:  11; FoldCase: @tfc[0]),    // 194
    (Script:  61; Category:  13; FoldCase: @tfc[0]),    // 195
    (Script:  61; Category:  14; FoldCase: @tfc[0]),    // 196
    (Script:  61; Category:  22; FoldCase: @tfc[0]),    // 197
    (Script:  61; Category:  27; FoldCase: @tfc[0]),    // 198
    (Script:  26; Category:  10; FoldCase: @tfc[45]),    // 199
    (Script:  26; Category:   8; FoldCase: @tfc[0]),    // 200
    (Script:  26; Category:   7; FoldCase: @tfc[0]),    // 201
    (Script:  32; Category:   8; FoldCase: @tfc[0]),    // 202
    (Script:  25; Category:   8; FoldCase: @tfc[0]),    // 203
    (Script:  25; Category:  13; FoldCase: @tfc[0]),    // 204
    (Script:  25; Category:  22; FoldCase: @tfc[0]),    // 205
    (Script:  25; Category:  16; FoldCase: @tfc[0]),    // 206
    (Script:  25; Category:  27; FoldCase: @tfc[0]),    // 207
    (Script:  18; Category:   8; FoldCase: @tfc[0]),    // 208
    (Script:  15; Category:  18; FoldCase: @tfc[0]),    // 209
    (Script:  15; Category:   8; FoldCase: @tfc[0]),    // 210
    (Script:  15; Category:  22; FoldCase: @tfc[0]),    // 211
    (Script:  63; Category:  30; FoldCase: @tfc[0]),    // 212
    (Script:  63; Category:   8; FoldCase: @tfc[0]),    // 213
    (Script:  63; Category:  23; FoldCase: @tfc[0]),    // 214
    (Script:  63; Category:  19; FoldCase: @tfc[0]),    // 215
    (Script:  74; Category:   8; FoldCase: @tfc[0]),    // 216
    (Script:  74; Category:  15; FoldCase: @tfc[0]),    // 217
    (Script:  93; Category:   8; FoldCase: @tfc[0]),    // 218
    (Script:  93; Category:  13; FoldCase: @tfc[0]),    // 219
    (Script:  34; Category:   8; FoldCase: @tfc[0]),    // 220
    (Script:  34; Category:  13; FoldCase: @tfc[0]),    // 221
    (Script:  13; Category:   8; FoldCase: @tfc[0]),    // 222
    (Script:  13; Category:  13; FoldCase: @tfc[0]),    // 223
    (Script:  85; Category:   8; FoldCase: @tfc[0]),    // 224
    (Script:  85; Category:  13; FoldCase: @tfc[0]),    // 225
    (Script:  43; Category:   8; FoldCase: @tfc[0]),    // 226
    (Script:  43; Category:  13; FoldCase: @tfc[0]),    // 227
    (Script:  43; Category:  11; FoldCase: @tfc[0]),    // 228
    (Script:  43; Category:  22; FoldCase: @tfc[0]),    // 229
    (Script:  43; Category:   7; FoldCase: @tfc[0]),    // 230
    (Script:  43; Category:  24; FoldCase: @tfc[0]),    // 231
    (Script:  43; Category:  14; FoldCase: @tfc[0]),    // 232
    (Script:  43; Category:  16; FoldCase: @tfc[0]),    // 233
    (Script:  59; Category:  22; FoldCase: @tfc[0]),    // 234
    (Script:  59; Category:  18; FoldCase: @tfc[0]),    // 235
    (Script:  59; Category:  13; FoldCase: @tfc[0]),    // 236
    (Script:  59; Category:  30; FoldCase: @tfc[0]),    // 237
    (Script:  59; Category:  14; FoldCase: @tfc[0]),    // 238
    (Script:  59; Category:   8; FoldCase: @tfc[0]),    // 239
    (Script:  59; Category:   7; FoldCase: @tfc[0]),    // 240
    (Script:  50; Category:   8; FoldCase: @tfc[0]),    // 241
    (Script:  50; Category:  13; FoldCase: @tfc[0]),    // 242
    (Script:  50; Category:  11; FoldCase: @tfc[0]),    // 243
    (Script:  50; Category:  27; FoldCase: @tfc[0]),    // 244
    (Script:  50; Category:  22; FoldCase: @tfc[0]),    // 245
    (Script:  50; Category:  14; FoldCase: @tfc[0]),    // 246
    (Script:  87; Category:   8; FoldCase: @tfc[0]),    // 247
    (Script:  88; Category:   8; FoldCase: @tfc[0]),    // 248
    (Script:  88; Category:  11; FoldCase: @tfc[0]),    // 249
    (Script:  88; Category:  14; FoldCase: @tfc[0]),    // 250
    (Script:  88; Category:  16; FoldCase: @tfc[0]),    // 251
    (Script:  88; Category:  27; FoldCase: @tfc[0]),    // 252
    (Script:  43; Category:  27; FoldCase: @tfc[0]),    // 253
    (Script:  12; Category:   8; FoldCase: @tfc[0]),    // 254
    (Script:  12; Category:  13; FoldCase: @tfc[0]),    // 255
    (Script:  12; Category:  11; FoldCase: @tfc[0]),    // 256
    (Script:  12; Category:  22; FoldCase: @tfc[0]),    // 257
    (Script:  46; Category:   8; FoldCase: @tfc[0]),    // 258
    (Script:  46; Category:  11; FoldCase: @tfc[0]),    // 259
    (Script:  46; Category:  13; FoldCase: @tfc[0]),    // 260
    (Script:  46; Category:  14; FoldCase: @tfc[0]),    // 261
    (Script:  46; Category:  22; FoldCase: @tfc[0]),    // 262
    (Script:  46; Category:   7; FoldCase: @tfc[0]),    // 263
    (Script:   5; Category:  13; FoldCase: @tfc[0]),    // 264
    (Script:   5; Category:  11; FoldCase: @tfc[0]),    // 265
    (Script:   5; Category:   8; FoldCase: @tfc[0]),    // 266
    (Script:   5; Category:  14; FoldCase: @tfc[0]),    // 267
    (Script:   5; Category:  22; FoldCase: @tfc[0]),    // 268
    (Script:   5; Category:  27; FoldCase: @tfc[0]),    // 269
    (Script:  82; Category:  13; FoldCase: @tfc[0]),    // 270
    (Script:  82; Category:  11; FoldCase: @tfc[0]),    // 271
    (Script:  82; Category:   8; FoldCase: @tfc[0]),    // 272
    (Script:  82; Category:  14; FoldCase: @tfc[0]),    // 273
    (Script:   7; Category:   8; FoldCase: @tfc[0]),    // 274
    (Script:   7; Category:  13; FoldCase: @tfc[0]),    // 275
    (Script:   7; Category:  11; FoldCase: @tfc[0]),    // 276
    (Script:   7; Category:  22; FoldCase: @tfc[0]),    // 277
    (Script:  49; Category:   8; FoldCase: @tfc[0]),    // 278
    (Script:  49; Category:  11; FoldCase: @tfc[0]),    // 279
    (Script:  49; Category:  13; FoldCase: @tfc[0]),    // 280
    (Script:  49; Category:  22; FoldCase: @tfc[0]),    // 281
    (Script:  49; Category:  14; FoldCase: @tfc[0]),    // 282
    (Script:  64; Category:  14; FoldCase: @tfc[0]),    // 283
    (Script:  64; Category:   8; FoldCase: @tfc[0]),    // 284
    (Script:  64; Category:   7; FoldCase: @tfc[0]),    // 285
    (Script:  64; Category:  22; FoldCase: @tfc[0]),    // 286
    (Script:  82; Category:  22; FoldCase: @tfc[0]),    // 287
    (Script: 103; Category:  11; FoldCase: @tfc[0]),    // 288
    (Script: 103; Category:   8; FoldCase: @tfc[0]),    // 289
    (Script:  21; Category:   7; FoldCase: @tfc[0]),    // 290
    (Script:  48; Category:   6; FoldCase: @tfc[46]),    // 291
    (Script:  48; Category:   6; FoldCase: @tfc[47]),    // 292
    (Script:  48; Category:   6; FoldCase: @tfc[48]),    // 293
    (Script:  48; Category:   6; FoldCase: @tfc[49]),    // 294
    (Script:  48; Category:   6; FoldCase: @tfc[50]),    // 295
    (Script:  48; Category:   6; FoldCase: @tfc[51]),    // 296
    (Script:  48; Category:  10; FoldCase: @tfc[4]),    // 297
    (Script:  29; Category:  10; FoldCase: @tfc[52]),    // 298
    (Script:  29; Category:   6; FoldCase: @tfc[53]),    // 299
    (Script:  29; Category:   6; FoldCase: @tfc[54]),    // 300
    (Script:  29; Category:   6; FoldCase: @tfc[55]),    // 301
    (Script:  29; Category:   6; FoldCase: @tfc[56]),    // 302
    (Script:  29; Category:   6; FoldCase: @tfc[57]),    // 303
    (Script:  29; Category:   6; FoldCase: @tfc[58]),    // 304
    (Script:  29; Category:   6; FoldCase: @tfc[59]),    // 305
    (Script:  29; Category:   6; FoldCase: @tfc[60]),    // 306
    (Script:  29; Category:   6; FoldCase: @tfc[61]),    // 307
    (Script:  29; Category:   6; FoldCase: @tfc[62]),    // 308
    (Script:  29; Category:   6; FoldCase: @tfc[63]),    // 309
    (Script:  29; Category:   6; FoldCase: @tfc[64]),    // 310
    (Script:  29; Category:   9; FoldCase: @tfc[57]),    // 311
    (Script:  29; Category:   9; FoldCase: @tfc[58]),    // 312
    (Script:  29; Category:   9; FoldCase: @tfc[59]),    // 313
    (Script:  29; Category:   9; FoldCase: @tfc[60]),    // 314
    (Script:  29; Category:   9; FoldCase: @tfc[61]),    // 315
    (Script:  29; Category:   9; FoldCase: @tfc[62]),    // 316
    (Script:  29; Category:   9; FoldCase: @tfc[63]),    // 317
    (Script:  29; Category:   9; FoldCase: @tfc[64]),    // 318
    (Script:  29; Category:   6; FoldCase: @tfc[65]),    // 319
    (Script:  29; Category:   6; FoldCase: @tfc[66]),    // 320
    (Script:  29; Category:   6; FoldCase: @tfc[67]),    // 321
    (Script:  29; Category:   6; FoldCase: @tfc[68]),    // 322
    (Script:  29; Category:   6; FoldCase: @tfc[69]),    // 323
    (Script:  29; Category:   6; FoldCase: @tfc[70]),    // 324
    (Script:  29; Category:   6; FoldCase: @tfc[71]),    // 325
    (Script:  29; Category:   6; FoldCase: @tfc[72]),    // 326
    (Script:  29; Category:   9; FoldCase: @tfc[65]),    // 327
    (Script:  29; Category:   9; FoldCase: @tfc[66]),    // 328
    (Script:  29; Category:   9; FoldCase: @tfc[67]),    // 329
    (Script:  29; Category:   9; FoldCase: @tfc[68]),    // 330
    (Script:  29; Category:   9; FoldCase: @tfc[69]),    // 331
    (Script:  29; Category:   9; FoldCase: @tfc[70]),    // 332
    (Script:  29; Category:   9; FoldCase: @tfc[71]),    // 333
    (Script:  29; Category:   9; FoldCase: @tfc[72]),    // 334
    (Script:  29; Category:   6; FoldCase: @tfc[73]),    // 335
    (Script:  29; Category:   6; FoldCase: @tfc[74]),    // 336
    (Script:  29; Category:   6; FoldCase: @tfc[75]),    // 337
    (Script:  29; Category:   6; FoldCase: @tfc[76]),    // 338
    (Script:  29; Category:   6; FoldCase: @tfc[77]),    // 339
    (Script:  29; Category:   6; FoldCase: @tfc[78]),    // 340
    (Script:  29; Category:   6; FoldCase: @tfc[79]),    // 341
    (Script:  29; Category:   6; FoldCase: @tfc[80]),    // 342
    (Script:  29; Category:   9; FoldCase: @tfc[73]),    // 343
    (Script:  29; Category:   9; FoldCase: @tfc[74]),    // 344
    (Script:  29; Category:   9; FoldCase: @tfc[75]),    // 345
    (Script:  29; Category:   9; FoldCase: @tfc[76]),    // 346
    (Script:  29; Category:   9; FoldCase: @tfc[0]),    // 347
    (Script:  29; Category:   9; FoldCase: @tfc[78]),    // 348
    (Script:  29; Category:   9; FoldCase: @tfc[79]),    // 349
    (Script:  29; Category:   9; FoldCase: @tfc[80]),    // 350
    (Script:  29; Category:   6; FoldCase: @tfc[81]),    // 351
    (Script:  29; Category:   6; FoldCase: @tfc[82]),    // 352
    (Script:  29; Category:   6; FoldCase: @tfc[83]),    // 353
    (Script:  29; Category:   6; FoldCase: @tfc[84]),    // 354
    (Script:  29; Category:   6; FoldCase: @tfc[85]),    // 355
    (Script:  29; Category:  10; FoldCase: @tfc[86]),    // 356
    (Script:  29; Category:   9; FoldCase: @tfc[82]),    // 357
    (Script:  29; Category:   6; FoldCase: @tfc[87]),    // 358
    (Script:  29; Category:   6; FoldCase: @tfc[88]),    // 359
    (Script:  29; Category:   6; FoldCase: @tfc[89]),    // 360
    (Script:  29; Category:   6; FoldCase: @tfc[90]),    // 361
    (Script:  29; Category:   6; FoldCase: @tfc[91]),    // 362
    (Script:  29; Category:   6; FoldCase: @tfc[92]),    // 363
    (Script:  29; Category:  10; FoldCase: @tfc[93]),    // 364
    (Script:  29; Category:   9; FoldCase: @tfc[89]),    // 365
    (Script:  29; Category:   6; FoldCase: @tfc[94]),    // 366
    (Script:  29; Category:   6; FoldCase: @tfc[95]),    // 367
    (Script:  29; Category:   6; FoldCase: @tfc[96]),    // 368
    (Script:  29; Category:  10; FoldCase: @tfc[97]),    // 369
    (Script:  29; Category:   6; FoldCase: @tfc[98]),    // 370
    (Script:  29; Category:   6; FoldCase: @tfc[99]),    // 371
    (Script:  29; Category:   6; FoldCase: @tfc[100]),    // 372
    (Script:  29; Category:   6; FoldCase: @tfc[101]),    // 373
    (Script:  29; Category:   6; FoldCase: @tfc[102]),    // 374
    (Script:  29; Category:  10; FoldCase: @tfc[103]),    // 375
    (Script:  29; Category:  10; FoldCase: @tfc[104]),    // 376
    (Script:  29; Category:   9; FoldCase: @tfc[99]),    // 377
    (Script: 102; Category:   1; FoldCase: @tfc[0]),    // 378
    (Script: 103; Category:  28; FoldCase: @tfc[0]),    // 379
    (Script: 103; Category:  29; FoldCase: @tfc[0]),    // 380
    (Script: 102; Category:  12; FoldCase: @tfc[0]),    // 381
    (Script: 103; Category:  10; FoldCase: @tfc[0]),    // 382
    (Script: 103; Category:   6; FoldCase: @tfc[0]),    // 383
    (Script:  29; Category:  10; FoldCase: @tfc[105]),    // 384
    (Script:  48; Category:  10; FoldCase: @tfc[106]),    // 385
    (Script:  48; Category:  10; FoldCase: @tfc[107]),    // 386
    (Script:  48; Category:  10; FoldCase: @tfc[108]),    // 387
    (Script:  48; Category:  15; FoldCase: @tfc[109]),    // 388
    (Script:  48; Category:  15; FoldCase: @tfc[0]),    // 389
    (Script:  11; Category:  27; FoldCase: @tfc[0]),    // 390
    (Script:  27; Category:  10; FoldCase: @tfc[43]),    // 391
    (Script:  27; Category:   6; FoldCase: @tfc[0]),    // 392
    (Script:  48; Category:  10; FoldCase: @tfc[110]),    // 393
    (Script:  48; Category:  10; FoldCase: @tfc[111]),    // 394
    (Script:  48; Category:  10; FoldCase: @tfc[112]),    // 395
    (Script:  48; Category:  10; FoldCase: @tfc[113]),    // 396
    (Script:  48; Category:  10; FoldCase: @tfc[114]),    // 397
    (Script:  48; Category:  10; FoldCase: @tfc[115]),    // 398
    (Script:  48; Category:  10; FoldCase: @tfc[116]),    // 399
    (Script:  48; Category:  10; FoldCase: @tfc[117]),    // 400
    (Script:  19; Category:  27; FoldCase: @tfc[0]),    // 401
    (Script:  19; Category:  13; FoldCase: @tfc[0]),    // 402
    (Script:  19; Category:  22; FoldCase: @tfc[0]),    // 403
    (Script:  19; Category:  16; FoldCase: @tfc[0]),    // 404
    (Script:  26; Category:   6; FoldCase: @tfc[0]),    // 405
    (Script:  92; Category:   8; FoldCase: @tfc[0]),    // 406
    (Script:  92; Category:   7; FoldCase: @tfc[0]),    // 407
    (Script:  92; Category:  22; FoldCase: @tfc[0]),    // 408
    (Script:  92; Category:  13; FoldCase: @tfc[0]),    // 409
    (Script:  33; Category:  27; FoldCase: @tfc[0]),    // 410
    (Script:  33; Category:   7; FoldCase: @tfc[0]),    // 411
    (Script:  33; Category:  15; FoldCase: @tfc[0]),    // 412
    (Script:  32; Category:  11; FoldCase: @tfc[0]),    // 413
    (Script:  36; Category:   8; FoldCase: @tfc[0]),    // 414
    (Script:  36; Category:   7; FoldCase: @tfc[0]),    // 415
    (Script:  41; Category:   8; FoldCase: @tfc[0]),    // 416
    (Script:  41; Category:   7; FoldCase: @tfc[0]),    // 417
    (Script:   9; Category:   8; FoldCase: @tfc[0]),    // 418
    (Script:  32; Category:  27; FoldCase: @tfc[0]),    // 419
    (Script:  41; Category:  27; FoldCase: @tfc[0]),    // 420
    (Script:  33; Category:   8; FoldCase: @tfc[0]),    // 421
    (Script: 101; Category:   8; FoldCase: @tfc[0]),    // 422
    (Script: 101; Category:   7; FoldCase: @tfc[0]),    // 423
    (Script: 101; Category:  27; FoldCase: @tfc[0]),    // 424
    (Script:  52; Category:   8; FoldCase: @tfc[0]),    // 425
    (Script:  52; Category:   7; FoldCase: @tfc[0]),    // 426
    (Script:  52; Category:  22; FoldCase: @tfc[0]),    // 427
    (Script:  98; Category:   8; FoldCase: @tfc[0]),    // 428
    (Script:  98; Category:   7; FoldCase: @tfc[0]),    // 429
    (Script:  98; Category:  22; FoldCase: @tfc[0]),    // 430
    (Script:  98; Category:  14; FoldCase: @tfc[0]),    // 431
    (Script:  21; Category:   8; FoldCase: @tfc[0]),    // 432
    (Script:  21; Category:  22; FoldCase: @tfc[0]),    // 433
    (Script:   6; Category:   8; FoldCase: @tfc[0]),    // 434
    (Script:   6; Category:  15; FoldCase: @tfc[0]),    // 435
    (Script:   6; Category:  13; FoldCase: @tfc[0]),    // 436
    (Script:   6; Category:  22; FoldCase: @tfc[0]),    // 437
    (Script:  48; Category:  10; FoldCase: @tfc[118]),    // 438
    (Script:  48; Category:  10; FoldCase: @tfc[119]),    // 439
    (Script:  83; Category:   8; FoldCase: @tfc[0]),    // 440
    (Script:  83; Category:  13; FoldCase: @tfc[0]),    // 441
    (Script:  83; Category:  11; FoldCase: @tfc[0]),    // 442
    (Script:  83; Category:  27; FoldCase: @tfc[0]),    // 443
    (Script:  68; Category:   8; FoldCase: @tfc[0]),    // 444
    (Script:  68; Category:  22; FoldCase: @tfc[0]),    // 445
    (Script:  77; Category:  11; FoldCase: @tfc[0]),    // 446
    (Script:  77; Category:   8; FoldCase: @tfc[0]),    // 447
    (Script:  77; Category:  13; FoldCase: @tfc[0]),    // 448
    (Script:  77; Category:  22; FoldCase: @tfc[0]),    // 449
    (Script:  77; Category:  14; FoldCase: @tfc[0]),    // 450
    (Script:  40; Category:  14; FoldCase: @tfc[0]),    // 451
    (Script:  40; Category:   8; FoldCase: @tfc[0]),    // 452
    (Script:  40; Category:  13; FoldCase: @tfc[0]),    // 453
    (Script:  40; Category:  22; FoldCase: @tfc[0]),    // 454
    (Script:  73; Category:   8; FoldCase: @tfc[0]),    // 455
    (Script:  73; Category:  13; FoldCase: @tfc[0]),    // 456
    (Script:  73; Category:  11; FoldCase: @tfc[0]),    // 457
    (Script:  73; Category:  22; FoldCase: @tfc[0]),    // 458
    (Script:  39; Category:  13; FoldCase: @tfc[0]),    // 459
    (Script:  39; Category:  11; FoldCase: @tfc[0]),    // 460
    (Script:  39; Category:   8; FoldCase: @tfc[0]),    // 461
    (Script:  39; Category:  22; FoldCase: @tfc[0]),    // 462
    (Script:  39; Category:   7; FoldCase: @tfc[0]),    // 463
    (Script:  39; Category:  14; FoldCase: @tfc[0]),    // 464
    (Script:  17; Category:   8; FoldCase: @tfc[0]),    // 465
    (Script:  17; Category:  13; FoldCase: @tfc[0]),    // 466
    (Script:  17; Category:  11; FoldCase: @tfc[0]),    // 467
    (Script:  17; Category:  14; FoldCase: @tfc[0]),    // 468
    (Script:  17; Category:  22; FoldCase: @tfc[0]),    // 469
    (Script:  61; Category:   7; FoldCase: @tfc[0]),    // 470
    (Script:  90; Category:   8; FoldCase: @tfc[0]),    // 471
    (Script:  90; Category:  13; FoldCase: @tfc[0]),    // 472
    (Script:  90; Category:   7; FoldCase: @tfc[0]),    // 473
    (Script:  90; Category:  22; FoldCase: @tfc[0]),    // 474
    (Script:  60; Category:   8; FoldCase: @tfc[0]),    // 475
    (Script:  60; Category:  11; FoldCase: @tfc[0]),    // 476
    (Script:  60; Category:  13; FoldCase: @tfc[0]),    // 477
    (Script:  60; Category:  22; FoldCase: @tfc[0]),    // 478
    (Script:  60; Category:   7; FoldCase: @tfc[0]),    // 479
    (Script:  60; Category:  14; FoldCase: @tfc[0]),    // 480
    (Script:  48; Category:   6; FoldCase: @tfc[120]),    // 481
    (Script:  48; Category:   6; FoldCase: @tfc[121]),    // 482
    (Script:  48; Category:   6; FoldCase: @tfc[122]),    // 483
    (Script:  48; Category:   6; FoldCase: @tfc[123]),    // 484
    (Script:  48; Category:   6; FoldCase: @tfc[124]),    // 485
    (Script:  48; Category:   6; FoldCase: @tfc[125]),    // 486
    (Script:   3; Category:   6; FoldCase: @tfc[126]),    // 487
    (Script:   3; Category:   6; FoldCase: @tfc[127]),    // 488
    (Script:   3; Category:   6; FoldCase: @tfc[128]),    // 489
    (Script:   3; Category:   6; FoldCase: @tfc[129]),    // 490
    (Script:   3; Category:   6; FoldCase: @tfc[130]),    // 491
    (Script:  35; Category:  26; FoldCase: @tfc[0]),    // 492
    (Script:   1; Category:  25; FoldCase: @tfc[0]),    // 493
    (Script:  51; Category:   8; FoldCase: @tfc[0]),    // 494
    (Script:  29; Category:  15; FoldCase: @tfc[0]),    // 495
    (Script:  29; Category:  16; FoldCase: @tfc[0]),    // 496
    (Script:  29; Category:  27; FoldCase: @tfc[0]),    // 497
    (Script:  53; Category:   8; FoldCase: @tfc[0]),    // 498
    (Script:  16; Category:   8; FoldCase: @tfc[0]),    // 499
    (Script:  38; Category:   8; FoldCase: @tfc[0]),    // 500
    (Script:  38; Category:  16; FoldCase: @tfc[0]),    // 501
    (Script:  28; Category:   8; FoldCase: @tfc[0]),    // 502
    (Script:  28; Category:  15; FoldCase: @tfc[0]),    // 503
    (Script:  97; Category:   8; FoldCase: @tfc[0]),    // 504
    (Script:  97; Category:  22; FoldCase: @tfc[0]),    // 505
    (Script:  99; Category:   8; FoldCase: @tfc[0]),    // 506
    (Script:  99; Category:  22; FoldCase: @tfc[0]),    // 507
    (Script:  99; Category:  15; FoldCase: @tfc[0]),    // 508
    (Script:  23; Category:  10; FoldCase: @tfc[131]),    // 509
    (Script:  23; Category:   6; FoldCase: @tfc[0]),    // 510
    (Script:  78; Category:   8; FoldCase: @tfc[0]),    // 511
    (Script:  67; Category:   8; FoldCase: @tfc[0]),    // 512
    (Script:  67; Category:  14; FoldCase: @tfc[0]),    // 513
    (Script:  20; Category:   8; FoldCase: @tfc[0]),    // 514
    (Script:   2; Category:   8; FoldCase: @tfc[0]),    // 515
    (Script:   2; Category:  22; FoldCase: @tfc[0]),    // 516
    (Script:   2; Category:  16; FoldCase: @tfc[0]),    // 517
    (Script:  70; Category:   8; FoldCase: @tfc[0]),    // 518
    (Script:  70; Category:  16; FoldCase: @tfc[0]),    // 519
    (Script:  70; Category:  22; FoldCase: @tfc[0]),    // 520
    (Script:  54; Category:   8; FoldCase: @tfc[0]),    // 521
    (Script:  54; Category:  22; FoldCase: @tfc[0]),    // 522
    (Script:  57; Category:   8; FoldCase: @tfc[0]),    // 523
    (Script:  56; Category:   8; FoldCase: @tfc[0]),    // 524
    (Script:  42; Category:   8; FoldCase: @tfc[0]),    // 525
    (Script:  42; Category:  13; FoldCase: @tfc[0]),    // 526
    (Script:  42; Category:  16; FoldCase: @tfc[0]),    // 527
    (Script:  42; Category:  22; FoldCase: @tfc[0]),    // 528
    (Script:  76; Category:   8; FoldCase: @tfc[0]),    // 529
    (Script:  76; Category:  16; FoldCase: @tfc[0]),    // 530
    (Script:  76; Category:  22; FoldCase: @tfc[0]),    // 531
    (Script:   4; Category:   8; FoldCase: @tfc[0]),    // 532
    (Script:   4; Category:  22; FoldCase: @tfc[0]),    // 533
    (Script:  72; Category:   8; FoldCase: @tfc[0]),    // 534
    (Script:  72; Category:  16; FoldCase: @tfc[0]),    // 535
    (Script:  69; Category:   8; FoldCase: @tfc[0]),    // 536
    (Script:  69; Category:  16; FoldCase: @tfc[0]),    // 537
    (Script:  65; Category:   8; FoldCase: @tfc[0]),    // 538
    (Script:   1; Category:  16; FoldCase: @tfc[0]),    // 539
    (Script:  10; Category:  11; FoldCase: @tfc[0]),    // 540
    (Script:  10; Category:  13; FoldCase: @tfc[0]),    // 541
    (Script:  10; Category:   8; FoldCase: @tfc[0]),    // 542
    (Script:  10; Category:  22; FoldCase: @tfc[0]),    // 543
    (Script:  10; Category:  16; FoldCase: @tfc[0]),    // 544
    (Script:  10; Category:  14; FoldCase: @tfc[0]),    // 545
    (Script:  45; Category:  13; FoldCase: @tfc[0]),    // 546
    (Script:  45; Category:  11; FoldCase: @tfc[0]),    // 547
    (Script:  45; Category:   8; FoldCase: @tfc[0]),    // 548
    (Script:  45; Category:  22; FoldCase: @tfc[0]),    // 549
    (Script:  45; Category:   1; FoldCase: @tfc[0]),    // 550
    (Script:  81; Category:   8; FoldCase: @tfc[0]),    // 551
    (Script:  81; Category:  14; FoldCase: @tfc[0]),    // 552
    (Script:  14; Category:  13; FoldCase: @tfc[0]),    // 553
    (Script:  14; Category:   8; FoldCase: @tfc[0]),    // 554
    (Script:  14; Category:  11; FoldCase: @tfc[0]),    // 555
    (Script:  14; Category:  14; FoldCase: @tfc[0]),    // 556
    (Script:  14; Category:  22; FoldCase: @tfc[0]),    // 557
    (Script:  79; Category:  13; FoldCase: @tfc[0]),    // 558
    (Script:  79; Category:  11; FoldCase: @tfc[0]),    // 559
    (Script:  79; Category:   8; FoldCase: @tfc[0]),    // 560
    (Script:  79; Category:  22; FoldCase: @tfc[0]),    // 561
    (Script:  79; Category:  14; FoldCase: @tfc[0]),    // 562
    (Script:  86; Category:   8; FoldCase: @tfc[0]),    // 563
    (Script:  86; Category:  13; FoldCase: @tfc[0]),    // 564
    (Script:  86; Category:  11; FoldCase: @tfc[0]),    // 565
    (Script:  86; Category:  14; FoldCase: @tfc[0]),    // 566
    (Script: 100; Category:   8; FoldCase: @tfc[0]),    // 567
    (Script: 100; Category:  15; FoldCase: @tfc[0]),    // 568
    (Script: 100; Category:  22; FoldCase: @tfc[0]),    // 569
    (Script:  24; Category:   8; FoldCase: @tfc[0]),    // 570
    (Script:  71; Category:   8; FoldCase: @tfc[0]),    // 571
    (Script:  71; Category:  11; FoldCase: @tfc[0]),    // 572
    (Script:  71; Category:  13; FoldCase: @tfc[0]),    // 573
    (Script:  71; Category:   7; FoldCase: @tfc[0]),    // 574
    (Script:  29; Category:  13; FoldCase: @tfc[0]),    // 575
    (Script:  36; Category:  27; FoldCase: @tfc[0]),    // 576
    (Script:   0; Category:  22; FoldCase: @tfc[0]),    // 577
    (Script:   0; Category:   0; FoldCase: @tfc[0]),    // 578
    (Script:   0; Category:  24; FoldCase: @tfc[0]),    // 579
    (Script:   0; Category:  16; FoldCase: @tfc[0]),    // 580
    (Script:   0; Category:   6; FoldCase: @tfc[0]),    // 581
    (Script:   0; Category:   9; FoldCase: @tfc[0]),    // 582
    (Script:   0; Category:  10; FoldCase: @tfc[0]),    // 583
    (Script:   0; Category:  25; FoldCase: @tfc[0]),    // 584
    (Script:   0; Category:   2; FoldCase: @tfc[0]),    // 585
    (Script:   0; Category:  13; FoldCase: @tfc[0]),    // 586
    (Script:   0; Category:  12; FoldCase: @tfc[0]),    // 587
    (Script:   0; Category:  27; FoldCase: @tfc[0]),    // 588
    (Script:   0; Category:   8; FoldCase: @tfc[0]),    // 589
    (Script:   0; Category:  14; FoldCase: @tfc[0]),    // 590
    (Script:   0; Category:   7; FoldCase: @tfc[0]),    // 591
    (Script:   0; Category:  11; FoldCase: @tfc[0]),    // 592
    (Script:   0; Category:  18; FoldCase: @tfc[0]),    // 593
    (Script:   0; Category:  21; FoldCase: @tfc[0]),    // 594
    (Script:   0; Category:   1; FoldCase: @tfc[0]),    // 595
    (Script:   0; Category:  26; FoldCase: @tfc[0]),    // 596
    (Script:   0; Category:  19; FoldCase: @tfc[0]),    // 597
    (Script:   0; Category:  15; FoldCase: @tfc[0]),    // 598
    (Script:   0; Category:   4; FoldCase: @tfc[0]),    // 599
    (Script:   0; Category:   3; FoldCase: @tfc[0])    // 600
  );

  UnicodeCategory1Table: array[$0..$220F] of Byte = (
      0,   1,   2,   3,   4,   5,   6,   7,   8,   9,  10,  11,  12,  13,  14,  15,   // $000000
     16,  17,  18,  19,  20,  21,  22,  23,  24,  25,  26,  27,  28,  29,  30,  31,   // $000800
     32,  33,  34,  34,  35,  36,  37,  38,  39,  40,  40,  40,  41,  42,  43,  44,   // $001000
     45,  46,  47,  48,  49,  50,  51,  52,  53,  54,  55,  56,  57,  58,  59,  60,   // $001800
     61,  62,  63,  64,  65,  65,  66,  67,  68,  69,  70,  71,  72,  73,  74,  75,   // $002000
     76,  76,  65,  77,  78,  78,  79,  80,  81,  82,  83,  84,  85,  86,  87,  88,   // $002800
     89,  90,  91,  92,  93,  94,  95,  96,  97,  97,  97,  97,  97,  97,  97,  97,   // $003000
     97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,   // $003800
     97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,   // $004000
     97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  98,  97,  97,  97,  97,   // $004800
     97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,   // $005000
     97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,   // $005800
     97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,   // $006000
     97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,   // $006800
     97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,   // $007000
     97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,   // $007800
     97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,   // $008000
     97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,   // $008800
     97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,   // $009000
     97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  99,   // $009800
    100,  40,  40,  40,  40,  40,  40,  40,  40, 101, 102, 102, 103, 104, 105, 106,   // $00A000
    107, 108, 109, 110, 111, 112, 113, 114,  34,  34,  34,  34,  34,  34,  34,  34,   // $00A800
     34,  34,  34,  34,  34,  34,  34,  34,  34,  34,  34,  34,  34,  34,  34,  34,   // $00B000
     34,  34,  34,  34,  34,  34,  34,  34,  34,  34,  34,  34,  34,  34,  34,  34,   // $00B800
     34,  34,  34,  34,  34,  34,  34,  34,  34,  34,  34,  34,  34,  34,  34,  34,   // $00C000
     34,  34,  34,  34,  34,  34,  34,  34,  34,  34,  34,  34,  34,  34,  34,  34,   // $00C800
     34,  34,  34,  34,  34,  34,  34,  34,  34,  34,  34,  34,  34,  34,  34, 115,   // $00D000
    116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116, 116,   // $00D800
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $00E000
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $00E800
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $00F000
    117, 117,  97,  97, 118, 119, 120, 121, 122, 122, 123, 124, 125, 126, 127, 128,   // $00F800
    129, 130, 131, 132,  80, 133, 134, 135, 136, 137,  80,  80,  80,  80,  80,  80,   // $010000
    138,  80, 139, 140, 141,  80, 142,  80, 143,  80,  80,  80, 144,  80,  80,  80,   // $010800
    145, 146, 147, 148,  80,  80,  80,  80,  80,  80,  80,  80,  80, 149,  80,  80,   // $011000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $011800
    150, 150, 150, 150, 150, 150, 151,  80, 152,  80,  80,  80,  80,  80,  80,  80,   // $012000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $012800
    153, 153, 153, 153, 153, 153, 153, 153, 154,  80,  80,  80,  80,  80,  80,  80,   // $013000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $013800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $014000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $014800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $015000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $015800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $016000
    155, 155, 155, 155, 156,  80,  80,  80,  80,  80,  80,  80,  80,  80, 157, 158,   // $016800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $017000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $017800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $018000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $018800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $019000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $019800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $01A000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $01A800
    159,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $01B000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $01B800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $01C000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $01C800
     96, 160, 161, 162, 163,  80, 164,  80, 165, 166, 167, 168, 169, 170, 171, 172,   // $01D000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $01D800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $01E000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80, 173, 174,  80,  80,   // $01E800
    175, 176, 177, 178, 179,  80, 180, 181, 182, 183, 184, 185, 186, 187, 188,  80,   // $01F000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $01F800
     97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,   // $020000
     97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,   // $020800
     97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,   // $021000
     97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,   // $021800
     97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,   // $022000
     97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,   // $022800
     97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,   // $023000
     97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,   // $023800
     97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,   // $024000
     97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,   // $024800
     97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,   // $025000
     97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,   // $025800
     97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,   // $026000
     97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,   // $026800
     97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,   // $027000
     97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,   // $027800
     97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,   // $028000
     97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,   // $028800
     97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,   // $029000
     97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,   // $029800
     97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97, 189,  97,  97,   // $02A000
     97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,   // $02A800
     97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97,  97, 190,  97,   // $02B000
    191,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $02B800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $02C000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $02C800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $02D000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $02D800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $02E000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $02E800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $02F000
     97,  97,  97,  97, 191,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $02F800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $030000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $030800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $031000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $031800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $032000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $032800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $033000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $033800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $034000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $034800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $035000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $035800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $036000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $036800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $037000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $037800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $038000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $038800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $039000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $039800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $03A000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $03A800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $03B000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $03B800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $03C000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $03C800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $03D000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $03D800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $03E000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $03E800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $03F000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $03F800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $040000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $040800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $041000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $041800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $042000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $042800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $043000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $043800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $044000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $044800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $045000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $045800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $046000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $046800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $047000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $047800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $048000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $048800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $049000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $049800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $04A000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $04A800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $04B000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $04B800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $04C000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $04C800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $04D000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $04D800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $04E000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $04E800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $04F000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $04F800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $050000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $050800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $051000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $051800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $052000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $052800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $053000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $053800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $054000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $054800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $055000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $055800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $056000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $056800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $057000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $057800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $058000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $058800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $059000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $059800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $05A000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $05A800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $05B000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $05B800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $05C000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $05C800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $05D000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $05D800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $05E000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $05E800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $05F000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $05F800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $060000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $060800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $061000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $061800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $062000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $062800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $063000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $063800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $064000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $064800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $065000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $065800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $066000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $066800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $067000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $067800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $068000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $068800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $069000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $069800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $06A000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $06A800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $06B000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $06B800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $06C000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $06C800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $06D000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $06D800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $06E000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $06E800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $06F000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $06F800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $070000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $070800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $071000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $071800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $072000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $072800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $073000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $073800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $074000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $074800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $075000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $075800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $076000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $076800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $077000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $077800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $078000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $078800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $079000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $079800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $07A000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $07A800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $07B000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $07B800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $07C000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $07C800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $07D000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $07D800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $07E000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $07E800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $07F000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $07F800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $080000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $080800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $081000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $081800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $082000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $082800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $083000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $083800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $084000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $084800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $085000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $085800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $086000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $086800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $087000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $087800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $088000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $088800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $089000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $089800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $08A000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $08A800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $08B000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $08B800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $08C000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $08C800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $08D000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $08D800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $08E000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $08E800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $08F000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $08F800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $090000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $090800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $091000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $091800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $092000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $092800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $093000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $093800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $094000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $094800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $095000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $095800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $096000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $096800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $097000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $097800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $098000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $098800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $099000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $099800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $09A000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $09A800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $09B000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $09B800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $09C000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $09C800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $09D000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $09D800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $09E000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $09E800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $09F000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $09F800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0A0000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0A0800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0A1000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0A1800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0A2000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0A2800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0A3000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0A3800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0A4000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0A4800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0A5000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0A5800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0A6000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0A6800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0A7000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0A7800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0A8000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0A8800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0A9000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0A9800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0AA000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0AA800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0AB000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0AB800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0AC000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0AC800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0AD000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0AD800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0AE000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0AE800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0AF000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0AF800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0B0000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0B0800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0B1000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0B1800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0B2000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0B2800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0B3000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0B3800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0B4000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0B4800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0B5000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0B5800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0B6000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0B6800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0B7000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0B7800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0B8000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0B8800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0B9000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0B9800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0BA000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0BA800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0BB000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0BB800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0BC000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0BC800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0BD000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0BD800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0BE000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0BE800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0BF000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0BF800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0C0000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0C0800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0C1000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0C1800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0C2000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0C2800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0C3000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0C3800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0C4000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0C4800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0C5000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0C5800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0C6000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0C6800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0C7000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0C7800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0C8000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0C8800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0C9000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0C9800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0CA000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0CA800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0CB000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0CB800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0CC000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0CC800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0CD000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0CD800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0CE000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0CE800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0CF000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0CF800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0D0000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0D0800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0D1000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0D1800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0D2000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0D2800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0D3000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0D3800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0D4000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0D4800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0D5000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0D5800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0D6000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0D6800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0D7000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0D7800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0D8000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0D8800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0D9000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0D9800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0DA000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0DA800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0DB000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0DB800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0DC000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0DC800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0DD000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0DD800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0DE000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0DE800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0DF000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0DF800
    192,  80, 193, 194,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0E0000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0E0800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0E1000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0E1800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0E2000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0E2800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0E3000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0E3800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0E4000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0E4800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0E5000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0E5800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0E6000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0E6800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0E7000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0E7800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0E8000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0E8800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0E9000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0E9800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0EA000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0EA800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0EB000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0EB800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0EC000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0EC800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0ED000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0ED800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0EE000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0EE800
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0EF000
     80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,  80,   // $0EF800
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $0F0000
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $0F0800
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $0F1000
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $0F1800
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $0F2000
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $0F2800
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $0F3000
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $0F3800
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $0F4000
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $0F4800
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $0F5000
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $0F5800
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $0F6000
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $0F6800
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $0F7000
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $0F7800
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $0F8000
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $0F8800
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $0F9000
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $0F9800
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $0FA000
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $0FA800
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $0FB000
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $0FB800
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $0FC000
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $0FC800
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $0FD000
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $0FD800
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $0FE000
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $0FE800
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $0FF000
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 195,   // $0FF800
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $100000
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $100800
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $101000
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $101800
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $102000
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $102800
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $103000
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $103800
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $104000
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $104800
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $105000
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $105800
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $106000
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $106800
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $107000
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $107800
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $108000
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $108800
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $109000
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $109800
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $10A000
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $10A800
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $10B000
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $10B800
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $10C000
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $10C800
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $10D000
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $10D800
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $10E000
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $10E800
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117,   // $10F000
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 195,   // $10F800
    117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 117, 195
  );

  UnicodeCategory2Table: array[0..$620F] of Word = (  //$61FE
  // *** block 0 ***
       0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
       0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
       1,    2,  577,  577,    3,    2,  577,  577,    4,    5,    2,    6,    2,    7,    2,  577,
       8,    8,    8,    8,    8,    8,    8,    8,    8,    8,    2,  577,    6,    6,    6,    2,
     577,    9,    9,    9,    9,    9,    9,    9,    9,   10,    9,    9,    9,    9,    9,    9,
       9,    9,    9,    9,    9,    9,    9,    9,    9,    9,    9,    4,    2,    5,   11,   12,
      11,   13,   13,   13,   13,   13,   13,   13,   13,   13,   13,   13,   13,   13,   13,   13,
      13,   13,   13,   13,   13,   13,   13,   13,   13,   13,   13,    4,    6,    5,    6,    0,
  // *** block 1 ***
     578,  578,  578,  578,  578,  578,  578,  578,  578,  578,  578,  578,  578,  578,  578,  578,
     578,  578,  578,  578,  578,  578,  578,  578,  578,  578,  578,  578,  578,  578,  578,  578,
       1,    2,    3,  579,  579,  579,   14,    2,   11,   14,   15,   16,    6,   17,   14,   11,
      14,    6,   18,  580,   11,   19,    2,  577,   11,   18,   15,   20,   18,  580,  580,    2,
       9,    9,    9,    9,    9,    9,    9,    9,    9,    9,    9,    9,    9,    9,    9,    9,
       9,    9,    9,    9,    9,    9,    9,    6,    9,    9,    9,    9,    9,    9,    9,   21,
      13,   13,   13,   13,   13,   13,   13,   13,   13,   13,   13,   13,   13,   13,   13,   13,
      13,   13,   13,   13,   13,   13,   13,    6,   13,   13,   13,   13,   13,   13,   13,   13,
  // *** block 2 ***
      22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,
      22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,
      22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,
      23,   13,   22,   13,   22,   13,   22,   13,   13,   22,   13,   22,   13,   22,   13,   22,
      13,   22,   13,   22,   13,   22,   13,   22,   13,   24,   22,   13,   22,   13,   22,   13,
      22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,
      22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,
      22,   13,   22,   13,   22,   13,   22,   13,   25,   22,   13,   22,   13,   22,   13,   26,
  // *** block 3 ***
      13,   27,   22,   13,   22,   13,   28,   22,   13,   29,   29,   22,   13,   13,   30,   31,
      32,   22,   13,   29,   33,   13,   34,   35,   22,   13,   13,   13,   34,   36,   13,   37,
      22,   13,   22,   13,   22,   13,   38,   22,   13,   38,   13,   13,   22,   13,   38,   22,
      13,   39,   39,   22,   13,   22,   13,   40,   22,   13,   13,   15,   22,  581,  581,  581,
      15,   15,   15,   15,   41,  582,  581,  583,  582,  581,  583,  582,  581,  583,  581,  583,
     581,  583,  581,  583,  581,  583,  581,  583,  581,  583,  581,  583,  581,  581,  583,  581,
     583,  581,  583,  581,  583,  581,  583,  581,  583,  581,  583,  581,  583,  581,  583,  581,
     581,  583,  582,  581,  583,  581,  583,  583,  583,  581,  583,  581,  583,  581,  583,  581,
  // *** block 4 ***
     583,  581,  583,  581,  583,  581,  583,  581,  583,  581,  583,  581,  583,  581,  583,  581,
     583,  581,  583,  581,  583,  581,  583,  581,  583,  581,  583,  581,  583,  581,  583,  581,
     583,  581,  583,  581,  583,  581,  583,  581,  583,  581,  583,  581,  583,  581,  583,  581,
     583,  581,  583,  581,  581,  581,  581,  581,  581,  581,  583,  583,  581,  583,  583,  581,
     581,  583,  581,  583,  583,  583,  583,  581,  583,  581,  583,  581,  583,  581,  583,  581,
     581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,
     581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,
     581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,
  // *** block 5 ***
     581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,
     581,  581,  581,  581,   15,   13,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,
     581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,
      42,   42,   42,   42,   42,   42,   42,   42,   42,   43,   43,   43,   43,   43,   43,   43,
      43,   43,   11,  584,  584,  584,   43,   43,   43,   43,   43,   43,   43,   43,   43,   43,
      43,   43,   11,  584,  584,  584,  584,  584,  584,  584,  584,  584,  584,  584,  584,  584,
      42,   42,   42,   42,   42,   11,   11,   11,   11,   11,   44,   44,   43,   11,   43,   11,
     584,  584,  584,  584,  584,  584,  584,  584,  584,  584,  584,  584,  584,  584,  584,  584,
  // *** block 6 ***
      45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,
      45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,
      45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,
      45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,
      45,   45,   45,   45,   45,   46,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,
      45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,
      45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,
      47,   48,   47,   48,   43,   49,   47,  581,  585,  585,   50,   48,  581,  581,    2,  585,
  // *** block 7 ***
     585,  585,  585,  585,   49,   11,   51,    2,   52,   52,   52,  585,   53,  585,   54,   54,
      55,   56,   56,   56,   56,   56,   56,   56,   56,   56,   56,   56,   56,   56,   56,   56,
      56,   56,  585,   56,   56,   56,   56,   56,   56,   56,   56,   56,   48,   48,   48,   48,
      57,   48,   48,   48,   48,   48,   48,   48,   48,   48,   48,   48,   48,   48,   48,   48,
      48,   48,   58,   48,   48,   48,   48,   48,   48,   48,   48,   48,   48,   48,   48,   59,
      60,   61,   62,   62,   62,   63,   64,   48,   47,   48,   47,   48,   47,   48,   47,   48,
      47,   48,   65,   66,   65,   66,   65,   66,   65,   66,   65,   66,   65,   66,   65,   66,
      67,   68,   48,   48,   69,   70,   71,   47,  581,  583,  583,  581,  581,  583,  583,  583,
  // *** block 8 ***
      72,   72,   72,   72,   72,   72,   72,   72,   72,   72,   72,   72,   72,   72,   72,   72,
      73,   73,   73,   73,   73,   73,   73,   73,   73,   73,   73,   73,   73,   73,   73,   73,
      73,   73,   73,   73,   73,   73,   73,   73,   73,   73,   73,   73,   73,   73,   73,   73,
      74,   74,   74,   74,   74,   74,   74,   74,   74,   74,   74,   74,   74,   74,   74,   74,
      74,   74,   74,   74,   74,   74,   74,   74,   74,   74,   74,   74,   74,   74,   74,   74,
      74,   74,   74,   74,   74,   74,   74,   74,   74,   74,   74,   74,   74,   74,   74,   74,
      75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,
      75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,
  // *** block 9 ***
      75,   74,   76,   77,  586,   45,   45,   77,   78,  587,   75,   74,   75,   74,   75,   74,
      75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,
      75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,
      75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,
      79,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   74,
      75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,
      75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,
      75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,
  // *** block 10 ***
      75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,
      75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,
      75,   74,   75,   74,   75,   74,   75,   74,  585,  585,  585,  585,  585,  585,  585,  585,
     585,   80,   80,   80,   80,   80,   80,   80,   80,   80,   80,   80,   80,   80,   80,   80,
      80,   80,   80,   80,   80,   80,   80,   80,   80,   80,   80,   80,   80,   80,   80,   80,
      80,   80,   80,   80,   80,   80,   80,  585,  585,   81,   82,  577,  577,  577,  577,  577,
     585,   83,   83,   83,   83,   83,   83,   83,   83,   83,   83,   83,   83,   83,   83,   83,
      83,   83,   83,   83,   83,   83,   83,   83,   83,   83,   83,   83,   83,   83,   83,   83,
  // *** block 11 ***
      83,   83,   83,   83,   83,   83,   83,   84,  585,    2,   85,  585,  585,  585,  585,   86,
     585,   87,   87,   87,   87,   87,   87,   87,   87,   87,   87,   87,   87,   87,   87,   87,
      87,   87,   87,   87,   87,   87,   87,   87,   87,   87,   87,   87,   87,   87,   87,   87,
      87,   87,   87,   87,   87,   87,   87,   87,   87,   87,   87,   87,   87,   87,   88,   87,
      89,   87,  586,   89,   87,  586,   89,   87,  585,  585,  585,  585,  585,  585,  585,  585,
      90,   90,   90,   90,   90,   90,   90,   90,   90,   90,   90,   90,   90,   90,   90,   90,
      90,   90,   90,   90,   90,   90,   90,   90,   90,   90,   90,  585,  585,  585,  585,  585,
      90,   90,   90,   89,  577,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 12 ***
      91,   91,   91,   91,   91,  585,   92,   92,   92,   93,  577,   94,    2,   93,   95,  588,
      96,   96,   96,   96,   96,   96,   96,   96,   96,   96,   96,    2,  585,  585,   93,    2,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
      43,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   45,   45,   45,   45,   45,
      45,   45,   45,   45,   45,   45,   96,   96,   96,   96,   96,   96,   96,   96,   96,   96,
       8,    8,    8,    8,    8,    8,    8,    8,    8,    8,   93,   93,   93,   93,   97,  589,
      45,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
  // *** block 13 ***
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
      97,   97,   97,   97,   93,   97,   96,  586,  586,  586,  586,  586,  586,   17,   95,   96,
     586,  586,  586,  586,  586,   98,   98,   96,  586,   95,   96,  586,  586,  586,   97,   97,
      99,  590,  590,  590,  590,  590,  590,  590,  590,  590,   97,   97,   97,   95,  588,   97,
  // *** block 14 ***
     100,  100,  100,  100,  100,  100,  100,  100,  100,  100,  100,  100,  100,  100,  585,  101,
     102,  103,  102,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     103,  103,  103,  103,  103,  103,  103,  103,  103,  103,  103,  103,  103,  103,  103,  103,
     103,  103,  103,  103,  103,  103,  103,  103,  103,  103,  103,  585,  585,  102,  102,  102,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
  // *** block 15 ***
     104,  104,  104,  104,  104,  104,  104,  104,  104,  104,  104,  104,  104,  104,  104,  104,
     104,  104,  104,  104,  104,  104,  104,  104,  104,  104,  104,  104,  104,  104,  104,  104,
     104,  104,  104,  104,  104,  104,  105,  586,  586,  586,  586,  586,  586,  586,  586,  586,
     586,  104,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     106,  106,  106,  106,  106,  106,  106,  106,  106,  106,  107,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  108,  108,  108,  108,  108,
     108,  108,  108,  108,  109,  591,  110,  111,  577,  577,  109,  585,  585,  585,  585,  585,
  // *** block 16 ***
     112,  112,  112,  112,  112,  112,  112,  112,  112,  112,  112,  112,  112,  112,  112,  112,
     112,  112,  112,  112,  112,  112,  113,  586,  586,  586,  114,  113,  586,  586,  586,  586,
     586,  586,  586,  586,  114,  113,  586,  586,  114,  113,  586,  586,  586,  586,  585,  585,
     115,  115,  115,  115,  115,  115,  115,  115,  115,  115,  115,  115,  115,  115,  115,  585,
     116,  116,  116,  116,  116,  116,  116,  116,  116,  116,  116,  116,  116,  116,  116,  116,
     116,  116,  116,  116,  116,  116,  116,  116,  116,  117,  586,  586,  585,  585,  118,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 17 ***
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
      97,  585,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,   96,   96,   96,   96,   96,   96,   96,   96,   96,   96,   96,   96,
      96,   96,   96,   96,   96,   96,   96,   96,   96,   96,   96,   96,   96,   96,   96,  585,
  // *** block 18 ***
     119,  119,  119,  120,  121,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  119,  120,  119,  121,  120,  592,
     592,  119,  119,  119,  119,  119,  119,  119,  119,  120,  592,  592,  592,  119,  120,  592,
     121,   45,   45,  119,  119,  119,  119,  119,  121,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  119,  119,    2,    2,  122,  122,  122,  122,  122,  122,  122,  122,  122,  122,
     123,  124,  121,  589,  589,  589,  589,  589,  585,  121,  121,  121,  121,  121,  121,  121,
  // *** block 19 ***
     585,  125,  126,  592,  585,  127,  127,  127,  127,  127,  127,  127,  127,  585,  585,  127,
     127,  585,  585,  127,  127,  127,  127,  127,  127,  127,  127,  127,  127,  127,  127,  127,
     127,  127,  127,  127,  127,  127,  127,  127,  127,  585,  127,  127,  127,  127,  127,  127,
     127,  585,  127,  585,  585,  585,  127,  127,  127,  127,  585,  585,  125,  127,  126,  592,
     592,  125,  125,  125,  125,  585,  585,  126,  126,  585,  585,  126,  126,  125,  127,  585,
     585,  585,  585,  585,  585,  585,  585,  126,  585,  585,  585,  585,  127,  127,  585,  127,
     127,  127,  125,  586,  585,  585,  128,  128,  128,  128,  128,  128,  128,  128,  128,  128,
     127,  589,  129,  129,  130,  580,  580,  580,  580,  580,  131,  129,  585,  585,  585,  585,
  // *** block 20 ***
     585,  132,  132,  133,  585,  134,  134,  134,  134,  134,  134,  585,  585,  585,  585,  134,
     134,  585,  585,  134,  134,  134,  134,  134,  134,  134,  134,  134,  134,  134,  134,  134,
     134,  134,  134,  134,  134,  134,  134,  134,  134,  585,  134,  134,  134,  134,  134,  134,
     134,  585,  134,  134,  585,  134,  134,  585,  134,  134,  585,  585,  132,  585,  133,  133,
     133,  132,  586,  585,  585,  585,  585,  132,  132,  585,  585,  132,  132,  132,  585,  585,
     585,  132,  585,  585,  585,  585,  585,  585,  585,  134,  134,  134,  134,  585,  134,  585,
     585,  585,  585,  585,  585,  585,  135,  135,  135,  135,  135,  135,  135,  135,  135,  135,
     132,  586,  134,  134,  134,  132,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 21 ***
     585,  136,  136,  137,  585,  138,  138,  138,  138,  138,  138,  138,  138,  138,  585,  138,
     138,  138,  585,  138,  138,  138,  138,  138,  138,  138,  138,  138,  138,  138,  138,  138,
     138,  138,  138,  138,  138,  138,  138,  138,  138,  585,  138,  138,  138,  138,  138,  138,
     138,  585,  138,  138,  585,  138,  138,  138,  138,  138,  585,  585,  136,  138,  137,  592,
     592,  136,  136,  136,  136,  136,  585,  136,  136,  137,  585,  137,  137,  136,  585,  585,
     138,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     138,  138,  136,  586,  585,  585,  139,  139,  139,  139,  139,  139,  139,  139,  139,  139,
     140,  141,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 22 ***
     585,  142,  143,  592,  585,  144,  144,  144,  144,  144,  144,  144,  144,  585,  585,  144,
     144,  585,  585,  144,  144,  144,  144,  144,  144,  144,  144,  144,  144,  144,  144,  144,
     144,  144,  144,  144,  144,  144,  144,  144,  144,  585,  144,  144,  144,  144,  144,  144,
     144,  585,  144,  144,  585,  144,  144,  144,  144,  144,  585,  585,  142,  144,  143,  142,
     143,  142,  586,  586,  586,  585,  585,  143,  143,  585,  585,  143,  143,  142,  585,  585,
     585,  585,  585,  585,  585,  585,  142,  143,  585,  585,  585,  585,  144,  144,  585,  144,
     144,  144,  142,  586,  585,  585,  145,  145,  145,  145,  145,  145,  145,  145,  145,  145,
     146,  144,  147,  580,  580,  580,  580,  580,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 23 ***
     585,  585,  148,  149,  585,  149,  149,  149,  149,  149,  149,  585,  585,  585,  149,  149,
     149,  585,  149,  149,  149,  149,  585,  585,  585,  149,  149,  585,  149,  585,  149,  149,
     585,  585,  585,  149,  149,  585,  585,  585,  149,  149,  149,  585,  585,  585,  149,  149,
     149,  149,  149,  149,  149,  149,  149,  149,  149,  149,  585,  585,  585,  585,  150,  150,
     148,  150,  592,  585,  585,  585,  150,  150,  150,  585,  150,  150,  150,  148,  585,  585,
     149,  585,  585,  585,  585,  585,  585,  150,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  151,  151,  151,  151,  151,  151,  151,  151,  151,  151,
     152,  580,  580,  153,  153,  153,  153,  153,  153,  154,  153,  585,  585,  585,  585,  585,
  // *** block 24 ***
     585,  155,  155,  155,  585,  156,  156,  156,  156,  156,  156,  156,  156,  585,  156,  156,
     156,  585,  156,  156,  156,  156,  156,  156,  156,  156,  156,  156,  156,  156,  156,  156,
     156,  156,  156,  156,  156,  156,  156,  156,  156,  585,  156,  156,  156,  156,  156,  156,
     156,  156,  156,  156,  585,  156,  156,  156,  156,  156,  585,  585,  585,  156,  157,  586,
     586,  155,  155,  155,  155,  585,  157,  157,  157,  585,  157,  157,  157,  157,  585,  585,
     585,  585,  585,  585,  585,  157,  157,  585,  156,  156,  585,  585,  585,  585,  585,  585,
     156,  156,  157,  586,  585,  585,  158,  158,  158,  158,  158,  158,  158,  158,  158,  158,
     585,  585,  585,  585,  585,  585,  585,  585,  159,  159,  159,  159,  159,  159,  159,  160,
  // *** block 25 ***
     585,  585,  161,  161,  585,  162,  162,  162,  162,  162,  162,  162,  162,  585,  162,  162,
     162,  585,  162,  162,  162,  162,  162,  162,  162,  162,  162,  162,  162,  162,  162,  162,
     162,  162,  162,  162,  162,  162,  162,  162,  162,  585,  162,  162,  162,  162,  162,  162,
     162,  162,  162,  162,  585,  162,  162,  162,  162,  162,  585,  585,  163,  162,  161,  163,
     161,  592,  592,  592,  592,  585,  163,  161,  592,  585,  161,  161,  163,  586,  585,  585,
     585,  585,  585,  585,  585,  161,  161,  585,  585,  585,  585,  585,  585,  585,  162,  585,
     162,  162,  163,  586,  585,  585,  164,  164,  164,  164,  164,  164,  164,  164,  164,  164,
     585,  162,  162,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 26 ***
     585,  585,  165,  165,  585,  166,  166,  166,  166,  166,  166,  166,  166,  585,  166,  166,
     166,  585,  166,  166,  166,  166,  166,  166,  166,  166,  166,  166,  166,  166,  166,  166,
     166,  166,  166,  166,  166,  166,  166,  166,  166,  166,  166,  166,  166,  166,  166,  166,
     166,  166,  166,  166,  166,  166,  166,  166,  166,  166,  166,  585,  585,  166,  165,  592,
     592,  167,  167,  167,  167,  585,  165,  165,  165,  585,  165,  165,  165,  167,  166,  585,
     585,  585,  585,  585,  585,  585,  585,  165,  585,  585,  585,  585,  585,  585,  585,  585,
     166,  166,  167,  586,  585,  585,  168,  168,  168,  168,  168,  168,  168,  168,  168,  168,
     169,  580,  580,  580,  580,  580,  585,  585,  585,  170,  166,  589,  589,  589,  589,  589,
  // *** block 27 ***
     585,  585,  171,  171,  585,  172,  172,  172,  172,  172,  172,  172,  172,  172,  172,  172,
     172,  172,  172,  172,  172,  172,  172,  585,  585,  585,  172,  172,  172,  172,  172,  172,
     172,  172,  172,  172,  172,  172,  172,  172,  172,  172,  172,  172,  172,  172,  172,  172,
     172,  172,  585,  172,  172,  172,  172,  172,  172,  172,  172,  172,  585,  172,  585,  585,
     172,  172,  172,  172,  172,  172,  172,  585,  585,  585,  173,  585,  585,  585,  585,  171,
     171,  171,  173,  586,  586,  585,  173,  585,  171,  171,  171,  171,  171,  171,  171,  171,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  171,  171,  174,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 28 ***
     585,  175,  175,  175,  175,  175,  175,  175,  175,  175,  175,  175,  175,  175,  175,  175,
     175,  175,  175,  175,  175,  175,  175,  175,  175,  175,  175,  175,  175,  175,  175,  175,
     175,  175,  175,  175,  175,  175,  175,  175,  175,  175,  175,  175,  175,  175,  175,  175,
     175,  176,  175,  589,  176,  176,  176,  176,  176,  176,  176,  585,  585,  585,  585,    3,
     175,  175,  175,  175,  175,  175,  177,  176,  586,  586,  586,  586,  586,  586,  586,  178,
     179,  590,  590,  590,  590,  590,  590,  590,  590,  590,  178,  178,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 29 ***
     585,  180,  180,  585,  180,  585,  585,  180,  180,  585,  180,  585,  585,  180,  585,  585,
     585,  585,  585,  585,  180,  180,  180,  180,  585,  180,  180,  180,  180,  180,  180,  180,
     585,  180,  180,  180,  585,  180,  585,  180,  585,  585,  180,  180,  585,  180,  180,  180,
     180,  181,  180,  589,  181,  181,  181,  181,  181,  181,  585,  181,  181,  180,  585,  585,
     180,  180,  180,  180,  180,  585,  182,  585,  181,  181,  181,  181,  181,  181,  585,  585,
     183,  183,  183,  183,  183,  183,  183,  183,  183,  183,  585,  585,  180,  180,  180,  180,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 30 ***
     184,  185,  588,  588,  186,  186,  186,  186,  186,  186,  186,  186,  186,  186,  186,  186,
     186,  186,  186,  185,  186,  185,  588,  588,  187,  187,  185,  588,  588,  588,  588,  588,
     188,  188,  188,  188,  188,  188,  188,  188,  188,  188,  189,  580,  580,  580,  580,  580,
     580,  580,  580,  580,  185,  187,  185,  187,  185,  187,  190,  191,  190,  191,  192,  592,
     184,  184,  184,  184,  184,  184,  184,  184,  585,  184,  184,  184,  184,  184,  184,  184,
     184,  184,  184,  184,  184,  184,  184,  184,  184,  184,  184,  184,  184,  184,  184,  184,
     184,  184,  184,  184,  184,  184,  184,  184,  184,  184,  184,  184,  184,  585,  585,  585,
     585,  187,  187,  187,  187,  187,  187,  187,  187,  187,  187,  187,  187,  187,  187,  192,
  // *** block 31 ***
     187,  586,  586,  586,  586,  186,  187,  586,  184,  184,  184,  184,  184,  187,  586,  586,
     586,  586,  586,  586,  586,  586,  586,  586,  585,  187,  187,  187,  187,  187,  187,  187,
     187,  187,  187,  187,  187,  187,  187,  187,  187,  187,  187,  187,  187,  187,  187,  187,
     187,  187,  187,  187,  187,  187,  187,  187,  187,  187,  187,  187,  187,  585,  185,  185,
     185,  185,  185,  185,  185,  185,  187,  185,  588,  588,  588,  588,  588,  585,  185,  185,
     186,  577,  577,  577,  577,   14,   14,   14,   14,  186,  186,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 32 ***
     193,  193,  193,  193,  193,  193,  193,  193,  193,  193,  193,  193,  193,  193,  193,  193,
     193,  193,  193,  193,  193,  193,  193,  193,  193,  193,  193,  193,  193,  193,  193,  193,
     193,  193,  193,  193,  193,  193,  193,  193,  193,  193,  193,  194,  592,  195,  195,  195,
     195,  194,  195,  586,  586,  586,  586,  586,  194,  195,  586,  194,  194,  195,  586,  193,
     196,  590,  590,  590,  590,  590,  590,  590,  590,  590,  197,  197,  197,  197,  197,  197,
     193,  589,  589,  589,  589,  589,  194,  194,  195,  586,  193,  193,  193,  193,  195,  586,
     586,  193,  194,  592,  592,  193,  193,  194,  592,  592,  592,  592,  592,  592,  193,  193,
     193,  195,  586,  586,  586,  193,  193,  193,  193,  193,  193,  193,  193,  193,  193,  193,
  // *** block 33 ***
     193,  193,  195,  194,  592,  195,  195,  194,  592,  592,  592,  592,  592,  195,  193,  194,
     196,  590,  590,  590,  590,  590,  590,  590,  590,  590,  194,  194,  194,  195,  198,  588,
     199,  199,  199,  199,  199,  199,  199,  199,  199,  199,  199,  199,  199,  199,  199,  199,
     199,  199,  199,  199,  199,  199,  199,  199,  199,  199,  199,  199,  199,  199,  199,  199,
     199,  199,  199,  199,  199,  199,  585,  199,  585,  585,  585,  585,  585,  199,  585,  585,
     200,  200,  200,  200,  200,  200,  200,  200,  200,  200,  200,  200,  200,  200,  200,  200,
     200,  200,  200,  200,  200,  200,  200,  200,  200,  200,  200,  200,  200,  200,  200,  200,
     200,  200,  200,  200,  200,  200,  200,  200,  200,  200,  200,    2,  201,  200,  589,  589,
  // *** block 34 ***
     202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,
     202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,
     202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,
     202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,
     202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,
     202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,
     202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,
     202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,
  // *** block 35 ***
     203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,
     203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,
     203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,
     203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,
     203,  203,  203,  203,  203,  203,  203,  203,  203,  585,  203,  203,  203,  203,  585,  585,
     203,  203,  203,  203,  203,  203,  203,  585,  203,  585,  203,  203,  203,  203,  585,  585,
     203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,
     203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,
  // *** block 36 ***
     203,  203,  203,  203,  203,  203,  203,  203,  203,  585,  203,  203,  203,  203,  585,  585,
     203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,
     203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,
     203,  585,  203,  203,  203,  203,  585,  585,  203,  203,  203,  203,  203,  203,  203,  585,
     203,  585,  203,  203,  203,  203,  585,  585,  203,  203,  203,  203,  203,  203,  203,  203,
     203,  203,  203,  203,  203,  203,  203,  585,  203,  203,  203,  203,  203,  203,  203,  203,
     203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,
     203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,
  // *** block 37 ***
     203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,
     203,  585,  203,  203,  203,  203,  585,  585,  203,  203,  203,  203,  203,  203,  203,  203,
     203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,
     203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,
     203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,
     203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  585,  585,  204,  204,  204,
     205,  577,  577,  577,  577,  577,  577,  577,  577,  206,  206,  206,  206,  206,  206,  206,
     206,  206,  206,  206,  206,  206,  206,  206,  206,  206,  206,  206,  206,  585,  585,  585,
  // *** block 38 ***
     203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,
     207,  588,  588,  588,  588,  588,  588,  588,  588,  588,  585,  585,  585,  585,  585,  585,
     208,  208,  208,  208,  208,  208,  208,  208,  208,  208,  208,  208,  208,  208,  208,  208,
     208,  208,  208,  208,  208,  208,  208,  208,  208,  208,  208,  208,  208,  208,  208,  208,
     208,  208,  208,  208,  208,  208,  208,  208,  208,  208,  208,  208,  208,  208,  208,  208,
     208,  208,  208,  208,  208,  208,  208,  208,  208,  208,  208,  208,  208,  208,  208,  208,
     208,  208,  208,  208,  208,  208,  208,  208,  208,  208,  208,  208,  208,  208,  208,  208,
     208,  208,  208,  208,  208,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 39 ***
     209,  210,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
  // *** block 40 ***
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
  // *** block 41 ***
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  211,  211,  210,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
  // *** block 42 ***
     212,  213,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  214,  215,  585,  585,  585,
     216,  216,  216,  216,  216,  216,  216,  216,  216,  216,  216,  216,  216,  216,  216,  216,
     216,  216,  216,  216,  216,  216,  216,  216,  216,  216,  216,  216,  216,  216,  216,  216,
     216,  216,  216,  216,  216,  216,  216,  216,  216,  216,  216,  216,  216,  216,  216,  216,
     216,  216,  216,  216,  216,  216,  216,  216,  216,  216,  216,  216,  216,  216,  216,  216,
     216,  216,  216,  216,  216,  216,  216,  216,  216,  216,  216,    2,    2,    2,  217,  217,
     217,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 43 ***
     218,  218,  218,  218,  218,  218,  218,  218,  218,  218,  218,  218,  218,  585,  218,  218,
     218,  218,  219,  586,  586,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     220,  220,  220,  220,  220,  220,  220,  220,  220,  220,  220,  220,  220,  220,  220,  220,
     220,  220,  221,  586,  586,    2,    2,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     222,  222,  222,  222,  222,  222,  222,  222,  222,  222,  222,  222,  222,  222,  222,  222,
     222,  222,  223,  586,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     224,  224,  224,  224,  224,  224,  224,  224,  224,  224,  224,  224,  224,  585,  224,  224,
     224,  585,  225,  225,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 44 ***
     226,  226,  226,  226,  226,  226,  226,  226,  226,  226,  226,  226,  226,  226,  226,  226,
     226,  226,  226,  226,  226,  226,  226,  226,  226,  226,  226,  226,  226,  226,  226,  226,
     226,  226,  226,  226,  226,  226,  226,  226,  226,  226,  226,  226,  226,  226,  226,  226,
     226,  226,  226,  226,  227,  586,  228,  227,  586,  586,  586,  586,  586,  586,  228,  228,
     228,  228,  228,  228,  228,  228,  227,  228,  592,  227,  227,  227,  227,  227,  227,  227,
     227,  227,  227,  227,  229,  577,  577,  230,  229,  577,  577,  231,  226,  227,  585,  585,
     232,  232,  232,  232,  232,  232,  232,  232,  232,  232,  585,  585,  585,  585,  585,  585,
     233,  233,  233,  233,  233,  233,  233,  233,  233,  233,  585,  585,  585,  585,  585,  585,
  // *** block 45 ***
     234,  234,    2,    2,  234,    2,  235,  234,  577,  577,  577,  236,  236,  236,  237,  585,
     238,  238,  238,  238,  238,  238,  238,  238,  238,  238,  585,  585,  585,  585,  585,  585,
     239,  239,  239,  239,  239,  239,  239,  239,  239,  239,  239,  239,  239,  239,  239,  239,
     239,  239,  239,  239,  239,  239,  239,  239,  239,  239,  239,  239,  239,  239,  239,  239,
     239,  239,  239,  240,  239,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 46 ***
     239,  239,  239,  239,  239,  239,  239,  239,  239,  239,  239,  239,  239,  239,  239,  239,
     239,  239,  239,  239,  239,  239,  239,  239,  239,  239,  239,  239,  239,  239,  239,  239,
     239,  239,  239,  239,  239,  239,  239,  239,  239,  236,  239,  585,  585,  585,  585,  585,
     210,  210,  210,  210,  210,  210,  210,  210,  210,  210,  210,  210,  210,  210,  210,  210,
     210,  210,  210,  210,  210,  210,  210,  210,  210,  210,  210,  210,  210,  210,  210,  210,
     210,  210,  210,  210,  210,  210,  210,  210,  210,  210,  210,  210,  210,  210,  210,  210,
     210,  210,  210,  210,  210,  210,  210,  210,  210,  210,  210,  210,  210,  210,  210,  210,
     210,  210,  210,  210,  210,  210,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 47 ***
     241,  241,  241,  241,  241,  241,  241,  241,  241,  241,  241,  241,  241,  241,  241,  241,
     241,  241,  241,  241,  241,  241,  241,  241,  241,  241,  241,  241,  241,  585,  585,  585,
     242,  242,  242,  243,  592,  592,  592,  242,  242,  243,  592,  592,  585,  585,  585,  585,
     243,  243,  242,  243,  592,  592,  592,  592,  592,  242,  242,  242,  585,  585,  585,  585,
     244,  585,  585,  585,  245,  245,  246,  590,  590,  590,  590,  590,  590,  590,  590,  590,
     247,  247,  247,  247,  247,  247,  247,  247,  247,  247,  247,  247,  247,  247,  247,  247,
     247,  247,  247,  247,  247,  247,  247,  247,  247,  247,  247,  247,  247,  247,  585,  585,
     247,  247,  247,  247,  247,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 48 ***
     248,  248,  248,  248,  248,  248,  248,  248,  248,  248,  248,  248,  248,  248,  248,  248,
     248,  248,  248,  248,  248,  248,  248,  248,  248,  248,  248,  248,  248,  248,  248,  248,
     248,  248,  248,  248,  248,  248,  248,  248,  248,  248,  248,  248,  585,  585,  585,  585,
     249,  249,  249,  249,  249,  249,  249,  249,  249,  249,  249,  249,  249,  249,  249,  249,
     249,  248,  589,  589,  589,  589,  589,  589,  249,  249,  585,  585,  585,  585,  585,  585,
     250,  250,  250,  250,  250,  250,  250,  250,  250,  250,  251,  585,  585,  585,  252,  252,
     253,  253,  253,  253,  253,  253,  253,  253,  253,  253,  253,  253,  253,  253,  253,  253,
     253,  253,  253,  253,  253,  253,  253,  253,  253,  253,  253,  253,  253,  253,  253,  253,
  // *** block 49 ***
     254,  254,  254,  254,  254,  254,  254,  254,  254,  254,  254,  254,  254,  254,  254,  254,
     254,  254,  254,  254,  254,  254,  254,  255,  586,  256,  256,  256,  585,  585,  257,  257,
     258,  258,  258,  258,  258,  258,  258,  258,  258,  258,  258,  258,  258,  258,  258,  258,
     258,  258,  258,  258,  258,  258,  258,  258,  258,  258,  258,  258,  258,  258,  258,  258,
     258,  258,  258,  258,  258,  258,  258,  258,  258,  258,  258,  258,  258,  258,  258,  258,
     258,  258,  258,  258,  258,  259,  260,  259,  260,  586,  586,  586,  586,  586,  586,  585,
     260,  259,  260,  259,  592,  260,  260,  260,  260,  260,  260,  260,  260,  259,  592,  592,
     592,  592,  592,  260,  260,  260,  260,  260,  260,  260,  260,  260,  260,  585,  585,  260,
  // *** block 50 ***
     261,  590,  590,  590,  590,  590,  590,  590,  590,  590,  585,  585,  585,  585,  585,  585,
     261,  261,  261,  261,  261,  261,  261,  261,  261,  261,  585,  585,  585,  585,  585,  585,
     262,  262,  262,  262,  262,  262,  262,  263,  262,  577,  577,  577,  577,  577,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 51 ***
     264,  264,  264,  264,  265,  266,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  264,  265,  264,  586,  586,  586,  586,  265,  264,  265,  592,  592,
     592,  592,  264,  265,  592,  266,  266,  266,  266,  266,  266,  266,  585,  585,  585,  585,
     267,  267,  267,  267,  267,  267,  267,  267,  267,  267,  268,  577,  577,  577,  577,  577,
     577,  269,  269,  269,  269,  269,  269,  269,  269,  269,  269,  264,  586,  586,  586,  586,
     586,  586,  586,  586,  269,  269,  269,  269,  269,  269,  269,  269,  269,  585,  585,  585,
  // *** block 52 ***
     270,  270,  271,  272,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  271,  270,  586,  586,  586,  271,  271,  270,  586,  271,  270,  271,  592,  272,  272,
     273,  590,  590,  590,  590,  590,  590,  590,  590,  590,  272,  272,  272,  272,  272,  272,
     274,  274,  274,  274,  274,  274,  274,  274,  274,  274,  274,  274,  274,  274,  274,  274,
     274,  274,  274,  274,  274,  274,  274,  274,  274,  274,  274,  274,  274,  274,  274,  274,
     274,  274,  274,  274,  274,  274,  275,  276,  275,  586,  276,  276,  276,  275,  276,  275,
     586,  586,  276,  276,  585,  585,  585,  585,  585,  585,  585,  585,  277,  277,  277,  277,
  // *** block 53 ***
     278,  278,  278,  278,  278,  278,  278,  278,  278,  278,  278,  278,  278,  278,  278,  278,
     278,  278,  278,  278,  278,  278,  278,  278,  278,  278,  278,  278,  278,  278,  278,  278,
     278,  278,  278,  278,  279,  592,  592,  592,  592,  592,  592,  592,  280,  280,  280,  280,
     280,  280,  280,  280,  279,  592,  280,  280,  585,  585,  585,  281,  281,  281,  281,  281,
     282,  590,  590,  590,  590,  590,  590,  590,  590,  590,  585,  585,  585,  278,  278,  278,
     283,  283,  283,  283,  283,  283,  283,  283,  283,  283,  284,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  285,  285,  285,  285,  285,  285,  286,  577,
  // *** block 54 ***
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     287,  287,  287,  287,  287,  287,  287,  287,  585,  585,  585,  585,  585,  585,  585,  585,
      45,   45,   45,    2,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,
      45,  288,   45,   45,   45,   45,   45,   45,   45,  289,  289,  289,  289,   45,  289,  289,
     289,  289,  288,  592,   45,  289,  289,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 55 ***
      13,   13,   13,   13,   13,   13,   13,   13,   13,   13,   13,   13,   13,   13,   13,   13,
      13,   13,   13,   13,   13,   13,   13,   13,   13,   13,   13,   13,   13,   13,   13,   13,
      13,   13,   13,   13,   13,   13,   48,   48,   48,   48,   48,   74,   42,   42,   42,   42,
      42,   42,   42,   42,   42,   42,   42,   42,   42,   42,   42,   42,   42,   42,   42,   42,
      42,   42,   42,   42,   42,   42,   42,   42,   42,   42,   42,   42,   42,   42,   42,   42,
      42,   42,   42,   42,   42,   42,   42,   42,   42,   42,   42,   42,   42,   50,   50,   50,
      50,   50,   42,   42,   42,   42,   50,   50,   50,   50,   50,   13,   13,   13,   13,   13,
      13,   13,   13,   13,   13,   13,   13,   13,  290,   13,   13,   13,   13,   13,   13,   13,
  // *** block 56 ***
      13,   13,   13,   13,   13,   13,   13,   13,   13,   13,   13,   13,   13,   13,   13,   13,
      13,   13,   13,   13,   13,   13,   13,   13,   13,   13,   13,   42,  591,  591,  591,  591,
     591,  591,  591,  591,  591,  591,  591,  591,  591,  591,  591,  591,  591,  591,  591,  591,
     591,  591,  591,  591,  591,  591,  591,  591,  591,  591,  591,  591,  591,  591,  591,   50,
      45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,
      45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,
      45,   45,   45,   45,   45,   45,   45,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,   45,   45,   45,   45,
  // *** block 57 ***
      22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,
      22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,
      22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,
      22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,
      22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,
      22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,
      22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,
      22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,
  // *** block 58 ***
      22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,
      22,   13,   22,   13,   22,   13,  291,  292,  293,  294,  295,  296,   13,   13,  297,   13,
      22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,
      22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,
      22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,
      22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,
      22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,
      22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,
  // *** block 59 ***
      48,   48,   48,   48,   48,   48,   48,   48,  298,  298,  298,  298,  298,  298,  298,  298,
      48,   48,   48,   48,   48,   48,  585,  585,  298,  298,  298,  298,  298,  298,  585,  585,
      48,   48,   48,   48,   48,   48,   48,   48,  298,  298,  298,  298,  298,  298,  298,  298,
      48,   48,   48,   48,   48,   48,   48,   48,  298,  298,  298,  298,  298,  298,  298,  298,
      48,   48,   48,   48,   48,   48,  585,  585,  298,  298,  298,  298,  298,  298,  585,  585,
     299,   48,  300,   48,  301,   48,  302,   48,  585,  298,  585,  298,  585,  298,  585,  298,
      48,   48,   48,   48,   48,   48,   48,   48,  298,  298,  298,  298,  298,  298,  298,  298,
      48,   48,   48,   48,   48,   48,   48,   48,   48,   48,   48,   48,   48,   48,  585,  585,
  // *** block 60 ***
     303,  304,  305,  306,  307,  308,  309,  310,  311,  312,  313,  314,  315,  316,  317,  318,
     319,  320,  321,  322,  323,  324,  325,  326,  327,  328,  329,  330,  331,  332,  333,  334,
     335,  336,  337,  338,  339,  340,  341,  342,  343,  344,  345,  346,  347,  348,  349,  350,
      48,   48,  351,  352,  353,  585,  354,  355,  298,  298,  356,  356,  357,   49,  358,   49,
     584,  584,  359,  360,  361,  585,  362,  363,  364,  364,  364,  364,  365,   49,  584,  584,
      48,   48,  366,   55,  585,  585,  367,  368,  298,  298,  369,  369,  585,   49,   49,   49,
      48,  581,  581,  581,  581,  581,  581,  581,  583,  583,  583,  583,  583,   49,   49,   49,
     585,  585,  370,  371,  372,  585,  373,  374,  375,  375,  376,  376,  377,   49,  584,  585,
  // *** block 61 ***
       1,    1,    1,    1,    1,    1,    1,    1,    1,    1,    1,   17,  378,  378,   17,   17,
       7,  593,  593,  593,  593,  593,    2,    2,   16,   20,    4,   16,  594,   20,    4,   16,
       2,  577,  577,  577,  577,  577,  577,  577,  379,  380,   17,  595,  595,  595,  595,    1,
       2,  577,  577,  577,  577,  577,  577,  577,  577,   16,   20,    2,  577,  577,  577,   12,
      12,    2,  577,  577,    6,    4,    5,    2,  577,  577,  577,  577,  577,  577,  577,  577,
     577,  577,    6,    2,   12,    2,  577,  577,  577,  577,  577,  577,  577,  577,  577,    1,
      17,  595,  595,  595,  595,  585,  585,  585,  585,  585,   17,   17,   17,   17,   17,   17,
      18,   42,  585,  585,   18,   18,   18,   18,   18,   18,    6,  596,  596,    4,    5,   42,
  // *** block 62 ***
      18,   18,   18,   18,   18,   18,   18,   18,   18,   18,    6,  596,  596,    4,    5,  585,
      42,   42,   42,   42,   42,   42,   42,   42,   42,   42,   42,   42,   42,  585,  585,  585,
       3,    3,    3,    3,    3,    3,    3,    3,    3,    3,    3,    3,    3,    3,    3,    3,
       3,    3,    3,    3,    3,    3,    3,    3,    3,    3,    3,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
      45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,  381,  587,  587,
     587,   45,  381,  587,  587,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,
      45,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 63 ***
      14,   14,  382,   14,  588,  588,  588,  382,   14,  588,  383,  382,  382,  382,  383,  383,
     382,  382,  382,  383,   14,  382,   14,  588,    6,  382,  583,  583,  583,  583,   14,   14,
      14,   14,   14,   14,  382,   14,  384,   14,  382,   14,  385,  386,  382,  382,   14,  383,
     583,  583,  387,  382,  383,  289,  589,  589,  589,  383,   14,  588,  383,  383,  382,  382,
       6,  596,  596,  596,  596,  382,  383,  383,  383,  383,   14,    6,   14,  588,   13,   14,
      18,  580,  580,  580,  580,  580,  580,  580,  580,  580,  580,  580,  580,  580,  580,  580,
     388,  388,  388,  388,  388,  388,  388,  388,  388,  388,  388,  388,  388,  388,  388,  388,
     389,  389,  389,  389,  389,  389,  389,  389,  389,  389,  389,  389,  389,  389,  389,  389,
  // *** block 64 ***
     389,  389,  389,   22,  581,  389,  389,  389,  389,   18,  585,  585,  585,  585,  585,  585,
       6,    6,    6,    6,    6,   14,  588,  588,  588,  588,    6,    6,   14,  588,  588,  588,
       6,   14,  588,    6,   14,  588,    6,   14,  588,  588,  588,  588,  588,  588,    6,   14,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,    6,    6,
      14,  588,    6,   14,    6,   14,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,
  // *** block 65 ***
       6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,
       6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,
       6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,
       6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,
       6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,
       6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,
       6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,
       6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,
  // *** block 66 ***
      14,  588,  588,  588,  588,  588,  588,  588,    6,    6,    6,    6,   14,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
       6,    6,   14,  588,  588,  588,  588,  588,  588,    4,    5,   14,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,    6,   14,  588,  588,
  // *** block 67 ***
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,    6,    6,    6,    6,    6,
       6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,
       6,    6,    6,    6,   14,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,    6,    6,    6,    6,
       6,    6,   14,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 68 ***
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
      18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,
      18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,
  // *** block 69 ***
      18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,
      18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   14,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,   18,   18,   18,   18,   18,   18,
      18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,
  // *** block 70 ***
      14,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
  // *** block 71 ***
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,    6,   14,  588,  588,  588,  588,  588,  588,  588,
     588,    6,   14,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,    6,    6,    6,    6,    6,    6,    6,    6,
  // *** block 72 ***
      14,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,    6,
      14,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
  // *** block 73 ***
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
  // *** block 74 ***
     585,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,    4,    5,    4,    5,    4,    5,    4,    5,
       4,    5,    4,    5,    4,    5,   18,  580,  580,  580,  580,  580,  580,  580,  580,  580,
  // *** block 75 ***
     580,  580,  580,  580,  580,  580,  580,  580,  580,  580,  580,  580,  580,  580,  580,  580,
     580,  580,  580,  580,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
       6,  596,  596,  596,  596,    4,    5,    6,  596,  596,  596,  596,  596,  596,  596,  596,
     596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,
     596,  596,  596,  596,  596,  596,    4,    5,    4,    5,    4,    5,    4,    5,    4,    5,
       6,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,
  // *** block 76 ***
     390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,
     390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,
     390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,
     390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,
     390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,
     390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,
     390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,
     390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,  390,
  // *** block 77 ***
       6,    6,    6,    4,    5,    4,    5,    4,    5,    4,    5,    4,    5,    4,    5,    4,
       5,    4,    5,    4,    5,    4,    5,    4,    5,    6,  596,  596,  596,  596,  596,  596,
     596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,
     596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,
     596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,
     596,  596,  596,  596,  596,  596,  596,  596,    4,    5,    4,    5,    6,  596,  596,  596,
     596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,
     596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,    4,    5,    6,  596,
  // *** block 78 ***
     596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,
     596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,
     596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,
     596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,
     596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,
     596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,
     596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,
     596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,
  // *** block 79 ***
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
       6,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,  596,
     596,  596,  596,  596,  596,   14,   14,    6,  596,  596,  596,  596,  596,  585,  585,  585,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 80 ***
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 81 ***
     391,  391,  391,  391,  391,  391,  391,  391,  391,  391,  391,  391,  391,  391,  391,  391,
     391,  391,  391,  391,  391,  391,  391,  391,  391,  391,  391,  391,  391,  391,  391,  391,
     391,  391,  391,  391,  391,  391,  391,  391,  391,  391,  391,  391,  391,  391,  391,  585,
     392,  392,  392,  392,  392,  392,  392,  392,  392,  392,  392,  392,  392,  392,  392,  392,
     392,  392,  392,  392,  392,  392,  392,  392,  392,  392,  392,  392,  392,  392,  392,  392,
     392,  392,  392,  392,  392,  392,  392,  392,  392,  392,  392,  392,  392,  392,  392,  585,
      22,   13,  393,  394,  395,   13,   13,   22,   13,   22,   13,   22,   13,  396,  397,  398,
     399,   13,   22,   13,   13,   22,   13,   13,   13,   13,   13,   13,   42,  591,  400,  400,
  // *** block 82 ***
      65,   66,   65,   66,   65,   66,   65,   66,   65,   66,   65,   66,   65,   66,   65,   66,
      65,   66,   65,   66,   65,   66,   65,   66,   65,   66,   65,   66,   65,   66,   65,   66,
      65,   66,   65,   66,   65,   66,   65,   66,   65,   66,   65,   66,   65,   66,   65,   66,
      65,   66,   65,   66,   65,   66,   65,   66,   65,   66,   65,   66,   65,   66,   65,   66,
      65,   66,   65,   66,   65,   66,   65,   66,   65,   66,   65,   66,   65,   66,   65,   66,
      65,   66,   65,   66,   65,   66,   65,   66,   65,   66,   65,   66,   65,   66,   65,   66,
      65,   66,   65,   66,   66,  401,  588,  588,  588,  588,  588,   65,   66,   65,   66,  402,
     586,  586,   65,   66,  585,  585,  585,  585,  585,  403,  403,  403,  403,  404,  403,  577,
  // *** block 83 ***
     405,  405,  405,  405,  405,  405,  405,  405,  405,  405,  405,  405,  405,  405,  405,  405,
     405,  405,  405,  405,  405,  405,  405,  405,  405,  405,  405,  405,  405,  405,  405,  405,
     405,  405,  405,  405,  405,  405,  585,  405,  585,  585,  585,  585,  585,  405,  585,  585,
     406,  406,  406,  406,  406,  406,  406,  406,  406,  406,  406,  406,  406,  406,  406,  406,
     406,  406,  406,  406,  406,  406,  406,  406,  406,  406,  406,  406,  406,  406,  406,  406,
     406,  406,  406,  406,  406,  406,  406,  406,  406,  406,  406,  406,  406,  406,  406,  406,
     406,  406,  406,  406,  406,  406,  406,  406,  585,  585,  585,  585,  585,  585,  585,  407,
     408,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  409,
  // *** block 84 ***
     203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,  203,
     203,  203,  203,  203,  203,  203,  203,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     203,  203,  203,  203,  203,  203,  203,  585,  203,  203,  203,  203,  203,  203,  203,  585,
     203,  203,  203,  203,  203,  203,  203,  585,  203,  203,  203,  203,  203,  203,  203,  585,
     203,  203,  203,  203,  203,  203,  203,  585,  203,  203,  203,  203,  203,  203,  203,  585,
     203,  203,  203,  203,  203,  203,  203,  585,  203,  203,  203,  203,  203,  203,  203,  585,
      77,   77,   77,   77,   77,   77,   77,   77,   77,   77,   77,   77,   77,   77,   77,   77,
      77,   77,   77,   77,   77,   77,   77,   77,   77,   77,   77,   77,   77,   77,   77,   77,
  // *** block 85 ***
       2,    2,   16,   20,   16,   20,    2,  577,  577,   16,   20,    2,   16,   20,    2,  577,
     577,  577,  577,  577,  577,  577,  577,    7,    2,  577,    7,    2,   16,   20,    2,  577,
      16,   20,    4,    5,    4,    5,    4,    5,    4,    5,    2,  577,  577,  577,  577,   43,
       2,  577,  577,  577,  577,  577,  577,  577,  577,  577,    7,    7,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 86 ***
     410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,
     410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  585,  410,  410,  410,  410,  410,
     410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,
     410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,
     410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,
     410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,
     410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,
     410,  410,  410,  410,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 87 ***
     410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,
     410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,
     410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,
     410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,
     410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,
     410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,
     410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,
     410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,
  // *** block 88 ***
     410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,
     410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,
     410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,
     410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,
     410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,  410,
     410,  410,  410,  410,  410,  410,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,  585,  585,  585,  585,
  // *** block 89 ***
       1,    2,  577,  577,   14,  411,  289,  412,    4,    5,    4,    5,    4,    5,    4,    5,
       4,    5,   14,  588,    4,    5,    4,    5,    4,    5,    4,    5,    7,    4,    5,  597,
      14,  412,  412,  412,  412,  412,  412,  412,  412,  412,   45,   45,   45,   45,  413,  413,
       7,   43,  591,  591,  591,  591,   14,   14,  412,  412,  412,  411,  289,    2,   14,  588,
     585,  414,  414,  414,  414,  414,  414,  414,  414,  414,  414,  414,  414,  414,  414,  414,
     414,  414,  414,  414,  414,  414,  414,  414,  414,  414,  414,  414,  414,  414,  414,  414,
     414,  414,  414,  414,  414,  414,  414,  414,  414,  414,  414,  414,  414,  414,  414,  414,
     414,  414,  414,  414,  414,  414,  414,  414,  414,  414,  414,  414,  414,  414,  414,  414,
  // *** block 90 ***
     414,  414,  414,  414,  414,  414,  414,  414,  414,  414,  414,  414,  414,  414,  414,  414,
     414,  414,  414,  414,  414,  414,  414,  585,  585,   45,   45,   11,   11,  415,  415,  414,
       7,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,
     416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,
     416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,
     416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,
     416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,
     416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,    2,   43,  417,  417,  416,
  // *** block 91 ***
     585,  585,  585,  585,  585,  418,  418,  418,  418,  418,  418,  418,  418,  418,  418,  418,
     418,  418,  418,  418,  418,  418,  418,  418,  418,  418,  418,  418,  418,  418,  418,  418,
     418,  418,  418,  418,  418,  418,  418,  418,  418,  418,  418,  418,  418,  418,  585,  585,
     585,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,
     202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,
     202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,
     202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,
     202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,
  // *** block 92 ***
     202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  585,
      14,   14,   18,  580,  580,  580,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
     418,  418,  418,  418,  418,  418,  418,  418,  418,  418,  418,  418,  418,  418,  418,  418,
     418,  418,  418,  418,  418,  418,  418,  418,  418,  418,  418,  585,  585,  585,  585,  585,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,
  // *** block 93 ***
     419,  419,  419,  419,  419,  419,  419,  419,  419,  419,  419,  419,  419,  419,  419,  419,
     419,  419,  419,  419,  419,  419,  419,  419,  419,  419,  419,  419,  419,  419,  419,  585,
      18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   14,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,  588,
     588,  588,  588,  588,  588,  588,  588,  588,   18,   18,   18,   18,   18,   18,   18,   18,
      14,   18,  580,  580,  580,  580,  580,  580,  580,  580,  580,  580,  580,  580,  580,  580,
     419,  419,  419,  419,  419,  419,  419,  419,  419,  419,  419,  419,  419,  419,  419,  419,
     419,  419,  419,  419,  419,  419,  419,  419,  419,  419,  419,  419,  419,  419,  419,   14,
  // *** block 94 ***
      18,  580,  580,  580,  580,  580,  580,  580,  580,  580,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   18,  580,  580,  580,  580,  580,  580,  580,  580,  580,  580,  580,  580,  580,  580,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
     420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,
     420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,
     420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  585,
  // *** block 95 ***
     420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,
     420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,
     420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,
     420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,
     420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,  420,
     420,  420,  420,  420,  420,  420,  420,  420,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
  // *** block 96 ***
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
  // *** block 97 ***
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,
  // *** block 98 ***
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,
     421,  421,  421,  421,  421,  421,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
  // *** block 99 ***
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 100 ***
     422,  422,  422,  422,  422,  422,  422,  422,  422,  422,  422,  422,  422,  422,  422,  422,
     422,  422,  422,  422,  422,  423,  422,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
  // *** block 101 ***
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  585,  585,  585,
     424,  424,  424,  424,  424,  424,  424,  424,  424,  424,  424,  424,  424,  424,  424,  424,
     424,  424,  424,  424,  424,  424,  424,  424,  424,  424,  424,  424,  424,  424,  424,  424,
     424,  424,  424,  424,  424,  424,  424,  424,  424,  424,  424,  424,  424,  424,  424,  424,
     424,  424,  424,  424,  424,  424,  424,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     425,  425,  425,  425,  425,  425,  425,  425,  425,  425,  425,  425,  425,  425,  425,  425,
     425,  425,  425,  425,  425,  425,  425,  425,  425,  425,  425,  425,  425,  425,  425,  425,
     425,  425,  425,  425,  425,  425,  425,  425,  426,  591,  591,  591,  591,  591,  427,  427,
  // *** block 102 ***
     428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,
     428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,
     428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,
     428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,
     428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,
     428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,
     428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,
     428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,
  // *** block 103 ***
     428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  429,  430,  577,  577,
     428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,  428,
     431,  590,  590,  590,  590,  590,  590,  590,  590,  590,  428,  428,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
      75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,
      75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,
      75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,   75,   74,  432,   77,
      78,  587,  587,  433,   77,  586,  586,  586,  586,  586,  586,  586,  586,  586,  433,  290,
  // *** block 104 ***
      75,  581,  583,  581,  583,  581,  583,  581,  583,  581,  583,  581,  583,  581,  583,  581,
     583,  581,  583,  581,  583,  581,  583,  581,  585,  585,  585,  585,  585,  585,  585,   77,
     434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,
     434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,
     434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,
     434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,
     434,  434,  434,  434,  434,  434,  435,  598,  598,  598,  598,  598,  598,  598,  598,  598,
     436,  436,  437,  577,  577,  577,  577,  577,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 105 ***
      11,   11,   11,   11,   11,   11,   11,   11,   11,   11,   11,   11,   11,   11,   11,   11,
      11,   11,   11,   11,   11,   11,   11,   43,  591,  591,  591,  591,  591,  591,  591,  591,
      11,   11,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,
      13,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,
      22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,
      22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,
      22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,   22,   13,
      42,   13,  581,  581,  581,  581,  581,  581,  581,  583,  581,  583,  581,  583,  583,  581,
  // *** block 106 ***
     583,  581,  583,  581,  583,  581,  583,  581,   43,   11,  584,   22,   13,  438,   13,  585,
      22,   13,   22,   13,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
      22,   13,   22,   13,   22,   13,   22,   13,   22,   13,  439,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,   42,   42,   13,   15,  589,  589,  589,  589,
  // *** block 107 ***
     440,  440,  441,  440,  589,  589,  441,  440,  589,  589,  589,  441,  440,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  442,  442,  441,  586,  442,  443,  588,  588,  588,  585,  585,  585,  585,
      18,   18,   18,   18,   18,   18,   14,  588,    3,   14,  585,  585,  585,  585,  585,  585,
     444,  444,  444,  444,  444,  444,  444,  444,  444,  444,  444,  444,  444,  444,  444,  444,
     444,  444,  444,  444,  444,  444,  444,  444,  444,  444,  444,  444,  444,  444,  444,  444,
     444,  444,  444,  444,  444,  444,  444,  444,  444,  444,  444,  444,  444,  444,  444,  444,
     444,  444,  444,  444,  445,  577,  577,  577,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 108 ***
     446,  446,  447,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  446,  446,  446,  446,  446,  446,  446,  446,  446,  446,  446,  446,
     446,  446,  446,  446,  448,  585,  585,  585,  585,  585,  585,  585,  585,  585,  449,  449,
     450,  590,  590,  590,  590,  590,  590,  590,  590,  590,  585,  585,  585,  585,  585,  585,
     119,  119,  119,  119,  119,  119,  119,  119,  119,  119,  119,  119,  119,  119,  119,  119,
     119,  119,  121,  589,  589,  589,  589,  589,  123,  123,  123,  121,  585,  585,  585,  585,
  // *** block 109 ***
     451,  451,  451,  451,  451,  451,  451,  451,  451,  451,  452,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  453,  453,  453,  453,  453,  453,  453,  453,  454,  577,
     455,  455,  455,  455,  455,  455,  455,  455,  455,  455,  455,  455,  455,  455,  455,  455,
     455,  455,  455,  455,  455,  455,  455,  456,  586,  586,  586,  586,  586,  586,  586,  586,
     586,  586,  457,  457,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  458,
     202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,
     202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  585,  585,  585,
  // *** block 110 ***
     459,  459,  459,  460,  461,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  459,  460,  592,  459,  459,  459,  459,  460,  592,  459,  460,  592,  592,
     592,  462,  462,  462,  462,  462,  462,  462,  462,  462,  462,  462,  462,  462,  585,  463,
     464,  590,  590,  590,  590,  590,  590,  590,  590,  590,  585,  585,  585,  585,  462,  462,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 111 ***
     465,  465,  465,  465,  465,  465,  465,  465,  465,  465,  465,  465,  465,  465,  465,  465,
     465,  465,  465,  465,  465,  465,  465,  465,  465,  465,  465,  465,  465,  465,  465,  465,
     465,  465,  465,  465,  465,  465,  465,  465,  465,  466,  586,  586,  586,  586,  586,  467,
     467,  466,  586,  467,  467,  466,  586,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     465,  465,  465,  466,  465,  589,  589,  589,  589,  589,  589,  589,  466,  467,  585,  585,
     468,  468,  468,  468,  468,  468,  468,  468,  468,  468,  585,  585,  469,  469,  469,  469,
     193,  193,  193,  193,  193,  193,  193,  193,  193,  193,  193,  193,  193,  193,  193,  193,
     470,  193,  589,  589,  589,  589,  589,  198,  198,  198,  193,  194,  585,  585,  585,  585,
  // *** block 112 ***
     471,  471,  471,  471,  471,  471,  471,  471,  471,  471,  471,  471,  471,  471,  471,  471,
     471,  471,  471,  471,  471,  471,  471,  471,  471,  471,  471,  471,  471,  471,  471,  471,
     471,  471,  471,  471,  471,  471,  471,  471,  471,  471,  471,  471,  471,  471,  471,  471,
     472,  471,  472,  586,  586,  471,  471,  472,  586,  471,  471,  471,  471,  471,  472,  586,
     471,  472,  471,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  471,  471,  473,  474,  577,
     475,  475,  475,  475,  475,  475,  475,  475,  475,  475,  475,  476,  477,  586,  476,  476,
     478,  577,  475,  479,  591,  476,  477,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 113 ***
     585,  203,  203,  203,  203,  203,  203,  585,  585,  203,  203,  203,  203,  203,  203,  585,
     585,  203,  203,  203,  203,  203,  203,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     203,  203,  203,  203,  203,  203,  203,  585,  203,  203,  203,  203,  203,  203,  203,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 114 ***
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     475,  475,  475,  475,  475,  475,  475,  475,  475,  475,  475,  475,  475,  475,  475,  475,
     475,  475,  475,  475,  475,  475,  475,  475,  475,  475,  475,  475,  475,  475,  475,  475,
     475,  475,  475,  476,  592,  477,  476,  592,  477,  476,  592,  478,  476,  477,  585,  585,
     480,  480,  480,  480,  480,  480,  480,  480,  480,  480,  585,  585,  585,  585,  585,  585,
  // *** block 115 ***
     202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,
     202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,
     202,  202,  202,  202,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,
     202,  202,  202,  202,  202,  202,  202,  585,  585,  585,  585,  202,  202,  202,  202,  202,
     202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,
     202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,
     202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  585,  585,  585,  585,
  // *** block 116 ***
     599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,
     599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,
     599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,
     599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,
     599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,
     599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,
     599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,
     599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,  599,
  // *** block 117 ***
     600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,
     600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,
     600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,
     600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,
     600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,
     600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,
     600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,
     600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,
  // *** block 118 ***
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  585,  585,
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,
  // *** block 119 ***
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 120 ***
     481,  482,  483,  484,  485,  486,  486,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  487,  488,  489,  490,  491,  585,  585,  585,  585,  585,   90,   87,   90,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  492,   90,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  585,   90,   90,   90,   90,   90,  585,   90,  585,
      90,   90,  585,   90,   90,  585,   90,   90,   90,   90,   90,   90,   90,   90,   90,   90,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
  // *** block 121 ***
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
      97,   97,  493,  584,  584,  584,  584,  584,  584,  584,  584,  584,  584,  584,  584,  584,
     584,  584,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
  // *** block 122 ***
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
  // *** block 123 ***
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,    4,    5,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
  // *** block 124 ***
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
     585,  585,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
      97,   97,   97,   97,   97,   97,   97,   97,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   94,   14,  585,  585,
  // *** block 125 ***
      45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,
       2,    2,    2,    2,    2,    2,    2,    4,    5,    2,  585,  585,  585,  585,  585,  585,
      45,   45,   45,   45,   45,   45,   45,  585,  585,  585,  585,  585,  585,  585,  585,  585,
       2,    7,  593,   12,   12,    4,    5,    4,    5,    4,    5,    4,    5,    4,    5,    4,
       5,    4,    5,    4,    5,    2,  577,    4,    5,    2,  577,  577,  577,   12,   12,   12,
       2,  577,  577,  585,    2,    2,    2,    2,    7,    4,    5,    4,    5,    4,    5,    2,
     577,  577,    6,    7,    6,  596,  596,  585,    2,    3,    2,  577,  585,  585,  585,  585,
      97,   97,   97,   97,   97,  585,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
  // *** block 126 ***
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,  585,  585,   17,
  // *** block 127 ***
     585,    2,    2,    2,    3,    2,  577,  577,    4,    5,    2,    6,    2,    7,    2,  577,
       8,    8,    8,    8,    8,    8,    8,    8,    8,    8,    2,  577,    6,    6,    6,    2,
     577,    9,    9,    9,    9,    9,    9,    9,    9,    9,    9,    9,    9,    9,    9,    9,
       9,    9,    9,    9,    9,    9,    9,    9,    9,    9,    9,    4,    2,    5,   11,   12,
      11,   13,   13,   13,   13,   13,   13,   13,   13,   13,   13,   13,   13,   13,   13,   13,
      13,   13,   13,   13,   13,   13,   13,   13,   13,   13,   13,    4,    6,    5,    6,    4,
       5,    2,    4,    5,    2,  577,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,
      43,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,
  // *** block 128 ***
     416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,
     416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,  416,   43,   43,
     202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,
     202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  202,  585,
     585,  585,  202,  202,  202,  202,  202,  202,  585,  585,  202,  202,  202,  202,  202,  202,
     585,  585,  202,  202,  202,  202,  202,  202,  585,  585,  202,  202,  202,  585,  585,  585,
       3,    3,    6,   11,   14,    3,  579,  585,   14,    6,  596,  596,  596,   14,   14,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,   17,   17,   17,   14,  588,  585,  585,
  // *** block 129 ***
     494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  585,  494,  494,  494,
     494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,
     494,  494,  494,  494,  494,  494,  494,  585,  494,  494,  494,  494,  494,  494,  494,  494,
     494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  585,  494,  494,  585,  494,
     494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  585,  585,
     494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 130 ***
     494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,
     494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,
     494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,
     494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,
     494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,
     494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,
     494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,
     494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  494,  585,  585,  585,  585,  585,
  // *** block 131 ***
       2,    2,    2,  585,  585,  585,  585,   18,   18,   18,   18,   18,   18,   18,   18,   18,
      18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,
      18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,
      18,   18,   18,   18,  585,  585,  585,   14,   14,   14,   14,   14,   14,   14,   14,   14,
     495,  495,  495,  495,  495,  495,  495,  495,  495,  495,  495,  495,  495,  495,  495,  495,
     495,  495,  495,  495,  495,  495,  495,  495,  495,  495,  495,  495,  495,  495,  495,  495,
     495,  495,  495,  495,  495,  495,  495,  495,  495,  495,  495,  495,  495,  495,  495,  495,
     495,  495,  495,  495,  495,  496,  580,  580,  580,  497,  497,  497,  497,  497,  497,  497,
  // *** block 132 ***
     497,  497,  497,  497,  497,  497,  497,  497,  497,  497,  496,  585,  585,  585,  585,  585,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   45,  585,  585,
  // *** block 133 ***
     498,  498,  498,  498,  498,  498,  498,  498,  498,  498,  498,  498,  498,  498,  498,  498,
     498,  498,  498,  498,  498,  498,  498,  498,  498,  498,  498,  498,  498,  585,  585,  585,
     499,  499,  499,  499,  499,  499,  499,  499,  499,  499,  499,  499,  499,  499,  499,  499,
     499,  499,  499,  499,  499,  499,  499,  499,  499,  499,  499,  499,  499,  499,  499,  499,
     499,  499,  499,  499,  499,  499,  499,  499,  499,  499,  499,  499,  499,  499,  499,  499,
     499,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 134 ***
     500,  500,  500,  500,  500,  500,  500,  500,  500,  500,  500,  500,  500,  500,  500,  500,
     500,  500,  500,  500,  500,  500,  500,  500,  500,  500,  500,  500,  500,  500,  500,  585,
     501,  501,  501,  501,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     502,  502,  502,  502,  502,  502,  502,  502,  502,  502,  502,  502,  502,  502,  502,  502,
     502,  503,  502,  589,  589,  589,  589,  589,  589,  589,  503,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 135 ***
     504,  504,  504,  504,  504,  504,  504,  504,  504,  504,  504,  504,  504,  504,  504,  504,
     504,  504,  504,  504,  504,  504,  504,  504,  504,  504,  504,  504,  504,  504,  585,  505,
     506,  506,  506,  506,  506,  506,  506,  506,  506,  506,  506,  506,  506,  506,  506,  506,
     506,  506,  506,  506,  506,  506,  506,  506,  506,  506,  506,  506,  506,  506,  506,  506,
     506,  506,  506,  506,  585,  585,  585,  585,  506,  506,  506,  506,  506,  506,  506,  506,
     507,  508,  598,  598,  598,  598,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 136 ***
     509,  509,  509,  509,  509,  509,  509,  509,  509,  509,  509,  509,  509,  509,  509,  509,
     509,  509,  509,  509,  509,  509,  509,  509,  509,  509,  509,  509,  509,  509,  509,  509,
     509,  509,  509,  509,  509,  509,  509,  509,  510,  510,  510,  510,  510,  510,  510,  510,
     510,  510,  510,  510,  510,  510,  510,  510,  510,  510,  510,  510,  510,  510,  510,  510,
     510,  510,  510,  510,  510,  510,  510,  510,  510,  510,  510,  510,  510,  510,  510,  510,
     511,  511,  511,  511,  511,  511,  511,  511,  511,  511,  511,  511,  511,  511,  511,  511,
     511,  511,  511,  511,  511,  511,  511,  511,  511,  511,  511,  511,  511,  511,  511,  511,
     511,  511,  511,  511,  511,  511,  511,  511,  511,  511,  511,  511,  511,  511,  511,  511,
  // *** block 137 ***
     512,  512,  512,  512,  512,  512,  512,  512,  512,  512,  512,  512,  512,  512,  512,  512,
     512,  512,  512,  512,  512,  512,  512,  512,  512,  512,  512,  512,  512,  512,  585,  585,
     513,  513,  513,  513,  513,  513,  513,  513,  513,  513,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 138 ***
     514,  514,  514,  514,  514,  514,  585,  585,  514,  585,  514,  514,  514,  514,  514,  514,
     514,  514,  514,  514,  514,  514,  514,  514,  514,  514,  514,  514,  514,  514,  514,  514,
     514,  514,  514,  514,  514,  514,  514,  514,  514,  514,  514,  514,  514,  514,  514,  514,
     514,  514,  514,  514,  514,  514,  585,  514,  514,  585,  585,  585,  514,  585,  585,  514,
     515,  515,  515,  515,  515,  515,  515,  515,  515,  515,  515,  515,  515,  515,  515,  515,
     515,  515,  515,  515,  515,  515,  585,  516,  517,  580,  580,  580,  580,  580,  580,  580,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 139 ***
     518,  518,  518,  518,  518,  518,  518,  518,  518,  518,  518,  518,  518,  518,  518,  518,
     518,  518,  518,  518,  518,  518,  519,  580,  580,  580,  580,  580,  585,  585,  585,  520,
     521,  521,  521,  521,  521,  521,  521,  521,  521,  521,  521,  521,  521,  521,  521,  521,
     521,  521,  521,  521,  521,  521,  521,  521,  521,  521,  585,  585,  585,  585,  585,  522,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 140 ***
     523,  523,  523,  523,  523,  523,  523,  523,  523,  523,  523,  523,  523,  523,  523,  523,
     523,  523,  523,  523,  523,  523,  523,  523,  523,  523,  523,  523,  523,  523,  523,  523,
     524,  524,  524,  524,  524,  524,  524,  524,  524,  524,  524,  524,  524,  524,  524,  524,
     524,  524,  524,  524,  524,  524,  524,  524,  585,  585,  585,  585,  585,  585,  524,  524,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 141 ***
     525,  526,  586,  586,  585,  526,  526,  585,  585,  585,  585,  585,  526,  526,  526,  526,
     525,  589,  589,  589,  585,  525,  525,  525,  585,  525,  525,  525,  525,  525,  525,  525,
     525,  525,  525,  525,  525,  525,  525,  525,  525,  525,  525,  525,  525,  525,  525,  525,
     525,  525,  525,  525,  585,  585,  585,  585,  526,  526,  526,  585,  585,  585,  585,  526,
     527,  580,  580,  580,  580,  580,  580,  580,  585,  585,  585,  585,  585,  585,  585,  585,
     528,  528,  528,  528,  528,  528,  528,  528,  528,  585,  585,  585,  585,  585,  585,  585,
     529,  529,  529,  529,  529,  529,  529,  529,  529,  529,  529,  529,  529,  529,  529,  529,
     529,  529,  529,  529,  529,  529,  529,  529,  529,  529,  529,  529,  529,  530,  580,  531,
  // *** block 142 ***
     532,  532,  532,  532,  532,  532,  532,  532,  532,  532,  532,  532,  532,  532,  532,  532,
     532,  532,  532,  532,  532,  532,  532,  532,  532,  532,  532,  532,  532,  532,  532,  532,
     532,  532,  532,  532,  532,  532,  532,  532,  532,  532,  532,  532,  532,  532,  532,  532,
     532,  532,  532,  532,  532,  532,  585,  585,  585,  533,  533,  533,  533,  533,  533,  533,
     534,  534,  534,  534,  534,  534,  534,  534,  534,  534,  534,  534,  534,  534,  534,  534,
     534,  534,  534,  534,  534,  534,  585,  585,  535,  535,  535,  535,  535,  535,  535,  535,
     536,  536,  536,  536,  536,  536,  536,  536,  536,  536,  536,  536,  536,  536,  536,  536,
     536,  536,  536,  585,  585,  585,  585,  585,  537,  537,  537,  537,  537,  537,  537,  537,
  // *** block 143 ***
     538,  538,  538,  538,  538,  538,  538,  538,  538,  538,  538,  538,  538,  538,  538,  538,
     538,  538,  538,  538,  538,  538,  538,  538,  538,  538,  538,  538,  538,  538,  538,  538,
     538,  538,  538,  538,  538,  538,  538,  538,  538,  538,  538,  538,  538,  538,  538,  538,
     538,  538,  538,  538,  538,  538,  538,  538,  538,  538,  538,  538,  538,  538,  538,  538,
     538,  538,  538,  538,  538,  538,  538,  538,  538,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 144 ***
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     539,  539,  539,  539,  539,  539,  539,  539,  539,  539,  539,  539,  539,  539,  539,  539,
     539,  539,  539,  539,  539,  539,  539,  539,  539,  539,  539,  539,  539,  539,  539,  585,
  // *** block 145 ***
     540,  541,  540,  542,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  541,  541,  541,  541,  541,  541,  541,  541,
     541,  541,  541,  541,  541,  541,  541,  543,  577,  577,  577,  577,  577,  577,  585,  585,
     585,  585,  544,  544,  544,  544,  544,  544,  544,  544,  544,  544,  544,  544,  544,  544,
     544,  544,  544,  544,  544,  544,  545,  590,  590,  590,  590,  590,  590,  590,  590,  590,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 146 ***
     546,  546,  547,  548,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     547,  547,  547,  546,  586,  586,  586,  547,  547,  546,  586,  549,  549,  550,  549,  577,
     577,  577,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     551,  551,  551,  551,  551,  551,  551,  551,  551,  551,  551,  551,  551,  551,  551,  551,
     551,  551,  551,  551,  551,  551,  551,  551,  551,  585,  585,  585,  585,  585,  585,  585,
     552,  552,  552,  552,  552,  552,  552,  552,  552,  552,  585,  585,  585,  585,  585,  585,
  // *** block 147 ***
     553,  553,  553,  554,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  553,  553,  553,  553,  553,  555,  553,  586,  586,
     586,  586,  586,  586,  586,  585,  556,  556,  556,  556,  556,  556,  556,  556,  556,  556,
     557,  577,  577,  577,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 148 ***
     558,  558,  559,  560,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,  589,
     589,  589,  589,  559,  559,  559,  558,  586,  586,  586,  586,  586,  586,  586,  586,  559,
     559,  560,  589,  589,  589,  561,  561,  561,  561,  585,  585,  585,  585,  585,  585,  585,
     562,  562,  562,  562,  562,  562,  562,  562,  562,  562,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 149 ***
     563,  563,  563,  563,  563,  563,  563,  563,  563,  563,  563,  563,  563,  563,  563,  563,
     563,  563,  563,  563,  563,  563,  563,  563,  563,  563,  563,  563,  563,  563,  563,  563,
     563,  563,  563,  563,  563,  563,  563,  563,  563,  563,  563,  564,  565,  564,  565,  592,
     564,  564,  564,  564,  564,  564,  565,  564,  585,  585,  585,  585,  585,  585,  585,  585,
     566,  566,  566,  566,  566,  566,  566,  566,  566,  566,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 150 ***
     567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,
     567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,
     567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,
     567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,
     567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,
     567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,
     567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,
     567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,
  // *** block 151 ***
     567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,
     567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,
     567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,
     567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,
     567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,
     567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,
     567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  567,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 152 ***
     568,  568,  568,  568,  568,  568,  568,  568,  568,  568,  568,  568,  568,  568,  568,  568,
     568,  568,  568,  568,  568,  568,  568,  568,  568,  568,  568,  568,  568,  568,  568,  568,
     568,  568,  568,  568,  568,  568,  568,  568,  568,  568,  568,  568,  568,  568,  568,  568,
     568,  568,  568,  568,  568,  568,  568,  568,  568,  568,  568,  568,  568,  568,  568,  568,
     568,  568,  568,  568,  568,  568,  568,  568,  568,  568,  568,  568,  568,  568,  568,  568,
     568,  568,  568,  568,  568,  568,  568,  568,  568,  568,  568,  568,  568,  568,  568,  568,
     568,  568,  568,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     569,  569,  569,  569,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 153 ***
     570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,
     570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,
     570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,
     570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,
     570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,
     570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,
     570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,
     570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,
  // *** block 154 ***
     570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,
     570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,
     570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  570,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 155 ***
     434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,
     434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,
     434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,
     434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,
     434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,
     434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,
     434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,
     434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,
  // *** block 156 ***
     434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,
     434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,
     434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,  434,
     434,  434,  434,  434,  434,  434,  434,  434,  434,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 157 ***
     571,  571,  571,  571,  571,  571,  571,  571,  571,  571,  571,  571,  571,  571,  571,  571,
     571,  571,  571,  571,  571,  571,  571,  571,  571,  571,  571,  571,  571,  571,  571,  571,
     571,  571,  571,  571,  571,  571,  571,  571,  571,  571,  571,  571,  571,  571,  571,  571,
     571,  571,  571,  571,  571,  571,  571,  571,  571,  571,  571,  571,  571,  571,  571,  571,
     571,  571,  571,  571,  571,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     571,  572,  592,  592,  592,  592,  592,  592,  592,  592,  592,  592,  592,  592,  592,  592,
     592,  592,  592,  592,  592,  592,  592,  592,  592,  592,  592,  592,  592,  592,  592,  592,
     592,  592,  592,  592,  592,  592,  592,  592,  592,  592,  592,  592,  592,  592,  592,  585,
  // *** block 158 ***
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  573,
     573,  573,  573,  574,  591,  591,  591,  591,  591,  591,  591,  591,  591,  591,  591,  591,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 159 ***
     416,  414,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 160 ***
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 161 ***
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,  585,  585,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,  288,  592,   45,   45,   45,   14,   14,   14,  288,  592,  592,
     592,  592,  592,   17,   17,   17,   17,   17,   17,   17,   17,   45,   45,   45,   45,   45,
  // *** block 162 ***
      45,   45,   45,   14,   14,   45,   45,   45,   45,   45,   45,   45,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   45,   45,   45,   45,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 163 ***
     497,  497,  497,  497,  497,  497,  497,  497,  497,  497,  497,  497,  497,  497,  497,  497,
     497,  497,  497,  497,  497,  497,  497,  497,  497,  497,  497,  497,  497,  497,  497,  497,
     497,  497,  497,  497,  497,  497,  497,  497,  497,  497,  497,  497,  497,  497,  497,  497,
     497,  497,  497,  497,  497,  497,  497,  497,  497,  497,  497,  497,  497,  497,  497,  497,
     497,  497,  575,  586,  586,  497,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 164 ***
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,  585,  585,  585,  585,  585,  585,  585,  585,  585,
      18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,
      18,   18,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 165 ***
     382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,
     382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  383,  383,  383,  383,  383,  383,
     383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,
     383,  383,  383,  383,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,
     382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  383,  383,
     383,  383,  383,  383,  383,  585,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,
     383,  383,  383,  383,  383,  383,  383,  383,  382,  382,  382,  382,  382,  382,  382,  382,
     382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,
  // *** block 166 ***
     382,  382,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,
     383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  382,  585,  382,  382,
     585,  585,  382,  585,  585,  382,  382,  585,  585,  382,  382,  382,  382,  585,  382,  382,
     382,  382,  382,  382,  382,  382,  383,  383,  383,  383,  585,  383,  585,  383,  383,  383,
     383,  383,  383,  383,  585,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,
     382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,
     382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  383,  383,  383,  383,  383,  383,
     383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,
  // *** block 167 ***
     383,  383,  383,  383,  382,  382,  585,  382,  382,  382,  382,  585,  585,  382,  382,  382,
     382,  382,  382,  382,  382,  585,  382,  382,  382,  382,  382,  382,  382,  585,  383,  383,
     383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,
     383,  383,  383,  383,  383,  383,  383,  383,  382,  382,  585,  382,  382,  382,  382,  585,
     382,  382,  382,  382,  382,  585,  382,  585,  585,  585,  382,  382,  382,  382,  382,  382,
     382,  585,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,
     383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  382,  382,  382,  382,
     382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,
  // *** block 168 ***
     382,  382,  382,  382,  382,  382,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,
     383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,
     382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,
     382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  383,  383,  383,  383,  383,  383,
     383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,
     383,  383,  383,  383,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,
     382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  383,  383,
     383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,
  // *** block 169 ***
     383,  383,  383,  383,  383,  383,  383,  383,  382,  382,  382,  382,  382,  382,  382,  382,
     382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,
     382,  382,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,
     383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  382,  382,  382,  382,
     382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,
     382,  382,  382,  382,  382,  382,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,
     383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,
     382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,
  // *** block 170 ***
     382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  383,  383,  383,  383,  383,  383,
     383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,  383,
     383,  383,  383,  383,  383,  383,  585,  585,  382,  382,  382,  382,  382,  382,  382,  382,
     382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,  382,
     382,    6,  383,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,
     581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,    6,  383,  581,  581,  581,
     581,  581,  583,  583,  583,  583,  583,  583,  583,  583,  583,  583,  583,  583,  583,  583,
     583,  583,  583,  583,  583,  583,  583,  583,  583,  583,  583,    6,  383,  581,  581,  581,
  // *** block 171 ***
     581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,
     581,  581,  581,  581,  581,    6,  383,  581,  581,  581,  581,  581,  583,  583,  583,  583,
     583,  583,  583,  583,  583,  583,  583,  583,  583,  583,  583,  583,  583,  583,  583,  583,
     583,  583,  583,  583,  583,    6,  383,  581,  581,  581,  581,  581,  581,  581,  581,  581,
     581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,    6,
     383,  581,  581,  581,  581,  581,  583,  583,  583,  583,  583,  583,  583,  583,  583,  583,
     583,  583,  583,  583,  583,  583,  583,  583,  583,  583,  583,  583,  583,  583,  583,    6,
     383,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,
  // *** block 172 ***
     581,  581,  581,  581,  581,  581,  581,  581,  581,    6,  383,  581,  581,  581,  581,  581,
     583,  583,  583,  583,  583,  583,  583,  583,  583,  583,  583,  583,  583,  583,  583,  583,
     583,  583,  583,  583,  583,  583,  583,  583,  583,    6,  383,  581,  581,  581,  581,  581,
     581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,  581,
     581,  581,  581,    6,  383,  581,  581,  581,  581,  581,  583,  581,  585,  585,    8,    8,
       8,    8,    8,    8,    8,    8,    8,    8,    8,    8,    8,    8,    8,    8,    8,    8,
       8,    8,    8,    8,    8,    8,    8,    8,    8,    8,    8,    8,    8,    8,    8,    8,
       8,    8,    8,    8,    8,    8,    8,    8,    8,    8,    8,    8,    8,    8,    8,    8,
  // *** block 173 ***
      97,   97,   97,   97,  585,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,
     585,   97,   97,  585,   97,  585,  585,   97,  585,   97,   97,   97,   97,   97,   97,   97,
      97,   97,   97,  585,   97,   97,   97,   97,  585,   97,  585,   97,  585,  585,  585,  585,
     585,  585,   97,  585,  585,  585,  585,   97,  585,   97,  585,   97,  585,   97,   97,   97,
     585,   97,   97,  585,   97,  585,  585,   97,  585,   97,  585,   97,  585,   97,  585,   97,
     585,   97,   97,  585,   97,  585,  585,   97,   97,   97,   97,  585,   97,   97,   97,   97,
      97,   97,   97,  585,   97,   97,   97,   97,  585,   97,   97,   97,   97,  585,   97,  585,
  // *** block 174 ***
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,  585,   97,   97,   97,   97,   97,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,  585,  585,  585,  585,
     585,   97,   97,   97,  585,   97,   97,   97,   97,   97,  585,   97,   97,   97,   97,   97,
      97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,   97,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
      92,   92,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 175 ***
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,  585,  585,  585,  585,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
  // *** block 176 ***
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,  585,
     585,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,  585,
     585,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
     585,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 177 ***
      18,   18,   18,   18,   18,   18,   18,   18,   18,   18,   18,  585,  585,  585,  585,  585,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,  585,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,  585,  585,  585,  585,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
  // *** block 178 ***
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
  // *** block 179 ***
     576,   14,   14,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,  585,  585,  585,  585,  585,
      14,   14,   14,   14,   14,   14,   14,   14,   14,  585,  585,  585,  585,  585,  585,  585,
      14,   14,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 180 ***
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
      14,   14,   14,   14,   14,   14,  585,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,  585,  585,  585,
  // *** block 181 ***
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,  585,   14,   14,   14,   14,   14,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 182 ***
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,  585,
      14,  585,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
  // *** block 183 ***
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,  585,   14,   14,   14,   14,  585,  585,  585,
  // *** block 184 ***
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,  585,  585,
      14,   14,   14,   14,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 185 ***
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,   14,   14,   14,   14,   14,
  // *** block 186 ***
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,  585,  585,  585,  585,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 187 ***
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 188 ***
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,   14,
      14,   14,   14,   14,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 189 ***
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,
     421,  421,  421,  421,  421,  421,  421,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 190 ***
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,
     421,  421,  421,  421,  421,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,
  // *** block 191 ***
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,
     421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  421,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 192 ***
     585,   17,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
      17,   17,   17,   17,   17,   17,   17,   17,   17,   17,   17,   17,   17,   17,   17,   17,
      17,   17,   17,   17,   17,   17,   17,   17,   17,   17,   17,   17,   17,   17,   17,   17,
      17,   17,   17,   17,   17,   17,   17,   17,   17,   17,   17,   17,   17,   17,   17,   17,
      17,   17,   17,   17,   17,   17,   17,   17,   17,   17,   17,   17,   17,   17,   17,   17,
      17,   17,   17,   17,   17,   17,   17,   17,   17,   17,   17,   17,   17,   17,   17,   17,
      17,   17,   17,   17,   17,   17,   17,   17,   17,   17,   17,   17,   17,   17,   17,   17,
  // *** block 193 ***
      45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,
      45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,
      45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,
      45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,
      45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,
      45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,
      45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,
      45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,
  // *** block 194 ***
      45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,
      45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,
      45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,
      45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,
      45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,
      45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,
      45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,   45,
     585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,  585,
  // *** block 195 ***
     600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,
     600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,
     600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,
     600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,
     600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,
     600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,
     600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,
     600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  585,  585,
     600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  600,  585,  585
  );

CONST_TurkishAzeri = 1;

function GetCharType(Ch: UCS4Char): TUnicodeCharTypeRec; inline;

function GetUnicodeCategory(Ch: UCS4Char): TUnicodeProperty; inline;
function GetUnicodeGeneralCategory(Ch: UCS4Char): TUnicodeProperty; inline;
function GetUnicodeScript(Ch: UCS4Char): TUnicodeProperty; inline;
function GetUnicodeFoldCase(Ch: UCS4Char): TUnicodeMultiChar;
procedure ClearUnicodeMultiChar(var AMultiChar: TUnicodeMultiChar); inline;
{$IFDEF BLOCK}
function GetUnicodeBlock(Ch: UCS4Char): TUnicodeProperty;
{$ENDIF BLOCK}

function IsUnicodeProperty(Ch: UCS4Char; AType: TUnicodeProperty): Boolean;

var
  PropertyNames: THashedStringList;

implementation

function GetCharType(Ch: UCS4Char): TUnicodeCharTypeRec;
begin
  Result := UnicodeCharTypeTable[
    UnicodeCategory2Table[UnicodeCategory1Table[Ch div $80]* $80 + (Ch mod $80)]];
end;

function GetUnicodeCategory(Ch: UCS4Char): TUnicodeProperty;
begin
  Result := TUnicodeProperty(GetCharType(Ch).Category + Ord(upCc));
end;

function GetUnicodeGeneralCategory(Ch: UCS4Char): TUnicodeProperty;
begin
  Result := UnicodeGeneralCategoryTable[GeTUnicodeCategory(Ch)];
end;

function GetUnicodeScript(Ch: UCS4Char): TUnicodeProperty;
begin
  Result := TUnicodeProperty(GetCharType(Ch).Script + Ord(upunKnown));
end;

function GetUnicodeFoldCase(Ch: UCS4Char): TUnicodeMultiChar;
var
  LCase: PUnicodeMultiChar;
begin
  ClearUnicodeMultiChar(Result);

  LCase := GetCharType(Ch).FoldCase;

  case LCase[0] of
    0:
      begin
        Result[0] := 1;
        Result[1] := Ch;
      end;
    1:
      begin
        Result[0] := 1;
        Result[1] := Integer(Ch) - LCase[1];
      end;
  else
    Result := LCase^;
  end;
end;

procedure ClearUnicodeMultiChar(var AMultiChar: TUnicodeMultiChar);
var
  I: Integer;
begin
  for I := Low(AMultiChar) to High(AMultiChar) do
    AMultiChar[I] := 0;
end;

{$IFDEF BLOCK}
function GeTUnicodeBlock(Ch: UCS4Char): TUnicodeProperty;
var
  ALeft, ARight, AMid: Integer;
begin
  Result := upInBasicLatin;
  ALeft := 0;
  ARight := Length(UnicodeBlockTable) - 1;

  while (ALeft <= ARight) do
  begin
    AMid := (ALeft + ARight) shr 1;
    if (Ch >= UnicodeBlockTable[AMid, 0]) and (Ch <= UnicodeBlockTable[AMid, 1]) then
    begin
      Result := TUnicodeProperty(AMid + Ord(upInBasicLatin));
      Exit;
    end;
    if UnicodeBlockTable[AMid, 1] > Ch then
      ARight := AMid - 1
    else
      ALeft := AMid + 1;
  end;
end;
{$ENDIF BLOCK}

function IsUnicodeProperty(Ch: UCS4Char; AType: TUnicodeProperty): Boolean; overload;
begin
{$IFDEF BLOCK}
  if AType >= upInBasicLatin then
    Result := GetUnicodeBlock(Ch) = AType
  else if AType >= upunKnown then
{$ELSE}
  if AType >= upunKnown then
{$ENDIF NOT BLOCK}
  begin
    Result := GetUnicodeScript(Ch) = AType
  end
  else if AType >= upCc then
  begin
    if AType = upLC then
    begin
      Result := GetUnicodeCategory(Ch) in [upLu, upLt, upLl];
    end
    else
      Result := GetUnicodeCategory(Ch) = AType
  end
  else
    Result := GetUnicodeGeneralCategory(Ch) = AType
end;

initialization

begin
  PropertyNames := THashedStringList.Create;
  PropertyNames.Sorted := True;

  PropertyNames.AddObject('C', TObject(upC));
  PropertyNames.AddObject('L', TObject(upL));
  PropertyNames.AddObject('M', TObject(upM));
  PropertyNames.AddObject('N', TObject(upN));
  PropertyNames.AddObject('P', TObject(upP));
  PropertyNames.AddObject('S', TObject(upS));
  PropertyNames.AddObject('Z', TObject(upZ));
  PropertyNames.AddObject('L&', TObject(upLC));
  PropertyNames.AddObject('Cc', TObject(upCc));
  PropertyNames.AddObject('Cf', TObject(upCf));
  PropertyNames.AddObject('Cn', TObject(upCn));
  PropertyNames.AddObject('Co', TObject(upCo));
  PropertyNames.AddObject('Cs', TObject(upCs));
  PropertyNames.AddObject('LC', TObject(upLC));
  PropertyNames.AddObject('Ll', TObject(upLl));
  PropertyNames.AddObject('Lm', TObject(upLm));
  PropertyNames.AddObject('Lo', TObject(upLo));
  PropertyNames.AddObject('Lt', TObject(upLt));
  PropertyNames.AddObject('Lu', TObject(upLu));
  PropertyNames.AddObject('Mc', TObject(upMc));
  PropertyNames.AddObject('Me', TObject(upMe));
  PropertyNames.AddObject('Mn', TObject(upMn));
  PropertyNames.AddObject('Nd', TObject(upNd));
  PropertyNames.AddObject('Nl', TObject(upNl));
  PropertyNames.AddObject('No', TObject(upNo));
  PropertyNames.AddObject('Pc', TObject(upPc));
  PropertyNames.AddObject('Pd', TObject(upPd));
  PropertyNames.AddObject('Pe', TObject(upPe));
  PropertyNames.AddObject('Pf', TObject(upPf));
  PropertyNames.AddObject('Pi', TObject(upPi));
  PropertyNames.AddObject('Po', TObject(upPo));
  PropertyNames.AddObject('Ps', TObject(upPs));
  PropertyNames.AddObject('Sc', TObject(upSc));
  PropertyNames.AddObject('Sk', TObject(upSk));
  PropertyNames.AddObject('Sm', TObject(upSm));
  PropertyNames.AddObject('So', TObject(upSo));
  PropertyNames.AddObject('Zl', TObject(upZl));
  PropertyNames.AddObject('Zp', TObject(upZp));
  PropertyNames.AddObject('Zs', TObject(upZs));
  PropertyNames.AddObject('unKnown', TObject(upunKnown));
  PropertyNames.AddObject('Arabic', TObject(upArabic));
  PropertyNames.AddObject('ImperialAramaic', TObject(upImperialAramaic));
  PropertyNames.AddObject('Armenian', TObject(upArmenian));
  PropertyNames.AddObject('Avestan', TObject(upAvestan));
  PropertyNames.AddObject('Balinese', TObject(upBalinese));
  PropertyNames.AddObject('Bamum', TObject(upBamum));
  PropertyNames.AddObject('Batak', TObject(upBatak));
  PropertyNames.AddObject('Bengali', TObject(upBengali));
  PropertyNames.AddObject('Bopomofo', TObject(upBopomofo));
  PropertyNames.AddObject('Brahmi', TObject(upBrahmi));
  PropertyNames.AddObject('Braille', TObject(upBraille));
  PropertyNames.AddObject('Buginese', TObject(upBuginese));
  PropertyNames.AddObject('Buhid', TObject(upBuhid));
  PropertyNames.AddObject('Chakma', TObject(upChakma));
  PropertyNames.AddObject('CanadianAboriginal', TObject(upCanadianAboriginal));
  PropertyNames.AddObject('Carian', TObject(upCarian));
  PropertyNames.AddObject('Cham', TObject(upCham));
  PropertyNames.AddObject('Cherokee', TObject(upCherokee));
  PropertyNames.AddObject('Coptic', TObject(upCoptic));
  PropertyNames.AddObject('Cypriot', TObject(upCypriot));
  PropertyNames.AddObject('Cyrillic', TObject(upCyrillic));
  PropertyNames.AddObject('Devanagari', TObject(upDevanagari));
  PropertyNames.AddObject('Deseret', TObject(upDeseret));
  PropertyNames.AddObject('EgyptianHieroglyphs', TObject(upEgyptianHieroglyphs));
  PropertyNames.AddObject('Ethiopic', TObject(upEthiopic));
  PropertyNames.AddObject('Georgian', TObject(upGeorgian));
  PropertyNames.AddObject('Glagolitic', TObject(upGlagolitic));
  PropertyNames.AddObject('Gothic', TObject(upGothic));
  PropertyNames.AddObject('Greek', TObject(upGreek));
  PropertyNames.AddObject('Gujarati', TObject(upGujarati));
  PropertyNames.AddObject('Gurmukhi', TObject(upGurmukhi));
  PropertyNames.AddObject('Hangul', TObject(upHangul));
  PropertyNames.AddObject('Han', TObject(upHan));
  PropertyNames.AddObject('Hanunoo', TObject(upHanunoo));
  PropertyNames.AddObject('Hebrew', TObject(upHebrew));
  PropertyNames.AddObject('Hiragana', TObject(upHiragana));
  PropertyNames.AddObject('KatakanaOrHiragana', TObject(upKatakanaOrHiragana));
  PropertyNames.AddObject('OldItalic', TObject(upOldItalic));
  PropertyNames.AddObject('Javanese', TObject(upJavanese));
  PropertyNames.AddObject('KayahLi', TObject(upKayahLi));
  PropertyNames.AddObject('Katakana', TObject(upKatakana));
  PropertyNames.AddObject('Kharoshthi', TObject(upKharoshthi));
  PropertyNames.AddObject('Khmer', TObject(upKhmer));
  PropertyNames.AddObject('Kannada', TObject(upKannada));
  PropertyNames.AddObject('Kaithi', TObject(upKaithi));
  PropertyNames.AddObject('TaiTham', TObject(upTaiTham));
  PropertyNames.AddObject('Lao', TObject(upLao));
  PropertyNames.AddObject('Latin', TObject(upLatin));
  PropertyNames.AddObject('Lepcha', TObject(upLepcha));
  PropertyNames.AddObject('Limbu', TObject(upLimbu));
  PropertyNames.AddObject('LinearB', TObject(upLinearB));
  PropertyNames.AddObject('Lisu', TObject(upLisu));
  PropertyNames.AddObject('Lycian', TObject(upLycian));
  PropertyNames.AddObject('Lydian', TObject(upLydian));
  PropertyNames.AddObject('Mandaic', TObject(upMandaic));
  PropertyNames.AddObject('MeroiticCursive', TObject(upMeroiticCursive));
  PropertyNames.AddObject('MeroiticHieroglyphs', TObject(upMeroiticHieroglyphs));
  PropertyNames.AddObject('Malayalam', TObject(upMalayalam));
  PropertyNames.AddObject('Mongolian', TObject(upMongolian));
  PropertyNames.AddObject('MeeteiMayek', TObject(upMeeteiMayek));
  PropertyNames.AddObject('Myanmar', TObject(upMyanmar));
  PropertyNames.AddObject('Nko', TObject(upNko));
  PropertyNames.AddObject('Ogham', TObject(upOgham));
  PropertyNames.AddObject('OlChiki', TObject(upOlChiki));
  PropertyNames.AddObject('OldTurkic', TObject(upOldTurkic));
  PropertyNames.AddObject('Oriya', TObject(upOriya));
  PropertyNames.AddObject('Osmanya', TObject(upOsmanya));
  PropertyNames.AddObject('PhagsPa', TObject(upPhagsPa));
  PropertyNames.AddObject('InscriptionalPahlavi', TObject(upInscriptionalPahlavi));
  PropertyNames.AddObject('Phoenician', TObject(upPhoenician));
  PropertyNames.AddObject('Miao', TObject(upMiao));
  PropertyNames.AddObject('InscriptionalParthian', TObject(upInscriptionalParthian));
  PropertyNames.AddObject('Rejang', TObject(upRejang));
  PropertyNames.AddObject('Runic', TObject(upRunic));
  PropertyNames.AddObject('Samaritan', TObject(upSamaritan));
  PropertyNames.AddObject('OldSouthArabian', TObject(upOldSouthArabian));
  PropertyNames.AddObject('Saurashtra', TObject(upSaurashtra));
  PropertyNames.AddObject('Shavian', TObject(upShavian));
  PropertyNames.AddObject('Sharada', TObject(upSharada));
  PropertyNames.AddObject('Sinhala', TObject(upSinhala));
  PropertyNames.AddObject('SoraSompeng', TObject(upSoraSompeng));
  PropertyNames.AddObject('Sundanese', TObject(upSundanese));
  PropertyNames.AddObject('SylotiNagri', TObject(upSylotiNagri));
  PropertyNames.AddObject('Syriac', TObject(upSyriac));
  PropertyNames.AddObject('Tagbanwa', TObject(upTagbanwa));
  PropertyNames.AddObject('Takri', TObject(upTakri));
  PropertyNames.AddObject('TaiLe', TObject(upTaiLe));
  PropertyNames.AddObject('NewTaiLue', TObject(upNewTaiLue));
  PropertyNames.AddObject('Tamil', TObject(upTamil));
  PropertyNames.AddObject('TaiViet', TObject(upTaiViet));
  PropertyNames.AddObject('Telugu', TObject(upTelugu));
  PropertyNames.AddObject('Tifinagh', TObject(upTifinagh));
  PropertyNames.AddObject('Tagalog', TObject(upTagalog));
  PropertyNames.AddObject('Thaana', TObject(upThaana));
  PropertyNames.AddObject('Thai', TObject(upThai));
  PropertyNames.AddObject('Tibetan', TObject(upTibetan));
  PropertyNames.AddObject('Ugaritic', TObject(upUgaritic));
  PropertyNames.AddObject('Vai', TObject(upVai));
  PropertyNames.AddObject('OldPersian', TObject(upOldPersian));
  PropertyNames.AddObject('Cuneiform', TObject(upCuneiform));
  PropertyNames.AddObject('Yi', TObject(upYi));
  PropertyNames.AddObject('Inherited', TObject(upInherited));
  PropertyNames.AddObject('Common', TObject(upCommon));
{$IFDEF BLOCK}
  PropertyNames.AddObject('InBasicLatin', TObject(upInBasicLatin));
  PropertyNames.AddObject('InLatin1Supplement', TObject(upInLatin1Supplement));
  PropertyNames.AddObject('InLatinExtendedA', TObject(upInLatinExtendedA));
  PropertyNames.AddObject('InLatinExtendedB', TObject(upInLatinExtendedB));
  PropertyNames.AddObject('InIPAExtensions', TObject(upInIPAExtensions));
  PropertyNames.AddObject('InSpacingModifierLetters', TObject(upInSpacingModifierLetters));
  PropertyNames.AddObject('InCombiningDiacriticalMarks', TObject(upInCombiningDiacriticalMarks));
  PropertyNames.AddObject('InGreekandCoptic', TObject(upInGreekandCoptic));
  PropertyNames.AddObject('InCyrillic', TObject(upInCyrillic));
  PropertyNames.AddObject('InCyrillicSupplement', TObject(upInCyrillicSupplement));
  PropertyNames.AddObject('InArmenian', TObject(upInArmenian));
  PropertyNames.AddObject('InHebrew', TObject(upInHebrew));
  PropertyNames.AddObject('InArabic', TObject(upInArabic));
  PropertyNames.AddObject('InSyriac', TObject(upInSyriac));
  PropertyNames.AddObject('InArabicSupplement', TObject(upInArabicSupplement));
  PropertyNames.AddObject('InThaana', TObject(upInThaana));
  PropertyNames.AddObject('InNKo', TObject(upInNKo));
  PropertyNames.AddObject('InSamaritan', TObject(upInSamaritan));
  PropertyNames.AddObject('InMandaic', TObject(upInMandaic));
  PropertyNames.AddObject('InArabicExtendedA', TObject(upInArabicExtendedA));
  PropertyNames.AddObject('InDevanagari', TObject(upInDevanagari));
  PropertyNames.AddObject('InBengali', TObject(upInBengali));
  PropertyNames.AddObject('InGurmukhi', TObject(upInGurmukhi));
  PropertyNames.AddObject('InGujarati', TObject(upInGujarati));
  PropertyNames.AddObject('InOriya', TObject(upInOriya));
  PropertyNames.AddObject('InTamil', TObject(upInTamil));
  PropertyNames.AddObject('InTelugu', TObject(upInTelugu));
  PropertyNames.AddObject('InKannada', TObject(upInKannada));
  PropertyNames.AddObject('InMalayalam', TObject(upInMalayalam));
  PropertyNames.AddObject('InSinhala', TObject(upInSinhala));
  PropertyNames.AddObject('InThai', TObject(upInThai));
  PropertyNames.AddObject('InLao', TObject(upInLao));
  PropertyNames.AddObject('InTibetan', TObject(upInTibetan));
  PropertyNames.AddObject('InMyanmar', TObject(upInMyanmar));
  PropertyNames.AddObject('InGeorgian', TObject(upInGeorgian));
  PropertyNames.AddObject('InHangulJamo', TObject(upInHangulJamo));
  PropertyNames.AddObject('InEthiopic', TObject(upInEthiopic));
  PropertyNames.AddObject('InEthiopicSupplement', TObject(upInEthiopicSupplement));
  PropertyNames.AddObject('InCherokee', TObject(upInCherokee));
  PropertyNames.AddObject('InUnifiedCanadianAboriginalSyllabics', TObject(upInUnifiedCanadianAboriginalSyllabics));
  PropertyNames.AddObject('InOgham', TObject(upInOgham));
  PropertyNames.AddObject('InRunic', TObject(upInRunic));
  PropertyNames.AddObject('InTagalog', TObject(upInTagalog));
  PropertyNames.AddObject('InHanunoo', TObject(upInHanunoo));
  PropertyNames.AddObject('InBuhid', TObject(upInBuhid));
  PropertyNames.AddObject('InTagbanwa', TObject(upInTagbanwa));
  PropertyNames.AddObject('InKhmer', TObject(upInKhmer));
  PropertyNames.AddObject('InMongolian', TObject(upInMongolian));
  PropertyNames.AddObject('InUnifiedCanadianAboriginalSyllabicsExtended', TObject(upInUnifiedCanadianAboriginalSyllabicsExtended));
  PropertyNames.AddObject('InLimbu', TObject(upInLimbu));
  PropertyNames.AddObject('InTaiLe', TObject(upInTaiLe));
  PropertyNames.AddObject('InNewTaiLue', TObject(upInNewTaiLue));
  PropertyNames.AddObject('InKhmerSymbols', TObject(upInKhmerSymbols));
  PropertyNames.AddObject('InBuginese', TObject(upInBuginese));
  PropertyNames.AddObject('InTaiTham', TObject(upInTaiTham));
  PropertyNames.AddObject('InBalinese', TObject(upInBalinese));
  PropertyNames.AddObject('InSundanese', TObject(upInSundanese));
  PropertyNames.AddObject('InBatak', TObject(upInBatak));
  PropertyNames.AddObject('InLepcha', TObject(upInLepcha));
  PropertyNames.AddObject('InOlChiki', TObject(upInOlChiki));
  PropertyNames.AddObject('InSundaneseSupplement', TObject(upInSundaneseSupplement));
  PropertyNames.AddObject('InVedicExtensions', TObject(upInVedicExtensions));
  PropertyNames.AddObject('InPhoneticExtensions', TObject(upInPhoneticExtensions));
  PropertyNames.AddObject('InPhoneticExtensionsSupplement', TObject(upInPhoneticExtensionsSupplement));
  PropertyNames.AddObject('InCombiningDiacriticalMarksSupplement', TObject(upInCombiningDiacriticalMarksSupplement));
  PropertyNames.AddObject('InLatinExtendedAdditional', TObject(upInLatinExtendedAdditional));
  PropertyNames.AddObject('InGreekExtended', TObject(upInGreekExtended));
  PropertyNames.AddObject('InGeneralPunctuation', TObject(upInGeneralPunctuation));
  PropertyNames.AddObject('InSuperscriptsandSubscripts', TObject(upInSuperscriptsandSubscripts));
  PropertyNames.AddObject('InCurrencySymbols', TObject(upInCurrencySymbols));
  PropertyNames.AddObject('InCombiningDiacriticalMarksforSymbols', TObject(upInCombiningDiacriticalMarksforSymbols));
  PropertyNames.AddObject('InLetterlikeSymbols', TObject(upInLetterlikeSymbols));
  PropertyNames.AddObject('InNumberForms', TObject(upInNumberForms));
  PropertyNames.AddObject('InArrows', TObject(upInArrows));
  PropertyNames.AddObject('InMathematicalOperators', TObject(upInMathematicalOperators));
  PropertyNames.AddObject('InMiscellaneousTechnical', TObject(upInMiscellaneousTechnical));
  PropertyNames.AddObject('InControlPictures', TObject(upInControlPictures));
  PropertyNames.AddObject('InOpticalCharacterRecognition', TObject(upInOpticalCharacterRecognition));
  PropertyNames.AddObject('InEnclosedAlphanumerics', TObject(upInEnclosedAlphanumerics));
  PropertyNames.AddObject('InBoxDrawing', TObject(upInBoxDrawing));
  PropertyNames.AddObject('InBlockElements', TObject(upInBlockElements));
  PropertyNames.AddObject('InGeometricShapes', TObject(upInGeometricShapes));
  PropertyNames.AddObject('InMiscellaneousSymbols', TObject(upInMiscellaneousSymbols));
  PropertyNames.AddObject('InDingbats', TObject(upInDingbats));
  PropertyNames.AddObject('InMiscellaneousMathematicalSymbolsA', TObject(upInMiscellaneousMathematicalSymbolsA));
  PropertyNames.AddObject('InSupplementalArrowsA', TObject(upInSupplementalArrowsA));
  PropertyNames.AddObject('InBraillePatterns', TObject(upInBraillePatterns));
  PropertyNames.AddObject('InSupplementalArrowsB', TObject(upInSupplementalArrowsB));
  PropertyNames.AddObject('InMiscellaneousMathematicalSymbolsB', TObject(upInMiscellaneousMathematicalSymbolsB));
  PropertyNames.AddObject('InSupplementalMathematicalOperators', TObject(upInSupplementalMathematicalOperators));
  PropertyNames.AddObject('InMiscellaneousSymbolsandArrows', TObject(upInMiscellaneousSymbolsandArrows));
  PropertyNames.AddObject('InGlagolitic', TObject(upInGlagolitic));
  PropertyNames.AddObject('InLatinExtendedC', TObject(upInLatinExtendedC));
  PropertyNames.AddObject('InCoptic', TObject(upInCoptic));
  PropertyNames.AddObject('InGeorgianSupplement', TObject(upInGeorgianSupplement));
  PropertyNames.AddObject('InTifinagh', TObject(upInTifinagh));
  PropertyNames.AddObject('InEthiopicExtended', TObject(upInEthiopicExtended));
  PropertyNames.AddObject('InCyrillicExtendedA', TObject(upInCyrillicExtendedA));
  PropertyNames.AddObject('InSupplementalPunctuation', TObject(upInSupplementalPunctuation));
  PropertyNames.AddObject('InCJKRadicalsSupplement', TObject(upInCJKRadicalsSupplement));
  PropertyNames.AddObject('InKangxiRadicals', TObject(upInKangxiRadicals));
  PropertyNames.AddObject('InIdeographicDescriptionCharacters', TObject(upInIdeographicDescriptionCharacters));
  PropertyNames.AddObject('InCJKSymbolsandPunctuation', TObject(upInCJKSymbolsandPunctuation));
  PropertyNames.AddObject('InHiragana', TObject(upInHiragana));
  PropertyNames.AddObject('InKatakana', TObject(upInKatakana));
  PropertyNames.AddObject('InBopomofo', TObject(upInBopomofo));
  PropertyNames.AddObject('InHangulCompatibilityJamo', TObject(upInHangulCompatibilityJamo));
  PropertyNames.AddObject('InKanbun', TObject(upInKanbun));
  PropertyNames.AddObject('InBopomofoExtended', TObject(upInBopomofoExtended));
  PropertyNames.AddObject('InCJKStrokes', TObject(upInCJKStrokes));
  PropertyNames.AddObject('InKatakanaPhoneticExtensions', TObject(upInKatakanaPhoneticExtensions));
  PropertyNames.AddObject('InEnclosedCJKLettersandMonths', TObject(upInEnclosedCJKLettersandMonths));
  PropertyNames.AddObject('InCJKCompatibility', TObject(upInCJKCompatibility));
  PropertyNames.AddObject('InCJKUnifiedIdeographsExtensionA', TObject(upInCJKUnifiedIdeographsExtensionA));
  PropertyNames.AddObject('InYijingHexagramSymbols', TObject(upInYijingHexagramSymbols));
  PropertyNames.AddObject('InCJKUnifiedIdeographs', TObject(upInCJKUnifiedIdeographs));
  PropertyNames.AddObject('InYiSyllables', TObject(upInYiSyllables));
  PropertyNames.AddObject('InYiRadicals', TObject(upInYiRadicals));
  PropertyNames.AddObject('InLisu', TObject(upInLisu));
  PropertyNames.AddObject('InVai', TObject(upInVai));
  PropertyNames.AddObject('InCyrillicExtendedB', TObject(upInCyrillicExtendedB));
  PropertyNames.AddObject('InBamum', TObject(upInBamum));
  PropertyNames.AddObject('InModifierToneLetters', TObject(upInModifierToneLetters));
  PropertyNames.AddObject('InLatinExtendedD', TObject(upInLatinExtendedD));
  PropertyNames.AddObject('InSylotiNagri', TObject(upInSylotiNagri));
  PropertyNames.AddObject('InCommonIndicNumberForms', TObject(upInCommonIndicNumberForms));
  PropertyNames.AddObject('InPhagspa', TObject(upInPhagspa));
  PropertyNames.AddObject('InSaurashtra', TObject(upInSaurashtra));
  PropertyNames.AddObject('InDevanagariExtended', TObject(upInDevanagariExtended));
  PropertyNames.AddObject('InKayahLi', TObject(upInKayahLi));
  PropertyNames.AddObject('InRejang', TObject(upInRejang));
  PropertyNames.AddObject('InHangulJamoExtendedA', TObject(upInHangulJamoExtendedA));
  PropertyNames.AddObject('InJavanese', TObject(upInJavanese));
  PropertyNames.AddObject('InCham', TObject(upInCham));
  PropertyNames.AddObject('InMyanmarExtendedA', TObject(upInMyanmarExtendedA));
  PropertyNames.AddObject('InTaiViet', TObject(upInTaiViet));
  PropertyNames.AddObject('InMeeteiMayekExtensions', TObject(upInMeeteiMayekExtensions));
  PropertyNames.AddObject('InEthiopicExtendedA', TObject(upInEthiopicExtendedA));
  PropertyNames.AddObject('InMeeteiMayek', TObject(upInMeeteiMayek));
  PropertyNames.AddObject('InHangulSyllables', TObject(upInHangulSyllables));
  PropertyNames.AddObject('InHangulJamoExtendedB', TObject(upInHangulJamoExtendedB));
  PropertyNames.AddObject('InHighSurrogates', TObject(upInHighSurrogates));
  PropertyNames.AddObject('InHighPrivateUseSurrogates', TObject(upInHighPrivateUseSurrogates));
  PropertyNames.AddObject('InLowSurrogates', TObject(upInLowSurrogates));
  PropertyNames.AddObject('InPrivateUseArea', TObject(upInPrivateUseArea));
  PropertyNames.AddObject('InCJKCompatibilityIdeographs', TObject(upInCJKCompatibilityIdeographs));
  PropertyNames.AddObject('InAlphabeticPresentationForms', TObject(upInAlphabeticPresentationForms));
  PropertyNames.AddObject('InArabicPresentationFormsA', TObject(upInArabicPresentationFormsA));
  PropertyNames.AddObject('InVariationSelectors', TObject(upInVariationSelectors));
  PropertyNames.AddObject('InVerticalForms', TObject(upInVerticalForms));
  PropertyNames.AddObject('InCombiningHalfMarks', TObject(upInCombiningHalfMarks));
  PropertyNames.AddObject('InCJKCompatibilityForms', TObject(upInCJKCompatibilityForms));
  PropertyNames.AddObject('InSmallFormVariants', TObject(upInSmallFormVariants));
  PropertyNames.AddObject('InArabicPresentationFormsB', TObject(upInArabicPresentationFormsB));
  PropertyNames.AddObject('InHalfwidthandFullwidthForms', TObject(upInHalfwidthandFullwidthForms));
  PropertyNames.AddObject('InSpecials', TObject(upInSpecials));
  PropertyNames.AddObject('InLinearBSyllabary', TObject(upInLinearBSyllabary));
  PropertyNames.AddObject('InLinearBIdeograms', TObject(upInLinearBIdeograms));
  PropertyNames.AddObject('InAegeanNumbers', TObject(upInAegeanNumbers));
  PropertyNames.AddObject('InAncientGreekNumbers', TObject(upInAncientGreekNumbers));
  PropertyNames.AddObject('InAncientSymbols', TObject(upInAncientSymbols));
  PropertyNames.AddObject('InPhaistosDisc', TObject(upInPhaistosDisc));
  PropertyNames.AddObject('InLycian', TObject(upInLycian));
  PropertyNames.AddObject('InCarian', TObject(upInCarian));
  PropertyNames.AddObject('InOldItalic', TObject(upInOldItalic));
  PropertyNames.AddObject('InGothic', TObject(upInGothic));
  PropertyNames.AddObject('InUgaritic', TObject(upInUgaritic));
  PropertyNames.AddObject('InOldPersian', TObject(upInOldPersian));
  PropertyNames.AddObject('InDeseret', TObject(upInDeseret));
  PropertyNames.AddObject('InShavian', TObject(upInShavian));
  PropertyNames.AddObject('InOsmanya', TObject(upInOsmanya));
  PropertyNames.AddObject('InCypriotSyllabary', TObject(upInCypriotSyllabary));
  PropertyNames.AddObject('InImperialAramaic', TObject(upInImperialAramaic));
  PropertyNames.AddObject('InPhoenician', TObject(upInPhoenician));
  PropertyNames.AddObject('InLydian', TObject(upInLydian));
  PropertyNames.AddObject('InMeroiticHieroglyphs', TObject(upInMeroiticHieroglyphs));
  PropertyNames.AddObject('InMeroiticCursive', TObject(upInMeroiticCursive));
  PropertyNames.AddObject('InKharoshthi', TObject(upInKharoshthi));
  PropertyNames.AddObject('InOldSouthArabian', TObject(upInOldSouthArabian));
  PropertyNames.AddObject('InAvestan', TObject(upInAvestan));
  PropertyNames.AddObject('InInscriptionalParthian', TObject(upInInscriptionalParthian));
  PropertyNames.AddObject('InInscriptionalPahlavi', TObject(upInInscriptionalPahlavi));
  PropertyNames.AddObject('InOldTurkic', TObject(upInOldTurkic));
  PropertyNames.AddObject('InRumiNumeralSymbols', TObject(upInRumiNumeralSymbols));
  PropertyNames.AddObject('InBrahmi', TObject(upInBrahmi));
  PropertyNames.AddObject('InKaithi', TObject(upInKaithi));
  PropertyNames.AddObject('InSoraSompeng', TObject(upInSoraSompeng));
  PropertyNames.AddObject('InChakma', TObject(upInChakma));
  PropertyNames.AddObject('InSharada', TObject(upInSharada));
  PropertyNames.AddObject('InTakri', TObject(upInTakri));
  PropertyNames.AddObject('InCuneiform', TObject(upInCuneiform));
  PropertyNames.AddObject('InCuneiformNumbersandPunctuation', TObject(upInCuneiformNumbersandPunctuation));
  PropertyNames.AddObject('InEgyptianHieroglyphs', TObject(upInEgyptianHieroglyphs));
  PropertyNames.AddObject('InBamumSupplement', TObject(upInBamumSupplement));
  PropertyNames.AddObject('InMiao', TObject(upInMiao));
  PropertyNames.AddObject('InKanaSupplement', TObject(upInKanaSupplement));
  PropertyNames.AddObject('InByzantineMusicalSymbols', TObject(upInByzantineMusicalSymbols));
  PropertyNames.AddObject('InMusicalSymbols', TObject(upInMusicalSymbols));
  PropertyNames.AddObject('InAncientGreekMusicalNotation', TObject(upInAncientGreekMusicalNotation));
  PropertyNames.AddObject('InTaiXuanJingSymbols', TObject(upInTaiXuanJingSymbols));
  PropertyNames.AddObject('InCountingRodNumerals', TObject(upInCountingRodNumerals));
  PropertyNames.AddObject('InMathematicalAlphanumericSymbols', TObject(upInMathematicalAlphanumericSymbols));
  PropertyNames.AddObject('InArabicMathematicalAlphabeticSymbols', TObject(upInArabicMathematicalAlphabeticSymbols));
  PropertyNames.AddObject('InMahjongTiles', TObject(upInMahjongTiles));
  PropertyNames.AddObject('InDominoTiles', TObject(upInDominoTiles));
  PropertyNames.AddObject('InPlayingCards', TObject(upInPlayingCards));
  PropertyNames.AddObject('InEnclosedAlphanumericSupplement', TObject(upInEnclosedAlphanumericSupplement));
  PropertyNames.AddObject('InEnclosedIdeographicSupplement', TObject(upInEnclosedIdeographicSupplement));
  PropertyNames.AddObject('InMiscellaneousSymbolsAndPictographs', TObject(upInMiscellaneousSymbolsAndPictographs));
  PropertyNames.AddObject('InEmoticons', TObject(upInEmoticons));
  PropertyNames.AddObject('InTransportAndMapSymbols', TObject(upInTransportAndMapSymbols));
  PropertyNames.AddObject('InAlchemicalSymbols', TObject(upInAlchemicalSymbols));
  PropertyNames.AddObject('InCJKUnifiedIdeographsExtensionB', TObject(upInCJKUnifiedIdeographsExtensionB));
  PropertyNames.AddObject('InCJKUnifiedIdeographsExtensionC', TObject(upInCJKUnifiedIdeographsExtensionC));
  PropertyNames.AddObject('InCJKUnifiedIdeographsExtensionD', TObject(upInCJKUnifiedIdeographsExtensionD));
  PropertyNames.AddObject('InCJKCompatibilityIdeographsSupplement', TObject(upInCJKCompatibilityIdeographsSupplement));
  PropertyNames.AddObject('InTags', TObject(upInTags));
  PropertyNames.AddObject('InVariationSelectorsSupplement', TObject(upInVariationSelectorsSupplement));
  PropertyNames.AddObject('InSupplementaryPrivateUseAreaA', TObject(upInSupplementaryPrivateUseAreaA));
  PropertyNames.AddObject('InSupplementaryPrivateUseAreaB', TObject(upInSupplementaryPrivateUseAreaB));
{$ENDIF BLOCK}


end;

finalization

PropertyNames.Free;

end.




