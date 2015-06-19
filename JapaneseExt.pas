unit JapaneseExt;

interface

uses SkRegExpDef;

const
  CONST_Dakuten = #$FF9E;
  CONST_Handakuten = #$FF9F;
  CONST_Wide_Dakuten = #$309B;
  CONST_Wide_Handakuten = #$309C;
  CONST_Wide_Dakuten_CS = #$3099;
  CONST_Wide_Handakuten_CS = #$309A;

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

function IsDaku(S: PWideChar): Boolean; inline;
function IsHanDaku(S: PWideChar): Boolean; inline;
function IsWideKana(Ch: WideChar): Boolean; inline;
function IsHalfKatakana(S: UChar): Boolean; inline; overload;
function IsHalfKatakana(S: WideChar): Boolean; inline; overload;
function IsWideHiragana(S: UChar): Boolean; inline; overload;
function IsWideHiragana(Ch: WideChar): Boolean; inline; overload;
function IsWideKatakana(Ch: WideChar): Boolean; inline;
function IsHalfAnk(S: UChar): Boolean; inline; overload;
function IsHalfAnk(S: WideChar): Boolean; inline; overload;
function IsWideAnk(Ch: WideChar): Boolean; inline;
function ToHalf(const S: REString): REString;
function ToWide(const S: REString): REString;
function ToHiragana(const S: REString): REString;
function ToKatakana(const S: REString): REString;

implementation

// ==========日本語処理用ルーチン==========

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

end.
