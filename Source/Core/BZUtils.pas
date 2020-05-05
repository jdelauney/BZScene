(*
  @abstract(L'unité BZUtils contiens des routines utiles dans différents domaines tel que
  les nombres, les chaine de caractères, la manipulation de bits, le calcul de CRC32 etc... @br
  Certaines routines ont étés optimisées gràce à l'utilisation de l'assembleur.)

  -------------------------------------------------------------------------------------------------------------

  @created(24/02/2019)
  @author(J.Delauney (BeanzMaster))
  @bold(Historique) : @br
  @unorderedList(
    @item(Creation : 24/02/2019)
  )

  -------------------------------------------------------------------------------------------------------------

  @bold(Notes :) @br
    Par défaut, certaines routines utilisent l'assembleur. @br
  Pour désactiver l'utilisation de l'assembleur, commentez 'USE_ASM_OPTIMIZATIONS' dans bzscene_options.inc @br

  -------------------------------------------------------------------------------------------------------------

  @bold(Dépendances) : BZSceneStrConsts, BZSystem

  -------------------------------------------------------------------------------------------------------------

    @bold(Credits :)@br
      @unorderedList(
        @item(J.Delauney (BeanzMaster))
      )

  ------------------------------------------------------------------------------------------------------------

  @bold(LICENCE) : MPL / GPL

  ------------------------------------------------------------------------------------------------------------- *)
unit BZUtils;

//==============================================================================
{$mode objfpc}{$H+}
{$i ..\bzscene_options.inc}
//==============================================================================

interface

uses
  Classes, SysUtils, LCLIntf, LCLType, Graphics
  {$IFDEF WINDOWS}
  ,windows
  {$ENDIF}
  {$IFDEF UNIX}
  ,BaseUnix
  {$ENDIF};

//==============================================================================

{ Remplie un pointer avec des données alignées sur 32 bits }
procedure FillLongword(var X; Count: Integer; Value: Longword);
{ Remplie un pointer avec des données alignées sur 16 bits }
procedure FillWord(var X; Count: Cardinal; Value: LongWord);

{ Deplace un pointer avec des données alignées sur 32 bits }
procedure MoveLongword(const Source; var Dest; Count: Integer);
{ Deplace un pointer avec des données alignées sur 16 bits }
procedure MoveWord(const Source; var Dest; Count: Integer);

{ Formatte un nombre en ko,mo,go,to,eo }
function FormatByteSize(ABytes: Int64): String;
{ Raccourcis le nom d'un fichier et son dossier }
function MiniMizeName(FileName: String; Canvas: TCanvas; MaxWidth: Integer): String;

{ Multiplier et diviser des integer }
function MulDiv(Multiplicand, Multiplier, Divisor: Integer): Integer;

{ Inverse les valeur de deux Integer}
procedure Swap(var A, B: Integer); overload;
{ Inverse les valeur de deux Word}
procedure Swap(var X, Y: Word); overload;
{ Inverse les valeur de deux Single}
procedure Swap(var X, Y: Single); overload;

{ renvoi le ratio d'une valeur par rapport au pourcentage demandé }
function GetPercentRatio(Const Max:Integer;Const apercent:Integer):Integer;
{ renvoi le pourcentage d'une valeur par rapport au maximum }
function GetPercent(Const Max:Integer;Const Current:Integer):Integer;

{ Alloue un bloc mémoire en vérifiant la quantité de ram disponible (utilise GetMem) }
function memAlloc(Out Buffer:Pointer; Size:Int64):Int64;
{ Ré-alloue un bloc mémoire en vérifiant la quantité de ram disponible (utilise ReAllocMem) }
function memReAlloc(var Buffer:Pointer; Size:Int64):Int64;

{ Décalage/rotation circulaire de 'K" des valeurs d'un tableau vers la gauche }
procedure CircularShiftLeftArray(var mat : array of single; TabSize,k : Byte);
{ Décalage/rotation circulaire de 'K" des valeurs d'un tableau vers la droite }
procedure CircularShiftRightArray(var mat : array of single; TabSize,k : Byte);

{ Efface le bit demandé d'une valeur de 32 bits }
function ClearBit(const AValue: DWORD; const Bit: Byte): DWORD;
{ Définis le bit demandé d'une valeur de 32 bits }
function SetBit(const AValue: DWORD; const Bit: Byte): DWORD;
{ Active ou désactive le bit demandé d'une valeur de 32 bits }
function EnableBit(const AValue: DWORD; const Bit: Byte; const Enable: Boolean): DWORD;
{ Renvoie l'état du bit demandé d'une valeur de 32 bits }
function GetBit(const AValue: DWORD; const Bit: Byte): Boolean;
{ Renvoie l'état de la plage de bits entre 'BitI' et 'BitF' d'une valeur de 32 bits }
function GetBitsValue(const AValue: DWORD; const BitI, BitF: Byte): DWORD;
{ Renvoie les états des bits 'NumBits' de 'AIndex' d'une valeur de 32 bits }
function GetBits(const AValue: DWORD;const AIndex, numbits: Integer): Integer;
{ Inverser des bits d'une valeur de type Byte}
function SwapBits(b: Byte): Byte; overload;
{ Renvoie le nombre de bits activés dans une valeur de typ byte }
function CountBits(Value: byte): shortint;
{ Renvoie le nombre de décalages d'un masque de 32 bits}
function ShiftCount(Mask: DWORD): shortint;
{ Inverse les Bits d'une valeur de type QWord }
function SwapBits(This : qword): qword; overload;
{ Inverse les Bits d'une valeur de type Int64 }
function SwapBits(This : int64): int64; overload;
{ Inverse les Bits d'une valeur de type LongWord }
function SwapBits(This : Longword): longword; overload;
{ Inverse les Bits d'une valeur de type Integer }
function SwapBits(This : integer): integer; overload;
{ Inverse les Bits d'une valeur de type Word }
function SwapBits(This : Word): Word; overload;

//function RotateLeftBit;
//function RotateRightBit;

{ Calcul le CRC32 d'un tampon de données }
procedure CRC32 (p: pointer; ByteCount: LongWord; VAR CRCValue: LongWord);

//==============================================================================

implementation

Uses
  //LConvEncoding,

  LazUTF8,
  BZSceneStrConsts,
  BZSystem;

//==============================================================================

const _CRC_TABLE32 : array[0..255] of LongWord =
 ($00000000, $77073096, $EE0E612C, $990951BA,
  $076DC419, $706AF48F, $E963A535, $9E6495A3,
  $0EDB8832, $79DCB8A4, $E0D5E91E, $97D2D988,
  $09B64C2B, $7EB17CBD, $E7B82D07, $90BF1D91,
  $1DB71064, $6AB020F2, $F3B97148, $84BE41DE,
  $1ADAD47D, $6DDDE4EB, $F4D4B551, $83D385C7,
  $136C9856, $646BA8C0, $FD62F97A, $8A65C9EC,
  $14015C4F, $63066CD9, $FA0F3D63, $8D080DF5,
  $3B6E20C8, $4C69105E, $D56041E4, $A2677172,
  $3C03E4D1, $4B04D447, $D20D85FD, $A50AB56B,
  $35B5A8FA, $42B2986C, $DBBBC9D6, $ACBCF940,
  $32D86CE3, $45DF5C75, $DCD60DCF, $ABD13D59,
  $26D930AC, $51DE003A, $C8D75180, $BFD06116,
  $21B4F4B5, $56B3C423, $CFBA9599, $B8BDA50F,
  $2802B89E, $5F058808, $C60CD9B2, $B10BE924,
  $2F6F7C87, $58684C11, $C1611DAB, $B6662D3D,

  $76DC4190, $01DB7106, $98D220BC, $EFD5102A,
  $71B18589, $06B6B51F, $9FBFE4A5, $E8B8D433,
  $7807C9A2, $0F00F934, $9609A88E, $E10E9818,
  $7F6A0DBB, $086D3D2D, $91646C97, $E6635C01,
  $6B6B51F4, $1C6C6162, $856530D8, $F262004E,
  $6C0695ED, $1B01A57B, $8208F4C1, $F50FC457,
  $65B0D9C6, $12B7E950, $8BBEB8EA, $FCB9887C,
  $62DD1DDF, $15DA2D49, $8CD37CF3, $FBD44C65,
  $4DB26158, $3AB551CE, $A3BC0074, $D4BB30E2,
  $4ADFA541, $3DD895D7, $A4D1C46D, $D3D6F4FB,
  $4369E96A, $346ED9FC, $AD678846, $DA60B8D0,
  $44042D73, $33031DE5, $AA0A4C5F, $DD0D7CC9,
  $5005713C, $270241AA, $BE0B1010, $C90C2086,
  $5768B525, $206F85B3, $B966D409, $CE61E49F,
  $5EDEF90E, $29D9C998, $B0D09822, $C7D7A8B4,
  $59B33D17, $2EB40D81, $B7BD5C3B, $C0BA6CAD,

  $EDB88320, $9ABFB3B6, $03B6E20C, $74B1D29A,
  $EAD54739, $9DD277AF, $04DB2615, $73DC1683,
  $E3630B12, $94643B84, $0D6D6A3E, $7A6A5AA8,
  $E40ECF0B, $9309FF9D, $0A00AE27, $7D079EB1,
  $F00F9344, $8708A3D2, $1E01F268, $6906C2FE,
  $F762575D, $806567CB, $196C3671, $6E6B06E7,
  $FED41B76, $89D32BE0, $10DA7A5A, $67DD4ACC,
  $F9B9DF6F, $8EBEEFF9, $17B7BE43, $60B08ED5,
  $D6D6A3E8, $A1D1937E, $38D8C2C4, $4FDFF252,
  $D1BB67F1, $A6BC5767, $3FB506DD, $48B2364B,
  $D80D2BDA, $AF0A1B4C, $36034AF6, $41047A60,
  $DF60EFC3, $A867DF55, $316E8EEF, $4669BE79,
  $CB61B38C, $BC66831A, $256FD2A0, $5268E236,
  $CC0C7795, $BB0B4703, $220216B9, $5505262F,
  $C5BA3BBE, $B2BD0B28, $2BB45A92, $5CB36A04,
  $C2D7FFA7, $B5D0CF31, $2CD99E8B, $5BDEAE1D,

  $9B64C2B0, $EC63F226, $756AA39C, $026D930A,
  $9C0906A9, $EB0E363F, $72076785, $05005713,
  $95BF4A82, $E2B87A14, $7BB12BAE, $0CB61B38,
  $92D28E9B, $E5D5BE0D, $7CDCEFB7, $0BDBDF21,
  $86D3D2D4, $F1D4E242, $68DDB3F8, $1FDA836E,
  $81BE16CD, $F6B9265B, $6FB077E1, $18B74777,
  $88085AE6, $FF0F6A70, $66063BCA, $11010B5C,
  $8F659EFF, $F862AE69, $616BFFD3, $166CCF45,
  $A00AE278, $D70DD2EE, $4E048354, $3903B3C2,
  $A7672661, $D06016F7, $4969474D, $3E6E77DB,
  $AED16A4A, $D9D65ADC, $40DF0B66, $37D83BF0,
  $A9BCAE53, $DEBB9EC5, $47B2CF7F, $30B5FFE9,
  $BDBDF21C, $CABAC28A, $53B39330, $24B4A3A6,
  $BAD03605, $CDD70693, $54DE5729, $23D967BF,
  $B3667A2E, $C4614AB8, $5D681B02, $2A6F2B94,
  $B40BBE37, $C30C8EA1, $5A05DF1B, $2D02EF8D);

//==============================================================================
Const
  cMaxArray = (MaxInt Shr 4);

Type
  TLongWordArray = Array [0 .. cMaxArray] Of Longword;
  PLongWordArray  = ^TLongWordArray;


Type
  TFillLongWordProc = procedure(var X; Count: Integer; Value: Longword);

Var
 Proc_FillLongWord :TFillLongWordProc;

{%region%=====[ Misc Utilities ]================================================}

procedure CRC32 (p: pointer; ByteCount: LongWord; VAR CRCValue: LongWord);
// Ce qui suit est un peu énigmatique (mais s'exécute très rapidement).
// L'algorithme est le suivant:
// 1. ou exclusif de l'octet d'entrée avec l'octet de poids faible du registre CRC pour obtenir un INDEX
// 2. déplace le registre CRC de huit bits vers la droite
// 3. ou exclusif ou du registre CRC avec le contenu de la table [INDEX]
// 4. répéte les étapes 1 à 3 pour tous les octets
VAR
  i:  LongWord;
  q: ^BYTE;
BEGIN
 q := p;
 FOR i := 0 TO ByteCount-1 DO
 BEGIN
   CRCvalue := (CRCvalue SHR 8) XOR _CRC_Table32[ q^ XOR (CRCvalue AND $000000FF) ];
   INC(q)
 END
END;

function Sar(const AValue: integer; const AShift: Byte): Integer;
begin
  if AValue < 0 then
    Result := not (not AValue shr AShift)
  else
    Result := AValue shr AShift;
end;

procedure Swap(var A, B: Integer); inline;
var
  T: Integer;
begin
  T := A;
  A := B;
  B := T;
end;

procedure Swap(var X, Y: Word); inline;
var F: Word;
begin
  F := X;
  X := Y;
  Y := F;
end;

procedure Swap(var X, Y: Single); inline;
var F: Single;
begin
  F := X;
  X := Y;
  Y := F;
end;

function MiniMizeName(FileName: String; Canvas: TCanvas; MaxWidth: Integer): String;
{
 This function will return a shortened version of FileName, so that it fits
 on the given Canvas, with a given MaxWidth.
 eg. C:\Documents and Settings\User\Application Data\Microsoft\Word\custom.dic
     would become something like: C:\...\Word\custom.dic
}
  procedure RemoveFirstDir(var Dir: String);
  {
   This procedure will remove the first directory from Dir
   and will set ADelim to the Delimiter that separated the first Dir
   eg. In: Dir: 'Dir1\Dir2\Dir3'
  }
  var p: Integer;
  begin
    p:= Pos(PathDelim,Dir);
    if (p > 0) then
    begin
      Dir := Copy(Dir,p+1,Length(Dir)-p);
    end;
  end;
var Drive, Dir, Fn: String;
    ComposedName: String;
    TWidth: Integer;
begin
  Result := FileName;
  //if FileName does not contain any (sub)dir then return FileName
  if Pos(PathDelim, FileName) = 0 then Exit;
  //if FileName fits, no need to do anyhing
  if Canvas.TextWidth(FileName) <= MaxWidth then Exit;
  Drive := ExtractFileDrive(FileName);
  Fn := ExtractFileName(FileName);
  Dir := ExtractFilePath(FileName);
  //Remove Drive from Dir
  if (Length(Drive) > 0) then System.Delete(Dir, 1, Length(Drive));
  //Transfer all PathDelimiters at the start of Dir to Drive
  While (Length(Dir) > 0) and (Dir[1] in ['/','\']) do
  begin
    Drive := Drive + Dir[1];
    System.Delete(Dir,1,1);
  end;
  //if Dir is empty then we cannot shorten it,
  //and we know at this point that Drive+FileName is too long, so we return only filename
  if (Length(Dir) = 0) then
  begin
    Result := Fn;
    Exit;
  end;
  repeat
    //at this point we know that Dir ends with PathDelim (otherwise we exited before this point,
    //so RemoveFirstDir will return a truncated Dir or an empty string
    RemoveFirstDir(Dir);
    ComposedName := Drive+'...'+PathDelim+Dir+Fn;
    TWidth := Canvas.TextWidth(ComposedName);
  until (Length(Dir) = 0) or (TWidth <= MaxWidth);
  if (TWidth <= MaxWidth) then Result := ComposedName else Result := Fn;
end;

function FormatByteSize(ABytes: Int64): String;
const
  suffix : array[0..6] of String = ('b', 'Kb', 'Mb', 'Go', 'To', 'Po', 'Eo');
var
  l : Integer;
  fr : Double;
begin
  l := 0;
  fr := ABytes;
  while fr >= 1024 do
  begin
    inc(l);
    fr := fr / 1024;
  end;
  if fr >= 1000 then   // ensures eg. 1022 MB will show 0.99 GB
  begin
    inc(l);
    fr := fr / 1024;
  end;
  if l > High(suffix) then
    Result := rsTooLarge
  else
    Result := Format('%f %s', [fr, suffix[l]]);
end;

{$IFDEF  USE_ASM_OPTIMIZATIONS}
  function MulDiv(Multiplicand, Multiplier, Divisor: Integer): Integer;assembler; nostackframe;
  {$IFDEF CPU32}
  asm
          PUSH    EBX             // Imperative save
          PUSH    ESI             // of EBX and ESI

          MOV     EBX, EAX        // Result will be negative or positive so set rounding direction
          XOR     EBX, EDX        //  Negative: substract 1 in case of rounding
          XOR     EBX, ECX        //  Positive: add 1

          OR      EAX, EAX        // Make all operands positive, ready for unsigned operations
          JNS     @m1Ok           // minimizing branching
          NEG     EAX
  @m1Ok:
          OR      EDX, EDX
          JNS     @m2Ok
          NEG     EDX
  @m2Ok:
          OR      ECX, ECX
          JNS     @DivOk
          NEG     ECX
  @DivOK:
          MUL     EDX             // Unsigned multiply (Multiplicand*Multiplier)

          MOV     ESI, EDX        // Check for overflow, by comparing
          SHL     ESI, 1          // 2 times the high-order 32 bits of the product (EDX)
          CMP     ESI, ECX        // with the Divisor.
          JAE     @Overfl         // If equal or greater than overflow with division anticipated

          DIV     ECX             // Unsigned divide of product by Divisor

          SUB     ECX, EDX        // Check if the result must be adjusted by adding or substracting
          CMP     ECX, EDX        // 1 (*.5 -> nearest integer), by comparing the difference of
          JA      @NoAdd          // Divisor and remainder with the remainder. If it is greater then
          INC     EAX             // no rounding needed; add 1 to result otherwise
  @NoAdd:
          OR      EBX, EDX        // From unsigned operations back the to original sign of the result
          JNS     @Exit           // must be positive
          NEG     EAX             // must be negative
          JMP     @Exit
  @Overfl:
          OR      EAX, -1         //  3 bytes alternative for MOV EAX,-1. Windows.MulDiv "overflow"
                                  //  and "zero-divide" return value
  @Exit:
          POP     ESI             // Restore
          POP     EBX             // esi and EBX
  end;
  {$ENDIF}
  {$IFDEF CPU64}
  asm
          MOV     EAX, ECX        // Result will be negative or positive so set rounding direction
          XOR     ECX, EDX        //  Negative: substract 1 in case of rounding
          XOR     ECX, R8D        //  Positive: add 1

          OR      EAX, EAX        // Make all operands positive, ready for unsigned operations
          JNS     @m1Ok           // minimizing branching
          NEG     EAX
  @m1Ok:
          OR      EDX, EDX
          JNS     @m2Ok
          NEG     EDX
  @m2Ok:
          OR      R8D, R8D
          JNS     @DivOk
          NEG     R8D
  @DivOK:
          MUL     EDX             // Unsigned multiply (Multiplicand*Multiplier)

          MOV     R9D, EDX        // Check for overflow, by comparing
          SHL     R9D, 1          // 2 times the high-order 32 bits of the product (EDX)
          CMP     R9D, R8D        // with the Divisor.
          JAE     @Overfl         // If equal or greater than overflow with division anticipated

          DIV     R8D             // Unsigned divide of product by Divisor

          SUB     R8D, EDX        // Check if the result must be adjusted by adding or substracting
          CMP     R8D, EDX        // 1 (*.5 -> nearest integer), by comparing the difference of
          JA      @NoAdd          // Divisor and remainder with the remainder. If it is greater then
          INC     EAX             // no rounding needed; add 1 to result otherwise
  @NoAdd:
          OR      ECX, EDX        // From unsigned operations back the to original sign of the result
          JNS     @Exit           // must be positive
          NEG     EAX             // must be negative
          JMP     @Exit
  @Overfl:
          OR      EAX, -1         //  3 bytes alternative for MOV EAX,-1. Windows.MulDiv "overflow"
                                  //  and "zero-divide" return value
  @Exit:
  end;
  {$ENDIF}
{$ELSE}
function MulDiv(Multiplicand, Multiplier, Divisor: Integer): Integer;
begin
  Result := Int64(Multiplicand) * Int64(Multiplier) div Divisor;
end;
{$ENDIF}

function GetPercentRatio(Const Max:Integer;Const apercent:Integer):Integer;
Begin
  result := MulDiv(Max, apercent, 100)
End;

function GetPercent(Const Max:Integer;Const Current:Integer):Integer;
Begin
  result := MulDiv(Current, 100, Max)
End;

function memAlloc(Out Buffer:Pointer; Size:Int64):Int64;
Var
  ms : TBZMemoryStatus;
begin
  Buffer := nil;
  ms := GetMemoryStatus;
  Result := -1;
  if Size > ms.AvailPhys then
  begin
    Raise Exception.Create(rsOutOfMemory);
    exit;
  End;
  GetMem(Buffer, Size);
End;

function memReAlloc(var Buffer:Pointer; Size:Int64):Int64;
Var
  ms : TBZMemoryStatus;
begin
 // if Buffer<>nil then FreeMem(Buffer);
 {$IFDEF WINDOWS}
  ms := GetMemoryStatus;
  Result := -1;

  if (Size + 8192) < ms.AvailPhys then // +8Mo
  begin
    Try
      Try
        ReAllocMem(Buffer, Size);
      except
        On E: EOutOfMemory do
        begin
          Result := -1;
        end;
      end;
    finally
      Result := Size;
    end;
  end
  else
  begin
    Raise Exception.Create(rsOutOfMemory);
    exit;
  End;
  {$ELSE}
  ReAllocMem(Buffer, Size);
  Result := Size;
  {$ENDIF}
End;

procedure CircularShiftRightArray(var mat : array of single; TabSize, k : Byte);
Var
  Row, Col, M, N, I, J : Integer;
  Prev, Curr : Single;
begin
  For j := 0 to k - 1 do
  begin
    Row := 0;
    Col := 0;
    M := TabSize;
    N := TabSize;
    While (Row < M) and (Col < N) do
    begin
      if ( ((row + 1) = m) or ((col + 1) = n)) then break;

      Prev := mat[(Row + 1) * TabSize + Col];
      // On déplace la 1ere ligne vers la gauche
      For i := Col to (N-1) do
      begin
        Curr := mat[(Row * TabSize) + i];
        mat[(Row * TabSize) + i] := Prev;
        Prev := Curr;
      end;

      Inc(Row);
      // On déplace la derniere colonne vers le bas
      For I := Row to (M-1) do
      begin
        Curr := mat[(I * TabSize) + (N-1)];
        mat[(I * TabSize) + (N-1)] := Prev;
        Prev := Curr;
      end;

      Dec(N);
      // On déplace la derniere ligne vers la droite
      if (Col < N) then
      begin
        For i := (N-1) downto Col do
        begin
          Curr := mat[((M-1) * TabSize) + I];
          mat[((M-1) * TabSize) + i] := Prev;
          Prev := Curr;
        end;
      end;

      Dec(M);
      // On déplace la 1ere colonne vers le haut
      if (Row < M) then
      begin
        For i := (M-1) downto Row do
        begin
          Curr := mat[(I * TabSize) + Col];
          mat[(I * TabSize) + Col] := Prev;
          Prev := Curr;
        end;
      end;
      Inc(Col);
    end;
  end;
end;

procedure CircularShiftLeftArray(var mat : array of single; TabSize,k : Byte);
Var
  Row, Col, M, N, I, J : Integer;
  Prev, Curr : Single;
begin
  For J := 0 to k-1 do
  begin
    Row := 0;
    Col := 0;
    M := TabSize;
    N := TabSize;
    While (Row < M) and (Col < N) do
    begin
      if ( ((row + 1) = m) or ((col + 1) = n)) then break;

      Prev := mat[(Row + 1) * TabSize + (N-1)];
      // On déplace la 1ere ligne vers la droite
      For i := (N-1) DownTo Col do
      begin
        Curr := mat[(Row * TabSize) + i];
        mat[(Row * TabSize) + i] := Prev;
        Prev := Curr;
      end;

      Inc(Row);
      // On déplace la premiere colonne vers le bas
      For I := Row to (M-1) do
      begin
        Curr := mat[(I * TabSize) + Col];
        mat[(I * TabSize) + Col] := Prev;
        Prev := Curr;
      end;
      Inc(Col);
      //Inc(Col);
      // On déplace la derniere ligne vers la gauche
      if (Col < N) then
      begin
        For i := Col to (N-1) do
        begin
          Curr := mat[((M-1) * TabSize) + I];
          mat[((M-1) * TabSize) + i] := Prev;
          Prev := Curr;
        end;
      end;
      Dec(M);
      // On déplace la derniere colonne vers le haut
      if (Row < M) then
      begin
        For i := (M-1) downto Row do
        begin
          Curr := mat[(I * TabSize) + (N-1)];
          mat[(I * TabSize) + (N-1)] := Prev;
          Prev := Curr;
        end;
      end;
      Dec(N);
    end;
  end;
end;

{%endregion%}

{%region%=====[ Manupilation de bits ]============================================}

function GetBit(const AValue: DWORD; const Bit: Byte): Boolean;
begin
  Result := (AValue and (1 shl Bit)) <> 0;
end;

function GetBits(const AValue: DWORD;const AIndex, numbits: Integer): Integer;
var
  Offset: Integer;
  BitCount: Integer;
  Mask: Integer;
begin
  BitCount := AIndex and numbits; //FF;
  Offset := AIndex shr 8;
  Mask := ((1 shl BitCount) - 1);
  Result := (AValue shr Offset) and Mask;
end;

function ClearBit(const AValue: DWORD; const Bit: Byte): DWORD;
begin
  Result := AValue and not (1 shl Bit);
end;

function SetBit(const AValue: DWORD; const Bit: Byte): DWORD;
begin
  Result := AValue or (DWORD(1) shl DWORD(Bit));
end;

function EnableBit(const AValue: DWORD; const Bit: Byte; const Enable: Boolean): DWORD;
begin
  Result := (AValue or (DWORD(1) shl Bit)) xor (DWord(not Enable) shl Bit);
end;

function GetBitsValue(const AValue: DWORD; const BitI, BitF: Byte): DWORD;
var
 i,j : Byte;
begin
 Result:=0;
  j:=0;
  for i := BitF to BitI do
  begin
    if GetBit(AValue, i) then
       Result:=Result+ (DWORD(1) shl j);
    inc(j);
  end;
end;

function CountBits(Value: byte): shortint;
var
  i, bits: shortint;
begin
  bits := 0;
  for i := 0 to 7 do
  begin
    if (value mod 2) <> 0 then
      inc(bits);
    value := value shr 1;
  end;
  Result := bits;
end;

function ShiftCount(Mask: DWORD): shortint;
var
  tmp: shortint;

begin
  tmp := 0;

  if Mask = 0 then
  begin
    Result := 0;
    exit;
  end;

  while (Mask mod 2) = 0 do // rightmost bit is 0
  begin
    inc(tmp);
    Mask := Mask shr 1;
  end;
  tmp := tmp - (8 - CountBits(Mask and $FF));
  Result := tmp;
end;

function SwapBits(b: Byte): Byte;
var c: Byte;
begin
  c := b;
  c := ((c shr 1) and $55) or ((c shl 1) and $AA);
  c := ((c shr 2) and $33) or ((c shl 2) and $CC);
  c := ((c shr 4) and $0F) or ((c shl 4) and $F0);
  result := c;
end;

function SwapBits(This : Word): Word;
var
  Tmp1, Tmp2 : Byte;
  AWord      : Word;
begin
  Tmp1 := This AND $00FF;
  Tmp2 := (This AND $FF00) SHR 8;
  AWord := Tmp1;
  result := (AWord SHL 8) + Tmp2;
end;

function SwapBits(This : integer): integer;
begin
  result := integer(Swap(longword(This)));
end;

function SwapBits(This : longword): longword;
var
  TmpW1 : Word;
  TmpB1,
  TmpB2 : Byte;
  AnInt : longword;
begin
  TmpW1 := This AND $0000FFFF;
  TmpB1 := TmpW1 AND $00FF;
  TmpB2 := (TmpW1 AND $FF00) SHR 8;
  AnInt := TmpB1;
  AnInt := (AnInt SHL 8) + TmpB2;
  TmpW1 := (This AND $FFFF0000) SHR 16;
  TmpB1 := TmpW1 AND $00FF;
  TmpB2 := (TmpW1 AND $FF00) SHR 8;
  TmpW1 := TmpB1;
  result := (AnInt SHL 16) + (TmpW1 SHL 8) + TmpB2;
end;

function SwapBits(This : qword): qword;
var l1, l2 : longword;
    res : qword;
begin
  l1:=This and $00000000FFFFFFFF;
  l2:=(This and $FFFFFFFF00000000) shr 32;
  l1:=swap(l1);
  l2:=swap(l2);
  res:=l1;
  Result:=(res shl 32) + l2;
end;

function SwapBits(This : int64): int64;
begin
  result := int64(Swap(qword(This)));
end;

{%endregion%}

{%region%=====[ Fill and Move optimized functions ]=============================}

procedure FillLongword(var X; Count: Integer; Value: Longword);
begin
 Proc_FillLongWord(X,Count,Value);
end;

procedure nc_FillLongword(var X; Count: Integer; Value: Longword);
var
  I: Integer;
  P: PLongWordArray ;
begin
  P := PLongWordArray(@X);
 // for I := Count - 1 downto 0 do
  I:=Count;
  While I>0 do
  begin
    P^[I] :=LongWord(Value);
    Dec(I);
  End;
end;

{$IFDEF  USE_ASM_OPTIMIZATIONS}
{$IFDEF WINDOWS}
{$IFDEF CPU32}
procedure asm_sse2_FillLongword(var X; Count: Integer; Value: Longword); assembler; nostackframe;
asm
        // EAX = X;   EDX = Count;   ECX = Value

        TEST       EDX, EDX        // if Count = 0 then
        JZ         @Exit           //   Exit

        PUSH       EDI             // push EDI on stack
        MOV        EDI, EAX        // Point EDI to destination

        CMP        EDX, 32
        JL         @SmallLoop

        AND        EAX, 3          // get aligned count
        TEST       EAX, EAX        // check if X is not dividable by 4
        JNZ        @SmallLoop      // otherwise perform slow small loop

        MOV        EAX, EDI
        SHR        EAX, 2          // bytes to count
        AND        EAX, 3          // get aligned count
        ADD        EAX,-4
        NEG        EAX             // get count to advance
        JZ         @SetupMain
        SUB        EDX, EAX        // subtract aligning start from total count

@AligningLoop:
        MOV        [EDI], ECX
        ADD        EDI, 4
        DEC        EAX
        JNZ        @AligningLoop

@SetupMain:
        MOV        EAX, EDX        // EAX = remaining count
        SHR        EAX, 2
        SHL        EAX, 2
        SUB        EDX, EAX        // EDX = remaining count
        SHR        EAX, 2

        MOVD       XMM0, ECX
        PUNPCKLDQ  XMM0, XMM0
        PUNPCKLDQ  XMM0, XMM0
@SSE2Loop:
        MOVDQA     [EDI], XMM0
        ADD        EDI, 16
        DEC        EAX
        JNZ        @SSE2Loop

@SmallLoop:
        MOV        EAX,ECX
        MOV        ECX,EDX

        REP        STOSD           // Fill count dwords

@ExitPOP:
        POP        EDI

@Exit:
end;
{$ENDIF}
{$IFDEF CPU64}
procedure asm_sse2_FillLongword(var X; Count: Integer; Value: Longword); assembler; nostackframe;
asm
        // RCX = X;   RDX = Count;   R8 = Value

        TEST       RDX, RDX        // if Count = 0 then
        JZ         @Exit           //   Exit

        MOV        R9, RCX         // Point R9 to destination

        CMP        RDX, 32
        JL         @SmallLoop

        AND        RCX, 3          // get aligned count
        TEST       RCX, RCX        // check if X is not dividable by 4
        JNZ        @SmallLoop      // otherwise perform slow small loop

        MOV        RCX, R9
        SHR        RCX, 2          // bytes to count
        AND        RCX, 3          // get aligned count
        ADD        RCX,-4
        NEG        RCX             // get count to advance
        JZ         @SetupMain
        SUB        RDX, RCX        // subtract aligning start from total count

@AligningLoop:
        MOV        [R9], R8D
        ADD        R9, 4
        DEC        RCX
        JNZ        @AligningLoop

@SetupMain:
        MOV        RCX, RDX        // RCX = remaining count
        SHR        RCX, 2
        SHL        RCX, 2
        SUB        RDX, RCX        // RDX = remaining count
        SHR        RCX, 2

        MOVD       XMM0, R8D
        PUNPCKLDQ  XMM0, XMM0
        PUNPCKLDQ  XMM0, XMM0
@SSE2Loop:
        MOVDQA     [R9], XMM0
        ADD        R9, 16
        DEC        RCX
        JNZ        @SSE2Loop

        TEST       RDX, RDX
        JZ         @Exit
@SmallLoop:
        MOV        [R9], R8D
        ADD        R9, 4
        DEC        RDX
        JNZ        @SmallLoop
@Exit:
end;
{$ENDIF}
(*{$ELSE}
procedure asm_sse2_FillLongword(var X; Count: Integer; Value: Longword); assembler; nostackframe;
{$IFDEF CPU32}
asm
        // EAX = X;   EDX = Count;   ECX = Value

        TEST       EDX, EDX        // if Count = 0 then
        JZ         @Exit           //   Exit

        PUSH       EDI             // push EDI on stack
        MOV        EDI, EAX        // Point EDI to destination

        CMP        EDX, 32
        JL         @SmallLoop

        AND        EAX, 3          // get aligned count
        TEST       EAX, EAX        // check if X is not dividable by 4
        JNZ        @SmallLoop      // otherwise perform slow small loop

        MOV        EAX, EDI
        SHR        EAX, 2          // bytes to count
        AND        EAX, 3          // get aligned count
        ADD        EAX,-4
        NEG        EAX             // get count to advance
        JZ         @SetupMain
        SUB        EDX, EAX        // subtract aligning start from total count

@AligningLoop:
        MOV        [EDI], ECX
        ADD        EDI, 4
        DEC        EAX
        JNZ        @AligningLoop

@SetupMain:
        MOV        EAX, EDX        // EAX = remaining count
        SHR        EAX, 2
        SHL        EAX, 2
        SUB        EDX, EAX        // EDX = remaining count
        SHR        EAX, 2

        MOVD       XMM0, ECX
        PUNPCKLDQ  XMM0, XMM0
        PUNPCKLDQ  XMM0, XMM0
@SSE2Loop:
        MOVDQA     [EDI], XMM0
        ADD        EDI, 16
        DEC        EAX
        JNZ        @SSE2Loop

@SmallLoop:
        MOV        EAX,ECX
        MOV        ECX,EDX

        REP        STOSD           // Fill count dwords

@ExitPOP:
        POP        EDI

@Exit:
end;
{$ELSE}
asm
  // RCX = X;   RDX = Count;   R8 = Value
  // RDI = X;   ESI = Count;   EDX = Value

  //push RDX

  push RCX
  {$IFDEF LINUX}
    MOV R8, RDX
    MOV RDX,[ESI]
    MOV RCX, RDI
  {$ELSE}
  MOV R8, RDX
  MOV RDX,[ESI]
  MOV RCX, RDI
  {$ENDIF}
  TEST       RDX, RDX        // if Count = 0 then
  JZ         @Exit           //   Exit

  MOV        R9, RCX         // Point R9 to destination

  CMP        RDX, 32
  JL         @SmallLoop

  AND        RCX, 3          // get aligned count
  TEST       RCX, RCX        // check if X is not dividable by 4
  JNZ        @SmallLoop      // otherwise perform slow small loop

  MOV        RCX, R9
  SHR        RCX, 2          // bytes to count
  AND        RCX, 3          // get aligned count
  ADD        RCX,-4
  NEG        RCX             // get count to advance
  JZ         @SetupMain
  SUB        ESI, RCX        // subtract aligning start from total count

  @AligningLoop:
  MOV        [R9], R8D
  ADD        R9, 4
  DEC        RCX
  JNZ        @AligningLoop

  @SetupMain:
  MOV        RCX, RDX        // RCX = remaining count
  SHR        RCX, 2
  SHL        RCX, 2
  SUB        RDX, RCX        // RDX = remaining count
  SHR        RCX, 2

  MOVD       XMM0, R8D
  PUNPCKLDQ  XMM0, XMM0
  PUNPCKLDQ  XMM0, XMM0
  @SSE2Loop:
  MOVDQA     [R9], XMM0
  ADD        R9, 16
  DEC        RCX
  JNZ        @SSE2Loop

  TEST       RDX, RDX
  JZ         @Exit
  @SmallLoop:
  MOV        [R9], R8D
  ADD        R9, 4
  DEC        RDX
  JNZ        @SmallLoop
  @Exit:
  //pop RDX
  pop RCX
end;
{$ENDIF} *)
{$ENDIF}
{$ENDIF}

//{$IFDEF  USE_ASM_OPTIMIZATIONS}
//procedure FillWord(var X; Count: Cardinal; Value: Word);assembler; nostackframe;
//{$IFDEF CPU32}
//asm
//        // EAX = X;   EDX = Count;   ECX = Value
//        PUSH    EDI
//
//        MOV     EDI,EAX  // Point EDI to destination
//        MOV     EAX,ECX
//        MOV     ECX,EDX
//        TEST    ECX,ECX
//        JZ      @exit
//
//        REP     STOSW    // Fill count words
//@exit:
//        POP     EDI
//end;
//{$ENDIF}
//{$IFDEF CPU64}
//asm
//        // ECX = X;   EDX = Count;   R8D = Value
//        PUSH    RDI
//
//        MOV     RDI,RCX  // Point EDI to destination
//        MOV     EAX,R8D
//        MOV     ECX,EDX
//        TEST    ECX,ECX
//        JZ      @exit
//
//        REP     STOSW    // Fill count words
//@exit:
//        POP     RDI
//end;
//{$ENDIF}
//{$ELSE}
procedure FillWord(var X; Count: Cardinal; Value: LongWord);
var
  I: Integer;
  P: PWordArray;
begin
  P := PWordArray(@X);
  //Dec(Count);
  //While Count>=0 do
  //begin
  //  P^[Count] := Value;
  //  Dec(Count);
  //end;
  Dec(Count);
  for I := Count downto 0 do
    P^[I] := Value;


end;
//{$ENDIF}



{$IFDEF  USE_ASM_OPTIMIZATIONS}
procedure MoveLongword(const Source; var Dest; Count: Integer); assembler; nostackframe;
{$IFDEF CPU32}
asm
        // EAX = Source;   EDX = Dest;   ECX = Count
        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX
        MOV     EDI,EDX
        CMP     EDI,ESI
        JE      @exit

        REP     MOVSD
@exit:
        POP     EDI
        POP     ESI
end;
{$ENDIF}
{$IFDEF CPU64}
//asm
//        // RCX = Source;   RDX = Dest;   R8 = Count
//        PUSH    RSI
//        PUSH    RDI
//
//        MOV     RSI,RCX
//        MOV     RDI,RDX
//        MOV     RCX,R8
//        CMP     RDI,RSI
//        JE      @exit
//
//        REP     MOVSD
//@exit:
//        POP     RDI
//        POP     RSI
//end;
{$IFDEF WINDOWS}
asm

        PUSH    RSI
        PUSH    RDI
        {$IFDEF WINDOWS}
        // RCX = Source;   RDX = Dest;   R8 = Count
        MOV     RSI,RCX
        MOV     RDI,RDX
        MOV     RCX,R8
        SHR RCX,7 // Divide by 128
        {$ELSE}
        // RDI = Source;   RSI = Dest;   ECX = Count
        MOV     RCX,EDX
        SHR RCX,7 // Divide by 128
        {$ENDIF}

@loop_copy:

     //prefetchnta [ESI]128; //SSE2 prefetch
     //prefetchnta [ESI]160;
     //prefetchnta [ESI]192;
     //prefetchnta [ESI]224;

     movdqa xmm0, [RSI]; //move data from src to registers
     movdqa xmm1, [RSI]16;
     movdqa xmm2, [RSI]32;
     movdqa xmm3, [RSI]48;
     movdqa xmm4, [RSI]64;
     movdqa xmm5, [RSI]80;
     movdqa xmm6, [RSI]96;
     movdqa xmm7, [RSI]112;

     movntdq [RDI]0, xmm0; //move data from registers to dest
     movntdq [RDI]16, xmm1;
     movntdq [RDI]32, xmm2;
     movntdq [RDI]48, xmm3;
     movntdq [RDI]64, xmm4;
     movntdq [RDI]80, xmm5;
     movntdq [RDI]96, xmm6;
     movntdq [RDI]112, xmm7;

     add rsi, 128;
     add rdi, 128;
     dec rcx;

     jnz @loop_copy; //loop please
@exit:
        POP     RDI
        POP     RSI
end;
{$ELSE}
asm
  // RDI = Source;   RSI = Dest;   ECX = Count
  MOV  ECX,EDX
  SHR ECX,7 // Divide by 128
@loop_copy:

  //prefetchnta [ESI]128; //SSE2 prefetch
  //prefetchnta [ESI]160;
  //prefetchnta [ESI]192;
  //prefetchnta [ESI]224;

  movdqa xmm0, [RDI]; //move data from src to registers
  movdqa xmm1, [RDI]16;
  movdqa xmm2, [RDI]32;
  movdqa xmm3, [RDI]48;
  movdqa xmm4, [RDI]64;
  movdqa xmm5, [RDI]80;
  movdqa xmm6, [RDI]96;
  movdqa xmm7, [RDI]112;

  movntdq [RSI]0, xmm0; //move data from registers to dest
  movntdq [RSI]16, xmm1;
  movntdq [RSI]32, xmm2;
  movntdq [RSI]48, xmm3;
  movntdq [RSI]64, xmm4;
  movntdq [RSI]80, xmm5;
  movntdq [RSI]96, xmm6;
  movntdq [RSI]112, xmm7;

  add rsi, 128;
  add rdi, 128;
  dec rcx;

  jnz @loop_copy; //loop please
@exit:

end;
{$ENDIF}
{$ENDIF}
{$ELSE}
procedure MoveLongword(const Source; var Dest; Count: Integer);
begin
  Move(Source, Dest, Count shl 2);
end;
{$ENDIF}

{$IFDEF  USE_ASM_OPTIMIZATIONS}
procedure MoveWord(const Source; var Dest; Count: Integer); assembler; nostackframe;
{$IFDEF CPU32}
asm
        // EAX = X;   EDX = Count;   ECX = Value
        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX
        MOV     EDI,EDX
        MOV     EAX,ECX
        CMP     EDI,ESI
        JE      @exit

        REP     MOVSW
@exit:
        POP     EDI
        POP     ESI
end;
{$ENDIF}
{$IFDEF CPU64}
asm
        // ECX = X;   EDX = Count;   R8 = Value
        PUSH    RSI
        PUSH    RDI

        MOV     RSI,RCX
        MOV     RDI,RDX
        MOV     RAX,R8
        CMP     RDI,RSI
        JE      @exit

        REP     MOVSW
@exit:
        POP     RDI
        POP     RSI
end;
{$ENDIF}
{$ELSE}
procedure MoveWord(const Source; var Dest; Count: Integer);
begin
  Move(Source, Dest, Count shl 1);
end;
{$ENDIF}

{%endregion%}

procedure RegisterBZUtilsFunctions();
begin
  {$IFDEF LINUX}
    Proc_FillLongWord:=@nc_FillLongword;
  {$ENDIF}

  {$IFDEF WINDOWS}
    {$IFDEF USE_ASM_OPTIMIZATIONS}
      //if CPU_HasFeature(cfSSE) then
      //begin
      //  Proc_FillLongWord:=@asm_sse2_FillLongword;
      //end
      //else
      begin
        Proc_FillLongWord:= @asm_sse2_FillLongword; // @nc_FillLongword;
      end;
    {$ELSE}
       Proc_FillLongWord:=@nc_FillLongword;
    {$ENDIF}
  {$ENDIF}
end;

//==============================================================================

initialization

  RegisterBZUtilsFunctions();

finalization

//==============================================================================
end.

