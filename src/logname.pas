//
//Copyright Larry Tyree, N6TR, 2011,2012,2013,2014,2015.
//
//This file is part of TR log for linux.
//
//TR log for linux is free software: you can redistribute it and/or
//modify it under the terms of the GNU General Public License as
//published by the Free Software Foundation, either version 2 of the
//License, or (at your option) any later version.
//
//TR log for linux is distributed in the hope that it will be useful,
//but WITHOUT ANY WARRANTY; without even the implied warranty of
//MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//GNU General Public License for more details.
//
//You should have received a copy of the GNU General
//    Public License along with TR log for linux.  If not, see
//<http://www.gnu.org/licenses/>.
//

UNIT LogName;

{$O+}
{$V-}

INTERFACE

Uses SlowTree, Tree, trCrt, ZoneCont, LogSCP;

CONST CommonNameList: ARRAY [0..255] OF FourBytes = (
        ($00, $00, $01, $AD), ($00, $00, $02, $39), ($00, $00, $3D, $EF),
        ($00, $00, $3E, $FD), ($00, $00, $42, $6F), ($00, $00, $43, $D5),
        ($00, $00, $44, $B5), ($00, $00, $4C, $8D), ($00, $00, $4E, $93),
        ($00, $00, $5D, $2C), ($00, $00, $5D, $2D), ($00, $00, $5F, $81),
        ($00, $00, $5F, $87), ($00, $00, $61, $EF), ($00, $00, $67, $4A),
        ($00, $00, $6C, $A3), ($00, $00, $6C, $AE), ($00, $00, $6D, $CA),
        ($00, $00, $6E, $A0), ($00, $00, $6E, $A9), ($00, $00, $6F, $87),
        ($00, $00, $71, $F7), ($00, $00, $71, $FC), ($00, $00, $72, $02),
        ($00, $00, $72, $07), ($00, $00, $72, $08), ($00, $00, $72, $8B),
        ($00, $00, $72, $90), ($00, $00, $73, $23), ($00, $00, $73, $FC),
        ($00, $00, $74, $09), ($00, $00, $77, $E0), ($00, $00, $77, $E9),
        ($00, $00, $77, $EA), ($00, $00, $77, $EE), ($00, $00, $79, $62),
        ($00, $00, $7C, $A3), ($00, $00, $7C, $B3), ($00, $00, $7C, $B4),
        ($00, $00, $7C, $B8), ($00, $00, $7D, $CB), ($00, $00, $82, $08),
        ($00, $00, $84, $01), ($00, $00, $8C, $BF), ($00, $00, $8D, $51),
        ($00, $00, $97, $76), ($00, $00, $99, $65), ($00, $00, $99, $67),
        ($00, $00, $99, $71), ($00, $00, $99, $7C), ($00, $00, $9A, $E4),
        ($00, $00, $9C, $C3), ($00, $00, $9D, $53), ($00, $00, $9D, $C8),
        ($00, $00, $9D, $CE), ($00, $00, $A2, $12), ($00, $00, $A2, $13),
        ($00, $00, $A2, $1A), ($00, $00, $A2, $A7), ($00, $00, $A3, $44),
        ($00, $00, $A4, $22), ($00, $00, $AA, $9B), ($00, $00, $AC, $CD),
        ($00, $00, $AD, $EC), ($00, $00, $AD, $FD), ($00, $00, $BC, $DF),
        ($00, $00, $BE, $DA), ($00, $00, $BF, $C5), ($00, $00, $C2, $C7),
        ($00, $0B, $77, $55), ($00, $0B, $77, $F3), ($00, $0B, $82, $81),
        ($00, $0B, $A4, $BE), ($00, $0C, $54, $61), ($00, $0C, $68, $DF),
        ($00, $0D, $3E, $F5), ($00, $0D, $61, $F6), ($00, $0E, $3E, $10),
        ($00, $0E, $3F, $82), ($00, $0E, $51, $E6), ($00, $0E, $67, $91),
        ($00, $0E, $68, $F9), ($00, $0E, $8A, $3D), ($00, $0F, $4D, $07),
        ($00, $0F, $98, $88), ($00, $10, $97, $F5), ($00, $10, $A8, $BE),
        ($00, $10, $A9, $2D), ($00, $11, $3F, $02), ($00, $11, $53, $BA),
        ($00, $11, $53, $BE), ($00, $11, $54, $51), ($00, $11, $97, $F8),
        ($00, $12, $3E, $60), ($00, $12, $3E, $68), ($00, $12, $3E, $FE),
        ($00, $12, $67, $B0), ($00, $12, $69, $C0), ($00, $12, $69, $D7),
        ($00, $12, $A8, $4E), ($00, $13, $48, $32), ($00, $13, $5E, $A2),
        ($00, $13, $9C, $C5), ($00, $13, $AC, $CF), ($00, $14, $3C, $C9),
        ($00, $14, $51, $E6), ($00, $14, $52, $97), ($00, $14, $69, $BA),
        ($00, $14, $88, $63), ($00, $14, $89, $F1), ($00, $14, $A7, $76),
        ($00, $15, $3E, $F5), ($00, $15, $40, $26), ($00, $15, $6A, $C3),
        ($00, $15, $88, $A8), ($00, $15, $A9, $52), ($00, $15, $A9, $F1),
        ($00, $16, $3C, $C7), ($00, $16, $3E, $FC), ($00, $17, $3D, $F5),
        ($00, $17, $3E, $F4), ($00, $17, $3F, $0F), ($00, $17, $3F, $47),
        ($00, $17, $68, $B3), ($00, $17, $68, $B7), ($00, $17, $6A, $10),
        ($00, $17, $89, $D0), ($00, $17, $8A, $20), ($00, $18, $3D, $E7),
        ($00, $18, $67, $91), ($00, $18, $87, $8C), ($00, $18, $89, $D0),
        ($00, $18, $89, $D4), ($00, $19, $77, $E2), ($00, $19, $A4, $DD),
        ($00, $1A, $3F, $64), ($00, $1A, $54, $9C), ($00, $1A, $54, $A9),
        ($00, $1A, $63, $17), ($00, $1C, $53, $BE), ($00, $1C, $67, $8E),
        ($00, $1C, $67, $91), ($00, $1C, $88, $EF), ($00, $1C, $A7, $E0),


        ($00, $1C, $A7, $F0), ($00, $1C, $AA, $15), ($00, $1D, $63, $19),
        ($00, $1D, $68, $1F), ($00, $1D, $A2, $1D), ($00, $1E, $3C, $C9),
        ($00, $1E, $3C, $E4), ($00, $1E, $3D, $E7), ($00, $1E, $3D, $EB),
        ($00, $1E, $3D, $FB), ($00, $1E, $54, $62), ($00, $1E, $89, $02),
        ($00, $1E, $89, $17), ($00, $1E, $89, $1D), ($00, $1E, $89, $4C),
        ($00, $1E, $89, $DC), ($00, $1E, $8A, $D9), ($00, $20, $77, $4B),
        ($00, $21, $3E, $1F), ($00, $21, $88, $EF), ($00, $23, $3E, $31),
        ($00, $23, $3F, $23), ($00, $23, $89, $F5), ($00, $23, $A8, $BE),
        ($00, $23, $A8, $E3), ($00, $23, $A9, $E6), ($00, $24, $88, $F2),
        ($01, $AC, $69, $B2), ($01, $AD, $3D, $AA), ($01, $AF, $4E, $F9),
        ($01, $C7, $99, $EB), ($01, $CB, $99, $42), ($01, $D5, $98, $98),
        ($01, $D8, $67, $4A), ($01, $D8, $A7, $B7), ($01, $D8, $A9, $58),
        ($01, $F3, $98, $98), ($01, $F3, $A7, $BD), ($01, $F7, $67, $FB),
        ($01, $FA, $78, $7D), ($01, $FD, $3D, $A3), ($02, $11, $AD, $ED),
        ($02, $47, $67, $8E), ($02, $47, $83, $26), ($02, $4A, $5D, $2C),
        ($02, $6C, $3E, $60), ($02, $6C, $3E, $6F), ($02, $6C, $6A, $15),
        ($02, $84, $99, $EB), ($02, $A5, $99, $EB), ($02, $A9, $69, $37),
        ($02, $A9, $84, $77), ($02, $A9, $84, $87), ($02, $F3, $99, $EB),
        ($02, $FD, $9D, $50), ($03, $03, $73, $61), ($03, $14, $97, $FD),
        ($03, $14, $A4, $BE), ($03, $14, $C5, $18), ($03, $18, $6A, $03),
        ($03, $18, $83, $4F), ($03, $18, $AD, $F7), ($03, $1F, $3F, $6B),
        ($03, $39, $99, $EB), ($03, $44, $8A, $CE), ($03, $47, $A8, $A3),
        ($03, $5E, $98, $94), ($03, $66, $48, $32), ($03, $66, $A4, $BE),
        ($03, $CD, $AD, $61), ($03, $D1, $A2, $B5), ($04, $17, $79, $7A),
        ($04, $17, $82, $81), ($04, $25, $5D, $30), ($04, $3E, $8A, $25),
        ($04, $40, $98, $40), ($04, $43, $68, $1F), ($04, $47, $3F, $7E),
        ($04, $4F, $54, $E6), ($04, $61, $A4, $BE), ($04, $65, $99, $EB),
        ($04, $65, $A4, $BE), ($04, $6F, $9D, $C2), ($04, $AB, $9D, $EA),
        ($04, $D0, $BE, $B2), ($04, $D8, $78, $F7), ($05, $28, $9D, $C2),
        ($47, $38, $79, $60), ($4D, $21, $83, $34), ($4D, $AC, $A2, $B5),
        ($54, $C3, $53, $BE), ($5D, $2D, $98, $40), ($5D, $30, $3E, $ED),
        ($5E, $A2, $4E, $93), ($5F, $7C, $A2, $B5), ($64, $00, $3E, $ED),
        ($77, $EA, $83, $25), ($7C, $B2, $A3, $45), ($7D, $CB, $62, $83),
        ($8D, $D8, $99, $D7), ($99, $65, $54, $61), ($99, $6F, $3E, $59),
        ($9D, $5C, $5D, $28), ($9D, $5C, $5D, $37), ($9F, $7A, $57, $3F),
        ($AC, $CD, $54, $66), ($AC, $D4, $68, $EC), ($AD, $EC, $A4, $27),
        ($AD, $FD, $3E, $24), ($B2, $26, $A2, $B5), ($B2, $2C, $97, $FF),
        ($B2, $C0, $82, $9F));

    NumberArrays = 2;

    MaximumCommonNames        = 16400;
    MaximumArrayCommonNames = MaximumCommonNames DIV NumberArrays;

    MaximumUncommonNames      =  7200;
    MaximumArrayUncommonNames = MaximumUncommonNames DIV NumberArrays;

    MaxClubCalls = 500;



TYPE
    CommonCallArray  = ARRAY [0..NumberArrays - 1, 0..MaximumArrayCommonNames] OF TwoBytes;
    CommonNameArray  = ARRAY [0..NumberArrays - 1, 0..MaximumArrayCommonNames] OF BYTE;

    CommonCallArrayPtr = ^CommonCallArray;
    CommonNameArrayPtr = ^CommonNameArray;
    CommonTotalArray   = ARRAY [0..NumberArrays - 1] OF INTEGER;

    UncommonCallArray  = ARRAY [0..NumberArrays - 1, 0..MaximumArrayUncommonNames] OF TwoBytes;
    UncommonNameArray  = ARRAY [0..NumberArrays - 1, 0..MaximumArrayUncommonNames] OF FourBytes;

    UncommonCallArrayPtr = ^UncommonCallArray;
    UncommonNameArrayPtr = ^UncommonNameArray;
    UncommonTotalArray   = ARRAY [0..NumberArrays - 1] OF INTEGER;

    ClubCallArray = ARRAY [0..MaxClubCalls] OF FourBytes;
    ClubCallArrayPtr = ^ClubCallArray;

    FourBytePtr = ^FourBytes;

    PossibleCallEntry = RECORD
        Call: CallString;
        Dupe: BOOLEAN;
        END;

    TotalArrayRecord = RECORD
        Name: FourBytes;
        Hits: INTEGER;
        END;



    NameDictionary = OBJECT
        CMQFileName:       Str80;
        CMQBackUpFileName: Str80;

        NameMemoryDisable: BOOLEAN;

        CommonCalls:    CommonCallArrayPtr;
        CommonNames:    CommonNameArrayPtr;
        CommonTotals:   CommonTotalArray;

        UncommonCalls:  UncommonCallArrayPtr;
        UncommonNames:  UncommonNameArrayPtr;
        UncommonTotals: UncommonTotalArray;

        ClubCalls: ClubCallArrayPtr;
        NumberClubCalls: INTEGER;

        PROCEDURE Abort;
        PROCEDURE AddCallToPossibleList (Call: CallString; VAR lList: CallListRecord);
        FUNCTION  AddClubCall (Call: CallString): BOOLEAN;
        FUNCTION  AddName (Call: CallString; Name: Str80): BOOLEAN;
        PROCEDURE ComputeTotalCalls;
        PROCEDURE DeleteName (Callsign: CallString);
        PROCEDURE DumpNames;
        FUNCTION  FindCommonCallAddress (Call: FourBytes; NameCode: INTEGER): INTEGER;
        FUNCTION  FindUncommonCallAddress (AddCall: FourBytes; NameCode: INTEGER): INTEGER;
        FUNCTION  GetName (Call: CallString): Str80;
        FUNCTION  GetRandomCall: CallString;
        PROCEDURE Init;
        PROCEDURE InputASCIIFile;
        FUNCTION  Load: BOOLEAN;
        PROCEDURE MakeASCIIList (FileName: Str80);
        PROCEDURE MakePossibleCallList (Call: CallString; VAR PossCallList: CallListRecord);
        FUNCTION  ReLoad: BOOLEAN;
        PROCEDURE Save;
        FUNCTION  TotalNumberOfCalls: INTEGER;
        PROCEDURE UpdateNameFile;
        END;

VAR Names: NameDictionary;
    ArraysSetUp: BOOLEAN;

IMPLEMENTATION

uses memlinux,keycode;

VAR LargestCommonArray: INTEGER;
    LargestUncommonArray: INTEGER;
    NamesCleared: BOOLEAN;
    TotalCalls: INTEGER;



FUNCTION GetNameCode (Call: CallString): INTEGER;

VAR TempBytes: FourBytes;
    Total: INTEGER;

    BEGIN
    CompressFormat (Call, TempBytes);

    Total := 0;

    Total := TempBytes [1] + TempBytes [2];

    TempBytes [1] := TempBytes [1] DIV 16;
    TempBytes [2] := TempBytes [2] DIV 16;

    Total := (Total + TempBytes [1] + TempBytes [2]) AND (NumberArrays - 1);

    GetNameCode := Total;
    END;



FUNCTION CommonName (Name: CallString; VAR NameAddress: BYTE): BOOLEAN;

VAR CompName: FourBytes;
    Address, StartAddress, StopAddress: INTEGER;

    BEGIN
    IF (Length (Name) < 2) OR (Length (Name) > 6) THEN
        BEGIN
        CommonName := False;
        Exit;
        END;

    CompressFormat (Name, CompName);

    IF CompName [1] = 0 THEN
        BEGIN
        IF CompName [2] = 0 THEN
            BEGIN
            IF CompName [3] < $80 THEN
                BEGIN
                StartAddress := 0;
                StopAddress := 40;
                END
            ELSE
                BEGIN
                StartAddress := 41;
                StopAddress := 68;
                END;
            END
        ELSE
            BEGIN
            IF CompName [2] < $18 THEN
                BEGIN
                StartAddress := 69;
                StopAddress := 127;
                END
            ELSE
                BEGIN
                StartAddress := 128;
                StopAddress := 170;
                END;
            END;
        END
    ELSE
        BEGIN
        IF CompName [1] < 4 THEN
            BEGIN
            StartAddress := 171;
            StopAddress := 214;
            END
        ELSE
            BEGIN
            StartAddress := 215;
            StopAddress := 255;
            END;
        END;

    FOR Address := StartAddress TO StopAddress DO
        IF (CommonNameList [Address] [4] = CompName [4]) AND
           (CommonNameList [Address] [3] = CompName [3]) AND
           (CommonNameList [Address] [2] = CompName [2]) AND
           (CommonNameList [Address] [1] = CompName [1]) THEN
               BEGIN
               NameAddress := Address;
               CommonName := True;
               Exit;
               END;

    CommonName := False;
    END;



FUNCTION StandardNameCallFormat (Call: CallString): CallString;

VAR TempCall: CallString;

    BEGIN
    TempCall := UpperCase (StandardCallFormat (Call, True));

    IF StringHas (TempCall, '/') THEN
        TempCall := PostcedingString (TempCall, '/');

    IF StringHas (TempCall, '/') THEN
        TempCall := PrecedingString (TempCall, '/');

    IF Length (TempCall) > 6 THEN TempCall [0] := Chr (6);
    StandardNameCallFormat := TempCall;
    END;


PROCEDURE NameDictionary.Abort;

    BEGIN
    IF CommonCalls   <> Nil THEN Dispose (CommonCalls);
    IF CommonNames   <> Nil THEN Dispose (CommonNames);
    IF UnCommonCalls <> Nil THEN Dispose (UncommonCalls);
    IF UnCommonNames <> Nil THEN Dispose (UncommonNames);
    IF ClubCalls     <> Nil THEN Dispose (ClubCalls);

    CommonCalls := Nil;
    CommonNames := Nil;
    UnCommonCalls := Nil;
    UnCommonNames := Nil;
    ClubCalls := Nil;
    END;


FUNCTION NameDictionary.AddClubCall (Call: CallString): BOOLEAN;

VAR TempBytes: FourBytes;
    CallAddress: INTEGER;

    BEGIN
    AddClubCall := False;
    CompressFormat (Call, TempBytes);

    IF NumberClubCalls = 0 THEN
        BEGIN
        ClubCalls^ [NumberClubCalls] := TempBytes;
        Inc (NumberClubCalls);
        Inc (TotalCalls);
        Exit;
        END;

    IF NumberClubCalls < MaxClubCalls THEN
        BEGIN
        FOR CallAddress := 0 TO NumberClubCalls - 1 DO
            IF (ClubCalls^ [CallAddress] [4] = TempBytes [4]) AND
               (ClubCalls^ [CallAddress] [3] = TempBytes [3]) AND
               (ClubCalls^ [CallAddress] [2] = TempBytes [2]) AND
               (ClubCalls^ [CallAddress] [1] = TempBytes [1]) THEN Exit;

        ClubCalls^ [NumberClubCalls] := TempBytes;
        Inc (NumberClubCalls);
        Inc (TotalCalls);
        AddClubCall := True;
        END;
    END;



FUNCTION NameDictionary.AddName (Call: CallString; Name: Str80): BOOLEAN;

VAR NameAddress: BYTE;
    Address, NameCode: INTEGER;
    CompCall, CompName: FourBytes;
    PrefixFlag: BOOLEAN;
    Prefix, Suffix: TwoBytes;
    aa: integer;

    BEGIN
    AddName := False;
    Call := StandardNameCallFormat (Call);

    IF (ClubCall (Call)) OR (Name = 'CLUB') THEN
        BEGIN
        AddName := AddClubCall (Call);
        Exit;
        END;

    NameCode := GetNameCode (Call);

    CompressFormat (Call, CompCall);
    CompressFormat (Name, CompName);

    Prefix [1] := CompCall [1];
    Prefix [2] := CompCall [2];
    Suffix [1] := CompCall [3];
    Suffix [2] := CompCall [4];

    IF CommonName (Name, NameAddress) THEN
        BEGIN
        IF CommonTotals [NameCode] = 0 THEN
            BEGIN
            CommonCalls^ [NameCode, 0] [1] := $FF;
            CommonCalls^ [NameCode, 0] [2] := $FF;
            CommonNames^ [NameCode, 0]     := $F;

            CommonCalls^ [NameCode, 1] := Prefix;

            CommonCalls^ [NameCode, 2] := Suffix;
            CommonNames^ [NameCode, 2] := NameAddress;

            CommonTotals [NameCode] := 3;

            AddName := True;
            Inc (TotalCalls);
            Exit;
            END;



        IF CommonTotals [NameCode] < (MaximumArrayCommonNames - 3) THEN
            BEGIN
            PrefixFlag := False;

            aa := -1;
            FOR Address := 0 TO CommonTotals [NameCode] - 1 DO
                BEGIN
                aa := aa + 1;
                if address < aa then continue;
                IF PrefixFlag THEN
                    BEGIN
                    IF (CommonCalls^ [NameCode, Address] [1] = Prefix [1]) AND
                       (CommonCalls^ [Namecode, Address] [2] = Prefix [2]) THEN
                           BEGIN
                           Inc (aa);

                           Move (CommonCalls^ [NameCode, aa],
                                 CommonCalls^ [Namecode, aa + 1],
                                 (CommonTotals [NameCode] - aa) * 2);

                           Move (CommonNames^ [NameCode, aa],
                                 CommonNames^ [Namecode, aa + 1],
                                 CommonTotals [NameCode] - aa);


                           CommonCalls^ [NameCode, aa] := Suffix;
                           CommonNames^ [NameCode, aa] := NameAddress;

                           Inc (CommonTotals [NameCode]);
                           Inc (TotalCalls);
                           AddName := True;
                           Exit;
                           END;

                    PrefixFlag := False;
                    END;

                IF (CommonCalls^ [NameCode, aa] [2] = $FF) AND
                   (CommonCalls^ [NameCode, aa] [1] = $FF) AND
                   (CommonNames^ [NameCode, aa] = $F) THEN
                       PrefixFlag := True;

                END;

      { Prefix not found, so add it at end of list.  First do prefix code }

            CommonCalls^ [NameCode, CommonTotals [NameCode]] [1] := $FF;
            CommonCalls^ [NameCode, CommonTotals [NameCode]] [2] := $FF;
            CommonNames^ [NameCode, CommonTotals [NameCode]] := $F;
            Inc (CommonTotals [NameCode]);

            CommonCalls^ [NameCode, CommonTotals [NameCode]] := Prefix;
            Inc (CommonTotals [NameCode]);

            CommonCalls^ [NameCode, CommonTotals [NameCode]] := Suffix;
            CommonNames^ [NameCode, CommonTotals [NameCode]] := NameAddress;
            Inc (CommonTotals [NameCode]);

            Inc (TotalCalls);
            AddName := True;
            END;
        Exit;  { No room to add a call }
        END


    ELSE
        BEGIN
        IF UnCommonTotals [NameCode] = 0 THEN
            BEGIN
            UnCommonCalls^ [NameCode, 0] [1] := $FF;
            UnCommonCalls^ [NameCode, 0] [2] := $FF;
            UnCommonNames^ [NameCode, 0] [1] := $FF;
            UnCommonNames^ [NameCode, 0] [2] := $FF;

            UnCommonCalls^ [NameCode, 1] := Prefix;

            UnCommonCalls^ [NameCode, 2] := Suffix;
            UnCommonNames^ [NameCode, 2] := CompName;

            UnCommonTotals [NameCode] := 3;
            Inc (TotalCalls);
            AddName := True;
            Exit;
            END;

        IF UnCommonTotals [NameCode] < (MaximumArrayUnCommonNames - 3) THEN
            BEGIN
            PrefixFlag := False;

            aa := -1;
            FOR Address := 0 TO UnCommonTotals [NameCode] - 1 DO
                BEGIN
                aa := aa + 1;
                if address < aa then continue;
                IF PrefixFlag THEN
                    BEGIN
                    IF (UnCommonCalls^ [NameCode, Address] [1] = Prefix [1]) AND
                       (UnCommonCalls^ [Namecode, Address] [2] = Prefix [2]) THEN
                           BEGIN
                           Inc (aa);

                           Move (UnCommonCalls^ [NameCode, aa],
                                 UnCommonCalls^ [Namecode, aa + 1],
                                (UnCommonTotals [NameCode] - aa) * 2);

                           Move (UnCommonNames^ [NameCode, aa],
                                 UnCommonNames^ [Namecode, aa + 1],
                                (UnCommonTotals [NameCode] - aa) * 4);


                           UnCommonCalls^ [NameCode, aa] := Suffix;
                           UnCommonNames^ [NameCode, aa] := CompName;

                           Inc (UnCommonTotals [NameCode]);
                           Inc (TotalCalls);
                           AddName := True;
                           Exit;
                           END;

                    PrefixFlag := False;
                    END;

                IF (UnCommonCalls^ [NameCode, aa] [2] = $FF) AND
                   (UnCommonCalls^ [NameCode, aa] [1] = $FF) AND
                   (UnCommonNames^ [NameCode, aa] [1] = $FF) AND
                   (UnCommonNames^ [NameCode, aa] [2] = $FF) THEN
                       PrefixFlag := True;

                END;



      { Prefix not found, so add it at end of list.  First do prefix code }

            UnCommonCalls^ [NameCode, UnCommonTotals [NameCode]] [1] := $FF;
            UnCommonCalls^ [NameCode, UnCommonTotals [NameCode]] [2] := $FF;
            UnCommonNames^ [NameCode, UnCommonTotals [NameCode]] [1] := $FF;
            UnCommonNames^ [NameCode, UnCommonTotals [NameCode]] [2] := $FF;
            Inc (UnCommonTotals [NameCode]);

      { Now add the new prefix }

            UnCommonCalls^ [NameCode, UnCommonTotals [NameCode]] := Prefix;
            Inc (UnCommonTotals [NameCode]);

      { Put the suffix and name on }

            UnCommonCalls^ [NameCode, UnCommonTotals [NameCode]] := Suffix;
            UnCommonNames^ [NameCode, UnCommonTotals [NameCode]] := CompName;
            Inc (UnCommonTotals [NameCode]);

            Inc (TotalCalls);
            AddName := True;
            END;

        Exit;  { No room to add a call }
        END;
    END;



PROCEDURE NameDictionary.DeleteName (Callsign: CallString);

VAR Address, NamePointer, Offset, NameCode: INTEGER;
    TempBytes: FourBytes;
    Call: CallString;

    BEGIN
    Call := StandardNameCallFormat (Callsign);
    NameCode := GetNameCode (Call);

    CompressFormat (Call, TempBytes);

    Address := FindCommonCallAddress (TempBytes, NameCode);

    IF Address <> -1 THEN
        BEGIN
        IF (CommonCalls^ [NameCode, Address - 2] [1] = $FF) AND
           (CommonCalls^ [NameCode, Address - 2] [2] = $FF) AND
           (CommonCalls^ [NameCode, Address + 1] [1] = $FF) AND
           (CommonCalls^ [NameCode, Address + 1] [2] = $FF) THEN
               BEGIN
               Move (CommonCalls^ [NameCode, Address + 1],
                     CommonCalls^ [NameCode, Address - 2],
                     (CommonTotals [NameCode] - Address - 1) * 2);

               Move (CommonNames^ [NameCode, Address + 1],
                     CommonNames^ [NameCode, Address - 2],
                     CommonTotals [NameCode] - Address - 1);

               Dec (CommonTotals [NameCode]);
               Dec (CommonTotals [NameCode]);
               Dec (CommonTotals [NameCode]);
               Dec (TotalCalls);
               END
           ELSE
               BEGIN
               Move (CommonCalls^ [NameCode, Address + 1],
                     CommonCalls^ [NameCode, Address],
                     (CommonTotals [NameCode] - Address - 1) * 2);

               Move (CommonNames^ [NameCode, Address + 1],
                     CommonNames^ [NameCode, Address],
                     CommonTotals [NameCode] - Address - 1);

               Dec (CommonTotals [NameCode]);
               Dec (TotalCalls);
               END;

        Exit;
        END;



    Address := FindUnCommonCallAddress (TempBytes, NameCode);

    IF Address <> -1 THEN
        BEGIN
        IF (UnCommonCalls^ [NameCode, Address - 2] [1] = $FF) AND
           (UnCommonCalls^ [NameCode, Address - 2] [2] = $FF) AND
           (UnCommonCalls^ [NameCode, Address + 1] [1] = $FF) AND
           (UnCommonCalls^ [NameCode, Address + 1] [2] = $FF) THEN
               BEGIN
               Move (UnCommonCalls^ [NameCode, Address + 1],
                     UnCommonCalls^ [NameCode, Address - 2],
                     (UnCommonTotals [NameCode] - Address - 1) * 2);

               Move (UnCommonNames^ [NameCode, Address + 1],
                     UnCommonNames^ [NameCode, Address - 2],
                     (UnCommonTotals [NameCode] - Address - 1) * 4);

               Dec (UnCommonTotals [NameCode]);
               Dec (UnCommonTotals [NameCode]);
               Dec (UnCommonTotals [NameCode]);
               Dec (TotalCalls);
               END
           ELSE
               BEGIN
               Move (UnCommonCalls^ [NameCode, Address + 1],
                     UnCommonCalls^ [NameCode, Address],
                     (UnCommonTotals [NameCode] - Address - 1) * 2);

               Move (UnCommonNames^ [NameCode, Address + 1],
                     UnCommonNames^ [NameCode, Address],
                    (UnCommonTotals [NameCode] - Address - 1) * 4);

               Dec (UnCommonTotals [NameCode]);
               Dec (TotalCalls);
               END;

        Exit;
        END;


    IF NumberClubCalls > 0 THEN
        FOR NamePointer := 0 TO NumberClubCalls - 1 DO
            IF (ClubCalls^ [NamePointer] [4] = TempBytes [4]) AND
               (ClubCalls^ [NamePointer] [3] = TempBytes [3]) AND
               (ClubCalls^ [NamePointer] [2] = TempBytes [2]) AND
               (ClubCalls^ [NamePointer] [1] = TempBytes [1]) THEN
                   BEGIN
                   IF NamePointer < NumberClubCalls - 1 THEN
                       FOR Offset := NamePointer TO NumberClubCalls - 2 DO
                           ClubCalls^ [Offset] := ClubCalls^ [Offset + 1];
                   Dec (NumberClubCalls);
                   Dec (TotalCalls);
                   Exit;
                   END;

    WriteLn ('Sorry, I could not find ', Callsign, '.');
    END;



PROCEDURE NameDictionary.DumpNames;

    BEGIN
    IF (NOT NamesCleared) AND ArraysSetUp THEN
        BEGIN
        NamesCleared := True;
        Dispose (CommonNames);
        Dispose (UncommonNames);
        NameMemoryDisable := True;
        ArraysSetUp := False;
        END;
    END;



FUNCTION NameDictionary.FindCommonCallAddress (Call: FourBytes; NameCode: INTEGER): INTEGER;

VAR Address: INTEGER;
    MatchPrefixFlag, PrefixFlag: BOOLEAN;

    BEGIN
    FindCommonCallAddress := -1;

    MatchPrefixFlag := False;
    PrefixFlag := False;

    IF CommonTotals [NameCode] > 0 THEN
        FOR Address := 0 TO CommonTotals [NameCode] - 1 DO
            BEGIN
            IF MatchPrefixFlag THEN
                BEGIN
                IF (CommonCalls^ [NameCode, Address] [2] = Call [4]) AND
                   (CommonCalls^ [NameCode, Address] [1] = Call [3]) THEN
                       BEGIN
                       FindCommonCallAddress := Address;
                       Exit;
                       END;

                IF (CommonCalls^ [NameCode, Address] [2] = $FF) AND
                   (CommonCalls^ [NameCode, Address] [1] = $FF) AND
                   (CommonNames^ [NameCode, Address] = $F) THEN
                       BEGIN
                       MatchPrefixFlag := False;
                       PrefixFlag := True;
                       END;

                Continue;
                END;

            IF PrefixFlag THEN
                BEGIN
                MatchPrefixFlag := (CommonCalls^ [NameCode, Address] [1] = Call [1]) AND
                                   (CommonCalls^ [NameCode, Address] [2] = Call [2]);
                PrefixFlag := False;
                Continue;
                END;

            IF (CommonCalls^ [NameCode, Address] [1] = $FF) AND
               (CommonCalls^ [NameCode, Address] [2] = $FF) AND
               (CommonNames^ [NameCode, Address] = $F) THEN
                   BEGIN
                   MatchPrefixFlag := False;
                   PrefixFlag := True;
                   Continue;
                   END;
            END;
    END;



FUNCTION NameDictionary.FindUncommonCallAddress (AddCall: FourBytes; NameCode: INTEGER): INTEGER;

VAR Address: INTEGER;
    MatchPrefixFlag, PrefixFlag: BOOLEAN;
    Prefix, Suffix: TwoBytes;

    BEGIN
    FindUnCommonCallAddress := -1;

    Prefix [1] := AddCall [1];
    Prefix [2] := AddCall [2];
    Suffix [1] := AddCall [3];
    Suffix [2] := AddCall [4];

    MatchPrefixFlag := False;
    PrefixFlag      := False;

    IF UnCommonTotals [NameCode] > 0 THEN
        FOR Address := 0 TO UnCommonTotals [NameCode] - 1 DO
            BEGIN
            IF MatchPrefixFlag THEN
                BEGIN
                IF (UnCommonCalls^ [NameCode, Address] [1] = Suffix [1]) AND
                   (UnCommonCalls^ [NameCode, Address] [2] = Suffix [2]) THEN
                       BEGIN
                       FindUnCommonCallAddress := Address;
                       Exit;
                       END;

                IF (UnCommonCalls^ [NameCode, Address] [1] = $FF) AND
                   (UnCommonCalls^ [NameCode, Address] [2] = $FF) AND
                   (UnCommonNames^ [NameCode, Address] [1] = $FF) AND
                   (UnCommonNames^ [NameCode, Address] [2] = $FF) THEN
                       BEGIN
                       MatchPrefixFlag := False;
                       PrefixFlag := True;
                       END;

                Continue;
                END;

            IF PrefixFlag THEN
                BEGIN
                MatchPrefixFlag := (UnCommonCalls^ [NameCode, Address] [1] = Prefix [1]) AND
                                   (UnCommonCalls^ [NameCode, Address] [2] = Prefix [2]);
                PrefixFlag := False;
                Continue;
                END;

            IF (UnCommonCalls^ [NameCode, Address] [1] = $FF) AND
               (UnCommonCalls^ [NameCode, Address] [2] = $FF) AND
               (UnCommonNames^ [NameCode, Address] [2] = $FF) AND
               (UnCommonNames^ [NameCode, Address] [1] = $FF) THEN
                   BEGIN
                   MatchPrefixFlag := False;
                   PrefixFlag := True;
                   Continue;
                   END;
            END;
    END;



FUNCTION NameDictionary.GetName (Call: CallString): Str80;

{ This procedure will return the name for the callsign passed to it.  If it
  does not know the name, it will return a null string.  The callsign will
  be converted to the root call eliminating any portable information.     }

VAR Address, NameCode, NamePointer: INTEGER;
    TempBytes: FourBytes;

    BEGIN
    GetName  := '';
    IF NamesCleared THEN Exit;

    Call := StandardNameCallFormat (Call);

    NameCode := GetNameCode (Call);
    CompressFormat (Call, TempBytes);

    Address := FindCommonCallAddress (TempBytes, NameCode);

    IF Address <> -1 THEN
        BEGIN
        GetName := ExpandedString (CommonNameList [CommonNames^ [NameCode, Address]]);
        Exit;
        END;

    Address := FindUncommonCallAddress (TempBytes, NameCode);

    IF Address <> - 1 THEN
        BEGIN
        GetName := ExpandedString (UnCommonNames^ [NameCode, Address]);
        Exit;
        END;

    IF NumberClubCalls > 0 THEN
        FOR NamePointer := 0 TO NumberClubCalls - 1 DO
            IF (ClubCalls^ [NamePointer] [4] = TempBytes [4]) AND
               (ClubCalls^ [NamePointer] [3] = TempBytes [3]) AND
               (ClubCalls^ [NamePointer] [2] = TempBytes [2]) AND
               (ClubCalls^ [NamePointer] [1] = TempBytes [1]) THEN
                   GetName := 'CLUB';
    END;



FUNCTION NameDictionary.GetRandomCall: CallString;

VAR NameCode, NamePointer: INTEGER;
    Prefix, Suffix: TwoBytes;
    Call: CallString;

    BEGIN
    IF TotalCalls < 100 THEN
        BEGIN
        CASE Random (20) OF
            0: GetRandomCall := 'G3FXB';
            1: GetRandomCall := 'N6TR';
            2: GetRandomCall := 'ZD8Z';
            3: GetRandomCall := 'F6IMS';
            4: GetRandomCall := '9Y4XX';
            5: GetRandomCall := 'EA9IE';
            6: GetRandomCall := 'FR5DX';
            7: GetRandomCall := 'VK6HD';
            8: GetRandomCall := 'JA8RWU';
            9: GetRandomCall := 'K6NA/KL7';
           10: GetRandomCall := 'N6AA';
           11: GetRandomCall := 'NJ2L';
           12: GetRandomCall := 'WN4KKN';
           13: GetRandomCall := 'N5AU';
           14: GetRandomCall := 'K1AR';
           15: GetRandomCall := 'W7RM';
           16: GetRandomCall := '4U1ITU';
           17: GetRandomCall := 'N6ZZ';
           18: GetRandomCall := 'WA6OTU';
           19: GetRandomCall := 'AA6RX';
           END;
        Exit;
        END;

    NameCode := Random (NumberArrays);



    IF Random (100) < 72 THEN    { Use common name array 72 percent of the time }
        BEGIN
        IF CommonTotals [NameCode] > 0 THEN
            BEGIN
            NamePointer := Random (CommonTotals [NameCode] - 1);

            IF (CommonCalls^ [NameCode, NamePointer] [1] = $FF) AND
               (CommonCalls^ [NameCode, NamePointer] [2] = $FF) AND
               (CommonNames^ [NameCode, NamePointer] = $F) THEN
                   BEGIN
                   Inc (NamePointer);
                   Prefix := CommonCalls^ [NameCode, NamePointer];
                   Inc (NamePointer);
                   Suffix := CommonCalls^ [NameCode, NamePointer];
                   Call := ExpandTwoBytes (Prefix) + ExpandTwoBytes (Suffix);
                   GetRidOfPrecedingSpaces (Call);
                   GetRandomCall := Call;
                   Exit;
                   END
               ELSE
                   IF (CommonCalls^ [NameCode, NamePointer - 1] [1] = $FF) AND
                      (CommonCalls^ [NameCode, NamePointer - 1] [2] = $FF) AND
                      (CommonNames^ [NameCode, NamePointer - 1]     = $F)  THEN
                          BEGIN
                          Prefix := CommonCalls^ [NameCode, NamePointer];
                          Inc (NamePointer);
                          Suffix := CommonCalls^ [NameCode, NamePointer];
                          Call := ExpandTwoBytes (Prefix) + ExpandTwoBytes (Suffix);
                          GetRidOfPrecedingSpaces (Call);
                          GetRandomCall := Call;
                          Exit;
                          END;

            Suffix := CommonCalls^ [NameCode, NamePointer];

            REPEAT
                Dec (NamePointer);
            UNTIL (CommonCalls^ [NameCode, NamePointer - 1] [1] = $FF) AND
                  (CommonCalls^ [NameCode, NamePointer - 1] [2] = $FF) AND
                  (CommonNames^ [NameCode, NamePointer - 1]     = $F);

            Prefix := CommonCalls^ [NameCode, NamePointer];

            Call := ExpandTwoBytes (Prefix) + ExpandTwoBytes (Suffix);
            GetRidOfPrecedingSpaces (Call);

            GetRandomCall := Call;
            END
        ELSE
            GetRandomCall := 'WB6ZVC';
        END


    ELSE
        BEGIN
        IF UnCommonTotals [NameCode] > 0 THEN
            BEGIN
            NamePointer := Random (UnCommonTotals [NameCode] - 1);

            IF (UnCommonCalls^ [NameCode, NamePointer] [1] = $FF) AND
               (UnCommonCalls^ [NameCode, NamePointer] [2] = $FF) AND
               (UnCommonNames^ [NameCode, NamePointer] [1] = $FF) AND
               (UnCommonNames^ [NameCode, NamePointer] [2] = $FF) THEN
                   BEGIN
                   Inc (NamePointer);
                   Prefix := UnCommonCalls^ [NameCode, NamePointer];
                   Inc (NamePointer);
                   Suffix := UnCommonCalls^ [NameCode, NamePointer];
                   Call := ExpandTwoBytes (Prefix) + ExpandTwoBytes (Suffix);
                   GetRidOfPrecedingSpaces (Call);
                   GetRandomCall := Call;
                   Exit;
                   END
               ELSE
                   IF (UnCommonCalls^ [NameCode, NamePointer - 1] [1] = $FF) AND
                      (UnCommonCalls^ [NameCode, NamePointer - 1] [2] = $FF) AND
                      (UnCommonNames^ [NameCode, NamePointer - 1] [1] = $FF) AND
                      (UnCommonNames^ [NameCode, NamePointer - 1] [2] = $FF) THEN
                          BEGIN
                          Prefix := UnCommonCalls^ [NameCode, NamePointer];
                          Inc (NamePointer);
                          Suffix := UnCommonCalls^ [NameCode, NamePointer];
                          Call := ExpandTwoBytes (Prefix) + ExpandTwoBytes (Suffix);
                          GetRidOfPrecedingSpaces (Call);
                          GetRandomCall := Call;
                          Exit;
                          END;

            Suffix := UnCommonCalls^ [NameCode, NamePointer];

            REPEAT
                Dec (NamePointer);
            UNTIL (UnCommonCalls^ [NameCode, NamePointer - 1] [1] = $FF) AND
                  (UnCommonCalls^ [NameCode, NamePointer - 1] [2] = $FF) AND
                  (UnCommonNames^ [NameCode, NamePointer - 1] [1] = $FF)  AND
                  (UnCommonNames^ [NameCode, NamePointer - 1] [2] = $FF);

            Prefix := UnCommonCalls^ [NameCode, NamePointer];

            Call := ExpandTwoBytes (Prefix) + ExpandTwoBytes (Suffix);
            GetRidOfPrecedingSpaces (Call);

            GetRandomCall := Call;
            END
        ELSE
            GetRandomCall := 'WA6TUT';

        END;
    END;



PROCEDURE NameDictionary.InputASCIIFile;

VAR NumberNamesAdded, Column, NameColumn, CallColumn: INTEGER;
    InputFileName, TempString, TestString: Str80;
    Name, DataBaseName, Call: CallString;
    FileRead: TEXT;
    Key: CHAR;
    Ask: BOOLEAN;

    BEGIN
    ClrScr;
    TextColor (Yellow);
    WriteLnCenter ('INPUT ASCII FILE TO NAME DATABASE');
    WriteLn;
    TextColor (Cyan);
    WriteLn ('This procedure will add name and call information from an ASCII file to the ');
    WriteLn ('name database.  The first line of the ASCII file must be a FORMAT statement');
    WriteLn ('which indicates the column number to find the callsign and the column number');
    WriteLn ('to find the name.  An example would be FORMAT 1 3, where the call is found  ');
    WriteLn ('in the first column of data and the name is found in the third column of  ');
    WriteLn ('data.  Columns may be separated by any amount of white space.');
    WriteLn;
    WriteLn ('If the call is found in the database with a different name, you will be ');
    WriteLn ('asked which name you want to keep.');
    WriteLn;

    InputFileName := GetResponse ('Enter ASCII file to process : ');
    IF InputFileName = '' THEN Exit;

    NumberNamesAdded := 0;

    REPEAT
        Key := UpCase (GetKey ('Do you want to be given the chance to overwrite different names? (Y/N) : '));
        IF Key = EscapeKey THEN Exit;
    UNTIL (Key = 'Y') OR (Key = 'N');

    Ask := Key = 'Y';

    IF OpenFileForRead (FileRead, InputFileName) THEN
        BEGIN
        ReadLn (FileRead, TempString);
        TempString := UpperCase (TempString);

        TestString := RemoveFirstString (TempString);

        IF UpperCase (TestString) <> 'FORMAT' THEN
            BEGIN
            ReportError ('Missing format statement at start of this file!!');

            REPEAT
                Key := UpCase (GetKey ('Was this file generated by the name editor? (Y/N) : '));
                IF (Key = EscapeKey) OR (Key = 'N') THEN Exit;
            UNTIL (Key = 'Y');
            WriteLn;

            CallColumn := 1;
            NameColumn := 2;
            Close (FileRead);
            OpenFileForread (FileRead, InputFileName);
            END
        ELSE
            BEGIN
            CallColumn := RemoveFirstLongInteger (TempString);
            NameColumn := RemoveFirstLongInteger (TempString);
            END;



        WHILE NOT Eof (FileRead) DO
            BEGIN
            PinWheel;
            ReadLn (FileRead, TempString);

            Call := '';
            Name := '';
            Column := 0;

            WHILE ((Call = '') OR (Name = '')) AND (TempString <> '') DO
                BEGIN
                TestString := RemoveFirstString (TempString);
                Inc (Column);
                IF Column = CallColumn THEN Call := TestString;
                IF Column = NameColumn THEN Name := TestString;
                END;

            IF (Call <> '') AND (Name <> '') THEN
                BEGIN
                DatabaseName := GetName (Call);

                GoToXY (1, WhereY);

                IF DataBaseName <> Name THEN
                    IF DataBaseName = '' THEN
                        BEGIN
                        IF AddName (Call, Name) THEN
                            Inc (NumberNamesAdded);
                        END
                    ELSE
                        BEGIN
                        WriteLn (Call, '  Database name = ', DataBaseName,
                                 '  Name from file = ', Name);

                        IF Ask THEN
                            BEGIN

                            REPEAT
                                Key := UpCase (GetKey ('Use (D)atabase or (F)ile name? (D/F) : '));
                            IF Key = EscapeKey THEN Exit;
                            UNTIL (Key = 'D') OR (Key = 'F');

                            GoToXY (1, WhereY);
                            ClrEol;
                            GoToXY (1, WhereY - 1);
                            ClrEol;

                            IF Key = 'F' THEN
                                BEGIN
                                DeleteName (Call);
                                IF AddName (Call, Name) THEN Inc (NumberNamesAdded);
                                END;
                            END;
                        END;
                END;
            END;

        Close (FileRead);
        END
    ELSE
        ReportError ('Unable to find ' + InputFileName + '!!');

    WriteLn ('There were ', NumberNamesAdded, ' names added to the database.');
    END;



PROCEDURE NameDictionary.Init;

VAR NameCode: INTEGER;
    Directory: Str80;

    BEGIN
    Directory := FindDirectory ('NAMES.CMQ');

    IF Directory = '' THEN
        Directory := FindDirectory ('trlog');

    CMQFileName := Directory + DirectorySeparator + 'NAMES.CMQ';
    CMQBackUpFileName := Directory + DirectorySeparator + 'NAMESBAK.CMQ';

    FOR NameCode := 0 TO NumberArrays - 1 DO
        BEGIN
        CommonTotals   [NameCode] := 0;
        UncommonTotals [NameCode] := 0;
        END;

    NumberClubCalls := 0;
    END;


FUNCTION NameDictionary.Load: BOOLEAN;

    BEGIN
    WriteLn ('Memory available before loading name database = ', MaxAvail, ' bytes.');

    Load := False;


    IF CommonCalls = Nil THEN
        BEGIN
        IF SizeOf (CommonCalls^) + SizeOf (UnCommonCalls^) + SizeOf (CommonNames^) +
           SizeOf (ClubCalls^)   + SizeOf (UnCommonNames^) + 10000 > MaxAvail THEN
               BEGIN
               ReportError ('Insufficient memory for name database.  It will not be loaded.');
               WaitForKeyPressed;
               Exit;
               END;

        New (CommonCalls);
        END;

    IF UnCommonCalls = Nil THEN New (UncommonCalls);
    IF ClubCalls = Nil     THEN New (ClubCalls);
    IF CommonNames = Nil   THEN New (CommonNames);
    IF UncommonNames = Nil THEN New (UncommonNames);

    ArraysSetUp := True;

    IF ReLoad THEN
        BEGIN
        WriteLn ('There were a total of ', TotalCalls, ' calls loaded into the name database.');
        Load := True;
        END;
    END;


PROCEDURE NameDictionary.ComputeTotalCalls;

VAR NameCode, Address: INTEGER;

    BEGIN
    TotalCalls := 0;

    FOR NameCode := 0 TO NumberArrays - 1 DO
        BEGIN
        TotalCalls := TotalCalls + CommonTotals [NameCode] + UnCommonTotals [NameCode];

        IF CommonTotals [NameCode] > 0 THEN
            FOR Address := 0 TO CommonTotals [NameCode] - 1 DO
                IF (CommonCalls^ [NameCode, Address] [1] = $FF) AND
                   (CommonCalls^ [NameCode, Address] [2] = $FF) AND
                   (CommonNames^ [NameCode, Address] = $F) THEN
                       TotalCalls := TotalCalls - 2;

        IF UnCommonTotals [NameCode] > 0 THEN
            FOR Address := 0 TO UnCommonTotals [NameCode] - 1 DO
                IF (UnCommonCalls^ [NameCode, Address] [1] = $FF) AND
                   (UnCommonCalls^ [NameCode, Address] [2] = $FF) AND
                   (UnCommonNames^ [NameCode, Address] [1] = $FF) AND
                   (UnCommonNames^ [NameCode, Address] [2] = $FF) THEN
                       TotalCalls := TotalCalls - 2;
        END;
    TotalCalls := TotalCalls + NumberClubCalls;
    END;



FUNCTION NameDictionary.ReLoad: BOOLEAN;

VAR FileRead: FILE;
    NameCode, xResult: INTEGER;

    BEGIN
    NamesCleared := False;
    NumberClubCalls := 0;

    FOR NameCode := 0 TO NumberArrays - 1 DO
        BEGIN
        CommonTotals [NameCode] := 0;
        UncommonTotals [NameCode] := 0;
        END;

    IF FileExists (CMQFileName) THEN
        BEGIN
        Assign (FileRead, CMQFileName);
        Reset  (FileRead, 1);
        BlockRead (FileRead, CommonTotals, SizeOf (CommonTotals), xResult);
        BlockRead (FileRead, UncommonTotals, SizeOf (UncommonTotals), xResult);

        FOR NameCode := 0 TO NumberArrays - 1 DO
            BEGIN
            BlockRead (FileRead, CommonCalls^ [NameCode], SizeOf (CommonCalls^ [NameCode]), xResult);
            BlockRead (FileRead, CommonNames^ [NameCode], SizeOf (CommonNames^ [NameCode]), xResult);
            BlockRead (FileRead, UncommonCalls^ [NameCode], SizeOf (UncommonCalls^ [NameCode]), xResult);
            BlockRead (FileRead, UncommonNames^ [NameCode], SizeOf (UncommonNames^ [NameCode]), xResult);
            END;

        IF NOT Eof (FileRead) THEN
            BEGIN
            BlockRead (FileRead, NumberClubCalls, SizeOf (NumberClubCalls), xResult);
            BlockRead (FileRead, ClubCalls^, SizeOf (ClubCalls^), xResult);
            END;

        Close (FileRead);
        ComputeTotalCalls;
        ReLoad := True;
        END
    ELSE
        BEGIN
        WriteLn;
        WriteLn ('The file names.cmq was not found.');
        WriteLn;
        WriteLn ('This is normal if you are just starting out with the program.  If you are');
        WriteLn ('upgrading from a very old version of the program you might want to read on.');
        WriteLn;
        WriteLn ('This version of the program uses a new name database format to save memory.');
        WriteLn ('To put your old names (from names.cmp) into the new format, you must use a');
        WriteLn ('previous version of the name editor in POST (4.18 or older) to create an');
        WriteLn ('ASCII list of the names and callsigns.  Then you need to read that file');
        WriteLn ('into the new database using the name editor with this version.');
        WriteLn;
        WriteLn ('It is also possible names.cmq was not found because the directory it is in');
        WriteLn ('does not appear in your path statement.');
        WriteLn;
        WaitForKeyPressed;

        ReLoad := False;
        END;

    IF NameMemoryDisable THEN DumpNames;
    END;



PROCEDURE NameDictionary.MakeASCIIList (FileName: Str80);

VAR NameCode, Address: INTEGER;
    FileWrite: TEXT;
    PrefixFlag: BOOLEAN;
    TempBytes: FourBytes;

    BEGIN
    IF OpenFileForWrite (FileWrite, FileName) THEN
        BEGIN
        FOR NameCode := 0 TO NumberArrays - 1 DO
            BEGIN
            PrefixFlag := False;

            IF CommonTotals [NameCode] > 0 THEN
                FOR Address := 0 TO CommonTotals [NameCode] - 1 DO
                    BEGIN
                    IF PrefixFlag THEN
                        BEGIN
                        TempBytes [1] := CommonCalls^ [Namecode, Address] [1];
                        TempBytes [2] := CommonCalls^ [Namecode, Address] [2];

                        PrefixFlag := False;

                        IF (CommonCalls^ [NameCode, Address] [1] = $FF) AND
                           (CommonCalls^ [NameCode, Address] [2] = $FF) AND
                           (CommonNames^ [Namecode, Address]     = $F) THEN
                               BEGIN
                               ReportError ('Consequetive Prefix flags found!!');
                               WriteLn ('CommonArray = ', NameCode, '  Address = ', Address);
                               Halt;
                               END;

                        Continue;
                        END;

                    IF (CommonCalls^ [NameCode, Address] [1] = $FF) AND
                       (CommonCalls^ [NameCode, Address] [2] = $FF) AND
                       (CommonNames^ [Namecode, Address]     = $F) THEN
                           BEGIN
                           PrefixFlag := True;
                           Continue;
                           END
                       ELSE
                           IF Address = 0 THEN
                               BEGIN
                               ReportError ('First entry is not prefix flag!!');
                               WriteLn ('CommonArray = ', NameCode, '  Address = ', Address);
                               Halt;
                               END;

                    TempBytes [3] := CommonCalls^ [Namecode, Address] [1];
                    TempBytes [4] := CommonCalls^ [Namecode, Address] [2];

                    WriteLn (FileWrite,
                             ExpandedString (TempBytes), ' ',
                             ExpandedString (CommonNameList [CommonNames^ [NameCode, Address]]));
                    END;

            PrefixFlag := False;



            IF UnCommonTotals [NameCode] > 0 THEN
                FOR Address := 0 TO UnCommonTotals [NameCode] - 1 DO
                    BEGIN
                    IF PrefixFlag THEN
                        BEGIN
                        TempBytes [1] := UnCommonCalls^ [Namecode, Address] [1];
                        TempBytes [2] := UnCommonCalls^ [Namecode, Address] [2];

                        PrefixFlag := False;

                        IF (UnCommonCalls^ [NameCode, Address] [1] = $FF) AND
                           (UnCommonCalls^ [NameCode, Address] [2] = $FF) AND
                           (UnCommonNames^ [Namecode, Address] [1] = $FF) AND
                           (UnCommonNames^ [Namecode, Address] [2] = $FF) THEN
                               BEGIN
                               ReportError ('Consequetive Prefix flags found!!');
                               WriteLn ('UnCommonArray = ', NameCode, '  Address = ', Address);
                               Halt;
                               END;

                        Continue;
                        END;

                    IF (UnCommonCalls^ [NameCode, Address] [1] = $FF) AND
                       (UnCommonCalls^ [NameCode, Address] [2] = $FF) AND
                       (UnCommonNames^ [Namecode, Address] [1] = $FF) AND
                       (UnCommonNames^ [Namecode, Address] [2] = $FF) THEN
                           BEGIN
                           PrefixFlag := True;
                           Continue;
                           END
                       ELSE
                           IF Address = 0 THEN
                               BEGIN
                               ReportError ('First entry is not prefix flag!!');
                               WriteLn ('UnCommonArray = ', NameCode, '  Address = ', Address);
                               WriteLn (UnCommonCalls^ [NameCode, Address] [1]);
                               WriteLn (UnCommonCalls^ [NameCode, Address] [2]);
                               WriteLn (UnCommonNames^ [Namecode, Address] [1]);
                               WriteLn (UnCommonNames^ [Namecode, Address] [2]);
                               Halt;
                               END;

                    TempBytes [3] := UnCommonCalls^ [Namecode, Address] [1];
                    TempBytes [4] := UnCommonCalls^ [Namecode, Address] [2];

                    WriteLn (FileWrite,
                             ExpandedString (TempBytes), ' ',
                             ExpandedString (UnCommonNames^ [NameCode, Address]));
                    END;

            END;

        IF NumberClubCalls > 0 THEN
            FOR Address := 0 TO NumberClubCalls - 1 DO
                WriteLn (FileWrite, ExpandedString (ClubCalls^ [Address]), ' CLUB');

        Close (FileWrite);
        END;
    END;



PROCEDURE NameDictionary.Save;

    BEGIN
    UpdateNameFile;
    GoToXY  (1, WhereY);
    ClrEol;
    WriteLn ('There were ', TotalCalls, ' calls saved.');
    WriteLn ('The fullest common call array is ',
              LargestCommonArray / MaximumArrayCommonNames * 100:3:1,
              '% full.');

    WriteLn ('The fullest uncommon call array is ',
              LargestUncommonArray / MaximumArrayUncommonNames * 100:3:1,
              '% full.');

    WriteLn ('There are ', NumberClubCalls, ' club calls (max = ', MaxClubCalls, ').');
    Abort;
    END;



FUNCTION NameDictionary.TotalNumberOfCalls: INTEGER;

    BEGIN
    TotalNumberOfCalls := TotalCalls;
    END;



PROCEDURE NameDictionary.UpdateNameFile;

VAR FileWrite: FILE;
    NameCode: INTEGER;
    xResult: INTEGER;

    BEGIN
    IF NamesCleared THEN Exit;

    IF FileExists (CMQFileName) THEN
        RenameFile (CMQFileName, CMQBackUpFileName);

    ASSIGN  (FileWrite, CMQFileName);
    REWRITE (FileWrite, 1);

    BlockWrite (FileWrite, CommonTotals, SizeOf (CommonTotals), xResult);
    BlockWrite (FileWrite, UncommonTotals, SizeOf (UncommonTotals), xResult);

    LargestCommonArray := 0;
    LargestUncommonArray := 0;

    FOR NameCode := 0 TO NumberArrays - 1 DO
        BEGIN
        BlockWrite (FileWrite, CommonCalls^ [NameCode], SizeOf (CommonCalls^ [NameCode]), xResult);
        BlockWrite (FileWrite, CommonNames^ [NameCode], SizeOf (CommonNames^ [NameCode]), xResult);
        BlockWrite (FileWrite, UncommonCalls^ [NameCode], SizeOf (UncommonCalls^ [NameCode]), xResult);
        BlockWrite (FileWrite, UncommonNames^ [NameCode], SizeOf (UncommonNames^ [NameCode]), xResult);

        IF CommonTotals [NameCode] > LargestCommonArray THEN
            LargestCommonArray := CommonTotals [NameCode];

        IF UncommonTotals [NameCode] > LargestUncommonArray THEN
            LargestUncommonArray := UncommonTotals [NameCode];

        END;

    BlockWrite (FileWrite, NumberClubCalls, SizeOf (NumberClubCalls), xResult);
    BlockWrite (FileWrite, ClubCalls^, SizeOf (ClubCalls^), xResult);
    Close (FileWrite);
    END;



PROCEDURE NameDictionary.AddCallToPossibleList (Call: CallString; VAR List: CallListRecord);

VAR Entry: INTEGER;

    BEGIN
    GetRidOfPrecedingSpaces (Call);

    IF List.NumberCalls < 12 THEN
        BEGIN
        IF List.NumberCalls > 0 THEN
            FOR Entry := 0 TO List.NumberCalls - 1 DO
                IF List.CallList [Entry].Call = Call THEN
                    Exit;

        List.CallList [List.NumberCalls].Call := Call;
        List.CallList [List.NumberCalls].Dupe := False;
        Inc (List.NumberCalls);
        END;
    END;



PROCEDURE NameDictionary.MakePossibleCallList (Call: CallString; VAR List: CallListRecord);


{ Possible calls either match prefixes and the suffixes are possible calls
  to each other, or the prefixes are possible calls and the suffixes match.}


VAR Address, NameCode, Index: INTEGER;
    TestCall, ListPrefix, Prefix, Suffix, TempCall: CallString;
    TestBytes: FourBytes;
    PrefixFlag: BOOLEAN;

    BEGIN
    IF TotalCalls = 0 THEN Exit;
    Call := StandardNameCallFormat (Call);

    Suffix := Copy (Call, Length (Call) - 2, 3);
    Prefix := Copy (Call, 1, Length (Call) - 3);



    FOR NameCode := 0 TO NumberArrays - 1 DO
        BEGIN
        PrefixFlag := False;

        IF CommonTotals [NameCode] > 0 THEN
            BEGIN
            FOR Address := 0 TO CommonTotals [NameCode] - 1 DO
                BEGIN
                IF PrefixFlag THEN
                    BEGIN
                    ListPrefix := ExpandTwoBytes (CommonCalls^ [Namecode, Address]);
                    GetRidOfPrecedingSpaces (ListPrefix);

                    IF ListPrefix = Prefix THEN
                        BEGIN
                        FOR Index := Address + 1 TO CommonTotals [NameCode] - 1 DO
                            BEGIN
                            IF (CommonCalls^ [NameCode, Index] [1] = $FF) AND
                               (CommonCalls^ [NameCode, Index] [2] = $FF) AND
                               (CommonNames^ [Namecode, Index]     = $F) THEN
                                   Break;

                            TestCall := ExpandTwoBytes (CommonCalls^ [NameCode, Index]);

                            IF SimilarCall (TestCall, Suffix) THEN
                                AddCallToPossibleList (Prefix + TestCall, List);
                            END;
                        END
                    ELSE
                        IF SimilarCall (ListPrefix, Prefix) THEN
                            BEGIN
                            FOR Index := Address + 2 TO CommonTotals [NameCode] - 1 DO
                                BEGIN
                                IF (CommonCalls^ [NameCode, Index] [1] = $FF) AND
                                   (CommonCalls^ [NameCode, Index] [2] = $FF) AND
                                   (CommonNames^ [Namecode, Index]     = $F) THEN
                                       Break;

                                TestCall := ExpandTwoBytes (CommonCalls^ [NameCode, Index]);

                                IF TestCall = Suffix THEN
                                    AddCallToPossibleList (ListPrefix + Suffix, List);

                                END;
                            END;

                    PrefixFlag := False;
                    END;

                IF (CommonCalls^ [NameCode, Address] [1] = $FF) AND
                   (CommonCalls^ [NameCode, Address] [2] = $FF) AND
                   (CommonNames^ [Namecode, Address]     = $F) THEN
                       BEGIN
                       PrefixFlag := True;
                       Continue;
                       END;

                END;

            PrefixFlag := False;
            END;



        IF UnCommonTotals [NameCode] > 0 THEN
            BEGIN
            FOR Address := 0 TO UnCommonTotals [NameCode] - 1 DO
                BEGIN
                IF PrefixFlag THEN
                    BEGIN
                    ListPrefix := ExpandTwoBytes (UnCommonCalls^ [Namecode, Address]);
                    GetRidOfPrecedingSpaces (ListPrefix);

                    IF ListPrefix = Prefix THEN
                        BEGIN
                        FOR Index := Address + 1 TO UnCommonTotals [NameCode] - 1 DO
                            BEGIN
                            IF (UnCommonCalls^ [NameCode, Index] [1] = $FF) AND
                               (UnCommonCalls^ [NameCode, Index] [2] = $FF) AND
                               (UnCommonNames^ [Namecode, Index] [1] = $FF) AND
                               (UnCommonNames^ [Namecode, Index] [2] = $FF) THEN
                                   Break;

                            TestCall := ExpandTwoBytes (UnCommonCalls^ [NameCode, Index]);

                            IF SimilarCall (TestCall, Suffix) THEN
                                AddCallToPossibleList (Prefix + TestCall, List);
                            END;
                        END
                    ELSE
                        IF SimilarCall (ListPrefix, Prefix) THEN
                            BEGIN
                            FOR Index := Address + 2 TO UnCommonTotals [NameCode] - 1 DO
                                BEGIN
                                IF (UnCommonCalls^ [NameCode, Index] [1] = $FF) AND
                                   (UnCommonCalls^ [NameCode, Index] [2] = $FF) AND
                                   (UnCommonNames^ [Namecode, Index] [1] = $FF) AND
                                   (UnCommonNames^ [Namecode, Index] [2] = $FF) THEN
                                       Break;

                                TestCall := ExpandTwoBytes (UnCommonCalls^ [NameCode, Index]);

                                IF TestCall = Suffix THEN
                                    AddCallToPossibleList (ListPrefix + Suffix, List);

                                END;
                            END;

                    PrefixFlag := False;
                    END;

                IF (UnCommonCalls^ [NameCode, Address] [1] = $FF) AND
                   (UnCommonCalls^ [NameCode, Address] [2] = $FF) AND
                   (UnCommonNames^ [Namecode, Address] [1] = $FF) AND
                   (UnCommonNames^ [Namecode, Address] [2] = $FF) THEN
                       BEGIN
                       PrefixFlag := True;
                       Continue;
                       END;

                END;

            PrefixFlag := False;
            END;

        END;



    IF NumberClubCalls > 0 THEN
        FOR Address := 0 TO NumberClubCalls - 1 DO
            BEGIN
            IF NumBytes (Addr (TestBytes), Addr (ClubCalls^ [Address])) >=2 THEN
                BEGIN
                TempCall := ExpandedString (ClubCalls^ [Address]);
                IF SimilarCall (TempCall, TestCall) THEN
                    AddCallToPossibleList (TempCall, List);
                END;
            END;

    END;



    BEGIN
    ArraysSetUp := False;

    Names.Init;
    NamesCleared := False;
    Names.NameMemoryDisable := False;
    Randomize;
    END.


