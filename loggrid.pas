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

UNIT LogGrid;

{$O+}

{ Minus lon is east }

INTERFACE

USES Tree, Dos, trCRT,communication,datetimec;

TYPE DistanceDisplayType = (NoDistanceDisplay, DistanceMiles, DistanceKM);

FUNCTION  ConvertLatLonToGrid (Lat, Lon: REAL): Str20;
FUNCTION  GetBeamHeading (MyGrid, HisGrid: Str20): INTEGER;
FUNCTION  GetDistanceBetweenGrids (Grid1, Grid2: Str20): INTEGER;
FUNCTION  GetEuropeanDistanceBetweenGrids (Grid1, Grid2: Str20): INTEGER;
PROCEDURE GetLatLon (Grid: Str20; VAR Lat, Lon: REAL);
FUNCTION  GetSunriseSunsetString (Lat: REAL; Lon: Real): Str80;

VAR ActivePacketPort: serialportx;
    DistanceMode: DistanceDisplayType;
    MyGrid: STRING [10];
    RadiusOfEarth: REAL;



IMPLEMENTATION

FUNCTION LooksLikeAGrid (Grid: Str20): BOOLEAN;

{ Verifies that the grid square is legitimate }

VAR CharPosition: INTEGER;

    BEGIN
    LooksLikeAGrid := False;

    IF NOT ((Length (Grid) = 4) OR (Length (Grid) = 6)) THEN Exit;

    Grid := UpperCase (Grid);

    FOR CharPosition := 1 TO Length (Grid) DO
        CASE CharPosition OF
            1, 2, 5, 6:
                IF (Grid [CharPosition] < 'A') OR (Grid [CharPosition] > 'Z') THEN
                    Exit;

            3, 4:
                IF (Grid [CharPosition] < '0') OR (Grid [CharPosition] > '9') THEN
                    Exit;
            END;

    LooksLikeAGrid := True;
    END;



PROCEDURE ConvertGridToLatLon (Grid: Str20; VAR Lat, Lon: REAL);

{ Converts a grid to Lat/Lon to the center of the grid }

VAR LonMin, londeg, latmin, latdeg: REAL;

    BEGIN
    LonMin := (5 * (Ord ( Grid [5])- Ord ('A'))) + 2.5 ;        { center }
    LonDeg := 180 -( 20 * (Ord (Grid [1]) - Ord ('A')))         { tens of deg }
                        - ( 2 * (Ord (Grid [3]) - Ord ('0'))) ; { two deg }

    Lon := londeg - (lonmin/60) ;

    latdeg := -90 + ( 10 * (Ord (Grid [2])-Ord ('A')))        { tens of deg }
                    + (Ord (Grid [4])-Ord ('0'));             { degrees }

    latmin := 2.5 * (Ord (Grid [6]) - Ord ('A'))                { minutes }
                       + 1.25 ;                              { for center }

    Lat := latdeg + (latmin / 60) ;
    END ;



FUNCTION ConvertLatLonToGrid (Lat, Lon: REAL): Str20;

{ This procedure will convert a latitude and longitude to a grid square. }
{ Minus lat is east }

VAR c: INTEGER;

    G4, L4 : REAL;
    M1, M2, M3, M4, M5, M6 : Char;


    BEGIN
    { First we do the longitude stuff - chars 1 3 and 5 }

    G4 := 180 - Lon;

    C  := Trunc (G4 / 20);              { Get first letter of Grid }
    M1 := CHR (C + Ord ('A'));

    G4 := G4 - C * 20;                  { Remove that compoenent }

    C  := Trunc (G4 / 2);               { Get third letter of Grid}
    M3 := Chr (C + Ord ('0'));

    G4 := G4 - C * 2;                   { Remove that component }

    C  := Trunc (G4 / (2 / 24));        { Get fifth letter }
    M5 := Chr (C + Ord ('A'));

    { Similar process for longitude }

    L4 := Lat + 90;

    C  := Trunc (L4 / 10);
    M2 := CHR (C + Ord ('A'));

    L4 := L4 - C * 10;

    C  := Trunc (L4);
    M4 := Chr (C + Ord ('0'));

    L4 := L4 - C;

    C  := Trunc (L4 / (1 / 24));
    M6 := Chr (C + Ord ('A'));

    ConvertLatLonToGrid := M1 + M2 + M3 + M4 + M5 + M6;
    END;



PROCEDURE Calc_GeoDist (MyLat, MyLon, HisLat, HisLon : REAL;
                        var Az, Baz, Dist: REAL);

                 {Taken directly from:       }
                 {Thomas, P.D., 1970, Spheroidal geodesics, reference systems,}
                 {    & local geometry, U.S. Naval Oceanographic Office SP-138,}
                 {    165 pp.}

                 { assumes North Latitude and East Longitude are positive}

                 {MyLat, MyLon = MyLat, MyLon}
                 {HisLat, HisLon = HisLat, HisLon}
                 {Az, BAz = direct & reverse azimuith}
                 {Dist = Dist (km); Deg = central angle, discarded }

CONST
{     AL = 6378206.4;  }{ Clarke 1866 ellipsoid - Equatorial radius in km }
{     BL = 6356583.8;   }{ Polar radius in km }

      DefaultRadiusOfEarth = 6378137.0;   { From K6SE - Dec '98 }
      BL = 6356752.3;   { Calculated from K6SE data - Dec '98 - no longer used }

      F = 1.0 / 298.257223563; { Flattening constant based on actual Earth size }
      BDA = 1.0 - F;           { Polar diameter divided by equatorial diameter }

      D2R = pi/180.0;   { degrees to radians conversion factor }
      Pi2 = 2.0 * Pi;

LABEL 1, 2, 3, 4, 5, 6, 7, 8, 9;

VAR P1R, P2R,
    L1R, L2R,
    DLR,
    T1R, T2R,
    TM, DTM,
    STM, CTM,
    SDTM,CDTM,
    KL, KK,
    SDLMR, TL,
    CD, DL, SD,
    T, U, V, D, X, E, Y, A,
    FF64, TDLPM,
    HAPBR, HAMBR,
    AL, A1M2, A2M1        : REAL;

    BEGIN
    IF RadiusOfEarth > 0 THEN
        AL := RadiusOfEarth
    ELSE
        AL := DefaultRadiusOfEarth;

    P1R := MyLat * D2R;
    P2R := HisLat * D2R;
    L1R := - MyLon * D2R;
    L2R := - HisLon * D2R;

    DLR := L2R - L1R;
    T1R := ArcTan (BDA * Tan(P1R));
    T2R := ArcTan (BDA * Tan(P2R));

    TM := (T1R + T2R) / 2.0;
    DTM := (T2R - T1R) / 2.0;
    STM := Sin(TM);
    CTM := Cos(TM);
    SDTM := Sin(DTM);
    CDTM := Cos(DTM);
    SDLMR := Sin(DLR/2.0);

    TL := SDTM * SDTM + SDLMR * SDLMR * (CDTM * CDTM - STM * STM);
    CD := 1.0 - 2.0 * TL;
    DL := ArcCos (CD);
    SD := Sin (DL);

    KL := STM * CDTM;
    KK := SDTM * CTM;
    TL := SDTM * SDTM + SDLMR * SDLMR * (CDTM * CDTM - STM * STM);
    DL := ArcCos (CD);
    SD := Sin (DL);

    { Anti divide by zero techniques }

    IF SD = 0 THEN SD := 0.000000001;

    T := DL/SD;
    U := 2.0 * KL * KL / (1.0 - TL);

    { Anti divide by zero techniques }

    IF  TL = 0 THEN  TL := 0.000000001;

    V := 2.0 * KK * KK / TL;
    D := 4.0 * T * T;
    X := U + V;
    E := -2.0 * CD;
    Y := U - V;
    A := -D * E;
    FF64 := F * F / 64.0;
    Dist := AL*SD*(T -(F/4.0)*(T*X-Y)+FF64*(X*(A+(T-(A+E)/2.0)*X)+Y*(-2.0*D+E*Y)+D*X*Y))/1000.0;
    TDLPM := Tan((DLR+(-((E*(4.0-X)+2.0*Y)*((F/2.0)*T+FF64*(32.0*T+(A-20.0*T)*X-2.0*(D+2.0)*Y))/4.0)*Tan(DLR)))/2.0);
    HAPBR := ATan2(SDTM,(CTM*TDLPM));
    HAMBR := Atan2(CDTM,(STM*TDLPM));
    A1M2 := Pi2 + HAMBR - HAPBR;
    A2M1 := Pi2 - HAMBR - HAPBR;

   1:  If (A1M2 >= 0.0) AND (A1M2 < Pi2) then GOTO 5
                                         else GOTO 2;

   2:  If A1M2 >= Pi2 then GOTO 3
                       else GOTO 4;

   3:  A1M2 := A1M2 - Pi2;
       GOTO 1;

   4:  A1M2 := A1M2 + Pi2;
       GOTO 1;

   5:  If (A2M1 >= 0.0) AND (A2M1 < Pi2) then GOTO 9
                                         else GOTO 6;

   6:  If A2M1 >= Pi2 then GOTO 7
                      else GOTO 8;

   7:  A2M1 := A2M1 - Pi2;
       GOTO 5;

   8:  A2M1 := A2M1 + Pi2;
       GOTO 5;

   9: Az := A1M2 / D2R;
      BAZ := A2M1 / D2R;

    END; {Calc_GeoDist}



FUNCTION GetDistanceBetweenGrids (Grid1, Grid2: Str20): INTEGER;

{ This function returns the distance between the two grids specified in
  kilometers. }

VAR Lat1, Lat2, Lon1, Lon2: REAL;
    ShortHeading, LongHeading, Distance: REAL;

    BEGIN
    IF Length (Grid1) <> 6 THEN Grid1 := Grid1 + 'll';
    Grid1 := UpperCase (Grid1);

    IF Length (Grid2) <> 6 THEN Grid2 := Grid2 + 'll';
    Grid2 := UpperCase (Grid2);

    IF Grid1 = Grid2 THEN
        BEGIN
        GetDistanceBetweenGrids := 0;
        Exit;
        END;

    ConvertGridToLatLon (Grid1, Lat1, Lon1);
    ConvertGridToLatLon (Grid2, Lat2, Lon2);

    Calc_GeoDist (Lat1, Lon1, Lat2, Lon2, ShortHeading, LongHeading, Distance);
    GetDistanceBetweenGrids := Round (Distance);
    END;



FUNCTION GetEuropeanDistanceBetweenGrids (Grid1, Grid2: Str20): INTEGER;

CONST
     R= PI/180;                                            {to Radian}
{     RZ=(180 * 111.2)/PI;}
     RZ= 6369; {produces same results as DL2NBU, LOCATOR 9.97b and others }
var  C1, C2, N1, N2, C3, C4, CC1, CC2, NN1, NN2, CC3, CC4: INTEGER;
     X, KM, A, B, FX, FY: REAL;
BEGIN

    Grid1 := UpperCase (Grid1);
    Grid2 := UpperCase (Grid2);

    C1 := Ord (Grid1 [1]) - 74;
    C2 := Ord (Grid1 [2]) - 74;
    N1 := Ord (Grid1 [3]) - 48;
    N2 := Ord (Grid1 [4]) - 48;
    C3 := Ord (Grid1 [5]) - 65;
    C4 := Ord (Grid1 [6]) - 65;
    CC1 := Ord (Grid2 [1]) - 74;
    CC2 := Ord (Grid2 [2]) - 74;
    NN1 := Ord (Grid2 [3]) - 48;
    NN2 := Ord (Grid2 [4]) - 48;
    CC3 := Ord (Grid2 [5]) - 65;
    CC4 := Ord (Grid2 [6]) - 65;
    A := R * ( C1 * 20 + N1 * 2 + C3 /12);
    B := R * ( C2 * 10 + N2 + C4 / 24);
    FX := R * (CC1 * 20 + NN1 * 2 + CC3 / 12);
    FY := R * (CC2 * 10 + NN2 + CC4 / 24);
    X := SIN(B) * SIN(FY) + COS(B) * COS(FY) * COS (FX-A);
    KM := RZ * ArcTan (Sqrt (1 - Sqr (x)) / X) + 0.5;
    GetEuropeanDistanceBetweenGrids := Round (KM);
    END;



FUNCTION GetBeamHeading (MyGrid, HisGrid: Str20): INTEGER;

VAR Lat1, Lat2, Lon1, Lon2: REAL;
    ShortHeading, LongHeading, Distance: REAL;

    BEGIN
    IF (Length (MyGrid) <> 4) AND (Length (MyGrid) <> 6) THEN Exit;
    IF (Length (HisGrid) <> 4) AND (Length (HisGrid) <> 6) THEN Exit;

    IF Length (MyGrid) <> 6 THEN MyGrid := MyGrid + 'll';
    MyGrid := UpperCase (MyGrid);

    IF Length (HisGrid) <> 6 THEN HisGrid := HisGrid + 'll';
    HisGrid := UpperCase (HisGrid);

    IF HisGrid = MyGrid THEN
        BEGIN
        GetBeamHeading := 0;
        Exit;
        END;

    ConvertGridToLatLon (MyGrid,  Lat1, Lon1);
    ConvertGridToLatLon (HisGrid, Lat2, Lon2);

    Calc_GeoDist (Lat1, Lon1, Lat2, Lon2, ShortHeading, LongHeading, Distance);
    GetBeamHeading := Round (ShortHeading);
    END;



PROCEDURE GetLatLon (Grid: Str20; VAR Lat, Lon: REAL);

    BEGIN
    IF Length (Grid) <> 6 THEN Grid := Grid + 'll';
    Grid := UpperCase (Grid);
    ConvertGridToLatLon (Grid,  Lat, Lon);
    END;



FUNCTION DayOfYear (Year, Month, Day: WORD): INTEGER;

VAR TempDay: INTEGER;

    BEGIN
    TempDay := Trunc ((30.55556 * Month) + Day - 30);

    Year := Year MOD 100;

    IF Month > 2 THEN
        IF (Year MOD 4 = 0) AND (Year <> 0) THEN { Leap year }
            TempDay := TempDay - 1
        ELSE
            TempDay := TempDay - 2;

    DayOfYear := TempDay;
    END;




FUNCTION CalculateTime (Year: INTEGER;
                        Month: INTEGER;
                        Day: INTEGER;
                        Lat: REAL;
                        Lon: REAL;
                        Sunrise: BOOLEAN;
                        VAR Hours: INTEGER; VAR Minutes: INTEGER): BOOLEAN;

VAR DOY: WORD;
    Doy1, UTC, UTCrad, T1, T2, T3, Q, DECSol, Anm, RAsol, LONsol: REAL;

    BEGIN
    CalculateTime := True;
    Lat := Lat * PI/180;

    DOY := DayOfYear (Year, Month, Day);

    IF Sunrise THEN
        Doy1 := DOY + lon / 360.0 + 0.25
    ELSE
        Doy1 := DOY + lon / 360.0 + 0.75;

    { Calculate Solar Mean Anomaly - Anm }

    Anm := (0.017202 * Doy1) - 0.0574039;

    { anm = 0.017202*doy1 - 0.0574039; }

    { Calculate Solar True Longitude - LONsol }

    LONsol := Anm + (0.033405 * Sin (Anm)) +
                    (3.49066E-04 * Sin (2 * Anm)) +
                    4.93289;

    { lonsol = anm + 0.0334405 * sin(anm) +
                     3.49066e-4 * sin(2*anm) +
                     4.93289; }

    { Force LONsol to be in the range 0 to 2pi by adding/subtracting 2pi }

    WHILE LONsol > (2 * Pi) DO
        LONsol := LONsol - (2 * Pi);

    WHILE LONsol < 0 DO
        LONsol := LONsol + (2 * Pi);

    {   if (lonsol<0)
	    lonsol += (2*PI);

        if (lonsol>(2*PI))
	    lonsol -= (2*PI); }

    { Calculate Solar RightAscension - RAsol }

    RAsol := ArcTan (0.91746 * ( Sin (LONsol) / Cos (LONsol) ) );

    IF (LONsol >= 0) AND (LONsol <= (Pi / 2)) THEN
        RAsol := RAsol + (0 * Pi)
    ELSE
        IF (LONsol > (Pi / 2) ) AND (LONsol <= (3 * Pi) / 2) THEN
            RAsol := RAsol + (1 * Pi)
    ELSE
        IF (LONsol > ((3 * Pi) / 2) ) THEN
            RAsol := RAsol + (2 * Pi);


    {   if (lonsol<(PI/2))
	    k1 = 0;
        else if (lonsol>(3*PI/2))
	    k1 = 2;
        else k1 = 1; }

    {   rasol = atan(0.91746*tan(lonsol)) + k1*PI; }


    { Calculate Solar Declination - DECsol }

    Q := 0.39872 * Sin (LONsol);

    DECSol := Q / Sqrt (1 - (Q * Q));

    {   q = 0.39872*sin(lonsol);
        decsol = q/sqrt(1 - q*q); }

    { Calculate Local Apparent Time for Desired Event - T3 }

    T1 := -0.01483 / (Cos (ArcTan (DECSol)) * Cos (LAT)) - DECsol * Tan (LAT);

    {   t1 = -0.01483/(cos(atan(decsol))*cos(lat)) - decsol*tan(lat); }

    IF (T1 > 1) OR (T1 < -1) THEN
        BEGIN
        CalculateTime := False;
        Exit;
        END;

    T2 := - ArcTan (T1 / Sqrt (1 - (T1 * T1))) + (Pi / 2);

    IF SunRise THEN T2 := (2 * Pi) - T2;

    {   t2 = -atan(t1/sqrt(1 - t1*t1)) + PI/2;
        if (sr == TRUE)
	    t2 = 2*PI - t2; }

    T3 := T2 + RAsol - (0.0172028 * Doy1) - 1.73364;

    {   t3 = t2 + rasol - 0.0172028*doy - 1.73364; }

    UTCrad := T3 + Lon * Pi / 180;

    IF UTCrad < 0 THEN UTCrad := UTCRad + 2*Pi;
    IF UTCrad > 2 *  Pi THEN UTCrad := UTCRad - 2*Pi;


    {   utcrad = t3 + lon*PI/180;
        if (utcrad < 0)
	    utcrad += (2*PI);
        if (utcrad >(2*PI))
	    utcrad -= (2*PI);  }

    UTC := UTCrad * 12/Pi;

    {   utc = utcrad*12/PI;
        utc_hr = (int)(utc);
        utc_min = 60.0*(utc - utc_hr); }

    Hours   := Round (Int (UTC));
    Minutes := Round (Frac (UTC) * 60);

    IF Minutes = 60 THEN
        BEGIN
        Minutes := 0;
        Inc (Hours);

        IF Hours = 24 THEN Hours := 0;
        END;
    END;


FUNCTION GetSunriseSunsetString (Lat: REAL; Lon: Real): Str80;

VAR Year, Month, Day, DayOfWeek: WORD;
    HourString, MinuteString, SunString: Str80;
    Hour, Minute: INTEGER;

    BEGIN
    GetDate (Year, Month, Day, DayOfWeek);

    IF CalculateTime (Year, Month, Day, Lat, Lon, True, Hour, Minute) THEN
        BEGIN
        Str (Hour, HourString);
        Str (Minute, MinuteString);
        IF Length (HourString) = 1 THEN HourString := '0' + HourString;
        IF Length (MinuteString) = 1 THEN MinuteString := '0' + MinuteString;
        SunString := HourString + MinuteString + 'z/';
        END
    ELSE
        BEGIN
        IF Lat > 0 THEN
            BEGIN
            IF (Month >= 3) AND (Month <=9) THEN
                GetSunriseSunSetString := 'All sun'
            ELSE
                GetSunriseSunSetString := 'All dark';
            END
        ELSE
            IF (Month >= 3) AND (Month <=9) THEN
                GetSunriseSunSetString := 'All dark'
            ELSE
                GetSunriseSunSetString := 'All sun';
        Exit;
        END;

    IF CalculateTime (Year, Month, Day, Lat, Lon, False, Hour, Minute) THEN
        BEGIN
        Str (Hour, HourString);
        Str (Minute, MinuteString);
        IF Length (HourString) = 1 THEN HourString := '0' + HourString;
        IF Length (MinuteString) = 1 THEN MinuteString := '0' + MinuteString;

        SunString := SunString + HourString + MinuteString + 'z';
        END
    ELSE
        BEGIN
        IF Lat > 0 THEN
            BEGIN
            IF (Month >= 3) AND (Month <=9) THEN
                GetSunriseSunSetString := 'All sun'
            ELSE
                GetSunriseSunSetString := 'All dark';
            END
        ELSE
            IF (Month >= 3) AND (Month <=9) THEN
                GetSunriseSunSetString := 'All dark'
            ELSE
                GetSunriseSunSetString := 'All sun';
        Exit;
        END;

    GetSunriseSunsetString := SunString;
    END;



    BEGIN
    END.



