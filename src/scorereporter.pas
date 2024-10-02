{$mode objfpc}
unit scorereporter;
{$l scoreposter.o}

interface
uses classes,baseunix,unix,ctypes;

TYPE
   scorereport = class
      private
         call: string;
         contest: string;
         club: string;
         ops: string;
         mode: string;
         power: string;
         bands: string;
         transmitter: string;
         assisted: string;
         overlay: string;
         breakdown: boolean;
         enable: boolean;
         debugenable: boolean;
         intervalcount: integer;
         icount: integer;
         url: ansistring;
         user: ansistring;
         password: ansistring;
         tstr: TStringStream;
         fd: longint;
         slave: string;
         pid: TPid;
         domesticmult: string;
         dxmult: string;
         zonemult: string;
         prefixmult: string;
         mult: array[1..4] of string;
         nmulttype: integer;
         procedure postfork;

     public
         constructor create;
         procedure setenable(b: boolean);
         procedure setdebugenable(b: boolean);
         procedure setcall(s: string);
         procedure setclub(s: string);
         procedure setcontest(s: string);
         procedure setclassops(s: string);
         procedure setclassmode(s: string);
         procedure setclasspower(s: string);
         procedure setclassbands(s: string);
         procedure setclasstransmitter(s: string);
         procedure setclassassisted(s: string);
         procedure setclassoverlay(s: string);
         procedure setbreakdown(b: boolean);
         procedure setinterval(i: integer);
         procedure setposturl(s: string);
         procedure setuser(s: string);
         procedure setpassword(s: string);
         procedure setdomesticmult(s: string);
         procedure setdxmult(s: string);
         procedure setzonemult(s: string);
         procedure setprefixmult(s: string);
         procedure setnmulttype (n: INTEGER);
         procedure writexmlmessage;
         procedure timer(caughtup: boolean);
         function enabled:boolean;
         procedure refork;
         procedure setup;
   end;

   procedure diewithparent;cdecl;external;
   function initscoreposter(url: pchar; user:pchar;
      debugin:integer):integer;cdecl;external;
   procedure endscoreposter;cdecl;external;
   procedure poster;cdecl;external;
   function getpt:cint;cdecl;external;
   function grantpt(fd:cint):cint;cdecl;external;
   function unlockpt(fd:cint):cint;cdecl;external;
   function ptsname_r(fd:cint; buf:Pchar; buflen: size_t):cint;cdecl;external;

implementation

uses sysutils,dom,xmlwrite,logcfg,tree,logedit,logdupe,logwind,logdom,logstuff;

//logcfg for version number
//tree for date string
//logdupe for mults and score

   constructor scorereport.create;
   begin
      call := '';
      contest := '';
      club := '';
      ops := '';
      mode := '';
      power := '';
      bands := '';
      transmitter := '';
      assisted := '';
      overlay := '';
      breakdown := false;
      enable := false;
      debugenable := false;
      intervalcount := 2*34000;
      icount := (95*intervalcount) div 100; // make first report soon
      url := '';
      user := '';
      password := '';
      tstr := TStringStream.create('');
      domesticmult := 'none';
      dxmult := 'none';
      zonemult := 'none';
      prefixmult := 'none';
   end;

   procedure scorereport.setenable(b: boolean);
   begin
      enable := b;
   end;

   procedure scorereport.setdebugenable(b: boolean);
   begin
      debugenable := b;
   end;

   function scorereport.enabled:boolean;
   begin
      enabled := enable;
   end;

   procedure scorereport.setcall(s: string);
   begin
      call := s;
   end;

   procedure scorereport.setclub(s: string);
   begin
      club := s;
   end;

   procedure scorereport.setcontest(s: string);
   begin
      contest := s;
   end;

   procedure scorereport.setclassops(s: string);
   begin
      ops := s;
   end;

   procedure scorereport.setclassoverlay(s: string);
   begin
      overlay := s;
   end;

   procedure scorereport.setbreakdown(b: boolean);
   begin
      breakdown := b;
   end;

   procedure scorereport.setinterval(i: integer);
   begin
      intervalcount := i*34000;
      if (intervalcount < 34000) then intervalcount := 34000;
      icount := (95*intervalcount) div 100; // make first report soon
   end;

   procedure scorereport.setposturl(s: string);
   begin
      url := s;
   end;

   procedure scorereport.setuser(s: string);
   begin
      user := s;
   end;

   procedure scorereport.setdomesticmult(s: string);
   begin
      domesticmult := s;
   end;

   procedure scorereport.setdxmult(s: string);
   begin
      dxmult := s;
   end;

   procedure scorereport.setzonemult(s: string);
   begin
      zonemult := s;
   end;

   procedure scorereport.setprefixmult(s: string);
   begin
      prefixmult := s;
   end;

   procedure scorereport.setnmulttype (n: INTEGER);
       BEGIN
       nmulttype := n;
       END;

   procedure scorereport.setpassword(s: string);
   begin
      password := s;
   end;

   procedure scorereport.setclassmode(s: string);
   begin
      mode := s;
   end;

   procedure scorereport.setclasspower(s: string);
   begin
      power := s;
   end;

   procedure scorereport.setclassbands(s: string);
   begin
      bands := s;
   end;

   procedure scorereport.setclasstransmitter(s: string);
   begin
      transmitter := s;
   end;

   procedure scorereport.setclassassisted(s: string);
   begin
      assisted := s;
   end;




PROCEDURE scorereport.writexmlmessage;

{ This is the new version of this routine that leverages the ability to
  put a $ in the contest name - which will get filled in based upon the
  ActiveMode.  This is necessary because some contests need to be identified
  by mode for the score reporter - but the MY CONTEST statement does not
  have that information.  }

{ This is where the message is created and shipped off to the score reporting URL  }

VAR Doc: TXMLDocument;
    n0,n1,n2,n3: TDOMNode;
    LocalContest, ScoreString: string;
    ModePosition, i,j: integer;
    c: byte;
    s: AnsiString;
    TempQSOTotals: QSOTotalArray;
    band: bandtype;
    m: modetype;
    QPoints: longint;
    MTotals: MultTotalArrayType;
    totalscore: longint;
    nmult: array[Bandtype,CW..Both,1..4] of integer;
    totalmults: array[1..4] of integer;
    MultTotal: INTEGER;

    BEGIN
    IF contest = '' THEN Exit;  { Not sure why we are here }

    { We will create the local variable LocalContest to use for this XML
      message.  It will be the same as the contest field in the scorereporter
      CLASS - unless there is a $ in the name.  In this case, the $ will be
      relaced with either CW, PH or RTTY depending on ActiveMode. }

    LocalContest := contest;

    { Look for $ and if found - replace it with the mode: CW SSB or RTTY }

    IF StringHas (contest, '$') THEN
        BEGIN
        LocalContest := contest;

        ModePosition := Pos ('$', LocalContest);
        Delete (LocalContest, ModePosition, 1);

        CASE ActiveMode OF
            CW:      Insert ('CW',   LocalContest, ModePosition);
            Digital: Insert ('RTTY', LocalContest, ModePosition);
            Phone:   Insert ('SSB',  LocalContest, ModePosition);
            END;
        END;

    { Make sure the various category fields are sync'd up with the
      Category record in LOGDUPE which is controlled by config commands }

    TransferCategoryInformationIntoScoreReporterData;

    { We will get the total QSO points from LogDupe and then add in any from
      the editable log window - and also QTCs if enabled }

    QPoints := TotalQSOPoints;  { From LogDupe }
    VisibleLog.IncrementQSOPointsWithContentsOfEditableWindow (QPoints);  { Add in editable window }
    IF QTCsEnabled THEN QPoints := QPoints + TotalNumberQTCsProcessed;

    { Same process for QSOs }

    TempQSOTotals := QSOTotals; { From LogDupe }
    VisibleLog.IncrementQSOTotalsWithContentsOfEditableWindow (TempQSOTotals);

    { Now deal with multiplier totals }

    MultTotal := 1;  { Default in case there are no active mults }

    { This is copy exact from the W9CF routine except for the IF statement }

    IF (ActiveDomesticMult <> NoDomesticMults) OR
       (ActiveDXMult       <> NoDXMults) OR
       (ActivePrefixMult   <> NoPrefixMults) OR
       (ActiveZoneMult     <> NoZoneMults) THEN
           BEGIN
           Sheet.MultSheetTotals (MTotals);
           VisibleLog.IncrementMultTotalsWithContentsOfEditableWindow (MTotals);

           FOR j := 1 TO nmulttype DO
               FOR Band := Band160 TO All DO
                   BEGIN
                   FOR m := cw TO both DO
                      BEGIN
                      nmult [band, m, j] := 0;
                      IF domesticmult = mult[j] THEN nmult[band,m,j] :=
                          nmult[band,m,j]+Mtotals[band,m].numberdomesticmults;
                      IF dxmult = mult[j] THEN nmult[band,m,j] :=
                          nmult[band,m,j]+Mtotals[band,m].numberdxmults;
                      IF zonemult = mult[j] THEN nmult[band,m,j] :=
                          nmult[band,m,j]+Mtotals[band,m].numberzonemults;
                      IF prefixmult = mult[j] THEN nmult[band,m,j] :=
                          nmult[band,m,j]+Mtotals[band,m].numberprefixmults;
                      END;
                   END;

           IF SingleBand <> All THEN
               BEGIN
               FOR j:=1 TO nmulttype DO totalmults[j] := nmult[singleband,both,j];
               END
           ELSE
               IF ActiveQSOPointMethod = WAEQSOPointMethod THEN
                   BEGIN
                   FOR j:=1 TO nmulttype DO totalmults[j] := nmult[band80,both,j]*4
                      +nmult[band40,both,j]*3+nmult[band20,both,j]*2
                      +nmult[band15,both,j]*2+nmult[band10,both,j]*2;
                   END
               ELSE
                   FOR j:=1 TO nmulttype DO totalmults[j] := nmult[all,both,j];

           MultTotal := 0;

           FOR j:=1 TO nmulttype DO MultTotal := MultTotal + totalmults[j];
           END;

    TotalScore := QPoints * MultTotal;

    { Now send the message - this is copy exact from W9CF's code I hope }

      try
         i := 0;
         tstr.size := 0;
         Doc := TXMLDocument.Create;

         n0 := Doc.CreateElement('dynamicresults');
         Doc.Appendchild(n0);
         n0 := Doc.DocumentElement;

         n1 := Doc.CreateElement('soft');
         n0.Appendchild(n1);
         n2 := Doc.CreateTextNode('trlinux');
         n1.Appendchild(n2);
         inc(i);

         n1 := Doc.CreateElement('version');
         n0.Appendchild(n1);
         n2 := Doc.CreateTextNode(Version);
         n1.Appendchild(n2);
         inc(i);

         n1 := Doc.CreateElement('contest');
         n0.Appendchild(n1);
         n2 := Doc.CreateTextNode(LocalContest);
         n1.Appendchild(n2);
         inc(i);

         n1 := Doc.CreateElement('call');
         n0.Appendchild(n1);
         n2 := Doc.CreateTextNode (Call);
         n1.Appendchild(n2);
         inc(i);

         n1 := Doc.CreateElement('class');

         TDOMElement(n1).SetAttribute('ops', ops);

         { We add in more attributes if they have been specified }

         if (mode <> '')  THEN TDOMElement(n1).SetAttribute('mode', mode);
         if (power <> '') THEN TDOMElement(n1).SetAttribute('power', power);
         if (bands <> '') THEN TDOMElement(n1).SetAttribute('bands', bands);

         if (transmitter <> '') then
            TDOMElement(n1).SetAttribute('transmitter', transmitter);

         if (assisted <> '') then
            TDOMElement(n1).SetAttribute('assisted', assisted);

          if (overlay <> '') then
            TDOMElement(n1).SetAttribute('overlay', overlay);

         n0.Appendchild(n1);
         inc(i);

         IF club <> '' THEN
             BEGIN
             n1 := Doc.CreateElement('club');
             n0.Appendchild (n1);
             n2 := Doc.CreateTextNode (club);
             n1.Appendchild (n2);
             Inc (i);
             END;

         n1 := Doc.CreateElement ('breakdown');
         n0.Appendchild(n1);

         n2 := Doc.CreateElement('qso');
         TDOMElement(n2).SetAttribute('band', 'total');
         TDOMElement(n2).SetAttribute('mode', 'ALL');
         n3 := Doc.CreateTextNode (IntToStr (TempQSOTotals [All, Both]));
         n2.Appendchild(n3);
         n0.ChildNodes.Item[i].Appendchild(n2);

         IF breakdown THEN
             FOR Band := Band160 TO Band10 DO
                 BEGIN
                 IF tempqsototals [Band, CW] > 0 THEN
                     BEGIN
                     n2 := Doc.CreateElement('qso');
                     TDOMElement(n2).SetAttribute('band',BandString[band]);
                     TDOMElement(n2).SetAttribute('mode','CW');
                     n3 := Doc.CreateTextNode(inttostr(tempQSOTotals[band,CW]));
                     n2.Appendchild(n3);
                     n0.ChildNodes.Item[i].Appendchild(n2);
                     END;

                 IF tempqsototals [Band, Phone] > 0 THEN
                     BEGIN
                     n2 := Doc.CreateElement('qso');
                     TDOMElement(n2).SetAttribute('band',BandString[band]);
                     TDOMElement(n2).SetAttribute('mode','PH');
                     n3 := Doc.CreateTextNode(inttostr(tempQSOTotals[band,Phone]));
                     n2.Appendchild(n3);
                     n0.ChildNodes.Item[i].Appendchild(n2);
                     END;

                 IF tempqsototals [Band, Both] > 0 THEN
                     BEGIN
                     n2 := Doc.CreateElement('qso');
                     TDOMElement(n2).SetAttribute('band',BandString[band]);
                     TDOMElement(n2).SetAttribute('mode','ALL');
                     n3 := Doc.CreateTextNode(inttostr(tempQSOTotals[band,both]));
                     n2.Appendchild(n3);
                     n0.ChildNodes.Item[i].Appendchild(n2);
                     END;

                 END;  { of FOR Band }

         for j := 1 TO nmulttype DO
             BEGIN
             n2 := Doc.CreateElement('mult');
             TDOMElement(n2).SetAttribute('band','total');
             TDOMElement(n2).SetAttribute('mode','ALL');
             TDOMElement(n2).SetAttribute('type',mult[j]);
             n3 := Doc.CreateTextNode(inttostr(totalmults[j]));
             n2.Appendchild(n3);
             n0.ChildNodes.Item[i].Appendchild(n2);

             IF breakdown THEN
                 FOR Band := Band160 to Band10 DO
                     BEGIN
                     IF MultByMode THEN
                         BEGIN
                         IF (nmult [Band, CW, j] > 0) THEN
                             BEGIN
                             n2 := Doc.CreateElement('mult');
                             TDOMElement(n2).SetAttribute('band',BandString[band]);
                             TDOMElement(n2).SetAttribute('mode','CW');
                             TDOMElement(n2).SetAttribute('type',mult[j]);
                             n3 := Doc.CreateTextNode(inttostr(nmult[band,cw,j]));
                             n2.Appendchild(n3);
                             n0.ChildNodes.Item[i].Appendchild(n2);
                             END;

                         IF (nmult [Band, Phone, j] > 0) THEN
                             BEGIN
                             n2 := Doc.CreateElement('mult');
                             TDOMElement(n2).SetAttribute('band',BandString[band]);
                             TDOMElement(n2).SetAttribute('mode','PH');
                             TDOMElement(n2).SetAttribute('type',mult[j]);
                             n3 := Doc.CreateTextNode(inttostr(nmult[band,phone,j]));
                             n2.Appendchild(n3);
                             n0.ChildNodes.Item[i].Appendchild(n2);
                             END;
                         END;

                     { Total mults by band for both modes }

                     IF (nmult [Band, Both, j] > 0) then
                         BEGIN
                         n2 := Doc.CreateElement('mult');
                         TDOMElement(n2).SetAttribute('band',BandString[band]);
                         TDOMElement(n2).SetAttribute('mode','ALL');
                         TDOMElement(n2).SetAttribute('type',mult[j]);
                         n3 := Doc.CreateTextNode(inttostr(nmult[band,both,j]));
                         n2.Appendchild(n3);
                         n0.ChildNodes.Item[i].Appendchild(n2);
                         END;
                     END;
             end;

         n2 := Doc.CreateElement('point');
         TDOMElement(n2).SetAttribute('band','total');
         TDOMElement(n2).SetAttribute('mode','ALL');
         n3 := Doc.CreateTextNode(inttostr(qpoints));
         n2.Appendchild(n3);
         n0.ChildNodes.Item[i].Appendchild(n2);

         n1 := Doc.CreateElement('score');
         n0.Appendchild(n1);
         scorestring := '';
         str(TotalScore,scorestring);
         n2 := Doc.CreateTextNode(scorestring);
         n1.Appendchild(n2);

         n1 := Doc.CreateElement('timestamp');
         n0.Appendchild(n1);
         n2 := Doc.CreateTextNode(GetFullDateString + ' ' + GetFullTimeString);
         n1.Appendchild(n2);

         writeXMLFile(Doc,tstr);

         s := tstr.DataString;

         FOR j := 1 to Length (s) DO
             BEGIN
             c := ord (s [j]);
             fpwrite (fd, c, 1);
             END;

         c := 0;               // tell poster that data is finished
         j := fpwrite(fd, c, 1);

         c := 10;              // newline to force transmission
         j := fpwrite(fd,c,1);
      finally
         Doc.Free; // free memory
      end;
   end;



PROCEDURE scorereport.timer(caughtup: boolean);

{ Give this some oxygen so that the score reports will go out every so often }

    BEGIN
    Inc (icount);
    if (not caughtup) or (icount < intervalcount) then exit;
    icount := 0;
    writexmlmessage;
    END;



procedure scorereport.setup;

{ This is the same as the old setup - except that we are going to determine
  the mulitplier values based upon the contest instead of having the user
  worry about them.  The values are taken from }

    BEGIN
    IF contest = 'ARRL-FIELD-DAY' THEN
        BEGIN
        END

    ELSE IF contest = 'JIDX-$' THEN
        BEGIN
        END

    ELSE IF contest = 'STEW-PERRY' THEN
        BEGIN
        END

    ELSE IF contest = '7QP-QSO-PARTY' THEN
        BEGIN
        END

    ELSE IF contest = 'AA-$' THEN
        BEGIN
        END

    ELSE IF contest = 'ARI-DX' THEN
        BEGIN
        END

    ELSE IF contest = 'ARRL-10' THEN
        BEGIN
        END

    ELSE IF contest = 'ARRL-160' THEN
        BEGIN
        END

    ELSE IF contest = 'ARRL-DX-$' THEN
        BEGIN
        END

    ELSE IF contest = 'ARRL-RTTY' THEN
        BEGIN
        END

    ELSE IF contest = 'ARRL-VHF' THEN
        BEGIN
        END

    ELSE IF contest = 'AP-Sprint-$' THEN
        BEGIN
        END

    ELSE IF contest = 'BALTIC-CONTEST' THEN
        BEGIN
        END

    ELSE IF contest = 'CA-QSO-PARTY' THEN
        BEGIN
        END

    ELSE IF contest = 'CQ-160-$' THEN
        BEGIN
        END

    ELSE IF contest = 'CQ-M' THEN
        BEGIN
        END

    ELSE IF contest = 'CQ-VHF' THEN
        BEGIN
        setbreakdown (true);
        setclassbands ('ALL');
        setprefixmult ('wpxprefix');
        setnmulttype (1);
        END

    ELSE IF contest = 'CQ-WPX-$' THEN
        BEGIN
        setbreakdown (true);
        setclassbands ('ALL');
        setprefixmult ('wpxprefix');
        setnmulttype (2);
        END

    ELSE IF contest = 'CQ-WPX-RTTY' THEN
        BEGIN
        END

    ELSE IF contest = 'CQ-WW-$' THEN
        BEGIN
        setbreakdown (true);
        setclassbands ('ALL');
        setdxmult ('country');
        setzonemult ('zone');
        setnmulttype (2);
        END

    ELSE IF contest = 'CQ-WW-RTTY' THEN
        BEGIN
        END

    ELSE IF contest = '9A-DX' THEN
        BEGIN
        END

    ELSE IF contest = 'CW-Ops' THEN
        BEGIN
        END

    ELSE IF contest = 'CWOPS-Open' THEN
        BEGIN
        END

    ELSE IF contest = 'EUHFC' THEN
        BEGIN
        END

    ELSE IF contest = 'FL-QSO-PARTY' THEN
        BEGIN
        END

    ELSE IF contest = 'HA-DX' THEN
        BEGIN
        END

    ELSE IF contest = 'HELVETIA' THEN
        BEGIN
        END

    ELSE IF contest = 'IARU-HF' THEN
        BEGIN
        END

    ELSE IF contest = 'RSGB-IOTA' THEN
        BEGIN
        END

    ELSE IF contest = 'JARTS-WW-RTTY' THEN
        BEGIN
        END

    ELSE IF contest = 'KCJ' THEN
        BEGIN
        END

    ELSE IF contest = 'KVP' THEN
        BEGIN
        END

    ELSE IF contest = 'MI-QSO-PARTY' THEN
        BEGIN
        END

    ELSE IF contest = 'MARCONIMEMORIAL' THEN
        BEGIN
        END

    ELSE IF contest = 'MN-QSO-PARTY' THEN
        BEGIN
        END

    ELSE IF contest = 'ICWC-MST' THEN
        BEGIN
        END

    ELSE IF contest = 'NAQP-$' THEN
        BEGIN
        END

    ELSE IF contest = 'NEWE-QSO-PARTY' THEN
        BEGIN
        END

    ELSE IF contest = 'NY-QSO-PARTY' THEN
        BEGIN
        END

    ELSE IF contest = 'NRAU-$' THEN
        BEGIN
        END

    ELSE IF contest = 'OH-QSO-PARTY' THEN
        BEGIN
        END

    ELSE IF contest = 'OK-OM-DX' THEN
        BEGIN
        END

    ELSE IF contest = 'PACC' THEN
        BEGIN
        END

    ELSE IF contest = 'RAC' THEN
        BEGIN
        END

    ELSE IF contest = 'RDXC' THEN
        BEGIN
        END

    ELSE IF contest = 'WA-QSO-PARTY' THEN
        BEGIN
        END

    ELSE IF contest = 'SAC-$' THEN
        BEGIN
        END

    ELSE IF contest = 'SP-DX' THEN
        BEGIN
        END

    ELSE IF contest = 'NA-SPRINT-$' THEN
        BEGIN
        END

    ELSE IF contest = 'K1USNSST' THEN
        BEGIN
        END

    ELSE IF contest = 'ARRL-SS-$' THEN
        BEGIN
        END

    ELSE IF contest = 'TX-QSO-PARTY' THEN
        BEGIN
        END

    ELSE IF contest = 'UBA-DX' THEN
        BEGIN
        END

    ELSE IF contest = 'UKRAINIAN-DX' THEN
        BEGIN
        END

    ELSE IF contest = 'Oceania-DX-$' THEN
        BEGIN
        END

    ELSE IF contest = 'WAG' THEN
        BEGIN
        END

    ELSE IF contest = 'DARC-WAEDC-$' THEN
        BEGIN
        END

    ELSE IF contest = 'WI-QSO-PARTY' THEN
        BEGIN
        END

    ELSE IF contest = 'YO-DX-HF' THEN
        BEGIN
        END

    ELSE
        BEGIN
        WriteLn ('I do not recognize ', contest, ' in the scorereporter init procedure.');
        WaitForKeyPressed;
        Halt;
        END;

    { And now for the stuff from the old routine }

    fd := getpt;
    grantpt(fd);
    unlockpt(fd);
    setlength(slave,200);
    ptsname_r(fd,@slave[1],200);
    setlength(slave,strlen(pchar(@slave[0])));
    postfork;
    END;



   procedure scorereport.postfork;
   var fdslave: longint;
   begin
      pid := fpfork;
      if (pid = 0) then
      begin
         diewithparent;
         fdslave := fpopen(slave,O_RDWR);
         fpclose(0);
         fpdup(fdslave);
         fpclose(1);
         fpclose(2);
//
// The scoreposter process acts like a standalone application. Therefore
// with minor changes an fpExeclp call could be used as in communication.pas
// and would clear all memory for this child process.
// This would have the advantage of using almost no virtual memory, but
// the disadvantage of having to make sure the scoreposter executable is
// somewhere in the $PATH.
//
         initscoreposter(pchar(url),pchar(user + ':' + password)
            ,ord(debugenable));
         poster;
      end;
   end;

   procedure scorereport.refork;
   var status:cint;
   begin
      if (pid <> 0) then
      begin
         if (fpwaitpid(pid,status,WNOHANG) = pid) then postfork;
      end;
   end;



   BEGIN
   END.
