{$mode objfpc}
unit scorereporter;
{$l scoreposter.o}

interface
uses classes,baseunix,unix,ctypes;

type
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
         intervalcount: integer;
         icount: integer;
         url: ansistring;
         user: ansistring;
         password: ansistring;
         tstr: TStringStream;
         fd: longint;
         slave: string;
         pid: TPid;
         function totalscore:integer;
         procedure postfork;
      public
         constructor create;
         procedure setenable(b: boolean);
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
         procedure writexmlmessage;
         procedure timer(caughtup: boolean);
         function enabled:boolean;
         procedure refork;
         procedure setup;
   end;

   procedure diewithparent;cdecl;external;
   function initscoreposter(url: pchar; user:pchar):integer;cdecl;external;
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
//logdupe for mults and score?

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
      intervalcount := 2*34000;
      icount := 0;
      url := '';
      user := '';
      password := '';
      tstr := TStringStream.create('');
   end;

   procedure scorereport.setenable(b: boolean);
   begin
      enable := b;
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
   end;

   procedure scorereport.setposturl(s: string);
   begin
      url := s;
   end;

   procedure scorereport.setuser(s: string);
   begin
      user := s;
   end;

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

   procedure scorereport.writexmlmessage;
   var Doc: TXMLDocument;
      n0,n1,n2,n3: TDOMNode;
      scorestr: string;
      i: integer;
      c: byte;
      s: ansistring;
   begin
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
         n2 := Doc.CreateTextNode(contest);
         n1.Appendchild(n2);
         inc(i);

         n1 := Doc.CreateElement('call');
         n0.Appendchild(n1);
         n2 := Doc.CreateTextNode(call);
         n1.Appendchild(n2);
         inc(i);

         n1 := Doc.CreateElement('class');
         TDOMElement(n1).SetAttribute('ops',ops);
         if (mode <> '') then TDOMElement(n1).SetAttribute('mode',mode);
         if (power <> '') then TDOMElement(n1).SetAttribute('power',power);
         if (bands <> '') then TDOMElement(n1).SetAttribute('bands',bands);
         if (transmitter <> '') then
            TDOMElement(n1).SetAttribute('transmitter',transmitter);
         if (assisted <> '') then
            TDOMElement(n1).SetAttribute('assisted',assisted);
         if (overlay <> '') then
            TDOMElement(n1).SetAttribute('overlay',overlay);
         n0.Appendchild(n1);
         inc(i);
         
         if (club <> '') then
         begin
            n1 := Doc.CreateElement('club');
            n0.Appendchild(n1);
            n2 := Doc.CreateTextNode(club);
            n1.Appendchild(n2);
            inc(i);
         end;

         n1 := Doc.CreateElement('breakdown');
         n0.Appendchild(n1);

         n2 := Doc.CreateElement('qso');
         TDOMElement(n2).SetAttribute('band','total');
         TDOMElement(n2).SetAttribute('mode','ALL');
         n3 := Doc.CreateTextNode('137');
         n2.Appendchild(n3);
         n0.ChildNodes.Item[i].Appendchild(n2);

         n2 := Doc.CreateElement('mult');
         TDOMElement(n2).SetAttribute('band','total');
         TDOMElement(n2).SetAttribute('mode','ALL');
         TDOMElement(n2).SetAttribute('type','state');
         n3 := Doc.CreateTextNode('17');
         n2.Appendchild(n3);
         n0.ChildNodes.Item[i].Appendchild(n2);

         n2 := Doc.CreateElement('point');
         TDOMElement(n2).SetAttribute('band','total');
         TDOMElement(n2).SetAttribute('mode','ALL');
         n3 := Doc.CreateTextNode('137');
         n2.Appendchild(n3);
         n0.ChildNodes.Item[i].Appendchild(n2);
 
         n1 := Doc.CreateElement('score');
         n0.Appendchild(n1);
         scorestr := '';
         str(TotalScore,scorestr);
         n2 := Doc.CreateTextNode(scorestr);
         n1.Appendchild(n2);

         n1 := Doc.CreateElement('timestamp');
         n0.Appendchild(n1);
         n2 := Doc.CreateTextNode(GetFullDateString + ' ' + GetFullTimeString);
         n1.Appendchild(n2);

         writeXMLFile(Doc,tstr);

         s := tstr.DataString;
         for i := 1 to length(s) do
         begin
            c := ord(s[i]);
            fpwrite(fd,c,1);
         end;
         c := 0;
         fpwrite(fd,c,1);
      finally
         Doc.Free; // free memory
      end;
   end;
         
FUNCTION scorereport.TotalScore: LONGINT;

{ This routine will return the current contest score }

VAR QPoints, TotalMults: LongInt;
    MTotals:    MultTotalArrayType;

    BEGIN
    QPoints := TotalQSOPoints;

    VisibleLog.IncrementQSOPointsWithContentsOfEditableWindow (QPoints);

    IF QTCsEnabled THEN QPoints := QPoints + TotalNumberQTCsProcessed;

    IF (ActiveDomesticMult = NoDomesticMults) AND
       (ActiveDXMult       = NoDXMults) AND
       (ActivePrefixMult   = NoPrefixMults) AND
       (ActiveZoneMult     = NoZoneMults) THEN
           BEGIN
           TotalScore := QPoints;
           Exit;
           END;

    {KK1L: 6.70 Ugly fix for FISTS because mults don't work...too long an exchange}
    IF ActiveExchange = RSTQTHNameAndFistsNumberOrPowerExchange THEN
        BEGIN
        TotalScore := QPoints;
        Exit;
        END;

    Sheet.MultSheetTotals (MTotals);
    VisibleLog.IncrementMultTotalsWithContentsOfEditableWindow (MTotals);

    IF SingleBand <> All THEN
        BEGIN
        TotalMults := MTotals [SingleBand, Both].NumberDomesticMults;
        TotalMults := TotalMults + MTotals [SingleBand, Both].NumberDXMults;
        TotalMults := TotalMults + MTotals [SingleBand, Both].NumberPrefixMults;
        TotalMults := TotalMults + MTotals [SingleBand, Both].NumberZoneMults;
        END
    ELSE
        IF ActiveQSOPointMethod = WAEQSOPointMethod THEN
            BEGIN
            TotalMults := MTotals [Band80, Both].NumberDXMults * 4;
            TotalMults := TotalMults + MTotals [Band40, Both].NumberDXMults * 3;
            TotalMults := TotalMults + MTotals [Band20, Both].NumberDXMults * 2;
            TotalMults := TotalMults + MTotals [Band15, Both].NumberDXMults * 2;
            TotalMults := TotalMults + MTotals [Band10, Both].NumberDXMults * 2;
            END
        ELSE
            BEGIN
            TotalMults := MTotals [All, Both].NumberDomesticMults;
            TotalMults := TotalMults + MTotals [All, Both].NumberDXMults;
            TotalMults := TotalMults + MTotals [All, Both].NumberPrefixMults;
            TotalMults := TotalMults + MTotals [All, Both].NumberZoneMults;
            END;

    TotalScore := QPoints * TotalMults;
    END;

   procedure scorereport.timer(caughtup: boolean);
   begin
      inc(icount);
      if (not caughtup) or (icount < intervalcount) then exit;
      icount := 0;
      writexmlmessage;
   end;

   procedure scorereport.setup;
   begin
      fd := getpt;
      grantpt(fd);
      unlockpt(fd);
      setlength(slave,200);
      ptsname_r(fd,@slave[1],200);
      setlength(slave,strlen(pchar(@slave[0])));
      postfork;
   end;

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
         initscoreposter(pchar(url),pchar(user + ':' + password));
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

end.
