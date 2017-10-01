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
      i,j: integer;
      c: byte;
      s: ansistring;
      tempqsototals: QSOTotalArray;
      band: bandtype;
      m: modetype;
      qpoints: longint;
      MTotals: MultTotalArrayType;
      totalscore: longint;
      nmult: array[Bandtype,CW..Both,1..4] of integer;
      totalmults: array[1..4] of integer;
      mtot: integer;

   begin
      QPoints := TotalQSOPoints;
      VisibleLog.IncrementQSOPointsWithContentsOfEditableWindow (QPoints);
      IF QTCsEnabled THEN QPoints := QPoints + TotalNumberQTCsProcessed;
      tempqsototals := QSOTotals;
      visiblelog.incrementqsototalswithcontentsofeditablewindow(tempqsototals);
      IF ((ActiveDomesticMult = NoDomesticMults) AND
         (ActiveDXMult       = NoDXMults) AND
         (ActivePrefixMult   = NoPrefixMults) AND
         (ActiveZoneMult     = NoZoneMults)) or
         (ActiveExchange = RSTQTHNameAndFistsNumberOrPowerExchange) THEN
      BEGIN
         mtot := 1;
      END
      ELSE
      BEGIN
         Sheet.MultSheetTotals (MTotals);
         VisibleLog.IncrementMultTotalsWithContentsOfEditableWindow (MTotals);
         for j := 1 to nmulttype do
         begin
            for band := band160 to all do
            begin
               for m := cw to both do
               begin
                  nmult[band,m,j] := 0;
                  if domesticmult = mult[j] then nmult[band,m,j] := 
                     nmult[band,m,j]+Mtotals[band,m].numberdomesticmults;
                  if dxmult = mult[j] then nmult[band,m,j] :=
                     nmult[band,m,j]+Mtotals[band,m].numberdxmults;
                  if zonemult = mult[j] then nmult[band,m,j] :=
                     nmult[band,m,j]+Mtotals[band,m].numberzonemults;
                  if prefixmult = mult[j] then nmult[band,m,j] :=
                     nmult[band,m,j]+Mtotals[band,m].numberprefixmults;
               end;
            end;
         end;
         IF SingleBand <> All THEN
         BEGIN
           for j:=1 to nmulttype do totalmults[j] := nmult[singleband,both,j];
         END
         ELSE IF ActiveQSOPointMethod = WAEQSOPointMethod THEN
         BEGIN
           for j:=1 to nmulttype do totalmults[j] := nmult[band80,both,j]*4
               +nmult[band40,both,j]*3+nmult[band20,both,j]*2
               +nmult[band15,both,j]*2+nmult[band10,both,j]*2;
         END
         ELSE
         BEGIN
           for j:=1 to nmulttype do totalmults[j] := nmult[all,both,j];
         END;
         mtot := 0;
         for j:=1 to nmulttype do mtot := mtot + totalmults[j];
      END;
      totalscore := qpoints*mtot;
      
{
for band:=band160 to all do
begin
   for m:=cw to both do
   begin
      for j:=1 to nmulttype do
      begin
       writeln(stderr,j,' ',bandstring[band],modestring[m],' ',nmult[band,m,j]);
      end;
   end;
end;
}
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
         n3 := Doc.CreateTextNode(inttostr(tempQSOTotals[All,Both]));
         n2.Appendchild(n3);
         n0.ChildNodes.Item[i].Appendchild(n2);

         if breakdown then
         begin
            for band := Band160 to Band10 do
            begin
               if (tempqsototals[band,cw] > 0) then
               begin
                  n2 := Doc.CreateElement('qso');
                  TDOMElement(n2).SetAttribute('band',BandString[band]);
                  TDOMElement(n2).SetAttribute('mode','CW');
                  n3 := Doc.CreateTextNode(inttostr(tempQSOTotals[band,CW]));
                  n2.Appendchild(n3);
                  n0.ChildNodes.Item[i].Appendchild(n2);
               end;
               if (tempqsototals[band,phone] > 0) then
               begin
                  n2 := Doc.CreateElement('qso');
                  TDOMElement(n2).SetAttribute('band',BandString[band]);
                  TDOMElement(n2).SetAttribute('mode','PH');
                  n3 := Doc.CreateTextNode(inttostr(tempQSOTotals[band,Phone]));
                  n2.Appendchild(n3);
                  n0.ChildNodes.Item[i].Appendchild(n2);
               end;
               if (tempqsototals[band,both] > 0) then
               begin
                  n2 := Doc.CreateElement('qso');
                  TDOMElement(n2).SetAttribute('band',BandString[band]);
                  TDOMElement(n2).SetAttribute('mode','ALL');
                  n3 := Doc.CreateTextNode(inttostr(tempQSOTotals[band,both]));
                  n2.Appendchild(n3);
                  n0.ChildNodes.Item[i].Appendchild(n2);
               end;
            end;
         end;

         for j:=1 to nmulttype do
         begin
            n2 := Doc.CreateElement('mult');
            TDOMElement(n2).SetAttribute('band','total');
            TDOMElement(n2).SetAttribute('mode','ALL');
            TDOMElement(n2).SetAttribute('type',mult[j]);
            n3 := Doc.CreateTextNode(inttostr(totalmults[j]));
            n2.Appendchild(n3);
            n0.ChildNodes.Item[i].Appendchild(n2);
            if breakdown then
            begin
               for band := Band160 to Band10 do
               begin
                  if multbymode then
                  begin
                     if (nmult[band,cw,j] > 0) then
                     begin
                        n2 := Doc.CreateElement('mult');
                        TDOMElement(n2).SetAttribute('band',BandString[band]);
                        TDOMElement(n2).SetAttribute('mode','CW');
                        TDOMElement(n2).SetAttribute('type',mult[j]);
                        n3 := Doc.CreateTextNode(inttostr(nmult[band,cw,j]));
                        n2.Appendchild(n3);
                        n0.ChildNodes.Item[i].Appendchild(n2);
                     end;
                     if (nmult[band,phone,j] > 0) then
                     begin
                        n2 := Doc.CreateElement('mult');
                        TDOMElement(n2).SetAttribute('band',BandString[band]);
                        TDOMElement(n2).SetAttribute('mode','PH');
                        TDOMElement(n2).SetAttribute('type',mult[j]);
                        n3 := Doc.CreateTextNode(inttostr(nmult[band,phone,j]));
                        n2.Appendchild(n3);
                        n0.ChildNodes.Item[i].Appendchild(n2);
                     end;
                  end;
                  if (nmult[band,both,j] > 0) then
                  begin
                     n2 := Doc.CreateElement('mult');
                     TDOMElement(n2).SetAttribute('band',BandString[band]);
                     TDOMElement(n2).SetAttribute('mode','ALL');
                     TDOMElement(n2).SetAttribute('type',mult[j]);
                     n3 := Doc.CreateTextNode(inttostr(nmult[band,both,j]));
                     n2.Appendchild(n3);
                     n0.ChildNodes.Item[i].Appendchild(n2);
                  end;
               end;
            end;
         end;

         n2 := Doc.CreateElement('point');
         TDOMElement(n2).SetAttribute('band','total');
         TDOMElement(n2).SetAttribute('mode','ALL');
         n3 := Doc.CreateTextNode(inttostr(qpoints));
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
         for j := 1 to length(s) do
         begin
            c := ord(s[j]);
            fpwrite(fd,c,1);
         end;
         c := 0; // tell poster that data is finished
         j := fpwrite(fd,c,1);
         c := 10; // newline to force transmission
         j := fpwrite(fd,c,1);
      finally
         Doc.Free; // free memory
      end;
   end;
         
   procedure scorereport.timer(caughtup: boolean);
   begin
      inc(icount);
      if (not caughtup) or (icount < intervalcount) then exit;
      icount := 0;
      writexmlmessage;
   end;

   procedure scorereport.setup;
   var i:integer;
   begin
      nmulttype := 0;
      if ActiveDomesticMult = NoDomesticMults then domesticmult := 'none';
      if ActiveDXMult = NoDXMults then dxmult := 'none';
      if ActivePrefixMult = NoPrefixMults then prefixmult := 'none';
      if ActiveZoneMult = NoZoneMults then zonemult := 'none';

      if (UPCASE(domesticmult) <> 'NONE') then
      begin
         inc(nmulttype);
         mult[nmulttype] := domesticmult;
      end;
      if ((UPCASE(dxmult) <> 'NONE') and (dxmult <> domesticmult)) then
      begin
         inc(nmulttype);
         mult[nmulttype] := dxmult;
      end;
      if ((UPCASE(zonemult) <> 'NONE') and (zonemult <> domesticmult)
          and (zonemult <> dxmult)) then
      begin
         inc(nmulttype);
         mult[nmulttype] := zonemult;
      end;
      if ((UPCASE(prefixmult) <> 'NONE') and (prefixmult <> domesticmult)
          and (prefixmult <> dxmult) and (prefixmult <> zonemult)) then
      begin
         inc(nmulttype);
         mult[nmulttype] := prefixmult;
      end;
      
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

end.
