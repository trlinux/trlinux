unit datetimec;
{$L dt}
{$linklib c}
interface
   procedure gettime(var hour:word ; var min:word; var sec:word;
      var sec100:word);
   procedure getdate(var year:word; var month:word; var day:word;
      var wday:word);
   procedure setdate(year:word; month:word; day:word);
   procedure settime(hour:word; min:word; sec:word; sec100:word);

implementation
   procedure gettimec(hour:pointer; min:pointer; sec:pointer;
      sec100:pointer );cdecl;external;
   procedure getdatec(year:pointer; month:pointer; mday:pointer; wday:pointer);
      cdecl;external;
   procedure setdatec(year:word; month:word; day:word);cdecl;external;
   procedure settimec(hour:word; min:word; sec:word; sec100:word);
      cdecl;external;

   procedure gettime(var hour:word ; var min:word; var sec:word;
      var sec100:word);
   begin
      gettimec(addr(hour),addr(min),addr(sec),addr(sec100));
   end;

   procedure getdate(var year:word; var month:word; var day:word;
      var wday:word);
   begin
      getdatec(addr(year),addr(month),addr(day),addr(wday));
   end;

   procedure setdate(year:word; month:word; day:word);
   begin
      setdatec(year,month,day);
   end;

   procedure settime(hour:word; min:word; sec:word; sec100:word);
   begin
     settimec(hour,min,sec,sec100);
   end;
end.
