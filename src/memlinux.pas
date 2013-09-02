unit memlinux;

interface
function memavail: longint;
function maxavail: longint;

implementation
function memavail: longint;
begin
   memavail := high(longint);
end;

function maxavail: longint;
begin
   maxavail := high(longint);
end;
end.
