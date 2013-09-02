unit xkb;
{$mode objfpc}  

interface

    procedure setupkeyboard;cdecl;
    function ctrlshift:boolean;cdecl;
    function ctrl:boolean;cdecl;
    function ctrlenter:boolean;cdecl;
    function ritshift:integer;cdecl;
    procedure shiftchange(ishift: integer);cdecl;

implementation

    procedure setupkeyboard;cdecl;external;
    function ctrlshift:boolean;cdecl;external;
    function ctrl:boolean;cdecl;external;
    function ctrlenter:boolean;cdecl;external;
    function ritshift:integer;cdecl;external;
    procedure shiftchange(ishift: integer);cdecl;external;
    
end.
