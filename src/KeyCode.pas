PROGRAM KeyCode;

Uses KeyCode, CRT, SlowTree, Tree;

VAR Key: CHAR;

    BEGIN
    REPEAT
        REPEAT UNTIL KeyPressed;

        Key := ReadKey;

        IF Key = EscapeKey THEN Exit;

        IF Key = NullKey THEN
            Key := ReadKey;

            WriteLn ('Extended key value = ', Integer (Key));
            END
        ELSE
            WriteLn ('Normal key value = ', Integer (Key));
    UNTIL False;
    END.