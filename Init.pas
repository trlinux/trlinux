PROCEDURE Initialize2BSIQOperatorInterface;

    BEGIN
    RemoveWindow (CallWindow);

    SaveSetandClearActiveWindow (TBSIQ_LeftCallWindow);
    SaveSetAndClearActiveWindow (TBSIQ_RightCallWindow);
    SaveSetandClearActiveWindow (TBSIQ_LeftExchangeWindow);
    SaveSetAndClearActiveWindow (TBSIQ_RightExchangeWindow);

    WITH Radio1QSOMachine DO
        BEGIN
        TBSIQ_ActiveWindow := TBSIQ_NoWindow;

        CallWindowString := '';
        CallWindowCursor := 1;
        ExchangeWindowString := '';
        ExchangeWindowCursor := 1;

        CallWindowLX := TBSIQ_LeftCallWindowLX;
        CallWindowLY := TBSIQ_LeftCallWindowLY;
        CallWindowRX := TBSIQ_LeftCallWindowRX;
        CallWindowRY := TBSIQ_LeftCallWindowRY;

        CallWindowColor := ColorColors.TBSIQ_LeftCallWindowColor;
        CallWindowBackground := ColorColors.TBSIQ_LeftCallWindowBackground;

        ExchangeWindowLX := TBSIQ_LeftExchangeWindowLX;
        ExchangeWindowLY := TBSIQ_LeftExchangeWindowLY;
        ExchangeWindowRX := TBSIQ_LeftExchangeWindowRX;
        ExchangeWindowRY := TBSIQ_LeftExchangeWindowRY;

        ExchangeWindowColor := ColorColors.TBSIQ_LeftExchangeWindowColor;
        ExchangeWindowBackground := ColorColors.TBSIQ_LeftExchangeWindowBackground;
        END;

    WITH Radio2QSOMachine DO
        BEGIN
        TBSIQ_ActiveWindow := TBSIQ_NoWindow;

        CallWindowString := '';
        ExchangeWindowString := '';

        CallWindowLX := TBSIQ_RightCallWindowLX;
        CallWindowLY := TBSIQ_RightCallWindowLY;
        CallWindowRX := TBSIQ_RightCallWindowRX;
        CallWindowRY := TBSIQ_RightCallWindowRY;

        CallWindowColor := ColorColors.TBSIQ_RightCallWindowColor;
        CallWindowBackground := ColorColors.TBSIQ_RightCallWindowBackground;

        ExchangeWindowLX := TBSIQ_RightExchangeWindowLX;
        ExchangeWindowLY := TBSIQ_RightExchangeWindowLY;
        ExchangeWindowRX := TBSIQ_RightExchangeWindowRX;
        ExchangeWindowRY := TBSIQ_RightExchangeWindowRY;

        ExchangeWindowColor := ColorColors.TBSIQ_RightExchangeWindowColor;
        ExchangeWindowBackground := ColorColors.TBSIQ_RightExchangeWindowBackground;
        END;

    END;



