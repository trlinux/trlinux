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

unit keycode;
{$V-}

INTERFACE


CONST

{ Key code definitions }

    NullCharacter  = Chr (0);
    NullKey        = Chr (0);
    Beep           = Chr (7);
    BackSpace      = Chr (8);
    TabKey         = Chr (9);
    CarriageReturn = Chr ($D);        { We use this extended too for ControlEnter }
    LineFeed       = Chr ($A);
    EscapeKey      = Chr ($1B);
    SpaceBar       = ' ';

    ShiftTab       = Chr (15);        { Extended }

    { Normal extended Keys }

    F1 = Chr (59);        { Function key codes }
    F2 = Chr (60);
    F3 = Chr (61);
    F4 = Chr (62);
    F5 = Chr (63);
    F6 = Chr (64);
    F7 = Chr (65);
    F8 = Chr (66);
    F9 = Chr (67);
    F10 = Chr (68);
    F11 = Chr ($85);
    F12 = Chr ($86);

    ShiftF1 = Chr (84);
    ShiftF2 = Chr (85);
    ShiftF3 = Chr (86);
    ShiftF4 = Chr (87);
    ShiftF5 = Chr (88);
    ShiftF6 = Chr (89);
    ShiftF7 = Chr (90);
    ShiftF8 = Chr (91);
    ShiftF9 = Chr (92);
    ShiftF10 = Chr (93);
    ShiftF11 = Chr ($87);
    ShiftF12 = Chr ($88);

    ControlF1 = Chr (94);
    ControlF2 = Chr (95);
    ControlF3 = Chr (96);
    ControlF4 = Chr (97);
    ControlF5 = Chr (98);
    ControlF6 = Chr (99);
    ControlF7 = Chr (100);
    ControlF8 = Chr (101);
    ControlF9 = Chr (102);
    ControlF10 = Chr (103);
    ControlF11 = Chr ($89);
    ControlF12 = Chr ($8A);

    AltF1 = Chr (104);
    AltF2 = Chr (105);
    AltF3 = Chr (106);
    AltF4 = Chr (107);
    AltF5 = Chr (108);
    AltF6 = Chr (109);
    AltF7 = Chr (110);
    AltF8 = Chr (111);
    AltF9 = Chr (112);
    AltF10 = Chr (113);
    AltF11 = Chr ($8B);
    AltF12 = Chr ($8C);

    AltQ = Chr (16);
    AltW = Chr (17);
    AltE = Chr (18);
    AltR = Chr (19);
    AltT = Chr (20);
    AltY = Chr (21);
    AltU = Chr (22);
    AltI = Chr (23);
    AltO = Chr (24);
    AltP = Chr (25);
    AltA = Chr (30);
    AltS = Chr (31);
    AltD = Chr (32);
    AltF = Chr (33);
    AltG = Chr (34);
    AltH = Chr (35);
    AltJ = Chr (36);
    AltK = Chr (37);
    AltL = Chr (38);
    AltZ = Chr (44);
    AltX = Chr (45);
    AltC = Chr (46);
    AltV = Chr (47);
    AltB = Chr (48);
    AltN = Chr (49);
    AltM = Chr (50);

    Alt1 = Chr (120);
    Alt2 = Chr (121);
    Alt3 = Chr (122);
    Alt4 = Chr (123);
    Alt5 = Chr (124);
    Alt6 = Chr (125);
    Alt7 = Chr (126);
    Alt8 = Chr (127);
    Alt9 = Chr (128);
    Alt0 = Chr (129);
    AltEqual = Chr (131);
    AltDash  = Chr (130);

    { These are extended }

    HomeKey     = Chr (71);
    UpArrow     = Chr (72);
    PageUpKey   = Chr (73);
    LeftArrow   = Chr (75);
    RightArrow  = Chr (77);
    EndKey      = Chr (79);
    DownArrow   = Chr (80);
    PageDownKey = Chr (81);
    InsertKey   = Chr (82);
    DeleteKey   = Chr (83);

    { KK1L: 6.65 Added following eight definitions }

    AltInsert        = Chr (162);
    AltDelete        = Chr (163);
    ControlInsert    = Chr (146);
    ControlDelete    = Chr (147);
    AltDownArrow     = Chr (160);
    AltUpArrow       = Chr (152);
    ControlDownArrow = Chr (145);
    ControlUpArrow   = Chr (141);

    { KK1L: 6.72 Added following set constant for Scandinavian letters }

    AccentedChars: set of char = [Chr(132), Chr(142),  {A umlaut  }
                                  Chr(134), Chr(143),  {A dot     }
                                  Chr(148), Chr(153)]; {O umlaut  }

    { These are all extended keys }

    ControlLeftArrow  = Chr (115);
    ControlRightArrow = Chr (116);
    ControlEnd        = Chr (117);
    ControlPageDown   = Chr (118);
    ControlHome       = Chr (119);
    ControlPageUp     = Chr (132);

    { Not extended }

    ControlA = Chr (1);
    ControlB = Chr (2);
    ControlC = Chr (3);
    ControlD = Chr (4);
    ControlE = Chr (5);
    ControlF = Chr (6);
    ControlG = Chr (7);
    ControlH = Chr (8);
    ControlI = Chr (9);
    ControlJ = Chr (10);
    ControlK = Chr (11);
    ControlL = Chr (12);
    ControlM = Chr (13);   { Same as a carriage return }
    ControlN = Chr (14);
    ControlO = Chr (15);
    ControlP = Chr (16);
    ControlQ = Chr (17);
    ControlR = Chr (18);
    ControlS = Chr (19);
    ControlT = Chr (20);
    ControlU = Chr (21);
    ControlV = Chr (22);
    ControlW = Chr (23);
    ControlX = Chr (24);
    ControlY = Chr (25);
    ControlZ = Chr (26);

    { Not extended }

    ControlLeftBracket  = Chr (27);
    ControlBackSlash    = Chr (28);
    ControlRightBracket = Chr (29);
    ControlDash         = Chr (31);

implementation
end.
