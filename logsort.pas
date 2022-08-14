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

UNIT LogSort;

{

    Heapsort discovered/invented by J. W. J. Williams and R. W. Floyd

    Heapsort has two phases.  First, the data is rearranged to form a heap.
    Second, the heap is rearranged to form a sorted array.  All this is done
    in-place on fixed-length records.

    A "heap" is a partially reverse-sorted binary tree.

    One trick used by heapsort is to embed the binary tree into an array.
    This way, no pointers are needed in the records to get from any node to
    its children.  Instead, the address (index) of a child node is computed
    from the index of its parent.

    In the picture below, the numbers 1-9 A-F are node labels which refer to
    the nodes' locations.  They are NOT the keys or data which are stored in
    the nodes.  (Note that the array is 1-based, not 0-based.)

	       1
	     /   \
	    2     3
	   /|     |\
	  4 5     6 7
	 /| |\   /| |\
	8 9 A B C D E F

    For any node at index J, its children are at 2J and 2J+1.

    To be a heap, the sort key stored at each node must be greater than or
    equal to its children.  By recursion, this implies greater-or-equal
    compared to its entire sub-tree.  I.e., the greatest node is at the top
    of the heap.

    To start the heap-creation phase, the "leaf" nodes (8-F) are compared to
    their parent nodes (4-7).  If the parent node is less than either of its
    children, it is swapped with the greater child.

    Next, nodes 4-7 are compared with their parent nodes 2-3.  If either 4
    or 5 is greater than node 2, then the greater of 4 or 5 is swapped with
    2.  Similarly with nodes 6 and 7 versus node 3.  This time, however, the
    swap may affect lower levels.  E.g., if 2 is swapped with 4, the new 4
    needs to be compared with 8 and 9, and swapped with the greater of 8 and
    9 if necessary.  This process is called "sift up" and, in general, may
    need to be applied all the way down to the leaf level.

    Finally, 2 and 3 are compared with node 1, and, if necessary, either 2
    or 3 is swapped with 1 and sifted up.  This completes the heap-creation
    phase of the sort.

    In the second phase, the heap is unravelled from the bottom up, and
    records popped from the top of the heap are stored at the bottom in
    their final positions.  The heap is re-sifted after each pop to make
    sure the greatest remaining key is in node 1.

    To start, node 1 is swapped with node F.  The record in node F is now in
    its final position, and node F is declared out of the heap.  The new
    record in node 1 must now be sifted.  I.e., its key compared against
    nodes 2 and 3, and swapped if necessary with the greater of 2 and 3, and
    so on down to the end of the heap.  HOWEVER, the sift must stop at the
    end of the heap so that nodes declared outside the heap are unaffected.

    After the first sift, the key in node 1 is again the greatest key in the
    *remaining* heap.  (It is the second-greatest key overall.)  Node 1 is
    then swapped with node E, nodes E and F are declared out of the heap,
    and node 1 is re-sifted within the newly reduced heap.

    This process is repeated until only one node is left in the heap, at
    which point the sort is completed.

    Note that the number of records to be sorted is usually not a power-of-2
    minus 1.  Here's an example with 12 records where node 6 has only one
    child and node 7 has none.

	       1
	     /   \
	    2     3
	   /|     |\
	  4 5     6 7
	 /| |\   /| |\
	8 9 A B C

    This makes node 7 a leaf node.  (It has no children.)  It is possible to
    perform the first set of compares with leaf nodes 7-C against their
    parents 3-6.  If the sift routine is sufficiently general, it can just
    be applied starting at node 6 and ending at node 1.  In this case, it is
    not necessary to distinguish "layers" between the first set of
    comparisons and subsequent sifts.

    Note the description above is stated in terms of 1-based arrays while
    the code below uses 0-based arrays according to Turbo Pascal's "open
    array" argument convention.
}

INTERFACE

{$P+}

Uses Tree;

TYPE
    EntryPointerListType = ARRAY [0..15000] OF POINTER;
    EntryPointerListPtr = ^EntryPointerListType;


PROCEDURE HeapSort (VAR PointerArray: EntryPointerListPtr;
		    NumberOfRecords: LONGINT);


IMPLEMENTATION
uses keycode;

FUNCTION Compare (x, y: POINTER): INTEGER;

{ Returns -1, 0, or +1 as the first argument is less than, equal to,
  or greater than the second argument. }

TYPE Entry = ARRAY [0..11] OF CHAR;
     EntryPointer = ^Entry;

VAR XCall, YCall: EntryPointer;
    Address: INTEGER;

    BEGIN
    XCall := X;
    YCall := Y;

    Address := 0;

    WHILE TRUE DO
        BEGIN
        IF XCall^ [Address] <= ControlZ THEN
            BEGIN
            IF YCall^ [Address] <= ControlZ THEN
                BEGIN
                Compare := 0;
                Exit;
                END;

            Compare := -1;
            Exit;
            END;

        IF XCall^ [Address] = YCall^ [Address] THEN
            BEGIN
            Inc (Address);
            Continue;
            END;

        IF YCall^ [Address] <= ControlZ THEN
            BEGIN
            Compare := 1;
            Exit;
            END;

        IF XCall^ [Address] > YCall^ [Address] THEN
            Compare := 1
        ELSE
            Compare := -1;

        Exit;
        END;
    END;




PROCEDURE HeapSift (VAR PointerArray: EntryPointerListPtr;
		    FirstHeapIndex, LastHeapIndex: LONGINT);

VAR RightChildIndex, LeftChildIndex, GreaterChildIndex: LONGINT;
    SavedPointer: POINTER;

    BEGIN

    WHILE TRUE DO
	BEGIN
	LeftChildIndex := (FirstHeapIndex * 2) + 1;

	IF LeftChildIndex > LastHeapIndex THEN Exit;

	RightChildIndex := LeftChildIndex + 1;

	IF RightChildIndex > LastHeapIndex THEN
	    GreaterChildIndex := LeftChildIndex
	ELSE
	    IF Compare (PointerArray^ [LeftChildIndex], PointerArray^ [RightChildIndex]) >= 0 THEN
		GreaterChildIndex := LeftChildIndex
	    ELSE
		GreaterChildIndex := RightChildIndex;

	IF Compare (PointerArray^ [FirstHeapIndex], PointerArray^ [GreaterChildIndex]) >= 0 THEN
	    Exit;

	SavedPointer := PointerArray^ [GreaterChildIndex];
	PointerArray^ [GreaterChildIndex] := PointerArray^ [FirstHeapIndex];
	PointerArray^ [FirstHeapIndex] := SavedPointer;

	FirstHeapIndex := GreaterChildIndex;
	END;
    END;



PROCEDURE HeapSort (VAR PointerArray: EntryPointerListPtr;
		    NumberOfRecords: LONGINT);

{ Perform an in-place rearrangement of the pointers in PointerArray so they
  are in ascending order according to the function Compare.  PointerArray is
  zero-based with indices [ 0 ..  NumberOfRecords - 1 ].  Compare takes two
  pointers as arguments and returns -1, 0, or +1 as the first argument is
  less than, equal to, or greater than the second argument. }

VAR FirstHeapIndex, LastHeapIndex: LONGINT;
    SavedPointer: POINTER;

    BEGIN

    { The portion of the array that constitutes a valid heap is between
      FirstHeapIndex and LastHeapIndex, inclusive.  To start, each LEAF node
      is a trivial heap since it has no children to be less than.  Start by
      pointing FirstHeapIndex at the first leaf node, and LastHeapIndex at
      the last (leaf) node. }

    FirstHeapIndex := NumberOfRecords DIV 2;
    LastHeapIndex := NumberOfRecords - 1;

    { Expand the heap to include non-leaf nodes. }

    WHILE FirstHeapIndex > 0 DO
	BEGIN
	FirstHeapIndex := FirstHeapIndex - 1;
	HeapSift (PointerArray, FirstHeapIndex, LastHeapIndex);
	END;

    { Swap the first and last heap nodes, then contract the heap by removing
      the last node. }

    WHILE LastHeapIndex > 0 DO
	BEGIN
	SavedPointer := PointerArray^ [LastHeapIndex];
	PointerArray^ [LastHeapIndex] := PointerArray^ [FirstHeapIndex];
	PointerArray^ [FirstHeapIndex] := SavedPointer;
	LastHeapIndex := LastHeapIndex - 1;
	HeapSift (PointerArray, FirstHeapIndex, LastHeapIndex);
	END;

    END;

END.

