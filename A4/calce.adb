with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Text_IO;                       use Ada.Text_IO;
with Ada.Strings.Unbounded.Text_IO;     use Ada.Strings.Unbounded.Text_IO;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Ada.Numerics;

-- Main program
-- This program generates e to the specified number of digits.
procedure calce is
    fileName    : Unbounded_String;
    digitsInput : Unbounded_String;
    n           : Integer := 0;
    m           : Integer := 4;
    test        : Float   := 0.0;

    procedure keepe (arr : String; fileName : String) is
        fp : File_Type;
    begin

        -- Open file
        begin
            Open (fp, Out_File, fileName);
        exception
            when Name_Error =>
                Create (fp, Out_File, fileName);
        end;

        Put (fp, arr);

    end keepe;
begin

    Put_Line ("Enter file name: ");
    Get_Line (fileName);

    New_Line;

    Put_Line ("Enter number of digits: ");
    Get_Line (digitsInput);

    n := Integer'Value (To_String (digitsInput));

    test := (Float (n) + 1.0) * 2.302_585_09;

    loop
        exit when
           (Float (m) * (Log (Float (m), Ada.Numerics.e) - 1.0) + 0.5 * Log (6.283_185_2 * Float (m), Ada.Numerics.e)) >= test;

        m := m + 1;
    end loop;

    declare
        coef  : array (1 .. m + 1) of Integer := (1 .. m + 1 => 1);
        arr   : String (1 .. n + 2);
        carry : Integer                       := 0;
        tmp   : Integer                       := 0;
    begin

        arr (1) := '2';
        arr (2) := '.';

        for i in 1 .. n loop
            carry := 0;
            for j in reverse 2 .. m loop
                tmp      := Integer (coef (j) * 10 + carry);
                carry    := Integer (tmp / j);
                coef (j) := Integer (tmp - carry * j);
            end loop;

            arr (i + 2) := Integer'Image (carry) (2);
        end loop;

        keepe (arr, To_String (fileName));

    end;

end calce;
