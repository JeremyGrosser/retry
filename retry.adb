--------------------------------------------------------------------------------
--  retry: run a command until it succeeds
--  Copyright (C) 2020-2021 Jeremy Grosser <jeremy@synack.me>
--
--  This program is free software; you can redistribute it and/or
--  modify it under the terms of the GNU General Public License
--  as published by the Free Software Foundation; either version 2
--  of the License, or (at your option) any later version.
--
--  This program is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--  GNU General Public License for more details.
--
--  You should have received a copy of the GNU General Public License
--  along with this program; if not, write to the
--  Free Software Foundation, Inc.
--  51 Franklin Street, Fifth Floor
--  Boston, MA 02110-1301 USA.
--------------------------------------------------------------------------------
with GNAT.Command_Line; use GNAT.Command_Line;
with GNAT.OS_Lib;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions;

procedure Retry is
    subtype Return_Code is Integer range 0 .. 255;
    type Success_Codes is array (1 .. 16) of Return_Code;
    subtype Seconds is Float range 0.0 .. (Float (Integer'Last) / 1000.0);

    package Seconds_IO is new Ada.Text_IO.Float_IO
        (Num => Seconds);
    package Integer_IO is new Ada.Text_IO.Integer_IO
        (Num => Integer);

    Num_Retries : Natural   := 0;
    Attempts    : Natural   := 0;
    Backoff     : Seconds   := 1.0;
    Max_Backoff : Seconds   := 0.0;
    SC_Index    : Positive  := Success_Codes'First;
    SC          : Success_Codes := (others => 0);
    Last        : Positive;
    Ok          : Boolean;

    procedure Help is
    begin
        Put_Line (Standard_Error, "retry -h -n NUM_RETRIES -b BACKOFF -m MAX_BACKOFF -s RETURN_CODE <command>");
        Put_Line (Standard_Error, "      ");
        Put_Line (Standard_Error, "      -n number of retries to attempt if the command does not return successfully.");
        Put_Line (Standard_Error, "         default: unlimited");
        Put_Line (Standard_Error, "      -b initial backoff in seconds. Each subsequent failure will double the backoff interval.");
        Put_Line (Standard_Error, "         default: 1.000");
        Put_Line (Standard_Error, "      -m maximum backoff in seconds.");
        Put_Line (Standard_Error, "         default: unlimited");
        Put_Line (Standard_Error, "      -s return value indicating success of the command. May be used up to 16 times.");
        Put_Line (Standard_Error, "         default: 0");
        Put_Line (Standard_Error, "      -h this help");
        Put_Line (Standard_Error, "      ");
        Put_Line (Standard_Error, "      backoff values have a resolution of 1 millisecond (0.001 seconds), ");
        Put      (Standard_Error, "      up to ");
        Seconds_IO.Put (Standard_Error, Seconds'Last);
        Put      (Standard_Error, " seconds (");
        Seconds_IO.Put (Standard_Error, Seconds'Last / 86_400.0);
        Put_Line (Standard_Error, " days).");
        Put_Line (Standard_Error, "      return codes must be in the range 0 .. 255");
        Put_Line (Standard_Error, "      command is executed in a subshell: /bin/sh -c ""command""");
        Set_Exit_Status (-1);
    end Help;

    procedure Parse_Options (Success : out Boolean) is
    begin
        Success := True;
        loop
            case Getopt ("h n: b: m: s:") is
                when 'n' =>
                    Num_Retries := Natural'Value (Parameter);
                when 'b' =>
                    Seconds_IO.Get (Parameter, Backoff, Last);
                when 'm' =>
                    Seconds_IO.Get (Parameter, Max_Backoff, Last);
                when 's' =>
                    SC (SC_Index) := Return_Code'Value (Parameter);
                    SC_Index := SC_Index + 1;
                when ASCII.NUL =>
                    exit;
                when others =>
                    Help;
                    Success := False;
                    return;
            end case;
        end loop;

        if SC_Index /= Success_Codes'First then
           SC_Index := SC_Index - 1;
        end if;
    end Parse_Options;
begin
    Seconds_IO.Default_Fore := 0;
    Seconds_IO.Default_Aft := 3;
    Seconds_IO.Default_Exp := 0;
    Integer_IO.Default_Width := 0;

    Parse_Options (Ok);
    if Ok /= True then
        return;
    end if;

    declare
        Command    : String := Get_Argument;
        Shell      : String := "/bin/sh";
        Shell_Args : String := "-c";
        Args       : GNAT.OS_Lib.Argument_List :=
            (new String'(Shell_Args),
             new String'(Command));
        RC          : Return_Code;
        Next_Retry  : Time;
    begin
        if Command'Length = 0 then
            Help;
            return;
        end if;

        loop
            RC := Return_Code (GNAT.OS_Lib.Spawn (Shell, Args));
            Set_Exit_Status (Exit_Status (RC));
            for I in Success_Codes'First .. SC_Index loop
                if RC = SC (I) then
                    return;
                end if;
            end loop;

            Attempts := Attempts + 1;
            Put (Standard_Error, "Attempt ");
            Integer_IO.Put (Standard_Error, Attempts);
            if Num_Retries > 0 then
                Put (Standard_Error, "/");
                Integer_IO.Put (Standard_Error, Num_Retries);
            end if;
            Put (Standard_Error, " exited with status ");
            Integer_IO.Put (Standard_Error, RC);

            if Num_Retries > 0 and Attempts = Num_Retries then
                Put_Line (Standard_Error, ", retries exceeded.");
                exit;
            else
                if Max_Backoff /= 0.0 and Backoff > Max_Backoff then
                    Backoff := Max_Backoff;
                end if;

                Put (Standard_Error, ", retrying after ");
                Seconds_IO.Put (Standard_Error, Backoff);
                Put_Line (Standard_Error, " seconds");

                Next_Retry := Clock + Milliseconds (Integer (Float (Backoff) * 1_000.0));
                delay until Next_Retry;

                if (Backoff * 2.0) > Seconds'Last then
                    Put_Line (Standard_Error, "Backoff exceeds limit, exit.");
                    Set_Exit_Status (-1);
                    return;
                end if;
                Backoff := Backoff * 2.0;
            end if;
        end loop;
    end;
end Retry;
