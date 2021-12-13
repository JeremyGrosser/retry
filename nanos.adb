with Interfaces.C; use Interfaces.C;
with Interfaces;
with Ada.Command_Line;
with Ada.Text_IO;

procedure Nanos is
   type clockid_t is
      (CLOCK_REALTIME, CLOCK_MONOTONIC, CLOCK_VIRTUAL, CLOCK_PROF,
       CLOCK_PROCESS_CPUTIME_ID, CLOCK_THREAD_CPUTIME_ID)
      with Size => int'Size;

   type time_t is new Interfaces.Integer_64;
   type timespec is record
      tv_sec  : time_t;
      tv_nsec : Long_Integer;
   end record
      with Convention => C;

   function clock_gettime
      (clock_id : clockid_t;
       tp       : access timespec)
       return int
   with Import, Convention => C, External_Name => "clock_gettime";

   Now : aliased timespec;

begin
   if clock_gettime (CLOCK_REALTIME, Now'Unchecked_Access) /= 0 then
      Ada.Text_IO.Put_Line (Ada.Text_IO.Standard_Error, "clock_gettime failed");
      Ada.Command_Line.Set_Exit_Status (1);
   else
      declare
         package LLI_IO is new Ada.Text_IO.Integer_IO (Num => Long_Long_Integer);
         Sec : Long_Long_Integer := Long_Long_Integer (Now.tv_sec);
         Ns  : Long_Long_Integer := Long_Long_Integer (Now.tv_nsec);
         S   : String (1 .. 9);
      begin
         LLI_IO.Default_Width := 9;
         LLI_IO.Put (Sec);
         Ada.Text_IO.Put ('.');
         LLI_IO.Put (S, Ns);
         for I in S'Range loop
            if S (I) = ' ' then
               S (I) := '0';
            end if;
         end loop;
         Ada.Text_IO.Put (S);
         Ada.Text_IO.New_Line;
      end;
   end if;
end Nanos;
