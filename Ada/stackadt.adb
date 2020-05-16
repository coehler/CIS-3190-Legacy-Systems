with Ada.Text_IO; use Ada.Text_IO;
package body StackADT is
	
	type pair is record -- Tuple of integers.
		l,r: integer;
	end record;

	type list is array(1..100) of pair; -- A list of pairs.

	type int_stack is -- A list of pairs with an index.
		record
			item : list;
			top : natural := 0;
		end record;
	st : int_stack;

	procedure push(l : in integer; r : in integer) is
	begin
		if st.top = 100 then
			put_line("stack is full");
		else
			st.top := st.top + 1;

			-- Set LEFT and RIGHT.
			st.item(st.top).l := l;
			st.item(st.top).r := r;
		end if;
	end push;
	
	procedure pop(l : out integer; r : out integer) is -- Will set l & r as the top of the stack.
	begin
		if st.top = 0 then
			put_line("stack is empty");
		else

			-- Set LEFT and RIGHT.
			l := st.item(st.top).l;
			r := st.item(st.top).r;
			st.top := st.top - 1;
		end if;
	end pop;
	
	function stack_is_empty return Boolean is
	begin
		return st.top = 0;
	end stack_is_empty;
	
	procedure reset_stack is
	begin
		st.top := 0;
	end reset_stack;
end StackADT;