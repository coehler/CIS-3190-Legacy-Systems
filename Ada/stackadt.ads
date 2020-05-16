package StackADT is
	procedure push(l : in integer; r : in integer);
	procedure pop(l : out integer; r : out integer);
	function stack_is_empty return Boolean;
	procedure reset_stack;
end StackADT;