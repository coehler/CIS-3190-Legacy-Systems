with Ada.Text_IO; use Ada.Text_IO;
with StackADT; use StackADT;
with ada.strings.unbounded; use ada.strings.unbounded;
with ada.strings.unbounded.Text_IO; use ada.strings.unbounded.Text_IO;
package body QuickSort is
	procedure nonRecursiveQsort(arr: in out sortArr; length: in integer) is
		-- Declarations.
		x, w: integer;
		i, j, l, r: integer := 0;
		s: integer range 0..length;
		flag: Boolean := false;
	begin
		s := 1;
		push(1, length);
		loop -- Take top request from stack.
			pop(l,r);
			loop -- Partition arr[l] to arr[r].
				i := l;
				j := r;
				x := arr((l+r) / 2);
				loop
					while arr(i) < x loop
						i := i + 1;
					end loop;
					while x < arr(j) loop
						j := j - 1;
					end loop;
					if i <= j then
						w := arr(i);
						arr(i) := arr(j);
						arr(j) := w;
						i := i+1;
						j := j-1;
					end if;
					exit when i > j;
				end loop;
				if j-l < r-i then -- Stack request to sort right partition.
					if i < r then
						s := s + 1;
						push(i,r);
					end if;
					r := j; -- Continue sorting left partition.
				else
					if l < j then -- Stack request for sorting left partition.
						push(l,j);
					end if;
					l := i; -- Continue sorting right partition.
				end if;
				exit when l >= r;
			end loop;
			flag := stack_is_empty;
			exit when flag;
		end loop;
	end nonRecursiveQsort;

	function readLength(path : String) return integer is
		infp : file_type;
		s : unbounded_string;
		length : integer := 0;
	begin
		open(infp,in_file,path); -- Open file.
		-- Read file and get length.
		loop
			exit when end_of_file(infp);
			get_line(infp,s);
			length := length + 1;
		end loop;
		close(infp);
		return length;
	end readLength;

	function readUnsorted(length : integer; path : string) return sortArr is 
		infp : file_type;
		s : unbounded_string;
		arr : sortArr(1..length);
		k : integer;
	begin
		open(infp,in_file,path); -- Open file.
		for i in 1..length loop
			exit when end_of_file(infp);
			get_line(infp,s);
			-- Convert unbounded string to integer.
			k := Integer'Value (To_String (s));
			arr(i) := k;
		end loop;
		close(infp);
		return arr;
	end readUnsorted;

	procedure writeSorted(length : in integer; arr: in sortArr) is
		outfp : file_type;
	begin
		create(outfp, out_file, "sortedNUM.txt");
		for i in 1..length loop
			put_line(outfp, integer'image(arr(i)));
		end loop;
		close(outfp);
	end writeSorted;
end QuickSort;
