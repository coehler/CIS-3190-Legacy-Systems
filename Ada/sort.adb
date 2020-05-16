with Ada.Text_IO; use Ada.Text_IO;
with QuickSort; use QuickSort;
with Ada.Strings; use Ada.Strings;
with Ada.Strings.Fixed; use Ada.Strings.Fixed;
procedure sort is

	-- Declarations.
	length : integer;
	path : String(1..256) := (others => ' ');
	last : integer;
begin

	put_line("Welcome to ADA Quick Sort | By Christopher Oehler");
	put_line("");
	put_line("Enter file path:");
	Get_Line(path, last);
	Put_Line("You entered: """ & trim(path,both) & """");
	length := readLength(trim(path,both));
	declare
		arr : sortArr(1..length);
	begin
		put_line("File found. Reading file.");
		arr := readUnsorted(length, trim(path,both));
		put_line("Sorting file.");
		nonRecursiveQsort(arr, length);
		writeSorted(length, arr);
		put_line("Results written to sortedNUM.txt.");
	end;

end Sort;
