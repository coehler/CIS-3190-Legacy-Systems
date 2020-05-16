package QuickSort is
	type sortArr is array (integer range <>) of integer;
	procedure nonRecursiveQsort(arr: in out sortArr; length: in integer);
	function readLength(path : String) return integer;
	function readUnsorted(length : integer; path : string) return sortArr;
	procedure writeSorted(length : in integer; arr: in sortArr); 
end QuickSort;