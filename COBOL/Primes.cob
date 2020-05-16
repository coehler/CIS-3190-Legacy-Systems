identification division.
program-id. primes.
environment division.
input-output section.
file-control.
	select input-file assign to dynamic ws-fname-in
		organization is line sequential.
	select output-file assign to dynamic ws-fname-out.
data division.
file section.
fd output-file.
01 out-line.
	02 num picture 9(36).
working-storage section.
77 ws-fname-in picture x(30).
77 ws-fname-out picture x(30).
77 n picture s9(9).
77 r picture s9(9) usage is computational.
77 i picture s9(9) usage is computational.
77 eof-switch pic 9 value 1.
77 proc-switch pic 9 value 1.
01 in-card.
	02 in-n picture 9(9).
	02 filler picture x(71).
01 title-line.
	02 filler picture x(6) value spaces.
	02 filler picture x(20) value 'PRIME NUMBER RESULTS'.
01 under-line.
	02 filler picture x(32) value
	' -------------------------------'.
01 not-a-prime-line.
	02 filler picture x value space.
	02 out-n-2 picture z(8)9.
	02 filler picture x(15) value ' IS NOT A PRIME'.
01 prime-line.
	02 filler picture x value space.
	02 out-n-3 picture z(8)9.
	02 filler picture x(11) value ' IS A PRIME'.
01 error-mess.
	02 filler picture x value space.
	02 out-n picture z(8)9.
	02 filler picture x(14) value ' ILLEGAL INPUT'.
procedure division.

	display "Input file name? "
	accept ws-fname-in

	display "Output file name? "
	accept ws-fname-out

	open input input-file, output output-file.
	write out-line from title-line after advancing 0 lines.
	write out-line from under-line after advancing 1 line.
	perform main-loop 
		until eof-switch = 0.
	close input-file, output-file.
stop run.
main-loop.
	read input-file into in-card
		at end move zero to
	eof-switch end-read
	move zero to proc-switch
	if eof-switch is not equal to zero
		move in-n to n
		display n
		if n is greater than 1
			if n is less than 4
				move in-n to out-n-3
				write out-line from prime-line after advancing 1 line
			else
				move 2 to r
				perform until not r is less than n
					Divide r into n giving i
					multiply r by i
					if i is equal to n
						move 1 to proc-switch
						exit perform
					end-if
					add 1 to r
				end-perform
				if proc-switch is not equal to zero
					move in-n to out-n-2
					write out-line from not-a-prime-line after advancing 1 line
				else
					move in-n to out-n-3
					write out-line from prime-line after advancing 1 line
				end-if
			end-if
		else
			move in-n to out-n-2
			write out-line from error-mess after advancing 1 line
		end-if
	end-if.
