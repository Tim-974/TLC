@
@ Question 3.2.2
@
@ En utilisant une boucle, réalisez un programme permettant
@ au robot de se déplacer 5 fois vers le sud.
@
_start:
	invoke 2, 0, 0
	invoke 2, 0, 0
	seti r0, #0
	seti r1, #1
	seti r2, #5
loop:
	invoke 	1, 0, 0
	add 	r0, r0, r1
	goto_eq end, r2, r0
	goto 	loop
end:
	stop
