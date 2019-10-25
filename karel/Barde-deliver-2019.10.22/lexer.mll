{
open Parser
}

let comment = '{' [^ '}']* '}'
let space = [' ' '\t' '\n']+
let integer = ['0'-'9']+
let id = ['a'-'z' 'A'-'Z' '_'] ['a'-'z' 'A'-'Z' '_' '0'-'9']*

rule scan =
parse	"BEGINNING-OF-PROGRAM"		{ BEGIN_PROG }
|		"BEGINNING-OF-EXECUTION"	{ BEGIN_EXEC }
|		"END-OF-EXECUTION"			{ END_EXEC }
|		"END-OF-PROGRAM"			{ END_PROG }
|		"move"						{ MOVE }
|		"turnleft"					{ TURN_LEFT }
|		"turnoff"					{ TURN_OFF }
|		"BEGIN"						{ BEGIN }
|		"END"						{ END }

|		"pickbeeper"				{ PICK_BEEPER }
|		"putbeeper"					{ PUT_BEEPER }

|		"next-to-a-beeper"			{ NEXT_TO_A_BEEPER }
|		"not-next-to-a-beeper"		{ NOT_NEXT_TO_A_BEEPER }
|		"front-is-clear"			{ FRONT_IS_CLEAR }
|		"front-is-blocked"			{ FRONT_IS_BLOCKED }
|		"left-is-clear"				{ LEFT_IS_CLEAR }
|		"left-is-blocked"			{ LEFT_IS_BLOCKED }
|		"right-is-clear"			{ RIGHT_IS_CLEAR }
|		"right-is-blocked"			{ RIGHT_IS_BLOCKED }
|		"facing-north"				{ FACING_NORTH }
|		"not-facing-north"			{ NOT_FACING_NORTH }
|		"facing-east"				{ FACING_EAST }
|		"not-facing-east"			{ NOT_FACING_EAST }
|		"facing-south"				{ FACING_SOUTH }
|		"not-facing-south"			{ NOT_FACING_SOUTH }
|		"facing-west"				{ FACING_WEST }
|		"not-facing-west"			{ NOT_FACING_WEST }
|		"any-beepers-in-beeper-bag"	{ ANY_BEEPERS_IN_BEEPER_BAG }
|		"no-beepers-in-beeper-bag"	{ NO_BEEPERS_IN_BEEPER_BAG }

|		"ITERATE"					{ ITERATE }
|		"TIMES"						{ TIMES }
|		"WHILE"						{ WHILE }
|		"DO"						{ DO }
|		"IF"						{ IF }
|		"THEN"						{ THEN }
|		"DEFINE-NEW-INSTRUCTION"	{ DEFINE_NEW_INSTRUCTION }
|		"AS"						{ AS }
|		"ELSE"						{ ELSE }

|		id as c						{ ID c }
		
|		";"							{ SEMI }


|		space						{ scan lexbuf }
|		comment						{ scan lexbuf }

|		integer	as c				{ INT(int_of_string c) }

|		_ as c						{ raise (Common.LexerError (Printf.sprintf "unknown character '%c'" c)) }
