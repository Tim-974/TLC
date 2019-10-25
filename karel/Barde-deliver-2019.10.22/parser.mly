%{

open Quad
open Common
open Comp
open Karel


%}

%token BEGIN_PROG
%token BEGIN_EXEC
%token END_EXEC
%token END_PROG

%token MOVE
%token TURN_LEFT
%token TURN_OFF

%token PICK_BEEPER
%token PUT_BEEPER

%token NEXT_TO_A_BEEPER
%token NOT_NEXT_TO_A_BEEPER
%token FRONT_IS_CLEAR
%token FRONT_IS_BLOCKED
%token LEFT_IS_CLEAR
%token LEFT_IS_BLOCKED
%token RIGHT_IS_CLEAR
%token RIGHT_IS_BLOCKED
%token FACING_NORTH
%token NOT_FACING_NORTH
%token FACING_EAST
%token NOT_FACING_EAST
%token FACING_SOUTH
%token NOT_FACING_SOUTH
%token FACING_WEST
%token NOT_FACING_WEST
%token ANY_BEEPERS_IN_BEEPER_BAG
%token NO_BEEPERS_IN_BEEPER_BAG

%token ITERATE
%token TIMES
%token WHILE
%token DO

%token <string> ID

%token DEFINE_NEW_INSTRUCTION
%token AS

%token SEMI
%token BEGIN
%token END

%token IF
%token THEN
%token ELSE

%token <int> INT

%type <unit> prog
%start prog

%%

prog:	BEGIN_PROG defines BEGIN_EXEC stmts_opt END_EXEC END_PROG
			{ () }
;



stmts_opt:	/* empty */		{ () }
|			stmts			{ () }
;

stmts:		stmt			{ () }
|			stmts SEMI stmt	{ () }
|			stmts SEMI		{ () }
;

stmt:		BEGIN stmts END { () }
|			ITERATE INT TIMES stmt { () }
|			open_stmt { () }
|			closed_stmt { () }
;

test: 		NEXT_TO_A_BEEPER
				{
					gen (INVOKE (next_beeper, 0, 0));
				}
|			NOT_NEXT_TO_A_BEEPER
				{
					gen (INVOKE (no_next_beeper, 0, 0));
				}
				
|			FRONT_IS_CLEAR
				{
					gen (INVOKE (is_clear, front, 0));
				}
				
|			FRONT_IS_BLOCKED
				{
					gen (INVOKE (is_blocked, front, 0));
				}
				
|			LEFT_IS_CLEAR
				{
					gen (INVOKE (is_clear, left, 0));
				}
				
|			LEFT_IS_BLOCKED
				{
					gen (INVOKE (is_blocked, left, 0));
				}
				
|			RIGHT_IS_CLEAR
				{
					gen (INVOKE (is_clear, right, 0));
				}
				
|			RIGHT_IS_BLOCKED
				{
					gen (INVOKE (is_blocked, right, 0));
				}
				
|			FACING_NORTH
				{
					gen (INVOKE (facing, north, 0));
				}
				
|			NOT_FACING_NORTH
				{
					gen (INVOKE (not_facing, north, 0));
				}
				
|			FACING_EAST
				{
					gen (INVOKE (facing, east, 0));
				}
				
|			NOT_FACING_EAST
				{
					gen (INVOKE (not_facing, east, 0));
				}
				
|			FACING_SOUTH
				{
					gen (INVOKE (facing, south, 0));
				}
				
|			NOT_FACING_SOUTH
				{
					gen (INVOKE (not_facing, south, 0));
				}
				
|			FACING_WEST
				{
					gen (INVOKE (facing, west, 0));
				}
				
|			NOT_FACING_WEST
				{
					gen (INVOKE (not_facing, west, 0));
				}
				
|			ANY_BEEPERS_IN_BEEPER_BAG
				{
					gen (INVOKE (any_beeper, 0, 0));
				}
				
|			NO_BEEPERS_IN_BEEPER_BAG
				{
					gen (INVOKE (no_beeper, 0, 0));
				}
;

simple_stmt: TURN_LEFT
				{ gen (INVOKE (turn_left, 0, 0)) }
|			TURN_OFF
				{ gen STOP  }
|			MOVE
				{ gen (INVOKE (move, 0, 0)) }
|			PICK_BEEPER
				{ gen (INVOKE (pick_beeper, 0, 0)) }
|			PUT_BEEPER
				{ gen (INVOKE (put_beeper, 0, 0)) }
|			ID 	
				{ (
					if not (is_defined $1)
					then raise (SyntaxError (Printf.sprintf "%s is not defined!" $1))
					else gen (CALL (get_define $1))
				) }
				
;



open_stmt:		IF test THEN closed_stmt { () }
|				IF test THEN open_stmt { () }
|				IF test THEN closed_stmt ELSE open_stmt { () }
|				WHILE test DO open_stmt { () }
;

define_new: 	DEFINE_NEW_INSTRUCTION ID AS stmts	
					{ (
						if (is_defined $2)
						then raise (SyntaxError (Printf.sprintf "%s is already defined!" $2))
						else define $2 0
					
					) }
;

defines: 		/* empty */		{ () }
|				defines define_new		{ () }
;

closed_stmt:	simple_stmt 		{ () }
|				IF test THEN closed_stmt ELSE closed_stmt { () }
|				WHILE test DO closed_stmt { () }		
;
