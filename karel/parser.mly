%{

open Quad
open Common
open Comp
open Karel

let gen_while(b, a) =
	gen (GOTO b);
	backpatch a (nextquad ())
	
	
let gen_iter(i,a) =
	let m = new_temp() in
	gen (SETI (m, 1));
	gen (ADD (i, m, i));
	gen (GOTO (a));
	backpatch a (nextquad ())

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
|			iteration_head stmt { ( gen_iter $1 ) }
|			open_stmt { () }
|			closed_stmt { () }
;

if_test: 	test
				{
					let v' = new_temp () in
					let _ = gen (SETI (v', 0)) in
					let a = nextquad () in
					let _ = gen (GOTO_EQ (0, $1, v')) in
					a
				}
;
					
test: 		NEXT_TO_A_BEEPER
				{
					let r = new_temp() in
					gen (INVOKE (next_beeper, r, 0));
					r
				}
|			NOT_NEXT_TO_A_BEEPER
				{
					let r = new_temp() in
					gen (INVOKE (no_next_beeper, r, 0));
					r
				}
				
|			FRONT_IS_CLEAR
				{
					let r = new_temp() in
					gen (INVOKE (is_clear, front, r));
					r
				}
				
|			FRONT_IS_BLOCKED
				{
					let r = new_temp() in
					gen (INVOKE (is_blocked, front, r));
					r
				}
				
|			LEFT_IS_CLEAR
				{
					let r = new_temp() in
					gen (INVOKE (is_clear, left, r));
					r
				}
				
|			LEFT_IS_BLOCKED
				{
					let r = new_temp() in
					gen (INVOKE (is_blocked, left, r));
					r
				}
				
|			RIGHT_IS_CLEAR
				{
					let r = new_temp() in
					gen (INVOKE (is_clear, right, r));
					r
				}
				
|			RIGHT_IS_BLOCKED
				{
					let r = new_temp() in
					gen (INVOKE (is_blocked, right, r));
					r
				}
				
|			FACING_NORTH
				{
					let r = new_temp() in
					gen (INVOKE (facing, north, r));
					r
				}
				
|			NOT_FACING_NORTH
				{
					let r = new_temp() in
					gen (INVOKE (not_facing, north, r));
					r
				}
				
|			FACING_EAST
				{
					let r = new_temp() in
					gen (INVOKE (facing, east, r));
					r
				}
				
|			NOT_FACING_EAST
				{
					let r = new_temp() in
					gen (INVOKE (not_facing, east, r));
					r
				}
				
|			FACING_SOUTH
				{
					let r = new_temp() in
					gen (INVOKE (facing, south, r));
					r
				}
				
|			NOT_FACING_SOUTH
				{
					let r = new_temp() in
					gen (INVOKE (not_facing, south, r));
					r
				}
				
|			FACING_WEST
				{
					let r = new_temp() in
					gen (INVOKE (facing, west, r));
					r
				}
				
|			NOT_FACING_WEST
				{
					let r = new_temp() in
					gen (INVOKE (not_facing, west, r));
					r
				}
				
|			ANY_BEEPERS_IN_BEEPER_BAG
				{
					let r = new_temp() in
					gen (INVOKE (any_beeper, r, 0));
					r
				}
				
|			NO_BEEPERS_IN_BEEPER_BAG
				{
					let r = new_temp() in
					gen (INVOKE (no_beeper, r, 0));
					r
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

while_head: WHILE mark test DO
				{
					let t = new_temp () in
					gen (SETI (t, 0));
					let a = nextquad () in
					gen (GOTO_EQ(0, $3, t));
					($2, a)
				}
;

iteration_head: ITERATE INT TIMES stmt
				{
					let t = new_temp () in
					let i = new_temp () in
					gen (SETI (i, 0));
					gen (SETI (t, $2));
					let a = nextquad () in
					gen (GOTO_GE(0, i, t));
					(i, a)	
				}
;

mark:
				{ nextquad () }
;


open_stmt:		IF if_test THEN closed_stmt { backpatch $2 (nextquad ()) }
|				IF if_test THEN open_stmt { ( backpatch $2 (nextquad ())) }
|				IF if_test THEN closed_stmt ELSE open_stmt { () }
|				while_head open_stmt { gen_while $1 }
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
|				IF if_test THEN closed_stmt ELSE closed_stmt { () }
|				while_head closed_stmt { gen_while $1 }
;
