(*Matic Mihelj 89181053 RIN 1*)


type chess_letter = A | B | C | D | E | F | G | H
and chess_piece = King | Queen | Rook | Bishop | Knight | Pawn
and chess_color = Black | White
and chess_position = Alive of chess_letter*int | Dead


type figura = chess_piece * chess_color * chess_position
type matx = figura option array array

type chess_move = chess_position*chess_position

let list = [A;B;C;D;E;F;G;H]
let get_number a = match a with
| A -> 0
| B -> 1
| C -> 2
| D -> 3
| E -> 4
| F -> 5
| G -> 6
| H -> 7




class chess = 
	object
	val tab=Array.make 32 (King,Black,Dead)
	val matrika : matx = Array.make_matrix 8 8 None
		method matrix =
		matrika.(0).(0) <- Some(Rook,Black,Alive(A,8));
		matrika.(0).(1) <- Some(Knight,Black,Alive(B,8));
		matrika.(0).(2) <- Some(Bishop,Black,Alive(C,8));
		matrika.(0).(3) <- Some(Queen,Black,Alive(D,8));
		matrika.(0).(4) <- Some(King,Black,Alive(E,8));
		matrika.(0).(5) <- Some(Bishop,Black,Alive(F,8));
		matrika.(0).(6) <- Some(Knight,Black,Alive(G,8));
		matrika.(0).(7) <- Some(Rook,Black,Alive(H,8));
		matrika.(1).(0) <- Some(Pawn,Black,Alive(A,7));
		matrika.(1).(1) <- Some(Pawn,Black,Alive(B,7));
		matrika.(1).(2) <- Some(Pawn,Black,Alive(C,7));
		matrika.(1).(3) <- Some(Pawn,Black,Alive(D,7));
		matrika.(1).(4) <- Some(Pawn,Black,Alive(E,7));
		matrika.(1).(5) <- Some(Pawn,Black,Alive(F,7));
		matrika.(1).(6) <- Some(Pawn,Black,Alive(G,7));
		matrika.(1).(7) <- Some(Pawn,Black,Alive(H,7));
		matrika.(6).(0) <- Some(Pawn,White,Alive(A,2));
		matrika.(6).(1) <- Some(Pawn,White,Alive(B,2));
		matrika.(6).(2) <- Some(Pawn,White,Alive(C,2));
		matrika.(6).(3) <- Some(Pawn,White,Alive(D,2));
		matrika.(6).(4) <- Some(Pawn,White,Alive(E,2));
		matrika.(6).(5) <- Some(Pawn,White,Alive(F,2));
		matrika.(6).(6) <- Some(Pawn,White,Alive(G,2));
		matrika.(6).(7) <- Some(Pawn,White,Alive(H,2));
		matrika.(7).(0) <- Some(Rook,White,Alive(A,1));
		matrika.(7).(1) <- Some(Knight,White,Alive(B,1));
		matrika.(7).(2) <- Some(Bishop,White,Alive(C,1));
		matrika.(7).(3) <- Some(Queen,White,Alive(D,1));
		matrika.(7).(4) <- Some(King,White,Alive(E,1));
		matrika.(7).(5) <- Some(Bishop,White,Alive(F,1));
		matrika.(7).(6) <- Some(Knight,White,Alive(G,1));
		matrika.(7).(7) <- Some(Rook,White,Alive(H,1));
	method new_game = 
		tab.(0) <- (Rook,Black,Alive(A,8));
		tab.(1) <- (Knight,Black,Alive(B,8));
		tab.(2) <- (Bishop,Black,Alive(C,8));
		tab.(3) <- (Queen,Black,Alive(D,8));
		tab.(4) <- (King,Black,Alive(E,8));
		tab.(5) <- (Bishop,Black,Alive(F,8));
		tab.(6) <- (Knight,Black,Alive(G,8));
		tab.(7) <- (Rook,Black,Alive(H,8));
		tab.(8) <- (Pawn,Black,Alive(A,7));
		tab.(9) <- (Pawn,Black,Alive(B,7));
		tab.(10) <- (Pawn,Black,Alive(C,7));
		tab.(11) <- (Pawn,Black,Alive(D,7));
		tab.(12) <- (Pawn,Black,Alive(E,7));
		tab.(13) <- (Pawn,Black,Alive(F,7));
		tab.(14) <- (Pawn,Black,Alive(G,7));
		tab.(15) <- (Pawn,Black,Alive(H,7));
		tab.(16) <- (Pawn,White,Alive(A,2));
		tab.(17) <- (Pawn,White,Alive(B,2));
		tab.(18) <- (Pawn,White,Alive(C,2));
		tab.(19) <- (Pawn,White,Alive(D,2));
		tab.(20) <- (Pawn,White,Alive(E,2));
		tab.(21) <- (Pawn,White,Alive(F,2));
		tab.(22) <- (Pawn,White,Alive(G,2));
		tab.(23) <- (Pawn,White,Alive(H,2));
		tab.(24) <- (Rook,White,Alive(A,1));
		tab.(25) <- (Knight,White,Alive(B,1));
		tab.(26) <- (Bishop,White,Alive(C,1));
		tab.(27) <- (Queen,White,Alive(D,1));
		tab.(28) <- (King,White,Alive(E,1));
		tab.(29) <- (Bishop,White,Alive(F,1));
		tab.(30) <- (Knight,White,Alive(G,1));
		tab.(31) <- (Rook,White,Alive(H,1))
	method print = 
		for i=0 to 7 do
			for j=0 to 7 do
				match matrika.(i).(j) with
				| None -> print_string "."
				| Some(a,b,c) -> match (a,b,c) with
											| (Rook,White,Alive(_,_)) -> print_string "R"
											| (Knight,White,Alive(_,_)) -> print_string "N"
											| (Bishop,White,Alive(_,_)) -> print_string "B"
											| (King,White,Alive(_,_)) -> print_string "K"
											| (Queen,White,Alive(_,_)) -> print_string "Q"
											| (Pawn,White,Alive(_,_)) -> print_string "P"
											| (Rook,Black,Alive(_,_)) -> print_string "r"
											| (Bishop,Black,Alive(_,_)) -> print_string "b"
											| (Knight,Black,Alive(_,_)) -> print_string "n"
											| (Queen,Black,Alive(_,_)) -> print_string "q"
											| (King,Black,Alive(_,_)) -> print_string "k"
											| (Pawn,Black,Alive(_,_)) -> print_string "p"
											| (_,_,_) -> print_string "."
				done;
				print_string "\n"
				done
				method get = matrika
			end;;
				
let premik x y = 
				match x with 
				| (_,Dead) -> ()
				| (Dead,_) -> ()
				| (Alive(s1,l1),Alive(s2,l2)) ->	
													 y.(l2-1).(get_number s2) <- y.(l1-1).(get_number s1);
																y.(l1-1).(get_number s1) <- None
													
													
(*match  y.(get_number s2).(l2) with
													| None -> 
													| Some(_,_,c) -> match c with
														| Dead -> 
														|Alive(_,_) -> *)
																																																																																																			
class chess_game = 
	object
	inherit chess
	val mutable list = []
	method premakni p1 = list <- p1::list; premik p1 matrika
	method getl = List.rev(list)
	end;;

let igra = new chess_game
igra#new_game
igra#matrix
igra#print
igra#premakni (Alive(F,2),Alive(F,3))
igra#premakni (Alive(E,7),Alive(E,5))
igra#premakni (Alive(G,2),Alive(G,4))
igra#premakni (Alive(D,8),Alive(H,4))
igra#getl


