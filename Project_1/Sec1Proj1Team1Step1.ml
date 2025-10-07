(* CSC201, Section 1, TEAM 1, John Spaugh,
	Taro Kumagai,
	Niravkumar Tandel *)

(*---- Sec1Proj1Team1Step1 ----------------------*)
(* Abastract Grammar of a Graal-like Language in BNF for Project *)

type Integer_Constant =  int;
type Boolean_Constant = bool;
datatype Variable = S of string;

datatype Arithmatic_Op = Plus | Minus | Times | Div;
datatype Relational_Op = Lt | Le | Eq | Ne | Ge | Gt;
datatype Boolean_Op = And | Or;
datatype Operator = AOp of Arithmatic_Op | ROp of Relational_Op | BOp of Boolean_Op; 

datatype Expression = Var of Variable |
			IC of Integer_Constant |
			BC of Boolean_Constant |
			EEO of Expression * Expression * Operator;


datatype Instruction = Skip | 
			VE of(Variable * Expression) | 
			IfThenElse of(Expression * Instruction * Instruction) |
            WhileLoop of(Expression * Instruction) | 
			Seq of Instruction list;


datatype Type = TypeName1Bool | TypeName2Int ;

type Declaration = Variable * Type;
type DeclarationList = Declaration list;  

type Program = DeclarationList * Instruction;

(* ----------------------------------------------*)

(*------sample1 Begain---- *)

(* define variable names *)
val var_n 		= S "n";
val var_cur 	= S "cur";
val var_prev1 	= S "prev1";
val var_prev2 	= S "prev2";
val var_i 		= S "i";
val var_answer 	= S "answer";

(* set variable Declaration *)
val declaration_n = (var_n, TypeName2Int);
val declaration_cur = (var_cur, TypeName2Int);
val declaration_prev1 = (var_prev1, TypeName2Int);
val declaration_prev2 = (var_prev2, TypeName2Int);
val declaration_i = (var_i, TypeName2Int);
val declaration_answer = (var_answer, TypeName2Int);

(*set DeclarationList *)
val all_declarationList = [
	declaration_n,
	declaration_cur,
	declaration_prev1,
	declaration_prev2,
	declaration_i,
	declaration_answer
];

val n_assign_15 = VE ( var_n, IC 15);

(* if-then-else *)
(* if expression *)
val isequal = Eq;
val O_equal = ROp isequal;
val exp_isEqual0 = EEO (Var var_n, IC 0, O_equal);

(* then statement *)
val Assign_answer = VE (var_answer, IC 2);

(* else expression *)
val Assign_i = VE (var_answer, IC 1);
(* 2-inner if expression *)
val isequal1 = Eq;
val O_equal1 = ROp isequal1;
val exp_isEqual1 = EEO (Var var_n, IC 1, O_equal1);

(* 2-then statement *)
val Assign_answer_1 = VE (var_answer, IC 1);

(* 2-else expression *)
val Assign_prev2_2 = VE (var_prev2, IC 2);
val Assign_prev1_1 = VE (var_prev1, IC 1);

val islessthan = Lt;
val O_less = ROp islessthan;
val iLessthenN = EEO (Var var_i, Var var_n, O_less);

val add = Plus;
val O_add = AOp add;
val expr_iplusplus = EEO (Var var_i, IC 1, O_add);
val Instruction_iplusplus = VE (var_i, expr_iplusplus)
val expr_cur_p1ADDp2 = EEO (Var var_prev1, Var var_prev2, O_add);
val Instruction_cur = VE (var_cur, expr_cur_p1ADDp2);
val expr_var_prev1 = Var var_prev1;
val Instruction_prev1 = VE (var_prev2, expr_var_prev1);
val expr_var_cur = Var var_cur;
val Instruction_var_cur = VE (var_prev1, expr_var_cur);

val whileInstruction = Seq [
	Instruction_iplusplus,
	Instruction_cur,
	Instruction_prev1,
	Instruction_var_cur
];

val instuction_while = WhileLoop ( iLessthenN, whileInstruction);
val var_var_cur = Var var_cur;
val Instruction_answer_cur = VE (var_answer, var_cur);

val elseInstruction = Seq [
	Assign_prev2_2,
	Assign_prev1_1,
	whileInstruction,
	Instruction_answer_cur
];

val Instruction_else = IfThenElse ( exp_isEqual1, Assign_answer_1, elseInstruction)

val Instruction_ifthenelse = IfThenElse (exp_isEqual0, Assign_answer, Instruction_else);

val all_instructions = Seq [
	n_assign,
	Instruction_ifthenelse
];

val Program_Lucus =  ( all_declarationList, all_instructions);



(* -----sample1 end -------*)
