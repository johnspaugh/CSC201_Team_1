(* CSC201, Section 1, TEAM 1, John Spaugh,
	Taro Kumagai,
	Niravkumar Tandel *)

(*---- Sec1Proj1Team1Step1 ----------------------*)
(* Abastract Grammar of a Graal-like Language in BNF for Project *)

datatype Integer_Constant = Z;
datatype Boolean_Constant = B;
datatype Variable = S of string;

datatype Arithmatic_Op = Plus | Minus | Times | Div;
datatype Relational_Op = Lt | Le | Eq | Ne | Ge | Gt;
datatype Boolean_Op = And | Or;
datatype Operator = AOp of Arithmatic_Op | ROp of Relational_Op | BOp of Boolean_Op; 

datatype Expression = Var of Variable |
			IC of Integer_Constant |
			BC of Boolean_Constant |
			EEO of Expression * Expression * Operator;

(* John Note: it is failing below, anybody know how to fix it? *)


(* replaced Skip with Empty *)
datatype Instruction = Empty |
			VE of Variable * Expression |
			EII of Expression * Instruction * Instruction |
			EI of Expression * Instruction |
			IList of Instruction * ;

datatype Type = TypeName1Bool | TypeName2Int ;

type Declaration = Variable * Type;
type DeclarationList = Declaration *; 

type Program = DeclarationList * Instruction;

(* ----------------------------------------------*)