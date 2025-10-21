(* CSC201, Section 1, TEAM 1, 
    John Spaugh,
    Taro Kumagai,
    Niravkumar Tandel,
    Charles Thomas *)

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
            VE of (Variable * Expression) | 
            IfThenElse of (Expression * Instruction * Instruction) |
            WhileLoop of (Expression * Instruction) | 
            Seq of Instruction list;


datatype Type = TypeBool | TypeInt ;

type Declaration = Variable * Type;
type DeclarationList = Declaration list;  

type Program = DeclarationList * Instruction;



(* Val Variable Declarations *)
val var_n       = S "n"
val var_cur     = S "cur"
val var_prev1   = S "prev1"
val var_prev2   = S "prev2"
val var_i       = S "i"
val var_answer  = S "answer"
val var_temp    = S "temp"

val declaration_n      = (var_n, TypeInt)
val declaration_cur    = (var_cur, TypeInt)
val declaration_prev1  = (var_prev1, TypeInt)
val declaration_prev2  = (var_prev2, TypeInt)
val declaration_i      = (var_i, TypeInt)
val declaration_answer = (var_answer, TypeInt)


(* Full Declaration List *)
val allDeclarations = [
    declaration_n, 
    declaration_cur, 
    declaration_prev1, 
    declaration_prev2, 
    declaration_i, 
    declaration_answer
    ]
    
(* Relational Logic *)
val n_equals_0 = EEO (Var var_n, IC 0, ROp Eq)           (* n == 0 *)
val n_equals_1 = EEO (Var var_n, IC 1, ROp Eq)           (* n == 1 *)
val i_lessthan_n = EEO (Var var_i, Var var_n, ROp Lt)    (* i < n *)

(* Assignment Instructions, FORMAT assign_variable_value *)
val assign_n_15         = VE (var_n, IC 15)                 (* n = 15 *)
val assign_answer_1     = VE (var_answer, IC 1)             (* answer = 1 *)
val assign_answer_2     = VE (var_answer, IC 2)             (* answer = 2 *)
val assign_i_1          = VE (var_i, IC 1)                  (* i = 1 *)
val assign_prev1_1      = VE (var_prev1, IC 1)              (* prev1 = 1 *)
val assign_prev2_2      = VE (var_prev2, IC 2)              (* prev2 = 2 *)
val assign_answer_cur   = VE (var_answer, Var var_cur)      (* answer = cur *)
val assign_prev2_prev1  = VE (var_prev2, Var var_prev1)     (* prev2 = prev1 *)
val assign_prev1_cur    = VE (var_prev1, Var var_cur)       (* prev1 = cur*)


(* Arithmatic Expressions *)
val add_i_1 = EEO (Var var_i, IC 1, AOp Plus)                       (* i + 1 *)
val add_prev1_prev2 = EEO (Var var_prev1, Var var_prev2, AOp Plus)  (* prev1 + prev2 *)

(* Arithmatic Instructions *)
val ipp =  VE (var_i, add_i_1)                              (* i++ *)
val cur_prev1_plus_prev2 = VE (var_cur, add_prev1_prev2)    (* cur = prev1 + prev2 *)


(* Instructions *)
val insideWhile = [
    ipp, 
    cur_prev1_plus_prev2, 
    assign_prev2_prev1, 
    assign_prev1_cur
    ]
    
val whileLoop = WhileLoop(i_lessthan_n, Seq insideWhile) 

val inner_Else = [
    assign_prev2_2, 
    assign_prev1_1, 
    whileLoop, 
    assign_answer_cur
    ]
    
val inner_ifThenElse = IfThenElse(n_equals_1, assign_answer_1, Seq inner_Else)   

val outer_Else = [
    assign_i_1, 
    inner_ifThenElse
    ]
    
val outer_ifThenElse = IfThenElse(n_equals_0, assign_answer_2, Seq outer_Else)  

val allInstructions = [assign_n_15, outer_ifThenElse]


(* The Program LUCAS*)
val lucas = (allDeclarations, Seq allInstructions)


(* -------------------
cpp file

#include <cstdlib>
#include <iomanip>
#include <iostream>

int main(){ //int argc, char *argv[]){
     int n;
     int cur;
     int prev1;
     int prev2;
     int i;
     int answer;

     n=15;

     if(n == 0){
          answer = 2;
     }else{
          i=1;
          if(n ==1){
               answer=1;
          }else{
               prev2 =2;
               prev1 =1;
               while (i < n){
                    i++; // i = i +1;
                    cur = prev1 + prev2;
                    prev2 = prev1;
                    prev1 = cur;
               }
               answer = cur;
          }
     }
     std::cout << "Answer: " << answer << std::endl;
     return 0;
}

---------------------------*)

(*------------step2 static semantics---------------*)
(* sml<Sec1Proj1Team1Step2.ml>Sec1Proj1Team1Step2Result.txt   *)

fun NoDuplicate((a: Variable, b: Type):: decListTail)(c: Variable) = (a <> c) andalso NoDuplicate decListTail c
  | NoDuplicate [] c = true;

val rec DecListVCheck =
     (fn ( ((a: Variable, b: Type):: decListTail):DeclarationList) =>
          DecListVCheck(decListTail) andalso
          NoDuplicate(decListTail)(a) |
          ([]) => true  
     ) ;

(*Good testing step 2, input allDeclarations list with int into DecListVCheck
Gtest1 expected true*)

val Gtest1 = DecListVCheck(allDeclarations);

(* Bad testing step 2, New List using allDeclarations adding on a duplicate n to the head
Btest1 expected false*)
val declarationslist_bad = declaration_n :: allDeclarations;
val Btest1 = DecListVCheck(declarationslist_bad);

(* 3-7 : DeclarationList -> AbsTypingTable *)

(* 3 *)
datatype TypeValue = NoDeclaration | DeclaredInt | DeclaredBool ;

(* 4 *)
type AbsTypingTable = Variable -> TypeValue 

(* 5 *)
val AbsTypingTableNoDeclaration = (fn (x:Variable) => NoDeclaration)    

(* 6 *)             
fun NewAbsTypingTable(oldatt: AbsTypingTable)(a:Variable, TypeBool)
     =(fn (b:Variable)=> if b = a then DeclaredBool
                                   else oldatt(b)) |
     NewAbsTypingTable(oldatt: AbsTypingTable)(a:Variable, TypeInt)
     =(fn(b:Variable)=> if b = a then DeclaredInt
                                   else oldatt(b));

(* Step 6 Testing 
Had to add var bb to test for boolean since allDeclarations has none*)
val myAbsTypingTable1 = NewAbsTypingTable(AbsTypingTableNoDeclaration)(declaration_n);
myAbsTypingTable1 var_n;
myAbsTypingTable1 var_answer;

val var_bb  = S "bb";
val declaration_bb      = (var_bb, TypeBool);

val myAbsTypingTable2 = NewAbsTypingTable(myAbsTypingTable1)(declaration_bb);
myAbsTypingTable2 var_n;
myAbsTypingTable2 var_bb;
myAbsTypingTable2 var_answer;

(* 7 *)
val rec wholeAbsTypingTable =
     (fn ((decListhead:: decListTail):DeclarationList)=>
          NewAbsTypingTable(wholeAbsTypingTable(decListTail) )(decListhead) |
          ([]) => AbsTypingTableNoDeclaration
     );

(*Step 7 Testing 
Every var in allDecs should output its' type, any var not in allDecs should be no dec*)
val myAbsTypingTable = wholeAbsTypingTable(allDeclarations);
myAbsTypingTable(var_n);
myAbsTypingTable(var_cur);
myAbsTypingTable(var_prev1);
myAbsTypingTable(var_prev2);
myAbsTypingTable(var_i);
myAbsTypingTable(var_answer);
myAbsTypingTable(var_temp);

(* 8 DetermineExpType: Expression -> AbsTypingTable -> TypeValue*)
fun DetermineExpType(Var(x)) = (fn(y:AbsTypingTable) => y(x)) |
     DetermineExpType(IC(x)) = (fn(y:AbsTypingTable) => DeclaredInt) |
     DetermineExpType(BC(x)) = (fn(y:AbsTypingTable) => DeclaredBool) |
     DetermineExpType(EEO(x1, x2, AOp(opa) ) ) =
                              (fn(y:AbsTypingTable) => DeclaredInt) |
     DetermineExpType(EEO(x1, x2, ROp(opa) ) ) =
                              (fn(y:AbsTypingTable) => DeclaredBool) |
     DetermineExpType(EEO(x1, x2, BOp(opa) ) ) =
                              (fn(y:AbsTypingTable) => DeclaredBool);

(* 9 ExpressionVCheck: Expression -> AbsTypingTable -> Bool*)
fun ExpressionVCheck(Var(a)) = (fn(b:AbsTypingTable) => b(a) <> NoDeclaration) |
     ExpressionVCheck(IC(a)) = (fn(b:AbsTypingTable) => true) |
     ExpressionVCheck(BC(a)) = (fn(b:AbsTypingTable) => true) |
     ExpressionVCheck(EEO(a1, a2, AOp(opa))) =
          (fn(b:AbsTypingTable) => ExpressionVCheck(a1)(b)  andalso
          (DetermineExpType(a1)(b) = DeclaredInt ) andalso
          ExpressionVCheck(a2)(b) andalso  
          (DetermineExpType(a2)(b) = DeclaredInt))  | 
     ExpressionVCheck(EEO(a1, a2, ROp(opa))) =
          (fn(b:AbsTypingTable) => 
               ExpressionVCheck(a1)(b)  andalso
         (DetermineExpType(a1)(b) = DeclaredInt ) andalso
          ExpressionVCheck(a2)(b) andalso  
          (DetermineExpType(a2)(b) = DeclaredInt))  | 
     ExpressionVCheck(EEO(a1, a2, BOp(opa))) =
          (fn(b:AbsTypingTable) => 
               ExpressionVCheck(a1)(b)  andalso
          (DetermineExpType(a1)(b) = DeclaredBool ) andalso
          ExpressionVCheck(a2)(b) andalso  
          (DetermineExpType(a2)(b) = DeclaredBool));

(*Test 9 Good in order*)
val and_t_t = EEO (BC true, BC true, BOp And);

val nExpression = Var (var_n);
val myExpressionVCheck = ExpressionVCheck(nExpression)(myAbsTypingTable);

val iCExpression = IC (0);
val myExpressionVCheck = ExpressionVCheck(iCExpression)(myAbsTypingTable);

val bCExpression = BC (false);
val myExpressionVCheck = ExpressionVCheck(bCExpression)(myAbsTypingTable);

val myExpressionVCheck = ExpressionVCheck(add_i_1)(myAbsTypingTable);
val myExpressionVCheck = ExpressionVCheck(i_lessthan_n)(myAbsTypingTable);
val myExpressionVCheck = ExpressionVCheck(and_t_t)(myAbsTypingTable);


(*Test 9 Bad (5 - 6) - (8 + t)*)
val minus_5_6 = EEO (IC 5, IC 6, AOp Minus);
val add_8_t = EEO (IC 8, BC true, AOp Plus);
val minus_56_8t = EEO (minus_5_6, add_8_t, AOp Minus);
val myExpressionVCheck = ExpressionVCheck(minus_56_8t)(myAbsTypingTable);


(*10 InstructionVCheck: AbsTypingTable -> Instruction -> Bool*)
val rec InstructionVCheck =
     (fn (a:AbsTypingTable) =>
          (fn (Skip) => true  |
          ( VE(x, y)) => 
               (a(x) = DetermineExpType(y)(a) ) andalso
               (a(x) <> NoDeclaration) andalso
               ExpressionVCheck(y)(a)   | 
          ( IfThenElse(x,y,z)) =>
               (DetermineExpType(x)(a) = DeclaredBool) andalso
               ExpressionVCheck(x)(a) andalso
               InstructionVCheck(a)(y) andalso InstructionVCheck(a)(z)  |
          ( WhileLoop(x,y)) => 
               (DetermineExpType(x)(a) = DeclaredBool) andalso
               ExpressionVCheck(x)(a) andalso
               InstructionVCheck(a)(y)  |
          ( Seq([])) => true  |
          ( Seq(InstListHead :: InstListTail))  =>
               InstructionVCheck(a)(InstListHead) andalso
               InstructionVCheck(a)(Seq(InstListTail)) 
          )
     );

(* ****testing part 10, 4-good cases **** *)
val ItestGood_1 = InstructionVCheck myAbsTypingTable cur_prev1_plus_prev2
val ItestGood_2 = InstructionVCheck myAbsTypingTable inner_ifThenElse
val ItestGood_3 = InstructionVCheck myAbsTypingTable ipp
val ItestGood_4 = InstructionVCheck myAbsTypingTable (Seq inner_Else)

(* ****testing part 10, 1-bad case**** *)
val badInstruction   = VE (var_n, BC true)  
val ItestBad_1 = InstructionVCheck myAbsTypingTable badInstruction

(* 11. checkvalidity fn Program*)
fun ProgramVCheck(a,b) =
     DecListVCheck(a) andalso
     InstructionVCheck(wholeAbsTypingTable(a))(b);

(*  ****testing part 11, 1-good case, apply to sample Program
     if false then wrong in (function validity) or (program code)*)
val test11_good1 = ProgramVCheck(lucas);

(* ****testing part 11, 1-bad case*)
val bad_program = (allDeclarations, badInstruction);
val test11_bad1 = ProgramVCheck(bad_program);

(*------------End step2 static sementics---------------*)




(*------------Start step3 Dynamic sementics---------------*)
(* sml<Sec1Proj1Team1Step3.sml>Sec1Proj1Team1Step3Result.txt   *)

(* 1 ValueInAbsProgState *)
datatype ValueInAbsProgState = ValueUnknown | ValueInt of int | ValueBool of bool

(* 2 AbsProgState: Variable -> ValueInAbsProgState*)
type AbsProgState = Variable -> ValueInAbsProgState

(* 3 AbsProgStateUnknown: Variable -> ValueInAbsProgState *)
val AbsProgStateUnknown = (fn(x:Variable) =>  ValueUnknown) 

(* 4 NewAbsProgState: Variable x ValueInAbsProgState -> AbsProgState -> Variable -> ValueInAbsProgState *)
fun NewAbsProgState (a:Variable, b:ValueInAbsProgState) (oldaps:AbsProgState) (c:Variable) =
      if c = a then b else oldaps c

(* 5 - 8 Exceptions *)
exception WrongDivision
exception WrongOpForValueInt
exception WrongOpForValueBool
exception WrongExpression

(* 9 ExpCalculation: ValueInAbsProgState x ValueInAbsProgState -> operator -> ValueInAbsProgState *)
val ExpCalculation =
	(fn(ValueInt v1, ValueInt v2) =>
		(fn(AOp Plus)  => ValueInt(v1 + v2) 
		   | (AOp Minus) => ValueInt(v1 - v2) 
		   | (AOp Times) => ValueInt(v1 * v2) 
           | (AOp Div)   => if v2 = 0 
                            then raise WrongDivision
                            else ValueInt(v1 div v2)  
           | (ROp Gt) => ValueBool(v1 > v2)    
		   | (ROp Lt) => ValueBool(v1 < v2)    
		   | (ROp Eq) => ValueBool(v1 = v2)    
		   | (ROp Ne) => ValueBool(v1 <> v2)    
		   | (ROp Ge) => ValueBool(v1 >= v2)    
		   | (ROp Le) => ValueBool(v1 <= v2)      
           | (_) => raise WrongOpForValueInt
        ) 
      
        | (ValueBool v1, ValueBool v2) =>
		(fn(BOp And) => ValueBool(v1 andalso v2) 
		   | (BOp Or)  => ValueBool(v1 orelse v2)  
		   | (_) => raise WrongOpForValueBool
        ) 
           
        | (_, _) => (fn(_) => raise WrongExpression)
    )

(* 10 ExpressionValue: Expression -> AbsProgState -> ValueInAbsProgState *)
fun ExpressionValue (Var x) (aps:AbsProgState) = aps(x) 
                    | ExpressionValue (IC x) (aps:AbsProgState) = ValueInt(x) 
                    | ExpressionValue (BC x) (aps:AbsProgState) = ValueBool(x) 
                    | ExpressionValue (EEO (a, b, c)) (aps:AbsProgState) = 
                    ExpCalculation (ExpressionValue a aps, ExpressionValue b aps) (c)

(* 11 MeaningInstruction: Intruction -> AbsProgState (Old) -> AbsProgState (New) *)
val rec MeaningInstruction = 
    (fn(Skip) => (fn(aps:AbsProgState) => aps)
        | (VE (a, b)) => (fn(aps:AbsProgState) => NewAbsProgState (a, ExpressionValue b aps) (aps))
        | (IfThenElse (a, b, c)) => (fn(aps:AbsProgState) => if ExpressionValue a aps = ValueBool(true)
                                                             then MeaningInstruction b aps
                                                             else MeaningInstruction c aps)
        | (WhileLoop (a, b)) => (fn(aps:AbsProgState) => if ExpressionValue a aps <> ValueBool(true)
                                                         then aps
                                                         else MeaningInstruction (WhileLoop (a, b))
                                                                (MeaningInstruction b aps))
        | Seq [] => (fn(aps:AbsProgState) => aps)
        | Seq (InstListHead::InstListTail) => (fn(aps:AbsProgState) => MeaningInstruction (Seq InstListTail)
                                                                        (MeaningInstruction InstListHead aps))
    )
    
(* 12 Exception *)
exception InvalidProgram

(* 13 MeaningProgram: Program -> AbsProgState *)
val MeaningProgram = 
    (fn((a,b): Program) => if ProgramVCheck(a,b)
                           then MeaningInstruction b AbsProgStateUnknown
                           else raise InvalidProgram
    )

(*------------End step3 Dynamic sementics---------------*)