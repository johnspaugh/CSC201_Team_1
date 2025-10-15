(* CSC201, Section 1, TEAM 1, 
    John Spaugh,
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

(*Step 2*)

fun NoDuplicate((a: Variable, b: Type):: decListTail)(c: Variable) = (a <> c) andalso NoDuplicate decListTail c
  | NoDuplicate [] c = true;

val rec DecListVCheck =
     (fn ( ((a: Variable, b: Type):: decListTail):DeclarationList) =>
          DecListVCheck(decListTail) andalso
          NoDuplicate(decListTail)(a) |
          ([]) => true  
     ) ;

(*Good testing step 2, input allDeclarations list with int into DecListVCheck
test1 expected false, Gtest1 expected true*)

val test1 = NoDuplicate(allDeclarations )(var_answer); 
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

(* ***Testing 5 one apply for variables for the declaration of all apply *)
val Gtest5_1 = AbsTypingTableNoDeclaration(var_answer);

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

(* 8 DetermineExpType: Expression -> AbsTypingTable -> TypeValue
fun DetermineExpType(EDC1(x)) = (fn(y:AbsTypingTable)=>y(x)) |
     DetermineExpType(EDC2=(x)) => (fn(y:AbsTypingTable) => DeclaredInt) |
     DetermineExpType(EDC3=(x)) => (fn(y:AbsTypingTable) => DeclaredBool) |
     DetermineExpType(EDC4=(x1,x2,ODC1(opa)))=>
                              (fn(y:AbsTypingTable)=>DeclaredInt) |
     DetermineExpType(EDC5=(x1,x2,ODC1(opa)))=>
                              (fn(y:AbsTypingTable)=>DeclaredBool) |
     DetermineExpType(EDC6=(x1,x2,ODC1(opa)))=>
                              (fn(y:AbsTypingTable)=>DeclaredBool) |
     ------realational--boolean
*)

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
