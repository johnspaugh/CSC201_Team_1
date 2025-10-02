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
          answer =2;
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