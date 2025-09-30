/*   
Sample 1. 
The Lucas number is defined to be the sum of its two immediate previous terms:       
L(0) = 2     
     L(1) = 1     
     L(n) = L(n-1)+L(n-2)  n>1 
In this sample program, define n = 15, and the final answer should be 1364.  
*/ 
 /*
PROGRAM lucas 
{ 
 
 n  Integer; 
 cur  Integer;  
 prev1   Integer; 
 prev2   Integer; 
 i  Integer;  
 answer   Integer;  
   
 
 n = 15; 
 
 
 IF (n Eq 0) THEN  answer = 2 
 ELSE 
 { 
  i = 1; 
       IF (n Eq 1) THEN   answer = 1 
       ELSE 
  { 
   prev2 = 2; 
   prev1 = 1; 
   WHILE (i Lt n) 
   { 
    i = i Plus 1; 
    cur = prev1 Plus prev2; 
    prev2 = prev1; 
    prev1 = cur;     
   }  
   answer = cur; 
    } 
 } 
 
} 
*/