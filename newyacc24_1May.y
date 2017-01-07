%{
#include<stdio.h>
#include<string.h>
#include"node.h"
int alif=0;
int dttype=0;
%}
%token NUM COMMENT PLUS MINUS MUL DIV SPACE DECIMAL G J1 L D KEYWORDS PR LIT GT AND OR NOT RELOP FOR WHILE IF ELSE
%token <ky> ID 
%token <ty> DT 
%type <ty> Data EXP MATHEXP FCALLS ARITH TRIG H NUM1 K LOG PAR T factor H1
%type <arg> VAR1 VAR
%%
STARTSTMT: STMT 
STMT :'(' STMT ')' STMT
	|'{' STMT '}' STMT
	|COMMENT      {printf("\nComment encountered!");}
	|VARDECL	  {printf("\nVariable declaration");}
	|FCALLS STMT  {printf("\nFunction Call");}
	|FCALLS		  {printf("\nFunction Call");}
	|COMMENT STMT {printf("\nComment encountered!");}
	|VARDECL STMT {printf("\nVariable declaration");}
	|'(' STMT ')' 
	|'{' STMT '}' 
	| IO STMT {printf("\nIO statement encountered!");}
	| IO {printf("\nIO statement encountered!");}
	| LOOP STMT 
	| LOOP	
	|SELECTION STMT ELSESELECT {printf("\nSelection statement encountered!");}
	|ASSIGN STMT {printf("\nAssign statement ");}
	|ASSIGN {printf("\nAssign statement ");}
VARDECL: Data Sp A1 ';'{ printf("\n Variable declaration statement!");}
Data:DT {	$<ty.type>$=$<ty.type>1;
			dttype=$<ty.type>1;
			printf("\n The type of the variable line is %d:",$<ty.type>$);}
Sp:SPACE
ASSIGN:VAR '=' EXP ';' { if($<arg.type>1!=$<ty.type>3)
							printf("\nerror at line : %d incompatible types",getLineNo());
						 if($<arg.type>1==-1)
						 	printf("\nerror at line : %d destination variable is of invalid type",getLineNo());
						 if($<ty.type>3==-1)
						 	printf("\nerror at line : %d invalid expression type",getLineNo());
						}
							
LOOP: FORLOOP | WHILELOOP
FORLOOP: FOR '(' INIT ';' CONDITION ';' INC ')' {printf("\nFOR Loop statement encountered!");}
WHILELOOP: WHILE '(' CONDITION ')'  {printf("\nWHILE Loop statement encountered!");}
SELECTION: IF '(' CONDITION ')' 
ELSESELECT: ELSE STMT 
INIT : VAR '=' NUM
CONDITION: CONDITION OR NC|NC
NC: NC AND N|N
N: NOT LOGICAL|LOGICAL
LOGICAL : '(' CONDITION ')'| VAR RELOP MATHEXP
INC: PLUS PLUS VAR | MINUS MINUS VAR| VAR PLUS PLUS | VAR MINUS MINUS
IO: PrintEXP | GetEXP
GetEXP : GT '(' A ')' ';'
PrintEXP:  PR '(' B ')' ';'
B: LIT PLUS B | LIT | VAR |VAR PLUS B 
EXP: FCALLS 	{$<ty.type>$=$<ty.type>1;}
	|MATHEXP	{$<ty.type>$=$<ty.type>1;}
FCALLS: ARITH {$<ty.type>$=$<ty.type>1;}
		|TRIG {$<ty.type>$=$<ty.type>1;}
		|LOG  {$<ty.type>$=$<ty.type>1;}
ARITH: AorM '(' H ')' { if($<ty.type>3==0||$<ty.type>3==1)
						$<ty.type>$=$<ty.type>3;
					   else
						   {
						   		$<ty.type>$=-1;
						   		//printf("\nerror at line : %d Function has invalid arguments",getLineNo());
						   }
					 }
		| SorD '(' H1 ')' { if($<ty.type>3==0||$<ty.type>3==1)
						$<ty.type>$=$<ty.type>3;
					   else
						   {
						   		$<ty.type>$=-1;
						   		//printf("\nerror at line : %d Function has invalid arguments",getLineNo());
						   }
					 }
					 
AorM: G
SorD: D
H : NUM1 ',' NUM1 { if(($<ty.type>1==0) &&($<ty.type>3==0))
						$<ty.type>$=0;
				   else if ((($<ty.type>1==1) &&($<ty.type>3==0))||(($<ty.type>1==0) &&($<ty.type>3==1))||(($<ty.type>1==1) &&($<ty.type>3==1)))
						$<ty.type>$=1;
				   else
				   {
				   		$<ty.type>$=-1;
				   		printf("\nerror at line : %d Function has invalid arguments",getLineNo());
				   }		
				 }
	|NUM1 ',' H	 { if(($<ty.type>1==0) &&($<ty.type>3==0))
						$<ty.type>$=0;
				   else if ((($<ty.type>1==1) &&($<ty.type>3==0))||(($<ty.type>1==0) &&($<ty.type>3==1))||(($<ty.type>1==1) &&($<ty.type>3==1)))
						$<ty.type>$=1;
				   else
				   {
				   		$<ty.type>$=-1;
				   		printf("\nerror at line : %d Function has invalid arguments",getLineNo());
				   }		
				 }
	|VAR ',' H	{ if(($<arg.type>1==0) &&($<ty.type>3==0))
						$<ty.type>$=0;
				   else if ((($<ty.type>1==1) &&($<ty.type>3==0))||(($<ty.type>1==0) &&($<ty.type>3==1))||(($<ty.type>1==1) &&($<ty.type>3==1)))
						$<ty.type>$=1;
				   else
				   {
				   		$<ty.type>$=-1;
				   		printf("\nerror at line : %d Function has invalid arguments",getLineNo());
				   }		
				 }
	|VAR ',' VAR { if(($<arg.type>1==0) &&($<arg.type>3==0))
						$<ty.type>$=0;
				else if ((($<arg.type>1==1) &&($<arg.type>3==0))||(($<arg.type>1==0) &&($<arg.type>3==1))||(($<arg.type>1==1) &&($<arg.type>3==1)))
						$<ty.type>$=1;
				   else
				   {
				   		$<ty.type>$=-1;
				   		printf("\nerror at line : %d Function has invalid arguments",getLineNo());
				   }		
				 }

H1 : NUM1 ',' NUM1 { if(($<ty.type>1==0) &&($<ty.type>3==0))
						$<ty.type>$=0;
				   else if ((($<ty.type>1==1) &&($<ty.type>3==0))||(($<ty.type>1==0) &&($<ty.type>3==1))||(($<ty.type>1==1) &&($<ty.type>3==1)))
						$<ty.type>$=1;
				   else
				   {
				   		$<ty.type>$=-1;
				   		printf("\nerror at line : %d Function has invalid arguments",getLineNo());
				   }		
				 }
	
	|VAR ',' VAR { if(($<arg.type>1==0) &&($<arg.type>3==0))
						$<ty.type>$=0;
				else if ((($<arg.type>1==1) &&($<arg.type>3==0))||(($<arg.type>1==0) &&($<arg.type>3==1))||(($<arg.type>1==1) &&($<arg.type>3==1)))
						$<ty.type>$=1;
				   else
				   {
				   		$<ty.type>$=-1;
				   		printf("\nerror at line : %d Function has invalid arguments",getLineNo());
				   }		
				 }

NUM1: NUM { $<ty.type>$=0;}
	|DECIMAL { $<ty.type>$=1;}
TRIG: J1'('K')' { if($<ty.type>3==0||$<ty.type>3==1)
						$<ty.type>$=1;
				  else
				   {
				   		$<ty.type>$=-1;
				   		printf("\nerror at line : %d Function has invalid arguments",getLineNo());
				   }
				}
K : MATHEXP { $<ty.type>$=$<ty.type>1; }
LOG:L '(' PAR ')' { if($<ty.type>3==0||$<ty.type>3==1)
						$<ty.type>$=1;
				  else
				   {
				   		$<ty.type>$=-1;
				   		printf("\nerror at line : %d Function has invalid arguments",getLineNo());
				   }
				}
MATHEXP : MATHEXP PLUS T { if(($<ty.type>1==0) &&($<ty.type>3==0))
						$<ty.type>$=0;
				   else if ((($<ty.type>1==1) &&($<ty.type>3==0))||(($<ty.type>1==0) &&($<ty.type>3==1))||(($<ty.type>1==1) &&($<ty.type>3==1)))
						$<ty.type>$=1;
				   else
				   {
				   		$<ty.type>$=-1;
				   		printf("\nerror at line : %d invalid types used in expression.",getLineNo());
				   }		
				 }
	|MATHEXP MINUS T { if(($<ty.type>1==0) &&($<ty.type>3==0))
						$<ty.type>$=0;
				   else if ((($<ty.type>1==1) &&($<ty.type>3==0))||(($<ty.type>1==0) &&($<ty.type>3==1))||(($<ty.type>1==1) &&($<ty.type>3==1)))
						$<ty.type>$=1;
				   else
				   {
				   		$<ty.type>$=-1;
				   		printf("\nerror at line : %d invalid types used in expression.",getLineNo());
				   }		
				 }
	|T 			{ $<ty.type>$=$<ty.type>1;}
PAR: VAR {$<ty.type>$=$<arg.type>1;}
	|NUM {$<ty.type>$=$<ty.type>1;}
	|DECIMAL {$<ty.type>$=$<ty.type>1;}	
T :T MUL factor { if(($<ty.type>1==0) &&($<ty.type>3==0))
						$<ty.type>$=0;
				   else if ((($<ty.type>1==1) &&($<ty.type>3==0))||(($<ty.type>1==0) &&($<ty.type>3==1))||(($<ty.type>1==1) &&($<ty.type>3==1)))
						$<ty.type>$=1;
				   else
				   {
				   		$<ty.type>$=-1;
				   		printf("\nerror at line : %d invalid types used in expression.",getLineNo());
				   }		
				 }
	|T DIV factor { if(($<ty.type>1==0) &&($<ty.type>3==0))
						$<ty.type>$=0;
				   else if ((($<ty.type>1==1) &&($<ty.type>3==0))||(($<ty.type>1==0) &&($<ty.type>3==1))||(($<ty.type>1==1) &&($<ty.type>3==1)))
						$<ty.type>$=1;
				   else
				   {
				   		$<ty.type>$=-1;
				   		printf("\nerror at line : %d invalid types used in expression.",getLineNo());
				   }		
				 }
	|factor { $<ty.type>$=$<ty.type>1;}

factor:NUM	{ $<ty.type>$=0;}
	|VAR	{ $<ty.type>$=$<arg.type>1;}
	|DECIMAL { $<ty.type>$=1;}
	|'(' MATHEXP ')' { $<ty.type>$=$<ty.type>2;}

A : VAR ',' A
	|VAR
VAR: ID '[' NUM ']'{ 		$<arg.key>$=$<ky.key>1;
							$<arg.type>$=getType($<arg.key>$);
							
							if($<arg.type>$==-1)
							{
								printf("\nerror at line : %d Variable not defined",getLineNo());
								//yyerror();
							}
							else
							{
								if(getAVP($<arg.key>$)==1)
								{}
								else
								{
									printf("\nerror at line : %d Variable is not an array!",getLineNo());
									//yyerror();
								}
							}
							
					}
	|ID				{ 		$<arg.key>$=$<ky.key>1;
							$<arg.type>$=getType($<arg.key>$);
							if($<arg.type>$==-1)
							{
								printf("\nerror at line : %d Variable not defined",getLineNo());
								//yyerror();
							}
							else
							{
								if(getAVP($<arg.key>$)==0)
								{}
								else
								{
									printf("\nerror at line : %d Variable is not an properly used!",getLineNo());
									//yyerror();
								}
							}
					}

A1 : VAR1 ',' A1 {				
							$<arg.type>1=dttype;
							//printf("\n VAR, A : type of the variable is :%d",$<arg.type>1);
							setType($<arg.key>1,$<arg.type>1);
			  }
	|VAR1	  {
							$<arg.type>1=dttype;
							//printf("\n (single var)type of the variable is :%d",$<arg.type>1);
							setType($<arg.key>1,$<arg.type>1);
		
			  }
VAR1: ID '[' NUM ']' { 		$<arg.key>$=$<ky.key>1;
							setAVP($<arg.key>1,1);
							printf("\nthe key of the variable is :%d",$<arg.key>$);
					}
	|ID		{ 				$<arg.key>$=$<ky.key>1;
							setAVP($<arg.key>1,0);
							printf("\nthe key of the variable is :%d",$<arg.key>$);
			}
%%
yyerror()
{
printf("\nSyntax Error encountered at line : %d\n",getLineNo());
}
main()
{
if(yyparse()==0)
printf("\nSuccess\n");
printtable();
}
