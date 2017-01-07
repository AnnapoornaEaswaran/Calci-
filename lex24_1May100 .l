%{
#include<stdio.h>
#include<stdlib.h>
#include<string.h>
#include"node.h"
#include"y.tab.h"
extern YYSTYPE yylval;
int INDEX=0,line=1;
struct symtable
{
	char name[20],ntype[11],navp[11];
	int type,avp,index;
	struct symtable *next;
} *head=NULL,*end=NULL,*temp;

void display()
{
	printf("\nThis is from a.c file");
}
int insert1(char id_name[20])
{
	int i=0;
	//printf("\n%c",id_name[0]);
	if(head!=NULL)
	{
	temp=head;
	do
	{
		if(strcmp(temp->name,id_name)==0)
			{
				//printf("\nDuplicate id ");
				i=temp->index;
				return i;
			}
		temp=temp->next;
	}while(temp!=NULL);
	}
	struct symtable *new_node=(struct symtable*)malloc(sizeof(struct symtable));
	new_node->next=NULL;
	new_node->avp=-1;
	new_node->type=-1; 
	strcpy(new_node->ntype,"Undefined");
	strcpy(new_node->navp,"Undefined");
	new_node->index=INDEX;
	INDEX++;
	strcpy(new_node->name,id_name);
	
  if(head==NULL)
  {
  	head=new_node;
  	end=new_node;
  }
  else
  {
  	end->next=new_node;
  	end=new_node;
  }
  return INDEX-1;
  
}
int getType(int key)
{
	int t=-1;
	if(head!=NULL)
	{
		temp=head;
		while(temp!=NULL)
		{
			if(temp->index==key)
			{
				t=temp->type;
				break;
			}
			temp=temp->next;
		}
	}
	return t;
	
}
int getAVP(int key)
{
	int a=-1;
	if(head!=NULL)
	{
		temp=head;
		while(temp!=NULL)
		{
			if(temp->index==key)
			{
				a=temp->avp;
				break;
			}
			temp=temp->next;
		}
	}
	return a;
	
}
void setType(int key, int t)
{
	int i=0;
	temp=head;
	while(temp!=NULL)
	{
		if(temp->index==key)
		{
			temp->type=t;
			//printf("\nType is set!");
			if(temp->type==0)
				strcpy(temp->ntype,"int");
			else if(temp->type==1)
				strcpy(temp->ntype,"float");
			break;
		}
		temp=temp->next;
	}
}
void setAVP(int key, int t)
{
	int i=0;
	temp=head;
	while(temp!=NULL)
	{
		if(temp->index==key)
		{
			temp->avp=t;
			//printf("\nAVP is set!");
			if(temp->avp==0)
				strcpy(temp->navp,"variable");
			else if(temp->avp==1)
				strcpy(temp->navp,"array");
			break;
		}
		temp=temp->next;
	}
}
void printtable()
{
	printf("The table: \n");
	if(head!=NULL)
	{
	temp=head;
	do
	{
		printf("\nID name: %s Key: %d    Datatype : %s   Var or Array: %s",temp->name,temp->index, temp->ntype,temp->navp);	
		
		temp=temp->next;
	}while(temp!=NULL);
	}
}
int getLineNo()
{
	return line;
}
int num_c=0;
int letter_c=0;
int mod_c=0;
int op_c=0;
int keywords_c=0;
int lp_c=0;
int rp_c=0;
int lcp_c=0;
int rcp_c=0;
int hash_c=0;
int comma_c=0;
int delim_c=0;
int space_c=0;
int datatype_c=0;
int var_c=0;
int string_c=0;
int comment_c=0;
int decimal_c=0;
%}
enf ['$']
digit [0-9]
number ['\-']?{digit}+
decimal ['\-']?{digit}+.{digit}+
letter [a-zA-Z]
mod  ['%']
plus ['+']
minus ['\-'] 
mul ['*']
div ['/']
lp      ['(']
rp      [')']
lcp     ['\{']
rcp     ['\}']
lb      ['[']
rb      ['\]']
equals ['=']
hash    ['#']
comma   [',']
delim   [';']
space   [' ']
datatype   "int"|"float"|"long"
var         {letter}({letter}|{digit}|'_')*
string     ({letter}|{digit})*
literals	['"'].*['"']
comment    {hash}({string}|{space})*
print 		"print"
get 		"get"
and 		"and"
or 			"or"
not 		"not"
for_word	"for"
while_word	"while"
if_word 	"if"
else_word	"else"
relop		">"|"<"|"<="|">="|"=="|"!="
keywords   "main"
g_fun 	 "Add"|"Multiply"
d_fun 	 "Subtract"|"Divide"
j_fun 	 "Sin"|"Cos"|"Tan"
l_fun 	 "Log"|"Ln"
new_line  \n
%%
{enf} {return 0;}
{relop} {return RELOP;}
{g_fun} {return G;}
{j_fun} {return J1;}
{l_fun} {return L;}
{d_fun} {return D;}
{and} {return AND;}
{or} {return OR;}
{not} {return NOT;}
{number} {num_c++; return NUM;}
{decimal} {decimal_c++;return DECIMAL;}
{mod}    mod_c++;
{plus} { return PLUS;}
{minus} {return MINUS;}
{mul} { return MUL;}
{div} {return DIV;}
{print} {return PR;}
{get} {return GT;}
{for_word} {return FOR;}
{while_word} { return WHILE;}
{if_word} 	{ return IF;}
{else_word} { return ELSE;}
{keywords}	 {keywords_c++;return KEYWORDS;}
{lp}      { lp_c++; return '(';}
{rp}      { rp_c++; return ')';}
{lcp}     { lcp_c++; return '{';}
{rcp}     { rcp_c++; return '}';}
{lb}      {return '[';}
{rb}      {return ']';}
{equals}  {return '=';}
{comma}   { comma_c++; return ',';}
{delim}   { delim_c++; return ';';}
{space}    {space_c++; return SPACE;}
{datatype} {datatype_c++; if(yytext[0]=='i') 
			yylval.type=0;										
		    if(yytext[0]=='f')
			yylval.type=1;	 return DT;}
{var}      {var_c++; yylval.key=insert1(yytext); return ID;}
{literals}  { string_c++; return LIT;}
{comment}  {comment_c++; return COMMENT;}
{new_line} {line++;}

%%

