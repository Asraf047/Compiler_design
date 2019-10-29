/* C Declarations */

%{
	#include<stdio.h>
	#include<stdlib.h>
	#include <string.h>

	int switchdone = 0;
	int switchvar;

	int ifval[1000];
	int ifptr = -1;
	int ifdone[1000];

    int ptr = 0;
    int value[1000];
    char varlist[1000][1000];

    //if already declared  return 1 else return 0
    int isdeclared(char str[]){
        int i;
        for(i = 0; i < ptr; i++){
            if(strcmp(varlist[i],str) == 0) return 1;
        }
        return 0;
    }

    // if already declared return 0 or add new value and return 1;
    int addnewval(char str[],int val){
        if(isdeclared(str) == 1) return 0;
        strcpy(varlist[ptr],str);
        value[ptr] = val;
        ptr++;
        return 1;
    }

    //get the value of corresponding string
    int getval(char str[]){
        int indx = -1;
        int i;
        for(i = 0; i < ptr; i++){
            if(strcmp(varlist[i],str) == 0) {
                indx = i;
                break;
            }
        }
        return value[indx];
    }

    //set the value of corresponding string
    int setval(char str[], int val){
    	int indx = -1;
        int i;
        for(i = 0; i < ptr; i++){
            if(strcmp(varlist[i],str) == 0) {
                indx = i;
                break;
            }
        }
        value[indx] = val;
    }
%}


%union {
  char text[1000];
  int val;
}


%token <text>  ID
%token <val>  NUM
%token <text> STR

%type <val> expression

%left LT GT LE GE
%left PLUS MINUS
%left MULT DIV

%token INT DOUBLE CHAR MAIN PBS PBE SBS SBE SM CM ASGN PRINTVAR PRINTSTR PRINTLN PLUS MINUS MULT DIV LT GT LE GE IF ELSE ELSEIF FOR WHILE INC TO SWITCH DEFAULT COL FUNCTION
%nonassoc IFX
%nonassoc ELSE


%%

everything 	: function mainfuntion function { printf("\nCompilation Successful\n"); }
			;

mainfuntion	: INT MAIN PBS PBE SBS statement SBE 
			;
statement	: /* empty */
			| statement declaration
			| statement print
			| statement expression 
			| statement ifelse
			| statement assign
			| statement forloop
			| statement whileloop
			| statement switch
			;


declaration : tyPBE variables SM {}
			;
tyPBE		: INT | DOUBLE | CHAR {}
			;
variables	: variable CM variables {}
			| variable {}
			;
variable   	: ID 	
					{
						//printf("%s\n",$1);
						int x = addnewval($1,0);
						if(!x) {
							printf("Compilation Error:Variable %s is already declared\n",$1);
							exit(-1);
						}

					}
			| ID ASGN expression 	
					{
						//printf("%s %d\n",$1,$3);
						int x = addnewval($1,$3);
						if(!x) {
							printf("Compilation Error: Variable %s is already declared\n",$1);
							exit(-1);
							}
					}

			;


assign : ID ASGN expression SM  
					{
						if(!isdeclared($1)) {
							printf("Compilation Error: Variable %s is not declared\n",$1);
							exit(-1);
						}
						else{
							setval($1,$3);
						}
				    }


print		: PRINTVAR PBS ID PBE SM 	
					{
						if(!isdeclared($3)){
							printf("Compilation Error: Variable %s is not declared\n",$3);
							exit(-1);
						}
						else{
							int v = getval($3);
							printf("%d",v);
						}
					}
			| PRINTSTR PBS STR PBE SM 
					{
						int l = strlen($3);
						int i;
						for(i = 1;  i < l-1; i++) printf("%c",$3[i]);
					}
			| PRINTLN PBS PBE	SM 	
					{
						printf("\n");
					}
			;


expression : NUM 	{$$ = $1;}

			| ID 	
					{
						if(!isdeclared($1)) {
							printf("Compilation Error: Variable %s is not declared\n",$1);
							exit(-1);
						}
						else{
							$$ = getval($1);
						}
				 	}
			| expression PLUS expression 
					{$$ = $1 + $3;}
			| expression MINUS expression 
					{$$ = $1 - $3;}
			| expression MULT expression 
					{$$ = $1 * $3;}
			| expression DIV expression 
					{
						if($3) {
 							$$ = $1 / $3;
							}
				  		else {
							$$ = 0;
							printf("\nRuntime Error: division by zero\t");
							exit(-1);
				  		} 
					}
			| expression LT expression	
					{ $$ = $1 < $3; }
			| expression GT expression	
					{ $$ = $1 > $3; }
			| expression LE expression	
					{ $$ = $1 <= $3; }
			| expression GE expression	
					{ $$ = $1 >= $3; }
			| PBS expression PBE
					{ $$ = $2; }
			| INC expression INC
					{ 
						$$=$2+1; 
						//printf("inc: %d\n",$$);
					}
			;


ifelse 	: IF PBS ifexp PBE SBS statement SBE elseif
					{

						ifdone[ifptr] = 0;
						ifptr--;
					}
		;
ifexp	: expression 
					{
						ifptr++;
						ifdone[ifptr] = 0;
						//printf("if= %d\n",$1);
						if(($1 != 0)){
							printf("If expressin %d executed\n",ifptr);
							ifdone[ifptr] = 1;
						}
					}
		;
elseif 	: /* empty */

		| elseif ELSEIF PBS expression PBE SBS statement SBE 
					{
						//printf("%d =elseif= %d\n",$4,ifdone[ifptr]);
						if( ($4 != 0) && ifdone[ifptr] == 0){
							printf("Else if block expressin %d executed\n",ifptr);
							ifdone[ifptr] = 1;
						}
					}
		| elseif ELSE SBS statement SBE
					{
						if(ifdone[ifptr] == 0){
							printf("Else block expressin %d executed\n",ifptr);
							ifdone[ifptr] = 1;
						}
					}

		;


forloop	: FOR PBS expression TO expression INC expression PBE SBS statement SBE 	
					{
						int st = $3;
						int ed = $5;
						int dif = $7;
						int cnt = 0;
						int k = 0;
						for(k = st; k <= ed; k += dif){
							cnt++;
						}	
						printf("for loop executes %d times\n",cnt);
					}
		;

whileloop : WHILE PBS expression GT expression PBE SBS expression SBE   
					{
						int i;
						int cnt = 0;
						for(i=$3;i<=$5;i++)
						{
							cnt++;
						}
						printf("while loop executes %d times\n",cnt);
					}
			;


switch	: SWITCH PBS expswitch PBE SBS switchinside SBE 
		;

expswitch 	:  expression 
					{
						switchdone = 0;
						switchvar = $1;
					}
			;


switchinside	: /* empty */

				| switchinside expression COL SBS statement SBE 
					{
						if($2 == switchvar){
							printf("switch expressin %d executed\n",$2);
							switchdone = 1;
						}					
					}
				| switchinside DEFAULT COL SBS statement SBE 
					{
						if(switchdone == 0){
							switchdone = 1;
							printf("Default Block executed\n");
						}
					}
				;


function 	: /* empty */

			| function func
			;

func 		: tyPBE FUNCTION PBS fparameter PBE SBS statement SBE
				{
					printf("Function Declared\n");
				}
			;

fparameter 	: /* empty */

			| tyPBE ID fsparameter
			;

fsparameter : /* empty */

			| fsparameter CM tyPBE ID
			;

%%


int yyerror(char *s){
	printf( "%s\n", s);
}
