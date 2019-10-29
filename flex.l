digit [0-9]
char [a-zA-Z]
special [!@#$%^&]
nl "\n"
sp " "

%{
	#include "bison.tab.h"
	#include <string.h>
%}

%%
{sp}|{nl} {}
"|" {return SM;}
"-cm" {return CM;}
{digit}+ 	{
				yylval.val = atoi(yytext);
				return NUM;
			}
"fun-main" { return MAIN;}
"print-v" { return PRINTVAR;}
"print-s" {return PRINTSTR;}
"print-n" {return PRINTLN;}

"fun-"({char}|{digit}|{special})+ {return FUNCTION;}

"v-"({char}|{digit}|{special})+ { strcpy(yylval.text,yytext);return ID;}

"-int" {  return INT;}
"-flo" { return DOUBLE;}
"-cha" { return CHAR;}

"-pbs" { return PBS;}
"-pbe" { return PBE;}

"-sbs" { return SBS;}
"-sbe" { return SBE;}

"-eq" {return ASGN;}
"-pl" {return PLUS;}
"-mi" {return MINUS;}
"-mu" {return MULT;}
"-di" {return DIV;}
"-lt" {return LT;}
"-gt" {return GT;}
"-le" {return LE;}
"-ge" {return GE;}

"check" { return IF;}
"recheck" {return ELSEIF;}
"atleast" {return ELSE;}

"circle" {return FOR;}
"-inc" {return INC;}
"-to" {return TO;}

"ring" {return WHILE;}

"shift" {return SWITCH;}
"/" {return COL;}
"-def" {return DEFAULT;}

[ \t\n]*	

. {yyerror("Unknown token found.\n");}

"\""(.)*"\"" {strcpy(yylval.text,yytext);return STR;}

%%

int yywrap(){
	return 1;
}

int main(){
	freopen("programs.txt","r",stdin);
	freopen("output.txt","w",stdout);
	yyparse();

// "-dec" {return INC;}
}