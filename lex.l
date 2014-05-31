/**
 * Introduction to Compiler Design by Prof. Yi Ping You
 * Project 1 Sample
 */

%{

#include <string.h>

#define LIST strcat( buf, yytext )
#define token(t) { LIST; if (Opt_T) printf("<%s>\n", #t );}
#define tokenChar(t) { LIST; if (Opt_T) printf("<%c>\n", t );}
#define tokenInteger(t, i) { LIST; if (Opt_T) printf("<%s: %d>\n", t, i); }
#define tokenOctInteger(t, i) { LIST; if (Opt_T) printf("<%s: %s>\n", t, i); }
#define tokenFloat(t, i) { LIST; if (Opt_T) printf("<%s: %s>\n", t, i ); }
#define tokenScientific(t, s) { LIST; if ( Opt_T ) printf("<%s: %s>\n", t, s); }
#define tokenString(t, s) { LIST; if (Opt_T) printf("<%s: %s>\n", t, s); }

#include "container.h"
#include "y.tab.h"
#define  MAX_LINE_LENG  256

int Opt_S = 1;				/* list source code option */
int Opt_T = 1;				/* list token option */
int Opt_D = 0;
int linenum = 1;			/* line number */
char buf[MAX_LINE_LENG];		/* source code line buffer */
char strContent[MAX_LINE_LENG];		/* string buffer */

void setPseudoComment( const char * );	/* pseudo comment option switch */
void dumpsymbol();
char* stringCopy(char* );

%}

whitespace [ \t]+
letter [A-Za-z_]
digit [0-9]
integer 0|[1-9][0-9]*
octal 0[0-7]+
fraction 0|[0-9]*
float {integer}"."{fraction}
scientific ({integer}|{float}|{octal})[Ee][+-]?({integer}|{octal})
identifier ({letter})({letter}|{digit})*

 /* exclusive start conditions */
%x STR
%x COMMENT
%x COMMENT_CPP

%%

","				{ tokenChar(','); return COMMA;}
";"				{ tokenChar(';'); return SEMICOLON;}
":"				{ tokenChar(':'); return COLON;}
")"				{ tokenChar(')'); return RROUND;}
"("				{ tokenChar('('); return LROUND;}
"["				{ tokenChar('['); return LSQ;}
"]"				{ tokenChar(']'); return RSQ;}

"+"				{ tokenChar('+'); return ADD;}
"-"				{ tokenChar('-'); return SUB;}
"*"				{ tokenChar('*'); return MUL;}
"/"				{ tokenChar('/'); return DIV;}
"mod"				{ token(mod); return MOD;}
":="				{ token(:=); return ASSIGN;}
"<"				{ tokenChar('<'); return GT;}
"<="				{ token(<=); return GE;}
"<>"				{ token(<>); return NE;}
">="				{ token(>=); return LE;}
">"				{ tokenChar('>'); return LT;}
"="				{ tokenChar('='); return EQ;}
"and"				{ token(and); return AND;}
"or"				{ token(or); return OR;}
"not"				{ token(not); return NOT;}

"array"				{ token(KWarray); return ARRAY;}
"begin"				{ token(KWbegin); return BGN;}
"boolean"			{ token(KWboolean); return BOOLEAN;}
"def"				{ token(KWdef); return DEF;}
"do"				{ token(KWdo); return DO;}
"else"				{ token(KWelse); return ELSE;}
"end"				{ token(KWend); return END;}
"false"				{ token(KWfalse); return FALSE;}
"for"				{ token(KWfor); return FOR;}
"integer"			{ token(KWinteger); return INTEGER;}
"if"				{ token(KWif); return IF;}
"of"				{ token(KWof); return OF;}
"print"				{ token(KWprint); return PRINT;}
"read"				{ token(KWread); return READ;}
"real"				{ token(KWreal); return REAL;}
"string"			{ token(KWstring); return STRING;}
"then"				{ token(KWthen); return THEN;}
"to"				{ token(KWto); return TO;}
"true"				{ token(KWtrue); return TRUE;}
"return"			{ token(KWreturn); return RETURN;}
"var"				{ token(KWvar); return VAR;}
"while"				{ token(KWwhile); return WHILE;}

{octal} { 
				tokenOctInteger("oct_integer",yytext); 
				yylval.X.V.value = atoi(yytext);
				yylval.X.type = 0;
				return OCT;
			}
{identifier}			{ 
								tokenString("id",yytext);
								yylval.X.V.text = stringCopy(yytext);
								return IDENT;
							}
{integer}			{ 
							tokenInteger("integer",atoi(yytext)); 
							yylval.X.V.value = atoi(yytext);
							yylval.X.type = 0;
							return DIGIT;
						}
{float}				{ 
							tokenFloat("float",yytext); 
							yylval.X.V.dvalue = atof(yytext);
							yylval.X.type = 1;
							return REALNUM;
						}
{scientific}		{
							tokenScientific("scientific", yytext); 
							yylval.X.V.dvalue = atoi(yytext);
							yylval.X.type = 1;
							return SCIENCE;
						}

"\""				{ /* begin string  */
				  LIST;
				  BEGIN STR; 
				}

<STR>([^"\n])*			{
				  LIST;
				  strcat( strContent, yytext );
				}

<STR>"\""			{ /* end of string or "" */
				  char c;
				  c = input();
				  if( c == '\"' )	/* "" */
				  {
					LIST;
					strcat( buf, "\"" );
					strcat( strContent, "\"" );
				  }
				  else			/* end of string */
				  {
					unput(c);
					strcat(buf,"\"");
					if (Opt_T)
						printf("<%s: %s>\n", "string", strContent );
					// strdup
					yylval.X.V.text = stringCopy(strContent);
					yylval.X.type = 3;
					
					memset( strContent, 0, MAX_LINE_LENG );	// clear strContent
				  	BEGIN INITIAL;
					return CHAR;
				  }
				}
<STR>"\n"			{
				  /* error, cancel execution immediately!  */
				  fprintf( stdout, "error at line %d: string `%s` missing terminating \" character\n", linenum, strContent );
				  exit(-1);
				}

"//&"[A-Z][+-][^\n]*		{ /* pseudo comment */
				  LIST;
				  setPseudoComment( yytext );
				}
"//"				{ /* C++ stype comment */
				  LIST;
				  BEGIN COMMENT_CPP;
				}

<COMMENT_CPP>[^\n]+		{
				  LIST;
				}

<COMMENT_CPP>"\n"		{
				  LIST;
				  if( Opt_S )
				  	printf("%d: %s",  linenum,  buf);
				  linenum++;
				  memset( buf, 0, MAX_LINE_LENG );
				  BEGIN INITIAL;
				}

"/*"				{ /* C-style comment */
				  LIST;
				  BEGIN COMMENT;
				}
<COMMENT>"*/"			{
				  LIST;
				  BEGIN INITIAL;
				}
<COMMENT>"\n"			{
				  LIST;
				  if  (Opt_S)
				  	printf("%d: %s",  linenum,  buf);
				  linenum++;
				  memset( buf, 0, MAX_LINE_LENG );
				}
<COMMENT>.			{
				  LIST;
				}

{whitespace}			{ 
				  LIST; 
				}

\n 				{
				  LIST;
				  if  (Opt_S)
				  	printf("%d: %s",  linenum,  buf);
				  linenum++;
				  memset( buf, 0, MAX_LINE_LENG );
		   		}

.				{
				  /* error */
				  printf("error at line %d: bad character \"%s\"\n", linenum, yytext );
				  exit(-1);
				}

%%
	/*
int  main( int argc, char **argv )
{
	if( argc != 2 ) {
		fprintf(  stdout,  "Usage:  ./scanner  [filename]\n"  );
		exit(0);
	}

	FILE *fp = fopen( argv[1], "r" );
	
	if( fp == NULL )  {
		fprintf( stdout, "Open  file  error\n" );
		exit(-1);
	}
	
	yyin = fp;
	yylex();
	exit(0);
}
	*/
void setPseudoComment( const char *text )
{
	/* format : //&[A-Z][+-] */
	switch( text[3] ) {
	 case 'S':
		if( text[4] == '+' )
			Opt_S = 1;
		else if( text[4] == '-' )
			Opt_S = 0;
		break;
	 case 'T':
		if( text[4] == '+' )
			Opt_T = 1;
		else if( text[4] == '-' )
			Opt_T = 0;
	 	break;
	case 'D':
		if( text[4] == '+' )
			Opt_D = 1;
		else if( text[4] == '-' )
			Opt_D = 0;
	 	break;	
	 default:
	 	break;
	}
}
/*
void dumpsymbol() {
	int i;
	printf("%-32s\t%-11s\t%-11s\t%-17s\t%-11s\t\n","Name","Kind","Level","Type","Attribute");
	for(i=0;i< 110;i++)
		printf("-");
		
	printf("\n");
	
	{
		printf("%-32s\t", "func");
		printf("%-11s\t", "function");
		printf("%d%-10s\t", 0,"(global)");
		printf("%-17s\t", "boolean");
		printf("%-11s\t", "integer, real [2][3]");
		printf("\n");
	}
	
	for(i=0;i< 110;i++)
		printf("-");
		
	printf("\n");
}*/

char* stringCopy(char * src){
	int _len = strlen(src);
	char * dst = (char*) calloc ((_len+1) ,  sizeof(char));
	strncpy(dst, src, _len);
	dst[_len] = '\0';
	return dst;
}
