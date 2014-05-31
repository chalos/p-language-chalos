%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "container.h"
#include "mySymbolTable.c"
#define debug(s) { printf("%s\n", s); }

extern int linenum;             /* declared in lex.l */
extern FILE *yyin;              /* declared by lex */
extern char *yytext;            /* declared by lex */
extern char buf[256];           /* declared in lex.l */
extern Opt_D;

int hasError = 0;
int scope = 0;

int return_valid = 1;
int return_type = -1;

char* progName;
char errorMsg[64];
mySymbolTable *st;

void declared(char*);
void notFound(char*);
double getvValue(container*);


int inFuncArg = 0;
int _tempGeneral;
myAttr *tempAttr;
myType *tempType;
mySymbol* tempSym;
mySymbol* tempFun;
mySymbol* tempArg;
%}

%union {
	container X;
}

%token ASSIGN 
%token BGN 
%token COLON 
%token COMMA 
%token DEF 
%token DO 
%token ELSE 
%token END 
%token FOR 
%token IDENT 
%token IF
%token 
%token OF 
%token PRINT 
%token READ 
%token RETURN 
%token SEMICOLON 
%token THEN 
%token TO 
%token VAR 
%token WHILE

%token LROUND
%token RROUND
%token LSQ
%token RSQ
%token LBRAC
%token RBRAC
%token INTEGER 
%token REAL 
%token STRING 
%token BOOLEAN 
%token ARRAY
%token DIGIT 
%token REALNUM 
%token OCT 
%token SCIENCE 
%token CHAR 
%token TRUE 
%token FALSE
%token ADD 
%token SUB 
%token MUL 
%token DIV 
%token MOD
%token AND 
%token OR 
%token NOT
%token GT 
%token GE 
%token EQ 
%token NE 
%token LT 
%token LE

%start program

%%

program	
		:	programname SEMICOLON 
			{ 
				if(strncmp(progName, $1.X.V.text, strlen($1.X.V.text)) != 0){
					yyerror("Program Name is not same T__T");
				} 
				
				if(pushId(st, $1.X.V.text, scope, kindProg) == -1) {
					declared($1.X.V.text);
					yyerror(errorMsg);
				}
				
			} 
				programbody 
			END IDENT { 
				//printf("scope %d: %s program end\n", scope,$6.text); 
				if(strcmp($1.X.V.text, $6.X.V.text)!= 0) {
					yyerror("End name is not same T__T");
				}
				if(Opt_D == 1) dumpScope(st, scope);
				popScope(st);
			} 
		;

programname	
		:	identifier
		;

identifier	
		:	IDENT
		;

programbody 
		: 	variable_declare_list 
			func_definition_list { return_valid = 0; } 
			compound_statement
		;
		
variable_declare_list
		:	variable_declare_list variable_declare 
		| 	
		;

variable_declare 
		:	VAR identifier_list COLON assignment_expression SEMICOLON {
				setSTType(st, $4.X.type, -1, -1);
				attr* _temp = (attr*) malloc (sizeof(attr));
				switch($4.X.type){
					case typeInte:
					case typeBool:
					_temp->integer = $4.X.V.value;
					break;
					case typeReal:
					_temp->real = $4.X.V.dvalue;
					break;
					case typeStri:
					_temp->string = $4.X.V.text;
					break;
					default:
					break;
				}
				setSTAttr(st, kindCons, $4.X.type, _temp);
				endVarDel(st); 
			}
		|	VAR identifier_list COLON literal_type SEMICOLON{
				setSTType(st, $4.X.type, -1, -1);
				endVarDel(st);
			}
		|	VAR identifier_list COLON array_type SEMICOLON {
				endVarDel(st); 
			}
		;
		
identifier_list
		:	identifier {
				if(pushId(st, $1.X.V.text, scope, kindVari) == -1) {
					declared($1.X.V.text);
					yyerror(errorMsg);
				}
			} 
		|	identifier_list COMMA identifier { 
				if(pushId(st, $3.X.V.text, scope, kindVari) == -1) {
					declared($3.X.V.text);
					yyerror(errorMsg);
				}
			} 
		;
		
array_type
		:	ARRAY DIGIT TO DIGIT OF { 
				setSTType(st, typeVoid, $2.X.V.value, $4.X.V.value);
			} atype 
		;

atype	: 	literal_type { editSTType(st, $1.X.type); }
		|	array_type
		;
		
constant
		:	DIGIT { $$.X.type = typeInte; $$.X.V.value = $$.X.V.value;}
		|	SUB DIGIT { $$.X.V.value=-$2.X.V.value; $$.X.type = typeInte; }
		|	REALNUM { $$.X.type = typeReal; $$.X.V.dvalue = $$.X.V.dvalue;}
		|	SUB REALNUM { $$.X.V.dvalue=-$2.X.V.dvalue; $$.X.type = typeReal; }
		|	SCIENCE {  }
		|	SUB SCIENCE { }
		|	OCT { $$.X.type = typeInte; $$.X.V.value = $$.X.V.value; }
		|	CHAR { $$.X.type = typeStri; $$.X.V.text = $$.X.V.text;}
		|	TRUE { $$.X.V.value = 1; $$.X.type = typeBool; }
		|	FALSE { $$.X.V.value = 0; $$.X.type = typeBool; }
		;
		
literal_type
		:	INTEGER { $$.X.type = typeInte; }
		|	REAL { $$.X.type = typeReal; }
		|	STRING { $$.X.type = typeStri; }
		|	BOOLEAN { $$.X.type = typeBool; }
		;
		
func_definition_list
		:	func_definition_list func_definition
		|	
		;

func_definition
		:	IDENT {
				if(pushId(st, $1.X.V.text, scope, kindFunc) == -1){
					declared($1.X.V.text);
					yyerror(errorMsg);
				}
				endVarDel(st);			
				scope++; 
			} LROUND arg_declare_list RROUND return_type SEMICOLON {
				setFuncRet(st, $6.X.type);
				endVarDel(st); 
				scope--; 
			}
				compound_statement 
			END IDENT {
				if(strcmp($1.X.V.text, $11.X.V.text) != 0 ) yyerror("End name is not same T__T");
				return_type = -1;
			}
		;

return_type
		:	COLON literal_type { $$.X.type = return_type = $2.X.type; return_valid =1;}
		|	{ $$.X.type = return_type = 5; return_valid = 0;}
		;
		
arg_declare_list
		:	arg_declare
		|	{ }
		;

arg_declare
		:	function_variable_declare 
		|	function_variable_declare SEMICOLON arg_declare
		;

function_variable_declare
		:	arg_list COLON literal_type {
				setSTType(st, $3.X.type, -1, -1);
				setSTAttr(st, kindPara, typeVoid, NULL);
				endVarDel(st); 
			} 
		|	arg_list COLON array_type {
				setSTAttr(st, kindPara, typeVoid, NULL);
				endVarDel(st); 
			} 
		;
		
arg_list
		:	identifier {
				if(pushId(st, $1.X.V.text, scope, kindPara) == -1) {
					declared($1.X.V.text);
					yyerror(errorMsg);
				}
			} 
		|	arg_list COMMA identifier { 
				if(pushId(st, $3.X.V.text, scope, kindPara) == -1) {
					declared($3.X.V.text);
					yyerror(errorMsg);
				}
			
			} 
		;

compound_statement
		:	BGN { scope++; } 
				variable_declare_list statement_list 
			END { 
				if(Opt_D == 1) dumpScope(st, scope);
				popScope(st);
				scope--; 
			}
		;

statement_list
		:	statement_list statement
		|	
		;
		
statement
		:	simple_statement SEMICOLON
		|	compound_statement
		|	conditional_statement
		|	while_statement
		|	for_statement
		|	return_statement SEMICOLON
		|	function_call SEMICOLON
		;

simple_statement
		:	variable_reference ASSIGN assignment_expression {
				int typeL = tempSym->type->type;
				switch(typeL){
					case typeStri:
						if($3.X.type != typeStri)
							yyerror("Error type coersion");
						break;
					default:
						if($3.X.type ==typeStri)
							yyerror("Error type coersion");
				}
			}
		|	PRINT assignment_expression
		|	READ variable_reference
		;
		
variable_reference
		:	IDENT {
				tempSym = searchSymbolFromTop(st->stack, $1.X.V.text, kindVari);
				if( tempSym == NULL) {
					notFound($1.X.V.text);
					yyerror(errorMsg);
				} else {
					$$.X.type = tempSym->type->type;
				}
				tempType = tempSym->type;
				_tempGeneral = dimSize(tempType);
				/*printf("============\n");
				printf("%d\n", _tempGeneral);
				dumpMyType(tempType);
				printf("\n============\n");*/
				//_tempGeneral = 0;
			} array_variable 
		;
		
array_variable
		:	LSQ assignment_expression {
				if(tempType != NULL) {
					if($2.X.type != typeInte) yyerror("Index must be integer");
					
					if($2.X.V.value < tempType->start 
					|| $2.X.V.value >= tempType->dimension + tempType->start) {
						yyerror("Index out of range");
					}
					
					tempType = tempType->nextDim;
				} 
				_tempGeneral--;
			} RSQ array_variable 
		|	{if(_tempGeneral != 0 && inFuncArg == 0) yyerror("Reference size not match");}
		;
		
assignment_expression
		:	boolean_expr { $$ = $1; }
		;
		
boolean_expr		
		: 	boolean_expr OR boolean_term {
				if($1.X.type >= typeStri || $3.X.type >= typeStri) yyerror("OR Operation type error");
				else {
					double A = (getvValue(&$1.X) == 0)?0:1;
					double B = (getvValue(&$3.X) == 0)?0:1;
					$$.X.type = typeBool;
					$$.X.V.value = (A==1 || B==1)?1:0;
				}
			}
		| 	boolean_term { $$ = $1; }
		;

boolean_term		
		: 	boolean_term AND boolean_factor {
				if($1.X.type >= typeBool || $3.X.type >= typeBool) yyerror("AND Operation type error");
				else {
					double A = (getvValue(&$1.X) == 0)?0:1;
					double B = (getvValue(&$3.X) == 0)?0:1;
					$$.X.type = typeBool;
					$$.X.V.value = (A==0 || B==0)?0:1;
				}
			}
		| 	boolean_factor { $$ = $1; }
		;

boolean_factor		
		: 	NOT boolean_factor {
				if($2.X.type != typeBool) yyerror("NOT Operation type error");
				else {
					double A = getvValue(&$2.X);
					$$.X.type = typeBool;
					$$.X.V.value = (A == 0)?1:0;
				}
			}
		| 	relop_expr { $$ = $1; }
		;

relop_expr		
		: 	expr LT expr {
				if($1.X.type >= typeBool || $3.X.type >= typeBool) yyerror("< Operation type error");
				else if($1.X.type != $3.X.type) yyerror("Operands must same type");
				else {
					double A = getvValue(&$1.X);
					double B = getvValue(&$3.X);
					$$.X.type = typeBool;
					$$.X.V.value = A < B;
				}
			}
		|	expr LE expr {
				if($1.X.type >= typeBool || $3.X.type >= typeBool) yyerror("<= Operation type error");
				else if($1.X.type != $3.X.type) yyerror("Operands must same type");
				else {
					double A = getvValue(&$1.X);
					double B = getvValue(&$3.X);
					$$.X.type = typeBool;
					$$.X.V.value = A <= B;
				}
			}
		|	expr EQ expr {
				if($1.X.type >= typeBool || $3.X.type >= typeBool) yyerror("= Operation type error");
				else if($1.X.type != $3.X.type) yyerror("Operands must same type");
				else {
					double A = getvValue(&$1.X);
					double B = getvValue(&$3.X);
					$$.X.type = typeBool;
					$$.X.V.value = A == B;
				}
			}
		|	expr GE expr {
				if($1.X.type >= typeBool || $3.X.type >= typeBool) yyerror(">= Operation type error");
				else if($1.X.type != $3.X.type) yyerror("Operands must same type");
				else {
					double A = getvValue(&$1.X);
					double B = getvValue(&$3.X);
					$$.X.type = typeBool;
					$$.X.V.value = A >= B;
				}
			}
		|	expr GT expr {
				if($1.X.type >= typeBool || $3.X.type >= typeBool) yyerror("> Operation type error");
				else if($1.X.type != $3.X.type) yyerror("Operands must same type");
				else {
					double A = getvValue(&$1.X);
					double B = getvValue(&$3.X);
					$$.X.type = typeBool;
					$$.X.V.value = A > B;
				}
			}
		|	expr NE expr {
				if($1.X.type >= typeBool || $3.X.type >= typeBool) yyerror("<> Operation type error");
				else if($1.X.type != $3.X.type) yyerror("Operands must same type");
				else {
					double A = getvValue(&$1.X);
					double B = getvValue(&$3.X);
					$$.X.type = typeBool;
					$$.X.V.value = A != B;
				}
			}
		| 	expr { $$ = $1; }
		;

expr			
		: 	expr ADD term {
				if($1.X.type > typeStri || $3.X.type > typeStri) yyerror("+ Operation type error");
				else {
					double A = getvValue(&$1.X);
					double B = getvValue(&$3.X);
					if($1.X.type == typeReal || $3.X.type == typeReal){
						$$.X.type = typeReal;
						$$.X.V.dvalue = A + B;
					} else if($1.X.type == typeStri || $3.X.type == typeStri){
						$$.X.type = typeStri;
						int len = strlen($1.X.V.text) + strlen($3.X.V.text);
						char* _temp = (char*) calloc(len+1, sizeof(char));
						strcpy(_temp, $1.X.V.text);
						strcat(_temp, $3.X.V.text);
						_temp[len] = '\0';
						$$.X.V.text = _temp;
					}else {
						$$.X.type = typeInte;
						$$.X.V.value = (int)(A + B);
					}
				}
			}
		| 	expr SUB term {
				if($1.X.type >= typeStri || $3.X.type >= typeStri) yyerror("- Operation type error");
				else {
					double A = getvValue(&$1.X);
					double B = getvValue(&$3.X);
					if($1.X.type == typeReal || $3.X.type == typeReal){
						$$.X.type = typeReal;
						$$.X.V.dvalue = A + B;
					} else {
						$$.X.type = typeInte;
						$$.X.V.value = (int)(A - B);
					}
				}
			}
		| 	term { $$ = $1; }
		;

term			
		: 	term MUL factor {
				if($1.X.type >= typeStri || $3.X.type >= typeStri) yyerror("* Operation type error");
				else {
					double A = getvValue(&$1.X);
					double B = getvValue(&$3.X);
					if($1.X.type == typeReal || $3.X.type == typeReal){
						$$.X.type = typeReal;
						$$.X.V.dvalue = A + B;
					} else {
						$$.X.type = typeInte;
						$$.X.V.value = (int)(A * B);
					}
				}
			}
		|	term DIV factor {
				if($1.X.type >= typeStri || $3.X.type >= typeStri) yyerror("/ Operation type error");
				else {
					double A = getvValue(&$1.X);
					double B = getvValue(&$3.X);
					if($1.X.type == typeReal || $3.X.type == typeReal){
						$$.X.type = typeReal;
						$$.X.V.dvalue = A + B;
					} else {
						$$.X.type = typeInte;
						$$.X.V.value = (int)(A / B);
					}
				}
			}
		|	term MOD factor {
				if($1.X.type >= typeReal || $3.X.type >= typeReal) yyerror("MOD Operation type error");
				else {
					double A = getvValue(&$1.X);
					double B = getvValue(&$3.X);
					int AA = (int) A;
					int BB = (int) B;
					$$.X.type = typeInte;
					$$.X.V.value = AA % BB;
				}
			}
		| 	factor { $$ = $1; }
		;

factor			
		: 	variable_reference { $$ = $1; }
		|	SUB variable_reference {
				if($2.X.type >= typeStri) yyerror("Unary - Operation type error");
				else {
					double A = getvValue(&$2.X);
					if($2.X.type == typeReal){
						$$.X.type = typeReal;
						$$.X.V.dvalue = -A;
					} else {
						$$.X.type = typeInte;
						$$.X.V.value = (int)(-A);
					}
				}
			}
		| 	LROUND boolean_expr RROUND { $$ = $2; }
		| 	SUB LROUND boolean_expr RROUND {
				if($3.X.type >= typeStri) yyerror("Unary - Operation type error");
				else {
					double A = getvValue(&$3.X);
					if($3.X.type == typeReal){
						$$.X.type = typeReal;
						$$.X.V.dvalue = -A;
					} else {
						$$.X.type = typeInte;
						$$.X.V.value = (int)(-A);
					}
				}
			}
		| 	function_call
		|	SUB function_call;
		|	constant { $$ = $1; }
		;
		
conditional_statement
		:	IF assignment_expression {
				if($2.X.type != typeBool){
					yyerror("if statement should boolean as result\n");
				}
			} THEN statement_list else_statement
		;
		
else_statement
		:	ELSE statement_list END IF
		|	END IF
		;
		
while_statement
		:	WHILE assignment_expression {
				if($2.X.type != typeBool){
					yyerror("if statement error");
				}
			} DO statement_list END DO
		;

for_statement
		:	FOR variable_reference {
				if($2.X.type != typeBool){
					yyerror("for statement error");
				}
			} ASSIGN DIGIT TO DIGIT {
				if($5.X.V.value < $7.X.V.value || $5.X.V.value <0){
					yyerror("Loop parameters error");
				}
			} DO statement_list END DO
		;

return_statement
		:	RETURN assignment_expression {
				if(return_valid == 1) {
					if($2.X.type != return_type) {
						yyerror("return type error");
					}
				} else {
					yyerror("return not allowed here");
				}
			}
		;	

function_call
		:	IDENT {
				tempSym = searchSymbolFromTop(st->stack, $1.X.V.text, kindFunc);
				if(tempSym == NULL) yyerror("Funcion Id not found");
				else $$.X.type = tempSym->type->type;
				tempFun = tempSym;
				inFuncArg = 1;
				if(tempFun != NULL) tempAttr = tempFun->attr;
			} LROUND function_arg_list RROUND { inFuncArg = 0;}
		;
		
function_arg_list
		:	 arg_list_call // tempFun is contain function data, tempAttr is tempFun->attr
		|	{	
				if(tempAttr != NULL){
					if(getAttrNum(tempAttr) != 0){
						yyerror("argument number not match");
					}
				}
			}
		;

arg_list_call
		:	{ tempSym = NULL; }
			assignment_expression { // tempArg should contain arg's data
				if(tempSym == NULL){ // constant arg
					if( tempAttr != NULL){
						myType* _ttemp = buildMyType();
						setMyType(_ttemp, $2.X.type, -1, -1);
						if(isSameType(_ttemp, tempAttr->attrObj->parameter->type) == 0){
							yyerror("1argument number not match");
						} 
						tempAttr = tempAttr->next;
						free(_ttemp);
					} else {
						yyerror("2argument number not match");
					}
				} else { // variable arg
					tempArg = tempSym;
					if( tempAttr != NULL){
						if(isSameType(tempArg->type, tempAttr->attrObj->parameter->type) == 0){
							yyerror("3argument number not match");
							printf("SELF DUMPTYPE:\n");
							dumpMyType(tempArg->type);
							printf("\n");
							dumpMyType(tempAttr->attrObj->parameter->type);
							printf("\n");
						} 
						tempAttr = tempAttr->next;
					} else {
						yyerror("4argument number not match");
					}
				}
			}
		|	arg_list_call COMMA {tempSym = NULL;} assignment_expression { 
				// tempArg should contain arg's data
				if(tempSym == NULL){ // constant arg
					if( tempAttr != NULL){
						myType* _ttemp = buildMyType();
						setMyType(_ttemp, $2.X.type, -1, -1);
						if(isSameType(_ttemp, tempAttr->attrObj->parameter->type) == 0){
							yyerror("5argument number not match");
						} 
						if(tempAttr->next != NULL) yyerror("6argument number not match");
						free(_ttemp);
					} else {
						yyerror("7argument number not match");
					}
				} else { // variable arg
					tempArg = tempSym;
					if( tempAttr != NULL){
						if(isSameType(tempArg->type, tempAttr->attrObj->parameter->type) == 0){
							yyerror("8argument number not match");
						} 
						if(tempAttr->next != NULL) yyerror("9argument number not match");
					} else {
						yyerror("10argument number not match");
					}
				}
			}
		;
		
%%

int yyerror( char *msg )
{
        fprintf( stderr, "\n|--------------------------------------------------------------------------\n" );
		fprintf( stderr, "| Error found in Line #%d: %s\n", linenum, msg );
		fprintf( stderr, "|\n" );
        fprintf( stderr, "|--------------------------------------------------------------------------\n" );
        //exit(-1);
		hasError = 1;
}

int  main( int argc, char **argv )
{
	if( argc != 2 ) {
		fprintf(  stdout,  "Usage:  ./parser  [filename]\n"  );
		exit(0);
	}
	
	// INITIAL
	progName = argv[1];
	st = buildSymbolTable();
	
	FILE *fp = fopen( argv[1], "r" );
	
	if( fp == NULL )  {
		fprintf( stdout, "Open  file  error\n" );
		exit(-1);
	}
	
	yyin = fp;
	yyparse();

	if(hasError ==0 ){
		fprintf( stdout, "\n" );
		fprintf( stdout, "|--------------------------------|\n" );
		fprintf( stdout, "|  There is no semantic error!  |\n" );
		fprintf( stdout, "|--------------------------------|\n" );
	}
	exit(0);
}

void declared(char* id){
	strcpy(errorMsg, "Variable \'");
	strcat(errorMsg, id);
	strcat(errorMsg, "\' redeclared");
}

void notFound(char* id){
	strcpy(errorMsg, "Variable \'");
	strcat(errorMsg, id);
	strcat(errorMsg, "\' not found");
}

double getvValue(container* var){
	switch(var->type){
		case typeInte:
		case typeBool:
			return var->V.value;
		case typeReal:
			return var->V.dvalue;
		return -1;
	}
}
