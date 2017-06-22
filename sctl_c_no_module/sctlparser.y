%locations
%{
    // #define _GNU_SOURCE
    #include <stdio.h>
    #include <stdlib.h>
    #include <string.h>
    #include "term.h"
    #include "formula.h"
    #include "modul.h"
    // #include "sctllexer.h"
    #include "parser_helper.h"
%}

//%parse-param {struct modul0* main_modul}
//%parse-param {struct modul0_list* sub_moduls}
%parse-param {struct modul_defs* moduls}
%initial-action {
    printf("Starting to parse the model file.\n");
//    printf("initial model name: %s, model size: %d\n", moduls->main_modul->name, moduls->main_modul->size);
}



%union {
    struct state* state_ptr;
    struct state_list* state_list_ptr;
    struct formula* fml_ptr;
    struct expr* expr_ptr;
    struct type* type_ptr;
    struct string_list* string_list_ptr;
    struct string_vars_expr_list* string_vars_expr_list_ptr;
    struct expr_pair_list* expr_pair_list_ptr;
    struct expr_list* expr_list_ptr;
    struct string_type_list* string_type_list_ptr;
    struct string_expr_list* string_expr_list_ptr;
    // struct expr_modul_instance_list* emi_list_ptr;
    struct string_emi_list* string_emi_list_ptr;
    struct transition* transition_ptr;
    struct transition_list* transition_list_ptr;
    struct string_fml_list* string_fml_list_ptr;
    struct modul0_list* module_list_ptr;
    // struct modul0* modul_ptr;
    char* str_ptr;
    int integer;
}

%{
    extern int yylex();
    void yyerror (struct modul_defs* moduls, const char* err_msg);
%}


%token Module Model Var Define Init Trans Atomic Spec Int Bool Top Bottom AX EX AF EG AR EU Neg
%token Colon Semicolon Comma Dot DotDot LB1 RB1 LB2 RB2 LB3 RB3 Assigno Scalar Nego Ando Oro Non_equal Mod 
%token And Or Equal Add Minus Mult LT GT LE GE File_end 
%token <str_ptr> Id
%token <integer> I
%token <integer> B 

%type <state_ptr> state
%type <state_list_ptr> state_list
%type <fml_ptr> fml
%type <expr_ptr> expr nested_var state_expr
// %type <expr_ptr> dexpr
%type <type_ptr> expr_type
%type <string_expr_list_ptr> symbol_decl symbol_decl_
%type <string_list_ptr> scalars bound_vars
%type <expr_pair_list_ptr> expr_pair_list
%type <expr_list_ptr> expr_list 
%type <string_type_list_ptr> parameters_decl var_decl var_decl_
%type <string_emi_list_ptr> init_decl init_decl_
%type <transition_list_ptr> trans_decl trans_decl_ 
%type <string_vars_expr_list_ptr> atomic_decl atomic_decl_
%type <string_fml_list_ptr> spec_decl spec_decl_
%type <module_list_ptr> sub_modules 


%start program
// %type <module_list_ptr> program

%nonassoc LT GT LE GE
%nonassoc Equal Non_equal
// %nonassoc DotDot      
%nonassoc Add
%nonassoc Minus 
%nonassoc Mult
%left Or Oro
%left And Ando
%right Mod
%right Neg Nego


%%
program: 
      sub_modules Model Id LB1 parameters_decl RB1 LB3 var_decl init_decl trans_decl atomic_decl spec_decl RB3 
        {
            // Modul0_list* mdls = (Modul0_list*)malloc(sizeof(Modul0_list));
            // Modul0* m0 = mk_modul0($Id, sizeof_string_type_list($var_decl), $parameters_decl, $var_decl, NULL, $init_decl, $trans_decl, $atomic_decl, $spec_decl);
            // mdls->head = *m0;
            // mdls->tail = $sub_modules;
            // $$ = mdls;
            printf("founded a main module %s size %d, without symbols definition\n", $3, len_string_type_list($var_decl));
            moduls->main_modul = mk_modul0($3, len_string_type_list($var_decl), $parameters_decl, $var_decl, NULL, $init_decl, $trans_decl, $atomic_decl, $spec_decl);
            moduls->sub_moduls = $sub_modules;
            YYACCEPT;
        }
    | sub_modules Model Id LB1 parameters_decl RB1 LB3 var_decl symbol_decl init_decl trans_decl atomic_decl spec_decl RB3
        {
            // Modul0_list* mdls = (Modul0_list*)malloc(sizeof(Modul0_list));
            // mdls->head = *(mk_modul0($Id, sizeof_string_type_list($var_decl), $parameters_decl, $var_decl, $symbol_decl, $init_decl, $trans_decl, $atomic_decl, $spec_decl));
            // mdls->tail = $sub_modules;
            // $$ = mdls;
            printf("founded a main module %s\n", $3);
            moduls->main_modul = mk_modul0($3, len_string_type_list($var_decl), $parameters_decl, $var_decl, $symbol_decl, $init_decl, $trans_decl, $atomic_decl, $spec_decl);
            moduls->sub_moduls = $sub_modules;
            YYACCEPT;
        }
;

sub_modules: /* empty */    {$$ = NULL;}
    | sub_modules Module Id LB1 parameters_decl RB1 LB3 var_decl init_decl trans_decl RB3
        {
            Modul0_list* mdls = (Modul0_list*)malloc(sizeof(Modul0_list));
            mdls->head = *(mk_modul0($Id, len_string_type_list($var_decl), $parameters_decl, $var_decl, NULL, $init_decl, $trans_decl, NULL, NULL));
            mdls->tail = $1;
            $$ = mdls;           
        }
    | sub_modules Module Id LB1 parameters_decl RB1 LB3 var_decl symbol_decl init_decl trans_decl RB3
        {
            Modul0_list* mdls = (Modul0_list*)malloc(sizeof(Modul0_list));
            mdls->head = *(mk_modul0($Id, len_string_type_list($var_decl), $parameters_decl, $var_decl, $symbol_decl, $init_decl, $trans_decl, NULL, NULL));
            mdls->tail = $1;
            $$ = mdls;           
        }
;
/******************************************************************************************************************************************************************/
parameters_decl: /* empty */ {$$ = NULL;}
    | Id Colon expr_type
        {
            String_type_list* stl = (String_type_list*)malloc(sizeof(String_type_list));
            stl->name = $1;
            stl->type = *$3;
            stl->next = NULL;
            $$ = stl;
        }
    | Id Colon expr_type Comma parameters_decl
        {
            String_type_list* stl = (String_type_list*)malloc(sizeof(String_type_list));
            stl->name = $1;
            stl->type = *$3;
            stl->next = $5;
            $$ = stl;            
        }
;
/******************************************************************************************************************************************************************/
var_decl: Var LB3 var_decl_ RB3 {printf("variable declaration.\n"); $$ = $3;}
;
var_decl_: 
      Id Colon expr_type Semicolon
        {
            String_type_list* stl = (String_type_list*)malloc(sizeof(String_type_list));
            stl->name = $1;
            stl->type = *$3;
            stl->next = NULL;
            $$ = stl;
        }
    | Id Colon expr_type Semicolon var_decl_
        {
            String_type_list* stl = (String_type_list*)malloc(sizeof(String_type_list));
            stl->name = $1;
            stl->type = *$3;
            stl->next = $5;
            $$ = stl;            
        }
;
/******************************************************************************************************************************************************************/
symbol_decl: Define LB3 symbol_decl_ RB3    {printf("symbol definition.\n"); $$ = $3;}
;
symbol_decl_: /* empty */   {$$ = NULL;}
    | Id Assigno expr Semicolon symbol_decl_
        {
            String_expr_list* sel = (String_expr_list*)malloc(sizeof(String_expr_list));
            sel->name = $1;
            sel->expr = *$3;
            sel->next = $5;
            $$ = sel;
        }
;
/******************************************************************************************************************************************************************/
init_decl: Init LB3 init_decl_ RB3  {printf("initial state definition.\n"); $$ = $3;}
;
init_decl_: 
      Id Assigno expr Semicolon
        {
            String_emi_list* sel = (String_emi_list*)malloc(sizeof(String_emi_list));
            sel->name = $1;
            sel->emi = mk_emi_expr($3);
            sel->next = NULL;
            $$ = sel; 
        }
    | Id Assigno expr Semicolon init_decl_
        {
            String_emi_list* sel = (String_emi_list*)malloc(sizeof(String_emi_list));
            sel->name = $1;
            sel->emi = mk_emi_expr($3);
            sel->next = $5;
            $$ = sel; 
        }
    | Id Assigno LB3 expr_list RB3 Semicolon
        {
            String_emi_list* sel = (String_emi_list*)malloc(sizeof(String_emi_list));
            sel->name = $1;
            sel->emi = mk_emi_expr(mk_expr_aray($4));
            sel->next = NULL;
            $$ = sel; 
        }
    | Id Assigno LB3 expr_list RB3 Semicolon init_decl_
        {
            String_emi_list* sel = (String_emi_list*)malloc(sizeof(String_emi_list));
            sel->name = $1;
            sel->emi = mk_emi_expr(mk_expr_aray($4));
            sel->next = $7;
            $$ = sel; 
        }
    | Id Assigno Id LB1 expr_list RB1 Semicolon
        {
            String_emi_list* sel = (String_emi_list*)malloc(sizeof(String_emi_list));
            sel->name = $1;
            sel->emi = mk_emi_modul_instance($3, $5);
            sel->next = NULL;
            $$ = sel; 
        }
    | Id Assigno Id LB1 expr_list RB1 Semicolon init_decl_
        {
            String_emi_list* sel = (String_emi_list*)malloc(sizeof(String_emi_list));
            sel->name = $1;
            sel->emi = mk_emi_modul_instance($3, $5);
            sel->next = $8;
            $$ = sel; 
        }
;
/******************************************************************************************************************************************************************/
trans_decl: Trans LB3 trans_decl_ RB3  {printf("transition declaration.\n"); $$ = $3;}
;
trans_decl_: /* empty */    {$$ = NULL;}
    | expr Colon LB3 expr_pair_list RB3 Semicolon trans_decl_
        {
            Transition_list* tl = (Transition_list*)malloc(sizeof(Transition_list));
            Transition* t = (Transition*)malloc(sizeof(Transition));
            t->guard = *$1;
            t->action = $4;
            tl->head = *t;
            tl->tail = $7;
            $$ = tl;
        }
;
/******************************************************************************************************************************************************************/
atomic_decl: Atomic LB3 atomic_decl_ RB3    {printf("atomic formulae declaration.\n"); $$ = $3;}
;
atomic_decl_: /* empty */   {$$ = NULL;}
    | Id LB1 bound_vars RB1 Assigno state_expr Semicolon atomic_decl_
        {
            String_vars_expr_list* svel = (String_vars_expr_list*)malloc(sizeof(String_vars_expr_list));
            svel->name = $1;
            svel->var_list = $3;
            svel->expr = $6;
            svel->next = $8;
            $$ = svel;
        }
;        
/******************************************************************************************************************************************************************/
spec_decl: Spec LB3 spec_decl_ RB3  {printf("specification declaration.\n"); $$ = $3;}
;
spec_decl_: /* empty */ {$$ = NULL;}
    | Id Assigno fml Semicolon spec_decl_
        {
            String_fml_list* sfl = (String_fml_list*)malloc(sizeof(String_fml_list));
            sfl->name = $1;
            sfl->fml = *$3;
            sfl->next = $5;
            $$ = sfl;
        }
;
/******************************************************************************************************************************************************************/
fml: 
      Top       {$$ = mk_fml_top();}
    | Bottom    {$$ = mk_fml_bottom();}
    | Id LB1 state_list RB1     {$$ = mk_fml_atom($1, $3);}
    | Neg fml   {$$ = mk_fml_neg($2);}
    | fml And fml   {$$ = mk_fml_and($1, $3);}
    | fml Or fml    {$$ = mk_fml_or($1, $3);}
    | AX LB1 Id Comma fml Comma state RB1  {$$ = mk_fml_ax($3, $5, $7);}
    | EX LB1 Id Comma fml Comma state RB1  {$$ = mk_fml_ex($3, $5, $7);}
    | AF LB1 Id Comma fml Comma state RB1  {$$ = mk_fml_af($3, $5, $7);}
    | EG LB1 Id Comma fml Comma state RB1  {$$ = mk_fml_eg($3, $5, $7);}
    | AR LB1 Id Comma Id Comma fml Comma fml Comma state RB1    {$$ = mk_fml_ar($3, $5, $7, $9, $11);}
    | EU LB1 Id Comma Id Comma fml Comma fml Comma state RB1    {$$ = mk_fml_eu($3, $5, $7, $9, $11);}
    | LB1 fml RB1   {$$ = $2;}
;
state_list: /* empty */ {$$ = NULL;}
    | state state_list
        {
            State_list* sl = (State_list*)malloc(sizeof(State_list));
            sl->head = $1;
            sl->tail = $2;
            $$ = sl;
        }
;

state: Id   {$$ = mk_state_iden($1);}
;
/******************************************************************************************************************************************************************/
expr_type:
      LB1 expr DotDot expr RB1        {$$ = mk_type_int($2, $4);}
    | Bool                      {$$ = mk_type_bool();}
    | LB3 scalars RB3           {$$ = mk_type_scalar(*$2);}
    | expr_type LB2 expr RB2    {$$ = mk_type_array($1, $3);}
    | Id                        {$$ = mk_type_modul($1);}
;

bound_vars: /* empty */ {$$ = NULL;}
    | Id                 
        {
            String_list* sl = (String_list*)malloc(sizeof(String_list));
            sl->head = $1;
            sl->tail = NULL;
            $$ = sl;
        }
    | Id Comma bound_vars   
        {
            String_list* sl = (String_list*)malloc(sizeof(String_list));
            sl->head = $1;
            sl->tail = $3;
            $$ = sl;            
        }
;

scalars: 
      Scalar Id                 
        {
            String_list* sl = (String_list*)malloc(sizeof(String_list));
            sl->head = $2;
            sl->tail = NULL;
            $$ = sl;
        }
    | Scalar Id Comma scalars   
        {
            String_list* sl = (String_list*)malloc(sizeof(String_list));
            sl->head = $2;
            sl->tail = $4;
            $$ = sl;            
        }
;

expr_pair_list: /* empty */ {$$ = NULL;}
    | Id Assigno expr Semicolon expr_pair_list  
        {
            Expr_pair_list* epl = (Expr_pair_list*)malloc(sizeof(Expr_pair_list));
            Expr_pair* ep = (Expr_pair*)malloc(sizeof(Expr_pair));
            ep->first = *(mk_expr_svar($1));
            ep->second = *$3;
            epl->head = *ep;
            epl->tail = $5;
            $$ = epl;
        }
    | Id LB2 expr RB2 Assigno expr Semicolon expr_pair_list  
        {
            Expr_pair_list* epl = (Expr_pair_list*)malloc(sizeof(Expr_pair_list));
            Expr_pair* ep = (Expr_pair*)malloc(sizeof(Expr_pair));
            ep->first = *(mk_expr_var_index($1, $3));
            ep->second = *$6;
            epl->head = *ep;
            epl->tail = $8;
            $$ = epl;
        }    
;

expr_list:  /* empty */ {$$ = NULL;}
    | expr  
        {
            Expr_list* el = (Expr_list*)malloc(sizeof(Expr_list));
            el->head = $1;
            el->tail = NULL;
            $$ = el;
        }
    | expr Comma expr_list  
        {
            Expr_list* el = (Expr_list*)malloc(sizeof(Expr_list));
            el->head = $1;
            el->tail = $3;
            $$ = el;
        }
;

expr: 
      I     {$$ = mk_expr_cnst($1);}
    | B     {$$ = mk_expr_cnst($1);}
    | Id    {$$ = mk_expr_svar($1);}
    // | Id LB1 expr RB1   {$$ = mk_expr_state_expr($1, $3);}
    | Id LB2 expr RB2   {$$ = mk_expr_var_index($1, $3);}
    | nested_var        {$$ = $1;}
    | Scalar Id         {$$ = mk_expr_scalar($2);}
    | Minus expr        {$$ = mk_expr_negi($2);}
    | Nego expr         {$$ = mk_expr_negb($2);}
    | expr Equal expr   {$$ = mk_expr_equl($1, $3);}
    | expr Non_equal expr   {$$ = mk_expr_negb(mk_expr_equl($1, $3));}
    | expr Ando expr    {$$ = mk_expr_ando($1, $3);}
    | expr Oro expr     {$$ = mk_expr_oro($1, $3);}
    | expr Add expr     {$$ = mk_expr_add($1, $3);}
    | expr Minus expr   {$$ = mk_expr_minus($1, $3);}
    | expr Mult expr    {$$ = mk_expr_mult($1, $3);}
    | expr Mod expr     {$$ = mk_expr_mod($1, $3);}
    | expr LT expr     {$$ = mk_expr_lt($1, $3);} 
    | expr GT expr     {$$ = mk_expr_gt($1, $3);}
    | expr LE expr     {$$ = mk_expr_le($1, $3);}
    | expr GE expr     {$$ = mk_expr_ge($1, $3);}
    | LB1 expr RB1     {$$ = $2;} 
;
state_expr: 
      Id LB1 expr RB1   {$$ = mk_expr_state_expr($1, $3);}
    | Minus state_expr        {$$ = mk_expr_negi($2);}
    | Nego state_expr         {$$ = mk_expr_negb($2);}
    | state_expr Equal state_expr   {$$ = mk_expr_equl($1, $3);}
    | state_expr Non_equal state_expr   {$$ = mk_expr_negb(mk_expr_equl($1, $3));}
    | state_expr Ando state_expr    {$$ = mk_expr_ando($1, $3);}
    | state_expr Oro state_expr     {$$ = mk_expr_oro($1, $3);}
    | state_expr Add state_expr     {$$ = mk_expr_add($1, $3);}
    | state_expr Minus state_expr   {$$ = mk_expr_minus($1, $3);}
    | state_expr Mult state_expr    {$$ = mk_expr_mult($1, $3);}
    | state_expr Mod state_expr     {$$ = mk_expr_mod($1, $3);}
    | state_expr LT state_expr     {$$ = mk_expr_lt($1, $3);} 
    | state_expr GT state_expr     {$$ = mk_expr_gt($1, $3);}
    | state_expr LE state_expr     {$$ = mk_expr_le($1, $3);}
    | state_expr GE state_expr     {$$ = mk_expr_ge($1, $3);}
    | LB1 state_expr RB1     {$$ = $2;} 
;
nested_var: 
      Id Dot Id
        {$$ = mk_expr_nested_var(mk_expr_svar($1), mk_expr_svar($3));}
    | Id Dot Id LB2 expr RB2 
        {$$ = mk_expr_nested_var(mk_expr_svar($1), mk_expr_var_index($3, $5));}
    | Id LB2 expr RB2 Dot Id
        {$$ = mk_expr_nested_var(mk_expr_var_index($1, $3), mk_expr_svar($6));}
    | Id LB2 expr RB2 Dot Id LB2 expr RB2
        {$$ = mk_expr_nested_var(mk_expr_var_index($1, $3), mk_expr_var_index($6, $8));}
    | Id Dot nested_var
        {$$ = mk_expr_nested_var(mk_expr_svar($1), $3);}
    | Id LB2 expr RB2 Dot nested_var
        {$$ = mk_expr_nested_var(mk_expr_var_index($1, $3), $6);}
;

/******************************************************************************************************************************************************************/


%%

void yyerror (struct modul_defs* moduls, const char* err_msg)
{
    // fprintf(stderr, "%s, in line %d: '%s'.\n", err_msg, yylineno, yytext);
    fprintf(stderr, "%s\n", err_msg);
}
