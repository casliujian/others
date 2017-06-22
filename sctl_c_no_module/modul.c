#include "modul.h"
#include <assert.h>
/********************************************************************************************************/
int len_string_type_list(String_type_list* stl)
{
    int acc = 0;
    while(stl != NULL)
    {
        acc++;
        stl = stl->next;
    }
    return acc;
}
/********************************************************************************************************/
Modul0* mk_modul0(char* name, 
                  int size, 
                  String_type_list* parameters, 
                  String_type_list* vars, 
                  String_expr_list* symbols, 
                  String_emi_list* init_assigns, 
                  Transition_list* transitions,
                  String_vars_expr_list* atomics,
                  String_fml_list* specs)
{
	printf("making new a module.\n");
    Modul0* modul = (Modul0*)malloc(sizeof(Modul0));
    modul->name = name;
    modul->size = size;
    modul->parameters = parameters;
    modul->vars = vars;
    modul->symbols = symbols;
    modul->init_assigns = init_assigns;
    modul->transitions = transitions;
    modul->atomics = atomics;
    modul->specs = specs;
    return modul;
}

void print_modul0(FILE* out, Modul0* modul, int is_model)
{
    // fprintf(out, "%s is printing module into file\n", "a");
    if (is_model == 1)
    {
        fprintf(out, "Model name: %s, size %d\n{\n", modul->name, modul->size);
    } 
    else
    {
        fprintf(out, "Module name: %s, size %d\n{\n", modul->name, modul->size);
    }

    //printing parameters
    fprintf(out, "\tFormal parameters:\n");
    String_type_list* pstl = modul->parameters;
    while(NULL != pstl)
    {
        char* strt = str_type(&(pstl->type));
        fprintf(out, "\t\t%s: %s;\n", pstl->name, strt);
        free(strt);
        pstl = pstl->next;
    }

    //printing variable definitions
    fprintf(out, "\n\tState variable definitions:\n");
    String_type_list* vstl = modul->vars;
    while(NULL != vstl)
    {
        char* strt = str_type(&(vstl->type));
        fprintf(out, "\t\t%s: %s;\n", vstl->name, strt);
        free(strt);
        vstl = vstl->next;
    }

    //printing symbol definitions
    fprintf(out, "\n\tUser defined symbols:\n");
    String_expr_list* sstl = modul->symbols;
    while(NULL != sstl)
    {
        char* strt = str_expr(&(sstl->expr));
        fprintf(out, "\t\t%s: %s;\n", sstl->name, strt);
        free(strt);
        sstl = sstl->next;
    }

    //printing initial state definition
    fprintf(out, "\n\tInitial state definition:\n");
    String_emi_list* istl = modul->init_assigns;
    while(NULL != istl)
    {
        char* strt = str_emi((istl->emi));
        fprintf(out, "\t\t%s := %s;\n", istl->name, strt);
        free(strt);
        istl = istl->next;
    }

    //printing transition definition
    fprintf(out, "\n\tTransition relation definition:\n");
    Transition_list* tl = modul->transitions;
    while(NULL != tl) 
    {
        Transition tlh = tl->head;
        char* strguard = str_expr(&(tlh.guard));
        fprintf(out, "\t\t%s: {", strguard);
        free(strguard);
        Expr_pair_list* tla = tlh.action;
        while(NULL != tla)
        {
            char* strtla1 = str_expr(&(tla->head.first));
            char* strtla2 = str_expr(&(tla->head.second));
            fprintf(out, "%s:=%s; ", strtla1, strtla2);
            free(strtla1);
            free(strtla2);
            tla = tla -> tail;
        }
        fprintf(out, "}\n");
        tl = tl->tail;
    }

    if (is_model == 1)
    {
        //printing atomic formulae definitions
        fprintf(out, "\n\tAtomic formulae definitions:\n");
        String_vars_expr_list* tmp_atomics = modul->atomics;
        while(NULL != tmp_atomics)
        {
            char* strvl = str_string_list(tmp_atomics->var_list);
            char* stre = str_expr(tmp_atomics->expr);
            fprintf(out, "\t\t%s%s := %s;\n", tmp_atomics->name, strvl, stre);
            free(strvl);
            free(stre);
            tmp_atomics = tmp_atomics -> next;
        }
        //printing specification definitions
        fprintf(out, "\n\tSpecifications:\n");
        String_fml_list* tmp_specs = modul->specs;
        while(NULL != tmp_specs)
        {
            char* strfml = str_fml(&(tmp_specs->fml));
            fprintf(out, "\t\t%s := %s;\n", tmp_specs->name, strfml);
            free(strfml);
            tmp_specs = tmp_specs->next;
        }

    }

    fprintf(out, "}\n");
}
/**********************************************************************************************************/
int is_svar_defined(char* svar, String_type_list* vars)
{
    while(NULL != vars)
    {
        if (0 == strcmp(svar, vars->name))
        {
            return 1;
        }
        vars = vars->next;
    }
    return 0;
}

Expr* expand_symbol(Expr* expr, String_type_list* vars, String_expr_list* symbols)
{
    switch(expr->type)
    {
        case svar:
        {
            if(is_svar_defined(expr->value.string_value, vars) == 0)
            {
                String_expr_list* tmp_symbols = symbols;
                while(NULL != tmp_symbols)
                {
                    if(0 == strcmp(tmp_symbols->name, expr->value.string_value))
                    {
                        return expand_symbol(&(tmp_symbols->expr), vars, symbols);
                    }
                    tmp_symbols = tmp_symbols->next;
                }
                printf("cannot expand symbol %s.\n", expr->value.string_value);
                return NULL;
            }
            return expr;
        }
        case var_index:
        case state_expr:
        case negi:
        case negb:
        {
            expr->sub_exprs->head = expand_symbol(expr->sub_exprs->head, vars, symbols);
            return expr;
        }
        case add:
        case minus:
        case mult:
        case mod:
        case ando:
        case oro:
        case lt:
        case gt:
        case le:
        case ge:
        {
            expr->sub_exprs->head = expand_symbol(expr->sub_exprs->head, vars, symbols);
            expr->sub_exprs->tail->head = expand_symbol(expr->sub_exprs->tail->head, vars, symbols);
            return expr;
        }
        case aray:
        {
            Expr_list* sub_exprs = expr->sub_exprs;
            while(NULL != sub_exprs)
            {
                sub_exprs->head = expand_symbol(sub_exprs->head, vars, symbols);
                sub_exprs = sub_exprs->tail;
            }
            return expr;
        }
        default: return expr;
    }
}

/*
may cause memory leak, as the symbols definition in modul0 is lost in modul1.
*/
Modul1* modul021(Modul0* modul0)
{
    Modul1* modul1 = (Modul1*)malloc(sizeof(Modul1));
    modul1->name = modul0->name;
    modul1->size = modul0->size;
    modul1->parameters = modul0->parameters;
    modul1->vars = modul0->vars;
    String_emi_list* tmp_init_assigns = modul0->init_assigns;
    while(NULL != tmp_init_assigns)
    {
        Expr_modul_instance* tmp_emi = tmp_init_assigns->emi;
        switch(tmp_emi->type)
        {
            case expr:
            {
                tmp_emi->expr = expand_symbol(tmp_emi->expr, modul0->vars, modul0->symbols);
                break;
            }
            case modul_instance:
            {
                Expr_list* tmp_args = tmp_emi->modul.args;
                while(NULL != tmp_args)
                {
                    tmp_args->head = expand_symbol(tmp_args->head, modul0->vars, modul0->symbols);
                    tmp_args = tmp_args->tail;
                }
                break;
            }
        }
        tmp_init_assigns = tmp_init_assigns->next;
    }
    modul1->init_assigns = modul0->init_assigns;
    Transition_list* tmp_trans = modul0->transitions;
    while(NULL != tmp_trans)
    {
        tmp_trans->head.guard = *(expand_symbol(&(tmp_trans->head.guard), modul0->vars, modul0->symbols));
        Expr_pair_list* tmp_action = tmp_trans->head.action;
        while(NULL != tmp_action)
        {
            tmp_action->head.first = *(expand_symbol(&(tmp_action->head.first), modul0->vars, modul0->symbols));
            tmp_action->head.second = *(expand_symbol(&(tmp_action->head.second), modul0->vars, modul0->symbols));
            tmp_action = tmp_action->tail;
        }
        tmp_trans = tmp_trans->tail;
    }
    modul1->transitions = modul0->transitions;
    String_vars_expr_list* tmp_atomics = modul0->atomics;
    while(NULL != tmp_atomics)
    {
        tmp_atomics->expr = expand_symbol(tmp_atomics->expr, modul0->vars, modul0->symbols);
        tmp_atomics = tmp_atomics->next;
    }
    modul1->atomics = modul0->atomics;
    modul1->specs = modul0->specs;
    return modul1;
}

void print_modul1(FILE* out, Modul1* modul, int is_model)
{
    // fprintf(out, "%s is printing module into file\n", "a");
    if (is_model == 1)
    {
        fprintf(out, "Model name: %s, size %d\n{\n", modul->name, modul->size);
    } 
    else
    {
        fprintf(out, "Module name: %s, size %d\n{\n", modul->name, modul->size);
    }

    //printing parameters
    fprintf(out, "\tFormal parameters:\n");
    String_type_list* pstl = modul->parameters;
    while(NULL != pstl)
    {
        char* strt = str_type(&(pstl->type));
        fprintf(out, "\t\t%s: %s;\n", pstl->name, strt);
        free(strt);
        pstl = pstl->next;
    }

    //printing variable definitions
    fprintf(out, "\n\tState variable definitions:\n");
    String_type_list* vstl = modul->vars;
    while(NULL != vstl)
    {
        char* strt = str_type(&(vstl->type));
        fprintf(out, "\t\t%s: %s;\n", vstl->name, strt);
        free(strt);
        vstl = vstl->next;
    }

    //printing initial state definition
    fprintf(out, "\n\tInitial state definition:\n");
    String_emi_list* istl = modul->init_assigns;
    while(NULL != istl)
    {
        char* strt = str_emi((istl->emi));
        fprintf(out, "\t\t%s := %s;\n", istl->name, strt);
        free(strt);
        istl = istl->next;
    }

    //printing transition definition
    fprintf(out, "\n\tTransition relation definition:\n");
    Transition_list* tl = modul->transitions;
    while(NULL != tl) 
    {
        Transition tlh = tl->head;
        char* strguard = str_expr(&(tlh.guard));
        fprintf(out, "\t\t%s: {", strguard);
        free(strguard);
        Expr_pair_list* tla = tlh.action;
        while(NULL != tla)
        {
            char* strtla1 = str_expr(&(tla->head.first));
            char* strtla2 = str_expr(&(tla->head.second));
            fprintf(out, "%s:=%s; ", strtla1, strtla2);
            free(strtla1);
            free(strtla2);
            tla = tla -> tail;
        }
        fprintf(out, "}\n");
        tl = tl->tail;
    }

    if (is_model == 1)
    {
        //printing atomic formulae definitions
        fprintf(out, "\n\tAtomic formulae definitions:\n");
        String_vars_expr_list* tmp_atomics = modul->atomics;
        while(NULL != tmp_atomics)
        {
            char* strvl = str_string_list(tmp_atomics->var_list);
            char* stre = str_expr(tmp_atomics->expr);
            fprintf(out, "\t\t%s%s := %s;\n", tmp_atomics->name, strvl, stre);
            free(strvl);
            free(stre);
            tmp_atomics = tmp_atomics -> next;
        }
        //printing specification definitions
        fprintf(out, "\n\tSpecifications:\n");
        String_fml_list* tmp_specs = modul->specs;
        while(NULL != tmp_specs)
        {
            char* strfml = str_fml(&(tmp_specs->fml));
            fprintf(out, "\t\t%s := %s;\n", tmp_specs->name, strfml);
            free(strfml);
            tmp_specs = tmp_specs->next;
        }

    }

    fprintf(out, "}\n");
}
/*********************************************************************************************************************/
Expr* find_expr(char* name, String_expr_list* sel)
{
    while(NULL != sel)
    {
        if (0 == strcmp(name, sel->name))
        {
            return &(sel->expr);
        }
        sel = sel->next;
    }
    return NULL;
}

Expr* replace_expr(Expr* expr, String_expr_list* sel)
{
    switch(expr->type)
    {
        case svar:
        {
            Expr* tmp_expr = find_expr(expr->value.string_value, sel);
            if(NULL != tmp_expr)
            {
                return tmp_expr;
            }
            else
            {
                // printf("error replacing expression %s\n", str_expr(expr));
                return expr;
            }
        }
        case var_index: case state_expr: case negi: case negb:
        {
            expr->sub_exprs->head = replace_expr(expr->sub_exprs->head, sel);
            return expr;
        }
        case add: case minus: case mult: case mod: case ando: case oro: case lt: case gt: case le: case ge:
        {
            expr->sub_exprs->head = replace_expr(expr->sub_exprs->head, sel);
            expr->sub_exprs->tail->head = replace_expr(expr->sub_exprs->tail->head, sel);
            return expr;
        }
        case aray:
        {
            Expr_list* sub_exprs = expr->sub_exprs;
            while(NULL != sub_exprs)
            {
                sub_exprs->head = replace_expr(sub_exprs->head, sel);
                sub_exprs = sub_exprs->tail;
            }
            return expr;
        }
        default: return expr;
    }    
}

Type* replace_expr_in_type(Type *t, String_expr_list* sel)
{
    // replacing type.
    switch(t->type)
    {
        case int_type:
        {
            t->int_range.low = replace_expr(t->int_range.low, sel);
            t->int_range.high = replace_expr(t->int_range.high, sel);
            return t;
        }
        case array_type:
        {
            t->array_rep.element_type = (replace_expr_in_type((t->array_rep.element_type), sel));
            t->array_rep.index = replace_expr(t->array_rep.index, sel);
            return t;
        }
        default:
            return t;
    }

}

Modul2* modul122(Modul1* modul1, String_expr_list* args, Modul1_list* moduls)
{
    Modul2* modul2 = (Modul2*)malloc(sizeof(Modul2));
    modul2->name = modul1->name;
    modul2->size = modul1->size;
    String_type_list* tmp_vars = modul1->vars;
    while(NULL != tmp_vars)
    {
        tmp_vars->type = *(replace_expr_in_type(&(tmp_vars->type), args));
        tmp_vars = tmp_vars->next;
    }
    modul2->vars = modul1->vars;
    String_emi_list* tmp_init_assigns = modul1->init_assigns;
    while(NULL != tmp_init_assigns)
    {
        Expr_modul_instance* tmp_emi = tmp_init_assigns->emi;
        switch(tmp_emi->type)
        {
            case expr:
            {
                tmp_emi->expr = replace_expr(tmp_emi->expr, args);
                break;
            }
            case modul_instance:
            {
                Expr_list* tmp_args = tmp_emi->modul.args;
                while(NULL != tmp_args)
                {
                    tmp_args->head = replace_expr(tmp_args->head, args);
                    tmp_args = tmp_args->tail;
                }
                // todo: copy modul1 and convert into modul2, then add to string_modul2_list.
                break;
            }
        }
        tmp_init_assigns = tmp_init_assigns->next;
    }
    modul2->init_assigns = modul1->init_assigns;
    Transition_list* tmp_trans = modul1->transitions;
    while(NULL != tmp_trans)
    {
        tmp_trans->head.guard = *(replace_expr(&(tmp_trans->head.guard), args));
        Expr_pair_list* tmp_action = tmp_trans->head.action;
        while(NULL != tmp_action)
        {
            tmp_action->head.first = *(replace_expr(&(tmp_action->head.first), args));
            tmp_action->head.second = *(replace_expr(&(tmp_action->head.second), args));
            tmp_action = tmp_action->tail;
        }
        tmp_trans = tmp_trans->tail;
    }
    modul2->transitions = modul1->transitions;
    String_vars_expr_list* tmp_atomics = modul1->atomics;
    while(NULL != tmp_atomics)
    {
        tmp_atomics->expr = replace_expr(tmp_atomics->expr, args);
        tmp_atomics = tmp_atomics->next;
    }
    modul2->atomics = modul1->atomics;
    modul2->specs = modul1->specs;
    return modul2;
}

void print_modul2(FILE* out, Modul2* modul, int is_model)
{
    // fprintf(out, "%s is printing module into file\n", "a");
    if (is_model == 1)
    {
        fprintf(out, "Model name: %s, size %d\n{\n", modul->name, modul->size);
    } 
    else
    {
        fprintf(out, "Module name: %s, size %d\n{\n", modul->name, modul->size);
    }

    //printing variable definitions
    fprintf(out, "\n\tState variable definitions:\n");
    String_type_list* vstl = modul->vars;
    while(NULL != vstl)
    {
        char* strt = str_type(&(vstl->type));
        fprintf(out, "\t\t%s: %s;\n", vstl->name, strt);
        free(strt);
        vstl = vstl->next;
    }

    //printing initial state definition
    fprintf(out, "\n\tInitial state definition:\n");
    String_emi_list* istl = modul->init_assigns;
    while(NULL != istl)
    {
        char* strt = str_emi((istl->emi));
        fprintf(out, "\t\t%s := %s;\n", istl->name, strt);
        free(strt);
        istl = istl->next;
    }

    //printing transition definition
    fprintf(out, "\n\tTransition relation definition:\n");
    Transition_list* tl = modul->transitions;
    while(NULL != tl) 
    {
        Transition tlh = tl->head;
        char* strguard = str_expr(&(tlh.guard));
        fprintf(out, "\t\t%s: {", strguard);
        free(strguard);
        Expr_pair_list* tla = tlh.action;
        while(NULL != tla)
        {
            char* strtla1 = str_expr(&(tla->head.first));
            char* strtla2 = str_expr(&(tla->head.second));
            fprintf(out, "%s:=%s; ", strtla1, strtla2);
            free(strtla1);
            free(strtla2);
            tla = tla -> tail;
        }
        fprintf(out, "}\n");
        tl = tl->tail;
    }

    if (is_model == 1)
    {
        //printing atomic formulae definitions
        fprintf(out, "\n\tAtomic formulae definitions:\n");
        String_vars_expr_list* tmp_atomics = modul->atomics;
        while(NULL != tmp_atomics)
        {
            char* strvl = str_string_list(tmp_atomics->var_list);
            char* stre = str_expr(tmp_atomics->expr);
            fprintf(out, "\t\t%s%s := %s;\n", tmp_atomics->name, strvl, stre);
            free(strvl);
            free(stre);
            tmp_atomics = tmp_atomics -> next;
        }
        //printing specification definitions
        fprintf(out, "\n\tSpecifications:\n");
        String_fml_list* tmp_specs = modul->specs;
        while(NULL != tmp_specs)
        {
            char* strfml = str_fml(&(tmp_specs->fml));
            fprintf(out, "\t\t%s := %s;\n", tmp_specs->name, strfml);
            free(strfml);
            tmp_specs = tmp_specs->next;
        }

    }

    fprintf(out, "}\n");
}
/***********************************************************************************************************/

Expr* expand_array(Expr* expr)
{
    switch(expr->type)
    {
        case var_index:
        {
            Expr* e1 = eval_expr(expr->sub_exprs->head);
            if (cnst == e1->type)
            {
                expr->type = svar;
                char* buffer = (char*)malloc(sizeof(char)*(strlen(expr->value.string_value) + 12));
                sprintf(buffer, "%s_%d", expr->value.string_value, e1->value.int_value);
                expr->value.string_value = buffer;
                clean_expr(e1);
                free(expr->sub_exprs);
                expr->sub_exprs = NULL;
            }
            return expr;
        }
        case state_expr: case negi: case negb:
        {
            expr->sub_exprs->head = expand_array(expr->sub_exprs->head);
            return expr;
        }
        case add: case minus: case mult: case mod: case ando: case oro: case lt: case gt: case le: case ge:
        {
            expr->sub_exprs->head = expand_array(expr->sub_exprs->head);
            expr->sub_exprs->tail->head = expand_array(expr->sub_exprs->tail->head);
            return expr;
        }
        case aray:
        {
            Expr_list* sub_exprs = expr->sub_exprs;
            while(NULL != sub_exprs)
            {
                sub_exprs->head = expand_array(sub_exprs->head);
                sub_exprs = sub_exprs->tail;
            }
            return expr;
        }
        default: return expr;
    }    
}

void expand_array_type(Type* t)
{
    switch(t->type)
    {
        case int_type:
        {
            t->int_range.low = eval_expr(t->int_range.low);
            t->int_range.high = eval_expr(t->int_range.high);
            break;
        }
        case array_type:
        {
            expand_array_type(t->array_rep.element_type);
            t->array_rep.index = eval_expr(t->array_rep.index);
            break;
        }
        default:
            break;
    }
}

Modul3* modul223(Modul2* modul2)
{
    Modul3* modul3 = (Modul3*)malloc(sizeof(Modul3));
    modul3->name = modul2->name;
    modul3->size = modul2->size;
    String_type_list* tmp_vars = modul2->vars;
    while(NULL != tmp_vars)
    {
        expand_array_type(&(tmp_vars->type));
        String_type_list* tmp_vars_next = tmp_vars->next;
        if(array_type == tmp_vars->type.type)
        {
            Type* elet = tmp_vars->type.array_rep.element_type;
            Expr* elei = tmp_vars->type.array_rep.index;
            if(cnst == elei->type)
            {
                char* tmp_name = tmp_vars->name;
                char* buffer = (char*)malloc(sizeof(char)*(strlen(tmp_name)+13));
                sprintf(buffer, "%s_%d", tmp_name, 0);
                tmp_vars->name = buffer;
                tmp_vars->type = *(elet);
                for (int i = 1; i < elei->value.int_value; i++)
                {
                    // Type* nt = mk_type_int(elet->int_range.low, elet->int_range.high);
                    String_type_list* tmp_vars_index = (String_type_list*)malloc(sizeof(String_type_list));
                    tmp_vars->next = tmp_vars_index;
                    char* buffer = (char*)malloc(sizeof(char)*(strlen(tmp_name)+13));
                    sprintf(buffer, "%s_%d", tmp_name, i);
                    tmp_vars_index->name = buffer;
                    tmp_vars_index->type = *(elet);
                    tmp_vars = tmp_vars_index;
                    if(i == elei->value.int_value - 1)
                    {
                        tmp_vars->next = tmp_vars_next;
                    }
                }
                modul3->size = modul3->size + elei->value.int_value - 1;
            }
        }
        tmp_vars = tmp_vars_next;
    }
    modul3->vars = modul2->vars;
    String_emi_list* tmp_init = modul2->init_assigns;
    String_expr_list* tmp_init_expr = NULL;
    String_expr_list* tmp_sei = NULL;
    while(NULL != tmp_init)
    {
        if(expr == tmp_init->emi->type && aray == tmp_init->emi->expr->type)
        {
            Expr_list* el = tmp_init->emi->expr->sub_exprs;
            int i = 0;
            
            while(NULL != el)
            {
                char* buffer = (char*)malloc(sizeof(char)*(strlen(tmp_init->name)+13));
                sprintf(buffer, "%s_%d", tmp_init->name, i);
                String_expr_list* tmp_se = (String_expr_list*)malloc(sizeof(String_expr_list));
                tmp_se->name = buffer;
                tmp_se->expr = *(el->head);
                if(NULL == tmp_init_expr)
                {
                    tmp_init_expr = tmp_se;
                    tmp_sei = tmp_se;
                }
                else
                {
                    tmp_sei->next = tmp_se;
                    tmp_sei = tmp_sei->next;
                }
                el = el->tail;
                i++;
            }
        } else if (expr == tmp_init->emi->type)
        {
            String_expr_list* tmp_se = (String_expr_list*)malloc(sizeof(String_expr_list));
            tmp_se->name = tmp_init->name;
            tmp_se->expr = *(tmp_init->emi->expr);
            if(NULL == tmp_init_expr)
            {
                tmp_init_expr = tmp_se;
                tmp_sei = tmp_se;
            }
            else
            {
                tmp_sei->next = tmp_se;
                tmp_sei = tmp_sei->next;
            }
        }
        tmp_init = tmp_init->next;
    }
    modul3->init_assigns = tmp_init_expr;
    Transition_list* tmp_trans = modul2->transitions;
    while(NULL != tmp_trans)
    {
        tmp_trans->head.guard = *(expand_array(&(tmp_trans->head.guard)));
        Expr_pair_list* tmp_action = tmp_trans->head.action;
        while(NULL != tmp_action)
        {
            tmp_action->head.first = *(expand_array(&(tmp_action->head.first)));
            tmp_action->head.second = *(expand_array(&(tmp_action->head.second)));
            tmp_action = tmp_action->tail;
        }
        tmp_trans = tmp_trans->tail;
    }
    modul3->transitions = modul2->transitions;
    String_vars_expr_list* tmp_atomics = modul2->atomics;
    while(NULL != tmp_atomics)
    {
        tmp_atomics->expr = expand_array(tmp_atomics->expr);
        tmp_atomics = tmp_atomics->next;
    }
    modul3->atomics = modul2->atomics;
    modul3->specs = modul2->specs;
    return modul3;
}

void print_modul3(FILE* out, Modul3* modul, int is_model)
{
    // fprintf(out, "%s is printing module into file\n", "a");
    if (is_model == 1)
    {
        fprintf(out, "Model name: %s, size %d\n{\n", modul->name, modul->size);
    } 
    else
    {
        fprintf(out, "Module name: %s, size %d\n{\n", modul->name, modul->size);
    }

    //printing variable definitions
    fprintf(out, "\n\tState variable definitions:\n");
    String_type_list* vstl = modul->vars;
    while(NULL != vstl)
    {
        char* strt = str_type(&(vstl->type));
        fprintf(out, "\t\t%s: %s;\n", vstl->name, strt);
        free(strt);
        vstl = vstl->next;
    }

    //printing initial state definition
    fprintf(out, "\n\tInitial state definition:\n");
    String_expr_list* istl = modul->init_assigns;
    while(NULL != istl)
    {
        char* strt = str_expr(&(istl->expr));
        fprintf(out, "\t\t%s := %s;\n", istl->name, strt);
        free(strt);
        istl = istl->next;
    }

    //printing transition definition
    fprintf(out, "\n\tTransition relation definition:\n");
    Transition_list* tl = modul->transitions;
    while(NULL != tl) 
    {
        Transition tlh = tl->head;
        char* strguard = str_expr(&(tlh.guard));
        fprintf(out, "\t\t%s: {", strguard);
        free(strguard);
        Expr_pair_list* tla = tlh.action;
        while(NULL != tla)
        {
            char* strtla1 = str_expr(&(tla->head.first));
            char* strtla2 = str_expr(&(tla->head.second));
            fprintf(out, "%s:=%s; ", strtla1, strtla2);
            free(strtla1);
            free(strtla2);
            tla = tla -> tail;
        }
        fprintf(out, "}\n");
        tl = tl->tail;
    }

    if (is_model == 1)
    {
        //printing atomic formulae definitions
        fprintf(out, "\n\tAtomic formulae definitions:\n");
        String_vars_expr_list* tmp_atomics = modul->atomics;
        while(NULL != tmp_atomics)
        {
            char* strvl = str_string_list(tmp_atomics->var_list);
            char* stre = str_expr(tmp_atomics->expr);
            fprintf(out, "\t\t%s%s := %s;\n", tmp_atomics->name, strvl, stre);
            free(strvl);
            free(stre);
            tmp_atomics = tmp_atomics -> next;
        }
        //printing specification definitions
        fprintf(out, "\n\tSpecifications:\n");
        String_fml_list* tmp_specs = modul->specs;
        while(NULL != tmp_specs)
        {
            char* strfml = str_fml(&(tmp_specs->fml));
            fprintf(out, "\t\t%s := %s;\n", tmp_specs->name, strfml);
            free(strfml);
            tmp_specs = tmp_specs->next;
        }

    }

    fprintf(out, "}\n");
}
/***************************************************************************************************************/
int find_index(char* name, String_int_list* sil)
{
    while(NULL != sil)
    {
        if (0 == strcmp(name, sil->name))
        {
            return sil->index;
        }
        sil = sil->next;
    }
    return -1;
}

Expr* recode_var(Expr* expr, String_int_list* vari)
{
    switch(expr->type)
    {
        case svar:
        {
            int index = find_index(expr->value.string_value, vari);
            char* buffer = (char*)malloc(sizeof(char)*12);
            sprintf(buffer, "%d", index);
            free(expr->value.string_value);
            expr->value.string_value = buffer;
            return expr;
        }
        case var_index:
        {
            printf("not supposed to be a var_index: %s\n", str_expr(expr));
            exit(1);
        }
        case state_expr: case negi: case negb:
        {
            expr->sub_exprs->head = recode_var(expr->sub_exprs->head, vari);
            return expr;
        }
        case add: case minus: case mult: case mod: case ando: case oro: case lt: case gt: case le: case ge:
        {
            expr->sub_exprs->head = recode_var(expr->sub_exprs->head, vari);
            expr->sub_exprs->tail->head = recode_var(expr->sub_exprs->tail->head, vari);
            return expr;
        }
        case aray:
        {
            Expr_list* sub_exprs = expr->sub_exprs;
            while(NULL != sub_exprs)
            {
                sub_exprs->head = recode_var(sub_exprs->head, vari);
                sub_exprs = sub_exprs->tail;
            }
            return expr;
        }
        default: return expr;
    }    
}

Expr* recode_state_var(Expr* expr, String_list* vari)
{
    switch(expr->type)
    {
        case state_expr:
        {
            int i = 0;
            while(NULL != vari)
            {
                if (0 == strcmp(expr->value.string_value, vari->head))
                {
                    char* buffer = (char*)malloc(sizeof(char)*12);
                    sprintf(buffer, "%d", i);
                    free(expr->value.string_value);
                    expr->value.string_value = buffer;
                }
                vari = vari->tail;
                i++;
            }
            return expr;
        }
        case var_index: case negi: case negb:
        {
            expr->sub_exprs->head = recode_state_var(expr->sub_exprs->head, vari);
            return expr;
        }
        case add: case minus: case mult: case mod: case ando: case oro: case lt: case gt: case le: case ge:
        {
            expr->sub_exprs->head = recode_state_var(expr->sub_exprs->head, vari);
            expr->sub_exprs->tail->head = recode_state_var(expr->sub_exprs->tail->head, vari);
            return expr;
        }
        case aray:
        {
            Expr_list* sub_exprs = expr->sub_exprs;
            while(NULL != sub_exprs)
            {
                sub_exprs->head = recode_state_var(sub_exprs->head, vari);
                sub_exprs = sub_exprs->tail;
            }
            return expr;
        }
        default: return expr;
    }    
}

Expr* recode_scalar(Expr* expr, String_type_list* vars)
{
    switch(expr->type) {
        case scalar: {
            while(NULL != vars) {
                if(scalar_type == vars->type.type) {
                    int pos = position_in_string_list(expr->value.string_value, &(vars->type.scalar_list));
                    if(-1 != pos) {
                        expr->type = cnst;
                        expr->value.int_value = pos;
                        free(expr->value.string_value);
                        expr->value.string_value = NULL;
                        return expr;
                    }
                }
                vars = vars->next;
            }
            return expr;
        }
        case var_index: case negi: case negb:
        {
            expr->sub_exprs->head = recode_scalar(expr->sub_exprs->head, vars);
            return expr;
        }
        case add: case minus: case mult: case mod: case ando: case oro: case lt: case gt: case le: case ge:
        {
            expr->sub_exprs->head = recode_scalar(expr->sub_exprs->head, vars);
            expr->sub_exprs->tail->head = recode_scalar(expr->sub_exprs->tail->head, vars);
            return expr;
        }
        case aray:
        {
            Expr_list* sub_exprs = expr->sub_exprs;
            while(NULL != sub_exprs)
            {
                sub_exprs->head = recode_scalar(sub_exprs->head, vars);
                sub_exprs = sub_exprs->tail;
            }
            return expr;
        }
        default: return expr;
    }
}

Modul4* modul324(Modul3* modul3)
{
    Modul4* modul4 = (Modul4*)malloc(sizeof(Modul4));
    modul4->name = modul3->name;
    modul4->size = modul3->size;
    modul4->vars = modul3->vars;
    int i = 0;
    String_type_list* tmp_stl = modul4->vars;
    String_int_list* sil_head = NULL;
    String_int_list* sil_tail = NULL;
    while(NULL != tmp_stl)
    {
        String_int_list* sil = (String_int_list*)malloc(sizeof(String_int_list));
        sil->name = tmp_stl->name;
        sil->index = i;
        sil->next = NULL;
        if(NULL == sil_head)
        {
            sil_head = sil;
            sil_tail = sil;
        } else
        {
            sil_tail->next = sil;
            sil_tail = sil_tail->next;
        }
        i++;
        tmp_stl = tmp_stl->next;
    }
    modul4->var_index = sil_head;
    int* tmp_init = (int*)malloc(sizeof(int)*(modul4->size));
    String_expr_list* tmp_sel = modul3->init_assigns;
    while(NULL != tmp_sel)
    {
        int index = find_index(tmp_sel->name, modul4->var_index);
        if(-1 != index)
        {
            Expr* value = eval_expr((recode_scalar(&(tmp_sel->expr), modul4->vars)));
            if (cnst == value->type)
            {
                tmp_init[index] = value->value.int_value;
            } else
            {
                printf("expression %s is supposed to be a const\n", str_expr(value));
                exit(1);
            }
        } else
        {
            printf("index of variable %s is -1\n", tmp_sel->name);
            exit(1);
        }
        tmp_sel = tmp_sel->next;
    }
    modul4->init_assigns = tmp_init;
    Transition_list* tmp_trans = modul3->transitions;
    while(NULL != tmp_trans)
    {
        tmp_trans->head.guard = *(recode_scalar(recode_var(&(tmp_trans->head.guard), modul4->var_index), modul4->vars));
        Expr_pair_list* tmp_action = tmp_trans->head.action;
        while(NULL != tmp_action)
        {
            tmp_action->head.first = *(recode_scalar(recode_var(&(tmp_action->head.first), modul4->var_index), modul4->vars));
            tmp_action->head.second = *(recode_scalar(recode_var(&(tmp_action->head.second), modul4->var_index), modul4->vars));
            tmp_action = tmp_action->tail;
        }
        tmp_trans = tmp_trans->tail;
    }
    modul4->transitions = modul3->transitions;
    String_vars_expr_list* tmp_atomics = modul3->atomics;
    while(NULL != tmp_atomics)
    {
        tmp_atomics->expr = recode_scalar(recode_var(tmp_atomics->expr, modul4->var_index), modul4->vars);
        tmp_atomics = tmp_atomics->next;
    }
    tmp_atomics = modul3->atomics;
    while(NULL != tmp_atomics)
    {
        tmp_atomics->expr = recode_scalar(recode_state_var(tmp_atomics->expr, tmp_atomics->var_list), modul4->vars);
        tmp_atomics = tmp_atomics->next;
    }
    modul4->atomics = modul3->atomics;
    modul4->specs = modul3->specs;
    return modul4;
}

void print_modul4(FILE* out, Modul4* modul, int is_model)
{
    // fprintf(out, "%s is printing module into file\n", "a");
    if (is_model == 1)
    {
        fprintf(out, "Model name: %s, size %d\n{\n", modul->name, modul->size);
    } 
    else
    {
        fprintf(out, "Module name: %s, size %d\n{\n", modul->name, modul->size);
    }

    //printing variable definitions
    fprintf(out, "\n\tState variable definitions:\n");
    String_type_list* vstl = modul->vars;
    while(NULL != vstl)
    {
        char* strt = str_type(&(vstl->type));
        fprintf(out, "\t\t%s: %s;\n", vstl->name, strt);
        free(strt);
        vstl = vstl->next;
    }
    fflush(out);
    //printing variable indics
    fprintf(out, "\n\tState variable indics:\n");
    String_int_list* vitl = modul->var_index;
    while(NULL != vitl)
    {
        // char* strt = str_type(&(vstl->type));
        assert(NULL != vitl);
        assert(NULL != vitl->name);
        fprintf(out, "\t\t%s: %d;\n", vitl->name, vitl->index);
        fflush(out);
        // free(strt);
        vitl = vitl->next;
    }

    //printing initial state definition
    fprintf(out, "\n\tInitial state definition:\n");
    int* istl = modul->init_assigns;
    for (int i = 0; i < modul->size; ++i)
    {
        fprintf(out, "\t\t%d;\n", istl[i]);
    }


    /*int i = 0;
    while(NULL != istl)
    {
        // char* strt = str_expr(&(istl->expr));
        fprintf(out, "\t\t%d;\n", istl[0]);
        // free(strt);
        istl = istl+1;
        // i++;
    }*/

    //printing transition definition
    fprintf(out, "\n\tTransition relation definition:\n");
    Transition_list* tl = modul->transitions;
    while(NULL != tl) 
    {
        Transition tlh = tl->head;
        char* strguard = str_expr(&(tlh.guard));
        fprintf(out, "\t\t%s: {", strguard);
        free(strguard);
        Expr_pair_list* tla = tlh.action;
        while(NULL != tla)
        {
            char* strtla1 = str_expr(&(tla->head.first));
            char* strtla2 = str_expr(&(tla->head.second));
            fprintf(out, "%s:=%s; ", strtla1, strtla2);
            free(strtla1);
            free(strtla2);
            tla = tla -> tail;
        }
        fprintf(out, "}\n");
        tl = tl->tail;
    }

    if (is_model == 1)
    {
        //printing atomic formulae definitions
        fprintf(out, "\n\tAtomic formulae definitions:\n");
        String_vars_expr_list* tmp_atomics = modul->atomics;
        while(NULL != tmp_atomics)
        {
            char* strvl = str_string_list(tmp_atomics->var_list);
            char* stre = str_expr(tmp_atomics->expr);
            fprintf(out, "\t\t%s%s := %s;\n", tmp_atomics->name, strvl, stre);
            free(strvl);
            free(stre);
            tmp_atomics = tmp_atomics -> next;
        }
        //printing specification definitions
        fprintf(out, "\n\tSpecifications:\n");
        String_fml_list* tmp_specs = modul->specs;
        while(NULL != tmp_specs)
        {
            char* strfml = str_fml(&(tmp_specs->fml));
            fprintf(out, "\t\t%s := %s;\n", tmp_specs->name, strfml);
            free(strfml);
            tmp_specs = tmp_specs->next;
        }

    }

    fprintf(out, "}\n");
}
/*********************************************************************************************************/