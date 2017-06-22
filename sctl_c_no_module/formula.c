#include "formula.h"
#include <assert.h>
/***********************************************************/
/* 
    make new formulae here.
*/
Formula* mk_fml_top()
{
    Formula * fml = (Formula*)malloc(sizeof(Formula));
    fml->type = top;
    return fml;
}

Formula* mk_fml_bottom()
{
    Formula * fml = (Formula*)malloc(sizeof(Formula));
    fml->type = bottom;
    return fml;
}

Formula* mk_fml_atom(char* iden, State_list* sl)
{
    Formula * fml = (Formula*)malloc(sizeof(Formula));
    fml->type = atom;
    fml->str1 = iden;
    fml->states = sl;
    return fml;
}

Formula* mk_fml_neg(Formula* fml1)
{
    Formula * fml = (Formula*)malloc(sizeof(Formula));
    fml->type = neg;
    fml->sub_fml1 = fml1;
    return fml;
}

Formula* mk_fml_and(Formula* fml1, Formula* fml2)
{
    Formula * fml = (Formula*)malloc(sizeof(Formula));
    fml->type = and_fml;
    fml->sub_fml1 = fml1;
    fml->sub_fml2 = fml2;
    return fml;
}

Formula* mk_fml_or(Formula* fml1, Formula* fml2)
{
    Formula * fml = (Formula*)malloc(sizeof(Formula));
    fml->type = or_fml;
    fml->sub_fml1 = fml1;
    fml->sub_fml2 = fml2;
    return fml;
}

Formula* mk_fml_ax(char* quan, Formula* fml1, State* s)
{
    Formula * fml = (Formula*)malloc(sizeof(Formula));
    fml->type = ax;
    fml->str1 = quan;
    fml->sub_fml1 = fml1;
    State_list* sl = (State_list*)malloc(sizeof(State_list));
    sl->head = s;
    sl->tail = NULL;
    fml->states = sl;
    return fml;
}

Formula* mk_fml_ex(char* quan, Formula* fml1, State* s)
{
    Formula * fml = (Formula*)malloc(sizeof(Formula));
    fml->type = ex;
    fml->str1 = quan;
    fml->sub_fml1 = fml1;
    State_list* sl = (State_list*)malloc(sizeof(State_list));
    sl->head = s;
    sl->tail = NULL;
    fml->states = sl;
    return fml;
}

Formula* mk_fml_af(char* quan, Formula* fml1, State* s)
{
    Formula * fml = (Formula*)malloc(sizeof(Formula));
    fml->type = af;
    fml->str1 = quan;
    fml->sub_fml1 = fml1;
    State_list* sl = (State_list*)malloc(sizeof(State_list));
    sl->head = s;
    sl->tail = NULL;
    fml->states = sl;
    return fml;
}

Formula* mk_fml_eg(char* quan, Formula* fml1, State* s)
{
    Formula * fml = (Formula*)malloc(sizeof(Formula));
    fml->type = eg;
    fml->str1 = quan;
    fml->sub_fml1 = fml1;
    State_list* sl = (State_list*)malloc(sizeof(State_list));
    sl->head = s;
    sl->tail = NULL;
    fml->states = sl;
    return fml;
}

Formula* mk_fml_ar(char* quan1, char* quan2, Formula* fml1, Formula* fml2, State* s)
{
    Formula * fml = (Formula*)malloc(sizeof(Formula));
    fml->type = ar;
    fml->str1 = quan1;
    fml->str2 = quan2;
    fml->sub_fml1 = fml1;
    fml->sub_fml2 = fml2;
    State_list* sl = (State_list*)malloc(sizeof(State_list));
    sl->head = s;
    sl->tail = NULL;
    fml->states = sl;
    return fml;
}

Formula* mk_fml_eu(char* quan1, char* quan2, Formula* fml1, Formula* fml2, State* s)
{
    Formula * fml = (Formula*)malloc(sizeof(Formula));
    fml->type = eu;
    fml->str1 = quan1;
    fml->str2 = quan2;
    fml->sub_fml1 = fml1;
    fml->sub_fml2 = fml2;
    State_list* sl = (State_list*)malloc(sizeof(State_list));
    sl->head = s;
    sl->tail = NULL;
    fml->states = sl;
    return fml;
}
/******************************************************************************************************************/
char* str_fml(Formula* fml)
{

    switch(fml->type)
    {
        case top:
        {
            // printf("str_fml: top\n");
            // char* buffer = (char*)malloc(sizeof(char)*4);
            // strcat(buffer, "TRUE");
            // return buffer;
            return strdup("TRUE");
        }
        case bottom:
        {
            // printf("str_fml: bottom\n");
            // char* buffer = (char*)malloc(sizeof(char)*5);
            // strcat(buffer, "FALSE");
            // return buffer;  
            return strdup("FALSE");
        }
        case atom:
        {
            // printf("str_fml: atom\n");
            char* str_sl = str_state_list(fml->states);
            // printf("at str_fml atom, %s, %s\n", fml->str1, str_sl);
            int len_str = strlen(fml->str1)+strlen(str_sl)+10;
            char* buffer = (char*)malloc(sizeof(char)*len_str);
            buffer[0] = '\0';
            strcat(buffer, fml->str1);
            strcat(buffer, str_sl);
            free(str_sl);
            return buffer;
        }
        case neg:
        {
            // printf("str_fml: neg\n");
            char* str_fml1 = str_fml(fml->sub_fml1);
            char* buffer = (char*)malloc(sizeof(char)*(strlen(str_fml1)+6));
            buffer[0] = '\0';
            strcat(buffer, "(not ");
            strcat(buffer, str_fml1);
            strcat(buffer, ")");
            free(str_fml1);
            return buffer;
        }
        case and_fml:
        {
            // printf("str_fml: and\n");
            char* str_fml1 = str_fml(fml->sub_fml1);
            char* str_fml2 = str_fml(fml->sub_fml2);
            char* buffer = (char*)malloc(sizeof(char)*(strlen(str_fml1)+strlen(str_fml2)+7));
            buffer[0] = '\0';
            strcat(buffer, "(");
            strcat(buffer, str_fml1);
            strcat(buffer, " /\\ ");
            strcat(buffer, str_fml2);
            strcat(buffer, ")");
            free(str_fml1);
            free(str_fml2);
            return buffer;          
        }
        case or_fml:
        {
            // printf("str_fml: or\n");
            char* str_fml1 = str_fml(fml->sub_fml1);
            char* str_fml2 = str_fml(fml->sub_fml2);
            char* buffer = (char*)malloc(sizeof(char)*(strlen(str_fml1)+strlen(str_fml2)+7));
            buffer[0] = '\0';
            strcat(buffer, "(");
            strcat(buffer, str_fml1);
            strcat(buffer, " \\/ ");
            strcat(buffer, str_fml2);
            strcat(buffer, ")");
            free(str_fml1);
            free(str_fml2);
            return buffer;          
        }
        case ax:
        {
            // printf("str_fml: ax\n");
            char* str1 = fml->str1;
            char* str_fml1 = str_fml(fml->sub_fml1);
            char* str_s = str_state(fml->states->head);
            char* buffer = (char*)malloc(sizeof(char)*(strlen(str1)+strlen(str_fml1)+strlen(str_s)+9));
            buffer[0] = '\0';
            strcat(buffer, "(AX(");
            strcat(buffer, str1);
            strcat(buffer, ", ");
            strcat(buffer, str_fml1);
            strcat(buffer, ", ");
            strcat(buffer, str_s);
            strcat(buffer, ")");
            free(str_fml1);
            free(str_s);
            return buffer;
        }
        case ex:
        {
            // printf("str_fml: ex\n");
            char* str1 = fml->str1;
            char* str_fml1 = str_fml(fml->sub_fml1);
            char* str_s = str_state(fml->states->head);
            char* buffer = (char*)malloc(sizeof(char)*(strlen(str1)+strlen(str_fml1)+strlen(str_s)+9));
            buffer[0] = '\0';
            strcat(buffer, "(EX(");
            strcat(buffer, str1);
            strcat(buffer, ", ");
            strcat(buffer, str_fml1);
            strcat(buffer, ", ");
            strcat(buffer, str_s);
            strcat(buffer, ")");
            free(str_fml1);
            free(str_s);
            return buffer;
        }
        case af:
        {
            // printf("str_fml: af\n");
            char* str1 = fml->str1;
            char* str_fml1 = str_fml(fml->sub_fml1);
            char* str_s = str_state(fml->states->head);
            char* buffer = (char*)malloc(sizeof(char)*(strlen(str1)+strlen(str_fml1)+strlen(str_s)+9));
            buffer[0] = '\0';
            strcat(buffer, "(AF(");
            strcat(buffer, str1);
            strcat(buffer, ", ");
            strcat(buffer, str_fml1);
            strcat(buffer, ", ");
            strcat(buffer, str_s);
            strcat(buffer, ")");
            free(str_fml1);
            free(str_s);
            return buffer;
        }
        case eg:
        {
            // printf("str_fml: eg\n");
            char* str1 = fml->str1;
            char* str_fml1 = str_fml(fml->sub_fml1);
            char* str_s = str_state(fml->states->head);
            char* buffer = (char*)malloc(sizeof(char)*(strlen(str1)+strlen(str_fml1)+strlen(str_s)+9));
            buffer[0] = '\0';
            strcat(buffer, "(EG(");
            strcat(buffer, str1);
            strcat(buffer, ", ");
            strcat(buffer, str_fml1);
            strcat(buffer, ", ");
            strcat(buffer, str_s);
            strcat(buffer, ")");
            free(str_fml1);
            free(str_s);
            return buffer;
        }
        case ar:
        {
            // printf("str_fml: ar\n");
            char* str1 = fml->str1;
            char* str2 = fml->str2;
            char* str_fml1 = str_fml(fml->sub_fml1);
            char* str_fml2 = str_fml(fml->sub_fml2);
            char* str_s = str_state(fml->states->head);
            char* buffer = (char*)malloc(sizeof(char)*(strlen(str1)+strlen(str2)+strlen(str_fml1)+strlen(str_fml2)+strlen(str_s)+13));
            buffer[0] = '\0';
            strcat(buffer, "(AR(");
            strcat(buffer, str1);
            strcat(buffer, ", ");
            strcat(buffer, str2);
            strcat(buffer, ", ");
            strcat(buffer, str_fml1);
            strcat(buffer, ", ");
            strcat(buffer, str_fml2);
            strcat(buffer, ", ");
            strcat(buffer, str_s);
            strcat(buffer, ")");
            free(str_fml1);
            free(str_fml2);
            free(str_s);
            return buffer;
        }
        case eu:
        {
            // printf("str_fml: eu\n");
            char* str1 = fml->str1;
            char* str2 = fml->str2;
            char* str_fml1 = str_fml(fml->sub_fml1);
            char* str_fml2 = str_fml(fml->sub_fml2);
            char* str_s = str_state(fml->states->head);
            char* buffer = (char*)malloc(sizeof(char)*(strlen(str1)+strlen(str2)+strlen(str_fml1)+strlen(str_fml2)+strlen(str_s)+13));
            buffer[0] = '\0';
            strcat(buffer, "(EU(");
            strcat(buffer, str1);
            strcat(buffer, ", ");
            strcat(buffer, str2);
            strcat(buffer, ", ");
            strcat(buffer, str_fml1);
            strcat(buffer, ", ");
            strcat(buffer, str_fml2);
            strcat(buffer, ", ");
            strcat(buffer, str_s);
            strcat(buffer, ")");
            free(str_fml1);
            free(str_fml2);
            free(str_s);
            return buffer;
        }
    }
}
/******************************************************************************************************************/
Formula* replicate_fml(Formula* fml)
{
    switch(fml->type) {
        case top: {
            return mk_fml_top();
        }
        case bottom: {
            return mk_fml_bottom();
        }
        case atom: {
            State_list* sl = fml->states;
            State_list* sl2 = NULL;
            while(NULL != sl) {
                State_list* tmp_sl = (State_list*)malloc(sizeof(State_list));
                tmp_sl->head = replicate_state(sl->head);
                tmp_sl->tail = sl2;
                sl2 = tmp_sl;
                sl = sl->tail;
            }
            while(NULL != sl2) {
                State_list* tmp_sl = (State_list*)malloc(sizeof(State_list));
                tmp_sl->head = sl2->head;
                tmp_sl->tail = sl;
                sl = tmp_sl;
                State_list* freed = sl2;
                sl2 = sl2->tail;
                free(freed);
            }
            return mk_fml_atom(fml->str1, sl);
        }
        case neg: {
            return mk_fml_neg(replicate_fml(fml->sub_fml1));
        }
        case and_fml: {
            return mk_fml_and(replicate_fml(fml->sub_fml1), replicate_fml(fml->sub_fml2));
        }
        case or_fml: {
            return mk_fml_or(replicate_fml(fml->sub_fml1), replicate_fml(fml->sub_fml2));
        }
        case ax: {
            return mk_fml_ax(fml->str1, replicate_fml(fml->sub_fml1), replicate_state(fml->states->head));
        }
        case ex: {
            return mk_fml_ex(fml->str1, replicate_fml(fml->sub_fml1), replicate_state(fml->states->head));
        }
        case af: {
            return mk_fml_af(fml->str1, replicate_fml(fml->sub_fml1), replicate_state(fml->states->head));
        }
        case eg: {
            return mk_fml_eg(fml->str1, replicate_fml(fml->sub_fml1), replicate_state(fml->states->head));
        }
        case ar: {
            return mk_fml_ar(fml->str1, fml->str2, replicate_fml(fml->sub_fml1), replicate_fml(fml->sub_fml2), replicate_state(fml->states->head));
        }
        case eu: {
            return mk_fml_eu(fml->str1, fml->str2, replicate_fml(fml->sub_fml1), replicate_fml(fml->sub_fml2), replicate_state(fml->states->head));
        }
    }
}
//replicate and return the formula with state variable replaced
Formula* fml_replace_state_var(Formula* fml, char* name, int* state)
{
    if(NULL != fml) {
        switch(fml->type) {
            case atom: {
                Formula* tmp_fml = replicate_fml(fml);
                State_list* sl = tmp_fml->states;
                while(NULL != sl) {
                    if(ident == sl->head->type && 0 == strcmp(name, sl->head->content.ident)) {
                        sl->head->type = real;
                        free(sl->head->content.ident);
                        sl->head->content.real = state;
                        // break;
                    }
                    sl = sl->tail;
                }
                return fml;
            }
            case neg: {
                return mk_fml_neg(fml_replace_state_var(fml->sub_fml1, name, state));
            }
            case and_fml: {
                return mk_fml_and(fml_replace_state_var(fml->sub_fml1, name, state), fml_replace_state_var(fml->sub_fml2, name, state));
            }
            case or_fml: {
                return mk_fml_or(fml_replace_state_var(fml->sub_fml1, name, state), fml_replace_state_var(fml->sub_fml2, name, state));
            }
            case ax: {
                State* s = replicate_state(fml->states->head);
                if(ident == s->type && 0 == strcmp(name, s->content.ident)) {
                    s->type = real;
                    s->content.real = state;
                    return mk_fml_ax(fml->str1, replicate_fml(fml->sub_fml1), s);
                } else {
                    return mk_fml_ax(fml->str1, fml_replace_state_var(fml->sub_fml1, name, state), s);
                }
            }
            case ex: {
                State* s = replicate_state(fml->states->head);
                if(ident == s->type && 0 == strcmp(name, s->content.ident)) {
                    s->type = real;
                    s->content.real = state;
                    return mk_fml_ex(fml->str1, replicate_fml(fml->sub_fml1), s);
                } else {
                    return mk_fml_ex(fml->str1, fml_replace_state_var(fml->sub_fml1, name, state), s);
                }
            }
            case af: {
                State* s = replicate_state(fml->states->head);
                if(ident == s->type && 0 == strcmp(name, s->content.ident)) {
                    s->type = real;
                    s->content.real = state;
                    return mk_fml_af(fml->str1, replicate_fml(fml->sub_fml1), s);
                } else {
                    return mk_fml_af(fml->str1, fml_replace_state_var(fml->sub_fml1, name, state), s);
                }
            }
            case eg: {
                State* s = replicate_state(fml->states->head);
                if(ident == s->type && 0 == strcmp(name, s->content.ident)) {
                    s->type = real;
                    s->content.real = state;
                    return mk_fml_eg(fml->str1, replicate_fml(fml->sub_fml1), s);
                } else {
                    return mk_fml_eg(fml->str1, fml_replace_state_var(fml->sub_fml1, name, state), s);
                }
            }
            case ar: {
                State* s = replicate_state(fml->states->head);
                if(ident == s->type && 0 == strcmp(name, s->content.ident)) {
                    s->type = real;
                    s->content.real = state;
                    return mk_fml_ar(fml->str1, fml->str2, replicate_fml(fml->sub_fml1), replicate_fml(fml->sub_fml2), s);
                } else {
                    return mk_fml_ar(fml->str1, fml->str2, fml_replace_state_var(fml->sub_fml1, name, state), fml_replace_state_var(fml->sub_fml2, name, state), s);
                }
            }
            case eu: {
                State* s = replicate_state(fml->states->head);
                if(ident == s->type && 0 == strcmp(name, s->content.ident)) {
                    s->type = real;
                    s->content.real = state;
                    return mk_fml_eu(fml->str1, fml->str2, replicate_fml(fml->sub_fml1), replicate_fml(fml->sub_fml2), s);
                } else {
                    return mk_fml_eu(fml->str1, fml->str2, fml_replace_state_var(fml->sub_fml1, name, state), fml_replace_state_var(fml->sub_fml2, name, state), s);
                }
            }
            default:
                return replicate_fml(fml);
        }
    }
    
}

void clean_fml(Formula* fml) 
{
    if(NULL != fml) {
        clean_fml(fml->sub_fml1);
        clean_fml(fml->sub_fml2);
        State_list* sl = fml->states;
        while(NULL != sl) {
            State_list* freed = sl;
            free(sl->head);
            sl = sl->tail;
            free(freed);
        }
        free(fml);
    }
}