#include "term.h"
//#include <string.h>
/*****************************************************************************************************/
Expr_modul_instance* mk_emi_expr(Expr* e)
{
	Expr_modul_instance* emi = (Expr_modul_instance*)malloc(sizeof(Expr_modul_instance));
	emi->type = expr;
	emi->expr = e;
	return emi;
}

Expr_modul_instance* mk_emi_modul_instance(char* name, Expr_list* args)
{
	Expr_modul_instance* emi = (Expr_modul_instance*)malloc(sizeof(Expr_modul_instance));
	emi->type = modul_instance;
	emi->modul.modul_name = name;
	emi->modul.args = args;
	return emi;
}
/*****************************************************************************************************/
char* str_emi(Expr_modul_instance* emi)
{
	switch(emi->type)
	{
		case expr:
		{
			return str_expr(emi->expr);
		}
		case modul_instance:
		{
			char* str_el = str_expr_list(emi->modul.args);
			int lenstr = strlen(emi->modul.modul_name) + strlen(str_el);
			char* buffer = (char*)malloc(sizeof(char)*lenstr);
			buffer[0] = '\0';
			strcat(buffer, emi->modul.modul_name);
			strcat(buffer, str_el);
			free(str_el);
			return buffer;
		}
	}
}
/*****************************************************************************************************/
Type* mk_type_int(Expr* low, Expr* high)
{
	Type* t = (Type*)malloc(sizeof(Type));
	t->type = int_type;
	t->int_range.low = low;
	t->int_range.high = high;
	return t;
}

Type* mk_type_bool()
{
	Type* t = (Type*)malloc(sizeof(Type));
	t->type = bool_type;
	return t;
}

Type* mk_type_scalar(String_list scalars)
{
	Type* t = (Type*)malloc(sizeof(Type));
	t->type = scalar_type;
	t->scalar_list = scalars;
	return t;
}

Type* mk_type_array(Type* element_type, Expr* index)
{
	Type* t = (Type*)malloc(sizeof(Type));
	t->type = array_type;
	t->array_rep.element_type = element_type;
	t->array_rep.index = index;
	return t;
}

Type* mk_type_modul(char* name)
{
	Type* t = (Type*)malloc(sizeof(Type));
	t->type = module_type;
	t->modul_name = name;
	return t;
}
/***************************************************************************************************/
int len_string_list(String_list* sl)
{
	int len = 0;
	while(NULL != sl)
	{
		len ++;
		sl = sl->tail;
	}
	return len;
}

char* str_string_list(String_list* sl)
{
	String_list* tmp_sl = sl;
	int lensl = len_string_list(sl);
	int total_str_len = lensl+2;
	while(NULL != tmp_sl)
	{
		total_str_len =  total_str_len + strlen(tmp_sl->head);
		tmp_sl = tmp_sl->tail;
	}
	char* buffer = (char*)malloc(sizeof(char)*total_str_len);
	buffer[0] = '\0';
	strcat(buffer, "(");
	while(NULL != sl)
	{
		if (NULL != sl->tail)
		{
			strcat(buffer, sl->head);
			strcat(buffer, ",");
		}
		else
		{
			strcat(buffer, sl->head);
		}
		sl = sl->tail;
	}
	strcat(buffer, ")");
	return buffer;
}

int position_in_string_list(char* str, String_list* sl)
{
	int index = 0;
	while(sl != NULL) {
		if(0 == strcmp(str, sl->head)) {
			return index;
		}
		index++;
		sl = sl->tail;
	}
	return -1;
}

char* str_type(Type* t)
{
	switch(t->type)
	{
		case int_type:
		{
			char* int_buffer = (char*)malloc(sizeof(char)*12);
			int_buffer[0] = '\0';
			char* buffer = (char*)malloc(sizeof(char)*(24+3));
			buffer[0] = '\0';
			strcat(buffer, "(");
			sprintf(int_buffer, "%d", t->int_range.low->value.int_value);
			strcat(buffer, int_buffer);
			sprintf(int_buffer, "%d", t->int_range.high->value.int_value);
			strcat(buffer, int_buffer);
			strcat(buffer, ")");
			free(int_buffer);
			return buffer;
		}
		case bool_type:
		{
			return strdup("Bool");
		}
		case scalar_type:
		{
			String_list* sl = &(t->scalar_list);
			int lensl = len_string_list(sl);
			int total_str_len = 2*lensl+2;
			while(NULL != sl)
			{
				total_str_len =  total_str_len + strlen(sl->head);
				sl = sl->tail;
			}
			char* buffer = (char*)malloc(sizeof(char)*total_str_len);
			buffer[0] = '\0';
			strcat(buffer, "{");
			sl = &(t->scalar_list);
			while(NULL != sl)
			{
				strcat(buffer, "#");
				if (NULL != sl->tail)
				{
					strcat(buffer, sl->head);
					strcat(buffer, ";");
				}
				else
				{
					strcat(buffer, sl->head);
				}
				sl = sl->tail;
			}
			strcat(buffer, "}");
			return buffer;
		}
		case array_type:
		{
			char* et = str_type(t->array_rep.element_type);
			char* indxstr = str_expr(t->array_rep.index);
			char* buffer = (char*)malloc(sizeof(char)*(strlen(et)+strlen(indxstr)+2));
			buffer[0] = '\0';
			strcat(buffer, et);
			strcat(buffer, "[");
			strcat(buffer, indxstr);
			strcat(buffer, "]");
			return buffer;
		}
		case module_type:
			return strdup(t->modul_name);
	}
}



/*****************************************************************************************************/
/*
	make new states here.
*/
State* mk_state_iden(char* id)
{
	State* s = (State*)malloc(sizeof(State));
	s->type = ident;
	s->content.ident = id;
	return s;
}

State* mk_state_real(int* r)
{
	State* s = (State*)malloc(sizeof(State));
	s->type = real;
	s->content.real = r;
	return s;	
}
State* replicate_state(State* s)
{
	if(ident == s->type) {
		return mk_state_iden(s->content.ident);
	} else {

		return mk_state_real(s->content.real);
	}
}

void clean_state(State* s)
{
	free(s);
}
/****************************************************************************************************/
int len_state_list(State_list* sl)
{
	int len = 0;
	while(NULL != sl)
	{
		len ++;
		sl = sl -> tail;
	}
	return len;
}

State* find_ith_state(State_list* sl, int i)
{
	int index = 0;
	while(NULL != sl) {
		if(i == index) {
			return sl->head;
		}
		index ++;
		sl = sl->tail;
	}
	return NULL;
}

char* str_state(State* s)
{
	switch(s->type)
	{
		case ident:
		{
			// char* siden = s->content.ident;
			// printf("state variable %s\n", siden);
			// char* buffer = (char*)malloc(sizeof(char)*(strlen(siden)+7));
			// strcat(buffer, "State ");
			// strcat(buffer, siden);
			// printf("str_state:%s\n", buffer);
			// return buffer;
			return strdup(s->content.ident);
		}
		case real:
		{
			int alen = sizeof(s->content.real)/sizeof((s->content.real)[0]);
			char* state_vars[alen];
			int total_len = alen + 2;
			for(int i=0; i<alen; i++)
			{
				char* int_buffer;
				sprintf(int_buffer, "%d", (s->content.real)[i]);
				state_vars[i] = int_buffer;
				total_len = total_len + strlen(int_buffer);
			}
			char* buffer = (char*)malloc(sizeof(char)*(total_len));
			buffer[0] = '\0';
			strcat(buffer, "[");
			for (int i = 0; i < alen - 1; ++i)
			{
				strcat(buffer, state_vars[i]);
				strcat(buffer, ",");
			}
			if (alen > 0)
			{
				strcat(buffer, state_vars[alen-1]);
			}
			strcat(buffer, "]");
			return buffer;
		}
	}
}

char* str_state_list(State_list* sl)
{
	int sllen = len_state_list(sl);
	State_list* tmp_sl = sl;
	int index = 0;
	int total_str_len = sllen+2;
	char* state_strs[sllen];
	while(NULL != tmp_sl)
	{
		char* tmp_str = str_state(tmp_sl->head);
		state_strs[index] = tmp_str;
		total_str_len = total_str_len + strlen(state_strs[index]);
		index ++;
		tmp_sl = tmp_sl->tail;
	}
	char* buffer = (char*)malloc(sizeof(char)*total_str_len);
	buffer[0] = '\0';
	strcat(buffer, "(");
	for (int i = 0; i < sllen - 1; ++i)
	{
		// printf("%s,\n", state_strs[i]);
		strcat(buffer, state_strs[i]);
		free(state_strs[i]);
		strcat(buffer, ",");
	}
	if (sllen > 0)
	{
		// printf("%s\n", state_strs[sllen-1]);
		strcat(buffer, state_strs[sllen-1]);
		free(state_strs[sllen-1]);
	}
	strcat(buffer, ")");
	return buffer;
}

/****************************************************************************************************/
int len_expr_list(Expr_list* el)
{
	int len = 0;
	while(el != NULL)
	{
		len++;
		el = el->tail;
	}
	return len;
}
/****************************************************************************************************/
/*
	make new expressions here.
*/
Expr* mk_expr_symbol(char* sym)
{
	Expr* e = (Expr*)malloc(sizeof (Expr));
	e->type = symbol;
	e->value.string_value = sym;
	// e->sub_exprs = null_expr_list;
	return e;
}

Expr* mk_expr_nested_var(Expr* e1, Expr* e2)
{
	Expr* nv = (Expr*)malloc(sizeof(Expr));
	nv->type = nested_var;
	Expr_list* el2 = (Expr_list*)malloc(sizeof(Expr_list));
	el2->head = e2;
	el2->tail = NULL;
	Expr_list* el1 = (Expr_list*)malloc(sizeof(Expr_list));
	el1->head = e1,
	el1->tail = el2;
	nv->sub_exprs = el1;
	return nv;
}

Expr* mk_expr_var_index(char* v, Expr* e)
{
	Expr* vindx = (Expr*)malloc(sizeof(Expr));
	vindx->type = var_index;
	vindx->value.string_value = v;
	Expr_list* el = (Expr_list*)malloc(sizeof(Expr_list));
	el->head = e;
	el->tail = NULL;
	vindx->sub_exprs = el;
	return vindx;
}

// Expr* mk_expr_var(int v) 
// {
// 	Expr* ve = (Expr*)malloc(sizeof(Expr));
// 	ve->type = var;
// 	ve->value.int_value = v;
// 	return ve;
// }

Expr* mk_expr_svar(char* sv)
{
	Expr* ve = (Expr*)malloc(sizeof(Expr));
	ve->type = svar;
	ve->value.string_value = sv;
	return ve;	
}

Expr* mk_expr_scalar(char* sc)
{
	Expr* ve = (Expr*)malloc(sizeof(Expr));
	ve->type = scalar;
	ve->value.string_value = sc;
	return ve;	
}

Expr* mk_expr_state_expr(char* state, Expr* e)
{
	Expr* se = (Expr*)malloc(sizeof(Expr));
	se->type = state_expr;
	se->value.string_value = state;
	Expr_list* el = (Expr_list*)malloc(sizeof(Expr_list));
	el->head = e;
	el->tail = NULL;
	se->sub_exprs = el;
	return se;
}

Expr* mk_expr_cnst(int value)
{
	Expr* ce = (Expr*)malloc(sizeof(Expr));
	ce->type = cnst;
	ce->value.int_value = value;
	return ce;
} 

Expr* mk_expr_aray(Expr_list* el) 
{
	Expr* ae = (Expr*)malloc(sizeof(Expr));
	ae->type = aray;
	ae->sub_exprs = el;
	return ae;
}

Expr* mk_expr_negi(Expr* e) 
{
	Expr* nei = (Expr*)malloc(sizeof(Expr));
	nei->type = negi;
	Expr_list* el = (Expr_list*)malloc(sizeof(Expr_list));
	el->head = e;
	el->tail = NULL;
	nei->sub_exprs = el;
	return nei;
}

Expr* mk_expr_add(Expr* e1, Expr* e2)
{
	Expr* e = (Expr*)malloc(sizeof(Expr));
	e->type = add;
	Expr_list* el2 = (Expr_list*)malloc(sizeof(Expr_list));
	el2->head = e2;
	el2->tail = NULL;
	Expr_list* el1 = (Expr_list*)malloc(sizeof(Expr_list));
	el1->head = e1,
	el1->tail = el2;
	e->sub_exprs = el1;
	return e;
}

Expr* mk_expr_minus(Expr* e1, Expr* e2)
{
	Expr* e = (Expr*)malloc(sizeof(Expr));
	e->type = minus;
	Expr_list* el2 = (Expr_list*)malloc(sizeof(Expr_list));
	el2->head = e2;
	el2->tail = NULL;
	Expr_list* el1 = (Expr_list*)malloc(sizeof(Expr_list));
	el1->head = e1,
	el1->tail = el2;
	e->sub_exprs = el1;
	return e;
}

Expr* mk_expr_mult(Expr* e1, Expr* e2)
{
	Expr* e = (Expr*)malloc(sizeof(Expr));
	e->type = mult;
	Expr_list* el2 = (Expr_list*)malloc(sizeof(Expr_list));
	el2->head = e2;
	el2->tail = NULL;
	Expr_list* el1 = (Expr_list*)malloc(sizeof(Expr_list));
	el1->head = e1,
	el1->tail = el2;
	e->sub_exprs = el1;
	return e;
}

Expr* mk_expr_ando(Expr* e1, Expr* e2)
{
	Expr* e = (Expr*)malloc(sizeof(Expr));
	e->type = ando;
	Expr_list* el2 = (Expr_list*)malloc(sizeof(Expr_list));
	el2->head = e2;
	el2->tail = NULL;
	Expr_list* el1 = (Expr_list*)malloc(sizeof(Expr_list));
	el1->head = e1,
	el1->tail = el2;
	e->sub_exprs = el1;
	return e;
}

Expr* mk_expr_oro(Expr* e1, Expr* e2)
{
	Expr* e = (Expr*)malloc(sizeof(Expr));
	e->type = oro;
	Expr_list* el2 = (Expr_list*)malloc(sizeof(Expr_list));
	el2->head = e2;
	el2->tail = NULL;
	Expr_list* el1 = (Expr_list*)malloc(sizeof(Expr_list));
	el1->head = e1,
	el1->tail = el2;
	e->sub_exprs = el1;
	return e;
}

Expr* mk_expr_negb(Expr* e) 
{
	Expr* neb = (Expr*)malloc(sizeof(Expr));
	neb->type = negb;
	Expr_list* el = (Expr_list*)malloc(sizeof(Expr_list));
	el->head = e;
	el->tail = NULL;
	neb->sub_exprs = el;
	return neb;
}

Expr* mk_expr_equl(Expr* e1, Expr* e2)
{
	Expr* e = (Expr*)malloc(sizeof(Expr));
	e->type = equl;
	Expr_list* el2 = (Expr_list*)malloc(sizeof(Expr_list));
	el2->head = e2;
	el2->tail = NULL;
	Expr_list* el1 = (Expr_list*)malloc(sizeof(Expr_list));
	el1->head = e1,
	el1->tail = el2;
	e->sub_exprs = el1;
	return e;
}

Expr* mk_expr_mod(Expr* e1, Expr* e2)
{
	Expr* e = (Expr*)malloc(sizeof(Expr));
	e->type = mod;
	Expr_list* el2 = (Expr_list*)malloc(sizeof(Expr_list));
	el2->head = e2;
	el2->tail = NULL;
	Expr_list* el1 = (Expr_list*)malloc(sizeof(Expr_list));
	el1->head = e1,
	el1->tail = el2;
	e->sub_exprs = el1;
	return e;
}

Expr* mk_expr_lt(Expr* e1, Expr* e2)
{
	Expr* e = (Expr*)malloc(sizeof(Expr));
	e->type = lt;
	Expr_list* el2 = (Expr_list*)malloc(sizeof(Expr_list));
	el2->head = e2;
	el2->tail = NULL;
	Expr_list* el1 = (Expr_list*)malloc(sizeof(Expr_list));
	el1->head = e1,
	el1->tail = el2;
	e->sub_exprs = el1;
	return e;
}

Expr* mk_expr_gt(Expr* e1, Expr* e2)
{
	Expr* e = (Expr*)malloc(sizeof(Expr));
	e->type = gt;
	Expr_list* el2 = (Expr_list*)malloc(sizeof(Expr_list));
	el2->head = e2;
	el2->tail = NULL;
	Expr_list* el1 = (Expr_list*)malloc(sizeof(Expr_list));
	el1->head = e1,
	el1->tail = el2;
	e->sub_exprs = el1;
	return e;
}

Expr* mk_expr_le(Expr* e1, Expr* e2)
{
	Expr* e = (Expr*)malloc(sizeof(Expr));
	e->type = le;
	Expr_list* el2 = (Expr_list*)malloc(sizeof(Expr_list));
	el2->head = e2;
	el2->tail = NULL;
	Expr_list* el1 = (Expr_list*)malloc(sizeof(Expr_list));
	el1->head = e1,
	el1->tail = el2;
	e->sub_exprs = el1;
	return e;
}

Expr* mk_expr_ge(Expr* e1, Expr* e2)
{
	Expr* e = (Expr*)malloc(sizeof(Expr));
	e->type = ge;
	Expr_list* el2 = (Expr_list*)malloc(sizeof(Expr_list));
	el2->head = e2;
	el2->tail = NULL;
	Expr_list* el1 = (Expr_list*)malloc(sizeof(Expr_list));
	el1->head = e1,
	el1->tail = el2;
	e->sub_exprs = el1;
	return e;
}

Expr* replicate_expr(Expr* e)
{
	switch(e->type) {
		case symbol: {
			return mk_expr_symbol(e->value.string_value);
		}
		case nested_var: {
			return mk_expr_nested_var(replicate_expr(e->sub_exprs->head), replicate_expr(e->sub_exprs->tail->head));
		}
		case var_index: {
			return mk_expr_var_index(e->value.string_value, replicate_expr(e->sub_exprs->head));
		}
		case scalar: {
			return mk_expr_scalar(e->value.string_value);
		}
		case state_expr: {
			return mk_expr_state_expr(e->value.string_value, replicate_expr(e->sub_exprs->head));
		}
		case cnst: {
			return mk_expr_cnst(e->value.int_value);
		}
		case aray: {
			Expr_list* el = NULL;
			Expr_list* el2 = NULL;
			Expr_list* tmp_el = e->sub_exprs;
			while(NULL != tmp_el) {
				Expr_list* tmptmp_el = (Expr_list*)malloc(sizeof(Expr_list));
				tmptmp_el->head = replicate_expr(tmp_el->head);
				tmptmp_el->tail = el2;
				el2 = tmptmp_el;
				tmp_el = tmp_el->tail;
			}
			while(NULL != el2) {
				Expr_list* freed = el2;
				Expr_list* tmptmp_el = (Expr_list*)malloc(sizeof(Expr_list));
				tmptmp_el->head = el2->head;
				tmptmp_el->tail = el;
				el = tmptmp_el;
				el2 = el2->tail;
				free(freed);
			}
			return mk_expr_aray(el);
		}
		case negi: {
			return mk_expr_negi(replicate_expr(e->sub_exprs->head));
		}
		case negb: {
			return mk_expr_negb(replicate_expr(e->sub_exprs->head));
		}
		case add: {
			return mk_expr_add(replicate_expr(e->sub_exprs->head), replicate_expr(e->sub_exprs->tail->head));
		}
		case minus: {
			return mk_expr_minus(replicate_expr(e->sub_exprs->head), replicate_expr(e->sub_exprs->tail->head));
		}
		case mult: {
			return mk_expr_mult(replicate_expr(e->sub_exprs->head), replicate_expr(e->sub_exprs->tail->head));
		}
		case ando: {
			return mk_expr_ando(replicate_expr(e->sub_exprs->head), replicate_expr(e->sub_exprs->tail->head));
		}
		case oro: {
			return mk_expr_oro(replicate_expr(e->sub_exprs->head), replicate_expr(e->sub_exprs->tail->head));
		}
		case equl: {
			return mk_expr_equl(replicate_expr(e->sub_exprs->head), replicate_expr(e->sub_exprs->tail->head));
		}
		case mod: {
			return mk_expr_mod(replicate_expr(e->sub_exprs->head), replicate_expr(e->sub_exprs->tail->head));
		}
		case lt: {
			return mk_expr_lt(replicate_expr(e->sub_exprs->head), replicate_expr(e->sub_exprs->tail->head));
		}
		case gt: {
			return mk_expr_gt(replicate_expr(e->sub_exprs->head), replicate_expr(e->sub_exprs->tail->head));
		}
		case le: {
			return mk_expr_le(replicate_expr(e->sub_exprs->head), replicate_expr(e->sub_exprs->tail->head));
		}
		case ge: {
			return mk_expr_ge(replicate_expr(e->sub_exprs->head), replicate_expr(e->sub_exprs->tail->head));
		}
	}
}
/****************************************************************************************************************/
char* str_expr(Expr* e)
{
	switch(e->type) 
	{
		case svar:
			return strdup(e->value.string_value);
		case nested_var:
		{
			Expr* e1 = (e->sub_exprs)->head;
			Expr* e2 = e->sub_exprs->tail->head;
			char* str_e1 = str_expr(e1);
			char* str_e2 = str_expr(e2);
			char* buffer = (char*)malloc(sizeof(char)*(strlen(str_e1)+strlen(str_e2)+1));
			buffer[0] = '\0';
			strcat(buffer, str_e1);
			strcat(buffer, ".");
			strcat(buffer, str_e2);
			// strcat(buffer, "]");
			free(str_e1);
			free(str_e2);
			return buffer;
		}
		case var_index:
		{
			Expr* e1 = e->sub_exprs->head;
			char* str_var = strdup(e->value.string_value);
			char* str_e1 = str_expr(e1);
			char* buffer = (char*)malloc(sizeof(char)*(strlen(str_var)+strlen(str_e1)+2));
			buffer[0] = '\0';
			strcat(buffer, str_var);
			strcat(buffer, "[");
			strcat(buffer, str_e1);
			strcat(buffer, "]");
			free(str_var);
			free(str_e1);
			return buffer;
		}
		case scalar:
		{
			char* str_scalar = strdup(e->value.string_value);
			char* buffer = (char*)malloc(sizeof(char)*(strlen(str_scalar)+1));
			buffer[0] = '\0';
			strcat(buffer, "#");
			strcat(buffer, str_scalar);
			free(str_scalar);
			return buffer;
		}
		case state_expr:
		{
			Expr* e1 = e->sub_exprs->head;
			char* str_var = strdup(e->value.string_value);
			char* str_e1 = str_expr(e1);
			char* buffer = (char*)malloc(sizeof(char)*(strlen(str_var)+strlen(str_e1)+2));
			buffer[0] = '\0';
			strcat(buffer, str_var);
			strcat(buffer, "(");
			strcat(buffer, str_e1);
			strcat(buffer, ")");
			free(str_var);
			free(str_e1);
			return buffer;
		}
		case cnst:
		{
			char* int_buffer = (char*)malloc(sizeof(char)*12);
			int_buffer[0] = '\0';
			sprintf(int_buffer, "%d", e->value.int_value);
			return int_buffer;
		}
		case aray:
		{
			int len_el = len_expr_list(e->sub_exprs);
			char* str_el[len_el];
			Expr_list* tmp_el = e->sub_exprs;
			int index = 0;
			int total_str_len = len_el + 2;
			while(NULL != tmp_el)
			{
				char* tmp_str = str_expr(tmp_el->head);
				str_el[index] = tmp_str;
				index++;
				total_str_len = total_str_len + strlen(tmp_str);
				tmp_el = tmp_el->tail;
			}
			char* buffer = (char*)malloc(sizeof(char)*total_str_len);
			buffer[0] = '\0';
			strcat(buffer, "{");
			for (int i = 0; i<len_el-1; i++) 
			{
				strcat(buffer, str_el[i]);
				free(str_el[i]);
				strcat(buffer, ",");
			}
			if(len_el > 0)
			{
				strcat(buffer, str_el[len_el-1]);
				free(str_el[len_el-1]);
			}
			strcat(buffer, "}");
			return buffer;
		}
		case negi:
		{
			Expr* e1 = e->sub_exprs->head;
			char* str_e1 = str_expr(e1);
			char* buffer = (char*)malloc(sizeof(char)*(strlen(str_e1)+3));
			buffer[0] = '\0';
			strcat(buffer, "(-");
			strcat(buffer, str_e1);
			strcat(buffer, ")");
			free(str_e1);
			return buffer;
		}			
		case negb:
		{
			Expr* e1 = e->sub_exprs->head;
			char* str_e1 = str_expr(e1);
			char* buffer = (char*)malloc(sizeof(char)*(strlen(str_e1)+3));
			buffer[0] = '\0';
			strcat(buffer, "(!");
			strcat(buffer, str_e1);
			strcat(buffer, ")");
			free(str_e1);
			return buffer;
		}
		case add:
		{
			Expr* e1 = e->sub_exprs->head;
			Expr* e2 = e->sub_exprs->tail->head;
			char* str_e1 = str_expr(e1);
			char* str_e2 = str_expr(e2);
			char* buffer = (char*)malloc(sizeof(char)*(strlen(str_e1)+strlen(str_e2)+3));
			buffer[0] = '\0';
			strcat(buffer, "(");
			strcat(buffer, str_e1);
			strcat(buffer, "+");
			strcat(buffer, str_e2);
			strcat(buffer, ")");
			// strcat(buffer, "]");
			free(str_e1);
			free(str_e2);
			return buffer;
		}
		case minus:
		{
			Expr* e1 = e->sub_exprs->head;
			Expr* e2 = e->sub_exprs->tail->head;
			char* str_e1 = str_expr(e1);
			char* str_e2 = str_expr(e2);
			char* buffer = (char*)malloc(sizeof(char)*(strlen(str_e1)+strlen(str_e2)+3));
			buffer[0] = '\0';
			strcat(buffer, "(");
			strcat(buffer, str_e1);
			strcat(buffer, "-");
			strcat(buffer, str_e2);
			strcat(buffer, ")");
			// strcat(buffer, "]");
			free(str_e1);
			free(str_e2);
			return buffer;
		}
		case mult:
		{
			Expr* e1 = e->sub_exprs->head;
			Expr* e2 = e->sub_exprs->tail->head;
			char* str_e1 = str_expr(e1);
			char* str_e2 = str_expr(e2);
			char* buffer = (char*)malloc(sizeof(char)*(strlen(str_e1)+strlen(str_e2)+3));
			buffer[0] = '\0';
			strcat(buffer, "(");
			strcat(buffer, str_e1);
			strcat(buffer, "*");
			strcat(buffer, str_e2);
			strcat(buffer, ")");
			// strcat(buffer, "]");
			free(str_e1);
			free(str_e2);
			return buffer;
		}
		case equl:
		{
			Expr* e1 = e->sub_exprs->head;
			Expr* e2 = e->sub_exprs->tail->head;
			char* str_e1 = str_expr(e1);
			char* str_e2 = str_expr(e2);
			char* buffer = (char*)malloc(sizeof(char)*(strlen(str_e1)+strlen(str_e2)+3));
			buffer[0] = '\0';
			strcat(buffer, "(");
			strcat(buffer, str_e1);
			strcat(buffer, "=");
			strcat(buffer, str_e2);
			strcat(buffer, ")");
			// strcat(buffer, "]");
			free(str_e1);
			free(str_e2);
			return buffer;
		}
		case ando:
		{
			Expr* e1 = e->sub_exprs->head;
			Expr* e2 = e->sub_exprs->tail->head;
			char* str_e1 = str_expr(e1);
			char* str_e2 = str_expr(e2);
			char* buffer = (char*)malloc(sizeof(char)*(strlen(str_e1)+strlen(str_e2)+4));
			buffer[0] = '\0';
			strcat(buffer, "(");
			strcat(buffer, str_e1);
			strcat(buffer, "&&");
			strcat(buffer, str_e2);
			strcat(buffer, ")");
			// strcat(buffer, "]");
			free(str_e1);
			free(str_e2);
			return buffer;
		}
		case oro:
		{
			Expr* e1 = e->sub_exprs->head;
			Expr* e2 = e->sub_exprs->tail->head;
			char* str_e1 = str_expr(e1);
			char* str_e2 = str_expr(e2);
			char* buffer = (char*)malloc(sizeof(char)*(strlen(str_e1)+strlen(str_e2)+4));
			buffer[0] = '\0';
			strcat(buffer, "(");
			strcat(buffer, str_e1);
			strcat(buffer, "||");
			strcat(buffer, str_e2);
			strcat(buffer, ")");
			// strcat(buffer, "]");
			free(str_e1);
			free(str_e2);
			return buffer;
		}
		case mod:
		{
			Expr* e1 = e->sub_exprs->head;
			Expr* e2 = e->sub_exprs->tail->head;
			char* str_e1 = str_expr(e1);
			char* str_e2 = str_expr(e2);
			char* buffer = (char*)malloc(sizeof(char)*(strlen(str_e1)+strlen(str_e2)+7));
			buffer[0] = '\0';
			strcat(buffer, "(");
			strcat(buffer, str_e1);
			strcat(buffer, " mod ");
			strcat(buffer, str_e2);
			strcat(buffer, ")");
			// strcat(buffer, "]");
			free(str_e1);
			free(str_e2);
			return buffer;
		}
		case lt:
		{
			Expr* e1 = e->sub_exprs->head;
			Expr* e2 = e->sub_exprs->tail->head;
			char* str_e1 = str_expr(e1);
			char* str_e2 = str_expr(e2);
			char* buffer = (char*)malloc(sizeof(char)*(strlen(str_e1)+strlen(str_e2)+3));
			buffer[0] = '\0';
			strcat(buffer, "(");
			strcat(buffer, str_e1);
			strcat(buffer, "<");
			strcat(buffer, str_e2);
			strcat(buffer, ")");
			// strcat(buffer, "]");
			free(str_e1);
			free(str_e2);
			return buffer;
		}
		case gt:
		{
			Expr* e1 = e->sub_exprs->head;
			Expr* e2 = e->sub_exprs->tail->head;
			char* str_e1 = str_expr(e1);
			char* str_e2 = str_expr(e2);
			char* buffer = (char*)malloc(sizeof(char)*(strlen(str_e1)+strlen(str_e2)+3));
			buffer[0] = '\0';
			strcat(buffer, "(");
			strcat(buffer, str_e1);
			strcat(buffer, ">");
			strcat(buffer, str_e2);
			strcat(buffer, ")");
			// strcat(buffer, "]");
			free(str_e1);
			free(str_e2);
			return buffer;
		}
		case le:
		{
			Expr* e1 = e->sub_exprs->head;
			Expr* e2 = e->sub_exprs->tail->head;
			char* str_e1 = str_expr(e1);
			char* str_e2 = str_expr(e2);
			char* buffer = (char*)malloc(sizeof(char)*(strlen(str_e1)+strlen(str_e2)+4));
			buffer[0] = '\0';
			strcat(buffer, "(");
			strcat(buffer, str_e1);
			strcat(buffer, "<=");
			strcat(buffer, str_e2);
			strcat(buffer, ")");
			// strcat(buffer, "]");
			free(str_e1);
			free(str_e2);
			return buffer;
		}
		case ge:
		{
			Expr* e1 = e->sub_exprs->head;
			Expr* e2 = e->sub_exprs->tail->head;
			char* str_e1 = str_expr(e1);
			char* str_e2 = str_expr(e2);
			char* buffer = (char*)malloc(sizeof(char)*(strlen(str_e1)+strlen(str_e2)+4));
			buffer[0] = '\0';
			strcat(buffer, "(");
			strcat(buffer, str_e1);
			strcat(buffer, ">=");
			strcat(buffer, str_e2);
			strcat(buffer, ")");
			// strcat(buffer, "]");
			free(str_e1);
			free(str_e2);
			return buffer;
		}
		default:
			printf("error printing expressions, unknown expression type: %d\n", e->type);
			exit(1);
	}
}

char* str_expr_list(Expr_list* el)
{
	int len_el = len_expr_list(el);
	char* str_el[len_el];
	Expr_list* tmp_el = el;
	int index = 0;
	int total_str_len = len_el + 2;
	while(NULL != tmp_el)
	{
		char* tmp_str = str_expr(tmp_el->head);
		str_el[index] = tmp_str;
		index++;
		total_str_len = total_str_len + strlen(tmp_str);
		tmp_el = tmp_el->tail;
	}
	char* buffer = (char*)malloc(sizeof(char)*total_str_len);
	buffer[0] = '\0';
	strcat(buffer, "(");
	for (int i = 0; i<len_el-1; i++) 
	{
		strcat(buffer, str_el[i]);
		free(str_el[i]);
		strcat(buffer, ",");
	}
	if(len_el > 0)
	{
		strcat(buffer, str_el[len_el-1]);
		free(str_el[len_el-1]);
	}
	strcat(buffer, ")");
	return buffer;
}

Expr_list* eval_expr_list(Expr_list* el)
{
	Expr_list* tmp_el = el;
	while(NULL != tmp_el)
	{
		tmp_el->head = eval_expr(tmp_el->head);
		tmp_el = tmp_el->tail;
	}
	return el;
}

Expr* eval_expr(Expr* e)
{
	switch(e->type)
	{
		case var_index:
		case state_expr:
		{
			e->sub_exprs->head = eval_expr(e->sub_exprs->head);
			return e;
		}
		case aray:
		{
			e->sub_exprs = eval_expr_list(e->sub_exprs);
			return e;
		}
		case negi:
		{
			Expr* e1 = eval_expr(e->sub_exprs->head);
			if (cnst == e1->type)
			{
				e->type = cnst;
				e->value.int_value = - (e1->value.int_value);
				clean_expr(e1);
				free(e->sub_exprs);
				e->sub_exprs = NULL;
			} 
			return e;
		}
		case add:
		{
			Expr* e1 = eval_expr(e->sub_exprs->head);
			Expr* e2 = eval_expr(e->sub_exprs->tail->head);
			if(cnst == e1->type && cnst == e2->type)
			{
				e->type = cnst;
				e->value.int_value = (e1->value.int_value + e2->value.int_value);
				clean_expr(e1);
				clean_expr(e2);
				free(e->sub_exprs);
				e->sub_exprs = NULL;
			} 
			return e;
		}
		case minus:
		{
			Expr* e1 = eval_expr(e->sub_exprs->head);
			Expr* e2 = eval_expr(e->sub_exprs->tail->head);
			if(cnst == e1->type && cnst == e2->type)
			{
				e->type = cnst;
				e->value.int_value = (e1->value.int_value - e2->value.int_value);
				clean_expr(e1);
				clean_expr(e2);
				free(e->sub_exprs);
				e->sub_exprs = NULL;
			} 
			return e;
		}
		case mult:
		{
			Expr* e1 = eval_expr(e->sub_exprs->head);
			Expr* e2 = eval_expr(e->sub_exprs->tail->head);
			if(cnst == e1->type && cnst == e2->type)
			{
				e->type = cnst;
				e->value.int_value = (e1->value.int_value * e2->value.int_value);
				clean_expr(e1);
				clean_expr(e2);
				free(e->sub_exprs);
				e->sub_exprs = NULL;
			} 
			return e;
		}
		case ando:
		{
			Expr* e1 = eval_expr(e->sub_exprs->head);
			Expr* e2 = eval_expr(e->sub_exprs->tail->head);
			if(cnst == e1->type && cnst == e2->type)
			{
				e->type = cnst;
				e->value.int_value = (e1->value.int_value == 1 && e2->value.int_value == 1)? 1 : 0;
				clean_expr(e1);
				clean_expr(e2);
				free(e->sub_exprs);
				e->sub_exprs = NULL;
			} 
			return e;
		}
		case oro:
		{
			Expr* e1 = eval_expr(e->sub_exprs->head);
			Expr* e2 = eval_expr(e->sub_exprs->tail->head);
			if(cnst == e1->type && cnst == e2->type)
			{
				e->type = cnst;
				e->value.int_value = (e1->value.int_value == 0 && e2->value.int_value == 0)? 0 : 1;
				clean_expr(e1);
				clean_expr(e2);
				free(e->sub_exprs);
				e->sub_exprs = NULL;
			} 
			return e;
		}
		case negb:
		{
			Expr* e1 = eval_expr(e->sub_exprs->head);
			if (cnst == e1->type)
			{
				e->type = cnst;
				e->value.int_value = 1 - (e1->value.int_value);
				clean_expr(e1);
				free(e->sub_exprs);
				e->sub_exprs = NULL;
			} 
			return e;
		}
		case equl:
		{
			Expr* e1 = eval_expr(e->sub_exprs->head);
			Expr* e2 = eval_expr(e->sub_exprs->tail->head);
			if(cnst == e1->type && cnst == e2->type)
			{
				e->type = cnst;
				e->value.int_value = (e1->value.int_value == e2->value.int_value)? 1 : 0;
				clean_expr(e1);
				clean_expr(e2);
				free(e->sub_exprs);
				e->sub_exprs = NULL;
			} else if (svar == e1->type && svar == e2->type)
			{
				e->type = cnst;
				e->value.int_value = (0 == strcmp(e1->value.string_value, e2->value.string_value))? 1 : 0;
				clean_expr(e1);
				clean_expr(e2);
				free(e->sub_exprs);
				e->sub_exprs = NULL;
			}
			return e;
		}
		case mod:
		{
			Expr* e1 = eval_expr(e->sub_exprs->head);
			Expr* e2 = eval_expr(e->sub_exprs->tail->head);
			if(cnst == e1->type && cnst == e2->type)
			{
				e->type = cnst;
				e->value.int_value = (e1->value.int_value % e2->value.int_value);
				clean_expr(e1);
				clean_expr(e2);
				free(e->sub_exprs);
				e->sub_exprs = NULL;
			} 
			return e;
		}
		case lt:
		{
			Expr* e1 = eval_expr(e->sub_exprs->head);
			Expr* e2 = eval_expr(e->sub_exprs->tail->head);
			if(cnst == e1->type && cnst == e2->type)
			{
				e->type = cnst;
				e->value.int_value = (e1->value.int_value < e2->value.int_value)? 1 : 0;
				clean_expr(e1);
				clean_expr(e2);
				free(e->sub_exprs);
				e->sub_exprs = NULL;
			} 
			return e;
		}
		case gt:
		{
			Expr* e1 = eval_expr(e->sub_exprs->head);
			Expr* e2 = eval_expr(e->sub_exprs->tail->head);
			if(cnst == e1->type && cnst == e2->type)
			{
				e->type = cnst;
				e->value.int_value = (e1->value.int_value > e2->value.int_value)? 1 : 0;
				clean_expr(e1);
				clean_expr(e2);
				free(e->sub_exprs);
				e->sub_exprs = NULL;
			} 
			return e;
		}
		case le:
		{
			Expr* e1 = eval_expr(e->sub_exprs->head);
			Expr* e2 = eval_expr(e->sub_exprs->tail->head);
			if(cnst == e1->type && cnst == e2->type)
			{
				e->type = cnst;
				e->value.int_value = (e1->value.int_value <= e2->value.int_value)? 1 : 0;
				clean_expr(e1);
				clean_expr(e2);
				free(e->sub_exprs);
				e->sub_exprs = NULL;
			} 
			return e;
		}
		case ge:
		{
			Expr* e1 = eval_expr(e->sub_exprs->head);
			Expr* e2 = eval_expr(e->sub_exprs->tail->head);
			if(cnst == e1->type && cnst == e2->type)
			{
				e->type = cnst;
				e->value.int_value = (e1->value.int_value >= e2->value.int_value)? 1 : 0;
				clean_expr(e1);
				clean_expr(e2);
				free(e->sub_exprs);
				e->sub_exprs = NULL;
			} 
			return e;
		}
		default:
			return e;
	}
}

Expr* eval_with_state(Expr* e, State* state)
{
	if(real == state->type) {
		switch(e->type) {
			case svar: {
				int c = atoi(e->value.string_value);
				Expr* tmp_e = mk_expr_cnst(state->content.real[c]);
				return tmp_e;
			}
			case negi: {
				Expr* e1 = eval_with_state(e->sub_exprs->head, state);
				if(cnst == e1->type) {
					clean_expr(e1);
					return mk_expr_cnst(-(e1->value.int_value));
				} else {
					return mk_expr_negi(e1);
				}
			}
			case negb: {
				Expr* e1 = eval_with_state(e->sub_exprs->head, state);
				if(cnst == e1->type) {
					clean_expr(e1);
					return mk_expr_cnst(1-(e1->value.int_value));
				} else {
					return mk_expr_negb(e1);
				}
			}
			case mod: {
				Expr* e1 = eval_with_state(e->sub_exprs->head, state);
				Expr* e2 = eval_with_state(e->sub_exprs->tail->head, state);
				if(cnst == e1->type && cnst == e2->type) {
					clean_expr(e1);
					clean_expr(e2);
					return mk_expr_cnst(e1->value.int_value % e2->value.int_value);
				} else {
					return mk_expr_mod(e1, e2);
				}
			}
			case equl: {
				Expr* e1 = eval_with_state(e->sub_exprs->head, state);
				Expr* e2 = eval_with_state(e->sub_exprs->tail->head, state);
				if(cnst == e1->type && cnst == e2->type) {
					clean_expr(e1);
					clean_expr(e2);
					return mk_expr_cnst(e1->value.int_value == e2->value.int_value? 1 : 0);
				} else {
					return mk_expr_equl(e1, e2);
				}
			}
			case add: {
				Expr* e1 = eval_with_state(e->sub_exprs->head, state);
				Expr* e2 = eval_with_state(e->sub_exprs->tail->head, state);
				if(cnst == e1->type && cnst == e2->type) {
					clean_expr(e1);
					clean_expr(e2);
					return mk_expr_cnst(e1->value.int_value + e2->value.int_value);
				} else {
					return mk_expr_add(e1, e2);
				}
			}
			case minus: {
				Expr* e1 = eval_with_state(e->sub_exprs->head, state);
				Expr* e2 = eval_with_state(e->sub_exprs->tail->head, state);
				if(cnst == e1->type && cnst == e2->type) {
					clean_expr(e1);
					clean_expr(e2);
					return mk_expr_cnst(e1->value.int_value - e2->value.int_value);
				} else {
					return mk_expr_minus(e1, e2);
				}
			}
			case lt: {
				Expr* e1 = eval_with_state(e->sub_exprs->head, state);
				Expr* e2 = eval_with_state(e->sub_exprs->tail->head, state);
				if(cnst == e1->type && cnst == e2->type) {
					clean_expr(e1);
					clean_expr(e2);
					return mk_expr_cnst(e1->value.int_value < e2->value.int_value? 1 : 0);
				} else {
					return mk_expr_lt(e1, e2);
				}
			}
			case gt: {
				Expr* e1 = eval_with_state(e->sub_exprs->head, state);
				Expr* e2 = eval_with_state(e->sub_exprs->tail->head, state);
				if(cnst == e1->type && cnst == e2->type) {
					clean_expr(e1);
					clean_expr(e2);
					return mk_expr_cnst(e1->value.int_value > e2->value.int_value? 1 : 0);
				} else {
					return mk_expr_gt(e1, e2);
				}
			}
			case le: {
				Expr* e1 = eval_with_state(e->sub_exprs->head, state);
				Expr* e2 = eval_with_state(e->sub_exprs->tail->head, state);
				if(cnst == e1->type && cnst == e2->type) {
					clean_expr(e1);
					clean_expr(e2);
					return mk_expr_cnst(e1->value.int_value <= e2->value.int_value? 1 : 0);
				} else {
					return mk_expr_le(e1, e2);
				}
			}
			case ge: {
				Expr* e1 = eval_with_state(e->sub_exprs->head, state);
				Expr* e2 = eval_with_state(e->sub_exprs->tail->head, state);
				if(cnst == e1->type && cnst == e2->type) {
					clean_expr(e1);
					clean_expr(e2);
					return mk_expr_cnst(e1->value.int_value >= e2->value.int_value? 1 : 0);
				} else {
					return mk_expr_ge(e1, e2);
				}
			}
			case ando: {
				Expr* e1 = eval_with_state(e->sub_exprs->head, state);
				Expr* e2 = eval_with_state(e->sub_exprs->tail->head, state);
				if(cnst == e1->type && cnst == e2->type) {
					clean_expr(e1);
					clean_expr(e2);
					return mk_expr_cnst(e1->value.int_value == 1 && e2->value.int_value == 1? 1 : 0);
				} else {
					return mk_expr_ando(e1, e2);
				}
			}
			case oro: {
				Expr* e1 = eval_with_state(e->sub_exprs->head, state);
				Expr* e2 = eval_with_state(e->sub_exprs->tail->head, state);
				if(cnst == e1->type && cnst == e2->type) {
					clean_expr(e1);
					clean_expr(e2);
					return mk_expr_cnst(e1->value.int_value == 0 && e2->value.int_value == 0? 0 : 1);
				} else {
					return mk_expr_oro(e1, e2);
				}
			}
			default:
				return replicate_expr(e);
		}
	} else {
		printf("evaluating an expression %s with an state identifier: %s\n", str_expr(e), state->content.ident);
		exit(1);
		// return e;
	}
}

Expr* eval_with_state_list(Expr* e, State_list* sl)
{
	switch(e->type) {
		case state_expr: {
			State* state = find_ith_state(sl, atoi(e->value.string_value));
			Expr* tmp_e = eval_with_state(e->sub_exprs->head, state);
			return tmp_e;
		}
		case negi: {
				Expr* e1 = eval_with_state_list(e->sub_exprs->head, sl);
				if(cnst == e1->type) {
					e1->value.int_value = -(e1->value.int_value);
					// free(e);
					return e1;
				} else {
					return mk_expr_negi(e1);
				}
			}
		case negb: {
			Expr* e1 = eval_with_state_list(e->sub_exprs->head, sl);
			if(cnst == e1->type) {
				e1->value.int_value = 1-(e1->value.int_value);
				// free(e);
				return e1;
			} else {
				return mk_expr_negb(e1);
			}
		}
		case mod: {
			Expr* e1 = eval_with_state_list(e->sub_exprs->head, sl);
			Expr* e2 = eval_with_state_list(e->sub_exprs->tail->head, sl);
			if(cnst == e1->type && cnst == e2->type) {
				e1->value.int_value = e1->value.int_value % e2->value.int_value;
				clean_expr(e2);
				// free(e);
				return e1;
			} else {
				return mk_expr_mod(e1, e2);
			}
		}
		case equl: {
			Expr* e1 = eval_with_state_list(e->sub_exprs->head, sl);
			Expr* e2 = eval_with_state_list(e->sub_exprs->tail->head, sl);
			if(cnst == e1->type && cnst == e2->type) {
				e1->value.int_value = e1->value.int_value == e2->value.int_value? 1 : 0;
				clean_expr(e2);
				// free(e);
				return e1;
			} else {
				return mk_expr_equl(e1, e2);
			}
		}
		case add: {
			Expr* e1 = eval_with_state_list(e->sub_exprs->head, sl);
			Expr* e2 = eval_with_state_list(e->sub_exprs->tail->head, sl);
			if(cnst == e1->type && cnst == e2->type) {
				e1->value.int_value = e1->value.int_value + e2->value.int_value;
				clean_expr(e2);
				// free(e);
				return e1;
			} else {
				return mk_expr_add(e1, e2);
			}
		}
		case minus: {
			Expr* e1 = eval_with_state_list(e->sub_exprs->head, sl);
			Expr* e2 = eval_with_state_list(e->sub_exprs->tail->head, sl);
			if(cnst == e1->type && cnst == e2->type) {
				e1->value.int_value = e1->value.int_value - e2->value.int_value;
				clean_expr(e2);
				// free(e);
				return e1;
			} else {
				return mk_expr_minus(e1, e2);
			}
		}
		case ando: {
			Expr* e1 = eval_with_state_list(e->sub_exprs->head, sl);
			Expr* e2 = eval_with_state_list(e->sub_exprs->tail->head, sl);
			if(cnst == e1->type && cnst == e2->type) {
				e1->value.int_value = e1->value.int_value == 1 && e2->value.int_value == 1? 1 : 0;
				clean_expr(e2);
				// free(e);
				return e1;
			} else {
				return mk_expr_ando(e1, e2);
			}
		}
		case oro: {
			Expr* e1 = eval_with_state_list(e->sub_exprs->head, sl);
			Expr* e2 = eval_with_state_list(e->sub_exprs->tail->head, sl);
			if(cnst == e1->type && cnst == e2->type) {
				e1->value.int_value = e1->value.int_value == 0 && e2->value.int_value == 0? 0 : 1;
				clean_expr(e2);
				// free(e);
				return e1;
			} else {
				return mk_expr_oro(e1, e2);
			}
		}
		case lt: {
			Expr* e1 = eval_with_state_list(e->sub_exprs->head, sl);
			Expr* e2 = eval_with_state_list(e->sub_exprs->tail->head, sl);
			if(cnst == e1->type && cnst == e2->type) {
				e1->value.int_value = e1->value.int_value < e2->value.int_value? 1 : 0;
				clean_expr(e2);
				// free(e);
				return e1;
			} else {
				return mk_expr_lt(e1, e2);
			}
		}
		case gt: {
			Expr* e1 = eval_with_state_list(e->sub_exprs->head, sl);
			Expr* e2 = eval_with_state_list(e->sub_exprs->tail->head, sl);
			if(cnst == e1->type && cnst == e2->type) {
				e1->value.int_value = e1->value.int_value > e2->value.int_value? 1 : 0;
				clean_expr(e2);
				// free(e);
				return e1;
			} else {
				return mk_expr_gt(e1, e2);
			}
		}
		case le: {
			Expr* e1 = eval_with_state_list(e->sub_exprs->head, sl);
			Expr* e2 = eval_with_state_list(e->sub_exprs->tail->head, sl);
			if(cnst == e1->type && cnst == e2->type) {
				e1->value.int_value = e1->value.int_value <= e2->value.int_value? 1 : 0;
				clean_expr(e2);
				// free(e);
				return e1;
			} else {
				return mk_expr_le(e1, e2);
			}
		}
		case ge: {
			Expr* e1 = eval_with_state_list(e->sub_exprs->head, sl);
			Expr* e2 = eval_with_state_list(e->sub_exprs->tail->head, sl);
			if(cnst == e1->type && cnst == e2->type) {
				e1->value.int_value = e1->value.int_value >= e2->value.int_value? 1 : 0;
				clean_expr(e2);
				// free(e);
				return e1;
			} else {
				return mk_expr_ge(e1, e2);
			}
		}
		default:
			return replicate_expr(e);
	}
}
/****************************************************************************************************************/
void clean_expr(Expr* e)
{
	printf("cleaning expression...\n");
	if(NULL != e) {
		Expr_list* sub_exprs = e->sub_exprs;
		while(NULL != sub_exprs)
		{
			Expr_list* freed = sub_exprs;
			clean_expr(sub_exprs->head);
			sub_exprs = sub_exprs->tail;
			free(freed);
		}
		free(e);
	}
	 

	// switch(e->type)
	// {
	// 	case nested_var:
	// 	case aray:
	// 	{
	// 		Expr_list* sub_exprs = e->sub_exprs;
	// 		while(NULL != sub_exprs)
	// 		{
	// 			Expr_list* freed = sub_exprs;
	// 			clean_expr(sub_exprs->head);
	// 			sub_exprs = sub_exprs->tail;
	// 			free(freed);
	// 		}
	// 		free(e); 
	// 		break;
	// 	}
	// 	case var_index:
	// 	case state_expr:
	// 	{
	// 		// free(e->value.string_value);
	// 		clean_expr(e->sub_exprs->head);
	// 		free(e);
	// 		break;
	// 	}
	// 	case svar:
	// 	case scalar:
	// 	{
	// 		free(e->value.string_value);
	// 		free(e);
	// 		break;
	// 	}
	// 	case negi:
	// 	case negb:
	// 	{
	// 		clean_expr(e->sub_exprs->head);
	// 		free(e);
	// 		break;
	// 	}
	// 	case add:
	// 	case minus:
	// 	case mult:
	// 	case ando:
	// 	case oro:
	// 	case mod:
	// 	case lt:
	// 	case gt:
	// 	case le:
	// 	case ge:
	// 	{
	// 		clean_expr(e->sub_exprs->head);
	// 		clean_expr(e->sub_exprs->tail->head);
	// 		free(e);
	// 		break;
	// 	}
	// 	default: free(e); break;
	// }
}
/***************************************************************************************************************/