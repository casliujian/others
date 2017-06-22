{
	open Parser
	let line_num = ref 1

(*let integer = ['0'-'9']+*)
}

let id = ['a'-'z' 'A' - 'Z'] ['a'-'z' 'A' - 'Z' '0'-'9' '_']*

rule token = parse
	| "MODULE"	{MODULE}
	| "main"		{MAIN}
	| "VAR"			{VAR}
	| "ASSIGN"	{ASSIGN}
	| "DEFINE"	{DEFINE}
	| "SPEC"		{SPEC}
	| "init"		{INIT}
	| "next"		{NEXT}
	| "AG"			{AG}
	| "("				{LB1}
	| ")"				{RB1}
	| ":"				{COLON}
	| ";"				{SEMICOLON}
	| ":="			{ASSIGNO}
	| "!"				{EXCLAM}
	| "--"			{comment lexbuf}
	| '0'				{FALSE}
	| '1'				{TRUE}
	| '&'				{AND}
	| '|'				{OR}
	| id as s		{ID s}
	| "\n"			{incr line_num; token lexbuf}
	| [' ' '\t' '\r']	{token lexbuf}
	| eof 			{File_end}
and comment = parse
	| "\n"	{incr line_num; token lexbuf}
	| eof		{File_end}
	| _			{comment lexbuf}


