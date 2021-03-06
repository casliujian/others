{
  open Parser
}

let integer = ['0'-'9']+
let float = ['0'-'9']+ '.' ['0'-'9']*
let iden = ['a'-'z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']*
let uiden = ['A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']*
let nl = '\r' | '\n' | "\r\n"

rule token = parse 
  | "import"    {Import}
  | "datatype"  {Datatype}
  | "int"       {TInt}
  | "float"     {TFloat}
  | "unit"      {TUnt}
  | "bool"      {TBool}
  | "array"     {TAray}
  | "list"      {TLst}
  | "function"  {Function}
  | "match"     {Match}
  | "with"      {With}
  | "Model"     {Model}
  | "property"  {Property}
  | "AX"        {AX}
  | "EX"        {EX}
  | "AF"        {AF}
  | "EG"        {EG}
  | "AR"        {AR}
  | "EU"        {EU}
  | "value"     {Value}
  | "let"       {Let}
  | "if"        {If}
  | "then"      {Then}
  | "else"      {Else}
  | "for"       {For}
  | "in"        {In}
  | "do"        {Do}
  | "done"      {Done}
  | "while"     {While}
  | "true"      {True}
  | "false"     {False}
  | "TRUE"      {Top}
  | "FALSE"     {Bottom}
  | "not"       {Neg}
  | "of"        {Of}
  | "state"     {State}
  | "next"      {Next}
  | integer as i  {Int (int_of_string i)}
  | float as f    {Float (float_of_string f)}
  | iden as id  {Iden id}
  | uiden as ui {UIden ui}
  | "|"         {Vertical}
  | "!"         {Negb}
  | "&&"        {Ando}
  | "||"        {Oro}
  | "/\\"       {And}
  | "\\/"       {Or}      
  | "="         {Equal}
  | "!="        {Non_Equal}
  | ","         {Comma}
  | ":"         {Colon}
  | "::"        {ColonColon}
  | ";"         {Semicolon}
  | "("         {LB1}
  | ")"         {RB1}
  | "["         {LB2}
  | "]"         {RB2}
  | "{"         {LB3}
  | "}"         {RB3}
  | "_"         {Underline}
  | "->"        {Arrow}
  | "<-"        {LArrow}
  | "<"         {LT}
  | ">"         {GT}
  | "<="        {LE}
  | ">="        {GE}
  | "."         {Dot}
  | ".."        {DotDot}
  | "+"         {Add}
  | "+."        {AddDot}
  | "-"         {Minus}
  | "-."        {MinusDot}
  | "*"         {Mult}
  | "*."        {MultDot}
  | nl        {Lexing.new_line lexbuf; token lexbuf}
  | [' ' '\t']+  {token lexbuf}
  | "//"        {comment_oneline_c lexbuf}
  | "/*"        {comment_multiline_c lexbuf}
  | "(*"        {comment_ocaml lexbuf}
  | eof         {EOF}
  | _ as s      {print_endline ("unknown charcter: "^(String.make 1 s)); token lexbuf}
and comment_oneline_c = parse
  | nl    {Lexing.new_line lexbuf; token lexbuf}
  | _       {comment_oneline_c lexbuf}
and comment_multiline_c = parse
  | "*/"    {token lexbuf}
  | nl      {Lexing.new_line lexbuf; comment_multiline_c lexbuf}
  | _       {comment_multiline_c lexbuf}
and comment_ocaml = parse
  | "*)"    {token lexbuf}
  | nl      {Lexing.new_line lexbuf; comment_ocaml lexbuf}
  | _       {comment_ocaml lexbuf}