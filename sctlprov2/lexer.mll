{
  open Parser
}

let integer = ['0'-'9']+
let float = ['0'-'9'] '.' ['0'-'9']*
let iden = ['a'-'z' 'A'-'Z'] ['a'-'z' 'A'-'Z' '0'-'9' '_' '-']*
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
  | "val"       {Val}
  | "var"       {Var}
  | "if"        {If}
  | "Then"      {Then}
  | "Else"      {Else}
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
  | integer as i  {Int (int_of_string i)}
  | float as f    {Float (float_of_string f)}
  | iden as id  {Iden id}
  | uiden as ui {UIden ui}
  | "!"         {Negb}
  | "&&"        {Ando}
  | "||"        {Oro}
  | "/\\"       {And}
  | "\\/"       {Or}      
  | "="         {Equal}
  | "!="        {Non_Equal}
  | "|"         {Vertical}
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
and comment_oneline_c = parse
  | '\n'    {token lexbuf}
  | _       {comment_oneline_c lexbuf}
and comment_multiline_c = parse
  | "*/"    {token lexbuf}
  | _       {comment_multiline_c lexbuf}
and comment_ocaml = parse
  | "*)"    {token lexbuf}
  | _       {comment_ocaml lexbuf}