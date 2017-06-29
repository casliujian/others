open Expr

type formula = 
	  Top
	| Bottom
	| Atomic of string * (expr list)
	| Neg of formula
	| And of formula * formula
	| Or of formula * formula
	| AX of string * formula * expr
	| EX of string * formula * expr
	| AF of string * formula * expr
	| EG of string * formula * expr
	| AR of string * string * formula * formula * expr
	| EU of string * string * formula * formula * expr

