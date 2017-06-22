#ifndef PARSER_HELPER_H_
#define PARSER_HELPER_H_
#include <stdio.h>
#include <stdlib.h>
#include "modul.h"
// #include "sctlparser.h"

typedef struct modul_defs
{
    Modul0* main_modul;
    Modul0_list* sub_moduls;
} Modul_defs;
#endif