
%{
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include "y.tab.h"
int yylex(void);
void yyerror(const char*);
#define YYERROR_VERBOSE

struct label_list {
    struct label_list* next;
    char* label;
    int address;
};

// O(1)
void add_to_list(struct label_list** l, char* label, int address) {
    printf("Adding %s\n", label);
    struct label_list* n = malloc(sizeof(struct label_list));
    n->next    = *l;
    n->label   = label;
    n->address = address;
    *l = n;
}

// O(len(l))
int lookup_address(struct label_list* l, char* label) {
    if(l == NULL)                         yyerror("Undefined label");
    else { printf("Comparing \"%s\" with \"%s\"\n", l->label, label);
    if(strcmp(l->label, label) == 0) return l->address;
    else                                  return lookup_address(l->next, label);
    }
}

// Points right after the last read instruction
static int next = 0;
// Gives the offset in multiple of 16 bits of the last instruction
static int address = -1;
// File descriptor to the out file
int fd;
// List of labels
struct label_list* labels = NULL;

#define MASK4 0x0f
#define MASK8 0xff

void write_short(int code, int fun, int reg1, int reg2) {
    char ins[2];
    ins[0]  = (code & MASK4) << 4;
    ins[0] |= reg2 & MASK4;
    ins[1]  = (reg1 & MASK4) << 4;
    ins[1] |= fun & MASK4;
    if(write(fd, ins, 2) != 2) yyerror("Write error");

    address = next;
    ++next;
}

void write_long(int code, int fun, int reg1, int reg2, int val) {
    char ins[4];
    ins[0]  = (code & MASK4) << 4;
    ins[0] |= reg2 & MASK4;
    ins[1]  = (reg1 & MASK4) << 4;
    ins[1] |= fun & MASK4;
    ins[2] = (val >> 8) & MASK8;
    ins[3] = val & MASK8;
    if(write(fd, ins, 4) != 4) yyerror("Write error");

    address = next;
    next += 2;
}

%}

%union {
    char* string;
    int   integer;
};

%token <integer> IMMEDIATE
%token <integer> REGISTER
%token <string>  LABEL
%token           DOTS
%token           NEWLINE

%token <integer> SINS_BIN
%token <integer> SINS_UN
%token <integer> SINS_UN_IMM
%token <integer> SINS_JFLAG
%token <integer> SINS_JTEST

%token <integer> LINS_JMP
%token <integer> LINS_CALL
%token <integer> LINS_LIMM
%token <integer> LINS_JFLAG
%token <integer> LINS_JTEST 

%token <integer> INS_MEM_BIN
%token <integer> INS_MEM_UN
%token <integer> INS_MEM
        
%%      

program:
     | LABEL DOTS NEWLINE program
         { printf("Hey : %s\n", $1); add_to_list(&labels, $1, next); }
     | SINS_BIN REGISTER REGISTER NEWLINE program
         { write_short(0, $1, $2, $3); }
     | SINS_UN  REGISTER          NEWLINE program
         { write_short(1, $1, 0, $2); }
     | SINS_UN_IMM IMMEDIATE REGISTER NEWLINE program
         { write_short(1, $1, $2, $3); }
     | SINS_JFLAG NEWLINE program
         { write_short(4, $1, 0, 0); }
     | SINS_JTEST REGISTER REGISTER NEWLINE program
         { write_short(5, $1, $2, $3); }
     | LINS_JMP LABEL NEWLINE program
         { write_long(10, 0, 0, 0, lookup_address(labels, $2)); }
     | LINS_CALL LABEL NEWLINE program
         { write_long(11, 0, 0, 0, lookup_address(labels, $2)); }
     | LINS_LIMM IMMEDIATE REGISTER NEWLINE program
         { write_long(13, 0, $3, 0, $2); }
     | LINS_JFLAG LABEL NEWLINE program
         { write_long(8, $1, 0, 0, lookup_address(labels, $2)); }
     | LINS_JTEST REGISTER REGISTER LABEL NEWLINE program
         { write_long(9, $1, $2, $3, lookup_address(labels, $4)); }
     | INS_MEM_BIN REGISTER REGISTER IMMEDIATE NEWLINE program
         { write_long(12, $1, $2, $3, $4); }
     | INS_MEM_BIN REGISTER REGISTER NEWLINE program
         { write_short(2, $1, $2, $3); }
     | INS_MEM_UN REGISTER NEWLINE program
         { write_short(2, $1, 0, $2); }
     | INS_MEM NEWLINE program
         { write_short(2, $1, 0, 0); }
     |
     ;

%%

void yyerror(const char* s) {
    fprintf(stderr, "%s\n", s);
    exit(EXIT_FAILURE);
}

int main(void) {
    fd = open("a.out", O_WRONLY | O_CREAT | O_TRUNC, S_IXUSR | S_IXGRP);
    yyparse();
    close(fd);
    return 0;
}

