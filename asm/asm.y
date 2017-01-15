%code requires{
#include <string>
#include <vector>
#include <map>
class Scanner;
#include "Instr.h"
}

%{
#include <iostream>
#include "asm.hpp"
#include "Scanner.h"
#include <string>
#include <vector>
#include <map>

using namespace std;

void error (const yy::location& loc,const std::string& st){
    //cout << "hey" << endl;
    if(loc.begin.line != loc.end.line)
        cerr <<"File \"" << *loc.begin.filename << "\" , line "
             << loc.begin.line << "-" << loc.end.line << " : " << st<< endl;
     else
         cerr <<"File \"" << *loc.begin.filename << "\" , line "
              << loc.begin.line << ", characters " << loc.begin.column
              << "-" << loc.end.column <<": " << st<< endl;
     exit(EXIT_FAILURE);
}

void yy::parser::error(const yy::location& loc,const std::string& st)
{
    ::error(loc,st); // calling global error function and not this one.
}

%}

// C++ declaration
%skeleton "lalr1.cc"
%language "c++"
%define api.value.type variant


// Token declaration
%token  <int>           IMMEDIATE
%token  <int>           REGISTER
%token  <std::string>   LABEL
%token                  DOTS
%token                  NEWLINE

%token  <int>           SINS_BIN
%token  <int>           SINS_UN
%token  <int>           SINS_UN_IMM
%token  <int>           SINS_JFLAG
%token  <int>           SINS_JTEST

%token                  LINS_JMP
%token                  LINS_CALL
%token                  LINS_LIMM
%token  <int>           LINS_JFLAG
%token  <int>           LINS_JTEST
%token                  LINS_GPU

%token  <int>           INS_MEM_BIN
%token  <int>           INS_MEM_UN
%token  <int>           INS_MEM

%token  <int>           SGPU

%type   <Instruction*> shortInstr
%type   <Instruction*> longInstr
%type   <Instruction*> membin



%parse-param {Scanner& scan}
%parse-param {std::vector<Instruction*>& res}
%parse-param {std::map<std::string,ushort>& labels}
%parse-param {int& size} // must be zero-initialized

%code{
    // declare the parser fonction to call :
#define yylex scan.yylex
 }

// Better error explanation than "syntax error"
%define parse.error verbose

 //Location tracking for errors
%locations

 //Location itnitialisation 
%initial-action
{
    @$.initialize (scan.getName());
};




%start program

%%

// recognize one or more NEWLINE
newlines:
    |   newlines NEWLINE
        ;

shortInstr:
        SINS_BIN REGISTER REGISTER
        {
            auto tmp = new ShortI;
            tmp->opcode = 0;
            tmp->dest = $3;
            tmp->src = $2;
            tmp->func = $1;
            $$ = tmp;
        }
    |   SINS_UN REGISTER
        {
            auto tmp = new ShortI;
            tmp->opcode = 1;
            tmp->dest = $2;
            tmp->func = $1;
            $$ = tmp;
        }
    |   SINS_UN_IMM IMMEDIATE REGISTER
        {
            auto tmp = new ShortI;
            tmp->opcode = 1;
            tmp->dest = $3;
            if($2 <0 or $2 >= 16) {
                error(@2,
                "This immediate value is for shifting and must not go beyond 15 or under 0");
            }
            tmp->src = $2;
            tmp->func = $1;
            $$ = tmp;
        }
    |   SINS_JFLAG
        {
            auto tmp = new ShortI;
            tmp->opcode = 4;

            tmp->func = $1;
            $$ = tmp;
        }
    |   SINS_JTEST REGISTER REGISTER
        {
            auto tmp = new ShortI;
            tmp->opcode = 5;
            tmp->dest = $3;
            tmp->src = $2;
            tmp->func = $1;
            $$ = tmp;
        }
    |   INS_MEM_UN REGISTER
        {
            auto tmp = new ShortI;
            tmp->opcode = 2;
            tmp->dest = $2;
            tmp->func = $1;
            $$ = tmp;
        }

    |   INS_MEM
        {
            auto tmp = new ShortI;
            tmp->opcode = 2;
            tmp->func = $1;
            $$ = tmp;
        }
    ;

longInstr:
        LINS_JMP LABEL
        {
            auto tmp = new LongI;
            tmp->opcode = 10;
            tmp->label = $2;
            $$ = tmp;
        }
    |   LINS_CALL LABEL
        {
            auto tmp = new LongI;
            tmp->opcode = 11;
            tmp->label = $2;
            $$ = tmp;
        }
    |   LINS_LIMM IMMEDIATE REGISTER
        {
            auto tmp = new LongI;
            tmp->opcode = 13;
            tmp->dest = $3;
            if ($2 <-(1<<15) or $2 >= 1 << 16){
                error(@2,"This value is outside the range of a 16 bit value (signed or not) ");
            }
            tmp->val = $2 & 0x0000FFFF;
            $$ = tmp;
        }
    |   LINS_JFLAG LABEL
        {
            auto tmp = new LongI;
            tmp->opcode = 8;
            tmp->label = $2;
            tmp->func = $1;
            $$ = tmp;
        }
    |   LINS_JTEST REGISTER REGISTER LABEL
        {
            auto tmp = new LongI;
            tmp->opcode = 10;
            tmp->dest = $3;
            tmp->src = $2;
            tmp->func = $1;
            tmp->label = $4;
            $$ = tmp;
        }
    |   LINS_GPU SGPU REGISTER
        {
            auto tmp = new LongI;
            tmp->opcode = 15;
            tmp->src = $3;
            tmp->val = $2;
            $$ = tmp;
        }
    ;

membin:
        INS_MEM_BIN REGISTER REGISTER
        {
            size +=1;
            auto tmp = new ShortI;
            tmp->opcode = 2;
            tmp->dest = $3;
            tmp->src = $2;
            tmp->func = $1;
            $$ = tmp;
        }
    |  INS_MEM_BIN REGISTER REGISTER IMMEDIATE
        {
            size += 2;
            auto tmp = new LongI;
            tmp->opcode = 12;
            tmp->dest = $3;
            if ($4 <-(1<<15) or $4 >= 1 << 15){
                error(@4,"This value is outside the range of a 16 bit signed value");
            }
            tmp->val = $4 & 0x0000FFFF;
            tmp->src = $2;
            tmp->func = $1;
            $$ = tmp;
        }
    ;

program:
        newlines
    |   program LABEL DOTS newlines
        {
            //cout << "Parser label :" << $2 << " at : " << size <<endl;
            labels[$2] = size;
        }

    |   program shortInstr newlines
        {
            size += 1;
            $2->loc = @2;
            res.push_back($2);
        }
    |   program longInstr newlines
        {
            size +=2;
            $2->loc = @2;
            res.push_back($2);
        }
    |   program membin newlines
        {
            $2->loc = @2;
            res.push_back($2);
        }
        ;

%%


