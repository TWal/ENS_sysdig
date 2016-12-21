#ifndef INSTR_H
#define INSTR_H

#include <iostream>
#include <map>

typedef unsigned short ushort;



struct Instruction {
    Instruction():opcode(0),dest(0),src(0),func(0){}
    char opcode;
    char dest;
    char src;
    char func;
    virtual int getSize() = 0;
    virtual void write(std::ostream& out,const std::map<std::string,ushort>& labels) = 0;
    virtual void print(std::ostream& out,const std::map<std::string,ushort>& labels) {
        (void) labels;
        out << "opcode : " << (int)opcode << " dest : " << (int)dest
            << " src : " << (int)src << " func : " << (int)func;
    }
};

struct ShortI : public Instruction
{
    int getSize() { return 2;}
    void write(std::ostream& out,const std::map<std::string,ushort>& labels){
        (void)labels;
        char res[2];
        res[0] = opcode;
        res[0] += dest << 4;
        res[1] = src;
        res [1] += func << 4;
        out.write(res,2);
    }
};

static_assert(sizeof(short) == 2);

struct LongI : public Instruction
{
    LongI() : val(0) {}
    ushort val;
    std::string label;
    int getSize() { return 4;}
    void write(std::ostream& out,const std::map<std::string,ushort>& labels){
        char res[4];
        res[0] = opcode;
        res[0] += dest << 4;
        res[1] = src;
        res [1] += func << 4;
        ushort rval = val;
        if (label != "") rval = labels.at(label);
        reinterpret_cast<ushort*>(res)[1] = rval;
        out.write(res,4);
    }
    virtual void print(std::ostream& out,const std::map<std::string,ushort>& labels) {
        Instruction::print(out,labels);
        if(label == "") out << " val : " << val;
        else out << " label :" << label << " to : " << labels.at(label);
    }
};



#endif
