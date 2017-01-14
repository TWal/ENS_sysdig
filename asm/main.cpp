#include <iostream>
#include <string>
#include <vector>
#include <fstream>
#include <map>
#include "Scanner.h"

using namespace std;

int main(int argc, char **argv) {
    string outfile = "a.out";
    string infile = "";
    bool verbose = false;
    for(int i =1 ; i < argc ; ++i){
        string s = argv[i];
        if (s == "-o"){
            if(i+1 >= argc){
                cerr<< "fatal error : option -o is not followed by a file name" <<endl;
                return 1;
            }
            outfile = argv[i+1];
            ++i;
        }
        else if(s == "-v"){
            verbose = true;
        }
        else {
            infile = s;
        }
    }
    if( infile == ""){
        cerr << "fatal error : No input file specified" << endl;
        return 1;
    }
    ifstream input (infile);
    if(!input){
        cerr << "fatal error : File \"" << infile << "\" not found."<<endl;
        return 1;
    }
    if(verbose) cout << "Beggining assembling" << endl;
    //return values;
    vector<Instruction*> instrs;
    map<string,ushort> labels;
    int finalsize = 0;

    Scanner scan(&input,infile);
    yy::parser parser(scan,instrs,labels,finalsize);
    parser.parse();
    if(verbose) {
        cout << "Parsing Ended : " <<endl ;
        for(auto i : instrs) {
            i->print(cout,labels);
            cout << endl;
        }
    }
    ofstream output (outfile);
    if(!output){
        cerr << "fatal error : Can't open output file" << endl;
        return 1;
    }
    for(auto i : instrs) {
        i->write(output,labels);
    }
    if(verbose) cout << "Assembling has finished with success";


    return 0 ;
}
