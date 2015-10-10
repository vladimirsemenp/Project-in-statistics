// this program uses "bin" approach
// it gathers the distribution and puts it into results1.txt


#include <iostream>
#include <fstream>
#include <stdio.h>
#include <string>

using namespace std;
const int size=1000000;

int main() {
    ifstream inStream;
    ofstream outStream;
    inStream.open("/Users/a1/Desktop/data/Hdif_T.txt");
    outStream.open("/Users/a1/Desktop/HW/results1.txt");
    string s;
    int pos[size]={0};
    int neg[size]={0};
    int except[100]={0};
    int l, res=0;
    while(getline(inStream,s)){
        if(isdigit(s[0]) || isdigit(s[1])){
            l=stoi(s);
            if(abs(l)<size){
                if(l>0){
                    pos[l]=pos[l]+1;
                }else{
                    neg[-l]=neg[-l]+1;
                }
            }else{
                except[res]=l;
                res=res+1;
            }
        }
    }
    outStream << 0 << "," << neg[0] << endl;
    for(l=0;l<res;++l){
        outStream << except[l] << "," << 1 << endl;
    }
    for (l=1;l<size;++l){
        if(pos[l]!=0){
            outStream << l << "," << pos[l] << endl;
        }
        if(neg[l]!=0){
            outStream << -l << "," << neg[l] << endl;
        }
    }
    cout << res;
    inStream.close();
    outStream.close();
    return 0;
}
