
#include <iostream>
#include <fstream>
#include <stdio.h>
#include <string>

using namespace std;

int main() {
    ifstream inDif, inTime, inSur;
    ofstream outStream;
    string sD, sT, sS;
    long long int sum[10]={0};
    int intD, intT, intS, k=0, i;
    inDif.open("/Users/a1/Desktop/data/Hdif_T.txt");
    inTime.open("/Users/a1/Desktop/data/Htime_T.txt");
    inSur.open("/Users/a1/Desktop/data/Hsurcharge_T.txt");
    outStream.open("/Users/a1/Desktop/HW/results3.txt");
    while(getline(inDif,sD)){
        getline(inTime,sT);
        getline(inSur,sS);
        intD=stoi(sD);
        intT=stoi(sT);
        intS=stoi(sS);
        if(abs(intT)<10000){
        k++;
        sum[0]=sum[0]+1;
        sum[1]=sum[1]+intD;
        sum[2]=sum[2]+intT;
        sum[3]=sum[3]+intS;
        sum[4]=sum[4]+intD*intD;
        sum[5]=sum[5]+intT*intT;
        sum[6]=sum[6]+intS*intS;
        sum[7]=sum[7]+intD*intT;
        sum[8]=sum[8]+intD*intS;
        sum[9]=sum[9]+intT*intS;
        }
    }
    inDif.close();
    inTime.close();
    inSur.close();
    for(i=0;i<10;++i){
        outStream << sum[i] << endl;
    }
    outStream.close();
    cout << "Le Fin ! " << k;
    return 0;
}
