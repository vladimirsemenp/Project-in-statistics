//
//
// functions to compute the density and the largest eigenvalues with Eigen

#include <iostream>
#include <complex>
#include <iostream>
#include <fstream>
#include <stdio.h>
#include <string>
#include<ctime>
#include <iostream>
#include <fstream>
#include <stdio.h>
#include <string>
#include <chrono>

#include "Eigen/Dense"

using namespace std;
using namespace Eigen;
using namespace std::chrono;

const int size=49;


double sqInv(MatrixXd A, double lambda, double epsilon){
    for(int i=0;i<size;i++){
        A(i,i)=A(i,i) - lambda;
    }
    A = A * A;
    for(int i=0;i<size;i++){
        A(i,i)=A(i,i) + epsilon * epsilon;
    }
    A = A.inverse();
    double trace=0;
    for(int i=0;i<size;i++){
        trace = trace + A(i,i);
    }
    trace = trace / size;
    trace = trace * epsilon;
    return trace;
}

double quadInv(MatrixXd A, double lambda, double epsilon){
    for(int i=0;i<size;i++){
        A(i,i)=A(i,i) - lambda;
    }
    A = A * A;
    A = A * A;
    for(int i=0;i<size;i++){
        A(i,i)=A(i,i) + (epsilon * epsilon) * (epsilon * epsilon);
    }
    A = A.inverse();
    double trace=0;
    for(int i=0;i<size;i++){
        trace = trace + A(i,i);
    }
    trace = trace / size;
    trace = trace * (epsilon * epsilon * epsilon);
    return trace;
}

double maxEigen(MatrixXd A, int degree){
    MatrixXd B=A;
    int Power=pow(2,degree);
    for(int i=0;i<Power-1;i++){
        A= A * B;
    }
    double trace=0;
    for(int i=0;i<size;i++){
        trace = trace + A(i,i);
    }
    for(int i=0;i<degree;i++){
        trace=sqrt(trace);
    }
    return trace;
}

int main()
{
    ofstream outStream;
    
    
    int i, j;
    double test;
    MatrixXd A(size,size);
    srand(time(0));
    for(i=0;i<size;i++){
        for(j=0;j<size;j++){
            A(i,j)=((rand() % 100)-50)/(sqrt(size)*100);
            A(j,i)=A(i,j);
        }
    }
    
    
    outStream.open("/Users/a1/Desktop/resCdensity.txt");
    outStream << "point,"<< "epsilon 0.3 sq," << "epsilon 0.05 sq," << "epsilon 0.3 quad," << "epsilon 0.05 quad" << endl;
    for(i=-300;i<301;i++){
        outStream << i*0.01 << "," << sqInv(A, i*0.01, 0.3) << "," << sqInv(A, i*0.01, 0.05)
        << "," <<
         quadInv(A, i*0.01, 0.3) << "," << quadInv(A, i*0.01, 0.05) << endl;
    }
    outStream.close();
    
    
    high_resolution_clock::time_point t1 = high_resolution_clock::now();
    for(i=-300;i<301;i++){
        test=sqInv(A, i*0.01, 0.3);
    }
    high_resolution_clock::time_point t2 = high_resolution_clock::now();
    auto duration1 = std::chrono::duration_cast<std::chrono::microseconds>( t2 - t1 ).count();
    outStream.open("/Users/a1/Desktop/resCother.txt");
    outStream << "max eigen: " << maxEigen(A, 8) << endl;
    outStream << "density duration: " << duration1 << endl;
    high_resolution_clock::time_point t3 = high_resolution_clock::now();
    for(int k=0;k<64;k++){
        for(i=0;i<size;i++){
            for(j=0;j<size;j++){
                A(i,j)=((rand() % 100)-50)/(sqrt(size)*100);
                A(j,i)=A(i,j);
            }
        }
        maxEigen(A, 8);
    }
    high_resolution_clock::time_point t4 = high_resolution_clock::now();
    auto duration2 = std::chrono::duration_cast<std::chrono::microseconds>( t4 - t3 ).count();
    outStream << "max eigen duration: " << duration2 << endl;
    outStream.close();
    
    cout << sqInv(A, 0, 0.01) << endl;
    cout << quadInv(A, 0, 0.01) << endl;
    cout << maxEigen(A, 8) << endl;

    return 0;
}
