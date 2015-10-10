#include <Rcpp.h>
using namespace Rcpp;

// Below is a simple example of exporting a C++ function to R. You can
// source this function into an R session using the Rcpp::sourceCpp 
// function (or via the Source button on the editor toolbar)

// For more on using Rcpp click the Help button on the editor toolbar

// [[Rcpp::export]]

IntegerMatrix moveRightInC(IntegerMatrix m){
            int r=m.nrow();
            int c=m.ncol();
            for(int i=0; i<r; i++){
            for(int j=c-1; j>0; j--){
            if(m(i,j)==0 & m(i,j-1)==1){
            m(i,j)=1;
            m(i,j-1)=4;
            }else{
            if(m(i,j)==4){m(i,j)=0;}
            }
            }
            }
            return m;
            }

// [[Rcpp::export]]

IntegerMatrix moveUpInC(IntegerMatrix m){
            int r=m.nrow();
            int c=m.ncol();
            for(int j=0; j<c; j++){
            for(int i=r-1; i>0; i--){
            if(m(i,j)==0 & m(i-1,j)==2){
            m(i,j)=2;
            m(i-1,j)=4;
            }else{
            if(m(i,j)==4){m(i,j)=0;}
            }
            }
            }
            return m;
            }

// [[Rcpp::export]]

IntegerMatrix wrapRightInC(IntegerMatrix m){
            int r=m.nrow();
            int c=m.ncol();       
            for(int i=0; i<r; i++){
            m(i,0)=m(i,c-2);
            m(i,c-1)=m(i,1);
            }
            return m;
            }

// [[Rcpp::export]]

IntegerMatrix wrapUpInC(IntegerMatrix m){
            int r=m.nrow();
            int c=m.ncol();       
            for(int j=0; j<c; j++){
            m(0,j)=m(r-2,j);
            m(r-1,j)=m(1,j);
            }
            return m;
            }
