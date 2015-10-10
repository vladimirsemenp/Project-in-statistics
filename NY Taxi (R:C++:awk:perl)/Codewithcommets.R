######################### matching rows #################

# here I test if rows match
setwd("~/Desktop/data")
# now I test matching
# I go through all the files and test some lines there
# I don't test too many lines because it's too slow even with "sed" in shell
funTest=function(){
  options(scipen=100)
  n=TRUE
  for(j in 1:12){
    for(i in (1:13)*1000000){
      commandLineF=paste("sed '",toString(i),"q;d' trip_fare_",toString(j),".csv", sep = '')
      commandLineD=paste("sed '",toString(i),"q;d' trip_data_",toString(j),".csv", sep = '')
      conF<-pipe(commandLineF)
      conD<-pipe(commandLineD)
      dataVecF=unlist(strsplit(readLines( conF ),","))
      dataVecD=unlist(strsplit(readLines( conD ),","))
      if(length(dataVecF)>1){ 
        if (dataVecF[1]!=dataVecD[1] || dataVecF[2]!=dataVecD[2]){ n=FALSE } 
      }
      close(conF)
      close(conD)
    }
  }
  n
}
funTest()
# [1] TRUE
# this means that all the lines match

######################### text processing #################

# I process data using shell
options(scipen=100)
# extract trip time and delete "trip_data"
setwd("~/Desktop/data")
for(j in 1:12){
  commandLine=paste("(cut -f 9 -d ',' trip_data_",toString(j),".csv) > time_",toString(j),".txt", sep = '')
  system(commandLine)
  commandLine=paste("rm –Rf trip_data_",toString(j),".csv", sep = '')
  system(commandLine)
}

# extract (total amount less the tolls)*100 and extract (the surchage)*100
for(j in 1:12){
  commandLine=paste("awk -F, '{ print ($11-$10)*100 }' trip_fare_",toString(j),
                    ".csv > dif_",toString(j),".txt", sep = '')
  system(commandLine)
  commandLine=paste("awk -F, '{ print ($7)*100 }' trip_fare_",toString(j),
                    ".csv > surcharge_",toString(j),".txt", sep = '')
  system(commandLine)
  print(j)
}

# delete head in every file
for(j in 1:12){
  for(i in c("time_","dif_","surcharge_")){
    commandLine=paste("sed '1d' ",i,toString(j),
                      ".txt > H",i,toString(j),".txt", sep = '')
    system(commandLine)
    print(j)
  }
}

# concatenate files
for(j in 1:12){
  for(i in c("Htime_","Hdif_","Hsurcharge_")){
    commandLine=paste("cat ",i,toString(j),".txt >> ",i,"T.txt", sep = '')
    system(commandLine)
    print(j)
  }
}
# I have all the necessary data in the files "Htime_T.txt",
#"Hdif_T","Hsurcharge_T"

# # I will need another data representation
# # let's paste each three files
# # I use directly terminal now for convenience
# paste Hsurcharge_T.txt Htime_T.txt Hdif_T.txt > alll.txt
# 
# # now I get random samples from the files "allSTD_"
# perl -ne 'print if (rand() < .001)' alll.txt > randall.txt
# # split "randall.txt" into three columns for speedup
# awk '{ print $1 }' randall.txt > randall_S.txt
# awk '{ print $2 }' randall.txt > randall_T.txt
# awk '{ print $3 }' randall.txt > randall_D.txt

######################### treating deciles ################

# use sampling first

# load table
setwd("~/Desktop/data")
tab=NULL
tab=read.table("randall_S.txt")
tab[,2]=read.table("randall_T.txt")
tab[,3]=read.table("randall_D.txt")
tab[,4]=1
# > tab[1,]
# V1 V1.1 V1.2 V4
# 1  0  420  950  1
names(tab)=c("sur","time","dif","freq")

# plot density
par(mfrow=c(1,2))
hist(tab$dif,breaks=220,prob=TRUE,xlim=c(0,6000),
     main="Histogram",
     xlab="total - tolls (in cents)")
plot(density(tab$dif, adjust=0.02),col="red",lwd=2,
     xlim=c(0,6000),xaxt='n',
     main="Smoothed line",
     xlab="total - tolls (in cents)",ylab="Relative density")
# deciles
vectr=vector(,9)
for(i in 1:9){
  vectr[i]=quantile(tab[,3],i*0.1)
}
vectr
# > vectr
# [1]  600  750  850  975 1100 1300 1500 1850 2630
axis(1, at=vectr, labels=1:9, col="red")

# use C++ and the bin approach

# the following code is used in Xcode "distribution.cppp"
# // this program uses "bin" approach
# // it gathers the distribution and puts it into results1.txt
# 
# #include <iostream>
# #include <fstream>
# #include <stdio.h>
# #include <string>
# 
# using namespace std;
# const int size=1000000;
# 
# int main() {
#   ifstream inStream;
#   ofstream outStream;
#   inStream.open("/Users/a1/Desktop/data/Hdif_T.txt");
#   outStream.open("/Users/a1/Desktop/HW/results1.txt");
#   string s;
#   int pos[size]={0};
#   int neg[size]={0};
#   int except[100]={0};
#   int l, res=0;
#   while(getline(inStream,s)){
#     if(isdigit(s[0]) || isdigit(s[1])){
#       l=stoi(s);
#       if(abs(l)<size){
#         if(l>0){
#           pos[l]=pos[l]+1;
#         }else{
#           neg[-l]=neg[-l]+1;
#         }
#       }else{
#         except[res]=l;
#         res=res+1;
#       }
#     }
#   }
#   outStream << 0 << "," << neg[0] << endl;
#   for(l=0;l<res;++l){
#     outStream << except[l] << "," << 1 << endl;
#   }
#   for (l=1;l<size;++l){
#     if(pos[l]!=0){
#       outStream << l << "," << pos[l] << endl;
#     }
#     if(neg[l]!=0){
#       outStream << -l << "," << neg[l] << endl;
#     }
#   }
#   cout << res;
#   inStream.close();
#   outStream.close();
#   return 0;
# }

# find deciles
findDeciles=function(Value,Freq){
  vectr=vector(,9)
  sumFreq=sum(Freq)
  for(j in 1:9){
    k=0
    for(i in 1:length(Value)){
      if(k<j*sumFreq/10 & (k+Freq[i]+1)>(j*sumFreq/10)){vectr[j]=Value[i]}
      k=k+Freq  
    }
  }
  vectr
}

# plot histogram "Relative smoothed frequency"
setwd("~/Desktop/HW")
tabl=read.table("results1.txt",sep=",")
tabl=tabl[order(tabl$V1),]
ran=50
num=100
vectr=vector(,num)
for(i in 1:num){
  vectr[i]=sum(tabl$V2[abs(tabl$V1-i*ran)<2*ran])/1000000
}
plot((1:num)*ran,vectr,main="Relative smoothed frequency",
     ylab="relative smoothed frequency",xlab="total - tolls (with deciles)"
     , type="n",xaxt='n')
lines((1:num)*ran,vectr)
# find vector of deciles
vectr=vector(,9)
vectr=findDeciles(tabl$V1,tabl$V2)
#> vectr
#[1]  600  750  850  975 1100 1300 1500 1850 2612
axis(1, at=vectr, labels=1:9, col="red")
# label x-axis with deciles and find deciles

######################### treating regression ###########

# first I use C++

#the code "results3.cppp" is applied in Xcode:
# #include <iostream>
# #include <fstream>
# #include <stdio.h>
# #include <string>
# 
# using namespace std;
# 
# int main() {
#   ifstream inDif, inTime, inSur;
#   ofstream outStream;
#   string sD, sT, sS;
#   long long int sum[10]={0};
#   int intD, intT, intS, k=0, i;
#   inDif.open("/Users/a1/Desktop/data/Hdif_T.txt");
#   inTime.open("/Users/a1/Desktop/data/Htime_T.txt");
#   inSur.open("/Users/a1/Desktop/data/Hsurcharge_T.txt");
#   outStream.open("/Users/a1/Desktop/HW/results3.txt");
#   while(getline(inDif,sD)){
#     getline(inTime,sT);
#     getline(inSur,sS);
#     intD=stoi(sD);
#     intT=stoi(sT);
#     intS=stoi(sS);
#     if(abs(intT)<10000){
#       k++;
#       sum[0]=sum[0]+1;
#       sum[1]=sum[1]+intD;
#       sum[2]=sum[2]+intT;
#       sum[3]=sum[3]+intS;
#       sum[4]=sum[4]+intD*intD;
#       sum[5]=sum[5]+intT*intT;
#       sum[6]=sum[6]+intS*intS;
#       sum[7]=sum[7]+intD*intT;
#       sum[8]=sum[8]+intD*intS;
#       sum[9]=sum[9]+intT*intS;
#     }
#   }
#   inDif.close();
#   inTime.close();
#   inSur.close();
#   for(i=0;i<10;++i){
#     outStream << sum[i] << endl;
#   }
#   outStream.close();
#   cout << "Le Fin ! " << k;
#   return 0;
# }


setwd("~/Desktop/HW")
res2=unlist(read.table("results3.txt"))
names(res2)=c("num","D","T","S","DD","TT","SS","DT","DS","TS")
XX1=matrix(c(res2["num"],res2["T"],res2["T"],res2["TT"]),
           nrow=2,ncol=2)
XY1=c(res2["D"],res2["DT"])
XX2=matrix(c(res2["num"],res2["T"],res2["S"],
             res2["T"],res2["TT"],res2["TS"],
             res2["S"],res2["TS"],res2["SS"]),
             nrow=3,ncol=3)
XY2=c(res2["D"],res2["DT"],res2["DS"])

solve(XX1)%*%XY1
# > solve(XX1)%*%XY1
# [1,] 244.081450
# [2,]   1.604833
solve(XX2)%*%XY2
# > solve(XX2)%*%XY2
# [1,] 232.248080
# [2,]   1.606063
# [3,]   0.341510
MSE1=(res2["DD"]-(res2["D"]^2/res2["num"]))/(res2["num"]-2)
MSE2=(res2["DD"]-(res2["D"]^2/res2["num"]))/(res2["num"]-3)
MSE1
solve(XX1)*MSE1
# > solve(XX1)*MSE1
# [,1]              [,2]
# [1,]  0.05486163861 -0.00004677295823
# [2,] -0.00004677296  0.00000006206116
MSE2
solve(XX2)*MSE2
# > solve(XX2)*MSE2
# [,1]              [,2]             [,3]
# [1,]  0.07369685738 -0.00004873133251 -0.0005435827305
# [2,] -0.00004873133  0.00000006226479  0.0000000565185
# [3,] -0.00054358273  0.00000005651850  0.0000156877493


# do the same with our random sample in R

# > head(tab)
#   sur time  dif freq
# 1   0  420  950    1
# 2  50  540 1250    1
# 3  50 1560 2810    1

# drop outliers in time just like we did in C++
for(i in nrow(tab):1){
  if(tab[i,2]>10000){
    tab=tab[-i,]
  }
}

lm1=lm(dif~time, data=tab)
summary(lm1)
lm2=lm(dif~time+sur, data=tab)
summary(lm2)

# plot graph
vec=sample(1:nrow(tab),50,replace=FALSE)
plot(tab[vec,3],fitted(lm1)[vec],main="fitted vs observed",
     xlab="observed (in cents)",
     ylab="fitted (in cents)", pch=1)
points(tab[vec,3],fitted(lm2)[vec], pch=4)
legend(locator(1),c("Model 1","Model 2"), pch=c(1,4))