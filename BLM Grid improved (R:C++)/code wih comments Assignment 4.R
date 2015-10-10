library(Rcpp)
library(devtools)
library(roxygen2)

###########################################################
# crunBMLGrid and C routines
###########################################################

######################## version 1 ########################
# version 1 is the version that performs the best

# Rccp function to move cars right
cppFunction('IntegerMatrix moveRightInC(IntegerMatrix m){
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
            }')

# Rccp function to move cars up
cppFunction('IntegerMatrix moveUpInC(IntegerMatrix m){
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
            }')

# Rccp function to wrap in the horizontal direction
cppFunction('IntegerMatrix wrapRightInC(IntegerMatrix m){
            int r=m.nrow();
            int c=m.ncol();       
            for(int i=0; i<r; i++){
            m(i,0)=m(i,c-2);
            m(i,c-1)=m(i,1);
            }
            return m;
            }')

# Rccp function to wrap in the vertical direction
cppFunction('IntegerMatrix wrapUpInC(IntegerMatrix m){
            int r=m.nrow();
            int c=m.ncol();       
            for(int j=0; j<c; j++){
            m(0,j)=m(r-2,j);
            m(r-1,j)=m(1,j);
            }
            return m;
            }')

# main function to move cars using Rccp
crunBMLGrid=function(m,n=1){
  r=nrow(m)
  c=ncol(m)
  m1=addTwo(m)
  for(i in 1:n){
    m1=moveRightInC(m1)
    m1=wrapRightInC(m1)
    m1=moveUpInC(m1)
    m1=wrapUpInC(m1)
  }
  m[]=m1[1:r,1:c]
  m
}

######################## version 2 ########################
# I change moveRightInC in this version

# Rccp function to move cars right version 2
cppFunction('IntegerMatrix moveRightInCv2(IntegerMatrix m){
            int r=m.nrow();
            int c=m.ncol();
            bool notMoved;
            /* notMoved indicates whether a car on the previous step moved */
            for(int i=0; i<r; i++){
            notMoved=true;
            for(int j=c-1; j>0; j--){
            if( notMoved & (m(i,j)==0) & (m(i,j-1)==1) ){
            m(i,j)=1;
            m(i,j-1)=0;
            notMoved=false;
            }else{
            notMoved=true;
            }
            }
            }
            return m;
            }')

# main function
crunBMLGridv2=function(m,n=1){
  r=nrow(m)
  c=ncol(m)
  m1=addTwo(m)
  for(i in 1:n){
    m1=moveRightInCv2(m1)
    m1=wrapRightInC(m1)
    m1=moveUpInC(m1)
    m1=wrapUpInC(m1)
  }
  m[]=m1[1:r,1:c]
  m
}

######################## version 3 ########################
# I replace wrapMatrixInC by a code in R 

crunBMLGridv3=function(m,n=1){
  r=nrow(m)
  c=ncol(m)
  m1=addTwo(m)
  for(i in 1:n){
    m1=moveRightInC(m1)
    m1[,1]=m1[,c+1]
    m1[,c+2]=m1[,2]
    m1=moveUpInC(m1)
    m1[1,]=m1[r+1,]
    m1[r+2,]=m1[2,]
  }
  m[]=m1[1:r,1:c]
  m
}

###########################################################
# profiling
###########################################################

# first table 
g=createBMLGrid(0.2,r=1000,c=1000)
Rprof()
g=crunBMLGrid(g,10000)
Rprof(NULL)
summaryRprof()

#first plot
v1=v2=v3=vector(,20)
v1[]=v2[]=v3[]=0
for(j in 1:5){
for(i in 1:20){
  g=createBMLGrid(i/21,r=500,c=500)
  Rprof()
  g1=crunBMLGrid(g,100)
  Rprof(NULL)
  v1[i]=summaryRprof()$sampling.time+v1[i]
  Rprof()
  g2=crunBMLGridv2(g,100)
  Rprof(NULL)
  v2[i]=summaryRprof()$sampling.time+v2[i]
  Rprof()
  g3=crunBMLGridv3(g,100)
  Rprof(NULL)
  v3[i]=summaryRprof()$sampling.time+v3[i]
}
}
plot((1:20)/21,v1,main="time vs density",
     xlab="density",ylab="time",xlim=c(0,1),ylim=c(0.5,3),
     pch=0)
points((1:20)/21,v2,pch=1)
points((1:20)/21,v3,pch=2)
legend(locator(1),c("Cv1","Cv2","Cv3"),pch=c(0,1,2))

# second plot
v1=v2=vector(,12)
v1[]=v2[]=0
for(i in 1:12){
    g=createBMLGrid(0.2,r=50*i,c=50*i)
    Rprof()
    g1=runBMLGrid(g,30)
    Rprof(NULL)
    v1[i]=summaryRprof()$sampling.time
    Rprof()
    g2=crunBMLGrid(g,30)
    Rprof(NULL)
    v2[i]=summaryRprof()$sampling.time
}
plot(1:12*50,v1,main="time vs linear size",
     xlab="linear size",ylab="time",xlim=c(0,700),ylim=c(0,17),
     pch=0)
points(1:12*50,v2*100,pch=1)
legend(locator(1),c("Rv2","Cv1*100"),pch=c(0,1))

# third plot
m=matrix(,20,5)
m[]=0
for(j in 1:4){
for(i in 1:20){
  g=createBMLGrid(0.2*j,r=50*i,c=50*i)
  Rprof()
  g1=crunBMLGrid(g,100)
  Rprof(NULL)
  m[i,j]=summaryRprof()$sampling.time
  m[i,5]=50*i
}
}

par(mfrow=c(2,2))
for(j in 1:4){
plot(m[,5],m[,j],main=c("time vs linear size, density=",
     as.character(j*0.2)), xlab="linear size",ylab="time",xlim=c(0,1050),
     ylim=c(0,1.4))
fit=lm(m[,j]~m[,5]+I(m[,5]^2))
points(m[,5], fitted(fit)[order(m[,5])], type='l')
}



###########################################################
# testing
###########################################################

# see the /test directory 

setwd("~/Desktop/22")
install("BMLGrid")
library(BMLGrid)
check("BMLGrid")
?createBMLGrid
?runBMLGrid
?crunBMLGrid
#tar("BMLGrid.tar.gz", "./BMLGrid", compression = 'gzip', tar="tar")

###########################################################
# old code from the assignment 2 
###########################################################

#create random matrix with entries 0, 1, 2 corresponding
#to none, red, blue cars
#cars of different color have the same number
createG=function(rowN, colN, redNum, blueNum){
  m=matrix(, rowN, colN);
  size=length(m);
  v=sample(1:size, redNum+blueNum, replace=FALSE);
  u=sample(v, blueNum, replace=FALSE);
  m[]=0; m[v]=1; m[u]=2;
  m
  #some assingments here are redundant but it doesn't
  #affect performance
}

#adds two rows and two columns two a matrix
#these new rows and columns repeat the first two
addTwo=function(m){
  m2=matrix(,nrow(m)+2,ncol(m)+2);
  m2[1:nrow(m),1:ncol(m)]=m[,];
  m2[1:2+nrow(m),]=m2[1:2,];
  m2[,1:2+ncol(m)]=m2[,1:2];
  m2
  #rbind/cbind work worse here
}

#this function performs n move for red cars and n
#for blue cars
#m is a matrix with 0, 1, 2
#that correspond to none, red, blue cars
moveN=function(m,n=1){
  colN=ncol(m);
  rowN=nrow(m);
  m=addTwo(m)
  m1=ifelse(m==1, TRUE, FALSE);
  m2=ifelse(m==2, TRUE, FALSE);
  #switch to Boolean matrices
  m3=mR1=mR2=matrix(, nrow(m), ncol(m));
  #preallocate
  for(i in 1:n){
    m3=m1 | m2;
    m1=moveRight(m1, m3, mR1, mR2, colN);
    m3=m1 | m2;
    m2=moveUp(m2, m3, mR1, mR2, rowN)
  };
  m=ifelse(m1, 1, ifelse(m2, 2, 0));
  m=m[1:(nrow(m)-2),1:(ncol(m)-2)];
  #switch back
  m
}

#right move
#m1 is a Boolean matrix of red cars
#m2 is a Boolean matrix of any cars
#mR1, mR2 are preallocated matrices - we don't want
#to create them inside a loop
#N - number of columns
moveRight=function(m1, m2, mR1, mR2, N){
  mR1[,2:(N+2)]=m1[,1:(N+1)];
  mR2[,1:(N+1)]=m2[,2:(N+2)];
  m1=ifelse(mR2, m1, FALSE) | ifelse(m2, FALSE, mR1);
  m1[,1]=m1[,N+1]; m1[,N+2]=m1[,2];
  m1
}

#move down in matrix, move up on plot
#m1 is a Boolean matrix of blue cars
#m2 is a Boolean matrix of any cars
#mR1, mR2 are preallocated matrices - we don't want to create them 
#inside a loop
#N - number of rows
moveUp=function(m1, m2, mR1, mR2, N){
  mR1[2:(N+2),]=m1[1:(N+1),];
  mR2[1:(N+1),]=m2[2:(N+2),];
  m1=ifelse(mR2,m1,FALSE) | ifelse(m2,FALSE,mR1);
  m1[1,]=m1[N+1,]; m1[N+2,]=m1[2,];
  m1
}
#we could make moveUp just applying moveRight to 
#transpose matrices but it would make moveUp slower

#create structures
createBMLGrid=function(density, r=100, c=100, ncars){
  if(missing(ncars)){
    redN=blueN=as.integer(density*r*c*0.5);
    m=createG(r, c, redN, blueN);
    structure(m, class=c("BMLGrid", class(m)))
  }else{
    if(missing(density)){
      m=createG(r, c, ncars[1], ncars[2]);
      structure(m, class=c("BMLGrid", class(m)))
    }else{
      print("Can't specify both density and ncars!")
    }
  }
}

#create structures
runBMLGrid=function(g, numSteps=100){
  g=moveN(g, numSteps);
  structure(g, class=c("BMLGrid", class(g)))
}

#function to measure the number of cars that moved,
#that were blocked, and the average
#velocity in each iteration between
#start time and end time
#returns matrix of these measurements
speedBMLGrid=function(m, start=0, end=1){
  m2=m;
  w=matrix(, end-start, 8);
  w[,8]=(start+1):end
  redT=sum(ifelse(m==1, 1, 0));
  blueT=sum(ifelse(m==2, 1, 0));
  colnames(w)=c("Red speed", "Blue speed", "Red move",
                "Blue move", "Red block", "Blue block", 
                "Total speed", "Step number");
  if(start!=0) { m=runBMLGrid(m,start) };
  for(i in (start+1):end){
    m2=runBMLGrid(m,1);
    w[i-start,5:6]=c(sum(ifelse(m==1 & m2==1, 1, 0)),
                     sum(ifelse(m==2 & m2==2, 1, 0)));
    m=m2
  };
  w[,3]=redT-w[,5]; w[,4]=blueT-w[,6]; 
  w[,1]=w[,3]/redT; w[,2]=w[,4]/blueT; 
  w[,7]=(w[,3]+w[,4])/(redT+blueT);
  w
}

#plot dependence between total speed at the iteration
#iterationStep and the density
#numberPoints specifies number of points on the plot
#in the interval (densitystart, densityend)
vectorDensitySpeed=function(numberPoints, densitystart,
                            densityend, iterationStep){
  dens=(densityend-densitystart)/(numberPoints+1);
  v=vector(,numberPoints+5);
  for(i in 1:numberPoints){
    m=createBMLGrid(densitystart+dens*i);
    w=speedBMLGrid(m,iterationStep,iterationStep+10);
    v[i]=sum(w[,7])/10
  };
  for(i in numberPoints+1:5){
    m=createBMLGrid((i-numberPoints)/6);
    w=speedBMLGrid(m,iterationStep,iterationStep+10);
    v[i]=sum(w[,7])/10
  };
  plot(c(densitystart+1:numberPoints * dens,1:5/6), v,
       main="Speed vs Density", 
       xlab="Density", ylab="Speed",);
  v
}

#plot s3 classes
plot.BMLGrid=function(x, submain=""){
  image(x,col=c("white","red","blue"),xaxt= "n", yaxt= "n", main=c("BMLGrid\n",submain) );
  arrows(0.05, 0.3, x1 = 0.95, y1 = 0.3, length = 0.7, col = "blue");
  arrows(0.05, 0.7, x1 = 0.95, y1 = 0.7, length = 0.7, col = "blue");
  arrows(0.7, 0.05, x1 = 0.7, y1 = 0.95, length = 0.7, col = "red");
  arrows(0.7, 0.05, x1 = 0.7, y1 = 0.95, length = 0.7, col = "red")
}

#summary s3 classes
summary.BMLGrid=function(x){
  l=list();
  for(i in 1:7){
    l[[i]]=speedBMLGrid(x,start=0,end=10)[1:10,i]
  };
  names(l)=c("Red_speed", "Blue_speed", "Red_move",
             "Blue_move", "Red_block", "Blue_block", 
             "Total_speed");
  l
}

# #make plots for the report
# v=vectorDensitySpeed(20,0.35,0.45,1800)
# v1=createBMLGrid(0.3)
# w1=speedBMLGrid(v1,end=800)
# v2=createBMLGrid(0.5)
# w2=speedBMLGrid(v2,end=800)
# par(mfrow=c(1,2))
# plot (c(0,800),c(0,1),type="n",xlab="step number",
# ylab="speed", main="Speed vs iteration \n with density=0.3") 
# lines(w1[,8],w1[,1],col="red",lwd=2.5)
# lines(w1[,8],w1[,2],col="blue",lwd=2.5)
# plot (c(0,800),c(0,1),type="n",xlab="step number",
# ylab="speed", main="Speed vs iteration \n with density=0.5")
# lines(w2[,8],w2[,1],col="red",lwd=2.5)
# lines(w2[,8],w2[,2],col="blue",lwd=2.5)
# 
# #plot image
#  g=createBMLGrid(0.2)
#  g=runBMLGrid(g,1000)
#  par(mfrow=c(1,2))
#  plot(g,"density=0.2")
#  g=createBMLGrid(0.5)
#  g=runBMLGrid(g,1000)
#  plot(g,"density=0.5")


#now we compare this approach to a naive one
#we write new functions that perform moves

moveNaiveR=function(m){
  r=nrow(m);
  c=ncol(m);
  for(i in 1:r){
    for(j in c:2){
      if(m[i,j]==0 & m[i,j-1]==1){
        m[i,j]=1;
        m[i,j-1]=4
      }
    }
  };
  m=ifelse(m==4, 0, m);
  m
}
moveNaiveU=function(m){
  r=nrow(m);
  c=ncol(m);
  for(j in 1:c){
    for(i in r:2){
      if(m[i,j]==0 && m[i-1,j]==2){
        m[i,j]=2;
        m[i-1,j]=4
      }
    }
  };
  m=ifelse(m==4, 0, m);
  m
}
#we could use transpose
#but it wouldn't be clear
moveNaive=function(m,n){
  r=nrow(m);
  c=ncol(m);
  m1=addTwo(m)
  for(i in 1:n){
    m1=moveNaiveR(m1);
    m1[,1]=m1[,c+1];
    m1[,c+2]=m1[,2];
    m1=moveNaiveU(m1);
    m1[1,]=m1[r+1,];
    m1[r+2,]=m1[2,]
  };
  m[]=m1[1:r,1:c];
  m
}