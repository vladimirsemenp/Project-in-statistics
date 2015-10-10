###########################################################
# crunBMLGrid and C routines
###########################################################

######################## version 1 ########################
# version 1 is the version that performs the best

# main function to move cars using Rccp
crunBMLGridv1=function(m,n){
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

#create structures
crunBMLGrid=function(g, numSteps=1){
    g=crunBMLGridv1(g, numSteps);
    structure(g, class=c("BMLGrid", class(g)))
}

###########################################################
# old code
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
runBMLGrid=function(g, numSteps=1){
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
plot.BMLGrid=function(x, submain="", ...){
  image(x,col=c("white","red","blue"),xaxt= "n", yaxt= "n", main=c("BMLGrid\n",submain) );
  arrows(0.05, 0.3, x1 = 0.95, y1 = 0.3, length = 0.5, col = "blue");
  arrows(0.05, 0.7, x1 = 0.95, y1 = 0.7, length = 0.5, col = "blue");
  arrows(0.3, 0.05, x1 = 0.3, y1 = 0.95, length = 0.5, col = "red");
  arrows(0.7, 0.05, x1 = 0.7, y1 = 0.95, length = 0.5, col = "red")
}

#summary s3 classes
summary.BMLGrid=function(x, ...){
  l=list();
  for(i in 1:7){
    l[[i]]=speedBMLGrid(x,start=0,end=10)[1:10,i]
  };
  names(l)=c("Red_speed", "Blue_speed", "Red_move",
             "Blue_move", "Red_block", "Blue_block", 
             "Total_speed");
  l
}