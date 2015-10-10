# Read data produced by C++

tabl=read.table("resCdensity.txt", skip=1, sep=",")
head(tabl)
tabl$V6=vector(,length(tabl$V1))
tabl$V6=0
tabl$V6[abs(tabl$V1)<0.551468]=3.13562*sqrt( 1- (tabl$V1[abs(tabl$V1)<0.551468]/0.551468)^2 )
plot(tabl$V1,tabl$V6)
tabl$V7=vector(,length(tabl$V1))
for(i in 1:length(tabl$V1)){
  tabl$V7[i]=sum( epsilon*tabl$V6/((tabl$V1-tabl$V1[i])^2+epsilon^2) )/sum ( tabl$V6 )
}

## Create plots.
plot(tabl$V1,tabl$V7, xlim=c(-1,1), ylim=c(0,3.2), pch=2, type="n",
     main="sq vs lambda", xlab="lambda", ylab="density")
lines(tabl$V1,tabl$V7, col="red", lwd=2)
lines(tabl$V1,tabl$V2, col="blue", lwd=6, lty="dotted")
lines(tabl$V1,tabl$V6, col="green")
legend('topright', c("Computed sq","Theoretical sq", "Theoretical DOS"), 
              lty=1, col=c('red', 'blue', 'green'), bty='n', cex=.5)

plot(tabl$V1,tabl$V3, xlim=c(-1,1), ylim=c(0,4), pch=0, col="red",
     main="sq and quad with small epsilon",
     xlab="lambda", ylab="sq/quad", "sq/quad")
points(tabl$V1,tabl$V5, pch=3, col= "blue")
legend('topright', c("sq","quad"), 
       pch=c(0,3), col=c('red', 'blue'), bty='n', cex=.5)

library(Matrix)
N=49
epsilon=0.3



matr=matrix(sample(1:100, N*N, repl=TRUE),N,N)
matr[lower.tri(matr)] = t(matr)[lower.tri(matr)]
matr[]=(matr[]-50)/(100*sqrt(N))

# Profiling and repeating in R what I did with C++

Rprof() 
tabl$V10=vector(,length(tabl$V1))
for(i in tabl$V1){
  tabl$V10=epsilon*sum(diag(solve((matr-i*diag(N))%*%(matr-i*diag(N))+epsilon^2)))/N
}
Rprof(NULL)
summaryRprof()

Rprof() 
for(i in 1:64){
  matr=matrix(sample(1:100, N*N, repl=TRUE),N,N)
  matr[lower.tri(matr)] = t(matr)[lower.tri(matr)]
  matr[]=(matr[]-50)/(100*sqrt(N))
  matrres=matr
  for(j in 1:(2^8)){matr=matr%*%matrres}
  temp=sum(diag(matr))
  temp^(2^(-8))
}
Rprof(NULL)
summaryRprof()