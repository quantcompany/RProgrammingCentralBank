##Generate the graph of confidence intervals
plot(c(10,10),c(0,100),type='l')
for(i in 1:100){
  if (int[i,1]<=10 & int[i,2]>=10) color='blue' else color='red'
  segments(int[i,1],i,int[i,2],i,col=color)}

x<-matrix(rnorm(100*4,mean=10),ncol=4)
Ux<-apply(x,1,mean)+1
Lx<-apply(x,1,mean)-1
int<-cbind(Lx,Ux)

f<-function(A){su<-0; m=dim(A)[1]; n=dim(A)[2]; i=1;
  while(i<=m){
    for(j in 1:n){su<-su+sum(A[i,j:n]);i=i+1}
  }   
print(su)
}