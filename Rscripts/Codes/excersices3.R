###Excersices 3 of PROGRAMMING EXERCISES FOR R;

f1<-function(x){
	y<-vector()
	for(i in 1:length(x)){
		y[i]<-x[i]^i
			}
y
}
f2<-function(x){
	y<-vector()
	for(i in 1:length(x)){
		y[i]<-x[i]^i
			}
y/(1:length(x))
}

f3<-function(x,n){
	sum(f2(rep(x,n))+1
}
##Moving averages, where size is the step size
f4<-function(x,size=3){
	y<-vector()
	for(i in 1:(length(x)-size)){
		y[i]<-(sum(x[i:(i+size-1)]))/size
}
y
}

## Don't forget to make this example, as "a function inside another function"

f4<-function(x) {
	if (x<0) {x^2+2*x+3}
	else if (x>=0 && x<2*pi) {20*sin(x*100)}
	else {x^2+4*x-7} ## this means x>2
}
f5<-function(x){
	y<-as.vector(sapply(x,f4))
	plot(x,y,type='l')
}
###################
## this function doubles the odd number in a matrix 

f6<-function(x){
if((x-2*(x%/%2))==1) x<-2*x
x
}
f7<-function(m){
	matrix(sapply(m,f6),nrow=dim(m)[1])
}
################
f8<-function(n,k=1,dif=0){
	m<-matrix(rep(0,n*n),ncol=n)
	for(i in 1:n) {
	if((i+dif)<=n) {m[i+dif,i]<-k}
}
m
}
f9<-function(n,k){
f8(n,dif=1)+f8(n,k,dif=0)+f8(n,dif=-1)
}
####################
f10<-function(n){
	a<-c(1,2,rep(0,n-2))
	for(i in 2:n){
	a[i]<-a[i-1]+2/a[i-1]
}
a
}
###########################
quadmap<-function(start,r,n){
a<-c(start,r*start*(1-start),rep(0,n-2))
	for(i in 2:n){
	a[i]<-r*a[i-1]*(1-a[i-1])
}
a
}
############################## Solving a differece equation.
quadmap2<-function(start,r,tol){
a<-c(start,r*start*(1-start))
	i=2
	while(abs(a[i]-a[i-1])>tol){
	a[i+1]<-r*a[i]*(1-a[i])
	if (i>1000000) break
	else i=i+1
}
print(c(a[i],i))
}a<-
########################### Autocovariance
autocov<-function(x,k){
xbar1=mean(x[1:(length(x)-k)])
xbar2=mean(x[(length(x)-k+1):length(x))]
temp<-vector()
for(i in 1:(length(x)-k)){
temp[i]<-(x[i]-xbar1)*(x[i+k]-xbar2)
}
sum(temp)
}

####################### EX2B

f3<-function(A){A[which(rowSums(apply(A,2,is.na))==0),which(colSums(apply(A,2,is.na))==0)]}

copula<-function(a,b,x,y){n=length(x)
sum((rank(x)/(n+1))<a & (rank(x)/(n+1))<b)/n
}

f3<-function(n,g){
  y<-1:n
  for (i in 1:n){x<-1:i
                   for(j in 1:i){
                     sum(g(j,i))
                   }
}
f3<-function(n,g){
  A<-matrix(rep(0,n*n),nrow=n)
  for (i in 1:n){
      for (j in 1:n){A[i,j]=g(i,j)}
  total=sum(rowSums(A[,j>=i]))
  }
  print(total)
  print(A)
}
  
