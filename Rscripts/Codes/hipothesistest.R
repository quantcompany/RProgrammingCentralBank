##Generating the matrix;
#Suppose theta=(0.1,0.5,0.75); and X~binomial(n,theta);
theta<-c(0.1,0.5,0.75)
n<-3
Mat<-matrix(NA,ncol=length(theta),nrow=(n+1));
for (i in 1:(n+1)){
  for (j in 1:length(theta)){
    Mat[i,j]<-dbinom(x=(i-1),size=n,prob=theta[j])
  }
}
colnames(Mat)<-as.character(theta)
rownames(Mat)<-as.character(0:n)
Mat

#Now creating the rule, which selects the max from each row;

guessvec<-which(apply(Mat,1,max)==Mat,arr.ind=TRUE)[,2]
guessvec1<-theta[guessvec]
names(guessvec1)<-names(guessvec)


## The simulation;
#initializing all the vectors;

k<-10000
guess<-0
truetheta<-0
x<-0
outcome<-0
##Powerful loop!;

for (i in 1:k){
  truetheta[i]=sample(theta,1)
  x[i]<-rbinom(1,size=n,prob=truetheta[i])
  guess[i]<-guessvec1[x[i]+1]
  if (guess[i]==truetheta[i]) outcome[i]='Correct' else outcome[i]='Error'
}
##This are the tables;
tabla<-data.frame('Realidad'=truetheta,'R.V.Value'=x,'Guess'=guess,'Resultado'=outcome)
table(tabla$Resultado)/k
table(tabla[,1],tabla[,2])
##Pending: Make a chi-square test expected vs observed;
#Also connect this with the hypothesis testing;
##Why is this the optimal rule?
##Now let's look at the first two columns from tabla, and suppose you are a 'data analyst'
##find the relationship among x and y;