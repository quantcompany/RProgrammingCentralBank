# A random walk, p=probability to go up by 1 unit, n=number of steps
rwalk<-function(start=0,n=100,p=0.5){
	x<-start
	for(i in 1:n){
	y<-rbinom(1,1,p)
	if (y==1){x[i+1]=x[i]+1} else {x[i+1]=x[i]-1}
}
plot(x,type='l',main='Random Walk',ylab='value')
}