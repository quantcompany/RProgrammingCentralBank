set.seed(1)
arrival<-rexp(500,rate=2)
times<-arrival[1]
for(i in 2:500){times[i]<-times[i-1]+arrival[i]}
service<-rexp(500,rate=2)
wait<-0
for(i in 2:500){wait[i]<-max(wait[i-1]+service[i-1]-arrival[i],0)}
servtimes<-service[1]+times[1]
for(i in 2:500){servtimes[i]<-times[i]+wait[i]+service[i]}
plot(times,type='l')
lines(servtimes,col='red')
plot(wait,type='l')

wait<-matrix(rep(0,500*500),nrow=500)
for(j in 1:500){
set.seed(j)
arrival<-rexp(500,rate=2)
times<-arrival[1]
for(i in 2:500){times[i]<-times[i-1]+arrival[i]}
service<-rexp(500,rate=2)
for(i in 2:500){wait[i,j]<-max(wait[i-1,j]+service[i-1]-arrival[i],0)}
servtimes<-service[1]+times[1]
for(i in 2:500){servtimes[i]<-times[i]+wait[i,j]+service[i]}
}