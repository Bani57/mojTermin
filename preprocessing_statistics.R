z.interval<-function(X,doverba)
{
  n=length(X)
  if(n>=30)
  {
    alfa=1-doverba
    zalfa=qnorm(1-alfa/2)
    S=sd(X)
    paste('Интервал на доверба за математичкото очекување: (',mean(X)-zalfa*S/sqrt(n),',',mean(X)+zalfa*S/sqrt(n),')')
  }
  else paste('Дисперзијата не е позната и примерокот е многу мал, не може да се користи z-тест!')
}

t.interval<-function(X,doverba)
{
  n=length(X)
  if(n>=30)
    print('Дисперзијата не е позната но примерокот е поголем, не мора да се користи t-тест.')
  alfa=1-doverba
  talfa=qt(1-alfa/2,n-1)
  S=sd(X)
  paste('Интервал на доверба за математичкото очекување: (',mean(X)-talfa*S/sqrt(n),',',mean(X)+talfa*S/sqrt(n),')')
}

normalize<-function(x,new_min=0,new_max=1)
{
  return ((new_max-new_min)*(x-min(x))/(max(x)-min(x))+new_min)
}

find_outliers<-function(x)
{
  q1=quantile(x,0.25)
  q3=quantile(x,0.75)
  iqr=q3-q1
  return(c(which(x<q1-1.5*iqr),which(x>q3+1.5*iqr)))
}

analyze_discrete<-function(x,name)
{
  x_stats=as.data.frame(table(x))
  x_freq=as.numeric(unname(unlist(x_stats$Freq)))
  m=mean(x_freq)
  S=sd(x_freq)
  par(mfrow=c(1,1))
  barplot(x_freq,main=paste('Barplot за ',name),xlab=name,names.arg=x_stats[,1],col=3)
  abline(h=sum(x_freq)/length(x_freq),col="blue",lwd=3,lty=2)
  p=chisq.test(x_freq)$p.value
  print(paste('Величината има рамномерна распределба: ',p>0.01))
  print(paste('p - вредноста: ', p))
  print(paste('Просек на примерокот: ',m))
  print(paste('Стандардна девијација на примерокот: ',S))
  t.interval(x_freq,doverba=0.99)
}

analyze_continous<-function(x,name)
{
  m=mean(x)
  S=sd(x)
  par(mfrow=c(1,2))
  hist(x,freq=FALSE,main=paste('Хистограм за ',name),xlab=name,ylim=c(0,2*max(density(x)$y)),col=3)
  lines(density(x),lwd=3)
  curve(dnorm(x,m,S),add=TRUE,col="blue",lwd=3,lty=2)
  legend("topright", c("Expected distribution", "Actual distribution"), lty=c(2, 1) , lwd=c(3, 3), col=c("blue","black"),box.lwd=3)
  boxplot(x,main=paste('Boxplot за ',name),varwidth=TRUE,col=3)
  par(mfrow=c(1,1))
  p=ks.test(x+runif(length(x),0,0.001),"pnorm",mean=m,sd=S)$p.value
  print(paste('Величината има нормална распределба: ',p>0.01))
  print(paste('p - вредноста: ', p))
  print(paste('Просек на примерокот: ', m))
  print(paste('Стандардна девијација на примерокот: ', S))
  z.interval(x,doverba=0.99)
}


dataset_referrals <- read.csv("./data26/task2_to_6_referrals.csv", sep=";", stringsAsFactors=FALSE)
date_referrals_missing=which(is.na(as.Date(dataset_referrals$realized_date,format="%d.%m.%Y")))
age_referrals_missing=which(is.na(dataset_referrals$age_at_realization))
print(100*length(union(date_referrals_missing,age_referrals_missing))/length(dataset_referrals[,1]))
dataset_referrals=dataset_referrals[which(!is.na(dataset_referrals$realized_date)&!is.na(dataset_referrals$age_at_realization)),]

dataset_referrals_children=dataset_referrals[which(dataset_referrals$age_at_realization<=18),]
dataset_referrals_adults=dataset_referrals[which(dataset_referrals$age_at_realization>18),]

date_referrals=as.Date(dataset_referrals$realized_date,format="%d.%m.%Y")
age_referrals=as.numeric(unname(unlist(dataset_referrals$age_at_realization)))

year_referrals=as.numeric(unname(unlist(format(date_referrals,"%Y"))))
date_referrals <- as.POSIXlt(date_referrals)
date_referrals[which(year_referrals<1900)]$year=date_referrals[which(year_referrals<1900)]$year+100
date_referrals[which(year_referrals<2000)]$year=date_referrals[which(year_referrals<2000)]$year+15
date_referrals=as.Date(date_referrals)

analyze_continous(as.numeric(date_referrals),"датум на дијагноза")
ks.test(as.numeric(unique(date_referrals)),"punif",min(date_referrals),max(date_referrals))$p.value>0.01

age_referrals=abs(age_referrals)
analyze_continous(age_referrals,"возраст при дијагноза")

dataset_referrals_mapped_level1 <- read.csv("./data26/dataset_referrals_mapped_level1.csv", sep=";", stringsAsFactors=FALSE)
dataset_referrals_mapped_level0 <- read.csv("./data26/dataset_referrals_mapped_level0.csv", sep=";", stringsAsFactors=FALSE)

diagnosis_referrals=dataset_referrals$icd10_code
analyze_discrete(diagnosis_referrals,"препишана дијагноза")
analyze_discrete(dataset_referrals_mapped_level1$Level.1,"препишана дијагноза мапирана до ниво 1")
analyze_discrete(dataset_referrals_mapped_level0$Level.0,"препишана дијагноза мапирана до ниво 0")
analyze_discrete(dataset_referrals_children$icd10_code,"препишана дијагноза за деца")
analyze_discrete(dataset_referrals_adults$icd10_code,"препишана дијагноза за возрасни")

dataset_referrals$realized_date=date_referrals
dataset_referrals$age_at_realization=age_referrals
write.csv(dataset_referrals,"./data26/dataset_referrals_cleaned.csv",row.names = FALSE)

############################################

dataset_prescriptions <- read.csv("./data26/task2_to_6_prescriptions.csv", sep=";", stringsAsFactors=FALSE)
date_prescriptions_missing=which(is.na(dataset_prescriptions$realized_date))
age_prescriptions_missing=which(is.na(dataset_prescriptions$age_at_realization))
dataset_prescriptions=dataset_prescriptions[which(!is.na(as.Date(dataset_prescriptions$realized_date,format="%d.%m.%Y"))&!is.na(dataset_prescriptions$age_at_realization)),]

dataset_prescriptions_children=dataset_prescriptions[which(dataset_prescriptions$age_at_realization<=18),]
dataset_prescriptions_adults=dataset_prescriptions[which(dataset_prescriptions$age_at_realization>18),]

date_prescriptions=as.Date(dataset_prescriptions$realized_date,format="%d.%m.%Y")
age_prescriptions=as.numeric(unname(unlist(dataset_prescriptions$age_at_realization)))


analyze_continous(as.numeric(date_prescriptions),"датум на препишување рецепт")
ks.test(as.numeric(unique(date_prescriptions)),"punif",min(date_prescriptions),max(date_prescriptions))$p.value>0.01

analyze_continous(age_prescriptions,"возраст при препишување рецепт")

medication_prescriptions=dataset_prescriptions$atc_code
analyze_discrete(medication_prescriptions,"препишан лек")
analyze_discrete(dataset_prescriptions_children$atc_code,"препишан лек за деца")
analyze_discrete(dataset_prescriptions_adults$atc_code,"препишан лек за возрасни")

therapy_prescriptions=dataset_prescriptions$therapy_type_id
analyze_discrete(therapy_prescriptions,"тип на препорачана терапија")
analyze_discrete(dataset_prescriptions_children$therapy_type_id,"тип на препорачана терапија за деца")
analyze_discrete(dataset_prescriptions_adults$therapy_type_id,"тип на препорачана терапија за возрасни")

write.csv(dataset_prescriptions,"./data26/dataset_prescriptions_cleaned.csv",row.names = FALSE)