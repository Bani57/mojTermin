t.test.ednakvost<-function(X,Y,isti=TRUE,tip="dvostran",alfa)
{
  n=length(X)
  m=length(Y)
  if(n>=30&m>=30) print('Дисперзиите не се познати но примероците се доволно големи, не мора да се користи t-тест.')
  Sx=sd(X)
  Sy=sd(Y)
  Sp=sqrt(((n-1)*Sx^2+(m-1)*Sy^2)/(n+m-2))
  if(isti==FALSE) T=(mean(X)-mean(Y))/sqrt((Sx^2)/n+(Sy^2)/m)
  else T=(mean(X)-mean(Y))/(Sp*sqrt(1/n+1/m))
  print(paste('Тест статистиката: ',T))
  if(tip=="dvostran")
  {
    talfa=qt(1-alfa/2,n+m-2)
    C=c(-talfa,talfa)
    print(paste('Критичниот домен: (',-Inf,',',C[1],')U(',C[2],',',Inf,')'))
    if(T<C[1]|T>C[2]) print(paste('Тест статистиката припаѓа во критичниот домен, се отфрла нултата хипотеза! Очекувањата не се еднакви!'))
    else print(paste('Тест статистиката не припаѓа во критичниот домен, се прифаќа нултата хипотеза! Очекувањата се еднакви!'))
  }
  else if(tip=="ednostran.pomalo")
  {
    talfa=qt(1-alfa,n+m-2)
    C=-talfa
    print(paste('Критичниот домен: (',-Inf,',',C,')'))
    if(T<C) print(paste('Тест статистиката припаѓа во критичниот домен, се отфрла нултата хипотеза! Математичкото очекување на првата променлива е помало од очекувањето на втората променлива!'))
    else print(paste('Тест статистиката не припаѓа во критичниот домен, се прифаќа нултата хипотеза! Очекувањата се еднакви!'))
  }
  else if(tip=="ednostran.pogolemo")
  {
    talfa=qt(1-alfa,n+m-2)
    C=talfa
    print(paste('Критичниот домен: (',C,',',Inf,')'))
    if(T>C) print(paste('Тест статистиката припаѓа во критичниот домен, се отфрла нултата хипотеза! Математичкото очекување на првата променлива е поголемо од очекувањето на втората променлива!'))
    else print(paste('Тест статистиката не припаѓа во критичниот домен, се прифаќа нултата хипотеза! Очекувањата се еднакви!'))
  }
  else print('Невалиден тип на тест!')
}

f.test<-function(X,Y,tip="dvostran",alfa)
{
  n=length(X)
  m=length(Y)
  Sx=sd(X)
  Sy=sd(Y)
  F=(Sx^2)/(Sy^2)
  print(paste('Тест статистиката: ',F))
  if(tip=="dvostran")
  {
    falfa1=qf(1-alfa/2,n-1,m-1)
    falfa2=qf(alfa/2,n-1,m-1)
    C=c(falfa2,falfa1)
    print(paste('Критичниот домен: (',-Inf,',',C[1],')U(',C[2],',',Inf,')'))
    if(F<C[1]|F>C[2]) print(paste('Тест статистиката припаѓа во критичниот домен, се отфрла нултата хипотеза! Дисперзиите не се еднакви!'))
    else print(paste('Тест статистиката не припаѓа во критичниот домен, се прифаќа нултата хипотеза! Дисперзиите се еднакви!'))
  }
  else if(tip=="ednostran.pomalo")
  {
    falfa=qf(alfa,n-1,m-1)
    C=falfa
    print(paste('Критичниот домен: (',-Inf,',',C,')'))
    if(F<C) print(paste('Тест статистиката припаѓа во критичниот домен, се отфрла нултата хипотеза! Дисперзијата на првата променлива е помала од дисперзијата на втората променлива!'))
    else print(paste('Тест статистиката не припаѓа во критичниот домен, се прифаќа нултата хипотеза! Дисперзиите се еднакви!'))
  }
  else if(tip=="ednostran.pogolemo")
  {
    falfa=qf(1-alfa,n-1,m-1)
    C=falfa
    print(paste('Критичниот домен: (',C,',',Inf,')'))
    if(F>C) print(paste('Тест статистиката припаѓа во критичниот домен, се отфрла нултата хипотеза! Дисперзијата на првата променлива е поголема од дисперзијата на втората променлива!'))
    else print(paste('Тест статистиката не припаѓа во критичниот домен, се прифаќа нултата хипотеза! Дисперзиите се еднакви!'))
  }
  else print('Невалиден тип на тест!')
}

library(readxl)

results_neprekinati <- read_excel("results/results.xlsx", sheet = "Непрекинати", range = "C2:F13")
knn=results_neprekinati$`К најблиски соседи`
nn=results_neprekinati$`Невронска мрежа`
f.test(knn,nn,alfa = 0.01)
t.test.ednakvost(knn,nn,isti = TRUE,alfa = 0.001)
boxplot(results_neprekinati,main=paste('Boxplot за множествата со непрекинати атрибути'),names=colnames(results_neprekinati),varwidth=TRUE,col=3)


results_diskretni <- read_excel("results/results.xlsx", sheet = "Дискретни", range = "C2:F13")
rf=results_diskretni$`Random Forest`
cn2=results_diskretni$`CN2 правила`
f.test(rf,cn2,alfa = 0.01)
t.test.ednakvost(rf,cn2,isti = TRUE,alfa = 0.001)
boxplot(results_diskretni,main=paste('Boxplot за множествата со дискретни атрибути'),names=colnames(results_diskretni),varwidth=TRUE,col=3)