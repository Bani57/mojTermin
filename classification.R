generate_dataset_for_diagnosis<-function(dataset,path,path2,diagnosis)
{
  all_diagnoses=sort(unique(dataset[,2]))
  all_diagnoses=all_diagnoses[which(all_diagnoses!=diagnosis)]
  D=length(all_diagnoses)
  d_vector=rep(0,D)
  names(d_vector)=all_diagnoses
  currID=-1
  stop_counting=FALSE
  n=length(dataset[,1])
  result_data=matrix(0,nrow=length(unique(dataset[,1])),ncol=D+2)
  j=0
  for (i in c(1:n)) {
    red=dataset[i,]
    ID=as.numeric(unname(unlist(red[1])))
    currDiag=unname(unlist(red[2]))
    age=as.numeric(unname(unlist(red[4])))
    if(currID!=ID)
    {
      j=j+1
      d_vector=rep(0,D)
      names(d_vector)=all_diagnoses
      result_data[j,D+1]=age
      currID=ID
      stop_counting=FALSE
    }
    if(currDiag!=diagnosis&&!stop_counting)
    {
      d_vector[currDiag]=d_vector[currDiag]+1
      result_data[j,1:D]=d_vector
    }
    else if(!stop_counting)
    {
      result_data[j,D+1]=age
      result_data[j,D+2]=1
      stop_counting=TRUE
    }
  }
  write.table(unname(data.frame(result_data)),path,sep=',')
  dataset_result <- read.csv(path, header=FALSE, stringsAsFactors=FALSE)
  dataset_result=dataset_result[,-1]
  colnames(dataset_result)=c(all_diagnoses,"age","class")
  write.csv(dataset_result,path,row.names = FALSE)
  dataset_discrete=dataset_result
  x=dataset_result[,-c(D+1,D+2)]
  x[x>1]=1
  dataset_discrete[,-c(D+1,D+2)]=x
  x=as.factor(dataset_discrete[,"age"])
  levels(x)[as.numeric(levels(x))<=2]="Infant/Toddler"
  levels(x)[as.numeric(levels(x))>=3&as.numeric(levels(x))<=5]="Preschool child"
  levels(x)[as.numeric(levels(x))>=6&as.numeric(levels(x))<=12]="School-aged child"
  levels(x)[as.numeric(levels(x))>=13&as.numeric(levels(x))<=18]="Adolescent"
  levels(x)[as.numeric(levels(x))>18&as.numeric(levels(x))<=40]="Young adult"
  levels(x)[as.numeric(levels(x))>40&as.numeric(levels(x))<=65]="Middle-aged adult"
  levels(x)[as.numeric(levels(x))>65]="Elder"
  dataset_discrete[,"age"]=x
  write.csv(dataset_discrete,path2,row.names = FALSE)
}

dataset_referrals_cleaned <- read.csv("./data26/dataset_referrals_cleaned.csv", stringsAsFactors=FALSE)
generate_dataset_for_diagnosis(dataset_referrals_cleaned,"./classification/dataset_referrals_I10.csv","./classification/dataset_referrals_I10_discrete.csv","I10")

dataset_referrals_mapped_level1 <- read.csv("./data26/dataset_referrals_mapped_level1.csv", encoding="UTF-8", sep=";", stringsAsFactors=FALSE)
generate_dataset_for_diagnosis(dataset_referrals_mapped_level1,"./classification/dataset_referrals_B00-B09.csv","./classification/dataset_referrals_B00-B09_discrete.csv","B00-B09")
generate_dataset_for_diagnosis(dataset_referrals_mapped_level1,"./classification/dataset_referrals_C15-C26.csv","./classification/dataset_referrals_C15-C26_discrete.csv","C15-C26")
generate_dataset_for_diagnosis(dataset_referrals_mapped_level1,"./classification/dataset_referrals_E08-E13.csv","./classification/dataset_referrals_E08-E13_discrete.csv","E08-E13")
generate_dataset_for_diagnosis(dataset_referrals_mapped_level1,"./classification/dataset_referrals_I10-I16.csv","./classification/dataset_referrals_I10-I16_discrete.csv","I10-I16")
generate_dataset_for_diagnosis(dataset_referrals_mapped_level1,"./classification/dataset_referrals_K70-K77.csv","./classification/dataset_referrals_K70-K77_discrete.csv","K70-K77")


dataset_referrals_mapped_level0 <- read.csv("./data26/dataset_referrals_mapped_level0.csv", encoding="UTF-8", sep=";", stringsAsFactors=FALSE)
generate_dataset_for_diagnosis(dataset_referrals_mapped_level0,"./classification/dataset_referrals_A00-B99.csv","./classification/dataset_referrals_A00-B99_discrete.csv","A00-B99")
generate_dataset_for_diagnosis(dataset_referrals_mapped_level0,"./classification/dataset_referrals_I00-I99.csv","./classification/dataset_referrals_I00-I99_discrete.csv","I00-I99")
generate_dataset_for_diagnosis(dataset_referrals_mapped_level0,"./classification/dataset_referrals_E00-E89.csv","./classification/dataset_referrals_E00-E89_discrete.csv","E00-E89")
generate_dataset_for_diagnosis(dataset_referrals_mapped_level0,"./classification/dataset_referrals_G00-G99.csv","./classification/dataset_referrals_G00-G99_discrete.csv","G00-G99")
generate_dataset_for_diagnosis(dataset_referrals_mapped_level0,"./classification/dataset_referrals_K00-K95.csv","./classification/dataset_referrals_K00-K95_discrete.csv","K00-K95")