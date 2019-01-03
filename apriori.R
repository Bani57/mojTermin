generate_transactions<-function(dataset,path)
{
  transaction=c()
  transactionList=list()
  currID=-1
  j=1
  n=length(dataset[,1])
  for (i in c(1:n)) {
    red=dataset[i,]
    ID=as.numeric(unname(unlist(red[1])))
    diagnosis=unname(red[2])
    datum=red[3]
    if(currID!=ID)
    {
      transaction=c()
      currID=ID
    }
    if(!(diagnosis %in% transaction))
    {
      transaction=c(transaction,diagnosis)
      transactionList[[j]]=transaction
      j=j+1
    }
  }
  invisible(lapply(transactionList, function(x) write.table(unname(data.frame(x)), path,append=TRUE,sep=',')))
}

dataset_mapped_level0 <- read.csv("./data26/dataset_referrals_mapped_level0.csv", sep=";", stringsAsFactors=FALSE)
generate_transactions(dataset_mapped_level0,"./transactions/transactions_referrals_level0.csv")

dataset_mapped_level1 <- read.csv("./data26/dataset_referrals_mapped_level1.csv", sep=";", stringsAsFactors=FALSE)
generate_transactions(dataset_mapped_level1,"./transactions/transactions_referrals_level1.csv")

dataset_mapped_level2 <- read.csv("./data26/dataset_referrals_cleaned.csv", stringsAsFactors=FALSE)
generate_transactions(dataset_mapped_level2,"./transactions/transactions_referrals_level2.csv")