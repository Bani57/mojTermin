icd10 <- read.csv("./icd10.csv", header=FALSE, sep=";", stringsAsFactors=FALSE)
colnames(icd10)=c("Level 0","Level 1","Level 2 Code and Name","Level 2","Level 2 Name")
dataset_referrals <- read.csv("./data26/dataset_referrals_cleaned.csv", stringsAsFactors=FALSE)
icd10$`Level 2`=gsub(" ","",icd10$`Level 2`)
joined=merge(x=dataset_referrals, y=icd10, by.x="icd10_code", by.y = "Level 2", all.x=TRUE)
diagnosis_level0=joined[,c("patient_id","Level 0","realized_date","age_at_realization")]
write.csv(diagnosis_level0,"./data26/dataset_referrals_mapped_level0.csv",row.names = FALSE)
diagnosis_level1=joined[,c("patient_id","Level 1","realized_date","age_at_realization")]
write.csv(diagnosis_level1,"./data26/dataset_referrals_mapped_level1.csv",row.names = FALSE)