samplesubmission <- read.csv("../dataset/sampleSubmission.csv")
str(samplesubmission)

library(data.table)

train <- as.data.frame(fread("../dataset/train.tsv"))
test <- as.data.frame(fread("../dataset/test.tsv"))

str(train)
str(test)
