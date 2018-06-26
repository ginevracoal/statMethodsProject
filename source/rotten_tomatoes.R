setwd("/home/ginevracoal/MEGA/Università/DSSC/semester_2/statistical_methods_for_data_science/statMethodsProject/source")
source("plot.R")
source("dataset_import.R")
library(ReadMe)
library(dplyr)
library(tidyverse)
set.seed(123)

# get labeled dataset
str(train)

# get only whole sentences
data <- train %>%
  group_by(SentenceId) %>% 
  filter(PhraseId == min(PhraseId)) %>% 
  select(SentenceId, Phrase, Sentiment)
head(data)

# number of sentences
nrow(data)
colnames(data)


# =============================================
# prepare for ReadMe

n <- nrow(data)

# create text files
unlink("../input_readme/*")
apply(data[1:n,], 1, function(x) write.table(data.frame(x[2]),  
                                             file = gsub(" ","",paste("../input_readme/",x[1],".txt",sep="")), row.names = FALSE, col.names = FALSE))

# readapt csv file
data1 <- data[1:n,] %>% 
  mutate(filename = paste(SentenceId,".txt", sep="")) %>% 
  rename(truth = Sentiment) %>% 
  add_column(trainingset = rbinom(n, 1, 0.5)) %>% 
  ungroup() %>% 
  select(filename, truth, trainingset)

# write.csv(data1, file = "../input_readme/control.csv", row.names = FALSE, quote=FALSE)
write.table(data1, file = "../input_readme/control.txt", sep = ',',row.names = FALSE, quote=FALSE)

unlink("results/rotten_control.rds")
saveRDS(data1, "results/rotten_control.rds")
# ======================================================
# ReadMe
oldwd <- getwd()
setwd("../input_readme/")
list.files()

# undergrad
output <- undergrad(sep = ',')

# remove columns with variance 0
preprocess <- preprocess(output)

# readme
results <- readme(undergradlist=preprocess)
str(results)
setwd(oldwd)

# ===========================
# https://www.svm-tutorial.com/2014/11/svm-classify-text-r/
library(RTextTools)
data2 <- data %>% 
  ungroup() %>% 
  select(Phrase, Sentiment)

# train-test split
smp_size <- floor(0.6 * nrow(data2))
train_ind <- sample(seq_len(nrow(data2)), size = smp_size)
train <- data2[train_ind, ]
validation <- data2[-train_ind, ]
test <- data2[-train_ind, ] %>% select(Phrase)

# Create the document term matrix
dtMatrix <- create_matrix(train$Phrase)

# Configure the training data
container <- create_container(dtMatrix, train$Sentiment, trainSize=1:nrow(train), virgin=FALSE)

# train a SVM Model
linear_svm <- train_model(container, "SVM", kernel="linear", cost=1)
radial_svm <- train_model(container, "SVM", kernel="radial", cost=1)


# trace("create_matrix", edit=T)
# https://github.com/timjurka/RTextTools/issues/4

# test set document term matrix
predMatrix <- create_matrix(test$Phrase, originalMatrix=dtMatrix)

# create the corresponding container
predictionContainer <- create_container(predMatrix, labels=rep(0,nrow(test)), testSize=1:nrow(test), virgin=FALSE)

# predict
linear_predictions <- classify_model(predictionContainer, linear_svm)
radial_predictions <- classify_model(predictionContainer, radial_svm)

library(caret)
confusionMatrix(table(linear_predictions$SVM_LABEL, validation$Sentiment, dnn=c("Prediction", "Actual")))

# compute proportions

# true_proportions <- validation %>% 
#   group_by(Sentiment) %>% 
#   summarise(prop = n()/nrow(validation))

svm_proportions <- function(predictions){
  df <- predictions %>% 
    group_by(SVM_LABEL) %>% 
    summarise(prop = n()/nrow(predictions))
  return(df$prop)
}

# compute mean absolute error

library(ModelMetrics)

linear_mae <- mae(true_proportions$prop, svm_proportions(linear_predictions))
radial_mae <- mae(true_proportions$prop, svm_proportions(radial_predictions))
readme_mae <- mae(results$true.CSMF, results$est.CSMF)

unlink("results/mae_table.rds")
mae_table <- data.frame(linear = linear_mae, radial=  radial_mae, readme = readme_mae, row.names = "mean_absolute_error")
saveRDS(mae_table, "results/mae_table.rds")


# ===========================================

rotten <- data.frame(true = results$true.CSMF, 
                       readme_est = results$est.CSMF,
                       linear_est = svm_proportions(linear_predictions),
                       radial_est = svm_proportions(radial_predictions)) %>% 
            add_column(rating = as.factor(seq(1,5,1))) %>% 
  gather(key = type, value = est, -c(rating, true))

unlink("results/rotten.rds")
saveRDS(rotten, "results/rotten.rds")



# library(RColorBrewer)
# ggplot(all_data, aes(true, est, color = rating, shape = type)) +
#   geom_point(size = 2) +
#   xlab(expression(P(D))) +
#   ylab(paste("Estimated ", expression(P(D)))) +
#   geom_abline(slope = 1, intercept = 0) +
#   xlim(c(0,0.6)) + ylim(c(0,0.6)) +
#   scale_color_brewer(palette="Set2")+
#   scale_shape_manual(values = c(0, 2, 16))


  