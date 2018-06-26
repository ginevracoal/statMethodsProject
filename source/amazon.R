# Time analysis of amazon reviews
set.seed(123)
setwd("/home/ginevracoal/MEGA/Università/DSSC/semester_2/statistical_methods_for_data_science/statMethodsProject/source")

# import json
source("json.R")
# automotive <- json_to_df("../dataset/Automotive_5.json")
cds <- json_to_df("../dataset/CDs_and_Vinyl_5.json")
instruments <- json_to_df("../dataset/Musical_Instruments_5.json")
cellphones <- json_to_df("../dataset/Cell_Phones_and_Accessories_5.json")
electronics <- json_to_df("../dataset/Electronics_5.json")

#=================================================
# plotting the distribution of ratings over time

# unlink("plots/amazon_prop.jpg"); jpeg(filename="plots/amazon_prop.jpg")
# par(mfrow=c(2,2))

unlink("plots/amazon_cds.png"); png(filename="plots/amazon_cds.png");
cds %>% mutate(year = format(reviewTime, "%Y")) %>%
  filter(year > "2004", year < "2014") %>%
  mutate(rating = factor(overall)) %>% 
  group_by(year, rating) %>%
  summarise(n=n()) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = year, y = prop, group = factor(rating), color = rating)) + 
  geom_point() + geom_line() +
  scale_color_brewer(palette="Set2")+
  xlab("review time") + ylab("proportion of reviews")+
  labs(title="CDs and Vinyls ratings")
dev.off()

unlink("plots/amazon_instruments.png"); png(filename="plots/amazon_instruments.png")
instruments %>% mutate(year = format(reviewTime, "%Y")) %>%
  filter(year > "2004", year < "2014") %>%
  mutate(rating = factor(overall)) %>% 
  group_by(year, rating) %>%
  summarise(n=n()) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = year, y = prop, group = factor(rating), color = rating)) + 
  geom_point() + geom_line() +
  scale_color_brewer(palette="Set2")+
  xlab("review time") + ylab("proportion of reviews")+
  labs(title="Musical instruments ratings")
dev.off()

unlink("plots/amazon_cellphones.png"); png(filename="plots/amazon_cellphones.png")
cellphones %>% mutate(year = format(reviewTime, "%Y")) %>%
  filter(year > "2004", year < "2014") %>%
  mutate(rating = factor(overall)) %>% 
  group_by(year, rating) %>%
  summarise(n=n()) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = year, y = prop, group = factor(rating), color = rating)) + 
  geom_point() + geom_line() +
  scale_color_brewer(palette="Set2")+
  xlab("review time") + ylab("proportion of reviews")+
  labs(title="Cellphones ratings")
dev.off()

unlink("plots/amazon_electronics.png"); png(filename="plots/amazon_electronics.png")
electronics %>% 
  mutate(year = format(reviewTime, "%Y")) %>%
  filter(year > "2004", year < "2014") %>%
  mutate(rating = factor(overall)) %>% 
  group_by(year, rating) %>%
  summarise(n=n()) %>% 
  mutate(prop = n/sum(n)) %>% 
  ggplot(aes(x = year, y = prop, group = factor(rating), color = rating)) + 
  geom_point() + geom_line() + 
  scale_color_brewer(palette="Set2")+
  xlab("review time") + ylab("proportion of reviews")+
  labs(title="Electronics ratings")
dev.off()

# par(mfrow=c(1,1))
# =================================================

# subset data
n <- 500
data <- df[1:n,]

head(df)

library(scales)

# use readme
library(ReadMe)

readme_inputs(data)

oldwd <- getwd()
setwd("../input_readme/")
list.files()

#step undergrad preprocessing
output <- undergrad(sep = ',') 

str(output$trainingset) 

# remove columns with variance 0 in the trainset
preprocess <- preprocess(output)

str(preprocess$trainingset) 
# removes all words with 0 variance, meaning words (almost) not 
# comparing in any document

# compute the proportion of text documents within each of 
# the user-specified categories
results <- readme(undergradlist=preprocess)
str(results)

# restore old working directory
setwd(oldwd)

# ======================================================
# plot results
source("plot.R")
basic_plot(results) +
  ggtitle("Difference in review frequencies")


