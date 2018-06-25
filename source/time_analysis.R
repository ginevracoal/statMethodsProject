# Time analysis of amazon reviews
set.seed(123)

setwd("/home/ginevracoal/MEGA/Università/DSSC/semester_2/statistical_methods_for_data_science/statMethodsProject/source")

# import json
source("json.R")
automotive <- json_to_df("../dataset/Automotive_5.json")
cds <- json_to_df("../dataset/CDs_and_Vinyl_5.json")
instruments <- json_to_df("../dataset/Musical_Instruments_5.json")
cellphones <- json_to_df("../dataset/Cell_Phones_and_Accessories_5.json")
electronics <- json_to_df("../dataset/Electronics_5.json")

# subset data
n <- 500
data <- df[1:n,]

head(df)

library(scales)

#=================================================
# plotting the distribution of ratings over time

cds %>% mutate(year = format(reviewTime, "%Y")) %>%
  mutate(rating = factor(overall)) %>%
  group_by(year, rating) %>%
  summarise(total = n()) %>%
  ggplot(aes(x = year, y = total, group = factor(rating), color = rating)) +
  geom_point() + geom_line() +
  scale_color_brewer(palette="Set2")+
  xlab("review time") + ylab("number of reviews")+
  scale_color_brewer(palette="Set2")+
  labs(title="Distribution of CDs and Vinyls ratings")+
  scale_fill_discrete(name = "Ratings")

instruments %>% mutate(year = format(reviewTime, "%Y")) %>%
  filter(year > "2006", year < "2014") %>%
  mutate(rating = factor(overall)) %>% 
  group_by(year, rating) %>%
  summarise(total = n()) %>% 
  ggplot(aes(x = year, y = log(total), group = factor(rating), color = rating)) + 
  geom_point() + geom_line() +
  scale_color_brewer(palette="Set2")+
  xlab("review time") + ylab("number of reviews")+
  labs(title="Distribution of musical instruments ratings")

cellphones %>% 
  mutate(year = format(reviewTime, "%Y"), month = format(reviewTime, "%m")) %>%
  filter(year > "2003", year < "2014") %>%
  mutate(rating = factor(overall)) %>% 
  group_by(year, rating) %>%
  summarise(total = n()) %>% 
  ggplot(aes(x = year, y = log(total), group = factor(rating), color = rating)) + 
  geom_point() + geom_line() + 
  scale_color_brewer(palette="Set2")+
  xlab("review time") + ylab("number of reviews")+
  labs(title="Distribution of cell phones ratings")

electronics %>% 
  mutate(year = format(reviewTime, "%Y"), month = format(reviewTime, "%m")) %>%
  filter(year > "2001", year < "2014") %>%
  mutate(rating = factor(overall)) %>% 
  group_by(year, rating) %>%
  summarise(total = n()) %>% 
  ggplot(aes(x = year, y = log(total), group = factor(rating), color = rating)) + 
  geom_point() + geom_line() + 
  scale_color_brewer(palette="Set2")+
  xlab("review time") + ylab("number of reviews")+
  labs(title="Distribution of electronics ratings")

# =================================================
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


