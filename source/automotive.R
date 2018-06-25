# Time analysis of amazon reviews
set.seed(123)

setwd("/home/ginevracoal/MEGA/Università/DSSC/semester_2/statistical_methods_for_data_science/statMethodsProject/source")

# import json
source("json.R")
df <- json_to_df("../dataset/Automotive_5.json")

# subset data
n <- 500
data <- df[1:n,]

head(df)


library(scales)

df %>% mutate(month = format(date, "%m"), year = format(date, "%Y")) %>%
  group_by(month, year) %>%
  summarise(total = sum(value))


df %>% filter(overall == 1) %>% 
  group_by()
  ggplot(aes(x=reviewTime, y=overall, color= overall, group = overall)) + 
  geom_point() + geom_line() + 
  xlab("review time")
  # scale_x_date(breaks="1 month") +
  labs(title="Distribution of ratings over time for each category") +
  theme_bw()

ggplot(df,aes(x=diff,group=type,fill=type))+
  geom_histogram(position="dodge",binwidth=0.25)+theme_bw()

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


