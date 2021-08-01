library(jsonlite)
library(tidyverse)

demo_table <- read.csv(file='demo.csv',check.names=F,stringsAsFactors = F)
demo_table2 <- fromJSON(txt='demo.json')
filter_table3 <- demo_table2[("clean" %in% demo_table2$title_status) & (demo_table2$price > 10000) & (demo_table2$drive == "4wd"),]
sample(c("cow", "deer", "pig", "chicken", "duck", "sheep", "dog"), 4)

# sample data in 3 steps
# step 1: get number of vector in variable num_rows
num_rows <- 1:nrow(demo_table)

# step 2: extract subset using sample() function into variable sample_rows
sample_rows <- sample(num_rows, 3)

# step 3: load dataframe of the subset using bracket method
demo_table[sample_rows,]

# To complete 3 steps in 1 do following:
demo_table[sample(1:nrow(demo_table), 3),]  

#add columns to original data frame
demo_table <- demo_table %>% mutate(Mileage_per_Year=Total_Miles/(2020-Year),IsActive=TRUE) 

#create summary table
summarize_demo <- demo_table2 %>% 
  group_by(condition) %>% 
  summarize(Mean_Mileage=mean(odometer), .groups = 'keep') 


#create summary table with multiple columns
summarize_demo <- demo_table2 %>% 
  group_by(condition) %>% 
  summarize(
    Mean_Mileage=mean(odometer),
    Maximum_Price=max(price),
    Num_Vehicles=n(), .groups = 'keep') 
