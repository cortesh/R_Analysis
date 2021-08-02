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

# load demo2 data
demo_table3 <- read.csv('demo2.csv',check.names = F,stringsAsFactors = F)


# pivot data set using gather()
long_table <- gather(demo_table3,key="Metric",value="Score",buying_price:popularity)

# another syntax to do the same thing
long_table <- demo_table3 %>% gather(key="Metric",value="Score",buying_price:popularity)

# practice using spread() to reverse what gather() did
wide_table <- long_table %>% spread(key="Metric",value="Score")

# check to see if tables are equal
all.equal(demo_table3,wide_table)

# to make sure you are comparing apples to apples, use colnames
table <- demo_table3[,(colnames(wide_table))]


all.equal(demo_table3,colnames(wide_table))

plt <- ggplot(mpg,aes(x=class)) #import dataset into ggplot2
plt + geom_bar() #plot a bar plot


mpg_summary <- mpg %>% group_by(manufacturer) %>% summarize(Vehicle_Count=n(), .groups = 'keep') #create summary table
plt <- ggplot(mpg_summary,aes(x=manufacturer,y=Vehicle_Count)) #import dataset into ggplot2
plt + geom_col() #plot a bar plot

#plot bar plot with labels
plt + geom_col() + xlab("Manufacturing Company") + ylab("Number of Vehicles in Dataset") 

#plot a boxplot with labels
plt + geom_col() + xlab("Manufacturing Company") + ylab("Number of Vehicles in Dataset") + 
  theme(axis.text.x=element_text(angle=45,hjust=1)) #rotate the x-axis label 45 degrees

#create summary table
mpg_summary <- subset(mpg,manufacturer=="toyota") %>% group_by(cyl) %>% summarize(Mean_Hwy=mean(hwy), .groups = 'keep') 

#import dataset into ggplot2
plt <- ggplot(mpg_summary,aes(x=cyl,y=Mean_Hwy)) 

#generate line chart
plt + geom_line()

#add line plot with labels
plt + geom_line() + scale_x_discrete(limits=c(4,6,8)) + scale_y_continuous(breaks = c(15:30)) 

#import dataset into ggplot2
plt <- ggplot(mpg,aes(x=displ,y=cty)) 

#add scatter plot with labels
plt + geom_point() + xlab("Engine Size (L)") + ylab("City Fuel-Efficiency (MPG)") 

#import dataset into ggplot2
plt <- ggplot(mpg,aes(x=displ,y=cty,color=class)) 
#add scatter plot with labels
plt + geom_point() + labs(x="Engine Size (L)", y="City Fuel-Efficiency (MPG)", color="Vehicle Class") 

#import dataset into ggplot2
plt <- ggplot(mpg,aes(x=displ,y=cty,color=class,shape=drv)) 

#add scatter plot with multiple aesthetics
plt + geom_point() + 
  labs(x="Engine Size (L)", y="City Fuel-Efficiency (MPG)", color="Vehicle Class",shape="Type of Drive") 

#import dataset into ggplot2
plt <- ggplot(mpg,aes(y=hwy)) 
#add boxplot
plt + geom_boxplot() 

#import dataset into ggplot2
plt <- ggplot(mpg,aes(x=manufacturer,y=hwy)) 
#add boxplot and rotate x-axis labels 45 degrees
plt + geom_boxplot() + theme(axis.text.x=element_text(angle=45,hjust=1)) 

#------------------------------------------------------------------------------------------------------------------
#statistics
#visualize distribution using density plot
ggplot(mtcars,aes(x=wt)) + geom_density() 

shapiro.test(mtcars$wt)
