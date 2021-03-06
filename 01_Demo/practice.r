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

#----------------------------------------------------------------------------------------------------------------------------------------
#create summary table
mpg_summary <- mpg %>% group_by(manufacturer) %>% summarize(Vehicle_Count=n(), .groups = 'keep') 

#import dataset into ggplot2
plt <- ggplot(mpg_summary,aes(x=manufacturer,y=Vehicle_Count)) 

#plot a bar plot
plt + geom_col() 

#plot bar plot with labels
plt + geom_col() + xlab("Manufacturing Company") + ylab("Number of Vehicles in Dataset") 


#----------------------------------------------------------------------------------------------------------------------------------------
#plot a boxplot with labels
plt + geom_col() + xlab("Manufacturing Company") + ylab("Number of Vehicles in Dataset") + 
  theme(axis.text.x=element_text(angle=45,hjust=1)) #rotate the x-axis label 45 degrees

#----------------------------------------------------------------------------------------------------------------------------------------
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


#----------------------------------------------------------------------------------------------------------------------------------------
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


#----------------------------------------------------------------------------------------------------------------------------------------
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

#----------------------------------------------------------------------------------------------------------------------------------------
#create summary table
mpg_summary <- mpg %>% group_by(class,year) %>% summarize(Mean_Hwy=mean(hwy), .groups = 'keep') 

plt <- ggplot(mpg_summary, aes(x=class,y=factor(year),fill=Mean_Hwy))

#create heatmap with labels
plt + geom_tile() + labs(x="Vehicle Class",y="Vehicle Year",fill="Mean Highway (MPG)") 


#----------------------------------------------------------------------------------------------------------------------------------------
#create summary table
mpg_summary <- mpg %>% group_by(model,year) %>% summarize(Mean_Hwy=mean(hwy), .groups = 'keep') 
#import dataset into ggplot2
plt <- ggplot(mpg_summary, aes(x=model,y=factor(year),fill=Mean_Hwy)) 

#add heatmap with labels
plt + geom_tile() + labs(x="Model",y="Vehicle Year",fill="Mean Highway (MPG)") + 
  #rotate x-axis labels 90 degrees
  theme(axis.text.x = element_text(angle=90,hjust=1,vjust=.5)) 

#----------------------------------------------------------------------------------------------------------------------------------------  
# 15.3.7 Add Layers to Plots
# two types of plot layers: those that use same variables / input data as original, and those that use different but complementary data to original

#import dataset into ggplot2
plt <- ggplot(mpg,aes(x=manufacturer,y=hwy))

#add boxplot
plt + geom_boxplot() + 

  #rotate x-axis labels 45 degrees
  theme(axis.text.x=element_text(angle=45,hjust=1)) + 
  #overlay scatter plot on top
  geom_point() 


# mapping argument functions exactly like ggplot() function
#create summary table
mpg_summary <- mpg %>% group_by(class) %>% summarize(Mean_Engine=mean(displ), .groups = 'keep') 

#import dataset into ggplot2
plt <- ggplot(mpg_summary,aes(x=class,y=Mean_Engine)) 

#add scatter plot
plt + geom_point(size=4) + labs(x="Vehicle Class",y="Mean Engine Size") 


mpg_summary <- mpg %>% group_by(class) %>% summarize(Mean_Engine=mean(displ),SD_Engine=sd(displ), .groups = 'keep')

#import dataset into ggplot2
plt <- ggplot(mpg_summary,aes(x=class,y=Mean_Engine)) 

#add scatter plot with labels
plt + geom_point(size=4) + labs(x="Vehicle Class",y="Mean Engine Size") + 
  
  #overlay with error bars
  geom_errorbar(aes(ymin=Mean_Engine-SD_Engine,ymax=Mean_Engine+SD_Engine)) 

#This process of separating out plots for each level is known as faceting in ggplot2.
#Faceting is performed by adding a facet() function to the end of our plotting statement. 

#convert to long format
mpg_long <- mpg %>% gather(key="MPG_Type",value="Rating",c(cty,hwy)) 
head(mpg_long)

#import dataset into ggplot2
plt <- ggplot(mpg_long,aes(x=manufacturer,y=Rating,color=MPG_Type)) 
#add boxplot with labels rotated 45 degrees
plt + geom_boxplot() + theme(axis.text.x=element_text(angle=45,hjust=1)) 


plt <- ggplot(mpg_long,aes(x=manufacturer,y=Rating,color=MPG_Type)) #import dataset into ggplot2
plt + geom_boxplot() + facet_wrap(vars(MPG_Type)) + #create multiple boxplots, one for each MPG type
theme(axis.text.x=element_text(angle=45,hjust=1),legend.position = "none") + xlab("Manufacturer") #rotate x-axis labels

#---------------------------------------------------------------------------------------------------------------------------------------
# 15.6.1 Samples vs Pop

# use 'used vehicle dataset'
population_table <- read.csv('used_car_data.csv',check.names = F,stringsAsFactors = F) #import used car dataset
plt <- ggplot(population_table,aes(x=log10(Miles_Driven))) #import dataset into ggplot2
plt + geom_density() #visualize distribution using density plot

sample_table <- population_table %>% sample_n(50) #randomly sample 50 data points
plt <- ggplot(sample_table,aes(x=log10(Miles_Driven))) #import dataset into ggplot2
plt + geom_density() #visualize distribution using density plot

#---------------------------------------------------------------------------------------------------------------------------------------
# 15.6.2 Use the One-Sample t-Test

#compare sample versus population means
t.test(log10(sample_table$Miles_Driven),mu=mean(log10(population_table$Miles_Driven))) 


#---------------------------------------------------------------------------------------------------------------------------------------
# 15.6.3 Use the Two-Sample t-Test

# first generate samples
sample_table <-population_table %>%sample_n(50)#generate 50 randomly sampled data points
sample_table2 <-population_table %>%sample_n(50)#generate another 50 randomly sampled data points

# now run test
t.test(log10(sample_table$Miles_Driven),log10(sample_table2$Miles_Driven)) #compare means of two samples

#---------------------------------------------------------------------------------------------------------------------------------------
# 15.6.4 Use the Two-Sample t-Test to Compare Samples

# test two samples using pair t-test.

#import dataset
mpg_data <- read.csv('mpg_modified.csv')
#select only data points where the year is 1999
mpg_1999 <- mpg_data %>% filter(year==1999)
#select only data points where the year is 2008
mpg_2008 <- mpg_data %>% filter(year==2008) 

#compare the mean difference between two samples
t.test(mpg_1999$hwy,mpg_2008$hwy,paired = T) 

#---------------------------------------------------------------------------------------------------------------------------------------
# 15.6.5 Use the ANOVA Test

#filter columns from mtcars dataset
mtcars_filt <- mtcars[,c("hp","cyl")] 
#convert numeric column to factor
mtcars_filt$cyl <- factor(mtcars_filt$cyl) 

#compare means across multiple levels
aov(hp ~ cyl,data=mtcars_filt) 

#initial output of our aov() function does not contain our p-values. 
#To retrieve our p-values, we have to wrap our aov()function in a summary() function
summary(aov(hp ~ cyl,data=mtcars_filt))

#---------------------------------------------------------------------------------------------------------------------------------------
# 15.7.1 The Correlation Conundrum

#import dataset into ggplot2
plt <- ggplot(mtcars,aes(x=hp,y=qsec)) 

#create scatter plot
plt + geom_point() 

#calculate correlation coefficient
cor(mtcars$hp,mtcars$qsec) 

#read in dataset
used_cars <- read.csv('used_car_data.csv',stringsAsFactors = F) 
head(used_cars)

#import dataset into ggplot2
plt <- ggplot(used_cars,aes(x=Miles_Driven,y=Selling_Price)) 
#create a scatter plot
plt + geom_point() 

#calculate correlation coefficient
cor(used_cars$Miles_Driven,used_cars$Selling_Price) 

used_matrix <- as.matrix(used_cars[,c("Selling_Price","Present_Price","Miles_Driven")]) #convert data frame into numeric matrix
cor(used_matrix)

#---------------------------------------------------------------------------------------------------------------------------------------
# 15.7.2 Return to Linear Regression

#create linear model
lm(qsec ~ hp,mtcars) 

#summarize linear model
summary(lm(qsec~hp,mtcars)) 

#create linear model
model <- lm(qsec ~ hp,mtcars) 
yvals <- model$coefficients['hp']*mtcars$hp +
  model$coefficients['(Intercept)'] #determine y-axis values from linear model

#import dataset into ggplot2
plt <- ggplot(mtcars,aes(x=hp,y=qsec)) 
#plot scatter and linear model
plt + geom_point() + geom_line(aes(y=yvals), color = "red") 

#---------------------------------------------------------------------------------------------------------------------------------------
#15.7.3 Perform Multiple Linear Regression

#generate multiple linear regression model
lm(qsec ~ mpg + disp + drat + wt + hp,data=mtcars) 

#generate summary statistics
summary(lm(qsec ~ mpg + disp + drat + wt + hp,data=mtcars)) 

#---------------------------------------------------------------------------------------------------------------------------------------
#15.8.1 Category Complexities
table(mpg$class,mpg$year) #generate contingency table

tbl <- table(mpg$class,mpg$year) #generate contingency table
chisq.test(tbl) #compare categorical distributions
