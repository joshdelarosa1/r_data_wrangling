# set working directory
setwd('/Users/josuedelarosa/dev/cheat_sheet')

## view working directory
getwd()

# create practice dataset
## package for with practice datasets
## install.packages("datasets")
### learn more about the iris dataset here -> https://en.wikipedia.org/wiki/Iris_flower_data_set
library("datasets")
data(iris)
summary(iris)

## export practice dataset as csv. this will write to the working directory
write.csv(iris,file = './iris.csv')

## clear workspace
rm(list=ls())

# read in the test dataset again but from the CSV file
## 150 cases, 5 vars.
library(readr)
iris <- read_csv("~/dev/cheat_sheet/iris.csv", 
                 col_types = cols(Petal.Length = col_number(), 
                                  Petal.Width = col_number(), 
                                  Sepal.Length = col_number(), 
                                  Sepal.Width = col_number(), 
                                  Species = col_character(), 
                                  X1 = col_skip()))


library("dplyr")
## different ways to preview a file
glimpse(iris)
str(iris)
## first 5 observations
head(iris)
# last five
tail(iris)

# create a table of oberservations
iris_summary<-iris %>% 
  group_by(Species) %>% 
  summarise(
    avg = mean (Sepal.Length)
    ,low = min (Sepal.Length)
    ,high = max (Sepal.Length)
    ,med = median (Sepal.Length)
    ,s_count = n()
    )
# rename column
iris_summary2<-rename(iris_summary, Species.Count = s_count)

## create new variable
iris_summary2$tripple <- iris_summary2$Species.Count*3


## split files by Species
iris_versicolor <- iris[ which(iris$Species=='versicolor'),]

iris_setosa <- iris[ which(iris$Species=='setosa'),]

iris_virginica <- iris[ which(iris$Species=='virginica'),]

iris_remerge<-do.call("rbind", list(iris_versicolor, iris_setosa, iris_virginica))

# compare datasets
install.packages("compare")
library ("compare")
comparison1 <- compare(iris,iris_remerge,allowAll=TRUE)
comparison1$result

comparison2 <- compare(iris_versicolor,iris_setosa,allowAll=TRUE)
comparison2$result

# random samples
## set random seed
set.seed(1)

## pull sample
iris_sample1 <- iris[sample(1:nrow(iris), 10,
                          replace=FALSE),]

### drop columns
iris_no_l <- names(iris_sample1) %in% c("Sepal.Length") 
iris_missing_l <- iris_sample1[!iris_no_l]

### drop columns
iris_no_w <- names(iris_sample1) %in% c("Sepal.Width") 
iris_missing_w <- iris_sample1[!iris_no_w]

# inner join
iris_joined<-merge(
  iris_missing_l, iris_missing_w, 
    by=c(
        "Species","Petal.Width","Petal.Length"
        )
  )

# ggplot2
# install.packages("ggplot2")
library("ggplot2")
## Simple Plot
ggplot(iris, # dataset
       aes(x = Sepal.Length, y = Petal.Length, # x and y
           color = Species)) + # classify by species
          geom_point() + geom_smooth(method = "lm", se = F) # prediction line
