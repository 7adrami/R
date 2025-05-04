library(dslabs)
data("murders")
View(murders)
class(murders)#the class of the dataframe
str(murders)#stricture
head(murders)#to show the first 6 lines of the dataframe
tail(murders)#to show the last 6 lines of the dataframe
murders$population  #to treat  a variable alone in a dataframe
names(x = murders)#it gives the names of the variables of the dataframe

###tests
length(murders$population)
log(1024, 4)
data("movielens")
class(movielens)
str(movielens)
levels(movielens$genres)
###tests ends

codes = c(italy = 380, usa = 180, canada = 360)
codes
names(codes)
seq(1, 10, 2)#sequance
#or
1:10
codes[1]

sort(murders$total)#sorting the dataframe
x = c(10,1, 20, 58, 0, 12 ,58, 4, 23, 99)
i = order(x)
x[i]
#sorting the database

index = order(murders$total)

murders$abb[index]


murders$state[which.max(murders$population)]#specifing the state which has 
#the max murders

murders_rate = (murders$total/murders$population)*100000#to specifie a vector 
#which of the rate of the murdering ratio
murders_rate
murders$state[order(murders_rate, decreasing = TRUE)]#to make that order of 
#the states which has the must murdering ratio in decreasing order


###tests
sort(x)
order(x)
rank(x)
y = c(2, 43, 27, 96, 18)
min(y)
which.min(y) #which is to show you the index of the object that you put right after it
max(y)
which.max(y)
name = c("mandi", "amy", "nicole", "olivia")
dis = c(0.8, 3.1, 2.8, 4.0)
time = c(10, 30, 40, 50)
time_h = time/60
speed = dis/time_h
print(name[4])
print(speed[4])
speed
which.max(speed)
###test ends

#index with the logical variables

index = murders_rate < 0.71
index = murders_rate <= 0.71
index
murders$state[index]

#using and with the logical variables to see which region is the must safety
west <- murders$region == "West"
safe <- murders_rate <= 1
index = west & safe
murders$state[index]

#using the logical vectors with a 'which' function to see when 
#the 'Massachusetts' is in the dataframe
index = which(murders$state == 'Massachusetts')
index
murders_rate[index]

#using the logical variables and the functoin 'match' to show 
#the indexs correspondant to the murders$state
index = match(c("New York", "Florida", "Texas"), murders$state)
index
murders$state[index]
murders_rate[index]

#using the logical variables and the functoin '%in%' to show 
#if the element is in the list or not
a_e = c('a', 'b', 'c', 'd', 'e')
a_b_f = c('a', 'b', 'f')
a_b_f %in% a_e
c("New York", "Dakota", "Washington", "London") %in% murders$state


###tests
!'ind' %in% murders$state #this line must return TREU
###test ends

#suing 'mutate', 'filter' and  to make thing easier 
#(murders) is the table that we're gonna
library(tidyverse)
murders = mutate(murders, rate = (total/population)*100000)
filter(murders, rate <=0.71)
new_table = select(murders, state, region,  rate)
filter(new_table, rate <=0.71)
library(dplyr)
library(magrittr)
#using the pipes to create new variables and select it and filter it in one line of code
murders %>% select(state, region, rate) %>% filter(rate <=0.71)



##creating dataframes
notes = data.frame(names = c("John", "Juan", "Jean", "Yao"), 
                   DS = c(10, 15, 14, 9), 
                   examen = c(11, 13, 10, 12),
                   stringsAsFactors = FALSE)
notes
class(notes$DS)

#plotting 
populatoin_in_millions = murders$population/10^6
total_gun_murders = murders$total
plot(populatoin_in_millions, total_gun_murders)

#histogram
hist(murders$rate, col = "red")

#using the summarize statistics
l = murders %>%
  filter(region == "West")%>%
  summarize(maximum = max(rate),
            minimum = min(rate),
            median = median(rate))
l
h = notes  %>% 
  filter(DS >=0)  %>% 
  summarize(le_plus_faible_moyen = min((examen + DS)/2),
            le_majeur= max((DS + examen)/2))
h

the_quantile = function(x){
  r = quantile(x, c(0, 0.5, 1))
  data.frame(minimum = r[1], maximum = r[2], median = r[3])
}
murders %>%
  filter(region == "West")%>%
  summarize(the_quantile(rate))

#using group_by to group the regions 
murders %>% group_by(region)

#using arrange() to sort the data
murders %>% arrange(desc(rate)) %>% head()#top_n(10)

murders %>% arrange(region,rate) %>% head()

murders %>% top_n(10, rate)

#using data.table to to tangle and analyze the data faster
library(data.table)
murders = setDT
murders[, c("state", "region")] 
murders[,rate :=total/population*100000]




#new data set treated with the data.table methods
library(dslabs)
data("heights")
heights = setDT(heights)

##in dplyr
s = heights %>%
  summarize(average = mean(height), equartype = sd(height))
s

#with filtering
l = heights %>%
  filter(sex == 'Female')%>%
  summarize(moyen = mean(height), eqtp = sd(height), minimum = min(height), 
            maximum = max(height))

l
##in data.table
s = heights[, .(average = mean(height), equartype = sd(height))]
s

#with filtering
l = heights[sex == 'Female',
            .(moyenne = mean(height), eqtp = sd(height),
              minimum = min(height), maximum = max(height))]
l

##returning the minimum and the maximum and the median with a function

#with dplyr
moyen_min_max = function(x){
  qs = quantile(x, c(0, 0.5, 1))
  data.frame(moyen = qs[2], minimum = qs[1], maximum = qs[3])
}

heights[, .(moyen_min_max(height))]

#summarize for different groups, using dplyr (group_by, summarize)


#summarize for different groups, using data.table (group_by, summarize)
heights[, .(moyen = mean(height), eqtp = sd(height)), by = sex]

murders %>% group_by(region)

#using tibble to make data frames more readable
gapminder

as_tibble(gapminder)


###test
library(dslabs)
data("heights")
options(digits = 3)

a = 2
if(a!=0){
  print(1/a)
 
}else{
  print("n'est pas inversible!")
}

ind = which.min(murders_rate)
if (murders_rate[ind] < 0.5){
  print(murders$state)
}else{
  print("No state has murder rate that low")
}


moyenne = function(x, arithmetic = TRUE){
  n = length(x)
  s = sum(x)
  ifelse(arithmetic, s/n, prod(x)^(1/n))
}
x = 1:100
moyenne(x, arithmetic = FALSE)

