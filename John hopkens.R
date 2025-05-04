x = read.csv('D:/john.csv')

#importing data
library(readxl)

data = read_xlsx('D:/Downloads/JOHN.xlsx')
View(data)

library(tidyverse)
data("mtcars")
view(mtcars)

#importing data with tidyverse's approach
cs = read_csv('D:/data.csv')
view(cs)
john = read_xlsx("D:/Downloads/JOHN.xlsx")
view(john)

#dropping the NA
john = drop_na(john)
view(john)
john_tibble = as_tibble(john)
john
women = filter(john, gender == 2)
women
dim(john_tibble)
dim(women)

table(john_tibble$gender)
women$gender


repablican_women = filter(john_tibble, gender == 2 & pid7 > 4)
repablican_women

dim(repablican_women)
repablican_women$gender
repablican_women$pid7

##select some columns from the data base
select(repablican_women, "employ", "educ")

##using pipes to do the same operation
repablican_women_employ_educ = john %>% filter(gender == 2 & pid7 > 4) %>% select("educ", "employ")
repablican_women_employ_educ


#recording the variables
policy = recode(john_tibble$pid7, '1' = "democrat", '2' = "democrat", '3' = "democrat",
                '4' = "independent", '5' = "republician", '6' = "republician",
                '7' = "republician")
policy
john_tibble$policy = policy
john_tibble$policy


#renaming variables

renamed = rename(john_tibble, trump_apporva = "CC18_308a")
renamed
john_tibble$trump_approval = renamed
john_tibble$trump_approval

#calculate a new numeric variable
rec_sen1_01 = recode(john_tibble$CC18_310b, '1' = 0, '5' = 0,
                    '2' = 1, '3' = 1, '4' = 1) 
rec_sen2_01 = recode(john_tibble$CC18_310c, '1' = 0, '5' = 0,
                    '2' = 1, '3' = 1, '4' = 1) 
rec_sen1_01
rec_sen2_01

john = mutate(john_tibble, know_sen = rec_sen1_01+rec_sen2_01)
john$know_sen


#sorting by variables
sorted_by_by_gender_and_policy = john %>% arrange(gender, pid7)
view(sorted_by_by_gender_and_policy)


sorted_by_by_gender_and_policy = john %>% arrange(gender, desc(pid7))
view(sorted_by_by_gender_and_policy)


#grouping data by 
grouped_by_gender_pid7 = john_tibble  %>% group_by(gender, pid7)
grouped_by_gender_pid7


#using summarize
summarize(john_tibble, mean_pid7 = mean(pid7), mean_faminc = mean(faminc_new))


#summarize for grouped data

#grouped_gender = group_by(john_tibble, gender)
grouped_gender = john_tibble  %>% group_by(gender)
summarize(grouped_gender, mean_pidd7 = mean(pid7), mean_faminc = mean(faminc_new))


#installing the 'tinytex'packege
install.packages('tinytex')
tinytex::install_tinytex()#install Tinytex
