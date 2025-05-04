library(tidyverse)
#loading the data
cel = read.csv("D:/cel_volden_wiseman _coursera.csv")

names(cel)
dim(cel)

table(cel$year)

#filter the data we want
fillg5 = cel %>% filter(congress == 115) %>% select("seniority", "all_pass")
head(fillg5)

#using the ggplot functions for selecting data and doing the aesthetics

ggplot(fillg5, aes(x = seniority, y = all_pass))

#adding the marks
ggplot(fillg5, aes(x = seniority, y = all_pass))+
  geom_point()

#jitter adds some random noise
ggplot(fillg5, aes(x = seniority, y = all_pass))+
  geom_jitter()

#adding labels
ggplot(fillg5, aes(x = seniority, y = all_pass))+
  geom_jitter()+
  labs(x = "seniority's", y = "Bills passed", title = "Graphe")

#modify filter and select to grab "dem"
fillg6 = cel %>%
  filter(congress == 115) %>%
  select("seniority", "all_pass", "dem")
fillg6$dem

ggplot(fillg6, aes(x = seniority, y = all_pass, colour = dem))+
  geom_jitter()+
  labs(x = "seniority's", y = "Bills passed", title = "Graphe")

#re-codding the data to make the graph in good shape

re_filg6 = recode(fillg6$dem, '1' = 'Democrat', '0' = 'Republican')
fillg6 = add_column(fillg6, re_filg6)
fillg6

ggplot(fillg6, aes(x = seniority, y = all_pass, colour = re_filg6))+
  geom_point()+
  labs(x = "seniority's", y = "Bills passed", title = "Graphe")+
  scale_color_manual(values = c('blue', 'red'))


#making 2 different figures based on the different observations 
ggplot(fillg6, aes(x = seniority, y = all_pass, colour = re_filg6))+
  geom_point()+
  labs(x = "seniority's", y = "Bills passed", title = "Graphe")+
  scale_color_manual(values = c('blue', 'red'))+
  facet_wrap(~re_filg6)


#making that command easier
cel %>%
  filter(congress == 115)%>%
  ggplot(aes(x = dem))+
  geom_bar()


#plotting the number of members of the congress 115 according to their states
cel %>% filter(congress == 115)%>%
  ggplot(aes(x = st_name))+
  geom_bar()

#flipping the graph
cel %>% filter(congress == 115)%>%
  ggplot(aes(y = st_name))+
  geom_bar()
 
#re-coddingand plotting
dem1 = recode(cel$dem, "1" = "Democrat", "0" = "Repablican")
cel = add_column(cel, dem1)
cel$dem1
cel %>% 
  filter(congress == 115) %>% 
  ggplot(aes(x = dem1, colour = dem1, fill = dem1))+
  geom_bar()+
  labs(x = "democrats", y = "counts")+
  scale_color_manual(values = c('black', 'orange'))+
  facet_wrap(~dem1)+scale_fill_manual(values = c("black", "orange"))+
  guides(fill = FALSE)




#creating a sequence of years
stock_id = rep("stock_1", 20)
y = seq(from = 2001, to = 2020, by = 1)

#creating a 'fake' data for price (note, your values will be different)
price = rnorm(20, mean = 15, sd = 5)

#putting them together
stock_1_time_series = tibble("years" = y, "stock_price" = price, "stock_id" = stock_id)
stock_1_time_series

ggplot(fig_data, aes(x = y, y = stock_price))+
  geom_line()

#creating a data for the second company
stock_id = rep("stock_2", 20)

years = seq(from = 2001, to = 2020, by = 1)
years
price = rnorm(20, mean = 10, sd = 3)
stock_2_time_series = tibble("year" = years,
                             "stock_price" = price,"stock_id" = stock_id)
stock_2_time_series

#combing the 2 data frames
all_stocks_time_series = bind_rows(stock_1_time_series, stock_2_time_series)
all_stocks_time_series
#plotting them
ggplot(all_stocks_time_series, (aes(x=years, y=stock_price, group = stock_id,
                                    linetype = stock_id, colour = stock_id)))+
  geom_line()
view(all_stocks_time_series)  






#creating my own data frame
players = c("Messi", "Maradona", "Pele")
goals = c(700, 300, 1026)
assists = c(600, 80, 112)
data_players = data_frame(players, goals, assists)

data_players = add_column(data_players,'assists devided by goals' = (assists/goals)*100)


#plotting the data

ggplot(data_players, aes(x = players, y = goals, colour = players))+
  geom_point()+
  scale_color_manual(values = c('blue', 'black', 'yellow'))
  facet_wrap(~players)+
    labs(x = 'Names of the greatest three players in the history', 
         y = 'The count of the assists')
  
    
  
  

    
ggplot(aes(x = players, y = assists))+
  geom_bar()+
  labs(x = 'Names of the greatest three players in the history', 
       y = 'The count of the assists')

 
ggplot(aes(x = players, y = `assists devided by goals`))+
  geom_bar()+
  labs(x = 'Names of the greatest three players in the history', 
       y = 'The count of the assists devided by the goals')







library(RColorBrewer)






##tetris
kid = c("Med", "sidi", "Abdellahi", "Ahmed", "Amar", "ya7ye")
time_spent = c(100, 54, 76, 74, 79, 12)
hight_score = c(200, 51, 56,15, 20, 84)
tetris = tibble(kid, time_spent, hight_score)
tetris

#normal plotting
ggplot(tetris, aes(x = time_spent, y = hight_score))+
  geom_point()+
  geom_text(aes(label = kid), nudge_y = 5)

#appling this on the data
cel = drop_na(read_csv("D:/cel_volden_wiseman _coursera.csv"))

cel %>% filter(congress==115)%>%
  ggplot(aes(x = dwnom1, y=all_pass, label = thomas_name))+
  geom_point()+
  geom_text()

#solving this ussie
cel%>%filter(congress==115)%>%
  ggplot(aes(x = dwnom1, y=all_pass, label = thomas_name))+
  geom_point()+
  scale_color_brewer(palette = "RdYlGn")+
  geom_text(data = filter(cel, congress==115 & all_pass > 8))

#solving that second issue by using the functions in this library
library(ggrepel)
cel%>%filter(congress==115)%>%
  ggplot(aes(x = dwnom1, y=all_pass, label = thomas_name))+
  geom_point()+
  geom_text_repel(data = filter(cel, congress==115 & all_pass > 8),
                  mapping = aes(x=dwnom1, y=all_pass, label = thomas_name))+
  annotate("rect", xmin = 0.5, xmax = 4, ymin = 13, ymax = 15, alpha=0.2, fill = "red")+
  annotate("text", x=0.6, y = 14, label = "Most Passed", color = "red")

##scale_color_gradient(low = "", high="")  #for making a scale of a color according to the observations
##scale_color_brewer(palette = "RdYlGn") it is a variable from the RColorBrewer to make graphs better

###using theme()
library(ggthemes)
ggplot(data = fillg5, aes(x= seniority, y= all_pass, color = all_pass))+
  geom_jitter()+
  theme_wsj()+
  theme(axis.text = element_text(angle = 90))






#treating the time_series
#the graph line
units = letters[1:2]
time = seq(1:10)
time_series = expand.grid(units = units, time = time)
time_series$variables = runif(20, 0, 5)
line_plot = ggplot(time_series, aes(x=time, y =variables, group = units))+
  geom_line()
line_plot
time_series

#barplot
cases = letters[1:5]
y_axis1 = runif(5, 0, 5)
y_axis2 = runif(5, 0, 5)
y_axis3 = runif(5, 0, 5)
bar_dat = data.frame(cases = cases, y1 = y_axis1, y2 = y_axis2, y3 = y_axis3)
long_bar_dat = pivot_longer(bar_dat, c("y1", "y2", "y3"), names_to = "measure", values_to = "value")
barplot = ggplot(long_bar_dat, aes(x = cases, y =  value, fill = measure))+
  geom_bar(stat = "identity", position = "dodge")
line_plot
scatter_plot
barplot
ggsave("hadrami's first graph.pdf", plot = line_plot)
getwd()
