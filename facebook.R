library(tidyverse)
Data = read.csv('D:/serie temporelles/hong kong/facebook.csv')
View(Data)
tib_data = as_tibble(Data)
tail(tib_data)
ggplot(tib_data, aes(x=Low, y=Close))+
  geom_point()
