df <- data.frame(month=rep(1:3,2),
                 student=rep(c("Amy", "Bob"), each=3),
                 A=c(9, 7, 6, 8, 6, 9),
                 B=c(6, 7, 8, 5, 6, 7))

df
library(tidyverse)
df %>% 
  gather(variable, value, -(month:student))
  unite(temp, student, variable) %>%
  spread(temp, value)

  
df %>% 
 gather(variable, value, -month, -student) %>% 
 unite(temp, student, variable) %>% 
 spread(temp, value)

        

df %>% 
 gather(variable, value, -month, -student) %>% 
 unite(tmp, student, variable) %>% 
 spread(tmp, value) %>% 
 gather(tmp, value, -month)
