mtcars[,c(1,2)]
library(PerformanceAnalytics)
my_data <- mtcars[, c(1,3,4,5,6,7)]
chart.Correlation(my_data, histogram=TRUE, pch=19)

head
library(dplyr)
mtcars %>% 
  select(mpg,wt,drat) %>% 
  chart.Correlation(histogram = T, pch=19, col="grey")
title(main="Correlations to ",
      sub="subtitle")



library(ggplot2)

# GET EQUATION AND R-SQUARED AS STRING
# SOURCE: http://goo.gl/K4yh

lm_eqn <- function(mtcars){
  m <- lm(y ~ x, mtcars);
  eq <- substitute(italic(y) == a + b %.% italic(x)*","~~italic(r)^2~"="~r2, 
                   list(a = format(coef(m)[1], digits = 2), 
                        b = format(coef(m)[2], digits = 2), 
                        r2 = format(summary(m)$r.squared, digits = 3)))
  as.character(as.expression(eq));                 
}


p3 <- function(x) {
  (5^2-x^2)^0.5
}

mtcars %>% 
  select(mpg,wt,drat) %>% 
  ggplot(aes(x=mpg, y=wt))+
  geom_point()+
  geom_smooth(method="lm",formula = y~x, level=0.9)+
  theme_bw()+
  xlim(0,5)+
  ylim(0,5)+
  labs(title="test",
       x="car type",
       y="weight")+
  geom_hline(aes(yintercept=3), color="red")+
  geom_abline(intercept = 0, slope = 1/7)+
  stat_function(fun=p3)

df <- data.frame(random=c(1:100)) 

ggplot(df, aes(x=random))+
  stat_function(fun=p3)+
  xlim(-5,5)+
  ylim(-5,5)+
  coord_equal(ratio=1)

speed <- function(g,h){
  sqrt(2*g*h)
}  

speed(9.8,15)

ggplot(data.frame(x=c(-2,2)), aes(x=x))+
  stat_function(fun = function(x){sin(x)}, aes(color="sin"))+
  stat_function(fun = function(x)cos(x), aes(color="cos"))+
  stat_function(fun = function(x)tan(x), aes(color="tan"))+
  # ylim(c(-1,1))+
  labs(color="test")

CuRec <- function(a, b, Cu) {
  (a*b*Cu/(1+b*Cu))*(1-exp(-b*Cu))
}

library(ggplot2)
ggplot(data.frame(Cu=c(0,1)), aes(x=Cu))+
  stat_function(fun = function(x)CuRec(98,14.5,x), aes(color="South West Ore"))+
  stat_function(fun = function(x){CuRec(72,15.0,x)}, aes(color="Central Chalcocite"))+
  stat_function(fun = function(x){CuRec(80,15.0,x)}, aes(color="Central Covellite"))+
  stat_function(fun = function(x){CuRec(88,12.2,x)}, aes(color="Central Chalcopyrite"))+
  labs(x = "Cu Head Grade",
       y = "Cu Recovery",
       color = "Ore Types")+
  theme_bw()
  

  
