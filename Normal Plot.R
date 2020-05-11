
#required package
library(ggplot2)
library(scales)

#Data
mean<-0.061
vol<-0.0906
range<- 4 #this is the range in terms of number of SDs from mean
min<- mean-range*vol
max<- mean+range*vol
data<-data.frame(x=c(min, max))



p1 <- ggplot(data, aes(x)) + 
  theme(panel.grid=element_blank(),
        panel.background=element_blank(),
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank())+ 
  stat_function(fun = dnorm, n = 501, args = list(mean, sd = vol)) + 
  labs(y="", x="Return") +
  scale_x_continuous(labels=percent, breaks=seq(min,max,vol), expand=c(0,0))
p1


