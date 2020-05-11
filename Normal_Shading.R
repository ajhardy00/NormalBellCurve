library("ggplot2")
library("scales")

#Inputs
m<-0.061
vol<-0.12
length<-100

#create vectors with z-values
x<-seq(m-3*vol,m+3*vol,length.out=6*length)
x1<-seq(m-3*vol,m-2*vol,length.out=length)
x2<-seq(m-2*vol,m-vol,length.out=length)
x3<-seq(m-vol,m+vol,length.out=2*length)
x4<-seq(m+vol,m+2*vol,length.out=length)
x5<-seq(m+2*vol,m+3*vol,length.out=length)

#create areas for graph
line<-data.frame(x=x,y=dnorm(x,mean=m,sd=vol))
areax1<-data.frame(x=x1,ymin=0,ymax=dnorm(x1,mean=m, sd=vol))
areax2<-data.frame(x=x2,ymin=0,ymax=dnorm(x2,mean=m, sd=vol))
areax3<-data.frame(x=x3,ymin=0,ymax=dnorm(x3,mean=m, sd=vol))
areax4<-data.frame(x=x4,ymin=0,ymax=dnorm(x4,mean=m, sd=vol))
areax5<-data.frame(x=x5,ymin=0,ymax=dnorm(x5,mean=m, sd=vol))

#position of labels
label_y=dnorm(m,mean=m, sd=vol)/7

#colours for area shading
shade1<-"skyblue2"
shade2<-"skyblue3"
shade3<-"skyblue4"

plot<-ggplot()+
  geom_line(data=line, mapping=aes(x=x,y=y), colour="grey")+
  geom_ribbon(data=areax1,mapping=aes(x=x,ymin=ymin,ymax=ymax), fill=shade1, alpha=0.25)+
  geom_ribbon(data=areax2,mapping=aes(x=x,ymin=ymin,ymax=ymax), fill=shade2, alpha=0.5)+
  geom_ribbon(data=areax3,mapping=aes(x=x,ymin=ymin,ymax=ymax), fill=shade3, alpha=1)+
  geom_ribbon(data=areax4,mapping=aes(x=x,ymin=ymin,ymax=ymax), fill=shade2, alpha=0.5)+
  geom_ribbon(data=areax5,mapping=aes(x=x,ymin=ymin,ymax=ymax), fill=shade1, alpha=0.25)+ 
  theme(panel.grid=element_blank(),
        panel.background=element_blank(),
        axis.ticks.y = element_blank(), 
        axis.text.y = element_blank())+
  scale_x_continuous(labels=label_percent(accuracy=0.1), breaks=seq(min(x),max(x),vol))+
  scale_y_continuous(breaks=c(0,10))+
  labs(x="Expected Return",y="")+
  annotate(geom="text",x=m-vol/2,y=label_y,label="34%",colour="black", size=6)+
  annotate(geom="text",x=m+vol/2,y=label_y,label="34%",colour="black", size=6)+
  annotate(geom="text",x=m+vol*3/2,y=label_y,label="13.5%",colour="black", size=6)+
  annotate(geom="text",x=m-vol*3/2,y=label_y,label="13.5%",colour="black", size=6)+
  annotate(geom="text",x=m+vol*5/2,y=label_y,label="2.35%",colour="black", size=6)+
  annotate(geom="text",x=m-vol*5/2,y=label_y,label="2.35%",colour="black", size=6)+
  geom_segment(aes(x=m,y=0,xend=m,yend=dnorm(m,mean=m, sd=vol)), colour="gray82")+
  annotate(geom="text",x=m,y=3.7,label="68%",colour="black", size=6)+
  geom_segment(aes(x=min(x3),y=3.5,xend=max(x3),yend=3.5),size=1,arrow=arrow(ends="both", length=unit(0.5,"cm")))+
  annotate(geom="text",x=m,y=4.1,label="95%",colour="black", size=6)+
  geom_segment(aes(x=min(x2),y=3.9,xend=max(x4),yend=3.9),size=1,arrow=arrow(ends="both",length=unit(0.5,"cm")))


plot
