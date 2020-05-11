library("ggplot2")
library("scales")
library("ggrepel")

#Inputs
m<-0.061
vol<-0.12
m_adj<-m+(vol^2)/2
actuals<-c(1.6,10.5,12.8,rep(NA,7))
graph_labels<-c("2017:1.6%","2018:10.5%","2019:12.8%",rep(NA,7))

years<-10
numsim<-10000

#N(0,1) matrix
set.seed(123456)
ar<-matrix(rnorm(numsim * years,mean = m_adj, sd = vol),years)

#Cumulative returns calculations
arcum<-ar
for(row in 2:nrow(arcum)){
  arcum[row,] <- ((1 + arcum[row-1,]) * (1 + arcum[row,])) - 1
}

avreturn <- arcum
for(row in 1:nrow(avreturn)){
  avreturn[row,] <- ((1+arcum[row,])^(1/row))-1
  avreturn[row,] <- avreturn[row,] * m/median(avreturn[row,])
}

#Graphic parameters
x<-2017+seq(0,years-1,1) #general parameter for x-axis on graphs
quantiles<-c(0.05,0.1, 0.25,0.5,0.75,0.9, 0.95) #quantiles to show in funnels
quantiles_names<-c("P5", "P10","P25", "Median", "P75", "P90","P95")

#Graph calculations

quant<-matrix(nrow=years, ncol=7) #change ncol if you want more ribbons
quant[1,]<-100*(m+vol*qnorm(quantiles)) #first row is just the range of returns for a 1 year return
for(row in 2:years){
  quant[row,] <- quantile(avreturn[row,], probs=c(0.05,0.1, 0.25,0.5,0.75,0.9, 0.95))*100 #change probs if you want more/less ribbons
}
colnames(quant)<-quantiles_names

data<-data.frame(x=2017+seq(0,years-1,1), actuals=actuals,labels=graph_labels)

data<-cbind(data,quant)

ggplot(data=data, aes(x=x))+ theme_classic(base_size = 8) +
  geom_ribbon(aes(ymax=P95, ymin=P5,fill="5%-95%")) + 
  geom_ribbon(aes(ymax=P90, ymin=P10,fill="10%-90%")) +
  geom_ribbon(aes(ymax=P75, ymin=P25,fill="25%-75%")) +
  scale_fill_manual(values=c("skyblue3","skyblue4", "skyblue2"))+
  geom_hline(yintercept=m*100, size=1, colour="Black", linetype=2)+
  scale_x_continuous(breaks = seq(2017,2026,1), expand=c(0,0.1))+
  scale_y_continuous(breaks = seq(-20,30,5),expand = c(0,0.5))+
  ggtitle("Infrastructure Allocation", subtitle= "Long-term Average Annualised Nominal Return - 10 Year View")+
  theme(plot.title=element_text(face="bold", size=12))+
  labs(fill="Probability Range", x = "Projection Year", y = "Geometric Average Annualised Nominal Return (%)", caption="Note: 2017 return covers 13 month period from December 2016.")+
  theme(legend.position=c(0.9,0.2),
        legend.background=element_rect(linetype="solid", colour="black"))+
  geom_point(aes(y=actuals), colour="white", alpha=0.75, size=4)+
  geom_point(aes(x=2019.25,y=9.5), colour="white", alpha=0.75, size=4)+
  geom_text_repel(aes(label=graph_labels,x=x,y=actuals), box.padding = 0.5,  colour="white", fontface=2, force=1)+
  annotate("text", x=2020,y=9.7,label="2020 Q1: 9.5%", size=4, colour="white", fontface=2)+
  annotate("text", x=2025.5,y=7,label="EROA 6.1%", size=4, colour="white", fontface=2)
