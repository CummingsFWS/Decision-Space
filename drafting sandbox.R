library(shiny)
library(tidyverse)

# All figures
time<-seq.int(0,100,10)
# Fig 1. Framework
cP_Read_Me<-seq.int(0,100,10)
data_Read_Me<-tibble(time,cP_Read_Me)

  ggplot(data_Read_Me,aes(time,cP_Read_Me))+
    labs(x='Time',y='Cumulative Probability of Extinction') +
    scale_x_continuous(labels=c("Present","Future"),breaks=c(0,100),expand=c(0,0))+
    scale_y_continuous(breaks=seq.int(0,100,10),expand=c(0,0))+
    coord_cartesian(xlim =c(0,100), ylim = c(0,100))+
    theme_bw()+
    theme(axis.text.x=element_text(hjust=c(0,1),vjust=0.2))+
    theme(text = element_text(size = 20))
  
# Fig 2. Risk Threshold
cP_Warranted<-seq.int(0,100,10)
Risk_Threshold<-rep(25,11)
data_Warranted<-tibble(time,cP_Warranted,Risk_Threshold)

ggplot(data_Warranted,aes(time,Risk_Threshold))+
  geom_ribbon(aes(ymin=Risk_Threshold,ymax=Risk_Threshold+100),fill="red",alpha=0.25)+
  geom_ribbon(aes(ymin=Risk_Threshold-100,ymax=Risk_Threshold),fill="green",alpha=0.25)+
  geom_line(color="red")+
  annotate(geom="label",x = 50, y = mean(c(100,mean(Risk_Threshold))),label="warranted",color="red")+
  annotate(geom="label",x = 50, y = mean(c(0,mean(Risk_Threshold))),label="not warranted",color="green")+
  labs(x='Time',y='Cumulative Probability of Extinction') +
  scale_x_continuous(labels=c("Present","Future"),breaks=c(0,100),expand=c(0,0))+
  scale_y_continuous(breaks=seq.int(0,100,10),expand=c(0,0))+
  coord_cartesian(xlim =c(0,100), ylim = c(0,100))+
  theme_bw()+
  theme(axis.text.x=element_text(hjust=c(0,1),vjust=0.2))+
  theme(text = element_text(size = 20))

# Fig 3. Time Threshold
cP_Time<-seq.int(0,100,10)
Time_Threshold<-20
data_Time<-tibble(time,cP_Time,Time_Threshold)

ggplot(data_Time)+
  geom_rect(aes(xmin=0,xmax=Time_Threshold,ymin=0,ymax=100),fill="orange",alpha=0.075)+
  geom_rect(aes(xmin=Time_Threshold,xmax=100,ymin=0,ymax=100),fill="khaki1",alpha=0.075)+
  geom_vline(xintercept=Time_Threshold)+
  annotate(geom="label",x = Time_Threshold/2, y = 50,label="endangered")+
  annotate(geom="label",x = Time_Threshold+(100-Time_Threshold)/2, y = 50,label="threatened")+
  labs(x='Time',y='Cumulative Probability of Extinction') +
  scale_x_continuous(labels=c("Present","Future"),breaks=c(0,100),expand=c(0,0))+
  scale_y_continuous(breaks=seq.int(0,100,10),expand=c(0,0))+
  coord_cartesian(xlim =c(0,100), ylim = c(0,100))+
  theme_bw()+
  theme(axis.text.x=element_text(hjust=c(0,1),vjust=0.2))+
  theme(text = element_text(size = 20))

# Fig 4. Policy Thresholds
data_Policy<-tibble(time,cP_Time,Time_Threshold,Risk_Threshold)

ggplot(data_Policy)+
  geom_rect(aes(xmin=0,xmax=Time_Threshold,ymin=0,ymax=100),fill="orange",alpha=0.075)+
  geom_rect(aes(xmin=Time_Threshold,xmax=100,ymin=0,ymax=100),fill="khaki1",alpha=0.075)+
  geom_ribbon(aes(ymin=Risk_Threshold,ymax=Risk_Threshold+100,x=time),fill="red",alpha=0.25)+
  geom_ribbon(aes(ymin=Risk_Threshold-100,ymax=Risk_Threshold,x=time),fill="green",alpha=0.25)+
  geom_vline(xintercept=Time_Threshold)+
  geom_hline(aes(yintercept=Risk_Threshold),color="red")+
  annotate(geom="label",x = Time_Threshold/2, y = 50,label="endangered")+
  annotate(geom="label",x = Time_Threshold+(100-Time_Threshold)/2, y = 50,label="threatened")+
  annotate(geom="label",x = Time_Threshold/2, y = mean(c(0,mean(Risk_Threshold))),label="not warranted",color="green")+
  annotate(geom="label",x = Time_Threshold+(100-Time_Threshold)/2, y = mean(c(0,mean(Risk_Threshold))),label="not warranted",color="green")+
  labs(x='Time',y='Cumulative Probability of Extinction') +
  scale_x_continuous(labels=c("Present","Future"),breaks=c(0,100),expand=c(0,0))+
  scale_y_continuous(breaks=seq.int(0,100,10),expand=c(0,0))+
  coord_cartesian(xlim =c(0,100), ylim = c(0,100))+
  theme_bw()+
  theme(axis.text.x=element_text(hjust=c(0,1),vjust=0.2))+
  theme(text = element_text(size = 20))

# Fig 5. Species Risk Curve
cP_Species_Risk_Curves<-seq.int(0,100,10)
data_Species_Risk_Curves<-tibble(time,cP_Species_Risk_Curves)

ggplot(data_Species_Risk_Curves,aes(time,cP_Species_Risk_Curves))+
  geom_ribbon(aes(ymin=cP_Species_Risk_Curves-0.5,ymax=cP_Species_Risk_Curves+0.5),fill="purple")+
  labs(x='Time',y='Cumulative Probability of Extinction') +
  scale_x_continuous(labels=c("Present","Future"),breaks=c(0,100),expand=c(0,0))+
  scale_y_continuous(breaks=seq.int(0,100,10),expand=c(0,0))+
  coord_cartesian(xlim =c(0,100), ylim = c(0,100))+
  theme_bw()+
  theme(axis.text.x=element_text(hjust=c(0,1),vjust=0.2))+
  theme(text = element_text(size = 20))

# Fig 6. Combined Threatened
cP_Species_Risk_Curves<-seq.int(0,100,10)
data_Combined<-tibble(time,cP_Species_Risk_Curves,Risk_Threshold,Time_Threshold)

ggplot(data_Combined)+
  geom_rect(aes(xmin=0,xmax=Time_Threshold,ymin=0,ymax=100),fill="orange",alpha=0.075)+
  geom_rect(aes(xmin=Time_Threshold,xmax=100,ymin=0,ymax=100),fill="khaki1",alpha=0.075)+
  geom_ribbon(aes(ymin=Risk_Threshold,ymax=Risk_Threshold+100,x=time),fill="red",alpha=0.25)+
  geom_ribbon(aes(ymin=Risk_Threshold-100,ymax=Risk_Threshold,x=time),fill="green",alpha=0.25)+
  geom_ribbon(aes(ymin=cP_Species_Risk_Curves-0.5,ymax=cP_Species_Risk_Curves+0.5,x=time),fill="purple")+
  geom_vline(xintercept=Time_Threshold)+
  geom_hline(aes(yintercept=Risk_Threshold),color="red")+
  annotate(geom="label",x = Time_Threshold/2, y = 50,label="endangered")+
  annotate(geom="label",x = Time_Threshold+(100-Time_Threshold)/2, y = 50,label="threatened")+
  annotate(geom="label",x = Time_Threshold/2, y = mean(c(0,mean(Risk_Threshold))),label="not warranted",color="green")+
  annotate(geom="label",x = Time_Threshold+(100-Time_Threshold)/2, y = mean(c(0,mean(Risk_Threshold))),label="not warranted",color="green")+
  labs(x='Time',y='Cumulative Probability of Extinction') +
  scale_x_continuous(labels=c("Present","Future"),breaks=c(0,100),expand=c(0,0))+
  scale_y_continuous(breaks=seq.int(0,100,10),expand=c(0,0))+
  coord_cartesian(xlim =c(0,100), ylim = c(0,100))+
  theme_bw()+
  theme(axis.text.x=element_text(hjust=c(0,1),vjust=0.2))+
  theme(text = element_text(size = 20))

# Fig 7. Combined E,T, & NW
cP_Species_Risk_T<-seq.int(0,100,10)
cP_Species_Risk_E<-c(seq.int(0,100,25),NA,NA,NA,NA,NA,NA)
cP_Species_Risk_NW<-seq.int(0,20,2)
data_Combined<-tibble(time,cP_Species_Risk_T,cP_Species_Risk_E,cP_Species_Risk_NW,Risk_Threshold,Time_Threshold)

ggplot(data_Combined)+
  geom_rect(aes(xmin=0,xmax=Time_Threshold,ymin=0,ymax=100),fill="orange",alpha=0.075)+
  geom_rect(aes(xmin=Time_Threshold,xmax=100,ymin=0,ymax=100),fill="khaki1",alpha=0.075)+
  geom_ribbon(aes(ymin=Risk_Threshold,ymax=Risk_Threshold+100,x=time),fill="red",alpha=0.25)+
  geom_ribbon(aes(ymin=Risk_Threshold-100,ymax=Risk_Threshold,x=time),fill="green",alpha=0.25)+
  geom_ribbon(aes(ymin=cP_Species_Risk_E-0.5,ymax=cP_Species_Risk_E+0.5,x=time),fill="brown")+
  geom_ribbon(aes(ymin=cP_Species_Risk_T-0.5,ymax=cP_Species_Risk_T+0.5,x=time),fill="purple")+
  geom_ribbon(aes(ymin=cP_Species_Risk_NW-0.5,ymax=cP_Species_Risk_NW+0.5,x=time),fill="grey")+
  geom_vline(xintercept=Time_Threshold)+
  geom_hline(aes(yintercept=Risk_Threshold),color="red")+
  annotate(geom="label",x = Time_Threshold/2, y = 50,label="endangered")+
  annotate(geom="label",x = Time_Threshold+(100-Time_Threshold)/2, y = 50,label="threatened")+
  annotate(geom="label",x = Time_Threshold/2, y = mean(c(0,mean(Risk_Threshold))),label="not warranted",color="green")+
  annotate(geom="label",x = Time_Threshold+(100-Time_Threshold)/2, y = mean(c(0,mean(Risk_Threshold))),label="not warranted",color="green")+
  labs(x='Time',y='Cumulative Probability of Extinction') +
  scale_x_continuous(labels=c("Present","Future"),breaks=c(0,100),expand=c(0,0))+
  scale_y_continuous(breaks=seq.int(0,100,10),expand=c(0,0))+
  coord_cartesian(xlim =c(0,100), ylim = c(0,100))+
  theme_bw()+
  theme(axis.text.x=element_text(hjust=c(0,1),vjust=0.2))+
  theme(text = element_text(size = 20))


# Fig 7. Risk Tolerance Area
cP_Warranted<-seq.int(0,100,10)
Warranted_Risk_Threshold<-rep(75,11)
Not_Warranted_Risk_Threshold<-rep(25,11)
data_Risk_Area<-tibble(time,cP_Warranted,Warranted_Risk_Threshold,Not_Warranted_Risk_Threshold)

ggplot(data_Risk_Area)+
  geom_line(aes(time,Warranted_Risk_Threshold),color="red")+
  geom_line(aes(time,Not_Warranted_Risk_Threshold),color="green")+
  geom_ribbon(aes(time, ymin=Warranted_Risk_Threshold,ymax=Warranted_Risk_Threshold+100),fill="red",alpha=0.25)+
  geom_ribbon(aes(time, ymin=Not_Warranted_Risk_Threshold,ymax=Warranted_Risk_Threshold),fill="yellow",alpha=0.25)+
  geom_ribbon(aes(time, ymin=Not_Warranted_Risk_Threshold-100,ymax=Not_Warranted_Risk_Threshold),fill="green",alpha=0.25)+
  annotate(geom="label",x = 50, y = mean(c(100,mean(Warranted_Risk_Threshold))),label="warranted",fill="red")+
  annotate(geom="label",x = 50, y = mean(c(Warranted_Risk_Threshold,Not_Warranted_Risk_Threshold)),label="dependent on case specific risk tolerance",fill="yellow")+
  annotate(geom="label",x = 50, y = mean(c(0,mean(Not_Warranted_Risk_Threshold))),label="not warranted",fill="green")+
  labs(x='Time',y='Cumulative Probability of Extinction') +
  scale_x_continuous(labels=c("Present","Future"),breaks=c(0,100),expand=c(0,0))+
  scale_y_continuous(breaks=seq.int(0,100,10),expand=c(0,0))+
  coord_cartesian(xlim =c(0,100), ylim = c(0,100))+
  theme_bw()+
  theme(axis.text.x=element_text(hjust=c(0,1),vjust=0.2))+
  theme(text = element_text(size = 20))

# Species Risk Curves with Uncertainty
cP_Species_Risk_Curves<-seq.int(0,100,10)
inner_Q<-20
middle_Q<-40
outer_Q<-60
upper_triangle.x<-c(0,0,100-outer_Q)
upper_triangle.y<-c(outer_Q,100,100)
lower_triangle.x<-c(outer_Q,100,100)
lower_triangle.y<-c(0,0,100-outer_Q)
group.triangle<-c(1,1,1,2,2,2)
dt.triangle<-tibble(group=group.triangle,triangle.x=c(upper_triangle.x,lower_triangle.x),triangle.y=c(upper_triangle.y,lower_triangle.y))
data_Species_Risk_Curves<-tibble(time,cP_Species_Risk_Curves)

# Fig 8. Species Risk Curve with Complete Uncertainty
ggplot(data_Species_Risk_Curves,aes(time,cP_Species_Risk_Curves))+
  geom_ribbon(aes(ymin=cP_Species_Risk_Curves-100,ymax=cP_Species_Risk_Curves+100),alpha=0.1,fill="purple")+
  annotate(geom="label",x = 50, y = 50,label="Decision Space",color="purple")+
  labs(x='Time',y='Cumulative Probability of Extinction') +
  scale_x_continuous(labels=c("Present","Future"),breaks=c(0,100),expand=c(0,0))+
  scale_y_continuous(breaks=seq.int(0,100,10),expand=c(0,0))+
  coord_cartesian(xlim =c(0,100), ylim = c(0,100))+
  theme_bw()+
  theme(axis.text.x=element_text(hjust=c(0,1),vjust=0.2))+
  theme(text = element_text(size = 20))

# Fig 9. Species Risk Curve with high Uncertainty
ggplot(data_Species_Risk_Curves,aes(time,cP_Species_Risk_Curves))+
  geom_ribbon(aes(ymin=cP_Species_Risk_Curves-100,ymax=cP_Species_Risk_Curves+100),alpha=0.1,fill="purple")+
  geom_polygon(data = dt.triangle,aes(x=triangle.x,y=triangle.y,group=group))+
  annotate(geom="label",x = 12.5, y = 92.5,label="Outside \n Decision Space",color="black")+
  annotate(geom="label",x = 87.5, y = 7.5,label="Outside \n Decision Space",color="black")+
  annotate(geom="label",x = 50, y = 50,label="Decision Space",color="purple")+
  labs(x='Time',y='Cumulative Probability of Extinction') +
  scale_x_continuous(labels=c("Present","Future"),breaks=c(0,100),expand=c(0,0))+
  scale_y_continuous(breaks=seq.int(0,100,10),expand=c(0,0))+
  coord_cartesian(xlim =c(0,100), ylim = c(0,100))+
  theme_bw()+
  theme(axis.text.x=element_text(hjust=c(0,1),vjust=0.2))+
  theme(text = element_text(size = 20))

# Fig 10. Species Risk Curve with low Uncertainty
inner_Q<-5
middle_Q<-10
outer_Q<-15
upper_triangle.x<-c(0,0,100-outer_Q)
upper_triangle.y<-c(outer_Q,100,100)
lower_triangle.x<-c(outer_Q,100,100)
lower_triangle.y<-c(0,0,100-outer_Q)
group.triangle<-c(1,1,1,2,2,2)
dt.triangle<-tibble(group=group.triangle,triangle.x=c(upper_triangle.x,lower_triangle.x),triangle.y=c(upper_triangle.y,lower_triangle.y))
data_Species_Risk_Curves<-tibble(time,cP_Species_Risk_Curves)

ggplot(data_Species_Risk_Curves,aes(time,cP_Species_Risk_Curves))+
  geom_ribbon(aes(ymin=cP_Species_Risk_Curves-100,ymax=cP_Species_Risk_Curves+100),alpha=0.1,fill="purple")+
  geom_polygon(data = dt.triangle,aes(x=triangle.x,y=triangle.y,group=group))+
  annotate(geom="label",x = 12.5, y = 92.5,label="Outside \n Decision Space",color="black")+
  annotate(geom="label",x = 87.5, y = 7.5,label="Outside \n Decision Space",color="black")+
  annotate(geom="label",x = 50, y = 50,label="Decision Space",color="purple")+
  labs(x='Time',y='Cumulative Probability of Extinction') +
  scale_x_continuous(labels=c("Present","Future"),breaks=c(0,100),expand=c(0,0))+
  scale_y_continuous(breaks=seq.int(0,100,10),expand=c(0,0))+
  coord_cartesian(xlim =c(0,100), ylim = c(0,100))+
  theme_bw()+
  theme(axis.text.x=element_text(hjust=c(0,1),vjust=0.2))+
  theme(text = element_text(size = 20))

# w/ uncertainty distribution
ggplot(data_Species_Risk_Curves,aes(time,cP_Species_Risk_Curves))+
  geom_ribbon(aes(ymin=cP_Species_Risk_Curves-outer_Q,ymax=cP_Species_Risk_Curves+outer_Q),alpha=0.1,fill="purple")+
  geom_ribbon(aes(ymin=cP_Species_Risk_Curves-middle_Q,ymax=cP_Species_Risk_Curves+middle_Q),alpha=0.1,fill="purple")+
  geom_ribbon(aes(ymin=cP_Species_Risk_Curves-inner_Q,ymax=cP_Species_Risk_Curves+inner_Q),alpha=0.1,fill="purple")+
  geom_polygon(data = dt.triangle,aes(x=triangle.x,y=triangle.y,group=group))+
  annotate(geom="label",x = 12.5, y = 92.5,label="Outside \n Decision Space",color="black")+
  annotate(geom="label",x = 87.5, y = 7.5,label="Outside \n Decision Space",color="black")+
  annotate(geom="label",x = 50, y = 50,label="Decision Space",color="purple")+
  labs(x='Time',y='Cumulative Probability of Extinction') +
  scale_x_continuous(labels=c("Present","Future"),breaks=c(0,100),expand=c(0,0))+
  scale_y_continuous(breaks=seq.int(0,100,10),expand=c(0,0))+
  coord_cartesian(xlim =c(0,100), ylim = c(0,100))+
  theme_bw()+
  theme(axis.text.x=element_text(hjust=c(0,1),vjust=0.2))+
  theme(text = element_text(size = 20))


# Combined
cP_Warranted<-seq.int(0,100,10)
Warranted_Risk_Threshold<-rep(30,11)
Not_Warranted_Risk_Threshold<-rep(10,11)
data_Combined<-tibble(time,cP_Warranted,cP_Species_Risk_Curves,Warranted_Risk_Threshold,Not_Warranted_Risk_Threshold)

# Fig 10 - warranted
ggplot(data_Combined,aes(time,cP_Species_Risk_Curves))+
  geom_ribbon(aes(ymin=cP_Species_Risk_Curves-outer_Q,ymax=cP_Species_Risk_Curves+outer_Q),alpha=0.15,fill="purple")+
  geom_ribbon(aes(ymin=cP_Species_Risk_Curves-middle_Q,ymax=cP_Species_Risk_Curves+middle_Q),alpha=0.15,fill="purple")+
  geom_ribbon(aes(ymin=cP_Species_Risk_Curves-inner_Q,ymax=cP_Species_Risk_Curves+inner_Q),alpha=0.15,fill="purple")+
  geom_line(aes(time,Warranted_Risk_Threshold),color="red")+
  geom_line(aes(time,Not_Warranted_Risk_Threshold),color="green")+
  geom_ribbon(aes(time, ymin=Warranted_Risk_Threshold,ymax=Warranted_Risk_Threshold+100),fill="red",alpha=0.2)+
  geom_ribbon(aes(time, ymin=Not_Warranted_Risk_Threshold,ymax=Warranted_Risk_Threshold),fill="yellow",alpha=0.2)+
  geom_ribbon(aes(time, ymin=Not_Warranted_Risk_Threshold-100,ymax=Not_Warranted_Risk_Threshold),fill="green",alpha=0.2)+
  geom_polygon(data = dt.triangle,aes(x=triangle.x,y=triangle.y,group=group))+
  annotate(geom="label",x = 50, y = mean(c(100,mean(Warranted_Risk_Threshold))),label="warranted",fill="red")+
  annotate(geom="label",x = 50, y = mean(c(Warranted_Risk_Threshold,Not_Warranted_Risk_Threshold)),label="dependent on case specific risk tolerance",fill="yellow")+
  annotate(geom="label",x = 50, y = mean(c(0,mean(Not_Warranted_Risk_Threshold))),label="not warranted",fill="green")+
  labs(x='Time',y='Cumulative Probability of Extinction') +
  scale_x_continuous(labels=c("Present","Future"),breaks=c(0,100),expand=c(0,0))+
  scale_y_continuous(breaks=seq.int(0,100,10),expand=c(0,0))+
  coord_cartesian(xlim =c(0,100), ylim = c(0,100))+
  theme_bw()+
  theme(axis.text.x=element_text(hjust=c(0,1),vjust=0.2))+
  theme(text = element_text(size = 20))

# Fig 11 - w/ decision space
cP_Species_Risk_Curves<-seq.int(0,50,5)
data_Combined<-tibble(time,cP_Warranted,cP_Species_Risk_Curves,Warranted_Risk_Threshold,Not_Warranted_Risk_Threshold)
upper_triangle.x<-c(0,0,100,100)
upper_triangle.y<-c(outer_Q,100,100,65)
lower_triangle.x<-c(30,100,100)
lower_triangle.y<-c(0,0,35)
group.triangle<-c(1,1,1,1,2,2,2)
dt.triangle<-tibble(group=group.triangle,triangle.x=c(upper_triangle.x,lower_triangle.x),triangle.y=c(upper_triangle.y,lower_triangle.y))

ggplot(data_Combined,aes(time,cP_Species_Risk_Curves))+
  geom_ribbon(aes(ymin=cP_Species_Risk_Curves-outer_Q,ymax=cP_Species_Risk_Curves+outer_Q),alpha=0.15,fill="purple")+
  geom_ribbon(aes(ymin=cP_Species_Risk_Curves-middle_Q,ymax=cP_Species_Risk_Curves+middle_Q),alpha=0.15,fill="purple")+
  geom_ribbon(aes(ymin=cP_Species_Risk_Curves-inner_Q,ymax=cP_Species_Risk_Curves+inner_Q),alpha=0.15,fill="purple")+
  geom_line(aes(time,Warranted_Risk_Threshold),color="red")+
  geom_line(aes(time,Not_Warranted_Risk_Threshold),color="green")+
  geom_ribbon(aes(time, ymin=Warranted_Risk_Threshold,ymax=Warranted_Risk_Threshold+100),fill="red",alpha=0.2)+
  geom_ribbon(aes(time, ymin=Not_Warranted_Risk_Threshold,ymax=Warranted_Risk_Threshold),fill="yellow",alpha=0.2)+
  geom_ribbon(aes(time, ymin=Not_Warranted_Risk_Threshold-100,ymax=Not_Warranted_Risk_Threshold),fill="green",alpha=0.2)+
  geom_polygon(data = dt.triangle,aes(x=triangle.x,y=triangle.y,group=group))+
  annotate(geom="label",x = 50, y = mean(c(100,mean(Warranted_Risk_Threshold))),label="warranted",fill="red")+
  annotate(geom="label",x = 50, y = mean(c(Warranted_Risk_Threshold,Not_Warranted_Risk_Threshold)),label="dependent on case specific risk tolerance",fill="yellow")+
  annotate(geom="label",x = 50, y = mean(c(0,mean(Not_Warranted_Risk_Threshold))),label="not warranted",fill="green")+
  labs(x='Time',y='Cumulative Probability of Extinction') +
  scale_x_continuous(labels=c("Present","Future"),breaks=c(0,100),expand=c(0,0))+
  scale_y_continuous(breaks=seq.int(0,100,10),expand=c(0,0))+
  coord_cartesian(xlim =c(0,100), ylim = c(0,100))+
  theme_bw()+
  theme(axis.text.x=element_text(hjust=c(0,1),vjust=0.2))+
  theme(text = element_text(size = 20))

# Fig 12 - examples
time<-seq.int(0,100,10)
cP_Warranted<-seq.int(0,100,10)
cP_Species_Risk_Curves<-seq.int(0,50,5)
Warranted_Risk_Threshold<-rep(40,11)
Not_Warranted_Risk_Threshold<-rep(10,11)
Time_Threshold<-20
data_Combined<-tibble(time,cP_Warranted,cP_Species_Risk_Curves,Warranted_Risk_Threshold,Not_Warranted_Risk_Threshold,Time_Threshold)

upper_triangle.x<-c(0,0,100,100)
upper_triangle.y<-c(outer_Q,100,100,65)
lower_triangle.x<-c(30,100,100)
lower_triangle.y<-c(0,0,35)
group.triangle<-c(1,1,1,1,2,2,2)
dt.triangle<-tibble(group=group.triangle,triangle.x=c(upper_triangle.x,lower_triangle.x),triangle.y=c(upper_triangle.y,lower_triangle.y))

ggplot(data_Combined,aes(time,cP_Species_Risk_Curves))+
  geom_ribbon(aes(ymin=cP_Species_Risk_Curves-outer_Q,ymax=cP_Species_Risk_Curves+outer_Q),alpha=0.15,fill="purple")+
  geom_ribbon(aes(ymin=cP_Species_Risk_Curves-middle_Q,ymax=cP_Species_Risk_Curves+middle_Q),alpha=0.15,fill="purple")+
  geom_ribbon(aes(ymin=cP_Species_Risk_Curves-inner_Q,ymax=cP_Species_Risk_Curves+inner_Q),alpha=0.15,fill="purple")+
  geom_line(aes(time,Warranted_Risk_Threshold),color="red")+
  geom_line(aes(time,Not_Warranted_Risk_Threshold),color="green")+
  geom_ribbon(aes(time, ymin=Warranted_Risk_Threshold,ymax=Warranted_Risk_Threshold+100),fill="red",alpha=0.2)+
  geom_ribbon(aes(time, ymin=Not_Warranted_Risk_Threshold,ymax=Warranted_Risk_Threshold),fill="yellow",alpha=0.2)+
  geom_ribbon(aes(time, ymin=Not_Warranted_Risk_Threshold-100,ymax=Not_Warranted_Risk_Threshold),fill="green",alpha=0.2)+
  geom_polygon(data = dt.triangle,aes(x=triangle.x,y=triangle.y,group=group))+
  annotate(geom="label",x = 50, y = mean(c(100,mean(Warranted_Risk_Threshold))),label="warranted",fill="red")+
  annotate(geom="label",x = 50, y = mean(c(Warranted_Risk_Threshold,Not_Warranted_Risk_Threshold)),label="dependent on case specific risk tolerance",fill="yellow")+
  annotate(geom="label",x = 50, y = mean(c(0,mean(Not_Warranted_Risk_Threshold))),label="not warranted",fill="green")+
  geom_vline(xintercept=Time_Threshold)+
  labs(x='Time',y='Cumulative Probability of Extinction') +
  scale_x_continuous(labels=c("Present","Future"),breaks=c(0,100),expand=c(0,0))+
  scale_y_continuous(breaks=seq.int(0,100,10),expand=c(0,0))+
  coord_cartesian(xlim =c(0,100), ylim = c(0,100))+
  theme_bw()+
  theme(axis.text.x=element_text(hjust=c(0,1),vjust=0.2))+
  theme(text = element_text(size = 20))



# Sandbox
uncertainty<-90
cP_Warranted<-seq.int(0,100,10)
cP_Species_Risk_Curves<-seq.int(0,100,10)
Upper_Plausible<-cP_Species_Risk_Curves+uncertainty
Lower_Plausible<-cP_Species_Risk_Curves-uncertainty
Warranted_Risk_Threshold<-rep(75,11)
Not_Warranted_Risk_Threshold<-rep(25,11)
data_Combined<-tibble(time,cP_Warranted,cP_Species_Risk_Curves,Upper_Plausible,Lower_Plausible,Warranted_Risk_Threshold,Not_Warranted_Risk_Threshold)
t0<-data_Combined %>% 
  filter(time==0) %>% 
  mutate(polygon="Upper Plausible")
y100<-data_Combined %>% 
  filter(Upper_Plausible==100) %>% 
  mutate(polygon="Upper Plausible")
t100<-data_Combined %>% 
  filter(time==100) %>% 
  mutate(polygon="Lower Plausible")
y0<-data_Combined %>% 
  filter(Lower_Plausible==0) %>% 
  mutate(polygon="Lower Plausible")
data_Polygons<-rows_append(t0,y100) %>% 
  rows_append(t100) %>% 
  rows_append(y0) 
# %>% add_row(0)

ggplot(data_Combined)+
  geom_polygon(data=data_Polygons,aes(x=time,y=cP_Species_Risk_Curves,group=polygon))
  geom_ribbon(aes(time,ymin=cP_Species_Risk_Curves-100,ymax=cP_Species_Risk_Curves+100),alpha=0.25,fill="black")+
  geom_ribbon(aes(time,ymin=cP_Species_Risk_Curves-90,ymax=cP_Species_Risk_Curves+90),alpha=0.25,fill="white",color="black")+
  geom_ribbon(aes(time,ymin=cP_Species_Risk_Curves-75,ymax=cP_Species_Risk_Curves+75),alpha=0.1,fill="purple")+
  geom_ribbon(aes(time,ymin=cP_Species_Risk_Curves-50,ymax=cP_Species_Risk_Curves+50),alpha=0.1,fill="purple")+
  geom_ribbon(aes(time,ymin=cP_Species_Risk_Curves-25,ymax=cP_Species_Risk_Curves+25),alpha=0.1,fill="purple")+
  geom_line(aes(time,Warranted_Risk_Threshold),color="red")+
  geom_line(aes(time,Not_Warranted_Risk_Threshold),color="green")+
  geom_ribbon(aes(time, ymin=Warranted_Risk_Threshold,ymax=Warranted_Risk_Threshold+100),fill="red",alpha=0.05)+
  geom_ribbon(aes(time, ymin=Not_Warranted_Risk_Threshold,ymax=Warranted_Risk_Threshold),fill="yellow",alpha=0.05)+
  geom_ribbon(aes(time, ymin=Not_Warranted_Risk_Threshold-100,ymax=Not_Warranted_Risk_Threshold),fill="green",alpha=0.05)+
  annotate(geom="label",x = 50, y = mean(c(100,mean(Warranted_Risk_Threshold))),label="warranted",fill="red")+
  annotate(geom="label",x = 50, y = mean(c(Warranted_Risk_Threshold,Not_Warranted_Risk_Threshold)),label="dependent on case specific risk tolerance",fill="yellow")+
  annotate(geom="label",x = 50, y = mean(c(0,mean(Not_Warranted_Risk_Threshold))),label="not warranted",fill="green")+
  labs(x='Time',y='Cumulative Probability of Extinction') +
  scale_x_continuous(labels=c("Present","Future"),breaks=c(0,100),expand=c(0,0))+
  scale_y_continuous(breaks=seq.int(0,100,10),expand=c(0,0))+
  coord_cartesian(xlim =c(0,100), ylim = c(0,100))+
  theme_bw()+
  theme(axis.text.x=element_text(hjust=c(0,1),vjust=0.2))+
  theme(text = element_text(size = 20))
  
  