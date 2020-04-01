#comparing non-senescent tissue (NST) to senescent tissue (ST) relative water content (RWC)
#at six sampling points. 

library(ggplot2)
rwc_aves <- read.csv("rwc_aves.txt", sep="")
rwc_aves

#basic bar plot
ggplot(data=rwc_aves, 
       mapping = aes(x=samplingpoint, y=avgRWC))+
  geom_bar(stat="identity")

#splitting each time point based on the tissue type
ggplot(data=rwc_aves, 
       mapping = aes(x=samplingpoint, y=avgRWC, colour=Tissue))+
  geom_bar(stat="identity", 
           position= "dodge" #allows the bars to be side by side
           )
ggplot(data=rwc_aves, 
       mapping = aes(x=samplingpoint, y=avgRWC, colour=Tissue, fill=Tissue))+
  geom_bar(stat="identity", 
           position= "dodge" 
  )

#stacking instead of dodging
ggplot(data=rwc_aves, 
       mapping = aes(x=samplingpoint, y=avgRWC, colour=Tissue, fill=Tissue))+
  geom_bar(stat="identity", 
           position= "stack" 
  )
#adding sd error bars
ggplot(data=rwc_aves, mapping = aes(x=samplingpoint, y=avgRWC, colour=Tissue, fill=Tissue))+
  geom_bar(stat="identity", 
           position= "dodge"
  )+
  geom_errorbar(mapping=aes(ymin=avgRWC-sdRWC, ymax=avgRWC+sdRWC), #using sdRWC column from df
                #position = position_dodge(0.9), #uncomment to fix - note the similarity to position = "dodge" above
                width=.2)

#changing the axis labels 
ggplot(data=rwc_aves, mapping = aes(x=samplingpoint, y=avgRWC, colour=Tissue, fill=Tissue))+
  geom_bar(stat="identity", 
           position= "dodge",
           alpha=0.5 #increased the opacity by 50% so that we can see sd bars
  )+
  geom_errorbar(mapping=aes(ymin=avgRWC-sdRWC, ymax=avgRWC+sdRWC), #using sdRWC column from df
                position = position_dodge(0.9), 
                width=.2)+
  labs(x= "Sampling Point",
       y= "Average % RWC")

#changing the x and y axis to be more legible
ggplot(data=rwc_aves, mapping = aes(x=samplingpoint, y=avgRWC, colour=Tissue, fill=Tissue))+
  geom_bar(stat="identity", 
           position= "dodge",
           alpha=0.5
  )+
  geom_errorbar(mapping=aes(ymin=avgRWC-sdRWC, ymax=avgRWC+sdRWC), #using sdRWC column from df
                position = position_dodge(0.9), 
                width=.2)+
  labs(x= "Sampling Point",
       y= "Average % RWC")+
  scale_x_discrete(labels=c("1", "2", "3", "4", "5", "6"))+
  scale_y_continuous(breaks=seq(0,100,10))

#changing colourscheme
ggplot(data=rwc_aves, mapping = aes(x=samplingpoint, y=avgRWC, colour=Tissue, fill=Tissue))+
  geom_bar(stat="identity", 
           position= "dodge",
           alpha=0.5
  )+
  geom_errorbar(mapping=aes(ymin=avgRWC-sdRWC, ymax=avgRWC+sdRWC), #using sdRWC column from df
                position = position_dodge(0.9), 
                width=.2)+
  labs(x= "Sampling Point",
       y= "Average % RWC")+
  scale_x_discrete(labels=c("1", "2", "3", "4", "5", "6"))+
  scale_y_continuous(breaks=seq(0,100,10))+
  scale_color_manual(values= c("pink", "green"))+
  scale_fill_manual(values= c("pink", "green"))

#changing colours with hex codes https://htmlcolorcodes.com/
ggplot(data=rwc_aves, mapping = aes(x=samplingpoint, y=avgRWC, colour=Tissue, fill=Tissue))+
  geom_bar(stat="identity", 
           position= "dodge",
           alpha=0.5
  )+
  geom_errorbar(mapping=aes(ymin=avgRWC-sdRWC, ymax=avgRWC+sdRWC), #using sdRWC column from df
                position = position_dodge(0.9), 
                width=.2)+
  labs(x= "Sampling Point",
       y= "Average % RWC")+
  scale_x_discrete(labels=c("1", "2", "3", "4", "5", "6"))+
  scale_y_continuous(breaks=seq(0,100,10))+
  scale_color_manual(values= c("#FFC300", "#900C3F"))+
  scale_fill_manual(values= c("#FFC300", "#900C3F"))
  
#adding theme changes to make it cute 
ggplot(data=rwc_aves, mapping = aes(x=samplingpoint, y=avgRWC, colour=Tissue, fill=Tissue))+
  geom_bar(stat="identity", 
           position= "dodge",
           alpha=0.5
  )+
  geom_errorbar(mapping=aes(ymin=avgRWC-sdRWC, ymax=avgRWC+sdRWC), #using sdRWC column from df
                position = position_dodge(0.9), 
                width=.2)+
  labs(x= "Sampling Point",
       y= "Average % RWC")+
  scale_x_discrete(labels=c("1", "2", "3", "4", "5", "6"))+
  scale_y_continuous(breaks=seq(0,100,10))+
  scale_color_manual(values= c("pink", "green"))+
  scale_fill_manual(values= c("pink", "green"))+
  theme_minimal()#try other built in themes here like theme_bw()

#manually tweaking themes
ggplot(data=rwc_aves, mapping = aes(x=samplingpoint, y=avgRWC, colour=Tissue, fill=Tissue))+
  geom_bar(stat="identity", 
           position= "dodge", 
           alpha=0.5
  )+
  geom_errorbar(mapping=aes(ymin=avgRWC-sdRWC, ymax=avgRWC+sdRWC), #using sdRWC column from df
                position = position_dodge(0.9), 
                width=.2)+
  labs(x= "Sampling Point",
       y= "Average % RWC")+
  scale_x_discrete(labels=c("1", "2", "3", "4", "5", "6"))+
  scale_y_continuous(breaks=seq(0,100,10))+
  scale_color_manual(values= c("pink", "green"))+
  scale_fill_manual(values= c("pink", "green"))+
  theme(
    panel.background = element_blank(),
    axis.line.x = element_line(size=1),
    axis.line.y = element_line(size=1),
    axis.text = element_text(family="arial")
  )



r <- ggplot(data=rwc_aves, mapping = aes(x=samplingpoint, y=avgRWC, fill=Tissue, colour=Tissue))+
  geom_bar(stat="identity", position= position_dodge())+
  geom_errorbar(mapping=aes(ymin=avgRWC-sdRWC, ymax=avgRWC+sdRWC), 
                position = position_dodge(0.9),
                width=.2)+
  scale_y_continuous(breaks=seq(0,100,10))+
  scale_x_discrete(labels=c("1", "2", "3", "4", "5", "6"))+
  labs(
    x="Sampling Point",
    y="%RWC relative to FT NST"
  )

r
