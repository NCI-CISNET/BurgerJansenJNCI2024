#This script generates Figures 2 and S3 from the manuscript 
#"Disparities in cervical cancer elimination time frames in the United States: a comparative modeling study"
#By Emily A. Burger et al.


setwd("...")

#Required packages
library("data.table")
library("tidyr")
library("ggplot2")
library("cowplot")
library("grid")
library("gridExtra")
library("ggtext")

#Read in the data file. 
Elimination_timings = read.table("Elimination_timings_main.txt", header=TRUE)

#scripts below change texts to ensure proper labels, and transforms character vectors into factors,
#so that labeling occurs in the right order
Elimination_timings$Herd_immunity[Elimination_timings$Herd_immunity=="Yes"] = "With herd immunity"
Elimination_timings$Herd_immunity[Elimination_timings$Herd_immunity=="No"] = "Without herd immunity"

Elimination_timings$Herd_immunity=factor(Elimination_timings$Herd_immunity, levels=c("Without herd immunity","With herd immunity"))

Elimination_timings$Population[Elimination_timings$Population == "Population_level"]="Population level"
Elimination_timings$Population[Elimination_timings$Population == "never_attenders"]="Never attenders"

Elimination_timings$Population = factor(Elimination_timings$Population, levels = c("Population level", "Annual", "1.5-yearly",
                                                                                   "2-yearly","3-yearly","4-yearly","5-yearly",
                                                                                   "Never attenders"))


Elimination_timings$Population = factor(Elimination_timings$Population, levels = c( "Never attenders","5-yearly","4-yearly",
                                                                                    "3-yearly","2-yearly","1.5-yearly","Annual",
                                                                                    "Population level"))

#Needed in order to place those that never eliminate or already eliminated are in the correct position in the graph. 
#In the datafile, these have values of -40 (already eliminated) and 40 (never eliminated)

#For the "already eliminated" strata:
Elimination_timings$Timing[Elimination_timings$Timing==-40] = -41.25

#For the "never eliminating" strata:
Elimination_timings$Timing[Elimination_timings$Timing==40] = 52.5



#Create the correct y-axis labels.
strata_list = c(" ",
                "National (weighted average)<br>with correlated behavior",
                "92.8% vaccination coverage<br>Annual screening",
                "86.2% vaccination coverage<br>1.5-yearly screening",
                "82.0% vaccination coverage<br>2-yearly screening",
                "76.1% vaccination coverage<br>3-yearly screening",
                "68.4% vaccination coverage<br>4-yearly screening",
                "64.6% vaccination coverage<br>5-yearly screening",
                "58.0% vaccination coverage<br>Never attenders"
                )

Elimination_timings$labels_new = rep(strata_list, times=6)





Elimination_timings$labels_new = factor(Elimination_timings$labels_new, levels = c(
  "National (weighted average)<br>with correlated behavior",
  " ",
  "58.0% vaccination coverage<br>Never attenders",
  "64.6% vaccination coverage<br>5-yearly screening",
  "68.4% vaccination coverage<br>4-yearly screening",
  "76.1% vaccination coverage<br>3-yearly screening",
  "82.0% vaccination coverage<br>2-yearly screening",
  "86.2% vaccination coverage<br>1.5-yearly screening",
  "92.8% vaccination coverage<br>Annual screening"
  
  
))



#Create the colors and correct legend labels for the graph 
color_DC = RColorBrewer::brewer.pal(4, "Greens")[4]
color_Harvard = RColorBrewer::brewer.pal(4, "Reds")[4]
color_Erasmus = RColorBrewer::brewer.pal(4, "Blues")[4]

colors_graph=c("Policy1-Cervix" = color_DC,"Harvard" = color_Harvard, "STDSIM-MISCAN" = color_Erasmus)


#Create the correct y-axis labels
ylabels = c("Never attenders","5-yearly","4-yearly",
               "3-yearly","2-yearly","1.5-yearly","Annual",
               "Population level")

ylabels_2 = c("92.8% vaccination coverage<br>Annual screening",
            "86.2% vaccination coverage<br>1.5-yearly screening",
            "82.0% vaccination coverage<br>2-yearly screening",
            "76.1% vaccination coverage<br>3-yearly screening",
            "68.4% vaccination coverage<br>4-yearly screening",
            "64.6% vaccination coverage<br>5-yearly screening",
            "58.0% vaccination coverage<br>Never attenders",
            "National (weighted average)<br>with correlated behavior")

###### MAKE FIGURE 2#####

#only select those the scenarios with herd immunity
Elimination_timings_herdimmunity = Elimination_timings[Elimination_timings$Herd_immunity=="With herd immunity",]
  

# Generate the graph  
Timings_graph_herdimmunity = ggplot(data=Elimination_timings_herdimmunity, aes(x=Timing,y=labels_new, color=Model))+
  geom_point(shape=19,size=3,position=position_dodge(width=0.2))+
  geom_vline(xintercept = -30, linetype="dashed")+
  geom_vline(xintercept = 40, linetype="dashed")+
  geom_vline(xintercept = 0)+
  labs(x="\nDifference compared to national (weighted average)\ncervical cancer elimination year without correlated behavior",y="Population strata\n")+
  scale_x_continuous(breaks = c(-41.25,seq(from=-30,to=40,by=10),52.5),limits = c(-47.5,57.5), labels = c("Already\neliminated",seq(from=-30,to=40,by=10),"Never\neliminated"))+
  scale_y_discrete(labels = ylabels_2,
                   breaks = ylabels_2)+
  scale_color_manual(values = colors_graph)+
  theme_bw(  )+
  guides(color = guide_legend(override.aes = list(size=3, stroke = 1)))+
  theme(axis.title.x=element_text(face="bold",size=10),
        axis.title.y=element_text(face="bold",size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        axis.ticks = element_line(color="black"),
        axis.text = element_text(color="black",size=9),
        axis.text.y = ggtext::element_markdown(),
        strip.text = element_text(size=10),
        legend.title=element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_rect(color="black"),
        legend.key = element_rect(fill=NA),
        legend.position = "top",
        legend.text = element_text(size=9),
        plot.margin = margin(0.4,0.4,0.4,0.4,"cm"),
        plot.background = element_rect(
          fill = "white",
          colour = "white"),)


#Save the graph
ggsave(file="Elimination timing Herd immunity.tiff",Timings_graph_herdimmunity, width=9.5, height = 6)


###### MAKE FIGURE S3#####

#only select those the scenarios without herd immunity
Elimination_timings_herdimmunity = Elimination_timings[Elimination_timings$Herd_immunity!="With herd immunity",]

#Needed in order to place those that never eliminate or already eliminated are in the correct position in the graph. 
#the no herd immunity graph has a wider x-axis, so values need to be adjusted to accommodate
Elimination_timings_herdimmunity$Timing[Elimination_timings_herdimmunity$Timing==-47.5] = -77.5
Elimination_timings_herdimmunity$Timing[Elimination_timings_herdimmunity$Timing==52.5] = 47.5

#Generate the graph
Timings_graph_noherdimmunity = ggplot(data=Elimination_timings_herdimmunity, aes(x=Timing,y=labels_new, color=Model))+
  geom_point(shape=19,size=3,position=position_dodge(width=0.2))+
  geom_vline(xintercept = -60, linetype="dashed")+
  geom_vline(xintercept = 30, linetype="dashed")+
  geom_vline(xintercept = 0)+
  labs(x="\nDifference compared to national (weighted average)\ncervical cancer elimination year without correlated behavior",y="Population strata\n")+
  scale_x_continuous(breaks = c(-77.5,seq(from=-60,to=30,by=10),47.5),limits = c(-90,60), labels = c("Already\neliminated",seq(from=-60,to=30,by=10),"Never\neliminated"))+
  scale_y_discrete(labels = ylabels_2,
                   breaks = ylabels_2)+
  scale_color_manual(values = colors_graph)+
  theme_bw(  )+
  guides(color = guide_legend(override.aes = list(size=3, stroke = 1)))+
  theme(axis.title.x=element_text(face="bold",size=10),
        axis.title.y=element_text(face="bold",size=10),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        axis.ticks = element_line(color="black"),
        axis.text = element_text(color="black",size=9),
        axis.text.y = ggtext::element_markdown(),
        strip.text = element_text(size=10),
        legend.title=element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_rect(color="black"),
        legend.key = element_rect(fill=NA),
        legend.position = "top",
        legend.text = element_text(size=9),
        plot.margin = margin(0.4,0.4,0.4,0.4,"cm"),
        plot.background = element_rect(
          fill = "white",
          colour = "white",
          size = 1),)


#Save the graph
ggsave(file="Elimination timing No Herd immunity.tiff",Timings_graph_noherdimmunity, width=9.5, height = 6)