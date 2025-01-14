#This script generates Figure S2 from the manuscript 
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

#Read in the data file
Elimination_timings = read.table("Elimination_timings_supplement.txt", header=TRUE)

#Do some modifications etc to ensure everything works and has good labels 
Elimination_timings$Vaccination[Elimination_timings$Vaccination=="Average"] = "Average vaccination coverage"
Elimination_timings$Vaccination[Elimination_timings$Vaccination=="Correlated"] = "Correlated vaccination coverage"

Elimination_timings$Population[Elimination_timings$Population == "Population_level"]="Population level"
Elimination_timings$Population[Elimination_timings$Population == "never_attenders"]="Never attenders"
Elimination_timings$Population[Elimination_timings$Population == "1-yearly"]="Annual"

Elimination_timings$Population = factor(Elimination_timings$Population, levels = c("Annual", "1.5-yearly",
                                                                                   "2-yearly","3-yearly","4-yearly","5-yearly",
                                                                                   "Never attenders"))


Elimination_timings$Population = factor(Elimination_timings$Population, levels = c( "Never attenders","5-yearly","4-yearly",
                                                                                    "3-yearly","2-yearly","1.5-yearly","Annual"
                                                                                   ))

#Needed in order to place those that never eliminate or already eliminated are in the correct position in the graph. 
#In the datafile, these have values of -40 (already eliminated) and 40 (never eliminated)
Elimination_timings$Timing[Elimination_timings$Timing==-40] = -41.25
Elimination_timings$Timing[Elimination_timings$Timing==40] = 52.5



strata_list = c("Annual screening",
                "1.5-yearly screening",
                "2-yearly screening",
                "3-yearly screening",
                "4-yearly screening",
                "5-yearly screening",
                "Never attenders"
                )

Elimination_timings$labels_new = rep(strata_list, times=3)


Elimination_timings$labels_new = factor(Elimination_timings$labels_new, levels = c(
  "Annual screening",
  "1.5-yearly screening",
  "2-yearly screening",
  "3-yearly screening",
  "4-yearly screening",
  "5-yearly screening",
  "Never attenders")
                                                                                   
                                                                                   )



Elimination_timings$Vaccination=factor(Elimination_timings$Vaccination, levels=c("Average vaccination coverage","Correlated vaccination coverage"))


color_average = RColorBrewer::brewer.pal(4, "Oranges")[2]
color_correlated = RColorBrewer::brewer.pal(4, "Oranges")[4]



colors_graph=c("Average vaccination coverage" = color_average,"Correlated vaccination coverage" = color_correlated)


ylabels = c("Never attenders","5-yearly screening","4-yearly screening",
               "3-yearly screening","2-yearly screening","1.5-yearly screening","Annual screening")

#script to make the graph

correlated_v_uncorrelated_herdimmunity = ggplot(data=Elimination_timings, aes(x=Timing,y=labels_new, color=Vaccination))+
  geom_point(shape=19,size=3,position=position_dodge(width=0.2))+
  geom_vline(xintercept = -30, linetype="dashed")+
  geom_vline(xintercept = 40, linetype="dashed")+
  geom_vline(xintercept = 0)+
  labs(x="\nDifference compared with national (weighted average)\ncervical cancer elimination year without correlated behavior",y="Population strata\n")+
  scale_x_continuous(breaks = c(-41.25,seq(from=-30,to=40,by=10),52.5),limits = c(-47.5,57.5), labels = c("Already\neliminated",seq(from=-30,to=40,by=10),"Never\neliminated"))+
  scale_y_discrete(labels = ylabels,
                   breaks = ylabels)+
  facet_wrap(~Model, nrow = 1)+
  scale_color_manual(values = colors_graph)+
  theme_bw(  )+
  guides(color = guide_legend(override.aes = list(size=3, stroke = 1)))+
  theme(axis.title.x=element_text(face="bold",size=9),
        axis.title.y=element_text(face="bold",size=9),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"),
        axis.ticks = element_line(color="black"),
        axis.text = element_text(color="black",size=8),
        axis.text.y = ggtext::element_markdown(),
        strip.text = element_text(size=9),
        legend.title=element_blank(),
        legend.background = element_blank(),
        legend.box.background = element_rect(color="black"),
        legend.key = element_rect(fill=NA),
        legend.position = "top",
        legend.text = element_text(size=8),
        plot.margin = margin(0.4,0.4,0.4,0.4,"cm"),
        plot.background = element_rect(
          fill = "white",
          colour = "white",
          size = 1),)

#save the graph
ggsave(file="Elimination timing average versus correlated vaccination V4.tiff",correlated_v_uncorrelated_herdimmunity, width=9.5, height = 6)

