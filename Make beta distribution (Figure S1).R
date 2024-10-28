#This script generates Figure S1 from the manuscript 
#"Disparities in cervical cancer elimination time frames in the United States: a comparative modeling study"
#By Emily A. Burger et al.

#Set your working directory
setwd("...")

#Load the required packages
library("ggplot2")


#These are the parameters of the beta distrubition
alpha = 11.265
beta = 3.725

#Generate a vector from 0 to 1 with 0.0001 as incremental steps
probabilities = seq(0,1, by=0.0001)


#Calculate the inverse cumulative density function of the beta distribution for all the vector of probabilities from 0 to 1,
#using the beta distribution parameters defined above
beta_values = qbeta(probabilities, shape1 = alpha, shape2 = beta)

#Generate the graph
beta_graph = ggplot()+
  geom_line(aes(x=probabilities, y=beta_values), color="blue")+

  #add vertical lines for each stratum mid-point
  geom_vline(xintercept = 0.072, linetype = "dashed")+
  geom_vline(xintercept = 0.169, linetype = "dashed")+
  geom_vline(xintercept = 0.254, linetype = "dashed")+
  geom_vline(xintercept = 0.4965, linetype = "dashed")+
  geom_vline(xintercept = 0.709, linetype = "dashed")+
  geom_vline(xintercept = 0.8455, linetype = "dashed")+
  geom_vline(xintercept = 0.976, linetype = "dashed")+
  scale_x_continuous(breaks = c(0,0.072,0.169,0.254,0.4965,0.709,0.8455,0.976,1.0), labels = c(0,"\nNever\nattenders",
                                                                                                      "\n5-yearly\nscreening",
                                                                                                      "\n4-yearly\nscreening",
                                                                                                      "\n3-yearly\nscreening",
                                                                                                      "\n2-yearly\nscreening",
                                                                                                      "\n1.5-yearly\nscreening",
                                                                                                      "\nAnnual\nscreening",
                                                                                                      1.0))+
  scale_y_continuous(breaks = seq(0,1,by=0.2), limits = c(0,1))+
  labs(x="Cumulative porportion of the population\n(by screen stratum midpoints)",y="Proportion vaccinated")+
  theme_bw(  )+
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
#        legend.title=element_blank(),
#        legend.background = element_blank(),
#        legend.box.background = element_rect(color="black"),
#        legend.key = element_rect(fill=NA),
#        legend.position = "top",
#        legend.text = element_text(size=9),
        plot.margin = margin(0.4,0.4,0.4,0.4,"cm"),
        plot.background = element_rect(
          fill = "white",
          colour = "white",
          size = 1),)
  

#Save the graph to a file
ggsave(file="Beta distirbution graph.tiff",beta_graph, width=10.5, height = 6)


  
  
  

