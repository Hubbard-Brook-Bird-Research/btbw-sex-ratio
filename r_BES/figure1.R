# BES paper Kaiser & Grabenstein et al.
# figure 1
#
library(ggplot2)

# figure 1
# 1a: Caterpillar density (log mg) by Proportion of sons per brood (scatterplot)

# load data
nest_sex_ratio <- read.csv("data_BES/nest_sex_ratio_BES.csv")

# remove incomplete broods
nest_sex_ratio = nest_sex_ratio[nest_sex_ratio$incomplete!="Y",]

# remove fed nests (only controls)
nest_sex_ratio = nest_sex_ratio[nest_sex_ratio$treatment!="Fed",]

# Scatterplot
p1 <-ggplot(nest_sex_ratio, aes(x = ln_caterpillar_biomass_egg_laying, y = sex_ratio)) +
  geom_point(na.rm = TRUE, shape = 20, size = 3) +
  geom_point(na.rm = TRUE, aes(colour = elevation)) +
  scale_x_continuous(limits = c(3,8)) +
  scale_y_continuous(limits = c(0,1)) +
  xlab("\n Caterpillar density (log mg)") +
  ylab("Proportion of sons per brood \n") +
  theme(axis.text.x = element_text(size = 12,color = "black"),
        axis.text.y = element_text(size = 12,color = "black"),
        axis.title.x = element_text(size = 14),
        axis.title.y = element_text(size = 14),
        axis.line.x = element_line(colour = "black"),
        axis.line.y = element_line(colour = "black"),
        plot.background = element_rect(fill = "white"), 
        panel.grid.major = element_line(colour = NA), 
        panel.grid.minor = element_line(colour = NA),
        panel.background = element_rect(fill = "white"))

p2 <- p1 + labs(colour = "Plot elevation") + theme(legend.key = element_rect(fill='white'),           
        legend.title = element_text(size = 14),
        legend.text = element_text(size = 12))
p2

p3 <- p2 + scale_color_manual(values = c("forestgreen", "grey67")) 
p3

p4 <- p3 + annotate(geom = "text", x = 3, y = 1.00, label = "(a)", size = 5)
p4


# 1b: Proportion of sons per brood by treatment

# load data
nest_sex_ratio <- read.csv("data_BES/nest_sex_ratio_BES.csv")

# remove incomplete broods
nest_sex_ratio = nest_sex_ratio[nest_sex_ratio$incomplete!="Y",]

install.packages("beeswarm")
library(beeswarm)

cols <- c("grey67", "forestgreen")

beeswarm(sex_ratio ~ treatment, data = nest_sex_ratio, col = cols, pch = 16, 
         method = "center", ylim = c(0,1), bty="n",
         ylab="Proportion of sons per brood", xlab = "Plot elevation", 
         xaxt="n", cex=0.7)

bxplot(sex_ratio ~ treatment, data = nest_sex_ratio, add = T, probs = 0.5, 
       lwd = 2, width = 0.4, col = "black") 

axis(1, at = c(1,2), lab = c("control (n = 64)","food supplemented (n = 15)")) 

text("(b)", x = 0.5, y = 1)