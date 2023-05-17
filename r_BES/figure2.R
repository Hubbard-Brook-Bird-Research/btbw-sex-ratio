# BES paper Kaiser & Grabenstein et al.
# figure 2
#
library(ggplot2)
library(forestmodel)

# figure 2 ---------------------------------------------------------------------

# 2a: Effect sizes of model: food supplementation on sex ratio

#forest plot for treatment model
conf.int.treatment <-read.csv("data_BES/CI_model_sex_ratio_treatment_BES.csv")

conf.int.treatment$Model.Term <- factor(conf.int.treatment$Model.Term, 
                                       levels = c("Plot elevation (low)",
                                                  "Treatment (food-supplemented)",
                                                  "Female scaled mass : Treatment", 
                                                  "Female age class (yearling)",
                                                  "Female scaled mass"))

conf.int.treatment$Model.Term

fp1<- ggplot(data=conf.int.treatment, aes(x=Model.Term, y=Beta, ymin=LowerLimit, ymax=UpperLimit)) +
  geom_pointrange() + 
  geom_hline(yintercept=0, lty=2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Model Term") + ylab("Beta (95% CI)") +
  ylim(-2,2) +
  theme_bw() +  # use a white background
  theme(axis.text.x = element_text(size=12,color="black"),
        axis.text.y = element_text(size=12,color="black"),
        axis.title.x=element_text(size=14),
        axis.title.y=element_text(size=14),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour =  "black"))

print(fp1)

fp2 <- fp1 + annotate(geom = "text", x = Inf, y = -2, label = "(a)", size = 5, vjust = 2)
fp2


# 2b: Effect sizes of model: food supplementation on nestling mass

# forest plot for scaled nestling mass
conf.int.nestling <- read.csv("data_BES/CI_model_nestl_mass_treatment_BES.csv")
conf.int.nestling$Model.Term
conf.int.nestling$Model.Term <- factor(conf.int.nestling$Model.Term, 
                                       levels = c("Treatment : Plot elevation", 
                                                  "Sex : Treatment",
                                                  "Plot elevation (low)", 
                                                  "Sex (male)",
                                                  "Sex : Plot elevation", 
                                                  "Treatment (food-supplemented)"))  
                                                 

conf.int.nestling$Model.Term

fp3<- ggplot(data = conf.int.nestling, aes(x = Model.Term, y = Beta, 
                                           ymin = LowerLimit, ymax = UpperLimit)) +
  geom_pointrange() + 
  geom_hline(yintercept = 0, lty = 2) +  # add a dotted line at x=1 after flip
  coord_flip() +  # flip coordinates (puts labels on y axis)
  xlab("Model Term") + ylab("Beta (95% CI)") +
  ylim(-2,2) +
  theme_bw() +
  theme(axis.text.x = element_text(size=12,color="black"),
        axis.text.y = element_text(size=12,color="black"),
        axis.title.x = element_text(size=14),
        axis.title.y = element_text(size=14),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour =  "black")) # use a white background
print(fp2)

fp4 <- fp3 + annotate(geom = "text", x = Inf, y = -2, label = "(b)", size = 5, vjust = 2)
fp4