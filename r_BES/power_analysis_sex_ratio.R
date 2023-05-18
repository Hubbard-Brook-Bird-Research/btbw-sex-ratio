#power analysis for BTBW Sex ratio
#do we have enough power in our sample size to detect an effect of food supplementation on sex ratio
#the probability of detecting a particular effect (of a particular strength), at a particular alpha level (usually 0.05) if this effect is present.

#alpha 0.05 <--type I error 
#power 0.8 <--type II error 
#sample size, need std dev of response variable 
#difference to detect  

install.packages("pwr")
library(pwr)
library(lme4)
library(lmerTest)

# Effects of food supplementation on sex ratio ---------------------------------

# each row is a nest
nest_sex_ratio <- read.csv("data_BES/nest_sex_ratio_BES.csv")

# remove incomplete broods
nest_sex_ratio = nest_sex_ratio[nest_sex_ratio$incomplete!="Y",]

# standardize female scaled mass
nest_sex_ratio$Z_female_scaled_mass = scale(nest_sex_ratio$female_scaled_mass)

# Effects of food supplementation and maternal condition on sex ratio ----------
# GLM fixed effects: treatment, plot elevation, female scaled mass, and female age.
# interactions: 1) female scaled mass and treatment.
# remove treatment: elevation interactions not enough food-supplemented nests
# at low elevation.
# standardize female scaled mass (µ = 0, SD = 1). 
# including female scaled mass and female scaled mass: treatment reduced 
# observations from 80 to 69; no factor is significant.
# result: sex ratio did not differ by treatment or other factors
sex_trt <-glm(cbind(males, females) ~ treatment + elevation + Z_female_scaled_mass 
              + female_age + Z_female_scaled_mass:treatment,
              data = nest_sex_ratio, family = "binomial")
summary(sex_trt)

sex_trt <-glm(cbind(males, females) ~ treatment + elevation + female_age,data = nest_sex_ratio, family = "binomial")
summary(sex_trt)

# simplified model
sex_trt <-glm(cbind(males, females) ~ treatment,
              data = nest_sex_ratio, family = "binomial")
summary(sex_trt)

# power analysis for glm 
# determine if data is sufficient to find a weak effect when comparing 2 groups
# with 64 
# u is degrees of freedom of numerator, v is degrees of freedom of denominator
# f2 is small effect size (0.02) from Cohen (1988) did not use below
# f2 is small effect size (0.17) from glm for treatment effect
pwr.f2.test(u = 1, v = 78, f2 = 0.17, sig.level = 0.05)
