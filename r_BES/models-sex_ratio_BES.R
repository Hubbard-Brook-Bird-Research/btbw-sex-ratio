# sex ratio models
# BES paper Kaiser & Grabenstein et al.
#
library(lme4)
library(lmerTest)
library(dplyr)
library(plotrix)
library(sjPlot)

# Data -------------------------------------------------------------------------

# each row is a nest
nest_sex_ratio <- read.csv("data_BES/nest_sex_ratio_BES.csv")
nest_sex_ratio

# standardize female scaled mass
nest_sex_ratio$Z_female_scaled_mass = scale(nest_sex_ratio$female_scaled_mass)

# standardize log caterpillar density during fertile period
# associate with sex ratio (i.e. when eggs are laid)
nest_sex_ratio$Z_ln_caterpillar_biomass_egg_laying = 
  scale(nest_sex_ratio$ln_caterpillar_biomass_egg_laying)


# Summary data -----------------------------------------------------------------

# summary of sampling
sum(nest_sex_ratio$nestlings) #348
sum(nest_sex_ratio$missing) #21
21/348
sum(nest_sex_ratio$males) + sum(nest_sex_ratio$females) #332
327/348

# remove incomplete broods
nest_sex_ratio = nest_sex_ratio[nest_sex_ratio$incomplete!="Y",]

# summary stats per year
# function "std.error" requires library(plotrix)
aggregate(males/(males + females) ~ year, data = nest_sex_ratio, FUN = function(x) c(mean = mean(x), se = std.error(x)))

table(nest_sex_ratio$year)

# mean +/- SE sex ratio (n) by year:
# 2007: 0.5833333 +/- 0.06114995; n = 13
# 2009: 0.5277778 +/- 0.08216777; n = 9
# 2010: 0.6388889 +/- 0.08724007; n = 12
# 2011: 0.5946970 +/- 0.05093388; n = 22
# 2012: 0.5615942 +/- 0.05424892; n = 23

# Did sex ratios differ significantly among years? NO
# result: beta = -0.004 +/- 0.069, z = -0.055, p = 0.956
# okay to pool data across years
nest_sex_ratio_year <-glm(cbind(males, females) ~ year, data = nest_sex_ratio, 
                          family="binomial")
summary(nest_sex_ratio_year)


# Brood size variation ---------------------------------------------------------

stderr <-function(x)sqrt(var(x, na.rm = TRUE) / length(na.omit(x)))

# brood size: mean +/- SE by treatment
# result: control = 3.578125 +/- 0.0796965, fed = 3.600000 +/- 0.1309307
broodsize_trt = tapply(nest_sex_ratio$nestlings, nest_sex_ratio$treatment, 
                       mean, na.rm = TRUE)
broodsize_trt
broodsize_trt_se = tapply(nest_sex_ratio$nestlings, nest_sex_ratio$treatment, 
                          stderr)
broodsize_trt_se

# brood size: mean +/- SE by plot elevation
# result: High = 3.587302 +/- 0.07379314, Low = 3.562500 +/- 0.18185961
broodsize_elev = tapply(nest_sex_ratio$nestlings, nest_sex_ratio$elevation, 
                        mean, na.rm = TRUE)
broodsize_elev
broodsize_elev_se = tapply(nest_sex_ratio$nestlings, nest_sex_ratio$elevation, 
                           stderr)
broodsize_elev_se

# brood size: mean +/- SE by age class
# result: ASY = 3.517241 +/- 0.11766080, SY = 3.620000 +/- 0.08518887
broodsize_age = tapply(nest_sex_ratio$nestlings, nest_sex_ratio$female_age, 
                       mean)
broodsize_age
broodsize_age_se = tapply(nest_sex_ratio$nestlings, nest_sex_ratio$female_age, 
                          stderr)
broodsize_age_se

# Welch's t-tests
# result: No significant differences in brood size by...
# brood size should not bias results examining fixed effects on sex ratio.

# treatments (t = -0.14271, df = 25.518, p-value = 0.8876)
t.test(nest_sex_ratio$nestlings ~ nest_sex_ratio$treatment)

# plot elevation (t = 0.12637, df = 20.214, p-value = 0.9007)
t.test(nest_sex_ratio$nestlings ~ nest_sex_ratio$elevation)

# female age class (t = -0.7074, df = 56.222, p-value = 0.4822)
t.test(nest_sex_ratio$nestlings ~ nest_sex_ratio$female_age)


# Tests of the assumptions of the Trivers-Willard hypothesis -------------------
# control broods only

# remove fed nests (only controls)
nest_sex_ratio = nest_sex_ratio[nest_sex_ratio$treatment!="Fed",]

# sample size = 64 control broods
table(nest_sex_ratio$treatment)

# assumption 1: high food availability associated with females in good condition 
# result: assumption not met (F = 3.13, df = 1, 48, p-value: 0.08341)
# downward trend (opposite prediction)
# LM (control broods only): log caterpillar density (x) by female scaled mass (y)
# not a strong test because we had limited data on female scaled mass to 
# associate with caterpillar density.
assumption1 <-lm(nest_sex_ratio$ln_caterpillar_biomass_female_capture ~ 
                   nest_sex_ratio$female_scaled_mass)
summary(assumption1)

plot(nest_sex_ratio$ln_caterpillar_biomass_female_capture, 
     nest_sex_ratio$female_scaled_mass)


# assumption 2: females in good condition and/or with access to high food 
# availability increase parental investment.
# result: assumption not met 
# previously showed that food-supplemented females provisioned their offspring 
# more than control females at food-limited low elevations.
# LM (control broods only): female scaled mass (x) by provisioning rate (y)
# result: no relationship (F = 0.06, df = 1, 33, p-value: 0.81)
# could not be tested well - few females captured during provisioning
# refer to Kaiser et al. 2014 as test of assumption instead

#assumption2 <-lm(nest_sex_ratio$f_prov_rate ~ nest_sex_ratio$female_scaled_mass)
#summary(assumption2)

#plot(nest_sex_ratio$female_scaled_mass, nest_sex_ratio$f_prov_rate)


# assumption 3: increased parental investment results in offspring in good 
# condition.
# result: assumption not met 
# LM (control broods only): female provisioning rate (x) by mean brood mass (y)
# result: no relationship (F = 0.28, df = 1, 40, p-value: 0.60)

assumption3<-lm(nest_sex_ratio$mean_nestl_mass ~ nest_sex_ratio$f_prov_rate)
summary(assumption3)

plot(nest_sex_ratio$f_prov_rate, nest_sex_ratio$mean_nestl_mass)


# assumption 4: offspring condition is correlated with their adult condition
# could not be tested


# assumption 5: parental investment affects the future reproductive performance 
# of sons and daughters differently.
# could not be tested


# Effects of food availability on sex ratio ------------------------------------
# control broods only

# Did caterpillar density during egg laying influence sex ratio? 
# GLM: fixed effects - log caterpillar density, plot elevation, female scaled 
# mass, female age class.
# interactions: 1) log caterpillar density and plot elevation 
# log caterpillar density standardized (µ = 0, SD = 1).
# did not consider maternal condition in model given lack of relationship
# between food availability and female condition (assumption 2)
# result: sex ratio differs between elevations (male-biased at high elevation)
# result: age is near significant (male-biased for older females)
sex_ratio_food <-glm(cbind(males, females) ~ Z_ln_caterpillar_biomass_egg_laying + 
                         elevation + female_age + 
                         Z_ln_caterpillar_biomass_egg_laying:elevation, 
                       data = nest_sex_ratio, family = "binomial")
sex_ratio_food
summary(sex_ratio_food)

# 95% CI for each fixed effect in GLMs as a measure of effect size.
confint(sex_ratio_food)

# Effects of parental provisioning on sex ratio --------------------------------
# control broods only

# LMM: Does provisioning rate differ based on proportion of sons in the brood?
# duplicate males and females with provisioning data in dataset
# males - result: NO 
# sex ratio: beta = -0.66 +/- 0.60, df = 5.60, t = -1.10, p = 0.32
male_prov<-lmer(m_prov_rate ~ sex_ratio + (1| male_fws), data = nest_sex_ratio)
summary(male_prov)

# females - result: NO
# sex ratio: beta 0.44 +/- 0.74, df = 39.97, t = 0.59, p = 0.56
female_prov<-lmer(f_prov_rate ~ sex_ratio + (1| female_fws), data = 
                    nest_sex_ratio)
summary(female_prov)


# Effect of food availability on nestling mass of sons and daughters -----------
# control broods only

# each row is a nestling (1 = M, 0 = F)
nestl_size <- read.csv("data_BES/nestling_sex_ratio_BES.csv")
nestl_size$nest_id = as.factor(nestl_size$nest_id)
nestl_size$sex = as.factor(nestl_size$sex)

# standardize caterpillar density during nestling period
nestl_size$Z_ln_caterpillar_biomass_nestl = 
  scale(nestl_size$ln_caterpillar_biomass_nestl)

# remove fed nests (only controls)
nestl_size = nestl_size[nestl_size$treatment!="Fed",]

# remove nestlings not 6 days old
nestl_size = nestl_size[nestl_size$nestl_age!="5",]
nestl_size = nestl_size[nestl_size$nestl_age!="7",]

# remove incomplete broods
nestl_size = nestl_size[nestl_size$incomplete!="Y",]

# Does nestling scaled mass differ by nestling sex or food availability? 
# LMM - response variable: nestling scaled mass
# fixed effects: log caterpillar density, nestling sex, plot elevation. 
# interactions: 1) nestling sex and log caterpillar density, 2) nestling sex 
# and plot elevation, and 3) log caterpillar density and plot elevation. 
# log caterpillar density was standardized (µ = 0, SD = 1). 
# results: no significant effects
nestl_mass <-lmer(nestl_scaled_mass ~ sex + Z_ln_caterpillar_biomass_nestl + 
                    elevation + sex:Z_ln_caterpillar_biomass_nestl + 
                    sex:elevation + Z_ln_caterpillar_biomass_nestl:elevation + 
                    (1|nest_id), data = nestl_size)
summary(nestl_mass)


# Do the ratio of variances in nestling scaled mass of females and males differ?
# F test
# results: NO
# issue with nestling_scaled_mass_f_test.csv
# 4 nestlings in brood, 2 males, 2 females, 1 male compared to 1 female, not both. Why?
var_scaled_mass <- read.csv("data_BES/nestling_scaled_mass_f_test_BES.csv")

var.test(var_scaled_mass$male, var_scaled_mass$female,ratio = 1)


# Effects of food supplementation on sex ratio ---------------------------------

# each row is a nest
nest_sex_ratio <- read.csv("data_BES/nest_sex_ratio_BES.csv")

# remove incomplete broods
nest_sex_ratio = nest_sex_ratio[nest_sex_ratio$incomplete!="Y",]

# standardize female scaled mass
nest_sex_ratio$Z_female_scaled_mass = scale(nest_sex_ratio$female_scaled_mass)

# sample size = 79 broods
table(nest_sex_ratio$treatment)

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

# 95% CI for each fixed effect in GLMs as a measure of effect size.
confint(sex_trt)

# Effect of food supplementation on nestling mass of sons and daughters --------

# each row is a nestling (1 = M, 0 = F)
nestl_size <- read.csv("data_BES/nestling_sex_ratio_BES.csv")
nestl_size$nest_id = as.factor(nestl_size$nest_id)
nestl_size$sex = as.factor(nestl_size$sex)

# remove nestlings not 6 days old
nestl_size = nestl_size[nestl_size$nestl_age!="5",]
nestl_size = nestl_size[nestl_size$nestl_age!="7",]

# remove incomplete broods
nestl_size = nestl_size[nestl_size$incomplete!="Y",]

# fixed effects: nestling sex, treatment, and plot elevation. 
# interactions: 1) nestling sex and treatment, 2) nestling sex and plot 
# elevation, and 3) treatment and plot elevation. 
# p-values for LMMs were estimated using lmerTest. 
# results: no significant effects
nestl_mass <-lmer(nestl_scaled_mass ~ sex + treatment + 
                    elevation + sex:treatment + 
                    sex:elevation + treatment:elevation + 
                    (1|nest_id), data = nestl_size)
summary(nestl_mass)

# 95% CI for each fixed effect in GLMs as a measure of effect size.
confint(nestl_mass)