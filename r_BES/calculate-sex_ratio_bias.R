# Test of sex ratio bias (population-wide, high elevation, low elevation)
# BES paper Kaiser and Grabenstein et al.


# Sample size ------------------------------------------------------------------

# each row is a nest
sexnest <- read.csv("data_BES/nest_sex_ratio_BES.csv")

# remove incomplete broods
sexnest = sexnest[sexnest$incomplete!="Y",]

# sample sizes of broods - results: High = 63, Low = 16
table(sexnest$elevation)


# Population-wide offspring sex ratio bias -------------------------------------

# each row is a nestling (1 = M, 0 = F)
nestling <- read.csv("data_BES/nestling_sex_ratio_BES.csv")
nestling$nest_id = as.factor(nestling$nest_id)

# remove incomplete broods
nestling = nestling[nestling$incomplete!="Y",]

# test of sex ratio bias (Neuhäuser 2004)
sex.ratio.test <- function(dataset) {
   #number males per nest
   m.by.nest = data.frame(t(table(dataset$sex, by=dataset$nest_id)))
   m.by.nest = m.by.nest[m.by.nest$Var2==1,c(1,3)]
   names(m.by.nest)[2] = 'nMales'
   dataset$nest_id = as.factor(as.character(dataset$nest_id))
   temp = data.frame(t(table(dataset$nest_id)))[,c(2,3)]
   names(temp)[2] = 'nNestl'
   names(temp)[1] = 'by'
  
   nest.data = merge(m.by.nest, temp, by = 'by')
   nrow(nest.data)
   names(nest.data)[1] = 'nest_id'
   #numbers
   phat.i = nest.data$nMales / nest.data$nNestl
   wts1 = nest.data$nNestl / sum(nest.data$nNestl)
  
   phat.ui = wts1*phat.i
  
   bigN = nrow(nest.data)
   bigM = nrow(dataset)
   MSC = sum(nest.data$nNestl * (phat.i - phat.ui)^2 / (bigM - 1))
   MSE = sum(nest.data$nMales * (1 - phat.i)) / (bigM - bigN)
   mA = (bigM - sum(nest.data$nNestl^2) / bigM) / (bigN - 1)
   rho.hat = (MSC - MSE) / (MSC + (mA - 1) * MSE)
  
   wts = nest.data$nNestl / (1 + (nest.data$nNestl - 1) * rho.hat)
   sum.wts = sum(wts) #this is an adjusted sample size
   wts = wts / sum.wts
  
   phat.w = sum(wts * phat.i)
   var.phat.w = phat.w * (1 - phat.w) / sum.wts
   (zstat = (phat.w - 0.5) / sqrt(var.phat.w))
   list(phat = phat.w, varhat = var.phat.w, z = zstat, 
        p = 2 * (1 - pnorm(abs(zstat))))
}

# Do brood sex ratios differ from parity? YES
# population-wide results: 
# phat = 0.5748716, varhat = 0.00113027, z = 2.227029, p = 0.02594535
sex.ratio.test(nestling)


# Data prep: High and low elevation comparison ---------------------------------

# low
lo.stats = sex.ratio.test(nestling[nestling$elevation=='Low',])

# High
hi.stats = sex.ratio.test(nestling[nestling$elevation=='High',])

# compare low and high - result: 0.2766991
# assuming sex ratios from low and high are independent, i.e., 0 covariance
z.hiVlo = (hi.stats$phat - lo.stats$phat) / sqrt(lo.stats$var + hi.stats$var)
2*(1 - pnorm(abs(z.hiVlo)))

# for reference, this is a naive binomial test - result: 0.01377092
naive.phat = mean(nestling$sex)
naive.zstat = (naive.phat - 0.5) / sqrt(naive.phat * (1 - naive.phat) / nrow(nestling)) #z statistic
2*(1 - pnorm(abs(naive.zstat)))


# High elevation offspring sex ratio bias --------------------------------------

# subset to high quality
nestling <- read.csv("data_BES/nestling_sex_ratio_BES.csv")
nestling$nest_id = as.factor(nestling$nest_id)
nestling <- nestling[nestling$elevation=='High',]

# remove incomplete broods
nestling = nestling[nestling$incomplete!="Y",]

# test of sex ratio bias (Neuhäuser 2004)
sex.ratio.test <- function(dataset) {
   #number males per nest
   m.by.nest = data.frame(t(table(dataset$sex, by = dataset$nest_id)))
   m.by.nest = m.by.nest[m.by.nest$Var2==1,c(1,3)]
   names(m.by.nest)[2] = 'nMales'
   dataset$nest_id = as.factor(as.character(dataset$nest_id))
   temp = data.frame(t(table(dataset$nest_id)))[,c(2,3)]
   names(temp)[2] = 'nNestl'
   names(temp)[1] = 'by'
  
   nest.data = merge(m.by.nest, temp, by = 'by')
   nrow(nest.data)
   names(nest.data)[1] = 'nest_id'
   #numbers
   phat.i = nest.data$nMales / nest.data$nNestl
   wts1 = nest.data$nNestl / sum(nest.data$nNestl)
  
   phat.ui = wts1*phat.i
  
   bigN = nrow(nest.data)
   bigM = nrow(dataset)
   MSC = sum(nest.data$nNestl * (phat.i - phat.ui)^2 / (bigM - 1))
   MSE = sum(nest.data$nMales * (1 - phat.i)) / (bigM - bigN)
   mA = (bigM - sum(nest.data$nNestl^2) / bigM) / (bigN - 1)
   rho.hat = (MSC - MSE) / (MSC + (mA - 1) * MSE)
  
   wts = nest.data$nNestl / (1 + (nest.data$nNestl - 1) * rho.hat)
   sum.wts = sum(wts) #this is an adjusted sample size
   wts = wts / sum.wts
  
   phat.w = sum(wts * phat.i)
   var.phat.w = phat.w * (1 - phat.w) / sum.wts
   (zstat = (phat.w - 0.5) / sqrt(var.phat.w))
   list(phat = phat.w, varhat = var.phat.w, z = zstat, 
        p = 2 * (1 - pnorm(abs(zstat))))
}

# Do brood sex ratios at high elevation differ from parity? YES
# high elevation results: 
# phat = 0.5912628, varhat = 0.001460813, z = 2.387793, p = 0.01694989
sex.ratio.test(nestling)


# Low elevation offspring sex ratio bias ----------------------------------

# subset to low quality
nestling <- read.csv("data_BES/nestling_sex_ratio_BES.csv")
nestling$nest_id = as.factor(nestling$nest_id)
nestling <- nestling[nestling$elevation=='Low',]

# remove incomplete broods
nestling = nestling[nestling$incomplete!="Y",]

# test of sex ratio bias (Neuhäuser 2004)
sex.ratio.test <- function(dataset) {
   #number males per nest
   m.by.nest = data.frame(t(table(dataset$sex, by = dataset$nest_id)))
   m.by.nest = m.by.nest[m.by.nest$Var2==1,c(1,3)]
   names(m.by.nest)[2] = 'nMales'
   dataset$nest_id = as.factor(as.character(dataset$nest_id))
   temp = data.frame(t(table(dataset$nest_id)))[,c(2,3)]
   names(temp)[2] = 'nNestl'
   names(temp)[1] = 'by'
  
   nest.data = merge(m.by.nest, temp, by = 'by')
   nrow(nest.data)
   names(nest.data)[1] = 'nest_id'
   #numbers
   phat.i = nest.data$nMales / nest.data$nNestl
   wts1 = nest.data$nNestl / sum(nest.data$nNestl)
  
   phat.ui = wts1*phat.i
  
   bigN = nrow(nest.data)
   bigM = nrow(dataset)
   MSC = sum(nest.data$nNestl * (phat.i - phat.ui)^2 / (bigM - 1))
   MSE = sum(nest.data$nMales * (1 - phat.i)) / (bigM - bigN)
   mA = (bigM - sum(nest.data$nNestl^2) / bigM) / (bigN - 1)
   rho.hat = (MSC - MSE) / (MSC + (mA - 1) * MSE)
  
   wts = nest.data$nNestl / (1 + (nest.data$nNestl - 1) * rho.hat)
   sum.wts = sum(wts) #this is an adjusted sample size
   wts = wts / sum.wts
  
   phat.w = sum(wts * phat.i)
   var.phat.w = phat.w * (1 - phat.w) / sum.wts
   (zstat = (phat.w - 0.5) / sqrt(var.phat.w))
   list(phat = phat.w, varhat = var.phat.w, z = zstat, 
        p = 2 * (1 - pnorm(abs(zstat))))
}

# Do brood sex ratios at low elevation differ from parity? NO
# low elevation results: 
# phat = 0.5086236, varhat = 0.004310873, z = 0.1313432, p = 0.8955038
sex.ratio.test(nestling)