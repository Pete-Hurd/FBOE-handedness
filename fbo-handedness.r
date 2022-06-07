## Code for analsyses in FBO handedness paper - 27 May 2022
rm(list=ls())
pub.dat <- read.csv("Handedness-FBO.csv",stringsAsFactors = FALSE)
attach(pub.dat)

### Publication version composite variables
# make bin.hand, 0=L, 1=R, all other NA
bin.hand <- hand=="R"
bin.hand <- replace(bin.hand, bin.hand=="FALSE", NA)
bin.hand <- replace(bin.hand, hand=="L", FALSE)

num.sibs <- young..bro + old..bro + young..sis + old..sis
other.sibs <- young..bro + young..sis + old..sis
old.sibs <-  old..bro + old..sis

# make a vector to contrast the two FBO cases in Kovanova's procedure
# make vector FALSE iff male has younger brother only, TRUE iff has older brother only, NA otherwise
m.khov.fbo <- sex=="M" & young..bro==0 & old..bro==1
m.khov.fbo <- replace(m.khov.fbo, m.khov.fbo=="FALSE", NA)
m.khov.fbo <- replace(m.khov.fbo, (sex=="M" & young..bro==1 & old..bro==0), FALSE)

# this one to test fecundity effect with Kovanova's procedure
# make vector FALSE iff male has no brother, TRUE iff has one younger brother, NA otherwise
m.khov.ffe <- sex=="M" & young..bro==1 & old..bro==0
m.khov.ffe <- replace(m.khov.ffe, m.khov.ffe=="FALSE", NA)
m.khov.ffe <- replace(m.khov.ffe, (sex=="M" & young..bro==0 & old..bro==0), FALSE)

# make a vector to contrast the two FBO cases in Kovanova's procedure, but for female subjects...
# make vector FALSE if female has younger brother only, TRUE if has older brother only, NA otherwise
f.khov.fbo <- sex=="F" & young..bro==0 & old..bro==1
f.khov.fbo <- replace(f.khov.fbo, f.khov.fbo=="FALSE", NA)
f.khov.fbo <- replace(f.khov.fbo, (sex=="F" & young..bro==1 & old..bro==0), FALSE)

# this one to test fecundity effect with Kovanova's procedure
# make vector true if female has no brother, false if has one older brother, NA otherwise  ##verify!!
f.khov.ffe <- sex=="F" & young..bro==1 & old..bro==0
f.khov.ffe <- replace(f.khov.ffe, f.khov.ffe=="FALSE", NA)
f.khov.ffe <- replace(f.khov.ffe, (sex=="F" & young..bro==0 & old..bro==0), FALSE)

pub.dat <- cbind(pub.dat,bin.hand,num.sibs,other.sibs,old.sibs,m.khov.fbo,m.khov.ffe,f.khov.fbo,f.khov.ffe)
rm(bin.hand,num.sibs,other.sibs,old.sibs,m.khov.fbo,m.khov.ffe,f.khov.fbo,f.khov.ffe)

###
####  TABLE 1
###
detach(pub.dat);attach(pub.dat)
f.pub.dat <- subset(pub.dat,sex=="F")
m.pub.dat <- subset(pub.dat,sex=="M")
detach(pub.dat)

#dim(pub.dat)
#[1] 2256 1210
#sum(sex=="M",na.rm=T)
#[1] 816
#sum(sex=="F",na.rm=T)
#[1] 1440

attach(m.pub.dat)
sum(hand=="L",na.rm=T);sum(hand=="R",na.rm=T);
#[1] 70
#[1] 722
mean(age[hand=="L"],na.rm=T);sd(age[hand=="L"],na.rm=T)
#[1] 19.73846
#[1] 2.922311
mean(age[hand=="R"],na.rm=T);sd(age[hand=="R"],na.rm=T)
#[1] 19.6291
#[1] 3.644872
mean(mat..age[hand=="L"],na.rm=T);sd(mat..age[hand=="L"],na.rm=T)
#[1] 29.03175
#[1] 5.358046
mean(mat..age[hand=="R"],na.rm=T);sd(mat..age[hand=="R"],na.rm=T)
#[1] 29.7076
#[1] 5.018782
mean(pat..age[hand=="L"],na.rm=T);sd(pat..age[hand=="L"],na.rm=T)
#[1] 30.69841
#[1] 5.449578
mean(pat..age[hand=="R"],na.rm=T);sd(pat..age[hand=="R"],na.rm=T)
#[1] 32.32939
#[1] 5.215411
mean(num.sibs[hand=="L"],na.rm=T);sd(num.sibs[hand=="L"],na.rm=T)
#[1] 1.7
#[1] 1.255134
mean(num.sibs[hand=="R"],na.rm=T);sd(num.sibs[hand=="R"],na.rm=T)
#[1] 1.574468
#[1] 1.152037
detach(m.pub.dat)

attach(f.pub.dat)
sum(hand=="L",na.rm=T);sum(hand=="R",na.rm=T);
#[1] 113
#[1] 1278
mean(age[hand=="L"],na.rm=T);sd(age[hand=="L"],na.rm=T)
#[1] 19.8785
#[1] 5.005111
mean(age[hand=="R"],na.rm=T);sd(age[hand=="R"],na.rm=T)
#[1] 19.10129
#[1] 2.445091
mean(mat..age[hand=="L"],na.rm=T);sd(mat..age[hand=="L"],na.rm=T)
#[1] 29.80909
#[1] 4.809195
mean(mat..age[hand=="R"],na.rm=T);sd(mat..age[hand=="R"],na.rm=T)
#[1] 29.6125
#[1] 5.078928
mean(pat..age[hand=="L"],na.rm=T);sd(pat..age[hand=="L"],na.rm=T)
#[1] 32.11009
#[1] 5.15741
mean(pat..age[hand=="R"],na.rm=T);sd(pat..age[hand=="R"],na.rm=T)
#[1] 32.18459
#[1] 5.501137
mean(num.sibs[hand=="L"],na.rm=T);sd(num.sibs[hand=="L"],na.rm=T)
#[1] 1.5625
#[1] 1.063576
mean(num.sibs[hand=="R"],na.rm=T);sd(num.sibs[hand=="R"],na.rm=T)
#[1] 1.584405
#[1] 1.158458
detach(f.pub.dat)


####
##    Section 2 - uncorrected Khovanova’s procedure
####
attach(pub.dat)

## Khovanova’s procedure:
## p11 = prob only son has trait
## p12 = prob first of 2 sons has trait
## p21 = prob second of two sons has trait

## SONS
n1 <- sum(sex=="M" & young..bro==0 & old..bro==0 & hand=="R", na.rm=T)
n2 <- sum(sex=="M" & young..bro==0 & old..bro==0 & hand=="L", na.rm=T)
p11 <- c(n2,n1)
n3 <- sum(sex=="M" & young..bro==1 & old..bro==0 & hand=="R", na.rm=T)
n4 <- sum(sex=="M" & young..bro==1 & old..bro==0 & hand=="L", na.rm=T)
p12 <- c(n4,n3)
n5 <- sum(sex=="M" & young..bro==0 & old..bro==1 & hand=="R", na.rm=T)
n6 <- sum(sex=="M" & young..bro==0 & old..bro==1 & hand=="L", na.rm=T)
p21 <- c(n6,n5)
rbind(p11,p12,p21)
#    [,1] [,2]
#p11   29  295
#p12   23  165
#p21    7  135

# FFE test, p11 vs p12.
chisq.test(rbind(p11,p12))
#X-squared = 1.0688, df = 1, p-value = 0.3012
#
## the logistic regression equivalent,
## where TRUE if no brothers, FALSE if only one older, no older brothers, NA otherwise (including all Sex=F).
summary(glm(bin.hand~m.khov.ffe,family="binomial",data=pub.dat))
#                 Estimate Std. Error z value Pr(>|z|)    
#(Intercept)        2.3197     0.1946  11.920   <2e-16 ***
#m.khov.ffeTRUE    -0.3492     0.2957  -1.181    0.238    

# FBOE test, p12 vs p21.
rbind(p12,p21)
#    [,1] [,2]
#p12   23  165
#p21    7  135
chisq.test(rbind(p12,p21))
#X-squared = 4.3762, df = 1, p-value = 0.03644

## the logistic regression equivalent,
## where TRUE if one younger brother only, FALSE if one older brother only, NA otherwise (including all Sex=F).
summary(glm(bin.hand~m.khov.fbo,family="binomial",data=pub.dat))
#                  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)         1.9705     0.2226   8.853   <2e-16 ***
#m.khov.fboTRUE   0.9889     0.4470   2.212   0.0269 *  

rm(n1, n2, n3, n4, n5, n6, p11, p12, p21)

## DAUGHTERS
n1 <- sum(sex=="F" & young..bro==0 & old..bro==0 & hand=="R", na.rm=T)
n2 <- sum(sex=="F" & young..bro==0 & old..bro==0 & hand=="L", na.rm=T)
p11 <- c(n2,n1)
n3 <- sum(sex=="F" & young..bro==1 & old..bro==0 & hand=="R", na.rm=T)
n4 <- sum(sex=="F" & young..bro==1 & old..bro==0 & hand=="L", na.rm=T)
p12 <- c(n4,n3)
n5 <- sum(sex=="F" & young..bro==0 & old..bro==1 & hand=="R", na.rm=T)
n6 <- sum(sex=="F" & young..bro==0 & old..bro==1 & hand=="L", na.rm=T)
p21 <- c(n6,n5)
rbind(p11,p12,p21)
#    [,1] [,2]
#p11   42  489
#p12   36  294
#p21   19  253

## FFE test
chisq.test(rbind(p11,p12))
#X-squared = 1.8734, df = 1, p-value = 0.1711

## the logistic regression equivalent,
## where TRUE if no brothers, FALSE if only one older, no older brothers, NA otherwise (including all Sex=F).
summary(glm(bin.hand~f.khov.ffe,family="binomial",data=pub.dat))
#                 Estimate Std. Error z value Pr(>|z|)    
#(Intercept)        2.4547     0.1608  15.266   <2e-16 ***
#f.khov.ffeTRUE  -0.3546     0.2388  -1.485    0.138    

rbind(p12,p21)
#    [,1] [,2]
#p12   36  294
#p21   19  253
chisq.test(rbind(p12,p21))
#X-squared = 2.3128, df = 1, p-value = 0.1283

## the logistic regression equivalent,
## where TRUE if one younger brother only, FALSE if one older brother only, NA otherwise (including all Sex=F).
summary(glm(bin.hand~f.khov.fbo,family="binomial",data=pub.dat))
#                  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)         2.1001     0.1766   11.89   <2e-16 ***
#f.khov.fboTRUE   0.4889     0.2962    1.65   0.0989 .  
rm(n1, n2, n3, n4, n5, n6, p11, p12, p21)
detach(pub.dat)

####
##    Section 3 - Parental age confound
####
attach(pub.dat)

t.test(mat..age[m.khov.fbo],mat..age[!m.khov.fbo])
#t = 5.5198, df = 276.89, p-value = 7.806e-08
#mean of x mean of y 
# 30.92593  28.08333 
sd(mat..age[m.khov.fbo],na.rm=T);sd(mat..age[!m.khov.fbo],na.rm=T)
#[1] 4.661096
#[1] 4.332384

t.test(pat..age[m.khov.fbo],pat..age[!m.khov.fbo])
#t = 4.7738, df = 300.07, p-value = 2.829e-06
#mean of x mean of y 
# 33.39259  30.72881 
sd(pat..age[m.khov.fbo],na.rm=T);sd(pat..age[!m.khov.fbo],na.rm=T)
#[1] 4.685794
#[1] 5.130669

###
### 
t.test(mat..age[sex=="M" & hand=="L"],mat..age[sex=="M" & hand=="R"])
#t = -0.96304, df = 72.382, p-value = 0.3387
#mean of x mean of y 
# 29.03175  29.70760 
sd(mat..age[sex=="M" & hand=="L"],na.rm=T);sd(mat..age[sex=="M" & hand=="R"],na.rm=T)
#[1] 5.358046
#[1] 5.018782
t.test(pat..age[sex=="M" & hand=="L"],pat..age[sex=="M" & hand=="R"])
#t = -2.2803, df = 72.971, p-value = 0.02551
#mean of x mean of y 
# 30.69841  32.32939 
sd(pat..age[sex=="M" & hand=="L"],na.rm=T);sd(pat..age[sex=="M" & hand=="R"],na.rm=T)
#[1] 5.449578
#[1] 5.215411
detach(pub.dat)
###

####
##    Section four - parental-age-corrected Khovanova's procedure
####

### Table 4 - FFE Male subjects
summary(glm(bin.hand~m.khov.ffe+pat..age+mat..age,family="binomial",data=pub.dat))
#                 Estimate Std. Error z value Pr(>|z|)   
#(Intercept)       1.22573    1.05330   1.164   0.2445  
#m.khov.ffe  TRUE -0.17955    0.32266  -0.556   0.5779  
#pat..age          0.08702    0.04745   1.834   0.0667 .
#mat..age         -0.05591    0.04919  -1.137   0.2557  

### Table 5 - FBOE Male subjects
summary(glm(bin.hand~m.khov.fbo+pat..age+mat..age,family="binomial",data=pub.dat))
#                  Estimate Std. Error z value Pr(>|z|)
#(Intercept)        0.80386    1.42919   0.562    0.574
#m.khov.fbo   TRUE  0.79391    0.50074   1.585    0.113
#pat..age           0.01436    0.06001   0.239    0.811
#mat..age           0.03112    0.06872   0.453    0.651

### Table 6 - FFE Female subjects
summary(glm(bin.hand~f.khov.ffe+pat..age+mat..age,family="binomial",data=pub.dat))
#                  Estimate Std. Error z value Pr(>|z|)    
#(Intercept)       3.780702   0.862954   4.381 1.18e-05 ***
#f.khov.ffe  TRUE -0.380655   0.252593  -1.507    0.132    
#pat..age          0.004539   0.031008   0.146    0.884    
#mat..age         -0.050214   0.033283  -1.509    0.131    

### Table 7 - FBOE Female subjects
summary(glm(bin.hand~f.khov.fbo+pat..age+mat..age,family="binomial",data=pub.dat))
#                    Estimate Std. Error z value Pr(>|z|)
#(Intercept)        0.9259867  1.0126433   0.914    0.360
#f.khov.fbo   TRUE  0.2959283  0.3180280   0.931    0.352
#pat..age           0.0398180  0.0440878   0.903    0.366
#mat..age          -0.0006333  0.0463468  -0.014    0.989

###
##### Section Five, Ablaza-esque
###

## Table 8
summary(glm(bin.hand~num.sibs + old.sibs + old..bro + young..bro
            + pat..age+mat..age,family="binomial",data=m.pub.dat))
#            Estimate Std. Error z value Pr(>|z|)   
#(Intercept)  2.00664    0.97487   2.058  0.03955 * 
#num.sibs    -0.51362    0.18971  -2.707  0.00678 **
#old.sibs     0.60222    0.27726   2.172  0.02985 * 
#old..bro     0.27266    0.33527   0.813  0.41608   
#young..bro   0.32022    0.27094   1.182  0.23724   
#pat..age     0.08980    0.04160   2.159  0.03088 * 
#mat..age    -0.07792    0.04271  -1.824  0.06810 . 

## Table 9
summary(glm(bin.hand~num.sibs + old.sibs + old..bro + young..bro
            + pat..age+mat..age,family="binomial",data=f.pub.dat))
#            Estimate Std. Error z value Pr(>|z|)   
#(Intercept)  2.10086    0.74676   2.813   0.0049 **
#num.sibs     0.26784    0.18582   1.441   0.1495   
#old.sibs    -0.52995    0.22936  -2.311   0.0209 * 
#old..bro     0.52313    0.23313   2.244   0.0248 * 
#young..bro  -0.35110    0.24025  -1.461   0.1439   
#pat..age     0.02987    0.02691   1.110   0.2671   
#mat..age    -0.02468    0.02944  -0.838   0.4018   

