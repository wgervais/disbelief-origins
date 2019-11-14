
# packages ----
library(rethinking)
library(tidyverse)
library(ggridges)

# get data ----
go <- read.csv('go.csv')

go$atheist <- go$god_y*-1 + 1
go$ment_lo <- go$pt_m * -1
go$cred_lo <- go$cred_m * -1
go$security <- go$mot_m * -1
go$reflection <- go$crtZ
go$ageZ <- scale(go$age, center=T, scale=T)[,]
go$educZ <- scale(go$educ, center=T, scale=T)[,]
go$femZ <- scale(go$female, scale=T, center = T)[,]
go$econ_consZ <- scale(go$econ_cons, center=T, scale=T)[,]
go$social_consZ <- scale(go$social_cons, center=T, scale=T)[,]
summary(go)

d2 <- go %>% select(atheist, 
                    ment_lo, 
                    security, 
                    cred_lo, 
                    reflection,
                    ageZ, 
                    educZ, 
                    femZ, 
                    econ_consZ, 
                    social_consZ, 
                    eXtra_m, 
                    Consc_m, 
                    Neuro_m, 
                    Agree_m, 
                    Open_m, 
                    Hum_m) %>%
              drop_na()
             
summary(d2)






# handy functions ----

dig <- 2


bigger <- function(first, second, digits) {
  pr.big <- ifelse(first > second, 1, 0) %>% mean %>% round(digits = dig)
  return(pr.big)
}

biggerZero <- function(input, digits) {
  pr.big <- ifelse(input > 0, 1, 0) %>% mean %>% round(digits = dig)
  return(pr.big)
}



# a couple of quick functions for pulling HPDIs


low <- function(x){
  v <- HPDI(x, prob=.97)[1]
  return(v)
}


high <- function(x){
  v <- HPDI(x, prob=.97)[2]
  return(v)
}


printB <- function(input) {
  p <- input %>% mean %>% round(digits = dig)
  l <- input %>% low %>% round(digits = dig)
  h <- input %>% high %>% round(digits = dig)
  paste0(p, ", [", l, ", ", h, "]")
}






#exploration zone ----

# full model ----

full_cv <- map2stan(
  alist(
    atheist ~ dbinom(1, p),
    logit(p) <- a + reflection*b_ref + b_cred*cred_lo + b_ment*ment_lo + b_sec*security + b_age*ageZ + b_educ*educZ + b_female*femZ + b_social_cons*social_consZ + b_econ_cons*econ_consZ + b_eXtra*eXtra_m + b_Consc*Consc_m + b_Neuro*Neuro_m + b_Agree*Agree_m  + b_Open*Open_m + b_Hum*Hum_m,
    c(a, b_ref, b_cred, b_sec, b_ment, b_age, b_educ, b_female, b_social_cons, b_econ_cons, b_eXtra, b_Consc, b_Neuro, b_Agree, b_Open, b_Hum) ~ dnorm(0,2)
  ), data=d2, WAIC= T,
  chains=1, cores=1, 
  iter=24000, warmup=1000
)

plot(full_cv)
precis(full_cv)

bin_p <- data.frame(extract.samples(full_cv, n=20000))

write.csv(bin_p, 'binary-post.csv')



maxRef <- max(d2$reflection)
minRef <- min(d2$reflection)
maxCRED <- min(d2$ment_lo)
minCRED <- max(d2$cred_lo)



Average <- bin_p$a %>% logistic()
Analytic <- (bin_p$a + bin_p$b_ref*maxRef) %>% logistic()
inCREDulous <- (bin_p$a + bin_p$b_cred*minCRED) %>% logistic()

Joes <- data.frame(Average, Analytic, inCREDulous)

apply(Joes, 2, median)
apply(Joes, 2, printB)

(.3/.7)/(.2/.8)

jl <- Joes %>% gather(target, probability)

jl$target <- factor(jl$target, levels=c(  'Average', 'Analytic','inCREDulous'))

ggplot(jl, aes(x=probability, fill = target)) +
  geom_density(adjust=2, alpha=.6) +
  xlim(0,.5) +
  labs(x='Atheist Probability')





