
# packages ----
library(rethinking)
library(tidyverse)
library(ggridges)

# get data ----
go <- read.csv('go.csv')

go$disbelief <- go$sbs_m * -1
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

d <- select(go, 
            disbelief, 
            ment_lo, 
            security, 
            cred_lo, 
            reflection,
            ageZ, educZ, femZ, econ_consZ, social_consZ, eXtra_m, Consc_m, Neuro_m, Agree_m, Open_m, Hum_m)
summary(d)
nrow(d)
# handy functions ----


bigger <- function(first, second) {
  pr.big <- mean(ifelse(first > second, 1, 0))
  return(pr.big)
}

biggerZero <- function(input) {
  pr.big <- mean(ifelse(input > 0, 1, 0))
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


# Preregistered confirmatory tests outline ----

# We will perform a series of 7 confirmatory tests on the full sample. Each model will be run twice: first with no covariates, then including age, gender, politics, education, & personality as covariates.

# I. Does each factor independently replicate?
#  1.	We will replicate mentalizing in a model with mentalizing (linear and quadratic terms) predicting religion.
#  2.	We will replicate motivation in a model with the motivation items predicting religion.
#  3.	We will replicate cultural learning in a model with CREDs predicting religion.
#  4.	We will replicate cognitive style in a model with the CRT predicting religion.
# II. Relative contributions?
#  5.	We will run a model with all four factors predicting religion.
# III. Specific hypothesized interactions?
#  6.	To test whether cognitive style’s effects differ across cultural learning, we will have a model in which CRT, CREDs, and their interaction predict religion.
#  7.	To test the hypothesis that mentalizing is especially important in the context of cultural learning and motivation, we will have a model with mentalizing, CREDs, motivation and the mentalizing*CREDs and mentalizing*motivation interaction terms predicting religion


# I. Does each factor independently replicate? ----

#   -1.	mentalizing challenges (linear and quadratic terms) predicting disbelief ----

# with covariates
dment_cv <- select(d, disbelief, ment_lo, ageZ, educZ, femZ, social_consZ, econ_consZ, eXtra_m, Consc_m, Neuro_m, Agree_m, Open_m, Hum_m)
dment_cv <- dment_cv[complete.cases(dment_cv),]
summary(dment_cv)
nrow(dment_cv)

ment_cv <- map2stan(
  alist(
    disbelief ~ dnorm(mu, sigma),
    mu <- a + b_ment*ment_lo + b_mentQuad*ment_lo^2 + b_age*ageZ + b_educ*educZ + b_female*femZ + b_social_cons*social_consZ + b_econ_cons*econ_consZ + b_eXtra*eXtra_m + b_Consc*Consc_m + b_Neuro*Neuro_m + b_Agree*Agree_m  + b_Open*Open_m + b_Hum*Hum_m,
    c(a, b_ment, b_mentQuad, b_age, b_educ, b_female, b_social_cons, b_econ_cons, b_eXtra, b_Consc, b_Neuro, b_Agree, b_Open, b_Hum) ~ dnorm(0,1),
    sigma ~ dcauchy(0, 1)
  ), data=dment_cv, WAIC = F, chains = 1, cores = 1,
  iter = 13000, warmup = 1000
)

plot(ment_cv)
precis(ment_cv, prob=.97)

ment_cv_post <- as.data.frame(extract.samples(ment_cv))



# ment no covariates
dment_nc <- select(d, disbelief, ment_lo)
dment_nc <- dment_nc[complete.cases(dment_nc),]
summary(dment_nc)
nrow(dment_nc)


ment_nc <- map2stan(
  alist(
    disbelief ~ dnorm(mu, sigma),
    mu <- a + b_ment*ment_lo + b_mentQuad*ment_lo^2,
    c(a, b_ment, b_mentQuad) ~ dnorm(0,1),
    sigma ~ dcauchy(0, 1)
  ), data=dment_nc, WAIC = F, chains = 1, cores = 1,
  iter = 13000, warmup = 1000, control=list(adapt_delta=0.9)
)



plot(ment_nc)
precis(ment_nc, prob=.97)

ment_nc_post <- as.data.frame(extract.samples(ment_nc))



#   -2.	existential security predicting disbelief. ----

# with covariates
dsec_cv <- select(d, disbelief, security, ageZ, educZ, femZ, social_consZ, econ_consZ, eXtra_m, Consc_m, Neuro_m, Agree_m, Open_m, Hum_m)
dsec_cv <- dsec_cv[complete.cases(dsec_cv),]
summary(dsec_cv)
nrow(dsec_cv)

sec_cv <- map2stan(
  alist(
    disbelief ~ dnorm(mu, sigma),
    mu <- a + b_sec*security + b_age*ageZ + b_educ*educZ + b_female*femZ + b_social_cons*social_consZ + b_econ_cons*econ_consZ + b_eXtra*eXtra_m + b_Consc*Consc_m + b_Neuro*Neuro_m + b_Agree*Agree_m  + b_Open*Open_m + b_Hum*Hum_m,
    c(a, b_sec, b_age, b_educ, b_female, b_social_cons, b_econ_cons, b_eXtra, b_Consc, b_Neuro, b_Agree, b_Open, b_Hum) ~ dnorm(0,1),
    sigma ~ dcauchy(0, 1)
  ), data=dsec_cv, WAIC = F, chains = 1, cores = 1,
  iter = 13000, warmup = 1000, control=list(adapt_delta=0.9)
)

plot(sec_cv)
precis(sec_cv, prob=.97)

sec_cv_post <- as.data.frame(extract.samples(sec_cv))

# sec no covariates
dsec_nc <- select(d, disbelief, security)
dsec_nc <- dsec_nc[complete.cases(dsec_nc),]
summary(dsec_nc)
nrow(dsec_nc)


sec_nc <- map2stan(
  alist(
    disbelief ~ dnorm(mu, sigma),
    mu <- a + b_sec*security ,
    c(a, b_sec) ~ dnorm(0,1),
    sigma ~ dcauchy(0, 1)
  ), data=dsec_nc, WAIC = F, chains = 1, cores = 1,
  iter = 13000, warmup = 1000, control=list(adapt_delta=0.9)
)


plot(sec_nc)
precis(sec_nc, prob=.97)

sec_nc_post <- as.data.frame(extract.samples(sec_nc))



#   -3.	fewer CREDs predicting disbelief. ----


# with covariates
dcred_cv <- select(d, disbelief, cred_lo, ageZ, educZ, femZ, social_consZ, econ_consZ, eXtra_m, Consc_m, Neuro_m, Agree_m, Open_m, Hum_m)
dcred_cv <- dcred_cv[complete.cases(dcred_cv),]
summary(dcred_cv)
nrow(dcred_cv)

cred_cv <- map2stan(
  alist(
    disbelief ~ dnorm(mu, sigma),
    mu <- a + b_cred*cred_lo + b_age*ageZ + b_educ*educZ + b_female*femZ + b_social_cons*social_consZ + b_econ_cons*econ_consZ + b_eXtra*eXtra_m + b_Consc*Consc_m + b_Neuro*Neuro_m + b_Agree*Agree_m  + b_Open*Open_m + b_Hum*Hum_m,
    c(a, b_cred, b_age, b_educ, b_female, b_social_cons, b_econ_cons, b_eXtra, b_Consc, b_Neuro, b_Agree, b_Open, b_Hum) ~ dnorm(0,1),
    sigma ~ dcauchy(0, 1)
  ), data=dcred_cv, WAIC = F, chains = 1, cores = 1,
  iter = 13000, warmup = 1000, control=list(adapt_delta=0.9)
)

plot(cred_cv)
precis(cred_cv, prob=.97)

cred_cv_post <- as.data.frame(extract.samples(cred_cv))

# cred no covariates
dcred_nc <- select(d, disbelief, cred_lo)
dcred_nc <- dcred_nc[complete.cases(dcred_nc),]
summary(dcred_nc)
nrow(dcred_nc)


cred_nc <- map2stan(
  alist(
    disbelief ~ dnorm(mu, sigma),
    mu <- a + b_cred*cred_lo ,
    c(a, b_cred) ~ dnorm(0,1),
    sigma ~ dcauchy(0, 1)
  ), data=dcred_nc, WAIC = F, chains = 1, cores = 1,
  iter = 13000, warmup = 1000, control=list(adapt_delta=0.9)
)


plot(cred_nc)
precis(cred_nc, prob=.97)

cred_nc_post <- as.data.frame(extract.samples(cred_nc))



#   -4.	cognitive reflection predicting disbelief. ----

# with covariates
dref_cv <- select(d, disbelief, reflection, ageZ, educZ, femZ, social_consZ, econ_consZ, eXtra_m, Consc_m, Neuro_m, Agree_m, Open_m, Hum_m)
dref_cv <- dref_cv[complete.cases(dref_cv),]
summary(dref_cv)
nrow(dref_cv)

ref_cv <- map2stan(
  alist(
    disbelief ~ dnorm(mu, sigma),
    mu <- a + b_ref*reflection + b_age*ageZ + b_educ*educZ + b_female*femZ + b_social_cons*social_consZ + b_econ_cons*econ_consZ + b_eXtra*eXtra_m + b_Consc*Consc_m + b_Neuro*Neuro_m + b_Agree*Agree_m  + b_Open*Open_m + b_Hum*Hum_m,
    c(a, b_ref, b_age, b_educ, b_female, b_social_cons, b_econ_cons, b_eXtra, b_Consc, b_Neuro, b_Agree, b_Open, b_Hum) ~ dnorm(0,1),
    sigma ~ dcauchy(0, 1)
  ), data=dref_cv, WAIC = F, chains = 1, cores = 1,
  iter = 13000, warmup = 1000, control=list(adapt_delta=0.9)
)

plot(ref_cv)
precis(ref_cv, prob=.97)

ref_cv_post <- as.data.frame(extract.samples(ref_cv))

# ref no covariates
dref_nc <- select(d, disbelief, reflection)
dref_nc <- dref_nc[complete.cases(dref_nc),]
summary(dref_nc)
nrow(dref_nc)


ref_nc <- map2stan(
  alist(
    disbelief ~ dnorm(mu, sigma),
    mu <- a + b_ref*reflection,
    c(a, b_ref) ~ dnorm(0,1),
    sigma ~ dcauchy(0, 1)
  ), data=dref_nc, WAIC = F, chains = 1, cores = 1,
  iter = 13000, warmup = 1000, control=list(adapt_delta=0.9)
)


plot(ref_nc)
precis(ref_nc, prob=.97)

ref_nc_post <- as.data.frame(extract.samples(ref_nc))



# summarize individual zero orders ----

zeros <- data.frame('Mentalizing' = ment_nc_post$b_ment, 'MentQuad' = ment_nc_post$b_mentQuad, 'Security' = sec_nc_post$b_sec, 'CREDs' = cred_nc_post$b_cred, 'Reflection' = ref_nc_post$b_ref)
write.csv(zeros, 'zero-order-posteriors.csv')
# zeros <- read.csv('zero-order-posteriors.csv')

zeros.cv <- data.frame('Mentalizing' = ment_cv_post$b_ment, 'MentQuad' = ment_cv_post$b_mentQuad, 'Security' = sec_cv_post$b_sec, 'CREDs' = cred_cv_post$b_cred, 'Reflection' = ref_cv_post$b_ref)
write.csv(zeros.cv, 'zero-order-posteriors-cv.csv')

zero.m <- zeros %>% sapply(mean) %>% unname %>% round(digits = 2)
zero.l <- zeros %>% sapply(low) %>% unname %>% round(digits = 2)
zero.h <- zeros %>% sapply(high) %>% unname %>% round(digits = 2)
zero.pr <- zeros %>% sapply(biggerZero) %>% unname %>% round(digits = 2) 

pdi <- paste0('[', zero.l, ', ', zero.h, ']')


v <- c('Low Mentalizing', 'Mentalizing (quad)', 'High Security', 'Low CREDs', 'High Reflection')

zero.table <- data.frame(Variable = v, r = zero.m, HPDI= pdi, Pr = zero.pr)
zero.table$Pr[zero.table$Pr == 1] <- '>0.99'
zero.table

# II. Relative contributions ----
#  5.	We will run a model with all four factors predicting religion.

# with covariates
dfull_cv <- select(d, disbelief, ment_lo, security, cred_lo, reflection, ageZ, educZ, femZ, social_consZ, econ_consZ, eXtra_m, Consc_m, Neuro_m, Agree_m, Open_m, Hum_m)
dfull_cv <- dfull_cv[complete.cases(dfull_cv),]
summary(dfull_cv)
nrow(dfull_cv)

full_cv <- map2stan(
  alist(
    disbelief ~ dnorm(mu, sigma),
    mu <- a + b_ment*ment_lo + b_mentQuad*ment_lo^2 + b_sec*security + b_cred*cred_lo + b_ref*reflection + b_age*ageZ + b_educ*educZ + b_female*femZ + b_social_cons*social_consZ + b_econ_cons*econ_consZ + b_eXtra*eXtra_m + b_Consc*Consc_m + b_Neuro*Neuro_m + b_Agree*Agree_m  + b_Open*Open_m + b_Hum*Hum_m,
    c(a, b_ment, b_mentQuad, b_sec, b_cred, b_ref, b_age, b_educ, b_female, b_social_cons, b_econ_cons, b_eXtra, b_Consc, b_Neuro, b_Agree, b_Open, b_Hum) ~ dnorm(0,1),
    sigma ~ dcauchy(0, 1)
  ), data=dfull_cv, WAIC = F, chains = 1, cores = 1,
  iter = 152000, warmup = 2000, control=list(adapt_delta=0.9)
)

plot(full_cv)
precis(full_cv, prob=.97)

full_cv_post <- as.data.frame(extract.samples(full_cv, n=145000))
write.csv(full_cv_post, 'full-posterior.csv')
# write.csv(full_cv_post, 'full_posterior_short.csv')

# full no covariates
dfull_nc <- select(d, disbelief, ment_lo, security, cred_lo, reflection)
dfull_nc <- dfull_nc[complete.cases(dfull_nc),]
summary(dfull_nc)
nrow(dfull_nc)


full_nc <- map2stan(
  alist(
    disbelief ~ dnorm(mu, sigma),
    mu <- a + b_ment*ment_lo + b_mentQuad*ment_lo^2 + b_sec*security + b_cred*cred_lo + b_ref*reflection,
    c(a, b_ment, b_mentQuad, b_sec, b_cred, b_ref) ~ dnorm(0,1),
    sigma ~ dcauchy(0, 1)
  ), data=dfull_nc, WAIC = F, chains = 1, cores = 1,
  iter = 13000, warmup = 1000, control=list(adapt_delta=0.9)
)


plot(full_nc)
precis(full_nc, prob=.97)

full_nc_post <- as.data.frame(extract.samples(full_nc))


# III. Specific hypothesized interactions ----


#  6.	CRT, CREDs, and their interaction predict religion.----


# crt * cred, w/ covariates

dcrt_cred_cv <- select(d, disbelief, cred_lo, reflection, ageZ, educZ, femZ, social_consZ, econ_consZ, eXtra_m, Consc_m, Neuro_m, Agree_m, Open_m, Hum_m)
dcrt_cred_cv <- dcrt_cred_cv[complete.cases(dcrt_cred_cv),]
summary(dcrt_cred_cv)
nrow(dcrt_cred_cv)

dcrt_cred_cv$belief <- dcrt_cred_cv$disbelief * -1
dcrt_cred_cv$cred <- dcrt_cred_cv$cred_lo * -1

crt_cred_cv <- map2stan(
  alist(
    belief ~ dnorm(mu, sigma),
    mu <- a + reflection*( b_ref + b_int*cred) + b_cred*cred + b_age*ageZ + b_educ*educZ + b_female*femZ + b_social_cons*social_consZ + b_econ_cons*econ_consZ + b_eXtra*eXtra_m + b_Consc*Consc_m + b_Neuro*Neuro_m + b_Agree*Agree_m  + b_Open*Open_m + b_Hum*Hum_m,
    c(a, b_cred, b_ref, b_int, b_age, b_educ, b_female, b_social_cons, b_econ_cons, b_eXtra, b_Consc, b_Neuro, b_Agree, b_Open, b_Hum) ~ dnorm(0,1),
    sigma ~ dcauchy(0, 1)
  ), data=dcrt_cred_cv, WAIC = F, 
  chains = 1, cores = 1, 
  iter = 22000, warmup = 2000
)

plot(crt_cred_cv)
precis(crt_cred_cv)

intx <- data.frame(extract.samples(crt_cred_cv, n = 20000))
precis(intx)
write.csv(intx, 'CrtCREDPost.csv')


# model comparison on interaction

crt_cred_cv.2 <- map2stan(
  alist(
    belief ~ dnorm(mu, sigma),
    mu <- a + reflection*b_ref + b_cred*cred + b_age*ageZ + b_educ*educZ + b_female*femZ + b_social_cons*social_consZ + b_econ_cons*econ_consZ + b_eXtra*eXtra_m + b_Consc*Consc_m + b_Neuro*Neuro_m + b_Agree*Agree_m  + b_Open*Open_m + b_Hum*Hum_m,
    c(a, b_cred, b_ref, b_age, b_educ, b_female, b_social_cons, b_econ_cons, b_eXtra, b_Consc, b_Neuro, b_Agree, b_Open, b_Hum) ~ dnorm(0,1),
    sigma ~ dcauchy(0, 1)
  ), data=dcrt_cred_cv, WAIC = T, 
  chains = 1, cores = 1, 
  iter = 22000, warmup = 2000
)


compare(crt_cred_cv, crt_cred_cv.2) #yes, the interaction is good to add

# mentalizing interactions ----
#  7.	mentalizing, CREDs, motivation and the mentalizing*CREDs and mentalizing*motivation interaction terms predicting religion.

# ment int covariates
dmentint_cv <- select(d, disbelief, ment_lo, security, cred_lo, ageZ, educZ, femZ, social_consZ, econ_consZ, eXtra_m, Consc_m, Neuro_m, Agree_m, Open_m, Hum_m)
dmentint_cv <- dmentint_cv[complete.cases(dmentint_cv),]
summary(dmentint_cv)
nrow(dmentint_cv)

mentint_cv <- map2stan(
  alist(
    disbelief ~ dnorm(mu, sigma),
    mu <- a + b_mc_int*ment_lo*cred_lo + b_mm_int*ment_lo*security + b_ment*ment_lo + b_mentQuad*ment_lo^2 + b_sec*security + b_cred*cred_lo + b_age*ageZ + b_educ*educZ + b_female*femZ + b_social_cons*social_consZ + b_econ_cons*econ_consZ + b_eXtra*eXtra_m + b_Consc*Consc_m + b_Neuro*Neuro_m + b_Agree*Agree_m  + b_Open*Open_m + b_Hum*Hum_m,
    c(a, b_mc_int, b_mm_int,  b_ment, b_mentQuad, b_sec, b_cred, b_age, b_educ, b_female, b_social_cons, b_econ_cons, b_eXtra, b_Consc, b_Neuro, b_Agree, b_Open, b_Hum) ~ dnorm(0,1),
    sigma ~ dcauchy(0, 1)
  ), data=dmentint_cv, WAIC = F, chains = 1, cores = 1,
  iter = 13000, warmup = 1000
)

plot(mentint_cv)
precis(mentint_cv, prob=.97)

mentint_cv_post <- as.data.frame(extract.samples(mentint_cv, n=145000))



## the plots thicken ----

# individual scatterplots ----

# mentalizing

ggplot(dment_nc, aes(x=-1*ment_lo, y=-1*disbelief)) +
  labs(x='Advanced\nMentalizing', y= 'Belief') +
  geom_point(alpha=.2, position='jitter', color='darkred') +
  stat_smooth(method=lm, color='darkred') +
  theme_bw() +
  theme(text = element_text(family="Times"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size = 20, color='darkred'),
        axis.title.y = element_text(size = 20),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color="darkgrey")
  )

# motivation

ggplot(dsec_nc, aes(x=security, y=-1*disbelief)) +
  labs(x='Existential\nSecurity', y= 'Belief') +
  geom_point(alpha=.2, position='jitter', color='darkblue') +
  stat_smooth(method=lm, color='darkblue') +
  theme_bw() +
  theme(text = element_text(family="Times"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size = 20, color='darkblue'),
        axis.title.y = element_text(size = 20),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color="darkgrey")
  )

# CREDs

ggplot(dcred_nc, aes(x=-1*cred_lo, y=-1*disbelief)) +
  labs(x='Religious\nCREDs', y= 'Belief') +
  geom_point(alpha=.2, position='jitter', color='purple3') +
  stat_smooth(method=lm, color='purple3') +
  theme_bw() +
  theme(text = element_text(family="Times"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size = 20, color='purple3'),
        axis.title.y = element_text(size = 20),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color="darkgrey")
  )


# reflection cognitive style

ggplot(dref_nc, aes(x=reflection, y=-1*disbelief)) +
  labs(x='Cognitive\nReflection', y= 'Belief') +
  geom_point(alpha=.2, position='jitter', color='darkgreen') +
  stat_smooth(method=lm, color='darkgreen') +
  theme_bw() +
  theme(text = element_text(family="Times"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title.x = element_text(size = 20, color='darkgreen'),
        axis.title.y = element_text(size = 20),
        panel.grid.minor.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color="darkgrey")
  )



# combined posteriors ----
full_cv_post <- read.csv('full-posterior.csv')

summary(full_cv_post)


post.b <-  full_cv_post %>% select( 'Lower\nMentalizing\n\n\n' = b_ment, 'Existential\nSecurity\n\n\n' = b_sec, 'Fewer\nCREDs\n\n\n' = b_cred, 'Cognitive\nReflection\n\n\n' = b_ref) 

post.long.b <- gather( post.b, type, beta)

post.long.b$type <- factor(post.long.b$type, levels=c(  'Existential\nSecurity\n\n\n', 'Lower\nMentalizing\n\n\n','Cognitive\nReflection\n\n\n',  'Fewer\nCREDs\n\n\n'))

summary(post.long.b)



ggplot(post.long.b, aes(x = beta, y = type, fill = type)) +
  geom_vline(xintercept = 0, color='darkgrey') +
  geom_density_ridges(alpha = .7, scale = 1.9, rel_min_height = 0.01, size=0) +
  labs(x = '\nAssociation With Disbelief\n(standardized beta)') +
  scale_x_continuous(limits = c(-.1, .4)) +
  scale_y_discrete(expand = expand_scale(add = c(0.3, 2))) +
  scale_fill_manual(values = c('darkblue', 'darkred',   'darkgreen', 'purple3' )) +
  theme_bw() +
  theme(text = element_text(family="Times"),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16, color = c('darkblue', 'darkred',   'darkgreen', 'purple3' )),
        axis.title = element_text(size = 20),
        axis.ticks = element_blank(),
        panel.grid.minor.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.x = element_blank(),
        legend.position = "none",
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.border = element_rect(color="darkgrey"),
        axis.title.y = element_blank()
  )



# interaction: reflection CREDs ----
# plotting interaction
# separate lines for CREDs, x-axis is crt
# a + b_ref*reflection + b_cred*cred_lo + b_int * reflection * cred_lo
# for slope of reflection: reflection * (b_ref + b_int * cred_lo) + b_cred * cred_lo + a
# intercept mean CRED (cred_lo = 0, reflection = 0):   a
# slope mean CRED (cred_lo = 0): b_ref
# intercept minimum CRED (cred_lo = leastcred) = b_cred * leastcred + a
# slope minimum CRED (cred_lo = leastcred) = b_ref + b_int * leastcred
# intercept maximum CRED (cred_lo = mostcred) = b_cred * mostcred + a
# slope maximum CRED (cred_lo = mostcred) = b_ref + b_int * mostcred

leastcred <- min(dcrt_cred_cv$cred)
mostcred <- max(dcrt_cred_cv$cred)


meanCRED_a <- intx$a
meanCRED_b <- intx$b_ref
minCRED_a <- intx$b_cred*leastcred + intx$a
minCRED_b <- intx$b_ref + intx$b_int * leastcred
maxCRED_a <- intx$b_cred * mostcred + intx$a
maxCRED_b <- intx$b_ref + intx$b_int * mostcred

cred_crt <- data.frame(meanCRED_a, meanCRED_b, minCRED_a, minCRED_b, maxCRED_a, maxCRED_b)
summary(cred_crt)

# summaries

apply(cred_crt, 2, mean)
apply(cred_crt, 2, low)
apply(cred_crt, 2, high)


# get means for lines

mean.int <- mean(cred_crt$meanCRED_a)
mean.slope <- mean(cred_crt$meanCRED_b)
min.int <- mean(cred_crt$minCRED_a)
min.slope <- mean(cred_crt$minCRED_b)
max.int <- mean(cred_crt$maxCRED_a)
max.slope <- mean(cred_crt$maxCRED_b)

# get 20 smaples of each

cred_crt <- cred_crt[sample(1:nrow(cred_crt)), ] #shuffle the deck

mean.20int <- c(cred_crt$meanCRED_a[1:100])
mean.20slope <- c(cred_crt$meanCRED_b[1:100])

min.20int <- c(cred_crt$minCRED_a[1:100])
min.20slope <- c(cred_crt$minCRED_b[1:100])

max.20int <- c(cred_crt$maxCRED_a[1:100])
max.20slope <- c(cred_crt$maxCRED_b[1:100])

# to plot this shizzle

d <- data.frame(x=c(min(dcrt_cred_cv$reflection), 0, max(dcrt_cred_cv$reflection)))

brange <- max(dcrt_cred_cv$reflection) - min(dcrt_cred_cv$reflection)

decs <- seq(from=0, to=1, by=.33)

b.breaks <- min(dcrt_cred_cv$reflection) + decs*brange


meancol <- 'black'
mincol <- 'purple'
maxcol <- 'hotpink'


ggplot(dcrt_cred_cv, aes(x=reflection, y=belief)) +
  geom_point(alpha=.05, position = 'jitter') +
  scale_x_continuous(breaks=b.breaks, labels=c(0, 3, 6, 9)) +
  labs(x="Cognitive Reflection\n(correct CRT items)", y="Belief") +
  geom_abline(slope=mean.20slope, intercept=mean.20int, lwd=.2, col=meancol, alpha=.15) +
  geom_abline(slope=min.20slope, intercept=min.20int, lwd=.2, col=mincol, alpha=.15) +
  geom_abline(slope=max.20slope, intercept=max.20int, lwd=.2, col=maxcol, alpha=.15) +
  geom_abline(slope=c(mean.slope, min.slope, max.slope), intercept=c(mean.int, min.int, max.int), col=c(meancol, mincol, maxcol), lwd=1.2) +
  annotate("text", x = 0.3, y = c(.7, 0.1, -1.2), label = c("Max CRED", "Mean CRED", "Min CRED"), hjust=0, vjust=0, size=6, color=c(maxcol, meancol, mincol), family = "Times") +
  theme_bw() +
  theme(text = element_text(family="Times"),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        panel.background = element_blank(),
        plot.background = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_blank() ,
        panel.grid.minor.x = element_blank() ,
        panel.grid.major.y = element_blank() ,
        panel.grid.minor.y = element_blank() ,
        panel.border = element_rect(color="darkgrey"),
        plot.margin = unit(c(1, 1, 1, 1), "lines")) 


# the tables turn ----

# full model summary



fullPost <- full_cv_post %>% select(-X, -a, -sigma)

fullPost$b_female <- fullPost$b_female * -1 # reverse to score for male = disbelief
fullPost$b_social_cons <- fullPost$b_social_cons * -1 # reverse so social lib = disbelief
fullPost$b_Agree <- fullPost$b_Agree * -1 #reverse so low aggreable = disbelief


beta <- fullPost %>% sapply(mean) %>% unname %>% round(digits=2)
beta.l <- fullPost %>% sapply(low) %>% unname %>% round(digits=2)
beta.h <- fullPost %>% sapply(high) %>% unname %>% round(digits=2)
hpdi <- paste0('[', beta.l, ', ', beta.h, ']')
Pr0 <- fullPost %>% sapply(biggerZero) %>% unname %>% round(digits=2)

pred <- c('Low Mentalizing', 'Mentalizing (quad)', 'Security', 'Low CREDs', 'Reflection', 'Age', 'Education', 'Male', 'Social Lib', 'Economic Cons', 'Extraversion', 'Conscientiousness', 'Neuroticism', 'Low Agreeableness', 'Openness', 'Honesty/Humility')

fulltab <- data.frame(variable = pred, beta = beta, HPDI = hpdi, Pr = Pr0 )
fulltab$Pr[fulltab$Pr == 1] <- '> 0.99'
fulltab$Pr[fulltab$Pr == 0] <- '< 0.01'
fulltab


