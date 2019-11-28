# get data 
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


d <- select(go, 
            disbelief, 
            ment_lo, 
            security, 
            cred_lo, 
            reflection,
            ageZ, educZ, femZ, econ_consZ, social_consZ, eXtra_m, Consc_m, Neuro_m, Agree_m, Open_m, Hum_m)

dfull_nc <- d %>% select(disbelief, ment_lo, security, cred_lo, reflection) %>% drop_na()

full_nc <- map2stan(
  alist(
    disbelief ~ dnorm(mu, sigma),
    mu <- a + b_ment*ment_lo + b_sec*security + b_cred*cred_lo + b_ref*reflection,
    c(a, b_ment, b_sec, b_cred, b_ref) ~ dnorm(0,1),
    sigma ~ dcauchy(0, 1)
  ), data=dfull_nc, WAIC = F, chains = 1, cores = 1,
  iter = 13000, warmup = 1000
)

full_nc_post <- as.data.frame(extract.samples(full_nc))
write.csv(full_nc_post, "full_nc_post.csv")


dmentint_cv <- select(d, disbelief, ment_lo, security, cred_lo, ageZ, educZ, femZ, social_consZ, econ_consZ, eXtra_m, Consc_m, Neuro_m, Agree_m, Open_m, Hum_m)
dmentint_cv <- dmentint_cv[complete.cases(dmentint_cv),]

mentint_cv <- map2stan(
  alist(
    disbelief ~ dnorm(mu, sigma),
    mu <- a + b_mc_int*ment_lo*cred_lo + b_mm_int*ment_lo*security + b_ment*ment_lo + b_sec*security + b_cred*cred_lo + b_age*ageZ + b_educ*educZ + b_female*femZ + b_social_cons*social_consZ + b_econ_cons*econ_consZ + b_eXtra*eXtra_m + b_Consc*Consc_m + b_Neuro*Neuro_m + b_Agree*Agree_m  + b_Open*Open_m + b_Hum*Hum_m,
    c(a, b_mc_int, b_mm_int,  b_ment, b_sec, b_cred, b_age, b_educ, b_female, b_social_cons, b_econ_cons, b_eXtra, b_Consc, b_Neuro, b_Agree, b_Open, b_Hum) ~ dnorm(0,1),
    sigma ~ dcauchy(0, 1)
  ), data=dmentint_cv, WAIC = F, chains = 1, cores = 1,
  iter = 13000, warmup = 1000
)

mentint <- as.data.frame(extract.samples(mentint_cv, n=145000))
write.csv(mentint, "mentint.csv")



