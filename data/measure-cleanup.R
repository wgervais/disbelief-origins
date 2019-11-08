library(psych)
library(tidyverse)

# this is a big script for tidying up our big data frame and gettins useful composite measures pieced together. End goal is 1) a separate data frame with composite measures (M and SD of composites within participant), and 2) a frame in which they're all standardized

# load explore dataset.----
# Note: When it's time to open our holdout set, we'll use the exact same code 
# put sets together

messy <- read.csv('gfk.csv')

messy[messy == -1] <- NA
messy <- subset(messy, Q12 == 2)  # Q 12 is a catch item. only keep 2s

head(messy)
summary(messy)



messy <- messy %>% rename(
  god_open = Q01,
  rel_groups = Q03,
  god_y = Q04,
  pct_believe = Q05a,
  pct_nonbelieve = Q05b,
  church = Q07,
  pray = Q07a,
  country_direct_open = Q7b,
  gender_ident = Q17,
  sex_or = Q18,
  stem = Q19,
  social_cons = Q20,
  econ_cons = Q21,
  vote2016 = PPPA1699,
  candidate2016 = PPPA1690
)


# scales: 
# Q06 is supernatural belief scale (SBS)
# Q08 qs are CRT
# Q09 is IRI-perspective taking
# Q10 is motivation I, Q11 is motivation II
# Q13 is CREDs
# Q14 is personality
# Q16 is tolerance for ambiguity


# supernatural belief scale ----

sbs <- select(messy, Q06_1:Q06_10)
alpha(sbs) # good alphas. let's create some composites

sbs_m <- scale( apply(sbs, 1, mean), center = T, scale=T)[,]
sbs_sd <- scale( apply(sbs, 1, sd), center=T, scale=T)[,]

# CRT ----

crt1 <- ifelse(messy$Q08a_1 == 5, 1, 0)
crt2 <- ifelse(messy$Q08a_2 == 5, 1, 0)
crt3 <- ifelse(messy$Q08a_3 == 47, 1, 0)
crt4 <- ifelse(messy$Q08b_1 == 3, 1, 0)
crt5 <- ifelse(messy$Q08b_2 == 29, 1, 0)
crt6 <- ifelse(messy$Q08b_3 == 15, 1, 0)
crt7 <- ifelse(messy$Q08c_1 == 4, 1, 0)
crt8 <- ifelse(messy$Q08c_2 == 20, 1, 0)
crt9 <- ifelse(messy$Q08d == 3, 1, 0)


crt.d <- data.frame(crt1, crt2, crt3, crt4, crt5, crt6, crt7, crt8, crt9)

crt <- apply( crt.d, 1, sum, na.rm=T) # counts incomplete as incorrect

crtZ <- scale(crt, center=T, scale=T)[,]

# perspective taking ----

persp <- select(messy, Q09_1:Q09_7)
#reverse score a few
persp$Q09_1 <- 8- persp$Q09_1
persp$Q09_4 <- 8- persp$Q09_4


persp2 <- persp[complete.cases(persp),]
cor(persp2)

alpha(persp)


pt_m <- scale( apply(persp, 1, mean), center = T, scale=T)[,]
pt_sd <- scale( apply(persp, 1, sd), center=T, scale=T)[,]


# CREDs ----


cred <- select(messy, Q13_1:Q13_7)

cred2 <- cred[complete.cases(cred),]
cor(cred2)

alpha(cred)


cred_m <- scale( apply(cred, 1, mean), center = T, scale=T)[,]
cred_sd <- scale( apply(cred, 1, sd), center=T, scale=T)[,]


# motivation. higher values == more insecurity ----

mot <- select(messy, Q10_1:Q11_9)

# reverse score a few. higher values == more insecurity
# mot$Q11_1 <- mot$Q11_1 * -1 # empirically, it says not to reverse score this one. odd. Actually, looks best to omit this item entirely?

mot <- select(mot, -Q11_1)

mot$Q11_2 <- 5 - mot$Q11_2 
mot$Q11_3 <- 5 - mot$Q11_3 
mot$Q11_4 <- 5 - mot$Q11_4 
mot$Q11_7 <- 5 - mot$Q11_7 
mot$Q11_9 <- 5 - mot$Q11_9



mot2 <- mot[complete.cases(mot),]
cor(mot2)

alpha(mot, check.keys=T)


mot_m <- scale( apply(mot, 1, mean), center = T, scale=T)[,]
mot_sd <- scale( apply(mot, 1, sd), center=T, scale=T)[,]


# tolerance for ambiguity ----

tfa <- select(messy, Q16_1:Q16_13)

# recode some
tfa$Q16_1 <- 8 - tfa$Q16_1
tfa$Q16_2 <- 8 - tfa$Q16_2
tfa$Q16_3 <- 8 - tfa$Q16_3
tfa$Q16_5 <- 8 - tfa$Q16_5
tfa$Q16_6 <- 8 - tfa$Q16_6
tfa$Q16_9 <- 8 - tfa$Q16_9
tfa$Q16_11 <- 8 - tfa$Q16_11
tfa$Q16_12 <- 8 - tfa$Q16_12

# 4 looks oddly worded. drop? '1.	I prefer familiar situations that are ambiguous.' WTF?
tfa <- select(tfa, -Q16_4)



tfa2 <- tfa[complete.cases(tfa),]
cor(tfa2)

alpha(tfa, check.keys=T)


tfa_m <- scale( apply(tfa, 1, mean), center = T, scale=T)[,]
tfa_sd <- scale( apply(tfa, 1, sd), center=T, scale=T)[,]



# personality ----
# eXtraversion
eXtra <- select(messy, Q14_1:Q14_4)

eXtra$Q14_2 <- 8- eXtra$Q14_2
eXtra$Q14_3 <- 8- eXtra$Q14_3

eXtra2 <- eXtra[complete.cases(eXtra),]
cor(eXtra2)

alpha(eXtra, check.keys=T)


eXtra_m <- scale( apply(eXtra, 1, mean), center = T, scale=T)[,]
eXtra_sd <- scale( apply(eXtra, 1, sd), center=T, scale=T)[,]

# Agreeableness
Agree <- select(messy, Q14_5:Q14_8)

Agree$Q14_6 <- 8- Agree$Q14_6
Agree$Q14_8 <- 8- Agree$Q14_8

Agree2 <- Agree[complete.cases(Agree),]
cor(Agree2)

alpha(Agree, check.keys=T)


Agree_m <- scale( apply(Agree, 1, mean), center = T, scale=T)[,]
Agree_sd <- scale( apply(Agree, 1, sd), center=T, scale=T)[,]

# Conscientiousness
Consc <- select(messy, Q14_9:Q14_12)

Consc$Q14_11 <- 8- Consc$Q14_11
Consc$Q14_12 <- 8- Consc$Q14_12

Consc2 <- Consc[complete.cases(Consc),]
cor(Consc2)

alpha(Consc, check.keys=T)


Consc_m <- scale( apply(Consc, 1, mean), center = T, scale=T)[,]
Consc_sd <- scale( apply(Consc, 1, sd), center=T, scale=T)[,]

# Neuroticism
Neuro <- select(messy, Q14_13:Q14_16)

Neuro$Q14_14 <- 8- Neuro$Q14_14
Neuro$Q14_16 <- 8- Neuro$Q14_16

Neuro2 <- Neuro[complete.cases(Neuro),]
cor(Neuro2)

alpha(Neuro, check.keys=T)


Neuro_m <- scale( apply(Neuro, 1, mean), center = T, scale=T)[,]
Neuro_sd <- scale( apply(Neuro, 1, sd), center=T, scale=T)[,]


# Openness
Open <- select(messy, Q14_17:Q14_20)

Open$Q14_18 <- 8- Open$Q14_18
Open$Q14_19 <- 8- Open$Q14_19
Open$Q14_20 <- 8- Open$Q14_20

Open2 <- Open[complete.cases(Open),]
cor(Open2)

alpha(Open, check.keys=T)


Open_m <- scale( apply(Open, 1, mean), center = T, scale=T)[,]
Open_sd <- scale( apply(Open, 1, sd), center=T, scale=T)[,]

# Honesty/Humility
Hum <- select(messy, Q14_21:Q14_24)

Hum$Q14_21 <- 8- Hum$Q14_21
Hum$Q14_22 <- 8- Hum$Q14_22
Hum$Q14_23 <- 8- Hum$Q14_23
Hum$Q14_24 <- 8- Hum$Q14_24

Hum2 <- Hum[complete.cases(Hum),]
cor(Hum2)

alpha(Hum, check.keys=T)


Hum_m <- scale( apply(Hum, 1, mean), center = T, scale=T)[,]
Hum_sd <- scale( apply(Hum, 1, sd), center=T, scale=T)[,]



# clean up reversals/demographics/etc
# recode belief in God to o=no, 1=yes, reverse church and prayer
messy$god_y <- (messy$god_y-2) * -1
messy$church <- 9- messy$church
messy$pray <- 8- messy$pray


# gender & orientation back to factors
messy$gender_ident <- factor(messy$gender_ident, levels = c(1, 2, 3, 4, 5, 6), labels = c('female', 'male', 'Tfemale', 'Tmale', 'nonconf', 'nolist'))
messy$sex_or <- factor(messy$gender_ident, levels = c(1, 2, 3, 4, 5, 6, 7, 8), labels = c('asexual', 'bisexual', 'gay', 'straight', 'lesbian', 'queer', 'unsure', 'nolist'))

# STEM to 1 = yes, 0 = no
messy$stem <- messy$stem - 1

# vote 2016 to 1 = yes
messy$vote2016 <- (messy$vote2016-2)*-1

# voted for...
messy$candidate2016 <- factor(messy$candidate2016, levels = c(1, 2, 3, 4, 5), labels = c('clinton', 'trump', 'third', 'non', NA))

# binary gender
female <- ifelse(messy$gender_ident == 'female', 1, 0)



# okay, get a usable data frame

go <- select(messy, CaseID, god_open, rel_groups, god_y:pct_nonbelieve, church, pray, gender_ident, sex_or, social_cons, econ_cons, vote2016, candidate2016, stem, age = PPAGE, educ = PPEDUC)


go <- data.frame(go, female, sbs_m, sbs_sd, crt, crtZ, pt_m, pt_sd, cred_m, cred_sd, mot_m, mot_sd, tfa_m, tfa_sd, eXtra_m, eXtra_sd, Consc_m, Consc_sd, Neuro_m, Neuro_sd, Agree_m, Agree_sd, Open_m, Open_sd, Hum_m, Hum_sd)

summary(go)

#save it

write.csv(go, 'go.csv')

