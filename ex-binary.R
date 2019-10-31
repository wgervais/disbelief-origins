
# packages ----
library(rethinking)
library(tidyverse)
library(ggridges)

# get data ----
go <- read.csv('go.csv')

go$atheist <- go$god_y*-1 + 1
go$ment <- go$pt_m
go$cred <- go$cred_m
go$security <- go$mot_m * -1
go$reflection <- go$crtZ
go$ageZ <- scale(go$age, center=T, scale=T)[,]
go$educZ <- scale(go$educ, center=T, scale=T)[,]
go$femZ <- scale(go$female, scale=T, center = T)[,]
go$econ_consZ <- scale(go$econ_cons, center=T, scale=T)[,]
go$social_consZ <- scale(go$social_cons, center=T, scale=T)[,]
summary(go)




# just CRED and CRT ----
d <- select(go, 
            atheist, 
            cred,
            reflection)
summary(d)
nrow(d)
# handy functions ----


bigger <- function(first, second) {
  pr.big <- mean(ifelse(first > second, 1, 0))
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




#exploration zone

#


#  E1.	how well can CRT, CREDs, and their interaction predict atheism.----


# crt * cred, w/o covariates 

d <- d[complete.cases(d),]
summary(d)
nrow(d)



athNC <- map2stan(
  alist(
    atheist ~ dbinom( 1 , p ),
    logit(p) <- a + reflection*( b_ref + b_int*cred) + b_cred*cred,
    c(a, b_ref, b_cred, b_int) ~ dnorm(0,2)
  ), data=d, WAIC=T, chains=1, cores=1, iter=22000, warmup=1000
)

plot(athNC)
precis(athNC)

intcc <- data.frame(extract.samples(athNC))



# plotting interaction ----
# separate lines for CREDs, x-axis is crt
# for predictionn: prediction = reflection * (b_ref + b_int * cred) + b_cred * cred + a
# intercept mean CRED (cred = 0, reflection = 0):   a
# slope mean CRED (cred = 0): b_ref
# intercept minimum CRED (cred = leastcred, reflection = 0) = b_cred * leastcred + a
# slope minimum CRED (cred = leastcred) = b_ref + b_int * leastcred
# intercept maximum CRED (cred = mostcred) = b_cred * mostcred + a
# slope maximum CRED (cred = mostcred) = b_ref + b_int * mostcred

leastcred <- min(d$cred)
mostcred <- max(d$cred)


meanCRED_a <- intcc$a
meanCRED_b <- intcc$b_ref
minCRED_a <- intcc$a + intcc$b_cred*leastcred
minCRED_b <- intcc$b_ref + intcc$b_int*leastcred
maxCRED_a <- intcc$a + intcc$b_cred*mostcred
maxCRED_b <- intcc$b_ref + intcc$b_int*mostcred


mean.int <- logistic(meanCRED_a)
mean.slope <- logistic(meanCRED_a + meanCRED_b) - logistic(meanCRED_a)

min.int <- logistic(minCRED_a)
min.slope <- logistic(minCRED_a + minCRED_b) - logistic(minCRED_a)

max.int <- logistic(maxCRED_a)
max.slope <- logistic(maxCRED_a + maxCRED_b) - logistic(maxCRED_a)


cred_crt <- data.frame(mean.int, mean.slope, min.int, min.slope, max.int, max.slope)
summary(cred_crt)

# get means for lines

mean.i <- mean(cred_crt$mean.int)
mean.s <- mean(cred_crt$mean.slope)
min.i <- mean(cred_crt$min.int)
min.s <- mean(cred_crt$min.slope)
max.i <- mean(cred_crt$max.int)
max.s <- mean(cred_crt$max.slope)

# get 100 smaples of each

cred_crt <- cred_crt[sample(1:nrow(cred_crt)), ] #shuffle the deck

mean.100int <- c(cred_crt$mean.int[1:100])
mean.100slope <- c(cred_crt$mean.slope[1:100])

min.100int <- c(cred_crt$min.int[1:100])
min.100slope <- c(cred_crt$min.slope[1:100])

max.100int <- c(cred_crt$max.int[1:100])
max.100slope <- c(cred_crt$max.slope[1:100])

# to plot this shizzle

d1 <- data.frame(x=c(min(d$reflection), 0, max(d$reflection)))

brange <- max(d$reflection) - min(d$reflection)

decs <- seq(from=0, to=1, by=.33)

b.breaks <- min(d$reflection) + decs*brange


meancol <- 'black'
mincol <- 'purple'
maxcol <- 'hotpink'


ggplot(d1, aes(x=x)) +
  theme_bw() +
  scale_y_continuous(limits=c(0, 1)) +
  scale_x_continuous(limits=c(min(d$reflection)+.2, max(d$reflection))-.1,breaks=b.breaks, labels=c(0, 3, 6, 9)) +
  labs(x="Cognitive Reflection\n(correct CRT items)", y="Atheist Probability\n") +
  geom_abline(slope=mean.100slope, intercept=mean.100int, lwd=.2, col=meancol, alpha=.15) +
  geom_abline(slope=min.100slope, intercept=min.100int, lwd=.2, col=mincol, alpha=.15) +
  geom_abline(slope=max.100slope, intercept=max.100int, lwd=.2, col=maxcol, alpha=.15) +
  geom_abline(slope=c(mean.s, min.s, max.s), intercept=c(mean.i, min.i, max.i), col=c(meancol, mincol, maxcol), lwd=1.2) +
  annotate("text", x = 0.3, y = c(.1, 0.3, .7), label = c("Max CRED", "Mean CRED", "Min CRED"), hjust=0, vjust=0, size=6, color=c(maxcol, meancol, mincol), family = "Times") +
  theme_bw() +
  theme(text = element_text(family="Times"),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid.major.x = element_blank() ,
        panel.grid.minor.x = element_blank() ,
        panel.grid.minor.y = element_blank() ,
        panel.border = element_rect(color="darkgrey"),
        plot.margin = unit(c(1, 1, 1, 1), "lines")) 




# now with covariates ----


d2 <- select(go, 
             atheist, 
             ment, 
             security, 
             cred, 
             reflection,
             ageZ, educZ, femZ, econ_consZ, social_consZ, eXtra_m, Consc_m, Neuro_m, Agree_m, Open_m, Hum_m)


d2 <- d2[complete.cases(d2),]
summary(d2)
nrow(d2)






ath <- map2stan(
  alist(
    atheist ~ dbinom( 1 , p ),
    logit(p) <- a + reflection*( b_ref + b_int*cred) + b_cred*cred + b_ment*ment + b_sec*security + b_age*ageZ + b_educ*educZ + b_female*femZ + b_social_cons*social_consZ + b_econ_cons*econ_consZ + b_eXtra*eXtra_m + b_Consc*Consc_m + b_Neuro*Neuro_m + b_Agree*Agree_m  + b_Open*Open_m + b_Hum*Hum_m,
    c(a, b_ref, b_cred, b_int, b_sec, b_ment, b_age, b_educ, b_female, b_social_cons, b_econ_cons, b_eXtra, b_Consc, b_Neuro, b_Agree, b_Open, b_Hum) ~ dnorm(0,2)
  ), data=d2, chains=1, cores=1, iter=22000, warmup=1000
)

plot(ath)
precis(ath)



Cintcc <- data.frame(extract.samples(ath))



# plotting interaction ----
# separate lines for CREDs, x-axis is crt
# for predictionn: prediction = reflection * (b_ref + b_int * cred) + b_cred * cred + a
# intercept mean CRED (cred = 0, reflection = 0):   a
# slope mean CRED (cred = 0): b_ref
# intercept minimum CRED (cred = leastcred, reflection = 0) = b_cred * leastcred + a
# slope minimum CRED (cred = leastcred) = b_ref + b_int * leastcred
# intercept maximum CRED (cred = mostcred) = b_cred * mostcred + a
# slope maximum CRED (cred = mostcred) = b_ref + b_int * mostcred

Cleastcred <- min(d2$cred)
Cmostcred <- max(d2$cred)


CmeanCRED_a <- Cintcc$a
CmeanCRED_b <- Cintcc$b_ref
CminCRED_a <- Cintcc$a + Cintcc$b_cred*Cleastcred
CminCRED_b <- Cintcc$b_ref + Cintcc$b_int*Cleastcred
CmaxCRED_a <- Cintcc$a + Cintcc$b_cred*Cmostcred
CmaxCRED_b <- Cintcc$b_ref + Cintcc$b_int*Cmostcred


Cmean.int <- logistic(CmeanCRED_a)
Cmean.slope <- logistic(CmeanCRED_a + CmeanCRED_b) - logistic(CmeanCRED_a)

Cmin.int <- logistic(CminCRED_a)
Cmin.slope <- logistic(CminCRED_a + CminCRED_b) - logistic(CminCRED_a)

Cmax.int <- logistic(CmaxCRED_a)
Cmax.slope <- logistic(CmaxCRED_a + CmaxCRED_b) - logistic(CmaxCRED_a)


Ccred_crt <- data.frame(Cmean.int, Cmean.slope, Cmin.int, Cmin.slope, Cmax.int, Cmax.slope)
summary(Ccred_crt)

# get means for lines

Cmean.i <- mean(Ccred_crt$Cmean.int)
Cmean.s <- mean(Ccred_crt$Cmean.slope)
Cmin.i <- mean(Ccred_crt$Cmin.int)
Cmin.s <- mean(Ccred_crt$Cmin.slope)
Cmax.i <- mean(Ccred_crt$Cmax.int)
Cmax.s <- mean(Ccred_crt$Cmax.slope)

# get 100 smaples of each

Ccred_crt <- Ccred_crt[sample(1:nrow(Ccred_crt)), ] #shuffle the deck

Cmean.100int <- c(Ccred_crt$Cmean.int[1:100])
Cmean.100slope <- c(Ccred_crt$Cmean.slope[1:100])

Cmin.100int <- c(Ccred_crt$Cmin.int[1:100])
Cmin.100slope <- c(Ccred_crt$Cmin.slope[1:100])

Cmax.100int <- c(Ccred_crt$Cmax.int[1:100])
Cmax.100slope <- c(Ccred_crt$Cmax.slope[1:100])

# to plot this shizzle

Cd1 <- data.frame(x=c(min(d2$reflection), 0, max(d2$reflection)))

Cbrange <- max(d2$reflection) - min(d2$reflection)

Cdecs <- seq(from=0, to=1, by=.33)

Cb.breaks <- min(d2$reflection) + Cdecs*Cbrange


meancol <- 'black'
mincol <- 'purple'
maxcol <- 'hotpink'


ggplot(Cd1, aes(x=x)) +
  theme_bw() +
  scale_y_continuous(limits=c(0, 1)) +
  scale_x_continuous(limits=c(min(d2$reflection)+.2, max(d2$reflection))-.1, breaks=Cb.breaks, labels=c(0, 3, 6, 9)) +
  labs(x="Cognitive Reflection\n(correct CRT items)", y="Atheist Probability\n") +
  geom_abline(slope=Cmean.100slope, intercept=Cmean.100int, lwd=.2, col=meancol, alpha=.15) +
  geom_abline(slope=Cmin.100slope, intercept=Cmin.100int, lwd=.2, col=mincol, alpha=.15) +
  geom_abline(slope=Cmax.100slope, intercept=Cmax.100int, lwd=.2, col=maxcol, alpha=.15) +
  geom_abline(slope=c(Cmean.s, Cmin.s, Cmax.s), intercept=c(Cmean.i, Cmin.i, Cmax.i), col=c(meancol, mincol, maxcol), lwd=1.2) +
  annotate("text", x = 0.3, y = c(.1, 0.3, .7), label = c("Max CRED", "Mean CRED", "Min CRED"), hjust=0, vjust=0, size=6, color=c(maxcol, meancol, mincol), family = "Times") +
  theme_bw() +
  theme(text = element_text(family="Times"),
        axis.text = element_text(size = 16),
        axis.title = element_text(size = 20),
        panel.background = element_blank(),
        plot.background = element_blank(),
        panel.grid.major.x = element_blank() ,
        panel.grid.minor.x = element_blank() ,
        panel.grid.minor.y = element_blank() ,
        panel.border = element_rect(color="darkgrey"),
        plot.margin = unit(c(1, 1, 1, 1), "lines")) 





