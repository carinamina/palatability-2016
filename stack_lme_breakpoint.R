df = read.csv("https://raw.githubusercontent.com/carinamina/palatability-2016/master/stack_lme_breakpoint.csv", header=TRUE)

#The experiment was measuring leaf mass for plants from populations collected at ten different latitudes. I measured multiple individuals per maternal line, and multiple maternal lines per population. There are not replicate populations per latitude. So I need to have some nested random effects to block plants by maternal lines nested in populations. 
#I am interested in whether there is a latitudinal threshold in the response.

###########
#plot of population means and SE (based on number of maternal lines per population)
library(plyr)
mass_line <- ddply(df, c("line","lat","pop"), summarise,
                   line_mean = mean(mass)
)
mass_pop <- ddply(mass_line, c("lat","pop"), summarise,
                  pop_mean = mean(line_mean),
                  N = length(pop),
                  sd = sd(line_mean),
                  se = sd/sqrt(N)
)

library(ggplot2)
ggplot(mass_pop,aes(x=lat, y=pop_mean))+  geom_point(size=3)  + geom_errorbar(aes(ymin=pop_mean-se, ymax=pop_mean+se),width=.2,size=.4) + xlab("Latitude N")+ylab("Mass (mg)") + theme_bw()

###########
#linear and breakpoint models using lmer
library(lme4) 

#linear model in lmer
linear <- lmer(mass ~ lat + (1 | pop/line), data = df)
plot(linear)
#uh-oh, very fan-shaped residual plot. We need to deal with heterogeneity of variance.

#breakpoint model in lmer
bp = 30
b1 <- function(x, bp) ifelse(x < bp, x, 0)
b2 <- function(x, bp) ifelse(x < bp, 0, x)
breakpoint <- lmer(mass ~ b1(lat, bp) + b2(lat, bp) + (1 | pop/line), data = df)
plot(breakpoint)

###########
#linear and breakpoint models using lme
library(nlme)

#linear model in lme
#ctrl is a fix for model not converging (which is not a problem when I use the real thing with the age interaction)
ctrl <- lmeControl(opt='optim')
linear2 <- lme(mass ~ lat , random=~1|pop/line, na.action = na.exclude, data=df, control = ctrl, weights=varIdent(form=~1|pop))
plot(linear2)
#this fixes the fan-shaped residuals, yay!

#breaking breakpoint model in lme
breakpoint2 <- lme(mass ~ b1(lat, bp) + b2(lat, bp), random=~1|pop/line, na.action = na.exclude, data=df, control = ctrl, weights=varIdent(form=~1|pop))
