library(haven)
library(ggplot2)
library(sjPlot)

rm(list = ls())

# set working directory
setwd("~/Dropbox/hsf/courses_202/R/github/R-Intro")

# load data from Stata format
classdata <- read_dta("~/Dropbox/hsf/courses_202/destat/classdata/classdata.dta")

# or from csv data
classdata <- read.csv("~/Dropbox/hsf/courses_202/destat/classdata/classdata.csv")

head(classdata)

# first look at data
ggplot(classdata, aes(x=height, y=weight, shape = sex)) +
  geom_point( aes(size = siblings)) 

## baseline model
model  <- lm(weight ~ height + sex , data = classdata )
interw <- model$coefficients[1]+model$coefficients[3] 
interm <- model$coefficients[1] 
slope  <- model$coefficients[2]

ggplot(classdata, aes(x=height, y=weight, shape = sex)) +
  geom_point( aes(size = 2)) +
  stat_smooth(formula = y ~ x,  method = "lm", se = FALSE, colour = "red", linetype = 1) +
  geom_abline(slope = slope, intercept = interw, linetype = 2, size=1.5)+
  geom_abline(slope = slope, intercept = interm, linetype = 2, size=1.5) +
  geom_abline(slope = coef(model)[[2]], intercept = coef(model)[[1]]) 

m1 <- lm(weight ~ height , data = classdata )
m2 <- lm(weight ~ height + sex , data = classdata )
m3 <- lm(weight ~ height + sex + height * sex , data = classdata )
m4 <- lm(weight ~ height + sex + height * sex + siblings , data = classdata )
m5 <- lm(weight ~ height + sex + height * sex , data = subset(classdata, siblings < 4 ))

tab_model(m1, m2, m3, m4, m5,
          p.style = "stars",
          p.threshold = c(0.2, 0.1, 0.05),
          show.ci = FALSE, 
          show.se = FALSE) 

plot(residuals(m4), fitted(m4))
plot(residuals(m4), classdata$siblings)





library(ggpmisc)
library(memisc)

library(sjmisc)
library(sjlabelled)



plot(resid, fit, data = classdata)

par(mfrow = c(2,2))



plot(m6, which = 2)
plot(m6, which = 4)

smtable123 <- mtable('Model 1' = m1,
                    'Model 2' = m2,
                    'Model 3' = m3, 
                    summary.stats=c("sigma","R-squared","F","p","N"))

mtable123



 # identify outliers
classdata$resid <-  abs(residuals(m6)) / fitted(m6)


  
  #geom_label_repel(data=subset(classdata, resid>.09), aes(label = sex), box.padding = 0.35, point.padding = 0.5, segment.color = 'grey50') +
    
  
  
  





stat_regline_equation(
  mapping = NULL,
  data = NULL,
  formula = y ~ x,
  label.x.npc = "left",
  label.y.npc = "top",
  label.x = NULL,
  label.y = NULL,
  output.type = "expression",
  geom = "text",
  position = "identity",
  na.rm = FALSE,
  show.legend = NA,
  inherit.aes = TRUE,
  ...
)



library(plyr)
library(lattice)
library(ggrepel)

xyplot(weight ~ height, group= sex, data = classdata,
             main = "My customized theme")


with(classdata[classdata$sex=="w",], 
     plot(height, weight,
     group= sex
     ))




y <- weight
x <- height
sex <- sex

ym <- subset(classdata, sex=="m", select=c(weight, height))
yw <- subset(classdata, sex=="w", select=c(weight, height))




#View(model)
fit <- fitted(model)


plot(height, weight, main = "Main title",
     xlab = "Height (cm)", ylab = "Weight (kg)",
     pch = 0, frame = TRUE,
     group=sex,
     data=subset(classdata, sex=="w"))
points(x, cex = .5, col = "dark red")
abline(a=interw, b=slope, col="red", type="l", lty=2)
abline(a=interm, b=slope, col="blue", type="l")

filter(sex == "Droid")
abline(mC <- lm(dist ~ 1, data = cars)) ## the same as



myplot <- ggplot(, aes(x=x, y=y, shape=sex)) +
  geom_point( size = 3) +
  theme_classic()
  
myplot %+% subset(,  %in% c("P1","P3"))




%+% subset(df, ID %in% c("P1","P3"))


# Add regression line

abline(lm(weight ~ height, data = classdata), col = "blue")

fitm <- fit %>% 
  filter()
height 

abline(), data = classdata), col = "red")
