library(tidyverse)
library(MASS)
library(lmtest)


race <- c(1,1,0,0)
azt <- c(1,0,1,0)
y1 <- c(14,32,11,12)
y2 <- c(93,81,52,43)
azt.fit <- glm(cbind(y1,y2)~race+azt,
               family = binomial(link = "logit"))
summary(azt.fit)


# test for conditional independence
drop1(azt.fit, test = "Chisq")
lrt_stat <- 6.8709-.0371

crab <- read.table("http://www.stat.ufl.edu/~aa/cat/data/Crabs.dat", header=TRUE)


crab.fit1 <- glm(y ~ w,
                 family=binomial,
                 data = crab2)
summary(crab.fit1)

crab.fit3 = glm(y~col+w,
                family = binomial,
                data = crab2)
summary(crab.fit3)

#comparing models
anova(crab.fit1, crab.fit3, test = "Chisq")

#Quantitative treatment of ordinal variables

is.factor(crab2$C)
is.factor(crab2$col)

crab.fit4 <- glm(y ~ C + w,
                 family = binomial,
                 data = crab2)
summary(crab.fit4)

crabs2$dark <- as.numeric(crab2$c != 5)

crab.fit5 <- glm(y~dark+w+ dark*w,
                 family = binomial,
                 data = crab2)
summary(crab.fit5)

crab.col <- glm(y~col+w+col*w,
                family = binomial,
                data = crab2)

# Is interaction term important
crab.fit6 <- glm(y~dark + w,
                 family = binomial,
                 data = crab2)
anova(crab.fit6, crab.fit5, test = "Chisq")
  # pvalue > 0.05, fail to reject to null
  # no evidence of interaction

# summarizing the effect
crab2$c4 <- ifelse(crab2$C == 4, 1, 0)
fit.dark <- glm(y~w+c4,
                family = binomial,
                data = crab2)
summary(fit.dark)

# predict at the mean width

predict(fit.dark, data.frame(c4 = 0, width = mean(crab2$w)),
                             type = "response")
# fix c4 at 1 predict at the quantiles of width
predict(fit.dark, data.frame(c4 = 1, width = quantile(crab2$w)),
        type = "response")
