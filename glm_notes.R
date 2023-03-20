#Generalized Linear Model

## Linear probability model
# ex: alcohol consumptions and organ malformation
# dv: infant organ malformation (binary)
# iv: mother's alc consumption (avg drinks per day)
# model prob of organ malformation as a linear function of alc consumption

malform <- matrix(c(48, 17088, 38, 14464, 5,
                    788, 1, 126, 1, 37),
                  ncol = 2, byrow = TRUE)
x <- c(0, 0.5, 1.5,4,7)

malform.lin <- glm(malform ~ x,
                   family = binomial(link = make.link("identity")))
summary(malform.lin)
# pvalue > pi
# reject null that alc consuption affects malformation
# not true 
# not the right model
# do not use linear model to predict probabilities

malform.log <- glm(malform ~ x,
                   family = binomial)
summary(malform.log)

# pvalue > 0 therefore the two variables are related
# positive estimate therefore prob(x) and x have positive relationship
# good for showing relationship of probabilities 


## Poisson log linear models
## Silicon wafers examples

A <- c(8,7,6,6,3,4,7,2,3,4)
B <- c(9,9,8,14,8,13,11,5,7,6)
trt <- factor(rep(c("A","B"), each = 10))

wafer <- data.frame(trt = trt, defect = c(A,B))
wafer

## Poisson linear regression

wafer.lin <- glm(defect ~ trt, data = wafer,
                 family = poisson(link = "identity"))
summary(wafer.lin)

## Log-linear regression

wafer.loglin <- glm(defect ~ trt, data = wafer,
                    family = poisson(link = "log"))
wafer.loglin

## Example: Female Horseshoe crabs
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
crabs <- read.table('../MATH327/data/Crabs.dat', header = T)
summary(crabs)

plot(sat ~ width, data = crabs)

crabs.fit <- glm(sat~width, data = crabs,
                 family = poisson(link = "log"))
summary(crabs.fit)
