# 03/23/2022
data("mtcars")
library(ggplot2)
attach(mtcars)

p <- ggplot(mtcars, aes(x=wt, y=mpg, color = as.factor(cyl), shape =as.factor(cyl)))
p+geom_smooth(method = lm, se = FALSE, fullrange = T)
p+geom_smooth(method = lm, se = FALSE, fullrange = F)

ggplot(mtcars,aes(x=wt, y=mpg, color = as.factor(cyl), shape =as.factor(cyl)))+
  geom_point() + geom_smooth(method = lm, aes(fill = as.factor(cyl)))

 q <- ggplot(faithful, aes(waiting, eruptions, color = eruptions >3))+
   geom_point()+
   stat_ellipse()
q + stat_ellipse(type = "norm")
q + stat_ellipse(type = "t")
q + stat_ellipse(type = "euclid")

# generating random numbers from a normal dist with mean = -1 and var =1
set.seed(1234)
w = rnorm(20,mean = 75, sd=100)
round(w)
summary(w)
sd(w)
x <- c(rnorm(500, mean = 65, 1.2), rnorm(1500, 72, 1.2))
y <- c(rnorm(500,1), rnorm(500, 1.7))

plot(density(x))

group <- as.factor(rep(c(1,2), each = 500))

df <- data.frame(x,y,group)
head(df)
scatterPlot = ggplot(df, aes(x,y,color=group))+
  geom_point()
scatterPlot
xdensity <- ggplot(df, aes(x,fill=group))+
  geom_density(alpha=0.5)
xdensity

# 04/04/2022
library(readxl)
dioxin <- read_excel("C:/Users/Taylor/Desktop/MATH268/Data/Dioxin.xlsx")

Site = as.factor(dioxin$site)
dyear = c(Year, Year)
TEQA = dioxin$Sitea
TEQB = dioxin$Siteb
TEQ = c(TEQA, TEQB)
library(ggplot2)

dat <- as.data.frame(cbind(dyear, TEQ, Site))
lm(TEQA~Year)
lm(TEQB~Year)
p = ggplot(dat, aes(dyear,TEQ, col = as.factor(site)))+
  geom_point()
p+ geom_abline(intercept=15920, 64, slope=-7.9, col="red", linetype = "solid", size = .5)+
  geom_abline(intercept = 11295.146, slope = -5.61, col= "blue", linetype = "solid", size = .5)+
  annotate("text",x=2000,y=150, label="y=15920,64-7.9*Year, Site A", col ="red")+
  annotate("text",x=2000,y=160, label="y=11295.146-5.6*Year, Site B", col ="blue")
p= ggplot( dat, aes(dyear, TEQ, col = as.factor(Site)))+
  geom_point()+
  geom_smooth(method =lm, se= FALSE)+
  annotate("text",x=2000,y=150, label="y=15920,64-7.9*Year, Site A", col ="red")+
  annotate("text",x=2000,y=160, label="y=11295.146-5.6*Year, Site B", col ="blue")
# 04/06/2022
before = c(5,3,9,7,6,4)
after = c(7,8,8,9,9,7)
t.test(after, before, alternative = "greater", paired = TRUE, conf.level = .95)

# 04/11/2022
left = scan()
216 198 165 158 173 184 166 173 179 175 212

right = scan()
16 194 163 154 170 180 163 168 180 175 209
mean(left)-mean(right)
t.test(left, right, alternative = "two.sided", paired = TRUE, conf.level = .95)
t.test(left, right, alternative = "greater", paired = TRUE, conf.level = .90)

fert_a = scan()

library(dpylr)
library(ISLR)

View(Credit)
 ggplot(Credit, aes(x= Income, y= Balance)) +
   geom_point(color = "orange")
cor(Credit$Income, Credit$Balance)

model <- lm(Balance ~ Income, Credit)
pre <- predict.lm(model)
head(pre)
tail(pre)
res <- residuals(model)
head(res)
tail(res)

plot(Credit$Income, res)+
  abline(h=0)

summary(model)

credit.data <- Credit %>% 
  select(Income, Limit, Rating, Cards, Age, Education, Balance)
credit.data %>% 
  cor()
model2<- lm(Balance ~., Credit)
library(MASS)
AIC.model <- stepAIC(model2, direction = "both", trace = FALSE)


library(readr)
space_shuttle_data <- read_csv("Data/space shuttle data.csv")
data <- space_shuttle_data

distress <- data$distress
temp <- data$temp
head(data)
mod = glm(distress~temp, binomial)
summary(mod)$coefficients