# 1/24/2022
library("readxl")

Example1 <- read_excel("C:/Users/Taylor/Documents/MATH 268/Data/Example1.xlsx")

head(Example1)
summary(Example1)
time = Example1$Time
hist(time)
summary(time)
boxplot(time)

sdT = sqrt(var(time))
sd(time)

fast1 = subset(Example1, Example1[,3]==1)
fast2 = subset(Example1, Example1[,3]==2)
fast3 = subset(Example1, Example1[,3]==3)

summary(fast1[,1])
summary(fast2[,1])
summary(fast3[,1])

summary(Example1[,1], Example1[,3])

Ex = data.frame(Example1)
boxplot(Ex[,1]~ Ex[,3])

# 1/26/2022

data(Titanic)
summary(Titanic)
ftable(Titanic)

Titanic1 <- margin.table(Titanic, 1)
#class distribution
Titanic1

Titanic2 <- margin.table(Titanic, c(4,1))
#gender distribution
Titanic2

Titanic3 <- margin.table(Titanic, 3)
Titanic3

Titanic4 <- margin.table(Titanic, c(3,1))
Titanic4

marginSums(Titanic1)
marginSums(Titanic2)

prop.table(Titanic1)
prop.table(Titanic2)

#1/31/2022
# one qualitative variable

# prop.test(x, n, p = NULL, alternative = "greater", conf.level= 0.95)
prop.test(470, 2201, p=0.25, alternative = "greater", conf.level=.95)


# two qualitative variables
res <- prop.test(x= c(490, 400), n = c(500, 500))

x = matrix(c(60,00,120,30,10,20,80,60,10,10), nrow=2, ncol=5, byrow=TRUE)
x

x[1,3]
x[2,4]
x[,2:4]
x[1,2:4]
x[c(1,3,5)]
chisq.test(x)

#2/2/2022 : Graphics

barplot(Titanic1, xlab = "class",
        ylab = "Frequency",
        main = "Individuals on the Titanic")

barplot(Titanic1, xlab = "class",
        horiz = TRUE,
        ylab = "Frequency",
        main = "Individuals on the Titanic")
# when using multiple variables us beside = TRUE to have bars next to eachother. instead of stacked
barplot(Titanic4, beside = TRUE, legend=c("Child", "Adult"))
pie(Titanic1, main = "Individuals on the Titanic")
par(mfrow = c(1,2))

barplot(Titanic4, beside = TRUE, ylim=c(0,1000), legend= c("child", "adult"))
barplot(Titanic2, beside = TRUE, ylim = c(0,2000))
legend("center",legend=legend)
legend

data("VADeaths")
x <- VADeaths[1:3, "Rural Male"]
barplot(y, horiz -TRUE)
barplot(y, names.arg = c("A", "B", "c"))
barplot(x, col = "purple", border = "black")
barplot(y, col = "pink", border = "green")
barplot(y, col = c("orange", "pink", "yellow"))

# Change border and fill color using one single color
barplot(x, col = "white", border = "steelblue")
# Change the color of border.
# Use different colors for each group
barplot(x, col = "white",
        border = c("#999999", "#E69F00", "#56B4E9"))
# Change fill color : single color
barplot(x, col = "steelblue")
# Change fill color: multiple colors
barplot(x, col = c("#999999", "#E69F00", "#56B4E9"))

# Change axis titles
# Change color (col = "gray") and remove frame
barplot(x, main = "Death Rates in Virginia",
        xlab = "Age", ylab = "Rate")

barplot(VADeaths,
        col = c("lightblue", "mistyrose", "lightcyan",
                "lavender", "cornsilk"),
        legend = rownames(VADeaths))
barplot(VADeaths,
        col = c("lightblue", "mistyrose", "lightcyan",
                "lavender", "cornsilk"),
        legend = rownames(VADeaths), beside = TRUE)
# Define a set of colors
my_colors <- c("lightblue", "mistyrose", "lightcyan",
               "lavender", "cornsilk")
# Bar plot
barplot(VADeaths, col = my_colors, beside = TRUE)
# Add legend
legend("topleft", legend = rownames(VADeaths),
       fill = my_colors, box.lty = 0, cex = 0.8)
legend(locator(1),paste("Class ",1:4),col=1:4 ,lty=1:4)

#02/07/2022 and 02/09/2022
#Summarizing quantitative variable
#measures of frequency (binomial prob distribution, Poisson dist, hypergeometric dist)
#measure of center (mean, median, mode, trimmed mean)
#measure of variability(range, variance, sd, IQR)
#measure of relative location (z-score, percentiles)

data(iris)
head(iris)
tail(iris)
names(iris)
dim(iris)

plength <- iris[,3]
summary(plength)

library(mosaic)
favstats(plength)
boxplot(plength)
favstats(plength~iris[,5])
stripchart(plength)
stripchart(plength, "jitter", col = "blue")
stripchart(plength~iris[,5], method = "stack", col = c("green", "red", "blue"))
boxplot(plength, col = "cyan")
boxplot(plength ~ iris[,5], main = "Side-by-Side
Boxplot")
favstats(plength ~ iris[,5])
favstats(plength)

hist(plength)
hist(plength, breaks = "Scott", freq = F)

par(mfrow = c(2.2))
hist(plength, breaks = 5, freq = F, main = "breaks = 5")
hist(plength, breaks = 20, freq = F, main = "breaks = 20")
hist(plength, breaks = 30, freq = F, main = "breaks = 30")
hist(plength, breaks = 50, freq = F, main = "breaks = 50")
plot(density.default(x = plength))

#02/14/2022
library(mosaic)
library(readxl)
campus_crime <- read_excel("C:/Users/Taylor/Desktop/MATH268/Data/Campus_Crime.xlsx")
ex <-data.frame(campus_crime)
ex1 <-data.matrix(campus_crime)

is.factor(ex$Private.School)
PS <-as.factor(ex$Private.School)
levels(PS)
attach(ex)
head(ex)
favstats(ex[,1])
favstats(Number.of.Crimes~PS)

private <- subset(ex, Private.School==1)
public <- subset(ex, Private.School==0)
fav_stats(private[,1])
fav_stats(public[,1])

boxplot(Number.of.Crimes~Private.School, main = "Crime by Type of School")
favstats(CP~PS)

CP=Number.of.Crimes/Total.Enrollment
boxplot(CP~Private.School, main = "Crime by Type of School")
names= c("Public", "Private")
legend("topright", names, lty=1.3)
legend(locator(1), names, lty=1.3)

library(UsingR)
data(emissions)
head(emissions)
CCC <- emissions$CO2
PCaptia <- emissions$perCapita
plot(PCapita, CCC, col = "red")
simple.scatterplot(PCaptia,CCC)
title("GDP/Capita vs CO2 emissions-1999")
detach(emissions)

# 02/16/2022
library(ggplot2)
library(manipulate)
x <- rnorm(100)
xx <- data.frame(c(1:100),x)
xx$c.1.100.
ggplot(xx,aes(x=xx$x)) + geom_histogram(bins=4)
ggplot(xx,aes(x=xx$x))+ geom_histogram(bins=8)
w = rnorm(100, 50,10)

# 02/21/2022
library(UsingR)
data(babies)
View(babies)
newB =  babies[,c(5,7,10)]
colnames(babies)
boxplot(newB[,1])
boxplot(newB[,2])
boxplot(newB[,3])

boxplot(newB[,1], newB[,2], newB[,3])
par(mfrow = c(1,3))

# remove the unknowns from columns 1 and 3
newB = subset(newB, (newB[,1] != 999) & (newB[,3] != 99))
max(newB[,1])

pairs(newB)

# add smoke column and remove unknowns
newB = babies[, c(5,7,10,21)]
newB = subset(newB, (newB[,1] != 999) & (newB[,3] != 99) & (newB[,4] != 9))
dim(newB)
max(newB)

# make smoke a factor
smoke = as.factor(newB[,4])
par(mfrow=c(1,1))
plot(newB[,1], newB[,2], pch = unclass(smoke))

#2/23/2022
# Pearson's Correlation: a measure of the strength of a linear relationship btwn two quantitative variables
# when two quantitative variables are normally distributed, a correlation = 0 , then the variables are independent.
# however when two variable are independent then the correlation = 0 regardless of distribution

data(state)
dimnames(state.x77)
colnames(state.x77)
rownames(state.x77)

illiteracy <-state.x77[,3]
murder <- state.x77[,5]
plot(illiteracy, murder)
head(state.x77)
plot(murder~illiteracy, col = "red", data= state.x77)

plot(illiteracy, murder, main = "Murder vs. Illiteracy", col = "red", pch = 16, xlim = c(0.2,3))
text(illiteracy, murder, labels = state.name)

state.region
unclass(state.region)
# used to seperate factors
ftable(state.region)

plot(illiteracy, murder, pch=unclass(state.region), xlim= c(0.2,3))
plot(illiteracy, murder, col=unclass(state.region), xlim= c(0.2,3), pch = 16)
plot(illiteracy, murder, col=unclass(state.region), pch=unclass(state.region), xlim= c(0.2,3), main = "Murder vs. Illiteracy Rates - US States")
legend("bottomright", levels(state.region), pch=1:4, col= 1:4)

identify(illiteracy, murder, state.name)

plto(age,weight)

# 3/2/2022
library(ggplot2)
data(mtcars)
head(mtcars)
is.data.frame(mycars)
names(mtcars)
df <- mtcars[,c("mpg","cyl","wt")]
# smoothing
qplot(mpg,wt,data=mtcars, col= factor(cyl), geom = c("point","smooth"))
# Linear fits by group
qplot(mpg, wt, data = mtcars, color = factor(cyl),
      geom=c("point", "smooth"))
# Change the color by a continuous numeric variable
qplot(mpg, wt, data = mtcars, colour = cyl)
# Change the color by groups (factor)
df <- mtcars
df[,'cyl'] <- as.factor(df[,'cyl'])
qplot(mpg, wt, data = df, colour = cyl)
# Add lines
qplot(mpg, wt, data = df, colour = cyl,
      geom=c("point", "line"))
# Change the size of points according to
# the values of a continuous variable
qplot(mpg, wt, data = mtcars, size = mpg)
# Change point shapes by groups
qplot(mpg, wt, data = mtcars, shape = factor(cyl))
# add text
qplot(mpg, wt, data = mtcars, label = rownames(mtcars),
      geom=c("point", "text"),
      hjust=0, vjust=0)
data(PlantGrowth)
head(PlantGrowth)
tapply(PlantGrowth$weight, PlantGrowth$group, mean)
shapiro.test(PlantGrowth$weight)
boxplot(PlantGrowth$weight~PlantGrowth$group)
# Basic box plot from data frame
qplot(group, weight, data = PlantGrowth,
      geom=c("boxplot"))
# Dot plot
qplot(group, weight, data = PlantGrowth,
      geom=c("dotplot"), stackdir = "center", binaxis = "y")
# Violin plot
qplot(group, weight, data = PlantGrowth,
      geom=c("violin"), trim = FALSE)
# Box plot from a data frame
# Add jitter and change fill color by group
qplot(group, weight, data = PlantGrowth,
      geom=c("boxplot", "jitter"), fill = group)
# Dot plot
qplot(group, weight, data = PlantGrowth,
      geom = "dotplot", stackdir = "center", binaxis = "y",
      color = group, fill = group)
# 03/07/2022
# Histogram and Density Plots
set.seed(1234)
mydata = data.frame(
  sex = factor(rep(c("F", "M"), each=200)),
  weight = c(rnorm(200, 55), rnorm(200, 58)))
head(mydata)
# Basic histogram
qplot(weight, data = mydata, geom = "histogram")
# Change histogram fill color by group (sex)
qplot(weight, data = mydata, geom = "histogram",
      fill = sex)
# Basic density plot
qplot(weight, data = mydata, geom = "density")
# Change density plot line color by group (sex)
# change line type
qplot(weight, data = mydata, geom = "density",
      color = sex, linetype = sex)
# Main title and axis labels
qplot(weight, data = mydata, geom = "density",
      xlab = "Weight (kg)", ylab = "Density",
      main = "Density plot of Weight")
# box plot
data(ToothGrowth)
# Convert the variable dose from a numeric to a factor variable
ToothGrowth$dose <- as.factor(ToothGrowth$dose)
head(ToothGrowth)
library(ggplot2)
# Basic box plot
p <- ggplot(ToothGrowth, aes(x=dose, y=len)) +
  geom_boxplot()
p
# Rotate the box plot
p + coord_flip()
# Notched box plot
ggplot(ToothGrowth, aes(x=dose, y=len)) +
  geom_boxplot(notch=TRUE)
# Change outlier, color, shape and size
ggplot(ToothGrowth, aes(x=dose, y=len)) +
  geom_boxplot(outlier.colour="red", outlier.shape=8,
               outlier.size=4)
# Box plot with mean points
p + stat_summary(fun=mean, geom="point", shape=23, size=4)
p + scale_x_discrete(limits=c("0.5", "2"))
# Box plot with dot plot
p + geom_dotplot(binaxis='y', stackdir='center', dotsize=1)
# Box plot with jittered points
# 0.2 : degree of jitter in x direction
p + geom_jitter(shape=16, position=position_jitter(0.2))
# Change box plot line colors by groups
p<-ggplot(ToothGrowth, aes(x=dose, y=len, color=dose)) +
  geom_boxplot()
p
# Use custom color palettes
p+scale_color_manual(values=c("#999999", "#E69F00", "#56B4E9"))
# Use brewer color palettes
p+scale_color_brewer(palette="Dark2")
# Use grey scale
p + scale_color_grey() + theme_classic()
# Use single color
ggplot(ToothGrowth, aes(x=dose, y=len)) +
  geom_boxplot(fill='#A4A4A4', color="black")+
  theme_classic()
# Change box plot colors by groups
p<-ggplot(ToothGrowth, aes(x=dose, y=len, fill=dose)) +
  geom_boxplot()
p
# Use custom color palettes
p+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
# use brewer color palettes
p+scale_fill_brewer(palette="Dark2")
# Use grey scale
p + scale_fill_grey() + theme_classic()
p + theme(legend.position="top")
p + theme(legend.position="bottom")
p + theme(legend.position="none") # Remove legend
# bocplot with mulitple groups
# Change box plot colors by groups
ggplot(ToothGrowth, aes(x=dose, y=len, fill=supp)) +
  geom_boxplot()
# Change the position
p<-ggplot(ToothGrowth, aes(x=dose, y=len, fill=supp)) +
  geom_boxplot(position=position_dodge(1))
p
# Add dots
p + geom_dotplot(binaxis='y', stackdir='center',
                 position=position_dodge(1))
# Change colors
p+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
# Basic box plot
ggplot(ToothGrowth, aes(x=dose, y=len)) +
  geom_boxplot(fill="gray")+
  labs(title="Plot of length per dose",x="Dose (mg)", y = "Length")+
  theme_classic()
# Change automatically color by groups
bp <- ggplot(ToothGrowth, aes(x=dose, y=len, fill=dose)) +
  geom_boxplot()+
  labs(title="Plot of length per dose",x="Dose (mg)", y = "Length")
bp + theme_classic()
# Basic violin plot
p <- ggplot(ToothGrowth, aes(x=dose, y=len)) +
  geom_violin()
p
# Rotate the violin plot
p + coord_flip()
# Set trim argument to FALSE
ggplot(ToothGrowth, aes(x=dose, y=len)) +
  geom_violin(trim=FALSE)
# violin plot with mean points
p + stat_summary(fun.y=mean, geom="point", shape=23, size=2)
# violin plot with median points
p + stat_summary(fun.y=median, geom="point", size=2, color="red")
# add median and quartile
p + geom_boxplot(width=0.1)
p <- ggplot(ToothGrowth, aes(x=dose, y=len)) +
  geom_violin(trim=FALSE)
p + stat_summary(fun.data="mean_sdl", mult=1,
                 geom="crossbar", width=0.2 )
p + stat_summary(fun.data=mean_sdl, mult=1,
                 geom="pointrange", color="red")
# violin plot with dot plot
p + geom_dotplot(binaxis='y', stackdir='center', dotsize=1)
# violin plot with jittered points
# 0.2 : degree of jitter in x direction
p + geom_jitter(shape=16, position=position_jitter(0.2))
# Change violin plot line colors by groups
p<-ggplot(ToothGrowth, aes(x=dose, y=len, color=dose)) +
  geom_violin(trim=FALSE)
p
# Use single color
ggplot(ToothGrowth, aes(x=dose, y=len)) +
  geom_violin(trim=FALSE, fill='#A4A4A4', color="darkred")+
  geom_boxplot(width=0.1) + theme_minimal()
# Change violin plot colors by groups
p<-ggplot(ToothGrowth, aes(x=dose, y=len, fill=dose)) +
  geom_violin(trim=FALSE)
p
# Change violin plot colors with multiple groups
ggplot(ToothGrowth, aes(x=dose, y=len, fill=supp)) +
  geom_violin()
# Change the position
p<-ggplot(ToothGrowth, aes(x=dose, y=len, fill=supp)) +
  geom_violin(position=position_dodge(1))
p
# Add dots
p + geom_dotplot(binaxis='y', stackdir='center',
                 position=position_dodge(1))
# Change colors
p+scale_fill_manual(values=c("#999999", "#E69F00", "#56B4E9"))
# customized violen plots
# Basic violin plot
ggplot(ToothGrowth, aes(x=dose, y=len)) +
  geom_violin(trim=FALSE, fill="gray")+
  labs(title="Plot of length by dose",x="Dose (mg)", y = "Length")+
  geom_boxplot(width=0.1)+
  theme_classic()
# Change color by groups
dp <- ggplot(ToothGrowth, aes(x=dose, y=len, fill=dose)) +
  geom_violin(trim=FALSE)+
  geom_boxplot(width=0.1, fill="white")+
  labs(title="Plot of length by dose",x="Dose (mg)", y = "Length")
dp + theme_classic()
# ggextra
library(ggExtra)
df <- data.frame(x = rnorm(1000, 50, 10), y = rnorm(1000, 50, 10))
p <- ggplot(df, aes(x, y)) + geom_point() + theme_classic()
ggExtra::ggMarginal(p, type = "histogram")
