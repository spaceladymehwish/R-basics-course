2+3
x <- 2
x
y <- 5
x+y
print(x)
install.packages("readxl")
library(readxl)
library(readxl)
library(readxl)
Book1 <- read_excel("Book1.xlsx", sheet = "Sheet1", 
                    range = "A1:C9")
View(Book1)
str(Book1)
head(Book1)
tail(Book1)

boxplot((book1))
boxplot(Book1)

boxplot(Book1$height, Book1$weight)
boxplot(Book1$height ~ Book1$crop)

library(readxl)
Book1 <- read_excel("Book1.xlsx", sheet = "Sheet1", 
                    range = "A1:C9", col_types = c("text", 
                                                   "numeric", "numeric"))
View(Book1)

#strip chart
stripchart(Book1$height)
stripchart(Book1$height ~ Book1$weight)

#histogram
hist(Book1$height)
hist(Book1$weight)

#plot
plot(Book1$height, Book1$weight)

#scatterplot
plot(Book1$height ~ Book1$weight)

#qqnorm plot
qqnorm(Book1$height)

#barplot
barplot(Book1$height)

#mosaicplot
mosaicplot(Book1$height)
mosaicplot(Book1$height ~ Book1$weight)

#boxplot
boxplot(Book1$height ~ Book1$weight)
boxplot(Book1$height)

boxplot(Book1$height ~ Book1$crop)

#labeling
boxplot(height ~ crop, data = Book1, main = "height of the crop")
boxplot(height ~ crop, data = Book1, main = "height of the crop", 
        xlab = "crop type", 
        ylab = "height(cm)")

#coloring
boxplot(height ~ crop, data = Book1, main = "height of the crop", 
        xlab = "crop type", 
        ylab = "height(cm)", 
        col="yellow", border = "grey")

#grouping of treatment
data2 <- read_excel("book1.xlsx", sheet = "Sheet2", 
                    col_types = c("text", "numeric", "numeric", 
                                  "text"))
View(data2)

boxplot(data2$height ~ data2$crop)
boxplot(data2$height ~ data2$crop * data2$water,
        main = "experiment",
        xlab = "crop type",
        ylab = "height")

data2$crop <- factor(data2$crop, levels = c("wheat", "maize", "rice"))

str(data2)
mean(data2$height)
mean(data2$weight)
median(data2$height)
median(data2$weight)
min(data2$height)
max(data2$height)
range(data2$height)
quantile(data2$height, 0.25)
quantile(data2$height, 0.75)
sd(data2$height)
var(data2$height)
lapply(data2[, 2:3], mean)
lapply(data2[, 2:3], median)
lapply(data2[, 2:3], var)
summary(data2)

#anova
aov(data2$height ~ data2$crop)
anova <- aov(data2$height ~ data2$crop)
summary(anova)

#tukey test post hocs
TukeyHSD(anova)

#group anova
anova2 <- aov(data2$height ~ data2$crop * data2$water)
summary(anova2)
TukeyHSD(anova2)

library(readxl)
x <- read_excel("book1.xlsx", sheet = "Sheet2", col_types = c("text", "numeric", "numeric", "text"))
View(x)

#boxplot
boxplot(x$height ~ x$crop)

#multiple comparison of means

#tukey hsd test

install.packages("agricolae")
library(agricolae)
help("agricolae-package")
?`agricolae-package`

model <- aov(height ~ crop, data = x)
model2 <- aov(x$height ~ x$crop)
summary(model2)
out <- HSD.test(model, "crop", group = TRUE, console = TRUE, main = "Tukey test")
plot(out)

#LSD test
model <- aov(height ~ crop, data = x)
out <- LSD.test(model, "crop", group = TRUE, console = TRUE, main = "LSD test")
plot(out)

#duncan test
model <- aov(height ~ crop, data = x)
out <- duncan.test(model, "crop", group = TRUE, console = TRUE, main = "Duncan test")
plot(out, las = 2)

#box plot
boxplot(height ~ crop * water, data = x)
model1 <- aov(height ~ crop * water, data = x)
out <- HSD.test(model1, c("crop", "water"), group = TRUE, console = TRUE,
                main = "Tuckey test")
plot(out, horiz = TRUE, las = 1)
plot(out, horiz = TRUE, las = 2)

#how to save images in high quality

jpeg(file = "tukey test.tiff",
     width = 6,
     height = 4,
     units = "in",
     res = 300)
  # input the plot snippet that you want to save 
boxplot(height ~ crop * water, data = x, las = 2, xlab = "")
dev.off()

#installing package
install.packages("ggplot2")
library(ggplot2)

library(readxl)
x <- read_excel("book1.xlsx", sheet = "Sheet2", col_types = c("text", "numeric", "numeric", "text"))
ggplot(data = x, mapping = aes(x= crop, y=height)) + geom_point()

ggplot(x, aes(crop, height)) + geom_point(size=5)+
  geom_line()

ggplot(x, aes(crop, height))+
  geom_point(size=3)

ggplot(x, aes(crop, height))+
  geom_boxplot()

ggplot(x, aes(crop, height))+
  geom_boxplot()+
  geom_point(size=2, colour="#ff0000", alpha=0.5)

ggplot(x, aes(crop, height, color=water))+
  geom_boxplot()


ggplot(x, aes(crop, height, fill=water))+
  geom_boxplot()

#dividing on basis of type of water
ggplot(x, aes(crop, height, fill=water))+
  geom_boxplot()+
  facet_wrap(~water)+
  labs(x="crop", y="height", title = "crop growth")+
  theme_bw()+
  coord_flip()+
  ggsave("ggplot1.tiff", units = "in", width = 8, 
         height = 6, dpi = 300, compression = 'lzw')

data()
data()
View(CO2)
View(iris)
View(PlantGrowth)
View(mtcars)
head(iris)
iris3
head(iris3)
names(iris3)
names(co2)
names(CO2)
names(iris)
nrow(iris)
ncol(iris)

install.packages('writexl')
library(writexl)
library(ggplot2)

#saving a dataset in xl format
write_xlsx(iris, path= "D:\\Others\\R basics course\\iris.xlsx")

ggplot(iris, aes(Sepal.Length, Sepal.Width, color=Species))+
  geom_point()+geom_smooth()

ggplot(iris, aes(Sepal.Length, Sepal.Width))+
  geom_point()+geom_smooth()

ggplot(iris, aes(Sepal.Length, Sepal.Width))+
  geom_point()+geom_smooth(method="lm")

#vectors in R (suppose vector is a column in excel sheet)
v1 <- c(3,5,6,7,8,9,13)
v2 <- c(12,13,5,6,7,8, 9)
print(v1)
c(v1,v2)
v3 <- c(v1,v2)
v1+v2
v1*v2
v1+v3

#vectors for strings
s1 <- c("i", "love", "R", "with", "codanics")
print(s1)
typeof(s1)

c(v1,s1)
s2 <- c(v1,s1)
s2
typeof(s2)

#sequence and repeats
seq(from= 0, to = 100)
seq(from= 1, to = 121)
seq(1,121)
seq(1, 121)
seq(1,100, by=5)
seq(1,100, by=10)
seq1 <- seq(3, 33, by=1.3) #(1.3 is difference, sequence from 3 to 33 with diff of 1.3)
seq1

rep("hello", times=3)
rep(123, times=100)
rep("homework",100)
rep("stars", 444)

rep(1:10, each=3)
rep(1:10, each=3, times=2)

#scatter plots in R
library(ggplot2)
data("cars")
View(cars)

ggplot(data = cars, aes(x= speed, y= dist))+geom_point()+
  geom_smooth(method = "lm", se=T, level=095)

#scatter plot with multi line
data("Orange")
View(Orange)

ggplot(Orange, aes(age, circumference, color = Tree))+
  geom_point(size=4, aes(shape=Tree))

ggplot(Orange, aes(age, circumference, color=Tree))+
  geom_point(size=4, shape=19)+
  geom_line(aes(linetype=Tree), size=1)+
  labs(x= "age", y= "circumference", title="graph")+
  ggsave("scatterplot.pdf")

#bubble plot
install.packages("viridis")
library(viridis)
data("quakes")
str(quakes)
nrow(quakes)
ncol(quakes)
head(quakes)

#creating a subset from a large dataset
quake_sample <- quakes[seq(from= 1, to= 1000, by=10),]
nrow(quake_sample)

#bubbleplot
ggplot(data = quake_sample, aes(x=lat, y=long))+
  geom_point(aes(size=mag, color=mag))+
  guides(size=F) #to remove the size legend

ggplot(data = quake_sample, aes(x=lat, y=long))+
  geom_point(aes(size=mag, color=mag))+
  guides(color=F) #to remove the color legend

ggplot(data = quake_sample, aes(x=lat, y=long))+
  geom_point(aes(size=mag, color=mag))+
  guides(size=F)+
  scale_colour_viridis_b(option = "B")+
  scale_size_continuous(range = c(1,9))+
  labs(x= "latitude", y="longitude", title="graph")+
  ggsave("bubbleplot.pdf")

#jitterplot
data("diamonds")
View(diamonds)
data() #to see the datasets available in R
str(diamonds)
nrow(diamonds)
ncol(diamonds)

d_sample <- diamonds[seq(from=1, to=53000, by=1000),]
nrow(d_sample)
View(d_sample)

ggplot(d_sample, aes(cut, price))+geom_point()
ggplot(d_sample, aes(cut, price, color=cut))+geom_jitter()

#voilin plots
library(ggplot2)
data("diamonds")
View(diamonds)
d_sample

ggplot(data=d_sample, aes(x=cut, y=price))+
  geom_violin()

p <- ggplot(data=d_sample, aes(x= cut, y= price, color=cut))+ geom_violin()
plot(p)

p <- ggplot(data=d_sample, aes(x= cut, y= price, fill=cut))+ geom_violin()
plot(p)

p + geom_boxplot(width=0.1)+ geom_jitter(size=0.5)+
  ggsave("voilinplot.pdf")

#PCA

data("iris")
View(iris)
x <- prcomp(iris[ ,-5], center= TRUE, scale = TRUE)
print(x)
summary(x)
plot(x)

iris <- cbind(iris, x$x)
View(iris)

ggplot(iris, aes(PC1, PC2, col=iris$Species, fill=iris$Species))+
  stat_ellipse(geom = "polygon", col="black", alpha=0.5)+
  geom_point(shape=21, col="black")+
  ggsave("pca_plot.pdf")

install.packages("factoextra")
install.packages("FactoMineR")
library(factoextra)
library(FactoMineR)


iris.pcs <- PCA(iris[,-5], graph=TRUE, scale.unit=TRUE)

#scree plot #how much data is lying in the first 2 components
fviz_eig(iris.pcs, addlabels = TRUE, ylim=c(0,80))

fviz_pca_var(iris.pcs, col.var="cos2",
             gradient.col= c("blue", "red", "yellow", "green"),
             repel=TRUE)+
  labs(title = "pca", x="PC1 (49%)", y="PC2 (23.9%)",
       colour = "cos2")+
  ggsave("pca.pdf")

#heatmap
x <- mtcars
View(mtcars)

x <- as.matrix(mtcars)
heatmap(x, scale="column")

#gplot heatmap
install.packages("gplots")
library(gplots)
heatmap.2(x, scale  = "column",
          col = bluered(100),
          trace = "none")

?heatmap.2() #to see the parameters #manual #help

#pheatmaps
install.packages("pheatmap")
library(pheatmap)
pheatmap(x, scale = "column", cutree_rows = 4,
         cutree_cols = 3)

#ggplot2
library(ggplot2)
y <- iris
install.packages("reshape")
library(reshape)

y1 <- melt(iris)

ggplot(y1, aes(y1$Species, y1$variable, fill=y1$value))+
  geom_tile()+
  scale_fill_gradient(low = "yellow", high = "blue")

#adding p value to boxplots
install.packages("ggplot2")
install.packages("ggpval")
install.packages("ggthemes")
library(ggplot2)
library(ggpval)
library(ggthemes)

data("ToothGrowth")

p <- ggplot(ToothGrowth) + 
  aes(x=supp, y=len, fill=supp)+
  geom_boxplot(shape="circle", width=0.5)+
  stat_boxplot(geom = 'errorbar', width=0.1)+
  scale_fill_viridis_d(option = "inferno", direction = 1)+
  labs(x = "Suplement", y = "Length", fill = "Suplement")+
  ggthemes::theme_par()+
  facet_wrap(vars(dose));p

#add p_value
add_pval(p, pairs = list(c(1,2)),
         test = "t.test")

help("add_pval")

#bar plot with anova and tukey test
install.packages("multcompView")
install.packages("dplyr")

library(multcompView)
library(dplyr)
library(ggplot2)
data("chickwts")
tibble(chickwts)

mean_data <- group_by(chickwts, feed) %>% 
  summarise(mean_weight = mean(weight), sd = sd(weight)) %>%
  arrange(desc(mean_weight))

tibble(mean_data)

anova <- aov(weight ~ feed, data = chickwts)
summary(anova)

tukey <- TukeyHSD(anova)
tukey

group_letters <- multcompLetters4(anova, tukey)
group_letters

group_letters <- as.data.frame.list(group_letters$feed)
mean_data$group_letters <- group_letters$Letters
tibble(mean_data)

p <-ggplot(mean_data, aes(x=feed, y=mean_weight))+
  geom_bar(stat="identity", aes(fill = feed), show.legend = FALSE, width = 0.5)+
  geom_errorbar(aes(ymin=mean_weight-sd, ymax=mean_weight+sd), width=0.1)+
  geom_text(aes(label=group_letters, y = mean_weight+sd), vjust=-0.4)+
  scale_fill_brewer(palette = "BrBG", direction = 1)+
  labs(x="Feed", y="Mean weight", fill="Feed", title = "publication ready barplot", subtitle = "made by mehwish")+
  ylim(0,410)+
  ggthemes::theme_par(); p

tiff('barplot.tiff', units="in", width=10, height=6, res=300, compression='lzw')
p
dev.off() #to save the plot

#hierarchical clustering 
pacman::p_load(pacman, dplyr, GGally, ggplot2, ggthemes, ggvis, httr, lubridate,
               plotly, rio, rmarkdown, shiny, stringr, tidyr)

if (!require("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install(version = "3.18")

BiocManager::version()
BiocManager::valid()
BiocManager::available()
BiocManager::install()
library(BiocManager)

install.packages("pacman")
library(pacman)
library(datasets)

head(mtcars)
cars <- mtcars[,c(1:4,6:7,9:11)]
head(cars)

hc <- cars %>% dist() %>% hclust()
plot(hc) #dendrogram

rect.hclust(hc, k=2, border="red")
rect.hclust(hc, k=3, border="yellow")
rect.hclust(hc, k=4, border="green")
rect.hclust(hc, k=5, border="blue")

#save the plot
tiff('dendrogram.tiff', units="in", width=10, height=6, res=300, compression='lzw')
plot(hc)
dev.off()

#regression analysis

?USJudgeRatings
data <- USJudgeRatings
View(data)

x <- as.matrix(data[-12]) #independent variables(select all columns except 12th column)
y <- as.matrix(data[, 12]) #dependent variable(select only 12th column)

reg1 <- lm(y ~ x) #linear regression/model, y is outcome, x is predictor, tilda is used to show the relationship
summary(reg1)

#or specify variables individually
reg2 <- lm(CONT ~ INTG + DMNR + DILG + CFMG + DECI + PREP, data = USJudgeRatings)

#results
reg1 #coefficients of the model
summary(reg1) #summary of the model #inferetial statistics
confint(reg1) #confidence intervals
anova(reg1) #analysis of variance #coefficients with inferential statistics
coef(reg1) #coefficients
resid(reg1) #residuals
hist(resid(reg1)) #histogram of residuals

#conventional step wise regression
p_load(lars, caret)

stepwise <- lars(x, y, type = "stepwise")

#stage wise regression, better generalizibility
forward <- lars(x, y, type = "forward.stagewise")

#lar is least angle regression
lar <- lars(x, y, type = 'lar')

#lasso regression is used for variable selection, it is a shrinkage method, least absolute shrinkage and selection operator
lasso <- lars(x, y, type = "lasso")

#comparison of R^2 models
r2comp <- c(stepwise$R2[6], forward$R2[6],
            lar$R2[6], lasso$R2[6]) %>% round(2)

names(r2comp) <- c("stepwise", "forward", "lar", "lasso")
r2comp

#clear everything from the environment
rm(list=ls())

#installing packages
install.packages("psych")
install.packages("corrplot")
install.packages("RColorBrewer")
library(psych)
library(corrplot)
library(RColorBrewer)

#correlation matrix
data("iris")
x <- corr.test(iris[-5]) #exclude the 5th column 
pairs.panels(iris[-5])
x
#save
tiff('iriscorrelation.tiff', units="in", width=10, height=6, res=300, compression='lzw')
pairs.panels(iris[-5])
dev.off()

#iris data set
i <- datasets::iris
head(i)
str(i)
summary(i)
View(i)

corr <- cor(i[,-5]) # -5 is used to exclude the species column
corr #pearson correlation
corrplot(corr)

tiff('iris_correlation.tiff', units="in", width=10, height=6, res=300, compression='lzw')
corrplot(corr)
dev.off()

corrplot(corr, method = "number")

corrplot(corr, type = "upper") #upper triangle
corrplot(corr, type = "lower") #lower triangle
corrplot(corr, type = "upper", order = "hclust") #upper triangle with hierarchical clustering

?corrplot
?cor

corrplot(corr, type = "upper", order = "hclust", method = "pie",
         col = brewer.pal(n = 8, name = "RdYlBu"))

corrplot.mixed(corr)
corrplot.mixed(corr, lower.col = "black", number.cex = 0.8)

corrplot.mixed(corr, lower = "square", upper = "pie", tl.col = "Red")
corrplot.mixed(corr, lower = "number", upper = "pie", tl.col = "red")

#if any other correlation, then:
#iris data set
i <- datasets::iris
head(i)
str(i)
summary(i)
View(i)

correlation <- cor(i[,-5], method = "spearman") 
correlation
corrplot(correlation, method = "number")
