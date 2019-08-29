rm(list = ls())

## install packages

install.packages('table1')
install.packages('lubridate')


library(table1)
library(lubridate)


## load data

load('diagnosis.RData')


## add labels

data.wide$Status <- factor(data.wide$Status, 
                            levels=c(0,1),
                            labels=c("No Disease", "Disease"))

data.wide$Sex <- factor(data.wide$Sex, 
                           levels=c(0,1),
                           labels=c("Male", "Female"))

## generate a categorical variable
data.wide$TestA.year <- as.factor(ifelse(year(data.wide$TestA.date)<2009,"<2009",
                               ifelse(year(data.wide$TestA.date)==2009,"2009",">2009")))



# attach data.wide
attach(data.wide)


## 1. Analyze one variable

# 1.1 Categorical variable

mytable_Sex <- table(Sex)

mytable_Sex

prop.table(mytable_Sex)

# test the proportion

prop.test(mytable_Sex,p=0.5) # H0: % of male = 0.5
prop.test(mytable_Sex,p=0.4) # H0: % of male = 0.4



# 1.2 Numerical variable

summary(Age)

# one sample t-test
t.test(Age,mu=35) # H0: mu=35
t.test(Age,mu=40) # H0: mu=40

# one sample wilcoxon test
wilcox.test(Age,mu=35) # H0: mu=35

## 2. Analyze two variables

# 2.1.1 Categorical by Categorical

# Cross table and Chi-squred test

mytable <- table(Sex,Status)
mytable
chisq.test(mytable)

margin.table(mytable, 1) # Sex frequencies (summed over Status) 
margin.table(mytable, 2) # Status frequencies (summed over Sex)


# 2.1.2 Numerical by Categorical


# independent 2-group t-test

t.test(Age ~ Sex,var.equal = TRUE)  # assume equal variance
t.test(Age ~ Sex,var.equal = FALSE)  # assume unequal variance



# Mann-Whitney U test

wilcox.test(TestA ~ Sex)



# ANOVA

fit <- aov(log(TestA) ~ TestA.year)
model.tables(fit,"means")
summary(fit)


# K-W test

kruskal.test(TestA ~ TestA.year)

# 2.2 correlation


# pearson correlation
cor(TestA,TestB, method="pearson") 

# p-value for rho
cor.test(TestA,TestB, method="pearson")

# spearman correlation
cor(TestA,TestB, method="spearman")





## make table 1
table1(~ Status + Sex + Age + TestA + TestB + TestC + TestA.year, data=data.wide)

## make table 1 by group (Status)

table1(~ Sex + Age + TestA + TestB + TestC + TestA.year| Status, data=data.wide)


# add units to table 1

units(data.wide$Age) <- "years"

table1(~ Sex + Age + TestA + TestB + TestC + TestA.year| Status, data=data.wide)


# add more statistics to table 1

table1(~ Sex + Age + TestA + TestB + TestC + TestA.year| Status, data=data.wide,
       render.continuous=c(.="Mean (SD)", .="Median [Min, Max]",.="Median [Q1, Q3]",.="Median [IQR]"))



# add p value

load('diagnosis.RData')

data.wide$Status <- factor(data.wide$Status, 
                           levels=c(0, 1, 2), 
                           labels=c("No Disease", "Disease", "P-value"))

data.wide$Sex <- factor(data.wide$Sex, 
                        levels=c(0,1),
                        labels=c("Male", "Female"))


# creat P values
data <- data.wide
outcome <- data.wide$Status

rndr <- function(x, name, ...) {
  if (length(x) == 0) {
    y <- data[[name]]
    s <- rep("", length(render.default(x=y, name=name, ...)))
    if (is.numeric(y)) {
      p <- t.test(y ~ outcome)$p.value
    } else {
      p <- chisq.test(table(y, droplevels(outcome)))$p.value
    }
    s[2] <- sub("<", "&lt;", format.pval(p, digits=3, eps=0.001))
    s
  } else {
    render.default(x=x, name=name, ...)
  }
}

rndr.strat <- function(label, n, ...) {
  ifelse(n==0, label, render.strat.default(label, n, ...))
}

# make table 1

table1(~ Sex + Age + TestA + TestB + TestC | Status, data=data.wide, 
       render.continuous=c(.="Mean (SD)"),droplevels=F, render=rndr, render.strat=rndr.strat, overall=F)



