if (!require("pacman")) {install.packages("pacman"); library(pacman)}
pacman::p_load(shiny)

dat <- read.csv("https://github.com/aecay/leeds-modeling-workshop/raw/master/w1/data.csv")
dat$part3 <- factor(dat$part3)
dat$lett.odd <- factor(dat$lett.odd)
dat$participant <- factor(dat$participant)

runGitHub("aecay/leeds-modeling-workshop", subdir="w2")
