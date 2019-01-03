#analysis of ialeUK conference proceedings

rm(list=ls())
library(tidyverse)
path <- "C:/Users/k1076631/Google Drive/Research/Papers/InProgress/ialeUK_25years/QuantAnalysis/"
setwd(path)
filename <- "abstract_review_export_2018-06-10.csv"
cpdata <- read.csv(filename)