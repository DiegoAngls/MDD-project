rm(list=ls())
setwd("~/Documents/Projects/Depression/Depression")

# Summary data ------------------------------------------------------------

pacman::p_load(tidyverse, qgraph, bootnet, mgm,memisc,foreign,summarytools)

# data --------------------------------------------------------------------

data <- read.csv("../Depression/Data_Newpride2.csv", sep = "")

stview(stby(data, data$Time, dfSummary))
stview(stby(data, data$group, dfSummary))
