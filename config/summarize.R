#!/usr/bin/env Rscript

require(dplyr)

## args <- commandArgs(trailingOnly = TRUE)
## d <- read.csv(args[1])
d <- read.csv("_data/dicecoefficients.csv")
d.summary <- group_by(d, algo, thresh) %>% summarize(mean=mean(coeff), sd=sd(coeff))
write.csv(d.summary, "_data/summary.csv", row.names=F)
write("Made _data/summary.csv", stdout())
