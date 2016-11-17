#!/usr/bin/env Rscript

require(data.table)
d <- fread('_data/dicecoefficients.csv')
setkey(d, algo, thresh)
d.summary <- d[,list(mean=mean(coeff),sd=sd(coeff)),by=list(algo,thresh)]
write.csv(d.summary, "_data/summary.csv", row.names=F)
write("Made _data/summary.csv", stdout())
