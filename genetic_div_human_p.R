## test code

rm(list = ls())

phi_data<-read.csv('results.csv',header=T)

library(vegan)
fit <- manova(cbind(Theta,Phi) ~ Zoonotic, data = phi_data)
summary(fit, test="Pillai")
summary.aov(fit)

fit <- manova(cbind(Species,Gene,Phi,Theta) ~ Zoonotic, data = phi_data)
summary(fit, test="Pillai")
summary.aov(fit)

## https://stats.stackexchange.com/questions/30788/whats-a-good-way-to-use-r-to-make-a-scatterplot-that-separates-the-data-by-trea

require ("ggplot2")
ggplot(phi_data, aes (x = Theta, y = Phi, colour = Zoonotic)) + stat_density_2d()
# require ('hexbin')
# ggplot(phi_data, aes (x = Theta, y = Phi, fill = Zoonotic)) + stat_binhex (bins=2, aes (alpha = 0.5)) + ## NB alpha can be count
#   facet_grid (. ~ Zoonotic)

 ggplot(phi_data, aes(x = Theta, y = Phi, colour = Zoonotic)) + geom_point()
# ggplot(phi_data, aes(x = Theta, y = Phi)) + geom_point() + facet_grid(~Zoonotic)
