## test code

rm(list = ls())

phi_data<-read.csv('results.csv',header=T)

library(vegan)
# fit <- manova(cbind(Theta,Phi) ~ Zoonotic, data = phi_data)
# summary(fit, test="Pillai")
# summary.aov(fit)

fit <- manova(cbind(Species,Gene,Phi,Theta) ~ Zoonotic, data = phi_data)
summary(fit, test="Pillai")
summary.aov(fit)

fit <- manova(cbind(Species,Phi,Theta) ~ Gene, data = phi_data)
summary(fit, test="Pillai")
summary.aov(fit)

## https://stats.stackexchange.com/questions/30788/whats-a-good-way-to-use-r-to-make-a-scatterplot-that-separates-the-data-by-trea

require ("ggplot2")
 p1 <- ggplot(phi_data, aes (x = Theta, y = Phi, colour = Zoonotic)) + stat_density_2d() +
  ylab(expression(paste(phi))) + xlab(expression(paste(theta)))

# require ('hexbin')
# ggplot(phi_data, aes (x = Theta, y = Phi, fill = Zoonotic)) + stat_binhex (bins=2, aes (alpha = 0.5)) + ## NB alpha can be count
#   facet_grid (. ~ Zoonotic)

 p2 <- ggplot(phi_data, aes(x = Theta, y = Phi, colour = Zoonotic)) + geom_point() +
   ylab(expression(paste(phi))) + xlab(expression(paste(theta)))
# ggplot(phi_data, aes(x = Theta, y = Phi)) + geom_point() + facet_grid(~Zoonotic)

 p3 <- ggplot(phi_data, aes (x = Theta, y = Phi, colour = Gene)) + stat_density_2d() +
   ylab(expression(paste(phi))) + xlab(expression(paste(theta)))
 p4 <-ggplot(phi_data, aes(x = Theta, y = Phi, colour = Gene)) + geom_point() +
   ylab(expression(paste(phi))) + xlab(expression(paste(theta)))
 
 # Multiple plot function
 #
 # ggplot objects can be passed in ..., or to plotlist (as a list of ggplot objects)
 # - cols:   Number of columns in layout
 # - layout: A matrix specifying the layout. If present, 'cols' is ignored.
 #
 # If the layout is something like matrix(c(1,2,3,3), nrow=2, byrow=TRUE),
 # then plot 1 will go in the upper left, 2 will go in the upper right, and
 # 3 will go all the way across the bottom.
 #
 multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
   library(grid)
   
   # Make a list from the ... arguments and plotlist
   plots <- c(list(...), plotlist)
   
   numPlots = length(plots)
   
   # If layout is NULL, then use 'cols' to determine layout
   if (is.null(layout)) {
     # Make the panel
     # ncol: Number of columns of plots
     # nrow: Number of rows needed, calculated from # of cols
     layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                      ncol = cols, nrow = ceiling(numPlots/cols))
   }
   
   if (numPlots==1) {
     print(plots[[1]])
     
   } else {
     # Set up the page
     grid.newpage()
     pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
     
     # Make each plot, in the correct location
     for (i in 1:numPlots) {
       # Get the i,j matrix positions of the regions that contain this subplot
       matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
       
       print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                       layout.pos.col = matchidx$col))
     }
   }
 }
 pdf('gene_zoonotic_mdim.pdf',width=8,height=6) 
 multiplot(p1, p2, p3, p4, cols=2)
 dev.off()
 
 gg_color_hue <- function(n) {
   hues = seq(15, 375, length = n + 1)
   hcl(h = hues, l = 65, c = 100)[1:n]
 }
 
 pdf('gene_zoonotic_bars.pdf',width=7,height=6) 
 
 layout(matrix(c(1,2,2,3,4,4), nrow = 2, ncol = 3, byrow = TRUE))
 #par(mfrow=c(2,2))
 n = 2
 cols = gg_color_hue(n)
 plot(phi_data$Zoonotic,phi_data$Phi,col=cols,ylab=expression(phi))
 n = 4
 cols = gg_color_hue(n)
 plot(phi_data$Gene,phi_data$Phi,col=cols,ylab=expression(phi))
 n = 2
 cols = gg_color_hue(n)
 plot(phi_data$Zoonotic,phi_data$Theta,col=cols,ylab=expression(theta))
 n = 4
 cols = gg_color_hue(n)
 plot(phi_data$Gene,phi_data$Theta,col=cols,ylab=expression(theta))
 
 dev.off()