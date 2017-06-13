## test code

rm(list = ls())

Pi_data<-read.csv('results.csv',header=T)

library(vegan)
# fit <- manova(cbind(Theta,Pi) ~ Zoonotic, data = Pi_data)
# summary(fit, test="Pillai")
# summary.aov(fit)

fit <- manova(cbind(Species,Gene,Pi,Theta) ~ Zoonotic, data = Pi_data)
summary(fit, test="Pillai")
summary.aov(fit)

fit <- manova(cbind(Species,Pi,Theta) ~ Gene, data = Pi_data)
summary(fit, test="Pillai")
summary.aov(fit)

## https://stats.stackexchange.com/questions/30788/whats-a-good-way-to-use-r-to-make-a-scatterplot-that-separates-the-data-by-trea

require ("ggplot2")
 p1 <- ggplot(Pi_data, aes (x = Theta, y = Pi, colour = Zoonotic)) + stat_density_2d() +
  ylab(expression(paste(Pi))) + xlab(expression(paste(theta)))

# require ('hexbin')
# ggplot(Pi_data, aes (x = Theta, y = Pi, fill = Zoonotic)) + stat_binhex (bins=2, aes (alpha = 0.5)) + ## NB alpha can be count
#   facet_grid (. ~ Zoonotic)

 p2 <- ggplot(Pi_data, aes(x = Theta, y = Pi, colour = Zoonotic)) + geom_point() +
   ylab(expression(paste(Pi))) + xlab(expression(paste(theta)))
# ggplot(Pi_data, aes(x = Theta, y = Pi)) + geom_point() + facet_grid(~Zoonotic)

 p3 <- ggplot(Pi_data, aes (x = Theta, y = Pi, colour = Gene)) + stat_density_2d() +
   ylab(expression(paste(Pi))) + xlab(expression(paste(theta)))
 p4 <-ggplot(Pi_data, aes(x = Theta, y = Pi, colour = Gene)) + geom_point() +
   ylab(expression(paste(Pi))) + xlab(expression(paste(theta)))
 
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
 plot(Pi_data$Zoonotic,Pi_data$Pi,col=cols,ylab=expression(Pi))
 n = 4
 cols = gg_color_hue(n)
 plot(Pi_data$Gene,Pi_data$Pi,col=cols,ylab=expression(Pi))
 n = 2
 cols = gg_color_hue(n)
 plot(Pi_data$Zoonotic,Pi_data$Theta,col=cols,ylab=expression(theta))
 n = 4
 cols = gg_color_hue(n)
 plot(Pi_data$Gene,Pi_data$Theta,col=cols,ylab=expression(theta))
 
 dev.off()
 
## gp60 ###
 
 newdata <- Pi_data[ which(Pi_data$Gene =='gp60'), ]

 pdf('gp60_zoonotic_bars.pdf',width=6,height=5) 
 par(mfrow=c(1,2))
 n = 2
 cols = gg_color_hue(n)
 plot(newdata$Zoonotic,newdata$Pi,col=cols,ylab=expression(Pi))
 plot(newdata$Zoonotic,newdata$Theta,col=cols,ylab=expression(theta))
 dev.off()
 
 fit <- manova(cbind(Species,Pi,Theta) ~ Zoonotic, data = newdata)
 summary(fit, test="Pillai")
 summary.aov(fit)
 
 p5 <- ggplot(newdata, aes (x = Theta, y = Pi, colour = Zoonotic)) + stat_density_2d() +
   ylab(expression(paste(Pi))) + xlab(expression(paste(theta)))
 
 p6 <- ggplot(newdata, aes(x = Theta, y = Pi, colour = Zoonotic)) + geom_point() +
   ylab(expression(paste(Pi))) + xlab(expression(paste(theta)))
 
 pdf('gp60_zoonotic_mdim.pdf',width=8,height=3) 
 multiplot(p5, p6, cols=2)
 dev.off()
 
 ###
 
 Pi_data1<-read.csv('results1.csv',header=T)
 
 pdf('gene_zoonotic_scatter_pi.pdf',width=6,height=5) 
 #par(mfrow=c(1,2))
 m <- rbind(c(1, 1, 2), c(1,1, 2))
 layout(m)
    Pi_data<-Pi_data1[1:8,]
    x<-1:length(Pi_data[,1])
    plot(x, Pi_data$Pi,
      ylim=range(c(Pi_data$Pi-1.96*Pi_data$SD_Pi, Pi_data$Pi+1.96*Pi_data$SD_Pi)),
      pch=19, xlab="Species", ylab="Mean +/- 95% CI",
      main=expression(paste('gp60 ',Pi)),
      col = as.numeric(Pi_data$Host),
      xaxt='n')
      
 rect(0.5, -1, 2.5, 1, density = NULL, border = "grey",col=rgb(0, 0, 1,0.05))
 rect(4.5, -1, 6.5, 1, density = NULL, border = "grey",col=rgb(0, 0, 1,0.05))
 axis(1, at=seq(from=1.5,to=9.5,by=2), labels=Pi_data$Species[seq(from=1,to=9,by=2)])
 arrows(x, Pi_data$Pi-1.96*Pi_data$SD_Pi, x, Pi_data$Pi+1.96*Pi_data$SD_Pi, length=0.05, angle=90, code=3,
        col = as.numeric(Pi_data$Host))
 legend('topright',c('Host','Human'),pch=19,col = as.numeric(Pi_data$Host),
        bty='n') 

 Pi_data<-Pi_data1[9:10,]
 x<-1:length(Pi_data[,1])
 plot(x, Pi_data$Pi,
      ylim=range(c(Pi_data$Pi-1.96*Pi_data$SD_Pi, Pi_data$Pi+1.96*Pi_data$SD_Pi)),
      xlim=c(0,2.5),
      pch=19, xlab="Species", ylab="Mean +/- 95% CI",
      main=expression(paste('cowp ',Pi)),
      col = as.numeric(Pi_data$Host),
      xaxt='n')
 arrows(x, Pi_data$Pi-1.96*Pi_data$SD_Pi, x, Pi_data$Pi+1.96*Pi_data$SD_Pi, length=0.05, angle=90, code=3,
        col = as.numeric(Pi_data$Host))
 axis(1, at=seq(from=1.5,to=9.5,by=2), labels=Pi_data$Species[seq(from=1,to=9,by=2)])
 legend('topleft',c('Host','Human'),pch=19,col = as.numeric(Pi_data$Host),
        bty='n') 
 dev.off()
 
 ###
 
 Pi_data1<-read.csv('results1.csv',header=T)
 
 pdf('gene_zoonotic_scatter_theta.pdf',width=6,height=5) 
 #par(mfrow=c(1,2))
 m <- rbind(c(1, 1, 2), c(1,1, 2))
 layout(m)
 Pi_data<-Pi_data1[1:8,]
 x<-1:length(Pi_data[,1])
 plot(x, Pi_data$Theta,
      ylim=range(c(Pi_data$Theta-1.96*Pi_data$SD_Theta, Pi_data$Theta+1.96*Pi_data$SD_Theta)),
      pch=19, xlab="Species", ylab="Mean +/- 95% CI",
      main=expression(paste('gp60 ',Theta)),
      col = as.numeric(Pi_data$Host),
      xaxt='n')
 
 rect(0.5, -1, 2.5, 1, density = NULL, border = "grey",col=rgb(0, 0, 1,0.05))
 rect(4.5, -1, 6.5, 1, density = NULL, border = "grey",col=rgb(0, 0, 1,0.05))
 axis(1, at=seq(from=1.5,to=9.5,by=2), labels=Pi_data$Species[seq(from=1,to=9,by=2)])
 arrows(x, Pi_data$Theta-1.96*Pi_data$SD_Theta, x, Pi_data$Theta+1.96*Pi_data$SD_Theta, length=0.05, angle=90, code=3,
        col = as.numeric(Pi_data$Host))
 legend('topright',c('Host','Human'),pch=19,col = as.numeric(Pi_data$Host),
        bty='n') 
 
 Pi_data<-Pi_data1[9:10,]
 x<-1:length(Pi_data[,1])
 plot(x, Pi_data$Theta,
      ylim=range(c(Pi_data$Theta-1.96*Pi_data$SD_Theta, Pi_data$Theta+1.96*Pi_data$SD_Theta)),
      xlim=c(0,2.5),
      pch=19, xlab="Species", ylab="Mean +/- 95% CI",
      main=expression(paste('cowp ',Theta)),
      col = as.numeric(Pi_data$Host),
      xaxt='n')
 arrows(x, Pi_data$Theta-1.96*Pi_data$SD_Theta, x, Pi_data$Theta+1.96*Pi_data$SD_Theta, length=0.05, angle=90, code=3,
        col = as.numeric(Pi_data$Host))
 axis(1, at=seq(from=1.5,to=9.5,by=2), labels=Pi_data$Species[seq(from=1,to=9,by=2)])
 legend('topleft',c('Host','Human'),pch=19,col = as.numeric(Pi_data$Host),
        bty='n') 
 dev.off()
 
 