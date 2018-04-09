loc_data<-read.csv('results_location_ci.csv',header=T)

pdf('gene_location_R2_Tajima.pdf',width=6,height=5) 

par(mfrow=c(1,2))
# m <- rbind(c(1, 1, 1,2), c(1,1,1, 2))
# layout(m)

x<-1:length(loc_data[,1])
plot(x, loc_data$Tajima_DT,
     ylim=range(c(min(loc_data$Tajima_DT+loc_data$Tajima_DT_95CI_Lower),
                  max(loc_data$Tajima_DT+loc_data$Tajima_DT_95CI_Upper),+1)),
     pch=19, xlab="Species", ylab="Tajima's D",
     main=expression(paste("gp60 Tajima's D")),
     col = as.numeric(loc_data$Location),
     xaxt='n')
abline(h=0)
mycol <- rgb(0, 0, 0, alpha = 0)
rect(0.5, -3, 2.5, 5, density = NULL, border = "grey",col=rgb(0, 0, 1,0.05))
axis(1, at=seq(from=1,to=4,by=1), labels=loc_data$Species[seq(from=1,to=4,by=1)])
legend('topright',c('World','NZ'),pch=19,col = as.numeric(loc_data$Location),
       bg=mycol,box.col = mycol) 
# points(x=c(2,4,9.8,10,10.2),y=loc_data$Tajima[c(2,4,10,10,10)]+
#          loc_data$Tajima_Upper_95_CI[c(2,4,10,10,10)]+0.1,pch=8)
 arrows(x, loc_data$Tajima_DT+loc_data$Tajima_DT_95CI_Lower,
        x, loc_data$Tajima_DT+loc_data$Tajima_DT_95CI_Upper, length=0.05, angle=90, code=3,
        col = as.numeric(loc_data$Location))

 plot(x, loc_data$R2,
      ylim=range(c(min(loc_data$R2-loc_data$R2_95CI_Lower),
                   max(loc_data$R2+loc_data$R2_95CI_Upper),+0.25)),
      pch=19, xlab="Species", ylab=expression(paste(R[{2}])),
      main=expression(paste("gp60 ", R[{2}])),
      col = as.numeric(loc_data$Location),
      xaxt='n')
 abline(h=0)
 mycol <- rgb(0, 0, 0, alpha = 0)
 rect(0.5, -3, 2.5, 5, density = NULL, border = "grey",col=rgb(0, 0, 1,0.05))
 axis(1, at=seq(from=1,to=4,by=1), labels=loc_data$Species[seq(from=1,to=4,by=1)])
 legend('topright',c('World','NZ'),pch=19,col = as.numeric(loc_data$Location),
        bg=mycol,box.col = mycol) 
 # points(x=c(2,4,9.8,10,10.2),y=loc_data$Tajima[c(2,4,10,10,10)]+
 #          loc_data$Tajima_Upper_95_CI[c(2,4,10,10,10)]+0.1,pch=8)
 arrows(x, loc_data$R2-loc_data$R2_95CI_Lower,
        x, loc_data$R2+loc_data$R2_95CI_Upper, length=0.05, angle=90, code=3,
        col = as.numeric(loc_data$Location))
 
 dev.off()

## pi and theta
 
 pdf('gene_location_pi_theta.pdf',width=6,height=5) 
 
 par(mfrow=c(1,2))
 # m <- rbind(c(1, 1, 1,2), c(1,1,1, 2))
 # layout(m)
 
 x<-1:length(loc_data[,1])
 plot(x, loc_data$Pi,
      ylim=range(c(min(loc_data$Pi-1.96*loc_data$SD.Pi),
                   max(loc_data$Pi+1.96*loc_data$SD.Pi),+0.2)),
      pch=19, xlab="Species", ylab=expression(Pi),
      main=expression(paste('gp60 ',Pi)),
      col = as.numeric(loc_data$Location),
      xaxt='n')
 abline(h=0)
 mycol <- rgb(0, 0, 0, alpha = 0)
 rect(0.5, -3, 2.5, 5, density = NULL, border = "grey",col=rgb(0, 0, 1,0.05))
 axis(1, at=seq(from=1,to=4,by=1), labels=loc_data$Species[seq(from=1,to=4,by=1)])
 legend('topright',c('World','NZ'),pch=19,col = as.numeric(loc_data$Location),
        bg=mycol,box.col = mycol) 
 # points(x=c(2,4,9.8,10,10.2),y=loc_data$Tajima[c(2,4,10,10,10)]+
 #          loc_data$Tajima_Upper_95_CI[c(2,4,10,10,10)]+0.1,pch=8)
 arrows(x, loc_data$Pi-1.96*loc_data$SD.Pi,
        x, loc_data$Pi+1.96*loc_data$SD.Pi, length=0.05, angle=90, code=3,
        col = as.numeric(loc_data$Location))
 
 plot(x, loc_data$Theta_W,
      ylim=range(c(min(loc_data$Theta_W-1.96*loc_data$Theta_SD),
                   max(loc_data$Theta_W+1.96*loc_data$Theta_SD),+0.1)),
      pch=19, xlab="Species", ylab=expression(paste(R[{2}])),
      main=expression(paste("gp60 ",Theta)),
      col = as.numeric(loc_data$Location),
      xaxt='n')
 abline(h=0)
 mycol <- rgb(0, 0, 0, alpha = 0)
 rect(0.5, -3, 2.5, 5, density = NULL, border = "grey",col=rgb(0, 0, 1,0.05))
 axis(1, at=seq(from=1,to=4,by=1), labels=loc_data$Species[seq(from=1,to=4,by=1)])
 legend('topright',c('World','NZ'),pch=19,col = as.numeric(loc_data$Location),
        bg=mycol,box.col = mycol) 
 # points(x=c(2,4,9.8,10,10.2),y=loc_data$Tajima[c(2,4,10,10,10)]+
 #          loc_data$Tajima_Upper_95_CI[c(2,4,10,10,10)]+0.1,pch=8)
 arrows(x, loc_data$Theta_W-1.96*loc_data$Theta_SD,
        x, loc_data$Theta_W+1.96*loc_data$Theta_SD, length=0.05, angle=90, code=3,
        col = as.numeric(loc_data$Location))
 
 dev.off()
 