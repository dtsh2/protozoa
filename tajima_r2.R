taj_data_full<-read.csv('results_all_ci.csv',header=T)

pdf('gene_zoonotic_tajima_CI.pdf',width=6,height=5) 
#par(mfrow=c(1,2))
m <- rbind(c(1, 1, 1,2), c(1,1,1, 2))
layout(m)

taj_data_gp60<-taj_data_full[1:10,]

x<-1:length(taj_data_gp60[,1])
plot(x, taj_data_gp60$Tajima,
     ylim=range(c(min(taj_data_gp60$Tajima+taj_data_gp60$Tajima_Lower_95_CI),
                  max(taj_data_gp60$Tajima+taj_data_gp60$Tajima_Upper_95_CI)+1)),
     pch=19, xlab="Species", ylab="Tajima's D",
     main=expression(paste("gp60 Tajima's D")),
     col = as.numeric(taj_data_gp60$Host),
     xaxt='n')
abline(h=0)
mycol <- rgb(0, 0, 0, alpha = 0)
rect(0.5, -3, 2.5, 5, density = NULL, border = "grey",col=rgb(0, 0, 1,0.05))
rect(4.5, -3, 6.5, 5, density = NULL, border = "grey",col=rgb(0, 0, 1,0.05))
rect(8.5, -3, 10.5, 5, density = NULL, border = "grey",col=rgb(0, 0, 1,0.05))
axis(1, at=seq(from=1.5,to=9.5,by=2), labels=taj_data_gp60$Species[seq(from=1,to=9,by=2)])
legend('topright',c('Host','Human'),pch=19,col = as.numeric(taj_data_gp60$Host),
       bg=mycol,box.col = mycol) 
points(x=c(2,4,9.8,10,10.2),y=taj_data_gp60$Tajima[c(2,4,10,10,10)]+
         taj_data_gp60$Tajima_Upper_95_CI[c(2,4,10,10,10)]+0.1,pch=8)
 arrows(x, taj_data_gp60$Tajima+taj_data_gp60$Tajima_Lower_95_CI,
        x, taj_data_gp60$Tajima+taj_data_gp60$Tajima_Upper_95_CI, length=0.05, angle=90, code=3,
        col = as.numeric(taj_data_gp60$Hosts))


taj_data_cowp<-taj_data_full[11:12,]

x<-1:length(taj_data_cowp[,1])
plot(x, taj_data_cowp$Tajima,
     ylim=range(c(min(taj_data_cowp$Tajima+taj_data_cowp$Tajima_Lower_95_CI),
                  max(taj_data_cowp$Tajima+taj_data_cowp$Tajima_Upper_95_CI)+1)),
     xlim=c(0,2.5),
     pch=19, xlab="Species", ylab="Tajima's D",
     main=expression(paste("cowp Tajima's D")),
     col = as.numeric(taj_data_cowp$Host),
     xaxt='n')
mycol <- rgb(0, 0, 0, alpha = 0)
axis(1, at=seq(from=1.5,to=9.5,by=2), labels=taj_data_cowp$Species[seq(from=1,to=9,by=2)])
legend('topleft',c('Host','Human'),pch=19,col = as.numeric(taj_data_cowp$Host),
       bg=mycol,box.col = mycol) 
points(x=c(1,1.2,2),y=taj_data_cowp$Tajima[c(1,1,2)]+
         taj_data_cowp$Tajima_Upper_95_CI[c(1,1,2)]+0.1,pch=8)
arrows(x, taj_data_cowp$Tajima+taj_data_cowp$Tajima_Lower_95_CI,
       x, taj_data_cowp$Tajima+taj_data_cowp$Tajima_Upper_95_CI, length=0.05, angle=90, code=3,
       col = as.numeric(taj_data_cowp$Hosts))

dev.off()

pdf('gene_zoonotic_R2_CI.pdf',width=6,height=5) 
#par(mfrow=c(1,2))
m <- rbind(c(1, 1, 1,2), c(1,1,1, 2))
layout(m)

taj_data_gp60<-taj_data_full[1:10,]

x<-1:length(taj_data_gp60[,1])
plot(x, taj_data_gp60$R2,
     ylim=range(c(min(taj_data_gp60$R2-taj_data_gp60$R2_Lower_CI),
                  max(taj_data_gp60$R2+taj_data_gp60$R2_Upper_CI)+.05)),
     pch=19, xlab="Species", ylab=expression(paste(R[{2}])),
     main=expression(paste("gp60 ", R[{2}])),
     col = as.numeric(taj_data_gp60$Host),
     xaxt='n')
mycol <- rgb(0, 0, 0, alpha = 0)
rect(0.5, -3, 2.5, 4.5, density = NULL, border = "grey",col=rgb(0, 0, 1,0.05))
rect(4.5, -3, 6.5, 4.5, density = NULL, border = "grey",col=rgb(0, 0, 1,0.05))
rect(8.5, -3, 10.5, 4.5, density = NULL, border = "grey",col=rgb(0, 0, 1,0.05))
axis(1, at=seq(from=1.5,to=9.5,by=2), labels=taj_data_gp60$Species[seq(from=1,to=9,by=2)])
legend('topright',c('Host','Human'),pch=19,col = as.numeric(taj_data_gp60$Host),
       bg=mycol,box.col = mycol) 
points(x=c(3),y=taj_data_gp60$R2[c(3)]+taj_data_gp60$R2_Upper_CI[c(3)]+0.02,pch=8)
arrows(x, taj_data_gp60$R2-taj_data_gp60$R2_Lower_CI,
       x, taj_data_gp60$R2+taj_data_gp60$R2_Upper_CI, length=0.05, angle=90, code=3,
       col = as.numeric(taj_data_cowp$Hosts))

taj_data_cowp<-taj_data_full[11:12,]

x<-1:length(taj_data_cowp[,1])
plot(x, taj_data_cowp$R2,
     ylim=range(c(min(taj_data_cowp$R2-taj_data_cowp$R2_Lower_CI),
                  max(taj_data_cowp$R2+taj_data_cowp$R2_Upper_CI)+0.05)),
     xlim=c(0,2.5),
     pch=19, xlab="Species", ylab=expression(paste(R[{2}])),
     main=expression(paste("cowp ", R[{2}])),
     col = as.numeric(taj_data_gp60$Host),
     xaxt='n')
mycol <- rgb(0, 0, 0, alpha = 0)
axis(1, at=seq(from=1.5,to=9.5,by=2), labels=taj_data_cowp$Species[seq(from=1,to=9,by=2)])
legend('topleft',c('Host','Human'),pch=19,col = as.numeric(taj_data_cowp$Host),
       bg=mycol,box.col = mycol) 
arrows(x, taj_data_cowp$R2-taj_data_cowp$R2_Lower_CI,
       x, taj_data_cowp$R2+taj_data_cowp$R2_Upper_CI, length=0.05, angle=90, code=3,
       col = as.numeric(taj_data_cowp$Hosts))

dev.off()
