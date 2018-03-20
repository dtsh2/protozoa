# references:
# Subramanian BMC Genomics (2016) 17:123 - The effects of sample size on population genomics analyses
# Pilkington Mol Biol Evol (2008) 25(3):517-525 - Contrasting Signatures of Population Growth
# Verra Mol & Biochemical Parasitology (206) 149:182-190 - Contrasting Signatures of Selection


x<-seq(0,4*pi,2*pi/100)
y1<-rep(sin(x),5)
y2<-sin(x)
y3<-seq(from=10,to=20,by=10/length(y2))
y4<-y3[1:length(y2)]+y2
y5<-c(y1+35,y2+10,y4)
y6<-rep(y1,length.out=length(y5))+35

par(mfrow=c(1,1))
plot(y5,type="l",main="Population size, evolutionary parameters, and fitness",
     xaxt='n',yaxt='n',ylab = '',xlab='',col='red',
     xlim=c(0,length(y5)),ylim=c(-20,max(y5)+10))
rect(length(y1), -20, length(y5), 32, density = NULL, border = rgb(0, 0, 1,0.05),col=rgb(0, 0, 1,0.05))
rect(length(y1), -20, length(y5), 27, density = NULL, border = rgb(0, 0, 1,0.05),col=rgb(0, 0, 1,0.05))
rect(length(y1)+length(y2), -20, length(y5), 27, density = NULL, border = rgb(0, 0, 1,0.05),col=rgb(0, 0, 1,0.05))
points(y6,type="l", col = 'blue')
title(ylab="Population size", line=0.5, cex.lab=1)
title(xlab="Time", line=0.5, cex.lab=1)
x=seq(from=(length(y5)-length(y4)),to=length(y5),by=1)
points(y=y4,x=x[-1],type="l", col = 'yellow')

rhost<-'Reservoir host'
text(600, 40, rhost,
     cex = .8)
# text(1200, 40, rhost,
#      cex = .8)
nhost<-'Spillover host'
text(1200, 30, nhost,
     cex = .8)
text(1100, 25, 'Transmission',
     cex = .8)
text(1300, 25, 'Expansion',
     cex = .8)

points(y=y6[c(100,200,320,length(y1))],x=c(100,200,320,length(y1)),pch=19, col = 'red')
arrows(x0=100, y0=y6[c(100)], 100,5,length = 0.05, col = 'red')
arrows(x0=200, y0=y6[c(200)], 200,5,length = 0.05, col = 'red')
arrows(x0=320, y0=y6[c(320)], 320,5,length = 0.05, col = 'red')
text(250, 0, expression(paste("Dead end events ",italic("sample")," from reservoir")),
     cex = .8)

# abline(v=1800,lty=2)

neff<-expression(paste(N[{e}]))
pit<-expression(paste(Pi))
thetat<-expression(paste(theta))
tajd<-expression(paste("Tajima's D"))
tR2<-expression(paste(R[{2}]))

arrows(x0=500, y0=28,500,30,length = 0.05)
text(600, 29, neff,
     cex = .8)
arrows(x0=500, y0=23,500,25,length = 0.05)
text(600, 25, pit,
     cex = .8)
arrows(x0=450, y0=21,500,21,length = 0)
text(600, 21, thetat,
     cex = .8)
arrows(x0=500, y0=15,500,17,length = 0.05)
arrows(x0=450, y0=17,450,15,length = 0.05)
text(600, 16, tajd,
     cex = .8)
arrows(x0=450, y0=12,500,12,length = 0)
text(600, 12, tR2,
     cex = .8)

#

arrows(x0=length(y1)+50, y0=5,length(y1)+50,3,length = 0.05)
text(length(y1)+200, 4, neff,
     cex = .8)
arrows(x0=length(y1)+50, y0= 0,length(y1)+50,-2,length = 0.05)
text(length(y1)+200, 0, pit,
     cex = .8)
arrows(x0=length(y1)+50, y0= -5,length(y1),-5,length = 0)
text(length(y1)+200, -5, thetat,
     cex = .8)
arrows(x0=length(y1)+30, y0= -9,length(y1),-9,length = 0)
arrows(x0=length(y1)+50, y0= -8,length(y1)+50,-10,length = 0.05)
text(length(y1)+200, -9, tajd,
     cex = .8)
arrows(x0=length(y1)+30, y0= -12,length(y1),-12,length = 0)
arrows(x0=length(y1)+50, y0= -12,length(y1)+50,-14,length = 0.05)
text(length(y1)+200, -12, tR2,
     cex = .8)
#

arrows(x0=length(y5)-50, y0=3,length(y5)-50,5,length = 0.05)
arrows(x0=length(y5), y0=0,length(y5)-50,0,length = 0)
arrows(x0=length(y5)-50, y0= -4,length(y5)-50,-2,length = 0.05)
arrows(x0=length(y5)-1, y0= -10,length(y5)-1,-8,length = 0.05)
arrows(x0=length(y5)-50, y0= -8,length(y5)-50,-10,length = 0.05)
arrows(x0=length(y5)-1, y0= -11,length(y5)-1,-13,length = 0.05)
arrows(x0=length(y5)-50, y0= -13,length(y5)-50,-11,length = 0.05)

##

legend(-30, -8, c(expression(paste("Reservoir ",italic("Cryptosporidium"),"population")),
                   expression(paste("Spillover ",italic("Cryptosporidium"),"population")),
                   'Spillover event'), 
       col = c('Black','Red','Red'),
       lty = c(1,1,NA),
       pch = c(NA,NA,19),
       bg = "gray95",
       cex = 0.7)

##

# x<-seq(0,2*pi,2*pi/100)
# yp<-cos(x)
# par(fig = c(0.1, 0.4, 0.25, 0.5), mar=c(1,1,1,1), new=TRUE)
# plot(yp,type="l",#main="Fitness",font.main = 1,cex.main=0.8,
#       xaxt='n',yaxt='n',
#      ylab = '',xlab='',
#      ylim=c(-1,1.5))
# mtext('Fitness',side=2)
# points(yp,
#        col=c(rep('black',length(yp)/2),rep('red',length(yp)/2)),
#        pch=20,cex=0.5)
# rect(50, -1, 100, 1, density = NULL,
#      border = rgb(0, 0, 1,0.05),col=rgb(0, 1, 1,0.05))
# text(25, 1.25, rhost,
#      cex = .8)
# text(75, 1.25, nhost,
#      cex = .8)
# 