rm(list = ls())
library(phytools)
library(ape) 
# https://joey711.github.io/phyloseq/plot_tree-examples.html

pdf("gp60.pdf",width = 7,height = 7)
par(mfrow=c(1,2))
tree = read.newick("SPSgp60.newick")
x<-tree$tip.label
x1 = sapply(strsplit(x, split='_', fixed=TRUE),function(x) (x[3]))
tree$tip.label<-x1
#tree<-ladderize(tree)
colorCodes <- c(rainbow(length(unique(tree$tip.label))))
names(colorCodes)<-unique(tree$tip.label)
plotTree(tree,ftype="off",main='Species')
add.scale.bar(x=0.5,y=1)
tiplabels(#x1, # if text
  col = colorCodes[tree$tip.label],bg=NA,frame = 'none',pch=16)
text(x=500,'Species')

tree = read.newick("SPSgp60.newick")
x<-tree$tip.label
x1 = sapply(strsplit(x, split='_', fixed=TRUE),function(x) (x[2]))
tree$tip.label<-x1
#tree<-ladderize(tree)
colorCodes <- c(rainbow(length(unique(tree$tip.label))))
names(colorCodes)<-unique(tree$tip.label)
plotTree(tree,ftype="off")
add.scale.bar(x=0.5,y=1)
tiplabels(#x1, # if text
  col = colorCodes[tree$tip.label],bg=NA,frame = 'none',pch=16)
text(x=500,'Host')
dev.off()