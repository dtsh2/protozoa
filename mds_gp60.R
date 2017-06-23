rm(list = ls())
library(phytools)
library(ape) 

# 
# cophenetic.phylo {ape}	R Documentation
# Pairwise Distances from a Phylogenetic Tree
# 
tree = read.tree("gp60.tre")

res<-cophenetic(tree)
res_mds<-cmdscale(res)

x <- res_mds[, 1]
y <- res_mds[, 2]

x1 = sapply(strsplit(rownames(res_mds), split='_', fixed=TRUE),function(x) (x[2]))
colorCodes <- c(rainbow(length(unique(x1))))

## note asp = 1, to ensure Euclidean distances are represented correctly
plot(x, y, type = "n", xlab = "", ylab = "", axes = T,asp=1,
     main = "MDS plot")
# text(x, y, x1, cex = 1,col = colorCodes[as.factor(x1)])
points(x,y, cex = 1.5,pch=16,col = colorCodes[as.factor(x1)])
legend('topleft', unique(x1),col = colorCodes[unique(as.integer(as.factor(x1)))],pch=16)
