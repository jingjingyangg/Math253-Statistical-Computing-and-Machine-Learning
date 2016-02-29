# Jingjing Yang
# 11-17-2015
# growing trees and pruning them

Cartoon_data <- data.frame(
  x = 1:8,
  y = c(2,5,1,3,8,5,4,6),
  class = c("A", "B", "A", "A", "B", "B", "A","B")
)

library(tree)

pure <- tree.control(8,mincut = 0, minsize = 1,mindev = 0)

#=================================
#regression tree
rtree_pure <- tree(y~x, data=Cartoon_data, control = pure)
plot(rtree_pure)
text(rtree_pure)
# output for x = 3 is 1

#=================================
#classification tree
ctree_pure <- tree(class~x, data = Cartoon_data, control = pure)
plot(ctree_pure)
text(ctree_pure)
# output for x = 7 is A

#=================================
#evaluating each tree
predict(rtree_pure)
predict(ctree_pure)
predict(rtree_pure, newdata = data.frame(x=3))
predict(ctree_pure, newdata = data.frame(x=7))

#=================================
#deviance of each node
rtree_pure
#just add up all the deviance at each node

#=================================
#deviance of a tree
## regression tree
dev_regression <- sum((y-predict(rtree_pure))^2)

## classification tree
## dev of a classification: -2 max(log(likelihood))
dev_classification <- -2 * 
  
#================================= 
#pruning the tree
rtree_5 <- prune.tree(rtree_pure, best = 5)
ctree_2 <- prune.misclass(ctree_pure, best = 2)

tree_deviance <- rep(NA,10)

nterminal <- c(2:8)
for (k in nterminal) {
  tree_deviance[k] <- sum((Cartoon_data$y - predict(prune.tree(rtree_pure, best=k)))^2)
}

#========================================
#Building a real classifier
CPS85 <- mosaicData::CPS85
pure_for_cps <- tree.control(nrow(CPS85), mincut = 0, minsize = 1, mindev = 0)
Sector_classifier <- tree(sector~wage + sex + educ + exper, data = CPS85, control = pure_for_cps)
cps85_20 <- prune.tree(Sector_classifier, best = 20)
