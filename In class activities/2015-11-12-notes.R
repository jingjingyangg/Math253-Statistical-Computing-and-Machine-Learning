# Classification and regression trees (CART)
# 11/12/2015

library(tree)
cartoon <- data.frame(x = 1:8, y = c(2,5,1,3,8,5,4,6), class = c("a","b", "a", "a", "b", "b", "a", "b"))
pure <- tree.control(8,mincut = 0, minsize = 1,mindev = 0)
t.first <- tree(y~x, data = cartoon, control = pure)
plot(t.first)
text(t.first)
t.first
