# Jingjing Yang
# Sept 8, 2015

#Task1
#download.file("http://www-bcf.usc.edu/~gareth/ISL/Auto.csv", destfile = "Auto.csv")

#Task2
auto_file_name <- "/Users/JingjingYang/Dropbox/fall 2015/253 statistical computing/Auto.csv"
#Auto_bad <- read.table(auto_file_name)
Auto <- read.csv(auto_file_name, header = T, na.strings = "?")

#Task3
task3 <- summary(Auto$horsepower)

#Task4
install.packages("ISLR")

#Task5
task5top <- Auto[1:5,1:3]
task5bottom <- Auto[393:397, 7:9]

#test
require(scoreActivity, quietly = TRUE )
score253(day = 2)