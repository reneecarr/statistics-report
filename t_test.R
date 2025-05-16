# import xlsx file

install.packages("readxl")
library("readxl")
surveydata1 <- read_excel("Book3.xlsx", sheet = 1)
surveydata2 <- read_excel("Book3.xlsx", sheet = 2)

# format data for t.test() function

qsnonmale <- list()
qsmale <- list()

for (j in 1:6) {
  q1 <- numeric(0)
  q2 <- numeric(0)
  for (i in 2:6) {
    q1 <- c(q1, rep(i-1,surveydata1[j,i]))
    q2 <- c(q2, rep(i-1,surveydata2[j,i]))
  }
  qsnonmale[[j]] <- q1
  qsmale[[j]] <- q2
}

# perform welch t-test

qnum <- 4 # tweak this for each question between 1 and 6
t.test(qsnonmale[[qnum]],qsmale[[qnum]])

# # calculate averages for each (was unnecessary)
# 
# avgs1 <- numeric(0)
# sqavgs1 <- numeric(0)
# avgs2 <- numeric(0)
# sqavgs2 <- numeric(0)
# 
# for(i in 1:20) {
#   avgs1 <- c(avgs1, weighted.mean(c(1:5), w = as.numeric(surveydata1[i,2:6])/5))
#   sqavgs1 <- c(sqavgs1, weighted.mean(c(1:5)^2, w = as.numeric(surveydata1[i,2:6])/5))
#   avgs2 <- c(avgs2, weighted.mean(c(1:5), w = as.numeric(surveydata2[i,2:6])/5))
#   sqavgs2 <- c(sqavgs2, weighted.mean(c(1:5)^2, w = as.numeric(surveydata2[i,2:6])/5))
# }
# 
# matrix(ncol = 0, nrow = 0)
