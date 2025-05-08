# Group composition data
semester1 <- c("6 men" = 0.40, "All male" = 0.30, "4-5 men" = 0.30)
semester2 <- c("All male" = 0.55, "Mixed" = 0.45)

# Bar plot - Semester 1
barplot(semester1, 
        main = "Group Composition - Semester 1", 
        ylab = "Proportion of Groups", 
        col = "lightblue", 
        ylim = c(0, 1))

# Bar plot - Semester 2
barplot(semester2, 
        main = "Group Composition - Semester 2", 
        ylab = "Proportion of Groups", 
        col = "lightgreen", 
        ylim = c(0, 1))
