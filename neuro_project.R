#library(pheatmap)
#library(ggplot2)
#data_read
a <- read.csv("C:\\Users\\Aryan\\Desktop\\neuro_data2.csv", header = TRUE)
d <- read.csv("C:\\Users\\Aryan\\Desktop\\data_neuro.csv", header = TRUE)

#pre experiment power analysis
ps <- seq(0.53, 1, 0.01)
ns <- c()
ses <- c()
for(p in ps){
  n <- 1
  while(TRUE){

    r <- floor(p * n)
    pv <- prop.test(r, n, alternative = "greater")
    if(pv$p.value < 0.05){
      ns <- c(ns, n)
      break()
    }
    n <- (n + 1)
    
  }
}

ggplot(mapping = aes(ps, ns)) + geom_point(size = 2)

#ttest
true_false <- (d$Response == d$stimulus)
true_false <- as.integer(true_false)
num_of_true <- sum(true_false)
num_of_false <- length(true_false) - num_of_true
sample_size <- length(true_false)

test <- prop.test(num_of_true, sample_size, alternative = "greater")


#fisher_test
cold_stimulus <- true_false[d$stimulus == "cold"]
cold_right <- sum(as.integer(cold_stimulus))
cold_len <- length(cold_stimulus)
cold_false <- cold_len - cold_right

warm_stimulus <- true_false[d$stimulus == "warm"]
warm_right <- sum(as.integer(warm_stimulus))
warm_len <- length(warm_stimulus)
warm_false <- warm_len - warm_right

cont_mat <- rbind(c(cold_right,warm_false),c(cold_false,warm_right))

fisher.test(cont_mat, alternative = 'g')

#correlatin 

pain_rate <- d$Pain.Rate
c <- cor(true_false, pain_rate)

cor_mat <- cor(a)
pheatmap(cor_mat)

#permutation_test

means <- c()

for(i in seq(1, 10000)){
  permutation <- sample(true_false)
  p_true <- pain_rate[permutation == TRUE]
  true_mean <- mean(p_true)
  
  p_false <- pain_rate[permutation == FALSE]
  false_mean <- mean(p_false)
  
  means <- c(means, true_mean - false_mean)
}

p_true <- pain_rate[true_false == TRUE]
true_mean <- mean(p_true)

p_false <- pain_rate[true_false == FALSE]
false_mean <- mean(p_false)

m <- true_mean - false_mean

p_value <- (length(means[means > m]) / 10000)

#another fisher test

cold_stimulus <- true_false[d$Pain.Rate >= 2.5]
cold_right <- sum(as.integer(cold_stimulus))
cold_len <- length(cold_stimulus)
cold_false <- cold_len - cold_right

warm_stimulus <- true_false[d$Pain.Rate < 2.5]
warm_right <- sum(as.integer(warm_stimulus))
warm_len <- length(warm_stimulus)
warm_false <- warm_len - warm_right

cont_mat <- rbind(c(cold_right,warm_false),c(cold_false,warm_right))

fisher.test(cont_mat, alternative = 'g')


