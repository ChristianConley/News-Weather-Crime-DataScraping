
#This script is a method of simulating many samples taken from a normally-distributed population, then running 6 different normality tests to 
# see how each algorithm classifies them (ie. normal or not). Because all samples are being taken from a normal population, in theory
# every dataset should be 'normal'. However, these normality algorithms are not accurate 100% of the time. This script can be
# seen as a method of assessing which of these 6 normality algorithms is most accurate at determining normality in this controlled environment.


#The number of simulated datasets to run
number_simulations <- 10000
#The sample size of each dataset. Higher the sample size, the more apparent normality becomes (law of large numebers)
sample_size <- 50

#the 6 normality tests to run on each sample
shapiro_p<- rep(NA,number_simulations)
Anderson_Darling_P<- rep(NA, number_simulations)
Cramer_P<- rep(NA, number_simulations)
Lilliefors_p<- rep(NA, number_simulations)
Pearson_Chi_p<- rep(NA, number_simulations)
Shapiro_Francia_p<- rep(NA, number_simulations)


#A 'for-loop' which runs everything contained within it once for every simulation.
for(i in 1:number_simulations){
  x<-rnorm(sample_size)
  
  #list of 6 normality algorithms to run on each of the simulations. The p-value (inidcator of normality) is stored in an array for each algorithm.
  shapiro<- shapiro.test(x)
  shapiro_p[i]<- shapiro$p.value
  
  Anderson<- ad.test(x)
  Anderson_Darling_P[i]<- Anderson$p.value
  
  Cramer<- cvm.test(x)
  Cramer_P[i]<- Cramer$p.value
  
  Lillie<- lillie.test(x)
  Lilliefors_p[i]<- Lillie$p.value
  
  Pearson<- pearson.test(x)
  Pearson_Chi_p[i]<- Pearson$p.value
  
  Shapiro_Francia<- sf.test(x)
  Shapiro_Francia_p[i]<- Shapiro_Francia$p.value
}

# Average p-value across x simulations for each algorithm (closer or less than 0.5 indicates stronger performance).
mean(shapiro_p<.05)
mean(Anderson_Darling_P<.05)
mean(Cramer_P<.05)
mean(Lilliefors_p<.05)
mean(Pearson_Chi_p<.05)
mean(Shapiro_Francia_p<.05)

#Sum of the number of times each algorithm incorrectly classified the 'normal' sample as 'not-normal'.
shapiro_sum<-sum(shapiro_p<.05)
anderson_sum<-sum(Anderson_Darling_P<.05)
cramer_sum<-sum(Cramer_P<.05)
lilli_sum<-sum(Lilliefors_p<.05)
pearson_sum<-sum(Pearson_Chi_p<.05)
Francia_sum<-sum(Shapiro_Francia_p<.05)

# The accuracy percentage for each of the normality algorithms (highest accuracy indicates the best algorithm for judging normality).
(number_simulations - shapiro_sum)/number_simulations * 100
(number_simulations - anderson_sum)/number_simulations * 100
(number_simulations - cramer_sum)/number_simulations * 100
(number_simulations - lilli_sum)/number_simulations * 100
(number_simulations - pearson_sum)/number_simulations * 100
(number_simulations - Francia_sum)/number_simulations * 100


#This nest section of code is a way of visualizing the distribution of the p-values for each of these algorithms.

#The next two lines create a pop-out window which will contain 12 graphs (2 per algorithm) showing the cummulative 
#distribution of p-values for each algorithm
windows()
par(mfrow=c(2,6))

#Histograms for each of the algorithms.
hist(shapiro_p, main = "Histogram of Shapiro p-values", xlab = ("Observed p-value"),labels = TRUE)
hist(Anderson_Darling_P, main = "Histogram of Anderson-Darling p-values", xlab = ("Observed p-value"),labels = TRUE)
hist(Cramer_P, main = "Histogram of Cramer von Mises p-values", xlab = ("Observed p-value"),labels = TRUE)
hist(Lilliefors_p, main = "Histogram of Lilliefors p-values", xlab = ("Observed p-value"),labels = TRUE)
hist(Pearson_Chi_p, main = "Histogram of Pearson Chi-Squared p-values", xlab = ("Observed p-value"),labels = TRUE)
hist(Shapiro_Francia_p, main = "Histogram of Shapiro Francia p-values", xlab = ("Observed p-value"),labels = TRUE)
#Q-Q plots (quantile-quantile plots) of the algorithms with best-fit lines superimposed.
qqnorm(shapiro_p, main = "QQ-Plot of Shapiro p-values")
qqline(shapiro_p)
qqnorm(Anderson_Darling_P, main = "QQ-Plot of Anderson-Darling p-values")
qqline(Anderson_Darling_P)
qqnorm(Cramer_P, main = "QQ-Plot of Cramer von Mises p-values")
qqline(Cramer_P)
qqnorm(Lilliefors_p, main = "QQ-Plot of Lilliefors p-values")
qqline(Lilliefors_p)
qqnorm(Pearson_Chi_p, main = "QQ-Plot of Pearson p-values")
qqline(Pearson_Chi_p)
qqnorm(Shapiro_Francia_p, main = "QQ-Plot of Shapiro Francia p-values")
qqline(Shapiro_Francia_p)

