"test for normal distribution"
data1<-read.csv(file.choose(),sep = ",",header=T)
shapiro.test(data1$C1)
"Average apple sold in a day are 97"
"H0:mu=97"
"H1:not H0"
"One sample t test"
apple<-read.csv(file.choose(),sep=",",header = T)
summary(apple)
t.test(apple$C1,alternative = "less",mu=97)
"Accept H0"
"The company is assessing the different in salary of males and females."
"Ho: Average time is equal for 2 groups"
"H1: not H0"
"independent t test"
Salary<-read.csv(file.choose(),sep=",",header = T)
summary(Salary)
t.test(Salary$MALES,Salary$FEMALES,alternative = "two.side",var.equal = TRUE)
"Accept H0"
"A survey was done organized to check poverty level"
"H0:Poverty is increased"
"H1:not H0"
"Paried t test"
poverty<-read.csv(file.choose(),sep=",",header = T)
t.test(poverty$X1,poverty$X2,alternative = "greater",paired = T)
"Accept H0"
"Paired t test"
"MIS report:
H0: Average time is equal (before and after)
H1: average time(after) is less than"
"paired t test"
time1<-read.csv(file.choose(),sep=",",header = T)
t.test(time1$time_before,time1$time_after, alternative = "less", paired = T)
"Accept H0"
"To study correlation between 'apptitude' and job_prof
H0:there is no correlation between scores and job proficiency(??=0)
H1: aptitude scores and job 
proficiency are correlated(?? ?? ?? 0)"
"t test for correlated"
cor<-read.csv(file.choose(),sep=",",header = T)
summary(cor)
cor.test(cor$aptitude,cor$job_prof,alternative = "two.sided",method = "pearson")
"Reject H0"
"varience- INDEPENDENT samples t test.xls
H0:??12=??22 and H1:??12 ??? ??22"
"t test for varience"
var<-read.csv(file.choose(),sep=",",header = T)
summary(var)
var.test(var$time_g1,var$time_g2,alternative = "two.sided")

