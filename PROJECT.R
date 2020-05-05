## ASSIGNMENT 1 

mydata <- read.delim("D:/project 1/incexp.txt")
index = 167
age = mydata$AGE[index:(index+99)] 
hhinc =mydata$HHINCOME[index:(index+29)]

## exploratory data analysis AGE
#**************************************par(mfrow=c(1, 2))
boxplot(age,main = "The age of the head of the household", col = "gray" )
summary(age)
hist(age, breaks = 15, main ="The age of the head of the household", col ="gray", border="blue" )

## exploratory data analysis HHINC

boxplot(hhinc, main = "total monthly income of the household", col = "gray" )
summary(hhinc)
hist(hhinc, main ="total monthly income of the household.", col ="gray", border="blue" )

#Construct a 95% confidence interval for the population mean age. Provide an interpretation of
#your 95% confidence interval.
sample_mean_age = mean(age)

age_bts <- matrix(0,ncol=100,nrow=4000)
for( i in 1:4000)
{samp=sample(age,size=100,replace=TRUE)
age_bts[i,]=samp}
age_bts=apply(age_bts,1,mean)
age_sort_bts=sort(age_bts)

## 95% CI AGE
# bootstrap CI
cat("The bootstrap 95% confidence interval is (",age_sort_bts[0.025*4000],",",age_sort_bts[0.975*4000],")")
#Error
lb_error = age_sort_bts[0.025*4000] - sample_mean_age
ub_error = age_sort_bts[0.975*4000] - sample_mean_age
cat("The lower bound error is", lb_error, "and the upper bound error is", ub_error)
# CI bounds 
bound1 = sample_mean_age - lb_error
bound2 = sample_mean_age - ub_error
if (bound1<bound2){
  cat("The 95% confidence interval for the population mean age is (", bound1, ",", bound2, ")")
}  else{   
  cat("The 95% confidence interval for the population mean age is (", bound2, ",", bound1, ")")
}

# Hypothesis test H0 : age <= 43 H1 : age > 43
# if null hypothesis is true then mean age does not exceed 43
age_null_hyp = 43
error = sample_mean_age - age_null_hyp
# vector of elements for p value
pv_elements <- age_bts > (sample_mean_age+error)
sum(pv_elements)
p_value = (sum(pv_elements))/4000
if (p_value < 0.05){
  cat ("The p-value is", p_value, "which is significantly small hence we reject null hypothesis and conclude that the mean age is greater than 43.")
} else {
cat ("The p-value is", p_value, "which is significantly large hence we fail to reject null hypothesis and conclude that the mean age is less than 43.")
}

# Use the bootstrap samples to generate the histogram of the distribution of the sample mean age.
# Comment on the shape of the distribution.
hist(age_bts, breaks = 20, main ="The bootstrap mean age of the head of the household", col ="gray", border="blue")
# comment

#Construct a 90 % confidence interval of "0:5, the median household income.

sample_median_hhinc = median(hhinc)

hhinc_bts <- matrix(0,ncol=30,nrow=4000)
for( i in 1:4000)
{samp1=sample(hhinc,size=30,replace=TRUE)
hhinc_bts[i,]=samp1}
hhinc_bts=apply(hhinc_bts,1,median)
hhinc_sort_bts=sort(hhinc_bts)

cat("Bootstrap 90% confidence interval is (",hhinc_sort_bts[0.05*4000],",",hhinc_sort_bts[0.95*4000],")")
#Error
lb1_error = hhinc_sort_bts[0.025*4000] - sample_median_hhinc
ub1_error = hhinc_sort_bts[0.975*4000] - sample_median_hhinc
cat("The lower bound error is", lb1_error, "and the upper bound error is", ub1_error)
# CI bounds 
bound3 = sample_median_hhinc - lb1_error
bound4 = sample_median_hhinc - ub1_error
if (bound1<bound2){
  cat("The 90% confidence interval for the house hould income median is (", bound3, ",", bound4, ")")
}  else{   
  cat("The 90% confidence interval for the house hould income median is  (", bound3, ",", bound4, ")")
}


# Question 2
data2 <-  read.delim("E:/project 1/E1.txt")
fertA = data2$Fert_A
fertB = data2$Fert_B

summary(fertA)
summary(fertB)
samp_meanA = mean(fertA)
samp_meanB = mean(fertB)
samp_meanDiff = samp_meanB - samp_meanA

### Bootstrap
all_A_B =c(fertA,fertB)
bstA=bstB=matrix(0,ncol=20,nrow=5000)
for(j in 1:5000)
{samp2=sample(all_A_B,size=40,replace=TRUE)
bstA[j,] =samp2[1:20]
bstB[j,]=samp2[21:40]
}
bstA_mean=apply(bstA,1,mean)
bstB_mean=apply(bstB,1,mean)
bst_mean_diff=bstB_mean-bstA_mean
pv_diff <- (bst_mean_diff > samp_meanDiff)|(bst_mean_diff < -samp_meanDiff)
sum(pv_diff)
p_value1 = sum(pv_diff)/5000  
p_value1
if (p_value1 < 0.05){
  cat ("The p-value is", p_value1, "which is significantly small hence we reject null hypothesis and conclude that the mean crop yields from the two fertilisers are not equal.")
} else {
  cat ("The p-value is", p_value1, "which is significantly large hence we fail to reject null hypothesis and conclude that the mean crop yields from the two fertilisers are equal.")
}

# Provide a 95% confidence interval for the difference in means using the bootstrap approach.

sort_bst_mean_diff = sort(bst_mean_diff)
cat("The bootstrap difference 95% confidence interval is (", sort_bst_mean_diff[0.025*5000],",",sort_bst_mean_diff[0.975*5000],")")
#Error
lb2_error = sort_bst_mean_diff[0.025*4000] - samp_meanDiff
ub2_error = sort_bst_mean_diff[0.975*4000] - samp_meanDiff
cat("The lower bound error is", lb2_error, "and the upper bound error is", ub2_error)
# CI bounds 
bound5 = samp_meanDiff - lb2_error
bound6 = samp_meanDiff - ub2_error
if (bound5<bound6){
  cat("The 95% confidence interval for the mean yield difference is (", bound5, ",", bound6, ")")
}  else{   
  cat("The 95% confidence interval for the mean yield difference is (", bound6, ",", bound5, ")")
}


# Use bootstrapping to test the variances for equality.
varA = var(fertA)
varB = var(fertB)
samp_F = varA/varB

bstA_var =apply(bstA,1,var)
bstB_var =apply(bstB,1,var)
bst_F = bstA_var/bstB_var
pv_F <- (bst_F > samp_F)|(bst_F < -samp_F)
sum(pv_F)
p_value2 = sum(pv_F)/5000
p_value2
if (p_value1 < 0.05){
  cat ("The p-value is", p_value2, "which is significantly small hence we reject null hypothesis and conclude that the variance of the crop yields from the two fertilisers are different.")
} else {
  cat ("The p-value is", p_value2, "which is significantly large hence we fail to reject null hypothesis and conclude that the variane of the crop yields from the two fertilisers are equal.")
}


#### Compare the results from bootstrapping and normal theory in a brief paragraph.
# F test

samp_F
df1 = length(fertA)-1
df2 = length(fertB)-1
p_value3 = pf(samp_F, df1,df2)
p_value3
if (p_value1 < 0.05){
  cat ("The p-value is", p_value3, "which is significantly small hence we reject null hypothesis and conclude that the variance of the crop yields from the two fertilisers are different.")
} else {
  cat ("The p-value is", p_value3, "which is significantly large hence we fail to reject null hypothesis and conclude that the variane of the crop yields from the two fertilisers are equal.")
}

F_value = qf(0.95,df1,df2)
F_value
if (samp_F< F_value){
  cat("We fail to reject null hypothesis and conclude that the variane of the crop yields from the two fertilisers are equal. ")
} else{
  cat("We reject the null hypotheis and and conclude that the variane of the crop yields from the two fertilisers are different.")
}

# Question 3
#test whether there are significant differences in means between the groups
# by using the bootstrap approach.

data3 <- read.delim("F:/project 1/aov1.txt")
data3 <- na.omit(data3) 

p1= data3$Price_9
p2 = data3$Price_10
p3 = data3$Price_11


#ANOVA WITH ORIGINAL DATA
price = list("numeric",length=3)
price[[1]]=c(p1)
price[[2]]=c(p2)
price[[3]]=c(p3)

n=c(length(price[[1]]),length(price[[2]]),length(price[[3]]))
N=sum(n)
k=length(price)

B3 = 5000
RATIOS =Yi.=vector("numeric")
Y.. = mean(unlist(price))

SSE = SST = 0

for(j in 1:k)
{Yi.[j] = mean(price[[j]])
SSE = SSE+sum((price[[j]]-Yi.[j])^2)
SST = SST+n[j]*(Yi.[j]-Y..)^2
}
F_sample = (SST/(k-1))/(SSE/(N-k))

# Bootstraps #

ALLprice = unlist(price)

bst1=bst2=bst3 = matrix(0, nrow = 5000, ncol = 20 )
for(h in 1:B3)
{SSE = SST = 0 
Yi.=vector("numeric")
price = list("numeric",length=3)
bst_anova = sample(ALLprice, size=60, replace=TRUE)
bst1[h,] = bst_anova[1:20]
bst2[h,] = bst_anova[21:40]
bst3[h,] = bst_anova[41:60]

price[[1]]=bst_anova[1:20]
price[[2]]=bst_anova[21:40]
price[[3]]=bst_anova[41:60]

for(j in 1:k)
{Yi.[j] = mean(price[[j]])
SSE = SSE+sum((price[[j]]-Yi.[j])^2)
SST = SST+n[j]*(Yi.[j]-Y..)^2
}
RATIOS[h]=(SST/(k-1))/(SSE/(N-k))
}

pvalue = length(RATIOS[RATIOS>F_sample])/B3

theory_pvalue = pf(F_sample, k-1, N-k, lower.tail = FALSE)
pvalue
theory_pvalue

for (h in 1:3) {
  print(bst_anova)
  
}
