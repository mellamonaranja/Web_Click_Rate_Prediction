#1.
library(ggplot2)
install.packages('dplyr')
library(dplyr)
install.packages('lmPerm')
library(lmPerm)

PSDS_PATH=file.path('/Users/joohyunyoon/workspace/Web Click Rate Prediction/')
session_times=read.csv(file.path(PSDS_PATH, 'data','web_page_data.csv'))
session_times[,2]=session_times[,2] * 100
four_sessions =read.csv(file.path(PSDS_PATH, 'data', 'four_sessions.csv'))
click_rate= read.csv(file.path(PSDS_PATH, 'data', 'click_rates.csv'))

#Boxplot Analysis
ggplot(session_times,aes(x=Page, y=Time))+
    geom_boxplot()+ 
    theme_bw()
##Visitors stay longer in Page B rather than Page A.

#Mean Analysis
mean_a=mean(session_times[session_times['Page']=='Page A','Time'])
mean_b=mean(session_times[session_times['Page']=='Page B','Time'])
mean_b-mean_a
##The mean of Page B session time is 35.6sec longer than Page A.
##Is this result by coincidence or statistic? Let's test with permutation.

#Permutation Test
perm_fun=function(x,n1,n2){
    #Allocate groupA to n1, groupB to n2 with without sampling.
    n=n1+n2
    idx_b=sample(1:n, n1)
    idx_a=setdiff(1:n, idx_b)
    mean_diff=mean(x[idx_b])-mean(x[idx_a])
    return(mean_diff)
}

perm_diffs=rep(0,1000)
for (i in 1:1000) {
    perm_diffs[i]=perm_fun(session_times[,'Time'],21,15)
    ##Allocate n1=21, n2=15, call 1000times in order to get session time difference.
}

hist(perm_diffs,xlab='Session time differences (in seconds)')
abline(v=mean_b-mean_a, col='orange',lty=5,lwd=3) 
##Mean difference between mean_a and mean_b is over vertical abline. 
##It means the difference is within probability distribution range.
##Therefore statistically insignificant.

