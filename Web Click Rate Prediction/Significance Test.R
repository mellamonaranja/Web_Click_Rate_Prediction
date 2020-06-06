#2.
#T-Test
t.test(Time~Page, data=session_times, alternative='less') 
##Alternative Hypothesis : The session time of page A is less than B.
##p-value = 0.1408, therefore, statistically significant.

#ANOVA(Analysis Of Variance)
ggplot(four_sessions,aes(x=Page, y=Time))+
    geom_boxplot()+
    labs(y='Time (in seconds)')+
    theme_bw()

summary(aovp(Time~Page, data = four_sessions)) 
##Iter means how many times repeat this trial.

#F-Statistic
summary(aov(Time~Page, data = four_sessions))
##Statistically significant if F-Statistic is higher.

#Chi-Square Test
clicks=matrix(click_rate$Rate,nrow = 3,ncol = 2, byrow = T)
chisq.test(clicks,simulate.p.value = T)




