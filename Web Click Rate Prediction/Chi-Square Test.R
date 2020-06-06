#3.
#Chi-Square Test
clicks=matrix(click_rate$Rate,nrow = 3,ncol = 2, byrow = T)
chisq.test(clicks,simulate.p.value = T)

#Chi-Square Test:Statistically Theory
x <- seq(1, 30, length=100)
chi <- data.frame(df = factor(rep(c(1, 2, 5, 10), rep(100, 4))),
                  x = rep(x, 4),
                  p = c(dchisq(x, 1), dchisq(x, 2), dchisq(x, 5), dchisq(x, 20)))

ggplot(chi,aes(x=x, y=p))+
    geom_line(aes(linetype=df))+
    theme_bw()+
    labs(x='', y='')

chisq.test(clicks,simulate.p.value = F) 
##P value is little bit less than P value from resampling.
##Because Chi-Square distribution is on approximate estimate but actual statistic distribution.

#Fisher Test
fisher.test(clicks) 
##p value is similar as resampling p value.


