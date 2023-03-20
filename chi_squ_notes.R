data <- matrix(c(53,110,27,3057,4621,606),2, byrow = T)
chi_test_out <- chisq.test(data)

names(chi_test_out)
chi_test_out$statistic
chi_test_out$expected

happy <- matrix(c(272, 294,49,454,835,131,185,527,208),
                nrow = 3, byrow = T)

happy_chi <- chisq.test(happy)
names(happy_chi)
happy_chi
happy_chi$residuals

## Fishers Exact Test for 2x2 tables

tea <- matrix(c(3,1,1,3), byrow = T, ncol = 2)

fisher.test(tea, alternative = "greater")     # one sided
  # fail to reject null hypothesis bc p-value is small

fisher.test(tea)  #two sided


## Fishers Exact Test for 3x3 tables (not on the test on 11/1)