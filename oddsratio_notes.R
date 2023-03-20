library(PropCIs)

riskscoreci(189, 10845, 104, 10933, conf.level =.95)
riskscoreci(104, 109033, 189, 10845, conf.level =.95)

diffscoreci(189,  10845, 104, 109033, conf.level =.95)

## Odds ratio
odds <- out$estimate(1-out$estimate)
OR <- odds[1]/odds[2]

## Confidence interval for OR

library(epitools)
or_out <- oddsratio(c(189,10845,104,10933),
                    method = "wald",
                    correction = FALSE)
