## ----fit----

fit <- procD.lm(lmks ~ spec * site, 
                data = data, iter = 999, turbo = TRUE,
                RRPP = TRUE, print.progress = FALSE)

data$Group <- interaction(data$site, data$spec)

## ----pairs----

pairs <- pairwise(fit, groups = data$Group)

## ----sum----

summary(pairs)