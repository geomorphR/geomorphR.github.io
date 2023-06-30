## ----prep----

data(Pupfish)
fit <- procD.lm(coords ~ Pop * Sex, data = Pupfish, iter = 999, print.progress = FALSE)

## ----analysis----

TA <- trajectory.analysis(fit, groups = Pupfish$Pop, traj.pts = Pupfish$Sex)

## ----sum1----

summary(TA, attribute = "MD")

## ----sum2----

summary(TA, attribute = "TC", angle.type = "deg")

## ----plot----

TP <- plot(TA, pch = as.numeric(Pupfish$Pop) + 20, bg = as.numeric(Pupfish$Sex),
           cex = 0.7, col = "gray")
add.trajectories(TP, traj.pch = c(21, 22), start.bg = 1, end.bg = 2)
legend("topright", levels(Pupfish$Pop), pch =  c(21, 22), pt.bg = 1)