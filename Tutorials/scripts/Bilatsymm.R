## ----labels----

lizards$ind

## ----side----

mosquito$side

## ----replicates----

lizards$rep

## ----pairs----

lizards$lm.pairs

## ----match----

mosq.sym <- bilat.symmetry(mosquito$wingshape, ind = mosquito$ind, side = mosquito$side,
                           replicate = mosquito$replicate, object.sym = FALSE, print.progress = FALSE)

## ----matchsum----

summary(mosq.sym)

## ----matchplot----

plot(mosq.sym)

## ----object----

scallop.sym <- bilat.symmetry(scallops$coorddata, ind = scallops$ind, object.sym = TRUE,
                              land.pairs = scallops$land.pairs, print.progress = FALSE)

## ----objsum----

summary(scallop.sym)

## ----objplot----

plot(scallop.sym)
rglwidget()