library(readr)
stats1 <- read_csv("~/R/ORIE 4740/Project/stats1.csv")
dim(stats1)
##data = stats1[, -c("id", "item 1","item 2","item 3","item 4","item 5","item 6","trinket")]
dataleague = stats1[,-(3:9)]
dataleague = dataleague[,-1]
dataleague = dataleague[, -35]

sum(is.na(dataleague)) # no need to clean

is.factor(dataleague$win)
dataleague$win = as.factor(dataleague$win)


league = dataleague[1:2000,]
is.factor(league$win)

initialmod = glm(win~., family = binomial, data = league)
summary(initialmod)

initialmod.prob = predict(initialmod, type = "response")
initialmod.pred = rep(0, 2000)
initialmod.pred[initialmod.prob > 0.5] = 1
table(initialmod.pred, league$win)

testleague = dataleague[2001:4000,]  # test set
table(initialmod.pred, testleague$win)

# using p values get rid of some vars
league2 = league[, c("win", "kills", "deaths", "assists", "longesttimespentliving", "quadrakills", "totheal", "totunitshealed", "dmgtoturrets", "goldearned", "turretkills", "inhibkills", "totminionskilled", "neutralminionskilled", "ownjunglekills", "wardsplaced")]

model2 = glm(win~., family = binomial, data = league2)
summary(model2) #much better

# first do forward subset
library(leaps)

forward2 = regsubsets(win~., data = league2, nvmax = 15, method = "forward")
for2summary = summary(forward2)

par(mfrow=c(1,2))

plot(for2summary$bic, type = "l", xlab = "number of vars", ylab = "bic")
points(12, for2summary$bic[12], col = "red", pch = 20)
plot(for2summary$adjr2, type = "l", xlab = "number of vars", ylab = "adjusted r2")
points(15, for2summary$adjr2[15], col = "red", pch = 20)
which.min(for2summary$bic)
which.max(for2summary$adjr2)
#using bic criteria, 12 vars has lowest bic

coef12 = coef(forward2, 12)

#building model with 12 vars
attach(league2)
model3 = glm(win~kills+deaths+assists+longesttimespentliving+totunitshealed+goldearned+turretkills+inhibkills+totminionskilled+neutralminionskilled+ownjunglekills+wardsplaced, family = binomial, data = league2)
summary(model3)
summary(model2)
summary(initialmod)

model3.prob = predict(model3, type = "response")
model3.pred = rep(0, 2000)
model3.pred[model3.prob > 0.5] = 1
table(model3.pred, league2$win)

table(initialmod.pred, testleague$win)
table(model3.pred, testleague$win)

#####
#tree?
install.packages("tree")
library(tree)
tree1 = tree(win~., league2)
tree1
par(mfrow=c(1,1))
plot(tree1)
text(tree1)
set.seed(1)
cv1 = cv.tree(tree1, FUN = prune.misclass)
plot(cv1$size, cv1$dev, type = 'b')
tree2 = prune.misclass(tree1, best = 4)
plot(tree2)
text(tree2)
pred2 = predict(tree2, newdata=league2, type="class")
table(pred2, league2$win)
table(pred2, testleague$win)

# tree with random sampling
set.seed(1)
train = sample(1:nrow(league), 1000)
league.test = league[-train,]
tree.league = tree(win~., league, subset = train)
tree.pred = predict(tree.league, league.test, type = "class")
table(tree.pred, league$win[-train])

plot(tree.league)
text(tree.league)

set.seed(1)
cv.league = cv.tree(tree.league, FUN = prune.misclass)
plot(cv.league$size, cv.league$dev, type = 'b')
prune.league = prune.misclass(tree.league, best = 6)
plot(prune.league)
text(prune.league, pretty = 0)
tree.pred = predict(prune.league, league.test, type = "class")
table(tree.pred, league$win[-train])


#using randomforest
library(MASS)
install.packages("randomForest")
library(randomForest)
set.seed(1)
train = sample(1:nrow(league), nrow(league)/2)
bag.league=randomForest(win~.,data=league,subset=train, mtry = 7, importance=TRUE)
bag.league

importance(bag.league)
varImpPlot(bag.league) #using gini plot, deaths,turretkills, assists, goldearned

#building GAM model using 4 vars above
library(gam)
library(splines)
gam.m1 = gam(win~deaths+turretkills+assists+goldearned, family = binomial, data = league)
gam.m2 = gam(win~s(deaths, 2)+turretkills+assists+goldearned, family = binomial, data = league)
gam.m3 = gam(win~s(deaths, 4)+turretkills+assists+goldearned, family = binomial, data = league)
gam.m4 = gam(win~s(deaths, 6)+turretkills+assists+goldearned, family = binomial, data = league)

anova(gam.m1, gam.m2, gam.m3, gam.m4, test = "Chisq") # m2 > m1, m3 > m2, m4 < m3, so pick m3

gam.t1 = gam(win~s(deaths, 4)+turretkills+assists+goldearned, family = binomial, data = league)
gam.t2 = gam(win~s(deaths, 4)+s(turretkills, 2)+assists+goldearned, family = binomial, data = league)
gam.t3 = gam(win~s(deaths, 4)+s(turretkills, 4)+assists+goldearned, family = binomial, data = league)
gam.t4 = gam(win~s(deaths, 4)+s(turretkills, 6)+assists+goldearned, family = binomial, data = league)
anova(gam.t1, gam.t2, gam.t3, gam.t4, test = "Chisq") #based on 0.01 level, t2 is better

gam.a1 = gam(win~s(deaths, 4)+s(turretkills, 2)+assists+goldearned, family = binomial, data = league)
gam.a2 = gam(win~s(deaths, 4)+s(turretkills, 2)+s(assists, 2)+goldearned, family = binomial, data = league)
gam.a3 = gam(win~s(deaths, 4)+s(turretkills, 2)+s(assists, 4)+goldearned, family = binomial, data = league)
gam.a4 = gam(win~s(deaths, 4)+s(turretkills, 2)+s(assists, 6)+goldearned, family = binomial, data = league)
anova(gam.a1, gam.a2, gam.a3, gam.a4, test = "Chisq") #based on 0.001 level, a2 is better

gam.g1 = gam(win~s(deaths, 4)+s(turretkills, 2)+assists+goldearned, family = binomial, data = league)
gam.g2 = gam(win~s(deaths, 4)+s(turretkills, 2)+assists+s(goldearned, 2), family = binomial, data = league)
gam.g3 = gam(win~s(deaths, 4)+s(turretkills, 2)+assists+s(goldearned, 4), family = binomial, data = league)
gam.g4 = gam(win~s(deaths, 4)+s(turretkills, 2)+assists+s(goldearned, 6), family = binomial, data = league)
anova(gam.g1, gam.g2, gam.g3, gam.g4, test = "Chisq") #based on 0.001 level, g2 is better

finalprob = predict(gam.g2, newdata = league, type = "response")
finalpred = rep(0, 2000)
finalpred[finalprob > 0.5] = 1
table(finalpred, league$win)






