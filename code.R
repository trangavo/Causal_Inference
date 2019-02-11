library(Matching)
# data
foo <- read.csv("https://course-resources.minerva.kgi.edu/uploaded_files/mke/00086677-3767/peace.csv")
# remove 2 rows with missing data (there are better ways to handle missing data)
foo <- foo[c(-19, -47), ]
# check that all missing data is gone...
which(is.na(foo) == TRUE)
# take a peek at the data set (identify the columns)
head(foo)
# part 2
# originalmodel
fit1 <- glm(pbs2s3 ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + develop
            + exp + decade + treaty + untype4, data = foo, family = binomial)
wardur1 <- c(1:315)
wartype1 <- rep(mean(foo$wartype), 315)
logcost1 <- rep(mean(foo$logcost), 315)
factnum1 <- rep(mean(foo$factnum), 315)
factnum21 <- rep(mean(foo$factnum2), 315)
trnsfcap1 <- rep(mean(foo$trnsfcap), 315)
develop1 <- rep(mean(foo$develop), 315)
exp1 <- rep(mean(foo$exp), 315)
decade1 <- rep(mean(foo$decade), 315)
treaty1 <- rep(mean(foo$treaty), 315)
untype41treated <- rep(1,315)
untype41control <- rep(0, 315)
treated1 <- data.frame("wartype" = wartype1, "logcost" = logcost1, "wardur" = wardur1,
                       "factnum" = factnum1, "factnum2" = factnum21, "trnsfcap" = trnsfcap1,
                       "develop" = develop1, "exp" = exp1, "decade" = decade1,
                       "treaty" = treaty1, "untype4" = untype41treated)
control1 <- data.frame("wartype" = wartype1, "logcost" = logcost1, "wardur" = wardur1,
                       "factnum" = factnum1, "factnum2" = factnum21, "trnsfcap" = trnsfcap1,
                       "develop" = develop1, "exp" = exp1, "decade" = decade1,
                       "treaty" = treaty1, "untype4" = untype41control)
marginaleffect1 <- predict(fit1, newdata = treated1, type = "response") - predict(fit1, newdata = control1, type = "response")
plot(treated1$wardur, marginaleffect1, type = "l", lty=3, ylim = c(0,0.8), xlim = c(0,315), xlab = "Duration of wars in months",
     ylab = "Marginal effects of UN peacekeeping operations", main = "Causal Effect of Multidimensional UN Peacekeeping Operations", labels=FALSE, tck=0)
# modifiedmodel
fit2 <- glm(pbs2s3 ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + develop
            + exp + decade + treaty + untype4 + untype4*wardur, data = foo, family = binomial)
interactionterm1 <- treated1$wardur*treated1$untype4
interactionterm2 <- control1$wardur*control1$untype4
treated2 <- cbind(treated1, "untype4*wardur" = interactionterm1)
control2 <- cbind(control1, "untype4*wardur" = interactionterm2)
marginaleffect2 <- predict(fit2, newdata = treated2, type = "response") - predict(fit2, newdata = control2, type = "response")
lines(treated2$wardur, marginaleffect2, ylim = c(0,0.8), xlim = c(0,315))
text(250, 0.35, "Dotted: Original model")
text(230, 0.75, "Model with interaction term")
axis(1, at = seq(0, 315, by = 15),tck=0.02,las=2)
axis(3, at = seq(0, 315, by = 15),tck=0.02,las=2, labels = FALSE)
axis(2, at = seq(0, 0.8, by = 0.1), tck=0.02)
axis(4, at = seq(0, 0.8, by = 0.1), tck=0.02, labels = FALSE)
# part 3
Tr <- rep(0, length(foo$untype))
Tr[which(foo$untype != "None")] <- 1
foo$untype <- Tr

# part 4
# logistic regression
# process data
aa <- rep(0, length(foo$untype))
aa[which(foo$pbs2l != "Failure")] <- 1
foo$pbs2l <- aa
foo5 <- foo[!is.na(foo$pbs5l),]
bb <- rep(0, length(foo5$untype))
bb[which(foo5$pbs5l != "Failure")] <- 1
foo5$pbs5l <- bb
# logreg 2 years
fit3 <- glm(pbs2l ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + develop
            + exp + decade + treaty + untype, data = foo, family = binomial)
tmteffect1 <- mean(predict(fit3,newdata = foo[which(foo$untype==1),],type="response")) - mean(predict(fit3,newdata=foo[which(foo$untype==0),],type="response"))
mb12 <- MatchBalance(untype ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + develop
                     + exp + decade + treaty, data = foo, nboots = 500)
# logreg 5 years
fit4 <- glm(pbs5l ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + develop
            + exp + decade + treaty + untype, data = foo5, family = binomial)
tmteffect2 <- mean(predict(fit4, newdata = foo5[which(foo5$untype==1),], type = "response")) - mean(predict(fit4, newdata = foo5[which(foo5$untype==0),], type = "response"))
mb15 <- MatchBalance(untype ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + develop
                     + exp + decade + treaty, data = foo5, nboots = 500)

# p-score matching
# p-score 2 years
set.seed(111)
glm1 <- glm(untype ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + develop + exp + decade, family = binomial, data = foo)
rr1a <- Match(Y=foo$pbs2l,Tr=foo$untype, X=glm1$fitted)
summary(rr1a, full=TRUE)
mb22a <- MatchBalance(untype ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + develop + exp + decade, match.out = rr1a, data = foo, nboots = 500)
rr1b <- Match(Y=foo$pbs2l, Tr=foo$untype, X=glm1$fitted, BiasAdjust = TRUE)
summary(rr1b, full=TRUE)
mb22b <- MatchBalance(untype ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + develop + exp + decade, match.out = rr1b, data = foo, nboots = 500)

# p-score 5 years
set.seed(222)
glm2 <- glm(untype ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + develop + exp + decade, family = binomial, data = foo5)
rr2a <- Match(Y=foo5$pbs5l,Tr=foo5$untype, X=glm2$fitted)
summary(rr1a, full=TRUE)
mb25a <- MatchBalance(untype ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + develop + exp + decade, match.out = rr2a, data = foo5, nboots = 500)
rr2b <- Match(Y=foo5$pbs5l, Tr=foo5$untype, X=glm2$fitted, BiasAdjust = TRUE)
mb25b <- MatchBalance(untype ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + develop + exp + decade, match.out = rr2b, data = foo5, nboots = 500)
summary(rr2b)

# GenMatch
library(rgenoud)
# genmatch 2 years
X1 = cbind(foo$wartype, foo$logcost, foo$wardur, foo$factnum, foo$factnum2, foo$trnsfcap, foo$develop, foo$exp, foo$decade)
genout1 <- GenMatch(Tr = foo$untype,X=X1,pop.size=200,max.generations=30,wait.generations = 1)
Y1 = foo$pbs2l
mout1a <- Match(Y=Y1, Tr=foo$untype, X=X1, Weight.matrix=genout1)
summary(mout1a)
mb_gm1a <- MatchBalance(foo$untype ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + develop + exp + decade, data = foo, match.out=mout1a)
mout1b <- Match(Y=Y1, Tr=foo$untype, X=X1, BiasAdjust = TRUE ,Weight.matrix=genout1)
summary(mout1b)
mb_gm1b <- MatchBalance(foo$untype ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + develop + exp + decade, data = foo, match.out=mout1b)

# genmatch 5 years
X2 = cbind(foo5$wartype, foo5$logcost, foo5$wardur, foo5$factnum, foo5$factnum2, foo5$trnsfcap, foo5$develop, foo5$exp, foo5$decade)
genout2 <- GenMatch(Tr = foo5$untype,X=X2,pop.size=200,max.generations=30,wait.generations = 1)
Y2 = foo5$pbs5l
mout2a <- Match(Y=Y2, Tr=foo5$untype, X=X2, Weight.matrix=genout2)
summary(mout2a)
mb_gm2a <- MatchBalance(foo5$untype ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + develop + exp + decade, data = foo5, match.out=mout2a)
mout2b <- Match(Y=Y2, Tr=foo5$untype, X=X2, BiasAdjust = TRUE, Weight.matrix=genout2)
summary(mout2b)
mb_gm2b <- MatchBalance(foo5$untype ~ wartype + logcost + wardur + factnum + factnum2 + trnsfcap + develop + exp + decade, data = foo5, match.out=mout2b)
