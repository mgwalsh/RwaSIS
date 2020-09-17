# Stacked spatial predictions of Rwanda soil properties
# M. Walsh, September 2020

# Required packages
# install.packages(c("devtools","caret","mgcv","MASS","randomForest","gbm","nnet","plyr","doParallel","dismo")), dependencies=T)
suppressPackageStartupMessages({
  require(devtools)
  require(caret)
  require(mgcv)
  require(MASS)
  require(randomForest)
  require(gbm)
  require(nnet)
  require(plyr)
  require(doParallel)
  require(dismo)
})

# Data setup --------------------------------------------------------------
rm(list=setdiff(ls(), c("gsdat","grids","glist"))) ## scrub extraneous objects in memory
gsdat <- gsdat[complete.cases(gsdat[ ,c(24:77)]),] ## removes incomplete cases

# set calibration/validation set randomization seed
seed <- 12358
set.seed(seed)

# split data into calibration and validation sets
gsIndex <- createDataPartition(gsdat$pH, p = 4/5, list = F, times = 1)
gs_cal <- gsdat[ gsIndex,]
gs_val <- gsdat[-gsIndex,]

# Soil calibration labels
labs <- c("pH") ## insert other labels (e.g. "C","N","P","K" ...) here!
lcal <- as.vector(t(gs_cal[labs]))

# raster calibration features
fcal <- gs_cal[,24:46,50:77]

# Spatial trend model <mgcv> -----------------------------------------------
# select central place covariates
gf_cpv <- gs_cal[,47:49]

# start doParallel to parallelize model fitting
mc <- makeCluster(detectCores())
registerDoParallel(mc)

# control setup
set.seed(1385321)
tc <- trainControl(method = "cv", allowParallel = T)

# model training
gm <- train(gf_cpv, lcal, 
            method = "gam",
            preProc = c("center","scale"), 
            metric = "RMSE",
            trControl = tc)

# model outputs & predictions
summary(gm)
gm.pred <- predict(grids, gm) ## spatial predictions
stopCluster(mc)
fname <- paste("./Results/", labs, "_gm.rds", sep = "")
saveRDS(gm, fname)

# Central place theory model <glm> -----------------------------------------
# select central place covariates
gf_cpv <- gs_cal[,33:46]

# start doParallel to parallelize model fitting
mc <- makeCluster(detectCores())
registerDoParallel(mc)

# control setup
set.seed(1385321)
tc <- trainControl(method = "cv", classProbs = T, allowParallel = T)

# model training
gl1 <- train(gf_cpv, lcal, 
             method = "glmStepAIC",
             preProc = c("center","scale"), 
             trControl = tc,
             metric = "RMSE")

# model outputs & predictions
summary(gl1)
print(gl1) ## ROC's accross cross-validation
gl1.pred <- predict(grids, gl1) ## spatial predictions
stopCluster(mc)
fname <- paste("./Results/", labs, "_gl1.rds", sep = "")
saveRDS(gl1, fname)

# GLM with all covariates -------------------------------------------------
# start doParallel to parallelize model fitting
mc <- makeCluster(detectCores())
registerDoParallel(mc)

# control setup
set.seed(1385321)
tc <- trainControl(method = "cv", allowParallel = T)

# model training
gl2 <- train(fcal, lcal, 
             method = "glmStepAIC",
             preProc = c("center","scale"), 
             trControl = tc,
             metric ="RMSE")

# model outputs & predictions
summary(gl2)
print(gl2)
gl2.pred <- predict(grids, gl2) ## spatial predictions
stopCluster(mc)
fname <- paste("./Results/", labs, "_gl2.rds", sep = "")
saveRDS(gl2, fname)

# Random forest <randomForest> --------------------------------------------
# start doParallel to parallelize model fitting
mc <- makeCluster(detectCores())
registerDoParallel(mc)

# control setup
set.seed(1385321)
tc <- trainControl(method = "cv", allowParallel = T)
tg <- expand.grid(mtry = seq(5,10, by=1)) ## model tuning steps

# model training
rf <- train(fcal, lcal,
            preProc = c("center","scale"),
            method = "rf",
            ntree = 501,
            metric = "RMSE",
            tuneGrid = tg,
            trControl = tc)

# model outputs & predictions
print(rf) ## RMSE's accross tuning parameters
rf.pred <- predict(grids, rf) ## spatial predictions
stopCluster(mc)
fname <- paste("./Results/", labs, "_rf.rds", sep = "")
saveRDS(rf, fname)

# Generalized boosting <gbm> ----------------------------------------------
# start doParallel to parallelize model fitting
mc <- makeCluster(detectCores())
registerDoParallel(mc)

# control setup
set.seed(1385321)
tc <- trainControl(method = "cv", allowParallel = T)

## for initial <gbm> tuning guidelines see @ https://stats.stackexchange.com/questions/25748/what-are-some-useful-guidelines-for-gbm-parameters
tg <- expand.grid(interaction.depth = seq(2,5, by=1), shrinkage = 0.01, n.trees = seq(101,501, by=50),
                  n.minobsinnode = 50) ## model tuning steps

# model training
gb <- train(fcal, lcal, 
            method = "gbm", 
            preProc = c("center", "scale"),
            trControl = tc,
            tuneGrid = tg,
            metric = "RMSE")

# model outputs & predictions
print(gb) ## RMSE's accross tuning parameters
gb.pred <- predict(grids, gb) ## spatial predictions
stopCluster(mc)
fname <- paste("./Results/", labs, "_gb.rds", sep = "")
saveRDS(gb, fname)

# Cubist <Cubist> ---------------------------------------------------------
# start doParallel to parallelize model fitting
mc <- makeCluster(detectCores())
registerDoParallel(mc)

# control setup
set.seed(seed)
tc <- trainControl(method="repeatedcv", number=10, repeats=3, allowParallel = T)
# tg <- needs tuning

cu <- train(fcal, lcal, 
            method = "cubist", 
            trControl = tc)

print(cu)
stopCluster(mc)
fname <- paste("./Results/", labs, "_cu.rds", sep = "")
saveRDS(cu, fname)


# Model stacking setup ----------------------------------------------------
preds <- stack(gm.pred, gl1.pred, gl2.pred, rf.pred, gb.pred, nn.pred)
names(preds) <- c("gm","gl1","gl2","rf","gb","nn")
plot(preds, axes = F)

# extract model predictions
coordinates(gs_val) <- ~x+y
projection(gs_val) <- projection(preds)
gspred <- extract(preds, gs_val)
gspred <- as.data.frame(cbind(gs_val, gspred))

# stacking model validation labels and features
gs_val <- as.data.frame(gs_val)
lval <- as.vector(t(gs_val[labs]))
fval <- gspred[,62:67] ## subset validation features

# Model stacking ----------------------------------------------------------
# start doParallel to parallelize model fitting
mc <- makeCluster(detectCores())
registerDoParallel(mc)

# control setup
set.seed(1385321)
tc <- trainControl(method = "cv", classProbs = T, 
                   summaryFunction = twoClassSummary, allowParallel = T)

# model training
si <- train(fval, lval,
            method = "glm",
            family = "binomial",
            metric = "ROC",
            trControl = tc)

# model outputs & predictions
summary(si)
print(si)
si.pred <- predict(preds, si, type = "prob") ## spatial predictions
plot(si.pred, axes = F)
stopCluster(mc)
fname <- paste("./Results/", labs, "_si.rds", sep = "")
saveRDS(si, fname)

# Receiver-operator characteristics ---------------------------------------
cp_pre <- predict(si, fval, type="prob")
cp_val <- cbind(lval, cp_pre)
cpa <- subset(cp_val, cp_val=="A", select=c(A))
cpb <- subset(cp_val, cp_val=="B", select=c(A))
cp_eval <- evaluate(p=cpa[,1], a=cpb[,1]) ## calculate ROC's on test set
plot(cp_eval, 'ROC') ## plot ROC curve

# Generate feature mask ---------------------------------------------------
t <- threshold(cp_eval) ## calculate thresholds based on ROC
r <- matrix(c(0, t[,1], 0, t[,1], 1, 1), ncol=3, byrow = T) ## set threshold value <kappa>
mask <- reclassify(si.pred, r) ## reclassify stacked predictions
plot(mask, axes=F)

# Write prediction grids --------------------------------------------------
gspreds <- stack(preds, si.pred, mask)
names(gspreds) <- c("gm","gl1","gl2","rf","gb","nn","si","mk")
fname <- paste("./Results/","OAF_", labs, "_preds_2020.tif", sep = "")
writeRaster(gspreds, filename=fname, datatype="FLT4S", options="INTERLEAVE=BAND", overwrite=T)

# Site index prediction check ---------------------------------------------
coordinates(gsdat) <- ~x+y
projection(gsdat) <- projection(grids)
gspre <- extract(gspreds, gsdat)
gsout <- as.data.frame(cbind(gsdat, gspre))
gsout$mzone <- as.factor(ifelse(gsout$mk == 1, "A", "B"))
confusionMatrix(gsout$mzone, gsout$qy) ## overall prediction accuracy stats
boxplot(yield~mzone, notch=T, xlab="Management zone", ylab="Measured yield (t/ha)",
        cex.lab=1.3, gsout) ## yield differences between predicted site index zones

# Maize yield potentials (t/ha) ------------------------------------------
yld.lme <- lmer(log(yield)~factor(trt)*si+I(dap/50)*I(can/50)+(1|year)+(1|GID), data = gsout)
summary(yld.lme) ## mixed model yield estimate results
gsout$yldf <- exp(fitted(yld.lme, gsout))

# Quantile regression (uncertainty) plot
par(pty="s")
par(mfrow=c(1,1), mar=c(5,5,1,1))
plot(yield~yldf, xlab="Maize yield prediction (t/ha)", ylab="Measured yield (t/ha)", cex.lab=1.3, 
     xlim=c(-1,15), ylim=c(-1,15), gsout)
stQ <- rq(yield~yldf, tau=c(0.05,0.5,0.95), data=gsout)
print(stQ)
curve(stQ$coefficients[2]*x+stQ$coefficients[1], add=T, from=0, to=15, col="blue", lwd=2)
curve(stQ$coefficients[4]*x+stQ$coefficients[3], add=T, from=0, to=15, col="red", lwd=2)
curve(stQ$coefficients[6]*x+stQ$coefficients[5], add=T, from=0, to=15, col="blue", lwd=2)
abline(c(0,1), col="grey", lwd=1)

# Write output data frame -------------------------------------------------
fname <- paste("./Results/","OAF_", labs, "_out.csv", sep = "")
write.csv(gsout, fname, row.names = F)

