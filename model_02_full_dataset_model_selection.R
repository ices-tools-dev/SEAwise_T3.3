# 0. clean-up ----------------------------------------------------------------

rm(list = ls())



# 1. load data objects -------------------------------------------------------
load("data/data_list_DF.Rdata")
load("data/lutLWcoefs.Rdata")

# 2. required functions ---------------------------------------------------
library(lmmlasso)
library(lmmen) # fork that handles multiple RE
library(cv.glmmLasso) # fork that handles multiple RE
library(glmmTMB)
library(MuMIn)
library(doParallel)


# 3. set-up ------------------------------------------------------------------

# scenarios / models
scen <- data.frame(name = c("lasso_BIC", "lasso_CV10perc", 
                            "lasso_CV20perc", "glmmTMB_AICc"))

# LMM inputs
#global model of weight-at-age without random effects
fullform <- logw2~logw + age+I(age^2)+I(age^3) + sal.s + ssb.s + temp.s +I(temp.s^2) + 
  logw:age +  logw:sal.s + logw:ssb.s + logw:temp.s

#global model of weight-at-age with random effects
fullformRE <- logw2~logw + age+I(age^2)+I(age^3) + sal.s  + ssb.s + temp.s +I(temp.s^2)  + 
  logw:age + logw:sal.s + logw:ssb.s + logw:temp.s +(1|cohortf) + (1|yearf)

#global model of recruit weights 
fullform.rec <- logw ~ sal.s + ssb.s + temp.s + I(temp.s^2) 

#range of penalty terms to try in LASSO methods
lambdas <-  seq(50, 0, by = -0.5)

#set up cluster 
ncores <- detectCores() # max available cores
clusterType <- ifelse(Sys.info()["sysname"] == "Windows", "PSOCK", "FORK")
mycl <- parallel::makeCluster(ncores, type=clusterType)  
doParallel::registerDoParallel(mycl)
invisible(clusterEvalQ(mycl, library(glmmTMB , logical = TRUE)))

# stk - stock loop START --------------------------------------------------------

STKs <- names(DF)[-1]
RES <- list()
REC <- list()

for(stk in STKs){
  
  dat <- DF[[stk]]
  
  dat$covar1 <- dat$ssb # for use in laaGrowth models
  
  dat <- transform(dat, logw2=log(w2), logw=log(w), 
                   ssb.s=scale(ssb), sal.s=scale(sal), temp.s=scale(temp),
                   yearf=factor(year), cohortf=factor(cohort))
  
  #need to make sure environmental variables are available
  dat <- na.omit(dat)
  
  ranYear <- range(dat$year)
  minage <- min(dat$age, na.rm = TRUE)
  
  # s - scenario loop START ----

  for(s in seq(nrow(scen))){
    
    # lasso_BIC -----
    if(scen$name[s] == "lasso_BIC"){ 
      gFit <- lmmen::cv.glmmLasso(form.fixed=fullform, 
                                  form.rnd = list(yearf=~1, cohortf=~1), 
                                  dat=dat, 
                                  lambda = lambdas)$fit.opt
      mod <- summary(gFit)$coefficients[,"Estimate"]
    }
    
    # lasso_CV10perc -----
    if(scen$name[s] == "lasso_CV10perc"){ 
      gFit <- cv.glmmLasso::cv.glmmLasso(fix=fullform, 
                                         rnd = list(yearf=~1, cohortf=~1), 
                                         data=dat, 
                                         family = gaussian(link = "identity"), 
                                         kfold = 10, 
                                         lambdas =  lambdas)$glmmLasso.final
      mod <- summary(gFit)$coefficients[,"Estimate"]
    } 
    
    # lasso_CV20perc -----
    if(scen$name[s] == "lasso_CV20perc"){ 
      gFit <- cv.glmmLasso::cv.glmmLasso(fix=fullform, 
                                         rnd = list(yearf=~1, cohortf=~1), 
                                         data=dat, 
                                         family = gaussian(link = "identity"), 
                                         kfold = 5, 
                                         lambdas =  lambdas)$glmmLasso.final
      mod <- summary(gFit)$coefficients[,"Estimate"]
    } 
    
    # glmmTMB_AICc -----
    if(scen$name[s] == "glmmTMB_AICc"){ 
      global.model1 <- glmmTMB(fullformRE,
                               data=dat, na.action = na.fail)
      clusterExport(mycl, "dat")
      dma1 <- dredge(global.model1, rank = "AICc", cluster=mycl)
      gFit <- get.models(dma1, subset= 1)[[1]]
      mod <- rep(0, length(fixef(global.model1)$cond))
      names(mod) <- names(fixef(global.model1)$cond)
      
      mod[names(fixef(gFit)$cond)] <- fixef(gFit)$cond
    }      
    
    RES[[paste(stk, scen$name[s], sep="_")]] <- list(mod=mod, stk=stk, scen=scen$name[s])
  } # end of scen loop ----
  
  # model recruit weights
  tmp <- subset(dat, age==minage)
  global.model <- glmmTMB(fullform.rec,
                          data=tmp, na.action = na.fail)
  clusterExport(mycl, "tmp")
  dma <- dredge(global.model, rank = "AICc", cluster = mycl)
  minage.i <- get.models(dma, subset= 1)[[1]]
  mod <- rep(0, length(fixef(global.model)$cond))
  names(mod) <- names(fixef(global.model)$cond)
  
  mod[names(fixef(minage.i)$cond)] <- fixef(minage.i)$cond
  REC[[stk]] <- mod
} # end of stk loop ----


save(RES, file = paste0("model/allLMMs.Rdata"))
save(REC, file = paste0("model/allrecLMs.Rdata"))

parallel::stopCluster(mycl)

