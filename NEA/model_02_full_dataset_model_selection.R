# 0. clean-up ----------------------------------------------------------------

rm(list = ls())


# 1. load data objects -------------------------------------------------------
load("NEA/data/data_list_DF.Rdata")
load("NEA/data/lutLWcoefs.Rdata")

# 2. required functions ---------------------------------------------------
library(lmmlasso)
library(lmmen) # 
library(cv.glmmLasso) 
library(glmmTMB)
library(MuMIn)
library(doParallel)
source("NEA/R/laaGrowth.R") # function to fit length increment growth models


# 3. set-up ------------------------------------------------------------------

# scenarios / models
scen <- data.frame(name = #c("laaGrowth","glmmTMB_AICc", "glmmTMB_GAMM_AICc"))
                  c("laaGrowth", "lasso_BIC", "lasso_CV10perc", 
                            "lasso_CV20perc", "glmmTMB_AICc", "glmmTMB_GAMM_AICc"))

nTry <- 20 # for use in laaGrowth models

# LMM inputs
#global model of weight-at-age without random effects
fullform <- logw2~logw + age+I(age^2)+logw:I(age^2) + sal.s + ssb.s + temp.s +I(temp.s^2) + 
  logw:age +  logw:sal.s + logw:ssb.s + logw:temp.s

#global model of weight-at-age with random effects
fullformRE <- logw2~logw + age+I(age^2)+logw:I(age^2) + sal.s  + ssb.s + temp.s +I(temp.s^2)  + 
  logw:age + logw:sal.s + logw:ssb.s + logw:temp.s +(1|cohortf) + (1|yearf)

#global model of weight-at-age GAMM
fullformGAMM <- logw2~logw + s(age, k=3) + sal.s  + ssb.s + s(temp.s, k=3)  + 
  logw:age + logw:sal.s + logw:ssb.s + logw:temp.s +(1|cohortf) + (1|yearf)

#global model of recruit weights 
fullform.rec <- logw ~ sal.s + ssb.s + temp.s + I(temp.s^2) 

#global GAM of recruit weights 
fullformGAM.rec <- logw ~ sal.s + ssb.s + s(temp.s, k=3) 

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
  
  dat <- na.omit(dat)
  
  dat <- transform(dat, logw2=log(w2), logw=log(w), 
                   ssb.s=scale(ssb), sal.s=scale(sal), temp.s=scale(temp),
                   yearf=factor(year), cohortf=factor(cohort))
  # save scale attributes to recreate scaling
  ssb.mu <- mean(dat$ssb)
  ssb.sd <- sqrt(var(dat$ssb))
  temp.mu <- mean(dat$temp)
  temp.sd <- sqrt(var(dat$temp))
  sal.mu <- mean(dat$sal)
  sal.sd <- sqrt(var(dat$sal))
  
  ranYear <- range(dat$year)
  minage <- min(dat$age, na.rm = TRUE)
  
  # s - scenario loop START ----

  for(s in seq(nrow(scen))){
    # laaGrowth -----
    if(scen$name[s] == "laaGrowth"){ 
      gFit <- laaGrowth(dat = dat, nTry = nTry) 
      save(gFit, file=paste0("NEA/model/fit_by_stk_n_method/", stk, "_", scen$name[s],".Rdata"))
    }
    
    # lasso_BIC -----
    if(scen$name[s] == "lasso_BIC"){ 
      gFit0 <- lmmen::cv.glmmLasso(form.fixed=fullform, 
                                  form.rnd = list(yearf=~1, cohortf=~1), 
                                  dat=dat, 
                                  lambda = lambdas)$fit.opt
      fix2 <- gFit0$fix
      keep <- abs(summary(gFit0)$coefficients[,"Estimate"])>1e-6
      if(length(which(!keep))>0){
        tt <- drop.terms(terms(fix2), dropx = which(!keep)-1, keep.response = TRUE)
        newform <- formula(tt)
        gFit <- lmmen::cv.glmmLasso(form.fixed=newform, 
                                    form.rnd = list(yearf=~1, cohortf=~1), 
                                    dat=dat, 
                                    lambda = lambdas)$fit.opt
      }else{
        gFit<- gFit0
      }
      
      save(gFit, gFit0,  ssb.mu, ssb.sd, temp.mu, temp.sd, sal.mu, sal.sd, 
           file=paste0("NEA/model/fit_by_stk_n_method/", stk, "_", scen$name[s],".Rdata"))
      mod <- summary(gFit0)$coefficients[,"Estimate"]
    }
    
    # lasso_CV10perc -----
    if(scen$name[s] == "lasso_CV10perc"){ 
      gFit0 <- cv.glmmLasso::cv.glmmLasso(fix=fullform, 
                                         rnd = list(yearf=~1, cohortf=~1), 
                                         data=dat, 
                                         family = gaussian(link = "identity"), 
                                         kfold = 10, 
                                         lambdas =  lambdas)$glmmLasso.final
      fix2 <- gFit0$fix
      keep <- abs(summary(gFit0)$coefficients[,"Estimate"])>1e-6
      if(length(which(!keep))>0){
        tt <- drop.terms(terms(fix2), dropx = which(!keep)-1, keep.response = TRUE)
        newform <- formula(tt)
        gFit <- cv.glmmLasso::cv.glmmLasso(fix=newform, 
                                           rnd = list(yearf=~1, cohortf=~1), 
                                           data=dat, 
                                           family = gaussian(link = "identity"), 
                                           kfold = 10, 
                                           lambdas =  lambdas)$glmmLasso.final
      }else{
        gFit<- gFit0
      }
      save(gFit, gFit0,  ssb.mu, ssb.sd, temp.mu, temp.sd, sal.mu, sal.sd, 
           file=paste0("NEA/model/fit_by_stk_n_method/", stk, "_", scen$name[s],".Rdata"))
      mod <- summary(gFit0)$coefficients[,"Estimate"]
    } 
    
    # lasso_CV20perc -----
    if(scen$name[s] == "lasso_CV20perc"){ 
      gFit0 <- cv.glmmLasso::cv.glmmLasso(fix=fullform, 
                                         rnd = list(yearf=~1, cohortf=~1), 
                                         data=dat, 
                                         family = gaussian(link = "identity"), 
                                         kfold = 5, 
                                         lambdas =  lambdas)$glmmLasso.final
      fix2 <- gFit0$fix
      keep <- abs(summary(gFit0)$coefficients[,"Estimate"])>1e-6
      if(length(which(!keep))>0){
        tt <- drop.terms(terms(fix2), dropx = which(!keep)-1, keep.response = TRUE)
        newform <- formula(tt)
        gFit <- cv.glmmLasso::cv.glmmLasso(fix=newform, 
                                          rnd = list(yearf=~1, cohortf=~1), 
                                          data=dat, 
                                          family = gaussian(link = "identity"), 
                                          kfold = 5, 
                                          lambdas =  lambdas)$glmmLasso.final
      }else{
        gFit<- gFit0
      }
      save(gFit, gFit0,  ssb.mu, ssb.sd, temp.mu, temp.sd, sal.mu, sal.sd, 
           file=paste0("NEA/model/fit_by_stk_n_method/", stk, "_", scen$name[s],".Rdata"))
      mod <- summary(gFit0)$coefficients[,"Estimate"]
    } 
    
    # glmmTMB_AICc -----
    if(scen$name[s] == "glmmTMB_AICc"){ 
      global.model1 <- glmmTMB(fullformRE,
                               data=dat, na.action = na.fail)
      clusterExport(mycl, "dat")
      dma1 <- dredge(global.model1, rank = "AICc", cluster=mycl)
      gFit <- get.models(dma1, subset= 1)[[1]]
      save(gFit, ssb.mu, ssb.sd, temp.mu, temp.sd, sal.mu, sal.sd, 
           file=paste0("NEA/model/fit_by_stk_n_method/", stk, "_", scen$name[s],".Rdata"))
      
      mod <- rep(0, length(fixef(global.model1)$cond))
      names(mod) <- names(fixef(global.model1)$cond)
      
      mod[names(fixef(gFit)$cond)] <- fixef(gFit)$cond
    } 
    # glmmTMB_GAMM_AICc -----
    if(scen$name[s] == "glmmTMB_GAMM_AICc"){ 
      global.model1 <- glmmTMB(fullformGAMM,
                               data=dat, na.action = na.fail)
      clusterExport(mycl, "dat")
      dma1 <- dredge(global.model1, rank = "AICc", cluster=mycl)
      gFit <- get.models(dma1, subset= 1)[[1]]
      save(gFit, ssb.mu, ssb.sd, temp.mu, temp.sd, sal.mu, sal.sd, 
           file=paste0("NEA/model/fit_by_stk_n_method/", stk, "_", scen$name[s],".Rdata"))
      
      mod <- rep(0, length(fixef(global.model1)$cond))
      names(mod) <- names(fixef(global.model1)$cond)
      
      mod[names(fixef(gFit)$cond)] <- fixef(gFit)$cond
    }      
    
    #save LMMs
    if(!scen$name[s] %in% c("laaGrowth", "glmmTMB_GAMM_AICc")){
      RES[[paste(stk, scen$name[s], sep="_")]] <- list(mod=mod, stk=stk, scen=scen$name[s])
    }
    
    # model recruit weights
    tmp <- subset(dat, age==minage)
    
    if(scen$name[s] =="glmmTMB_GAMM_AICc"){
      global.model <- glmmTMB(fullformGAM.rec,
                              data=tmp, na.action = na.fail)
    }else{
      global.model <- glmmTMB(fullform.rec,
                              data=tmp, na.action = na.fail)
    }
    clusterExport(mycl, "tmp")
    dma <- dredge(global.model, rank = "AICc", cluster = mycl)
    minage.i <- get.models(dma, subset= 1)[[1]]
    mod <- rep(0, length(fixef(global.model)$cond))
    names(mod) <- names(fixef(global.model)$cond)
    
    mod[names(fixef(minage.i)$cond)] <- fixef(minage.i)$cond
    
    #save recruitment LMs
    if(!scen$name[s] %in% c("laaGrowth", "glmmTMB_GAMM_AICc")){
      REC[[paste(stk, scen$name[s], sep="_")]] <- mod
    }
    save(minage.i, ssb.mu, ssb.sd, temp.mu, temp.sd, sal.mu, sal.sd, 
         file=paste0("NEA/model/fit_by_stk_n_method/recruits_", stk, "_", scen$name[s],".Rdata"))
    
  } # end of scen loop ----
  
    
} # end of stk loop ----


save(RES, file = paste0("NEA/model/allLMMs.Rdata"))
save(REC, file = paste0("NEA/model/allrecLMs.Rdata"))

parallel::stopCluster(mycl)

