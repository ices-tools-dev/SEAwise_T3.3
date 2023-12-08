# 0. clean-up ----------------------------------------------------------------

rm(list = ls())



# 1. load data objects -------------------------------------------------------
load("data/data_list_DF.Rdata")
load("data/lutLWcoefs.Rdata")

# 2. required functions ---------------------------------------------------

source("R/laaGrowth.R") # function to fit length increment growth models
source("R/growmod_selfun.R")


# 3. set-up ------------------------------------------------------------------

maxNForecastYear <- 15 # max number of years to forcast
nTry <- 20 # for use in laaGrowth models

nYearsNaive <- 3 # number of data years to use for naive model fitting
      
# scenarios / models
scen <- data.frame(name = c("naive", "laaGrowth"))#, "lasso_BIC", 
  # "lasso_CV10perc",  "lasso_CV20perc", "growmod_BIC"))

# glmmLasso inputs
fullform <- logw2~logw + age+I(age^2)+I(age^3) + nao.s + ssb.s + sst.s + 
    logw:age +logw:I(age^2)+ logw:I(age^3) + logw:nao.s + logw:ssb.s + logw:sst.s

lambdas <-  seq(50, 0, by = -0.5)

# stk - stock loop START --------------------------------------------------------

STKs <- names(DF)[-1]
for(stk in STKs){
  # stk <- "cod.27.7a"
  dat <- DF[[stk]]
  
  dat$covar1 <- dat$ssb # for use in laaGrowth models
   
  dat <- transform(dat, logw2=log(w2), logw=log(w), 
    ssb.s=scale(ssb), sal.s=scale(sal), temp.s=scale(temp),
    yearf=factor(year), cohortf=factor(cohort))
  
  ranYear <- range(dat$year)
  minage <- min(dat$age, na.rm = TRUE)

  # s - scenario loop START ----
  RES <- vector("list", nrow(scen))
  for(s in seq(nrow(scen))){ 

    # i - moving window loop START ----
    res <- vector("list", maxNForecastYear)
    for(i in rev(seq(maxNForecastYear))){
      set.seed(1234+i)
      
      maxDataYear <- ranYear[2] - i
      
      dat.train <- subset(dat, year <= maxDataYear)
      dat.valid <- subset(dat, year > maxDataYear)
      # naive training data gets the last three years of data from dat.train
      # new subset accommodates stocks with breaks in allowed data, e.g "cod.27.7a"
      dat.train.naive <- subset(dat.train, 
        year %in% rev(sort(unique(dat.train$year)))[seq(nYearsNaive)] | 
        year2 %in% rev(sort(unique(dat.train$year)))[seq(nYearsNaive)])
      tmp1 <- dat.train.naive[, c("year", "age", "l", "w")]
      tmp2 <- dat.train.naive[, c("year2", "age2", "l2", "w2")]
      names(tmp2) <- c("year", "age", "l", "w")
      dat.train.naive <- unique(rbind(tmp1, tmp2))
      dat.train.naive <- subset(dat.train.naive, year <= maxDataYear)
      dat.train.naive <- dat.train.naive[order(dat.train.naive$year, dat.train.naive$age),]
      dat.train.naive <- subset(dat.train.naive, year %in% rev(sort(unique(dat.train.naive$year)))[seq(nYearsNaive)])
      
      # 4. model fitting -----------------------------------------------------
      
      # naive model  (always fit for use in starting sizes) -----
      nFitL <- lm(l ~ factor(age), data = dat.train.naive)
      nFitW <- lm(w ~ factor(age), data = dat.train.naive)

      # laaGrowth -----
      if(scen$name[s] == "laaGrowth"){ 
        gFit <- laaGrowth(dat = dat.train, nTry = nTry) 
      }
      
      # lasso_BIC -----
      if(scen$name[s] == "lasso_BIC"){ 
        gFit <- lmmen::cv.glmmLasso(form.fixed=fullform, 
          form.rnd = list(yearf=~1, cohortf=~1), 
          dat=dat.train, 
          lambda = lambdas)$fit.opt
        gFit$name <- scen$name[s]
      }
      
      # lasso_CV10perc -----
      if(scen$name[s] == "lasso_CV10perc"){ 
        gFit <- cv.glmmLasso::cv.glmmLasso(fix=fullform, 
          rnd = list(yearf=~1, cohortf=~1), 
          data=dat.train, 
          family = gaussian(link = "identity"), kfold = 10, 
          lambdas =  lambdas)$glmmLasso.final
        gFit$name <- scen$name[s]
      }
      
      # lasso_CV20perc -----
      if(scen$name[s] == "lasso_CV20perc"){ 
        gFit <- cv.glmmLasso::cv.glmmLasso(fix=fullform, 
          rnd = list(yearf=~1, cohortf=~1), 
          data=dat.train, 
          family = gaussian(link = "identity"), kfold = 5, 
          lambdas =  lambdas)$glmmLasso.final
        gFit$name <- scen$name[s]
      }
      
      # growmod_BIC -----
      if(scen$name[s] == "growmod_BIC"){ 
        # variables growmod needs
        tmp <- transform(dat.train, size=log(w2), prevsize=log(w), 
          t=year, ID=cohort, t0=cohort+minage) 
        tmp <- subset(tmp, t0>=min(tmp$year))
        
        # Fit all models in parallel 
        mods.s <- foreach(i=1:nrow(forms2.s), .combine=rbind) %dopar% { 
          source("R/growmod_selfun.R")
          growmod_BIC(tmp, i)
        }
          
        # Best model
        gFit <- with(forms2.s[which.min(mods.s),], growmod(formulaX =as.formula(formulaX[[1]]), 
          formulaM =as.formula(formulaM[[1]]), 
          data=tmp))

        gFit$name <- scen$name[s]
      } 
        

      # 5. model forecast -----------------------------------------------------
      
      # set-up -----
      dat.valid$l.orig <- dat.valid$l # pass original size1 values to new variable
      dat.valid$l2.orig <- dat.valid$l2 # pass original size2 values to new variable
      dat.valid$w.orig <- dat.valid$w # pass original size1 values to new variable
      dat.valid$w2.orig <- dat.valid$w2 # pass original size2 values to new variable
      fcYears <- unique(dat.valid$year) # define forecast years (start of growth increment)
      hit <- which(dat.valid$year > min(fcYears)) # which data is after first forecast year
      dat.valid$l[hit] <- dat.valid$w[hit] <- dat.valid$logw[hit] <- NaN # set size1 values from all but first forecast year to NaN
      dat.valid$l2 <- dat.valid$w2 <- dat.valid$logw2 <- NaN # set all size2 values to NaN
      
      # 5.1. naive forecast ----
      if(scen$name[s] == "naive"){
        # predict w,l at age
        NAs <- which(is.na(dat.valid$w))
        newdat <- data.frame(age = dat.valid$age[NAs])
        dat.valid$w[NAs] <- predict(object = nFitW, newdata = newdat)
        dat.valid$l[NAs] <- predict(object = nFitL, newdata = newdat)
        
        # predict w2,l2 at age2
        NAs <- which(is.na(dat.valid$w2))
        newdat <- data.frame(age = dat.valid$age2[NAs])
        dat.valid$w2[NAs] <- predict(object = nFitW, newdata = newdat)
        dat.valid$l2[NAs] <- predict(object = nFitL, newdata = newdat)

        dat.valid$fcYear <- dat.valid$year - maxDataYear
        dat.valid$nFitYear <- 3
        dat.valid$best <- "naive"
        
      }else{
      
      # 5.2. growth model forecasts (loop) ----
      # here we will cycle through all validation years to track the growth of cohorts
      # loop only required for non-naive models
        for(y in fcYears){
          hit.y <- which(dat.valid$year == y)
          
          dat.valid.y <- dat.valid[hit.y,] # subset validation dataset to year y
          if(scen$name[s] == "laaGrowth"){ 
            dat.valid.y$l2 <- predict(gFit, newdata = dat.valid.y) # predict size2
            dat.valid$l2[hit.y] <- dat.valid.y$l2 # pass predictions back to main dat.valid
            LWab <- subset(lutLWcoefs, stockcode == stk)
            dat.valid.y$w2 <- (LWab$a*dat.valid.y$l2^LWab$b)/1000
            dat.valid$w2[hit.y] <- dat.valid.y$w2 # pass predictions back to main dat.valid
          }
          if(scen$name[s] == "growmod_BIC"){
            dat.valid.y$l2 <- NA
            dat.valid$l2[hit.y] <- dat.valid.y$l2 # pass predictions back to main dat.valid
            tmp <- dat.valid.y
            tmp$prevsize <- log(tmp$w) #predict.growmod needs "prevsize"
            dat.valid.y$w2 <- predict(gFit, tmp, exp=TRUE) # predict on real scale
            dat.valid$w2[hit.y] <- dat.valid.y$w2 # pass predictions back to main dat.valid
            #add log scale in case it's needed later
            dat.valid.y$logw2 <- log(dat.valid.y$w2)
            dat.valid$logw2[hit.y] <- dat.valid.y$logw2 # pass predictions back to main dat.valid
          }
          if(scen$name[s] %in% c("lasso_BIC", "lasso_CV10perc", "lasso_CV20perc")){
            dat.valid.y$l2 <- NA
            dat.valid$l2[hit.y] <- dat.valid.y$l2 # pass predictions back to main dat.valid
            dat.valid.y$logw2 <- -1000 #bug where it won't predict if there's an NA here
            dat.valid.y$logw2 <- predict(gFit, newdata = dat.valid.y) # predict size2
            dat.valid$logw2[hit.y] <- dat.valid.y$logw2 # pass predictions back to main dat.valid
            #predict on real scale with all 3 variance components included
            tot.var <- gFit$phi + sum(unlist(gFit$StdDev)^2) #phi is residual variance
            dat.valid.y$w2 <- exp(dat.valid.y$logw2 + tot.var/2) #lognormal mean
            dat.valid$w2[hit.y] <- dat.valid.y$w2 # pass predictions back to main dat.valid
          }
          
          # if forecast year is not the last one, pass size2 values to next year's size1 values, plus naive new recruit size1
          if(y < max(fcYears)){ 
            hit.yplus1 <- which(dat.valid$year == (y+1))
            dat.valid.yplus1 <- dat.valid[hit.yplus1,]
            
            # pass other sizes one age forward
            mat.y <- match(dat.valid.yplus1$age, dat.valid.y$age2)
            dat.valid.yplus1$l <- dat.valid.y$l2[mat.y]
            dat.valid.yplus1$w <- dat.valid.y$w2[mat.y]
            dat.valid.yplus1$logw <- dat.valid.y$logw2[mat.y]
            
            # naive model for new cohorts ----
            recruits <- which(is.na(dat.valid.yplus1$w))
            if(scen$name[s] == "laaGrowth"){ 
              dat.valid.yplus1$l[recruits] <- predict(object = nFitL, newdata = dat.valid.yplus1[recruits,])
              dat.valid.yplus1$w[recruits] <- predict(object = nFitW, newdata = dat.valid.yplus1[recruits,])
              dat.valid$l[hit.yplus1] <- dat.valid.yplus1$l # pass predictions back to main dat.valid
              dat.valid$w[hit.yplus1] <- dat.valid.yplus1$w
            } else { #scen$name[s] %in% c("lasso_BIC", "lasso_CV10perc", "lasso_CV20perc", "growmod_BIC")
              dat.valid.yplus1$logw[recruits] <- predict(object = minage.i, newdata = dat.valid.yplus1[recruits,])
              dat.valid$logw[hit.yplus1] <- dat.valid.yplus1$logw # pass predictions back to main dat.valid
              
              #predict on real scale with variance components included
              tot.var <- summary(minage.i)$sigma^2 #sigma is residual standard deviation
              dat.valid$w[hit.yplus1] <- exp(dat.valid$logw[hit.yplus1] + tot.var/2) #lognormal expectation
            }
            
          }
          
        } # end of growth model forecast year loop
  
        dat.valid$fcYear <- dat.valid$year - maxDataYear # number of forecast years from last traing year
        dat.valid$nFitYear <- length(unique(dat.train$year)) # number of training years
        dat.valid$best <- gFit$name # record the name of the model / formula
        
      } # end of forecast procedure for non-naive models
      
      res[[i]] <- dat.valid
      print(i) 
    
    } # i - moving window loop END -----
    
    res <- do.call("rbind", res)
    res$mod <- scen$name[s]
    res$stock <- stk
    
    RES[[s]] <- res
       
  } # s - scenario loop END -----
    

  # make sure column order is same, then collate
  Names <- names(RES[[1]])
  RES2 <- lapply(RES, function(x){x[,Names]})
  res <- do.call("rbind", RES2)
  
  # calc. error 
  res$rerr <- res$w2 / res$w2.orig
  res$err <- res$w2 - res$w2.orig
  unique(subset(res, mod == "laaGrowth")[,c("nFitYear", "best")])
  
  # define forecast classes (short, medium, or long)
  res$fcClass <- cut(res$fcYear, breaks = c(0,3,6,Inf), 
    labels = c("short (1-3)", "medium (4-6)", "long (7-15)"))
  
  # turn model scenario into factor
  res$mod <- factor(res$mod, levels = unique(scen$name))

  # 6. save results -------------------------------------------------------------------------

  save(res, file = paste0("model/", stk, "_res.Rdata"))
  

} # stk - stock loop END -----



