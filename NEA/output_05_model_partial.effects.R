
# load packages -----------------------------------------------------------

rm(list = ls())

# load packages
library(pdp)
library(reshape2)
library(ggplot2)
library(patchwork)
library(terra)
library(data.table)

# model specific packages
library(lmmlasso)
library(lmmen) # fork that handles multiple RE
library(cv.glmmLasso) # fork that handles multiple RE
library(glmmTMB)


# create output folder -----------------------------------------------------
dir.create("NEA/output/figures/marginalEffects", showWarnings = F)
dir.create("NEA/output/figures/marginalEffects2", showWarnings = F)
dir.create("NEA/output/figures/partialEffects", showWarnings = F)


# 1. load data objects -------------------------------------------------------
load("NEA/data/lutLWcoefs.Rdata")

# load final model table directly (as this is updated on github)
load("NEA/output/stocks and methods with forecasting potential.Rdata")
model.lut = potential
names(model.lut) = c("stock","model")


mods = list.files("NEA/model/fit_by_stk_n_method/",pattern = "*.Rdata")
final.mods = mods[gsub(".Rdata","",mods) %in% apply(model.lut,1,function(x) paste(x,collapse = "_"))]
if(length(final.mods) != nrow(model.lut)){
  stop("Not all models found! Check directory and model look-up table!")
}

# load all 
model.list = list()
for(i in 1:length(final.mods)){
  load(file.path("NEA/model/fit_by_stk_n_method",final.mods[i]))
  model.list[[i]] = get("gFit")
}
names(model.list) = gsub(".Rdata","",final.mods)



# required functions ------------------------------

calc.marginal.effects = function(model.obj,
  y, x, predict.func, log.space = FALSE) {
  
  xy = data.frame(y = y, x)
  # ------------------------ #
  # evaluate marginal effects
  # ------------------------ #
  
  vars = model.obj$vars
  # get age terms
  ageTerms <- c("age", "I(age^2)", "I(age^3)")[match(vars,c("age", "I(age^2)", "I(age^3)"))]
  ageTerms <- ageTerms[!is.na(ageTerms)]
  
  maxYearf <- max(as.numeric(xy$yearf))
  
  datsub <- subset(xy, yearf %in% levels(xy$yearf)[c((maxYearf-2):maxYearf)])
  datsub
  
  # minL <- mean(datsub$l, na.rm = TRUE)
  minA <- min(datsub$age)
  maxA <- max(datsub$age)+1
  #minW <- mean(subset(datsub, age == minA)$logw, na.rm = T)
  minW <- quantile(subset(datsub, age == minA)$logw,
                   probs = c(0.05, 0.25, 0.5, 0.75, 0.95), na.rm = T)
  if(any(names(xy) %in% "l")){
    #minl <- mean(subset(datsub,age == minA)$l,na.rm = T)
    minl <- quantile(subset(datsub,age == minA)$l,
                     probs = c(0.05, 0.25, 0.5, 0.75, 0.95),na.rm = T)
    minA.w = data.frame(minW = minW,minl = minl)
  } else{
    minA.w = data.frame(minW = minW)
  }
  # get covariates
  covs <- names(xy)[match(c("ssb.s", "temp.s", "sal.s"), names(xy))]
  
  if(sum(!is.na(covs))==0) return()
  
  covs <- covs[!is.na(covs)]
  L <- lapply(as.list(covs), FUN = function(x){unname(c(quantile(xy[,x], probs = c(0.05, 0.25, 0.5, 0.75, 0.95))))})
  names(L) <- covs
  covLevs <- expand.grid(L)
  # covLevs <- expand.grid(ssb.s = unname(c(quantile(dat$ssb.s, probs = c(0.05, 0.25, 0.5, 0.75, 0.95)))),
  #   temp.s = unname(c(quantile(dat$temp.s, probs = c(0.05, 0.25, 0.5, 0.75, 0.95)))))
  covLevsQs <- apply(covLevs, 2, FUN = quantile, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))
  #apply(covLevs, 2, FUN = function(x){x == median(x)})
  
  
  RES.all = list()
  for(n in 1:nrow(minA.w)){
    minW <- minA.w$minW[n]
    if("minl" %in% names(minA.w)){
      minl <- minA.w$minl[n]  
    } else {
      minl = 0
    }
    
  RES <- vector("list", nrow(covLevs))
  for(i in seq(RES)){
    df <- unique(xy[,unique(c("age",ageTerms)), drop = F])
    df <- df[order(df$age),,drop = F]
    
    df <- merge(df, covLevs[i,, drop = F], all.x = T)
    df$logw <- NA
    df$logw[1] <- minW
    df$l <- NA
    df$l[1] <- minl
    df$cohortf <- "new"
    df$yearf <- "new"
    df$logw2 <- "new"
    if(any(names(df) == "ssb.s")){
      df$covar1 = df$ssb.s # add covar1 in case of laaGrowth
    }
    df$trace = n
    for(j in 1:nrow(df)){
      if(j == 1){
        df$w[j] <- exp(minW)
      } else{
      # predict log-weights first and convert those to weights...
      prediction <- predict.func(model.obj,
        newdata = df[j-1,, drop = F])
      if(any(grepl("logw",names(prediction)))){
        df$logw[j] <- prediction$logw
      } else {
        df$l[j] <- prediction$l
      }
      df$w[j] <- prediction$w
      }
    }
    RES[[i]] <- df
  }
  RES.all[[n]] <- RES
  }
  
  RES = list()
  for(ii in 1:length(RES.all[[1]])){
    RES[[ii]] = do.call(rbind,lapply(RES.all,function(x) x[[ii]]))
  }
  
  ylim <- range(c(unlist(lapply(RES, FUN = function(x){range(x$w,na.rm = T)}))))
  ylim
  zlims <- apply(covLevs, 2, range)
  
  # plot
  par(mfcol = c(1, ncol(covLevs)), oma = c(1.5,1.5,2,0), mar = c(3,3,2,1))

  for(j in seq(ncol(covLevs))){
    covOth <- covLevs[,seq(ncol(covLevs))[-j], drop = F]
    incl <- which(apply(covOth, 1, FUN = function(x){all(x == covLevsQs["50%",-j])}))
    PAL <- function(x) colorspace::sequential_hcl(n = x,palette = "Sunset_Dark") #colorRamps::blue2green# 
    VALS <- covLevsQs[,j]
    COLS <- PAL(length(VALS))
    plot(w ~ age, data = RES[[incl[1]]], t = "n", ylim = ylim, ylab = "", xlab = "")
    for(i in incl){
      subRes = split(RES[[i]],f = RES[[i]]$trace)
      lapply(subRes,function(x)
      lines(w ~ age, data = x, t = "o", pch = 20, lwd = 1,cex = 1, 
            col = COLS[match(x[,names(covLevs)[j]], VALS)]))
    }
    legend("topleft", legend = rev(names(VALS)),
           col = rev(COLS), lty = 1, lwd = 1, bty = "n",
           cex = 1, seg.len = 0.5)
    mtext(names(covLevs)[j], side = 3, line = 0.25, font = 2, adj = 0)
    mtext(model.obj$stk.name, side = 3, line = 0, outer = T, adj = 0.5, font = 2, cex = 1.5)
    mtext("weight", side = 2, line = 0, outer = T, adj = 0.5, font = 1, cex = 1)
    mtext("age", side = 1, line = 0, outer = T, adj = 0.5, font = 1, cex = 1)
  }
  
}


walford.plot = function(model.obj,
  y, x, predict.func, log.space = FALSE) {
  
  xy = setDT(data.frame(y = y, x))
  # ------------------------ #
  # evaluate marginal effects
  # ------------------------ #
  
  vars = model.obj$vars
  vars <- unique(unlist(lapply(as.list(vars), strsplit, split = ":")))
  
  # get age terms
  ageTerms <- c("age", "I(age^2)", "I(age^3)")[match(vars,c("age", "I(age^2)", "I(age^3)"))]
  ageTerms <- ageTerms[!is.na(ageTerms)]
  

  # get covariates
  covs <- vars[match(c("ssb.s", "temp.s", "sal.s", "covar1"), vars)]
  covs <- covs[!is.na(covs)]
  covNames <- c("SSB", "Temperature", "Salinity", "SSB")[match(covs, c("ssb.s", "temp.s", "sal.s", "covar1"))]
  
  fixedCovs <- sum(!is.na(covs))>0
  if(!fixedCovs) return()
  
  Qs <- c(0.05, 0.95)  
  L <- lapply(as.list(covs), FUN = function(x){unname(c(quantile(xy[,get(x)], probs = Qs)))})
  names(L) <- covs
  covLevs <- expand.grid(L)
  covLevsQs <- as.data.frame(L)
  rownames(covLevsQs) <- paste0(Qs*100, "%")
  
  RES = vector("list", length(covs)) # loop for each cov
  for(i in seq(RES)){
    d <- setDT(xy)
    LUT <- d[, .(minwaa = exp(min(logw)), maxwaa = exp(max(logw))), by=.(age)]
    summary(d$logw)
    
    Li <- lapply(as.list(covs), FUN = function(x){
      if(x == covs[i]){probs = Qs}else{ probs = c(0.5)}
      unname(c(quantile(d[,get(x)], probs = probs)))
    })
    names(Li) <- covs
    Li$logw = seq(min(d$logw), max(d$logw), by = 0.01)
    Li$age = sort(unique(d$age))

    newdata <- expand.grid(Li)
    newdata$covtest <- covs[i]
    q <- match(round(newdata[,covs[i]],6), round(covLevsQs[,covs[i]],6))
    newdata$quantile <- rownames(covLevsQs)[q]
    newdata$cohortf <- newdata$yearf <- "new"
    newdata$logw2 <- -1000
    
    if(model.obj$name_mod == "laaGrowth"){
      LWab <- model.obj$lw.coefs
      newdata$l <- 10^(log10(exp(newdata$logw)*1000/LWab$a)/LWab$b)
      # names(xy)
      # names(newdata)
    }
    
    # if(length(grep(pattern = "lasso", meth)) > 0){
    #   df$logw[j] <- predict(gFit, newdata = df[j-1,, drop = F])
    # }
    
    prediction <- predict.func(model.obj, newdata = newdata)
    newdata$pred <- log(prediction$w)
      
    # newdata$pred <- predict(mod, newdata, re.form=NA, allow.new.levels=TRUE)
    
    setDT(newdata)
    newdata <- LUT[newdata, on = "age"]
    newdata[, real := ifelse(minwaa<=exp(logw) & exp(logw)<=maxwaa, 'realistic', 'extrapolated')]

    newdata[real=="realistic", logwMin := min(logw) , by = .(age) ]
    
    agelabs <- newdata[real=="realistic" & logw == logwMin, 
      .(w1 = logw, w2 = pred), by = .(age)]
    agelabs <- agelabs[, maxw2 := max(w2), by = .(age)]
    agelabs <- agelabs[w2 == maxw2]
    agelabs$x <- exp(agelabs$w1)
    agelabs$y <- exp(agelabs$w2)
    agelabs
    
    theme_set(theme_bw())
    axisLims <- exp(range(c(d$logw, d$logw2, newdata[real=="realistic"]$pred), na.rm = T))*c(1, 1.15)
    p <- ggplot(data = d) + aes(x = exp(logw), y = exp(logw2)) +
      geom_point(color = "grey80") + 
      geom_abline(lty = 2, color = "grey50") +
      geom_line(data = newdata[real=="realistic"], 
        aes(x = exp(logw), y = exp(pred), col = quantile,
        group = interaction(factor(age), factor(quantile))),
        linetype="solid", lwd=0.7) +
      geom_text(data = agelabs, aes(x, y, label = age), color = "grey50", nudge_y = 0.05) +
      scale_x_continuous(trans = "log10", limits = axisLims) +
      scale_y_continuous(trans = "log10", limits = axisLims) +
      # scale_color_brewer(palette = "Dark2") +
      labs(
        title = covNames[i], 
        # subtitle = "scaled temperature effect is set to zero",
        # caption = "Solid lines fall within biomass limits of the respective age classes",
        x = "weight in year t [kg] (log-scaled)", y = "weight in year t+1 [kg] (log-scaled)", 
        col = paste0("Covariate", "\nquantile"))
    
    if(i != length(RES)){
      p <- p + guides(color = "none") 
    }else{
      p <- p + 
        theme(legend.position = c(0.98, 0.02), legend.justification = c(1, 0))
    }
    # p
      
    RES[[i]] <- p
    
  }
  
  if(length(RES)==1){p <- RES[[1]]}
  if(length(RES)==2){p <- (RES[[1]] + RES[[2]])}
  if(length(RES)==3){p <- (RES[[1]] + RES[[2]] + RES[[3]])}
  p <- p + 
    plot_layout(axes = "collect") +
    # plot_layout(guides = "collect") +
    plot_annotation(title = paste("Marginal effects to growth of", model.obj$stk.name)) #&
    # theme(axis.title.x = element_text(size = 7), axis.title.y = element_text(size = 7),
    #   axis.text.x = element_text(size = 7), axis.text.y = element_text(size = 7))
  
  return(res = list(p = p, n = length(RES)))
  

}



calc.partial.effects2 = function(model.obj,y,x,predict.func,log.space = FALSE) {
  stopifnot(require(pdp))
  stopifnot(require(reshape2))
  stopifnot(require(ggplot2))
  stopifnot(require(patchwork))
  
  xy = data.frame(y = y,x)
  # ------------------------ #
  # evaluate partial effects
  # ------------------------ #
  
  vars = model.obj$vars
  # check if there are interactions in there
  interactions = vars[grepl(":",vars)]
  interaction_plot = list()
  #if(length(interactions) == 0){
  pdp_out.all = list()
  median_pdp.all = list()
  n = 1; ii = 1
  for(v in vars){
    if(length(interactions) >0 && any(v %in% interactions)){
      # split variable
      v.split = unlist(strsplit(v,split = ":"))
      
      # add 20% extrapolation bounds
      pred.grid = expand.grid(seq(ifelse(min(xy[,v.split[1]])<0,1.2*min(xy[,v.split[1]]),0.8*min(xy[,v.split[1]])),
                                  ifelse(max(xy[,v.split[1]])<0,0.8*max(xy[,v.split[1]]),1.2*max(xy[,v.split[1]])),length.out = 20),
                              seq(ifelse(min(xy[,v.split[2]])<0,1.2*min(xy[,v.split[2]]),0.8*min(xy[,v.split[2]])),
                                  ifelse(max(xy[,v.split[2]])<0,0.8*max(xy[,v.split[2]]),1.2*max(xy[,v.split[2]])),length.out = 20))
      names(pred.grid) = v.split
      
      
      pdp_out = partial(object = model.obj,pred.var = v.split,train = xy,pred.grid = pred.grid,
                        pred.fun = function(object,newdata) predict.func(object,newdata),plot = F)
      pdp_melt = as.data.frame(terra::rast(as.data.frame(pdp_out)),xy = T)
      pal = colorspace::sequential_hcl(n = 1000,palette = "Sunset_dark")
      interaction_plot[[ii]] = ggplot(pdp_melt,aes(x = x, y = y)) +
        geom_tile(aes(fill = yhat)) +
        coord_cartesian(expand = FALSE) +
        xlab(v.split[1]) + 
        ylab(v.split[2]) + 
        scale_fill_gradientn(aes(values = yhat),colours = pal) + 
        geom_contour(aes(z = yhat),binwidth = (max(pdp_melt$yhat) - min(pdp_melt$yhat))/25,color = "gray50") + 
        ggtitle(paste(v,collapse = ":")) + 
        theme(legend.key.size = 6) + 
        guides(fill=guide_legend(title=paste("yhat","\n","~",v))) + 
        theme_bw()
      ii = ii +1
    } else {
      if(grepl("I\\(",v)){
        # skip this one
        next()
        #   
        #   # get only the variable 
        #   v_simple = gsub("I\\(|\\^2\\)|\\^3\\)","",v)
        #   pred.grid = data.frame(seq(ifelse(min(xy[,v_simple])<0,1.2*min(xy[,v_simple]),0.8*min(xy[,v_simple])),
        #                              ifelse(max(xy[,v_simple])<0,0.8*max(xy[,v_simple]),1.2*max(xy[,v_simple])),length.out = 100))  
      } else {
        # add 20% extrapolation bounds
        pred.grid = data.frame(seq(ifelse(min(xy[,v])<0,1.2*min(xy[,v]),0.8*min(xy[,v])),
                                   ifelse(max(xy[,v])<0,0.8*max(xy[,v]),1.2*max(xy[,v])),length.out = 100))
      }
      names(pred.grid) = v
      pdp_out = partial(object = model.obj,pred.var = v,train = xy,pred.grid = pred.grid,
                        pred.fun = function(object,newdata) predict.func(object,newdata),plot = F)
    }
    if(!any(v %in% interactions)){
      by.id = pdp_out[,v]
      median_pdp = aggregate(pdp_out,by = list(by.id),FUN = median)
      median_pdp = median_pdp[,1:3]
      names(median_pdp)[which(names(median_pdp) == "Group.1")] = "yhat.id"
      
      pdp_out.all[[n]] = melt(pdp_out,id.vars = c("yhat.id","yhat"))
      median_pdp.all[[n]] = melt(median_pdp,id.vars = c("yhat.id","yhat"))
    }
    n = n+1
  }
  pdp_out = do.call(rbind,pdp_out.all)
  median_pdp = do.call(rbind,median_pdp.all)
  vars = vars[!grepl("I\\(",vars)]
  vars_pdp = melt(xy[,c("y","age",vars[!grepl(":",vars)]),drop = F],id.vars = c("y","age"))
  #vars_pdp = melt(xy,id.vars = c("y","age"))
  if("age" %in% vars){
    # add age as variable
    vars_age = melt(xy[,c("y","age"),drop = F],id.vars = c("y"))
    vars_age$age = vars_age$value
    vars_age = vars_age[,colnames(vars_pdp)]
    vars_pdp = rbind(vars_age,vars_pdp)
  }
  if(log.space == FALSE){
    vars_pdp$y = exp(vars_pdp$y)
  }
  
  col_age = colorspace::sequential_hcl(n = length(unique(vars_pdp$age)),palette = "Purple-Yellow",rev = T)
  
  single.effects = vars[!grepl(":",vars)]
  pdp.plot = list()
  for(i in 1:length(single.effects)){
    pdp.plot[[i]] = ggplot(pdp_out[pdp_out$variable == single.effects[i],],aes(x = value,y = yhat)) + 
      geom_line(aes(group = yhat.id),alpha = 0.3) +
      geom_line(data = median_pdp[median_pdp$variable == single.effects[i],],aes(x = value,y = yhat),col = "red",linewidth = 2) +
      geom_point(data = vars_pdp[vars_pdp$variable == single.effects[i],],aes(x = value,y = y,fill = as.factor(age)),
                 pch = 21,
                 #fill = "burlywood1",
                 colour = "gray50") +
      scale_fill_manual(values = col_age) + 
      ggtitle(single.effects[i]) + 
      xlab(single.effects[i]) +
      theme(legend.key.size = 6) + 
      guides(fill=guide_legend(title="Age")) + 
      theme_bw()
  }
  if(length(interaction_plot)>0){
    all.plots = c(pdp.plot,interaction_plot)
  } else {
    all.plots = pdp.plot
  }
  
  if(length(all.plots) == 1){
    nr = 1;nc = 1
  } else if(length(all.plots)%%2 <= 1 & length(all.plots)>3){
    nr = 2;nc = ceiling(length(all.plots)/2)
  } else if(length(all.plots)%%3 <= 0 & length(all.plots)>3){
    nr = 3;nc = length(all.plots)/3
  } else{
    nr = 1;nc = length(all.plots)
  }
  
  pdp.plots = patchwork::wrap_plots(all.plots, guides = "collect",
                                    nrow = nr, ncol = nc) &
    theme(legend.position = 'bottom',
          legend.direction = 'horizontal')
  
  # return
  return.list = list(pdp = pdp_out,
                     median_pdp = median_pdp,
                     plot = pdp.plots)
  return(return.list)
}


# evaluate effects -----------------------------------------------

pdps.eval_list = list()
plot.marginal <- F
plot.marginal2 <- T
plot.partial <- T

walford.plot.list <- list()

for(k in 1:length(model.list)){
  print(k)
  mod <- model.list[[k]]
  name_stk_mod = names(model.list)[k]
  name_mod = sub("^.*?\\_", "",name_stk_mod)
  name_stk = sub("_.*","",name_stk_mod)


  
  # get fitted data & chosen variables
  if(name_mod == "laaGrowth"){
    fitted.data = mod$data
    if(mod$name %in% c("svbgfM", "gvbgfM", "gompM")){
      vars <- c("l", "covar1")
    }else{
      vars <- c("l")
    }
    # vars = c("l","covar1")
    y.var = "w2"
    mod$lw.coefs = lutLWcoefs[lutLWcoefs$stockcode == name_stk,c("a","b")]
    
    predict.func.marg = function(model.obj,newdata){
      newdata$dt = 1
      log.pred = predict(model.obj, newdata) 
      # backtransform from length to weight
      LWab <- model.obj$lw.coefs
      w2 <- (LWab$a*log.pred^LWab$b)/1000
      # return  both the new length & converted weight
      return(data.frame(l = log.pred, w = w2))
    }
    predict.func.part = function(model.obj,newdata){
      newdata$dt = 1
      log.pred = predict(model.obj, newdata) 
      # backtransform from length to weight
      LWab <- model.obj$lw.coefs
      w2 <- (LWab$a*log.pred^LWab$b)/1000
      # return  both the new length & converted weight
      return(w2)
    }
    
  } else if(grepl("lasso",name_mod)){
    fitted.data = mod$data
    vars = names(mod$coefficients)[mod$coefficients != 0]
    vars = vars[!grepl("Intercept",vars)]
    # remove polynomial terms
    vars = gsub("I\\(|\\^[0-9]+\\)","",vars)
    vars = unique(vars)
    y.var = "logw2"
    
    predict.func.marg = function(model.obj,newdata){
      log.pred = predict(model.obj, newdata) 
      tot.var <-  model.obj$phi + sum(unlist(model.obj$StdDev)^2)
      w2 <- exp(log.pred + tot.var/2) #lognormal mean
      # return both logw & w
      return(data.frame(logw = log.pred,w = w2))  
    }
    predict.func.part = function(model.obj,newdata){
      log.pred = predict(model.obj, newdata) 
      tot.var <-  model.obj$phi + sum(unlist(model.obj$StdDev)^2)
      w2 <- exp(log.pred + tot.var/2) #lognormal mean
      # return both logw & w
      return(w2)  
    }
  } else if(grepl("glmmTMB",name_mod)){
    fitted.data = mod$frame
    mod.s = summary(mod)
    vars = rownames(mod.s$coefficients$cond[mod.s$coefficients$cond[,1] != 0,])
    # remove intercept
    vars = vars[!grepl("Intercept",vars)]
    # control for the case that the original fitted data.frame contains an interaction term...
    # was the case for "pil.27.8abd"...
    names_dat = names(fitted.data)
    poly_names = names_dat[grepl("I\\(",names_dat)]
    if(length(poly_names)> 0){
      # add the "original" unpolynomialised version of the variable to the data
      for(p in 1:length(poly_names)){
        if(stringr::str_extract(poly_names[p],"[0-9]+") == "2"){
          unpoly_var = gsub("I\\(|\\^[0-9]+\\)","",poly_names[p])
          fitted.data[unpoly_var] = sqrt(as.numeric(fitted.data[,poly_names[p]]))
        }  else if(stringr::str_extract(poly_names[p],"[0-9]+") == "3"){
          unpoly_var = gsub("I\\(|\\^[0-9]+\\)","",poly_names[p])
          fitted.data[unpoly_var] = as.numeric(fitted.data[,poly_names[p]])^(1/3)
        }
      }
    }
    # remove polynomial terms
    vars = gsub("I\\(|\\^[0-9]+\\)","",vars)
    # replace splines with their "normal" names
    vars = gsub("s\\(|\\)[0-9]+","",vars)
    vars = unique(vars)
    y.var = "logw2"
    predict.func.marg = function(model.obj, newdata){
      log.pred = predict(model.obj, newdata, re.form=NA, allow.new.levels=TRUE) 
      tot.var <- as.numeric(VarCorr(model.obj)$cond$cohortf+VarCorr(model.obj)$cond$yearf+attr(VarCorr(model.obj)$cond,"sc")^2)
      w2 <- exp(log.pred + tot.var/2) #lognormal mean
      # return both logw and w
      return(data.frame(logw = log.pred,w = w2))  
    }
    predict.func.part = function(model.obj, newdata){
      log.pred = predict(model.obj, newdata, re.form=NA, allow.new.levels=TRUE) 
      tot.var <- as.numeric(VarCorr(model.obj)$cond$cohortf+VarCorr(model.obj)$cond$yearf+attr(VarCorr(model.obj)$cond,"sc")^2)
      w2 <- exp(log.pred + tot.var/2) #lognormal mean
      # return both logw and w
      return(w2)  
    }
  } 
  
  # put variable names in the model obj
  mod$vars = vars
  mod$stk.name =  name_stk 
  mod$name_mod =  name_mod

  ## 1. produce marginal effects plot ----------------- 
  if(plot.marginal){
  fname <- file.path("NEA/output/figures/marginalEffects", paste0("marginalEffects_", name_stk, ".png"))
  png(fname, width = 7, height = 4, units = "in", res = 600)
  op <- par(no.readonly = T)
  par(cex = 1, ps = 9)
  marginalEffects.eval = calc.marginal.effects(model.obj = mod, y = fitted.data[,y.var],
    x = fitted.data, log.space = ifelse(name_mod == "laaGrowth",T,F),
    predict.func = predict.func.marg)
  par(op)
  dev.off()
  }
  
  # 2. produce Walford marginal effects ----------------- 
  if(plot.marginal2){
  fname <- file.path("NEA/output/figures/marginalEffects2", paste0("marginalEffects_", name_stk, ".png"))
  res <- walford.plot(model.obj = mod, y = as.data.frame(fitted.data)[,y.var],
    x = fitted.data, log.space = ifelse(name_mod == "laaGrowth",T,F),
    predict.func = predict.func.marg)
  if(!is.null(res)){
    walford.plot.list[[k]] <- res
    p <- p & theme(
      axis.title.x = element_text(size = 7), 
      axis.title.y = element_text(size = 7),
      axis.text.x = element_text(size = 7), 
      axis.text.y = element_text(size = 7))
    
    png(fname, width = 15/3*res$n, height = 15/3, units = "in", res = 600)
      print(res$p)
    dev.off()
  }
  }
  res <- NULL
  
  # 3. produce partial effects plots -----------------
  if(plot.partial){
  pdps.eval = calc.partial.effects2(model.obj = mod, y = fitted.data[,y.var],
    x = fitted.data, log.space = ifelse(name_mod == "laaGrowth",T,F),
    predict.func = predict.func.part)#stats::predict)
  pdps.eval$plot <- pdps.eval$plot + plot_annotation(title = name_stk,subtitle = name_mod)
  # store in list
  pdps.eval_list[[k]] <- pdps.eval
  }
}
names(pdps.eval_list) = names(model.list)

fname <- file.path("NEA/output", paste0("pdps.eval_list.Rdata"))

save(pdps.eval_list, file = fname)


# make pdps.eval_list plots -----------------------------------------------

for(k in seq(pdps.eval_list)){
  name_stk_mod = names(pdps.eval_list)[k]
  name_mod = sub("^.*?\\_", "",name_stk_mod)
  name_stk = sub("_.*","",name_stk_mod)
  
  fname <- file.path("NEA/output/figures/partialEffects/", paste0("partialEffects_", name_stk, ".png"))
  p <- pdps.eval_list[[k]]$plot
  png(fname, width = 7, height = 7, units = "in", res = 400)
    print(p)
  dev.off()
}



