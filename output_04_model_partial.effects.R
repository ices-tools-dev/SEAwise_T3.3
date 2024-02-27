# ---------------------------------------------------------------- #
# Script to get the partial effects from those final growth models
# author: Bernhard Kuehn
# last change: 2024-02-09
# ---------------------------------------------------------------- #

rm(list = ls())

# load packages
library(pdp)
library(reshape2)
library(ggplot2)
library(patchwork)
library(terra)

# model specific packages
library(lmmlasso)
library(lmmen) # fork that handles multiple RE
library(cv.glmmLasso) # fork that handles multiple RE
library(glmmTMB)

# 1. load data objects -------------------------------------------------------
load("data/lutLWcoefs.Rdata")

# # load final model table directly (as this is updated on github)
# load("./output/stocks and methods with forecasting potential.Rdata")
# model.lut = potential
# names(model.lut) = c("stock","model")

# get models
# updated on the 2024-02-09: from image "stocks and methods with forecast potential.png" (2024-02-08,3:49pm)
# not including GAMM models...
# model.lut = as.data.frame(rbind(c("cod.27.47d20","glmmTMB_AICc"), #"laaGrowth"),
#                   c("cod.27.7a","glmmTMB_AICc"),#"glmmTMB_GAMM_AICc"),
#                   c("had.27.46a20","glmmTMB_AICc"),
#                   c("had.27.7a","laaGrowth"),
#                   c("had.27.7b-k","glmmTMB_AICc"), #"glmmTMB_GAMM_AICc"),
#                   c("meg.27.7b-k8abd","lasso_CV20perc"),
#                   c("pil.27.8abd","glmmTMB_AICc"), #"laaGrowth"),
#                   c("pil.27.8c9a","laaGrowth"),#"glmmTMB_GAMM_AICc"),
#                   c("pok.27.3a46","laaGrowth"),
#                   c("sol.27.20-24","lasso_BIC"),
#                   c("sol.27.7a","glmmTMB_AICc"),
#                   c("sol.27.7e","laaGrowth"), # new stk
#                   c("whg.27.47d","laaGrowth"),
#                   c("whg.27.7a","glmmTMB_AICc"), #"laaGrowth"),
#                   c("whg.27.7b-ce-k","glmmTMB_AICc"))) #"glmmTMB_GAMM_AICc")))

load("output/stocks and methods with forecasting potential.Rdata")
model.lut = potential.braod[,c(2,1)]
names(model.lut) = c("stock","model")

mods = list.files("./model/fit_by_stk_n_method/",pattern = "*.Rdata")
final.mods = mods[gsub(".Rdata","",mods) %in% apply(model.lut,1,function(x) paste(x,collapse = "_"))]
if(length(final.mods) != nrow(model.lut)){
  stop("Not all models found! Check directory and model look-up table!")
}

# load all 
model.list = list()
for(i in 1:length(final.mods)){
  load(file.path("./model/fit_by_stk_n_method",final.mods[i]))
  model.list[[i]] = get("gFit")
}
names(model.list) = gsub(".Rdata","",final.mods)

# ------------------------------------------------------------------------------------- #
# define function to evaluate partial effects

# add the possibility to interpret interaction terms in the partial dependence plot
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
        theme_bw() + 
        theme(legend.key.size = unit(0.2, 'cm'),
              legend.title = element_text(size = 7), 
              legend.text = element_text(size = 7)) +
        guides(fill= guide_colorbar(title=paste("yhat","\n","~",v)))
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
  if("age" %in% names(xy)){
    vars_pdp = melt(xy[,c("y","age",vars[!grepl(":",vars)]),drop = F],id.vars = c("y","age"))
    if("age" %in% vars){
      # add age as variable
      vars_age = melt(xy[,c("y","age"),drop = F],id.vars = c("y"))
      vars_age$age = vars_age$value
      vars_age = vars_age[,colnames(vars_pdp)]
      vars_pdp = rbind(vars_age,vars_pdp)
    }
    col_age = colorspace::sequential_hcl(n = length(unique(vars_pdp$age)),palette = "Purple-Yellow",rev = T)
  } else {
    col_age = "gray90"
    pdp_out$age = NA
    vars_pdp = melt(xy[,c("y",vars[!grepl(":",vars)]),drop = F],id.vars = c("y"))
    vars_pdp$age = NA
  }
  if(log.space == FALSE){
    vars_pdp$y = exp(vars_pdp$y)
  }
  
  single.effects = vars[!grepl(":",vars)]
  pdp.plot = list()
  for(i in 1:length(single.effects)){
    p.single = ggplot(pdp_out[pdp_out$variable == single.effects[i],],aes(x = value,y = yhat)) + 
      geom_line(aes(group = yhat.id),alpha = 0.3) +
      geom_line(data = median_pdp[median_pdp$variable == single.effects[i],],aes(x = value,y = yhat),col = "red",linewidth = 2) +
      scale_fill_manual(values = col_age) + 
      ggtitle(single.effects[i]) + 
      xlab(single.effects[i]) +
      theme_bw() + 
      theme(legend.key.size = unit(0.5, 'cm'),
            legend.spacing.x = unit(0.1, 'cm'),
            legend.spacing.y = unit(0.1, 'cm'),
            legend.title = element_text(size = 8), 
            legend.text = element_text(size = 8)) + 
      guides(fill=guide_legend(title="Age",
                               nrow = ifelse(length(unique(vars_pdp$age))<= 6,1,
                                             ifelse(length(unique(vars_pdp$age)) < 12,
                                                    2,3)),
                               override.aes = list(size = 2)),
             shape = guide_legend(override.aes = list(size = 2))) 
    if(all(is.na(vars_pdp$age))){
      pdp.plot[[i]] = p.single + geom_point(data = vars_pdp[vars_pdp$variable == single.effects[i],],aes(x = value,y = y),
                                            pch = 21,color = "gray50",fill = "burlywood")
    } else {
      pdp.plot[[i]] = p.single  + geom_point(data = vars_pdp[vars_pdp$variable == single.effects[i],],aes(x = value,y = y,fill = as.factor(age)),
                                             pch = 21,colour = "gray50") 
    }
    
    
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
    legend.direction = 'vertical')
  
  # return
  return.list = list(pdp = pdp_out,
                     median_pdp = median_pdp,
                     plot = pdp.plots)
  return(return.list)
}

# ------------------------ # 
# evaluate partial effects
# ------------------------ #

pdps.eval_list = list()
for(k in 1:length(model.list)){
  print(k)
  mod <- model.list[[k]]
  name_stk_mod = names(model.list)[k]
  name_mod = sub("^.*?\\_", "",name_stk_mod)
  name_stk = sub("_.*","",name_stk_mod)
  # get fitted data & chosen variables
  if(name_mod == "laaGrowth"){
    fitted.data = mod$data
    vars = c("l","covar1")
    y.var = "w2"
    mod$lw.coefs = lutLWcoefs[lutLWcoefs$stockcode == name_stk,c("a","b")]
    
    predict.func = function(model.obj,newdata){
      log.pred = predict(model.obj, newdata) 
      # backtransform from length to weight
      LWab <- model.obj$lw.coefs
      w2 <- (LWab$a*log.pred^LWab$b)/1000
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
   
   predict.func = function(model.obj,newdata){
     log.pred = predict(model.obj, newdata) 
     tot.var <-  model.obj$phi + sum(unlist(model.obj$StdDev)^2)
     w2 <- exp(log.pred + tot.var/2) #lognormal mean
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
    predict.func = function(model.obj,newdata){
      log.pred = predict(model.obj, newdata, allow.new.levels=TRUE) 
      tot.var <- as.numeric(VarCorr(model.obj)$cond$cohortf+VarCorr(model.obj)$cond$yearf+attr(VarCorr(model.obj)$cond,"sc")^2)
      w2 <- exp(log.pred + tot.var/2) #lognormal mean
      return(w2)  
    }
  } 

  # put variable names in the model obj
  mod$vars = vars
  
# run evaluation function to calc. pdps
  pdps.eval = calc.partial.effects2(model.obj = mod,y = fitted.data[,y.var],
                                    x = fitted.data,log.space = ifelse(name_mod == "laaGrowth",T,F),
                                    predict.func = predict.func)#stats::predict)
  pdps.eval$plot <- pdps.eval$plot + plot_annotation(title = name_stk,subtitle = name_mod)
  
  # store in list
  pdps.eval_list[[k]] <- pdps.eval
}
names(pdps.eval_list) = names(model.list)

# put plots in pdf 
pdf("./output/figures/Final_models_pdps.pdf",width = 10,height = 7)
lapply(pdps.eval_list,function(x) x$plot)
dev.off()

# save in extra directory
dir.create("./output/figures/pdps",showWarnings = F)
for(i in 1:length(pdps.eval_list)){
  png(file.path("./output/figures/pdps",paste0(names(pdps.eval_list)[i],"_pdp",".png")),
      width = 250*10,height = 250*7,res = 300)
  print(pdps.eval_list[[i]]$plot)
  dev.off()
}

# save object
save(pdps.eval_list,file = "./model/PDPs_finalModels.Rdata")


