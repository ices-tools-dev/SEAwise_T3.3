## ----setup, echo=FALSE---------------------------------------------------------------------------
library(knitr)
library(plyr)
library(ggplot2)


## organize stock by area -----------

allSTKs <- list(
  "NS" = c("cod.27.47d20","ple.27.420", "had.27.46a20", "whg.27.47d","pok.27.3a46","sol.27.4", "her.27.3a47d"),
  "IS" = c("cod.27.7a", "had.27.7a", "ple.27.7a", "sol.27.7a", "whg.27.7a"),
  "CS" = c("ank.27.78abd", "cod.27.7e-k", "had.27.7b-k", "hke.27.3a46-8abd", "meg.27.7b-k8abd", "mon.27.78abd","sol.27.7fg","sol.27.7a", "whg.27.7b-ce-k"),
  "Baltic"= c("cod.27.24-32", "her.27.28", "sol.27.20-24", "cod.27.22-24", "her.27.25-2932", "spr.27.22-32", "ple.27.21-23"),
  "Biscay"= c("ane.27.8", "hke.27.3a46-8abd", "hke.27.8c9a", "mon.27.78abd","bss.27.8ab", "pil.27.8abd","pil.27.8c9a" )
)

#completed forecast evaluations
models <- list.files("model/")
results <- models[grep("_res.Rdata", models)]
completedSTKs <- unlist(lapply(strsplit(results, split = "_"), function(x){x[1]}))
  
for(area in names(allSTKs)){
  STKs <- subset(allSTKs[[area]], allSTKs[[area]] %in% completedSTKs)

  ## summarize by year -----
  
  allstks <- list()
  
  for(stk in STKs){
    load(paste0("model/", stk, "_res.Rdata"))
    res <- transform(res, fitYear = year-fcYear)
    sums <-  ddply(res, ~ fitYear +mod, summarize, 
                   mse = mean((w2.orig-w2)^2), 
                   msre = mean(((w2.orig-w2)/w2.orig)^2),
                   rmse = mean((w2.orig-w2)^2)^.5,
                   mae = mean(abs(w2.orig-w2)),
                   stk=stk
    )
    sums <- ddply(sums, ~ fitYear, mutate, 
                  rel_rmse=rmse/subset(rmse, mod=="naive"),
                  rel_mae=mae/subset(mae, mod=="naive"))

    allstks[[stk]] <- sums
  }
  predw1lm <- do.call(rbind, allstks)
  
  predw1lm$w1type <- "predicted"
  
  
  both_by_year <- predw1lm #rbind(simplew1, predw1lm)
  both_by_year <- subset(both_by_year, fitYear<=2019)

  ## plot by year -----
  
  ggplot(subset(both_by_year, mod !="naive"), aes(fitYear, rel_mae))+
    geom_line(aes(colour=mod))+
    #  geom_line(aes(colour=mod, lty=w1type))+
    ylab(expression(MAE[modeled]/MAE[naive]))+
    geom_hline(yintercept = 1)+
    #  guides(lty=guide_legend(title="recruit \nweights"))+
    facet_grid(stk~., scale="free")+
    xlab("Forecast Origin")
  ggsave(paste0("output/figures/method_evaluation/method_mean_sq_rerr by fitYear", area, ".png"), height=6.5, width=5)
  
  
  ggplot(subset(both_by_year, mod !="naive"), aes(fitYear, rel_rmse))+
    geom_line(aes(colour=mod))+
    #  geom_line(aes(colour=mod, lty=w1type))+
    ylab(expression(RMSE[modeled]/RMSE[naive]))+
    geom_hline(yintercept = 1)+
    #  guides(lty=guide_legend(title="recruit \nweights"))+
    facet_grid(stk~., scale="free")+
    xlab("Forecast Origin")
  ggsave(paste0("output/figures/method_evaluation/method_rtmean_sq_rerr by fitYear", area, ".png"), height=6.5, width=5)
  
  ## summarize by fcClass -----

  allres <- list()
  
  for(stk in STKs){
    load(paste0("model/", stk, "_res.Rdata"))
    res <- transform(res, fitYear = year-fcYear)
    res <- subset(res, fitYear<=2019)
    sums <-  ddply(res, ~ fcClass +mod, summarize,
                   mse = mean((w2.orig-w2)^2),
                   msre = mean(((w2.orig-w2)/w2.orig)^2),
                   rmse = mean((w2.orig-w2)^2)^.5,
                   mae = mean(abs(w2.orig-w2)),
                   stk=stk
    )
    sums <- ddply(sums, ~ fcClass, mutate,
                  rel_rmse=rmse/subset(rmse, mod=="naive"),
                  rel_mae=mae/subset(mae, mod=="naive"))
    
    allres[[stk]] <- sums
  }
  predw1lm <- do.call(rbind, allres)
  
  predw1lm$w1type <- "predicted"
  
  both_by_fcClass <- predw1lm #rbind(simplew1, predw1lm)
  
  ## plot by fcClass -----
  levels(both_by_fcClass$fcClass) <- c("short horizon \n (1-3 yrs)", "medium horizon \n(4-6 yrs)", "long horizon \n (7-15 yrs)" )
  ggplot(subset(both_by_fcClass, mod !="naive"), aes(mod, rel_rmse))+
    facet_grid(stk~fcClass, scale="free")+
    geom_point()+xlab(NULL)+
    #  geom_point(aes(colour=w1type))+xlab(NULL)+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
    ylab(expression(RMSE[modeled]/RMSE[naive]))+
    geom_hline(yintercept = 1)
  #  guides(colour=guide_legend(title="recruit \nweights"))
  ggsave(paste0("output/figures/method_evaluation/method_relative_rt_mean_sq_err by fcClass", area, ".png"), height=7, width=5)
  
  ggplot(subset(both_by_fcClass, mod !="naive"), aes(mod, rel_mae))+
    facet_grid(stk~fcClass, scale="free")+
    #  geom_point(aes(colour=w1type))+
    geom_point()+ 
    xlab(NULL)+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
    ylab(expression(MAE[modeled]/MAE[naive]))+
    geom_hline(yintercept = 1)
  #  guides(colour=guide_legend(title="recruit \nweights"))
  ggsave(paste0("output/figures/method_evaluation/method_relative_mean_abs_err by fcClass", area, ".png"), height=5, width=6)
  
  ## summarize by method only -------------
  
  for(stk in STKs){
    load(paste0("model/", stk, "_res.Rdata"))
    res <- ddply(res, ~mod+fcYear, mutate,
                 nfc = length(err)) #number of forecast horizons of that length
    
    sums <-  ddply(res, ~ mod, summarize, 
                   mse = weighted.mean((w2.orig-w2)^2, w = 1/nfc), 
                   msre = weighted.mean(((w2.orig-w2)/w2.orig)^2, w = 1/nfc),
                   rmse = weighted.mean(((w2.orig-w2)^2)^.5, w = 1/nfc),
                   mae = weighted.mean(abs(w2.orig-w2), w = 1/nfc),
                   stk=stk
    )
    sums <- ddply(sums, ~ stk, mutate, 
                  rel_rmse=rmse/subset(rmse, mod=="naive"),
                  rel_mae=mae/subset(mae, mod=="naive"))
    
    allstks[[stk]] <- sums
  }
  
  predw1lm <- do.call(rbind, allstks)
  tmp <- reshape::melt(predw1lm[,c("mod" , "stk", "rel_mae")],
                       id.vars = c("mod", "stk"))
  
  ggplot(subset(tmp, mod !="naive"), aes(mod, value))+
    facet_grid(stk~., scale="free")+
    #  geom_point(aes(colour=w1type))+
    geom_point()+ 
    xlab(NULL)+
    theme(axis.text.x = element_text(angle = 45, vjust = 1, hjust=1))+
    ylab(expression(MAE[modeled]/MAE[naive]))+
    geom_hline(yintercept = 1)
  #  guides(colour=guide_legend(title="recruit \nweights"))
  ggsave(paste0("output/figures/method_evaluation/method_overall_relative_mean_abs_err_", area, ".png"), height=7, width=5)
  
} #end area loop


## all areas by method -------------
## summarize by method only -------------

 for(stk in completedSTKs){
  load(paste0("model/", stk, "_res.Rdata"))
  res <- ddply(res, ~mod+fcYear, mutate,
               nfc = length(err)) #number of forecast horizons of that length
  
  sums <-  ddply(res, ~ mod, summarize, 
                 mse = weighted.mean((w2.orig-w2)^2, w = 1/nfc), 
                 msre = weighted.mean(((w2.orig-w2)/w2.orig)^2, w = 1/nfc),
                 rmse = weighted.mean(((w2.orig-w2)^2)^.5, w = 1/nfc),
                 mae = weighted.mean(abs(w2.orig-w2), w = 1/nfc),
                 stk=stk
  )
  sums <- ddply(sums, ~ stk, mutate, 
                rel_rmse=rmse/subset(rmse, mod=="naive"),
                rel_mae=mae/subset(mae, mod=="naive"))
  
  allstks[[stk]] <- sums
}

predw1lm <- do.call(rbind, allstks)
tmp <- reshape::melt(predw1lm[,c("mod" , "stk", "rel_mae")],
                     id.vars = c("mod", "stk"))

(potential <- ddply(subset(tmp, value<1, drop = TRUE), ~stk, summarize, 
                   method = mod[which.min(value)]))

save(potential, file="output/stocks and methods with forecasting potential.Rdata")
