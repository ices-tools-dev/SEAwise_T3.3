## ----setup, echo=FALSE---------------------------------------------------------------------------
library(knitr)
library(plyr)
library(ggplot2); theme_set(theme_bw())


## organize stock by area -----------
load("NEA/data/data_list_DF.Rdata")
modeledSTKs <- names(DF)
  
allSTKs <- list(
  "NS" = c("cod.27.47d20","ple.27.420", "had.27.46a20", "whg.27.47d","pok.27.3a46","sol.27.4", "her.27.3a47d"),
  "IS" = c("cod.27.7a", "had.27.7a", "ple.27.7a", "sol.27.7a", "whg.27.7a"),
  "CS" = c("cod.27.7e-k", "had.27.7b-k", "meg.27.7b-k8abd", "sol.27.7fg","sol.27.7e", "whg.27.7b-ce-k"),
#  "CS" = c("ank.27.78abd", "cod.27.7e-k", "had.27.7b-k", "hke.27.3a46-8abd", "meg.27.7b-k8abd", "mon.27.78abd","sol.27.7fg","sol.27.7e", "whg.27.7b-ce-k"),
  "Baltic"= c("cod.27.22-24","her.27.25-2932", "her.27.28", "her.27.3031","sol.27.20-24",  "spr.27.22-32"),
  "Biscay"= c("ane.27.8", "hke.27.8c9a", "bss.27.8ab", "pil.27.8abd","pil.27.8c9a" )
)

load("NEA/model/allLMMs.Rdata")
stks <- ldply(.data =RES, .fun= function(x){x$stk})
scens <- ldply(.data =RES, .fun= function(x){x$scen})

for(area in names(allSTKs)){
  ## ----summarize, echo=FALSE, warning=FALSE--------------------------------------------------------
  RES2 <- RES[stks[,2] %in% allSTKs[[area]]]
                
  (keep <- names(RES2[[1]]$mod))
  mods2 <- ldply(.data =RES2, .fun= function(x){
    if(x$scen != "glmmTMB_GAMM_AICc"){
    tmp <- x$mod[keep]
    names(tmp) <- c("(Intercept)", "logw", "age", "age^2", 
                 "sal",
                 "SSB", "temp", "temp^2", "age^2:logw",
                 "age:logw",  
                 "sal:logw", "SSB:logw", "temp:logw" )
    round(tmp, 2)}
  })
  stks2 <- ldply(.data =RES2, .fun= function(x){if(x$scen !="glmmTMB_GAMM_AICc") x$stk})
  scens2 <- ldply(.data =RES2, .fun= function(x){if(x$scen !="glmmTMB_GAMM_AICc") x$scen})
  
  mods3 <- cbind(stock=stks2[,2], scen=scens2[,2], mods2)
  
  my_table<-subset(mods3, scen=="glmmTMB_AICc")[,c("stock","sal", "SSB", "temp", "temp^2", "sal:logw", "SSB:logw", "temp:logw")]
  
  kable(replace(my_table, my_table==0, ""), row.names = FALSE)
  
  #kable(my_table, digits = 2 )
  
  
  ## ----all-----------------------------------------------------------------------------------------
  tmp <- mods3[,c("stock","scen","sal", "SSB", "temp", "temp^2", "sal:logw", "SSB:logw", "temp:logw")]
  mods4 <- reshape2::melt(tmp, id.vars=1:2)
  mods4 <- transform(mods4, scen=factor(scen), stock=factor(stock))
  mods5 <- mods4
  mods5$value[mods5$value==0]=NA
  
  # laa <- data.frame(stock = allSTKs[[area]], variable = "SSB", value= NA, scen="laaGrowth")
  #   
  # # add in laaGrowth models
  # for (i in 1:nrow(laa)){
  #   try(rm("gFit"))
  #   tmp <- try(load(paste0("model/fit_by_stk_n_method/", laa[i,"stock"], "_laaGrowth.Rdata")))
  #   if(!inherits(tmp, "try-error")){
  #     if(!is.na(coef(gFit)["C1"])){
  #       laa[i,"value"] <- coef(gFit)["C1"]
  #     }
  #   }
  # }
  # mods6 <- rbind(mods5, laa)

  mods6 <- mods5
  
  ggplot(mods6, aes(y=scen, x=value))+
    geom_point(position = position_dodge(width=.75))+theme_bw()+
    facet_grid(stock~variable)+
    xlab(NULL)+ylab(NULL)+
    geom_vline(xintercept=0)
#    scale_x_continuous(breaks=round(seq(-0.1, 0.05, length=4), 2), labels = round(seq(-0.1, 0.05, length=4), 2))
  
  ggsave(file.path("NEA", "output","figures", paste0("full_dataset_best_models_", area,".png")), height=7, width=9)
}


load("NEA/model/allrecLMs.Rdata")
stks <- unlist(lapply(strsplit(names(REC), split = "_"), function(x){x[1]}))
scen <- unlist(lapply(names(REC), function(x){substring(x, first=gregexpr("_", x)[[1]][1]+1)}))

for(area in names(allSTKs)){
  ## ----summarize, echo=FALSE, warning=FALSE--------------------------------------------------------
  REC2 <- REC[stks %in% allSTKs[[area]]]
  stks2 <- unlist(lapply(strsplit(names(REC2), split = "_"), function(x){x[1]}))
  scen2 <- unlist(lapply(names(REC2), function(x){substring(x, first=gregexpr("_", x)[[1]][1]+1)}))
  
  (keep <- names(REC2[[1]]))
  mods2 <- ldply(.data =REC2, .fun= function(x){
    tmp <- x[keep]
    names(tmp) <- c("(Intercept)", "sal", "SSB", "temp", "temp^2" )
    round(tmp, 2)
  })

  mods3 <- cbind(stock=stks2, mods2)
  

  
  ## ----all-----------------------------------------------------------------------------------------
  tmp <- mods3[,c("stock","sal", "SSB", "temp", "temp^2")]
  mods4 <- reshape2::melt(tmp, id.vars=1)
  mods4 <- transform(mods4,  stock=factor(stock))
  mods5 <- mods4
  mods5$value[mods5$value==0]=NA
  
  ggplot(mods5, aes(y=stock, x=value))+
    geom_point(position = position_dodge(width=.75))+theme_bw()+
    facet_grid(variable~.)+
    xlab(NULL)+ylab(NULL)+
    geom_vline(xintercept=0)
#    scale_x_continuous(breaks=round(seq(-0.1, 0.1, length=4), 2), labels = round(seq(-0.1, 0.05, length=4), 2))
  
  ggsave(file.path("NEA", "output","figures", paste0("full_dataset_best_models_recruit_weight", area,".png")), height=7, width=7)
}
