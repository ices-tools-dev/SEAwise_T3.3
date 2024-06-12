load("NEA/output/stocks and methods with forecasting potential.Rdata")
library(glmmTMB)
library(plyr)
library(reshape)
library(ggplot2); theme_set(theme_bw())

mods = subset(potential.broad, mod %in% c("glmmTMB_AICc", "glmmTMB_GAMM_AICc"))

for(i in 1:nrow(mods)){
	load(paste0("NEA/model/fit_by_stk_n_method/", mods[i,"stk"], "_", mods[i,"mod"], ".Rdata"))
  tmpC = data.frame(year=as.numeric(as.character(rownames(ranef(gFit)$cond$cohortf))),
                   cohortRE_scaled = ranef(gFit)$cond$cohortf[,1]/abs(fixef(gFit)$cond[1]))
  tmpY = data.frame(year=as.numeric(as.character(rownames(ranef(gFit)$cond$yearf))),
                   yearRE_scaled = ranef(gFit)$cond$yearf[,1]/abs(fixef(gFit)$cond[1]))
  tmp = join(tmpC, tmpY)
  tmp2 = melt(tmp, id.vars = 1)
	ggplot(tmp2, aes(year, value))+
	  facet_grid(variable~., scales = "free", switch = "both")+
	  geom_line()+ylab(NULL)
	ggsave(paste0("NEA/output/figures/RE/", mods[i,"stk"], "_", mods[i,"mod"], "_RE.png"), height=5, width=6)
	}