library(glmmTMB)
library(glmmLasso)

load("data/lutLWcoefs.Rdata")
lutLWcoefs


bestMods <- read.table(text = "               stk            method

1     cod.27.47d20 glmmTMB_GAMM_AICc
2        cod.27.7a glmmTMB_GAMM_AICc
3     had.27.46a20      glmmTMB_AICc
4        had.27.7a         laaGrowth
5      had.27.7b-k glmmTMB_GAMM_AICc
6  meg.27.7b-k8abd    lasso_CV20perc
7      pil.27.8abd glmmTMB_GAMM_AICc
8      pil.27.8c9a laaGrowth
9      pok.27.3a46         laaGrowth
10    sol.27.20-24         lasso_BIC
11       sol.27.7a      glmmTMB_AICc
12      sol.27.7e laaGrowth
13 whg.27.47d laaGrowth
14       whg.27.7a glmmTMB_GAMM_AICc
15  whg.27.7b-ce-k glmmTMB_GAMM_AICc")



STKs <- c("cod.27.7a", "had.27.46a20", "had.27.7b-k", "pil.27.8c9a", "sol.27.7a", "whg.27.7b-ce-k")

pdf(paste0("marinalEffects.pdf"), width = 7, height = 4, onefile = T)

for(k in seq(nrow(bestMods))){
# stk <- "cod.27.7a" "sol.27.7a" # "whg.27.7b-ce-k", "had.27.46a20"

stk <- bestMods$stk[k]
meth <- bestMods$method[k]

# tmp <- load(paste0("model/fit_by_stk_n_method/", stk, "_glmmTMB_AICc.Rdata"), verbose = TRUE)

tmp <- load(paste0("model/fit_by_stk_n_method/", stk, "_", meth, ".Rdata"), verbose = TRUE)

# terms(gFit)
class(gFit)

Terms <- dat <- NULL
if(length(grep(pattern = "glmmTMB", meth)) > 0){
  Terms <- names(fixef(gFit)$cond)
  dat <- gFit$frame
} 
if(length(grep(pattern = "lasso", meth)) > 0){
  Terms <- names(gFit$coefficients)
  dat <- gFit$data
}
if(length(grep(pattern = "laaGrowth", meth)) > 0){
  dat <- gFit$data
  if(gFit$name %in% c("svbgfM", "gvbgfM", "gompM")){
    Terms <- "covar1"
  }else{
    Terms <- NULL
  }
}

covs <- c("ssb.s", "temp.s", "sal.s")[unlist(lapply(as.list(c("ssb.s", "temp.s", "sal.s")), FUN = function(x){
  ifelse(length(grep(pattern = x, Terms)) > 0, T, F)}))]

if(length(covs)==0) next()

# covs <- names(dat)[match(c("ssb.s", "temp.s", "sal.s"), names(dat))]



# ageTerms <- c("age", "I(age^2)", "I(age^3)")[match(c("age", "I(age^2)", "I(age^3)"), Terms)]
# ageTerms <- ageTerms[!is.na(ageTerms)]

# dat <- gFit$frame
# head(dat)

maxYearf <- max(as.numeric(dat$yearf))

# datsub <- subset(dat, yearf %in% levels(dat$yearf)[c((maxYearf-2):maxYearf)])
# datsub

# minL <- mean(datsub$l, na.rm = TRUE)
minA <- min(dat$age)
maxA <- max(dat$age)+1
minW <- mean(subset(dat, age == minA)$logw, na.rm = T)

# head(dat)
# covs <- names(dat)[match(c("ssb.s", "temp.s", "sal.s"), names(dat))]


# covs <- covs[!is.na(covs)]
L <- lapply(as.list(covs), FUN = function(x){unname(c(quantile(dat[,x], probs = c(0.05, 0.25, 0.5, 0.75, 0.95))))})
names(L) <- covs
covLevs <- expand.grid(L)
# covLevs <- expand.grid(ssb.s = unname(c(quantile(dat$ssb.s, probs = c(0.05, 0.25, 0.5, 0.75, 0.95)))),
#   temp.s = unname(c(quantile(dat$temp.s, probs = c(0.05, 0.25, 0.5, 0.75, 0.95)))))
covLevsQs <- apply(covLevs, 2, FUN = quantile, probs = c(0.05, 0.25, 0.5, 0.75, 0.95))

# apply(covLevs, 2, FUN = function(x){x == median(x)})


RES <- vector("list", nrow(covLevs))
for(i in seq(RES)){
  # df <- unique(dat[,ageTerms, drop = F])
  df <- unique(dat[,"age", drop = F])
  df <- df[order(df$age),,drop = F]
  
  df <- merge(df, covLevs[i,, drop = F], all.x = T)
  
  df$logw[1] <- minW
  df$logw2 <- -1000
  df$cohortf <- "new"
  df$yearf <- "new"
  
  for(j in 2:nrow(df)){
    if(length(grep(pattern = "glmmTMB", meth)) > 0){
      df$logw[j] <- predict(gFit, newdata = df[j-1,, drop = F], allow.new.levels=TRUE)
    }
    if(length(grep(pattern = "lasso", meth)) > 0){
      df$logw[j] <- predict(gFit, newdata = df[j-1,, drop = F])
    }
  }
  df$w <- exp(df$logw)
  RES[[i]] <- df
}

ylim <- range(c(unlist(lapply(RES, FUN = function(x){range(x$w)}))))
ylim
zlims <- apply(covLevs, 2, range)

# png(paste0("marinalEffects_", stk, ".png"), width = 7, height = 3, units = "in", res = 600)
op <- par(no.readonly = T)
par(mfcol = c(1, ncol(covLevs)), oma = c(1.5,1.5,2,0), mar = c(3,3,2,1), mgp = c(2,0.5,0))
par(cex = 1, ps = 9)
for(j in seq(ncol(covLevs))){
  covOth <- covLevs[,seq(ncol(covLevs))[-j], drop = F]
  incl <- which(apply(covOth, 1, FUN = function(x){all(x == covLevsQs["50%",-j])}))
  PAL <- colorRamps::blue2green# pals::brewer.blues
  VALS <- covLevsQs[,j]
  COLS <- PAL(length(VALS))
  plot(w ~ age, data = RES[[incl[1]]], t = "n", ylim = ylim, ylab = "", xlab = "")
  for(i in incl){
    lines(w ~ age, data = RES[[i]], t = "o", pch = 20, lwd = 1, 
      col = COLS[match(RES[[i]][,names(covLevs)[j]], VALS)])
  }
  legend("topleft", legend = rev(names(VALS)), col = rev(COLS), lty = 1, lwd = 1, bty = "n")
  mtext(names(covLevs)[j], side = 3, line = 0.25, font = 2, adj = 0)
  mtext(stk, side = 3, line = 0, outer = T, adj = 0.5, font = 2, cex = 1.5)
  mtext("weight", side = 2, line = 0, outer = T, adj = 0.5, font = 1, cex = 1)
  mtext("age", side = 1, line = 0, outer = T, adj = 0.5, font = 1, cex = 1)
}
par(op)
# dev.off()

}

dev.off() # close pdf
