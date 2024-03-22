
# 0. clean-up ----------------------------------------------------------------

rm(list = ls())


# 1. plots of fc performance by year and window (short, med., & long) --------

load("NEA/data/lutFLStocks.Rdata")

STKs <- sort(lutFLStocks$stockcode)
STKs <- STKs[-which(STKs=="ane.27.8")]

# 1.1. stk - loop by stock ------------------------------------------------

for(stk in STKs){
  # load results object
  load(file = paste0("NEA/model/", stk, "_res.Rdata"))

  # calculate error quantiles - error ~ model + forecast_years
  tmp <- aggregate(rerr ~ mod + fcYear, data = res, 
    FUN = quantile, probs = c(0.05, 0.5, 0.95), na.rm = TRUE)
  tmp2 <- as.data.frame(tmp[,3])
  names(tmp2) <- paste0("q", c("05","50","95"))
  resQ <- cbind(tmp[,1:2], tmp2)
  
  # plot
  p <- ggplot(data = resQ) +
    aes(x = fcYear, y = q50, colour = mod) + 
    geom_ribbon(aes(x = fcYear, ymin = q05, ymax = q95,
      fill = mod), alpha = 0.2, lty = 0) +
    geom_line() +
    # geom_line(aes(x = fcYear, y = q05, col = mod), lty = 2) +
    # geom_line(aes(x = fcYear, y = q95, col = mod), lty = 2) +
    geom_hline(yintercept = 1, lty = 1) +
    geom_hline(yintercept = 1.1, lty = 2) +
    geom_hline(yintercept = 0.9, lty = 2) +
    theme(text = element_text(size = 10), 
      strip.text = element_text(size = 10)) +
    ggtitle(label =  paste0("Relative error by forecast year", " (", stk, ")")) +
    ylab("rerr") 
  # p
   
  png(paste0("NEA/output/figures/", stk, "_fc_rerr~mod+fcYear.png"), width = 6, height = 5, units = "in", res = 400)
    print(p)
  dev.off()
  
  
  
  # calculate error quantiles - error ~ model + forecast_class
  tmp <- aggregate(rerr ~ mod + fcClass + year, data = res, 
    FUN = quantile, probs = c(0.05, 0.5, 0.95), na.rm = TRUE)
  tmp2 <- as.data.frame(tmp[,4])
  names(tmp2) <- paste0("q", c("05","50","95"))
  resQ3 <- cbind(tmp[,1:3], tmp2)
  
  p <- ggplot(data = resQ3) +
    aes(x = year, y = q50, colour = mod) + 
    facet_wrap(~fcClass) +
    geom_ribbon(aes(x = year, ymin = q05, ymax = q95,
      fill = mod), alpha = 0.2, lty = 0) +
    geom_line() +
    # geom_line(aes(x = year, y = q05, col = mod), lty = 2) +
    # geom_line(aes(x = year, y = q95, col = mod), lty = 2) +
    geom_hline(yintercept = 1, lty = 1) +
    geom_hline(yintercept = 1.1, lty = 2) +
    geom_hline(yintercept = 0.9, lty = 2) +
    theme(text = element_text(size = 10), 
      strip.text = element_text(size = 10)) + 
    ggtitle(label =  paste0("Relative error by year and forecast class", " (", stk, ")")) +
    ylab("rerr") 
  # p
  
  png(paste0("NEA/output/figures/", stk, "_fc_rerr~fcClass+mod+year.png"), width = 7, height = 4, units = "in", res = 400)
    print(p)
  dev.off()
}



# 2. summary tables for laaGrowth --------------------------------------------



RES <- vector("list", length(STKs))
names(RES) <- STKs
for(stk in STKs){
  load(file = paste0("NEA/model/", stk, "_res.Rdata"))
  RES[[stk]] <- res
}

res <- do.call("rbind", RES)
res$err <- (res$w2 - res$w2.orig) / res$w2.orig
agg <- aggregate(err ~ mod + fcClass + stock, data = res, FUN = median)


df <- dcast(data = agg, stock + fcClass ~ mod, fun.aggregate = sum, value.var = "err")
df[,3:4] <- t(apply(df[,3:4], 1, FUN = function(x){
  hit <- which(abs(x) == min(abs(x)))
  paste0(sprintf(an(x), fmt = "%.3f"), ifelse(seq(x) %in% hit, "*", " "))
}))
df
write.csv(df, file = "NEA/output/laaGrowth_naive_vs_model_err.csv")

B <- vector("list", length(STKs))
names(B) <- STKs
for(stk in STKs){
  B[[stk]] <- unique(subset(res, stock == stk & mod == "laaGrowth")[,c("stock", "nFitYear", "best")])
}
B <- do.call("rbind", B)
rownames(B) <- seq(nrow(B))
B
write.csv(df, file = "NEA/output/laaGrowth_best_by_nFitYear.csv")
