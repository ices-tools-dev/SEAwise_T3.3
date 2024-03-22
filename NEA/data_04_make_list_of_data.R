# 0. clean-up ----------------------------------------------------------------

rm(list = ls())


# 1. load files ----------------------------------------------------------

load(file = "NEA/data/FLStocks_list_stks.Rdata")
load(file = "NEA/data/lutYearAge.Rdata")
load(file = "NEA/data/lutLWcoefs.Rdata")

envTemp <- readRDS("./NEA/data/env/ORAS5_AnnualTEMPByDemPelStock3.RDS")
envSal <- readRDS("./NEA/data/env/ORAS5_AnnualSALByDemPelStock3.RDS")

# # temporary fix !!!
# envTemp$sol.27.7e <- envTemp$sol.27.7fg
# envSal$sol.27.7e <- envSal$sol.27.7fg

# 2. extract data from FLStocks ---------------------------------------------

DF <- vector("list", length = length(stks))
names(DF) <- names(stks)

pb <- txtProgressBar(min = 0, max = length(DF), style = 3)
for(i in seq(stks)){
  stockcode.i <- names(stks)[i]
  stock.i <- stks[[stockcode.i]]$stock
  ran.i <- range(stock.i)
  minage.i <- ran.i["min"]
  maxage.i <- ran.i["max"]
  
  # 2.1. extract cohort variables -----
  flq <- stock.n(stock.i)
  flc_n <- FLCohort(flq)
  flq <- stock.wt(stock.i)
  flc_w <- FLCohort(flq)
  flq <- catch.n(stock.i)
  flc_cn <- FLCohort(flq)
  
  tmp <- as.data.frame(flc_n)
  names(tmp)[which(names(tmp)=="data")] <- "n"
  dfn <- tmp
  
  tmp <- as.data.frame(flc_w)
  names(tmp)[which(names(tmp)=="data")] <- "w"
  dfw <- tmp
  
  tmp <- as.data.frame(flc_cn)
  names(tmp)[which(names(tmp)=="data")] <- "catch.n"
  dfcn <- tmp
  
  df <- merge(x = dfw, y = dfn, all.x = TRUE)
  df <- merge(x = df, y = dfcn, all.x = TRUE)
  
  df$b <- df$w*df$n
  df$year <- df$cohort + df$age
  
  tmp <- as.data.frame(ssb(stock.i))
  names(tmp)[which(names(tmp)=="data")] <- "ssb"
  SSB <- tmp
  SSB <- SSB[,-which(names(SSB)=="age")]
  
  # merge
  df <- merge(x = df, y = SSB, all.x = TRUE)
  
  # 2.2. environmental covariates -----
  # temperature
  colNames <- c("year", stockcode.i)
  tmp <- as.data.frame(envTemp)[, colNames]
  names(tmp) <- c('year', 'temp')
  df <- merge(x = df, y = tmp, all.x = TRUE)
  # salinity
  colNames <- c("year", stockcode.i)
  tmp <- as.data.frame(envSal)[, colNames]
  names(tmp) <- c('year', 'sal')
  df <- merge(x = df, y = tmp, all.x = TRUE)
  df <- df[order(df$cohort, df$age),]
  
  # 2.3. calculate lengths and expand variables ----
  a <- lutLWcoefs$a[match(stockcode.i, lutLWcoefs$stockcode)]
  b <- lutLWcoefs$b[match(stockcode.i, lutLWcoefs$stockcode)]
  
  df <- as.data.table(df)
  df[, age2 := ifelse(age == maxage.i, age, age + 1), by = cohort]
  df[, dt :=  c(diff(age),NaN), by = cohort]
  df[, year2 := year + 1]
  df[, wdev := w / mean(w, na.rm = T), by = age]
  df[, dwdt := c(diff(w), NaN), by = cohort]
  df[, w2 := w+dwdt, by = cohort]
  df[, l := 10^(log10(w*1000/..a)/..b), by = cohort]
  df[, l2 := 10^(log10(w2*1000/..a)/..b), by = cohort]
  df[, dldt := l2-l, by = cohort]
  df <- as.data.frame(df)
  

  # 2.3. subset data -----
  # remove excluded ages and years
  lutYearAge.i <- subset(lutYearAge, stockcode == stockcode.i)
  dfsubs <- vector("list", nrow(lutYearAge.i))
  for(j in seq(dfsubs)){
    lutYearAge.ij <- lutYearAge.i[j,]
    year.ij <-  eval(parse(text = lutYearAge.ij$year))
    age.ij <-  eval(parse(text = lutYearAge.ij$age))
    dfsubs[[j]] <- subset(df, (year %in% year.ij) & 
      (age %in% age.ij & age2 %in% age.ij))
  }
  dfsubs <- do.call("rbind", dfsubs)
  df <- unique(dfsubs)
  
  # remove growth increment into plus group  
  df <- subset(df, age2 < maxage.i)
  
  # final ordering
  df <- df[order(df$year, df$age),]
  
  # final selection of variables
  # df <- df[,c("cohort", "year", "age", "dt", "n", "l", "l2", "dldt", "lmid", "sst", "ssb", "nao", "w", "w2")]
  df <- df[,c("cohort", "year", "year2", "age", "age2", "dt", "n", "l", "l2", "w", "w2", "ssb", "temp", "sal")]
  
  # final removal of non-growth-increment samples (i.e. missing w2)
  df <- subset(df, !is.na(w2))
  
  DF[[i]] <- df
  setTxtProgressBar(pb, i)
}
close(pb)




# 3. save -----------------------------------------------------------------

save(DF, file = "NEA/data/data_list_DF.Rdata")

