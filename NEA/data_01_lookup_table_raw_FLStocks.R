
# 0. clean-up ----------------------------------------------------------------

rm(list = ls())


# 1. generate list of files --------------------------------------------------

res <- list()
for(i in seq(list.files("NEA/data/FLStocks_in/"))){
  path.i <- list.files("NEA/data/FLStocks_in/", full.names = T)[i]
  res[[length(res)+1]] <- data.frame(
    casestudy = list.files("NEA/data/FLStocks_in/")[i],
    path = list.files(path.i, full.names = T), 
    fname = list.files(path.i))
}
df <- do.call("rbind", res)
head(df)
(tmp <- lapply(strsplit(x = df$fname, ".", fixed = T), function(x){x[-length(x)]}))
df$stockcode <- unlist(lapply(tmp, paste, collapse = "."))
df

df$objname <- NA
df$keep <- TRUE

# 2. fixes to stock codes --------------------------------------------------------------
dput(df$stockcode)
# c("ank78_FLStock", "wgbie2021_BSS_SBassBB", "wgbie2021_hke.27.3a46-8abd_FLStock_27102021", 
# "wgbie2021_MON78ab", "wgbie2021_SOL_xsa_ass_stock", "wgbie2022_hke8c9a_stock_2021", 
# "wgbie2022_nhke_FLStock_csim", "wghansa2022_aneq.stk", "wghansa2022_pil8abd", 
# "wghansa2022_pil8c9a", "wgwide2021_neaMAC_stock", "wgwide2021_WHB_FLstock", 
# "wgwide2021_WHOM_SSresults", "wgwide2022_MAC_stock", "wgwide2022_WHOM_SSresults", 
# "ank.27.78abd", "cod.27.7e-k", "had.27.7b-k", "hke.27.3a46-8abd", 
# "meg.27.7b-k8abd", "mon.27.78abd", "nep.fu.16", "nep.fu.17", 
# "nep.fu.19", "nep.fu.2021", "nep.fu.22", "nep.out.7", "sol.27.7e", 
# "sol.27.7fg", "whg.27.7b-ce-k", "cod.27.7a", "had.27.7a", "nep.7a", 
# "ple.27.7a", "sol.27.7a", "whg.27.7a", "cod.27.47d20", "had.27.46a20", 
# "her.27.3a47d", "ple.27.420", "pok.27.3a46", "sol.27.4", "sol.27.7d", 
# "spr.27.3a4", "whg.27.47d")

# ank78_FLStock ----
stockcode <- "ank78_FLStock"
hit <- which(df$stockcode == stockcode)
(tmp <- load(df$path[hit], verbose = TRUE))
get(tmp[1])@name; get(tmp[1])@desc
df$objname[hit] <- "stk"
df$stockcode[hit] <- "ank.27.78abd"
# df$keep[hit] <- TRUE
rm(list = tmp)

# wgbie2021_BSS_SBassBB ----
stockcode <- "wgbie2021_BSS_SBassBB"
hit <- which(df$stockcode == stockcode)
(tmp <- load(df$path[hit], verbose = TRUE))
get(tmp[1])@name; get(tmp[1])@desc
# df$objname[hit] <- NA
df$stockcode[hit] <- "bss.27.8ab"
df$keep[hit] <- FALSE
rm(list = tmp)

# wgbie2021_hke.27.3a46-8abd_FLStock_27102021 ----
stockcode <- "wgbie2021_hke.27.3a46-8abd_FLStock_27102021"
hit <- which(df$stockcode == stockcode)
(tmp <- load(df$path[hit], verbose = TRUE))
get(tmp[1])@name; get(tmp[1])@desc
# df$objname[hit] <- NA
df$stockcode[hit] <- "hke.27.3a46-8abd"
# df$keep[hit] <- TRUE
rm(list = tmp)

# wgbie2021_MON78ab ----
stockcode <- "wgbie2021_MON78ab"
hit <- which(df$stockcode == stockcode)
(tmp <- load(df$path[hit], verbose = TRUE))
get(tmp[1])@name; get(tmp[1])@desc
# df$objname[hit] <- NA
df$stockcode[hit] <- "mon.27.78ab"
# df$keep[hit] <- TRUE
rm(list = tmp)

# wgbie2021_SOL_xsa_ass_stock ----
stockcode <- "wgbie2021_SOL_xsa_ass_stock"
hit <- which(df$stockcode == stockcode)
(tmp <- load(df$path[hit], verbose = TRUE))
get(tmp[1])@name; get(tmp[1])@desc
# df$objname[hit] <- NA
df$stockcode[hit] <- "sol.27.8ab"
# df$keep[hit] <- TRUE
rm(list = tmp)

# wgbie2022_hke8c9a_stock_2021 ----
stockcode <- "wgbie2022_hke8c9a_stock_2021"
hit <- which(df$stockcode == stockcode)
(tmp <- load(df$path[hit], verbose = TRUE))
get(tmp[1])@name; get(tmp[1])@desc
# df$objname[hit] <- NA
df$stockcode[hit] <- "hke.27.8c9a"
df$keep[hit] <- FALSE
rm(list = tmp)

# wgbie2022_nhke_FLStock_csim ----
stockcode <- "wgbie2022_nhke_FLStock_csim"
hit <- which(df$stockcode == stockcode)
(tmp <- load(df$path[hit], verbose = TRUE))
get(tmp[1])@name; get(tmp[1])@desc
df$objname[hit] <- "hke.stk"
df$stockcode[hit] <- "hke.27.8c9a" # ***?
df$keep[hit] <- FALSE
rm(list = tmp)

# wghansa2022_aneq.stk ----
stockcode <- "wghansa2022_aneq.stk"
hit <- which(df$stockcode == stockcode)
(tmp <- load(df$path[hit], verbose = TRUE))
get(tmp[1])@name; get(tmp[1])@desc
# df$objname[hit] <- NA
df$stockcode[hit] <- "ane.27.8"
# df$keep[hit] <-  FALSE
rm(list = tmp)

# wghansa2022_pil8abd ----
stockcode <- "wghansa2022_pil8abd"
hit <- which(df$stockcode == stockcode)
(tmp <- load(df$path[hit], verbose = TRUE))
get("pil.stock")@name; get("pil.stock")@desc
df$objname[hit] <- "pil.stock"
df$stockcode[hit] <- "pil.27.8abd"
# df$keep[hit] <-  FALSE
rm(list = tmp)

# wghansa2022_pil8c9a ----
stockcode <- "wghansa2022_pil8c9a"
hit <- which(df$stockcode == stockcode)
(tmp <- load(df$path[hit], verbose = TRUE))
get(tmp[1])@name; get(tmp[1])@desc
# df$objname[hit] <- NA
df$stockcode[hit] <- "pil.27.8c9a"
# df$keep[hit] <-  FALSE
rm(list = tmp)

# wgwide2021_neaMAC_stock ----
stockcode <- "wgwide2021_neaMAC_stock"
hit <- which(df$stockcode == stockcode)
(tmp <- load(df$path[hit], verbose = TRUE))
get(tmp[1])@name; get(tmp[1])@desc
# df$objname[hit] <- NA
df$stockcode[hit] <- "NA"
df$keep[hit] <-  FALSE
rm(list = tmp)

# wgwide2021_WHB_FLstock ----
stockcode <- "wgwide2021_WHB_FLstock"
hit <- which(df$stockcode == stockcode)
(tmp <- load(df$path[hit], verbose = TRUE))
get(tmp[1])@name; get(tmp[1])@desc
# df$objname[hit] <- NA
df$stockcode[hit] <- "NA"
df$keep[hit] <-  FALSE
rm(list = tmp)

# wgwide2021_WHOM_SSresults ----
stockcode <- "wgwide2021_WHOM_SSresults"
hit <- which(df$stockcode == stockcode)
(tmp <- load(df$path[hit], verbose = TRUE))
get(tmp[1])@name; get(tmp[1])@desc
# df$objname[hit] <- NA
df$stockcode[hit] <- "NA"
df$keep[hit] <-  FALSE
rm(list = tmp)

# wgwide2022_MAC_stock ----
stockcode <- "wgwide2022_MAC_stock"
hit <- which(df$stockcode == stockcode)
(tmp <- load(df$path[hit], verbose = TRUE))
get(tmp[1])@name; get(tmp[1])@desc
# df$objname[hit] <- NA
df$stockcode[hit] <- "NA"
df$keep[hit] <-  FALSE
rm(list = tmp)

# wgwide2022_WHOM_SSresults ----
stockcode <- "wgwide2022_WHOM_SSresults"
hit <- which(df$stockcode == stockcode)
(tmp <- load(df$path[hit], verbose = TRUE))
get(tmp[1])@name; get(tmp[1])@desc
# df$objname[hit] <- NA
df$stockcode[hit] <- "NA"
df$keep[hit] <-  FALSE
rm(list = tmp)


# 3. filter keepers and add year and age restrictions ------------------------
sort(subset(df, keep == TRUE)$stockcode)

df$keep[which(df$stockcode %in% 
    c("ank.27.78abd", "bss.27.8ab", "hke.27.3a46-8abd", "hke.27.8c9a", 
      "mon.27.78ab", "mon.27.78abd", 
      "nep.7a", "nep.fu.16", "nep.fu.17", "nep.fu.19", "nep.fu.2021", "nep.fu.22", "nep.out.7",
      "sol.27.7d", "spr.27.3a4")
)] <-  FALSE


df <- subset(df, keep == TRUE)
head(df)
sort(df$stockcode)
length(df$stockcode)

# missing: cod.27.22-24, sol.27.20-24


# 4. save -----------------------------------------------------------------

lutFLStocks <- df

save(lutFLStocks, file = "NEA/data/lutFLStocks.Rdata")





