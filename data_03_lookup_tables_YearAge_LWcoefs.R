# 0. clean-up ----------------------------------------------------------------

rm(list = ls())

# 1. load files ----------------------------------------------------------

load(file = "data/FLStocks_list_stks.Rdata")


# 2. year and age restrictions  ---------------------------------------------------------------------

lutYearAge <- matrix(
  data = c(
    "ane.27.8", "1987:2022", "1:3",
    "cod.27.47d20", "1963:2021", "1:6", 
    "cod.27.7a", "c(1982:2002, 2006:2021)", "1:6",  
    "cod.27.7e-k", "2004:2021", "1:7", 
    "cod.27.22-24", "1985:2020", "1:7",
    "had.27.46a20", "1972:2021", "0:8", 
    "had.27.7a", "1993:2021", "0:5",
    "had.27.7b-k", "1997:2021", "0:8", # *** 2 criteria
    "had.27.7b-k", "1993:1996", "1:8", # *** 2 criteria
    "her.27.3a47d", "1947:2021", "0:8",
    "her.27.25-2932", "1974:2020", "1:8",
    "her.27.28", "1977:2020", "1:8",
    "her.27.3031", "1970:2020", "0:16",
    "meg.27.7b-k8abd", "1984:2021", "1:10",
    "pil.27.8abd", "2000:2022", "1:6",
    "pil.27.8c9a", "1999:2022", "1:6",
    "ple.27.420", "1957:2021", "1:10",
    "ple.27.7a", "1981:2021", "1:8",
    "pok.27.3a46", "1967:2021", "3:10",
    "sol.27.4", "1957:2021", "1:10",
    "sol.27.7a", "1970:2021", "2:8",
    "sol.27.7e", "1969:2021", "2:12",
    "sol.27.7fg", "1971:2021", "2:10",
    "sol.27.8ab", "1984:2021", "2:8",
    "sol.27.20-24", "1984:2020", "2:9",
    "spr.27.3a4", "1969:2021", "0:3",
    "spr.27.22-32", "1974:2020", "1:8",
    "whg.27.47d", "1978:2021", "0:6",
    "whg.27.7a", "1980:2021", "1:5",
    "whg.27.7b-ce-k","1999:2021", "0:7"), 
  ncol = 3,
  byrow = TRUE)

lutYearAge <- as.data.frame(lutYearAge)
names(lutYearAge) <- c("stockcode", "year", "age")

# add age range of stock object
tmp <- unlist(lapply(stks, function(x){
  ran <- x$range
  paste(ran["min"], ":", ran["max"], sep = "")
}))
tmp <- as.data.frame(tmp)
colnames(tmp) <- "range"
tmp$stockcode <- rownames(tmp)
rownames(tmp) <- seq(nrow(tmp))


lutYearAge <- merge(lutYearAge, tmp)
lutYearAge


# 3. look-up LW coefs from Fishbase ------------------------------------------

# The current approach is to use coefficients based on "mixed" or "unsexed" coefficients
# A geometric mean is applied to "a", and a mean to "b" (as per Fishbase), 
#   both weighted by sample size in the study

lutSpp <- rbind(
  data.frame(stockcode = "ane.27.8", genusSpecies = "Engraulis encrasicolus"),
  data.frame(stockcode = "cod.27.47d20", genusSpecies = "Gadus morhua"),
  data.frame(stockcode = "cod.27.7a", genusSpecies = "Gadus morhua"),
  data.frame(stockcode = "cod.27.7e-k", genusSpecies = "Gadus morhua"),
  data.frame(stockcode = "cod.27.22-24", genusSpecies = "Gadus morhua"),
  data.frame(stockcode = "cod.27.24-32", genusSpecies = "Gadus morhua"),
  data.frame(stockcode = "had.27.46a20", genusSpecies = "Melanogrammus aeglefinus"),
  data.frame(stockcode = "had.27.7a", genusSpecies = "Melanogrammus aeglefinus"),
  data.frame(stockcode = "had.27.7b-k", genusSpecies = "Melanogrammus aeglefinus"),
  data.frame(stockcode = "her.27.3a47d", genusSpecies = "Clupea harengus"),
  data.frame(stockcode = "her.27.28", genusSpecies = "Clupea harengus"),
  data.frame(stockcode = "her.27.25-2932", genusSpecies = "Clupea harengus"),
  data.frame(stockcode = "meg.27.7b-k8abd", genusSpecies = "Lepidorhombus whiffiagonis"),
  data.frame(stockcode = "pil.27.8abd", genusSpecies = "Sardina pilchardus"),
  data.frame(stockcode = "pil.27.8c9a", genusSpecies = "Sardina pilchardus"),
  data.frame(stockcode = "ple.27.420", genusSpecies = "Pleuronectes platessa"),
  data.frame(stockcode = "ple.27.7a", genusSpecies = "Pleuronectes platessa"),
  data.frame(stockcode = "ple.27.21-23", genusSpecies = "Pleuronectes platessa"),
  data.frame(stockcode = "pok.27.3a46", genusSpecies = "Pollachius virens"),
  data.frame(stockcode = "sol.27.4", genusSpecies = "Solea solea"),
  data.frame(stockcode = "sol.27.7a", genusSpecies = "Solea solea"),
  data.frame(stockcode = "sol.27.7e", genusSpecies = "Solea solea"),
  data.frame(stockcode = "sol.27.7fg", genusSpecies = "Solea solea"),
  data.frame(stockcode = "sol.27.8ab", genusSpecies = "Solea solea"),
  data.frame(stockcode = "sol.27.20-24", genusSpecies = "Solea solea"),
  data.frame(stockcode = "spr.27.22-32", genusSpecies = "Sprattus sprattus"),
  data.frame(stockcode = "whg.27.47d", genusSpecies = "Merlangius merlangus"),
  data.frame(stockcode = "whg.27.7a", genusSpecies = "Merlangius merlangus"),
  data.frame(stockcode = "whg.27.7b-ce-k", genusSpecies = "Merlangius merlangus")
)

uSpp <- unique(lutSpp$genusSpecies)
L <- lapply(X = as.list(uSpp), FUN = function(x){length_weight(x[1])})
names(L) <- uSpp

L2 <- lapply(X = L, FUN = function(x){
  tmp <- as.data.frame(x)
  tmp <- subset(tmp, Sex %in% c("mixed", "unsexed"))
  tmp <- unique(tmp[!is.na(tmp$Number),c("a", "b", "Number")])
  c(a = exp(weighted.mean(log(tmp$a), w = tmp$Number)), 
    b = weighted.mean(tmp$b, w = tmp$Number))
})

lutLWcoefs <- as.data.frame(do.call("rbind", L2))
lutLWcoefs$genusSpecies <- rownames(lutLWcoefs)

lutLWcoefs <- merge(lutSpp[,c("genusSpecies", "stockcode")], lutLWcoefs)
lutLWcoefs
  



# 4. save --------------------------------------------------------------------

save(lutYearAge, file = "data/lutYearAge.Rdata")
save(lutLWcoefs, file = "data/lutLWcoefs.Rdata")