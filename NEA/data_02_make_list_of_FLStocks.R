
# 0. clean-up ----------------------------------------------------------------

rm(list = ls())

# 1. load files ----------------------------------------------------------

load(file = "NEA/data/lutFLStocks.Rdata")



# 2. make list of growth data ---------------------------------------------
stockcodes <- sort(unique(lutFLStocks$stockcode))

stks <- vector("list", length = length(stockcodes))
names(stks) <- stockcodes

pb <- txtProgressBar(min = 0, max = length(stks), style = 3)
for(i in seq(stks)){
  stockcode.i <- names(stks)[i]
  mat.i <- which(lutFLStocks$stockcode == stockcode.i)
  tmp <- load(lutFLStocks$path[mat.i], verbose = TRUE)
  if(length(tmp) == 1){
    stock.i <- get(tmp)
  }else{
    stock.i <- get(lutFLStocks$objname[mat.i])
  }
  
  stks[[i]]$stock <- stock.i
  stks[[i]]$stockcode <- stockcode.i
  stks[[i]]$casestudy <- lutFLStocks$casestudy[mat.i]
  stks[[i]]$range <- range(stock.i)
  
  rm(stock.i)
  setTxtProgressBar(pb, i)
}
close(pb)


# 3. summary plot ---------------------------------------------------------

pdf("NEA/data/FLStock_summaries.pdf", width = 7, height = 5, onefile = T)
for(i in seq(stks)){
  p1 <- plot(stks[[i]]$stock)
  # p1 <- p1 + ggtitle(stks[[i]]$stockcode)
  
  wt <- as.data.frame(stks[[i]]$stock@stock.wt)
  p2 <- ggplot(data = wt) + aes(x = year, y = data, group = age, color = factor(age)) + 
    geom_line(linewidth = 1) + 
    xlab("") + ylab("") +
    scale_color_brewer(palette = "Spectral")
  # p2 <- p2 + ggtitle(stks[[i]]$stockcode)
  
  p <- (p1 + p2) +
    plot_annotation(title = stks[[i]]$stockcode)
  
  print(p)
}
dev.off()



# 4. save -----------------------------------------------------------------

save(stks, file = "NEA/data/FLStocks_list_stks.Rdata")

