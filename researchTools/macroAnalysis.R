


md <- mdata.man$get.mdata("common.table.macro")

Country.set <- na.omit(unique(md$get.taxonomy()$CountryID))
Template.set <- na.omit(unique(md$get.taxonomy()$TemplateId))
macro.fids.matrix <- do.call(
  rbind,
  lapply( 
    Template.set,
    function(tmplt){
      search.fids(md$get.taxonomy(), CountryID = Country.set, TemplateId = tmplt)
    }
  )
)
colnames(macro.fids.matrix) <- Country.set
rownames(macro.fids.matrix) <- Template.set
rownames(macro.fids.matrix)

timeline <- md$get.field.values(md$get.fids(fname = "DATADATE"))
stock <- apply(as.matrix(md$get.field.values())[,macro.fids.matrix["ST.STOCK",]], 2, as.numeric)
cpi <- apply(as.matrix(md$get.field.values())[,macro.fids.matrix["ST.CPI",]], 2, as.numeric)
gdp <- apply(as.matrix(md$get.field.values())[,macro.fids.matrix["ST.GDP",]], 2, as.numeric)
gdp.1 <- apply(as.matrix(md$get.field.values())[,macro.fids.matrix["GDP",]], 2, as.numeric)
irate <- apply(as.matrix(md$get.field.values())[,macro.fids.matrix["ST.IRATE",]], 2, as.numeric)/100
indprod <- apply(as.matrix(md$get.field.values())[,macro.fids.matrix["ST.INDPROD",]], 2, as.numeric)
cli <- apply(as.matrix(md$get.field.values())[,macro.fids.matrix["ST.CLI",]], 2, as.numeric)



library(caTools)
delay.N = 6
stock.rets.cur <- apply(stock, 2, function(s){ 
  s1 <- c(NA, diff(s)/s[-length(s)])
  s1
})
stock.rets <- apply(stock, 2, function(s){ 
  s1 <- c(NA, diff(s)/s[-length(s)])
  c(s1[-(1:delay.N )],rep(NA,delay.N ))
})
stock.rets[abs(stock.rets)>1] <- 0
inflation <- apply(cpi, 2, function(s){ c(NA,diff(s)/s[-length(s)])})
inflation <- 12*apply(inflation,2,runmean, 12, align= "right")

signal <- (-1) * t(apply(gdp,1, function(s){ s- mean(s, na.rm=T)}))
#signal <-  stock.rets.cur #- inflation
#signal <- stock.rets.cur
#signal <-  (-1)* gdp
#signal <- (-1) * apply(signal, 2, function(s){ c(NA,diff(s))})
signal <-  apply(signal, 2, runmean, 12, align = "right")
#signal <-  apply(signal, 2, runmean, 6, align = "right")
signal <- signal/ apply(signal, 2, runsd, 12*5, align = "right")
signal <-  t(apply(signal ,1, function(s){s-mean(s,na.rm=T)}))
#signal <-  t(apply(signal ,1, function(s){rank(s)}))
signal <- signal/rowSums(abs(signal), na.rm=T)

par(mfrow=c(1,1))
plot(timeline, cumsum(rowSums( signal*stock.rets, na.rm=T)),type="l")
barplot(sort(tail(signal,1)[1,]))
