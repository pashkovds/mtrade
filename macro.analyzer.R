library(Mdata)
library(MUpdaters)
library(TTR)
library(timeSeries)
library(gtools)
options(stringsAsFactors = F)


###################
mdata.man <- MDataManager$new()
mdata.man$get.global.params()$add("Keys.Quandl", MParameter$new("HNtnQqxZm6RHifHEfTJA"))


mdata.man$add.updater(
  MDataUpdater.Custom$new(
    mdata.man$get.mdata.set(),
    MData$new(),
    mdata.name = "db.Country",
    description = data.frame(type="db", name="Country"),
    data.frame.input = read.csv("./db/Country.csv")
  )
)

mdata.man$add.updater(
  MDataUpdater.Custom$new(
    mdata.man$get.mdata.set(),
    MData$new(),
    mdata.name = "db.Suffix",
    description = data.frame(type="db", name="Suffix"),
    data.frame.input = read.csv("./db/CountrySourceSuffix.csv")
  )
)

mdata.man$add.updater(
  MDataUpdater.Custom$new(
    mdata.man$get.mdata.set(),
    MData$new(),
    mdata.name = "db.Template",
    description = data.frame(type="db", name="Template"),
    data.frame.input = read.csv("./db/Template.csv")
  )
)




mdata.man$add.updater(
  MDataUpdater.Custom$new(
    mdata.man$get.mdata.set(),
    MData$new(),
    mdata.name = "MacroTickersTable",
    description = data.frame(type="ticker.table", name="macro"),
    data.frame.input = function(){
      df.tmp <- merge(
        merge(
          mdata.man$get.mdata("db.Template")$get.field.values(),
          mdata.man$get.mdata("db.Suffix")$get.field.values(),
          by = "SourceID"
        ),
        mdata.man$get.mdata("db.Country")$get.field.values(),
        by = "CountryID"
      )
      df.tmp$Request <- sprintf(df.tmp$Template, df.tmp$CountrySuffix)
      df.tmp$NewDataName <- paste(df.tmp$TemplateId,df.tmp$CountryShortCode,sep="_")
      df.tmp
    }
  )
)




df.tmp <- mdata.man$get.mdata("MacroTickersTable")$get.field.values()
for(i in 1:nrow(df.tmp)){
  mdata.man$add.updater(
    MDataUpdater.Quandl$new(
      mdata.man$get.mdata.set(),
      MData$new(),
      mdata.name = df.tmp$NewDataName[i],
      description = data.frame(
        type="macro.series",
        src = df.tmp$SourceID[i]
      ),
      ticker = df.tmp$Request[i],
      Keys.Quandl = mdata.man$get.global.params()$eval("Keys.Quandl")
    )
  )
}

mdata.man$update.all()
mdata.man$remove.non.updatable()

#save(mdata.man, file="./data/macro.analyzer.RData")


descriptions <- mdata.man$get.updater.descriptions()
descriptions <- descriptions[descriptions$src %in% "OECD", ]

ts.common <- do.call(
  cbind,
  lapply(
    descriptions$Name.Updater,
    function(upd.name) {
      df.tmp <- mdata.man$get.mdata(upd.name)$get.field.values()
      ts.data <- timeSeries(df.tmp$Value, df.tmp$Date)
      colnames(ts.data) <- upd.name
      ts.data
    }
  )
)

ts.common <- apply(ts.common, 2, na.locf, na.rm = F)
macro.tickers <- mdata.man$get.mdata("MacroTickersTable")$get.field.values()
macro.tickers <- macro.tickers[match(colnames(ts.common), macro.tickers$NewDataName),]
rownames(macro.tickers) <- NULL

macro.tickers$FieldType <-  "Variable"
macro.tickers <- smartbind(macro.tickers, data.frame(FieldType = "Date"))
common.df <- as.data.frame(ts.common)
common.df[["DATADATE"]] <- as.Date(time(ts.common))

mdata.man$add.updater(
  MDataUpdater.CustomTaxonomy$new(
    mdata.man$get.mdata.set(),
    MData$new(),
    mdata.name = "common.table.macro.oecd",
    description = data.frame(
      type="common.table",
      name="OECD.macro.series"
    ),
    data.frame.input = common.df,
    new.taxonomy = macro.tickers
  )
)

search.fids <- function(taxonomy, ...){
  lst.input <- list(...)
    id.lst <- lapply(
      names(lst.input),
      function(nms){
        which(taxonomy[[nms]] %in% lst.input[[nms]] )
      }
    )
    
    id.fin <- Reduce(intersect, id.lst)
    if(!is.null(id.fin)){
      tax.tmp <- taxonomy[id.fin,]
      id.fin.order <- match(lst.input[[1]], tax.tmp[[names(lst.input)[1]]])
      id.fin <- id.fin[id.fin.order]
    }
    id.fin
}




md <- mdata.man$get.mdata("common.table.macro.oecd")

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

#signal <- (-1) * t(apply(gdp,1, function(s){ s- mean(s, na.rm=T)}))
#signal <-  irate #- inflation
#signal <- stock.rets.cur
signal <- (-1) * indprod
#signal <- (-1) * apply(signal, 2, function(s){ c(NA,diff(s))})
signal <- signal - apply(signal, 2, runmean, 12*5, align = "right")
#signal <-  apply(signal, 2, runmean, 6, align = "right")
signal <- signal/ apply(signal, 2, runsd, 12*5, align = "right")
signal <-  t(apply(signal ,1, function(s){ s- mean(s, na.rm=T)}))
signal <- signal/rowSums(abs(signal), na.rm=T)

par(mfrow=c(1,1))
plot(timeline, cumsum(rowSums( signal*stock.rets, na.rm=T)),type="l")
sort(tail(signal,1)[1,])

x <- indprod[,3]
y <- gdp[,3]

f.nrm <- function(s){
  (s - min(s, na.rm=T))/ (max(s, na.rm=T) - min(s, na.rm=T))
}
par(mfrow=c(2,1))
plot(tail(gdp[,3],500),type="l")
plot(tail(x,500),type="l")

par(mfrow=c(4,4))
for (i in 1:ncol(signal)){
z <- (signal*stock.rets)[,i]
z[is.na(z)] <- 0
plot(timeline,cumsum(z),type="l", main = colnames(signal)[i])
}
