library(Mdata)
library(MUpdaters)
library(TTR)
library(gtools)
options(stringsAsFactors = F)


###################
mdata.man <- MDataManager$new()

mdata.man$add.updater(
  MDataUpdater.Custom$new(
    mdata.man$get.mdata.set(),
    MData$new(),
    mdata.name = "universe.index.list",
    data.frame.input = do.call(
      rbind,
      list(
        data.frame(ticker = "^HSI", Country="HongKong"),
        data.frame(ticker = "^FCHI", Country="France"),
        data.frame(ticker = "^GDAXI", Country="Germany"),
        data.frame(ticker = "^SSMI", Country="Switherland"),
        data.frame(ticker = "^FTSE", Country="UK")
      )
    )
  )
)


df.with.index.tickers <- mdata.man$get.mdata("universe.index.list")$get.field.values()
for(I in 1:nrow(df.with.index.tickers)){
  cur.cfg.df <- df.with.index.tickers[I,]
    mdata.man$add.updater(
      MDataUpdater.Universe.YF.IndexComponents$new(
        mdata.set = mdata.man$get.mdata.set(),
        mdata = MData$new(),
        mdata.name = paste("universe",df.with.index.tickers$Country[I],sep="."),
        description = data.frame(type="ticker.list.type1"),
        index.ticker = df.with.index.tickers$ticker[I],
        country = df.with.index.tickers$Country[I],
        index.type = "Equities"
      )
    )
}

mdata.man$add.updater(
  MDataUpdater.Universe.US.Equities$new(
    mdata.set = mdata.man$get.mdata.set(),
    mdata = MData$new(),
    mdata.name = "universe.US",
    description = data.frame(type="ticker.list.type2")
  )
)



mdata.man$add.updater(
  MDataUpdater.Custom$new(
    mdata.man$get.mdata.set(),
    MData$new(),
    mdata.name = "universe.global",
    description = data.frame(type="ticker.list.global"),
    data.frame.input = function(){
      
      tmp.desc <- Reduce(smartbind, lapply(mdata.man$get.updater.set()$list.names(), function(upd.name) {
            df<- mdata.man$get.updater.set()$get(upd.name)$get.updater.description()
            df$Name.Updater <- upd.name
            df
        }))
      updater.names.set <- tmp.desc$Name.Updater[tmp.desc$type%in%"ticker.list.type1"]
      universe.df.type1 <- do.call(
        rbind,
        lapply(
          updater.names.set,
          function(upd.name){
            mdata.man$get.mdata(upd.name)$get.field.values()   
          }
        )
      )
      universe.df.type2 <- mdata.man$get.mdata(tmp.desc$Name.Updater[tmp.desc$type%in%"ticker.list.type2"])$get.field.values()  
      universe.df.type2 <- universe.df.type2[order(universe.df.type2$MarketCap.num,decreasing = T)[1:300],]
      universe.df <- rbind(
        universe.df.type1[,c("Symbol","Name","Index.Country")],
        universe.df.type2[,c("Symbol","Name","Index.Country")]
      )
      universe.df
      
    }
  )
)



mdata.man$get.mdata("universe.global")$get.field.values()


mdata.man$update.all()
mdata.uni <- mdata.man$get.mdata("universe")
tail(mdata.uni$get.field.values())


for( ticker in mdata.uni$get.field.values(fids = mdata.uni$get.fids(fname="Symbol"), tids = 1:20)){
  mdata.man$add.updater(
    MDataUpdater.YF.fundamentals$new(
      mdata.set = mdata.man$get.mdata.set(),
      mdata = MData$new(),
      mdata.name = paste(ticker,"cf",sep="."),
      country = "US",
      ticker = ticker,
      fnd.type = "cf",
      is.annual = T
    )
  )
  
  mdata.man$add.updater(
    MDataUpdater.YF.fundamentals$new(
      mdata.set = mdata.man$get.mdata.set(),
      mdata = MData$new(),
      mdata.name = paste(ticker,"bs",sep="."),
      country = "US",
      ticker = ticker,
      fnd.type = "bs",
      is.annual = T
    )
  )
  
  mdata.man$add.updater(
    MDataUpdater.YF.fundamentals$new(
      mdata.set = mdata.man$get.mdata.set(),
      mdata = MData$new(),
      mdata.name = paste(ticker,"is",sep="."),
      country = "US",
      ticker = ticker,
      fnd.type = "is",
      is.annual = T
    )
  )
  
}




mdata.man$update.all()
mdata.man$remove.non.updatable()
descr.df <- mdata.man$get.descriptions()



mdata.man$add.updater(
  MDataUpdater.ExtractByTime$new(
    mdata.set = mdata.man$get.mdata.set(),
    mdata = MData$new(),
    mdata.name = "aggregated.balance.sheet",
    date.to.find= as.Date("2015-01-01"),
    date.field.name = "DAT.DAT",
    fields.to.extract = c("DATE","Cas.And.Cas.Equ","Pro.Pla.and.Equ","Tot.Ass","Acc.Pay","Tot.Cur.Lia","Tot.Lia","Ret.Ear","Tot.Sto.Equ"),
    add.from.desc = c("Company","Curncy","Country","FundamentalFrequency"),
    descriptions = mdata.man$get.descriptions(),
    rule.to.find.tables = list(FundamentalType  = "bs")
  )
)

mdata.man$add.updater(
  MDataUpdater.ExtractByTime$new(
    mdata.set = mdata.man$get.mdata.set(),
    mdata = MData$new(),
    mdata.name = "aggregated.income.statement",
    date.to.find= as.Date("2015-01-01"),
    date.field.name = "DAT.DAT",
    fields.to.extract = c("DATE","Tot.Rev","Cos.of.Rev","Res.Dev","Ope.Inc.or.Los","Net.Inc"),
    add.from.desc = NULL,
    descriptions = mdata.man$get.descriptions(),
    rule.to.find.tables = list(FundamentalType  = "is")
  )
)

mdata.man$add.updater(
  MDataUpdater.ExtractByTime$new(
    mdata.set = mdata.man$get.mdata.set(),
    mdata = MData$new(),
    mdata.name = "aggregated.cash.flow",
    date.to.find= as.Date("2015-01-01"),
    date.field.name = "DAT.DAT",
    fields.to.extract = c("DATE","Tot.Cas.Flo.Fro.Ope.Act", "Tot.Cas.Flo.Fro.Ope.Act", "Tot.Cas.Flo.Fro.Fin.Act"),
    add.from.desc = NULL,
    descriptions = mdata.man$get.descriptions(),
    rule.to.find.tables = list(FundamentalType  = "cf")
  )
)


mdata.man$add.updater(
  MDataUpdater.MultipleMerge$new(
    mdata.set = mdata.man$get.mdata.set(),
    mdata = MData$new(),
    mdata.name = "fundamentals.snapshot",
    md.names  = c("aggregated.balance.sheet", "aggregated.income.statement","aggregated.cash.flow"),
    merging.fields = c("tid","DATE")
  )
)


merge(
  mdata.man$get.mdata("fundamentals.snapshot")$get.field.values(),
  mdata.man$get.mdata("universe")$get.field.values(),
  by.x = "Company", by.y = "Symbol"
)


########################################
###################
mdata.man <- MDataManager$new()

mdata.man$add.updater(
  MDataUpdater.Universe.YF.IndexComponents$new(
    mdata.set = mdata.man$get.mdata.set(),
    mdata = MData$new(),
    mdata.name = "universe",
    index.ticker = "^HSI",
    country = "China",
    index.type = "Equities"
  )
)

mdata.man$update.all()
mdata.uni <- mdata.man$get.mdata("universe")
tail(mdata.uni$get.field.values())



Country <- mdata.uni$get.field.values(fids = mdata.uni$get.fids(fname="Index.Country"))
names(Country) <- mdata.uni$get.field.values(fids = mdata.uni$get.fids(fname="Symbol"))
for( ticker in mdata.uni$get.field.values(fids = mdata.uni$get.fids(fname="Symbol"))){
  mdata.man$add.updater(
    MDataUpdater.Universe.YF.CorpKeyStat $new(
      mdata.set = mdata.man$get.mdata.set(),
      mdata = MData$new(),
      mdata.name = ticker,
      country = Country[ticker],
      ticker = ticker,
      is.annual = T
    )
  )
}

mdata.man$update.all()
mdata.man$remove.non.updatable()
descr.df <- mdata.man$get.descriptions()

do.call(rbind,lapply(descr.df$Name.Updater[-1], function(nm){mdata.man$get.mdata(nm)$get.field.values()}))




