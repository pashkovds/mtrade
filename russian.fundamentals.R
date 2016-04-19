library(Mdata)
library(MUpdaters)
library(TTR)
library(gtools)
options(stringsAsFactors = F)


###################
mdata.man <- MDataManager$new()
mdata.man$add.updater(
    MDataUpdater.Universe.Russian.Equities.ENG$new(
      mdata.man$get.mdata.set(),
      MData$new(),
      mdata.name = "universe.russia",
      description = data.frame(type="ticker.list.type3")
    )
)





universe.df <- mdata.man$get.mdata("universe.russia")$get.field.values()
for( i in 1:nrow(universe.df)){
  for(fundamental.type in c("is","bs","cf")){
    for(freq in c("a","q")){
        mdata.man$add.updater(
          MDataUpdater.YF.fundamentals$new(
            mdata.set = mdata.man$get.mdata.set(),
            mdata = MData$new(),
            mdata.name = paste(universe.df$Symbol[i],fundamental.type,freq,sep="."),
            description=data.frame(
              type = "fundamentals",
              fnd.type = fundamental.type,
              ticker = universe.df$Symbol[i],
              is.annual = freq
            ),
            country = universe.df$Country[i],
            ticker = universe.df$Symbol[i],
            fnd.type = fundamental.type,
            is.annual = ifelse(freq=="a",T,F)
          )
        )
    }
  }
}

mdata.man$update.all()
mdata.man$remove.non.updatable()


mdata.man$list.names()
mdata.man$get.mdata("AVAZ.ME.is.q")$get.field.values()



mdata.man$add.updater(
  MDataUpdater.ExtractByTime$new(
    mdata.set = mdata.man$get.mdata.set(),
    mdata = MData$new(),
    mdata.name = "aggregated.balance.sheet",
    date.to.find= Sys.Date(),
    date.field.name = "DAT.DAT",
    fields.to.extract = c("DATE","Cas.And.Cas.Equ","Pro.Pla.and.Equ","Tot.Ass","Acc.Pay","Tot.Cur.Lia","Tot.Lia","Ret.Ear","Tot.Sto.Equ"),
    add.from.desc = c("Company","Curncy","Country","FundamentalFrequency"),
    descriptions = mdata.man$get.updater.descriptions(),
    rule.to.find.tables = list(fnd.type  = "bs", is.annual="a")
  )
)


mdata.man$add.updater(
  MDataUpdater.ExtractByTime$new(
    mdata.set = mdata.man$get.mdata.set(),
    mdata = MData$new(),
    mdata.name = "aggregated.income.statement",
    date.to.find= Sys.Date(),
    date.field.name = "DAT.DAT",
    fields.to.extract = c("DATE","Tot.Rev","Cos.of.Rev","Res.Dev","Ope.Inc.or.Los","Net.Inc"),
    add.from.desc = NULL,
    descriptions = mdata.man$get.updater.descriptions(),
    rule.to.find.tables = list(fnd.type  = "is", is.annual="a")
  )
)

mdata.man$add.updater(
  MDataUpdater.ExtractByTime$new(
    mdata.set = mdata.man$get.mdata.set(),
    mdata = MData$new(),
    mdata.name = "aggregated.cash.flow",
    date.to.find= Sys.Date(),
    date.field.name = "DAT.DAT",
    fields.to.extract = c("DATE","Tot.Cas.Flo.Fro.Ope.Act", "Tot.Cas.Flo.Fro.Ope.Act", "Tot.Cas.Flo.Fro.Fin.Act"),
    add.from.desc = NULL,
    descriptions = mdata.man$get.updater.descriptions(),
    rule.to.find.tables = list(fnd.type  = "cf", is.annual="a")
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



head(mdata.man$get.mdata("fundamentals.snapshot")$get.field.values())
mdata.man$get.mdata("fundamentals.snapshot")$get.taxonomy()
#save(mdata.man, file="./data/russian.fundamentals.RData")


mdata.man$get.updater.descriptions()

mdata.man$get.mdata("NMTP.ME.is.a")$get.taxonomy()
df <- mdata.man$get.mdata("NMTP.ME.is.a")$get.field.values()
diff(df$Tot.Rev)/df$Tot.Rev[-nrow(df)]
df$Gro.Pro/df$Tot.Rev
