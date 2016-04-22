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
descriptions <- descriptions[descriptions$type %in% "macro.series", ]

ts.common <- do.call(
  cbind,
  lapply(
    descriptions$Name.Updater,
    function(upd.name) {
      cat(upd.name,"\n")
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
    mdata.name = "common.table.macro",
    description = data.frame(
      type="common.table",
      name="macro.series.aggreagted"
    ),
    data.frame.input = common.df,
    new.taxonomy = macro.tickers
  )
)
