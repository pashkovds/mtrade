library(Mdata)
library(MUpdaters)
mdata.man <- MDataManager$new()
mdata.man$add.instruction(
  instr.function = function(){
    mdata.man$add.updater(
      MDataUpdater.InvestFunds.FundList$new(
        mdata.man$get.mdata.set(),
        MData$new(),
        mdata.name = "pif.fund.set",
        description = data.frame(type="am.comp.universe")
      )
    )
  }
)


mdata.man$add.instruction(
  instr.function = function(){
      df <- mdata.man$get.mdata("pif.fund.set")$get.field.values()
      df <- df[!is.na(df$Perf.3y),]
      df <- df[order(df$Perf.3y),]
      df <- df[df$Type == "Open-end",]
      
      for(i in 1:nrow(df)){
        mdata.man$add.updater(
          MDataUpdater.InvestFunds.FundHistory.RUB$new(
            mdata.man$get.mdata.set(),
            MData$new(),
            mdata.name = paste("i",df$AK.id[i],sep="."),
            description = data.frame(type="am.comp.history", 
                                     currency = "RUB",
                                     fund.name = df$Fund[i],
                                     category=df$Category[i]),
            fund.id = df$AK.id[i]
          )
        )
      }
  })


mdata.man$add.instruction(
  instr.function = function(){
    mdata.man$update.all()
  }
)


mdata.man$execute.all.instructions()
