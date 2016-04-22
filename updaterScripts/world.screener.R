library(Mdata)
library(MUpdaters)
library(TTR)
library(gtools)
options(stringsAsFactors = F)


###################
mdata.man <- MDataManager$new()

mdata.man$add.instruction(
  function(){
      mdata.man$add.updater(
        MDataUpdater.Custom$new(
          mdata.man$get.mdata.set(),
          MData$new(),
          mdata.name = "universe.index.list",
          description = data.frame(type="reference.ticker.table"),
          data.frame.input = do.call(
            rbind,
            list(
              data.frame(ticker = "^HSI", Country="HongKong"),
              data.frame(ticker = "^FCHI", Country="France"),
              data.frame(ticker = "^GDAXI", Country="Germany"),
              data.frame(ticker = "^FTSE", Country="UK")
            )
          )
        )
      )
  }
)
  



mdata.man$add.instruction(
  function(){
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
  }
)




mdata.man$add.instruction(
  function(){
    mdata.man$add.updater(
      MDataUpdater.Universe.US.Equities$new(
        mdata.set = mdata.man$get.mdata.set(),
        mdata = MData$new(),
        mdata.name = "universe.US",
        description = data.frame(type="ticker.list.type2")
      )
    )
  }
)




mdata.man$add.instruction(
  function(){
      mdata.man$add.updater(
        MDataUpdater.Custom$new(
          mdata.man$get.mdata.set(),
          MData$new(),
          mdata.name = "universe.global",
          description = data.frame(type="ticker.list.global"),
          data.frame.input = function(){
            
            tmp.desc <- mdata.man$get.updater.descriptions()
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
  }
)




mdata.man$add.instruction(
  function(){
    universe.df <- mdata.man$get.mdata("universe.global")$get.field.values()
    #universe.df <- tail(universe.df,10)
    for( I in 1:nrow(universe.df)){
        mdata.man$add.updater(
          MDataUpdater.Universe.YF.CorpKeyStat$new(
            mdata.set = mdata.man$get.mdata.set(),
            mdata = MData$new(),
            mdata.name = paste("keystat",universe.df$Symbol[I],sep="_"),
            description = data.frame(type = "key.statistics"),
            country = universe.df$Index.Country[I],
            ticker = universe.df$Symbol[I],
            is.annual = T
          )
        )
    }
  }
)

mdata.man$add.instruction(
  function(){
    mdata.man$update.all()
    mdata.man$remove.non.updatable()
  }
)


mdata.man$add.instruction(
  function(){
      mdata.man$add.updater(
        MDataUpdater.Custom$new(
          mdata.man$get.mdata.set(),
          MData$new(),
          mdata.name = "global.key.stat.table",
          description = data.frame(type="global.key.stat.table"),
          data.frame.input = function(){
            
            upd.desc.table <- mdata.man$get.updater.descriptions()
            key.stat.table <- do.call(
              rbind,
              lapply(
                upd.desc.table$Name.Updater[upd.desc.table$type=="key.statistics"],
                function(upd.name){
                  mdata.man$get.mdata(upd.name)$get.field.values()
                }
              )
            )
            key.stat.table <- key.stat.table[,c("Sector","Currency","Company","Trailing.P.E.ttm.intraday",
                                                "Price.Sales.ttm","Price.Book.mrq","Operating.Margin.ttm",
                                                "Profit.Margin.ttm", "Return.on.Assets.ttm","Return.on.Equity.ttm",
                                                "Qtrly.Revenue.Growth.yoy","Qtrly.Earnings.Growth.yoy","Total.Debt.Equity.mrq")]
            
            key.stat.table
            
          }
        )
      )
  }
)

mdata.man$add.instruction(
  function(){
    mdata.man$add.updater(
      MDataUpdater.Custom$new(
        mdata.man$get.mdata.set(),
        MData$new(),
        mdata.name = "global.key.stat.table.rank",
        description = data.frame(type="global.key.stat.table", processing="ranked"),
        data.frame.input = function(){
            ranked.stats <- mdata.man$get.mdata("global.key.stat.table")$get.field.values()
            ranked.stats <- ranked.stats[ranked.stats$Sector!="N/A",]
            ranked.stats.n <- as.data.frame(
              lapply(
                ranked.stats,
                function(l){
                  if(is.numeric(l)){
                     l.r <- rank(l,na.last = T)
                     l.r[is.na(l)]<-NA
                     l.r.n <- (l.r-1)/max(l.r,na.rm=T)
                     l.r.n[is.na(l.r.n)] <- mean(l.r.n, na.rm=T)
                     l.r.n
                  }else{
                    l
                  }
                }
              )
            )
            ranked.stats.n 
        }
      )
    )
  }
)


mdata.man$add.instruction(
  function(){
    mdata.man$add.updater(
      MDataUpdater.Custom$new(
        mdata.man$get.mdata.set(),
        MData$new(),
        mdata.name = "global.key.stat.table.rank.neutral",
        description = data.frame(type="global.key.stat.table", processing="ranked.neutralized"),
        data.frame.input = function(){
          
            ranked.stats.n <- mdata.man$get.mdata("global.key.stat.table.rank")$get.field.values()
            ranked.stats.n.d<-do.call(
                rbind,
                by(
                 ranked.stats.n, 
                 list(ranked.stats.n$Sector,ranked.stats.n$Currency),
                 function(c.df){
                    as.data.frame(
                      lapply(
                        c.df,
                        function(l){
                          if(is.numeric(l)){
                            l1 <- l - mean(l)
                            l2 <- l1/sd(l1,na.rm=T)
                            l2
                          } else{
                            l
                          }
                        }
                      )
                    )
                }
              )
            )
            
            ranked.stats.n.d$quality <- with(ranked.stats.n.d, Operating.Margin.ttm + Profit.Margin.ttm + Return.on.Assets.ttm + Return.on.Equity.ttm  + Qtrly.Revenue.Growth.yoy + Qtrly.Earnings.Growth.yoy  - Total.Debt.Equity.mrq)
            ranked.stats.n.d$price <- with(ranked.stats.n.d, Trailing.P.E.ttm.intraday +Price.Sales.ttm +  Price.Book.mrq )
            ranked.stats.n.d$quality[is.na(ranked.stats.n.d$quality)] <- 0
            ranked.stats.n.d$price[is.na(ranked.stats.n.d$price)] <- 0
            ranked.stats.n.d$quality <- with(ranked.stats.n.d, quality/sd(quality))
            ranked.stats.n.d$price <- with(ranked.stats.n.d, price/sd(price))
            ranked.stats.n.d
        }
      )
    )
  }
)

mdata.man$execute.all.instructions()
save(mdata.man, file = "./data/world.equity.screener.RData")

