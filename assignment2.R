assignment2 <- function() {
  
  library("downloader")
  library("plyr")
  library("ggplot2")
  
  data.URL <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2"
  
  wx.data <- data.processing.do(data.URL)
  wx.summaries <- analysis.do(wx.data)
  
  x <- analysis.cross.tabulate(wx.summaries)
  
  f <- wx.summaries[order(wx.summaries$Total.Fatalities, decreasing=TRUE), c(1,2)]
  print(f[1:10,])
  
  d <- wx.summaries[order(wx.summaries$Total.Damage, decreasing=TRUE), c(1,3)]
  print(d[1:10,])
  
  analysis.plot(x, "Correlation between Fatalities and Destruction Caused by Weather Events")
  
  return(x)
}

data.processing.do <- function(location) {
  
  data <- data.processing.read(location)
  data$BGN_DATE <- data.processing.convert.dates(data$BGN_DATE)
  data <- transform(data, YEAR = data.processing.get.year(data$BGN_DATE))

  return(data)
}

data.processing.read <- function(location) {
  
  zipped.file.name <- "repdata-data-StormData.csv.bz2"  
  if (!file.exists(zipped.file.name)) {    
    download(location, zipped.file.name, quiet=FALSE)          
  }   
  bz.con <- bzfile(zipped.file.name)
  data <- read.csv(bz.con, nrows=400000) ## be sure to change this before final
  
  return(data)
}

data.processing.convert.dates <- function(dates) {
  return (as.Date(dates, "%m/%d/%Y"))
}

data.processing.get.year <- function(date) {
  return(as.numeric(format(date, format="%Y")))
}

analysis.do <- function(wx.df) {
  
  fatalities <- ddply(wx.df, .(EVTYPE), summarise, total=sum(FATALITIES, na.rm=TRUE))
  damage <- ddply(wx.df, .(EVTYPE), summarise, total=sum(PROPDMG, na.rm=TRUE))

  impact <- cbind(fatalities, damage)
  impact[, 3] <- NULL
  
  colnames(impact) <- c("Event", "Total.Fatalities", "Total.Damage")
  return(impact)
}

analysis.cross.tabulate <- function(wx.summ) {

  sort.by.fatalities <- wx.summ[ order(wx.summ$Total.Fatalities, decreasing=TRUE), ]
  sort.by.fatalities <- sort.by.fatalities[1:10, ]
  
  sort.by.damage <- wx.summ[ order(wx.summ$Total.Damage, decreasing=TRUE), ]
  sort.by.damage <- sort.by.damage[1:10, ]
  
  cross.tab <- merge(sort.by.damage, sort.by.fatalities)
  return(cross.tab)
}

analysis.plot <- function(data, t="Analysis") {
  
  ## png("analysis.png", width=800, height=600)
  p <- ggplot(data, aes(x=Total.Fatalities, y=(Total.Damage/1000), color=Event))
  p <- p + geom_point(size=6)
  p <- p + xlab("Total Fatalities")
  p <- p + ylab("Total Damage (in $1,000s)")
  p <- p + ggtitle(t)
  
  print(p)
  ## dev.off()
}

