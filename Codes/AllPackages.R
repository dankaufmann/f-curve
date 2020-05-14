#-------------------------------------------------------------------------------------
# A daily fever curve for the Swiss economy
#-------------------------------------------------------------------------------------
# Feel free to copy, adapt, and use this code for your own purposes at 
# your own risk.
#
# Please cite: 
# Burri, Marc and Daniel Kaufmann (2020): "A daily fever curve for the
# Swiss economy", IRENE Working Paper No., University of Neuchâtel,
# https://github.com/dankaufmann/f-curve
#
# Marc Burri and Daniel Kaufmann, 2020 (daniel.kaufmann@unine.ch)
#-------------------------------------------------------------------------------------
# V 1.0
#-------------------------------------------------------------------------------------

library(tsbox)
library(ggplot2)
library(forecast)
library(xts)
library(ggpubr)
library(lubridate)
library(xlsx)
library(quantmod)
library(missMDA)
library(zoo)
library(seasonal)
library(XLConnect)
library(matlib)
library(reshape2)
library(scales)
library(stringr)
library(kableExtra)
library(magick)
library(webshot)
webshot::install_phantomjs()

# SEttings for all scripts
figwidth  <- 6.5
figheight <- 5.5

myLines  <- c("2001-09-11", "2008-09-16", "2011-08-04", "2015-01-15", "2020-03-16")
myLabels <- c("9/11 attacks", "Collapse Lehman Brothers", "Barroso: Euro debt crisis spreads", "Removal exchange rate floor", "Covid-19 lockdown")

# Make output directory forder
makeOutDir <- function(mainDir, outDir){
  
  if (file.exists(outDir)){
    setwd(file.path(mainDir, outDir))
  } else {
    dir.create(file.path(mainDir, outDir))
  }
  return(paste(mainDir, outDir , sep="")) # combines the stringe mainDir and outDir with seperation "" (i.e. w/o any separation)
}


# Function for computing overall and (optional) domestic and foreign decomposition
computeFactors <- function(Indicators, leadTS, noMANews, normStart, startDate, endDate, whichInd, indexDom) {
  
  
  
  # Shorten data to those indicators that should be used and to the sample for the normalization
  AllIndicators <- ts_span(Indicators, normStart, endDate)
  AllIndicators <- AllIndicators[,whichInd]
  
  # Remove all observation if not at least 4 indicators available (on these dates, there is no update of indicator)
  toDelete <- FALSE
  delIndex <- c()
  for(t in 1:dim(AllIndicators)[1]){
    thisObs <- AllIndicators[t,]
    toDelete <- sum(!is.na(thisObs))<4
    if(toDelete){
      delIndex <- c(delIndex, t)
    }
  }
  if(sum(delIndex)>0){
    AllIndicators <- AllIndicators[-delIndex,]  
  }
  
  # Remove weekends
  AllIndicators <- AllIndicators[weekdays(index(AllIndicators)) != "Saturday",]
  AllIndicators <- AllIndicators[weekdays(index(AllIndicators)) != "Sunday",]
  
  # Remove all indicators that are completely missing
  delIndex <- c()
  for(i in 1:dim(AllIndicators)[2]){
    toDelete <- sum(!is.na(AllIndicators[,i]))==0
    if(toDelete){
      delIndex <- c(delIndex, i)
      whichInd = whichInd[whichInd != i]
      delIndex <- c(delIndex, i)
      
    }
  }
  if(sum(delIndex)>0){
    AllIndicators <- AllIndicators[, -delIndex]
    indexDom      <- indexDom[-delIndex]
  }
  
  # Last observation after sampling decisions
  lastObsDate   <- index(AllIndicators[dim(AllIndicators)[1]])
  
  # Normalize and set the same start date for all
  for(i in 1:dim(AllIndicators)[2]){
    if(startsWith(colnames(AllIndicators[, i]), "TS.")){
      #print(colnames(AllIndicators[, i]))
      AllIndicators[, i]        <- normalize(-L.op(AllIndicators[, i], leadTS*300))[1:length(AllIndicators[,i])]
    }
    if(startsWith(colnames(AllIndicators[, i]), "News.")){
      #print(colnames(AllIndicators[, i]))
      AllIndicators[, i]        <- normalize(rollapply(AllIndicators[, i], noMANews, mean, na.rm = TRUE))
    }
    if(!startsWith(colnames(AllIndicators[, i]), "News.") & !startsWith(colnames(AllIndicators[, i]), "TS.")){
      AllIndicators[, i]        <- normalize(AllIndicators[, i])
    }
  }
  AllIndicators <- ts_span(AllIndicators, startDate, endDate)
  
  # Estimate factor model all variables
  # 0) Impute data matrix based on all observation
  X   <- na.locf(AllIndicators, maxgap = 3)
  X   <- AllIndicators
  X   <- imputePCA(as.matrix(X), ncp=4)$completeObs
  
  # 1) Estimate only one commom factor
  PC   <- prcomp(X)
  fc  <- xts(PC$x[,"PC1"], order.by=as.Date(index(AllIndicators)))
  colnames(fc) <- "fc"

  # 2) If there are foreign variables, compute the decomposition
  if(sum(!indexDom)>0) {
    
    
    # 2) Estimate the foreign factor on foreign variables only
    PCfor   <- prcomp(X[, !indexDom])
    fc_for  <- -xts(PCfor$x[,"PC1"], order.by=as.Date(index(AllIndicators[, !indexDom])))
    
    # 2) Remove what we explain by foreign factor
    PCdom   <- prcomp(X[, indexDom])
    fc_dom  <- xts(PCdom$x[,"PC1"], order.by=as.Date(index(AllIndicators[, indexDom])))
    
    Xdom   <- X[, indexDom]
    Xresid <- X[, indexDom]
    
    nrep = 50
    for(n in 1:nrep){
      for(i in 1:dim(Xdom)[2]){
        # Estimate residuals in domestic variables
        
        tempData <- data.frame(Xdom[,i], fc_for, fc_dom)
        colnames(tempData) <- c("x", "fc_for", "fc_dom")
        tempPred <- lm(x~fc_for+fc_dom, data = tempData, na.action="na.exclude")
        Xresid[,i] <- Xdom[,i] - tempPred$coefficients[1] - tempPred$coefficients[2]*fc_for
        #tempPred <- lm(fc~fc_for, data = tempData, na.action="na.exclude")
        #summary(tempPred)
      }
      PCdom   <- prcomp(Xresid)
      fc_dom  <- xts(PCdom$x[,"PC1"], order.by=as.Date(index(AllIndicators[, indexDom])))
    }
    
    tempData <- data.frame(fc, fc_for, fc_dom)
    colnames(tempData) <- c("fc", "fc_for", "fc_dom")
    tempPred <- lm(fc~fc_for+fc_dom, data = tempData, na.action="na.exclude")
    
    fc_for <- tempPred$coefficients[1]/2+tempPred$coefficients[2]*fc_for
    fc_dom <- tempPred$coefficients[1]/2+tempPred$coefficients[3]*fc_dom
    fc_res <- xts(residuals(tempPred), order.by=as.Date(index(AllIndicators[, !indexDom])))
    #tempData <- data.frame(fc, fc_for)
    #tempPred <- lm(fc~fc_for, data = tempData, na.action="na.exclude")
    #summary(tempPred)
    #fc_dom <- tempPred$coefficients[1]/2+xts(residuals(tempPred), order.by=as.Date(index(AllIndicators[, !indexDom])))
    #fc_for <- tempPred$coefficients[1]/2+tempPred$coefficients[2]*fc_for
  }
  
  if(sum(!indexDom)>0) {
    Results <- list(ts_c(fc, fc_dom, fc_for, fc_res), lastObsDate)
  }
  else{
    Results <- list(ts_c(fc), lastObsDate)
  }
  
  return(Results)
  
}


L.op <- function(ts, p) {
  # Lead/lag operator
  
  if(p<0){
    ts <- ts_xts(dplyr::lead(ts_ts(ts), abs(p)))
  }
  else{
    ts <- dplyr::lag(ts, p)
  }
  return(ts)
}


ggLayout <- function(p) {
  p <- p + theme_minimal() + ylab("")+xlab("")+
    ggplot2::scale_color_brewer(palette = "Dark2")+
    theme(legend.position="bottom",legend.margin=margin(0,0,0,0),legend.box.margin=margin(-20,-5,0,-5))+ggplot2::guides(col=guide_legend(byrow=TRUE))+ggplot2::theme(legend.title = element_blank())+
    theme(axis.line = element_line(colour = "black", size = 0.1))+ theme(panel.background = element_blank())+
    theme(panel.border = element_rect(linetype = "solid", colour = "black", fill = NA))+theme(text = element_text(family = "Palatino"))+
    theme(panel.grid.major = element_line(colour = "black",size=0.1,linetype="dotted"), panel.grid.minor = element_blank()) 
  return(p)
}

ggColor2 <- function(p) {
  p <- p + ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
    scale_color_manual(values = c("firebrick4", "blue4"))+ 
    scale_alpha_manual(values = c(0.5, 0.5))
  return(p)
}


ggColor3 <- function(p) {
  p <- p + ggplot2::geom_line(aes(),size=1)+ggplot2::scale_color_brewer(palette = "Dark2")+
    scale_color_manual(values = c("tomato1", "firebrick4", "blue4"))+ 
    scale_alpha_manual(values = c(0.5, 0.5, 1))
  return(p)
}

addLines <- function(p, myLines, myLabels, yInter, hor){
  if(missing(hor)) {
    hor = "vert" 
  }
    
  myLines <- as.Date(myLines)
  p <- p + geom_vline(xintercept=myLines , colour="black", size = .5, alpha = 0.5) 
  for(i in 1:length(myLines)){
    
    if(hor == "hor"){
      p <- p + ggplot2::annotate(geom="text", x=myLines[i], y=yInter, label=myLabels[i], color="black",  alpha = 0.7, hjust = -.3)
    }
    else{
      p <- p + ggplot2::annotate(geom="text", x=myLines[i], y=yInter, label=myLabels[i], color="black",  angle=90, alpha = 0.7, vjust = -1, hjust = 0)
    }
    
    
  }
  return(p)
}

addCorr <- function(p, Corr, xInter, yInter){
  p <- p + ggplot2::annotate(geom="text", x=as.Date(xInter), y=yInter, label=paste("Correlation: ", round(Corr, 2), sep = ""), vjust=0, hjust = -.2, color="black")
  return(p)
}

ts_fred <- function(..., class = "data.frame") {
  symb <- c(...)
  dta.env <- new.env()
  suppressMessages(getSymbols(symb, env = dta.env, src = "FRED"))
  z <- data.table::rbindlist(lapply(as.list(dta.env), ts_dt), idcol = "id")
  tsbox:::as_class(class)(z)
}
normalize <- function(x){
  x_norm = (x-mean(x, na.rm =TRUE))/sd(x, na.rm =TRUE)
  return(x_norm)
}
getForecastVariance <- function(fcst){
  # Function to extract forecast error variance from a forecast object
  # CI lower = y(t+h|t)-1.96*sig(h)
  # Therefore sig(h)^2 = [CI lower - y(t+h|t))/(-1.96)]^2
  # Get exact percentile (1.96 yield basically the same)
  
  z957 = qnorm(0.975, 0, 1)
  sigh2 = ((fcst$lower[,"95%"]-fcst$mean)/(-z957))^2
  return(sigh2)
}


timebased_ma <- function(df, days) {
  # Efficient function to calculate time based moving average per observation
  # Input: 
  # df = DataFrame
  # days: number of days for ma
  
  # aggregate in sliding window by a non-equi join
  ma <- data.table::setDT(df)[.(ud = time, ld = time -days), on = .(time <= ud,time >= ld), 
                              mean(as.double(SentimentScore)), by = .EACHI]
  colnames(ma) <- c("time", "timel", "ma")
  ma <- select(ma, c("time", "ma")) %>% na.omit() %>% tibble::as_tibble()
  df <- tibble::add_column(df, !!(paste0("ma",days)) := ma$ma)
  
}



# Functions and packages for News Indicator
library(rjson)
library(tm)
library(filesstrings)
library(dplyr)
library(rvest)


update_ta <- function() {
  load("../Data/News/ta.RData")
  # Load german stopwords
  stopw_de <- read.delim("../Data/News/Stopwords/stopwords_de.txt", fileEncoding = 'WINDOWS-1252', header=F, stringsAsFactors = F)
  colnames(stopw_de) <- "words"
  
  senti <- read_sentiws()
  # get positive and negative words of lexicon
  pos_w <-  filter(senti, value >= 0)
  neg_w <-  filter(senti, value <= 0)
  neg_w <- neg_w %>% # add some custom negative words, set value to same as e.g epidemie or krise
    add_row("words" = "corona", "value" = -0.0048, "class" = "NN", "pol" = "neg") %>% 
    add_row("words" = "pandemie", "value" = -0.0048, "class" = "NN", "pol" = "neg") %>% 
    add_row("words" = "kurzarbeit", "value" = -0.0048, "class" = "NN", "pol" = "neg")
  
  # update domestic
  path <- "../Data/News/TA/dom"
  files <- list.files(path, pattern = ".xlsx")
  df <- tibble()
  for (file in files) {
    tmp <- read.xlsx(paste0(path, "/", file), sheetName = "Sheet1", encoding = "UTF-8")
    df <- bind_rows(df, tmp)
    file.move(paste0(path, "/", file), "../Data/News/TA/dom/_archive", overwrite = T)
  }
  
  try({df <- df %>%
    mutate(time = as.Date(date, "%d.%m.%Y")) %>% 
    arrange(time) %>%
    rowwise() %>%
    mutate(cleanText = cleanTA(title, text, stopwds = stopw_de)) %>%
    mutate(pos_n = sum(!is.na(match(pos_w$words, str_split(cleanText, pattern = "\\s+")[[1]]))))  %>%
    mutate(neg_n = sum(!is.na(match(neg_w$words, str_split(cleanText, pattern = "\\s+")[[1]])))) %>%
    mutate(n_w = length(str_split(cleanText, pattern = "\\s+")[[1]])) %>%
    mutate(SentimentScore = (pos_n-neg_n)/n_w) %>%
    as_tibble() 
  
  df <- df[!duplicated(df),]
  
  df <- df %>% 
    group_by(time) %>%
    dplyr::summarize(mean = mean(SentimentScore, na.rm=TRUE),
                     sum = sum(n())) %>%
    filter(time > as.Date("1999-01-01") & time < Sys.Date())
  
  df_ta_ch <- bind_rows(df_ta_ch, df)}, 
  silent = T)
  
  
  
  # update foreign
  path <- "../Data/News/TA/for"
  files <- list.files(path, pattern = ".xlsx")
  df <- tibble()
  for (file in files) {
    tmp <- read.xlsx(paste0(path, "/", file), sheetName = "Sheet1", encoding = "UTF-8")
    df <- bind_rows(df, tmp)
    file.move(paste0(path, "/", file), "../Data/News/TA/for/_archive", overwrite = T)
  }
  
  
  try({df <- df %>%
    mutate(time = as.Date(date, "%d.%m.%Y")) %>% 
    arrange(time) %>%
    rowwise() %>%
    mutate(cleanText = cleanTA(title, text, stopwds = stopw_de)) %>%
    mutate(pos_n = sum(!is.na(match(pos_w$words, str_split(cleanText, pattern = "\\s+")[[1]]))))  %>%
    mutate(neg_n = sum(!is.na(match(neg_w$words, str_split(cleanText, pattern = "\\s+")[[1]])))) %>%
    mutate(n_w = length(str_split(cleanText, pattern = "\\s+")[[1]])) %>%
    mutate(SentimentScore = (pos_n-neg_n)/n_w) %>%
    as_tibble()
  
  df <- df[!duplicated(df),]
  
  df <- df %>% 
    group_by(time) %>%
    dplyr::summarize(mean = mean(SentimentScore, na.rm=TRUE),
                     sum = sum(n())) %>%
    filter(time > as.Date("1999-01-01") & time < Sys.Date())
  
  df_ta_int <- bind_rows(df_ta_int, df)},
  silent = T)
  
  save(df_ta_ch, df_ta_int, file = "../Data/News/ta.RData")
  
}


update_nzz <- function() {
  load("../Data/News/nzz.RData")
  # Load german stopwords
  stopw_de <- read.delim("../Data/News/Stopwords/stopwords_de.txt", fileEncoding = 'WINDOWS-1252', header=F, stringsAsFactors = F)
  colnames(stopw_de) <- "words"
  
  senti <- read_sentiws()
  # get positive and negative words of lexicon
  pos_w <-  filter(senti, value >= 0)
  neg_w <-  filter(senti, value <= 0)
  neg_w <- neg_w %>% # add some custom negative words, set value to same as e.g epidemie or krise
    add_row("words" = "corona", "value" = -0.0048, "class" = "NN", "pol" = "neg") %>% 
    add_row("words" = "pandemie", "value" = -0.0048, "class" = "NN", "pol" = "neg") %>% 
    add_row("words" = "kurzarbeit", "value" = -0.0048, "class" = "NN", "pol" = "neg")
  
  # update domestic
  path <- "../Data/News/NZZ/dom"
  files <- list.files(path, pattern = ".xlsx")
  df <- tibble()
  for (file in files) {
    tmp <- read.xlsx(paste0(path, "/", file), sheetName = "Sheet1", encoding = "UTF-8")
    df <- bind_rows(df, tmp)
    file.move(paste0(path, "/", file), "../Data/News/NZZ/dom/_archive", overwrite = T)
  }
  
  try({df <- df %>%
    mutate(time = as.Date(date, "%d.%m.%Y")) %>% 
    arrange(time) %>%
    rowwise() %>%
    mutate(cleanText = cleanNZZ(text, stopwds = stopw_de)) %>%
    mutate(pos_n = sum(!is.na(match(pos_w$words, str_split(cleanText, pattern = "\\s+")[[1]]))))  %>%
    mutate(neg_n = sum(!is.na(match(neg_w$words, str_split(cleanText, pattern = "\\s+")[[1]])))) %>%
    mutate(n_w = length(str_split(cleanText, pattern = "\\s+")[[1]])) %>%
    mutate(SentimentScore = (pos_n-neg_n)/n_w) %>%
    as_tibble() 
  
  df <- df[!duplicated(df),]
  
  df <- df %>% 
    group_by(time) %>%
    dplyr::summarize(mean = mean(SentimentScore, na.rm=TRUE),
                     sum = sum(n())) %>%
    filter(time > as.Date("1999-01-01") & time < Sys.Date())
  
  df_nzz_ch <- bind_rows(df_nzz_ch, df)}, 
  silent = T)
  
  
  
  # update foreign
  path <- "../Data/News/NZZ/for"
  files <- list.files(path, pattern = ".xlsx")
  df <- tibble()
  for (file in files) {
    tmp <- read.xlsx(paste0(path, "/", file), sheetName = "Sheet1", encoding = "UTF-8")
    df <- bind_rows(df, tmp)
    file.move(paste0(path, "/", file), "../Data/News/NZZ/for/_archive", overwrite = T)
  }
  
  
  try({df <- df %>%
    mutate(time = as.Date(date, "%d.%m.%Y")) %>% 
    arrange(time) %>%
    rowwise() %>%
    mutate(cleanText = cleanNZZ(text, stopwds = stopw_de)) %>%
    mutate(pos_n = sum(!is.na(match(pos_w$words, str_split(cleanText, pattern = "\\s+")[[1]]))))  %>%
    mutate(neg_n = sum(!is.na(match(neg_w$words, str_split(cleanText, pattern = "\\s+")[[1]])))) %>%
    mutate(n_w = length(str_split(cleanText, pattern = "\\s+")[[1]])) %>%
    mutate(SentimentScore = (pos_n-neg_n)/n_w) %>%
    as_tibble()
  
  df <- df[!duplicated(df),]
  
  df <- df %>% 
    group_by(time) %>%
    dplyr::summarize(mean = mean(SentimentScore, na.rm=TRUE),
                     sum = sum(n())) %>%
    filter(time > as.Date("1999-01-01") & time < Sys.Date())
  
  df_nzz_int <- bind_rows(df_nzz_int, df)},
  silent = T)
  
  save(df_nzz_ch, df_nzz_int, file = "../Data/News/nzz.RData")
  
}

update_fuw <- function(){
  load("../Data/News/fuw.RData")
  path <- "../Data/News/FUW"
  enddate <- Sys.Date()-1
  startdate <- max(df_fuw_int$time)+1
  # Makro
  system(paste0('curl "https://www.fuw.ch/wp-content/plugins/fuw-list/api/ajax.php" -H "User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:75.0) Gecko/20100101 Firefox/75.0" -H "Accept: */*" -H "Accept-Language: de,en-US;q=0.7,en;q=0.3" -H "Content-Type: application/x-www-form-urlencoded; charset=UTF-8" -H "X-Requested-With: XMLHttpRequest" -H "Origin: https://www.fuw.ch" -H "Connection: keep-alive" -H "Referer: https://www.fuw.ch/markte/makro/" -H "Cookie: __cfduid=d8695fc49dc4fa7cb3042d9e31b42d4ff1588857885; fuwStats2020=45A5QbY4rzxxU; POPUPCHECK=1588944296151; _ga=GA1.2.1748096360.1588857897; _gid=GA1.2.874599476.1588857897; _gat_main=1; _gat_g=1; _gat_h=1; _gcl_au=1.1.255979795.1588857898; _gat_UA-58327930-30=1; _fbp=fb.1.1588857898454.1896769323; _parsely_session={"%"22sid"%"22:1"%"2C"%"22surl"%"22:"%"22https://www.fuw.ch/"%"22"%"2C"%"22sref"%"22:"%"22https://www.google.com/"%"22"%"2C"%"22sts"%"22:1588857899212"%"2C"%"22slts"%"22:0}; dakt_2_uuid=71a0d9a81ee50f88734c66be98d23a1e; dakt_2_uuid_ts=1588857899328; dakt_2_session_id=367922baff27ee84ac6e40f4ce0ce5dc; _parsely_visitor={"%"22id"%"22:"%"22pid=8a2cb26c1f41d7b807d841ad77d88f9b"%"22"%"2C"%"22session_count"%"22:1"%"2C"%"22last_session_ts"%"22:1588857899212}; __gads=ID=64e2249ac7a0219b:T=1588857897:S=ALNI_Ma_DcuF0BRgdFUd9ot7LGh6ZZUpJg" -H "TE: Trailers" --data "query=category&id=33&offset=0&count=100&excludeCategory"%"5B"%"5D=1229&listId=list-5eb40c2a14444579&listStart=0&listOrderBy=date&listIncludeDraftsAsPreview=0&listTemplate=default&listPage=1&listPages=2&listMoreButton=1&listMoreAutoload=0&listMoreLink=&articleDate=1&articleTime=0&articleTimeTodayFormat=0&articleHighlightDate=0&articleCategories=1&articleCategoriesLinked=1&articleBookmark=1&articleImage=0&articleKicker=1&articleLead=1&articleAuthor=1&articleRanking=1&articleTeaserMarkerDisplay"%"5B"%"5D=7&articleTeaserMarkerDisplay"%"5B"%"5D=8&articleTeaserMarkerDisplay"%"5B"%"5D=9&articleTags=1&articleLinkToBlank=1&articleLinkFreeKey=0&amp=0&jsonLd=0&insertAds=0" -o ',path,'/makro_',enddate,'.txt'))
  Sys.sleep(3)
  
  
  # Unternehmen
  system(paste0('curl "https://www.fuw.ch/wp-content/plugins/fuw-list/api/ajax.php" -H "User-Agent: Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:75.0) Gecko/20100101 Firefox/75.0" -H "Accept: */*" -H "Accept-Language: de,en-US;q=0.7,en;q=0.3" -H "Content-Type: application/x-www-form-urlencoded; charset=UTF-8" -H "X-Requested-With: XMLHttpRequest" -H "Origin: https://www.fuw.ch" -H "Connection: keep-alive" -H "Referer: https://www.fuw.ch/unternehmen/" -H "Cookie: __cfduid=d8695fc49dc4fa7cb3042d9e31b42d4ff1588857885; fuwStats2020=45A5QbY4rzxxU; POPUPCHECK=1588944296151; _ga=GA1.2.1748096360.1588857897; _gid=GA1.2.874599476.1588857897; _gcl_au=1.1.255979795.1588857898; _fbp=fb.1.1588857898454.1896769323; dakt_2_uuid=71a0d9a81ee50f88734c66be98d23a1e; dakt_2_uuid_ts=1588857899328; _parsely_visitor={"%"22id"%"22:"%"22pid=8a2cb26c1f41d7b807d841ad77d88f9b"%"22"%"2C"%"22session_count"%"22:2"%"2C"%"22last_session_ts"%"22:1588859745035}; __gads=ID=64e2249ac7a0219b:T=1588857897:S=ALNI_Ma_DcuF0BRgdFUd9ot7LGh6ZZUpJg; _parsely_session={"%"22sid"%"22:2"%"2C"%"22surl"%"22:"%"22https://www.fuw.ch/markte/makro/"%"22"%"2C"%"22sref"%"22:"%"22https://www.fuw.ch/markte/"%"22"%"2C"%"22sts"%"22:1588859745035"%"2C"%"22slts"%"22:1588857899212}; _gat_main=1; _gat_UA-58327930-30=1; _gat_g=1; _gat_h=1; dakt_2_session_id=eb9218ff7a6afeeebb53f87066e2bb9f" -H "TE: Trailers" --data "query=category&id=17&offset=0&count=100&excludeCategory"%"5B"%"5D=1229&listId=list-5eb4137c44993298&listStart=0&listOrderBy=date&listIncludeDraftsAsPreview=0&listTemplate=default&listPage=1&listPages=2&listMoreButton=1&listMoreAutoload=0&listMoreLink=&articleDate=1&articleTime=0&articleTimeTodayFormat=0&articleHighlightDate=0&articleCategories=1&articleCategoriesLinked=1&articleBookmark=1&articleImage=1&articleKicker=1&articleLead=1&articleAuthor=1&articleRanking=1&articleTeaserMarkerDisplay"%"5B"%"5D=7&articleTeaserMarkerDisplay"%"5B"%"5D=8&articleTeaserMarkerDisplay"%"5B"%"5D=9&articleTags=1&articleLinkToBlank=1&articleLinkFreeKey=0&amp=0&jsonLd=0&insertAds=0" -o ',path,'/unternehmen_',enddate,'.txt'))
  Sys.sleep(3)
  
  # Load german stopwords
  stopw_de <- read.delim("../Data/News/Stopwords/stopwords_de.txt", fileEncoding = 'WINDOWS-1252', header=F, stringsAsFactors = F)
  colnames(stopw_de) <- "words"
  
  senti <- read_sentiws()
  # get positive and negative words of lexicon
  pos_w <-  filter(senti, value >= 0)
  neg_w <-  filter(senti, value <= 0)
  neg_w <- neg_w %>% # add some custom negative words, set value to same as e.g epidemie or krise
    add_row("words" = "corona", "value" = -0.0048, "class" = "NN", "pol" = "neg") %>% 
    add_row("words" = "pandemie", "value" = -0.0048, "class" = "NN", "pol" = "neg") %>% 
    add_row("words" = "kurzarbeit", "value" = -0.0048, "class" = "NN", "pol" = "neg")
  
  
  filelist <- list.files(path, pattern = ".txt")
  
  df<-tibble()
  
  for (file in filelist) { # for every text file in the folder  
    
    html <- read_html(paste0(path,"/", file), encoding = "UTF-8") 
    
    li_nodes <- html %>% html_nodes(".teaser-list__item ")
    for (li in li_nodes){
      row <- tibble(
        time = li %>% html_nodes(".teaser__time") %>% html_text(),
        category = li %>% html_nodes(".teaser__category a") %>% html_text() %>% paste(collapse = ", "),
        title = li %>% html_nodes(".teaser__title a") %>% html_text(),
        text = li %>% html_nodes(".teaser__lead a") %>% html_text(),
        tags = li %>% html_nodes(".teaser__tags span") %>% html_text() %>% paste(collapse = ", ")
      )
      
      # Clean the text
      title <- row$title
      title <- gsub("</?[^>]+>", " ", title) # remove html tags
      title <- gsub("[[:punct:]]", " ", title) #remove puctuation
      title <- gsub("[0-9]", " ", title) # remove numbers
      txt <- row$text
      txt <- gsub("</?[^>]+>", " ", txt) # remove html tags
      txt <- gsub("[[:punct:]]", " ", txt) #remove puctuation
      txt <- gsub("[0-9]", " ", txt) # remove numbers
      txt <- paste(title,txt, collapse = " ")
      txt <- tolower(txt) # set to lower case
      txt <- removeWords(txt, c(stopw_de$words, stopwords("deu"))) # remove stopwords
      # txt <- gsub(pattern = "\\b[A-Za-z]\\b{1}", replace = " ", txt)
      txt <- stripWhitespace(txt) # remove whitespace > 1
      row["cleanText"] <- txt # append clean text to main df
      
      t_bag <- str_split(txt, pattern = "\\s+")
      
      # Calculate Sentiment Score
      pos_n <- sum(!is.na(match(pos_w$words, t_bag[[1]])))
      neg_n <- sum(!is.na(match(neg_w$words, t_bag[[1]])))
      n_w <- length(t_bag[[1]])
      score <- (pos_n-neg_n)/n_w
      
      # append to main df
      row["NumPosW"] <- pos_n
      row["NumNegW"] <- neg_n
      row["NumW"] <- n_w
      row["SentimentScore"] <- score
      
      df <- bind_rows(df, row)
    }
    file.move(paste0(path, "/", file), "../Data/News/FUW/_archive", overwrite = T)
  }
  
  df <- df %>% 
    select(c(time, category, tags, cleanText, SentimentScore)) %>%
    mutate(time = as.Date(time, "%d.%m.%Y")) %>% arrange(time)
  
  df_fuw_ch_new <- df %>% 
    filter((grepl("schweiz", tolower(category))  | grepl("schweiz", tolower(tags)) | grepl("schweiz", cleanText)))  
  df_fuw_ch_new <- df_fuw_ch_new[!duplicated(df_fuw_ch_new),] %>%
    select(c("time", "SentimentScore")) %>% na.omit()
  
  
  df_fuw_int_new <- df %>% 
    filter((grepl("ausland", tolower(category)) | grepl("ausland|\\beu\\eu|euro|usa|amerika|deutsch", tolower(tags)) | grepl("ausland|\\beu\\b|euro|usa|amerika|deutsch", cleanText)))   
  df_fuw_int_new <- df_fuw_int_new[!duplicated(df_fuw_int_new),] %>%
    select(c("time", "SentimentScore")) %>% na.omit()
  
  df_fuw_ch_new <- df_fuw_ch_new %>%
    group_by(time) %>%
    dplyr::summarize(mean = mean(SentimentScore, na.rm=TRUE),
                     sum = sum(n())) %>%
    filter(time >= startdate & time <= enddate)
  
  df_fuw_int_new <- df_fuw_int_new %>%
    group_by(time) %>%
    dplyr::summarize(mean = mean(SentimentScore, na.rm=TRUE),
                     sum = sum(n())) %>%
    filter(time >= as.Date(startdate) & time <= as.Date(enddate))
  
  df_fuw_ch <- bind_rows(df_fuw_ch, df_fuw_ch_new)
  df_fuw_int <- bind_rows(df_fuw_int, df_fuw_int_new)
  
  save(df_fuw_ch, df_fuw_int, file = "../Data/News/fuw.RData")
  
}



cleanTA <- function(title, txt, stopwds) {
  # Clean the text
  title <- gsub("</?[^>]+>", " ", title) # remove html tags
  title <- gsub("[[:punct:]]", " ", title) #remove puctuation
  title <- gsub("[0-9]", " ", title) # remove numbers
  txt <- gsub("</?[^>]+>", " ", txt) # remove html tags
  txt <- gsub("[[:punct:]]", " ", txt) #remove puctuation
  txt <- gsub("[0-9]", " ", txt) # remove numbers
  txt <- stripWhitespace(txt) # remove whitespace > 1
  #txt <- str_split(txt, pattern = "\\s+")
  txt <- paste(title,paste(txt, collapse = " "), collapse = " ")
  txt <- tolower(txt) # set to lower case
  txt <- removeWords(txt, c(stopwds$words, stopwords("deu"))) # remove stopwords
  # txt <- gsub(pattern = "\\b[A-Za-z]\\b{1}", replace = " ", txt)
  txt <- stripWhitespace(txt) # remove whitespace > 1
  txt # append clean text to main df
}

cleanNZZ <- function(txt, stopwds) {
  # Clean the text
  txt <- gsub("</?[^>]+>", " ", txt) # remove html tags
  txt <- gsub("[[:punct:]]", " ", txt) #remove puctuation
  txt <- gsub("-", " ", txt)
  txt <- gsub("[0-9]", " ", txt) # remove numbers
  #txt <- str_split(txt, pattern = "\\s+")
  txt <- tolower(txt) # set to lower case
  txt <- removeWords(txt, c(stopwds$words, stopwords("deu"))) # remove stopwords
  # txt <- gsub(pattern = "\\b[A-Za-z]\\b{1}", replace = " ", txt)
  txt <- stripWhitespace(txt) # remove whitespace > 1
  txt # append clean text to main df
}

merge_news <- function() {
  load("../Data/News/ta.RData")
  load("../Data/News/nzz.RData")
  load("../Data/News/fuw.RData")
  
  
  df_all_ch <- full_join(df_fuw_ch, df_nzz_ch, by="time") %>%
    full_join(df_ta_ch, by="time") %>%
    mutate(tSum = rowSums(.[names(.)[c(3,5,7)]], na.rm = TRUE))  %>%
    mutate(tM1 = mean*sum) %>%
    mutate(tM2 = mean.x*sum.x) %>%
    mutate(tM3 = mean.y*sum.y)  %>%
    mutate(tMeanS = rowSums(.[names(.)[c(9,10,11)]], na.rm = TRUE)) %>%
    mutate(tMean = tMeanS/tSum ) %>%
    select(time, tMean, tSum)
  
  colnames(df_all_ch) <- c("time", "mean", "sum")
  
  df_all_int <- full_join(df_fuw_int, df_nzz_int, by="time") %>%
    full_join(df_ta_int, by="time") %>%
    mutate(tSum = rowSums(.[names(.)[c(3,5,7)]], na.rm = TRUE))  %>%
    mutate(tM1 = mean*sum) %>%
    mutate(tM2 = mean.x*sum.x) %>%
    mutate(tM3 = mean.y*sum.y)  %>%
    mutate(tMeanS = rowSums(.[names(.)[c(9,10,11)]], na.rm = TRUE)) %>%
    mutate(tMean = tMeanS/tSum ) %>%
    select(time, tMean, tSum)
  
  colnames(df_all_int) <- c("time", "mean", "sum")
  
  save(df_all_ch, df_all_int, file =  "../Data/News/all.RData")
  
}


updateNewsIndicator <- function() {
  # Update News
  
  # Update News via WebScarping (Slow and not very stable)
  # Works on Windows with Python and a set of installations
  
  # TA
  enddate <- Sys.Date() -1
  load("../Data/News/ta.RData")
  
  # Domestic:
  startdate <- max(df_ta_ch$time)+1
  searchkeys_ta_ch <- c("wirtschaft schweiz", "konjunktur schweiz", "rezession schweiz")
  for (searchkey in searchkeys_ta_ch) {
    # Run Python Script with input arguments
    system(paste0('python  ..\\Data\\News\\tagi_args.py -k "', searchkey,'" -s "',startdate , '" -e "', enddate , '"'))
  }
  
  #Foreign:
  startdate <- max(df_ta_int$time)+1
  searchkeys_ta_for <- c("wirtschaft ausland", "wirtschaft eu", "wirtschaft euro" , "wirtschaft europa", "wirtschaft deutschland", "wirtschaft usa", "wirtschaft us", "wirtschaft amerika", "konjunktur ausland", "konjunktur eu", "konjunktur euro" , "konjunktur europa", "konjunktur deutschland", "konjunktur usa", "konjunktur us", "konjunktur amerika", "rezession ausland", "rezession eu", "rezession euro" , "rezession europa", "rezession deutschland", "rezession usa", "rezession us", "rezession amerika")
  for (searchkey in searchkeys_ta_for) {
    # Run Python Script with input arguments
    system(paste0('python  ..\\Data\\News\\tagi_args.py -k "', searchkey,'" -s "',startdate , '" -e "', enddate , '"'))
  }
  
  # Update .RData files with recently downloaded News
  Sys.sleep(2)
  update_ta()
  Sys.sleep(2)
  
  # NZZ
  enddate <- Sys.Date() -1
  load("../Data/News/nzz.RData")
  
  # Domestic:
  startdate <- max(df_nzz_ch$time)+1
  searchkeys_nzz_ch <- c("wirtschaft* schweiz*", "konjunktur* schweiz*", "rezession* schweiz*")
  for (searchkey in searchkeys_nzz_ch) {
    # Run Python Script with input arguments
    system(paste0('python  ..\\Data\\News\\nzz_args.py -k "', searchkey,'" -s "',startdate , '" -e "', enddate , '"'))
  }
  
  #Foreign:
  startdate <- max(df_nzz_int$time)+1
  searchkeys_nzz_for <- c("wirtschaft* ausland", "wirtschaft* eu", "wirtschaft* euro*" , "wirtschaft* deutsch*", "wirtschaft* us*", "wirtschaft* amerika*", "konjunktur* ausland", "konjunktur* eu", "konjunktur* euro*" , "konjunktur* deutsch*", "konjunktur* us*", "konjunktur* amerika*", "rezession* ausland", "rezession* eu", "rezession* euro*" , "rezession* deutsch*", "rezession* us*", "rezession* amerika*")
  for (searchkey in searchkeys_nzz_for) {
    # Run Python Script with input arguments
    system(paste0('python  ..\\Data\\News\\nzz_args.py -k "', searchkey,'" -s "',startdate , '" -e "', enddate , '"'))
  }
  
  # Update .RData files with recently downloaded News
  Sys.sleep(2)
  update_nzz()
  Sys.sleep(2)
  
  # FUW
  update_fuw()
  Sys.sleep(2)
  
  # Merge to ALL
  merge_news()
}

read_sentiws <- function(){
  # Load Sentiment Lexicon
  
  #positive
  df_pos <- readLines("../Data/News/SentiWS_v2.0/SentiWS_v2.0_Positive.txt",
                      encoding = "UTF-8") %>%
    stringi::stri_replace_all("ss", fixed = "\u00DF") %>% # Replace "Esszett" with ss
    lapply(
      function(x) {
        # Extrahieren der einzelnen Spalten
        res <- strsplit(x, "\t|,|\\|")[[1]]
        if (length(res) > 3){
          wordlist <- c(res[1], res[4:length(res)])
        } else {
          wordlist <- res[1]
        }
        
        return(data.frame(words = tolower(wordlist), class = res[2], value = res[3], pol = "pos",
                          stringsAsFactors = FALSE))
      }
    )  %>%
    bind_rows() %>%
    mutate(value=as.numeric(value))
  
  #negative
  df_neg <- readLines("../Data/News/SentiWS_v2.0/SentiWS_v2.0_Negative.txt",
                      encoding = "UTF-8") %>%
    stringi::stri_replace_all("ss", fixed = "\u00DF") %>% # Replace "Esszet"
    lapply(
      function(x) {
        # Extrahieren der einzelnen Spalten
        res <- strsplit(x, "\t|,|\\|")[[1]]
        if (length(res) > 3){
          wordlist <- c(res[1], res[4:length(res)])
        } else {
          wordlist <- res[1]
        }
        
        return(data.frame(words = tolower(wordlist), class = res[2], value = res[3], pol = "neg",
                          stringsAsFactors = FALSE))
      }
    )  %>%
    bind_rows() %>%
    mutate(value=as.numeric(value))
  
  df <- bind_rows(df_pos, df_neg) %>%
    arrange(words) %>%
    # manche W?rter kommen doppelt vor, hier nehmen wir den mittleren Wert
    group_by(words) %>% 
    summarise(value = mean(value),
              class = ifelse(n()>1, ifelse(first(class) == nth(class,2), first(class), paste(class, collapse = "/")), first(class)), 
              pol = ifelse(n()>1, ifelse(first(pol) == nth(pol,2), first(pol), paste(pol, collapse = "/")), first(pol))) %>%
    ungroup
}


