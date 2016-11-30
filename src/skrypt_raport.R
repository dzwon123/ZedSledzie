library(dplyr)
library(ggplot2)
library(caret)
library(plotly)
sledzie<- read.csv("sledzie.csv")

#podzial na zbiór uczacy i treningowy
set.seed(23)
inTraining <- 
  createDataPartition(
    # atrybut do stratyfikacji
    y = sledzie$length,
    # procent w zbiorze uczacym
    p = .75,
    # chcemy indeksy a nie liste
    list = FALSE)
#sledzie<-sledzie[inTraining,]
# wstawienie dodatkowej kolumny zawierajacej rok
sledzie<-sledzie%>%mutate(year=0)
gr<-sledzie%>%group_by(recr,xmonth)%>%summarize()
liczba<-0
for(i in 1:length(gr$xmonth))
{
  if(gr$xmonth[i]==6)
  {
    liczba<-liczba+1
  }
}

#zamiana '?' na wartosci NA 
matrix<-as.matrix(sledzie)
matrix[matrix=="?"]<-NA
sledzie <- as.data.frame(matrix)

#zamiana factorów na numerici i intager
sledzie$length<-as.numeric(as.character(sledzie$length))
sledzie$cfin1<-as.numeric(as.character(sledzie$cfin1))
sledzie$cfin2<-as.numeric(as.character(sledzie$cfin2))
sledzie$chel1<-as.numeric(as.character(sledzie$chel1))
sledzie$chel2<-as.numeric(as.character(sledzie$chel2))
sledzie$lcop1<-as.numeric(as.character(sledzie$lcop1))
sledzie$lcop2<-as.numeric(as.character(sledzie$lcop2))
sledzie$fbar<-as.numeric(as.character(sledzie$fbar))
sledzie$recr<-as.numeric(as.character(sledzie$recr))
sledzie$cumf<-as.numeric(as.character(sledzie$cumf))
sledzie$totaln<-as.numeric(as.character(sledzie$totaln))
sledzie$sst<-as.numeric(as.character(sledzie$sst))
sledzie$sal<-as.numeric(as.character(sledzie$sal))
sledzie$xmonth<-as.integer(as.character(sledzie$xmonth))
sledzie$nao<-as.numeric(as.character(sledzie$nao))

#wstawienie do pustcyh miejsc srednich z danego miesiaca danej wartosci
mean_cfin1<-sledzie%>%group_by(xmonth)%>%summarize(mean=mean(cfin1, na.rm=TRUE))
mean_cfin2<-sledzie%>%group_by(xmonth)%>%summarize(mean=mean(cfin2, na.rm=TRUE))
mean_chel1<-sledzie%>%group_by(xmonth)%>%summarize(mean=mean(chel1, na.rm=TRUE))
mean_chel2<-sledzie%>%group_by(xmonth)%>%summarize(mean=mean(chel2, na.rm=TRUE))
mean_lcop1<-sledzie%>%group_by(xmonth)%>%summarize(mean=mean(lcop1, na.rm=TRUE))
mean_lcop2<-sledzie%>%group_by(xmonth)%>%summarize(mean=mean(lcop2, na.rm=TRUE))
mean_fbar<-sledzie%>%group_by(xmonth)%>%summarize(mean=mean(fbar, na.rm=TRUE))
mean_recr<-sledzie%>%group_by(xmonth)%>%summarize(mean=mean(recr, na.rm=TRUE))
mean_cumf<-sledzie%>%group_by(xmonth)%>%summarize(mean=mean(cumf, na.rm=TRUE))
mean_totaln<-sledzie%>%group_by(xmonth)%>%summarize(mean=mean(totaln, na.rm=TRUE))
mean_sst<-sledzie%>%group_by(xmonth)%>%summarize(mean=mean(sst, na.rm=TRUE))
mean_sal<-sledzie%>%group_by(xmonth)%>%summarize(mean=mean(sal, na.rm=TRUE))
mean_nao<-sledzie%>%group_by(xmonth)%>%summarize(mean=mean(nao, na.rm=TRUE))

#uzupelnienie wartosci pustych wartosciami srednimi z danego miesiaca
FillNullValues<-function(column,data,meanTable)
{
  for(i in 1:length(column))
  {
    if(is.na(column[i]))
    {
      column[i] <- as.numeric(meanTable[data$xmonth[i],2])
      
    }
  }
  return(column)
}
sledzie$cfin1<-FillNullValues(sledzie$cfin1,sledzie,mean_cfin1)
sledzie$cfin2<-FillNullValues(sledzie$cfin2,sledzie,mean_cfin2)
sledzie$chel1<-FillNullValues(sledzie$chel1,sledzie,mean_chel1)
sledzie$chel2<-FillNullValues(sledzie$chel2,sledzie,mean_chel2)

sledzie$lcop1<-FillNullValues(sledzie$lcop1,sledzie,mean_lcop1)
sledzie$lcop2<-FillNullValues(sledzie$lcop2,sledzie,mean_lcop2)
sledzie$fbar<-FillNullValues(sledzie$fbar,sledzie,mean_fbar)
sledzie$recr<-FillNullValues(sledzie$recr,sledzie,mean_recr)
sledzie$cumf<-FillNullValues(sledzie$cumf,sledzie,mean_cumf)

sledzie$totaln<-FillNullValues(sledzie$totaln,sledzie,mean_totaln)
sledzie$sst<-FillNullValues(sledzie$sst,sledzie,mean_sst)
sledzie$sal<-FillNullValues(sledzie$sal,sledzie,mean_sal)
sledzie$nao<-FillNullValues(sledzie$nao,sledzie,mean_nao)
p <- plot_ly(sledzie, x = ~X, y = ~length, name = 'change length in time', type = 'scatter', mode = 'lines')
p
#sledzie<-sledzie%>%select(length,cfin1,cfin2,lcop1,lcop2,fbar,recr,totaln,sst,sal,nao)


