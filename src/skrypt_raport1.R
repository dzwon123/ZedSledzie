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
#sledzie<-sledzie%>%mutate(year=0)

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

for(i in 1:length(sledzie$cfin1))
{
  if(is.na(sledzie$cfin1[i]))
  {
    cf1<-sledzie%>%filter((cfin2==sledzie$cfin2[i]|chel1==sledzie$chel1[i]|chel2==sledzie$chel2[i])&xmonth==sledzie$xmonth[i])
    index<-1
    for(k in 1:length(cf1$cfin1))
    {
      if(!is.na(cf1$cfin1[k]))
      {
        index<-k
        break
      }
    }
    sledzie$cfin1[i]<-cf1$cfin1[index]
  }
  if(is.na(sledzie$cfin2[i]))
  {
    cf2<-sledzie%>%filter(cfin1==sledzie$cfin1&xmonth==sledzie$xmonth[i])
    index<-1
    for(k in 1:length(cf2$cfin2))
    {
      if(!is.na(cf2$cfin2[k]))
      {
        index<-k
        break
      }
    }
    sledzie$cfin2[i]<-cf2$cfin2[index]
  }
  if(is.na(sledzie$chel1[i]))
  {
    ch1<-sledzie%>%filter((chel2==sledzie$chel2[i]|fbar==sledzie$fbar[i])&xmonth==sledzie$xmonth[i])
    index<-1
    if(length(ch1$chel1)==0)
    {
      print(length(ch1$chel1))
    }
    for(k in 1:length(ch1$chel1))
    {
      if(!is.na(ch1$chel1[k]))
      {
        index<-k
        break
      }
    }
    sledzie$chel1[i]<-ch1$chel1[index]
  }
  if(is.na(sledzie$chel2[i]))
  {
    ch2<-sledzie%>%filter(chel1==sledzie$chel1[i]&xmonth==sledzie$xmonth[i])
    index<-1
    for(k in 1:length(ch2$chel2))
    {
      if(!is.na(ch2$chel2[k]))
      {
        index<-k
        break
      }
    }
    sledzie$chel2[i]<-ch2$chel2[index]
  }
  if(is.na(sledzie$lcop1[i]))
  {
    l1<-sledzie%>%filter((lcop2==sledzie$lcop2[i]|fbar==sledzie$fbar[i])&xmonth==sledzie$xmonth[i])
    index<-1
    for(k in 1:length(l1$lcop1))
    {
      if(!is.na(l1$lcop1[k]))
      {
        index<-k
        break
      }
    }
    sledzie$lcop1[i]<-l1$lcop1[index]
  }
  if(is.na(sledzie$lcop2[i]))
  {
    l2<-sledzie%>%filter(lcop1==sledzie$lcop1[i]&xmonth==sledzie$xmonth[i])
    index<-1
    for(k in 1:length(l2$lcop2))
    {
      if(!is.na(l2$lcop2[k]))
      {
        index<-k
        break
      }
    }
    sledzie$lcop2[i]<-l2$lcop2[index]
  }
  if(is.na(sledzie$fbar[i]))
  {
    f<-sledzie%>%filter(recr==sledzie$recr[i]&xmonth==sledzie$xmonth[i])
    index<-1
    for(k in 1:length(f$fbar))
    {
      if(!is.na(f$lfbar[k]))
      {
        index<-k
        break
      }
    }
    sledzie$fbar[i]<-f$fbar[index]
  }
  if(is.na(sledzie$recr[i]))
  {
    r<-sledzie%>%filter(fbar==sledzie$fbar[i]&xmonth==sledzie$xmonth[i])
    index<-1
    for(k in 1:length(r$recr))
    {
      if(!is.na(r$recr[k]))
      {
        index<-k
        break
      }
    }
    sledzie$recr[i]<-r$recr[index]
  }
  if(is.na(sledzie$cumf[i]))
  {
    cum<-sledzie%>%filter(totaln==sledzie$totaln[i]&xmonth==sledzie$xmonth[i])
    index<-1
    for(k in 1:length(cum$cumf))
    {
      if(!is.na(cum$cumf[k]))
      {
        index<-k
        break
      }
    }
    sledzie$cumf[i]<-c$cumf[index]
  }
  if(is.na(sledzie$totaln[i]))
  {
    tot<-sledzie%>%filter(cumf==sledzie$cumf[i]&xmonth==sledzie$xmonth[i])
    index<-1
    for(k in 1:length(tot$totaln))
    {
      if(!is.na(tot$totaln[k]))
      {
        index<-k
        break
      }
    }
    sledzie$totaln[i]<-t$totaln[index]
  }
  if(is.na(sledzie$sst[i]))
  {
    s<-sledzie%>%filter(sal==sledzie$sal[i]&xmonth==sledzie$xmonth[i])
    index<-1
    for(k in 1:length(s$sst))
    {
      if(!is.na(s$sst[k]))
      {
        index<-k
        break
      }
    }
    sledzie$sst[i]<-s$sst[index]
  }
  if(is.na(sledzie$sal[i]))
  {
    sa<-sledzie%>%filter(sst==sledzie$sst[i]&xmonth==sledzie$xmonth[i])
    index<-1
    for(k in 1:length(sa$sal))
    {
      if(!is.na(s$sal[k]))
      {
        index<-k
        break
      }
    }
    sledzie$sal[i]<-sa$sal[index]
  }
}



