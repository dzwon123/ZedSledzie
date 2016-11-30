library(dplyr)
library(ggplot2)
library(caret)
library(plotly)
sledzie<- read.csv("sledzie.csv")


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

#uzupelnienie wartosci zerowych
for(i in 1:length(sledzie$cfin1))
{
  if(is.na(sledzie$cfin1[i]))
  {
    if(!is.na(sledzie$cfin2[i+1])&!is.na(sledzie$cfin2[i]))
    {
      if(sledzie$cfin2[i]==sledzie$cfin2[i+1])
      {
        if(!is.na(sledzie$cfin1[i+1]))
        {
          sledzie$cfin1[i]<-sledzie$cfin1[i+1]
        }
        else
        {
          sledzie$cfin1[i]<-sledzie$cfin1[i-1]
        }
      }
      else
      {
        sledzie$cfin1[i]<-sledzie$cfin1[i-1]
      }
    }
    else
    {
      sledzie$cfin1[i]<-sledzie$cfin1[i-1]
    }
  }
  if(is.na(sledzie$cfin2[i]))
  {
    if(!is.na(sledzie$cfin1[i+1])&!is.na(sledzie$cfin1[i]))
    {
      if(sledzie$cfin1[i]==sledzie$cfin1[i+1])
      {
        if(!is.na(sledzie$cfin2[i+1]))
        {
          sledzie$cfin2[i]<-sledzie$cfin2[i+1]
        }
        else
        {
          sledzie$cfin2[i]<-sledzie$cfin2[i-1]
        }
      }
      else
      {
        sledzie$cfin2[i]<-sledzie$cfin2[i-1]
      }
    }
    else
    {
      sledzie$cfin2[i]<-sledzie$cfin2[i-1]
    }
  }
  if(is.na(sledzie$chel1[i]))
  {
    if(!is.na(sledzie$chel2[i+1])&!is.na(sledzie$chel2[i]))
    {
      if(sledzie$chel2[i]==sledzie$chel2[i+1])
      {
        if(!is.na(sledzie$chel1[i+1]))
        {
          sledzie$chel1[i]<-sledzie$chel1[i+1]
        }
        else
        {
          sledzie$chel1[i]<-sledzie$chel1[i-1]
        }
      }
      else
      {
        sledzie$chel1[i]<-sledzie$chel1[i-1]
      }
    }
    else
    {
      sledzie$chel1[i]<-sledzie$chel1[i-1]
    }
  }
  if(is.na(sledzie$chel2[i]))
  {
    if(!is.na(sledzie$chel1[i+1])&!is.na(sledzie$chel1[i]))
    {
      if(sledzie$chel1[i]==sledzie$chel1[i+1])
      {
        if(!is.na(sledzie$chel2[i+1]))
        {
          sledzie$chel2[i]<-sledzie$chel2[i+1]
        }
        else
        {
          sledzie$chel2[i]<-sledzie$chel2[i-1]
        }
      }
      else
      {
        sledzie$chel2[i]<-sledzie$chel2[i-1]
      }
    }
    else
    {
      sledzie$chel2[i]<-sledzie$chel2[i-1]
    }
    
  }
  if(is.na(sledzie$lcop1[i]))
  {
    if(!is.na(sledzie$lcop2[i+1])&!is.na(sledzie$lcop2[i]))
    {
      if(sledzie$lcop2[i]==sledzie$lcop2[i+1])
      {
        if(!is.na(sledzie$lcop1[i+1]))
        {
          sledzie$lcop1[i]<-sledzie$lcop1[i+1]
        }
        else
        {
          sledzie$lcop1[i]<-sledzie$lcop1[i-1]
        }
      }
      else
      {
        sledzie$lcop1[i]<-sledzie$lcop1[i-1]
      }
    }
    else
    {
      sledzie$lcop1[i]<-sledzie$lcop1[i-1]
    }
  }
  if(is.na(sledzie$lcop2[i]))
  {
    if(!is.na(sledzie$lcop2[i+1])&!is.na(sledzie$lcop2[i]))
    {
      if(sledzie$lcop1[i]==sledzie$lcop1[i+1])
      {
        sledzie$lcop2[i]<-sledzie$lcop2[i+1]
      }
      else
      {
        sledzie$lcop2[i]<-sledzie$lcop2[i-1]
      }
    }
    else
    {
      sledzie$lcop2[i]<-sledzie$lcop2[i-1]
    }
  }
  if(is.na(sledzie$fbar[i]))
  {
    if(sledzie$recr[i]==sledzie$recr[i+1])
    {
      sledzie$fbar[i]<-sledzie$fbar[i+1]
    }
    else
    {
      sledzie$fbar[i]<-sledzie$fbar[i-1]
    }
    
  }
  if(is.na(sledzie$recr[i]))
  {
    if(sledzie$fbar[i]==sledzie$fbar[i+1])
    {
      sledzie$recr[i]<-sledzie$recr[i+1]
    }
    else
    {
      sledzie$recr[i]<-sledzie$recr[i-1]
    }
  }
  if(is.na(sledzie$cumf[i]))
  {
    if(sledzie$totaln[i]==sledzie$totaln[i+1])
    {
      sledzie$cumf[i]<-sledzie$cumf[i+1]
    }
    else
    {
      sledzie$cumf[i]<-sledzie$cumf[i-1]
    }
    
  }
  if(is.na(sledzie$totaln[i]))
  {
    if(sledzie$cumf[i]==sledzie$cumf[i+1])
    {
      sledzie$totaln[i]<-sledzie$totaln[i+1]
    }
    else
    {
      sledzie$totaln[i]<-sledzie$totaln[i-1]
    }
    
  }
  if(is.na(sledzie$sst[i]))
  {
    if(sledzie$sal[i]==sledzie$sal[i+1])
    {
      if(!is.na(sledzie$sst[i+1]))
      {
        sledzie$sst[i]<-sledzie$sst[i+1]
      }
      else
      {
        sledzie$sst[i]<-sledzie$sst[i-1]
      }
    }
    else
    {
      sledzie$sst[i]<-sledzie$sst[i-1]
    }
    
  }
  if(is.na(sledzie$sal[i]))
  {
    if(sledzie$sst[i]==sledzie$sst[i+1])
    {
      sledzie$sal[i]<-sledzie$sal[i+1]
    }
    else
    {
      sledzie$sal[i]<-sledzie$sal[i-1]
    }
    
  }
}
sledzieGroupedBy<-sledzie%>%group_by(recr)%>%summarize(mean=mean(length))
p <- plot_ly(sledzieGroupedBy, y = ~mean, name = 'change length in time', type = 'scatter', mode = 'lines')

#funkcja normalizujaca dane
range01 <- function(x){(x-min(x))/(max(x)-min(x))}


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
sledzieTraining<-sledzie[inTraining,]
sledzieTest<-sledzie[-inTraining,]

#skalownaie zmiennych 

sledzieTraining$cfin1<-range01(sledzieTraining$cfin1)
sledzieTraining$cfin2<-range01(sledzieTraining$cfin2)
sledzieTraining$chel1<-range01(sledzieTraining$chel1)
sledzieTraining$chel2<-range01(sledzieTraining$chel2)
sledzieTraining$lcop1<-range01(sledzieTraining$lcop1)
sledzieTraining$lcop2<-range01(sledzieTraining$lcop2)
sledzieTraining$fbar<-range01(sledzieTraining$fbar)
sledzieTraining$recr<-range01(sledzieTraining$recr)
sledzieTraining$cumf<-range01(sledzieTraining$cumf)
sledzieTraining$sst<-range01(sledzieTraining$sst)
sledzieTraining$sal<-range01(sledzieTraining$sal)
sledzieTraining$nao<-range01(sledzieTraining$nao)

#zaokraglenie totaln do int

sledzieTraining$totaln<-as.integer(sledzieTraining$totaln)
sledzieTraining$totaln<-range01(sledzieTraining$totaln)

sledzieTraining<-sledzieTraining%>%select(length,cfin1,cfin2,lcop1,lcop2,fbar,recr,totaln,sst,sal,nao)

ctrl <- trainControl(
  # powtórzona ocena krzyzowa
  method = "repeatedcv",
  # liczba podzialów
  number = 2,
  # liczba powtórzen
  repeats = 2)

fit <- train(length ~ .,
             data = sledzieTraining,
             method = "rf",
             trControl = ctrl,
             importance =TRUE,
             ntree = 10)
prediction <- predict(fit, sledzieTest)
okPredicted <- sledzieTest[, "length"]
randomForest <- sqrt(mean((prediction-okPredicted)^2))