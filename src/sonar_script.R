library(caret)
set.seed(23)
inTraining <- 
  createDataPartition(
    # atrybut do stratyfikacji
    y = Sonar$Class,
    # procent w zbiorze uczacym
    p = .75,
    # chcemy indeksy a nie liste
    list = FALSE)

training <- Sonar[ inTraining,]
testing  <- Sonar[-inTraining,]

ctrl <- trainControl(
  # powt�rzona ocena krzyzowa
  method = "repeatedcv",
  # liczba podzial�w
  number = 2,
  # liczba powt�rzen
  repeats = 5)

set.seed(23)
fit <- train(Class ~ .,
             data = training,
             method = "rf",
             trControl = ctrl,
             # Paramter dla algorytmu uczacego
             ntree = 10)