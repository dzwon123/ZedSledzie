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
  # powtórzona ocena krzyzowa
  method = "repeatedcv",
  # liczba podzialów
  number = 2,
  # liczba powtórzen
  repeats = 5)

set.seed(23)
fit <- train(Class ~ .,
             data = training,
             method = "rf",
             trControl = ctrl,
             # Paramter dla algorytmu uczacego
             ntree = 10)