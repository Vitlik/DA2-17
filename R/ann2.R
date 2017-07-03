setwd("C:/dev/DA2/DA2-17/")
load("data/colorHistOriginal.rda")
load("data/classesOrig.rda")

library(keras)

train_x2 <- colorHistOriginal[0:2120, ]
test_x2 <- colorHistOriginal[2121:2650, ]

train_y2 <- classesOrig[0:2120, ]
test_y2 <- classesOrig[2121:2650,]

model %>%
  layer_dense(units = 5, activation = 'relu', input_shape= c(48)) %>%
  layer_dropout(rate = 0.2) %>%
  layer_dense(units = 3, activation = 'softmax')

summary(model)

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_sgd(lr = 0.02),
  metrics = c('accuracy')
)

history <- model %>% fit(
  train_x2, train_y2, epochs = 3, batch_size = 106, validation_split = 0.2
)




model <- keras_model_sequential()

model %>%
  layer_dense(units = 3, input_shape = 3) %>%
  layer_dropout(rate=0.4) %>%
  layer_activation(activation = 'relu') %>%
  layer_dense(units = 3) %>%
  layer_activation(activation = 'softmax')

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = 'adam',
  metrics = c('accuracy')
)

model %>% fit(train_x2, train_y2, epochs = 3, batch_size = 106)

loss_and_metrics <- model %>% evaluate(test_x2, test_y2, batch_size = 106)