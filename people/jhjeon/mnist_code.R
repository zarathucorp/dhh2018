library(keras)
library(mnist)
library(shiny)
library(png)

mnist <- dataset_mnist()
x_train <- mnist$train$x
y_train <- mnist$train$y
x_test <- mnist$test$x
y_test <- mnist$test$y

# reshape
x_train <- array_reshape(x_train, c(nrow(x_train), 784))
x_test <- array_reshape(x_test, c(nrow(x_test), 784))
# rescale
x_train <- x_train / 255
x_test <- x_test / 255
y_train <- to_categorical(y_train, 10)
y_test <- to_categorical(y_test, 10)

# Model definition
model <- keras_model_sequential() 
model %>% 
  layer_dense(units = 256, activation = 'relu', input_shape = c(784)) %>% 
  layer_dropout(rate = 0.4) %>% 
  layer_dense(units = 128, activation = 'relu') %>%
  layer_dropout(rate = 0.3) %>%
  layer_dense(units = 10, activation = 'softmax')

model %>% compile(
  loss = 'categorical_crossentropy',
  optimizer = optimizer_rmsprop(),
  metrics = c('accuracy')
)

# Training and evaluation
history <- model %>% fit(
  x_train, y_train, 
  epochs = 30, batch_size = 128, 
  validation_split = 0.2
)


model %>% evaluate(x_test, y_test)

x_new <- mnist$test$x
y_new <- mnist$test$y

matrix.rotate <- function(img) {
  t(apply(img, 2, rev))
}

training_img <-readPNG("C:/Users/user/Documents/hackathon/training.png")

#i = 4
#label <- y_new[i]
#image(matrix.rotate(x_new[i,,]), col = grey(level=seq(1, 0, by=-1/255)), axes=F, main=label)

#model %>% predict_classes(array(x_test[i,], dim=c(1,784)))

ui <- fluidPage(
  titlePanel("Classification with Deep learning"),
  sidebarLayout(
    sidebarPanel(
      selectInput("dataSelect", "Dataset Name", choices=list("mnist")),
      textInput("index", "Data file name : ", 'mnist_x_test1.jpg'),
      plotOutput(outputId = 'training')
    ),
    mainPanel(
      plotOutput(
        outputId = "image"
      ),
      textOutput("Prediction"),
      textOutput("number")
    )
  )
)
  server <- function(input, output){
  output$training <- renderPlot({plot(history)})
  output$image <- renderPlot({
    image(matrix.rotate(x_new[as.integer(str_extract(input$index,"[0-9]+")),,]), col = grey(level=seq(1, 0, by=-1/255)), axes=F)
  })
  output$Prediction <- renderText({"Classification of selected data with deep learning is"})
  output$number <- renderText({model %>% predict_classes(array(x_test[as.integer(str_extract(input$index,"[0-9]+")),], dim=c(1,784)))})
}

shinyApp(ui=ui,server=server)
