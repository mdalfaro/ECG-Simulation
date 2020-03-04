# Libraries
library(shiny)
library(dplyr)

# Set working directory
#setwd("/Users/maxalfaro/Desktop/next/projects/ecg_simulation/bio project/2017/")

ui <- fluidPage(
  
  titlePanel("Fit a classic ECG Curve with a series of Gaussian Curves"),
  
  plotOutput("ecg_fitter"),
  
  fluidRow(
    column(1),
    column(2,
      h4("Curve #1"),
      numericInput(inputId = "curve_1_amplitude", label = "Amplitude", value = .5, step = 0.1),
      numericInput(inputId = "curve_1_position", label = "Position", min = 0, max = 1.5, value = .75, step = 0.01),
      numericInput(inputId = "curve_1_width", label = "Width", min = 0.001, max = 2, value = 0.01, step = 0.01)
    ),
    column(2,
      h4("Curve #2"),
      numericInput(inputId = "curve_2_amplitude", label = "Amplitude", value = .5, step = 0.1),
      numericInput(inputId = "curve_2_position", label = "Position", min = 0, max = 1.5, value = .75, step = 0.01),
      numericInput(inputId = "curve_2_width", label = "Width", min = 0.001, max = 2, value = 0.01, step = 0.01)
    ),
    column(2,
      h4("Curve #3"),
      numericInput(inputId = "curve_3_amplitude", label = "Amplitude", value = .5, step = 0.1),
      numericInput(inputId = "curve_3_position", label = "Position", min = 0, max = 1.5, value = .75, step = 0.01),
      numericInput(inputId = "curve_3_width", label = "Width", min = 0.001, max = 2, value = 0.01, step = 0.01)
    ),
    column(2,
      h4("Curve #4"),
      numericInput(inputId = "curve_4_amplitude", label = "Amplitude", value = .5, step = 0.1),
      numericInput(inputId = "curve_4_position", label = "Position", min = 0, max = 1.5, value = .75, step = 0.01),
      numericInput(inputId = "curve_4_width", label = "Width", min = 0.001, max = 2, value = 0.01, step = 0.01)
    ),
    column(2,
      h4("Curve #5"),
      numericInput(inputId = "curve_5_amplitude", label = "Amplitude", value = .5, step = 0.1),
      numericInput(inputId = "curve_5_position", label = "Position", min = 0, max = 1.5, value = .75, step = 0.01),
      numericInput(inputId = "curve_5_width", label = "Width", min = 0.001, max = 2, value = 0.01, step = 0.01)
    )
    
  )
)
server <- function(input, output) {
  output$ecg_fitter <- renderPlot({
    # Read data (from Rushabh)
    ecg_data <- read.csv("ecg_real_sample.csv")
    
    ecg_data <- ecg_data
    ecg_data$x <- (ecg_data$x - min(ecg_data$x))/diff(range(ecg_data$x)) # scale to range [0,1]
    ecg_data$y <- ecg_data$y - median(ecg_data$y)  # bring baseline to 0
    ecg_data$y <- ecg_data$y / max(ecg_data$y) * 3 # scale mV between 0 & 3 
  
    # We use the function peak() to define our own bell curve function
    # Motivation: Define our own bell curve function instead of dnorm so that we don't have to fight the function always trying to integrate to 0. 
    peak <- function(x, amplitude = 1, position = 50, width = 1) {   
      amplitude * exp(-((x - position)^2)/(2*width^2))
    }
    
    # x value range
    x <- seq(from = min(ecg_data$x), to = max(ecg_data$x), length = length(ecg_data$x))
    
    # Input as list
    uinput <- sapply(reactiveValuesToList(input), as.numeric)
  
    # Function that creates user-defined curve
    newCurveFunc <- function(param_vector, time_vector){
      normal1 <- peak(time_vector,
                      amplitude = param_vector["curve_1_amplitude"],
                      position = param_vector["curve_1_position"],
                      width = param_vector["curve_1_width"])
      normal2 <- peak(time_vector,
                      amplitude = param_vector["curve_2_amplitude"],
                      position = param_vector["curve_2_position"],
                      width = param_vector["curve_2_width"])
      normal3 <- peak(time_vector,
                      amplitude = param_vector["curve_3_amplitude"],
                      position = param_vector["curve_3_position"],
                      width = param_vector["curve_3_width"])
      normal4 <- peak(time_vector,
                      amplitude = param_vector["curve_4_amplitude"],
                      position = param_vector["curve_4_position"],
                      width = param_vector["curve_4_width"])
      normal5 <- peak(time_vector,
                      amplitude = param_vector["curve_5_amplitude"],
                      position = param_vector["curve_5_position"],
                      width = param_vector["curve_5_width"])
      return(normal1 + normal2 + normal3 + normal4 + normal5)
    }
    
    # Error.............................................
    
    # Encapsulate error score into function
    	error_score <- function(guess, target){
  	    # this "newCurve" is what changes every time the manual parameters are adjusted
  	    newCurve <- newCurveFunc(param_vector = guess, time_vector = target$x)
  	    diff <- (target$y - newCurve) ^ 2
      	rmse <- sqrt(mean(diff))
      	return(rmse)
    	}
    
    # Optimizer..........................................
    	
    # Tune parameters with optimizer
    control_params <- list(maxit=1e5)
    fit <- optim(par = uinput, fn = error_score, target=ecg_data, control=control_params)
      
    # Plot fitted data
    x <- ecg_data$x
    with(ecg_data, plot(x, y))
    
    # Curve fitted by user
    lines(x, newCurveFunc(uinput, x), col="blue")
    # Curve fitted by optim()
    lines(x, newCurveFunc(fit$par, x), col="red")
    
    # Plotting..........................................
    
    # Plot real ecg curve (goal) & user-defined curve (guess) & optimizer 
      real_x <- ecg_data$x
      real_y <- ecg_data$y
  
      user_y <- newCurveFunc(param_vector = uinput, time_vector = ecg_data$x)
      optim_y <- newCurveFunc(param_vector = fit$par, time_vector = x)
      
    matplot(cbind(real_x, x, x), cbind(real_y, user_y, optim_y), col = cbind("black", "blue", "green"), xlab = "Time / Second", ylab = "Surface potential / mV", pch = ".")
    lines(real_x, real_y, col = 'black', lwd=2)
    lines(x, user_y, col = 'blue')
    lines(x, optim_y, col = "darkgreen")
    
    userError <- round(error_score(guess = uinput, target = ecg_data), 5)
    optimError <- round(error_score(guess = fit$par, target = ecg_data), 5)
    
    # Print error to Shiny app
    mtext(paste("User RMSE =", userError), side=3, line=1, col='blue')
    mtext(paste("Optimizer RMSE =", optimError), side=3, line=2, col='darkgreen')
  })
  
}
