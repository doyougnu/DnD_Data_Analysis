genBasicPlots <- function (df) {
  
  getData <- function() {
    #do this later
    #apparently there is a google Docs to R api somewhere to automate this even more
  }
  
  #function to load required libs
  loadPackages <- function() {
    tryCatch(
    {
      library(stringr)
      library(ggplot2)
      library(dplyr)
      library(xtable)
    },
    error = function(cond) {
    message("loading packages threw an error")
    message("here is the original error message: ")
    message(cond)
    stop("please install required packages with install.packages", call. = F)
    })
  }
  
  #load packages
  loadPackages()
  
  #deal with possible side effects
  df$Person <- df$Person %>%
    tolower() %>%
    str_trim()
  
  if (class(df$Date) != "Date")
  {
    df$Date <- as.Date(as.character(df$Date), format = "%m/%d/%Y")
  }
  
  #plot hist
  histPlot <- qplot(Roll_Value, data = df, fill = Person, facets = Date ~ Person, binwidth = 1/2)
  
  #plot KDE's
  dieKDE <- ggplot(df, aes(Roll_Value, fill=Person)) + 
    geom_density(alpha = 0.2) + 
    facet_grid(Date ~ Person) + 
    scale_x_continuous(c(1,20))
  
  #save plots
  ggsave(filename = "Die_Roll_Hist.png", plot = histPlot, width = 7, height = 5)
  ggsave(filename = "Die_Roll_Density.png", plot = dieKDE, width = 7, height = 5)
  
  #table and return frequency dataframe
  #write.csv is in tidy format, need to figure out a good table display maybe LaTeX??
  returnDF <- data.frame(table(df$Date, df$Person))
  write.csv(returnDF, "Die_Rolls_by_Person_Week.csv", row.names = FALSE)
}