#******************************************************************************
#
# plots.R
# Plots forecast results using ggvis
#
#******************************************************************************

# Plots Actual Release Date vs Forecasted Date for TFDEA & LR
# Parameters:
# lr      -> linear regression results
# tfdea   -> tfdea results
# dea     -> dea results
plot.result <- function(lr, tfdea, dea, mdea) {
  
  if (length(tfdea) != 0) {
    print("Inside TFDEA section")
    # return(NULL)
  
    # Isolate ensures that the plot does not change when frontier date changes
    isolate({
      front.date <- get.front.date()
      # Only select linear regression results after frontier.date
      forecast.b <- which(lr$forecast[, "release.date"] > front.date)
    })
    
    # If there are no linear regression results, replace with NA
    lr.forecast <- rep(NA, length(forecast.b))
    if (length(lr) != 0)                               
      lr.forecast <- lr$forecast[forecast.b, "forecasted.date"]
    
    # Create a dataframe with all the data to be plotted
    plot.data <- data.frame(rownames(tfdea$forecast)[forecast.b], 
                            tfdea$forecast[forecast.b, c("release.date","forecasted.date")], 
                            lr.forecast)
    names(plot.data) <- c("dmu", "actual", "tfdea.forecast", "lr.forecast")
    
    # Specify the ideal forecast line and bind to plot data data.frame
    min.axis <- min(plot.data[,-1], na.rm = TRUE) - 2
    max.axis <- max(plot.data[,-1], na.rm = TRUE) + 2
    ticks <- round((max.axis - min.axis)/10)
    line.xy <- seq(min.axis, max.axis, length.out = nrow(plot.data))
    plot.data <- cbind(plot.data, line.xy) 
    
    # Create ggvis plot
    p <- plot.data %>% ggvis() %>%
         layer_points(x = ~actual, y = ~tfdea.forecast, fill = "TFDEA") %>%
         layer_paths(x = ~line.xy, y = ~line.xy, stroke := "red") %>%
         add_tooltip(function(df) paste("DMU:", df$dmu, "Actual:", df$actual, "Forecast:")) %>%
         add_legend("fill", title = "Method") %>%
         add_axis("x", title = "Actual Release Date", title_offset = 50, 
                  values = seq(min.axis, max.axis, by = ticks)) %>%
         add_axis("y", title = "Predicted Release Date", title_offset = 50, 
                  values = seq(min.axis, max.axis, by = ticks))
    
    # Binary value specifying whether to include linear regressions results in plot
    if (get.plot.lr()) {
      p <- p %>% layer_points(x = ~actual, y = ~lr.forecast, fill = "LR")
    }
      
    # Binary value specifying whether to include labels in plot
    if (get.plot.label()) {
        p <- p %>% layer_text(x = ~actual, y = ~tfdea.forecast, text := ~dmu)
        if (get.plot.lr())
          p <- p %>% layer_text(x = ~actual, y = ~lr.forecast, text := ~dmu)
    }
    # print("TFDEA plot.data")
    # print(plot.data)
  }
  else if(length(dea) != 0) {
      print("DEA results non-empty")
      eff <- round(dea$eff, digits = 3)

      # Create a dataframe with all the data to be plotted
      plot.data <- data.frame(rownames(data.frame(eff)), eff)
      names(plot.data) <- c("dmu", "efficiency")
      # print("DEA plot print")
      # print(plot.data)
      
      p <- plot.data %>% ggvis(~dmu, ~efficiency) %>%
        layer_points() %>%
        layer_text(x = ~dmu, y = ~efficiency, text := ~efficiency)


  }
  else if(length(mdea) != 0) {
    print("MULTIPLIER_DEA results non-empty")
    eff <- round(mdea$Efficiency, digits = 3)
    
    # Create a dataframe with all the data to be plotted
    plot.data <- data.frame(rownames(data.frame(eff)), eff)
    names(plot.data) <- c("dmu", "efficiency")
    # print("DEA plot print")
    # print(plot.data)
    
    p <- plot.data %>% ggvis(~dmu, ~efficiency) %>%
      layer_points() %>%
      layer_text(x = ~dmu, y = ~efficiency, text := ~efficiency)

  }
  else 
    return(NULL)

  p %>% bind_shiny("ggvis")
  
  
}