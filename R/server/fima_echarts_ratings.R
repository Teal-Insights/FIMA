
# starts: -----------------------------------------------------------------
fima_echarts_ratings <- function(
    data,                   # Data frame containing the data
    x_col,                  # Column name for x-axis (typically "year")
    y_col,                  # Column name for credit rating values
    group_col = NULL,       # Optional column name for grouping (faceting)
    group_levels = NULL,    # Optional custom ordering for group levels
    title = NULL,           # Optional chart title
    x_axis_name = ""        # Optional x-axis label
) {
  # Check if required columns exist in the data
  if (!x_col %in% colnames(data)) {
    stop(paste("x_col '", x_col, "' not found in data"))
  }
  if (!y_col %in% colnames(data)) {
    stop(paste("y_col '", y_col, "' not found in data"))
  }
  
  # Prepare the data
  # First check if group column exists
  if (!is.null(group_col) && !group_col %in% colnames(data)) {
    warning(paste("group_col '", group_col, "' not found in data. Ignoring grouping."))
    group_col <- NULL
  }
  
  # Select data columns based on what's available
  if (is.null(group_col)) {
    plot_data <- data %>% select(all_of(c(x_col, y_col)))
  } else {
    plot_data <- data %>% select(all_of(c(x_col, y_col, group_col)))
    
    # Apply custom factor levels if provided
    if (!is.null(group_levels)) {
      plot_data <- plot_data %>%
        mutate(!!sym(group_col) := factor(get(group_col), levels = group_levels))
    }
    
    # Group by the specified column
    plot_data <- plot_data %>% dplyr::group_by(!!sym(group_col))
  }
  
  # Generate rating labels
  rating_labels <- c(
    "AAA", "AA+", "AA", "AA-", "A+", "A", "A-", 
    "BBB+", "BBB", "BBB-", "BB+", "BB", "BB-", 
    "B+", "B", "B-", "CCC+", "CCC", "CCC-", "CC", "C", "RD"
  )
  
  # Convert the mapping to JSON for all ratings
  rating_json <- jsonlite::toJSON(setNames(rating_labels, 1:22))
  
  # Start building the chart
  chart <- plot_data %>%
    echarts4r::e_charts_(x_col)
  
  # Add line series
  chart <- chart %>% echarts4r::e_line_(y_col)
  
  # Add title if provided
  if (!is.null(title)) {
    chart <- chart %>% echarts4r::e_title(title)
  }
  
  # Add x-axis configuration
  chart <- chart %>%
    echarts4r::e_x_axis(
      name = x_axis_name,
      type = "category"
    )
  
  number_map <- c(
    "22" = "AAA",
    "21" = "AA+",
    "20" = "AA",
    "19" = "AA-",
    "18" = "A+",
    "17" = "A",
    "16" = "A-",
    "15" = "BBB+",
    "14" = "BBB",
    "13" = "BBB-",
    "12" = "BB+",
    "11" = "BB",
    "10" = "BB-",
    "9" = "B+",
    "8" = "B",
    "7" = "B-",
    "6" = "CCC+",
    "5" = "CCC",
    "4" = "CCC-",
    "3" = "CC",
    "2" = "C",
    "1" = "RD"
  )
  # Convert the named vector to a list
  labels_list <- as.list(number_map)
  names(labels_list) <- as.numeric(names(number_map))
  labels_json <- jsonlite::toJSON(labels_list, auto_unbox = TRUE)
  
  # min and max values in y-axis
  y_min_value <- data %>% pull(y_col) %>% min(na.rm = TRUE)
  y_max_value <- data %>% pull(y_col) %>% max(na.rm = TRUE)
  # Add custom y-axis configuration for credit ratings
  chart <- chart %>%
    e_y_axis(
      scale = FALSE,
      min = y_min_value,
      max = y_max_value,
      minInterval = 1,
      axisLabel = list(
        formatter = htmlwidgets::JS(paste0("
          function(value) {
            var labels = ", labels_json, ";
            return labels[value] || value;
          }
        "))
      )
    ) 
  
  chart <- chart %>% echarts4r::e_legend(
    bottom = "0%",
    orient = "horizontal",
    x = "center",
    padding = c(5, 10, 5, 10)
  )  %>% 
    echarts4r::e_legend(
      bottom = "0%",
      orient = "horizontal",
      x = "center",
      padding = c(5, 10, 5, 10)
    )
  
  # Add tooltip with custom formatter for credit ratings
  chart <- chart %>%
    echarts4r::e_tooltip(
      trigger = "axis",
      formatter = htmlwidgets::JS(paste0("
        function(params) {
          var labels = ", labels_json, ";
          var result = params[0].name + '<br/>';
          
          params.forEach(function(param) {
            var markerSpan = '<span style=\"display:inline-block;margin-right:5px;border-radius:10px;width:10px;height:10px;background-color:' + param.color + ';\"></span>';
            var seriesName = param.seriesName;
            var rawValue = param.value[1]; // Get the raw numeric value
            var formattedValue = labels[rawValue]; // Only show rating labels, not numeric values
            
            result += markerSpan + seriesName + ': ' + formattedValue + '<br/>';
          });
          
          return result;
        }
      ")),
      axisPointer = list(
        type = "cross",
        label = list(
          backgroundColor = "#6a7985",
          formatter = htmlwidgets::JS(paste0("
            function(params) {
              var labels = ", labels_json, ";
              if (params.axisDimension === 'y') {
                return labels[params.value] || '';
              }
              return params.value;
            }
          "))
        )
      )
    )
  
  # Add grid configuration
  chart <- chart %>%
    echarts4r::e_grid(
      containLabel = TRUE,
      top = "10%",  
      bottom = "7%",
      left = "1%",
      right = "1%"
    )
  
  # Add toolbox with features
  chart <- chart %>%
    echarts4r::e_toolbox(
      orient = "horizontal",
      right = 15,
      top = 0,
      feature = list(
        # Image download option
        saveAsImage = list(
          title = "Save as image",
          name = "credit_rating_chart",
          pixelRatio = 2,
          type = "png"
        ),
        # Restore chart
        restore = list(
          title = "Restore"
        ),
        # Data zoom feature
        dataZoom = list(
          title = list(
            zoom = "Zoom area",
            back = "Reset zoom"
          )
        ),
        # Magic type for changing chart types
        magicType = list(
          type = c("line", "bar"),
          title = list(
            line = "Switch to Line",
            bar = "Switch to Bar"
          )
        )
      )
    )
  # return plot
  return(chart)
}

# ends: -------------------------------------------------------------------


