#' Create a corrplot of drivers and dependent variable.
#' Without diagonal elements, and with original ordering of variables.
#'
#' @param data: a data frame containing variables for correlation matrix
#' @param title: a title of plot
#' @param filter: a logical value, if filtering is needed
#' @param variable: a variable used to filter
#' @param value: a filtered value
#'
#' @return A correlation plot of drivers and dependent variable present in data.
make_corrplot <-
  function(data,
           title,
           filter,
           variable = NULL,
           value = NULL) {
    if (filter == FALSE) {
      cor_matrix <- cor(data %>% select(-c(ID, brand, country)))
    }
    if (filter == TRUE) {
      #make variable a quosure
      variable = enquo(variable)
      cor_matrix <-
        cor(data %>% filter(!!variable == value) %>% select(-c(ID, brand, country)))
    }
    corrplot(
      cor_matrix,
      type = "full",
      order = "original",
      diag = F,
      addCoef.col = T,
      number.digits = 2,
      title = title,
      mar = c(0, 0, 1, 0)
    )
  }

#' Calculate mean drivers scores for plots.
#' To facilitate comparability, means are scaled as a % of the highest value.
#' This makes all plots comparable.
#'
#' @param data: a data set
#' @param filter_country: a country used for filtering
#' @param filter_brand: a brand used for filtering
#'
#' @return: a data frame, where "driver" contains drivers' names,
#' and "mean_value" contains mean driver value as a percentage of max
drivers_for_plot <-
  function(data, filter_country, filter_brand) {
    scaled_means <- data %>%
      filter(country == filter_country & brand == filter_brand) %>%
      select(starts_with("driver")) %>%
      apply(., 2, function(x) mean(x) / max(x)) %>%
      as.data.frame() %>%
      rownames_to_column()
    
    colnames(scaled_means) <- c("driver", "mean_value")
    
    return(scaled_means)
  }


#' Title
#'
#' @param data: a data set with SHAP values 
#' @param data2: a data set
#' @param filter_country: a country used for filtering
#' @param filter_brand: a brand used for filtering
#'
#' @return a data frame, where "driver" contains drivers' names,
#' and "mean_abs_value" contains mean absolute SHAP value scaled a percentage
#' of their sum
shap_for_plots <-
  function(data, data2, filter_country, filter_brand) {
    mean_shap <- data %>%
      mutate(brand = data2$brand,
             country = data2$country) %>%
      filter(country == filter_country & brand == filter_brand) %>%
      select(starts_with("driver")) %>%
      abs() %>%
      apply(., 2, function(x)
        mean(x)) %>%
      as.data.frame() %>%
      rownames_to_column()
    
    colnames(mean_shap) <- c("driver", "mean_abs_shap")
    
    mean_shap <- mean_shap %>%
      transmute(driver = driver,
                mean_abs_shap = mean_abs_shap / sum(mean_abs_shap) * 100)
    
    return(mean_shap)
  }





