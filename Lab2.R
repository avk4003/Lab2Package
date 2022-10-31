
#' Boxplot
#'
#' @param data  a dataframe
#' @param payments a string name for variable y
#'
#' @return a boxplot of payments vs DRG codes
#' @export
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 geom_boxplot
#' @examples
#' boxplot_payments(DRG_data, "Average.Total.Payments")
boxplot_payments <- function(data, payments){
  DRG <- data %>%
    mutate(def = sub("-.*", "", DRG.Definition))  ## mutate the DRG.Definition to first 3 digitals

  ggplot(DRG, aes(x = def, y = get(payments))) +  ## initial plot
    geom_boxplot() +  ## make boxplot
    theme(axis.text.x = element_text(angle = 90))+  ## tilt the x-axis
    labs(
      x = 'Codes', ## relabel x axis
      y = "Average Payments", ## relabel y axis
      title = str_to_title(gsub("[.]", " ", str_to_title(payments))))  ## name the title
}
#' DRG statistics
#'
#' @param data a dataframe
#' @param column1 sort by variable's names (DRG codes)
#' @param column2 variable with numerical data
#' @param type string to indicate mean, median, or standard deviation
#'
#' @return a data table via knitr::kable
#' @export
#' @importFrom knitr
#' @examples
#' statistics_summary(DRG_data, "DRG.Definition", "Average.Medicare.Payments", "stdev")
statistics_summary <- function(data, column1, column2, type)#type = mean, median
{
  DRG <- data %>%
    mutate(def = sub("-.*", "", DRG.Definition)) ## mutate the DRG.Definition to first 3 digitals
  
  if (type == "average"){  ## calculate the mean
    stats<-data%>%group_by(get(column1))%>%summarise(Average.Payments=(mean(get(column2))))
  }

  if(type == "median"){  ## calculate the median
    stats<-data%>%group_by(get(column1))%>%summarise(Median.Payments=(median(get(column2))))
  }
  
  if (type == "stdev"){  ## calculate the standard deviation
    stats<-data%>%group_by(get(column1))%>%summarise(Standard.Deviation=(sd(get(column2))))
  }
  
  colnames(stats)[1] <- (column1)

  kable(stats, col.names = gsub("[.]", " ", names(stats)), caption = "Table of DRG Codes Stats")  ## kable the table

}
