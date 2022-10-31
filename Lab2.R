
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
    mutate(def = sub("-.*", "", DRG.Definition))

  ggplot(DRG, aes(x = def, y = get(payments))) +
    geom_boxplot()+theme(axis.text.x = element_text(angle = 90))+
    labs(
      x = 'Codes', ## relabel x axis # rubric says "Create function for DRG ONLY"
      y = "Average Payments", ## relabel y axis
      title = str_to_title(gsub("[.]", " ", str_to_title(payments))))
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
statistics_summary<-function(data, column1, column2, type)#type = mean, median
{
  if (type == "average"){
    stats<-data%>%group_by(get(column1))%>%summarise(Average.Payments=(mean(get(column2))))

  }

  if(type == "median"){
    stats<-data%>%group_by(get(column1))%>%summarise(Median.Payments=(median(get(column2))))
  }
  if (type == "stdev"){
    stats<-data%>%group_by(get(column1))%>%summarise(Standard.Deviation=(sd(get(column2))))
  }
  colnames(stats)[1] <- (column1)

  kable(stats, col.names = gsub("[.]", " ", names(stats)), caption = "Table of DRG Codes Stats")
}
