#' @title Binarybalancedcut
#'
#' @description This Supports the datascientist to determine the optimal threshold for binary classifier problem by visuallizing the sensitivity, specificity and accurarcy of the given model.
#'
#' @param probability,class
#'
#' @return NULL
#'
#' @examples set.seed(100);disease <- sample(c("yes","no"), 1000, replace=TRUE);Probabilities<-sample(seq(0,1,by=0.01),1000,replace=TRUE);Binary_threshold(Probabilities,disease)
#'
#' @export
globalVariables(c("Probability", "Percentage","Legends"))
Binary_threshold<-function(probability,class){
  Unique_Prob<-sort(unique(probability))
  Unique_Prob<-Unique_Prob[-1]
  df<-data.frame()
  for(i in Unique_Prob){
    cut<-ifelse(probability<i,0,1)
    cm<-table(cut,class)
    df<-rbind(df,data.frame(Sensitivity=as.numeric(cm[1]/(cm[1]+cm[2])),Specificity=as.numeric(cm[4]/(cm[3]+cm[4]))))
  }
  df$Probability<-Unique_Prob
  test_data_long <- melt(df, id="Probability")  # convert to long format
  test_data_long$Legends<-test_data_long$variable
  test_data_long$Percentage<-test_data_long$value
  P1<-ggplot(data=test_data_long,
             aes(x=Probability, y=Percentage, colour=Legends)) +
    geom_line()+ggtitle("Binary Cut-Off Plot")
  print(P1)
}
