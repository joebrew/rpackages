########
# VISUALIZE RESULTS OF A LOGISTIC REGRESSION MODEL IN BARPLOT WITH CI BARS
########
BarModel <- function(model, conf = 0.95, las = 3){
  # View confidence intervals on "odds" of assignment to phase 1
  ci <- exp(cbind(OR = coef(model), confint(model, level = conf)))
  #ci
  bp <- barplot(ci[-1,1], border = FALSE, plot=TRUE, 
                ylim=c(
                  ifelse(min(ci[-1,], na.rm = T) < 0, 
                         min(ci[-1,], na.rm = T), 0),
                  max(ci[-1,], na.rm = T)),
                cex.names = 6 / nrow(ci),
                las = las,
                main = paste("Odds of assignement with", conf*100, "% confidence bars"),
                cex.main = 0.7)
  abline(h=1, col = "red")
  errbar(x=bp[,1],
         y=ci[-1,1],
         yplus=ci[-1,3],
         yminus=ci[-1,2],
         add=TRUE, 
         type="n")
  text(bp[,1],
       y= 0,
       pos = 3,
       labels = round(ci[-1,1], digits = 2))
  text(bp[,1],
       y= 0,
       pos = 1,
       labels = paste0(
         "(",
         round(ci[-1,2], digits = 2),
         " - ",
         round(ci[-1,3], digits = 2), 
         ")"))
}
