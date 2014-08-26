#' cdph.
#'
#' @name cdph
#' @docType package


############
# WRITE FUNCTION FOR AREA UNDER CURVE
############
AUCModel <- function(model, 
                     test.data=test,
                     plot = TRUE,
                     type = "cont",
                     threshold = 5,
                     title.size = 1,
                     RMSE = TRUE){
  
  if(type == "cont"){
    
    x <- predict(model, test.data, type = "response")
    
    auc.x <- vector(length = 101, mode = "numeric")
    auc.y <- vector(length = 101, mode = "numeric")
    
    seq.vec <- 0:100
    prob.vec <- seq(0,20, length = 101)
    
    for (i in seq.vec){
      test.data[,"g5.predicted"] <- ifelse(x > prob.vec[i], "predicted.true", "predicted.false")
      test.data[,"g5.observed"] <- ifelse(test.data[,"bll"] > threshold,  "observed.true", "observed.false") #bll"] > 5      
      ########
      
      
      truth.table <- table(test.data[,"g5.predicted"], test.data[,"g5.observed"])
      truth.table
      truth <- prop.table(truth.table, 1)*100  
      
      true.positives <- length(test.data[,"g5.predicted"][which(test.data[,"g5.predicted"] == "predicted.true" &
                                                             test.data[,"g5.observed"] == "observed.true")])
      false.positives <- length(test.data[,"g5.predicted"][which(test.data[,"g5.predicted"] == "predicted.true" &
                                                              test.data[,"g5.observed"] == "observed.false")])
      false.negatives <- length(test.data[,"g5.predicted"][which(test.data[,"g5.predicted"] == "predicted.false" &
                                                              test.data[,"g5.observed"] == "observed.true")])
      true.negatives <- length(test.data[,"g5.predicted"][which(test.data[,"g5.predicted"] == "predicted.false" &
                                                             test.data[,"g5.observed"] == "observed.false")])
      
      sensitivity <-  true.positives / (true.positives + false.negatives)
      specificity <- true.negatives / (true.negatives + false.positives)
      
      ppv <- true.positives / (true.positives + false.positives)
      npv <- true.negatives / (true.negatives + false.negatives)
      
      auc.x[i] <- 1 - specificity
      auc.y[i] <- sensitivity
    }
    
    if(RMSE){
      y <- ifelse(test.data[,"bll"] > threshold,  1, 0) 
      
      rmse <- sqrt(mean((y-x)^2,na.rm=TRUE))
    }
    
    
  }else{
    
    x <- predict(model, test.data, type = "response")

    auc.x <- vector(length = 101, mode = "numeric")
    auc.y <- vector(length = 101, mode = "numeric")
    
    seq.vec <- 0:100
    prob.vec <- seq(0,1, length = 100)
    
    for (i in seq.vec){
      test.data[,"g5.predicted"] <- ifelse(x > prob.vec[i], "predicted.true", "predicted.false")
      test.data[,"g5.observed"] <- ifelse(test.data[,"bll"] > threshold,  "observed.true", "observed.false") #bll"] > 5      
      ########
      
      
      truth.table <- table(test.data[,"g5.predicted"], test.data[,"g5.observed"])
      truth.table
      truth <- prop.table(truth.table, 1)*100  
      
      true.positives <- length(test.data[,"g5.predicted"][which(test.data[,"g5.predicted"] == "predicted.true" &
                                                                  test.data[,"g5.observed"] == "observed.true")])
      false.positives <- length(test.data[,"g5.predicted"][which(test.data[,"g5.predicted"] == "predicted.true" &
                                                                   test.data[,"g5.observed"] == "observed.false")])
      false.negatives <- length(test.data[,"g5.predicted"][which(test.data[,"g5.predicted"] == "predicted.false" &
                                                                   test.data[,"g5.observed"] == "observed.true")])
      true.negatives <- length(test.data[,"g5.predicted"][which(test.data[,"g5.predicted"] == "predicted.false" &
                                                                  test.data[,"g5.observed"] == "observed.false")])
      
      sensitivity <-  true.positives / (true.positives + false.negatives)
      specificity <- true.negatives / (true.negatives + false.positives)
      
      ppv <- true.positives / (true.positives + false.positives)
      npv <- true.negatives / (true.negatives + false.negatives)
      
      auc.x[i] <- 1 - specificity
      auc.y[i] <- sensitivity
    }
    
    if(RMSE){
      
      y <- ifelse(test.data[,"bll"] > threshold,  1, 0) 
      
      rmse <- sqrt(mean((y-x)^2,na.rm=TRUE))
    }
    
  }
  
 
  
  mod.auc <- auc(auc.x, auc.y, thresh = NULL, dens = 100)
  cat(paste0("Area under curve: " ,mod.auc, "\n"))
  cat(paste0("RMSE: " ,rmse))
  
  
  if(plot){
    plot(0:1, 0:1,
         xlab="False positive rate",
         ylab="True positive rate",
         main = paste0("ROC curve for ", as.character(model["formula"])),
         cex.main = title.size,
         type = "n",
         yaxt = "n")
    axis(side = 2,
         at = seq(0,1,.2),
         labels=seq(0,1,.2),
         las=1)
    lines(x = auc.x,
           y = auc.y,
          lwd=3,
           col=adjustcolor("darkblue", alpha.f=0.6))
    lines(x = 0:1,
          y = 0:1,
          col=adjustcolor("black", alpha.f=0.5))
    
    abline(h=seq(0,1,.2), col=adjustcolor("black", alpha.f=0.1))
    abline(v=seq(0,1,.2), col=adjustcolor("black", alpha.f=0.1))
    
    text(x = 0.7,
         y = 0.3,
         labels = paste0("AUC: ", round(mod.auc, digits=3)),
         cex=2)
    
    text(x = 0.7,
         y = 0.1,
         labels = paste0("Model was able to predict\n on ", 
                         length(x[which(!is.na(x))]), 
                         " of ",
                         length(x), 
                         " individuals\n (",
                         round(length(x[which(!is.na(x))])/length(x)*100, digits=2),
                         "%)"))
    if(RMSE){
      text(x = 0.1,
           y = 0.9,
           labels = paste0("RMSE: ", round(rmse, digits=3)),
           cex=1)
      
    }

  }
}


#############
# WRITE FUNCTION FOR TESTING MODELS
#############
TestModel <- function(model, 
                      plot=TRUE, 
                      label.points=TRUE,
                      test.data = test, # test data set
                      threshold = NULL,
                      text=TRUE,
                      cex=0.5,
                      tree=FALSE,
                      show.truth=FALSE,
                      type = "class"){
  
  wide.lim <- c(-0.2, 1.2)
  
  
  if(tree){
    x <- predict(model, test.data, type="prob")
    x <- as.numeric(x[,2])
    
  } else{
    x <- predict(model, test.data, type="response")
    
  }
  
  if(type == "cont"){    
    
    if(!is.null(threshold)){
      
      
      ############ THRESHOLD
      
      test.data$g5.predicted <- ifelse(x > threshold, "predicted.true", "predicted.false")
      test.data$g5.observed <- ifelse(test.data$bll5 == "TRUE", "observed.true", "observed.false") # bll > 5
    } else{
      test.data$g5.predicted <- ifelse(x > 5, "predicted.true", "predicted.false")
      test.data$g5.observed <- ifelse(test.data$bll5 == "TRUE", "observed.true", "observed.false") # bll > 5
      
    }
    
    truth.table <- table(test.data[,"g5.predicted"], test.data[,"g5.observed"])
    truth.table
    truth <- prop.table(truth.table, 1)*100  
    
    true.positives <- length(test.data[,"g5.predicted"][which(test.data[,"g5.predicted"] == "predicted.true" &
                                                                test.data[,"g5.observed"] == "observed.true")])
    false.positives <- length(test.data[,"g5.predicted"][which(test.data[,"g5.predicted"] == "predicted.true" &
                                                                 test.data[,"g5.observed"] == "observed.false")])
    false.negatives <- length(test.data[,"g5.predicted"][which(test.data[,"g5.predicted"] == "predicted.false" &
                                                                 test.data[,"g5.observed"] == "observed.true")])
    true.negatives <- length(test.data[,"g5.predicted"][which(test.data[,"g5.predicted"] == "predicted.false" &
                                                                test.data[,"g5.observed"] == "observed.false")])
    
    sensitivity <-  true.positives / (true.positives + false.negatives)
    specificity <- true.negatives / (true.negatives + false.positives)
    
    ppv <- true.positives / (true.positives + false.positives)
    npv <- true.negatives / (true.negatives + false.negatives)
    
    false.positive.rate <- 
      length(test.data$g5.predicted[which(test.data$g5.predicted == "predicted.true" &
                                            test.data$g5.observed == "observed.false")]) /
      length(test.data$g5.predicted[which(test.data$g5.predicted == "predicted.true")])
    
    true.positive.rate <- 
      length(test.data$g5.predicted[which(test.data$g5.predicted == "predicted.true" &
                                            test.data$g5.observed == "observed.true")]) /
      length(test.data$g5.predicted[which(test.data$g5.predicted == "predicted.true")])
    
    false.negative.rate <- 
      length(test.data$g5.predicted[which(test.data$g5.predicted == "predicted.false" &
                                            test.data$g5.observed == "observed.true")]) /
      length(test.data$g5.predicted[which(test.data$g5.predicted == "predicted.false")])
    
    true.negative.rate <- 
      length(test.data$g5.predicted[which(test.data$g5.predicted == "predicted.false" &
                                            test.data$g5.observed == "observed.false")]) /
      length(test.data$g5.predicted[which(test.data$g5.predicted == "predicted.false")])
    
    
    cat(paste0("Sensitivity: ", round(sensitivity, digits=2)), "\n")
    cat(paste0("Specificity: ", round(specificity, digits=2)), "\n\n")
    
    cat(paste0("PPV: ", round(ppv, digits=2)), "\n")
    cat(paste0("NPV: ", round(npv, digits =2)), "\n\n")
    
    cat(paste0("True positive rate: ", round(true.positive.rate*100, digits=2), "%" ), "\n")
    cat(paste0("True negative rate: ", round(true.negative.rate*100, digits=2), "%" ), "\n\n")
    
    cat(paste0("False positive rate: ", round(false.positive.rate*100, digits=2), "%" ), "\n")
    cat(paste0("False negative rate: ", round(false.negative.rate*100, digits=2), "%" ), "\n\n")
    
    cat(paste0("At a threshold of ", threshold,
               ",\nwe would inspect the homes of\n", 
               length(test.data$g5.predicted[which(test.data$g5.predicted == "predicted.true")]),
               " children, \nand would find that ",
               length(test.data$g5.predicted[which(test.data$g5.predicted == "predicted.true" &
                                                     test.data$g5.observed == "observed.true")]),
               "\nof them have BLL > 5."))
    
    if(show.truth){
      cat(paste0("\n"))
      print(truth.table)
      print(prop.table(truth.table, 1)*100)  
    }
    
    
  }else{
    
    
    if(!is.null(threshold)){
      
      
      ############ THRESHOLD
      
      test.data$g5.predicted <- ifelse(x > threshold, "predicted.true", "predicted.false")
      test.data$g5.observed <- ifelse(test.data$bll5 == "TRUE", "observed.true", "observed.false") # bll > 5
    }
    
    truth.table <- table(test.data$g5.predicted, test.data$g5.observed)
    truth.table
    truth <- prop.table(truth.table, 1)*100  
    
    true.positives <- length(test.data$g5.predicted[which(test.data$g5.predicted == "predicted.true" &
                                                            test.data$g5.observed == "observed.true")])
    false.positives <- length(test.data$g5.predicted[which(test.data$g5.predicted == "predicted.true" &
                                                             test.data$g5.observed == "observed.false")])
    false.negatives <- length(test.data$g5.predicted[which(test.data$g5.predicted == "predicted.false" &
                                                             test.data$g5.observed == "observed.true")])
    true.negatives <- length(test.data$g5.predicted[which(test.data$g5.predicted == "predicted.false" &
                                                            test.data$g5.observed == "observed.false")])
    
    sensitivity <-  true.positives / (true.positives + false.negatives)
    specificity <- true.negatives / (true.negatives + false.positives)
    
    ppv <- true.positives / (true.positives + false.positives)
    npv <- true.negatives / (true.negatives + false.negatives)
    
    
    false.positive.rate <- 
      length(test.data$g5.predicted[which(test.data$g5.predicted == "predicted.true" &
                                            test.data$g5.observed == "observed.false")]) /
      length(test.data$g5.predicted[which(test.data$g5.predicted == "predicted.true")])
    
    true.positive.rate <- 
      length(test.data$g5.predicted[which(test.data$g5.predicted == "predicted.true" &
                                            test.data$g5.observed == "observed.true")]) /
      length(test.data$g5.predicted[which(test.data$g5.predicted == "predicted.true")])
    
    false.negative.rate <- 
      length(test.data$g5.predicted[which(test.data$g5.predicted == "predicted.false" &
                                            test.data$g5.observed == "observed.true")]) /
      length(test.data$g5.predicted[which(test.data$g5.predicted == "predicted.false")])
    
    true.negative.rate <- 
      length(test.data$g5.predicted[which(test.data$g5.predicted == "predicted.false" &
                                            test.data$g5.observed == "observed.false")]) /
      length(test.data$g5.predicted[which(test.data$g5.predicted == "predicted.false")])
    
    
    cat(paste0("Sensitivity: ", round(sensitivity, digits=2)), "\n")
    cat(paste0("Specificity: ", round(specificity, digits=2)), "\n\n")
    
    cat(paste0("PPV: ", round(ppv, digits=2)), "\n")
    cat(paste0("NPV: ", round(npv, digits =2)), "\n\n")
    
    cat(paste0("True positive rate: ", round(true.positive.rate*100, digits=2), "%" ), "\n")
    cat(paste0("True negative rate: ", round(true.negative.rate*100, digits=2), "%" ), "\n\n")
    
    cat(paste0("False positive rate: ", round(false.positive.rate*100, digits=2), "%" ), "\n")
    cat(paste0("False negative rate: ", round(false.negative.rate*100, digits=2), "%" ), "\n\n")
    
    cat(paste0("At a threshold of ", threshold,
               ",\nwe would inspect the homes of\n", 
               length(test.data$g5.predicted[which(test.data$g5.predicted == "predicted.true")]),
               " children, \nand would find that ",
               length(test.data$g5.predicted[which(test.data$g5.predicted == "predicted.true" &
                                                     test.data$g5.observed == "observed.true")]),
               "\nof them have BLL > 5."))
    
    if(show.truth){
      cat(paste0("\n"))
      print(truth.table)
      print(prop.table(truth.table, 1)*100)  
    }
    
    ############## \ THRESHOLD
    
    
  }
  
  
  
  
  ### <PLOT>> ####################################
  if(plot){
    plot(0:1, 0:1,
         xlab = "False positive rate",
         ylab = "True positive rate",
         type="n",
         xlim=if(text){wide.lim}else{c(0,1)},
         ylim=if(text){wide.lim}else{c(0,1)},
         xaxt="n",
         yaxt="n")
    
    axis(side = 1,
         at = seq(0,1, 0.2), 
         labels = seq(0, 1, 0.2))
    
    axis(side = 2,
         at = seq(0,1, 0.2), 
         labels = seq(0, 1, 0.2),
         las = 1)
    
    l.col <- adjustcolor("black", alpha.f=0.4)
    lines(0:1, 0:1, col=adjustcolor("black", alpha.f=0.5),
          lwd=3)
    lines(c(0,0),
          c(0,1), col = l.col)
    lines(c(0,1),
          c(0,0), col = l.col)
    lines(c(1,1),
          c(0,1), col = l.col)
    lines(c(0,1),
          c(1,1), col = l.col)
    
    if(label.points == FALSE){
      points(x = 1 - specificity,
             y = sensitivity,
             pch=20,
             col=adjustcolor("darkblue", alpha.f=0.6))
    }
    
    if(type == "cont"){
      
      for (cutoff in seq(2,6, 0.1)){  
        
        test.data[,"g5.predicted"] <- ifelse(x > cutoff, "predicted.true", "predicted.false")
        test.data[,"g5.observed"] <- ifelse(test.data[,"bll5"] == "TRUE",  "observed.true", "observed.false") #bll"] > 5      
        ########
        
        
        truth.table <- table(test.data[,"g5.predicted"], test.data[,"g5.observed"])
        truth.table
        truth <- prop.table(truth.table, 1)*100  
        
        true.positives <- length(test.data[,"g5.predicted"][which(test.data[,"g5.predicted"] == "predicted.true" &
                                                                    test.data[,"g5.observed"] == "observed.true")])
        false.positives <- length(test.data[,"g5.predicted"][which(test.data[,"g5.predicted"] == "predicted.true" &
                                                                     test.data[,"g5.observed"] == "observed.false")])
        false.negatives <- length(test.data[,"g5.predicted"][which(test.data[,"g5.predicted"] == "predicted.false" &
                                                                     test.data[,"g5.observed"] == "observed.true")])
        true.negatives <- length(test.data[,"g5.predicted"][which(test.data[,"g5.predicted"] == "predicted.false" &
                                                                    test.data[,"g5.observed"] == "observed.false")])
        
        sensitivity <-  true.positives / (true.positives + false.negatives)
        specificity <- true.negatives / (true.negatives + false.positives)
        
        ppv <- true.positives / (true.positives + false.positives)
        npv <- true.negatives / (true.negatives + false.negatives)
        
        
        if(text){
          text(x = 1 - specificity,
               y = sensitivity,
               labels = paste0(cutoff),
               cex=cex,
               col=adjustcolor("black", alpha.f=0.6),
               pos=4)        
        } else{
          points(x = 1 - specificity,
                 y = sensitivity,
                 cex=cex,
                 col=adjustcolor("darkred", alpha.f=0.9),
                 pch=16)  
        }  
      }
      
    } else{
      
      for (cutoff in seq(0,1, 0.01)){  
        
        test.data[,"g5.predicted"] <- ifelse(x > cutoff, "predicted.true", "predicted.false")
        test.data[,"g5.observed"] <- ifelse(test.data[,"bll5"] == "TRUE",  "observed.true", "observed.false") #bll"] > 5      
        ########
        
        
        truth.table <- table(test.data[,"g5.predicted"], test.data[,"g5.observed"])
        truth.table
        truth <- prop.table(truth.table, 1)*100  
        
        true.positives <- length(test.data[,"g5.predicted"][which(test.data[,"g5.predicted"] == "predicted.true" &
                                                                    test.data[,"g5.observed"] == "observed.true")])
        false.positives <- length(test.data[,"g5.predicted"][which(test.data[,"g5.predicted"] == "predicted.true" &
                                                                     test.data[,"g5.observed"] == "observed.false")])
        false.negatives <- length(test.data[,"g5.predicted"][which(test.data[,"g5.predicted"] == "predicted.false" &
                                                                     test.data[,"g5.observed"] == "observed.true")])
        true.negatives <- length(test.data[,"g5.predicted"][which(test.data[,"g5.predicted"] == "predicted.false" &
                                                                    test.data[,"g5.observed"] == "observed.false")])
        
        sensitivity <-  true.positives / (true.positives + false.negatives)
        specificity <- true.negatives / (true.negatives + false.positives)
        
        ppv <- true.positives / (true.positives + false.positives)
        npv <- true.negatives / (true.negatives + false.negatives)
        
        
        if(text){
          text(x = 1 - specificity,
               y = sensitivity,
               labels = paste0(cutoff),
               cex=cex,
               col=adjustcolor("black", alpha.f=0.6),
               pos=4)        
        } else{
          points(x = 1 - specificity,
                 y = sensitivity,
                 cex=cex,
                 col=adjustcolor("darkred", alpha.f=0.9),
                 pch=16)  
        }  
      }
      
    }
    
    
    ### <NO THRESHOLD>
    
  }
  
}
### </PLOT> ####################################




################# GAM COLORS myvis.gam

# from

library(RColorBrewer)
jet.colors <-colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan",
                                "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))
joe.colors <- brewer.pal(9, "YlOrRd")

myvis.gam <- function (x, view = NULL, cond = list(), n.grid = 30, too.far = 0, 
                       col = NA, color = "heat", contour.col = NULL, se = -1, type = "link", 
                       plot.type = "persp", zlim = NULL, nCol = 50, ...) 
{
  fac.seq <- function(fac, n.grid) {
    fn <- length(levels(fac))
    gn <- n.grid
    if (fn > gn) 
      mf <- factor(levels(fac))[1:gn]
    else {
      ln <- floor(gn/fn)
      mf <- rep(levels(fac)[fn], gn)
      mf[1:(ln * fn)] <- rep(levels(fac), rep(ln, fn))
      mf <- factor(mf, levels = levels(fac))
    }
    mf
  }
  dnm <- names(list(...))
  v.names <- names(x$var.summary)
  if (is.null(view)) {
    k <- 0
    view <- rep("", 2)
    for (i in 1:length(v.names)) {
      ok <- TRUE
      if (is.matrix(x$var.summary[[i]])) 
        ok <- FALSE
      else if (is.factor(x$var.summary[[i]])) {
        if (length(levels(x$var.summary[[i]])) <= 1) 
          ok <- FALSE
      }
      else {
        if (length(unique(x$var.summary[[i]])) == 1) 
          ok <- FALSE
      }
      if (ok) {
        k <- k + 1
        view[k] <- v.names[i]
      }
      if (k == 2) 
        break
    }
    if (k < 2) 
      stop("Model does not seem to have enough terms to do anything useful")
  }
  else {
    if (sum(view %in% v.names) != 2) 
      stop(paste(c("view variables must be one of", v.names), 
                 collapse = ", "))
    for (i in 1:2) if (!inherits(x$var.summary[[view[i]]], 
                                 c("numeric", "factor"))) 
      stop("Don't know what to do with parametric terms that are not simple numeric or factor variables")
  }
  ok <- TRUE
  for (i in 1:2) if (is.factor(x$var.summary[[view[i]]])) {
    if (length(levels(x$var.summary[[view[i]]])) <= 1) 
      ok <- FALSE
  }
  else {
    if (length(unique(x$var.summary[[view[i]]])) <= 1) 
      ok <- FALSE
  }
  if (!ok) 
    stop(paste("View variables must contain more than one value. view = c(", 
               view[1], ",", view[2], ").", sep = ""))
  if (is.factor(x$var.summary[[view[1]]])) 
    m1 <- fac.seq(x$var.summary[[view[1]]], n.grid)
  else {
    r1 <- range(x$var.summary[[view[1]]])
    m1 <- seq(r1[1], r1[2], length = n.grid)
  }
  if (is.factor(x$var.summary[[view[2]]])) 
    m2 <- fac.seq(x$var.summary[[view[2]]], n.grid)
  else {
    r2 <- range(x$var.summary[[view[2]]])
    m2 <- seq(r2[1], r2[2], length = n.grid)
  }
  v1 <- rep(m1, n.grid)
  v2 <- rep(m2, rep(n.grid, n.grid))
  newd <- data.frame(matrix(0, n.grid * n.grid, 0))
  for (i in 1:length(x$var.summary)) {
    ma <- cond[[v.names[i]]]
    if (is.null(ma)) {
      ma <- x$var.summary[[i]]
      if (is.numeric(ma)) 
        ma <- ma[2]
    }
    if (is.matrix(x$var.summary[[i]])) 
      newd[[i]] <- matrix(ma, n.grid * n.grid, ncol(x$var.summary[[i]]), 
                          byrow = TRUE)
    else newd[[i]] <- rep(ma, n.grid * n.grid)
  }
  names(newd) <- v.names
  newd[[view[1]]] <- v1
  newd[[view[2]]] <- v2
  if (type == "link") 
    zlab <- paste("linear predictor")
  else if (type == "response") 
    zlab <- type
  else stop("type must be \"link\" or \"response\"")
  fv <- predict.gam(x, newdata = newd, se.fit = TRUE, type = type)
  z <- fv$fit
  if (too.far > 0) {
    ex.tf <- exclude.too.far(v1, v2, x$model[, view[1]], 
                             x$model[, view[2]], dist = too.far)
    fv$se.fit[ex.tf] <- fv$fit[ex.tf] <- NA
  }
  if (is.factor(m1)) {
    m1 <- as.numeric(m1)
    m1 <- seq(min(m1) - 0.5, max(m1) + 0.5, length = n.grid)
  }
  if (is.factor(m2)) {
    m2 <- as.numeric(m2)
    m2 <- seq(min(m1) - 0.5, max(m2) + 0.5, length = n.grid)
  }
  if (se <= 0) {
    old.warn <- options(warn = -1)
    av <- matrix(c(0.5, 0.5, rep(0, n.grid - 1)), n.grid, 
                 n.grid - 1)
    options(old.warn)
    max.z <- max(z, na.rm = TRUE)
    z[is.na(z)] <- max.z * 10000
    z <- matrix(z, n.grid, n.grid)
    surf.col <- t(av) %*% z %*% av
    surf.col[surf.col > max.z * 2] <- NA
    if (!is.null(zlim)) {
      if (length(zlim) != 2 || zlim[1] >= zlim[2]) 
        stop("Something wrong with zlim")
      min.z <- zlim[1]
      max.z <- zlim[2]
    }
    else {
      min.z <- min(fv$fit, na.rm = TRUE)
      max.z <- max(fv$fit, na.rm = TRUE)
    }
    surf.col <- surf.col - min.z
    surf.col <- surf.col/(max.z - min.z)
    surf.col <- round(surf.col * nCol)
    con.col <- 1
    if (color == "heat") {
      pal <- heat.colors(nCol)
      con.col <- 3
    }
    else if (color == "topo") {
      pal <- topo.colors(nCol)
      con.col <- 2
    }
    else if (color == "cm") {
      pal <- cm.colors(nCol)
      con.col <- 1
    }
    else if (color == "terrain") {
      pal <- terrain.colors(nCol)
      con.col <- 2
    }
    else if (color == "gray" || color == "bw") {
      pal <- gray(seq(0.1, 0.9, length = nCol))
      con.col <- 1
    }
    ### customized here
    else if (color == 'jet') {
      pal <- jet.colors(nCol)
      con.col = 1
    }
    
    else if (color == 'joe') {
      pal <- joe.colors(nCol)
      con.col = 1
    }
    
    ####
    else stop("color scheme not recognised")
    if (is.null(contour.col)) 
      contour.col <- con.col
    surf.col[surf.col < 1] <- 1
    surf.col[surf.col > nCol] <- nCol
    if (is.na(col)) 
      col <- pal[as.array(surf.col)]
    z <- matrix(fv$fit, n.grid, n.grid)
    if (plot.type == "contour") {
      stub <- paste(ifelse("xlab" %in% dnm, "", ",xlab=view[1]"), 
                    ifelse("ylab" %in% dnm, "", ",ylab=view[2]"), 
                    ifelse("main" %in% dnm, "", ",main=zlab"), ",...)", 
                    sep = "")
      if (color != "bw") {
        txt <- paste("image(m1,m2,z,col=pal,zlim=c(min.z,max.z)", 
                     stub, sep = "")
        eval(parse(text = txt))
        txt <- paste("contour(m1,m2,z,col=contour.col,zlim=c(min.z,max.z)", 
                     ifelse("add" %in% dnm, "", ",add=TRUE"), ",...)", 
                     sep = "")
        eval(parse(text = txt))
      }
      else {
        txt <- paste("contour(m1,m2,z,col=1,zlim=c(min.z,max.z)", 
                     stub, sep = "")
        eval(parse(text = txt))
      }
    }
    else {
      stub <- paste(ifelse("xlab" %in% dnm, "", ",xlab=view[1]"), 
                    ifelse("ylab" %in% dnm, "", ",ylab=view[2]"), 
                    ifelse("main" %in% dnm, "", ",zlab=zlab"), ",...)", 
                    sep = "")
      if (color == "bw") {
        op <- par(bg = "white")
        txt <- paste("persp(m1,m2,z,col=\"white\",zlim=c(min.z,max.z) ", 
                     stub, sep = "")
        eval(parse(text = txt))
        par(op)
      }
      else {
        txt <- paste("persp(m1,m2,z,col=col,zlim=c(min.z,max.z)", 
                     stub, sep = "")
        eval(parse(text = txt))
      }
    }
  }
  else {
    if (color == "bw" || color == "gray") {
      subs <- paste("grey are +/-", se, "s.e.")
      lo.col <- "gray"
      hi.col <- "gray"
    }
    else {
      subs <- paste("red/green are +/-", se, "s.e.")
      lo.col <- "green"
      hi.col <- "red"
    }
    if (!is.null(zlim)) {
      if (length(zlim) != 2 || zlim[1] >= zlim[2]) 
        stop("Something wrong with zlim")
      min.z <- zlim[1]
      max.z <- zlim[2]
    }
    else {
      z.max <- max(fv$fit + fv$se.fit * se, na.rm = TRUE)
      z.min <- min(fv$fit - fv$se.fit * se, na.rm = TRUE)
    }
    zlim <- c(z.min, z.max)
    z <- fv$fit - fv$se.fit * se
    z <- matrix(z, n.grid, n.grid)
    if (plot.type == "contour") 
      warning("sorry no option for contouring with errors: try plot.gam")
    stub <- paste(ifelse("xlab" %in% dnm, "", ",xlab=view[1]"), 
                  ifelse("ylab" %in% dnm, "", ",ylab=view[2]"), ifelse("zlab" %in% 
                                                                         dnm, "", ",zlab=zlab"), ifelse("sub" %in% dnm, 
                                                                                                        "", ",sub=subs"), ",...)", sep = "")
    txt <- paste("persp(m1,m2,z,col=col,zlim=zlim", ifelse("border" %in% 
                                                             dnm, "", ",border=lo.col"), stub, sep = "")
    eval(parse(text = txt))
    par(new = TRUE)
    z <- fv$fit
    z <- matrix(z, n.grid, n.grid)
    txt <- paste("persp(m1,m2,z,col=col,zlim=zlim", ifelse("border" %in% 
                                                             dnm, "", ",border=\"black\""), stub, sep = "")
    eval(parse(text = txt))
    par(new = TRUE)
    z <- fv$fit + se * fv$se.fit
    z <- matrix(z, n.grid, n.grid)
    txt <- paste("persp(m1,m2,z,col=col,zlim=zlim", ifelse("border" %in% 
                                                             dnm, "", ",border=hi.col"), stub, sep = "")
    eval(parse(text = txt))
  }
}



##############
# DEFINE A FUNCTION FOR MAPPING RESIDUALS
##############

MapModel <- function(model,
                     test.data = test,
                     cex = 0.5,
                     legend.details = FALSE,
                     type = "cont",
                     threshold = 0.5,
                     pt.cex=0.15,
                     alpha = 0.2){
  
  if(type == "cont"){  
    predicted <- predict(model, test.data)
    observed <- test.data[,"bll"] 
    
    plotvar <- predicted - observed
    
    nclr <- 9
    plotclr <- rev(brewer.pal(nclr, "RdYlBu"))
    suppressWarnings(class <- classIntervals(as.numeric(plotvar), nclr, style = "quantile", dataPrecision=1)) #use "equal" instead
    #class <- classIntervals(0:100, nclr, style="equal")
    colcode <- findColours(class, plotclr)
    legcode <- paste0(gsub(",", " - ", gsub("[[]|[]]|[)]", "", names(attr(colcode, "table")))), "")
    
    plot(x = test.data[,"civis_longitude"],
         y = test.data[,"civis_latitude"],
         xlab = "Longitude",
         ylab= "Latitude",
         col = adjustcolor(colcode, alpha.f= alpha),
         cex=pt.cex,
         pch=16)
    
    if(legend.details){
      legend("left", # position
             legend = legcode, #names(attr(colcode, "table")), 
             fill = attr(colcode, "palette"), 
             cex = 0.6, 
             border=NA,
             bty = "n",
             main="Predicted - observed") 
      
    }
    
    
    
    legend("bottomleft",
           fill = attr(colcode, "palette"),
           cex=0.6,
           border=NA,
           bty="n",
           y.intersp=0.5,
           legend=c("Model underpredicted",
                    rep(NA, nclr-2),
                    "Model overpredicted"))
    
    legend(x="topright",
           fill = NA,
           legend = paste0("Model was able to generate\nestimates for ", 
                           round(as.numeric(prop.table(table(is.na(predicted)))[1])*100, digits=1), 
                           " % \nof subjects in the test data"),
           border= FALSE,
           bty= "n",
           cex=0.5)
    
  } else{
    
    
    if(type == "class"){
      predicted <- predict(model, test.data, type="response") > threshold
      observed <- test.data[,"bll5"]
      
      plotcol <- ifelse(is.na(predicted), "grey",
                        ifelse(predicted == observed, "darkgreen",
                               ifelse(predicted == TRUE & observed == FALSE, "darkblue",
                                      ifelse(observed == TRUE & predicted == FALSE, "darkred",
                                             NA))))
      plotcol <- adjustcolor(plotcol, alpha.f= alpha)
      # plotcol <- factor(plotcol)
      
      plot(x = test.data[,"civis_longitude"], 
           y = test.data[,"civis_latitude"],
           xlab = "Longitude",
           ylab= "Latitude",
           col = plotcol,
           cex=pt.cex,
           pch=16)
      
      legend(x="bottomleft",
             fill = adjustcolor(c("Grey", "darkgreen", "darkblue", "darkred"), alpha.f=0.4),
             legend = c("No prediction possible",
                        "correct prediction",
                        "Model incorrectly predicted lead poisoning",
                        "Model incorrectly predicted safety"),
             bty = "n",
             border=FALSE,
             cex=0.6)
      
      legend(x="topright",
             fill = NA,
             legend = paste0("Model was able to generate\nestimates for ", 
                             round(as.numeric(prop.table(table(is.na(predicted)))[2])*100, digits=1), 
                             " % \nof subjects in the test data"),
             border= FALSE,
             bty= "n",
             cex=0.5)
      
      
      
    }
  }
  
  
  
}