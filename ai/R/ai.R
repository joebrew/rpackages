library(Hmisc)
library(data.table)

########
# REMOVE TRAILING / LEADING WHITE SPACES
########
RemoveSpaces <- function(var){
  x <- gsub("^\\s+|\\s+$", "", as.character(var))
  return(x)
}


########
# VISUALIZE THE NUMBER OF CHARACTERS IN A VARIABLE
########
SeeChars <- function(var, cex = 0.5, border = FALSE, cutoff = 1, make_var = FALSE){
  y <- RemoveSpaces(var)
  x <- table(nchar(y))
  bp <- barplot(x, border = border)
  text(bp[,1], 0, x, pos=3, cex = cex) # ~ 5,000 have a one letter first name...
  legend(x="topright",
         fill = NA,
         border = NA,
         bty = "n",
         legend = paste(length(var[which(nchar(as.character(var)) <= cutoff)]),
                        "observations have\nless than or equal to",
                        cutoff,
                        "characters"))
  if(make_var){
    return(as.numeric(nchar(y)))
  }
}




########
# SET TO NA ALL OBSERVATIONS WITH FEWER THAN X CHARACTERS (DEFAULT)
########
MakeNA <- function(var, cutoff = 0, remove_spaces = FALSE){
  
  if(remove_spaces){
    nChars <- nchar(RemoveSpaces(var))
  }else{
    nChars <- nchar(as.character(var))
    
  }
  nCharsBoo <- nChars > cutoff
  x <- ifelse(nCharsBoo, var, NA)
  
  return(x)
}

########
# BARPLOT AND LABEL CATEGORICAL DISTRIBUTIONS
########
SeeBars <- function(var, cex = 0.5, border = FALSE, las = 1, cex.names = 0.75,
                    see_table = TRUE){
  
  x <- table(var)
  bp <- barplot(x, border = border, las = las, cex.names = cex.names)
  
  text(bp[,1], 0, pos=3,
       labels = x,
       cex = cex,
       col = adjustcolor("black", alpha.f= 0.75))
  
  text(bp[,1], ifelse(x < 0.1*max(x), 0.2*max(x), x),
       pos = ifelse(x > 0.9*max(x), 1, 3),
       labels = paste0(round(x/sum(x)*100, digits = 2), "%"),
       cex = cex,
       col = adjustcolor("darkred", alpha.f=0.75))
  
  if(see_table){
    return(x)
  }
  
}

########
# FUNCTION FOR MISSING
########
Missing <- function(var){
  ifelse(is.na(var), 
         NA,
         nchar(as.character(var)) == 0)
  
}

########
# ESTABLISH WHETHER VOTED OR NOT IN A PARTICULAR COLUMN
########
# VoteFun <- function(var){
#   ifelse(nchar(RemoveSpaces(var)) > 0,
#          TRUE, 
#          FALSE)
# }

########
# CREATE A FULL ADDRESS
########
MakeAddress <- function(ad,
                   city,
                   state,
                   zip){
  
  # PASTE TOGETHER ADDRESS COMPONENTS
  x <- paste0(RemoveTrail(ad), ", ",
              RemoveTrail(city), ", ",
              RemoveTrail(state), " ",
              RemoveTrail(zip))
  return(x)
}

########
# RANDOMIZE ROW ORDER OF A DATAFRAME
########
RandomRows = function(df){
  return(df[sample(nrow(df),nrow(df)),])
}

########
# VISUALIZE RESULTS OF A LOGISTIC REGRESSION MODEL IN BARPLOT WITH CI BARS
########
BarModel <- function(model, conf = 0.9, las = 3){
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

########
# CHECK DUPLICATION ON ANY VARIABLE
########
DupCheck <- function(var){
  
  x <- length(var)
  
  if(
    length(unique(var)) == length(var)
  ){
    "No duplicates"
  } else {
    "There are duplicates, dude!"
  }
}

########
# RANDOMLY SELECT HOUSEHOLD REPS AND CALCULATE N OF PEOPLE PER HOUSEHOLD
########
HouseHold <- function(data, hh_id, keep_only_reps = FALSE){
  
  # Randomize order of rows in data
  x <- RandomRows(df = data)
  
  # Create dummy variable used to give a number to each person in a household
  x$one <- 1
  
  # Calculate cumulative sum of people at that address and 
  # assign to each person a hh_rep number (1:nPeople at that address)
  x <- data.table(x)
  
  # ASSIGN AN INTEGER FOR EACH NUMBER AT AN ADDRESS
  x[, hh_rep := cumsum(one), by = hh_id] #"hh_rep" means "household person"
  
  # GET NUMBER OF PEOPLE AT EACH ADDRESS
  x[, hh_size := max(hh_rep), by = hh_id]
  
  x <- as.data.frame(x) # convert back to df object
  
  if(keep_only_reps){
    x <- x[which(x$hh_rep == 1),]  
  } 
  
  return(x) # return the new dataframe
}


#######
# RANDOMLY CUT DOWN A DATAFRAME TO A SPECIFIED SIZE
#######
CutDown <- function(data=NULL, seed=NULL, size=NULL){
  if(is.null(seed)){
    stop("You must set a seed")
  }else if(is.null(size)){
    stop("You must specify a size (number of rows you want to keep)")
  } else if(is.null(data)){
    stop("You must specify a dataframe")
  }else{
    
    nRows <- size 
    set.seed(seed)
    ind <- sample(1:nrow(data), nRows)
    data$keep <- FALSE
    data[ind, "keep"] <- TRUE
    data <- data[which(data$keep),]
    data$keep <- NULL
    return(data)
    
  }  
}

#########
# ASSIGN TREATMENT GROUPS BY STRATA
#########

AssignTreatment <- function(data=NULL, seed=NULL, strata_var=NULL){
  
  if(is.null(seed)){
    stop("You must set a seed")
  }else if(is.null(strata_var)){
    stop("You must specify a variable on which you're stratifying")
  } else if(is.null(data)){
    stop("You must specify a dataframe")
  }else{
  
  set.seed(seed)
  
  strat <- by(data, data[,strata_var], function(x){
    x$treat <- "control"
    treats <- sample(1:nrow(x), round(nrow(x) / 2))
    x[treats, "treat"] <- "treatment"
    x
  })
  
  # BIND STRAT INTO postRobo
  x <- data.frame(do.call(rbind, strat), row.names=NULL)
  # Set the "treat" variable as a categorical
  x$treat <- as.factor(x$treat)
  
  return(x)
  }
}



