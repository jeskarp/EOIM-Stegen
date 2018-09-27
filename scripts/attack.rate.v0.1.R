attack.rate <- function(table) {
  prop <- round(prop.table(table,1),digits = 2)
  denominator <- rowSums(table) 
  output <- cbind(Ill = table[,2], N = denominator, Proportions = prop[,2])
  return(output)
}

# Function to provide counts, denominator and proportions (equivalent of attack rate)
attack_rate <- function(exposure, outcome, data, rowcol = "cols") {
  #create an empty list to store results
  output <- list()
  #for each variable named in exposure
  for (var in exposure) {
    counts <- table(data[[var]], data[[outcome]] )
    if (rowcol == "cols") {
      #get column proportions
      prop <- round(prop.table(counts, 1) * 100, digits = 2)
      #get row totals
      denominator <- rowSums(counts)[2]
      #pull counts together
      intermediate <- cbind(Ill = counts[2, ], N = denominator, Proportions = prop[2, ])
    }
    if (rowcol == "rows") {
      #get column proportions
      prop <- round(prop.table(counts, 2) * 100, digits = 2)
      #get column totals
      denominator <- colSums(counts)[2]
      #pull counts together
      intermediate <- cbind(Exposed = counts[ , 2],
                            N = denominator, Proportions = prop[ , 2])
    }
    if (nrow(counts) > 2) {
      #get column proportions
      prop <- round(prop.table(counts, 1) * 100, digits = 2)
      #get row totals
      denominator <- rowSums(counts)
      #pull counts together
      intermediate <- cbind(Ill = counts[ , 2], N = denominator, Proportions = prop[ , 2])
    }
    #store your output table in the list
    output[[var]] <- intermediate
  }
  return(output)
}