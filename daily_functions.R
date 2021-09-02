# Indicate the severity of the change
# Input: dfCol - a data frame column; majThr- threshold value for major change; minThr - threshold value for minor change
# Output: scCol - a data frame column
sevChange <- function(dfCol, majThr, minThr){
  
  majorSyn <- list("drastically", "significantly", "considerably", "notably", "tremendously", "immensely")
  minorSyn <- list("slightly", "somewhat", "marginally")
  
  scCol <- abs(dfCol)
  
  sevCol <- case_when(scCol >= majThr ~ unlist(sample(majorSyn, length(scCol), replace = TRUE)),
                      scCol < majThr & scCol >= minThr ~ unlist(sample(minorSyn, length(scCol), replace = TRUE)),
                      scCol < minThr ~ "",
  )
  
  return(sevCol)
}

# Indicate the severity of the change in French
# Input: dfCol - a data frame column; majThr- threshold value for major change; minThr - threshold value for minor change
# Output: scCol - a data frame column
sevChangeFr <- function(dfCol, majThr, minThr){
  
  minorSyn <- list("légèrement", "quelque peu", "plutôt", "un peu")
  majorSyn <- list("de façon drastique", "radicalement", "significativement", "considérablement", "remarquablement", "visiblement", "énormément")
  
  scCol <- abs(dfCol)
  
  sevCol <- case_when(scCol >= majThr ~ unlist(sample(majorSyn, length(scCol), replace = TRUE)),
                      scCol < majThr & scCol >= minThr ~ unlist(sample(minorSyn, length(scCol), replace = TRUE)),
                      scCol < minThr ~ "",
  )
  
  return(sevCol)
}

# Determine increase/decrease/no change
# Input: dfCol - a data frame column
# Output: change - a data frame column
change <- function(dfCol) {
  
  incSyn <- list("increased", "grew", "rose", "escalated", "elevated")
  decSyn <- list("decreased", "reduced", "dropped", "diminished", "declined")
  constSyn <- list("did not change", "remained constant", "remained the same", "stayed constant", "stayed the same")
  
  change <- case_when(dfCol > 0 ~ unlist(sample(incSyn, length(dfCol), replace = TRUE)),
                      dfCol < 0 ~ unlist(sample(decSyn, length(dfCol), replace = TRUE)),
                      dfCol == 0 ~ unlist(sample(constSyn, length(dfCol), replace = TRUE))
  )
  return(change)
}

# Determine increase/decrease/no change in French
# Input: dfCol - a data frame column
# Output: change - a data frame column
changeFr <- function(dfCol) {
  
  incSyn <- list("plus élevé", "accru", "a augmenté", "est monté", "a fait remonter")
  decSyn <- list("réduit", "diminué", "baissé", "a décliné")
  constSyn <- list("n'a pas changé", "est resté constant", "est resté le même")
  
  change <- case_when(dfCol > 0 ~ unlist(sample(incSyn, length(dfCol), replace = TRUE)),
                      dfCol < 0 ~ unlist(sample(decSyn, length(dfCol), replace = TRUE)),
                      dfCol == 0 ~ unlist(sample(constSyn, length(dfCol), replace = TRUE))
  )
  return(change)
}

# Take a percentage and convert it to the nearest ratio in words
# Input: x - numeric value in the range [0, 1]
# Output: ratio - a string
prct_to_ratio <- function(x) {
  
  # Round to the nearest .05
  rx <- round(x/.05)*.05
  
  # Determine the closeness
  closeness <- case_when(x > rx ~ "more than",
                         x < rx ~ "nearly",
                         x == rx ~ "")
  # Create the fraction
  frac <- MASS:::.rat(rx)$rat
  
  # Create the ratio
  ratio <- paste(closeness, frac[1], "out of", frac[2])
  
  return(ratio)
}

# Take a percentage and convert it to the nearest ratio in words in French
# Input: x - numeric value in the range [0, 1]
# Output: ratio - a string
prct_to_ratio_fr <- function(x) {
  
  # Round to the nearest .05
  rx <- round(x/.05)*.05
  
  # Determine the closeness
  closeness <- case_when(x > rx ~ "plus que",
                         x < rx ~ "preque",
                         x == rx ~ "")
  # Create the fraction
  frac <- MASS:::.rat(rx)$rat
  
  # Create the ratio
  ratio <- paste(closeness, frac[1], "sur", frac[2])
  
  return(ratio)
}

# Enumerate a list. Concatenate list into a list separated by commas with the word "and" before the last word.
# Input: l - a list
# Output: enum - a string
enumerate <- function(l){
  
  # If the list is longer than 2
  if (length(l) > 2){
    
    # Assign the first word to p
    enum <- l[1]
    
    # For every word from the 2nd word to the 2nd last
    for (i in 2:(length(l)-1)){
      
      # Update P by adding the next item separated by a comma and space
      enum <- paste(enum, l[i], sep = ", ")
    }
    
    # Finally, add the last word with "and" in front of it
    enum <- paste(enum, l[length(l)], sep = ", and ")
    
    # Return enum
    return(enum)
    
    # Else if the list has 2 words
  } else if (length(l) == 2) {
    
    # Return the 1st item and 2nd item with the word "and" in between them
    return(paste(l[1], l[2], sep = " and "))
    
    # Else, the list is length 0 or 1
  } else {
    
    # Return the first item in the list
    # If it is length 0, it will return NULL
    return(l[1])
  }
}

# Enumerate a list. Concatenate list into a list separated by commas with the word "and" before the last  in French
# Input: l - a list
# Output: enum - a string
enumerateFr <- function(l){
  
  # If the list is longer than 2
  if (length(l) > 2){
    
    # Assign the first word to p
    enum <- l[1]
    
    # For every word from the 2nd word to the 2nd last
    for (i in 2:(length(l)-1)){
      
      # Update P by adding the next item separated by a comma and space
      enum <- paste(enum, l[i], sep = ", ")
    }
    
    # Finally, add the last word with "et" in front of it
    enum <- paste(enum, l[length(l)], sep = " et ")
    
    # Return enum
    return(enum)
    
    # Else if the list has 2 words
  } else if (length(l) == 2) {
    
    # Return the 1st item and 2nd item with the word "and" in between them
    return(paste(l[1], l[2], sep = " et "))
    
    # Else, the list is length 0 or 1
  } else {
    
    # Return the first item in the list
    # If it is length 0, it will return NULL
    return(l[1])
  }
}

# Get the major contributing factors
# Input: df - a data frame; fCol - factor column, str; pctCol - percentages column, str; thr - threshold percent, float
# Output: List of major contributing factors
get_cf <- function(df, fCol, pctCol, thr){
  
  # Empty list to store the contributing factors
  cf <- list()
  
  # Initialize sum to be 0
  sum = 0
  
  # For each record
  for (i in 1:nrow(df)){
    
    # If the sum is less than the threshold
    if (sum < thr){
      
      # Append the contributing factor
      cf[[i]] <- df[[i, fCol]]
      
      # Add the value of the contributing factor to sum
      sum = sum + df[[i, pctCol]]
      
      # Else, exit the loop
    } else {
      break
    }
  }
  
  # If the length of the contributing factors list is greater than or equal to half of the number of total contributing factors
  if (length(cf) >= 0.5*nrow(df)){
    
    # Return only the top 3 contributors
    cf <- cf[1:3]
  }
  
  # Return cf
  return(cf)
}


# Generate the paragraph for contributing factors
# Input: df - a data frame
# Output: pgh - a string
cf_pgh <- function(df){
  
  # Copy df
  d <- df
  
  # Store the original number of columns
  numCols <- ncol(df)
  
  # For each column, get the change
  for (i in 1:ncol(d)){
    d[paste0(colnames(d)[i], "_change")] <- change(d[, i])
  }
  
  # For each factor, calculate its contribution percentage
  for (j in 2:numCols){
    d[paste0(colnames(d)[j], "_pct")] <- dplyr::case_when(df[, 1] != 0 ~ df[, j]/df[, 1] * 100,
                                                          df[, 1] == 0 ~ df[, j])
  }
  
  # Rename the column names (Replace all non alpha-numeric characters with a space)
  names(d)[1:numCols] <- str_replace_all(names(d)[1:numCols], "[^[:alnum:]]", " ")
  
  # Capitalize the first word
  name <- names(d)[1]
  names(d)[1] <- paste0(toupper(substr(name, 1, 1)), substr(name, 2, nchar(name)))
  
  # Print to confirm with the sentence(s)
  print(d)
  
  # Store the sentences
  pgh_list <- list()
  
  # Store total sales change
  for (k in 1:nrow(d)){
    p <- paste(colnames(d)[1], d[k, colnames(d)[numCols+1]], "as")
    
    # Empty list to store contributing factors and their change
    cf_list = list()
    
    # For each factor
    for (l in 2:numCols){
      
      # Add each contributing factor to a list in the format of its name, whether or not to display its 
      # contribution percentage, and the absolute value of its difference between periods
      
      cf_list[[l-1]] <- str_glue("{colnames(d)[l]} {d[k, colnames(d)[numCols+l]]} {case_when(d[k, 1] != 0 & d[k, numCols*2+l-1] != 0 ~ paste0(\"by \", abs(round(d[k, numCols*2+l-1], 2)), \"%\"), d[k, 1] == 0 | d[k, numCols*2+l-1] == 0 ~ \"\")} ({abs(d[k, l])})")
      
    }
    
    # Combine p with the enumerated to create the sentence
    pgh <- str_squish(str_glue("{p} {enumerate(cf_list)}."))
    
    # Make a list of these sentences
    pgh_list[k] <- pgh
  }
  
  # Build the paragraph
  # pgh <- paste(pgh, pgh_list)
  
  # Remove extra spaces
  pgh <- str_squish(pgh_list)
  
  # Return the paragraph
  return(pgh)
}


# Generate the paragraph for contributing factors in French
# Input: df - a data frame
# Output: pgh - a string
cf_pgh_Fr <- function(df){
  
  # Copy df
  d <- df
  
  # Store the original number of columns
  numCols <- ncol(df)
  
  # For each column, get the change
  for (i in 1:ncol(d)){
    d[paste0(colnames(d)[i], "_change")] <- change(d[, i])
  }
  
  # For each factor, calculate its contribution percentage
  for (j in 2:numCols){
    d[paste0(colnames(d)[j], "_pct")] <- dplyr::case_when(df[, 1] != 0 ~ df[, j]/df[, 1] * 100,
                                                          df[, 1] == 0 ~ df[, j])
  }
  
  # Rename the column names (Replace all non alpha-numeric characters with a space)
  names(d)[1:numCols] <- str_replace_all(names(d)[1:numCols], "[^[:alnum:]]", " ")
  
  # Capitalize the first word
  name <- names(d)[1]
  names(d)[1] <- paste0(toupper(substr(name, 1, 1)), substr(name, 2, nchar(name)))
  
  # Print to confirm with the sentence(s)
  print(d)
  
  # Store the sentences
  pgh_list <- list()
  
  # Store total sales change
  for (k in 1:nrow(d)){
    p <- paste(colnames(d)[1], d[k, colnames(d)[numCols+1]], "comme")
    
    # Empty list to store contributing factors and their change
    cf_list = list()
    
    # For each factor
    for (l in 2:numCols){
      
      # Add each contributing factor to a list in the format of its name, whether or not to display its 
      # contribution percentage, and the absolute value of its difference between periods
      
      cf_list[[l-1]] <- str_glue("{colnames(d)[l]} {d[k, colnames(d)[numCols+l]]} {case_when(d[k, 1] != 0 & d[k, numCols*2+l-1] != 0 ~ paste0(\"par \", abs(round(d[k, numCols*2+l-1], 2)), \"%\"), d[k, 1] == 0 | d[k, numCols*2+l-1] == 0 ~ \"\")} ({abs(d[k, l])})")
      
    }
    
    # Combine p with the enumerated to create the sentence
    pgh <- str_squish(str_glue("{p} {enumerateFr(cf_list)}."))
    
    # Make a list of these sentences
    pgh_list[k] <- pgh
  }
  
  # Build the paragraph
  # pgh <- paste(pgh, pgh_list)
  
  # Remove extra spaces
  pgh <- str_squish(pgh_list)
  
  # Return the paragraph
  return(pgh)
}


# R does not have a built-in function to caclulate mode.
# Define the mode function
# Input: x, a list
# Output: the mode
get_mode <- function(x) {
  
  # Get the unique values
  ux <- unique(x)
  
  # Find the max number of occurances per each value. Return it.
  return(ux[which.max(tabulate(match(x, ux)))])
}


# Define the unit of measure
# Alternative: Make str_replace calls
# Input: u - unit of measutre as string; fw - first word; lw - last word
# Output: New unit of measure
unitMeasure <- function(u, fw, lw){
  
  # Convert the inputs to character type
  f <- as.character(fw)
  l <- as.character(lw)
  
  # Replace Rate
  um <- str_replace(u, "Rate", f)
  
  # Replace population
  um <- str_replace(um, "population", l)
  
  return(um)
}


# Define the unit of measure in French
# Alternative: Make str_replace calls
# Input: u - unit of measutre as string; fw - first word; lw - last word
# Output: New unit of measure
unitMeasureFr <- function(u, fw, lw){
  
  # Convert the inputs to character type
  f <- as.character(fw)
  l <- as.character(lw)
  
  # Replace Rate
  um <- str_replace(u, "Taux", f)
  
  # Replace population
  um <- str_replace(um, "personnes", l)
  
  return(um)
}


# Compare 2 groups and specify which had a greater value
# Input: a - a numeric value; b - another numeric value; str1 - a string; str2 - another string
# Output: list of 2 in the descending order to indicate which group had a greater value
gr <- function(a, b, str1, str2){
  
  gr <- list()
  
  if (a >= b) {
    
    gr[1] <- str1
    gr[2] <- str2
    
    return(gr)
    
  } else {
    
    gr[1] <- str2
    gr[2] <- str1
    
    return(gr)
  }
}


# Calculate percent difference
# Input: a - a numeric value; b - another numeric value
# Output: The percent difference
pct_dif <- function(a, b){
  
  # Return the percent difference
  return((a-b)/a*100)
}

# Conjugate regular French verbs
# Input: v - verb (Must be a REGULAR -ER, -IR, -RE verb; character type), 
#        q - quantity (default: 0 - singular, 1 - plural), 
#        t - tense (default: 0 - présent, 1 - passé composée, 2 - imparfait, 3 - futur simple, 4 - conditionnel)
# Output: Conjugated verb
conjugateFr <- function(v, q = 0, t = 0){
  
  # If it is an -ER verb
  if (str_sub(v, -2, -1) == "er"){
    
    # If the subject is singular
    if (q == 0){
      
      # Check tense, otherwise return infinitive
      return(case_when(t == 0 ~ str_replace(v, str_sub(v, -2, -1), "e"),
                       t == 1 ~ paste("a", str_replace(v, str_sub(v, -2, -1), "é")),
                       t == 2 ~ str_replace(v, str_sub(v, -2, -1), "ait"),
                       t == 3 ~ paste0(v, "a"),
                       t == 4 ~ paste0(v, "ait"),
                       TRUE ~ v)
      )
      
      # Else, the subject is plural
    } else {
      
      # Check tense, otherwise return infinitive
      return(case_when(t == 0 ~ str_replace(v, str_sub(v, -2, -1), "ent"),
                       t == 1 ~ paste("ont", str_replace(v, str_sub(v, -2, -1), "é")),
                       t == 2 ~ str_replace(v, str_sub(v, -2, -1), "aient"),
                       t == 3 ~ paste0(v, "ont"),
                       t == 4 ~ paste0(v, "aient"),
                       TRUE ~ v)
      )
      
    }
    
    # Else if it is an -IR verb
  } else if (str_sub(v, -2, -1) == "ir"){
    
    # If the subject is singular
    if (q == 0){
      
      # Check tense, otherwise return infinitive
      return(case_when(t == 0 ~ str_replace(v, str_sub(v, -2, -1), "it"),
                       t == 1 ~ paste("a", str_replace(v, str_sub(v, -2, -1), "i")),
                       t == 2 ~ str_replace(v, str_sub(v, -2, -1), "issait"),
                       t == 3 ~ paste0(v, "a"),
                       t == 4 ~ paste0(v, "ait"),
                       TRUE ~ v)
      )
      
      # Else, the subject is plural
    } else {
      
      # Check tense, otherwise return infinitive
      return(case_when(t == 0 ~ str_replace(v, str_sub(v, -2, -1), "issent"),
                       t == 1 ~ paste("ont", str_replace(v, str_sub(v, -2, -1), "i")),
                       t == 2 ~ str_replace(v, str_sub(v, -2, -1), "issaient"),
                       t == 3 ~ paste0(v, "ont"),
                       t == 4 ~ paste0(v, "aient"),
                       TRUE ~ v)
      )
      
    }
    
    # Else if it is an -RE verb
  } else if (str_sub(v, -2, -1) == "re"){
    
    # If the subject is singular
    if (q == 0){
      
      # Check tense, otherwise return infinitive
      return(case_when(t == 0 ~ str_replace(v, str_sub(v, -2, -1), ""),
                       t == 1 ~ paste("a", str_replace(v, str_sub(v, -2, -1), "u")),
                       t == 2 ~ str_replace(v, str_sub(v, -2, -1), "ait"),
                       t == 3 ~ str_replace(v, str_sub(v, -2, -1), "ra"),
                       t == 4 ~ str_replace(v, str_sub(v, -2, -1), "rait"),
                       TRUE ~ v)
      )
      
      # Else, the subject is plural
    } else {
      
      # Check tense, otherwise return infinitive
      return(case_when(t == 0 ~ str_replace(v, str_sub(v, -2, -1), "ent"),
                       t == 1 ~ paste("ont", str_replace(v, str_sub(v, -2, -1), "u")),
                       t == 2 ~ str_replace(v, str_sub(v, -2, -1), "aient"),
                       t == 3 ~ str_replace(v, str_sub(v, -2, -1), "ront"),
                       t == 4 ~ str_replace(v, str_sub(v, -2, -1), "raient"),
                       TRUE ~ v)
      )
      
    }
    
    # Else it is none
  } else {
    return("The word was not an -ER, -IR, or -RE verb. Le mot n'était pas un verbe -ER, -IR ou -RE")
  }
}