### UTIL
# add conditions to functions
addConditions <- function(df) {
  if (nrow(df) > 0) {
    df$condition <- NA
    
    # Task1
    df$condition[df$group_id %in% c(154:159)] <- "Ko"
    df$condition[df$group_id %in% c(160:165)] <- "M"
    df$condition[df$group_id %in% c(166:171)] <- "MG"
    df$condition[df$group_id %in% c(172:174)] <- "KR"
    
    # Task2
    df$condition[df$group_id %in% c(187:192)] <- "Ko"
    df$condition[df$group_id %in% c(193:197)] <- "M"
    df$condition[df$group_id %in% c(198:202)] <- "MG"
    
    # Task3
    df$condition[df$group_id %in% c(211:215)] <- "Ko"
    df$condition[df$group_id %in% c(216:219)] <- "M"
    df$condition[df$group_id %in% c(221:224)] <- "MG"
  }
  
  df
}

# calculate wordcount for each user for sub-sequence
calculateWordcount <- function(df) {
  df <- df %>% 
    group_by(group_id, user_id) %>% 
    summarise(wordcount = sum(wordcount))
  df
}

# calculate activity count for each group for sub-sequence
calculateActivityCount <- function(df) {
  df <- df %>% 
    group_by(group_id) %>% 
    summarise(activity_count = n())
  df
}

addValues <- function (df1, df2, id) {
  df2$period <- id
  rbind(df1, df2)
}