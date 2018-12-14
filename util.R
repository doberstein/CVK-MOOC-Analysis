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
    
    # Task4
    df$condition[df$group_id %in% c(230:235)] <- "NA"
    df$condition[df$group_id %in% c(237:242)] <- "WA"
  }
  
  df
}

addTask <- function(df) {
  if (nrow(df) > 0) {
    df$task <- NA
    
    # Task1
    df$task[df$group_id %in% c(154:174)] <- "task1"
    
    # Task2
    df$task[df$group_id %in% c(187:202)] <- "task2"
    
    # Task3
    df$task[df$group_id %in% c(211:224)] <- "task3"
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

# calculate activity count for each group for sub-sequence
calculateUserActivityCount <- function(df) {
  df <- df %>% 
    group_by(group_id, user_id) %>% 
    summarise(activity_count = n())
  df
}

addValues <- function (df1, df2, id) {
  df2$period <- id
  rbind(df1, df2)
}



# df2 <- df
# df2$new_rank <- cut(df$activity_count, length(unique(df$activity_count)), labels = F)
# 
# 
# df2 <- df
# current_rank <- 1
# rank_counter <- 1
# #i = 1
# df2$new_rank <- NA
# df2 <- df2 %>% 
#   arrange(desc(activity_count))
# df2[1,]$new_rank <- current_rank
# for (i in 2:nrow(df2)) {
#   if (df2[i,]$activity_count == df2[i-1,]$activity_count) {
#     # keep same rank
#     df2[i,]$new_rank <- current_rank
#     rank_counter <<- rank_counter + 1
#   } else {
#     # increase rank
#     rank_counter <- rank_counter + 1
#     current_rank <<- rank_counter
#     df2[i,]$new_rank <- current_rank
#   }
# } 
# View(df2)


#df2$new_rank <- rank(df$activity_count)

# length(unique(df$activity_count))

# ranking based on overall standing (1:n) where activitiy_count decides the rank and same activity_count results in the same rank
addActivityRank <- function(data1) {
  browser()
  current_rank <- 1
  rank_counter <- 1
  data1$rank <- NA
  browser()
  data1 <- data1 %>% 
    arrange(desc(activity_count))
  data1[1,]$rank <<- current_rank
  for (i in 2:nrow(data1)) {
    if (data1[i,]$activity_count == data1[i-1,]$activity_count) {
      # keep same rank
      data1[i,]$rank <- current_rank
      rank_counter <- rank_counter + 1
    } else {
      # increase rank
      rank_counter <- rank_counter + 1
      current_rank <- rank_counter
      data1[i,]$rank <- current_rank
    }
   # browser()
  } 
  #browser()
  
  data1
}


addActivityRank2 <- function(df) {
  current_rank <- 1
  rank_counter <- 1
  df$rank <- NA
  #browser()
  df <- df %>% 
    arrange(desc(activity_count))
  df[1,]$rank <- current_rank
  if(nrow(df) > 1) {
    for (i in 2:nrow(df)) {
      if (df[i,]$activity_count == df[i-1,]$activity_count) {
        # keep same rank
        df[i,]$rank <- current_rank
        rank_counter <- rank_counter + 1
      } else {
        # increase rank
        rank_counter <- rank_counter + 1
        current_rank <- rank_counter
        df[i,]$rank <- current_rank
      }
      # browser()
    }
  }
   
  #browser()
  
  df
}


# normalize a rank to range betwee 0 (best) and 1 (worst)
# The general one-line formula to linearly rescale data values having observed min and max into a new arbitrary range min' to max' is
# newvalue= (max'-min')/(max-min)*(value-max)+max'
#   or
# newvalue= (max'-min')/(max-min)*(value-min)+min'.
normalizeRank <- function (df) {
  df <- df %>%
    mutate( norm_rank =  round(    (1-0) / (max(df$rank) - min(df$rank)) * (rank - max(df$rank)) + 1  , digits = 2 )    )
  
  df
}


addActivityRankingClass <- function (df) {
  
### CLASSIFY INTO 3 CATEGORIES
### ADDING RANK BASED ON ACTIVITY COUNT
### RANK1: TOP THIRD
### RANK2: MEDIUM THIRD
### RANK3: BOTTOM THIRD
  df$activity_rank <- cut(df$activity_count, 3, labels = F)

  df
  
}
