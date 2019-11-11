### UTIL
# add conditions to functions
addConditionsCVK2 <- function(df) {
  if (nrow(df) > 0) {
    df$condition <- NA
    
    # Task1
    df$condition[df$group_id %in% c(154:159)] <- "Ko"
    df$condition[df$group_id %in% c(160:165)] <- "M"
    df$condition[df$group_id %in% c(166:171)] <- "MG"
    df$condition[df$group_id %in% c(172:174)] <- "KR"
    
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
    
    # Task4
    df$task[df$group_id %in% c(230:242)] <- "task4"
    
    # Task5
    df$task[df$group_id %in% c(247:259)] <- "task5"
    
    # Task6
    df$task[df$group_id %in% c(265:277)] <- "task6"
  }
  
  df
}