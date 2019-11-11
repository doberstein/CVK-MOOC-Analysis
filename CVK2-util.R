### UTIL
# add conditions to functions
addConditionsCVK2 <- function(df) {
  if (nrow(df) > 0) {
    df$condition <- NA
    
    # M_H:    493 - 498
    # M_H_T:  500 - 505
    # O_H:    486 - 491
    
    # Task1
    df$condition[df$group_id %in% c(493:498)] <- "M_H"
    df$condition[df$group_id %in% c(500:505)] <- "M_H_T"
    df$condition[df$group_id %in% c(486:491)] <- "O_H"
    
  }
  
  df
}

addTaskCVK2 <- function(df) {
  if (nrow(df) > 0) {
    df$task <- NA
    
    # Task1
    df$task[df$group_id %in% c(486:505)] <- "task1"
  }
  
  df
}
