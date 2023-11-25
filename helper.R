# Collapse collections of sellers organized by first letter into a given number of target groups
make_groups <- function(num_groups=10, seller_splits, sellers_list, max_overage = 5) {
  
  rough_size <- round(length(sellers_list)/num_groups)
  
  groups_by_name <- vector("list", length = num_groups)
  
  seller_index <- 1
  for(i in 1:num_groups){
    if (seller_index <= length(seller_splits)){
      group <- seller_splits[[seller_index]]
      while(length(group) < rough_size & seller_index < length(seller_splits)){
        if (length(group) + length(seller_splits[[seller_index+1]]) > rough_size + max_overage){
          break
        }
        seller_index <- seller_index + 1
        group <- c(group, seller_splits[[seller_index]])
        
      }
      groups_by_name[[i]] <- group
    }
    seller_index <- seller_index + 1
  }
  return(groups_by_name)
}

# Optimize overage for best group split
optimal_overage <- function(num_groups, seller_splits, sellers_list){
  
  optimal <- c(DIFF = 10000, STDEV = 500)
  best_diff <- 0
  best_stdv <- 0
  
  for(i in 1:50){
    groups <- make_groups(num_groups = num_groups,
                          seller_splits = seller_splits,
                          sellers_list = sellers_list,
                          max_overage = i)
    
    if (identical(unlist(groups), sellers_list)){
      counts <- sapply(groups, length)
      diff <- max(counts)-min(counts)
      stdv <- sd(counts)
      message('Overage #',i,': Diff - ',diff,', St Dev - ',stdv)
      
      if (diff < optimal[1][[1]]){
        optimal <- setNames(c(diff, optimal[2][[1]]),c(paste0("Overage #",i), names(optimal[2])))
        best_diff <- i
      }
      
      if (stdv < optimal[2][[1]]){
        optimal <- setNames(c(optimal[1][[1]], stdv),c(names(optimal[1]),paste0("Overage #",i)))
        best_stdv <- i
      }
      
    } else {
      message('Overage #',i,': Sellers Missing')
    }
  }
  message("")
  message("Optimization Results for ", num_groups, " Groups:")
  message("Lowest Difference Between High and Low Group Sizes - ", optimal[1][[1]], " (",names(optimal[1]),")")
  message("Lowest Standard Deviation Among Group Sizes - ", optimal[2][[1]], " (",names(optimal[2]),")")
  
  if(best_diff == best_stdv){
    return(best_diff)
  } else {
    return(min(best_stdv,best_diff))
  }
}