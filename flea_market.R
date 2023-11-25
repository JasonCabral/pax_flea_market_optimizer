library(rvest)
library(dplyr)
source('helper.R')

# Pull sale information as html
flea <- read_html("https://www.yulgame.com/Vfm/Items/321225?Status=Any&Sort=Name")

# Create table from html data
table_header <- html_element(flea, "thead") %>% 
  html_table() %>% tail(1) %>% as.list()

table <- html_element(flea, "tbody") %>% 
  html_table()

colnames(table) <- table_header
table <- table %>% dplyr::filter(Seller != '(n/a)', Seller != '')

# Unique Sellers that listed
listing_sellers <- table %>% 
  dplyr::select(Seller) %>% pull() %>% 
  unique() %>% base::sort() %>% tolower()
length(listing_sellers)
# 281 unique Sellers (listing)

# Unique Sellers with a sold item
sellers_sold <- table %>% dplyr::filter(Buyer!= '') %>% 
  dplyr::select(Seller) %>% pull() %>% 
  unique() %>% 
  base::sort() %>% tolower()
length(sellers_sold)
# 206 unique Sellers with sold items

# Non-Unique Sellers of all sold items (for weighting)
sellers_all_sold <- table %>% dplyr::filter(Buyer!= '') %>% 
  dplyr::select(Seller) %>% pull() %>% 
  base::sort() %>% tolower()

# Unique Buyers
buyers <- table$Buyer[!table$Buyer %in% c('(n/a)','')] %>% 
  unique() %>% sort() %>% tolower()
length(buyers)
# 241 unique buyers

# Additional Stats
length(unique(c(buyers, sellers_sold)))
# 315 unique participants

both <- intersect(sellers_sold, buyers)
# 120 attendees buying and selling


num_groups <- 10

# Option 1: Optimize for each seller getting equal space
seller_splits <- split(sellers_sold, substr(sellers_sold,1,1))

max_overage <- optimal_overage(num_groups = num_groups,
                               seller_splits = seller_splits,
                               sellers_list = sellers_sold)

final_groups <- make_groups(num_groups = num_groups,
                            seller_splits = seller_splits,
                            sellers_list = sellers_sold,
                            max_overage = max_overage)


letter_splits <- sapply(final_groups, function(group){unique(substr(group,1,1))})
letter_splits

# Option 2: Optimize while weighting for number of games sold
cut_divisor <- 10 # Max number of games before allocating more space
weighted_seller_count <- ceiling(table(sellers_all_sold)/cut_divisor)
weighted_sellers <- rep(names(weighted_seller_count),
                        times = weighted_seller_count)
weighted_seller_splits <- split(weighted_sellers, substr(weighted_sellers,1,1))

max_overage <- optimal_overage(num_groups = num_groups,
                               seller_splits = weighted_seller_splits,
                               sellers_list = weighted_sellers)

final_groups <- make_groups(num_groups = num_groups,
                            seller_splits = weighted_seller_splits,
                            sellers_list = weighted_sellers,
                            max_overage = max_overage)


letter_splits <- sapply(final_groups, function(group){unique(substr(group,1,1))})
letter_splits
