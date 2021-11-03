is_whole <- function(x) all(floor(x) == x, na.rm = T) # Gives TRUE if all values (except NAs) 
                                                      # for a specific column are whole numbers (and likely factors)

