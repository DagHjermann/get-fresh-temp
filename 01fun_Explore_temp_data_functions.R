create_sequence_list <- function (param_values, values_are_text = FALSE) {
  sel_na <- is.na(param_values)
  param_values <- param_values[!sel_na]
  if (sum(sel_na) > 0) 
    warning(paste("Supplied values contained", sum(sel_na), 
                  "missing value(s)"))
  L <- length(param_values)
  sq1 <- seq(1, L, 1000)
  sq2 <- c(seq(1, L, 1000)[-1] - 1, L)
  sequence_list <- vector("list", length(sq1))
  if (values_are_text)
    old.o <- options(useFancyQuotes = FALSE)
  for (i in seq_along(sq1)) {
    pick_sequence <- seq(sq1[i], sq2[i])
    if (!values_are_text) {
      sequence_list[[i]] <- paste0(param_values[pick_sequence], collapse = ",")
    }
    else {
      sequence_list[[i]] <- paste0(sQuote(param_values[pick_sequence]), 
                         collapse = ",")
    }
  }
  if (values_are_text)
    options(old.o)
  sequence_list
}

# Tests:
# create_sequence_list(5:15)
# create_sequence_list(letters[1:10], values_are_text = TRUE)
# test <- create_sequence_list(1:2005)
# str(test, 1)
