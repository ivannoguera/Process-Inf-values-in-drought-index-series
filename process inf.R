#' Process Inf values and outliers in a standardized drought index series
#'
#' This function processes a standardized drought index series by:
#' 1. Optionally truncating outliers based on a specified range of values.
#' 2. Replacing `Inf` and `-Inf` values according to the 99th and 1st percentiles.
#'
#' @param series Numeric vector representing the drought index series.
#' @param frequency Integer. Number of observations per year (e.g., 12 for monthly series). Default is 12.
#' @param range Numeric or NULL. If specified, finite values higher than `range` are set to `range`
#'   and values lower than `-range` are set to `-range`. Default is NULL.
#'
#' @return Numeric vector with processed series, where Inf/-Inf are replaced and optional truncation is applied.
#'
#' @examples
#' # Process monthly series with truncation at +/-4
#' process_inf(series, frequency = 12, range = 4)
#'
#' @export
process_inf <- function(series, frequency = 12, range = NULL) {
  
  # --------------------------
  # Input validation
  # --------------------------
  if (!is.numeric(series)) stop("`series` must be a numeric vector.")
  if (!is.numeric(frequency) || length(frequency) != 1 || frequency <= 0) {
    stop("`frequency` must be a single positive number.")
  }
  if (!is.null(range)) {
    if (!is.numeric(range) || length(range) != 1 || range <= 0) {
      stop("`range` must be a single positive number or NULL.")
    }
  }
  
  n <- length(series)
  
  # --------------------------
  # Step 1: Optional truncation
  # --------------------------
  if (!is.null(range)) {
    finite_idx <- is.finite(series)
    series[finite_idx & series > range]  <- range
    series[finite_idx & series < -range] <- -range
  }
  
  # --------------------------
  # Step 2: Replace Inf/-Inf
  # --------------------------
  for (i in seq_len(frequency)) {
    idx <- seq(i, n, by = frequency)
    sub <- series[idx]
    
    sub_finite <- sub[is.finite(sub)]
    
    if (length(sub_finite) > 0) {
      p1  <- quantile(sub_finite, 0.01, na.rm = TRUE)
      p99 <- quantile(sub_finite, 0.99, na.rm = TRUE)
      
      sub[is.infinite(sub) & sub > 0] <- p99
      sub[is.infinite(sub) & sub < 0] <- p1
    }
    
    series[idx] <- sub
  }
  
  return(series)
}



