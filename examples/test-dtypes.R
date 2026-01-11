library(reticulate)

# Test with your actual data structure
library(dplyr)

# Simulate the data structure you're using
# b |> dplyr::select(-MOS) |> dplyr::slice(1)

# Let's check the data types
pd <- import("pandas")

# Create test data
test_data <- data.frame(
  feature1 = c(1.0, 2.0),
  feature2 = c(3.5, 4.5),
  stringsAsFactors = FALSE
)

# Convert to double explicitly
for (col in names(test_data)) {
  if (is.numeric(test_data[[col]])) {
    test_data[[col]] <- as.double(test_data[[col]])
  }
}

# Create pandas DataFrame
test_data_py <- pd$DataFrame(test_data)

# Check dtypes
cat("Pandas DataFrame dtypes:\n")
print(test_data_py$dtypes)
cat("\n")

# Check individual column types
for (col in names(test_data)) {
  col_py <- test_data_py[[col]]
  cat("Column", col, "- type:", class(col_py), ", dtype:", col_py$dtype, "\n")
}
