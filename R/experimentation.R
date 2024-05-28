# Load necessary libraries
library(readxl)
library(dplyr)

# Step 1: Import the entire Excel sheet
data <- read_excel("C:/Users/Dan/Downloads/A2 Serology 5.15.24.xlsx")
print("Data imported successfully")

# Print the first few rows of the data to inspect it
print(head(data))

# Step 2: Manually define the rows for each data block
blocks <- list(
  IgG1_GFMI = data[2:7, ],
  IgG2a_GFMI = data[10:15, ],
  IgG2b_GFMI = data[18:23, ],
  IgG3_GFMI = data[26:31, ],
  IgA_GFMI = data[34:39, ],
  FcRn_GFMI = data[42:47, ],
  FcgR1_GFMI = data[50:55, ],
  FcgR2b_GFMI = data[58:63, ],
  FcgR3_GFMI = data[66:71, ],
  FcgR4_GFMI = data[74:79, ]
)
print("Data blocks defined successfully")

# Step 3: Function to clean each data block
clean_block <- function(block) {
  # Check if the first row is numeric, indicating it might be data instead of headers
  if(all(sapply(block[1, ], is.numeric))) {  # Check each column in the first row
    names(block) <- c("Sample", "Measurement1", "Measurement2", "Measurement3", "Measurement4", "Measurement5")
  } else {
    names(block) <- as.character(block[1, ])  # Use the first row as headers if not all columns are numeric
    block <- block[-1, ]  # Remove the header row
  }
  block[] <- lapply(block, function(x) type.convert(as.character(x), as.is = TRUE))
  return(block)
}

# Apply the cleaning function to each block and ensure blocks are correctly assigned
blocks <- lapply(blocks, clean_block)
print("Data blocks cleaned successfully")

# Step 4: Use each block for further analysis
# Example: Summary statistics of the first IgG1 GFMI block
summary(blocks$IgG1_GFMI)

# Print summaries for each block
lapply(blocks, summary)

