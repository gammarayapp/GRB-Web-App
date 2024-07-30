# process_data.R
args <- commandArgs(trailingOnly = TRUE)
input_file <- args[1]
output_file <- args[2]

# Example: Read the file and simply write it back out
data <- read.csv(input_file, header = TRUE)
write.csv(data, file = output_file, row.names = FALSE)