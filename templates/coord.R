# Coordinated the folder, file creation and template copy processes

# Source: https://nrennie.rbind.io/blog/script-templates-r/
# Reference
# Creating template files with R.
# Nicola Rennie. August 22, 2023.
# nrennie.rbind.io/blog/script-templates-r

# Date for the new project
date_chr <- "2025-01-14"
yr <- sub("-.*", "", date_chr)

# Create the new folder
new_folder <- file.path(yr, date_chr)
if (!file.exists(new_folder)) {
  dir.create(new_folder, recursive = TRUE)
  message("New folder created succesfully!")
}

# Create the data folder
data_folder <- file.path(yr, date_chr, "data")
if (!file.exists(data_folder)) {
  dir.create(data_folder, recursive = TRUE)
  message("New folder created succesfully!")
}

# Create the code.R file
code_file <- file.path(yr, date_chr, "code.R")
if (!file.exists(code_file)) {
  file.create(code_file)
  message("'code.R' file created successfully!")
}

# Create the README.md file
readme_file <- file.path(yr, date_chr, "README.md")
if (!file.exists(readme_file)) {
  file.create(readme_file)
  message("Created 'README.md' file")
}
 
# Copy lines to README file
readme_txt <- readLines("templates/readme-template.md")

# Replace placeholder text with variables
readme_txt <- gsub(pattern = "yr", replacement = yr, x = readme_txt)
readme_txt <- gsub(pattern = "date_chr", replacement = date_chr, x = readme_txt)

# write to file
writeLines(readme_txt, con = readme_file)
message("'README.md' contents successfully copied!")

# Copy lines to code.R file
r_txt <- readLines("templates/code-template.R")

# replace placeholder text with variables
r_txt <- gsub(pattern = "yr", replacement = paste0("\"", yr, "\""), x = r_txt)
r_txt <- gsub(pattern = "date_chr", replacement = paste0("\"", date_chr, "\""), x = r_txt)

# write to new file
writeLines(r_txt, con = code_file)
message("'.R' contents successfully copied!")
