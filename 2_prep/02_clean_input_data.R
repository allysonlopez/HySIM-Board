# 2_prep/02_clean_input_data

validation_no_missing_value <- function(data_file, file_name){
  missing_columns <- names(data_file)[colSums(is.na(data_file)) > 0]
  
  if (length(missing_columns) > 0){
    print(
      paste0(file_name,
             " dataset has missing value(s) in: ",
            #missing_columns)
             paste(missing_columns, collapse = ", "))
      )
    }
}

for (input_data in names(input_data_list)){
  validation_no_missing_value(input_data_list[[input_data]], input_data)
}