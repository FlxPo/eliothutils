library(tools)
library(data.table)
library(readxl)

import = function(file_paths, mapping = NULL, merge = F) {

  # Find files absolute paths, names and extensions
  file_names = basename(file_paths)
  if (file_names[1] == file_paths[1]) {file_paths = file.path(getwd(), file_names)}
  file_exts = tools::file_ext(file_names)

  # Check if the file exists
  e = file.exists(file_paths)
  if (sum(e) < 1) {stop("No files were found.")}
  ne = which(e == F)
  if (sum(ne) > 1) {warning(paste("The following files were not found : ", file_names[ne]))}

  # Filter out non existing files
  file_names = file_names[e]
  file_exts = file_exts[e]
  file_paths = file_paths[e]

  # Loop over the the list of files
  dt.list = list()

  for (i in 1:length(file_paths)) {

    # Try to open the file based on extension
    fp = file_paths[i]
    fe = file_exts[i]

    if (fe %in% c("csv", "txt")) {
      dt = fread(fp, check.names = T, na.strings = c("ND", "NA", "", "-9999"))
    } else if (fe %in% c("xls", "xlsx")) {
      dt = as.data.table(read_excel(fp))
    } else {
      warning("Unknown .", fe, " file format (supported formats are csv, xls, xlsx).")
      next
    }

    dt.list[[i]] = copy(dt)

  }

  dt.list = Filter(function(x) {!is.null(x)}, dt.list)
  if (length(dt.list) < 2) { dt.list = dt.list[[1]] }

  return(dt.list)


}
