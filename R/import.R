import = function(file_paths, merge_tables = F, sheets = 1) {

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
  len = length(file_paths)
  k = 1

  while(len > 0) {

    # Try to open the file based on extension
    fp = file_paths[len]
    fe = file_exts[len]

    if (fe %in% c("csv", "txt")) {

      dt = data.table::fread(fp,
                             check.names = T,
                             na.strings = c("ND", "NA", "", "-9999"))

      dt.list[[k]] = data.table::copy(dt)
      k = k + 1

    } else if (fe %in% c("xls", "xlsx")) {

      if (sheets == "all") {
        sh = readxl::excel_sheets(fp)
      } else {
        sh = sheets
      }

      for (s in sh) {
        dt = data.table::as.data.table(readxl::read_excel(fp, sheet = s))
        dt.list[[k]] = data.table::copy(dt)
        k = k + 1
      }

    } else {
      warning("Unknown .", fe, " file format (supported formats are csv, xls, xlsx).")
      next
    }

    # Next file
    len = len - 1

  }

  dt.list = Filter(function(x) {!is.null(x)}, dt.list)

  # Format result
  if (length(dt.list) < 2) { dt.list = dt.list[[1]] }
  if (length(dt.list) < 2 & merge_tables == T) {dt.list = rbindlist(dt.list)}

  return(dt.list)

}
