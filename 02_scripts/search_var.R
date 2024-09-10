# Este script define una función que busca algún patrón en una lista de scripts

find_obj <- function(pattern, files) {
  lines <- readLines(files, warn = FALSE)
  match <- grep(pattern, lines)
  if (length(match) > 0) {
    return(files)
  }
}

lista_de_scripts <- list.files(full.names = TRUE, recursive = TRUE, pattern = ".R$")

find_script <- function(pattern, files) {
  script <- map(
    files,
    \(files) find_obj(pattern, files)
  ) %>%
    reduce(c)

  return(script)
}
