# https://users.nber.org/~rdehejia/data/.nswdata2.html

# NSW Data Files (Lalonde Sample) ----
noms <- c('treat', 'age', 'education', 'black', 'hispanic', 'married', 'nodegree', 're75', 're78')

control <- readr::read_delim(
  file = 'data-raw/nsw_control.txt', 
  delim = '  ',
  col_names = c('x', noms)
) |> 
  dplyr::select(-x)

treated <- readr::read_delim(
  file = 'data-raw/nsw_treated.txt', 
  delim = '  ',
  col_names = c('x', noms)
) |> 
  dplyr::select(-x)

nsw <- dplyr::bind_rows(treated, control) |> 
  dplyr::mutate(
    treat = as.logical(treat),
    black = as.logical(black),
    hispanic = as.logical(hispanic),
    married = as.logical(married),
    nodegree = as.logical(nodegree)
  )

usethis::use_data(nsw, overwrite = TRUE)

# NSW Data Files (Dehejia-Wahha Sample) ----
noms <- c('treat', 'age', 'education', 'black', 'hispanic', 'married', 'nodegree', 're74', 're75', 're78')

control <- readr::read_delim(
  file = 'data-raw/nswre74_control.txt', 
  delim = '  ',
  col_names = c('x', noms)
) |> 
  dplyr::select(-x)

treated <- readr::read_delim(
  file = 'data-raw/nswre74_treated.txt', 
  delim = '  ',
  col_names = c('x', noms)
) |> 
  dplyr::select(-x)

nsw_dw <- dplyr::bind_rows(treated, control) |>
  dplyr::mutate(
    treat = as.logical(treat),
    black = as.logical(black),
    hispanic = as.logical(hispanic),
    married = as.logical(married),
    nodegree = as.logical(nodegree)
  )

usethis::use_data(nsw_dw, overwrite = TRUE)

# PSID controls ----
psid_controls <- readr::read_delim(
  file = 'data-raw/psid_controls.txt', 
  delim = '  ',
  col_names = c('x', noms)
) |> 
  dplyr::select(-x) |> 
  dplyr::mutate(
    black = as.logical(black),
    hispanic = as.logical(hispanic),
    married = as.logical(married),
    nodegree = as.logical(nodegree)
  )
usethis::use_data(psid_controls, overwrite = TRUE)
