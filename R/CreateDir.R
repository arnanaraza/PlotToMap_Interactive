### Function that creates results folder per sub-scale (country or biome)

CreateDir <- function(main, sub){
  inside <- list.files(file.path(main, sub))
  if (length(inside) == 0) {
    dir.create(file.path(main,  sub), showWarnings = FALSE)
  }else {print('directory chosen have files, be careful for overwriting')}
  
}

