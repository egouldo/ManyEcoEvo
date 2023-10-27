
#' Read ManyEcoEvo Hex Key
#'
#' @return Hex Key image as .jpg
#' @export
hex_key <- function(){
  magick::image_read(system.file("hex_keys/ManyEcoEvoHex.jpg"))
}
