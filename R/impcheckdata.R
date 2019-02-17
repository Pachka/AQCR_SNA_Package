# Import your dataset
#
# Cette fonction sert a ...
# @keywords dataset import validation formatting
# @importFrom utils read.table
# @examples
# df <- impdatamob()
#
impdatamob <- function(){
  cat("Quel est votre s\xe9parateur de colonnes ?
      \n 1: virgule
      \n 2: point virgule
      \n 3: tabulation")
  x <-readline(prompt ="Tapez le chiffre correspondant \xe0 votre s\xe9parateur : ")
  if(!(x %in% c("1", "2", "3"))) stop("Votre fichier de donn\xe9es doit \xeatre un fichier csv avec l'un des s\xe9parateurs ci-dessus")
  x <- ifelse(x == "1",",",ifelse(x=="2", ";", if(x == "3")"\t"))
  read.table(file.choose(),sep=x, header=TRUE)
}
# Controle your Dataset
#
# Cette fonction sert a ...
# @keywords dataset import validation formatting
# @export checkmissing
#
# @examples
# df <- checkmissing(rawdata)
#
checkmissing <- function(x){
  validcolumns <- rawdata[,!(names(rawdata) %in% c("EFFFECTIF"))]
  y <- match(toupper(colnames(x)),colnames(validcolumns))
  y <- y[!is.na(y)]
  y <- colnames(validcolumns)[-y]
  if(length(y) != 0) stop(paste0("Les colonnes suivantes manques \xe0 votre jeu de donn\xe9es (elles doivent porter exactement le m\xeame nom): ",y))
  message("Parfait, votre tableau contient toutes les donn\xe9es n\xe9cessaires")

  if(!(c("EFFFECTIF") %in% toupper(colnames(x)))) warning('Votre jeu de donn\xe9es pourrait \xeatre optimis\xe9 en ajoutant les effectifs.')
}
# Formate your Dataset
#
# Cette fonction sert a ...
# @export formattingdata
# @keywords dataset import validation formatting
#
# @examples
# df <- formattingdata(rawdata)
#
formattingdata <- function(x){
  colnames(x) <- toupper(colnames(x))
  y <- x[,names(rawdata)[!(names(rawdata) %in% c("EFFFECTIF"))]]
  y$EFFECTIF <- 1

  if(c("EFFFECTIF") %in% names(x)) y <- x[,names(rawdata)]

  y
}
#' Import, control and format your dataset
#'
#' This function is used to import your dataset,
#' to verify that it contains all the necessary information and
#' to format it into a suitable data.frame that will be analyzed by the other functions of the package.
#'
#' Cette fonction sert à importer votre jeu de données,
#' à vérifier qu'il contient bien toutes les informations nécessaires et
#' à le formatter en une data.frame adaptée qui sera analysée par les autres fonctions du package.
#'
#' @importFrom utils read.table
#' @export impcheckdata
#' @keywords dataset import validation formatting
#'
#' @usage impcheckdata()
#'
impcheckdata <- function(){
  x <- impdatamob()
  checkmissing(x)
  formattingdata(x)
}
