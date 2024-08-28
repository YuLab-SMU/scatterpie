#' @importFrom yulab.utils yulab_msg
.onAttach <- function(libname, pkgname) {
  packageStartupMessage(yulab.utils::yulab_msg(pkgname))
}

