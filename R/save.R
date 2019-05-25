#'Save to .pdf, .html or .docx
#'
#'\code{dt_save} export a rendered dt object in the corresponding file format
#'
#'@param x A dt object rendered by one of those functions \code{dt_to_html()},
#'  \code{dt_to_latex()} or \code{dt_to_flextable()}
#'@param file A file name without extension file specified.
#'@param path A path to directory where file should be save. If NULL (default)
#'  file will be save in the current working directory.
#'@exemples
#'dt_create(data, group = treatment, stat_num = "mean") %>%
#'  dt_to_latex(title = "Compare treated and untreated individuals") %>%
#'  dt_save(file = "table_treatment_groups")
#'@return pdf, html or docx file

dt_save <- function(x, file, path = NULL){

  # check
  if(!is.null(path) && !is.character(path)){
    stop("path argument needs to be a character string")
  } else if (!is.null(path) && stringr::str_sub(path, -1, -1) != .Platform$file.sep){
    path <- paste0(path, .Platform$file.sep)
  }
  file <- rlang::enexpr(file)

  # save a flextable in a word document
  if (class(x) == "flextable"){
    doc <- officer::read_docx()
    doc <- flextable::body_add_flextable(doc, value = x)
    print(doc, target = paste0(path, file, ".docx"))

    # save a latex table in pdf document
  } else if(attr(x, "format") == "latex") {
    x %>% kableExtra::save_kable(paste0(path, file, ".pdf"))

    # save a html table in html page
  } else if(attr(x, "format") == "html") {
    x %>% kableExtra::save_kable(paste0(path, file, ".html"))
  }
}
