.First <- function() {
  if (grepl("Windows", Sys.getenv("OS"))) {
    .libPaths(paste(getwd(), "../library", sep = "/"))
    Sys.setenv(USERPROFILE=Sys.getenv("HOME"))
  } else {
      .libPaths(paste(getwd(), "", sep = "/"))
    }
}
.First()