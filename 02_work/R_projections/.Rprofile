############################################################################## #
# Filename
#   .Rprofile
#
# Description
#   .Rprofile file
#
# Author(s) Thomas Riebenbaur <thomas.riebenbauer@joannem.at>
#           Michaela Dvorzak <michaela.dvorzak@joanneum.at>
# Date      2017-03-10
# Version   1.0.1
#
# Copyright JOANNEUM RESEARCH, 2017
############################################################################## #


##############################################################################~#
# get working direcotry of Rstudio-project and save it to an environment #######

RPROJ <- list(wd_rproj = normalizePath(getwd(), winslash = "/"))
if (!("RPROJ" %in% search())) {
  base::attach(RPROJ)
} else {
  assign("wd_rproj", RPROJ$wd_rproj, "RPROJ")
}
rm(RPROJ)

# working with environments
# - get("wd_rproj", "RPROJ") get value
# - exists("test", "RPROJ") does variable exist?
# - assign("test", 1, "RPROJ") set new value
# - rm("test", envir = as.environment("RPROJ")) # remove value from envir
# - "RPROJ" %in% search() does enironment exist?
# - detach("RPROJ") for deleting the environment


############################################################################## #
# setProjectDirectory
#
# Description
#   function to set a project directory based on 'wd_rproj' (in envir RPROJ)
#
# Input
#   project_details:
#     list of project details as defined by project.yaml file
#
# Output
#   -
############################################################################## #
setProjectDirectory <- function(project_details = list()) {
  # check project_directory
  wd_rproj <- get("wd_rproj", "RPROJ")
  # is project_directory (correctly) defined?
  if ("project_directory" %in% names(project_details)) {
    project_directory <- get("project_directory", "RPROJ")
    if (!is.null(project_directory) && !is.na(project_directory)) {
      if (!dir.exists(as.character(project_directory))) {
        warning("Defined 'project_directory' in project.yaml not found!")
        reset_project_directory <- TRUE
      } else {
        reset_project_directory <- FALSE
      }
    } else {
      reset_project_directory <- TRUE
    }
  } else {
    warning("No 'project_directory' defined in project.yaml!")
    reset_project_directory <- TRUE
  }
  # search for project_directory based on wd_rproj + set it
  if (reset_project_directory) {
    cat("\n- searching project directory ... ")
    if (!grepl("02_work", wd_rproj, perl = TRUE)) {
      warning("R-project is not within folder '02_work'!")
    } else {
      project_directory <-
        regmatches(x = wd_rproj,
                   m = regexpr("^(?:(?!/02_work).)*",
                               wd_rproj,
                               perl = TRUE))
      assign("project_directory", project_directory, "RPROJ")
    }
  }
}


############################################################################## #
# loadFile
#
# Description
#   load file, if it exists
#
# Input
#   file_name:
#     name of the file
#   path:
#     optional path to file
#
# Output
#   -
############################################################################## #
loadFile <- function(file_name, path = ".") {
  if (file.exists(file.path(path, file_name))) {
    cat("Loading", file_name ,"... ")
    try(source(file.path(path, file_name), encoding = 'UTF-8'))
    cat("DONE!\n")
  } else {
    warning("'", file_name,"'-file not found!")
  }
}


##############################################################################~#
# .First function ##############################################################
# gets called on R startup
.First <- function(){
  # load basic libararies (to be able to source files using them)
  require("utils", quietly = TRUE, character.only = TRUE)
  require("grDevices", quietly = TRUE, character.only = TRUE)
  require("utils", quietly = TRUE, character.only = TRUE)
  require("grDevices", quietly = TRUE, character.only = TRUE)
  require("graphics", quietly = TRUE, character.only = TRUE)
  require("stats", quietly = TRUE, character.only = TRUE)
  
  # load project.yaml, if it exists
  if (file.exists('project.yaml')) {
    cat("Loading project.yaml ... ")
    if (!("yaml" %in% rownames(installed.packages()))) {
      cat("ERROR: package yaml not installed. Cannot load 'project.yaml\n")
    } else {
      # try to load yaml file (can fail due to many reasons)
      try({
        project_details <- yaml::yaml.load_file("project.yaml")
        for (pd in names(project_details)) {
          assign(pd, project_details[[pd]], "RPROJ")
        }
        # print project.yaml version
        if (exists("project_yaml_version", "RPROJ")) {
          if (!is.null(get("project_yaml_version", "RPROJ"))) {
            cat(paste0("(v", get("project_yaml_version", "RPROJ"), ") ..."))
          }
        }
        # set project directory
        setProjectDirectory(project_details = project_details)
        cat("DONE!\n")
      })
    }
  } else {
    cat("'project.yaml'-file not found!\n")
    setProjectDirectory()
  }
  
  # source file "00_init.R", if it exists
  if ("RPROJ" %in% search() && exists("load_init", "RPROJ")) {
    if (get("load_init", "RPROJ")) {
      loadFile("00_init.R")
    }
  } else {
    loadFile("00_init.R")
  }
  
  # # source other files (if any)
  if ("RPROJ" %in% search() && exists("load_files", "RPROJ")) {
    load_files <- get("load_files", "RPROJ")
    if (length(load_files > 0)) {
      for (file in load_files) {
        loadFile(file)
      }
    }
  }
  
  # clean up
  rm("loadFile", "setProjectDirectory", envir = .GlobalEnv)
  
  # Welcome message
  cat("\n Happy programming with R by STA-R-s! :)\n\n")
}

##############################################################################~#
# .Last function ###############################################################
# gets called on closing R
.Last <- function(){
  cat("\n Stay save and be happy :)\n\n")
  
  # just in case an error occurs
  try({
    # automatically log the sessionInfo() after every RStudio session
    # see https://rviews.rstudio.com/2017/04/19/r-for-enterprise-understanding-r-s-startup/
    if (interactive()) {
      
      ## check to see if we're in an RStudio project (requires the rstudioapi package)
      if (!requireNamespace("rstudioapi"))
        return(NULL)
      pth <- rstudioapi::getActiveProject()
      if (is.null(pth)) {
        return(NULL)
      }
      if (!dir.exists(pth)) {
        return(NULL)
      }
      
      ## append date + sessionInfo to a file called sessionInfo.log
      cat("Recording session info into the project's sessionInfo.log file...")
      info_session <-  capture.output(sessionInfo())
      info_sys <-
        capture.output(as.list(Sys.info()[c("nodename", "user")]))
      # get version of project structure
      if ("RPROJ" %in% search() && exists("project_yaml_version", "RPROJ")) {
        if (!is.null(get("project_yaml_version", "RPROJ"))) {
          info_project_version <- get("project_yaml_version", "RPROJ")
        }
      } else {
        info_project_version <- "not available"
      }
      
      info <- paste("\n",
                    paste0(rep("#", 80), collapse = ""),
                    paste0('Session Info for ', Sys.time()),
                    paste(info_session, collapse = "\n"),
                    "\n",
                    "User info:",
                    paste(info_sys, collapse = "\n"),
                    paste("Project structure version:", info_project_version),
                    "\n",
                    sep  = "\n")
      f <- file.path(pth, "sessionInfo.log")
      cat(info, file = f, append = TRUE)
    }
  })
}
