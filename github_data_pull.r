# Git history
# source Dean Attali https://daattali.com/shiny/visualize-git-commits-time/

library('git2r')
library('dplyr')
library('ggplot2') 
library('ggExtra')
library('scales')

# api.github.com

# Option A, git.log
#Command line
# git log --date=local --no-merges --shortstat\
# --pretty=format:"%h%x09%an%x09%ad%x09%sEOL" |\
# perl -pi -e 's/EOL\n/\t/g' > git.log


log <- read.delim(
        file = 'git.log',
        sep = "\t",
        col.names = c('commit', 'author', 'time', 'message', 'effect'),
        stringsAsFactors = FALSE
)


# Option B

# Functions
username_github <- c("Randy", "R-Avalos")

repo_github <- c("onthemargin", "Zoom-Video-Conference-Management")

#### This is a bit long....
create_git_log_file <- function(
        username = username_github,
        repos = repo_github,
        dir ="git_repos_vis",
        logfile = "project-logs.csv") {
        
        if (!requireNamespace("git2r", quietly = TRUE)) {
                stop("You need to install the 'git2r' package", call. = FALSE)
        }
        
        if (!dir.exists(dir)) {
                dir.create(dir, recursive = TRUE, showWarnings = FALSE)
        }
        dir <- normalizePath(dir)
        
        # clone all the git repos into one folder and get their commit messages
        logs <- lapply(repos, function(repo) {
                # get the unique repo
                if (!grepl("/", repo)) {
                        stop(repo, " is not a valid repo name (you forgot to specify the user)", call. = FALSE)
                }
                repo_name <- sub(".*/(.*)", replacement = "\\1", repo)
                repo_dir <- file.path(dir, repo_name)
                
                # clone the repo
                if (dir.exists(repo_dir)) {
                        message("Note: Not cloning ", repo, " because a folder with that name already exists")
                } else {
                        message("Cloning ", repo)
                        repo_url <- paste0("https://github.com/", repo)
                        git2r::clone(url = repo_url, local_path = repo_dir,
                                     progress = FALSE)
                }
                # get the git log
                repo <- git2r::repository(repo_dir)
                commits <- git2r::commits(repo)
                
                dates <- unlist(lapply(commits, function(commit) {
                        if (commit@author@name %in% username) {
                                as.character(as.POSIXlt(commit@author@when@time, origin = "1970-01-01"))
                        } else {
                                NULL
                        }
                }))
                data.frame(project = rep(repo_name, length(dates)),
                           timestamp = dates,
                           stringsAsFactors = FALSE)
        })
        
        # write the logs to a file
        logs <- do.call(rbind, logs)
        logfile <- file.path(dir, logfile)
        write.csv(logs, logfile, quote = FALSE, row.names = FALSE)
        
        if (file.exists(logfile)) {
                message("Created logfile at ", normalizePath(logfile))
        } else {
                stop("The git log file could not get creatd for some reason", call. = FALSE)
        }
        
        return(logfile)
}

        
        

create_git_log_file()
