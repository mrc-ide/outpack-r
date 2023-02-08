## In orderly we also add the result of git status --porcelain here,
## but we've never really used this and it's quite slow for big
## repos. More problematic, the format looks hard to replicate with
## gert (which provides a really nice data frame of status
## information) and I imagine we'd have similar issues with a python
## client. Most of the time when these things are run on a controlled
## server we only need the hash really.
##
## Also note that there might be 0, 1, or more urls depending on the
## way that the repo is configured; this feels ok really.
git_info <- function(path) {
  repo <- tryCatch(gert::git_open(path), error = function(e) NULL)
  if (is.null(repo)) {
    return(NULL)
  }
  list(sha = gert::git_commit_id(repo = repo),
       branch = gert::git_branch(repo = repo),
       url = gert::git_remote_list(repo = repo)$url)
}
