test_that("git_info on non-version-controlled path is NULL", {
  path <- withr::local_tempdir()
  expect_null(git_info(path))
})


test_that("can get git information from a path", {
  path <- withr::local_tempdir()
  gert::git_init(path)
  file.create(file.path(path, "hello"))
  gert::git_add(".", repo = path)
  user <- "author <author@example.com>"
  hash <- gert::git_commit("initial", author = user, committer = user,
                           repo = path)
  branch <- gert::git_branch(repo = path)
  expect_mapequal(
    git_info(path),
    list(sha = hash, branch = branch, url = character(0)))

  gert::git_remote_add("git@example.com/example", "origin", repo = path)
  expect_mapequal(
    git_info(path),
    list(sha = hash, branch = branch, url = "git@example.com/example"))

  gert::git_remote_add("https://example.com/git/example", "other", repo = path)
  expect_mapequal(
    git_info(path),
    list(sha = hash,
         branch = branch,
         url = c("git@example.com/example", "https://example.com/git/example")))
})
