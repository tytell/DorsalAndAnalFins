test_that("proj_path resolves relative and absolute entries", {
  expect_equal(proj_path("raw_dir"), fs::path(here::here("data/raw")))
  expect_true(fs::is_absolute_path(proj_path("data_dir")))
})

test_that("proj_path errors on unknown keys", {
  expect_error(proj_path("not_a_key"), "No entry")
})
