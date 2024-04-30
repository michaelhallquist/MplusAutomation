# examine whether we get

p <- testthat::test_path("submitModels")

track <- submitModels(p, sched_args=c("--mail=user", "--export=v"), debug=TRUE)

test_that("submitModels job ID check", {
    expect_equal(track$jobid[1], "dummy_1")
})

test_that("submitModels job allocation checks", {
    expect_equal(track$memgb[1], 16),
    expect_equal(track$cores[1], 2),
    expect_equal(track$wall_time[1], "3:00:00")
})
