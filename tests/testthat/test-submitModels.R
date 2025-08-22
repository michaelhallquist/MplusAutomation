# tests for submitModels
p <- test_path("submitModels")

# checks on parsing of scheduling arguments and script setup
track <- submitModels(p, sched_args=c("--mail=user", "--export=v"), debug=TRUE, max_time_per_job = "4:10:00", combine_jobs = TRUE)

# single model
inp <- file.path(p, "ex3.1.inp")

submitModels(inp, sched_args=c("--mail=user", "--export=v"), debug=TRUE, max_time_per_job = "2:10:00", combine_jobs = TRUE, replace="always")

test_that("submitModels job ID check", {
    expect_equal(track$jobid[1], "dummy_1")
})

test_that("submitModels job allocation checks", {
    expect_equal(track$memgb[1], 4)
    expect_equal(track$cores[1], 1)
    expect_equal(track$wall_time[1], "3:00:00")
    
})

# setwd("/proj/mnhallqlab/users/michael/MplusAutomation")
# checks on cluster (must be run on slurm cluster!)
# track <- submitModels(p,
#     scheduler = "slurm",
#     batch_outdir = "/proj/mnhallqlab/users/michael/submitModels_test",
#     Mplus_command = "/proj/mnhallqlab/local/bin/mplus",
#     sched_args = c("--mail-user=mnhallq"),
#     debug=FALSE,
#     replaceOutfile = "always"
# )
# 
# track <- checkSubmission(track)
# summary(track)

# combine jobs challenge
p <- test_path("submitModels/job_combine")
track <- submitModels(p, sched_args=c("--mail=user", "--export=v"), debug=TRUE, max_time_per_job = "48:10:00", batch_outdir = file.path(p, "batchfiles"))
test_that("submitModels combines jobs as expected", {
  expect_equal(track$file[[5]], c("job_19.inp", "job_20.inp"))
  expect_equal(track$wall_hr[5], 39)
})