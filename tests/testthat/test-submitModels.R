# tests for submitModels
p <- test_path("submitModels")

# checks on parsing of scheduling arguments and script setup
track <- submitModels(p, sched_args=c("--mail=user", "--export=v"), debug=TRUE, max_time_per_job = "2:10:00", combine_jobs = TRUE)

test_that("submitModels job ID check", {
    expect_equal(track$jobid[1], "dummy_1")
})

test_that("submitModels job allocation checks", {
    expect_equal(track$memgb[1], 16)
    expect_equal(track$cores[1], 2)
    expect_equal(track$wall_time[1], "0:30:00")
    
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
