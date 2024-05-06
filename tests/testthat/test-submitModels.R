# tests for submitModels
setwd("/proj/mnhallqlab/users/michael/MplusAutomation")

p <- test_path("submitModels")

# checks on parsing of scheduling arguments and script setup
track <- submitModels(p, sched_args=c("--mail=user", "--export=v"), debug=TRUE)

test_that("submitModels job ID check", {
    expect_equal(track$jobid[1], "dummy_1")
})

test_that("submitModels job allocation checks", {
    expect_equal(track$memgb[1], 16)
    expect_equal(track$cores[1], 2)
    expect_equal(track$wall_time[1], "3:00:00")
})

# checks on cluster (must be run on slurm cluster!)
track <- submitModels(p,
    scheduler = "slurm",
    batch_outdir = "/proj/mnhallqlab/users/michael/submitModels_test",
    Mplus_command = "/proj/mnhallqlab/local/bin/mplus",
    sched_args = c("--mail-user=mnhallq"),
    debug=FALSE,
    replaceOutfile = "always"
)

track <- checkSubmission(track)
summary(track)

