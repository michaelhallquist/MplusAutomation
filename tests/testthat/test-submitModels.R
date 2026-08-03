# tests for submitModels
p <- test_path("submitModels")

test_that("validate_dhms normalizes overflow in clock components", {
  expect_equal(validate_dhms("0:90"), "0:01:30")
  expect_equal(validate_dhms("2:99:00"), "3:39:00")
  expect_equal(validate_dhms("1-24:00"), "2-00:00:00")
})

test_that("minutes_to_dhms delegates to the common duration converter", {
  expect_equal(minutes_to_dhms(60), "1:00:00")
  expect_equal(minutes_to_dhms(160), "2:40:00")
})

test_that("submitModels job ID check", {
  # checks on parsing of scheduling arguments and script setup
  mplus_fake <- tempfile()
  file.create(mplus_fake)
  batch_outdir <- tempfile("mplus-batch-files-")
  on.exit(unlink(batch_outdir, recursive = TRUE, force = TRUE), add = TRUE)
  track <- submitModels(p, sched_args=c("--mail=user", "--export=v"), debug=TRUE, replaceOutfile = "always",
                        max_time_per_job = "4:10:00", combine_jobs = FALSE,
                        batch_outdir = batch_outdir, Mplus_command = mplus_fake)
  
  expect_equal(track$jobid[1], "dummy_1")
  
  # job allocation checks
  expect_equal(track$memgb[1], 16)
  expect_equal(track$cores[1], 2)
  expect_equal(track$wall_time[1], "0:30:00")
  
})

# single model
# inp <- file.path(p, "ex3.1.inp")
# 
# submitModels(inp, sched_args=c("--mail=user", "--export=v"), debug=TRUE, 
#              max_time_per_job = "2:10:00", combine_jobs = TRUE, replace="always", Mplus_command = mplus_fake)


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

test_that("submitModels combines jobs as expected", {
  mplus_fake <- tempfile()
  file.create(mplus_fake)
  batch_outdir <- tempfile("mplus-batch-files-")
  on.exit(unlink(batch_outdir, recursive = TRUE, force = TRUE), add = TRUE)
  track <- submitModels(p, sched_args=c("--mail=user", "--export=v"), debug=TRUE, max_time_per_job = "48:10:00",
                        batch_outdir = batch_outdir, Mplus_command = mplus_fake)
  expect_equal(track$file[[5]], c("job_19.inp", "job_20.inp"))
  expect_equal(track$wall_hr[5], 39)
})

test_that("submitModels combines fractional-hour model times", {
  model_dir <- tempfile("fractional-walltime-")
  dir.create(model_dir)
  model_files <- file.path(model_dir, sprintf("model_%d.inp", 1:3))
  for (model_file in model_files) {
    writeLines(c("! time 2:40:00", "TITLE: fractional wall time;"), model_file)
  }

  mplus_fake <- tempfile()
  file.create(mplus_fake)
  track <- submitModels(
    model_dir,
    debug = TRUE,
    quiet = TRUE,
    replaceOutfile = "always",
    max_time_per_job = "8:00:00",
    batch_outdir = file.path(model_dir, "batchfiles"),
    Mplus_command = mplus_fake
  )

  expect_equal(nrow(track), 1L)
  expect_equal(track$wall_hr, 8)
  expect_equal(track$wall_time, "8:00:00")
  expect_equal(track$file[[1]], basename(model_files))
})

test_that("submitModels uses scheduler-specific day-based walltime formats", {
  model_dir <- tempfile("day-walltime-")
  dir.create(model_dir)
  writeLines(c("! time 1-2:03:04", "TITLE: day-based wall time;"), file.path(model_dir, "model.inp"))

  mplus_fake <- tempfile()
  file.create(mplus_fake)
  slurm_track <- submitModels(
    model_dir,
    scheduler = "slurm",
    combine_jobs = FALSE,
    debug = TRUE,
    quiet = TRUE,
    replaceOutfile = "always",
    batch_outdir = file.path(model_dir, "slurm-batchfiles"),
    Mplus_command = mplus_fake
  )
  torque_track <- submitModels(
    model_dir,
    scheduler = "torque",
    combine_jobs = FALSE,
    debug = TRUE,
    quiet = TRUE,
    replaceOutfile = "always",
    batch_outdir = file.path(model_dir, "torque-batchfiles"),
    Mplus_command = mplus_fake
  )

  expect_equal(slurm_track$wall_time, "1-02:03:04")
  expect_true(any(grepl("^#SBATCH --time=1-02:03:04$", readLines(slurm_track$sched_script))))
  expect_true(any(grepl("^#PBS -l walltime=26:03:04$", readLines(torque_track$sched_script))))
})
