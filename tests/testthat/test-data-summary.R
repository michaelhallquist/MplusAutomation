test_that("extractDataSummary parses cross-classified cluster information subsections", {
  txt <- c(
    "SUMMARY OF DATA",
    "",
    "     Cluster information for SCHOOL",
    "       Number of clusters                        1",
    "       Size (s)   Cluster ID with Size s",
    "           1      SCH1",
    "",
    "     Cluster information for TEACHER",
    "       Number of clusters                        2",
    "       Size (s)   Cluster ID with Size s",
    "           2      T1 T2",
    "",
    "MODEL FIT INFORMATION"
  )
  
  outfile <- MplusAutomation:::parse_into_sections(txt)
  ds <- MplusAutomation:::extractDataSummary(outfile, "synthetic.out")
  
  expect_s3_class(ds, "mplus.data_summary")
  expect_equal(ds$overall$NClusters_SCHOOL, 1)
  expect_equal(ds$overall$ClusterSize_SCHOOL_1, 1L)
  expect_equal(ds$overall$ClusterSize_SCHOOL_1_IDs, "SCH1")
  expect_equal(ds$overall$NClusters_TEACHER, 2)
  expect_equal(ds$overall$ClusterSize_TEACHER_1, 2L)
  expect_equal(ds$overall$ClusterSize_TEACHER_1_IDs, "T1,T2")
})
