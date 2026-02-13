test_that("mplusModel variable detection includes IDVARIABLE and CLUSTER", {
  syn <- "
VARIABLE:
  NAMES = id clus y x z;
  USEVARIABLES = y x;
  IDVARIABLE = id;
  CLUSTER = clus;
MODEL:
  y ON x;
"

  dat <- data.frame(
    id = 1:10,
    clus = rep(1:2, each = 5),
    y = rnorm(10),
    x = rnorm(10),
    z = rnorm(10),
    extra = rnorm(10)
  )

  mplus_fake <- tempfile()
  file.create(mplus_fake)
  m <- mplusModel(
    syntax = syn,
    data = dat,
    inp_file = file.path(tempdir(), "detect_id_cluster.inp"),
    Mplus_command = mplus_fake
  )

  expect_equal(
    m$.__enclos_env__$private$pvt_variables,
    c("y", "x", "id", "clus")
  )
})

test_that("mplusModel variable detection recovers DEFINE source variables", {
  syn <- "
VARIABLE:
  NAMES = y x z;
DEFINE:
  xz = x*z;
MODEL:
  y ON xz;
"

  dat <- data.frame(
    y = rnorm(12),
    x = rnorm(12),
    z = rnorm(12)
  )

  mplus_fake <- tempfile()
  file.create(mplus_fake)
  m <- mplusModel(
    syntax = syn,
    data = dat,
    inp_file = file.path(tempdir(), "detect_define_sources.inp"),
    Mplus_command = mplus_fake
  )

  vars <- m$.__enclos_env__$private$pvt_variables
  expect_true(all(c("y", "x", "z") %in% vars))
  expect_false("xz" %in% vars)
})

test_that("knownclass observed variables are detected", {
  syn <- "
VARIABLE:
  NAMES = y x g;
  USEVARIABLES = y x;
  CLASSES = c(2) cg(2);
  KNOWNCLASS = cg(g=1 g=2);
MODEL:
  %OVERALL%
  y ON x;
"

  dat <- data.frame(
    y = rnorm(10),
    x = rnorm(10),
    g = sample(1:2, 10, replace = TRUE)
  )

  mplus_fake <- tempfile()
  file.create(mplus_fake)
  m <- mplusModel(
    syntax = syn,
    data = dat,
    inp_file = file.path(tempdir(), "detect_knownclass.inp"),
    Mplus_command = mplus_fake
  )

  vars <- m$.__enclos_env__$private$pvt_variables
  expect_true(all(c("y", "x", "g") %in% vars))
})

test_that("detectVariables uses syntax-aware parser", {
  dat <- data.frame(
    id = 1:8,
    clus = rep(1:2, each = 4),
    y = rnorm(8),
    x = rnorm(8),
    z = rnorm(8)
  )

  obj <- list(
    VARIABLE = "
      NAMES = id clus y x z;
      USEVARIABLES = y x;
      IDVARIABLE = id;
      CLUSTER = clus;
    ",
    DEFINE = "xz = x*z;",
    MODEL = "y ON xz;",
    rdata = dat,
    imputed = FALSE
  )

  expect_equal(
    detectVariables(obj),
    c("y", "x", "id", "clus", "z")
  )
})

test_that("mplusObject autov=TRUE detects usevariables from syntax", {
  dat <- data.frame(
    id = 1:8,
    clus = rep(1:2, each = 4),
    y = rnorm(8),
    x = rnorm(8),
    z = rnorm(8)
  )

  obj <- mplusObject(
    VARIABLE = "
      NAMES = id clus y x z;
      USEVARIABLES = y x;
      IDVARIABLE = id;
      CLUSTER = clus;
    ",
    DEFINE = "xz = x*z;",
    MODEL = "y ON xz;",
    rdata = dat,
    autov = TRUE
  )

  expect_equal(
    obj[["usevariables"]],
    c("y", "x", "id", "clus", "z")
  )
})

test_that("mplusObject autov=FALSE leaves usevariables unchanged when NULL", {
  dat <- data.frame(
    id = 1:8,
    clus = rep(1:2, each = 4),
    y = rnorm(8),
    x = rnorm(8),
    z = rnorm(8)
  )

  obj <- mplusObject(
    VARIABLE = "
      NAMES = id clus y x z;
      USEVARIABLES = y x;
      IDVARIABLE = id;
      CLUSTER = clus;
    ",
    DEFINE = "xz = x*z;",
    MODEL = "y ON xz;",
    rdata = dat,
    autov = FALSE
  )

  expect_null(obj[["usevariables"]])
})

test_that("mplusObject explicit usevariables takes precedence over autov", {
  dat <- data.frame(
    id = 1:8,
    y = rnorm(8),
    x = rnorm(8),
    z = rnorm(8)
  )

  obj <- mplusObject(
    VARIABLE = "NAMES = id y x z;",
    MODEL = "y ON x;",
    usevariables = c("id", "y"),
    rdata = dat,
    autov = TRUE
  )

  expect_equal(obj[["usevariables"]], c("id", "y"))
})
