test_that("Wrapper successfully reproduces a specified loglik", {
  set.seed(128)
  testdata <- data.from.causality.table()
  elrmod.test <- EffectLiteR::effectLite(y="Y", x="X", data=testdata)
  test = constrained.inference(elr.object = elrmod.test, estimand = c("adjmeans"))

  expect_equal(
    test$objectList$adjmeans.ascending$loglik, -24.82884, tolerance = 1e-4)
})
