test_that("Estimand adjmeans gets converted succesfully with two leveled X to parnames adjmean0 and adjmean1", {
  expect_equal(estimand.to.parnames(estimand="adjmeans", x_levels=c("0","1")), c("adjmean0","adjmean1"))
})
