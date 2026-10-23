test_that("Default Hypothesis without giving an hypothesis set get build correctly", {
  expect_equal(build_hypotheses(parnames=c("adjmean0", "adjmean1")), list(adjmeans.descending=c("adjmean0 > adjmean1"), adjmeans.ascending=c("adjmean0 < adjmean1")))
})


test_that("Ordering Ascending hypotheses using 2 adjmeans get build correctly", {
  expect_equal(build_hypotheses(hypothesis.set=c("ordering.asc"), parnames=c("adjmean0", "adjmean1")), list(h.order="adjmean0 > adjmean1"))
})
