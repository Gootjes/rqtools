context("Plotting dataset")

d <- mtcars
d <- d[,4:10]
d$id <- rownames(d)

test_that(desc = "", {
  expect_equal(names(plotRespondents(data = d[1:5,], id.var = "id")), d$id[1:5])
})
