test_that("'output_file' should produce a string", {
  expect_error(output_file(7, "coverage_final"))
  expect_error(output_file("12gr", 12))
})

test_that("'output_file' should go through the specified new folder in the root directory", {
  expect_equal(output_file("6gr", "reasons_for_Non_Imms","test_result_newcode"),
               here::here("test_result_newcode/VCHA_6gr_reasons_for_Non_Imms.xlsx"))
})

test_that("'output_file' should produce a path through the defult folder 'result/' in the root directory if roor_dir is missing", {
  expect_equal(output_file("7yr", "coverage_final"),
               here::here("result/VCHA_7yr_coverage_final.xlsx"))
  expect_equal(output_file("12gr", "reasons_for_Non_Imms"),
               here::here("result/VCHA_12gr_reasons_for_Non_Imms.xlsx"))
})
