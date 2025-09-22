# map_chr

    Code
      map_chr(1:2, function(x) 1)
    Condition
      Error in `vapply()`:
      ! values must be type 'character',
       but FUN(X[[1]]) result is type 'double'

# map_int

    Code
      map_int(1:2, function(x) 1)
    Condition
      Error in `vapply()`:
      ! values must be type 'integer',
       but FUN(X[[1]]) result is type 'double'

# map_dbl

    Code
      map_dbl(1:2, function(x) "foo")
    Condition
      Error in `vapply()`:
      ! values must be type 'double',
       but FUN(X[[1]]) result is type 'character'

# map_lgl

    Code
      map_lgl(1:2, function(x) 1)
    Condition
      Error in `vapply()`:
      ! values must be type 'logical',
       but FUN(X[[1]]) result is type 'double'

# summary_line [plain]

    Code
      writeLines(summary_line(10, 2, 2, 3))
    Output
      PASS x10  FAIL x2  WARN x2  SKIP x3
    Code
      writeLines(summary_line(10, 0, 0, 0))
    Output
      PASS x10  FAIL x0  WARN x0  SKIP x0
    Code
      writeLines(summary_line(0, 1, 0, 0))
    Output
      PASS x0  FAIL x1  WARN x0  SKIP x0
    Code
      writeLines(summary_line(0, 0, 1, 0))
    Output
      PASS x0  FAIL x0  WARN x1  SKIP x0
    Code
      writeLines(summary_line(0, 0, 0, 1))
    Output
      PASS x0  FAIL x0  WARN x0  SKIP x1
    Code
      writeLines(summary_line(0, 0, 0, 0))
    Output
      PASS x0  FAIL x0  WARN x0  SKIP x0

# summary_line [ansi]

    Code
      writeLines(summary_line(10, 2, 2, 3))
    Output
      [40m[37mPASS[39m[49m x10  [41m[37mFAIL[39m[49m x2  [43m[37mWARN[39m[49m x2  [44m[37mSKIP[39m[49m x3
    Code
      writeLines(summary_line(10, 0, 0, 0))
    Output
      [40m[37mPASS[39m[49m x10  [90mFAIL x0[39m  [90mWARN x0[39m  [90mSKIP x0[39m
    Code
      writeLines(summary_line(0, 1, 0, 0))
    Output
      [90mPASS x0[39m  [41m[37mFAIL[39m[49m x1  [90mWARN x0[39m  [90mSKIP x0[39m
    Code
      writeLines(summary_line(0, 0, 1, 0))
    Output
      [90mPASS x0[39m  [90mFAIL x0[39m  [43m[37mWARN[39m[49m x1  [90mSKIP x0[39m
    Code
      writeLines(summary_line(0, 0, 0, 1))
    Output
      [90mPASS x0[39m  [90mFAIL x0[39m  [90mWARN x0[39m  [44m[37mSKIP[39m[49m x1
    Code
      writeLines(summary_line(0, 0, 0, 0))
    Output
      [90mPASS x0[39m  [90mFAIL x0[39m  [90mWARN x0[39m  [90mSKIP x0[39m

# style_orange [plain]

    Code
      style_orange("orange")
    Output
      <cli_ansi_string>
      [1] orange

# style_orange [ansi]

    Code
      style_orange("orange")
    Output
      <cli_ansi_string>
      [1] [33morange[39m

# style_bg_grey [plain]

    Code
      style_bg_grey("grey background")
    Output
      <cli_ansi_string>
      [1] grey background

# style_bg_grey [ansi]

    Code
      style_bg_grey("grey background")
    Output
      <cli_ansi_string>
      [1] [40mgrey background[49m

# empty_data_frame

    Code
      empty_data_frame(10)
    Output
      # A data frame: 10 x 0
    Code
      empty_data_frame(0)
    Output
      # A data frame: 0 x 0

