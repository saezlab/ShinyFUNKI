
# This tests that the segfault discovered in Mavericks and fixed
# on 8/12/2014 does not recur.
test_segfault <- function()
{
    args <- structure(list(arg1 = 14L, arg2 = 55L, arg3 = c(0L, 10L, 0L, 
1L, 0L, 2L, 0L, 3L, 0L, 11L, 0L, 12L, 0L, 4L, 0L, 5L, 0L, 6L, 
0L, 7L, 0L, 8L, 0L, 9L, 1L, 10L, 1L, 2L, 1L, 3L, 1L, 11L, 1L, 
12L, 1L, 4L, 1L, 5L, 1L, 6L, 1L, 7L, 1L, 8L, 1L, 9L, 2L, 10L, 
2L, 3L, 2L, 11L, 2L, 4L, 2L, 5L, 2L, 6L, 2L, 9L, 3L, 10L, 3L, 
11L, 3L, 12L, 3L, 4L, 3L, 5L, 3L, 6L, 3L, 7L, 3L, 9L, 4L, 11L, 
4L, 12L, 4L, 5L, 4L, 6L, 4L, 7L, 4L, 9L, 5L, 11L, 5L, 12L, 5L, 
6L, 5L, 9L, 6L, 7L, 6L, 9L, 7L, 11L, 7L, 12L, 7L, 9L, 9L, 11L, 
11L, 12L), arg4 = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 
1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), 
    arg5 = 3L, arg6 = 3L, arg7 = c(3L, 2L, 1L)), .Names = c("arg1", 
"arg2", "arg3", "arg4", "arg5", "arg6", "arg7"))
    with(args, {ans = .Call("BGL_highly_conn_sg",
                arg1, arg2,
                arg3, arg4,
                arg5, arg6, arg7,
                PACKAGE="RBGL")})
}