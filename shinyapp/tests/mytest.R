app <- ShinyDriver$new("../")
app$snapshotInit("mytest")

# Input 'N_require_rows_current' was set, but doesn't have an input binding.
# Input 'N_require_rows_all' was set, but doesn't have an input binding.
# Input 'N_inCrop_rows_current' was set, but doesn't have an input binding.
# Input 'N_inCrop_rows_all' was set, but doesn't have an input binding.
app$snapshot()
