# A little function to reduce the size of 'n = ...' on plots

labellerN <- function(x) {
  sub(
    "\n\\((n = [0-9]+/[0-9]+)\\)",
    "<br><span style='font-size:6pt'><i>(\\1)</i></span>",
    x
  )
}
