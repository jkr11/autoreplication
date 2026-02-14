Ideas for now:

Make a shared nix base to inherit packages that are expensive to build.

Insert a preamble to the R files: 

```R
auto_mkdir_wrapper <- function(orig_func) {
  function(x, file, ...) {
    if (is.character(file)) {
      dir_path <- dirname(file)
      if (!dir.exists(dir_path)) {
        dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)
      }
    }
    orig_func(x, file, ...)
  }
}

write.csv  <- auto_mkdir_wrapper(utils::write.csv)
write.csv2 <- auto_mkdir_wrapper(utils::write.csv2)
saveRDS    <- auto_mkdir_wrapper(base::saveRDS)
...
```