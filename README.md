# Autoreplication

Make sure that an up to date version of nix is installed. TODO... 

Given a OSF id `id` run
```R
R
> library(metacheck)
> metacheck::osf_file_download("<id>")
```
This creates a dir corresponding to "{id}/{name-on-osf}"

Then run 
```
main("<id>")
```
This creates a nix.shell with the necessary `R` packages in the toplevel of the directory.