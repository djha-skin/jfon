# JFon: JZON for FSet
An attempt at porting JZON to FSet.

The package exports two symbols, `parse` and `stringify`, which are straight
ports of the functions in the `#:com.inuoe.jzon` package of the same name but
which use FSet seqs and maps instead of lists and hash tables.