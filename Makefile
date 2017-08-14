.POSIX:

# default to compiling without debug symbols
all: a-linear-lisp

# compile and link main executable
a-linear-lisp: clean
	ghc a-linear-lisp.hs
	./a-linear-lisp

clean:
	rm -rf *.o *.hi a-linear-lisp

.PHONY: all clean example

