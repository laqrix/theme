.PHONY: all clean

SRC := $(shell git ls-files '*.ss')

all: theme

theme: ${SRC}
	swish-build -o $@ main.ss -b petite --rtlib swish --libs-visible

clean:
	rm -f theme theme.boot
	rm -f *.{so,mo,wpo,sop,ss.html}
