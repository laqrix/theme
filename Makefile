.PHONY: all clean coverage test

SRC := $(shell git ls-files '*.ss')

all: theme

theme: ${SRC}
	swish-build -o $@ main.ss -b petite --rtlib swish --libs-visible

test: all
	swish-test --progress test --report mat-report.html .

coverage: all
	rm -f profile.data
	swish-test --progress test --report mat-report.html --save-profile profile.data --coverage coverage.html .

clean:
	rm -f theme theme.boot
	rm -f *.{so,mo,wpo,sop,ss.html}
	rm -f profile.data coverage.html mat-report.html
