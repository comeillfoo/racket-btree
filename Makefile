SHELL:=/bin/bash

RACO?=raco
FMT?=fmt
REVIEW?=review
TEST?=test

RKTDIR=.


racket-format:
	diff -u <(cat $(RKTDIR)/*.rkt ) <($(RACO) $(FMT) $(RKTDIR)/*.rkt )


racket-format-fix:
	$(RACO) $(FMT) -i $(RKTDIR)/*.rkt


racket-lint:
	find $(RKTDIR) -type f -name *.rkt -print0 | xargs -0 -n1 $(RACO) $(REVIEW)

lint: racket-lint


racket-test:
	$(RACO) $(TEST) $(RKTDIR)

test: python-test racket-test


.PHONY: test lint racket-lint racket-format racket-format-fix racket-test
