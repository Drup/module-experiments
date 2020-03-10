REPO=Drup/module-experiments
DOCDIR=.gh-pages

$(DOCDIR)/.git:
	mkdir -p $(DOCDIR)
	cd $(DOCDIR) && (\
		git clone -b gh-pages git@github.com:$(REPO).git . \
	)

.PHONY: up
up: $(DOCDIR)/.git
	cp -r incr/main.pdf incr.pdf
	git -C $(DOCDIR) add --all
	git -C $(DOCDIR) commit -a -m "updates"
	git -C $(DOCDIR) push origin gh-pages
