PUBLIC=public
RENDER_PAGE=script/render_page.py
RENDER_BLOG=script/render_blog.py
SPELL_CHECK=script/spell_check

pages=$(PUBLIC)/index.html $(PUBLIC)/projects.html $(PUBLIC)/blog_index.html
PORT=5000

all: $(PUBLIC)/blog $(pages) $(PUBLIC)/static $(PUBLIC)/CNAME

$(PUBLIC)/%.html: pages/%.yaml templates/%.html $(PUBLIC)
	$(RENDER_PAGE) $< $@

clean:
	rm -rf $(PUBLIC)

$(PUBLIC)/CNAME: CNAME $(PUBLIC)
	cp $< $@

$(PUBLIC)/blog: data/blog.yaml templates/blog_article.html $(PUBLIC)
	mkdir -p $@
	$(SPELL_CHECK)
	$(RENDER_BLOG) $< $@

$(PUBLIC)/static: static/ $(PUBLIC)
	mkdir -p $(PUBLIC)
	cp -r $< "$@"

$(PUBLIC):
	mkdir -p $@

deploy: all
	ghp-import -b master -p $(PUBLIC)

test: all
	python -m doctest *.py
	killall python3 || true
	cd $(PUBLIC); python3 -m http.server $(PORT) &
	sleep 0.5
	open "http://localhost:$(PORT)"
