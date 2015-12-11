PUBLIC=public

pages=$(PUBLIC)/index.html $(PUBLIC)/projects.html $(PUBLIC)/blog_index.html
PORT=5000

all: $(PUBLIC)/blog $(pages) $(PUBLIC)/static $(PUBLIC)/CNAME

$(PUBLIC)/%.html: pages/%.yaml render_page.py templates/%.html $(PUBLIC)
	./render_page.py $<

clean:
	rm -rf $(PUBLIC)

$(PUBLIC)/CNAME: CNAME $(PUBLIC)
	cp $< $@

$(PUBLIC)/blog: blog/ templates/blog_article.html $(PUBLIC)
	./spell_check
	./render_blog.py

$(PUBLIC)/static: static/ $(PUBLIC)
	mkdir -p $(PUBLIC)
	cp -r $< "$@"

$(PUBLIC):
	mkdir -p $@
	mkdir -p $@/blog

deploy: all
	ghp-import -b master -p $(PUBLIC)

test: all
	python -m doctest *.py
	killall python3 || true
	cd $(PUBLIC); python3 -m http.server $(PORT) &
	sleep 0.5
	open "http://localhost:$(PORT)"
