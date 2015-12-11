pages=public/index.html public/projects.html public/blog_index.html
PORT=5000

all: public/blog $(pages) public/static public/CNAME

public/%.html: pages/%.yaml render_page.py templates/%.html
	./render_page.py $<

clean:
	rm -rf public

public/CNAME: CNAME
	cp $< $@

public/blog_index.html: templates/blog_index.html
	./render_blog_index.py

public/blog: blog/ templates/blog_article.html public
	./spell_check
	./render_blog.py

public/static: static/ public
	mkdir -p public
	cp -r $< "$@"

public:
	mkdir -p $@
	mkdir -p $@/blog

deploy: public all
	ghp-import -b master -p $<

test: all
	python -m doctest *.py
	killall python3 || true
	cd public; python3 -m http.server $(PORT) &
	sleep 0.5
	open "http://localhost:$(PORT)"
