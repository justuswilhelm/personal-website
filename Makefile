PORT=5000

all: public/blog public/index.html public/projects.html public/blog public/static public/CNAME

public/%.html: pages/%.yaml render_page.py templates/%.html
	./render_page.py $<

clean:
	rm -rf public/*

public/CNAME: CNAME
	cp $< $@

public/blog: blog/ render_blog.py templates/*blog*
	./spell_check
	mkdir -p $@
	./render_blog.py

public/static: static
	mkdir -p $@
	cp -r $</* "$@"

public:
	mkdir -p $@

deploy: all
	ghp-import -b master -p $<

test: all
	python -m doctest *.py
	killall python3 || true
	cd public; python3 -m http.server $(PORT) &
	sleep 0.5
	open "http://localhost:$(PORT)"
