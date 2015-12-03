PORT=5000

all: public
	./spell_check

clean:
	rm -rf public/*

public/CNAME: CNAME
	cp $< $@
public/blog:
	mkdir -p $@

public/static: static
	mkdir -p $@
	cp -r static/* $@/

public: public/blog public/static public/CNAME templates
	mkdir -p $@
	python render_site.py

deploy: public
	ghp-import -b master -p $<

test: public
	python -m doctest *.py
	killall python3 || true
	cd public; python3 -m http.server $(PORT) &
	sleep 0.5
	open "http://localhost:$(PORT)"
