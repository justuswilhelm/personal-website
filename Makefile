all: public

clean:
	rm -rf public/
public: clean
	python render_site.py

deploy: public
	ghp-import -b master -p $<
