all: public

clean:
	rm -rf public/
public: clean
	python render_site.py
	./spell_check

deploy: public
	ghp-import -b master -p $<
