public:
	python render_site.py
deploy: public
	ghp-import -p $<
