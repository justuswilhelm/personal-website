PORT=5000

all: public

clean:
	rm -rf public/
public: clean
	python render_site.py
	./spell_check

deploy: public
	ghp-import -b master -p $<

test: public
	python -m doctest *.py
	killall python3
	cd public; python3 -m http.server $(PORT) &
	sleep 0.5
	open "http://localhost:$(PORT)"
