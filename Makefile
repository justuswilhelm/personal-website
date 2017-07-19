.PHONY: all copy freeze post

domain := justus.pw

all: copy

copy: freeze
	rsync -rv "$(domain)" "root@$(domain):/www/"
	rsync -rv caddy/Caddyfile "root@$(domain):~/Caddyfile"

freeze: clean
	./manage.py freeze

post:
	script/create_post

clean:
	rm -rf $(domain)
