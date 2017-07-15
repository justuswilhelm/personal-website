.PHONY: all copy freeze post

domain := justus.pw

all: copy

copy: freeze
	rsync -rv "$(domain)" "root@$(domain):/www/"
	rsync -rv caddy/Caddyfile "root@$(domain):~/Caddyfile"

freeze:
	rm -rf $(domain)
	./manage.py freeze

post:
	script/create_post
