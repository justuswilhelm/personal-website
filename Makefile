domain := justus.pw

all: copy

copy: freeze
	rsync -rv "$(domain)" "root@$(domain):/www/"
	rsync -rv caddy/Caddyfile "root@$(domain):~/Caddyfile"

freeze:
	./manage.py freeze

post:
	script/create_post
