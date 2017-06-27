all: copy

copy: freeze
	rsync -rv justus.pw root@justus.pw:/www/

freeze:
	./manage.py freeze
