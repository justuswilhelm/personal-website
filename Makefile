GIT_BRANCH=master
PUBLIC=build

freeze:
	./manage.py freeze

deploy: freeze
	ghp-import -b $(GIT_BRANCH) -p $(PUBLIC)
