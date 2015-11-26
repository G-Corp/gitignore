PROJECT = gitignore

DEPS = lager eutils
DOC_DEPS = edown

dep_lager = git https://github.com/basho/lager.git master
dep_eutils = git https://github.com/emedia-project/eutils.git master
dep_edown = git https://github.com/botsunit/edown.git master

include erlang.mk

EDOC_OPTS = {doclet, edown_doclet} \
						, {app_default, "http://www.erlang.org/doc/man"} \
						, {source_path, ["src"]} \
						, {overview, "overview.edoc"} \
						, {stylesheet, ""} \
						, {image, ""} \
						, {top_level_readme, {"./README.md", "https://github.com/botsunit/${PROJECT}"}}

