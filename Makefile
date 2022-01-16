
ifeq ($(origin .RECIPEPREFIX), undefined)
  $(error This Make does not support .RECIPEPREFIX. Please use GNU Make 4.0 or later)
endif
.RECIPEPREFIX = >

SHELL := bash
.SHELLFLAGS := -eu -o pipefail -c
.ONESHELL:
.DELETE_ON_ERROR:
MAKEFLAGS += --warn-undefined-variables
MAKEFLAGS += --no-builtin-rules

.PHONY: all test build build-dev build-gh-pages

all: test build

test:
>npx elm-test 'tests/**/*Test.elm'

build/elm.js: $(shell find src -iname '*.elm' -type f)
>elm make src/Main.elm --optimize --output=build/elm.js

build/elm.min.js: build/elm.js
>uglifyjs build/elm.js --compress 'pure_funcs="F2,F3,F4,F5,F6,F7,F8,F9,A2,A3,A4,A5,A6,A7,A8,A9",pure_getters,keep_fargs=false,unsafe_comps,unsafe' \
   | uglifyjs --mangle --output build/elm.min.js

site/main.css: src/main.scss
>sass --no-source-map src/main.scss site/main.css

build-dev: build/elm.js site/main.css
>cp build/elm.js site/main.js

build-gh-pages: build/elm.min.js site/index.html site/main.css
>cp site/* docs/
>cp build/elm.min.js docs/main.js
