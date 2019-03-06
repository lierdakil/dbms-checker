.PHONY: all base server api client image dist deploy
all: base server api client image dist

base:
	make -C dist/base

server:
	stack build

api: server
	stack exec client-api-generator > client/src/api/api.d.ts

client: api
	make -C client

image: base client server
	stack image container

dist: image
	make -C dist/entry

deploy: dist
	docker tag dbms-checker-server:latest solar.lan:5000/dbms-checker-server:latest
	docker push solar.lan:5000/dbms-checker-server:latest
