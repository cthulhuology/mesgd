.PHONY: deps all keys start

all: deps
	beamer make

deps:
	beamer deps

keys:
	openssl genrsa -out keys/private/admin.pem 4096
	openssl rsa -in keys/private/admin.pem -pubout -out keys/public/admin.pem
	openssl genrsa -out keys/private/mesgd.pem 4096
	openssl rsa -in keys/private/mesgd.pem -pubout -out keys/public/mesgd.pem

start:
	epmd -daemon
	./mesgd start
