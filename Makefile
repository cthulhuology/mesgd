.PHONY: deps all certs keys

all: deps
	beamer make

deps:
	beamer deps

certs:
	mkdir -p certs && mkcert -cert-file certs/new-cert.pem -key-file certs/new-key.pem mesgd.example.com localhost 127.0.0.1 ::1

keys:
	mkdir -p keys && ssh-keygen -t rsa -f keys/new-key && mv keys/new-key keys/new-key.priv
