.PHONY: deps all certs keys docker run

all: deps
	beamer make

deps:
	beamer deps

docker:
	docker build . -t erlang-dev

certs:
	mkdir -p certs && mkcert -cert-file certs/new-cert.pem -key-file certs/new-key.pem mesgd.example.com localhost 127.0.0.1 ::1

keys:
	mkdir -p keys && ssh-keygen -t rsa -f keys/new-key && mv keys/new-key keys/new-key.priv

run: docker
	docker run -itd \
		-p 4433:4433 \
		-v $$(pwd)/certs:/root/src/mesgd/certs \
		-v $$(pwd)/keys:/root/src/mesgd/keys \
		--name mesgd \
		erlang-dev \
		/root/src/mesgd/mesgd start
