
SRCS=$(wildcard src/*.erl)
OBJS=$(patsubst src/%.erl, ebin/%.beam, $(SRCS))

all: $(OBJS)

ebin/%.beam: src/%.erl
	erlc +debug_info -W -o ebin $<

clean:
	rm -rf ebin/*.beam


