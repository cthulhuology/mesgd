FROM erlang

RUN apt-get update \
    && runtimeDeps='vim' \
    && buildDeps='' \
    && apt-get install -y --no-install-recommends $runtimeDeps \
    && apt-get install -y --no-install-recommends $buildDeps \
    && apt-get purge -y --auto-remove $buildDeps

ENV HOME /root
ENV SRC_DIR $HOME/src

RUN mkdir -p $SRC_DIR \
    && repos='cthulhuology/beamer cthulhuology/mesgd' \
    && for repo in $repos; do mkdir -p $SRC_DIR/`basename $repo`; git clone https://github.com/$repo $SRC_DIR/`basename $repo`; done \
    && chmod 755 $SRC_DIR/beamer/beamer \
    && mkdir -p $HOME/.beamer \
    && touch /tmp/foo \
    && erlc -W -o $HOME/.beamer $SRC_DIR/beamer/src/beamer.erl

ENV PATH $SRC_DIR/beamer:$PATH

COPY .erlang /root/.erlang

# optional; mesgd-specific
RUN cd $SRC_DIR/mesgd \
    && make


WORKDIR $SRC_DIR
CMD [ "/bin/bash", "-l" ]
