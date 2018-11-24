
# use latest version
FROM clojure:lein-alpine

# install update
RUN apk update

# copy my 4clojure solutions into image
WORKDIR /4clojure
ADD . /4clojure

# add lein-exec plugin
RUN echo "{:user {:plugins [[lein-exec \"0.3.7\"]]}}" > profiles.clj
