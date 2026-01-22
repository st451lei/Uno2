FROM hseeberger/scala-sbt

WORKDIR /uno2

ADD . /uno2

CMD ["sbt", "run"]