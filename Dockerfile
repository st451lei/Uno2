FROM sbtscala/scala-sbt:eclipse-temurin-jammy-17.0.10_7_1.10.0_3.4.2

WORKDIR /uno2

RUN apt-get update && apt-get install -y --no-install-recommends \
    libxext6 libxrender1 libxtst6 libxi6 libx11-6 \
    libxrandr2 libxinerama1 libxcursor1 \
    libfreetype6 fontconfig \
 && rm -rf /var/lib/apt/lists/*

COPY build.sbt /uno2/
COPY project /uno2/project
RUN sbt --batch update

COPY . /uno2

ENV JAVA_TOOL_OPTIONS="-Djava.awt.headless=false -Dui=gui"
CMD ["sbt", "--batch", "run"]