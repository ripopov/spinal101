FROM mcr.microsoft.com/devcontainers/base:ubuntu-24.04

ARG DEBIAN_FRONTEND=noninteractive
ARG MILL_VERSION=0.12.17

RUN apt-get update && \
    apt-get install -y --no-install-recommends \
      ca-certificates \
      curl \
      gawk \
      git \
      jq \
      make \
      openjdk-17-jdk \
      python3-pip \
      python3-venv \
      verilator && \
    rm -rf /var/lib/apt/lists/*

RUN curl -fsSL "https://repo1.maven.org/maven2/com/lihaoyi/mill-dist/${MILL_VERSION}/mill-dist-${MILL_VERSION}-mill.sh" \
      -o /usr/local/bin/mill && \
    chmod +x /usr/local/bin/mill

RUN python3 -m venv /opt/volare-venv && \
    /opt/volare-venv/bin/pip install --no-cache-dir --upgrade pip && \
    /opt/volare-venv/bin/pip install --no-cache-dir volare

ENV JAVA_HOME=/usr/lib/jvm/java-17-openjdk-amd64
ENV PATH=/opt/volare-venv/bin:${PATH}
