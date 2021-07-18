GOCMD=go
GOBUILD=$(GOCMD) build -buildmode=plugin
OUTPUT?=${HOME}/work/sf-processor/resources/plugins

.PHONY: all
all:
	mkdir -p $(OUTPUT); $(GOBUILD) -o $(OUTPUT)/midsexporter.so .
