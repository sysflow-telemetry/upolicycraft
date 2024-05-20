GOCMD=go
GOBUILD=$(GOCMD) build -buildmode=plugin
OUTPUT ?=${HOME}/sf-processor/resources/plugins

.PHONY: all
all:
	mkdir -p $(OUTPUT); $(GOBUILD) -o $(OUTPUT)/mpm.so .
