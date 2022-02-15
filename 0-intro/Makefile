FML = ../../FML/target/release/fml
SRCS = $(wildcard src/*.fml)
TARGETS = $(subst .fml,,$(subst src/,run-,$(SRCS)))

.PHONY: all

all: $(TARGETS)

run-%: src/%.fml
	@echo -e '\033[32m$@\033[0m'
	@$(FML) run $<
