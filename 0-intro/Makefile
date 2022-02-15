FML = ../../FML/target/release/fml
SRCS = $(wildcard src/*.fml)
TARGETS = $(subst .fml,,$(subst src/,run-,$(SRCS)))

all: $(TARGETS)

run-%: src/%.fml
	@echo "$@"
	@$(FML) run $<
