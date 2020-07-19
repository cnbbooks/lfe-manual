BIN = mdbook
GEN := $(shell which $(BIN) 2> /dev/null)
DOWNLOAD = https://github.com/rust-lang/mdBook/releases

define BINARY_ERROR

No $(BIN) found in Path.

Download $(BIN) from $(DOWNLOAD).

endef

build:
ifndef GEN
	$(error $(BINARY_ERROR))
endif
	@$(GEN) build

serve:
	@$(GEN) serve

run: serve

publish:
	-@cd $(PUBLISH_DIR) && \
	git add images/* && \
	git commit -am "Regenerated documentation site." > /dev/null && \
	git push origin master
	-@git add $(PUBLISH_DIR) && \
	git commit -am "Updated submodule for recently published site content." && \
	git submodule update && \
	git push origin site-builder
