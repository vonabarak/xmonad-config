BUILDROOT := $(shell dirname $(realpath $(firstword $(MAKEFILE_LIST))))
INSTALL_PATH ?= ${HOME}/.local/bin
STACK_YAML ?= $(BUILDROOT)/stack.yaml
STACK ?= $(INSTALL_PATH)/stack --stack-yaml $(STACK_YAML)

.DEFAULT_GOAL := xmonad-x86_64-linux

compile:
	$(STACK) build

install: compile
	$(STACK) install --local-bin-path $(INSTALL_PATH)

xmonad-x86_64-linux: install
	rm -f $(BUILDROOT)/xmonad-x86_64-linux
	ln -s $(INSTALL_PATH)/xmonad $(BUILDROOT)/xmonad-x86_64-linux

uninstall:
	for i in xmonad runner xmonadctl xmobar; do rm -f $(INSTALL_PATH)/$$i; done

clean:
	rm -f $(BUILDROOT)/xmonad-x86_64-linux
	rm -f xmonad.errors
	rm -f xmonad-config.cabal
	$(STACK) clean --full
