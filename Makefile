# Gopher, the browser.
#
#   make deps    restore the dependencies ocicl.csv pins, into ./ocicl/
#   make test    run the FiveAM suite
#   make run     run the browser from source, unbundled
#   make app     build build/Gopher.app
#   make icon    draw res/icon.png again, from tools/icon.lisp
#   make dmg     the bundle in a disk image, dist/Gopher-<version>-<arch>.dmg
#   make notarize-dmg   submit that disk image to Apple, and staple the ticket
#   make clean   remove the bundle, the disk images and the fasls
#
# The gopher library is not in ocicl: it is the sibling checkout GOPHER_DIR
# (../gopher by default).  Build with a safepoint SBCL: asdf-macos-app ships
# the runtime of whichever SBCL performs the build, and fetches run on Lisp
# threads beside AppKit.

SBCL ?= sbcl

# Signing, which is personal: local.mk, never committed.
-include local.mk
# Exported: gopher-browser-app.asd reads it from the environment, and the
# child SBCL that builds the bundle sees only that.
export MACOS_SIGNING_IDENTITY

GOPHER_DIR ?= ../gopher
# Optional sibling checkouts that shadow the pinned objc and asdf-macos-app.
OBJC_DIR ?=
MACOS_APP_DIR ?=

# :IGNORE-INHERITED-CONFIGURATION so that a dependency missing from ocicl.csv
# fails here, rather than resolving to whatever the developer's own source
# registry happens to reach.  The library is a :DIRECTORY, not a :TREE, so
# that its own ocicl/ directory stays out of the way.
REGISTRY = (asdf:initialize-source-registry \
              (list :source-registry \
                    (list :directory (truename "$(GOPHER_DIR)/")) \
                    $(if $(OBJC_DIR),(list :tree (truename "$(OBJC_DIR)/"))) \
                    $(if $(MACOS_APP_DIR),(list :tree (truename "$(MACOS_APP_DIR)/"))) \
                    (list :tree (truename "./")) \
                    :ignore-inherited-configuration))

LISP = $(SBCL) --non-interactive --no-userinit --no-sysinit \
         --eval '(require :asdf)' --eval '$(REGISTRY)'

# What the disk image is called: the version the bundle says, and the machine
# it was built on, which is the machine it runs on.
VERSION := $(shell sed -n 's/.*:version "\(.*\)".*/\1/p' gopher-browser-app.asd | head -1)
ARCH := $(shell uname -m)
DIST = dist
DMG = $(DIST)/Gopher-$(VERSION)-$(ARCH).dmg
APP = build/Gopher.app

.PHONY: deps test run app icon dmg notarize-dmg clean

deps:
	ocicl install

test:
	$(LISP) --eval '(asdf:load-system "gopher-browser/tests")' \
	        --eval '(uiop:quit (if (gopher-browser/tests:run-tests) 0 1))'

run:
	$(LISP) --eval '(asdf:load-system "gopher-browser")' \
	        --eval '(gopher-browser:main)'

app:
	$(LISP) --eval '(asdf:make "gopher-browser-app")'

# The icon is drawn by a program.  Its PNG is checked in, so this is only for
# when tools/icon.lisp changes.
icon:
	$(LISP) --eval '(asdf:load-system "objc")' \
	        --load tools/icon.lisp \
	        --eval '(gopher-browser-icon:main)'

# A disk image, which is what people expect to download: the application and
# a link to /Applications beside it, to drag it onto.  Signed when
# MACOS_SIGNING_IDENTITY names a Developer ID, as the application inside is.
dmg: app
	@mkdir -p "$(DIST)"
	rm -rf "$(DIST)/stage" "$(DMG)"
	mkdir -p "$(DIST)/stage"
	ditto "$(APP)" "$(DIST)/stage/Gopher.app"
	ln -s /Applications "$(DIST)/stage/Applications"
	hdiutil create -volname "Gopher $(VERSION)" -srcfolder "$(DIST)/stage" \
	  -fs HFS+ -format UDZO -ov "$(DMG)"
	rm -rf "$(DIST)/stage"
	@if [ -n "$(MACOS_SIGNING_IDENTITY)" ] && [ "$(MACOS_SIGNING_IDENTITY)" != "-" ]; then \
	  codesign --force --sign "$(MACOS_SIGNING_IDENTITY)" --timestamp "$(DMG)"; \
	else \
	  echo "note: the disk image is unsigned, and the application in it signed ad hoc"; \
	fi
	@echo "built $(DMG)"

# Notarise the disk image and staple the ticket to it.  Needs a Developer ID
# build, and either a keychain profile (NOTARY_PROFILE, made once with
# `xcrun notarytool store-credentials') or an App Store Connect API key
# (NOTARY_KEY, NOTARY_KEY_ID, NOTARY_ISSUER), which is what CI has.
notarize-dmg:
	@test -f "$(DMG)" || { echo "error: no $(DMG); make dmg first" >&2; exit 1; }
	@codesign -dvv "$(APP)" 2>&1 | grep -q 'flags=.*adhoc' && { \
	  echo "error: $(APP) is signed ad hoc, and Apple will refuse it." >&2; exit 1; } || true
	@if [ -n "$(NOTARY_KEY)" ]; then \
	  xcrun notarytool submit "$(DMG)" --key "$(NOTARY_KEY)" --key-id "$(NOTARY_KEY_ID)" \
	    --issuer "$(NOTARY_ISSUER)" --wait; \
	else \
	  xcrun notarytool submit "$(DMG)" --keychain-profile "$(NOTARY_PROFILE)" --wait; \
	fi
	xcrun stapler staple "$(DMG)"
	spctl -a -vvv -t open --context context:primary-signature "$(DMG)"

clean:
	rm -rf build dist
	find . -name '*.fasl' -not -path './ocicl/*' -delete
