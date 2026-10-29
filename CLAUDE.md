# CLAUDE.md

Guidance for Claude Code when working in this repository.

## What this is

A macOS browser for Gopher, Gemini, Spartan, Nex, Guppy and Finger, in Common
Lisp. The protocols are the `gopher` library, a sibling checkout at
`../gopher` (not in ocicl). AppKit is reached through `objc`; the bundle is
built by `asdf-macos-app`. SBCL (a safepoint build), ocicl, FiveAM.

## Build & test

```
ocicl install    # dependencies (ocicl/ is gitignored)
make test        # the suite; the ui suite skips without a window server
make run         # from source
make app         # build/Gopher.app
make icon        # res/icon.png from tools/icon.lisp
make dmg         # dist/Gopher-<version>-<arch>.dmg
tools/check-dmg.sh dist/*.dmg   # run it from the image against a local server
```

CI (`.github/workflows/macos.yml`) checks the library out into `./gopher` and
sets `GOPHER_DIR=gopher`. It runs on master of lispnik/gopher, so push a
library change before the app change that needs it.

The Makefile's source registry is this tree, `GOPHER_DIR` as a `:directory`
(not a `:tree`, which would sweep in the library's own `ocicl/`), and
`:ignore-inherited-configuration`. A dependency missing from `ocicl.csv`
fails here. When one does, `ocicl install <name>` it.

To run one test from a REPL with that registry:
`(asdf:load-system "gopher-browser/tests")`, then
`(fiveam:run! 'gopher-browser/tests::stop-stops)`.

## Architecture

- `src/` is `gopher-browser/core` and knows nothing of AppKit, so its
  decisions are tested headless.
  - `page.lisp`: a library DOCUMENT becomes a PAGE of RUNs (string and style
    keyword, and a link index). Both whole documents and streamed batches go
    through `page-add-lines`, so they can't diverge. Menu lines are detected
    by `line-item-type`.
  - `navigation.lisp`: `session-fetch` never signals. Everything comes back as
    an OUTCOME whose kind says what the window must do (`:page :download
    :input :redirect :external :certificate-changed :certificate-required
    :error :cancelled`). Cross-scheme redirects are the window's to follow
    (the library follows same-scheme ones). Schemes the library doesn't speak
    are `:external`.
  - The cache and `known_hosts` are shared by all windows; history is per
    window. `forget-shared-state` drops them (the tests change data directory).
- `src/macos/`: one `browser-controller` per window is target, window
  delegate and text-view delegate, so menu actions reach it through the
  responder chain. The application delegate answers the same actions when no
  window is key.
  - Fetches run on `sb-thread`s. They report through `post-to-main-thread`
    (a queue drained by `gbDrainQueue:` on a `queue-target` object), tagged
    with the controller's GENERATION. A newer navigation or Stop moves the
    generation on, and stale reports are dropped.
  - Links in the text carry `gopher-browser-link:N` as their `NSLink` value;
    `textView:clickedOnLink:atIndex:` maps it back to the page's link N.
  - Style dictionaries are cached in `*style-attributes*`. MAIN calls
    `reset-text-styles` because pointers from the build are garbage in the
    saved image. The same goes for any foreign object: make it at run time.
- Dialogs go through `*dialog-answerer*` when it is set. That is how the UI
  tests answer them.
- The window is `GopherBrowserWindow`, an NSWindow subclass whose
  `sendEvent:` sees every event first: mouse buttons 3/4 and swipes become
  Back/Forward, and keys on the page (Tab, Return, digits, Escape) go to
  `page-key`. Anything else goes to `super`.
- Back, Forward, Reload, Home and the address field are an NSToolbar
  (`install-toolbar`), unified style, title hidden: macOS then puts the tab
  bar under the toolbar, as Safari does. The items are made with the window
  and kept in `controller-toolbar-items` (the delegate hands them over), so
  they exist for windows never shown; `controller-back-button` and friends
  are NSToolbarItems, with `setAutovalidates: NO` so `refresh-controls`
  decides what is enabled.
- The self test's snapshot captures the window from the screen with
  `CGWindowListCreateImage`: a process may capture its own windows without
  Screen Recording permission (`screencapture -l` from a shell may not). The
  rectangle is passed by value with SB-ALIEN, which CFFI cannot do. If that
  fails it draws the window offscreen (`cacheDisplayInRect:`), where toolbar
  items and the selected tab come out as blank white shapes.
- Tabs are macOS window tabs (`tabbingIdentifier` "GopherBrowser"); a tab is a
  window with its own controller. `current-session-groups` reads them back
  through `tabGroup` for `session.sexp`.
- `controller-link-ranges` maps link index -> (START . LENGTH) of its text in
  **UTF-16 units**, as NSString counts: `runs-attributed-string` computes them
  (link numbers included). Tab focus and hover both use them.
- `set-status` keeps the status text; while a link is hovered the line shows
  its address instead (`hover`), and the text comes back after.
- Downloads belong to `*downloads*` and the Downloads window, not to the
  browser window: they don't touch its generation or loading state. UI tests
  wait for them with `wait-for-downloads`.
- Every request goes through `network-options` (proxy, TLS on Gopher, the
  trusted certificates, the host's identity): fetches, downloads, Gopher+
  attributes and forms, and Titan uploads alike.
- Special addresses `session-fetch` answers itself: `about:…`,
  `view-source:<url>` (the inner page as text), and a Gopher URL whose Gopher+
  field is `!` (`gopher-plus-info-url`), drawn as an Item Information page.
- A `?` Gopher+ link navigates with `:form t`; the :FORM outcome's questions
  are asked with `ask-gopher-plus-form` and sent with `:answers`. A Note has
  no answer. Titan uploads navigate with `:upload (data mime token)`.
- Forms, the Titan upload and Settings are one modal builder, `run-form`
  (dialogs.lisp), over a list of FIELDs. With `*dialog-answerer*` set it calls
  that with (kind fields) and expects one value per field.
- A page answering a query is recorded at `url-with-search` (Gopher: the
  `%09` search field; others: `?query`), so Back, Reload, history and
  bookmarks repeat the search instead of asking again. This was a reported
  bug: search, follow a result, Back → asked again.
- Swipes: two-finger page swipes arrive as scroll events with phases; the
  window's `swipe-tracker` (swipe.lisp, pure) watches them pass and decides
  on the gesture's end. Fingers moving right = Back, whichever way the
  scroll-direction setting turns the deltas (`isDirectionInvertedFromDevice`).
  A three-finger NSEventTypeSwipe with deltaX > 0 is Back (as in Chromium and
  Firefox). Logitech Options+ sends one for its mouse Back/Forward buttons, so
  the sign reversed is the side buttons reversed: a reported bug.
- Gopher+ files: ChooseF sends the file after the answers (the library's
  `:gopher-plus-file`, a counted `+N` block); AskF names a local file the reply
  is saved to. Both send only the file's name as their answer line.
- Identities (identities.lisp) are listed in `identities.sexp`, each with a
  host and a path; `identity-for-url` picks the longest whole-segment path
  prefix. The first read of the list adopts pre-list files
  (`identities/HOST-cert.pem`), so `create-identity` reads the list before
  writing its files, or it would adopt itself.
- Settings is one form (`settings-fields` / `apply-settings`): everything is
  checked before anything is kept.
- `*show-new-windows*` NIL keeps new windows off screen (tests).
- History has two levels. A window's Back/Forward is `session-history`, and
  it moves only when the page arrives (`navigate :record :back`). The History
  menu's history is `visits` in `history.sexp`.

## Things that are easy to get wrong

- A `let` binding is invisible to the fixture servers, which run on threads of
  their own: set globals (`*gopher-port-for-redirect*`) with `setf`.
- `cl+ssl::ssl-stream-x509-certificate` gives a null pointer, not NIL, when
  the client sent no certificate.
- A Gopher `URL:` item's link is already the web URL (the library rewrites
  it), so it reaches `session-fetch` as `https://…`, not as a Gopher fetch.
- Names: FiveAM exports `with-fixture`; `go` and `byte-size` are in CL. All
  three have been defined by accident here once.
- A BOOL argument arrives in a Lisp method as T/NIL or 1/0 depending on the
  path; test with `(member x '(0 nil))`.
- `write-data-file` binds `*print-readably*` to NIL: otherwise a base-string
  (titles made by FORMAT can be one) prints as `#A((19) BASE-CHAR . "…")`.
- Data files are read in the keyword package, so a hand-written `T` is `:T`.
- A response can come from the cache, and then no request is made at all:
  a test that wants to see the proxy (or anything else on the wire) used must
  ask for a page it has not fetched.
- The snapshot a self test writes has a transparent background where the
  window draws its own, so light-on-dark text in the bars disappears in it.
  That is the snapshot, not the app.
