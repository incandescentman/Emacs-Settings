# Jay's Emacs Configuration Documentation

## Overview
This is a sophisticated Emacs configuration that uses Spacemacs as its foundation while incorporating extensive customizations through literate programming (org-mode files that tangle to elisp) and modular design patterns.

## Documentation Files

The following documentation files provide detailed information about various aspects of this configuration:

- [`docs/instructions.org`](docs/instructions.org) - Operational instructions for installation, usage, and maintenance
- [`docs/work-log.org`](docs/work-log.org) - History of changes and updates to the configuration
- [`docs/codebase-wisdom.org`](docs/codebase-wisdom.org) - Non-obvious insights and lessons learned from debugging
- [`docs/design-architecture.org`](docs/design-architecture.org) - Design principles and architectural decisions
- [`docs/the-emacs-settings-approach.org`](docs/the-emacs-settings-approach.org) - Philosophy and approach of this configuration
- [`docs/visual-design-philosophy.org`](docs/visual-design-philosophy.org) - Visual design principles and theming approach
- [`docs/pasteboard-architecture.org`](docs/pasteboard-architecture.org) - Architecture of the clipboard integration system
- [`docs/org-element-cache-fix.org`](docs/org-element-cache-fix.org) - Documentation of org-element cache fixes
- [`docs/backup-system.org`](docs/backup-system.org) - Backup and recovery system documentation
- [`docs/critical-next-steps.org`](docs/critical-next-steps.org) - Priority items for future development
- [`docs/experimental-mode-playbook.org`](docs/experimental-mode-playbook.org) - Guide for experimental mode features
- [`docs/ORG-ROAM-PROFILES-README.md`](docs/ORG-ROAM-PROFILES-README.md) - Guide to the multi-profile Org Roam system
- [`docs/pasteboard-markdown-detection.org`](docs/pasteboard-markdown-detection.org) - Documentation for the smart pasteboard system
- [`docs/goals-system.org`](docs/goals-system.org) - Goals workflow integration (rep counter, morning ritual, anti-stall)

## Configuration Architecture

### Core Structure
- **Framework**: Spacemacs (with option for standalone GNU Emacs)
- **Primary Config**: `spacemacs.d/init.el` (Spacemacs layers and configuration)
- **Secondary Config**: `gnu-emacs-startup.el` (standalone GNU Emacs option)
- **Shared Functions**: `shared-functions.org` → `shared-functions.el` (large collection of utilities)
- **Additional Config**: `spacemacs-new-config.el` (loaded by Spacemacs init)

### Key Components

#### 1. Literate Programming Files (*.org → *.el)
- `shared-functions.org` - Core utilities and functions
- `pasteboard-copy-and-paste-functions.org` - macOS clipboard integration
- `spacecraft-mode.org` - Custom writing mode
- `gnu-emacs-startup.org` - Alternative startup configuration
- `fonts-and-themes.org` - Appearance configuration
- Auto-tangling enabled via `#+auto_tangle: t` header

#### 2. Platform Integration
- `jay-osx.el` - macOS specific settings and `key-minor-mode-map` definition
- `pasteboard-copy-and-paste-functions.el` - Enhanced clipboard operations
- `reveal-in-finder.el` - Finder integration

#### 3. Org-Mode & Knowledge Management
- `jay-org-roam-suite/` - **Core Architecture**: Modular Org Roam system (Core, Profiles, Templates)
- `org-roam-config.el` - Legacy entry point (delegates to suite)
- `goals-agenda.org` / `goals-agenda.el` - Dynamic org-agenda config (journal + projects + leads)
- `jay-goals-system.el` - Goals workflow (rep counter, anti-stall, revenue block)
- `org-yt.el` - YouTube link support
- `org-visual-style.el` - Visual enhancements for org-mode

#### 4. Publishing & Export
- `latex-templates/` - Extensive library of custom LaTeX classes (CVs, Books, Letters)
- `ox-koma-letter.el` - KOMA-Script letter integration

#### 5. UI and Editing
- `keys.el` - Global keybinding overrides
- `fonts-and-themes.el` - Dynamic font sizing and theme management
- `smart-return.el` - Context-aware return key behavior
- `spacecraft-mode.el` - Distraction-free writing environment

#### 6. Utility Modules
- `search-commands.el` - Enhanced search functionality
- `skeletons.el` - Code templates and snippets

### Loading Order
1. Spacemacs core initialization
2. Layer configuration (org, auto-completion, compleseus, etc.)
3. `spacemacs-new-config.el`
4. `jay-osx.el` (provides key-minor-mode-map)
5. `gnu-emacs-startup.el`
6. `shared-functions.el`
7. Various feature modules (deferred or immediate)
8. Local configuration (`local-config.el`)

## Recent Major Updates

### 0. Org-Agenda Dynamic Dashboard & Dailies Fix (Mar 2026)
- **Agenda Rewrite**: Replaced static tag-driven agenda (`#focus`/`#admin`/`#connect`/`#batch`) with dynamic TODO dashboard in `goals-agenda.org`.
- **Dynamic Sources**: Agenda files built on every open via `:before` advice on `org-agenda` — pulls from today's journal + last 3 journal files + all velocity project files + lead tracking files.
- **No Tags Required**: Plain `TODO` headings are sufficient for agenda visibility. Energy tags are optional organizational sugar.
- **Journal Carry-Forward**: `jay/recent-journal-files` lists the journal directory and picks the actual last 3-4 files by filename sort (not calendar math), so skipped days don't break the chain.
- **Dailies Navigation Fix**: Removed stray daily note at roam root that was confusing `org-roam-dailies-goto-previous-note`. Daily notes live in `~/Dropbox/roam/journal/`.
- **Recurring Reset**: Rescheduled 7 recurring items (daily/weekly repeaters) from 2024–2025 dates to current, eliminating "Sched.400x" clutter.

### 0a. Org 9.8 Compatibility & Navigation (Feb 2026)
- **Link Preview Migration**: Migrated `org-yt.el` and `shared-functions` from deprecated `org-toggle-inline-images`/`org-display-inline-images` to new `org-link-preview` API.
- **Backward Compatibility**: Added version-checking wrappers that work on Org 9.6+ and will use native Org 9.8 features when available.
- **Repeat-mode Navigation**: Enabled `repeat-mode` with custom repeat maps for Org heading/block/link navigation (press `C-c C-n`, then just `n` to continue).
- **Which-key Descriptions**: Added descriptive labels for all custom prefix keys (`s-k`, `s-/`, `s-u`, `]`) and org-roam bindings for better discoverability.
- **Legacy Cleanup**: Deprecated `org-roam-config.el` to a compatibility shim pointing to `jay-org-roam-suite/`.

### 1. Web Page Capture (Feb 2026)
- **org-web-tools**: Installed for capturing web pages as org-mode content
- **Chrome Integration**: `jay/get-url-from-chrome` grabs URL from frontmost Chrome tab via AppleScript
- **Clean Commands**: `jay/org-web-tools-read-url-as-org-clean` and `jay/org-web-tools-insert-web-page-as-entry-clean`
- **Enhanced Cleanup**: All paste/capture methods (`chatgpt2org`, `org-web-tools`, `pasteboard-paste-adaptive`) now:
  - Strip social share links, base64 images, view counts
  - Remove relative timestamps ("2 years ago", "3 months ago")
  - Remove whitespace-only lines
  - Remove blank lines between headings and body text (proper org-mode style)
  - Convert bold lines that look like headings to actual org headings

### 2. Org Roam Profile System (Nov 2025, updated Mar 2026)
- **Multi-Graph Support**: Distinct profiles for 'Work' (default), 'My Life', 'Social', and 'Parents'.
- **Architecture**: Modularized into `jay-org-roam-suite/` (Core, Profiles, Templates).
- **Features**: Independent databases, capture templates, and export paths per profile.
- **Template Cleanup (Mar 2026)**: Merged default profile's two template lists into one alphabetically sorted list (A a B b C c...). Simplified parents profile to standard templates (conversation, document, interview, note, person, story, task).
- **Commands**: `s-u P` to switch, `s-u 1/2/3/4` shortcuts.

### 3. Documentation Overhaul (Oct 2025)
- **Complete Rewrite**: Transformed technical notes into user-friendly guides (`instructions.org`, `design-architecture.org`).
- **New Guides**: Added `ORG-ROAM-PROFILES-README.md`, `pasteboard-markdown-detection.org`.
- **Philosophy**: Shifted to "comprehensive guides teaching concepts".

### 4. Pasteboard & Editor Hardening
- **Pure Elisp**: Converted `pasteboard-copy-and-paste-functions` to pure `.el` for reliability.
- **Smart Paste**: Improved Markdown detection, blockquote handling, and table formatting.
- **Formatting**: Added `scripts/org_spacing_formatter.py` for automatic heading spacing.

### 5. LaTeX & Publishing
- **New Templates**: Added 'Hipster CV' and 'Elegant Less Whitespace' templates in `latex-templates/`.
- **Export**: Enhanced `ox-astro` integration with profile-aware source roots.

### 6. System Stability
- **Cloud Storage**: Fixed critical Dropbox/CloudStorage path resolution issues and timeouts.
- **File Watchers**: Optimized `file-notify` to prevent sync errors with mobile edits.
- **Startup**: Cleaned up unused packages and deprecated variables.

## Potential Future Improvements

### High Priority
1. **Deprecated Functions**: Update remaining deprecated functions (e.g., `cl-return` instead of `return`)
2. **Lexical Binding**: Ensure all files properly declare lexical-binding on first line
3. **Error Handling**: Add error checking for missing files/directories in configuration

### Medium Priority
1. **Performance**: Profile startup time and optimize loading order
2. **Documentation**: Add docstrings to custom functions
3. **Consistency**: Standardize quote style ('quote vs ')
4. **Path Management**: Use variables instead of hardcoded paths ("/Users/jay/")
5. **Cleanup**: Remove large blocks of commented-out code

### Low Priority
1. **Indentation**: Standardize to spaces or tabs (currently mixed)
2. **Naming Convention**: Establish consistent prefix for custom functions
3. **Compilation**: Set up automatic byte-compilation for better performance
4. **Testing**: Add basic tests for critical custom functions

## Configuration Patterns

### Good Practices Already in Place
- Literate programming for documentation
- Modular file organization
- Deferred loading for performance
- Platform-specific customizations isolated
- Version control with recovery scripts
- Custom layer for Spacemacs integration

### Areas for Enhancement
- Dependency management between modules
- Better separation of concerns
- More consistent error handling
- Automated testing/validation
- Performance monitoring

## Key Dependencies
- **External**: Spacemacs, org-roam, Hunspell, various ELPA packages
- **Internal**: `key-minor-mode-map` (defined in jay-osx.el)
- **Paths**: ~/emacs/Spelling/, ~/Dropbox/writing/, various project directories

## Notes for Next Session
1. Review startup messages for any remaining warnings
2. Profile load times to identify bottlenecks (if performance becomes an issue)
3. Consider migrating to use-package for all package configuration
4. Review and update deprecated org-mode variables
5. Set up proper error handling for missing dependencies
6. Add a tiny batch smoke-test (`emacs --batch --load ~/.emacs.d/init.el --eval "(with-temp-buffer (org-mode) (message \"shift %S\" org-support-shift-select))"`) after tweaking startup; this catches regressions like the `shift-select-mode` autoload failure before they land.

### Guidance for AI/LLM Editing Sessions
- **Don't ask, just do**: When Jay provides a file path, READ IT FIRST before asking questions. If he says "here's an error" or "look at this" with a path, just read the file—don't ask what it is or request more context.
- When touching any literate Org file (e.g., `pasteboard-copy-and-paste-functions.org`), delete the stale tangled `.el`, retangle immediately, and run `emacs --batch <file.el> --eval '(check-parens)'` before restarting.
- Treat regex tweaks with extra caution: smoke-test them in a scratch buffer (`re-search-forward`) to avoid `invalid-regexp` surprises.
- Keep the edit loop tight—edit → tangle → `check-parens` (or byte-compile)—to catch structural errors before they reach Spacemacs startup.
- **Test runtime behavior instead of theorizing**: When uncertain whether a keybinding, function, package, or config setting actually works at runtime, do not speculate — ask Jay to test it, or run a direct safe test yourself if appropriate.
- **Config analysis is not runtime proof**: Static config inspection tells you what should happen, not what does happen. Autoloads, stale installs, indirect loading, compatibility shims, and cached state can all keep something working even when the config looks contradictory.
- **No “probably broken” claims without evidence**: Avoid phrases like “likely failing silently” unless you have reproduced the failure or Jay has reported the exact behavior.
- **For keybinding questions, ask for the shortest live check**: If the real question is “does this binding still work?”, ask Jay to press the binding and report the result before recommending cleanup or replacement.
- **State hypotheses as hypotheses**: If you must reason before testing, label conclusions as tentative and explicitly defer to the runtime test.

### Checkpoint Protocol
- At each checkpoint, update `/Users/jay/emacs/emacs-settings/docs/work-log.org` before handing off.
- Then commit the current checkpoint in git before moving on.
- Then Jay launches Emacs and confirms startup is clean with no parsing or startup errors.
- Only after Jay confirms Emacs launches correctly should the next step begin.
- If the checkpoint changed a literate Org file, make sure the corresponding tangled `.el` has been regenerated and validated before the checkpoint commit.

## Note on File Size
- `shared-functions.org`: ~316KB (9983 lines) 
  - **Splitting not recommended**: For a personal config, having everything in one searchable file is actually more convenient than managing multiple files with inter-dependencies. The monolithic approach is working well - Emacs handles it fine, and it's easier to maintain when you know where everything is
- `spacemacs.d/init.el`: ~50K lines
- `gnu-emacs-startup.el`: ~46K lines  
- `pasteboard-copy-and-paste-functions.el`: ~33K lines

---
*Generated: 2025-09-05*
*Updated: 2025-10-11*
*Last fixes: Fixed file-notify errors with Dropbox sync and cloud storage*
