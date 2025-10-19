# Org-Roam Multi-Database Profile System

A clean system for managing multiple independent org-roam databases with different capture templates and configurations.

## Files

1. **jay-org-roam-profiles.el** - Core profile switching system
2. **jay-org-roam-lazy-v2.el** - Updated main config with profile integration

## Quick Setup

### 1. Place the files in your config directory

```bash
cp jay-org-roam-profiles.el ~/emacs/emacs-settings/
cp jay-org-roam-lazy-v2.el ~/emacs/emacs-settings/jay-org-roam-lazy.el
```

### 2. Load in your init.el

Replace your current org-roam config with:

```elisp
;; Load the profile system first
(load "~/emacs/emacs-settings/jay-org-roam-profiles.el")

;; Then load the main config
(load "~/emacs/emacs-settings/jay-org-roam-lazy.el")
```

### 3. Create the directories

The directories will be created automatically, but you can create them manually:

```bash
mkdir -p ~/Dropbox/roam-life/journal
```

## Usage

### Switching Profiles

**Interactive (with completion):**
```
M-x jay/org-roam-switch-profile RET
```
Then select either:
- `default` (your work database at ~/Dropbox/roam)
- `my-life` (your personal database at ~/Dropbox/roam-life)

**Quick switches with keybindings:**
```
s-u P         Switch profile (interactive)
s-u C-p       Show current profile info
s-u 1         Quick switch to default (work)
s-u 2         Quick switch to my-life (personal)
```

### What Happens When You Switch?

1. **Database closed** - Current database connection is safely closed
2. **Configuration updated** - Directory, database location, and capture templates are changed
3. **Database reopened** - New database connection is established
4. **Profile saved** - Your choice is remembered for next session
5. **Confirmation** - You'll see: "Switched to org-roam profile: my-life (My Life)"

## Profiles

### Default (Work)
- **Directory:** `~/Dropbox/roam/`
- **Database:** Auto-managed in XDG cache
- **Templates:** Your full work setup (accountability, AI, job hunt, people, quotes, etc.)

### My-Life (Personal)
- **Directory:** `~/Dropbox/roam-life/`
- **Database:** `~/Dropbox/roam-life/.org-roam.db`
- **Templates:** Life-focused
  - `j` - journal entry
  - `g` - gratitude
  - `r` - reflection
  - `p` - person (personal)
  - `h` - health & wellness
  - `f` - family
  - `m` - memory / experience
  - `i` - idea / creative
  - `b` - book notes (personal)
  - `t` - travel
  - `l` - learning / study
  - `n` - note (general)

## Customization

### Adding a Third Profile

Edit `jay-org-roam-profiles.el` and add to `jay/org-roam-profiles`:

```elisp
(research
  :name "Research Notes"
  :directory "~/Documents/research-roam"
  :db-location "~/Documents/research-roam/.org-roam.db"
  :dailies-directory "daily/"
  :capture-templates jay/org-roam-capture-templates-research)
```

Then define the templates:

```elisp
(defvar jay/org-roam-capture-templates-research
  (list
   '("p" "paper" plain "* Paper\n- Authors :: \n- Year :: \n\n%?"
     :target (file+head "papers/%<%Y%m%d%H%M%S>-${slug}.org"
                        "#+TITLE: ${title}\n#+FILETAGS: :paper:")
     :unnarrowed t)
   ;; ... more templates
   ))
```

Add a quick-switch keybinding:

```elisp
(global-set-key (kbd "s-u 3") 
  (lambda () (interactive) (jay/org-roam-apply-profile 'research)))
```

### Customizing My-Life Templates

Edit the `jay/org-roam-capture-templates-mylife` variable in `jay-org-roam-profiles.el`.

### Adding Mode Line Indicator

Uncomment the mode-line setup at the end of `jay-org-roam-profiles.el` to see `[Roam:Default]` or `[Roam:My]` in your mode line.

## How It Works

### Profile Storage
Profiles are defined in `jay/org-roam-profiles` as plists containing:
- `:name` - Human-readable name
- `:directory` - Root directory for notes
- `:db-location` - Database file path (nil = auto)
- `:dailies-directory` - Subdirectory for dailies
- `:capture-templates` - List or symbol of templates

### Persistence
Your current profile is saved to `~/.cache/org-roam-current-profile` and restored on startup.

### Database Safety
The system safely closes the old database before opening the new one, preventing corruption.

### Independence
Each profile is completely independent:
- Separate databases (no cross-contamination)
- Separate directories
- Separate capture templates
- Separate file structures

## Troubleshooting

### "Profile not found" error
Make sure the profile name is exactly as defined in `jay/org-roam-profiles` (e.g., `my-life` not `mylife`).

### Database locked
If you get database lock errors:
1. Close all org-roam buffers
2. Run `M-x jay/org-roam-switch-profile` again
3. If that fails, restart Emacs

### Templates not showing up
Run `M-x jay/org-roam-show-current-profile` to verify which profile is active and check that `org-roam-capture-templates` is set correctly.

### Can't find files from other profile
This is by design! Profiles are independent. If you need to reference files across profiles, use full file paths, not org-roam links.

## Tips

1. **Start your day**: Switch to `my-life` in the morning for journaling
2. **Work mode**: Switch to `default` when starting work
3. **End of day**: Switch back to `my-life` for reflection
4. **Muscle memory**: Use `s-u 1` and `s-u 2` for quick switches
5. **Check before capture**: Use `s-u C-p` to verify which profile you're in

## Integration with Existing Workflows

The profile system doesn't break any existing org-roam functionality:
- All commands work the same (`org-roam-node-find`, etc.)
- Keybindings unchanged
- Dailies work normally
- Search/ripgrep respects current profile directory

You're just working in a different database!
