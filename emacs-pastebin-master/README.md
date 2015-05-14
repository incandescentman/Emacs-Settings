# Emacs Pastebin Interface

This is a huge inteface to pastebin.com. With it you can
* Paste buffers 
* Fetch pastes
* Delete pastes
* Get a nice list of pastes
* Sort the pastes list by data, title, private, format, key

## Install
``` bash
cd ~/.emacs.d/
wget https://github.com/gkos/emacs-pastebin/archive/master.zip
unzip master.zip 
rm master.zip
cd emacs-pastebin-master/
make
```

Then put this on your `.emacs` file:
```elisp
(add-to-list 'load-path "~/.emacs.d/emacs-pastebin/")
(require 'neopastebin)
(pastebin-create-login :dev-key "YOURDEVKEY"
                       :username "YOURUSER")
```

Restart emacs or eval `.emacs` again. On emacs `M-x pastebin-l<TAB> <RET>`. Type password. Save password in disk, it will be saved in clear text at `~/.emacs.d/pastebin-data/pass` or whatever you set to `pastebin-data-dir`. If you really care, you can setup this variable to an encrypted partition. :P

You should see a nice list of pastes on your screen right now.

## Usage

### Listing
M-x `pastebin-list-buffer-refresh` -> Fetch and list pastes on "list buffer". 
After logged you can list your pastes with command `pastebin-list-buffer-refresh`, just type pastebin-l and press TAB.

Here is a list of keybinds from list buffer.

```
RET -> fetch paste and switch to it
r ->   refresh list and list buffer
d ->   delete paste, you'll be asked for confirmation
t ->   order by title
D ->   order by date
f ->   order by format
k ->   order by key
p ->   order by private
```

### Creating a new paste

M-x `pastebin-new` -> will create a new paste from current buffer

The name of the paste is given from current buffer name
The format from buffers major mode
Prefix argument makes private 

