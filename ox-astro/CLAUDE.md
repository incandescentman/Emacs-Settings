

Hello!

We just added an important feature, which now works perfectly: the ability to in the body of the blog post include a raw image path (a local path ending in .jpg, jpeg, or .png) and have the exporter automatically copy the image to the correct folder, import the image for astro, and add it in the correct Astro format using the Image component. It works perfectly.

that's this file:
/Users/jay/emacs/emacs-settings/ox-astro/ox-astro.el

However, in this current version of the exporter, now one thing is broken. It now creates duplicate front matter. When I export from org-mode the first time, the front matter is correct. But when I do it a second time, it adds duplicate front matter.

