

Hello!

We just added an important feature, which now works perfectly: the ability to in the body of the blog post include a raw image path (a local path ending in .jpg, jpeg, or .png) and have the exporter automatically copy the image to the correct folder, import the image for astro, and add it in the correct Astro format using the Image component. It works perfectly.

that's this file:
/Users/jay/emacs/emacs-settings/ox-astro/ox-astro.el

However, in this current version of the exporter, now one thing is broken. It now creates duplicate front matter. When I export from org-mode the first time, the front matter is correct. But when I do it a second time, it adds duplicate front matter.

After numerous attempts and multiple strategies trying for fix this, it still isn't fixed.

However, it used to work!

Before we added the ability to include a raw path to an image, I could export from org-mode to mdx many times and it would never duplicate the front matter!

that's this old version:
/Users/jay/emacs/emacs-settings/ox-astro/old-ox-astro-does-not-have-the-duplicating-problem-but-cannot-handle-raw-image-paths.el

So, let's go back and look at the old version here, and see why it successfully avoided the trap of duplicating front matter. and let's use what we learn to fix the current version!
