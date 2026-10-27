// Page HTML comes from Emacs. Only candidate original images are probed;
// extraction never refetches the page or calls third-party APIs.
import fs from 'node:fs';
import {parseHTML} from 'linkedom';
import {Defuddle} from 'defuddle/node';

const url = process.argv[2];
const document = parseHTML(fs.readFileSync(0, 'utf8')).document;
// Preserve standalone legacy HTML section titles before Defuddle strips size.
// These are font-size headings in the source, not guessed from short prose.
const boundaryTags = /^(BR|P|DIV|H[1-6]|HR|UL|OL|BLOCKQUOTE|TABLE)$/;
function adjacentContent(node, direction) {
  let sibling = node[direction];
  while (sibling && ((sibling.nodeType === 3 && !sibling.textContent.trim()) ||
         (sibling.nodeType === 1 && sibling.tagName === 'A' && !sibling.textContent.trim() && !sibling.children.length))) {
    sibling = sibling[direction];
  }
  return sibling;
}
for (const font of document.querySelectorAll('font[size]')) {
  if (!/^[4-7]$/.test(font.getAttribute('size')) || !font.textContent.trim() ||
      font.closest('h1,h2,h3,h4,h5,h6') || font.querySelector('br,p,div,img')) continue;
  let wrapper = font;
  while (wrapper.parentElement && /^(B|STRONG|I|EM|SPAN|FONT)$/.test(wrapper.parentElement.tagName) &&
         wrapper.parentElement.textContent.trim() === wrapper.textContent.trim()) wrapper = wrapper.parentElement;
  const before = adjacentContent(wrapper, 'previousSibling');
  const after = adjacentContent(wrapper, 'nextSibling');
  if ([before, after].some(node => node && !(node.nodeType === 1 && boundaryTags.test(node.tagName)))) continue;
  const heading = document.createElement('h2');
  heading.innerHTML = wrapper.innerHTML;
  wrapper.replaceWith(heading);
}
// Defuddle's figure standardization can discard a photo's enclosing link.
// Restore it only for an image actually retained, with an exact source match.
const imageLinks = new Map();
for (const img of document.querySelectorAll('a[href] img[src]')) {
  const src = new URL(img.getAttribute('src'), url).href;
  const href = new URL(img.closest('a').getAttribute('href'), url).href;
  if (/^https?:/.test(href)) imageLinks.set(src, href);
}
const result = await Defuddle(document, url, {
  useAsync: false,
  removeSmallImages: false,
  // In the real-page comparison, these filters removed chapter lists,
  // embedded article cards, and recipe equipment. Prefer a little clutter.
  removeLowScoring: false,
  removeContentPatterns: false,
  removeExactSelectors: false,
  removePartialSelectors: false,
});
if (!result?.content?.trim()) throw new Error('Defuddle returned no article');
const output = parseHTML('<html><body>' + result.content + '</body></html>').document;
const checkedImages = new Map();
function availableImage(href) {
  if (!checkedImages.has(href)) checkedImages.set(href, (async () => {
    try {
      // Some image-looking URLs serve an HTML viewer or a broken original.
      // Check GET headers, then cancel the body; Emacs downloads the chosen file.
      const response = await fetch(href, {signal: AbortSignal.timeout(5000)});
      const valid = response.ok && /^image\//i.test(response.headers.get('content-type') || '');
      await response.body?.cancel();
      return valid;
    } catch {
      return false;
    }
  })());
  return checkedImages.get(href);
}
await Promise.all([...output.querySelectorAll('img[src]')].map(async img => {
  const href = img.closest('a')?.getAttribute('href') ||
    imageLinks.get(new URL(img.getAttribute('src'), url).href);
  if (href && !img.closest('a')) {
    const link = output.createElement('a');
    link.setAttribute('href', href);
    img.replaceWith(link);
    link.appendChild(img);
  }
  // An enclosing image-file link is the source's full-image target. Article
  // links remain links; never manufacture a larger URL from a thumbnail name.
  if (href) {
    const original = new URL(href, url);
    if (/^https?:$/.test(original.protocol) &&
        /\.(?:jpe?g|png|gif|webp|avif|svg)$/i.test(original.pathname) &&
        original.href !== new URL(img.getAttribute('src'), url).href &&
        await availableImage(original.href)) {
      img.setAttribute('src', original.href);
      img.removeAttribute('srcset');
    }
  }
}));
// Pandoc drops video embeds. Keep a usable link at the video's position.
for (const video of output.querySelectorAll('iframe[src],video')) {
  const src = video.getAttribute('src') || video.querySelector('source[src]')?.getAttribute('src');
  if (!src) continue;
  const target = new URL(src, url);
  if (!/^https?:$/.test(target.protocol)) continue;
  const youtube = /^(?:www\.)?youtube(?:-nocookie)?\.com$/.test(target.hostname);
  const vimeo = target.hostname === 'player.vimeo.com';
  if (video.tagName === 'IFRAME' && !youtube && !vimeo) continue;
  if (youtube && /^\/embed\/[^/]+$/.test(target.pathname)) {
    const id = target.pathname.split('/').pop();
    const start = target.searchParams.get('start');
    target.href = 'https://www.youtube.com/watch';
    target.searchParams.set('v', id);
    if (start) target.searchParams.set('t', start);
  }
  const paragraph = output.createElement('p');
  const link = output.createElement('a');
  link.setAttribute('href', target.href);
  link.textContent = video.getAttribute('title')?.trim() || 'Watch video';
  paragraph.appendChild(link);
  video.replaceWith(paragraph);
}
process.stdout.write(output.body.innerHTML);
