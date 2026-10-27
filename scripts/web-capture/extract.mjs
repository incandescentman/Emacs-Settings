// HTML comes from Emacs; extraction never refetches or calls third-party APIs.
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
for (const img of output.querySelectorAll('img[src]')) {
  const href = imageLinks.get(new URL(img.getAttribute('src'), url).href);
  if (href && !img.closest('a')) {
    const link = output.createElement('a');
    link.setAttribute('href', href);
    img.replaceWith(link);
    link.appendChild(img);
  }
}
process.stdout.write(output.body.innerHTML);
