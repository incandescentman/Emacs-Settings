import assert from 'node:assert/strict';
import {test} from 'node:test';
import {spawnSync} from 'node:child_process';
import {fileURLToPath} from 'node:url';
import fs from 'node:fs';
import {parseHTML} from 'linkedom';

// Real 64 KiB prefixes, not complete photos; JFIF declares 2 and 240 DPI.
const lowDpiImage = fs.readFileSync(new URL('./fixtures/strobist-tardis-2dpi-header.jpg', import.meta.url));
const highDpiImage = fs.readFileSync(new URL('./fixtures/strobist-kassi-240dpi-header.jpg', import.meta.url));
function withResolution(x, y, unit = 'inches') {
  const result = spawnSync('exiftool', [`-JFIF:XResolution=${x}`, `-JFIF:YResolution=${y}`,
    `-JFIF:ResolutionUnit=${unit}`, '-o', '-', '-'], {input: lowDpiImage});
  assert.equal(result.status, 0, result.stderr.toString());
  return result.stdout;
}
const normalImage = withResolution(72, 72);

function extract(body, response = {ok: true, type: 'image/jpeg'}, bytes = normalImage) {
  const script = fileURLToPath(new URL('./extract.mjs', import.meta.url));
  const setup = `globalThis.fetch = async (_url, options) => {
      if (options.headers.Range !== 'bytes=0-65535') throw new Error('Unbounded image request');
      const bytes = Buffer.from(${JSON.stringify(bytes.toString('base64'))}, 'base64');
      return {status: ${response.status ?? (response.ok ? 206 : 404)},
        headers: new Headers({'content-type': ${JSON.stringify(response.type)}, 'content-range': 'bytes 0-' + (bytes.length - 1) + '/${response.total ?? 124966}'}),
        body: ${response.status === 200 ? "{cancel: async () => {}, getReader: () => {process.exitCode = 71; throw new Error('Read ignored range response')}}" : 'new Response(bytes).body'}};
    };
    process.argv = [process.execPath, ${JSON.stringify(script)}, 'https://strobist.blogspot.com/2019/11/slc-0l-03-joker-cinematographer-on.html'];
    await import(${JSON.stringify(script)});`;
  const result = spawnSync(process.execPath, ['--input-type=module', '--eval', setup], {
    input: `<html><head><title>Strobist</title></head><body><article>${body}</article></body></html>`,
    encoding: 'utf8',
  });
  assert.equal(result.status, 0, result.stderr);
  return result.stdout;
}

test('the five captured Strobist YouTube embeds survive Pandoc as links', () => {
  const ids = ['JkwT3YFNUCw', 'L4HyDb5DSpk', 'th9pG9Q6Kuo', 'puTbQrEEGvI', 'cK-pUoCBc-4'];
  const html = extract(ids.map(id => `<p>Please watch this video.</p><iframe width="425" height="240" src="https://www.youtube.com/embed/${id}" allowfullscreen></iframe>`).join(''));
  const org = spawnSync('pandoc', ['--from=html', '--to=org', '--wrap=none'], {input: html, encoding: 'utf8'});
  assert.equal(org.status, 0, org.stderr);
  for (const id of ids) assert.ok(org.stdout.includes(`[[https://www.youtube.com/watch?v=${id}][Watch video]]`), id);
  assert.equal((org.stdout.match(/Watch video/g) || []).length, 5);
});

test('a linked photo uses its original while an article link keeps its image', () => {
  // The welcome photo and Lighting 101 article-linked photo that exposed the
  // original image-link bug, reduced to their actual URL structures.
  const original = 'https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEhwKHyNRi5FCeGU7RJRrHJYgR7zTWGI2ec6TiotwG4p7aQhZykbQ0R4MoAoWC2ctoiK_W8AKHRI991-xgX7tRl69HRmg03F5U67zbFi5xMpN2VnJhCZ-81f6nVtWop6dRZ-faTb/s1600/David_Hobby_Headshot_240.jpg';
  const thumbnail = original.replace('/s1600/', '/s200/');
  const article = 'http://strobist.blogspot.com/2009/06/on-assignment-hcac-dancer.html';
  const html = extract(`<p>What is Strobist?</p><a href="${original}"><img src="${thumbnail}"></a><p>Lighting 101</p><a href="${article}"><img src="${thumbnail}"></a>`);
  const doc = parseHTML(`<html><body>${html}</body></html>`).document;
  const photos = [...doc.querySelectorAll('img')];
  assert.equal(photos.length, 2);
  assert.equal(photos[0].getAttribute('src'), original);
  assert.equal(photos[1].getAttribute('src'), thumbnail);
  assert.equal(photos[1].closest('a').getAttribute('href'), article);
});

test('unavailable originals and HTML image viewers keep the displayed image', () => {
  // Blogger's s1600-h links return image-viewer HTML rather than image bytes.
  const original = 'https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEgitPUs04lSyYvrEeINyELcp6I4ubA9mZucNQ-PCLqSjdRSOf38-JDKnOPEQQdljNElVG_l7sanA3CWJH1DFHhsuTDAloUpNL0IwbI0daVWCZUVJ5Im0uk_aqqDRoPIxGWxI1se6Q/s1600-h/Shootout800p.jpg';
  const displayed = original.replace('/s1600-h/', '/s400/');
  for (const response of [{ok: false, type: 'text/html'}, {ok: true, type: 'text/html'}]) {
    const html = extract(`<p>On assignment</p><a href="${original}"><img src="${displayed}"></a>`, response);
    const doc = parseHTML(`<html><body>${html}</body></html>`).document;
    assert.equal(doc.querySelector('img').getAttribute('src'), displayed);
  }
});

const umbrella = 'https://blogger.googleusercontent.com/img/b/R29vZ2xl/AVvXsEjX8vGPtKqNkIrbYH0x0lShYF7dl5ce-gV5iG7gm9ye8_yXWXWpiEJD-p8fCi2c_HjNOdH8zoOksAcoHKNRiScnwkMW0zDjs8cHWqNgLLeLn413oQ_pi3gKrgfCrRXg219wisdp/s1600/UMBRELLA_0320.jpg';
const umbrellaThumbnail = umbrella.replace('/s1600/', '/s400/');
const umbrellaHtml = `<p>To gel your umbrella and spill kill, tape it across the front as shown.</p><a href="${umbrella}"><img src="${umbrellaThumbnail}"></a>`;
function selectedImage(bytes, response) {
  const html = extract(umbrellaHtml, response, bytes);
  return parseHTML(`<html><body>${html}</body></html>`).document.querySelector('img').getAttribute('src');
}

test('the real 2-DPI original keeps its thumbnail; a 72-DPI copy is selected', () => {
  assert.equal(selectedImage(lowDpiImage), umbrellaThumbnail);
  assert.equal(selectedImage(normalImage), umbrella);
});

test('a real 64 KiB slice of a 240-DPI original still upgrades', () => {
  assert.equal(highDpiImage.length, 65536);
  assert.equal(selectedImage(highDpiImage, {ok: true, type: 'image/jpeg', total: 135302}), umbrella);
});

test('the guard checks both axes and physical units without rejecting aspect ratios', () => {
  assert.equal(selectedImage(withResolution(72, 2)), umbrellaThumbnail);
  assert.equal(selectedImage(withResolution(10, 10)), umbrella);
  assert.equal(selectedImage(withResolution(2, 2, 'cm')), umbrellaThumbnail);
  assert.equal(selectedImage(withResolution(4, 4, 'cm')), umbrella);
  assert.equal(selectedImage(withResolution(1, 1, 'None')), umbrella);
});

test('a server that ignores Range keeps the thumbnail without reading its body', () => {
  assert.equal(selectedImage(normalImage, {ok: true, status: 200, type: 'image/jpeg'}), umbrellaThumbnail);
});
