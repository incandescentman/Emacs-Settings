import assert from 'node:assert/strict';
import {test} from 'node:test';
import {spawnSync} from 'node:child_process';
import {fileURLToPath} from 'node:url';
import {parseHTML} from 'linkedom';

function extract(body, response = {ok: true, type: 'image/jpeg'}) {
  const script = fileURLToPath(new URL('./extract.mjs', import.meta.url));
  const setup = `globalThis.fetch = async () => ({ok: ${response.ok}, headers: new Headers({'content-type': ${JSON.stringify(response.type)}}), body: null});
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
