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

function extract(body, response = {ok: true, type: 'image/jpeg'}, bytes = normalImage, url = 'https://strobist.blogspot.com/2019/11/slc-0l-03-joker-cinematographer-on.html') {
  const script = fileURLToPath(new URL('./extract.mjs', import.meta.url));
  const setup = `globalThis.fetch = async (_url, options) => {
      if (options.headers.Range !== 'bytes=0-65535') throw new Error('Unbounded image request');
      const bytes = Buffer.from(${JSON.stringify(bytes.toString('base64'))}, 'base64');
      return {status: ${response.status ?? (response.ok ? 206 : 404)},
        headers: new Headers({'content-type': ${JSON.stringify(response.type)}, 'content-range': 'bytes 0-' + (bytes.length - 1) + '/${response.total ?? 124966}'}),
        body: ${response.status === 200 ? "{cancel: async () => {}, getReader: () => {process.exitCode = 71; throw new Error('Read ignored range response')}}" : 'new Response(bytes).body'}};
    };
    process.argv = [process.execPath, ${JSON.stringify(script)}, ${JSON.stringify(url)}];
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

const programsUrl = 'https://sps.columbia.edu/academics/masters';
const curriculumUrl = programsUrl + '/strategic-communication/full-time-master-science/curriculum-courses';
const normalize = text => text.replace(/\s+/g, ' ').trim();
function fixture(name) {
  return parseHTML(fs.readFileSync(new URL(`./fixtures/${name}.html`, import.meta.url), 'utf8')).document;
}
function convert(doc, url) {
  const html = extract(doc.querySelector('main').innerHTML, undefined, undefined, url);
  return parseHTML('<html><body>' + html + '</body></html>').document;
}

test('all 20 public Columbia program destinations and summaries survive', () => {
  const source = fixture('columbia-program-cards');
  const output = convert(source, programsUrl);
  const expected = [...source.querySelectorAll('.program-card__title a')];
  assert.equal(expected.length, 20);
  const actual = [...output.querySelectorAll('h2 a,h3 a')];
  assert.equal(actual.length, 20);
  assert.deepEqual(actual.map(a => a.getAttribute('href')),
    expected.map(a => new URL(a.getAttribute('href'), programsUrl).href));
  for (const summary of source.querySelectorAll('.program-card__teaser')) {
    assert.ok(normalize(output.body.textContent).includes(normalize(summary.textContent)));
  }
  assert.equal(output.body.textContent.includes('(opens in a new window)'), false);
  const org = spawnSync('pandoc', ['-f', 'html', '-t', 'org', '--wrap=none'], {input: output.body.innerHTML, encoding: 'utf8'});
  assert.equal(org.status, 0, org.stderr);
  for (const anchor of actual) assert.ok(org.stdout.includes(`[[${anchor.getAttribute('href')}][${anchor.textContent}]]`));
});

test('hidden course descriptions survive and labeled decorative icons do not reach Pandoc', () => {
  const source = fixture('columbia-course-content');
  assert.equal(source.querySelectorAll('.accordion-pane').length, 27);
  const output = convert(source, curriculumUrl);
  const paragraphs = [...source.querySelectorAll('.accordion-pane__body p')].filter(p => normalize(p.textContent));
  assert.equal(paragraphs.length, 29);
  for (const paragraph of paragraphs) {
    assert.ok(normalize(output.body.textContent).includes(normalize(paragraph.textContent)));
  }
  for (const detail of source.querySelectorAll('.accordion-pane__course-details-course-number')) {
    const code = detail.textContent.replace('Course Number', '').trim();
    assert.ok(output.body.textContent.includes(code), code);
  }
  const social = output.querySelector('a[href="https://www.linkedin.com/company/cu-sps-stratcomm/"]');
  assert.equal(normalize(social.textContent), 'Visit us on LinkedIn');
  assert.equal(social.querySelector('svg,img'), null);
  const org = spawnSync('pandoc', ['-f', 'html', '-t', 'org', '--wrap=none'], {input: output.body.innerHTML, encoding: 'utf8'});
  assert.equal(org.status, 0, org.stderr);
  assert.ok(org.stdout.includes('[[https://www.linkedin.com/company/cu-sps-stratcomm/][Visit us on LinkedIn]]'));
  assert.equal(org.stdout.includes('data:image'), false);
});

test('a new-window phrase in ordinary prose is retained', () => {
  const source = fixture('columbia-program-cards');
  const paragraph = source.createElement('p');
  paragraph.textContent = 'The accessibility label says (opens in a new window).';
  source.querySelector('main').prepend(paragraph);
  assert.ok(convert(source, programsUrl).body.textContent.includes(paragraph.textContent));
});

test('ambiguous repeated heading labels never acquire a guessed destination', () => {
  const source = fixture('columbia-program-cards');
  const card = source.querySelector('.program-card').cloneNode(true);
  card.querySelector('.program-card__title a').setAttribute('href', 'https://example.org/different-program');
  source.querySelector('main').appendChild(card);
  const output = convert(source, programsUrl);
  const ambiguous = [...output.querySelectorAll('h2,h3')].filter(h => h.textContent === 'Actuarial Science');
  assert.ok(ambiguous.length > 0);
  for (const heading of ambiguous) assert.equal(heading.querySelector('a'), null);
});

test('unmarked SVG artwork and icon-only links are not treated as labeled decoration', () => {
  const source = fixture('columbia-course-content');
  const link = source.querySelector('a[href="https://www.linkedin.com/company/cu-sps-stratcomm/"]');
  const iconOnly = link.cloneNode(true);
  iconOnly.querySelector('.social-links__content').remove();
  iconOnly.setAttribute('href', 'https://example.org/icon-only');
  source.querySelector('main').appendChild(iconOnly);
  const artworkLink = link.cloneNode(true);
  artworkLink.querySelector('[aria-hidden]').removeAttribute('aria-hidden');
  artworkLink.setAttribute('href', 'https://example.org/artwork');
  source.querySelector('main').appendChild(artworkLink);
  const output = convert(source, curriculumUrl);
  assert.ok(output.querySelector('a[href="https://example.org/artwork"] svg'));
  assert.ok(output.querySelector('a[href="https://example.org/icon-only"] svg'));
});
