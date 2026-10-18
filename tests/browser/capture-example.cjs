/* Reproduce README/PR previews from render-gallery.R's real fitted results.
 * Usage: node tests/browser/capture-example.cjs /tmp/mplus-gallery OUTPUT
 * Screenshots contain the actual widget; no coefficients are added or edited.
 */
const { chromium } = require('playwright');
const fs = require('node:fs');
const path = require('node:path');
const { pathToFileURL } = require('node:url');

(async () => {
  const [gallery, output] = process.argv.slice(2);
  if (!gallery || !output) throw new Error('Supply gallery and output directories');
  fs.mkdirSync(output, {recursive: true});
  const browser = await chromium.launch(process.env.CHROME_PATH
    ? {executablePath: process.env.CHROME_PATH, headless: true} : {headless: true});
  try {
    const context = await browser.newContext({viewport: {width: 1220, height: 840}, deviceScaleFactor: 1});
    await context.setOffline(true);
    const page = await context.newPage();
    for (const model of ['cfa', 'clpm']) {
      await page.goto(pathToFileURL(path.resolve(gallery, model + '.html')).href);
      const widget = page.locator('.mplus-diagram');
      await widget.locator('.ld-node').first().waitFor();
      await page.evaluate(() => document.fonts.ready);
      await widget.screenshot({path: path.join(output, 'mplus-diagram-' + model + '.png')});
    }
  } finally {
    await browser.close();
  }
})().catch(error => { console.error(error); process.exitCode = 1; });
