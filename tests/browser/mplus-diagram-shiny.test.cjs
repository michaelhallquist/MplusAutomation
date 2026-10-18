// Start the public R action='edit' in an interactive R process first.
const {chromium}=require('playwright');
const assert=require('node:assert/strict');
(async()=>{
  const browser=await chromium.launch({headless:true,executablePath:process.env.CHROME_PATH});
  const page=await browser.newPage({viewport:{width:1400,height:1000}});
  const errors=[];page.on('pageerror',e=>errors.push(e.message));
  await page.goto(process.env.MPLUS_DIAGRAM_SHINY_URL||'http://127.0.0.1:3493');
  const widget=page.locator('.mplus-diagram'); await widget.locator('.ld-node').first().waitFor();
  const node=widget.locator('.ld-node').first();await node.click();
  await widget.getByLabel('Display name',{exact:true}).fill('中文保存验证');
  await widget.locator('[data-action="alias"]').click();
  const b=await node.boundingBox();await page.mouse.move(b.x+b.width/2,b.y+b.height/2);await page.mouse.down();await page.mouse.move(b.x+b.width/2+35,b.y+b.height/2+25,{steps:5});await page.mouse.up();
  assert.match(await node.textContent(),/中文保存验证/);
  await widget.locator('[data-action="save"]').click();
  await page.waitForTimeout(1000);assert.deepEqual(errors,[]);
  await browser.close();console.log('PASS: real Shiny browser alias/drag and Save and return event submitted; verify returned R widget/config independently.');
})().catch(e=>{console.error(e);process.exit(1);});
