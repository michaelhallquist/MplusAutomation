// Rendering-only mediation fixture: no estimated model is fabricated here.
const {chromium}=require('playwright');
const assert=require('node:assert/strict'),fs=require('node:fs'),path=require('node:path');
const repo=path.resolve(__dirname,'../..');
function fixture(vertical=false) {
  const nodes=['X','M','Y'].map(name=>({id:'node:'+name,name,role:'observed'}));
  const edges=[['regression','X','M'],['regression','M','Y'],['regression','X','Y'],['covariance','X','M'],['variance','Y','Y']]
    .map(([type,a,b])=>({id:`${type}:${a}:${b}`,type,from:'node:'+a,to:'node:'+b,fixed:false,origin:'explicit'}));
  const view={nodes:Object.fromEntries(nodes.map((n,i)=>[n.id,vertical?{x:300,y:120+220*i}:{x:120+220*i,y:300}])),edges:{},viewport:{x:0,y:0,k:1}};
  return {schema_version:1,model:{structure:{family:'path',nodes,edges,constraints:[],mapping:{}},
    results:Object.fromEntries(edges.map(e=>[e.id,{stdyx:{estimate:.32,p:.001},unstandardized:{estimate:.45,p:.001}}])),diagnostics:[],provenance:{},fingerprint:'line-types-fixture'},
    state:{options:{scale:'stdyx',digits:2,residuals:true,default_covariances:true,parameter_labels:false,values:true,fixed_values:true,constraints:false,view:'full'},aliases:{},hidden_edges:[],views:{full:structuredClone(view),simplified:structuredClone(view)}},
    automatic:{full:structuredClone(view),simplified:structuredClone(view)},editor:null};
}
async function config(widget) {
  return widget.evaluate(async root=>{
    const create=URL.createObjectURL,click=HTMLAnchorElement.prototype.click;let blob;
    try {URL.createObjectURL=b=>{blob=b;return create(b);};HTMLAnchorElement.prototype.click=function(){};root.querySelector('[data-action="export-config"]').click();}
    finally {URL.createObjectURL=create;HTMLAnchorElement.prototype.click=click;}
    return JSON.parse(await blob.text());
  });
}
async function route(edge) {
  return edge.evaluate(e=>{
    const p=e.querySelector('.ld-line'),t=e.querySelector('.ld-edge-text');
    return {d:p.getAttribute('d'),type:e.dataset.lineType,label:{x:+t.getAttribute('x'),y:+t.getAttribute('y')},text:t.textContent};
  });
}
async function drag(page,locator,dx,dy) {
  const b=await locator.boundingBox();assert.ok(b);
  await page.mouse.move(b.x+b.width/2,b.y+b.height/2);await page.mouse.down();
  await page.mouse.move(b.x+b.width/2+dx,b.y+b.height/2+dy,{steps:6});await page.mouse.up();
}
async function importConfig(widget,c) {
  await widget.getByLabel('Import configuration file').setInputFiles({name:'routes.json',mimeType:'application/json',buffer:Buffer.from(JSON.stringify(c))});
}
(async()=>{
  const browser=await chromium.launch({headless:true,...(process.env.CHROME_PATH?{executablePath:process.env.CHROME_PATH}:{})});
  try {
    const context=await browser.newContext({viewport:{width:1400,height:1100},acceptDownloads:true});await context.setOffline(true);
    const page=await context.newPage(),errors=[];page.on('pageerror',e=>errors.push(e.message));
    await page.setContent('<div id="one" style="width:1100px;height:850px"></div>');
    await page.addStyleTag({content:fs.readFileSync(path.join(repo,'inst/htmlwidgets/lib/mplus-diagram/mplus-diagram.css'),'utf8')});
    await page.evaluate(()=>{window.HTMLWidgets={widget:b=>window.binding=b};});
    await page.addScriptTag({content:fs.readFileSync(path.join(repo,'inst/htmlwidgets/mplus_diagram.js'),'utf8')});
    await page.evaluate(data=>{window.instance=binding.factory(document.getElementById('one'),1100,850);instance.renderValue(data);},fixture());
    const widget=page.locator('#one'),id='regression:X:Y',edge=widget.locator(`[data-edge-id="${id}"]`);
    const choose=widget.getByLabel('Path',{exact:true}),type=widget.getByLabel('Line type',{exact:true});
    await widget.locator('[data-action="edit"]').click();
    assert.equal(await type.isDisabled(),true);
    await choose.selectOption(id);assert.equal(await type.inputValue(),'straight');
    assert.match((await route(edge)).d,/ L/);assert.doesNotMatch((await route(edge)).d,/[QC]/);
    const baseline=await config(widget);
    await type.selectOption('curve');const curved=await route(edge);assert.match(curved.d,/ Q/);assert.equal(curved.type,'curve');
    assert.ok(curved.label.y<270,'Curve routes above the middle variable');
    await type.selectOption('polyline');const bent=await route(edge);assert.equal((bent.d.match(/ L/g)||[]).length,3);
    assert.equal(bent.type,'polyline');assert.equal(bent.label.x,340);assert.equal(bent.label.y,200);
    const saved=await config(widget);
    // Drag the middle segment itself, away from its centered label.
    const point=await edge.locator('.ld-polyline-middle').evaluate(p=>{
      const q=p.getPointAtLength(p.getTotalLength()*.2).matrixTransform(p.getScreenCTM());return{x:q.x,y:q.y};
    });
    await page.mouse.move(point.x,point.y);await page.mouse.down();await page.mouse.move(point.x,point.y-40,{steps:6});await page.mouse.up();
    assert.ok((await config(widget)).state.views.full.edges[id].bend_offset<-100);
    await widget.locator('[data-action="undo"]').click();assert.deepEqual(await config(widget),saved);
    await drag(page,edge.locator('.ld-handle'),0,-45);assert.ok((await route(edge)).label.y<200);
    await widget.locator('[data-action="undo"]').click();assert.deepEqual(await config(widget),saved);
    await widget.locator('[data-action="redo"]').click();assert.ok((await route(edge)).label.y<200);
    await importConfig(widget,saved);await widget.getByRole('status').filter({hasText:'Configuration imported'}).waitFor();
    assert.deepEqual(await config(widget),saved);assert.deepEqual(await route(edge),bent);
    await choose.selectOption(id);await type.selectOption('straight');assert.equal((await route(edge)).type,'straight');
    await widget.locator('[data-action="undo"]').click();assert.deepEqual(await route(edge),bent);
    // Moving an endpoint preserves the stored lane offset and two right angles.
    await drag(page,widget.locator('[data-node-id="node:Y"]'),50,30);
    const moved=await route(edge),parts=moved.d.match(/-?\d+(?:\.\d+)?(?:e[+-]?\d+)?/gi).map(Number);
    assert.equal(parts[0],parts[2]);assert.equal(parts[3],parts[5]);assert.equal(parts[4],parts[6]);
    assert.equal((await config(widget)).state.views.full.edges[id].bend_offset,-100);
    await widget.locator('[data-action="undo"]').click();assert.deepEqual(await route(edge),bent);
    await widget.getByLabel('View',{exact:true}).selectOption('simplified');assert.equal((await route(edge)).type,'straight');
    await widget.getByLabel('View',{exact:true}).selectOption('full');assert.deepEqual(await route(edge),bent);
    for(const invalid of [{line_type:'spline'},{line_type:null},{line_type:1},{bend_offset:25},{line_type:'curve',bend_offset:2},{line_type:'polyline',bend_offset:1e7}]) {
      const bad=structuredClone(saved);bad.state.views.full.edges[id]=invalid;
      await importConfig(widget,bad);await widget.getByRole('status').filter({hasText:'Import rejected'}).waitFor();
      assert.deepEqual(await config(widget),saved);
    }
    // Old curve-only overrides must still render as curves.
    const legacy=structuredClone(baseline);legacy.state.views.full.edges[id]={curve:-180};
    await importConfig(widget,legacy);await widget.getByRole('status').filter({hasText:'Configuration imported'}).waitFor();
    await choose.selectOption(id);assert.equal(await type.inputValue(),'curve');assert.match((await route(edge)).d,/ Q/);
    await choose.selectOption('variance:Y:Y');assert.equal(await type.isDisabled(),true);
    const badResidual=structuredClone(saved);badResidual.state.views.full.edges['variance:Y:Y']={line_type:'polyline'};
    await importConfig(widget,badResidual);await widget.getByRole('status').filter({hasText:'Import rejected'}).waitFor();
    // Existing covariance curves remain default; explicit types retain two arrows.
    await choose.selectOption('covariance:X:M');assert.equal(await type.inputValue(),'curve');
    const covariance=widget.locator('[data-edge-id="covariance:X:M"]');
    for(const value of ['straight','polyline','curve']) {
      await type.selectOption(value);assert.equal((await route(covariance)).type,value);
      assert.ok(await covariance.locator('.ld-line').getAttribute('marker-start'));
      assert.ok(await covariance.locator('.ld-line').getAttribute('marker-end'));
    }
    await importConfig(widget,saved);await widget.getByRole('status').filter({hasText:'Configuration imported'}).waitFor();
    for(const kind of ['svg','png']) {
      const pending=page.waitForEvent('download');await widget.locator(`[data-action="export-${kind}"]`).click();
      const bytes=fs.readFileSync(await (await pending).path());
      if(kind==='svg') {assert.ok(bytes.toString().includes(bent.d));assert.doesNotMatch(bytes.toString(),/data-editor-only/);}
      else assert.equal(bytes.subarray(1,4).toString(),'PNG');
    }
    await page.evaluate(data=>instance.renderValue(data),fixture(true));
    await widget.locator('[data-action="edit"]').click();await choose.selectOption(id);await type.selectOption('polyline');
    const vertical=await route(edge);assert.equal(vertical.label.x,200);assert.equal(vertical.label.y,340);
    const vparts=vertical.d.match(/-?\d+(?:\.\d+)?/g).map(Number);
    assert.equal(vparts[1],vparts[3]);assert.equal(vparts[2],vparts[4]);assert.equal(vparts[5],vparts[7]);
    assert.deepEqual(errors,[]);
    console.log('PASS: line type selection, overlapped path selection, polyline middle/handle drags, endpoints, history, view isolation, legacy config, validation, covariance, SVG/PNG and vertical routing.');
  } finally {await browser.close();}
})().catch(error=>{console.error(error);process.exitCode=1;});
