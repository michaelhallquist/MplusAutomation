/* Browser acceptance for the shipped binding. This synthetic graph tests the
 * rendering contract only; it is not evidence of Mplus parser/family support.
 * Set MPLUS_DIAGRAM_HTML=/path/report.html to additionally smoke-test real output.
 */
const {chromium} = require('playwright');
const fs = require('node:fs');
const path = require('node:path');
const assert = require('node:assert/strict');
const os = require('node:os');
const root = path.resolve(__dirname, '../..');
const defaults = {scale:'stdyx',digits:2,residuals:false,default_covariances:false,parameter_labels:false,values:true,fixed_values:true,constraints:false,view:'full',stars:true,nonsignificant_dotted:true};
function fixture() {
  const nodes = [{id:'node:F',name:'F',role:'latent'}, {id:'node:Y1',name:'Y1',role:'observed'}, {id:'node:Y2',name:'Y2',role:'observed'}, {id:'node:X',name:'X',role:'observed'}];
  const edges = [
    {id:'loading:F:Y1',from:'node:F',to:'node:Y1',type:'loading',fixed:true,fixed_value:1,origin:'explicit',label:'lambda'},
    {id:'loading:F:Y2',from:'node:F',to:'node:Y2',type:'loading',fixed:false,origin:'explicit',label:null},
    {id:'regression:X:F',from:'node:X',to:'node:F',type:'regression',fixed:false,origin:'explicit',label:'b'},
    {id:'variance:Y1:Y1',from:'node:Y1',to:'node:Y1',type:'variance',fixed:false,origin:'explicit'},
    {id:'covariance:X:Y2',from:'node:X',to:'node:Y2',type:'covariance',fixed:false,origin:'unknown'}];
  const results = Object.fromEntries(edges.map((e,i)=>[e.id,{unstandardized:{estimate:i===0?1:0.4+i/10,se:0.1,z:4,p:0.001},stdyx:i===1?null:{estimate:0.7+i/10,se:0.1,z:7,p:0.001}}]));
  const v = {nodes:{'node:F':{x:150,y:140},'node:Y1':{x:450,y:100},'node:Y2':{x:450,y:250},'node:X':{x:150,y:400}},edges:{'covariance:X:Y2':{curve:55}},viewport:{x:0,y:0,k:1}};
  return {schema_version:1,model:{structure:{family:'sem',nodes,edges,constraints:[],mapping:{}},results,diagnostics:[],provenance:{title:'Rendering contract fixture',output:'fixture.out',estimator:'ML',engine:'test',result_id:'a'.repeat(64)},fingerprint:'fixture-fingerprint'},state:{options:{...defaults},aliases:{},hidden_edges:[],views:{full:structuredClone(v),simplified:structuredClone(v)}},automatic:{full:structuredClone(v),simplified:structuredClone(v)},editor:null};
}
function measurementFixture() {
  const data=fixture(), ids=['F','G','Y1','Y2','Y3','Y4'];
  data.model.structure.family='cfa';
  data.model.structure.nodes=ids.map(name=>({id:'node:'+name,name,role:name.length===1?'latent':'observed',endogenous:name.length>1}));
  const edge=(type,from,to,extra={})=>({id:`${type}:${from}:${to}`,type,from:'node:'+from,to:'node:'+to,fixed:false,origin:'explicit',...extra});
  data.model.structure.edges=[edge('loading','F','Y1'),edge('loading','F','Y2'),edge('loading','G','Y3'),edge('loading','G','Y4'),
    edge('covariance','F','G',{residual_from:false,residual_to:false}),edge('covariance','Y1','Y2',{residual_from:true,residual_to:true}),
    ...ids.map(n=>edge('variance',n,n,{residual:n.length>1}))];
  data.model.results=Object.fromEntries(data.model.structure.edges.map(e=>[e.id,{unstandardized:{estimate:.25,p:.02},stdyx:{estimate:.25,p:.02}}]));
  const nodes=Object.fromEntries(ids.map((n,i)=>['node:'+n,{x:i<2?150:450,y:i<2?140+i*300:80+(i-2)*140}]));
  for(const view of ['full','simplified']) {data.state.views[view]={nodes:structuredClone(nodes),edges:{},viewport:{x:0,y:0,k:1}};data.automatic[view]=structuredClone(data.state.views[view]);}
  return data;
}
async function endpoints(edge) {
  return edge.locator('.ld-line').evaluate(p=>{
    const a=p.getPointAtLength(0),b=p.getPointAtLength(p.getTotalLength());
    return {start:{x:a.x,y:a.y},end:{x:b.x,y:b.y},d:p.getAttribute('d'),markerStart:p.getAttribute('marker-start'),markerEnd:p.getAttribute('marker-end')};
  });
}
async function exported(page, widget, type='config') {
  const pending=page.waitForEvent('download'); await widget.locator(`[data-action="export-${type}"]`).click();
  const d=await pending; const file=await d.path(); return fs.readFileSync(file);
}
async function config(page, widget) {
  // Inspect the exported Blob without repeatedly triggering Chromium's bulk
  // download protection. Actual config/SVG/PNG downloads are tested separately.
  return widget.evaluate(async root => {
    const makeURL=URL.createObjectURL, click=HTMLAnchorElement.prototype.click; let blob;
    try {
      URL.createObjectURL=b=>{blob=b;return makeURL(b);};
      HTMLAnchorElement.prototype.click=function(){};
      root.querySelector('[data-action="export-config"]').click();
    } finally { URL.createObjectURL=makeURL; HTMLAnchorElement.prototype.click=click; }
    return JSON.parse(await blob.text());
  });
}
(async()=>{
  const chrome=process.env.CHROME_PATH;
  const browser=await chromium.launch(chrome?{executablePath:chrome,headless:true,timeout:30000}:{headless:true,timeout:30000});
  console.log('Chrome launched');
  const context=await browser.newContext({viewport:{width:1500,height:1100},acceptDownloads:true});
  await context.setOffline(true);
  const page=await context.newPage(); const errors=[]; page.on('pageerror',e=>errors.push(e.message));
  await page.setContent('<html><head></head><body><div id="one" style="width:1100px;height:650px"></div><div id="two" style="width:1100px;height:650px"></div></body></html>');
  await page.addStyleTag({content:fs.readFileSync(path.join(root,'inst/htmlwidgets/lib/mplus-diagram/mplus-diagram.css'),'utf8')});
  await page.evaluate(()=>{window.HTMLWidgets={widget:binding=>{window.binding=binding;}};});
  await page.addScriptTag({content:fs.readFileSync(path.join(root,'inst/htmlwidgets/mplus_diagram.js'),'utf8')});
  const data=fixture(); await page.evaluate(data=>{window.fixture=data; window.instances=['one','two'].map(id=>{const w=binding.factory(document.getElementById(id),1100,650);w.renderValue(data);return w;});},data);
  console.log('Binding mounted');
  const one=page.locator('#one'),two=page.locator('#two');
  assert.equal(await one.locator('.ld-node').count(),4); assert.equal(await one.locator('.ld-edge').count(),4);
  assert.equal(await one.locator('.ld-heading').count(),0);
  assert.equal(await one.getByLabel('Scale',{exact:true}).inputValue(),'stdyx');
  assert.equal(await one.locator('[data-edge-id="loading:F:Y2"] .ld-edge-text').textContent(),'NA');
  console.log('Rendering assertions passed');
  const baseline=await config(page,one);
  assert.deepEqual(JSON.parse((await exported(page,one)).toString()),baseline);
  await one.getByLabel('Residuals',{exact:true}).check(); assert.equal(await one.locator('.ld-edge').count(),5);
  await one.getByLabel('Residuals',{exact:true}).uncheck();
  await one.getByLabel('Scale',{exact:true}).selectOption('unstandardized');
  assert.equal(await one.locator('[data-edge-id="loading:F:Y1"] .ld-edge-text').textContent(),'1.00');
  assert.equal(await two.getByLabel('Scale',{exact:true}).inputValue(),'stdyx');
  await one.locator('[data-action="edit"]').click();
  const f=one.locator('[data-node-id="node:F"]'), box=await f.boundingBox();
  await page.mouse.move(box.x+box.width/2,box.y+box.height/2); await page.mouse.down(); await page.mouse.move(box.x+box.width/2+65,box.y+box.height/2+35,{steps:6}); await page.mouse.up();
  console.log('Dragged node');
  const moved=await config(page,one); assert.notDeepEqual(moved.state.views.full.nodes,baseline.state.views.full.nodes);
  assert.deepEqual((await config(page,two)).state,baseline.state);
  await one.locator('[data-action="undo"]').click(); assert.deepEqual((await config(page,one)).state.views.full.nodes,baseline.state.views.full.nodes);
  await one.locator('[data-action="redo"]').click(); assert.deepEqual((await config(page,one)).state.views.full.nodes,moved.state.views.full.nodes);
  await f.click(); await one.getByLabel('Display name',{exact:true}).fill('中文变量 <img src=x onerror=alert(1)>'); await one.locator('[data-action="alias"]').click();
  assert.match(await f.textContent(),/中文变量/); assert.equal(await one.locator('img').count(),0);
  await one.getByLabel('View',{exact:true}).selectOption('simplified'); assert.equal(await one.locator('.ld-node').count(),2);
  assert.deepEqual((await config(page,one)).state.views.full.nodes,moved.state.views.full.nodes);
  const simpleNode=one.locator('[data-node-id="node:F"]'),sb=await simpleNode.boundingBox();
  await page.mouse.move(sb.x+sb.width/2,sb.y+sb.height/2);await page.mouse.down();await page.mouse.move(sb.x+sb.width/2+20,sb.y+sb.height/2+20,{steps:3});await page.mouse.up();
  const simpleMoved=await config(page,one);assert.notDeepEqual(simpleMoved.state.views.simplified.nodes,baseline.state.views.simplified.nodes);assert.deepEqual(simpleMoved.state.views.full.nodes,moved.state.views.full.nodes);
  await one.getByLabel('View',{exact:true}).selectOption('full');
  const pathClick=await one.locator('[data-edge-id="loading:F:Y1"] .ld-line').evaluate(line=>{
    const p=line.getPointAtLength(line.getTotalLength()*.2),m=line.getScreenCTM();
    return {x:m.a*p.x+m.c*p.y+m.e,y:m.b*p.x+m.d*p.y+m.f};
  });
  await page.mouse.click(pathClick.x,pathClick.y); // The centered text legitimately covers the midpoint.

  const handle=one.locator('.ld-handle'), hb=await handle.boundingBox();
  await page.mouse.move(hb.x+hb.width/2,hb.y+hb.height/2);await page.mouse.down();await page.mouse.move(hb.x+hb.width/2,hb.y+hb.height/2+40,{steps:5});await page.mouse.up();
  assert.ok(Math.abs((await config(page,one)).state.views.full.edges['loading:F:Y1'].curve)>1);
  const edgeText=one.locator('[data-edge-id="loading:F:Y1"] .ld-edge-label'),lb=await edgeText.boundingBox();
  await page.mouse.move(lb.x+lb.width/2,lb.y+lb.height/2);await page.mouse.down();await page.mouse.move(lb.x+lb.width/2+25,lb.y+lb.height/2+15,{steps:3});await page.mouse.up();
  assert.ok(Math.abs((await config(page,one)).state.views.full.edges['loading:F:Y1'].label_dx)>1);
  // Multi-selection and alignment are presentation changes, and undo restores them.
  await one.locator('[data-node-id="node:Y1"]').click();await one.locator('[data-node-id="node:Y2"]').click({modifiers:['Shift']});
  const beforeAlign=await config(page,one);await one.locator('[data-action="align-horizontal"]').click();
  const aligned=await config(page,one);assert.equal(aligned.state.views.full.nodes['node:Y1'].y,aligned.state.views.full.nodes['node:Y2'].y);
  await one.locator('[data-action="undo"]').click();assert.deepEqual(await config(page,one),beforeAlign);
  console.log('Curve, label, selection and alignment assertions passed');
  const current=await config(page,one);
  assert.deepEqual(current.source, baseline.source); // Display edits never change result identity.
  for (const source of [undefined, Object.fromEntries(Object.keys(baseline.source).map(k=>[k,null])),
      {title:'Older run',output:'elsewhere.json',estimator:'BAYES',engine:'old',result_id:'b'.repeat(64)}]) {
    const imported=structuredClone(baseline);
    if (source === undefined) delete imported.source; else imported.source=source;
    await one.getByLabel('Import configuration file').setInputFiles({name:'compatible.json',mimeType:'application/json',buffer:Buffer.from(JSON.stringify(imported))});
    await one.getByRole('status').filter({hasText:'Configuration imported'}).waitFor();
    assert.deepEqual(await config(page,one),baseline); // Re-export uses current source, even for historical metadata.
    assert.equal(await one.locator('[data-edge-id="loading:F:Y1"] .ld-edge-text').textContent(),'.70**');
    await one.locator('[data-action="undo"]').click();assert.deepEqual(await config(page,one),current);
  }
  for (const source of [null, {...baseline.source,result_id:'bad'}, {...baseline.source,title:3},
      {...baseline.source,estimate:99}, {title:null}]) {
    const invalid=structuredClone(current);invalid.source=source;
    await one.getByLabel('Import configuration file').setInputFiles({name:'bad-source.json',mimeType:'application/json',buffer:Buffer.from(JSON.stringify(invalid))});
    await one.getByRole('status').filter({hasText:'Import rejected'}).waitFor();
    assert.deepEqual(await config(page,one),current);
  }
  const wrong=structuredClone(current);wrong.fingerprint='wrong';
  await one.getByLabel('Import configuration file').setInputFiles({name:'bad.json',mimeType:'application/json',buffer:Buffer.from(JSON.stringify(wrong))});
  await one.getByRole('status').filter({hasText:'Import rejected'}).waitFor(); assert.deepEqual(await config(page,one),current);
  const evil=structuredClone(current);evil.state.views.full.nodes['node:F'].x=1e7;
  await one.getByLabel('Import configuration file').setInputFiles({name:'bad.json',mimeType:'application/json',buffer:Buffer.from(JSON.stringify(evil))});
  await page.waitForTimeout(50); assert.deepEqual(await config(page,one),current);
  await one.locator('[data-action="restore"]').click(); assert.deepEqual((await config(page,one)).state.views.full.nodes,baseline.state.views.full.nodes);
  await one.locator('[data-action="undo"]').click(); assert.deepEqual(await config(page,one),current);
  await one.getByLabel('Import configuration file').setInputFiles({name:'valid.json',mimeType:'application/json',buffer:Buffer.from(JSON.stringify(baseline))});
  await one.getByRole('status').filter({hasText:'Configuration imported'}).waitFor();assert.deepEqual(await config(page,one),baseline);
  await one.locator('[data-action="undo"]').click();assert.deepEqual(await config(page,one),current);
  await one.locator('[data-action="zoom-in"]').click();
  const svg=(await exported(page,one,'svg')).toString(); assert.match(svg,/中文变量/);assert.match(svg,/<marker/);assert.doesNotMatch(svg,/data-editor-only|ld-toolbar|https?:\/\/(?!www.w3.org)/);
  assert.doesNotMatch(svg,/ld-heading|Display projection only|Double arrows: covariance|Short arrows: variance|Dotted: displayed|p &lt; \.05/);
  console.log('SVG assertions passed');
  const png=await exported(page,one,'png');assert.equal(png.subarray(1,4).toString(),'PNG');assert.ok(png.readUInt32BE(16)>500);
  // PNG scales both pixel dimensions, not diagram coordinates or coefficients.
  await one.getByLabel('PNG scale',{exact:true}).selectOption('1');
  const png1=await exported(page,one,'png');
  assert.equal(png.readUInt32BE(16),2*png1.readUInt32BE(16));
  assert.equal(png.readUInt32BE(20),2*png1.readUInt32BE(20));
  // Initial report state, not browser history, remains the refresh baseline.
  await page.evaluate(()=>instances[0].renderValue(fixture)); assert.deepEqual(await config(page,one),baseline);
  assert.deepEqual(errors,[]);
  // Removing a variable removes every incident path, including its residual.
  await one.locator('[data-action="edit"]').click();
  assert.equal(await one.locator('[data-action="hide-edge"], [data-action="show-edges"]').count(),0);
  assert.equal(await one.locator('[data-action="remove-object"]').isDisabled(),true);
  await one.getByLabel('Residuals',{exact:true}).check();
  const beforeRemoval=await config(page,one);
  await one.locator('[data-node-id="node:Y1"]').click();
  await one.locator('[data-action="remove-object"]').click();
  assert.equal(await one.locator('[data-node-id="node:Y1"]').count(),0);
  assert.equal(await one.locator('[data-edge-id="loading:F:Y1"], [data-edge-id="variance:Y1:Y1"]').count(),0);
  assert.equal(await one.locator('.ld-node').count(),3);
  assert.equal(await one.locator('.ld-edge').count(),3);
  const removed=await config(page,one);
  assert.deepEqual(removed.state.hidden_nodes,['node:Y1']);
  assert.deepEqual(removed.source,baseline.source);
  const removedSVG=(await exported(page,one,'svg')).toString();
  assert.doesNotMatch(removedSVG,/data-node-id="node:Y1"|data-edge-id="(?:loading:F:Y1|variance:Y1:Y1)"/);
  await one.locator('[data-action="undo"]').click();
  assert.deepEqual(await config(page,one),beforeRemoval);
  assert.equal(await one.locator('.ld-edge').count(),5);
  await one.locator('[data-action="redo"]').click();
  assert.deepEqual(await config(page,one),removed);
  await one.locator('[data-action="restore"]').click();
  assert.equal(await one.locator('.ld-node').count(),4);
  assert.equal(await one.locator('.ld-edge').count(),5);
  await one.locator('[data-action="undo"]').click();
  assert.deepEqual(await config(page,one),removed);
  // Import restores removal state without changing the statistical payload.
  await page.evaluate(()=>instances[0].renderValue(fixture));
  await one.getByLabel('Import configuration file').setInputFiles({name:'removed.json',mimeType:'application/json',buffer:Buffer.from(JSON.stringify(removed))});
  await one.getByRole('status').filter({hasText:'Configuration imported'}).waitFor();
  assert.deepEqual(await config(page,one),removed);
  assert.equal(await one.locator('[data-node-id="node:Y1"]').count(),0);
  for (const hidden of [['node:missing'],['node:F','node:F'],[1],null,{}]) {
    const invalid=structuredClone(removed);invalid.state.hidden_nodes=hidden;
    await one.getByLabel('Import configuration file').setInputFiles({name:'bad-removal.json',mimeType:'application/json',buffer:Buffer.from(JSON.stringify(invalid))});
    await one.getByRole('status').filter({hasText:'Import rejected'}).waitFor();
    assert.deepEqual(await config(page,one),removed);
  }
  await one.locator('[data-action="edit"]').click();
  await one.locator('[data-action="restore"]').click();
  // Remove a single path while keeping both endpoints.
  await one.locator('[data-edge-id="regression:X:F"] .ld-edge-label').click();
  await one.locator('[data-action="remove-object"]').click();
  assert.equal(await one.locator('.ld-node').count(),4);
  assert.equal(await one.locator('[data-edge-id="regression:X:F"]').count(),0);
  assert.deepEqual((await config(page,one)).state.hidden_edges,['regression:X:F']);
  await one.locator('[data-action="restore"]').click();
  // Multiple nodes, both views, and the empty diagram all remain recoverable.
  await one.locator('[data-node-id="node:X"]').click();
  await one.locator('[data-node-id="node:F"]').click({modifiers:['Shift']});
  await one.locator('[data-action="remove-object"]').click();
  assert.equal(await one.locator('.ld-node').count(),2);
  assert.equal(await one.locator('.ld-edge').count(),1);
  await one.getByLabel('View',{exact:true}).selectOption('simplified');
  assert.equal(await one.locator('.ld-node').count(),0);
  assert.equal(await one.locator('.ld-edge').count(),0);
  await one.getByLabel('View',{exact:true}).selectOption('full');
  await one.locator('[data-node-id="node:Y1"]').click();
  await one.locator('[data-node-id="node:Y2"]').click({modifiers:['Shift']});
  await one.locator('[data-action="remove-object"]').click();
  assert.equal(await one.locator('.ld-node').count(),0);
  assert.equal(await one.locator('.ld-edge').count(),0);
  await one.locator('[data-action="restore"]').click();
  assert.equal(await one.locator('.ld-node').count(),4);
  assert.equal(await one.locator('.ld-edge').count(),5);
  assert.deepEqual((await config(page,two)).state,baseline.state);
  console.log('PASS: object removal, incident paths and residuals, multiple selection, undo/redo/restore, config, SVG and empty views.');
  await page.evaluate(()=>instances[0].renderValue(fixture));
  // Literal display expectations: statistical values stay in the payload;
  // p comes from the selected scale, never borrowed from another table.
  const displayCases = [
    {estimate:.805,p:.0009,label:'.81***',dotted:false},
    {estimate:-.125,p:.001,label:'−.13**',dotted:false},
    {estimate:.5,p:.01,label:'.50*',dotted:false},
    {estimate:.5,p:.05,label:'',dotted:true},
    {estimate:.5,p:1,label:'',dotted:true},
    {estimate:0,p:0,label:'.00***',dotted:false},
    {estimate:-.0001,p:null,label:'.00',dotted:false},
    {estimate:1.005,p:null,label:'1.01',dotted:false},
    {estimate:-1.005,p:null,label:'−1.01',dotted:false},
    {estimate:999,p:999,label:'999.00',dotted:false},
    {estimate:.5,p:-.1,label:'.50',dotted:false},
    {estimate:.5,p:null,label:'.50',dotted:false}
  ];
  for (const test of displayCases) {
    await page.evaluate(test=>{
      const data=structuredClone(fixture);
      data.model.results['regression:X:F'].stdyx={estimate:test.estimate,p:test.p};
      data.model.results['regression:X:F'].unstandardized={estimate:-.125,p:.0001};
      instances[0].renderValue(data);
    },test);
    const edge=one.locator('[data-edge-id="regression:X:F"]');
    assert.equal(await edge.locator('.ld-edge-text').textContent(),test.label);
    assert.equal(await edge.evaluate(e=>e.classList.contains('ld-nonsignificant')),test.dotted);
    assert.equal(await edge.locator('.ld-line').evaluate(e=>getComputedStyle(e).strokeDasharray!=='none'),test.dotted);
    await one.getByLabel('Scale',{exact:true}).selectOption('unstandardized');
    assert.equal(await edge.locator('.ld-edge-text').textContent(),'−0.13***');
    assert.equal(await edge.evaluate(e=>e.classList.contains('ld-nonsignificant')),false);
  }
  // The three-state checkbox is keyboard accessible and persists through JSON.
  await page.evaluate(()=>{
    const data=structuredClone(fixture);
    data.model.results['regression:X:F'].stdyx.p=.5;
    instances[0].renderValue(data);
  });
  const dottedControl=one.getByLabel('Dotted nonsignificant',{exact:true});
  const nonsig=one.locator('[data-edge-id="regression:X:F"]');
  assert.equal(await dottedControl.getAttribute('aria-checked'),'true');
  assert.equal(await nonsig.locator('.ld-edge-text').textContent(),'');
  await one.getByLabel('Parameter labels',{exact:true}).check();
  assert.equal(await nonsig.locator('.ld-edge-text').textContent(),'');
  const fullCheck=await config(page,one);
  await dottedControl.click();
  assert.equal(await dottedControl.evaluate(e=>e.indeterminate),true);
  assert.equal(await dottedControl.getAttribute('aria-checked'),'mixed');
  assert.equal(await nonsig.locator('.ld-edge-text').textContent(),'.90 (b)');
  assert.equal(await nonsig.evaluate(e=>e.classList.contains('ld-nonsignificant')),true);
  const mixedCheck=await config(page,one);
  await dottedControl.focus();await page.keyboard.press('Space');
  assert.equal(await dottedControl.getAttribute('aria-checked'),'false');
  assert.equal(await dottedControl.evaluate(e=>e.indeterminate),false);
  assert.equal(await nonsig.locator('.ld-edge-text').textContent(),'.90 (b)');
  assert.equal(await nonsig.evaluate(e=>e.classList.contains('ld-nonsignificant')),false);
  const unchecked=await config(page,one);
  await dottedControl.click();
  assert.deepEqual(await config(page,one),fullCheck);
  await one.locator('[data-action="edit"]').click();
  await one.locator('[data-action="undo"]').click();
  assert.deepEqual(await config(page,one),unchecked);
  await one.locator('[data-action="undo"]').click();
  assert.deepEqual(await config(page,one),mixedCheck);
  assert.equal(await dottedControl.getAttribute('aria-checked'),'mixed');
  await one.locator('[data-action="redo"]').click();
  for (const [saved,aria,label,dotted] of [[fullCheck,'true','',true],[mixedCheck,'mixed','.90 (b)',true],[unchecked,'false','.90 (b)',false]]) {
    await one.getByLabel('Import configuration file').setInputFiles({name:'three-state.json',mimeType:'application/json',buffer:Buffer.from(JSON.stringify(saved))});
    await one.getByRole('status').filter({hasText:'Configuration imported'}).waitFor();
    assert.deepEqual(await config(page,one),saved);
    assert.equal(await dottedControl.getAttribute('aria-checked'),aria);
    assert.equal(await nonsig.locator('.ld-edge-text').textContent(),label);
    assert.equal(await nonsig.evaluate(e=>e.classList.contains('ld-nonsignificant')),dotted);
    const svg=(await exported(page,one,'svg')).toString();
    assert.equal(svg.includes('.90 (b)'),label!== '');
  }
  // Hiding values globally still applies in the mixed state.
  await dottedControl.click();await dottedControl.click();
  await one.getByLabel('Values',{exact:true}).uncheck();
  assert.equal(await nonsig.locator('.ld-edge-text').textContent(),'(b)');
  console.log('PASS: three-state significance, keyboard, labels, history, JSON and SVG.');
  await page.evaluate(()=>instances[0].renderValue(fixture));
  const fixed=one.locator('[data-edge-id="loading:F:Y1"]');
  assert.equal(await fixed.locator('.ld-edge-text').textContent(),'.70**');
  assert.equal(await fixed.locator('.ld-line').evaluate(e=>getComputedStyle(e).strokeDasharray),'none');
  assert.match(await fixed.locator('title').textContent(),/Fixed original value: 1/);
  await page.evaluate(()=>{
    const changed=structuredClone(fixture);changed.model.results['loading:F:Y1'].stdyx.p=.05;
    instances[0].renderValue(changed);
  });
  assert.equal(await fixed.locator('.ld-edge-text').textContent(),'');
  assert.equal(await fixed.evaluate(e=>e.classList.contains('ld-nonsignificant')),true);
  await one.getByLabel('Scale',{exact:true}).selectOption('unstandardized');
  assert.equal(await fixed.locator('.ld-edge-text').textContent(),'1.00');
  assert.equal(await fixed.locator('.ld-line').evaluate(e=>getComputedStyle(e).strokeDasharray),'none');
  await page.evaluate(()=>instances[0].renderValue(fixture));

  const centered=await fixed.evaluate(e=>{
    const p=e.querySelector('.ld-line'),t=e.querySelector('.ld-edge-text'),m=p.getPointAtLength(p.getTotalLength()/2);
    return {dx:Number(t.getAttribute('x'))-m.x,dy:Number(t.getAttribute('y'))-m.y,baseline:t.getAttribute('dominant-baseline')||getComputedStyle(t).dominantBaseline};
  });
  assert.ok(Math.abs(centered.dx)<.1 && Math.abs(centered.dy)<.1);
  assert.equal(centered.baseline,'middle');
  const legacy=await config(page,one);delete legacy.state.options.stars;delete legacy.state.options.nonsignificant_dotted;delete legacy.state.options.nonsignificant_hide_values;delete legacy.state.hidden_nodes;
  await one.getByLabel('Import configuration file').setInputFiles({name:'old-options.json',mimeType:'application/json',buffer:Buffer.from(JSON.stringify(legacy))});
  await one.getByRole('status').filter({hasText:'Configuration imported'}).waitFor();
  assert.equal((await config(page,one)).state.options.stars,true);
  assert.equal((await config(page,one)).state.options.nonsignificant_dotted,true);
  await one.getByLabel('Significance stars',{exact:true}).uncheck();
  assert.equal(await one.locator('[data-edge-id="regression:X:F"] .ld-edge-text').textContent(),'.90');
  assert.equal((await config(page,one)).state.options.stars,false);
  await one.getByLabel('Dotted nonsignificant',{exact:true}).click();
  await one.getByLabel('Dotted nonsignificant',{exact:true}).click();
  assert.equal((await config(page,one)).state.options.nonsignificant_dotted,false);
  await page.evaluate(()=>instances[0].renderValue(fixture));
  console.log('PASS: APA labels, significance thresholds, p-value isolation, fixed-path styling, centered labels, legacy options.');
  await page.evaluate(data=>instances[0].renderValue(data),measurementFixture());
  const factorCov=one.locator('[data-edge-id="covariance:F:G"]'),residualCov=one.locator('[data-edge-id="covariance:Y1:Y2"]');
  const factorBefore=await endpoints(factorCov),residualBefore=await endpoints(residualCov);
  assert.match(factorBefore.d,/C/); // Separate endpoint tangents for a smooth outer bow.
  assert.ok(Math.abs(factorBefore.start.x-97)<.1 && Math.abs(factorBefore.end.x-97)<.1);
  assert.ok(Math.abs(factorBefore.start.y-140)<.1 && Math.abs(factorBefore.end.y-440)<.1);
  const curvature=await factorCov.locator('.ld-line').evaluate(line=>{
    const a=line.getPointAtLength(0),b=line.getPointAtLength(line.getTotalLength()),m=line.getPointAtLength(line.getTotalLength()/2);
    return Math.abs(m.x-(a.x+b.x)/2);
  });
  assert.ok(Math.abs(curvature-300*.15)<.1,'CFA uses the approximately 15% reference bow ratio');
  for (const span of [150,600]) {
    const differentlySpaced=measurementFixture();
    for(const v of ['full','simplified']) {
      differentlySpaced.state.views[v].nodes['node:G'].y=140+span;
      differentlySpaced.automatic[v].nodes['node:G'].y=140+span;
    }
    await page.evaluate(data=>instances[0].renderValue(data),differentlySpaced);
    const bend=await factorCov.locator('.ld-line').evaluate(line=>{
      const a=line.getPointAtLength(0),b=line.getPointAtLength(line.getTotalLength()),m=line.getPointAtLength(line.getTotalLength()/2);
      return Math.abs(m.x-(a.x+b.x)/2);
    });
    assert.ok(Math.abs(bend/span-.15)<.001,'Short and long links share the reference bow-to-span ratio');
  }
  await page.evaluate(data=>instances[0].renderValue(data),measurementFixture());
  assert.match(await factorCov.locator('.ld-edge-text').evaluate(t=>getComputedStyle(t).fontFamily),/Times New Roman/);
  assert.match(await one.locator('.ld-node-text').first().evaluate(t=>getComputedStyle(t).fontFamily),/Times New Roman/);
  const clearances=await one.evaluate(root=>{
    const labels=Array.from(root.querySelectorAll('.ld-edge-text')).filter(e=>e.textContent);
    const holes=Array.from(root.querySelectorAll('.ld-label-clearance'));
    return labels.every(t=>{const b=t.getBBox();return holes.some(r=>Number(r.getAttribute('x'))<=b.x-3 &&
      Number(r.getAttribute('y'))<=b.y-1.5 && Number(r.getAttribute('x'))+Number(r.getAttribute('width'))>=b.x+b.width+3 &&
      Number(r.getAttribute('y'))+Number(r.getAttribute('height'))>=b.y+b.height+1.5);});
  });
  assert.ok(clearances,'Whole-label clearances protect decimal points and inter-character gaps');
  assert.equal(await one.locator('[data-edge-id^="variance:"]').count(),0);
  await one.getByLabel('Residuals',{exact:true}).check();
  const factorAfter=await endpoints(factorCov),residualAfter=await endpoints(residualCov);
  assert.deepEqual(factorAfter,factorBefore); // Factor covariance never becomes a residual covariance.
  for (const [id,which] of [['Y1','start'],['Y2','end']]) {
    const arrow=await endpoints(one.locator(`[data-edge-id="variance:${id}:${id}"]`));
    assert.equal(arrow.markerStart,null);assert.ok(arrow.markerEnd);
    assert.doesNotMatch(arrow.d,/[CQ]/); // Short arrow, not a self-loop.
    const valueBox=await one.locator(`[data-edge-id="variance:${id}:${id}"] .ld-edge-text`).evaluate(t=>{
      const b=t.getBBox();return {x:b.x,y:b.y,width:b.width,height:b.height};
    });
    assert.ok(valueBox.x>=arrow.start.x+5,'Residual number sits beyond the right-hand outer tail, not on the shaft');

    assert.ok(Math.hypot(residualAfter[which].x-arrow.start.x,residualAfter[which].y-arrow.start.y)<.1);
  }
  const residualBend=await residualCov.locator('.ld-line').evaluate(line=>{
    const a=line.getPointAtLength(0),b=line.getPointAtLength(line.getTotalLength()),m=line.getPointAtLength(line.getTotalLength()/2);
    return Math.abs(m.x-(a.x+b.x)/2);
  });
  assert.ok(Math.abs(residualBend/140-curvature/300)<.001,'Residual and CFA covariances share the same default proportion');
  const exportWithTails=(await exported(page,one,'svg')).toString();
  assert.match(exportWithTails,/Times New Roman/);assert.match(exportWithTails,/<mask/);assert.match(exportWithTails,/ld-label-clearance/);
  assert.notDeepEqual(residualAfter,residualBefore);
  assert.equal(await one.locator('.ld-node').count(),6); // Decorations do not invent statistical nodes.
  await one.getByLabel('Residuals',{exact:true}).uncheck();
  assert.deepEqual(await endpoints(residualCov),residualBefore);
  await one.locator('[data-action="edit"]').click();
  await factorCov.locator('.ld-edge-text').click();
  const controlBox=await one.locator('.ld-handle').boundingBox(),labelBox=await factorCov.locator('.ld-edge-text').boundingBox();
  assert.ok(controlBox.x+controlBox.width<=labelBox.x || labelBox.x+labelBox.width<=controlBox.x ||
    controlBox.y+controlBox.height<=labelBox.y || labelBox.y+labelBox.height<=controlBox.y,
    'Curve control must not obstruct dragging the centered label');
  const curveBaseline=await config(page,one);
  await page.mouse.move(controlBox.x+controlBox.width/2,controlBox.y+controlBox.height/2);
  await page.mouse.down();await page.mouse.move(controlBox.x+controlBox.width/2-30,controlBox.y+controlBox.height/2,{steps:5});await page.mouse.up();
  const curved=await endpoints(factorCov);
  assert.notEqual(curved.d,factorBefore.d);assert.deepEqual(curved.start,factorBefore.start);assert.deepEqual(curved.end,factorBefore.end);
  await one.locator('[data-action="undo"]').click();assert.deepEqual(await config(page,one),curveBaseline);
  await page.evaluate(()=>instances[0].renderValue(fixture));
  console.log('PASS: CFA left-side cubic covariance, short variance arrows and conditional residual covariance docking.');
  // Three factors reproduce the reference's adjacent/outer covariance nesting.
  // This is a rendering-contract fixture, not a fabricated Mplus analysis.
  const three=measurementFixture();
  for(const [name,role,x,y] of [['H','latent',150,740],['Z1','observed',450,680],['Z2','observed',450,820]]) {
    three.model.structure.nodes.push({id:'node:'+name,name,role,endogenous:role==='observed'});
    for(const view of ['full','simplified']) {three.state.views[view].nodes['node:'+name]={x,y};three.automatic[view].nodes['node:'+name]={x,y};}
  }
  for(const [type,from,to] of [['loading','H','Z1'],['loading','H','Z2'],['covariance','F','H'],['covariance','G','H']]) {
    const id=`${type}:${from}:${to}`;three.model.structure.edges.push({id,type,from:'node:'+from,to:'node:'+to,fixed:false,origin:'explicit'});
    three.model.results[id]={unstandardized:{estimate:.25,p:.02},stdyx:{estimate:.25,p:.02}};
  }
  await page.evaluate(data=>instances[0].renderValue(data),three);
  const nesting=await one.evaluate(root=>{
    const xAt=(id,y)=>{const p=root.querySelector(`[data-edge-id="${id}"] .ld-line`);let lo=0,hi=p.getTotalLength();for(let i=0;i<30;i++){const m=(lo+hi)/2;if(p.getPointAtLength(m).y<y)lo=m;else hi=m;}return p.getPointAtLength((lo+hi)/2).x;};
    return [xAt('covariance:F:G',290)-xAt('covariance:F:H',290),xAt('covariance:G:H',590)-xAt('covariance:F:H',590)];
  });
  assert.ok(nesting.every(clearance=>clearance>10),'Long outer covariance stays outside both adjacent-factor curves');
  await page.evaluate(()=>instances[0].renderValue(fixture));

  const intermediate=measurementFixture();intermediate.model.structure.family='clpm';intermediate.state.options.residuals=true;
  intermediate.model.structure.edges.forEach(e=>{
    if(e.type==='loading'){const old=e.id;e.type='regression';e.id=old.replace('loading:','regression:');intermediate.model.results[e.id]=intermediate.model.results[old];delete intermediate.model.results[old];}
  });
  for(const [from,to] of [['Y1','Y3'],['Y2','Y4']]) {
    const id=`regression:${from}:${to}`;intermediate.model.structure.edges.push({id,type:'regression',from:'node:'+from,to:'node:'+to,fixed:false,origin:'explicit'});
    intermediate.model.results[id]={unstandardized:{estimate:.3,p:.001},stdyx:{estimate:.3,p:.001}};
  }
  for(const view of ['full','simplified']) {
    for(const [name,x,y] of [['F',150,100],['G',150,350],['Y1',450,100],['Y2',450,350],['Y3',750,100],['Y4',750,350]])
      intermediate.state.views[view].nodes['node:'+name]={x,y};
    intermediate.automatic[view]=structuredClone(intermediate.state.views[view]);
  }
  await page.evaluate(data=>instances[0].renderValue(data),intermediate);
  const route=await one.evaluate(root=>{
    const path=root.querySelector('[data-edge-id="covariance:Y1:Y2"] .ld-line'),length=path.getTotalLength();
    const boxes=['Y1','Y2'].map(id=>{const n=root.querySelector(`[data-node-id="node:${id}"]`),b=n.querySelector('.ld-shape').getBBox(),m=n.transform.baseVal.consolidate().matrix;return{x:b.x+m.e,y:b.y+m.f,w:b.width,h:b.height};});
    const a=path.getPointAtLength(0),z=path.getPointAtLength(length),mid=path.getPointAtLength(length/2);
    const points=Array.from({length:101},(_,i)=>path.getPointAtLength(length*i/100));
    return {crosses:points.some(p=>boxes.some(b=>p.x>b.x+.1 && p.x<b.x+b.w-.1 && p.y>b.y+.1 && p.y<b.y+b.h-.1)),bow:Math.abs(mid.x-(a.x+z.x)/2),span:Math.hypot(z.x-a.x,z.y-a.y)};
  });
  assert.equal(route.crosses,false,'A shallow intermediate residual covariance must not pass through its own variables');
  assert.ok(Math.abs(route.bow/route.span-.15)<.001,'Residual ports preserve the shared 15% reference proportion');
  await page.evaluate(()=>instances[0].renderValue(fixture));


  // A present table with no usable standardized estimates is unavailable.
  // The displayed scale, selector and downloaded config must agree, including
  // after importing a configuration that requests the unavailable scale.
  await page.evaluate(()=>{
    const unavailable=structuredClone(fixture);
    Object.values(unavailable.model.results).forEach(r=>{r.stdyx={estimate:null};});
    unavailable.model.diagnostics=[{severity:'warning',code:'engine-warning',message:'Original Mplus warning must remain visible.'}];
    instances[0].renderValue(unavailable);
  });
  assert.equal(await one.getByLabel('Scale',{exact:true}).inputValue(),'unstandardized');
  assert.equal(await one.locator('option[value="stdyx"]').evaluate(e=>e.disabled),true);
  assert.equal((await config(page,one)).state.options.scale,'unstandardized');
  assert.equal(await one.locator('.ld-heading').count(),0);
  const unavailableConfig=await config(page,one); unavailableConfig.state.options.scale='stdyx';
  await one.getByLabel('Import configuration file').setInputFiles({name:'request-stdyx.json',mimeType:'application/json',buffer:Buffer.from(JSON.stringify(unavailableConfig))});
  await one.getByRole('status').filter({hasText:'Configuration imported'}).waitFor();
  assert.equal((await config(page,one)).state.options.scale,'unstandardized');
  assert.match(await one.locator('.ld-diagnostics').textContent(),/Original Mplus warning/);
  assert.match(await one.locator('.ld-diagnostics').textContent(),/STDYX unavailable/);
  assert.deepEqual(errors,[]);
  if(process.env.MPLUS_DIAGRAM_HTML){
    await page.goto('file://'+path.resolve(process.env.MPLUS_DIAGRAM_HTML));
    await page.locator('.mplus-diagram .ld-node').first().waitFor(); assert.ok(await page.locator('.ld-node').count()>0);
    const widgets=page.locator('.mplus-diagram'),first=widgets.first(),firstBaseline=await config(page,first);
    const secondBaseline=await widgets.count()>1?await config(page,widgets.nth(1)):null;
    await first.locator('[data-action="edit"]').click();await first.locator('.ld-node').first().click();
    await first.getByLabel('Display name',{exact:true}).fill('真实报告离线修改');await first.locator('[data-action="alias"]').click();
    const realChanged=await config(page,first);assert.notDeepEqual(realChanged,firstBaseline);
    if(secondBaseline) assert.deepEqual(await config(page,widgets.nth(1)),secondBaseline);
    await first.getByLabel('Import configuration file').setInputFiles({name:'real-config.json',mimeType:'application/json',buffer:Buffer.from(JSON.stringify(realChanged))});
    await first.getByRole('status').filter({hasText:'Configuration imported'}).waitFor();assert.deepEqual(await config(page,first),realChanged);
    if(process.env.MPLUS_DIAGRAM_CONFIG_EXPORT) fs.writeFileSync(process.env.MPLUS_DIAGRAM_CONFIG_EXPORT,JSON.stringify(realChanged,null,2));
    await page.reload();await page.locator('.mplus-diagram .ld-node').first().waitFor();assert.deepEqual(await config(page,page.locator('.mplus-diagram').first()),firstBaseline);
    assert.deepEqual(errors,[]);
    console.log('PASS: real R HTML offline alias/config round-trip, multi-instance isolation (when present), and reload baseline.');
  }
  await browser.close(); console.log('PASS: offline rendering, scale isolation, node drag, undo/redo, aliases, views, curves, config rejection, restoration, SVG/PNG, baseline reset.');
})().catch(e=>{console.error(e);process.exit(1);});
