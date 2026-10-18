/* Residual-arrow interaction contract. This synthetic graph is deliberately NOT
 * a Mplus fit or parser-support fixture. It mounts the shipped SVG binding and
 * uses actual browser pointer input; no handlers or state setters are invoked.
 * Run: CHROME_PATH=/path/to/chromium node tests/browser/residual-orbit.test.cjs
 * Optional MPLUS_DIAGRAM_HTML smoke-tests a real report; MPLUS_ORBIT_RESULTS writes
 * machine-readable checks. Browser context is offline throughout.
 */
const {chromium}=require('playwright');
const assert=require('node:assert/strict');
const fs=require('node:fs');
const path=require('node:path');
const {pathToFileURL}=require('node:url');
const root=path.resolve(__dirname,'../..');
const ids=['A','F'];
const arrowLength=64*2/3;
const varianceID=id=>`variance:${id}:${id}`;
function fixture() {
  const nodes=[{id:'node:X',name:'X',role:'observed',endogenous:false},
    {id:'node:A',name:'A',role:'observed',endogenous:true},
    {id:'node:F',name:'F',role:'latent',endogenous:true},
    {id:'node:Z',name:'Z',role:'observed',endogenous:true}];
  const edge=(type,from,to,extra={})=>({id:`${type}:${from}:${to}`,type,from:'node:'+from,to:'node:'+to,fixed:false,origin:'explicit',...extra});
  const edges=[edge('regression','X','A'),edge('regression','X','F'),edge('regression','A','Z'),edge('regression','F','Z'),
    edge('variance','A','A',{residual:true}),edge('variance','F','F',{residual:true}),
    edge('covariance','A','F',{residual_from:true,residual_to:true})];
  const results=Object.fromEntries(edges.map(e=>[e.id,{unstandardized:{estimate:e.type==='variance'?1.25:.42,p:.0005},stdyx:{estimate:e.type==='variance'?.36:.24,p:.0005}}]));
  // Legacy curve overrides exercise initial non-radial placement without
  // assuming an initial residual_angle or manufacturing its expected value.
  const view={nodes:{'node:X':{x:140,y:440},'node:A':{x:470,y:230},'node:F':{x:470,y:660},'node:Z':{x:780,y:440}},
    edges:{'variance:A:A':{curve:17},'variance:F:F':{curve:-19},'covariance:A:F':{curve:45}},viewport:{x:0,y:0,k:1}};
  return {schema_version:1,model:{structure:{family:'sem',nodes,edges,constraints:[],mapping:{}},results,diagnostics:[],
    provenance:{title:'Synthetic residual-orbit rendering contract',output:'rendering-contract-only',estimator:'fixture',engine:'fixture',result_id:'b'.repeat(64)},fingerprint:'orbit-rendering-fixture'},
    state:{options:{scale:'stdyx',digits:2,residuals:true,default_covariances:true,parameter_labels:false,values:true,fixed_values:true,constraints:false,view:'full',stars:true,nonsignificant_dotted:true},
      aliases:{},hidden_edges:[],views:{full:structuredClone(view),simplified:structuredClone(view)}},automatic:{full:structuredClone(view),simplified:structuredClone(view)},editor:null};
}
async function config(widget) {
  return widget.evaluate(async root=>{
    const originalURL=URL.createObjectURL,originalClick=HTMLAnchorElement.prototype.click;let blob;
    try {URL.createObjectURL=b=>{blob=b;return originalURL(b);};HTMLAnchorElement.prototype.click=function(){};
      root.querySelector('[data-action="export-config"]').click();
    } finally {URL.createObjectURL=originalURL;HTMLAnchorElement.prototype.click=originalClick;}
    return JSON.parse(await blob.text());
  });
}
async function geometry(widget,id) {
  return widget.evaluate((root,id)=>{
    const node=root.querySelector(`[data-node-id="node:${id}"]`),edge=root.querySelector(`[data-edge-id="variance:${id}:${id}"]`);
    const shape=node.querySelector('.ld-shape'),box=shape.getBBox(),matrix=node.transform.baseVal.consolidate().matrix;
    const line=edge.querySelector('.ld-line'),tail=line.getPointAtLength(0),head=line.getPointAtLength(line.getTotalLength());
    const text=edge.querySelector('.ld-edge-text'),label=text.getBBox();
    const cov=root.querySelector('[data-edge-id="covariance:A:F"] .ld-line');
    const covStart=cov.getPointAtLength(0),covEnd=cov.getPointAtLength(cov.getTotalLength());
    return {id,shape:shape.localName,node:{x:matrix.e,y:matrix.f,width:box.width,height:box.height},
      tail:{x:tail.x,y:tail.y},head:{x:head.x,y:head.y},length:line.getTotalLength(),path:line.getAttribute('d'),
      markerStart:line.getAttribute('marker-start'),markerEnd:line.getAttribute('marker-end'),
      label:{x:label.x,y:label.y,width:label.width,height:label.height,text:text.textContent},
      samples:[.1,.3,.5,.7,.9].map(t=>{const p=line.getPointAtLength(line.getTotalLength()*t);return {x:p.x,y:p.y};}),
      cov:{from:{x:covStart.x,y:covStart.y},to:{x:covEnd.x,y:covEnd.y}},
      attributes:{tailX:Number(edge.dataset.tailX),tailY:Number(edge.dataset.tailY),headX:Number(edge.dataset.headX),headY:Number(edge.dataset.headY),
        angle:edge.dataset.residualAngle,offset:edge.dataset.residualOffset}};
  },id);
}
const distance=(a,b)=>Math.hypot(a.x-b.x,a.y-b.y);
const bearing=g=>Math.atan2(g.tail.y-g.node.y,g.tail.x-g.node.x);
const angularDelta=(a,b)=>Math.atan2(Math.sin(b-a),Math.cos(b-a));
let assertions=0,activeCircle=null;
function check(test,message) {assertions++;assert.ok(test,message);}
function equal(actual,expected,message) {assertions++;assert.deepEqual(actual,expected,message);}
function boundaryValue(point,g) {
  const x=Math.abs(point.x-g.node.x)/(g.node.width/2),y=Math.abs(point.y-g.node.y)/(g.node.height/2);
  return g.shape==='ellipse'?x*x+y*y:Math.max(x,y);
}
function verify(g,phase) {
  const label=`${g.id} ${phase}`;
  check(Math.abs(g.length-arrowLength)<.03,`${label}: path length must remain ${arrowLength} model units, got ${g.length}`);
  check(Math.abs(distance(g.tail,g.head)-arrowLength)<.03,`${label}: straight arrow must not stretch`);
  check(Math.abs(boundaryValue(g.head,g)-1)<.015,`${label}: inward head must stay on ${g.shape}`);
  check(g.samples.every(p=>boundaryValue(p,g)>=1-.002),`${label}: arrow shaft must stay outside its variable`);
  check(!g.markerStart && Boolean(g.markerEnd),`${label}: one inward arrowhead only`);
  check(Math.hypot(g.attributes.tailX-g.tail.x,g.attributes.tailY-g.tail.y)<.03,`${label}: published tail agrees with path`);
  check(Math.hypot(g.attributes.headX-g.head.x,g.attributes.headY-g.head.y)<.03,`${label}: published head agrees with path`);
  const u={x:(g.tail.x-g.head.x)/arrowLength,y:(g.tail.y-g.head.y)/arrowLength};
  const centerVector={x:g.node.x-g.head.x,y:g.node.y-g.head.y};
  check(Math.abs(centerVector.x*u.y-centerVector.y*u.x)<.03,`${label}: arrow axis must pass through the variable center`);
  check(centerVector.x*u.x+centerVector.y*u.y<0,`${label}: arrow must point toward, not away from, the center`);
  const corners=[[g.label.x,g.label.y],[g.label.x+g.label.width,g.label.y],
    [g.label.x,g.label.y+g.label.height],[g.label.x+g.label.width,g.label.y+g.label.height]];
  const outer=Math.min(...corners.map(([x,y])=>(x-g.tail.x)*u.x+(y-g.tail.y)*u.y));
  check(g.label.width>0 && outer>0.25,`${label}: entire label box must be beyond the tail (clearance ${outer})`);
  check(distance(g.id==='A'?g.cov.from:g.cov.to,g.tail)<.05,`${label}: covariance must follow the current residual tail`);
}
async function verifyBoth(widget,phase) {for(const id of ids) verify(await geometry(widget,id),phase);}
async function modelPoint(widget,p) {
  return widget.evaluate((root,p)=>{
    const svg=root.querySelector('svg.ld-canvas'),point=svg.createSVGPoint();point.x=p.x;point.y=p.y;
    const screen=point.matrixTransform(svg.getScreenCTM());return {x:screen.x,y:screen.y,width:innerWidth,height:innerHeight};
  },p);
}
async function moveModel(page,widget,p,steps=2) {
  const screen=await modelPoint(widget,p);
  check(screen.x>0 && screen.y>0 && screen.x<screen.width && screen.y<screen.height,'Test pointer target must remain inside browser viewport');
  await page.mouse.move(screen.x,screen.y,{steps});
}
async function startDrag(page,widget,id,part) {
  await widget.locator('.ld-stage').scrollIntoViewIfNeeded();
  const edge=widget.locator(`[data-edge-id="${varianceID(id)}"]`);
  if(part==='control') {
    // Select through a real pointer click on the shaft to expose the control.
    const shaft=await edge.locator('.ld-line').evaluate(line=>{const p=line.getPointAtLength(line.getTotalLength()*.45).matrixTransform(line.getScreenCTM());return {x:p.x,y:p.y};});
    await page.mouse.click(shaft.x,shaft.y);
  }
  let screen;
  if(part==='shaft') screen=await edge.locator('.ld-line').evaluate(line=>{const p=line.getPointAtLength(line.getTotalLength()*.45).matrixTransform(line.getScreenCTM());return {x:p.x,y:p.y};});
  else {
    const target=edge.locator(part==='label'?'.ld-edge-label':'.ld-handle'),box=await target.boundingBox();
    check(Boolean(box && box.width && box.height),`${id} ${part}: visible drag target`);
    screen={x:box.x+box.width/2,y:box.y+box.height/2};
    const hit=await page.evaluate(p=>{const e=document.elementFromPoint(p.x,p.y);return e && e.closest('.ld-edge') && e.closest('.ld-edge').dataset.edgeId;},screen);
    equal(hit,varianceID(id),`${id} ${part}: actual pointer target belongs to the variance arrow`);
  }
  await page.mouse.move(screen.x,screen.y);await page.mouse.down();
  const actual=await page.evaluate(()=>window.orbitPointerEvents.filter(e=>e.type==='pointerdown').slice(-1)[0]);
  equal(actual && actual.edge,varianceID(id),`${id} ${part}: trusted pointerdown must reach the intended arrow`);
  if(part==='control') check(actual.target && actual.target.split(' ').includes('ld-handle'),`${id}: control must be the actual pointerdown target, got ${JSON.stringify(actual)}`);
  if(part==='label') check(actual.target && actual.target.split(' ').includes('ld-edge-label'),`${id}: label must be the actual pointerdown target, got ${JSON.stringify(actual)}`);
  return geometry(widget,id);
}
async function circle(page,widget,id,part) {
  const first=await startDrag(page,widget,id,part),initial=bearing(first),radius=Math.max(first.node.width,first.node.height)/2+130;
  await moveModel(page,widget,{x:first.node.x+Math.cos(initial)*radius,y:first.node.y+Math.sin(initial)*radius});
  const start=await geometry(widget,id),quadrants=new Set();let previous=bearing(start),travel=0;
  activeCircle={id,part,start,samples:[]};
  for(let n=1;n<=24;n++) {
    const angle=initial+n*Math.PI/12;
    await moveModel(page,widget,{x:first.node.x+Math.cos(angle)*radius,y:first.node.y+Math.sin(angle)*radius});
    const g=await geometry(widget,id);verify(g,`${part} orbit sample ${n}`);
    equal(Number(g.attributes.offset),0,`${id} ${part}: orbit has no lateral displacement from the center`);
    const current=bearing(g);travel+=angularDelta(previous,current);previous=current;
    activeCircle.samples.push({n,bearing:current*180/Math.PI,tail:g.tail,angle:g.attributes.angle});
    quadrants.add(`${g.tail.x>=g.node.x?'+':'-'}${g.tail.y>=g.node.y?'+':'-'}`);
  }
  const end=await geometry(widget,id);await page.mouse.up();
  equal(quadrants.size,4,`${id} ${part}: all four quadrants reached`);
  check(Math.abs(travel)>Math.PI*1.8,`${id} ${part}: full orbit, not a small control-point sway (${travel*180/Math.PI} degrees)`);
  check(distance(start.tail,end.tail)<3,`${id} ${part}: circular input returns to the same orbit point`);
  verify(await geometry(widget,id),`${part} circle released`);
  return {node:id,part,quadrants:quadrants.size,travelDegrees:travel*180/Math.PI};
}
async function radial(page,widget,id,part) {
  const first=await startDrag(page,widget,id,part),angle=bearing(first);
  for(const radius of [14,210,24,250,90]) {
    await moveModel(page,widget,{x:first.node.x+Math.cos(angle)*radius,y:first.node.y+Math.sin(angle)*radius});
    verify(await geometry(widget,id),`${part} radial distance ${radius}`);
  }
  await page.mouse.up();verify(await geometry(widget,id),`${part} radial released`);
}
async function rotateLabel(page,widget,id) {
  const before=await config(widget),first=await startDrag(page,widget,id,'label'),angle=bearing(first)+Math.PI/2;
  await moveModel(page,widget,{x:first.node.x+Math.cos(angle)*160,y:first.node.y+Math.sin(angle)*160},8);
  await page.mouse.up();const after=await config(widget),g=await geometry(widget,id);verify(g,'label rotates arrow');
  check(distance(first.tail,g.tail)>20,`${id}: label drag must rotate the arrow, not detach its number`);
  const override=after.state.views.full.edges[varianceID(id)];
  check(Number.isFinite(override.residual_angle) && Math.abs(override.residual_angle)<=180,`${id}: exported orbit angle is valid`);
  if(Object.hasOwn(override,'residual_offset')) check(Number.isFinite(override.residual_offset) && Math.abs(override.residual_offset)<=.95,`${id}: exported lateral offset is valid`);
  check(!Object.hasOwn(override,'label_dx') && !Object.hasOwn(override,'label_dy'),`${id}: label drag does not create independent label offsets`);
  await widget.locator('[data-action="undo"]').click();equal((await config(widget)).state.views,before.state.views,`${id}: undo restores orbit`);await verifyBoth(widget,'undo');
  await widget.locator('[data-action="redo"]').click();equal((await config(widget)).state.views,after.state.views,`${id}: redo restores orbit`);await verifyBoth(widget,'redo');
}
async function importConfig(widget,c,name='orbit.json') {
  await widget.getByLabel('Import configuration file').setInputFiles({name,mimeType:'application/json',buffer:Buffer.from(JSON.stringify(c))});
}
(async()=>{
  const browser=await chromium.launch({headless:true,...(process.env.CHROME_PATH?{executablePath:process.env.CHROME_PATH}:{})});
  const context=await browser.newContext({viewport:{width:2100,height:1800},acceptDownloads:true});await context.setOffline(true);
  const page=await context.newPage(),errors=[],network=[],report={browser:browser.version(),fixture:'synthetic-rendering-contract-only',offline:true,circles:[]};
  page.on('pageerror',e=>errors.push(e.message));page.on('request',r=>{if(/^https?:/.test(r.url()))network.push(r.url());});
  try {
    await page.setContent('<html><body style="margin:0"><div id="one" style="margin:220px 0 0 300px;width:1400px;height:1100px"></div><div id="two" style="margin-left:300px;width:1400px;height:1100px"></div></body></html>');
    await page.evaluate(()=>{window.orbitPointerEvents=[];for(const type of ['pointerdown','pointerup']) document.addEventListener(type,e=>{const edge=e.target.closest('.ld-edge');orbitPointerEvents.push({type,target:e.target.getAttribute('class'),edge:edge&&edge.dataset.edgeId,x:e.clientX,y:e.clientY,trusted:e.isTrusted});},true);});
    await page.addStyleTag({content:fs.readFileSync(path.join(root,'inst/htmlwidgets/lib/mplus-diagram/mplus-diagram.css'),'utf8')});
    await page.evaluate(()=>{window.HTMLWidgets={widget:b=>{window.orbitBinding=b;}};});
    await page.addScriptTag({content:fs.readFileSync(path.join(root,'inst/htmlwidgets/mplus_diagram.js'),'utf8')});
    await page.evaluate(data=>{for(const id of ['one','two'])orbitBinding.factory(document.getElementById(id),1400,1100).renderValue(data);},fixture());
    const widget=page.locator('#one'),other=page.locator('#two'),otherBaseline=await config(other);
    equal((await geometry(widget,'A')).shape,'rect','Observed variable is rectangular');equal((await geometry(widget,'F')).shape,'ellipse','Endogenous latent is elliptical');
    await verifyBoth(widget,'initial');
    for(const id of ids) {const initial=await geometry(widget,id);equal(Number(initial.attributes.offset),0,`${id}: legacy curve placement must be rendered through the center`);}
    const legacyOrbit=await config(widget);
    legacyOrbit.state.views.full.edges[varianceID('A')]={residual_angle:35,residual_offset:.8};
    legacyOrbit.state.views.full.edges[varianceID('F')]={residual_angle:-35,residual_offset:-.7};
    await importConfig(widget,legacyOrbit,'legacy-offset.json');
    await widget.getByRole('status').filter({hasText:'Configuration imported'}).waitFor();
    await verifyBoth(widget,'legacy offset imported');
    await widget.locator('[data-action="edit"]').click();
    for(const id of ids) {
      for(const part of ['shaft','control']) report.circles.push(await circle(page,widget,id,part));
      await radial(page,widget,id,'shaft');await radial(page,widget,id,'control');
      await rotateLabel(page,widget,id);
      equal(await config(other),otherBaseline,`${id}: orbit editing does not change the other widget`);
    }
    const stored=await config(widget),beforeMove=await geometry(widget,'A');
    const node=widget.locator('[data-node-id="node:A"] .ld-shape'),box=await node.boundingBox();
    await page.mouse.move(box.x+box.width/2,box.y+box.height/2);await page.mouse.down();
    await moveModel(page,widget,{x:beforeMove.node.x+35,y:beforeMove.node.y+20},6);await page.mouse.up();
    const afterMove=await geometry(widget,'A'),movedConfig=await config(widget);
    equal(movedConfig.state.views.full.edges[varianceID('A')],stored.state.views.full.edges[varianceID('A')],'Moving a node preserves orbit angle/offset');
    check(Math.abs((afterMove.tail.x-beforeMove.tail.x)-(afterMove.node.x-beforeMove.node.x))<.04 && Math.abs((afterMove.tail.y-beforeMove.tail.y)-(afterMove.node.y-beforeMove.node.y))<.04,'Residual tail translates with its node');
    await verifyBoth(widget,'node moved');
    for(const scale of ['unstandardized','stdyx']) {
      await widget.getByLabel('Scale',{exact:true}).selectOption(scale);equal((await config(widget)).state.views,movedConfig.state.views,'Scale preserves both orbit layouts');await verifyBoth(widget,scale);
    }
    await widget.getByLabel('View',{exact:true}).selectOption('simplified');equal(await widget.locator('.ld-variance').count(),0,'Simplified projection hides residual decorations');
    await widget.getByLabel('View',{exact:true}).selectOption('full');equal((await config(widget)).state.views,movedConfig.state.views,'Returning to full restores the orbit');await verifyBoth(widget,'restored full');
    await widget.getByLabel('Residuals',{exact:true}).uncheck();
    const direct=await widget.locator('[data-edge-id="covariance:A:F"]').evaluate(g=>({from:g.dataset.fromDock,to:g.dataset.toDock}));equal(direct,{from:'node',to:'node'},'Residuals-off covariance docks to variable nodes');
    await widget.getByLabel('Residuals',{exact:true}).check();await verifyBoth(widget,'residuals restored');
    const exportedState=await config(widget),download=page.waitForEvent('download');await widget.locator('[data-action="export-config"]').click();
    const file=await(await download).path(),downloaded=JSON.parse(fs.readFileSync(file,'utf8'));equal(downloaded,exportedState,'Real downloaded config preserves orbit');
    await rotateLabel(page,widget,'A');await importConfig(widget,downloaded);await widget.getByRole('status').filter({hasText:'Configuration imported'}).waitFor();
    equal(await config(widget),exportedState,'Config import restores exported orbit exactly');await verifyBoth(widget,'config roundtrip');
    const invalid=structuredClone(exportedState);invalid.state.views.full.edges[varianceID('A')].residual_offset=.4;delete invalid.state.views.full.edges[varianceID('A')].residual_angle;
    await importConfig(widget,invalid,'invalid-offset.json');await widget.getByRole('status').filter({hasText:'Import rejected'}).waitFor();equal(await config(widget),exportedState,'Offset without angle is rejected without changing diagram');
    equal(await config(other),otherBaseline,'Other widget still matches its embedded baseline');
    if(process.env.MPLUS_DIAGRAM_HTML) {
      await page.goto(pathToFileURL(path.resolve(process.env.MPLUS_DIAGRAM_HTML)).href);
      const real=page.locator('.mplus-diagram').first();await real.locator('.ld-node').first().waitFor();await real.getByLabel('Residuals',{exact:true}).check();
      const arrows=await real.locator('.ld-variance').evaluateAll(gs=>gs.map(g=>({length:g.querySelector('.ld-line').getTotalLength(),markerStart:g.querySelector('.ld-line').getAttribute('marker-start')})));
      check(arrows.length>0,'Real report has visible variance arrows');check(arrows.every(a=>Math.abs(a.length-arrowLength)<.03 && !a.markerStart),'Real report variance arrows also have fixed length and one head');report.realReport=process.env.MPLUS_DIAGRAM_HTML;
    }
    equal(errors,[],'No JavaScript errors');equal(network,[],'No external network requests');report.assertions=assertions;report.status='passed';
    console.log(JSON.stringify(report));
  } catch(error) {
    report.status='failed';report.assertions=assertions;report.error=error.stack;report.activeCircle=activeCircle;
    try {report.pointerEvents=await page.evaluate(()=>window.orbitPointerEvents || []);} catch(_) {}
    if(process.env.MPLUS_ORBIT_RESULTS) {
      try {const screenshot=process.env.MPLUS_ORBIT_RESULTS.replace(/\.json$/,'')+'.failure.png';await page.screenshot({path:screenshot});report.failureScreenshot=screenshot;} catch(_) {}
    }
    throw error;
  }
  finally {if(process.env.MPLUS_ORBIT_RESULTS)fs.writeFileSync(process.env.MPLUS_ORBIT_RESULTS,JSON.stringify(report,null,2)+'\n');await browser.close();}
})().catch(e=>{console.error(e);process.exitCode=1;});
