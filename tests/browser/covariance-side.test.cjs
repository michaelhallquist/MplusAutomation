// Synthetic rendering contract; real pointer gestures, not a Mplus fit.
const {chromium}=require('playwright');
const assert=require('node:assert/strict'),fs=require('node:fs'),path=require('node:path');
const repo=path.resolve(__dirname,'../..');
const options={scale:'stdyx',digits:2,residuals:false,default_covariances:true,parameter_labels:false,values:true,fixed_values:true,constraints:false,view:'full',stars:true,nonsignificant_dotted:true};
function fixture(horizontal=false) {
  const names=['A','B','C','Y1','Y2','Y3'];
  const nodes=names.map((name,i)=>({id:'node:'+name,name,role:name==='B'?'latent':'observed',endogenous:i>=3}));
  const edge=(type,a,b,extra={})=>({id:`${type}:${a}:${b}`,type,from:'node:'+a,to:'node:'+b,fixed:false,origin:'explicit',...extra});
  const edges=[edge('regression','A','Y1'),edge('regression','B','Y2'),edge('regression','C','Y3'),
    edge('covariance','A','B'),edge('covariance','B','C'),edge('covariance','A','C'),
    edge('covariance','Y1','Y3',{residual_from:true,residual_to:true}),
    edge('variance','Y1','Y1',{residual:true}),edge('variance','Y3','Y3',{residual:true})];
  const coords=Object.fromEntries(names.map((name,i)=>['node:'+name,horizontal?
    {x:160+(i%3)*300,y:i<3?200:650}:{x:i<3?200:650,y:140+(i%3)*300}]));
  const view={nodes:coords,edges:{'covariance:A:B':{curve:73}},viewport:{x:0,y:0,k:1}};
  return {schema_version:1,model:{structure:{family:'sem',nodes,edges,constraints:[],mapping:{}},
    results:Object.fromEntries(edges.map(e=>[e.id,{stdyx:{estimate:.24,p:.002},unstandardized:{estimate:.36,p:.003}}])),
    diagnostics:[],provenance:{},fingerprint:'covariance-side-fixture'},
    state:{options,aliases:{},hidden_edges:[],views:{full:structuredClone(view),simplified:structuredClone(view)}},
    automatic:{full:structuredClone(view),simplified:structuredClone(view)},editor:null};
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
    const e=root.querySelector(`[data-edge-id="${id}"]`),p=e.querySelector('.ld-line'),length=p.getTotalLength();
    const point=t=>{const q=p.getPointAtLength(length*t);return {x:q.x,y:q.y};};
    const nodes=id.split(':').slice(1).map(name=>{
      const n=root.querySelector(`[data-node-id="node:${name}"]`),b=n.querySelector('.ld-shape').getBBox(),m=n.transform.baseVal.consolidate().matrix;
      return {x:m.e,y:m.f,w:b.width,h:b.height};
    });
    const a=point(0),b=point(1),mid=point(.5),dx=b.x-a.x,dy=b.y-a.y,span=Math.hypot(dx,dy);
    return {a,b,mid,nodes,path:p.getAttribute('d'),side:e.dataset.connectionSide,fromDock:e.dataset.fromDock,toDock:e.dataset.toDock,
      ratio:Math.abs((mid.x-(a.x+b.x)/2)*dy-(mid.y-(a.y+b.y)/2)*dx)/(span*span),text:e.querySelector('.ld-edge-text').textContent};
  },id);
}
function ports(g,side) {
  for(const [i,p] of [g.a,g.b].entries()) {
    const n=g.nodes[i],expected={left:[n.x-n.w/2,n.y],right:[n.x+n.w/2,n.y],top:[n.x,n.y-n.h/2],bottom:[n.x,n.y+n.h/2]}[side];
    assert.ok(Math.hypot(p.x-expected[0],p.y-expected[1])<.01,`${side} endpoint on side midpoint`);
  }
}
async function selectEdge(page,widget,id) {
  const p=await widget.locator(`[data-edge-id="${id}"] .ld-line`).evaluate(line=>{
    const id=line.closest('.ld-edge').dataset.edgeId;
    for (const t of [.22,.3,.7,.78,.4,.6]) {
      const p=line.getPointAtLength(line.getTotalLength()*t).matrixTransform(line.getScreenCTM());
      const hit=document.elementFromPoint(p.x,p.y);
      if (hit && hit.closest('.ld-edge')?.dataset.edgeId===id) return{x:p.x,y:p.y};
    }
    throw Error('No reachable painted point on '+id);
  });
  await page.mouse.click(p.x,p.y);
  assert.equal(await widget.locator('.ld-handle').count(),1,'Only one visible curve handle');
  assert.equal(await widget.locator(`[data-edge-id="${id}"].ld-selected`).count(),1);
}
async function switchSide(page,widget,id,nodeID,side) {
  await selectEdge(page,widget,id);
  const points=await widget.evaluate((root,{id,nodeID,side})=>{
    const hit=root.querySelector(`[data-covariance-id="${id}"][data-node-id-ref="${nodeID}"]`);
    const r=hit.getBoundingClientRect(),n=root.querySelector(`[data-node-id="${nodeID}"]`),b=n.querySelector('.ld-shape').getBBox();
    const target={left:[-b.width/2-30,0],right:[b.width/2+30,0],top:[0,-b.height/2-30],bottom:[0,b.height/2+30]}[side];
    const screen=new DOMPoint(...target).matrixTransform(n.getScreenCTM());
    return {start:{x:r.x+r.width/2,y:r.y+r.height/2},end:{x:screen.x,y:screen.y}};
  },{id,nodeID,side});
  await page.mouse.move(points.start.x,points.start.y);await page.mouse.down();
  const actual=await page.evaluate(()=>window.lastDown);
  assert.deepEqual(actual,{id,node:nodeID,trusted:true},'Actual arrowhead receives trusted pointerdown, including shared endpoints');
  await page.mouse.move(points.end.x,points.end.y,{steps:10});await page.mouse.up();
  assert.equal(await widget.locator('.ld-handle').count(),1);
}
(async()=>{
  const browser=await chromium.launch({headless:true,...(process.env.CHROME_PATH?{executablePath:process.env.CHROME_PATH}:{})});
  try {
    const context=await browser.newContext({viewport:{width:1500,height:1200},acceptDownloads:true});await context.setOffline(true);
    const page=await context.newPage(),errors=[];page.on('pageerror',e=>errors.push(e.message));
    await page.setContent('<div id="one" style="width:1200px;height:1000px"></div><div id="two" style="width:1200px;height:1000px"></div>');
    await page.addStyleTag({content:fs.readFileSync(path.join(repo,'inst/htmlwidgets/lib/mplus-diagram/mplus-diagram.css'),'utf8')});
    await page.evaluate(()=>{window.HTMLWidgets={widget:b=>window.binding=b};document.addEventListener('pointerdown',e=>{
      window.lastDown={id:e.target.dataset.covarianceId,node:e.target.dataset.nodeIdRef,trusted:e.isTrusted};
    },true);});
    await page.addScriptTag({content:fs.readFileSync(path.join(repo,'inst/htmlwidgets/mplus_diagram.js'),'utf8')});
    await page.evaluate(data=>{window.instances=['one','two'].map(id=>{const w=binding.factory(document.getElementById(id),1200,1000);w.renderValue(data);return w;});},fixture());
    const one=page.locator('#one'),two=page.locator('#two'),baseline=await config(one),other=await config(two),id='covariance:A:B';
    for(const key of [id,'covariance:B:C','covariance:A:C']) ports(await geometry(one,key),'left');
    const outer=await geometry(one,'covariance:A:C'),inner=await geometry(one,'covariance:B:C');
    assert.ok(outer.mid.x<inner.mid.x,'Long-span correlation nests outside short arcs');
    const before=await geometry(one,id);assert.ok(Math.abs(before.ratio-.15*73/55)<.001);
    await one.locator('[data-action="edit"]').click();
    await switchSide(page,one,id,'node:A','right');
    const after=await geometry(one,id),saved=await config(one);ports(after,'right');
    assert.ok(Math.abs(before.ratio-after.ratio)<.001,'Side switch preserves normalized curvature');
    assert.equal(after.text,before.text);assert.equal(saved.state.views.full.edges[id].curve,73);
    assert.deepEqual(saved.state.views.full.nodes,baseline.state.views.full.nodes);
    assert.deepEqual(saved.state.views.simplified,baseline.state.views.simplified);assert.deepEqual(await config(two),other);
    await one.locator('[data-action="undo"]').click();assert.deepEqual(await config(one),baseline);
    await one.locator('[data-action="redo"]').click();assert.deepEqual(await config(one),saved);
    // The existing single handle still changes bend without moving side ports.
    const handle=one.locator('.ld-handle'),box=await handle.boundingBox();
    await page.mouse.move(box.x+box.width/2,box.y+box.height/2);await page.mouse.down();
    await page.mouse.move(box.x+box.width/2+35,box.y+box.height/2,{steps:8});await page.mouse.up();
    ports(await geometry(one,id),'right');assert.notEqual((await config(one)).state.views.full.edges[id].curve,73);
    await one.locator('[data-action="undo"]').click();assert.deepEqual(await config(one),saved);
    await switchSide(page,one,id,'node:B','left');ports(await geometry(one,id),'left');
    await one.locator('[data-action="undo"]').click();assert.deepEqual(await config(one),saved);
    const pending=page.waitForEvent('download');await one.locator('[data-action="export-config"]').click();
    const file=await(await pending).path();assert.deepEqual(JSON.parse(fs.readFileSync(file,'utf8')),saved);
    await one.locator('[data-action="restore"]').click();
    await one.getByLabel('Import configuration file').setInputFiles(file);await one.getByRole('status').filter({hasText:'Configuration imported'}).waitFor();assert.deepEqual(await config(one),saved);
    const invalid=structuredClone(saved);invalid.state.views.full.edges['regression:A:Y1']={covariance_side:'left'};
    await one.getByLabel('Import configuration file').setInputFiles({name:'invalid.json',mimeType:'application/json',buffer:Buffer.from(JSON.stringify(invalid))});
    await one.getByRole('status').filter({hasText:'Import rejected'}).waitFor();assert.deepEqual(await config(one),saved);
    const svgDownload=page.waitForEvent('download');await one.locator('[data-action="export-svg"]').click();
    const svg=fs.readFileSync(await(await svgDownload).path(),'utf8');assert.doesNotMatch(svg,/ld-covariance-endpoint|data-editor-only/);
    // Visible residual covariances stay attached when their heads switch sides.
    await one.getByLabel('Residuals',{exact:true}).check();const residualID='covariance:Y1:Y3';
    const residualBefore=await config(one);await switchSide(page,one,residualID,'node:Y1','left');
    const docked=await geometry(one,residualID),residualAfter=await config(one);
    assert.equal(docked.fromDock,'residual-tail');assert.equal(docked.toDock,'residual-tail');
    for(const [i,name] of ['Y1','Y3'].entries()) {
      const variance=one.locator(`[data-edge-id="variance:${name}:${name}"]`);
      const pose=await variance.evaluate(g=>({x:Number(g.dataset.tailX),y:Number(g.dataset.tailY),length:g.querySelector('.ld-line').getTotalLength()}));
      assert.ok(Math.abs(pose.length-128/3)<.01);assert.ok(pose.x<docked.nodes[i].x-docked.nodes[i].w/2);
      assert.ok(Math.hypot(pose.x-[docked.a,docked.b][i].x,pose.y-[docked.a,docked.b][i].y)<.01);
    }
    await one.locator('[data-action="undo"]').click();assert.deepEqual(await config(one),residualBefore);
    await one.locator('[data-action="redo"]').click();assert.deepEqual(await config(one),residualAfter);
    await one.getByLabel('Residuals',{exact:true}).uncheck();ports(await geometry(one,residualID),'left');
    // Horizontal pairs use top/bottom instead of awkward inline side ports.
    await page.evaluate(data=>instances[0].renderValue(data),fixture(true));
    ports(await geometry(one,id),'top');await one.locator('[data-action="edit"]').click();
    await switchSide(page,one,id,'node:A','bottom');ports(await geometry(one,id),'bottom');
    if (process.env.MPLUS_COVARIANCE_HTML) {
      await page.goto(require('node:url').pathToFileURL(process.env.MPLUS_COVARIANCE_HTML).href);
      const real=page.locator('.mplus-diagram').first();await real.locator('.ld-node').first().waitFor();
      await page.evaluate(()=>document.addEventListener('pointerdown',e=>window.lastDown={id:e.target.dataset.covarianceId,node:e.target.dataset.nodeIdRef,trusted:e.isTrusted},true));
      await real.locator('[data-action="edit"]').click();
      await switchSide(page,real,'covariance:F1:F2','node:F1','right');
      ports(await geometry(real,'covariance:F1:F2'),'right');
      const downloaded=page.waitForEvent('download');await real.locator('[data-action="export-config"]').click();
      const file=await downloaded;
      if(process.env.MPLUS_COVARIANCE_CONFIG_EXPORT) await file.saveAs(process.env.MPLUS_COVARIANCE_CONFIG_EXPORT);
      assert.deepEqual(JSON.parse(fs.readFileSync(await file.path(),'utf8')),await config(real));
    }
    if (process.env.MPLUS_COVARIANCE_SEM_HTML) {
      await page.goto(require('node:url').pathToFileURL(process.env.MPLUS_COVARIANCE_SEM_HTML).href);
      const real=page.locator('.mplus-diagram').first();await real.locator('.ld-node').first().waitFor();
      await real.getByLabel('Default covariances',{exact:true}).check();
      ports(await geometry(real,'covariance:F1:F2'),'left');
      const collisions=await real.evaluate(root=>{
        const edge=root.querySelector('[data-edge-id="covariance:F1:F2"]'),p=edge.querySelector('.ld-line');
        const boxes=Array.from(root.querySelectorAll('.ld-node')).filter(n=>!['node:F1','node:F2'].includes(n.dataset.nodeId)).map(n=>{
          const b=n.querySelector('.ld-shape').getBBox(),m=n.transform.baseVal.consolidate().matrix;return {id:n.dataset.nodeId,x:b.x+m.e,y:b.y+m.f,w:b.width,h:b.height};
        });
        const points=Array.from({length:199},(_,i)=>p.getPointAtLength(p.getTotalLength()*(i+1)/200));
        return boxes.filter(b=>points.some(p=>p.x>b.x && p.x<b.x+b.w && p.y>b.y && p.y<b.y+b.h)).map(b=>b.id);
      });
      assert.deepEqual(collisions,[],'Default SEM left arc avoids measurement indicator bodies');
    }
    assert.deepEqual(errors,[]);
    console.log('PASS: SEM left-side covariance, nested arcs, single handle, trusted arrowhead drags, curvature, undo, views, download/import, residual-tail switching and horizontal pairs.');
  } finally {await browser.close();}
})().catch(e=>{console.error(e);process.exitCode=1;});
