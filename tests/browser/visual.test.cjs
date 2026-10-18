const {chromium}=require('playwright');
const fs=require('node:fs'),path=require('node:path'),assert=require('node:assert/strict');
const {pathToFileURL}=require('node:url');
const arrowLength=64*2/3;
async function state(widget){
 return widget.evaluate(async root=>{
  const make=URL.createObjectURL,click=HTMLAnchorElement.prototype.click;let blob;
  try{URL.createObjectURL=b=>{blob=b;return make(b);};HTMLAnchorElement.prototype.click=function(){};
   root.querySelector('[data-action="export-config"]').click();
  }finally{URL.createObjectURL=make;HTMLAnchorElement.prototype.click=click;}
  return JSON.parse(await blob.text());
 });
}
function assertNoClippingOrLabels(view, name){
 assert.deepEqual(view.clipped,[],name+' no canvas clipping');
 assert.deepEqual(view.labelOverlaps,[],name+' no coefficient-label overlap');
 assert.deepEqual(view.residualCurveNodeIntersections,[],name+' docked residual covariance does not pass through any node interior');
}
function assertCubicBend(view, config, viewName, name){
 for(const curve of view.cubicCurves){
  const override=config.state.views[viewName].edges[curve.id];
  const configured=override && Object.prototype.hasOwnProperty.call(override,'curve') ? override.curve : 55;
  assert.equal(configured,55,name+' default covariance bend is 55');
  assert.ok(Math.abs(curve.bowRatio-.15)<1e-6,name+' default sagitta is 15 percent of endpoint span');
  assert.ok(Math.abs(curve.chordDeflection)<.001,name+' bow is perpendicular to its chord');
  const angle=Math.atan2(4*.15,1-4*.15*.15)*180/Math.PI;
  assert.ok(Math.abs(curve.startTangentDegrees-angle)<.01,name+' start tangent is about 33.4 degrees from chord');
  assert.ok(Math.abs(curve.endTangentDegrees-angle)<.01,name+' end tangent is about 33.4 degrees from chord');
  if(curve.kind==='cfa-covariance')assert.ok(curve.midpointDeflection.x<0,name+' CFA bow is to the left');
 }
}
async function captureResidualOrbit(page, widget, dest){
 const id='variance:Y1:Y1',before=await state(widget);
 await widget.locator('[data-action="edit"]').click();
 const pose=await widget.evaluate(root=>{
  const edge=root.querySelector('[data-edge-id="variance:Y1:Y1"]'),line=edge.querySelector('.ld-line');
  const node=root.querySelector('[data-node-id="node:Y1"]');
  const p=line.getPointAtLength(line.getTotalLength()*.45),screen=new DOMPoint(p.x,p.y).matrixTransform(line.getScreenCTM());
  const center=new DOMPoint(0,0).matrixTransform(node.getScreenCTM());
  return{start:{x:screen.x,y:screen.y},center:{x:center.x,y:center.y},label:edge.querySelector('.ld-edge-text').textContent};
 });
 const angle=Math.atan2(pose.start.y-pose.center.y,pose.start.x-pose.center.x),radius=Math.hypot(pose.start.x-pose.center.x,pose.start.y-pose.center.y);
 const began=performance.now();await page.mouse.move(pose.start.x,pose.start.y);await page.mouse.down();
 for(let step=1;step<=12;step++){
  const a=angle-Math.PI/2*step/12;
  await page.mouse.move(pose.center.x+radius*Math.cos(a),pose.center.y+radius*Math.sin(a));
 }
 await page.mouse.up();const gestureMs=performance.now()-began;
 await widget.locator('[data-action="edit"]').click();
 const after=await state(widget),override=after.state.views.full.edges[id];
 assert.ok(override && Math.abs(override.residual_angle+90)<1,'actual Y1 pointer orbit reaches upper side');
 assert.ok(Math.abs(override.residual_offset)<=.95,'normalized lateral offset');
 assert.deepEqual(after.state.views.full.nodes,before.state.views.full.nodes,'residual orbit preserves every full-view body pose');
 assert.deepEqual(after.state.views.simplified,before.state.views.simplified,'residual orbit preserves the other view');
 const geometry=await widget.evaluate(root=>{
  const e=root.querySelector('[data-edge-id="variance:Y1:Y1"]'),line=e.querySelector('.ld-line'),label=e.querySelector('.ld-edge-text'),b=label.getBBox();
  const tail=line.getPointAtLength(0),head=line.getPointAtLength(line.getTotalLength());
  const node=root.querySelector('[data-node-id="node:Y1"]'),box=node.querySelector('.ld-shape').getBBox(),m=node.transform.baseVal.consolidate().matrix;
  return{length:line.getTotalLength(),tail:{x:tail.x,y:tail.y},head:{x:head.x,y:head.y},
   label:label.textContent,labelBox:{x:b.x,y:b.y,width:b.width,height:b.height},
   nodeBox:{x:box.x+m.e,y:box.y+m.f,width:box.width,height:box.height}};
 });
 assert.ok(Math.abs(geometry.length-arrowLength)<.001,'orbited residual keeps two-thirds length');
 assert.ok(Math.abs(geometry.head.y-geometry.nodeBox.y)<.1,'orbited arrowhead remains on Y1 upper boundary');
 assert.equal(geometry.label,pose.label,'parameter value is unchanged by orbit');
 const u={x:(geometry.tail.x-geometry.head.x)/arrowLength,y:(geometry.tail.y-geometry.head.y)/arrowLength},b=geometry.labelBox;
 geometry.labelOutwardGap=Math.min(...[{x:b.x,y:b.y},{x:b.x+b.width,y:b.y},{x:b.x,y:b.y+b.height},{x:b.x+b.width,y:b.y+b.height}].map(p=>(p.x-geometry.tail.x)*u.x+(p.y-geometry.tail.y)*u.y));
 assert.ok(geometry.labelOutwardGap>0,'bound value box follows the outer tail');
 const inspection=await inspectBounds(widget);assertNoClippingOrLabels(inspection,'orbited CFA residual');
 await widget.screenshot({path:path.join(dest,'cfa-residual-orbit-edited.png')});
 const configDownload=page.waitForEvent('download');await widget.locator('[data-action="export-config"]').click();
 const downloaded=await configDownload;const configPath=path.join(dest,'cfa-residual-orbit-edited.diagram.json');
 await downloaded.saveAs(configPath);
 assert.deepEqual(JSON.parse(fs.readFileSync(configPath,'utf8')),after,'actual browser download matches orbited state');
 return{model:'cfa',relationship:id,method:'Actual pointer drag on variance shaft; 12-step quarter-turn; no injected configuration',
  gestureMs:Math.round(gestureMs),override,geometry,inspection,bodyPosesUnchanged:true,otherViewUnchanged:true,configDownload:true};
}
async function inspectBounds(widget){
 return widget.evaluate(root=>{
  const svg=root.querySelector('.ld-canvas'),area=svg.getBoundingClientRect(),clipped=[];
  for(const e of svg.querySelectorAll('.ld-node,.ld-edge-text,.ld-heading,.ld-note,.ld-line')){
   const b=e.getBoundingClientRect();if(b.width===0&&b.height===0)continue;
   if(b.left<area.left-1||b.right>area.right+1||b.top<area.top-1||b.bottom>area.bottom+1)
    clipped.push({element:e.getAttribute('data-node-id')||e.closest('[data-edge-id]')?.dataset.edgeId||e.className.baseVal,
     bounds:{left:b.left,top:b.top,right:b.right,bottom:b.bottom}});
  }
  const labels=Array.from(svg.querySelectorAll('.ld-edge-text')).map(e=>({
   id:e.closest('[data-edge-id]').dataset.edgeId,box:e.getBoundingClientRect()})),labelOverlaps=[];
  for(let i=0;i<labels.length;i++)for(let j=i+1;j<labels.length;j++){
   const a=labels[i].box,b=labels[j].box;
   const width=Math.min(a.right,b.right)-Math.max(a.left,b.left),height=Math.min(a.bottom,b.bottom)-Math.max(a.top,b.top);
   if(width>1&&height>1)labelOverlaps.push({first:labels[i].id,second:labels[j].id,width,height});
  }
  const cubicCurves=Array.from(svg.querySelectorAll('.ld-edge[data-geometry="cfa-covariance"],.ld-edge[data-geometry="residual-covariance"]'),e=>{
   const line=e.querySelector('.ld-line'),d=line.getAttribute('d');
   const n=d.match(/[-+]?(?:\d*\.)?\d+(?:e[-+]?\d+)?/gi).map(Number);
   if(!/^M[^C]+C/.test(d)||n.length!==8)throw Error('Expected a single SVG cubic segment for '+e.dataset.edgeId);
   const a={x:n[0],y:n[1]},b={x:n[6],y:n[7]};
   const mid={x:(n[0]+3*n[2]+3*n[4]+n[6])/8,y:(n[1]+3*n[3]+3*n[5]+n[7])/8};
   const span=Math.hypot(b.x-a.x,b.y-a.y),u={x:(b.x-a.x)/span,y:(b.y-a.y)/span};
   const deflection={x:mid.x-(a.x+b.x)/2,y:mid.y-(a.y+b.y)/2};
   const angle=(x,y)=>Math.acos(Math.max(-1,Math.min(1,(x*u.x+y*u.y)/Math.hypot(x,y))))*180/Math.PI;
   return{id:e.dataset.edgeId,kind:e.dataset.geometry,span,path:d,
    midpointDeflection:deflection,bowRatio:Math.hypot(deflection.x,deflection.y)/span,
    chordDeflection:deflection.x*u.x+deflection.y*u.y,
    startTangentDegrees:angle(n[2]-n[0],n[3]-n[1]),endTangentDegrees:angle(n[6]-n[4],n[7]-n[5])};
  });
  const nodeShapes=Array.from(svg.querySelectorAll('.ld-node'),node=>{
   const shape=node.querySelector('.ld-shape'),b=shape.getBBox(),m=node.transform.baseVal.consolidate().matrix;
   return{id:node.dataset.nodeId,shape:shape.tagName.toLowerCase(),x:b.x+m.e,y:b.y+m.f,width:b.width,height:b.height};
  }),residualCurveNodeIntersections=[];
  for(const edge of svg.querySelectorAll('.ld-edge[data-geometry="residual-covariance"]')){
   const line=edge.querySelector('.ld-line'),length=line.getTotalLength();
   for(const node of nodeShapes){
    for(let i=1;i<200;i++){
     const p=line.getPointAtLength(length*i/200);
     const inside=node.shape==='ellipse' ?
      ((p.x-node.x-node.width/2)/(node.width/2))**2+((p.y-node.y-node.height/2)/(node.height/2))**2<.98 :
      p.x>node.x+1&&p.x<node.x+node.width-1&&p.y>node.y+1&&p.y<node.y+node.height-1;
     if(inside){residualCurveNodeIntersections.push({edge:edge.dataset.edgeId,node:node.id,point:{x:p.x,y:p.y}});break;}
    }
   }
  }
  const graphFonts=Array.from(new Set(Array.from(svg.querySelectorAll('.ld-node-text,.ld-edge-text,.ld-heading,.ld-note,.ld-role'),e=>getComputedStyle(e).fontFamily)));
  return {nodes:root.querySelectorAll('.ld-node').length,nodeIDs:Array.from(root.querySelectorAll('.ld-node'),e=>e.dataset.nodeId).sort(),visibleEdges:root.querySelectorAll('.ld-edge').length,clipped,labelOverlaps,cubicCurves,graphFonts,residualCurveNodeIntersections};
 });
}
(async()=>{
 const browser=await chromium.launch({headless:true,executablePath:process.env.CHROME_PATH});
 const context=await browser.newContext({viewport:{width:1250,height:850},acceptDownloads:true});
 await context.setOffline(true);
 const source=process.env.MPLUS_DIAGRAM_GALLERY||'/tmp/mplus-diagram-gallery';
 const dest=path.resolve(process.env.MPLUS_VISUAL_ARTIFACTS||path.join(source,'visual-artifacts'));fs.mkdirSync(dest,{recursive:true});
 const manifest=JSON.parse(fs.readFileSync(path.join(source,'review-manifest.json'),'utf8'));
 const measurements=[],residualComparisons=[],residualOrbitExamples=[];
 try{
  for(const name of ['cfa','sem','path','mediation','clpm','ri_explicit','ri_hat','growth']){
   const page=await context.newPage(),errors=[],network=[];
   page.on('pageerror',e=>errors.push(e.message));
   page.on('request',r=>{if(/^https?:/.test(r.url()))network.push(r.url());});
   const start=performance.now();await page.goto(pathToFileURL(path.join(source,name+'.html')).href);
   const w=page.locator('.mplus-diagram');await w.locator('.ld-node').first().waitFor();
   const renderMs=performance.now()-start;
   const full=await inspectBounds(w);
   await w.screenshot({path:path.join(dest,name+'-full.png')});
   await w.getByLabel('View',{exact:true}).selectOption('simplified');
   const simplified=await inspectBounds(w);
   await w.screenshot({path:path.join(dest,name+'-simplified.png')});
   await w.getByLabel('View',{exact:true}).selectOption('full');
   const before=await state(w);
   assertNoClippingOrLabels(full,name+' full');assertNoClippingOrLabels(simplified,name+' simplified');
   assertCubicBend(full,before,'full',name+' full');assertCubicBend(simplified,before,'simplified',name+' simplified');
   for(const font of full.graphFonts)assert.match(font,/Times New Roman/i,name+' graph font');
   await w.locator('[data-action="edit"]').click();
   const node=w.locator('.ld-node').first(),id=await node.getAttribute('data-node-id');
   const b=await node.boundingBox(),dragStart=performance.now();
   await page.mouse.move(b.x+b.width/2,b.y+b.height/2);await page.mouse.down();
   await page.mouse.move(b.x+b.width/2+30,b.y+b.height/2+25,{steps:10});await page.mouse.up();
   const dragMs=performance.now()-dragStart,afterDrag=await state(w);
   assert.notDeepEqual(afterDrag.state.views.full.nodes[id],before.state.views.full.nodes[id],`${name} drag must change position`);
   if(name==='sem'){
    await node.click();await w.getByLabel('Display name',{exact:true}).fill('中文因子显示名称');
    await w.locator('[data-action="alias"]').click();
   }
   await w.locator('[data-action="edit"]').click();
   const edited=await inspectBounds(w),saved=await state(w);
   assertNoClippingOrLabels(edited,name+' edited');assertCubicBend(edited,saved,'full',name+' edited');
   await w.screenshot({path:path.join(dest,name+'-edited.png')});
   fs.writeFileSync(path.join(dest,name+'-edited.diagram.json'),JSON.stringify(saved,null,2)+'\n');
   const exportStart=performance.now(),download=page.waitForEvent('download');
   await w.locator('[data-action="export-svg"]').click();await(await download).path();const svgMs=performance.now()-exportStart;
   const pngStart=performance.now(),pngDownload=page.waitForEvent('download');
   await w.locator('[data-action="export-png"]').click();await(await pngDownload).path();const pngMs=performance.now()-pngStart;
   assert.deepEqual(errors,[],name+' JavaScript errors');assert.deepEqual(network,[],name+' external requests');
   measurements.push({model:name,source:manifest.models[name],full,simplified,edited,
    changedNode:id,beforePosition:before.state.views.full.nodes[id],afterPosition:saved.state.views.full.nodes[id],
    chineseAlias:name==='sem'?saved.state.aliases[id]:null,
    navigationAndRenderMs:Math.round(renderMs),tenStepDragMs:Math.round(dragMs),
    svgDownloadMs:Math.round(svgMs),png2xDownloadMs:Math.round(pngMs),errors,network});
   console.log(name+': full/simplified/edited captured');await page.close();
  }
  for(const name of ['cfa_residuals','clpm_residuals']){
   const page=await context.newPage(),errors=[],network=[];
   page.on('pageerror',e=>errors.push(e.message));
   page.on('request',r=>{if(/^https?:/.test(r.url()))network.push(r.url());});
   const start=performance.now();await page.goto(pathToFileURL(path.join(source,name+'.html')).href);
   const w=page.locator('.mplus-diagram');await w.locator('.ld-node').first().waitFor();
   const renderMs=performance.now()-start,full=await inspectBounds(w),cfg=await state(w);
   assert.equal(cfg.state.options.residuals,true,name+' residual switch enabled');
   assertNoClippingOrLabels(full,name);assertCubicBend(full,cfg,'full',name);
   for(const font of full.graphFonts)assert.match(font,/Times New Roman/i,name+' graph font');
   const geometry=await w.evaluate(root=>({
    varianceArrows:Array.from(root.querySelectorAll('.ld-edge[data-edge-type="variance"]'),e=>({
     id:e.dataset.edgeId,kind:e.dataset.geometry,fromDock:e.dataset.fromDock,toDock:e.dataset.toDock,
     tail:{x:Number(e.dataset.tailX),y:Number(e.dataset.tailY)},head:{x:Number(e.dataset.headX),y:Number(e.dataset.headY)},
     length:e.querySelector('.ld-line').getTotalLength(),path:e.querySelector('.ld-line').getAttribute('d'),
     markerStart:e.querySelector('.ld-line').getAttribute('marker-start'),markerEnd:e.querySelector('.ld-line').getAttribute('marker-end'),
     actualTail:(()=>{const p=e.querySelector('.ld-line').getPointAtLength(0);return{x:p.x,y:p.y};})(),
     actualHead:(()=>{const line=e.querySelector('.ld-line'),p=line.getPointAtLength(line.getTotalLength());return{x:p.x,y:p.y};})(),
     label:e.querySelector('.ld-edge-text').textContent,
     labelBox:(()=>{const b=e.querySelector('.ld-edge-text').getBBox();return{x:b.x,y:b.y,width:b.width,height:b.height};})(),
     nodeBoxes:Array.from(root.querySelectorAll('.ld-node'),n=>{const b=n.querySelector('.ld-shape').getBBox(),m=n.transform.baseVal.consolidate().matrix;return{id:n.dataset.nodeId,shape:n.querySelector('.ld-shape').tagName.toLowerCase(),x:b.x+m.e,y:b.y+m.f,width:b.width,height:b.height};})})),
    dockedCovariances:Array.from(root.querySelectorAll('.ld-edge[data-geometry="residual-covariance"]'),e=>({
     id:e.dataset.edgeId,fromDock:e.dataset.fromDock,toDock:e.dataset.toDock,
     fromVariance:e.dataset.fromVarianceId,toVariance:e.dataset.toVarianceId,
     actualFrom:(()=>{const p=e.querySelector('.ld-line').getPointAtLength(0);return{x:p.x,y:p.y};})(),
     actualTo:(()=>{const line=e.querySelector('.ld-line'),p=line.getPointAtLength(line.getTotalLength());return{x:p.x,y:p.y};})()}))
   }));
   const baseline=measurements.find(m=>m.model===name.replace('_residuals',''));
   assert.deepEqual(full.nodeIDs,baseline.full.nodeIDs,name+' statistical node identities unchanged');
   assert.ok(geometry.varianceArrows.length>0,name+' short variance arrows present');
   const near=(a,b)=>Math.hypot(a.x-b.x,a.y-b.y)<.001;
   for(const arrow of geometry.varianceArrows){
    assert.ok(arrow.markerEnd && !arrow.markerStart,arrow.id+' exactly one inward arrowhead');
    assert.deepEqual(arrow.path.match(/[MLCQSAHVTZ]/gi),['M','L'],arrow.id+' straight short arrow without self-loop');
    assert.ok(near(arrow.actualTail,arrow.tail) && near(arrow.actualHead,arrow.head),arrow.id+' actual path matches declared tail/head');
    const owner=arrow.nodeBoxes.find(node=>node.id==='node:'+arrow.id.split(':')[2]);
    assert.ok(owner,arrow.id+' original variance node exists');
    const hx=arrow.actualHead.x-owner.x-owner.width/2,hy=arrow.actualHead.y-owner.y-owner.height/2;
    const onBoundary=owner.shape==='ellipse' ? Math.abs(hx*hx/(owner.width/2)**2+hy*hy/(owner.height/2)**2-1)<.03 :
     Math.abs(hx)<=owner.width/2+1&&Math.abs(hy)<=owner.height/2+1&&Math.min(Math.abs(Math.abs(hx)-owner.width/2),Math.abs(Math.abs(hy)-owner.height/2))<1;
    assert.ok(onBoundary,arrow.id+' actual arrowhead sits on owning node boundary');

    assert.ok(Math.abs(arrow.length-Math.hypot(arrow.tail.x-arrow.head.x,arrow.tail.y-arrow.head.y))<.001,arrow.id+' straight-line length');
    const dx=arrow.actualTail.x-arrow.actualHead.x,dy=arrow.actualTail.y-arrow.actualHead.y,len=Math.hypot(dx,dy),u={x:dx/len,y:dy/len};
    arrow.centerLineError=Math.abs(hx*u.y-hy*u.x);
    assert.ok(arrow.centerLineError<.001,arrow.id+' axis passes through owning node center');
    assert.ok(hx*u.x+hy*u.y>0,arrow.id+' arrow points inward toward center');
    const box=arrow.labelBox,corners=[{x:box.x,y:box.y},{x:box.x+box.width,y:box.y},{x:box.x,y:box.y+box.height},{x:box.x+box.width,y:box.y+box.height}];
    arrow.labelOutwardGap=Math.min(...corners.map(p=>(p.x-arrow.actualTail.x)*u.x+(p.y-arrow.actualTail.y)*u.y));
    assert.ok(arrow.labelOutwardGap>0,arrow.id+' entire value box is beyond outer tail, never on shaft');
    arrow.overlappingNodes=arrow.nodeBoxes.filter(node=>Math.min(box.x+box.width,node.x+node.width)>Math.max(box.x,node.x)&&Math.min(box.y+box.height,node.y+node.height)>Math.max(box.y,node.y)).map(node=>node.id);
    assert.deepEqual(arrow.overlappingNodes,[],arrow.id+' value box does not overlap any node');

   }
   const byID=Object.fromEntries(geometry.varianceArrows.map(arrow=>[arrow.id,arrow]));
   for(const covariance of geometry.dockedCovariances){
    for(const side of ['from','to'])if(covariance[side+'Dock']==='residual-tail'){
     const arrow=byID[covariance[side+'Variance']];
     assert.ok(arrow,covariance.id+' visible '+side+' residual arrow');
     assert.ok(near(covariance[side==='from'?'actualFrom':'actualTo'],arrow.actualTail),covariance.id+' '+side+' endpoint on actual residual-arrow tail');
    }
   }
   if(name==='clpm_residuals')assert.ok(geometry.dockedCovariances.length>0,'CLPM residual covariance docks present');
   await w.screenshot({path:path.join(dest,name.replace('_','-')+'-on.png')});
   assert.deepEqual(errors,[],name+' JavaScript errors');assert.deepEqual(network,[],name+' external requests');
   residualComparisons.push({model:name,source:manifest.models[name],full,...geometry,
    navigationAndRenderMs:Math.round(renderMs),errors,network});
   if(name==='cfa_residuals')residualOrbitExamples.push(await captureResidualOrbit(page,w,dest));
   console.log(name+': residual comparison captured');await page.close();
  }
  const bends=measurements.flatMap(m=>m.full.cubicCurves).concat(residualComparisons.flatMap(m=>m.full.cubicCurves));
  assert.ok(bends.some(c=>c.kind==='cfa-covariance') && bends.some(c=>c.kind==='residual-covariance'),'CFA and residual cubic families measured');
  assert.ok(new Set(bends.map(c=>Math.round(c.span*1000)/1000)).size>1,'same 15-percent default bend ratio verified across distinct spans');
  fs.writeFileSync(path.join(dest,'measurements.json'),JSON.stringify({
   note:'One offline headless Edge run. Edited images follow actual automated pointer drags, not human approval. Timings include Playwright scheduling and local resource loading.',
   browser:browser.version(),executable:process.env.CHROME_PATH,platform:process.platform,
   sourceCommit:manifest.commit,runtimeDirty:Boolean(manifest.dirty_runtime_files&&manifest.dirty_runtime_files.length),dirtyRuntimeFiles:manifest.dirty_runtime_files,runtimeSourceHashes:manifest.runtime_source_hashes,packagePath:manifest.package_path,packageVersion:manifest.package_version,
   mplusAutomationVersion:manifest.mplus_automation_version,verifiedFunctionBodies:manifest.verified_function_bodies,
   installedWidgetHashes:manifest.installed_widget_hashes,measuredAt:new Date().toISOString(),measurements,residualComparisons,residualOrbitExamples},null,2)+'\n');
 }finally{await browser.close();}
})().catch(e=>{console.error(e);process.exitCode=1;});
