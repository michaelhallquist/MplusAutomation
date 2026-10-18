/* Offline acceptance of every real positive fixture, using independently
 * transcribed source tables and the documented full/simplified projections.
 * node tests/browser/model-matrix.test.cjs /path/to/manifest.json
 * Optional CHROME_PATH and MPLUS_MATRIX_RESULTS override executable/result path.
 * This is semantic/interaction acceptance, not human visual approval.
 */
const {chromium} = require('playwright');
const fs = require('node:fs');
const path = require('node:path');
const {pathToFileURL} = require('node:url');
const crypto = require('node:crypto');
const assert = require('node:assert/strict');
const manifestPath = process.argv[2];
if (!manifestPath) throw Error('Supply the rendered model matrix manifest');
const manifest = JSON.parse(fs.readFileSync(manifestPath, 'utf8'));
const resultPath = process.env.MPLUS_MATRIX_RESULTS || path.join(path.dirname(manifestPath), 'browser-results.json');
function sha256(bytes) { return crypto.createHash('sha256').update(bytes).digest('hex'); }
function identity(e) {
  if (e.operator === 'ON') return `regression:${e.rhs}:${e.lhs}`;
  if (e.operator === 'WITH') return ['covariance', ...[e.lhs, e.rhs].sort()].join(':');
  return `loading:${e.lhs}:${e.rhs}`;
}
function ends(e) {
  if (e.operator === 'ON') return [e.rhs, e.lhs];
  if (e.operator === 'WITH') return [e.lhs, e.rhs].sort();
  return [e.lhs, e.rhs];
}
function sourceModel(expected) {
  const roles = Object.fromEntries((expected.observed || []).map(id => [id, 'observed']));
  for (const [id, role] of Object.entries(expected.latent_roles || {})) roles[id] = role === 'factor' ? 'latent' : role;
  const kinds = new Set(Object.values(roles));
  // Classification and projections use the reviewed source card, never the
  // renderer's model payload, DOM, or runtime-produced family label.
  const family = kinds.has('within') && kinds.has('random_intercept') ? 'ri_clpm' :
    kinds.has('growth_intercept') || kinds.has('growth_slope') ? 'growth' :
    expected.waves && !Object.keys(expected.latent_roles || {}).length ? 'clpm' :
    expected.relationships.some(e => e.operator === 'BY') && !expected.relationships.some(e => e.operator === 'ON') ? 'cfa' : 'sem';
  const edges = expected.relationships.map(e => ({...e}));
  const edgeIDs = new Set(edges.map(identity));
  // A source-annotated residual-hat component defines a fixed original-scale
  // within -> observed link; it has no invented STDYX estimate. Enumerating
  // these independently detects missing definitions as well as extra edges.
  for (const [id, role] of Object.entries(roles)) {
    const observed = id.endsWith('^') ? id.slice(0, -1) : null;
    if (role === 'within' && observed && roles[observed] === 'observed') {
      const definition = {lhs:id, rhs:observed, operator:'BY', fixed:true, fixed_value:1,
        unstandardized:1, stdyx:null, unstandardized_p:null, stdyx_p:null, definition:true};
      if (!edgeIDs.has(identity(definition))) edges.push(definition);
    }
  }
  return {roles, edges, family};
}
function projection(source, view) {
  let names = Object.keys(source.roles);
  if (view === 'simplified') {
    if (source.family === 'ri_clpm') {
      names = names.filter(id => source.roles[id] !== 'observed');
    } else if (source.family === 'cfa' || source.family === 'sem') {
      // Factor/structural focus hides only observed measurement-only variables;
      // observed predictors/outcomes remain under their original identities.
      const measured = new Set(source.edges.filter(e => e.operator === 'BY').map(e => e.rhs));
      const structural = new Set(source.edges.filter(e => e.operator === 'ON').flatMap(e => [e.lhs,e.rhs]));
      names = names.filter(id => source.roles[id] !== 'observed' || !measured.has(id) || structural.has(id));
    }
    // CLPM keeps its time grid; growth keeps factors, indicators and time design.
  }
  const retained = new Set(names);
  const edges = source.edges.filter(e => ends(e).every(id => retained.has(id)) &&
    !(e.fixed && e.fixed_value === 0 && e.operator !== '|'));
  return {names, edges};
}
// Independent decimal-string arithmetic: round the printed source value
// half away from zero without sharing the widget's formatting implementation.
function sourceNumber(value, digits, standardised) {
  const [mantissa, exponentText='0'] = String(Math.abs(value)).toLowerCase().split('e');
  const [whole, fraction=''] = mantissa.split('.');
  const coefficient = BigInt(whole + fraction);
  const shift = digits + Number(exponentText) - fraction.length;
  let rounded;
  if (shift >= 0) rounded = coefficient * 10n ** BigInt(shift);
  else {const divisor=10n ** BigInt(-shift); rounded=(coefficient+divisor/2n)/divisor;}
  const padded=rounded.toString().padStart(digits+1,'0');
  let label=digits ? padded.slice(0,-digits)+'.'+padded.slice(-digits) : padded;
  if (standardised && label.startsWith('0.')) label=label.slice(1);
  if (value < 0 && rounded !== 0n) label='\u2212'+label;
  return label;
}
function sourceP(edge, scale) {
  const p=edge[scale+'_p'];
  return typeof p==='number' && Number.isFinite(p) && p>=0 && p<=1 ? p : null;
}
function sourceNonsignificant(edge, scale) {
  const p=sourceP(edge,scale);
  return p!==null && (scale==='stdyx' || edge.fixed===false) && p>=.05;
}
function sourceLabel(edge, scale, view, digits=3, hideNonsignificant=true) {
  if (hideNonsignificant && sourceNonsignificant(edge,scale)) return '';
  if (view==='simplified' && edge.fixed) return '';
  const value=edge[scale]==null ? (scale==='unstandardized' && edge.fixed ? edge.fixed_value : null) : edge[scale];
  if(value==null) return 'NA';
  const p=sourceP(edge,scale);
  // User-approved policy: an actual STDYX p-value applies to that scale even
  // when the original loading was fixed; raw fixed/unknown parameters do not infer.
  const infer=scale==='stdyx' || edge.fixed===false;
  const stars=infer && p!==null ? (p<.001?'***':p<.01?'**':p<.05?'*':'') : '';
  return sourceNumber(value,digits,scale==='stdyx')+stars;
}
function verifyEditedRelations(svg, source, view, scale, label, check) {
  const want=projection(source,view);
  check(() => assert.deepEqual(Object.keys(svg.edges).sort(),want.edges.map(identity).sort(),`${label} edited relationship identities`));
  for (const edge of want.edges) {
    const id=identity(edge), actual=svg.edges[id], [from,to]=ends(edge);
    check(() => assert.equal(actual.text,sourceLabel(edge,scale,view),`${label} ${id} editing preserves source coefficient`));
    check(() => assert.equal(actual.nonsignificant,sourceNonsignificant(edge,scale),`${label} ${id} editing preserves source-p style`));
    check(() => assert.ok(onBoundary(actual.start,svg.nodes[from]),`${label} ${id} edited source anchor`));
    check(() => assert.ok(onBoundary(actual.end,svg.nodes[to]),`${label} ${id} edited target anchor`));
    check(() => assert.ok(actual.markerEnd && actual.markerValid,`${label} ${id} edited arrowhead`));
  }
}
function onBoundary(point, node) {
  const dx = point.x-node.x, dy = point.y-node.y;
  if (node.shape === 'ellipse') return Math.abs(dx*dx/(node.width/2)**2 + dy*dy/(node.height/2)**2 - 1) < .03;
  return Math.abs(dx) <= node.width/2+1 && Math.abs(dy) <= node.height/2+1 &&
    Math.min(Math.abs(Math.abs(dx)-node.width/2), Math.abs(Math.abs(dy)-node.height/2)) < 1;
}
async function inspectSVG(widget) {
  return widget.evaluate(root => {
    const nodes = {}, edges = {};
    for (const node of root.querySelectorAll('.ld-node')) {
      const transform = node.transform.baseVal.consolidate().matrix;
      const shape = node.querySelector('.ld-shape'), box = shape.getBBox();
      nodes[node.dataset.nodeId.slice(5)] = {classes:node.getAttribute('class'), shape:shape.tagName.toLowerCase(),
        x:transform.e, y:transform.f, width:box.width, height:box.height,
        label:node.querySelector('.ld-node-text').textContent,
        roleCaption:node.querySelector('.ld-role') ? node.querySelector('.ld-role').textContent : ''};
    }
    for (const edge of root.querySelectorAll('.ld-edge')) {
      const line = edge.querySelector('.ld-line');
      const first = line.getPointAtLength(0), last = line.getPointAtLength(line.getTotalLength());
      const markerMatch = (line.getAttribute('marker-end') || '').match(/^url\(#(.+)\)$/);
      const marker = markerMatch && document.getElementById(markerMatch[1]);
      edges[edge.dataset.edgeId] = {text:edge.querySelector('.ld-edge-text').textContent,
        fixed:edge.classList.contains('ld-fixed'), nonsignificant:edge.classList.contains('ld-nonsignificant'),
        dasharray:getComputedStyle(line).strokeDasharray, linecap:getComputedStyle(line).strokeLinecap,
        tooltip:edge.querySelector('title').textContent, start:{x:first.x,y:first.y}, end:{x:last.x,y:last.y},
        markerStart:line.getAttribute('marker-start'), markerEnd:line.getAttribute('marker-end'),
        markerValid:Boolean(marker && marker.querySelector('path') && marker.getAttribute('orient') === 'auto-start-reverse')};
    }
    return {nodes,edges,headingCount:root.querySelectorAll('.ld-heading').length,
      scale:root.querySelector('[aria-label="Scale"]').value,view:root.querySelector('[aria-label="View"]').value,
      notes:Array.from(root.querySelectorAll('.ld-note'), e => e.textContent)};
  });
}
async function config(widget) {
  // Read the public exported configuration Blob without triggering bulk-download
  // protection. This observes presentation only; it is not a semantic oracle.
  return widget.evaluate(async root => {
    const makeURL = URL.createObjectURL, click = HTMLAnchorElement.prototype.click;
    let blob;
    try {
      URL.createObjectURL = b => {blob=b; return makeURL(b);};
      HTMLAnchorElement.prototype.click = function(){};
      root.querySelector('[data-action="export-config"]').click();
    } finally { URL.createObjectURL=makeURL; HTMLAnchorElement.prototype.click=click; }
    return JSON.parse(await blob.text());
  });
}
async function dragNode(page, widget, id, dx, dy) {
  await widget.locator('.ld-stage').scrollIntoViewIfNeeded();
  const shape = widget.locator(`[data-node-id="node:${id}"] .ld-shape`);
  const box = await shape.boundingBox();
  assert.ok(box && box.width>0 && box.height>0, `Visible draggable node ${id}`);
  const x=box.x+box.width/2, y=box.y+box.height/2;
  await page.mouse.move(x,y); await page.mouse.down();
  await page.mouse.move(x+dx,y+dy,{steps:5}); await page.mouse.up();
}
(async () => {
  const browser = await chromium.launch({headless:true,
    ...(process.env.CHROME_PATH ? {executablePath:process.env.CHROME_PATH} : {})});
  const context = await browser.newContext({viewport:{width:1500,height:1100}});
  await context.setOffline(true);
  const report = {browser:browser.version(), executable:process.env.CHROME_PATH || 'Playwright Chromium',
    offline:true, manifest:path.resolve(manifestPath), manifest_sha256:sha256(fs.readFileSync(manifestPath)),
    runtime_commit:manifest.runtime_provenance && manifest.runtime_provenance.commit,
    coverage:{views:['full','simplified'],requested_scales:['unstandardized','stdyx'],model_view_scale_cases:0,
      layout_isolation_models:0}, models:[], assertions:0};
  const requiredPairs=['cfa','sem','path','mediation','clpm','ri-explicit','ri-hat','growth']
    .flatMap(family=>['ML','MLR'].map(estimator=>({id:`${family}-${estimator.toLowerCase()}`,estimator})));
  report.coverage.required_fixture_pairs=requiredPairs.map(p=>p.id);
  let failed=false;
  function check(fn) {report.assertions++; fn();}
  try {
    for (const required of requiredPairs) {
      check(() => assert.ok(manifest.models.some(item=>item.id===required.id && item.estimator===required.estimator),
        `Required model/estimator fixture missing: ${required.id}`));
    }
    for (const item of manifest.models) {
      const record = {id:item.id,estimator:item.estimator,family:item.family,scales:[],status:'running'};
      report.models.push(record);
      const page = await context.newPage(), errors=[], network=[];
      page.on('pageerror', e => errors.push(e.message));
      page.on('request', r => {if (/^https?:/.test(r.url())) network.push(r.url());});
      try {
        const sourceBytes=fs.readFileSync(item.expected), expected=JSON.parse(sourceBytes);
        check(() => assert.equal(sha256(sourceBytes),item.expected_sha256,`${item.id} reviewed source hash`));
        const source=sourceModel(expected);
        check(() => assert.equal(item.family,source.family,`${item.id} source-derived family`));
        await page.goto(pathToFileURL(item.html).href);
        const widget=page.locator('.mplus_diagram');
        await widget.locator('.ld-node').first().waitFor();
        const expectedStdyx=expected.relationships.some(e => e.stdyx!=null);
        check(() => assert.equal(item.stdyx_available,expectedStdyx,`${item.id} independent scale availability`));
        const initial=await config(widget);
        check(() => assert.equal(initial.state.options.default_covariances,true,`${item.id} all covariance origins selected`));
        check(() => assert.equal(initial.state.options.residuals,false,`${item.id} residual decorations excluded`));
        check(() => assert.equal(initial.state.options.parameter_labels,false,`${item.id} numeric source-label baseline`));
        check(() => assert.equal(initial.state.options.nonsignificant_hide_values,true,`${item.id} default hides only nonsignificant labels`));
        for (const view of ['full','simplified']) {
          const want=projection(source,view);
          await widget.getByLabel('View',{exact:true}).selectOption(view);
          const positionsByScale=[];
          for (const requested of ['unstandardized','stdyx']) {
            const label=`${item.id} ${view} ${requested}`;
            if (requested==='stdyx' && !expectedStdyx) {
              const scale=widget.getByLabel('Scale',{exact:true});
              const disabled=await scale.locator('option[value="stdyx"]').evaluate(option=>option.disabled);
              check(() => assert.equal(disabled,true,`${label} unavailable STDYX control disabled`));
              const selected=await scale.inputValue();
              check(() => assert.equal(selected,'unstandardized',`${label} fallback state is raw`));
            } else await widget.getByLabel('Scale',{exact:true}).selectOption(requested);
            const actualScale=requested==='stdyx' && expectedStdyx ? 'stdyx' : 'unstandardized';
            const svg=await inspectSVG(widget);
            check(() => assert.equal(svg.scale,actualScale,label));
            check(() => assert.equal(svg.headingCount,0,`${label} no figure heading`));
            check(() => assert.ok(svg.notes.every(note=>note.startsWith('Wave / time ')),`${label} no explanatory legend`));
            check(() => assert.equal(svg.view,view,`${label} selected projection`));
            check(() => assert.deepEqual(Object.keys(svg.nodes).sort(),[...want.names].sort(),`${label} exact source-node projection`));
            check(() => assert.deepEqual(Object.keys(svg.edges).sort(),want.edges.map(identity).sort(),`${label} exact source relations; none invented or remapped`));
            for (const id of want.names) {
              const role=source.roles[id], node=svg.nodes[id];
              check(() => assert.ok(node.classes.split(' ').includes(`ld-${role}`),`${label} ${id} role`));
              check(() => assert.equal(node.shape,role==='observed' ? 'rect' : 'ellipse',`${label} ${id} shape`));
              check(() => assert.equal(node.label,id,`${label} ${id} visible original identity`));
              if (role==='within') check(() => assert.match(node.roleCaption,/within-person/,`${label} ${id} explicit within-person caption`));
              if (role==='random_intercept') check(() => assert.match(node.roleCaption,/between.*random intercept/,`${label} ${id} explicit between-person caption`));
            }
            for (const edge of want.edges) {
              const id=identity(edge), actual=svg.edges[id];
              const text=sourceLabel(edge,actualScale,view);
              check(() => assert.equal(actual.text,text,`${label} ${id} unchanged source coefficient / documented fixed-text suppression`));
              check(() => assert.equal(actual.fixed,Boolean(edge.fixed),`${label} ${id} fixed metadata`));
              const nonsignificant=sourceNonsignificant(edge,actualScale);
              check(() => assert.equal(actual.nonsignificant,nonsignificant,`${label} ${id} source-p significance style`));
              check(() => assert.equal(actual.dasharray!=='none' && actual.dasharray!=='',nonsignificant,`${label} ${id} only nonsignificant estimates dotted`));
              if(nonsignificant) check(() => assert.equal(actual.linecap,'round',`${label} ${id} dotted line caps`));
              const row=actual.tooltip.split('\n').find(line=>line.startsWith(actualScale+': '));
              check(() => assert.ok(row,`${label} ${id} selected-scale source details`));
              const sourceDetails=JSON.parse(row.slice(actualScale.length+2));
              check(() => assert.equal(sourceDetails && sourceDetails.estimate!=null ? sourceDetails.estimate : null,
                edge[actualScale] == null ? null : edge[actualScale],`${label} ${id} tooltip retains the source coefficient even when its label is hidden`));
              check(() => assert.equal(sourceDetails && sourceDetails.p!=null ? sourceDetails.p : null,
                sourceP(edge,actualScale),`${label} ${id} p belongs to same source scale`));
              check(() => assert.ok(actual.markerEnd && actual.markerValid,`${label} ${id} visible arrowhead`));
              check(() => assert.equal(Boolean(actual.markerStart),edge.operator==='WITH',`${label} ${id} direction`));
              const [from,to]=ends(edge);
              check(() => assert.ok(onBoundary(actual.start,svg.nodes[from]),`${label} ${id} begins on source identity`));
              check(() => assert.ok(onBoundary(actual.end,svg.nodes[to]),`${label} ${id} ends on target identity`));
              if (source.family==='ri_clpm' && edge.operator==='ON') {
                check(() => assert.equal(source.roles[from],'within',`${label} dynamic source remains within-person`));
                check(() => assert.equal(source.roles[to],'within',`${label} dynamic target remains within-person`));
              }
            }
            // Also exercise the user's two-decimal presentation on the same
            // source rows (including .495 -> .50), then restore the lossless
            // three-decimal source display for layout/identity assertions.
            const precision=widget.getByLabel('Decimals',{exact:true});
            await precision.fill('2'); await precision.press('Tab');
            const roundedSVG=await inspectSVG(widget);
            for (const edge of want.edges) {
              check(() => assert.equal(roundedSVG.edges[identity(edge)].text,
                sourceLabel(edge,actualScale,view,2),`${label} ${identity(edge)} half-up two-decimal APA label`));
            }
            await precision.fill('3'); await precision.press('Tab');
            // The middle checkbox state restores every source coefficient
            // while retaining dotted paths. Cycle back to the default after
            // checking the exact labels against the independent source rows.
            const significance=widget.getByLabel('Dotted nonsignificant',{exact:true});
            await significance.click();
            const shown=await inspectSVG(widget);
            check(() => assert.deepEqual(Object.keys(shown.edges).sort(),Object.keys(svg.edges).sort(),`${label} showing coefficients preserves every path`));
            for (const edge of want.edges) {
              const id=identity(edge);
              check(() => assert.equal(shown.edges[id].text,sourceLabel(edge,actualScale,view,3,false),`${label} ${id} restored exact source coefficient`));
              check(() => assert.equal(shown.edges[id].nonsignificant,sourceNonsignificant(edge,actualScale),`${label} ${id} restoring labels preserves significance style`));
            }
            await significance.click();
            await significance.click();
            // The exact edge set already excludes observed substitutions and
            // invented total/indirect effects. These counts explicitly preserve
            // every selected source covariance and dynamic path in each view.
            const covarianceIDs=want.edges.filter(e=>e.operator==='WITH').map(identity).sort();
            const dynamicIDs=want.edges.filter(e=>e.operator==='ON').map(identity).sort();
            check(() => assert.deepEqual(Object.keys(svg.edges).filter(id=>id.startsWith('covariance:')).sort(),covarianceIDs,`${label} selected covariances retained`));
            check(() => assert.deepEqual(Object.keys(svg.edges).filter(id=>id.startsWith('regression:')).sort(),dynamicIDs,`${label} original dynamics retained`));
            if (expected.time_scores) {
              for (const time of new Set(Object.values(expected.time_scores))) {
                check(() => assert.ok(svg.notes.includes(`Wave / time ${time}`),`${label} growth time ${time} preserved`));
              }
            }
            if (view==='simplified') {
              check(() => assert.ok(!svg.notes.some(note=>note.includes('no effects combined')),`${label} no simplified-view caption`));
            }
            positionsByScale.push(Object.fromEntries(Object.entries(svg.nodes).map(([id,n])=>[id,{x:n.x,y:n.y}])));
            record.scales.push({view,requested,actual:actualScale,nodes:want.names.length,
              visible_relations:want.edges.length,independently_checked_relations:want.edges.length,
              covariances:covarianceIDs.length,dynamics:dynamicIDs.length,
              definition_links:want.edges.filter(e=>e.definition).length,
              checked_label_digits:[3,2],source_p_values_checked:true});
            report.coverage.model_view_scale_cases++;
          }
          check(() => assert.deepEqual(positionsByScale[0],positionsByScale[1],`${item.id} ${view} scale preserves coordinates`));
        }
        // Every source fixture exercises two independent editable layouts. The
        // exported config is used only to observe presentation state here.
        const common=projection(source,'simplified').names.sort()[0];
        check(() => assert.ok(common,`${item.id} retained editable node`));
        await widget.getByLabel('View',{exact:true}).selectOption('full');
        const before=await config(widget);
        await widget.locator('[data-action="edit"]').click();
        await dragNode(page,widget,common,28,18);
        const fullMoved=await config(widget);
        verifyEditedRelations(await inspectSVG(widget),source,'full',fullMoved.state.options.scale,`${item.id} after full drag`,check);
        check(() => assert.notDeepEqual(fullMoved.state.views.full.nodes[`node:${common}`],before.state.views.full.nodes[`node:${common}`],`${item.id} full drag changes its position`));
        check(() => assert.deepEqual(fullMoved.state.views.simplified,before.state.views.simplified,`${item.id} full drag preserves complete simplified layout`));
        await widget.getByLabel('View',{exact:true}).selectOption('simplified');
        const beforeSimple=await config(widget);
        check(() => assert.deepEqual(beforeSimple.state.views.full,fullMoved.state.views.full,`${item.id} simplified switch preserves full edit`));
        await dragNode(page,widget,common,-24,21);
        const simpleMoved=await config(widget);
        verifyEditedRelations(await inspectSVG(widget),source,'simplified',simpleMoved.state.options.scale,`${item.id} after simplified drag`,check);
        check(() => assert.notDeepEqual(simpleMoved.state.views.simplified.nodes[`node:${common}`],beforeSimple.state.views.simplified.nodes[`node:${common}`],`${item.id} simplified drag changes its position`));
        check(() => assert.deepEqual(simpleMoved.state.views.full,fullMoved.state.views.full,`${item.id} simplified drag preserves full layout`));
        await widget.locator('[data-action="undo"]').click();
        const undone=await config(widget);
        check(() => assert.deepEqual(undone.state.views,beforeSimple.state.views,`${item.id} undo restores independent layouts`));
        await widget.locator('[data-action="redo"]').click();
        const redone=await config(widget);
        check(() => assert.deepEqual(redone.state.views,simpleMoved.state.views,`${item.id} redo restores independent layouts`));
        await widget.getByLabel('Scale',{exact:true}).selectOption('unstandardized');
        if (expectedStdyx) await widget.getByLabel('Scale',{exact:true}).selectOption('stdyx');
        const rescaled=await config(widget);
        check(() => assert.deepEqual(rescaled.state.views,simpleMoved.state.views,`${item.id} scale changes preserve both edited layouts`));
        await widget.getByLabel('View',{exact:true}).selectOption('full');
        const restoredFull=await config(widget), fullSVG=await inspectSVG(widget);
        verifyEditedRelations(fullSVG,source,'full',restoredFull.state.options.scale,`${item.id} restored edited full view`,check);
        check(() => assert.deepEqual(restoredFull.state.views,simpleMoved.state.views,`${item.id} full restoration retains both edits`));
        check(() => assert.deepEqual(Object.keys(fullSVG.nodes).sort(),Object.keys(source.roles).sort(),`${item.id} all original identities return in full`));
        check(() => assert.deepEqual(Object.keys(fullSVG.edges).sort(),projection(source,'full').edges.map(identity).sort(),`${item.id} all full relations return without substitution`));
        check(() => assert.ok(Math.abs(fullSVG.nodes[common].x-fullMoved.state.views.full.nodes[`node:${common}`].x)<.001 &&
          Math.abs(fullSVG.nodes[common].y-fullMoved.state.views.full.nodes[`node:${common}`].y)<.001,`${item.id} rendered full node uses its full-view edit`));
        await widget.locator('[data-action="restore"]').click();
        const automatic=await config(widget);
        check(() => assert.deepEqual(automatic.state.views.full,initial.state.views.full,`${item.id} automatic full layout restored deterministically`));
        check(() => assert.deepEqual(automatic.state.views.simplified,simpleMoved.state.views.simplified,`${item.id} restore does not reset other view`));
        await widget.locator('[data-action="undo"]').click();
        const undoRestore=await config(widget);
        check(() => assert.deepEqual(undoRestore.state.views,simpleMoved.state.views,`${item.id} automatic restoration is undoable`));
        record.layout_isolation={status:'passed',edited_source_node:common,full_and_simplified:true,
          undo_redo:true,scale_preservation:true,full_structure_restored:true,automatic_restore_undo:true,
          edited_coefficients_and_exact_anchors:true};
        report.coverage.layout_isolation_models++;
        check(() => assert.deepEqual(errors,[],`${item.id} JavaScript errors`));
        check(() => assert.deepEqual(network,[],`${item.id} external requests`));
        record.status='passed';
        console.log(`${item.id}: full/simplified × both scales + independent layout edits passed`);
      } catch (error) {
        record.status='failed'; record.error=error.stack; failed=true;
        console.error(`${item.id}: ${error.message}`);
      } finally {await page.close();}
    }
  } catch (error) {
    failed=true; report.error=error.stack; throw error;
  } finally {
    report.status=failed ? 'failed' : 'passed';
    fs.writeFileSync(resultPath,JSON.stringify(report,null,2)+'\n');
    await browser.close();
  }
  if (failed) process.exitCode=1;
  console.log(`${report.assertions} assertions; ${report.models.length} models; ${report.coverage.model_view_scale_cases} view/scale cases; ${resultPath}`);
})().catch(error=>{console.error(error);process.exitCode=1;});
