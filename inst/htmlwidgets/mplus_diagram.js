/* MplusAutomation Mplus diagram. Package-owned SVG rendering; no network or browser storage. */
(function () {
  'use strict';
  var NS = 'http://www.w3.org/2000/svg';
  var serial = 0;
  var legacyOptionKeys = ['scale', 'digits', 'residuals', 'default_covariances', 'parameter_labels', 'values', 'fixed_values', 'constraints', 'view'];
  var optionKeys = legacyOptionKeys.concat(['stars', 'nonsignificant_dotted', 'nonsignificant_hide_values']);
  var boolKeys = ['residuals', 'default_covariances', 'parameter_labels', 'values', 'fixed_values', 'constraints', 'stars', 'nonsignificant_dotted', 'nonsignificant_hide_values'];
  var sourceKeys = ['title', 'output', 'estimator', 'engine', 'result_id'];
  var exportStyle = '.ld-line{fill:none;stroke:#43596b;stroke-width:1.7}.ld-nonsignificant .ld-line{stroke-dasharray:0 5;stroke-linecap:round}.ld-shape{fill:#fff;stroke:#344e63;stroke-width:1.6}.ld-latent .ld-shape{fill:#edf5fa}.ld-random_intercept .ld-shape{fill:#f6edf8}.ld-within .ld-shape{fill:#edf7ee}.ld-node-text{fill:#172b3a;font:14px "Times New Roman",Times,"Liberation Serif",serif;text-anchor:middle}.ld-role{fill:#526578;font:10px "Times New Roman",Times,"Liberation Serif",serif;text-anchor:middle}.ld-edge-text{fill:#172b3a;font:12px "Times New Roman",Times,"Liberation Serif",serif;text-anchor:middle;dominant-baseline:middle;paint-order:stroke;stroke:white;stroke-width:6px;stroke-linejoin:round}.ld-note{fill:#526578;font:11px "Times New Roman",Times,"Liberation Serif",serif}';
  function clone(value) { return JSON.parse(JSON.stringify(value)); }
  function object(value) { return value !== null && typeof value === 'object' && !Array.isArray(value); }
  function keysExactly(value, allowed, required) {
    if (!object(value) || Object.keys(value).some(function (key) { return allowed.indexOf(key) < 0; })) return false;
    return (required || []).every(function (key) { return Object.prototype.hasOwnProperty.call(value, key); });
  }
  function finite(value, bound) { return typeof value === 'number' && Number.isFinite(value) && Math.abs(value) <= bound; }
  function dict(value) { return object(value) ? value : {}; }
  function optionalDefaults(options) {
    ['stars', 'nonsignificant_dotted', 'nonsignificant_hide_values'].forEach(function (key) {
      if (!Object.prototype.hasOwnProperty.call(options, key)) options[key] = true;
    });
    return options;
  }
  function displayNumber(value, digits, scale) {
    // Match coeff_table's signed 1e-8 nudge at decimal half points. Unlike its
    // historical table sentinel rule, a legitimate estimate of 999 stays data.
    var magnitude = (Math.abs(value) + (value === 0 ? 0 : 1e-8)).toFixed(digits);
    var rounded = Number(magnitude), negative = value < 0 && rounded !== 0;
    if (scale === 'stdyx' && rounded < 1) magnitude = magnitude.replace(/^0\./, '.');
    return (negative ? '\u2212' : '') + magnitude;
  }
  function element(tag, attrs, text) {
    var e = document.createElement(tag);
    Object.keys(attrs || {}).forEach(function (k) { e.setAttribute(k, attrs[k]); });
    if (text !== undefined) e.textContent = text;
    return e;
  }
  function svgElement(tag, attrs, text) {
    var e = document.createElementNS(NS, tag);
    Object.keys(attrs || {}).forEach(function (k) { e.setAttribute(k, attrs[k]); });
    if (text !== undefined) e.textContent = text;
    return e;
  }
  HTMLWidgets.widget({
    name: 'mplus_diagram', type: 'output',
    factory: function (el, width, height) {
      var uid = 'mplus-diagram-' + (++serial), payload, model, state, automatic;
      var editing = false, selected = new Set(), selectedEdge = null, history = [], future = [];
      var svg, scene, toolbar, editor, footer, message, diagnosticsMessage, constraintsPanel, constraintsText, baseBox, stage, drag = null, controls = {};
      function view() { return state.views[state.options.view]; }
      function coordinate(id) { return view().nodes[id]; }
      function config() {
        var source = {}, provenance = model.provenance || {};
        sourceKeys.forEach(function (key) { source[key] = typeof provenance[key] === 'string' ? provenance[key] : null; });
        return {schema_version: 1, fingerprint: model.fingerprint, layout_version: 1, state: clone(state), source: source};
      }
      function validateConfig(c) {
        if (!keysExactly(c, ['schema_version', 'fingerprint', 'layout_version', 'state', 'source'], ['schema_version', 'fingerprint', 'layout_version', 'state']) || c.schema_version !== 1 || c.layout_version !== 1) throw Error('Unsupported configuration schema or layout version.');
        if (Object.prototype.hasOwnProperty.call(c, 'source')) {
          if (!keysExactly(c.source, sourceKeys, sourceKeys) || sourceKeys.some(function (key) { return c.source[key] !== null && typeof c.source[key] !== 'string'; })) throw Error('Invalid source metadata.');
          if (c.source.result_id !== null && !/^[a-fA-F0-9]{64}$/.test(c.source.result_id)) throw Error('Invalid source result identifier.');
        }
        if (c.fingerprint !== model.fingerprint) throw Error('Configuration belongs to a different model structure.');
        var s = c.state;
        if (!keysExactly(s, ['options', 'aliases', 'hidden_edges', 'hidden_nodes', 'views'], ['options', 'aliases', 'hidden_edges', 'views'])) throw Error('Invalid state fields.');
        if (!keysExactly(s.options, optionKeys, legacyOptionKeys)) throw Error('Invalid display option fields.');
        s = clone(s); s.options = optionalDefaults(s.options);
        if ( ['stdyx', 'unstandardized'].indexOf(s.options.scale) < 0 || ['full', 'simplified'].indexOf(s.options.view) < 0 || !Number.isInteger(s.options.digits) || s.options.digits < 0 || s.options.digits > 6 || boolKeys.some(function (k) { return typeof s.options[k] !== 'boolean'; })) throw Error('Invalid display options.');
        var nodeIDs = model.structure.nodes.map(function (n) { return n.id; });
        var edgeIDs = model.structure.edges.map(function (e) { return e.id; });
        if (!Object.prototype.hasOwnProperty.call(s, 'hidden_nodes')) s.hidden_nodes = [];
        if (!Array.isArray(s.hidden_nodes) || s.hidden_nodes.some(function (id) { return nodeIDs.indexOf(id) < 0; }) || new Set(s.hidden_nodes).size !== s.hidden_nodes.length) throw Error('Invalid hidden node IDs.');
        if (!object(s.aliases) || Object.keys(s.aliases).some(function (id) { return nodeIDs.indexOf(id) < 0 || typeof s.aliases[id] !== 'string' || Array.from(s.aliases[id]).length > 500; })) throw Error('Invalid node alias.');
        if (!Array.isArray(s.hidden_edges) || s.hidden_edges.some(function (id) { return edgeIDs.indexOf(id) < 0; }) || new Set(s.hidden_edges).size !== s.hidden_edges.length) throw Error('Invalid hidden edge IDs.');
        if (!keysExactly(s.views, ['full', 'simplified'], ['full', 'simplified'])) throw Error('Both independent views are required.');
        ['full', 'simplified'].forEach(function (name) {
          var v = s.views[name];
          if (!keysExactly(v, ['nodes', 'edges', 'viewport'], ['nodes', 'edges', 'viewport']) || !keysExactly(v.nodes, nodeIDs, nodeIDs)) throw Error('Invalid node coordinates for ' + name + '.');
          Object.keys(v.nodes).forEach(function (id) {
            var p = v.nodes[id];
            if (!keysExactly(p, ['x', 'y'], ['x', 'y']) || !finite(p.x, 1e6) || !finite(p.y, 1e6)) throw Error('Invalid node position.');
          });
          if (!object(v.edges)) throw Error('Invalid edge overrides.');
          Object.keys(v.edges).forEach(function (id) {
            var e = v.edges[id];
            if (edgeIDs.indexOf(id) < 0 || !keysExactly(e, ['curve', 'label_dx', 'label_dy', 'residual_angle', 'residual_offset', 'covariance_side', 'line_type', 'bend_offset']) || Object.keys(e).some(function (k) { return ['covariance_side', 'line_type'].indexOf(k) < 0 && !finite(e[k], 1e6); })) throw Error('Invalid edge override.');
            var relationship = model.structure.edges.find(function (edge) { return edge.id === id; });
            if (Object.prototype.hasOwnProperty.call(e, 'line_type') &&
                (relationship.type === 'variance' || ['straight', 'curve', 'polyline'].indexOf(e.line_type) < 0)) throw Error('Invalid line_type.');
            if (Object.prototype.hasOwnProperty.call(e, 'bend_offset') && e.line_type !== 'polyline') throw Error('bend_offset requires polyline line_type.');
            if (Object.prototype.hasOwnProperty.call(e, 'covariance_side') &&
                (model.structure.edges.find(function (edge) { return edge.id === id; }).type !== 'covariance' ||
                 ['left', 'right', 'top', 'bottom'].indexOf(e.covariance_side) < 0)) throw Error('Invalid covariance connection side.');
            var hasAngle = Object.prototype.hasOwnProperty.call(e, 'residual_angle'), hasOffset = Object.prototype.hasOwnProperty.call(e, 'residual_offset');
            if (hasAngle || hasOffset) {
              var relationship = model.structure.edges.find(function (edge) { return edge.id === id; });
              if (relationship.type !== 'variance' || !hasAngle || !finite(e.residual_angle, 180) ||
                  (hasOffset && !finite(e.residual_offset, .95))) throw Error('Invalid residual orbit.');
              if (!hasOffset) e.residual_offset = 0;
            }
          });
          if (!keysExactly(v.viewport, ['x', 'y', 'k'], ['x', 'y', 'k']) || !finite(v.viewport.x, 1e6) || !finite(v.viewport.y, 1e6) || !finite(v.viewport.k, 10) || v.viewport.k < 0.1) throw Error('Invalid viewport.');
        });
        return clone(s);
      }
      function snapshot() { return clone(state); }
      function record(before) {
        if (JSON.stringify(before) !== JSON.stringify(state)) { history.push(before); if (history.length > 100) history.shift(); future = []; }
      }
      function change(fn) { var before = snapshot(); fn(); record(before); draw(); }
      function notify(text) { message.textContent = text; }
      function button(parent, title, fn, name) {
        var b = element('button', {type: 'button', 'data-action': name || title}, title);
        b.addEventListener('click', fn); parent.appendChild(b); return b;
      }
      function selectControl(parent, label, key, items) {
        var l = element('label', {}, label + ' '), input = element('select', {'aria-label': label});
        items.forEach(function (item) { input.appendChild(element('option', {value: item[0]}, item[1])); });
        input.addEventListener('change', function () { change(function () { state.options[key] = input.value; }); });
        l.appendChild(input); parent.appendChild(l); controls[key] = input;
      }
      function checkbox(parent, label, key) {
        var l = element('label'), input = element('input', {type: 'checkbox', 'aria-label': label});
        input.addEventListener('change', function () { change(function () { state.options[key] = input.checked; }); });
        l.appendChild(input); l.appendChild(document.createTextNode(label)); parent.appendChild(l); controls[key] = input;
      }
      function nonsignificantControl(parent) {
        var l = element('label'), input = element('input', {type: 'checkbox', 'aria-label': 'Dotted nonsignificant',
          title: 'Click to cycle: checked = dotted without coefficients; mixed = dotted with coefficients; unchecked = solid with coefficients.'});
        input.addEventListener('change', function () {
          change(function () {
            var o = state.options;
            if (!o.nonsignificant_dotted) { o.nonsignificant_dotted = true; o.nonsignificant_hide_values = true; }
            else if (o.nonsignificant_hide_values) o.nonsignificant_hide_values = false;
            else o.nonsignificant_dotted = false;
          });
        });
        l.appendChild(input); l.appendChild(document.createTextNode('Dotted nonsignificant')); parent.appendChild(l);
        controls.nonsignificant_dotted = input;
      }
      function build() {
        el.textContent = ''; el.classList.add('mplus-diagram'); el.setAttribute('tabindex', '0');
        toolbar = element('div', {'class': 'ld-toolbar'}); el.appendChild(toolbar);
        controls.edit = button(toolbar, editing ? 'Finish editing' : 'Edit diagram', function () { editing = !editing; selected.clear(); selectedEdge = null; draw(); }, 'edit');
        selectControl(toolbar, 'Scale', 'scale', [['stdyx', 'STDYX'], ['unstandardized', 'Unstandardized']]);
        selectControl(toolbar, 'View', 'view', [['full', 'Full structure'], ['simplified', 'Simplified']]);
        var digitsLabel = element('label', {}, 'Decimals '), digits = element('input', {type: 'number', min: 0, max: 6, step: 1, 'aria-label': 'Decimals'});
        digits.addEventListener('change', function () { var n = Number(digits.value); if (Number.isInteger(n) && n >= 0 && n <= 6) change(function () { state.options.digits = n; }); else draw(); });
        digitsLabel.appendChild(digits); toolbar.appendChild(digitsLabel); controls.digits = digits;
        checkbox(toolbar, 'Significance stars', 'stars'); nonsignificantControl(toolbar);
        checkbox(toolbar, 'Values', 'values'); checkbox(toolbar, 'Fixed values', 'fixed_values'); checkbox(toolbar, 'Parameter labels', 'parameter_labels');
        checkbox(toolbar, 'Residuals', 'residuals'); checkbox(toolbar, 'Default covariances', 'default_covariances'); checkbox(toolbar, 'Constraints', 'constraints');
        editor = element('div', {'class': 'ld-editor'}); el.appendChild(editor);
        controls.undo = button(editor, 'Undo', undo, 'undo'); controls.redo = button(editor, 'Redo', redo, 'redo');
        button(editor, 'Restore automatic layout', function () { change(function () {
          state.views[state.options.view] = clone(automatic[state.options.view]);
          state.hidden_nodes = []; state.hidden_edges = []; selected.clear(); selectedEdge = null;
        }); }, 'restore').title = 'Restore removed objects in both views and reset the current view layout.';
        button(editor, 'Align horizontally', function () { align('y'); }, 'align-horizontal');
        button(editor, 'Align vertically', function () { align('x'); }, 'align-vertical');
        var alias = element('input', {type: 'text', 'class': 'ld-alias', 'aria-label': 'Display name', placeholder: 'Display name (one node)'});
        editor.appendChild(alias); controls.alias = alias;
        button(editor, 'Set name', function () { if (selected.size === 1 && Array.from(alias.value).length <= 500) change(function () { state.aliases[Array.from(selected)[0]] = alias.value; }); }, 'alias');
        controls.remove = button(editor, 'Remove object', function () {
          if (selected.size || selectedEdge) change(function () {
            selected.forEach(function (id) { if (state.hidden_nodes.indexOf(id) < 0) state.hidden_nodes.push(id); });
            if (selectedEdge && state.hidden_edges.indexOf(selectedEdge) < 0) state.hidden_edges.push(selectedEdge);
            selected.clear(); selectedEdge = null;
          });
        }, 'remove-object');
        var pathLabel = element('label', {}, 'Path '), pathSelect = element('select', {'aria-label': 'Path', style: 'max-width:260px'});
        pathSelect.addEventListener('change', function () { selected.clear(); selectedEdge = pathSelect.value || null; draw(); });
        pathLabel.appendChild(pathSelect); editor.appendChild(pathLabel); controls.path = pathSelect;
        var typeLabel = element('label', {}, 'Line type '), typeSelect = element('select', {'aria-label': 'Line type'});
        [['straight', 'Straight'], ['curve', 'Curve'], ['polyline', 'Polyline']].forEach(function (item) {
          typeSelect.appendChild(element('option', {value: item[0]}, item[1]));
        });
        typeSelect.addEventListener('change', function () {
          var edge = model.structure.edges.find(function (e) { return e.id === selectedEdge; });
          if (!edge || edge.type === 'variance') return;
          var type = typeSelect.value;
          change(function () {
            var override = clone(view().edges[edge.id] || {});
            override.line_type = type;
            delete override.label_dx; delete override.label_dy;
            if (type === 'polyline') override.bend_offset = -100;
            else delete override.bend_offset;
            if (type === 'curve' && !override.curve) override.curve = edge.type === 'covariance' ? 55 : -160;
            view().edges[edge.id] = override;
          });
        });
        typeLabel.appendChild(typeSelect); editor.appendChild(typeLabel); controls.lineType = typeSelect;
        button(toolbar, 'Fit', function () { change(function () { view().viewport = {x: 0, y: 0, k: 1}; }); }, 'fit');
        button(toolbar, '−', function () { zoom(1 / 1.2); }, 'zoom-out'); button(toolbar, '+', function () { zoom(1.2); }, 'zoom-in');
        button(toolbar, 'Export config', function () { download(new Blob([JSON.stringify(config(), null, 2)], {type: 'application/json'}), 'model.diagram.json'); }, 'export-config');
        var importer = element('input', {type: 'file', accept: '.json,application/json', hidden: 'hidden', 'aria-label': 'Import configuration file'});
        importer.addEventListener('change', function () {
          if (!importer.files.length) return;
          notify('Importing configuration…');
          var reader = new FileReader();
          reader.onload = function () { try { var next = validateConfig(JSON.parse(reader.result)); change(function () { state = next; selected.clear(); selectedEdge = null; }); notify('Configuration imported. Edits remain temporary until exported or saved to the project.'); } catch (e) { notify('Import rejected: ' + e.message + ' Current diagram was preserved.'); } importer.value = ''; };
          reader.onerror = function () { notify('Could not read configuration. Current diagram was preserved.'); };
          reader.readAsText(importer.files[0]);
        });
        toolbar.appendChild(importer); button(toolbar, 'Import config', function () { importer.click(); }, 'import-config');
        button(toolbar, 'SVG', exportSVG, 'export-svg');
        var multiplier = element('select', {'aria-label': 'PNG scale'});
        [1, 2, 3, 4].forEach(function (n) { multiplier.appendChild(element('option', {value: n}, n + '× PNG')); }); multiplier.value = '2'; toolbar.appendChild(multiplier); controls.pngScale = multiplier;
        button(toolbar, 'PNG', exportPNG, 'export-png');
        if (payload.editor) {
          button(toolbar, 'Save and return', function () { sendEditor('save'); }, 'save');
          button(toolbar, 'Open in browser', function () { sendEditor('browser'); }, 'browser');
          button(toolbar, 'Cancel and return', function () { sendEditor('cancel'); }, 'cancel');
        }
        diagnosticsMessage = element('div', {'class': 'ld-message ld-diagnostics', role: 'note'}); el.appendChild(diagnosticsMessage);
        message = element('div', {'class': 'ld-message', role: 'status', 'aria-live': 'polite'}); el.appendChild(message);
        constraintsPanel = element('details', {'class': 'ld-constraints', open: 'open'});
        constraintsPanel.appendChild(element('summary', {}, 'Original model constraints (original scale)'));
        constraintsText = element('pre'); constraintsPanel.appendChild(constraintsText); el.appendChild(constraintsPanel);
        stage = element('div', {'class': 'ld-stage'}); el.appendChild(stage);
        svg = svgElement('svg', {'class': 'ld-canvas', role: 'img', 'aria-label': 'Mplus model diagram'}); stage.appendChild(svg);
        footer = element('div', {'class': 'ld-footer'}); el.appendChild(footer);
        svg.addEventListener('pointerdown', beginBackground);
        svg.addEventListener('pointermove', movePointer);
        svg.addEventListener('pointerup', endPointer); svg.addEventListener('pointercancel', endPointer);
        el.onkeydown = function (event) {
          if (/^(INPUT|SELECT|TEXTAREA)$/.test(event.target.tagName)) return;
          if (editing && (event.ctrlKey || event.metaKey) && event.key.toLowerCase() === 'z') { event.preventDefault(); if (event.shiftKey) redo(); else undo(); }
          if (editing && (event.ctrlKey || event.metaKey) && event.key.toLowerCase() === 'y') { event.preventDefault(); redo(); }
          if (event.key === 'Escape') { selected.clear(); selectedEdge = null; draw(); }
        };
      }
      function sendEditor(action) {
        if (!window.Shiny || typeof window.Shiny.setInputValue !== 'function') { notify('The local R editor connection is unavailable. Export config to preserve these edits.'); return; }
        try { var c = config(); if (action !== 'cancel') validateConfig(c); window.Shiny.setInputValue(payload.editor.input_id, {action: action, config: action !== 'cancel' ? c : null}, {priority: 'event'}); }
        catch (e) { notify('Save rejected: ' + e.message); }
      }
      function undo() { if (history.length) { future.push(snapshot()); state = history.pop(); draw(); } }
      function redo() { if (future.length) { history.push(snapshot()); state = future.pop(); draw(); } }
      function align(axis) { if (selected.size < 2) return; change(function () { var ids = Array.from(selected), mean = ids.reduce(function (sum, id) { return sum + coordinate(id)[axis]; }, 0) / ids.length; ids.forEach(function (id) { coordinate(id)[axis] = mean; }); }); }
      function zoom(factor) { change(function () { view().viewport.k = Math.max(0.1, Math.min(10, view().viewport.k * factor)); }); }
      function hasStdyx() {
        return Object.keys(model.results).some(function (id) { var r = model.results[id].stdyx; return r && Number.isFinite(r.estimate); });
      }
      function actualScale() {
        if (!hasStdyx()) state.options.scale = 'unstandardized';
        return state.options.scale;
      }
      function visibleNodes() {
        var nodes = model.structure.nodes.filter(function (n) { return state.hidden_nodes.indexOf(n.id) < 0; });
        if (state.options.view === 'full') return nodes;
        if (model.structure.family === 'ri_clpm') return nodes.filter(function (n) { return n.role !== 'observed'; });
        if (['cfa', 'sem'].indexOf(model.structure.family) >= 0) {
          var targets = new Set(model.structure.edges.filter(function (e) { return e.type === 'loading'; }).map(function (e) { return e.to; }));
          var structural = new Set(); model.structure.edges.forEach(function (e) { if (e.type === 'regression') { structural.add(e.from); structural.add(e.to); } });
          return nodes.filter(function (n) { return !targets.has(n.id) || structural.has(n.id) || n.role !== 'observed'; });
        }
        return nodes;
      }
      function visibleEdges(nodes) {
        var ids = new Set(nodes.map(function (n) { return n.id; })), o = state.options;
        return model.structure.edges.filter(function (e) {
          return ids.has(e.from) && ids.has(e.to) && state.hidden_edges.indexOf(e.id) < 0 &&
            (e.type !== 'variance' || (o.residuals && o.view === 'full')) &&
            (e.type !== 'covariance' || e.origin !== 'default' || o.default_covariances) &&
            (!e.fixed || e.fixed_value !== 0 || o.constraints || (model.structure.family === 'growth' && e.type === 'loading'));
        });
      }
      function nodeName(n) { return Object.prototype.hasOwnProperty.call(state.aliases, n.id) && state.aliases[n.id] !== '' ? state.aliases[n.id] : n.name; }
      function nodeSize(n) { var len = Math.max.apply(null, wrap(nodeName(n), 24).map(function (s) { return Array.from(s).reduce(function (sum, ch) { return sum + (ch.charCodeAt(0) > 255 ? 1.7 : 1); }, 0); })); return {w: Math.max(n.role === 'random_intercept' ? 174 : 106, Math.min(320, len * 8 + 24)), h: 52 + (wrap(nodeName(n), 24).length - 1) * 17 + (n.role !== 'observed' && n.role !== 'latent' ? 13 : 0)}; }
      function wrap(text, max) { var chars = Array.from(String(text)), lines = []; while (chars.length) lines.push(chars.splice(0, max).join('')); return lines.length ? lines : ['']; }
      function anchor(n, toward) {
        var p = coordinate(n.id), d = nodeSize(n), dx = toward.x - p.x, dy = toward.y - p.y;
        if (!dx && !dy) return {x: p.x + d.w / 2, y: p.y};
        var ratio = n.role === 'observed' ? 1 / Math.max(Math.abs(dx) / (d.w / 2), Math.abs(dy) / (d.h / 2)) : 1 / Math.sqrt(dx * dx / (d.w * d.w / 4) + dy * dy / (d.h * d.h / 4));
        return {x: p.x + dx * ratio, y: p.y + dy * ratio};
      }
      function significance(e, scale) {
        var result = model.results[e.id], row = result && result[scale], p = row && row.p;
        var valid = typeof p === 'number' && Number.isFinite(p) && p >= 0 && p <= 1;
        var free = e.fixed === false;
        // A loading fixed on the raw scale can have its own estimated STDYX
        // uncertainty. Honor Mplus's actual displayed-scale p, without borrowing
        // raw p or treating the raw fixed flag as a standardized test result.
        var inferential = scale === 'stdyx' || free;
        return {p: valid ? p : null, valid: valid, free: free, inferential: inferential,
          dotted: state.options.nonsignificant_dotted && inferential && valid && p >= 0.05,
          stars: state.options.stars && inferential && valid ? (p < 0.001 ? '***' : p < 0.01 ? '**' : p < 0.05 ? '*' : '') : ''};
      }
      function normalizedAngle(degrees) { return ((degrees + 180) % 360 + 360) % 360 - 180; }
      function orbitGeometry(n, degrees) {
        var p = coordinate(n.id), radians = degrees * Math.PI / 180;
        var u = {x: Math.cos(radians), y: Math.sin(radians)};
        // Intersect a ray from the variable CENTER with its shape, then extend
        // two thirds of the original 64 units outward along that same ray.
        // Legacy residual_offset is readable but no longer displaces the shaft.
        var head = anchor(n, {x: p.x + u.x, y: p.y + u.y});
        var length = 64 * 2 / 3;
        return {head: head, tail: {x: head.x + u.x * length, y: head.y + u.y * length}, side: u,
          normal: {x: -u.y, y: u.x}, angle: normalizedAngle(degrees), offset: 0};
      }
      function varianceGeometry(e, nodeMap, visibleRelations) {
        var n = nodeMap[e.to], p = coordinate(e.to), size = nodeSize(n);
        var incoming = model.structure.edges.filter(function (edge) { return edge.type === 'loading' && edge.to === e.to; });
        var terminal = !model.structure.edges.some(function (edge) {
          return (edge.type === 'loading' || edge.type === 'regression') && edge.from === e.to;
        });
        var side = terminal ? {x: 1, y: 0} : {x: 0, y: -1};
        var cornerPort = !terminal && e.residual === true && !(incoming.length && n.role === 'observed') &&
          visibleRelations.some(function (edge) {
            return edge.type === 'covariance' && ((edge.from === e.to && edge.residual_from === true) ||
              (edge.to === e.to && edge.residual_to === true));
          });
        if (incoming.length && n.role === 'observed') {
          // A measurement residual approaches from the side opposite its factor
          // loadings. Terminal structural outcomes use the right side; earlier
          // variables use the upper side to keep the regression grid clear.
          // No statistical node is added.
          var dx = 0, dy = 0;
          incoming.forEach(function (edge) { var source = coordinate(edge.from); dx += p.x - source.x; dy += p.y - source.y; });
          side = Math.abs(dx) >= Math.abs(dy) ? {x: dx < 0 ? -1 : 1, y: 0} : {x: 0, y: dy < 0 ? -1 : 1};
        }
        var curve = (view().edges[e.id] || {}).curve || 0;
        var port = anchor(n, {x: p.x + side.x, y: p.y + side.y});
        if (cornerPort) {
          // Keep the residual shaft distinct from horizontal autoregressions and
          // diagonal cross-lags. Start near the upper-right border and rise to
          // a tail outside the column; the covariance still uses the same bow.
          port = n.role === 'observed' ? {x: p.x + size.w / 2 - 6, y: p.y - size.h / 2} :
            {x: p.x + size.w * Math.sqrt(3) / 4, y: p.y - size.h / 4};
          var tailDx = Math.max(16, p.x + size.w / 2 + 10 - port.x);
          side = {x: tailDx / 64, y: -Math.sqrt(1 - Math.pow(tailDx / 64, 2))};
        }
        // Retain the original layout bearing; orbitGeometry sets shaft length.
        var perpendicular = {x: -side.y, y: side.x};
        var tail = {x: port.x + side.x * 64 + perpendicular.x * curve,
                    y: port.y + side.y * 64 + perpendicular.y * curve};
        var override = view().edges[e.id] || {};
        // Retain the default/legacy tail's bearing, but always aim at the center.
        var angle = Math.atan2(tail.y - p.y, tail.x - p.x) * 180 / Math.PI;
        if (Object.prototype.hasOwnProperty.call(override, 'residual_angle')) angle = override.residual_angle;
        var orbit = orbitGeometry(n, angle), head = orbit.head; tail = orbit.tail;
        return {path: 'M' + tail.x + ',' + tail.y + ' L' + head.x + ',' + head.y,
          control: tail, normal: orbit.normal, tail: tail, head: head, side: orbit.side,
          angle: orbit.angle, offset: orbit.offset,
          label: {x: (tail.x + head.x) / 2, y: (tail.y + head.y) / 2},
          fromDock: 'variance-tail', toDock: 'node', kind: e.residual === true ? 'residual-variance' : 'variance'};
      }
      function cubicCovariance(a, b, outward, curve, docking) {
        // User-reference screenshots show a sagitta about 14--15% of the
        // endpoint span. Keep this proportion across short and long links.
        // curve=55 is the default; zero stays straight and manual edits scale it.
        var dx = b.x - a.x, dy = b.y - a.y, span = Math.max(1, Math.hypot(dx, dy));
        var nx = -dy / span, ny = dx / span;
        var direction = typeof outward === 'number' ? {x: outward, y: 0} : outward;
        var facing = nx * direction.x + ny * direction.y;
        if (facing < 0 || (Math.abs(facing) < 1e-9 && ny * (direction.x || direction.y) < 0)) { nx = -nx; ny = -ny; }
        var unitBend = span * 0.15 / 55, sagitta = curve * unitBend;
        var ratio = sagitta / span;
        // Cubic approximation of a shallow circular arc. At the default ratio
        // its endpoint tangents lean about 33 degrees from the endpoint chord,
        // instead of forming horizontal hooks; midpoint bow is exactly sagitta.
        var along = Math.max(0.1, (1 - 4 * ratio * ratio) / 3), bow = sagitta * 4 / 3;
        var c1 = {x: a.x + dx * along + nx * bow, y: a.y + dy * along + ny * bow};
        var c2 = {x: b.x - dx * along + nx * bow, y: b.y - dy * along + ny * bow};
        var center = {x: (a.x + b.x) / 2 + nx * sagitta, y: (a.y + b.y) / 2 + ny * sagitta};
        return {path: 'M' + a.x + ',' + a.y + ' C' + c1.x + ',' + c1.y + ' ' + c2.x + ',' + c2.y + ' ' + b.x + ',' + b.y,
          label: center, control: center, normal: {x: nx / unitBend, y: ny / unitBend},
          fromDock: docking.fromDock, toDock: docking.toDock, kind: docking.kind};
      }
      function sideDirection(side) {
        return {left: {x: -1, y: 0}, right: {x: 1, y: 0}, top: {x: 0, y: -1}, bottom: {x: 0, y: 1}}[side];
      }
      function covarianceAxis(e) {
        var a = coordinate(e.from), b = coordinate(e.to);
        return Math.abs(b.y - a.y) >= Math.abs(b.x - a.x) ? 'x' : 'y';
      }
      function defaultCovarianceSide(e) {
        // Structural incoming paths, not names or display aliases, identify
        // exogenous variables. Left-column predictors route outside their column.
        var exogenous = function (id) { return !model.structure.edges.some(function (edge) {
          return (edge.type === 'loading' || edge.type === 'regression') && edge.to === id;
        }); };
        if (model.structure.family === 'cfa' && [e.from, e.to].every(function (id) {
          return model.structure.nodes.find(function (n) { return n.id === id; }).role === 'latent';
        })) return 'left';
        if (covarianceAxis(e) === 'y') return 'top';
        if (exogenous(e.from) && exogenous(e.to)) return 'left';
        var xs = visibleNodes().map(function (n) { return coordinate(n.id).x; });
        var middle = (Math.min.apply(null, xs) + Math.max.apply(null, xs)) / 2;
        return (coordinate(e.from).x + coordinate(e.to).x) / 2 < middle ? 'left' : 'right';
      }
      function lineType(e) {
        var override = view().edges[e.id] || {};
        // Legacy configurations encode curvature without an explicit type.
        return override.line_type || (e.type === 'covariance' || override.curve ? 'curve' : 'straight');
      }
      function polylineGeometry(e, nodeMap, aTail, bTail, docks) {
        var from = coordinate(e.from), to = coordinate(e.to), override = view().edges[e.id] || {};
        var horizontal = Math.abs(to.x - from.x) >= Math.abs(to.y - from.y);
        var axis = horizontal ? 'y' : 'x';
        var sizeFrom = nodeSize(nodeMap[e.from]), sizeTo = nodeSize(nodeMap[e.to]);
        var aCenter = aTail || from, bCenter = bTail || to;
        var halfFrom = aTail ? 0 : (horizontal ? sizeFrom.h : sizeFrom.w) / 2;
        var halfTo = bTail ? 0 : (horizontal ? sizeTo.h : sizeTo.w) / 2;
        var midpoint = (aCenter[axis] + bCenter[axis]) / 2;
        var offset = Object.prototype.hasOwnProperty.call(override, 'bend_offset') ? override.bend_offset : -100;
        // Keep the two corners outside both endpoint shapes. This is a manual
        // routing lane, not a claim of automatic avoidance of intervening nodes.
        var lane = offset < 0 ? Math.min(midpoint + offset, aCenter[axis] - halfFrom - 24, bCenter[axis] - halfTo - 24) :
          Math.max(midpoint + offset, aCenter[axis] + halfFrom + 24, bCenter[axis] + halfTo + 24);
        var first = {x: aCenter.x, y: aCenter.y}, last = {x: bCenter.x, y: bCenter.y};
        first[axis] = lane; last[axis] = lane;
        var a = aTail || anchor(nodeMap[e.from], first), b = bTail || anchor(nodeMap[e.to], last);
        var center = {x: (first.x + last.x) / 2, y: (first.y + last.y) / 2};
        return {path: 'M' + a.x + ',' + a.y + ' L' + first.x + ',' + first.y + ' L' + last.x + ',' + last.y + ' L' + b.x + ',' + b.y,
          label: center, control: center, normal: horizontal ? {x: 0, y: 1} : {x: 1, y: 0},
          bendOffset: lane - midpoint, middle: {from: first, to: last},
          fromDock: docks.fromDock, toDock: docks.toDock, kind: docks.kind};
      }
      function geometry(e, nodeMap, varianceMap) {
        if (e.type === 'variance') return varianceMap[e.to];
        var from = coordinate(e.from), to = coordinate(e.to), override = view().edges[e.id] || {};
        var curve = Object.prototype.hasOwnProperty.call(override, 'curve') ? override.curve : (e.type === 'covariance' ? 55 : 0);
        var fromVariance = e.type === 'covariance' && e.residual_from === true && varianceMap[e.from];
        var toVariance = e.type === 'covariance' && e.residual_to === true && varianceMap[e.to];
        var fromResidual = fromVariance && fromVariance.residual === true;
        var toResidual = toVariance && toVariance.residual === true;
        var aTail = fromResidual ? fromVariance.tail : null, bTail = toResidual ? toVariance.tail : null;
        var docks = {fromDock: aTail ? 'residual-tail' : 'node', toDock: bTail ? 'residual-tail' : 'node', kind: 'ordinary'};
        if (e.type === 'covariance') docks.kind = aTail || bTail ? 'residual-covariance' : 'node-covariance';
        var type = lineType(e);
        if (type === 'polyline') return polylineGeometry(e, nodeMap, aTail, bTail, docks);
        if (type === 'straight') {
          var a = aTail || anchor(nodeMap[e.from], bTail || to), b = bTail || anchor(nodeMap[e.to], aTail || from);
          var dx = b.x - a.x, dy = b.y - a.y, span = Math.max(1, Math.hypot(dx, dy));
          var center = {x: (a.x + b.x) / 2, y: (a.y + b.y) / 2};
          return {path: 'M' + a.x + ',' + a.y + ' L' + b.x + ',' + b.y, label: center, control: center,
            normal: {x: -dy / span, y: dx / span}, fromDock: docks.fromDock, toDock: docks.toDock, kind: docks.kind};
        }
        if (aTail || bTail) {
          var outward = Math.sign((fromVariance && fromVariance.side.x) || (toVariance && toVariance.side.x) || 1);
          var a = aTail || anchor(nodeMap[e.from], {x: from.x + outward, y: from.y});
          var b = bTail || anchor(nodeMap[e.to], {x: to.x + outward, y: to.y});
          docks.kind = 'residual-covariance';
          return cubicCovariance(a, b, outward, curve, docks);
        }
        if (e.type === 'covariance') {
          var side = override.covariance_side || defaultCovarianceSide(e);
          var direction = sideDirection(side);
          var a = anchor(nodeMap[e.from], {x: from.x + direction.x, y: from.y + direction.y});
          var b = anchor(nodeMap[e.to], {x: to.x + direction.x, y: to.y + direction.y});
          docks.kind = model.structure.family === 'cfa' && nodeMap[e.from].role === 'latent' && nodeMap[e.to].role === 'latent' ? 'cfa-covariance' : 'node-covariance';
          var arc = cubicCovariance(a, b, direction, curve, docks); arc.connectionSide = side;
          return arc;
        }
        var dx = to.x - from.x, dy = to.y - from.y, length = Math.max(1, Math.hypot(dx, dy)), normal = {x: -dy / length, y: dx / length};
        var mid = {x: (from.x + to.x) / 2, y: (from.y + to.y) / 2};
        var control = {x: mid.x + normal.x * curve, y: mid.y + normal.y * curve};
        var a = anchor(nodeMap[e.from], control), b = anchor(nodeMap[e.to], control);
        return {path: 'M' + a.x + ',' + a.y + ' Q' + control.x + ',' + control.y + ' ' + b.x + ',' + b.y,
          label: {x: (a.x + 2 * control.x + b.x) / 4, y: (a.y + 2 * control.y + b.y) / 4},
          control: control, normal: normal, fromDock: 'node', toDock: 'node', kind: 'ordinary'};
      }
      function edgeLabel(e, scale) {
        var o = state.options, parts = [], r = model.results[e.id], value = r && r[scale] && r[scale].estimate;
        var sig = significance(e, scale);
        if (sig.dotted && o.nonsignificant_hide_values) return '';
        if (o.values && (!e.fixed || o.fixed_values) && !(o.view === 'simplified' && e.fixed)) {
          if (Number.isFinite(value)) parts.push(displayNumber(value, o.digits, scale) + sig.stars);
          else if (scale === 'unstandardized' && e.fixed && Number.isFinite(e.fixed_value)) parts.push(displayNumber(e.fixed_value, o.digits, scale));
          else parts.push('NA');
        }
        if (o.parameter_labels && e.label) parts.push('(' + e.label + ')');
        return parts.join(' ');
      }
      function draw() {
        if (!state) return;
        var o = state.options, scale = actualScale(), nodes = visibleNodes(), edges = visibleEdges(nodes), nodeMap = {};
        // A view change or history replay must not leave invisible objects selected.
        selected.forEach(function (id) { if (!nodes.some(function (n) { return n.id === id; })) selected.delete(id); });
        if (!edges.some(function (e) { return e.id === selectedEdge; })) selectedEdge = null;
        // Short arrows remain directly reachable where covariance tails meet them.
        edges.sort(function (a, b) { return Number(a.type === 'variance') - Number(b.type === 'variance'); });
        model.structure.nodes.forEach(function (n) { nodeMap[n.id] = n; });
        var varianceMap = {};
        edges.forEach(function (e) { if (e.type === 'variance') {
          varianceMap[e.to] = varianceGeometry(e, nodeMap, edges);
          varianceMap[e.to].residual = e.residual === true;
          varianceMap[e.to].edgeID = e.id;
        } });
        el.classList.toggle('ld-editing', editing); editor.hidden = !editing; controls.edit.textContent = editing ? 'Finish editing' : 'Edit diagram';
        controls.scale.querySelector('option[value="stdyx"]').disabled = !hasStdyx();
        optionKeys.forEach(function (k) { if (controls[k]) { if (boolKeys.indexOf(k) >= 0) controls[k].checked = o[k]; else controls[k].value = o[k]; } });
        controls.nonsignificant_dotted.indeterminate = o.nonsignificant_dotted && !o.nonsignificant_hide_values;
        controls.nonsignificant_dotted.setAttribute('aria-checked', controls.nonsignificant_dotted.indeterminate ? 'mixed' : String(o.nonsignificant_dotted));
        constraintsPanel.hidden = !o.constraints;
        if (o.constraints) {
          var constraintLines = model.structure.edges.filter(function (e) { return e.fixed; }).map(function (e) { return e.id + ' = ' + (e.fixed_value == null ? 'unknown' : e.fixed_value) + (e.label ? ' (' + e.label + ')' : ''); });
          (model.structure.constraints || []).forEach(function (c) { constraintLines.push(c.source || JSON.stringify(c)); });
          constraintsText.textContent = constraintLines.length ? constraintLines.join('\n') : 'No explicit constraints are available in this model.';
        }
        controls.undo.disabled = !history.length; controls.redo.disabled = !future.length;
        controls.remove.disabled = !selected.size && !selectedEdge;
        controls.path.textContent = ''; controls.path.appendChild(element('option', {value: ''}, 'Select a path'));
        edges.forEach(function (e) {
          controls.path.appendChild(element('option', {value: e.id}, nodeName(nodeMap[e.from]) +
            (e.type === 'covariance' ? ' ↔ ' : ' → ') + nodeName(nodeMap[e.to]) + ' (' + e.type + ')'));
        });
        controls.path.value = selectedEdge || '';
        var chosen = edges.find(function (e) { return e.id === selectedEdge; });
        controls.lineType.disabled = !chosen || chosen.type === 'variance';
        controls.lineType.value = chosen && chosen.type !== 'variance' ? lineType(chosen) : 'straight';
        controls.alias.disabled = selected.size !== 1; if (selected.size === 1) controls.alias.value = nodeName(nodeMap[Array.from(selected)[0]]);
        svg.textContent = '';
        var defs = svgElement('defs'), marker = svgElement('marker', {id: uid + '-arrow', viewBox: '0 0 10 10', refX: 9, refY: 5, markerWidth: 7, markerHeight: 7, orient: 'auto-start-reverse', markerUnits: 'userSpaceOnUse'});
        marker.appendChild(svgElement('path', {d: 'M0,0 L10,5 L0,10 Z', fill: '#43596b'})); defs.appendChild(marker);
        var labelMask = svgElement('mask', {id: uid + '-label-gap', maskUnits: 'userSpaceOnUse', maskContentUnits: 'userSpaceOnUse'});
        var maskBase = svgElement('rect', {fill: 'white'}); labelMask.appendChild(maskBase); defs.appendChild(labelMask);
        var hitClip = svgElement('clipPath', {id: uid + '-hit-clearance', clipPathUnits: 'userSpaceOnUse', 'data-editor-only': 'true'});
        var hitRegion = svgElement('path', {'clip-rule': 'evenodd'}), interactionHoles = [], covarianceTargets = [];
        hitClip.appendChild(hitRegion); defs.appendChild(hitClip); svg.appendChild(defs); svg.appendChild(svgElement('style', {}, exportStyle));
        scene = svgElement('g', {'class': 'ld-scene'}); svg.appendChild(scene);
        edges.forEach(function (e) {
          var geo = geometry(e, nodeMap, varianceMap), override = view().edges[e.id] || {}, sig = significance(e, scale);
          var g = svgElement('g', {'class': 'ld-edge ld-' + e.type +
            (e.fixed === true ? ' ld-fixed' : '') + (sig.dotted ? ' ld-nonsignificant' : '') +
            (geo.kind !== 'ordinary' && geo.kind !== e.type ? ' ld-' + geo.kind : '') +
            (selectedEdge === e.id ? ' ld-selected' : ''), 'data-edge-id': e.id,
            'data-edge-type': e.type, 'data-geometry': geo.kind,
            'data-line-type': e.type === 'variance' ? 'residual' : lineType(e),
            'data-from-dock': geo.fromDock, 'data-to-dock': geo.toDock});
          if (e.type === 'variance') {
            g.setAttribute('data-tail-x', geo.tail.x); g.setAttribute('data-tail-y', geo.tail.y);
            g.setAttribute('data-head-x', geo.head.x); g.setAttribute('data-head-y', geo.head.y);
            g.setAttribute('data-residual-angle', geo.angle); g.setAttribute('data-residual-offset', geo.offset);
          }
          if (geo.fromDock === 'residual-tail') g.setAttribute('data-from-variance-id', varianceMap[e.from].edgeID);
          if (geo.toDock === 'residual-tail') g.setAttribute('data-to-variance-id', varianceMap[e.to].edgeID);
          var pathAttrs = {d: geo.path, 'class': 'ld-line', 'pointer-events': 'none', mask: 'url(#' + uid + '-label-gap)', 'marker-end': 'url(#' + uid + '-arrow)'};
          if (e.type === 'covariance') pathAttrs['marker-start'] = 'url(#' + uid + '-arrow)';
          if (geo.connectionSide) g.setAttribute('data-connection-side', geo.connectionSide);
          var line = svgElement('path', pathAttrs);
          g.appendChild(line); scene.appendChild(g);
          if (editing && e.type === 'covariance' && lineType(e) === 'curve') {
            [e.from, e.to].forEach(function (nodeID, index) {
              var endpoint = line.getPointAtLength(index ? line.getTotalLength() : 0);
              covarianceTargets.push({edge: e, nodeID: nodeID, point: {x: endpoint.x, y: endpoint.y}, geo: geo});
            });
          }
          // Relationship labels sit on their paths; variance values follow the
          // outer tail of the short arrow, away from the variable and arrowhead.
          var center = line.getPointAtLength(line.getTotalLength() / 2), textAnchor = 'middle';
          if (!geo.middle) geo.label = {x: center.x, y: center.y};
          if (e.type === 'variance') {
            var vx = geo.tail.x - geo.head.x, vy = geo.tail.y - geo.head.y;
            var vl = Math.max(1, Math.hypot(vx, vy)), nx = vx / vl, ny = vy / vl;
            geo.label = {x: geo.tail.x + nx * 12, y: geo.tail.y + ny * 12};
            if (Math.abs(nx) >= 0.2) {
              textAnchor = nx >= 0 ? 'start' : 'end';
              geo.label.x = geo.tail.x + (nx >= 0 ? 12 : -12);
            }
            else geo.label.y += ny * 6;
          }
          var hit = svgElement('path', {d: geo.path, 'class': 'ld-hit', 'clip-path': 'url(#' + uid + '-hit-clearance)', stroke: 'transparent', 'stroke-width': 14, fill: 'none', 'data-editor-only': 'true'});
          hit.addEventListener('pointerdown', function (event) { if (!editing) return; if (e.type === 'variance') { beginEdge(event, e, 'orbit', geo); return; } event.stopPropagation(); selected.clear(); selectedEdge = e.id; draw(); }); g.appendChild(hit);
          if (editing && geo.middle && selectedEdge === e.id) {
            var middleHit = svgElement('path', {d: 'M' + geo.middle.from.x + ',' + geo.middle.from.y + ' L' + geo.middle.to.x + ',' + geo.middle.to.y,
              'class': 'ld-hit ld-polyline-middle', 'clip-path': 'url(#' + uid + '-hit-clearance)',
              stroke: 'transparent', 'stroke-width': 14, fill: 'none', 'data-editor-only': 'true'});
            middleHit.addEventListener('pointerdown', function (event) { beginEdge(event, e, 'bend', geo); }); g.appendChild(middleHit);
          }
          var label = svgElement('text', {'class': 'ld-edge-text ld-edge-label', 'pointer-events': 'all', 'dominant-baseline': 'middle', style: 'text-anchor:' + textAnchor, x: geo.label.x + (e.type === 'variance' ? 0 : (override.label_dx || 0)), y: geo.label.y + (e.type === 'variance' ? 0 : (override.label_dy || 0))}, edgeLabel(e, scale));
          label.addEventListener('pointerdown', function (event) { beginEdge(event, e, 'label', geo); }); g.appendChild(label);
          if (label.textContent) {
            if (e.type === 'variance') {
              var valueBox = label.getBBox(), outward = geo.side;
              var clearance = Math.min.apply(null, [valueBox.x, valueBox.x + valueBox.width].flatMap(function (x) {
                return [valueBox.y, valueBox.y + valueBox.height].map(function (y) { return (x - geo.tail.x) * outward.x + (y - geo.tail.y) * outward.y; });
              }));
              if (clearance < 12) {
                label.setAttribute('x', Number(label.getAttribute('x')) + outward.x * (12 - clearance));
                label.setAttribute('y', Number(label.getAttribute('y')) + outward.y * (12 - clearance));
              }
            }
            var ink = label.getBBox();
            interactionHoles.push({x: ink.x - 4, y: ink.y - 4, width: ink.width + 8, height: ink.height + 8});
            // One common path mask clears even other crossing lines behind the
            // entire label box, including tiny decimal points and inter-glyph gaps.
            labelMask.appendChild(svgElement('rect', {x: ink.x - 3.5, y: ink.y - 2,
              width: ink.width + 7, height: ink.height + 4, fill: 'black', 'class': 'ld-label-clearance'}));
          }
          var result = model.results[e.id] || {};
          var inference = !sig.inferential ? (e.fixed === true ? 'Fixed original-scale parameter: no raw-scale significance stars or dotted styling.' :
            'Fixed/free status unknown on the raw scale: solid line, no significance stars.') :
            !sig.valid ? 'p is unavailable or invalid on ' + scale + ': solid line, no significance stars; no other-scale substitution.' :
            'p on ' + scale + ' = ' + sig.p + (sig.p >= 0.05 ? ' (not significant at .05).' : ' (significant at .05).');
          if (scale === 'stdyx' && e.fixed === true) inference += ' Raw fixing does not suppress an actual Mplus STDYX p-value.';
          var docking = e.type === 'variance' ? (e.residual === true ? 'Endogenous residual variance; short arrow points inward.' : 'Variance; short arrow points inward.') :
            geo.kind === 'residual-covariance' ? 'Covariance attached to visible residual-arrow tails; statistical identities unchanged.' : '';
          g.appendChild(svgElement('title', {}, e.id + '\nOrigin: ' + (e.origin || 'unknown') + '\nFixed original value: ' + (e.fixed ? e.fixed_value : 'not fixed / unknown') + '\nParameter label: ' + (e.label || 'none') + '\n' + scale + ': ' + JSON.stringify(result[scale] || null) + '\n' + inference + (docking ? '\n' + docking : '') + '\n' + (e.source || '')));
          if (editing && selectedEdge === e.id) {
            var grip = {x: geo.control.x, y: geo.control.y}, labelBox = label.getBBox(), padding = e.type === 'variance' ? 8 : 13;
            var left = labelBox.x - padding, right = labelBox.x + labelBox.width + padding;
            var top = labelBox.y - padding, bottom = labelBox.y + labelBox.height + padding;
            if (label.textContent && grip.x >= left && grip.x <= right && grip.y >= top && grip.y <= bottom) {
              // Centered labels must remain draggable after selecting a path.
              // Move only the editor grip outside the label; the stored curve
              // and its normal-based drag delta keep their original meaning.
              var normalLength = Math.hypot(geo.normal.x, geo.normal.y);
              var nx = normalLength ? geo.normal.x / normalLength : 0;
              var ny = normalLength ? geo.normal.y / normalLength : -1;
              var tx = nx > 0 ? (right - grip.x) / nx : nx < 0 ? (left - grip.x) / nx : Infinity;
              var ty = ny > 0 ? (bottom - grip.y) / ny : ny < 0 ? (top - grip.y) / ny : Infinity;
              var shift = Math.max(28, Math.min(tx, ty) + 2);
              grip.x += nx * shift; grip.y += ny * shift;
              g.insertBefore(svgElement('line', {x1: geo.control.x, y1: geo.control.y, x2: grip.x, y2: grip.y,
                stroke: '#c77c10', 'stroke-width': 1, opacity: 0.55, 'class': 'ld-handle-guide', 'pointer-events': 'none', 'data-editor-only': 'true'}), label);
            }
            interactionHoles.push({x: grip.x - 9, y: grip.y - 9, width: 18, height: 18});
            var handle = svgElement('circle', {cx: grip.x, cy: grip.y, r: 7, fill: '#fff5df', stroke: '#c77c10', 'class': 'ld-handle', 'data-editor-only': 'true'});
            handle.appendChild(svgElement('title', {}, e.type === 'variance' ? 'Drag around the variable to rotate this fixed-length arrow and its value' : geo.middle ? 'Drag to move the polyline routing lane' : 'Drag to adjust path curvature'));
            handle.addEventListener('pointerdown', function (event) { beginEdge(event, e, geo.middle ? 'bend' : 'curve', geo); }); g.appendChild(handle);
          }
        });
        nodes.forEach(function (n) {
          var p = coordinate(n.id), size = nodeSize(n), g = svgElement('g', {'class': 'ld-node ld-' + n.role + (selected.has(n.id) ? ' ld-selected' : ''), transform: 'translate(' + p.x + ',' + p.y + ')', 'data-node-id': n.id});
          g.appendChild(n.role === 'observed' ? svgElement('rect', {'class': 'ld-shape', x: -size.w / 2, y: -size.h / 2, width: size.w, height: size.h, rx: 3}) : svgElement('ellipse', {'class': 'ld-shape', cx: 0, cy: 0, rx: size.w / 2, ry: size.h / 2}));
          var lines = wrap(nodeName(n), 24), text = svgElement('text', {'class': 'ld-node-text', y: -(lines.length - 1) * 8.5 + 5 - (n.role !== 'observed' && n.role !== 'latent' ? 7 : 0)});
          lines.forEach(function (line, i) { text.appendChild(svgElement('tspan', {x: 0, dy: i ? 17 : 0}, line)); }); g.appendChild(text);
          var roles = {random_intercept: 'between · random intercept', within: 'within-person', growth_intercept: 'growth intercept', growth_slope: 'growth slope'};
          if (roles[n.role]) g.appendChild(svgElement('text', {'class': 'ld-role', y: size.h / 2 - 10}, roles[n.role]));
          g.appendChild(svgElement('title', {}, 'Mplus: ' + n.name + '\nRole: ' + n.role + (n.wave != null ? '\nWave/time: ' + n.wave : '')));
          g.addEventListener('pointerdown', function (event) { beginNode(event, n); }); scene.appendChild(g);
        });
        // Invisible hit regions on the existing arrowheads, above node paint.
        // No extra visible endpoint grips: selected edges keep one curve handle.
        var arrowTargets = svgElement('g', {'class': 'ld-covariance-targets', 'data-editor-only': 'true'});
        covarianceTargets.sort(function (a, b) { return Number(a.edge.id === selectedEdge) - Number(b.edge.id === selectedEdge); });
        covarianceTargets.forEach(function (target) {
          var hit = svgElement('circle', {cx: target.point.x, cy: target.point.y, r: 9,
            fill: 'transparent', 'pointer-events': 'all', 'clip-path': 'url(#' + uid + '-hit-clearance)', cursor: 'grab', 'class': 'ld-covariance-endpoint',
            'data-covariance-id': target.edge.id, 'data-node-id-ref': target.nodeID});
          hit.appendChild(svgElement('title', {}, 'Drag this arrowhead to switch both connections ' +
            ((target.geo.connectionSide ? ['left', 'right'].indexOf(target.geo.connectionSide) >= 0 : covarianceAxis(target.edge) === 'x') ? 'left/right' : 'top/bottom') + '; bend stays unchanged'));
          hit.addEventListener('pointerdown', function (event) { beginCovarianceSide(event, target); });
          arrowTargets.appendChild(hit);
        });
        scene.appendChild(arrowTargets);
        var waveGroups = {};
        nodes.forEach(function (n) {
          if (n.wave == null || !Number.isFinite(n.wave)) return;
          var key = String(n.wave); if (!waveGroups[key]) waveGroups[key] = [];
          waveGroups[key].push(coordinate(n.id));
        });
        var waveHeaderY = scene.getBBox().y - 25;
        Object.keys(waveGroups).sort(function (a, b) { return Number(a) - Number(b); }).forEach(function (key) {
          var points = waveGroups[key], x = points.reduce(function (sum, p) { return sum + p.x; }, 0) / points.length;
          var y = o.view === 'full' && (model.structure.family === 'ri_clpm' || o.residuals) ? waveHeaderY : Math.min.apply(null, points.map(function (p) { return p.y; })) - 65;
          scene.appendChild(svgElement('text', {'class': 'ld-note', x: x, y: y, 'text-anchor': 'middle'}, 'Wave / time ' + key));
        });
        // Model title, scale and explanatory legend belong outside the figure.
        // Keep the displayed SVG and both exported formats free of captions.
        var b = scene.getBBox();
        [labelMask, maskBase].forEach(function (item) {
          item.setAttribute('x', b.x - 35); item.setAttribute('y', b.y - 35);
          item.setAttribute('width', b.width + 70); item.setAttribute('height', b.height + 70);
        });
        // SVG masks do not constrain pointer hit testing. Clip every transparent
        // path hit target around all labels and handles, so a later covariance
        // cannot steal their drags. Union rectangles into non-overlapping strips
        // before even-odd subtraction, including overlapping label clearances.
        function rectanglePath(x, y, width, height) { return 'M' + x + ',' + y + 'h' + width + 'v' + height + 'h' + (-width) + 'Z'; }
        var regions = [rectanglePath(b.x - 35, b.y - 35, b.width + 70, b.height + 70)];
        var breaks = Array.from(new Set(interactionHoles.flatMap(function (r) { return [r.x, r.x + r.width]; }))).sort(function (a, b) { return a - b; });
        for (var strip = 0; strip + 1 < breaks.length; strip++) {
          var x0 = breaks[strip], x1 = breaks[strip + 1], probe = (x0 + x1) / 2;
          var intervals = interactionHoles.filter(function (r) { return r.x < probe && r.x + r.width > probe; })
            .map(function (r) { return [r.y, r.y + r.height]; }).sort(function (a, b) { return a[0] - b[0]; });
          var low = null, high = null;
          intervals.forEach(function (interval) {
            if (low === null) { low = interval[0]; high = interval[1]; }
            else if (interval[0] <= high) high = Math.max(high, interval[1]);
            else { regions.push(rectanglePath(x0, low, x1 - x0, high - low)); low = interval[0]; high = interval[1]; }
          });
          if (low !== null) regions.push(rectanglePath(x0, low, x1 - x0, high - low));
        }
        hitRegion.setAttribute('d', regions.join(' '));
        if (!drag) baseBox = {x: b.x - 35, y: b.y - 35, width: Math.max(200, b.width + 70), height: Math.max(160, b.height + 70)};
        applyViewport();
        footer.textContent = 'Temporary report edits · ' + nodes.length + '/' + model.structure.nodes.length + ' nodes · ' + edges.length + '/' + model.structure.edges.length + ' paths' + (editing ? ' · Drag nodes; Shift-click or Shift-drag to select. Use Path to select overlapping lines and Line type to change their shape. Drag a curve handle or a selected polyline middle segment; drag its label. Drag a correlation arrowhead to switch its connection side. Drag a residual arrow or its value around the variable to rotate both at fixed length. Drag background to pan.' : ' · Click Edit diagram to adjust positions and display names.');
        var diagnostics = (model.diagnostics || []).map(function (d) { return d.message; });
        if (!hasStdyx() && !(model.diagnostics || []).some(function (d) { return d.code === 'missing_stdyx'; })) diagnostics.push('STDYX unavailable: the whole diagram uses unstandardized values.');
        diagnosticsMessage.textContent = diagnostics.join('\n');
      }
      function applyViewport() { if (!baseBox) return; var v = view().viewport; svg.setAttribute('viewBox', (baseBox.x + v.x) + ' ' + (baseBox.y + v.y) + ' ' + baseBox.width / v.k + ' ' + baseBox.height / v.k); }
      function point(event, matrix) { var p = svg.createSVGPoint(); p.x = event.clientX; p.y = event.clientY; return p.matrixTransform(matrix || svg.getScreenCTM().inverse()); }
      function capture(event, data) { drag = data; drag.matrix = svg.getScreenCTM().inverse(); drag.start = point(event, drag.matrix); drag.before = snapshot(); svg.setPointerCapture(event.pointerId); event.preventDefault(); }
      function beginNode(event, n) {
        if (!editing || event.button !== 0) return; event.stopPropagation(); selectedEdge = null;
        if (event.shiftKey) { if (selected.has(n.id)) selected.delete(n.id); else selected.add(n.id); } else if (!selected.has(n.id)) { selected.clear(); selected.add(n.id); }
        var original = {}; selected.forEach(function (id) { original[id] = clone(coordinate(id)); });
        capture(event, {type: 'nodes', original: original}); draw();
      }
      function beginEdge(event, edge, type, geo) { if (!editing || event.button !== 0) return; event.stopPropagation(); selected.clear(); selectedEdge = edge.id; var original = clone(view().edges[edge.id] || {});
        if (edge.type === 'variance') {
          capture(event, {type: 'residual-orbit', id: edge.id, center: clone(coordinate(edge.to)), original: original,
            angle: geo.angle, offset: geo.offset, hadOverride: Object.prototype.hasOwnProperty.call(view().edges, edge.id)});
          drag.pointerAngle = Math.atan2(drag.start.y - drag.center.y, drag.start.x - drag.center.x);
          draw(); return;
        }
        if (type === 'curve' && !Object.prototype.hasOwnProperty.call(original, 'curve')) original.curve = edge.type === 'covariance' ? 55 : 0;
        if (type === 'curve' && lineType(edge) === 'straight') original.curve = 0;
        if (type === 'bend') original.bend_offset = geo.bendOffset;
        capture(event, {type: type, id: edge.id, original: original, normal: geo.normal}); draw(); }
      function beginCovarianceSide(event, target) {
        if (!editing || event.button !== 0) return;
        event.stopPropagation(); selected.clear(); selectedEdge = target.edge.id;
        var center = clone(coordinate(target.nodeID)), axis = target.geo.connectionSide ? (['left', 'right'].indexOf(target.geo.connectionSide) >= 0 ? 'x' : 'y') : covarianceAxis(target.edge);
        var side = axis === 'x' ? (target.point.x < center.x ? 'left' : 'right') : (target.point.y < center.y ? 'top' : 'bottom');
        var residualIDs = model.structure.edges.filter(function (edge) {
          return edge.type === 'variance' && edge.residual === true &&
            ((edge.to === target.edge.from && target.geo.fromDock === 'residual-tail') ||
             (edge.to === target.edge.to && target.geo.toDock === 'residual-tail'));
        }).map(function (edge) { return edge.id; });
        capture(event, {type: 'covariance-side', id: target.edge.id, axis: axis, center: center,
          side: side, originalEdges: clone(view().edges), residualIDs: residualIDs}); draw();
      }
      function beginBackground(event) {
        if (event.button !== 0 || event.target.closest('.ld-node,.ld-edge')) return;
        if (editing && event.shiftKey) { selectedEdge = null; capture(event, {type: 'box', oldSelected: new Set(selected)}); }
        else { selected.clear(); selectedEdge = null; capture(event, {type: 'pan', original: clone(view().viewport)}); }
        draw();
      }
      function bounded(value) { return Math.max(-1e6, Math.min(1e6, value)); }
      function movePointer(event) {
        if (!drag) return; var p = point(event, drag.matrix), dx = p.x - drag.start.x, dy = p.y - drag.start.y;
        if (drag.type === 'nodes') Object.keys(drag.original).forEach(function (id) { view().nodes[id] = {x: bounded(drag.original[id].x + dx), y: bounded(drag.original[id].y + dy)}; });
        if (drag.type === 'covariance-side') {
          if (Math.hypot(dx, dy) < 3) return;
          var side = drag.axis === 'x' ? (p.x < drag.center.x ? 'left' : 'right') : (p.y < drag.center.y ? 'top' : 'bottom');
          view().edges = clone(drag.originalEdges);
          if (side !== drag.side) {
            var covariance = clone(view().edges[drag.id] || {}); covariance.covariance_side = side;
            view().edges[drag.id] = covariance;
            drag.residualIDs.forEach(function (id) {
              var residual = clone(view().edges[id] || {});
              delete residual.curve; delete residual.label_dx; delete residual.label_dy;
              residual.residual_angle = {left: 180, right: 0, top: -90, bottom: 90}[side];
              residual.residual_offset = 0; view().edges[id] = residual;
            });
          }
        }
        if (drag.type === 'residual-orbit') {
          var cx = p.x - drag.center.x, cy = p.y - drag.center.y;
          if (Math.hypot(cx, cy) < 1e-6) return;
          var current = Math.atan2(cy, cx), delta = Math.atan2(Math.sin(current - drag.pointerAngle), Math.cos(current - drag.pointerAngle));
          if (Math.abs(delta) < 1e-10) {
            if (drag.hadOverride) view().edges[drag.id] = clone(drag.original); else delete view().edges[drag.id];
          }
          else {
            var orbitOverride = clone(drag.original);
            delete orbitOverride.curve; delete orbitOverride.label_dx; delete orbitOverride.label_dy;
            orbitOverride.residual_angle = normalizedAngle(drag.angle + delta * 180 / Math.PI);
            orbitOverride.residual_offset = drag.offset;
            view().edges[drag.id] = orbitOverride;
          }
        }
        if (drag.type === 'label' || drag.type === 'curve' || drag.type === 'bend') {
          var override = clone(drag.original);
          if (drag.type === 'label') { override.label_dx = bounded((override.label_dx || 0) + dx); override.label_dy = bounded((override.label_dy || 0) + dy); }
          else if (drag.type === 'bend') override.bend_offset = bounded(override.bend_offset + dx * drag.normal.x + dy * drag.normal.y);
          else { override.curve = bounded((override.curve || 0) + dx * drag.normal.x + dy * drag.normal.y); override.line_type = 'curve'; }
          view().edges[drag.id] = override;
        }
        if (drag.type === 'pan') { view().viewport.x = bounded(drag.original.x - dx); view().viewport.y = bounded(drag.original.y - dy); applyViewport(); return; }
        if (drag.type === 'box') {
          selected = new Set(drag.oldSelected); visibleNodes().forEach(function (n) { var c = coordinate(n.id); if (c.x >= Math.min(p.x, drag.start.x) && c.x <= Math.max(p.x, drag.start.x) && c.y >= Math.min(p.y, drag.start.y) && c.y <= Math.max(p.y, drag.start.y)) selected.add(n.id); });
          draw(); svg.appendChild(svgElement('rect', {'class': 'ld-selection-box', x: Math.min(p.x, drag.start.x), y: Math.min(p.y, drag.start.y), width: Math.abs(dx), height: Math.abs(dy), 'data-editor-only': 'true'})); return;
        }
        draw();
      }
      function endPointer(event) { if (!drag) return; record(drag.before); drag = null; if (svg.hasPointerCapture(event.pointerId)) svg.releasePointerCapture(event.pointerId); draw(); }
      function download(blob, filename) { var url = URL.createObjectURL(blob), a = element('a', {href: url, download: filename}); el.appendChild(a); a.click(); a.remove(); setTimeout(function () { URL.revokeObjectURL(url); }, 1000); }
      function exportDocument() {
        var copy = svg.cloneNode(true); copy.querySelectorAll('[data-editor-only]').forEach(function (e) { e.remove(); });
        copy.querySelectorAll('.ld-selected').forEach(function (e) { e.classList.remove('ld-selected'); });
        var b = scene.getBBox(), bounds = {x: b.x - 35, y: b.y - 35, width: Math.ceil(b.width + 70), height: Math.ceil(b.height + 70)};
        copy.setAttribute('xmlns', NS); copy.setAttribute('viewBox', [bounds.x, bounds.y, bounds.width, bounds.height].join(' ')); copy.setAttribute('width', bounds.width); copy.setAttribute('height', bounds.height); copy.removeAttribute('class'); copy.setAttribute('style', 'background:white');
        return {xml: new XMLSerializer().serializeToString(copy), width: bounds.width, height: bounds.height};
      }
      function exportSVG() { download(new Blob([exportDocument().xml], {type: 'image/svg+xml;charset=utf-8'}), 'model-diagram.svg'); }
      function exportPNG() {
        var doc = exportDocument(), factor = Number(controls.pngScale.value);
        if (doc.width * factor > 16384 || doc.height * factor > 16384 || doc.width * doc.height * factor * factor > 1e8) { notify('PNG dimensions exceed the browser export limit. Reduce PNG scale, move distant nodes closer, or export SVG.'); return; }
        var url = URL.createObjectURL(new Blob([doc.xml], {type: 'image/svg+xml;charset=utf-8'})), img = new Image();
        img.onload = function () {
          try {
            var canvas = document.createElement('canvas'); canvas.width = doc.width * factor; canvas.height = doc.height * factor;
            var ctx = canvas.getContext('2d'); if (!ctx) throw Error('Canvas is unavailable.');
            ctx.fillStyle = '#fff'; ctx.fillRect(0, 0, canvas.width, canvas.height); ctx.drawImage(img, 0, 0, canvas.width, canvas.height);
            canvas.toBlob(function (blob) { if (blob) download(blob, 'model-diagram.png'); else notify('PNG export failed. Try a smaller PNG scale or export SVG.'); }, 'image/png');
          } catch (e) { notify('PNG export failed: ' + e.message); } finally { URL.revokeObjectURL(url); }
        };
        img.onerror = function () { URL.revokeObjectURL(url); notify('PNG export failed. Export SVG to preserve the current figure.'); }; img.src = url;
      }
      return {
        renderValue: function (x) {
          payload = x; model = clone(x.model); state = clone(x.state); automatic = clone(x.automatic);
          // R/jsonlite represents some empty named lists as arrays; normalize only
          // the trusted initial payload. Imported files are validated strictly.
          ['full', 'simplified'].forEach(function (name) { state.views[name].edges = dict(state.views[name].edges); automatic[name].edges = dict(automatic[name].edges); });
          state.aliases = dict(state.aliases); state.options = optionalDefaults(state.options); model.results = dict(model.results);
          if (!Object.prototype.hasOwnProperty.call(state, 'hidden_nodes')) state.hidden_nodes = [];
          editing = !!x.editor; selected.clear(); selectedEdge = null; history = []; future = []; drag = null;
          build(); draw();
        },
        resize: function () { if (svg) applyViewport(); }
      };
    }
  });
}());
