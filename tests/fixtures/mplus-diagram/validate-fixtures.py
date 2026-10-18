#!/usr/bin/env python3
"""Audit fixed bytes and literal expectations without Lin or MplusAutomation.

Run from the repository root. This is a source-table transcription check, not
an implementation acceptance test; public-workflow tests consume these JSONs.
No network, private data, R package, or Mplus engine is used.
"""
from pathlib import Path
import hashlib
import json
import re

root = Path(__file__).resolve().parents[3]
manifest = json.loads((root / 'inst/extdata/mplus-diagram/manifest.json').read_text())
checks = 0
p_checks = 0
p_valid = 0
p_unavailable = 0
fixed_with_valid_stdyx_p = 0
for entry in manifest['fixtures']:
    file = root / entry['path']
    assert hashlib.sha256(file.read_bytes()).hexdigest() == entry['sha256'], file
    text = file.read_text()
    lines = text.splitlines()
    assert text.splitlines()[0] == entry['engine'], file
    if 'input_path' in entry:
        inp = root / entry['input_path']
        assert hashlib.sha256(inp.read_bytes()).hexdigest() == entry['input_sha256'], inp
    if not entry['expected_path']:
        continue
    expected = json.loads((root / entry['expected_path']).read_text())
    assert expected['pvalue_source_output'] == entry['path']
    # All expected values are committed literals. Only comparison reads output.
    for scale, heading in [('unstandardized', 'MODEL RESULTS'),
                           ('stdyx', 'STDYX Standardization')]:
        if heading not in text:
            assert all(e[scale] is None and e[scale + '_p'] is None and
                       e['p_sources'][scale] is None for e in expected['relationships'])
            continue
        section_start = text.index(heading) + len(heading)
        table = text[section_start:]
        table = table.split('QUALITY OF NUMERICAL RESULTS')[0] if scale == 'unstandardized' else table.split('R-SQUARE')[0]
        for edge in expected['relationships']:
            if edge[scale] is None:
                continue
            pattern = (r'(?m)^\s*' + re.escape(edge['lhs']) + r'\s+' +
                       re.escape(edge['operator']) + r'\s*\n((?:\s+[^\n]+\n)+)')
            header = re.search(pattern, table)
            assert header, (file, scale, edge, 'missing header')
            row = re.search(r'(?m)^[ \t]*' + re.escape(edge['rhs']) +
                            r'[ \t]+([-\d.]+)[ \t]+([-\d.]+)[ \t]+([-\d.]+)[ \t]+([-\d.]+)[ \t]*$', header.group(1))
            assert row and float(row.group(1)) == edge[scale], (file, scale, edge, row.group(1) if row else None)
            checks += 1
            # P-values are checked against the P-Value column, independently of
            # the estimate. A legitimate estimate 999 must never be discarded.
            source = edge['p_sources'][scale]
            p_literal = row.group(4)
            p_number = float(p_literal)
            p_expected = p_number if 0 <= p_number <= 1 else None
            assert p_expected is not None or p_number == 999, (file, edge, p_literal)
            assert edge[scale + '_p'] == p_expected, (file, scale, edge, 'p value')
            line_number = text[:section_start + header.start(1) + row.start()].count('\n') + 1
            assert source['line_number'] == line_number, (file, edge, 'source line')
            assert source['row'] == lines[line_number - 1].strip()
            assert source['section'] == heading and source['p_literal'] == p_literal
            if p_expected is None:
                assert 'P-Value column placeholder 999' in source['unavailable_reason']
                p_unavailable += 1
            else:
                assert source['unavailable_reason'] is None
                p_valid += 1
                fixed_with_valid_stdyx_p += bool(edge['fixed'] and scale == 'stdyx')
            p_checks += 1
        for edge in expected['relationships']:
            if edge['fixed']:
                assert edge['fixed_value'] == edge['unstandardized']
    if 'stdyx_available' in expected:
        assert expected['stdyx_available'] == entry['stdyx_available']
print(f"Verified {len(manifest['fixtures'])} fixed output hashes, {checks} literal relationship estimates, "
      f"and {p_checks} source-referenced P-Value cells ({p_valid} valid, {p_unavailable} placeholder-null). "
      f"{fixed_with_valid_stdyx_p} originally fixed relationships retain valid STDYX p-values, usable for STDYX inference; raw fixed-parameter inference remains suppressed.")
