#!/usr/bin/env python3
"""Optional maintainer reproduction. Does not run Mplus; never used by CI.

python3 prepare-data.py /tmp/lin-fixture-reproduction \
  --public-examples /Applications/Mplus/Examples/usersguide
Copy inputs/*.inp to that temporary directory, then run a licensed Mplus there.
The normal package/CI workflow reads the committed .out files only.
"""
import argparse
from pathlib import Path
import random
import shutil

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('destination', type=Path)
parser.add_argument('--public-examples', type=Path)
args = parser.parse_args()
args.destination.mkdir(parents=True, exist_ok=True)
if args.public_examples:
    for example in ['ex5.20', 'ex5.11', 'ex5.12', 'ex3.11', 'ex6.1']:
        shutil.copyfile(args.public_examples / (example + '.dat'),
                        args.destination / (example + '.dat'))
rng = random.Random(741920)
rows = []
for _ in range(1000):
    ix, iy = rng.gauss(0, 1), rng.gauss(0, 1)
    xs, ys = [], []
    x, y = rng.gauss(0, 1), rng.gauss(0, 1)
    for wave in range(3):
        if wave:
            x, y = (.45*x + .15*y + rng.gauss(0, .8),
                    -.1*x + .5*y + rng.gauss(0, .8))
        xs.append(ix + x)
        ys.append(iy + y)
    rows.append(xs + ys)
(args.destination / 'panel.dat').write_text(
    '\n'.join(' '.join(f'{v:.10f}' for v in row) for row in rows) + '\n')
# Full factorial orthogonal design yields a legitimate estimated slope 999,
# with another estimated slope exactly zero; neither is a fixed parameter.
rows = [(x, z, 999*x + e) for _ in range(125)
        for x in [-.01, .01] for z in [-1, 1] for e in [-.2, .2]]
(args.destination / 'special.dat').write_text(
    '\n'.join(' '.join(str(v) for v in row) for row in rows) + '\n')
