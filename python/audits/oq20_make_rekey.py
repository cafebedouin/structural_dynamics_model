#!/usr/bin/env python3
"""OQ-20 re-key map: filename base -> canonical (in-file) constraint id.

The two code eras label per_constraint `id` differently (tag = in-file
constraint_metric subject via known_constraint/1; HEAD = filename base via
corpus_constraint/1, commit 801390a5). To compare DR *values* across eras the
HEAD (filename-keyed) rows must be remapped into the tag's canonical id space.

CIRCULARITY GUARD (operator flag): this map is derived ONLY from the source
`.pl` files (the first constraint_metric subject), never from matching DR output
fields. It is orthogonal to every field under comparison.

Controls:
  - no collisions (two filenames -> same canonical id) unless --allow-collisions
  - every file resolves (subject found, or explicit filename fallback logged)
Emits JSON {filename_base: canonical_id} to stdout or --out.
"""
import sys, re, glob, os, json, argparse
from collections import Counter

SUBJ = re.compile(r'\s*narrative_ontology:constraint_metric\(([A-Za-z0-9_]+),')


def build(src_dir):
    fmap, fallback = {}, []
    for p in sorted(glob.glob(os.path.join(src_dir, '*.pl'))):
        base = os.path.splitext(os.path.basename(p))[0]
        subj = None
        with open(p) as f:
            for line in f:
                m = SUBJ.match(line)
                if m:
                    subj = m.group(1); break
        if subj is None:
            # file whose in-file id is unparseable as a fact subject (e.g. a
            # digit-leading atom the tag itself drops). Fall back to the
            # filename base; flagged so it never silently masquerades as a match.
            subj = base
            fallback.append(base)
        fmap[base] = subj
    return fmap, fallback


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument('src_dir')
    ap.add_argument('--out')
    ap.add_argument('--allow-collisions', action='store_true')
    args = ap.parse_args()
    fmap, fallback = build(args.src_dir)
    coll = {k: v for k, v in Counter(fmap.values()).items() if v > 1}
    print(f"[rekey] {len(fmap)} files; {len(fallback)} filename-fallback "
          f"(unparseable in-file id): {fallback[:10]}", file=sys.stderr)
    if coll:
        print(f"[rekey] COLLISIONS ({len(coll)}): {dict(list(coll.items())[:10])}",
              file=sys.stderr)
        if not args.allow_collisions:
            print("[rekey] ABORT: collisions present (use --allow-collisions to "
                  "accept, e.g. OQ-25 chimera id-reuse)", file=sys.stderr)
            sys.exit(1)
    else:
        print("[rekey] control PASS: no collisions (clean bijection)", file=sys.stderr)
    txt = json.dumps(fmap, indent=0)
    if args.out:
        open(args.out, 'w').write(txt)
        print(f"[rekey] wrote {args.out}", file=sys.stderr)
    else:
        print(txt)


if __name__ == '__main__':
    main()
