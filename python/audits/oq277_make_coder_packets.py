#!/usr/bin/env python3
"""OQ-277 — build the coder-facing packet from an extraction file.

Two things this does that a hand-written file would get wrong:

1. OPAQUE IDS. Wu's own ids name their subject (`movespeed_tcc_sandbox`,
   `whatsapp_client_display_folding`), which leaks both the source system and, for some,
   the mechanism. The coder sees `i-01`.. only; the mapping is written to a separate file
   that is never assembled into a payload.

2. SHUFFLED EMISSION ORDER. The extraction file is in catalog order, which is
   A,B,B,B,B,C,C,C,C,C,D,D,D,D,E,E,E,E,E,E,E,E — contiguous class blocks. Emitting in that
   order hands a coder the class grouping without a single class name being present. The
   shuffle is seeded (frame seed 20260810) so it is reproducible, and the resulting class
   sequence is written into the map file so the de-blocking is auditable rather than
   asserted.

The output is swept by oq277_lexicon before it is written; a leak aborts the build.

Usage:
  python3 python/audits/oq277_make_coder_packets.py \
      --units packets/wu_units.json --direction i --out packets/coder_direction_i.json \
      --map packets/wu_unit_id_map.json
"""
from __future__ import annotations
import argparse, json, random, sys, pathlib

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
import oq277_lexicon as LEX

SEED = 20260810  # frame seed, reused so every draw in this audit shares one recorded seed


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--units", required=True)
    ap.add_argument("--direction", choices=["i", "ii"], required=True)
    ap.add_argument("--out", required=True)
    ap.add_argument("--map", required=True)
    a = ap.parse_args()

    src = json.load(open(a.units))
    units = src["units"]

    order = list(range(len(units)))
    random.Random(SEED).shuffle(order)

    coder, mapping = [], []
    for n, idx in enumerate(order, 1):
        u = units[idx]
        oid = f"{a.direction}-{n:02d}"
        coder.append({"id": oid, **{f: u[f] for f in LEX.CODER_FACING_FIELDS}})
        mapping.append({
            "opaque_id": oid,
            "source_id": u["id"],
            "catalog_class": u["metadata"].get("catalog_class"),
            "dataset_class": u["metadata"].get("dataset_class"),
            "agreeing": u["metadata"].get("agreeing"),
        })

    # Gate: the packet is swept BEFORE it is written. A leak aborts rather than warns.
    blob = json.dumps(coder, ensure_ascii=False)
    hits = LEX.scan(blob, a.direction)
    if hits:
        print(f"ABORT — {len(hits)} banned term(s) in the assembled packet:", file=sys.stderr)
        for g, p, t, c in hits[:20]:
            print(f"  [{g}] {p} -> {t!r}\n     ...{c}...", file=sys.stderr)
        return 1

    def max_run(s):
        """Longest contiguous same-class run in s. Written as an explicit loop after a
        groupby one-liner silently returned 2 for every input: `for g in groupby(...)`
        yields (key, grouper) TUPLES, so `sum(1 for _ in g)` counted the tuple, not the
        group. It made a blocked baseline look de-blocked — a plausible number measuring
        the wrong object."""
        best = cur = 1
        for x, y in zip(s, s[1:]):
            cur = cur + 1 if x == y else 1
            best = max(best, cur)
        return best if s else 0

    seq = [m["catalog_class"] for m in mapping]
    emitted_run = max_run(seq)
    blocked_run = max_run(sorted(seq))   # what catalog order would have handed the coder

    pathlib.Path(a.out).write_text(json.dumps(coder, indent=2, ensure_ascii=False) + "\n")
    pathlib.Path(a.map).write_text(json.dumps({
        "_warning": "NOT CODER-FACING. Never assemble this file, or any field of it, into a payload.",
        "seed": SEED,
        "source_units": a.units,
        "emitted_catalog_class_sequence": seq,
        "max_contiguous_same_class_run": emitted_run,
        "blocked_input_max_run": blocked_run,
        "map": mapping,
    }, indent=2) + "\n")

    print(f"wrote {a.out}: {len(coder)} units, fields {LEX.CODER_FACING_FIELDS}")
    print(f"wrote {a.map}: opaque->source mapping (NOT coder-facing)")
    print(f"leak sweep over the assembled packet: 0 hits")
    print(f"emitted class sequence: {''.join(seq)}")
    print(f"max contiguous same-class run: {emitted_run} "
          f"(catalog-ordered input would be {blocked_run})")
    return 0


if __name__ == "__main__":
    sys.exit(main())
