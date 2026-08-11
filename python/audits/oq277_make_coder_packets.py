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

--------------------------------------------------------------------------------------
RUN MODE (`--build-run`, added 2026-08-11) — assemble the three FULL packets that are
actually sent.
--------------------------------------------------------------------------------------
The original mode above built `packets/coder_direction_i.json` at step 2, BEFORE the
controls existed: it holds Wu's 22 redacted units and no anchors, no decoys, no twins.
Run mode assembles the complete packets into `packets/run/`.

INTERLEAVING LOCATION — DECIDED, and recorded here because HANDOFF_TWINS_AND_DRIVER.md
§1.5 says either choice is defensible and leaving it implicit is not:

    FULL PACKETS ARE ASSEMBLED HERE AND THE PACKET IS THE RECORD.
    THE DRIVER ONLY SENDS.

Full packets win because the freeze needs an md5-able artifact that IS what was sent. If
the driver interleaved at send time, the payload dump would be the only authoritative
record and the packet would be a mere unit source — auditable only after the spend.

WHAT RUN MODE MUST NOT DO. `packets/coder_direction_i.json` is the frozen step-2 artifact
and HANDOFF.md prohibits re-running the seeded shuffle over it. Run mode READS it and
preserves its 22 items' relative order and opaque ids `i-01`..`i-22` EXACTLY, inserting
the anchors, decoys and twin arms at seeded positions around them. It never rewrites it.

QUARANTINE keys on `matrix_unit`, and on nothing else (§I.2) — never on `role`, never on
`overlap_source` alone, which yields 18 cells where the ruling says 22.

CODER-FACING SURFACE. A packet item is `{id} + the four fields` and nothing more. Role,
true label, matrix_unit, source id and quarantine status live in the sibling `*_map.json`
carrying the NOT CODER-FACING header. The opaque id is a bookkeeping key for the driver;
the rendered prompt shows only the four fields, so no ordinal ever reaches a coder.

Usage:
  python3 python/audits/oq277_make_coder_packets.py \
      --units packets/wu_units.json --direction i --out packets/coder_direction_i.json \
      --map packets/wu_unit_id_map.json

  python3 python/audits/oq277_make_coder_packets.py --build-run [--audit DIR]
"""
from __future__ import annotations
import argparse, glob, hashlib, json, os, random, sys, pathlib

sys.path.insert(0, str(pathlib.Path(__file__).resolve().parent))
import oq277_lexicon as LEX

SEED = 20260810  # frame seed, reused so every draw in this audit shares one recorded seed
FIELDS = LEX.CODER_FACING_FIELDS


def max_run(s):
    """Longest contiguous same-key run in s. Written as an explicit loop after a groupby
    one-liner silently returned 2 for every input: `for g in groupby(...)` yields
    (key, grouper) TUPLES, so `sum(1 for _ in g)` counted the tuple, not the group. It
    made a blocked baseline look de-blocked — a plausible number measuring the wrong
    object. Hoisted to module scope for run mode; behaviour unchanged."""
    best = cur = 1
    for x, y in zip(s, s[1:]):
        cur = cur + 1 if x == y else 1
        best = max(best, cur)
    return best if s else 0


def _fields(d):
    return {f: d[f] for f in FIELDS}


def _md5(item):
    """Identity of an item's CODER-FACING CONTENT, not its label.

    §P standing hazard: three self-comparisons have been caught in this arc, each an
    apparatus measuring agreement between two things that are not independent and
    reporting it at full confidence, because agreement is what a working version produces
    too. The live version here is an anchor, decoy or twin arm that is secretly the same
    TEXT as a unit — it would code identically and read as perfect agreement. Labels
    cannot catch that; only content identity can."""
    blob = "\x00".join(str(item[f]) for f in FIELDS)
    return hashlib.md5(blob.encode("utf-8")).hexdigest()


def _seeded_insert(frozen, extras, seed):
    """Insert `extras` at seeded positions among `frozen`, preserving frozen's RELATIVE
    ORDER exactly. Returns the merged list.

    Not a shuffle of the whole: re-shuffling the 22 is prohibited (HANDOFF.md). The draw
    chooses insertion slots only, so the frozen subsequence is recoverable from the output
    and is asserted below rather than trusted.
    """
    rng = random.Random(seed)
    out = list(frozen)
    for e in extras:
        out.insert(rng.randrange(len(out) + 1), e)
    return out


def _sweep(items, direction, exempt_ids, label, errors):
    """Three-way sweep. Exempt items MUST fire (an unredacted twin that sweeps clean
    un-redacted nothing and reports a floor of zero by construction); everything else
    MUST be clean. A pre-listed exemption that stays silent is a failure, not a pass."""
    other = "i" if direction == "ii" else "ii"
    # Cross-direction check is scoped to source_identifying ONLY. Sweeping a direction-(ii)
    # item under the full direction-(i) list would fire on shared incident vocabulary;
    # source_identifying is the group that must hold in both directions.
    cross = {other: {"source_identifying":
                     LEX.LEXICON_DETECT[other]["source_identifying"]}}
    for it in items:
        blob = " ".join(str(it[f]) for f in FIELDS)
        hits = LEX.scan(blob, direction)
        xhits = LEX.scan(blob, other, cross)
        if it["id"] in exempt_ids:
            if not hits:
                errors.append(f"{label}: EXEMPT item {it['id']} swept CLEAN — it was supposed "
                              f"to fire. Nothing was un-redacted; this pair reports a floor "
                              f"of zero by construction.")
        elif hits:
            errors.append(f"{label}: LEAK in {it['id']} — "
                          f"{sorted({m for _g, _p, m, _c in hits})}")
        if xhits:
            errors.append(f"{label}: CROSS-DIRECTION source-identifying leak in {it['id']} — "
                          f"{sorted({m for _g, _p, m, _c in xhits})}")


def gates(name, items, meta, exempt, sweep_dir, frozen_ref, frozen_ids_expected):
    """Every pre-write gate, in one place so it can be run against DELIBERATELY BROKEN
    input. Returns a list of error strings; empty means pass.

    Extracted rather than left inline because all four gates passed on the first assembly,
    and a gate that has only ever passed is indistinguishable from a gate that cannot
    fail. `--selftest` breaks each one on purpose and requires it to fire."""
    errors: list[str] = []
    # (a) frozen subsequence preserved exactly, in order and in text
    if frozen_ids_expected is not None:
        got = [x["id"] for x in items if x["id"] in set(frozen_ids_expected)]
        if got != frozen_ids_expected:
            errors.append(f"{name}: frozen order NOT preserved — got {got}")
        byid = {x["id"]: x for x in items}
        for fr in frozen_ref:
            if fr["id"] not in byid:
                errors.append(f"{name}: frozen item {fr['id']} MISSING from the packet")
            elif _md5(byid[fr["id"]]) != _md5(fr):
                errors.append(f"{name}: frozen item {fr['id']} text CHANGED")
    # (b) content identity — §P self-comparison guard
    seen = {}
    for it in items:
        h = _md5(it)
        if h in seen:
            errors.append(
                f"{name}: IDENTICAL coder-facing text in {seen[h]} and {it['id']} "
                f"({meta[seen[h]]['role']} vs {meta[it['id']]['role']}) — a "
                f"self-comparison that would report agreement at full confidence")
        seen[h] = it["id"]
    # (c) coder-facing surface is exactly id + the four fields
    for it in items:
        if set(it) != {"id", *FIELDS}:
            errors.append(f"{name}: item {it['id']} carries non-coder-facing keys "
                          f"{sorted(set(it) - {'id', *FIELDS})}")
    # (d) three-way leak sweep
    _sweep(items, sweep_dir, exempt, name, errors)
    return errors


def selftest() -> int:
    """Break each gate on purpose; every one MUST fire. A packet builder whose gates have
    only ever been green is not a builder with working gates."""
    ok = True

    def check(label, errs, want_substr):
        nonlocal ok
        fired = any(want_substr in e for e in errs)
        print(f"  {'PASS' if fired else 'FAIL'}  {label}")
        if not fired:
            print(f"        gate did NOT fire; errors were: {errs}")
        ok = ok and fired

    def check_bool(label, cond):
        nonlocal ok
        print(f"  {'PASS' if cond else 'FAIL'}  {label}")
        ok = ok and cond

    base = [{"id": "x-01", "symptom": "a counter read zero on every input",
             "mechanism_as_described": "the enumerator returned only its first solution",
             "detection_path": "a probe with hand-computed expected values",
             "consequence": "every reading was a failure that looked like a measurement"},
            {"id": "x-02", "symptom": "an artifact stopped being regenerated",
             "mechanism_as_described": "the step was never wired into the orchestrator",
             "detection_path": "comparing its row count against the live corpus",
             "consequence": "a stale file read as current for months"}]
    meta = {"x-01": {"role": "unit"}, "x-02": {"role": "anchor"}}

    print("gate discrimination controls — each MUST fire on a deliberate break:\n")

    dup = [base[0], {**base[1], **{f: base[0][f] for f in FIELDS}}]
    check("(b) identical coder-facing text in two items",
          gates("t", dup, meta, set(), "i", None, None), "IDENTICAL coder-facing text")

    # Two-sided: a gate that fires on everything is as useless as one that fires on
    # nothing, and both look like a working gate from the failing side alone.
    check_bool("(b) CONVERSE — a clean packet trips NO gate",
               gates("t", base, meta, set(), "i", None, None) == [])

    extra = [{**base[0], "true_label": "P1"}, base[1]]
    check("(c) a non-coder-facing key leaked onto an item",
          gates("t", extra, meta, set(), "i", None, None), "non-coder-facing keys")

    leaky = [{**base[0], "mechanism_as_described": "a textbook Class B fail-plausible case"},
             base[1]]
    check("(d) a banned term in a NON-exempt item",
          gates("t", leaky, meta, set(), "i", None, None), "LEAK in x-01")

    check("(d) an EXEMPT item that swept CLEAN (nothing was un-redacted)",
          gates("t", base, meta, {"x-01"}, "i", None, None), "swept CLEAN")

    cross = [{**base[0], "consequence": "filed as OQ-999 in the tracker"}, base[1]]
    check("(d) cross-direction source-identifying term in a direction-(i) item",
          gates("t", cross, meta, set(), "i", None, None), "CROSS-DIRECTION")

    fz = [dict(base[0]), dict(base[1])]
    perm = [base[1], base[0]]
    check("(a) frozen relative order permuted",
          gates("t", perm, meta, set(), "i", fz, ["x-01", "x-02"]), "order NOT preserved")

    edited = [{**base[0], "symptom": "quietly reworded"}, base[1]]
    check("(a) frozen item text edited",
          gates("t", edited, meta, set(), "i", fz, ["x-01", "x-02"]), "text CHANGED")

    check("(a) frozen item dropped entirely",
          gates("t", [base[1]], meta, set(), "i", fz, ["x-01", "x-02"]), "MISSING from the packet")

    print(f"\n{'GREEN — every gate discriminates' if ok else 'RED — a gate cannot fail'}")
    return 0 if ok else 1


def build_run(audit: pathlib.Path) -> int:
    P, C = audit / "packets", audit / "controls"
    out_dir = P / "run"
    out_dir.mkdir(exist_ok=True)
    errors: list[str] = []
    summary = []

    anchors = json.load(open(C / "anchors.json"))
    decoys = json.load(open(C / "decoys.json"))["decoys"]
    wu = {u["id"]: u for u in json.load(open(P / "wu_units.json"))["units"]}
    twins_i = json.load(open(C / "redaction_twins_direction_i.json"))["pairs"]
    twins_ii = json.load(open(C / "redaction_twins_direction_ii.json"))["pairs"]

    # ---------------- direction (i) ----------------
    frozen = json.load(open(P / "coder_direction_i.json"))
    frozen_ids = [x["id"] for x in frozen]
    items_i = [{"id": x["id"], **_fields(x)} for x in frozen]
    meta_i = {x["id"]: {"role": "unit", "matrix_unit": True, "quarantined": False}
              for x in frozen}

    n = len(frozen)
    extras_i = []
    for a in anchors["direction_i"]["anchors"]:
        n += 1
        oid = f"i-{n:02d}"
        extras_i.append({"id": oid, **_fields(a)})
        meta_i[oid] = {"role": "anchor", "matrix_unit": False, "quarantined": True,
                       "source_id": a["id"], "true_label": a["true_label"]}
    for d in decoys:
        n += 1
        oid = f"i-{n:02d}"
        extras_i.append({"id": oid, **_fields(d)})
        meta_i[oid] = {"role": "decoy", "matrix_unit": False, "quarantined": True,
                       "source_id": d["id"], "true_label": d["want"]}
    exempt_i = set()
    for t in twins_i:
        n += 1
        oid = f"i-{n:02d}"
        extras_i.append({"id": oid, **_fields(t["unredacted"])})
        meta_i[oid] = {"role": "twin_unredacted", "matrix_unit": False, "quarantined": True,
                       "leak_exempt": True, "source_id": t["pair_id"],
                       "pairs_with_source_unit": t["unit_id"]}
        exempt_i.add(oid)

    items_i = _seeded_insert(items_i, extras_i, SEED)
    summary.append(("direction_i", items_i, meta_i, exempt_i, "i", frozen_ids))

    # ---------------- direction (ii) ----------------
    our = [json.load(open(f)) for f in sorted(glob.glob(str(P / "our_units" / "*.json")))]
    our_stems = [pathlib.Path(f).stem
                 for f in sorted(glob.glob(str(P / "our_units" / "*.json")))]
    order = list(range(len(our)))
    random.Random(SEED).shuffle(order)
    items_ii, meta_ii = [], {}
    for k, idx in enumerate(order, 1):
        u, stem = our[idx], our_stems[idx]
        oid = f"ii-{k:02d}"
        items_ii.append({"id": oid, **_fields(u)})
        meta_ii[oid] = {"role": "unit", "matrix_unit": bool(u.get("matrix_unit")),
                        "quarantined": not bool(u.get("matrix_unit")),
                        "source_id": stem, "source_dir": u.get("source_dir"),
                        "overlap_source": bool(u.get("overlap_source"))}

    n = len(items_ii)
    extras_ii = []
    for a in anchors["direction_ii"]["anchors"]:
        n += 1
        oid = f"ii-{n:02d}"
        # Text is NOT re-extracted: it is taken verbatim from the already-redacted Wu unit,
        # exactly as anchors.json's `text_source` declares, so the anchor and the
        # direction-(i) packet cannot drift apart.
        extras_ii.append({"id": oid, **_fields(wu[a["wu_unit_id"]])})
        meta_ii[oid] = {"role": "anchor", "matrix_unit": False, "quarantined": True,
                        "source_id": a["id"], "true_label": a["true_label"],
                        "text_from": a["wu_unit_id"]}
    for d in decoys:
        n += 1
        oid = f"ii-{n:02d}"
        extras_ii.append({"id": oid, **_fields(d)})
        meta_ii[oid] = {"role": "decoy", "matrix_unit": False, "quarantined": True,
                        "source_id": d["id"], "true_label": d["want"]}
    exempt_ii = set()
    for t in twins_ii:
        n += 1
        oid = f"ii-{n:02d}"
        extras_ii.append({"id": oid, **_fields(t["unredacted"])})
        meta_ii[oid] = {"role": "twin_unredacted", "matrix_unit": False, "quarantined": True,
                        "leak_exempt": True, "source_id": t["pair_id"],
                        "pairs_with_source_unit": t["unit_id"],
                        "restoration_kind": t.get("restoration_kind"),
                        "set_membership": t.get("set_membership")}
        exempt_ii.add(oid)

    items_ii = _seeded_insert(items_ii, extras_ii, SEED + 1)
    summary.append(("direction_ii", items_ii, meta_ii, exempt_ii, "ii", None))

    # ---------------- (iii') ----------------
    ip_dir = P / "iii_prime_units"
    ip_files = sorted(glob.glob(str(ip_dir / "[0-9]*.json")))
    if not ip_files:
        # DECLARED, never silently emitted as an empty packet. An empty packet written
        # without comment is precisely the absence-satisfies-the-gate shape: downstream
        # would compute expected_calls from it and get a smaller, self-consistent total.
        print("\n(iii') packet: NOT BUILT — packets/iii_prime_units/ has no unit files yet.")
        print("  This is the pending hand-back from HANDOFF_IIIPRIME_EXTRACTOR.md (7 units).")
        print("  Declared, not skipped: the run packet is absent, NOT empty, and the")
        print("  expected-call total below is stated as INCOMPLETE rather than as a total.")
    else:
        items_ip, meta_ip = [], {}
        for k, f in enumerate(ip_files, 1):
            u = json.load(open(f))
            oid = f"iii-{k:02d}"
            items_ip.append({"id": oid, **_fields(u)})
            meta_ip[oid] = {"role": "iii_prime_unit", "matrix_unit": False,
                            "quarantined": True, "source_id": pathlib.Path(f).stem,
                            "true_label": u.get("true_label"),
                            "label_source": u.get("label_source")}
        summary.append(("iii_prime", items_ip, meta_ip, set(), "ii", None))

    # ---------------- gates, then write ----------------
    total_items = 0
    for name, items, meta, exempt, sweep_dir, frozen_ids_expected in summary:
        errors += gates(name, items, meta, exempt, sweep_dir,
                        frozen if frozen_ids_expected is not None else None,
                        frozen_ids_expected)
        total_items += len(items)

    if errors:
        print(f"\nABORT — {len(errors)} gate failure(s); NOTHING written:\n", file=sys.stderr)
        for e in errors:
            print(f"  {e}", file=sys.stderr)
        return 1

    for name, items, meta, exempt, sweep_dir, _fz in summary:
        (out_dir / f"coder_{name}.json").write_text(
            json.dumps(items, indent=2, ensure_ascii=False) + "\n")
        roles = [meta[x["id"]]["role"] for x in items]
        (out_dir / f"coder_{name}_map.json").write_text(json.dumps({
            "_warning": "NOT CODER-FACING. Never assemble this file, or any field of it, "
                        "into a payload.",
            "seed": SEED,
            "sweep_direction": sweep_dir,
            "n_items": len(items),
            "n_matrix_cells": sum(1 for m in meta.values() if m.get("matrix_unit")),
            "n_quarantined": sum(1 for m in meta.values() if m.get("quarantined")),
            "leak_exempt_ids": sorted(exempt),
            "emitted_role_sequence": "".join(r[0] for r in roles),
            "max_contiguous_same_role_run": max_run(roles),
            "blocked_input_max_run": max_run(sorted(roles)),
            "map": meta,
        }, indent=2, ensure_ascii=False) + "\n")

    print("\n=== run packets assembled — the packet IS the record; the driver only sends ===")
    for name, items, meta, exempt, sweep_dir, _fz in summary:
        cells = sum(1 for m in meta.values() if m.get("matrix_unit"))
        print(f"  packets/run/coder_{name}.json  {len(items):>3} items  "
              f"{cells:>2} matrix cells  {len(exempt)} leak-exempt  sweep=dir({sweep_dir})")
    built = {s[0] for s in summary}
    complete = built == {"direction_i", "direction_ii", "iii_prime"}
    print(f"\n  items: {total_items}   calls at k=3: {total_items * 3}")
    print(f"  escape units: 0 calls — the escape row is CLOSED UNRESOLVED and there is no "
          f"pre-registered escape-coding row for such data to land in")
    if not complete:
        print(f"\n  *** INCOMPLETE — {sorted({'direction_i','direction_ii','iii_prime'} - built)} "
              f"not built. The totals above are a PARTIAL, not the expected call count. ***")
    print("\n  gates passed: frozen-order preserved · frozen text unchanged · content-md5 "
          "unique within each packet · coder surface = id + 4 fields · three-way leak sweep "
          "(exempt fired, rest clean, no cross-direction source-identifying leak)")
    return 0


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("--build-run", action="store_true",
                    help="assemble the three full run packets into packets/run/")
    ap.add_argument("--selftest", action="store_true",
                    help="break each pre-write gate on purpose; every one must fire")
    ap.add_argument("--audit", default=str(pathlib.Path(__file__).resolve().parents[2]
                                           / "audits" / "2026-08-10_oq277_rq2_crosscoding"))
    ap.add_argument("--units")
    ap.add_argument("--direction", choices=["i", "ii"])
    ap.add_argument("--out")
    ap.add_argument("--map")
    a = ap.parse_args()

    if a.selftest:
        return selftest()
    if a.build_run:
        return build_run(pathlib.Path(a.audit))
    missing = [f"--{n}" for n in ("units", "direction", "out", "map") if not getattr(a, n)]
    if missing:
        ap.error("single-packet mode requires " + ", ".join(missing)
                 + "   (or pass --build-run)")

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

    # max_run is the module-level function. It was hoisted, not copied, when run mode
    # needed it: two copies of the de-blocking measure would be a P2 fork inside the
    # experiment that measures P2, and this particular function already has one silent
    # wrong-object bug in its history.
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
