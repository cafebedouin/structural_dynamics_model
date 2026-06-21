#!/usr/bin/env python3
"""OQ-58 cross-corpus referential-integrity census driver (read-only).

Reproduces the audit's evidence table by driving the (pure, no-engine) functions
of python/audits/reading_reference_linter.py over every corpus on disk. Writes
nothing to the corpus; globs *.pl and does set-membership only.

Run from the repo root:  python3 audits/2026-06-20_oq58_cross_corpus_incompleteness/census_driver.py
"""
from __future__ import annotations
import pathlib
import sys

REPO = pathlib.Path(__file__).resolve().parents[2]
sys.path.insert(0, str(REPO / "python" / "audits"))
import reading_reference_linter as L  # noqa: E402

ROOT = REPO / "prolog"
CORPORA = [
    ("LIVE testsets",  ROOT / "testsets"),
    ("testsets_haiku", ROOT / "testsets_haiku"),
    ("testsets_flash", ROOT / "testsets_flash"),
    ("arch kernel_v1", ROOT / "archives/datasets/kernel_v1"),
    ("arch kernel_test", ROOT / "archives/datasets/kernel_test"),
    ("arch kernel_v2_test", ROOT / "archives/datasets/kernel_v2_test"),
    ("arch original_v5", ROOT / "archives/datasets/original_v5"),
    ("arch original_v6", ROOT / "archives/datasets/original_v6"),
    ("arch sotu",      ROOT / "archives/datasets/sotu"),
]


def metrics(d: pathlib.Path):
    files = list(d.glob("*.pl"))
    refs = L.census(d)
    declared = L.declared_set(d)
    csr = [r for r in refs if r.predicate == "cs_reading_relation"]
    dangling = L.rule_dangling(csr, declared)
    n_edges, n_missing, n_kernels, missing, sources_per = L.incompleteness_rate(dangling, declared)
    defensible = {kc for kc, s in sources_per.items() if len(s) >= 2}
    rate = (100.0 * n_missing / len(csr)) if csr else 0.0
    kernels = {L.kernel_of(c) for c in declared}
    rkern = (len(declared) / len(kernels)) if kernels else 0.0
    return dict(files=len(files), csr=len(csr), dangl=len(dangling), miss=n_missing,
                defensible=len(defensible), rate=rate, rkern=rkern, missing_set=missing)


def all_missing(d: pathlib.Path) -> set:
    refs = L.census(d)
    declared = L.declared_set(d)
    csr = [r for r in refs if r.predicate == "cs_reading_relation"]
    dangling = L.rule_dangling(csr, declared)
    _, _, _, missing, _ = L.incompleteness_rate(dangling, declared)
    out: set = set()
    for v in missing.values():
        out |= v
    return out


def defensible_set(d: pathlib.Path) -> set:
    s = L.summarize(d)
    return {x["missing"] for x in s["defensible_ge2"]}


def main():
    print("== Linter selftest (positive controls) ==")
    ok = L.selftest()
    print(f"  -> {'PASS' if ok else 'FAIL'}\n")

    print("== Cross-corpus census (rate% = distinct-missing / cs_reading_relation edges) ==")
    hdr = f"{'corpus':22} {'files':>6} {'cs_rr':>6} {'dangl':>6} {'miss':>6} {'id>=2':>6} {'r/kern':>7} {'rate%':>7}"
    print(hdr)
    print("-" * len(hdr))
    for name, d in CORPORA:
        if not d.exists():
            print(f"{name:22} MISSING ({d})")
            continue
        m = metrics(d)
        print(f"{name:22} {m['files']:>6} {m['csr']:>6} {m['dangl']:>6} {m['miss']:>6} "
              f"{m['defensible']:>6} {m['rkern']:>7.2f} {m['rate']:>7.1f}")

    print("\n== LIVE defensible (id>=2) set ==")
    s = L.summarize(ROOT / "testsets")
    for x in s["defensible_ge2"]:
        print(f"  {x['missing']}  (x{x['in_degree']})")

    print("\n== Durable defensible set: twin-reproducible (haiku INT flash, id>=2) ==")
    dh, df = defensible_set(ROOT / "testsets_haiku"), defensible_set(ROOT / "testsets_flash")
    print(f"  haiku id>=2 = {len(dh)}   flash id>=2 = {len(df)}   haiku INT flash = {len(dh & df)}")

    print("\n== Cross-lineage missing-set intersections (any in-degree) ==")
    ah = all_missing(ROOT / "testsets_haiku")
    af = all_missing(ROOT / "testsets_flash")
    ak = all_missing(ROOT / "archives/datasets/kernel_v1")
    print(f"  missing(any): haiku={len(ah)} flash={len(af)} kernel_v1={len(ak)}")
    print(f"  haiku INT flash               = {len(ah & af)}")
    print(f"  haiku INT flash INT kernel_v1 = {len(ah & af & ak)}   "
          f"(kernel_v1 is a DIFFERENT kernel population)")

    print("\n== Concrete per-edge witness: jewish_sovereignty_palestine__cultural_zionist_reading ==")
    decl = L.declared_set(ROOT / "testsets")
    src = "jewish_sovereignty_palestine__cultural_zionist_reading"
    f = (ROOT / "testsets" / f"{src}.pl")
    refs = [r for r in L.census(ROOT / "testsets")
            if r.predicate == "cs_reading_relation" and r.source == src]
    for r in refs:
        res = L.resolution(r.source, r.target, decl)
        print(f"  {r.rel:14} -> {r.target:55} [{res}]")


if __name__ == "__main__":
    main()
