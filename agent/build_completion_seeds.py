#!/usr/bin/env python3
"""Build enriched generation seeds for the DECLARED-but-dropped readings.

Root cause (OQ-58): SCOPE manifests declare a kernel's full reading set in
`commitment_system_recognition.readings`, but `generation_sequence` was capped
(axes=3), so readings were dropped and never generated. This builds completion
seeds for every dropped reading (per the manifest's own declaration), enriched
with the EXISTING siblings' structure (eps, type, narrative) from json/ so the
new reading is generated consistent with what the kernel already holds.

Output: outputs/completion_seeds/all_seeds.json (enriched seeds, ranked), plus a
printed ranking by kernel incompleteness (dropped / declared). Feed slices to
generate_kernel_corpus.py --seeds <file> N (background-polled).

Run:  python3 agent/build_completion_seeds.py
"""
import json
import glob
import collections
import pathlib

ROOT = pathlib.Path(__file__).resolve().parents[1]
TESTSETS = ROOT / "prolog" / "testsets"
JSON_DIR = ROOT / "json"
OUT = ROOT / "outputs" / "completion_seeds"


def load_manifests():
    paths = set(glob.glob(str(ROOT / "outputs/**/*.manifest.json"), recursive=True))
    paths |= set(glob.glob(str(ROOT / "outputs/**/manifests/*.json"), recursive=True))
    mans = []
    for p in paths:
        try:
            mans.append(json.load(open(p)))
        except Exception:
            pass
    return mans


def canonical_per_kernel(mans):
    """One manifest per kernel_id — the one declaring the most readings."""
    best = {}
    for m in mans:
        csr = m.get("commitment_system_recognition", {}) or {}
        if not csr.get("is_contested_kernel"):
            continue
        kid = csr.get("kernel_id")
        if not kid:
            continue
        n = len([r for r in csr.get("readings", []) if r.get("reading_id")])
        if kid not in best or n > best[kid][0]:
            best[kid] = (n, m)
    return {k: m for k, (_, m) in best.items()}


def sibling_context(kid, existing_rids):
    lines = []
    for rid in sorted(existing_rids):
        jp = JSON_DIR / f"{kid}__{rid}.json"
        if not jp.exists():
            continue
        try:
            d = json.load(open(jp))
        except Exception:
            continue
        bp = d.get("base_properties", {})
        narr = (d.get("commentary", {}) or {}).get("narrative_context", "")
        lines.append(f"  - {rid} (eps={bp.get('extractiveness')}, "
                     f"{bp.get('claimed_type')}): {narr[:280]}")
    if not lines:
        return ""
    return ("EXISTING SIBLING READINGS already generated for this kernel — author the "
            "new reading as a DISTINCT position consistent with (not duplicating) these:\n"
            + "\n".join(lines))


def main():
    mans = load_manifests()
    canon = canonical_per_kernel(mans)
    rows = []                 # PARTIAL kernels (existing>0, dropped>0) — the completion backlog
    never_generated = []      # existing==0 — scoped-but-never-generated (separate decision)
    for kid, m in canon.items():
        csr = m["commitment_system_recognition"]
        readings = {r["reading_id"]: r for r in csr.get("readings", []) if r.get("reading_id")}
        declared = set(readings)
        existing = {rid for rid in declared if (TESTSETS / f"{kid}__{rid}.pl").exists()}
        dropped = declared - existing
        if not dropped:
            continue
        if not existing:
            # Manifest scoped but the kernel was never generated — NOT a completion
            # candidate (often a near-duplicate / superseded / exploratory kernel).
            never_generated.append((kid, len(declared)))
            continue
        sib = sibling_context(kid, existing)
        kernel_ctx = ("KERNEL CONTEST — all declared readings of this kernel:\n"
                      + "\n".join(f"  - {rid}: {readings[rid].get('commitment','')}"
                                  for rid in sorted(declared)))
        seeds = []
        for rid in sorted(dropped):
            r = readings[rid]
            summary = (f"{r.get('commitment','')}\n"
                       f"Expected structural delta: {r.get('expected_structural_delta','')}\n\n"
                       f"{kernel_ctx}\n\n{sib}")
            seeds.append({
                "constraint_id": f"{kid}__{rid}",
                "kernel_id": kid,
                "reading_id": rid,
                "human_readable": r.get("commitment", rid),
                "topic_domain": m.get("domain", ""),
                "family_id": m.get("family_id", ""),
                "sibling_reading_ids": r.get("sibling_readings", []),
                "expected_structural_delta": r.get("expected_structural_delta", ""),
                "summary": summary,
            })
        rows.append((len(dropped) / len(declared), kid, len(existing), seeds))

    rows.sort(reverse=True)
    all_seeds = [s for _, _, _, seeds in rows for s in seeds]
    OUT.mkdir(parents=True, exist_ok=True)
    (OUT / "all_seeds.json").write_text(json.dumps(all_seeds, indent=2, ensure_ascii=False))

    print(f"PARTIAL kernels needing completion (existing>0): {len(rows)}   "
          f"dropped readings: {len(all_seeds)}")
    print(f"  (separately: {len(never_generated)} scoped-but-never-generated kernels — "
          f"NOT a completion task; likely duplicates/superseded/exploratory)")
    print(f"wrote {OUT/'all_seeds.json'}\n")
    print(f"{'incompl':>7}  {'kernel':45} existing  dropped")
    for ic, kid, nex, seeds in rows:
        print(f"{ic:6.0%}   {kid:45} {nex:^8} {len(seeds):^7}  [{', '.join(s['reading_id'] for s in seeds)}]")


if __name__ == "__main__":
    main()
