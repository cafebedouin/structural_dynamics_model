#!/usr/bin/env python3
"""Build generation seeds for the NEVER-GENERATED kernels.

Sibling of build_completion_seeds.py. That script handles PARTIAL kernels
(existing>0, some readings dropped) and deliberately SKIPS kernels whose declared
readings have zero .pl on disk. This script emits seeds for exactly that skipped
set — every declared reading of every never-generated contested kernel — so they
can be generated through the ordinary no-scope path:

    python3 -m agent.generate_kernel_corpus --seeds <this output>

Disposition decision (recorded): we do NOT triage these against existing same-topic
kernels. Per the westphalia ruling, a same-topic sibling kernel is a DISTINCT kernel
(different reading-set + ε), so generating these adds cross-kernel invariant probes,
not redundant duplicates; any residual duplication is accepted. See
memory feedback_near_duplicate_kernels_are_invariant_probes.

Seed schema matches build_completion_seeds / flatten_manifests so the no-scope
generator (build_cached_messages) threads kernel context correctly:
  constraint_id, kernel_id, reading_id, human_readable, topic_domain, family_id,
  sibling_reading_ids, expected_structural_delta, summary.

Output: outputs/completion_seeds/never_generated_seeds.json
"""
import json
import glob
import pathlib

ROOT = pathlib.Path(__file__).resolve().parents[1]
TESTSETS = ROOT / "prolog" / "testsets"
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


def main():
    mans = load_manifests()
    canon = canonical_per_kernel(mans)
    seeds = []
    kernels = 0
    for kid, m in canon.items():
        csr = m["commitment_system_recognition"]
        readings = {r["reading_id"]: r for r in csr.get("readings", []) if r.get("reading_id")}
        declared = set(readings)
        if not declared:
            continue
        existing = {rid for rid in declared if (TESTSETS / f"{kid}__{rid}.pl").exists()}
        if existing:
            continue  # partial or complete -> handled by build_completion_seeds, not here
        kernels += 1
        kernel_ctx = ("KERNEL CONTEST — all declared readings of this kernel:\n"
                      + "\n".join(f"  - {rid}: {readings[rid].get('commitment','')}"
                                  for rid in sorted(declared)))
        for rid in sorted(declared):
            r = readings[rid]
            commitment = (r.get("commitment") or rid).strip()
            delta = r.get("expected_structural_delta", "")
            siblings = r.get("sibling_readings", []) or sorted(declared - {rid})
            summary = (f"{commitment}\n"
                       f"Expected structural delta: {delta}\n\n{kernel_ctx}")
            seeds.append({
                "constraint_id": f"{kid}__{rid}",
                "kernel_id": kid,
                "reading_id": rid,
                "human_readable": commitment,
                "topic_domain": m.get("domain", ""),
                "family_id": m.get("family_id", ""),
                "sibling_reading_ids": siblings,
                "expected_structural_delta": delta,
                "summary": summary,
            })

    OUT.mkdir(parents=True, exist_ok=True)
    out_path = OUT / "never_generated_seeds.json"
    out_path.write_text(json.dumps(seeds, indent=2, ensure_ascii=False))
    print(f"never-generated kernels: {kernels}")
    print(f"reading seeds emitted:   {len(seeds)}")
    print(f"wrote {out_path.relative_to(ROOT)}")
    # collision preview: any seed constraint_id whose .pl already exists?
    clash = [s["constraint_id"] for s in seeds
             if (TESTSETS / f"{s['constraint_id']}.pl").exists()]
    print(f"constraint_ids already on disk (would get __uuid8 suffix): {len(clash)}")
    if clash:
        for c in clash:
            print(f"   {c}")


if __name__ == "__main__":
    main()
