#!/usr/bin/env python3
"""OQ-72 C4 negative-control generator: the lexical adversary.

Token-overlap clusterer over the pilot axiom names (the OQ-64 morphology trap
repurposed as ADVERSARY, never proposer). Emits within-kernel merge candidates —
lexically close name pairs — for the operator to mark truly-distinct at R3; the
blind proposer must have labeled every marked-distinct pair differently.

Positive control (the adversary is itself an instrument and owes its own
witness): a planted lexically-close pair is injected into one kernel's name
pool; the adversary must emit it as a candidate, else the run FAILS LOUD (an
empty candidate list would otherwise be "didn't look", not "nothing there").
Planted rows are stripped from the shipped candidate list.

Deterministic: no randomness, no timestamps. Reads inventory.tsv; writes
adversary_candidates.tsv + adversary_control.log in the audit dir.
"""
import sys
from itertools import combinations
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
AUDIT_DIR = REPO / "audits" / "2026-07-03_oq72_concept_key_pilot"

STOP = {"of", "for", "as", "to", "not", "the", "a", "an", "is", "are", "and",
        "or", "over", "under", "with", "via", "per"}

PLANT_KERNEL = "digital_money_legitimacy"
PLANT_A = "planted_sovereign_issuance_control"
PLANT_B = "planted_sovereign_issuance_mandate"


def tokens(name: str) -> frozenset:
    return frozenset(t for t in name.split("_") if t not in STOP and len(t) > 2)


def close(a: str, b: str) -> tuple[bool, int, float]:
    ta, tb = tokens(a), tokens(b)
    shared = len(ta & tb)
    jac = shared / len(ta | tb) if ta | tb else 0.0
    return (shared >= 2 or jac >= 0.5), shared, jac


def main() -> None:
    rows = (AUDIT_DIR / "inventory.tsv").read_text().splitlines()[1:]
    by_kernel: dict[str, list[str]] = {}
    for r in rows:
        f = r.split("\t")
        by_kernel.setdefault(f[1], []).append(f[5])

    # inject the planted pair (adversary positive control)
    by_kernel[PLANT_KERNEL] = by_kernel[PLANT_KERNEL] + [PLANT_A, PLANT_B]

    candidates, planted_hits = [], []
    for kernel, names in sorted(by_kernel.items()):
        for a, b in combinations(sorted(set(names)), 2):
            hit, shared, jac = close(a, b)
            if not hit:
                continue
            row = (kernel, a, b, shared, round(jac, 3))
            if PLANT_A in (a, b) or PLANT_B in (a, b):
                planted_hits.append(row)
            else:
                candidates.append(row)

    control_ok = any(
        {PLANT_A, PLANT_B} == {r[1], r[2]} for r in planted_hits)
    log = [
        f"planted pair: {PLANT_A} / {PLANT_B} (kernel pool: {PLANT_KERNEL})",
        f"planted-pair merged by adversary: {'YES' if control_ok else 'NO'}",
        f"all planted-involving rows (stripped from shipped list): {planted_hits}",
        f"shipped candidates: {len(candidates)}",
    ]
    (AUDIT_DIR / "adversary_control.log").write_text("\n".join(log) + "\n")
    print("\n".join(log))
    if not control_ok:
        print("FAIL: adversary positive control silent — adversary invalid, "
              "candidate list VOID (C4 halt condition)", file=sys.stderr)
        sys.exit(1)

    out = AUDIT_DIR / "adversary_candidates.tsv"
    hdr = "kernel\tname_a\tname_b\tshared_tokens\tjaccard\toperator_distinct(Y/N)"
    out.write_text(hdr + "\n" + "\n".join(
        f"{k}\t{a}\t{b}\t{s}\t{j}\t" for k, a, b, s, j in candidates) + "\n")
    print(f"wrote {out}")
    for k, a, b, s, j in candidates:
        print(f"  {k}: {a} ~ {b} (shared={s}, jac={j})")


if __name__ == "__main__":
    main()
