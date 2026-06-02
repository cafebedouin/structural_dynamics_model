#!/usr/bin/env python3
"""Reading-reference linter (reporter, not a fixer).

Census: every reference to a reading/constraint NAME in the corpus, across both
reference predicates — cs_reading_relation/3 (committer edges) and
affects_constraint/2 (network edges).

Three rules run against the census, each with a synthetic positive control that
must flag a known-dirty fixture before the rule is trusted on the corpus:

  R1  dangling          target resolves to no declared reading, even kernel-qualified
                        (the OQ-58 referential-integrity failure, generalized to
                        both predicates).
  R2  non-canonical     target resolves only after kernel-qualification (a short
                        form that should be <kernel>__<short>); self-witnessing
                        consumers under-count these.
  R3  duplication       within one kernel, two declared readings with near-identical
                        stems (OQ-59) — likely one position under two names, which
                        corrupts the obstruction cover.

The headline OUTPUT is the INCOMPLETENESS RATE: the R1 danglings collapsed to
distinct (kernel, canonical missing-target) — the unit a "complete the kernels vs
patch the edges" decision is ruled against (the dangling edges are a symptom; the
missing readings are the thing).

Run:  python3 python/audits/reading_reference_linter.py
"""
from __future__ import annotations
import re
import sys
import pathlib
import collections

TESTSETS = pathlib.Path(__file__).resolve().parents[2] / "prolog" / "testsets"

_CSR = re.compile(r"cs_reading_relation\(\s*'[^']*'\s*,\s*([a-z0-9_]+)\s*,\s*([a-z_]+)\s*\)")
_AFF = re.compile(r"affects_constraint\(\s*([a-z0-9_]+)\s*,\s*([a-z0-9_]+)\s*\)")

Ref = collections.namedtuple("Ref", "predicate source target rel")


def kernel_of(cid: str) -> str:
    return cid.split("__", 1)[0]


def census(testsets_dir: pathlib.Path) -> list[Ref]:
    refs: list[Ref] = []
    for pl in sorted(testsets_dir.glob("*.pl")):
        cid = pl.stem
        text = pl.read_text(encoding="utf-8", errors="replace")
        for tgt, rel in _CSR.findall(text):
            refs.append(Ref("cs_reading_relation", cid, tgt, rel))
        for src, tgt in _AFF.findall(text):
            # the referrer is the source constraint; reference is to the target
            refs.append(Ref("affects_constraint", src, tgt, ""))
    return refs


def declared_set(testsets_dir: pathlib.Path) -> set[str]:
    return {pl.stem for pl in testsets_dir.glob("*.pl")}


def resolution(source: str, target: str, declared: set[str]) -> str:
    """exact | short | typo | dangling — does the target resolve to a declared name?

    short  — bare reading stem; <kernel>__<target> exists.
    typo   — target carries the kernel prefix with a wrong delimiter (single _ instead
             of __); <kernel>__<rest> exists. A non-canonical reference to an EXISTING
             reading (a repair, not a missing reading).
    dangling — resolves to no declared reading even after those normalizations (the
             upper bound on genuinely-missing readings; the narrative read splits the
             residual truncated-kernel typos from real gaps).
    """
    if target in declared:
        return "exact"
    k = kernel_of(source)
    if f"{k}__{target}" in declared:
        return "short"
    if target.startswith(k + "_"):
        rest = target[len(k):].lstrip("_")
        if rest and f"{k}__{rest}" in declared:
            return "typo"
    return "dangling"


def rule_dangling(refs, declared):
    return [r for r in refs if resolution(r.source, r.target, declared) == "dangling"]


def rule_noncanonical(refs, declared):
    """Non-canonical reference to an EXISTING reading (short form or delimiter typo) —
    repairable, not missing."""
    return [r for r in refs if resolution(r.source, r.target, declared) in ("short", "typo")]


def rule_duplication(declared):
    """R3: per kernel, declared-reading short-stems that are near-duplicates."""
    by_kernel = collections.defaultdict(list)
    for cid in declared:
        if "__" in cid:
            by_kernel[kernel_of(cid)].append(cid.split("__", 1)[1])
    SUFFIXES = ("_reading", "_sovereignty", "_interpretation", "_mechanism", "_constraint")

    def root(stem):
        for s in SUFFIXES:
            if stem.endswith(s):
                return stem[: -len(s)]
        return stem

    def editdist(a, b):
        if abs(len(a) - len(b)) > 2:
            return 99
        prev = list(range(len(b) + 1))
        for i, ca in enumerate(a, 1):
            cur = [i]
            for j, cb in enumerate(b, 1):
                cur.append(min(prev[j] + 1, cur[-1] + 1, prev[j - 1] + (ca != cb)))
            prev = cur
        return prev[-1]

    dups = []
    for k, stems in by_kernel.items():
        for i in range(len(stems)):
            for j in range(i + 1, len(stems)):
                ri, rj = root(stems[i]), root(stems[j])
                if ri == rj or editdist(ri, rj) <= 2 or ri in rj or rj in ri:
                    if {ri, rj} != {""} and min(len(ri), len(rj)) >= 3:
                        dups.append((k, stems[i], stems[j]))
    return dups


def incompleteness_rate(danglings, declared):
    """Collapse dangling refs to distinct (kernel, canonical missing-target)."""
    missing = collections.defaultdict(set)  # kernel -> {canonical target}
    edges_per = collections.Counter()
    for r in danglings:
        k = kernel_of(r.source)
        canon = r.target if "__" in r.target else f"{k}__{r.target}"
        missing[k].add(canon)
        edges_per[(k, canon)] += 1
    n_edges = len(danglings)
    n_missing = sum(len(v) for v in missing.values())
    n_kernels = len(missing)
    return n_edges, n_missing, n_kernels, missing, edges_per


# ---------------------------------------------------------------------------
# Positive controls — synthetic fixtures each rule MUST flag (and must not over-flag)
# ---------------------------------------------------------------------------
def selftest() -> bool:
    declared = {
        "k__alpha_reading", "k__beta_reading",      # two real readings of kernel k
        "k2__gradated_reading", "k2__graduated_reading",  # near-dup pair in k2
    }
    refs = [
        Ref("cs_reading_relation", "k__alpha_reading", "k__beta_reading", "forecloses"),  # exact
        Ref("cs_reading_relation", "k__alpha_reading", "beta_reading", "coexists_with"),  # short
        Ref("cs_reading_relation", "k__alpha_reading", "k_beta_reading", "influences"),    # delimiter typo -> existing
        Ref("cs_reading_relation", "k__alpha_reading", "k__ghost_reading", "forecloses"), # dangling
    ]
    ok = True
    d = rule_dangling(refs, declared)
    if {r.target for r in d} != {"k__ghost_reading"}:
        print(f"  R1 selftest FAIL: {[r.target for r in d]}"); ok = False
    nc = rule_noncanonical(refs, declared)
    if {r.target for r in nc} != {"beta_reading", "k_beta_reading"}:
        print(f"  R2 selftest FAIL: {[r.target for r in nc]}"); ok = False
    dup = rule_duplication(declared)
    if not any(k == "k2" for k, _, _ in dup):
        print(f"  R3 selftest FAIL (missed k2 gradated/graduated): {dup}"); ok = False
    if any(k == "k" for k, _, _ in dup):
        print(f"  R3 selftest FAIL (over-flagged alpha/beta): {dup}"); ok = False
    print(f"  positive controls: {'PASS' if ok else 'FAIL'} "
          f"(R1 flags ghost, R2 flags short, R3 flags gradated/graduated, no over-flag)")
    return ok


def main():
    if not selftest():
        print("SELFTEST FAILED — rules not trusted; aborting corpus report.")
        sys.exit(1)

    refs = census(TESTSETS)
    declared = declared_set(TESTSETS)
    by_pred = collections.Counter(r.predicate for r in refs)
    print(f"\n== Census ({len(refs)} references over {len(declared)} declared constraints) ==")
    for p, n in by_pred.most_common():
        print(f"  {p}: {n}")

    # Referential integrity applies ONLY to cs_reading_relation — its targets MUST be
    # sibling readings. affects_constraint is a CAUSAL network edge whose targets may be
    # abstract effect-nodes (mass_shooting_externality, ...) that are not declared
    # constraints; subjecting it to the same rule mixes two semantics. Scope R1/R2 and
    # the incompleteness rate to cs_reading_relation; report affects_constraint apart.
    csr = [r for r in refs if r.predicate == "cs_reading_relation"]
    aff = [r for r in refs if r.predicate == "affects_constraint"]

    csr_dangling = rule_dangling(csr, declared)
    csr_noncanon = rule_noncanonical(csr, declared)
    aff_dangling = rule_dangling(aff, declared)
    dups = rule_duplication(declared)

    print(f"\n== R1 dangling — cs_reading_relation (referential integrity APPLIES): {len(csr_dangling)} ==")
    print(f"   by relation: "
          + ", ".join(f"{rel}={n}" for rel, n in
                      collections.Counter(r.rel for r in csr_dangling).most_common()))
    print(f"== R2 non-canonical short-form — cs_reading_relation: {len(csr_noncanon)} ==")
    print(f"== R3 within-kernel near-duplicate reading pairs: {len(dups)} "
          f"(REVIEW-TRIGGER — near-naming is usually intentional for contrasting positions; "
          f"only westphalian gradated/graduated confirmed) ==")
    for k, a, b in dups:
        print(f"  {k}: {a}  <->  {b}")

    print(f"\n== affects_constraint dangling (SEPARATE — NOT a clean integrity signal): {len(aff_dangling)} ==")
    print(f"   targets are a mix of sibling-reading refs and abstract causal-effect nodes;")
    print(f"   integrity does NOT require these to be declared readings. Reported for awareness, "
          f"not folded into the rate.")

    n_edges, n_missing, n_kernels, missing, edges_per = incompleteness_rate(csr_dangling, declared)
    print(f"\n== INCOMPLETENESS RATE (cs_reading_relation only) ==")
    print(f"  {n_edges} dangling committer edges  ->  {n_missing} distinct missing readings  "
          f"across  {n_kernels} kernels")
    multi = [(k, len(v)) for k, v in missing.items() if len(v) > 1]
    print(f"  kernels missing >1 reading: {len(multi)}  (=> 'complete kernels' if large; "
          f"'patch edges' if it's ~1 per kernel)")
    for k, n in sorted(multi, key=lambda x: -x[1])[:15]:
        print(f"    {k}: {n} missing  ({', '.join(sorted(s.split('__',1)[1] for s in missing[k]))})")


if __name__ == "__main__":
    main()
