#!/usr/bin/env python3
"""OQ-75(b) grain-sensitivity precursor probe (pre-registered: PROPOSAL.md in
audits/2026-07-04_oq75b_grain_probe/). Unratified mechanical grain arms over the
OQ-72 tranche-1 registry; measures how the pilot's concept-key partitions move
per grain step. Zero API spend. Does NOT discharge the ruled Stage-1 check
(that tests the unbuilt correlation statistic).

Arms: A1 coarsen-max, A2 coarsen-2, A3 refine-arbitrary (A0 baseline read from
the committed registry; A4 refine-limit == exact_name, cited not re-run).
Controls: overlay-took-effect (fact-count + A1 atom-set), known-changer (A1
must merge digital_money's two baseline vantages)."""
import re
import subprocess
import sys
from collections import defaultdict
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
PROLOG_DIR = REPO / "prolog"
AUDIT = REPO / "audits" / "2026-07-04_oq75b_grain_probe"
REGISTRY = PROLOG_DIR / "axiom_concept_registry.pl"

PILOT = {
    "testsets": ["digital_money_legitimacy", "moral_causation_locus",
                 "visual_evidentiary_authority"],
    "testsets_haiku": ["marriage_authority_kernel", "vatican_ii_doctrinal_authority",
                       "software_source_status", "ai_governance_legitimacy",
                       "animal_moral_status", "tordesillas_demarcation_kernel",
                       "wto_treaty_framework"],
}
C2_PAIRS = [
    ("state_monopoly_on_legitimate_issuance", "consensus_suffices_for_legitimacy"),
    ("situational_primacy_over_disposition", "character_cross_situational_stability"),
    ("indexical_traces_recoverable", "verification_impossibility_at_scale"),
]

FACT = re.compile(r"^axiom_diff:axiom_concept\((\w+),\s*(\w+)\)\.")


def load_baseline():
    m = {}
    for ln in REGISTRY.read_text().splitlines():
        f = FACT.match(ln)
        if f:
            m[f.group(1)] = f.group(2)
    return m


def kernel_of(concept):
    return concept.split("__", 1)[0]


def arm_mappings(base):
    by_kernel = defaultdict(lambda: defaultdict(list))  # kernel -> slot -> axioms
    for ax, c in base.items():
        by_kernel[kernel_of(c)][c].append(ax)
    a1, a2, a3 = {}, {}, {}
    for k, slots in by_kernel.items():
        # A1: everything -> kernel__all
        for c, axs in slots.items():
            for ax in axs:
                a1[ax] = f"{k}__all"
        # A2: merge the two largest slots (tie -> alphabetical); others unchanged
        ranked = sorted(slots, key=lambda c: (-len(slots[c]), c))
        merged = set(ranked[:2])
        target = f"{k}__merged2"
        for c, axs in slots.items():
            for ax in axs:
                a2[ax] = target if c in merged else c
        # A3: split every >=2-occupant slot alphabetically into halves
        for c, axs in slots.items():
            axs_sorted = sorted(axs)
            if len(axs_sorted) < 2:
                for ax in axs_sorted:
                    a3[ax] = c
            else:
                half = (len(axs_sorted) + 1) // 2
                for ax in axs_sorted[:half]:
                    a3[ax] = f"{c}_r1"
                for ax in axs_sorted[half:]:
                    a3[ax] = f"{c}_r2"
    return {"A1": a1, "A2": a2, "A3": a3}


def write_arm_file(name, mapping):
    p = AUDIT / f"registry_{name}.pl"
    lines = [f"% OQ-75(b) grain probe arm {name} — UNRATIFIED mechanical overlay; never canonical.",
             ":- multifile axiom_diff:axiom_concept/2."]
    lines += [f"axiom_diff:axiom_concept({a}, {c})." for a, c in sorted(mapping.items())]
    p.write_text("\n".join(lines) + "\n")
    return p


GOAL = """
[stack],
{load},
aggregate_all(count, axiom_diff:axiom_concept(_, _), NF),
format('FACTS\\t~w~n', [NF]),
forall(axiom_diff:axiom_concept(_, C), format('ATOM\\t~w~n', [C])),
retractall(config:param(corpus_path, _)),
assertz(config:param(corpus_path, '{leg}')),
corpus_loader:load_all_testsets,
forall(member(K, [{kernels}]),
  ( cs_kernel_registry:cs_readings_for_kernel(K, Pairs),
    pairs_values(Pairs, Cs0), sort(Cs0, Cs),
    forall(( member(A, Cs), member(B, Cs), A @< B ),
      ( axiom_diff:axiom_diff(A, B, concept, CAg, CD, CB),
        length(CAg, NAg), length(CD, ND), length(CB, NB),
        axiom_diff:ax_stability_verdict(A, B, V),
        format('PAIR\\t~w\\t~w\\t~w\\t~w\\t~w\\t~w\\t~w~n', [K, A, B, NAg, ND, NB, V]) )) )),
halt(0)
"""


def run_arm(name, armfile, expected_facts):
    # A0: stack already loaded the canonical registry — no retract/consult
    # (re-consulting a stack-loaded file throws a module-permission error).
    load = ("true" if armfile is None else
            f"retractall(axiom_diff:axiom_concept(_, _)), "
            f"consult('{armfile.as_posix()}')")
    rows, atoms = [], set()
    for leg, kernels in PILOT.items():
        goal = " ".join(GOAL.format(load=load, leg=leg,
                                    kernels=",".join(kernels)).split())
        proc = subprocess.run(["swipl", "-q", "-g", goal, "-t", "halt(4)"],
                              cwd=PROLOG_DIR, capture_output=True, text=True, timeout=600)
        (AUDIT / f"run_{name}_{leg}.log").write_text(
            f"# exit={proc.returncode}\n{proc.stdout}\n--- stderr ---\n{proc.stderr}\n")
        if proc.returncode != 0:
            print(f"FAIL arm={name} leg={leg}\n{proc.stderr}", file=sys.stderr)
            sys.exit(1)
        nf = None
        for ln in proc.stdout.splitlines():
            f = ln.split("\t")
            if f[0] == "FACTS":
                nf = int(f[1])
            elif f[0] == "ATOM":
                atoms.add(f[1])
            elif f[0] == "PAIR":
                rows.append((leg, *f[1:]))
        if nf != expected_facts:
            print(f"CONTROL FAIL (overlay-took-effect): arm={name} leg={leg} "
                  f"in-image facts {nf} != arm-file facts {expected_facts}", file=sys.stderr)
            sys.exit(2)
    return rows, atoms


def summarize(name, rows, mapping):
    cells = sum(int(r[4]) + int(r[5]) for r in rows)
    conv = {(r[0], r[1]) for r in rows if int(r[4]) + int(r[5]) > 0}
    kernels = {(r[0], r[1]) for r in rows}
    verdicts = defaultdict(int)
    for r in rows:
        verdicts[r[7]] += 1
    c2 = sum(1 for a, b in C2_PAIRS if mapping.get(a) and mapping.get(a) == mapping.get(b))
    return {"arm": name, "cells": cells, "conversions": f"{len(conv)}/{len(kernels)}",
            "c2_coslotted": f"{c2}/3", "verdicts": dict(verdicts)}


def main():
    base = load_baseline()
    arms = arm_mappings(base)
    # known-changer precondition: A1 must actually merge digital_money's two slots
    dm = {c for a, c in base.items() if kernel_of(c) == "digital_money_legitimacy"}
    assert len(dm) == 2, f"baseline digital_money slots: {dm}"
    results = []
    # A0 from committed sweep output (same shape recomputed here for symmetry)
    a0_rows, _ = run_arm("A0", None, len(base))
    results.append(summarize("A0-baseline", a0_rows, base))
    for name in ("A1", "A2", "A3"):
        f = write_arm_file(name, arms[name])
        rows, atoms = run_arm(name, f, len(arms[name]))
        if name == "A1":
            expect = {f"{k}__all" for legk in PILOT.values() for k in legk}
            if atoms != expect:
                print(f"CONTROL FAIL: A1 atom set {sorted(atoms)} != expected", file=sys.stderr)
                sys.exit(3)
            # known-changer: digital_money vantages must have merged vs baseline
            dm_a1 = {c for a, c in arms["A1"].items()
                     if kernel_of(c) == "digital_money_legitimacy"}
            if len(dm_a1) != 1:
                print("CONTROL FAIL: known-changer — A1 did not merge digital_money slots",
                      file=sys.stderr)
                sys.exit(4)
        results.append(summarize(f"{name}", rows, arms[name]))
        (AUDIT / f"pairs_{name}.tsv").write_text(
            "\n".join("\t".join(map(str, r)) for r in rows) + "\n")
    (AUDIT / "pairs_A0.tsv").write_text(
        "\n".join("\t".join(map(str, r)) for r in a0_rows) + "\n")
    print(f"{'arm':<14}{'cells':>6}{'conversions':>14}{'C2 co-slot':>12}  verdicts")
    for r in results:
        print(f"{r['arm']:<14}{r['cells']:>6}{r['conversions']:>14}{r['c2_coslotted']:>12}  {r['verdicts']}")
    print("\nA4 refine-limit == exact_name: all-blind by prior witness "
          "(0 cells; 0/935 corpus-wide) — cited, not re-run.")


if __name__ == "__main__":
    main()
