#!/usr/bin/env python3
"""OQ-72 Phase-1 inventory probe (read-only; zero API spend).

One row per axiom on the pilot kernels, per leg. Legs are loaded in SEPARATE
swipl processes, serially (one corpus_path overlay per process — never two legs
in one image). Fails loud (exit nonzero) if any pilot kernel resolves to zero
readings or zero axioms, or if a reading is not corpus_constraint/1-backed.

Also pastes listing(axiom_diff:axiom_aligned/3) into the per-leg log so the
per-axiom-arity assumption of the concept join (axiom_concept(Name, Concept))
carries its witness in-band.

Output: audits/2026-07-03_oq72_concept_key_pilot/inventory.tsv (+ per-leg raw logs).
"""
import subprocess
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
PROLOG_DIR = REPO / "prolog"
AUDIT_DIR = REPO / "audits" / "2026-07-03_oq72_concept_key_pilot"

PILOT = {
    "testsets": [
        "digital_money_legitimacy",
        "moral_causation_locus",
        "visual_evidentiary_authority",
    ],
    "testsets_haiku": [
        "marriage_authority_kernel",
        "vatican_ii_doctrinal_authority",
        "software_source_status",
        "ai_governance_legitimacy",
        "animal_moral_status",
        "tordesillas_demarcation_kernel",
        "wto_treaty_framework",
    ],
}

GOAL_TEMPLATE = """
[stack],
retractall(config:param(corpus_path, _)),
assertz(config:param(corpus_path, '{leg}')),
corpus_loader:load_all_testsets,
listing(axiom_diff:axiom_aligned/3),
forall(member(K, [{kernels}]),
  ( cs_kernel_registry:cs_readings_for_kernel(K, Pairs),
    ( Pairs == [] -> format(user_error, 'EMPTY_KERNEL ~w~n', [K]), halt(2) ; true ),
    forall(member(UID-C, Pairs),
      ( ( corpus_loader:corpus_constraint(C) -> InCorpus = yes ; InCorpus = no ),
        aggregate_all(count, narrative_ontology:cs_axiom(UID, _, _), NAx),
        ( NAx =:= 0 -> format(user_error, 'ZERO_AXIOMS ~w ~w~n', [K, C]), halt(3) ; true ),
        forall(narrative_ontology:cs_axiom(UID, Tier, Name),
          ( ( narrative_ontology:cs_axiom_grounding(UID, Name, G) -> true ; G = unknown ),
            format('ROW\\t{leg}\\t~w\\t~w\\t~w\\t~w\\t~w\\t~w\\t~w~n',
                   [K, C, UID, Tier, Name, G, InCorpus]) ) ) ) ) ) ),
halt(0)
"""


def run_leg(leg: str, kernels: list[str]) -> list[str]:
    goal = GOAL_TEMPLATE.format(leg=leg, kernels=",".join(kernels))
    goal = " ".join(goal.split())  # single line for -g
    proc = subprocess.run(
        ["swipl", "-q", "-g", goal, "-t", "halt(4)"],
        cwd=PROLOG_DIR, capture_output=True, text=True, timeout=600,
    )
    (AUDIT_DIR / f"inventory_{leg}.log").write_text(
        f"# exit={proc.returncode}\n# --- stdout ---\n{proc.stdout}\n# --- stderr ---\n{proc.stderr}\n"
    )
    if proc.returncode != 0:
        print(f"FAIL leg={leg} exit={proc.returncode}", file=sys.stderr)
        print(proc.stderr, file=sys.stderr)
        sys.exit(proc.returncode)
    rows = [ln[4:] for ln in proc.stdout.splitlines() if ln.startswith("ROW\t")]
    loaded = [ln for ln in proc.stderr.splitlines() if "Loaded" in ln]
    print(f"[{leg}] {loaded[0].strip() if loaded else 'NO LOAD LINE'} | pilot rows: {len(rows)}")
    return rows


def main() -> None:
    all_rows = []
    for leg, kernels in PILOT.items():
        all_rows.extend(run_leg(leg, kernels))
    header = "leg\tkernel\treading\tuid\ttier\tname\tgrounding\tin_corpus"
    out = AUDIT_DIR / "inventory.tsv"
    out.write_text(header + "\n" + "\n".join(all_rows) + "\n")
    # summary: readings + axioms per kernel; loud check on in_corpus
    per_kernel: dict[tuple, dict] = {}
    bad = 0
    for r in all_rows:
        f = r.split("\t")
        key = (f[0], f[1])
        d = per_kernel.setdefault(key, {"readings": set(), "axioms": 0})
        d["readings"].add(f[2])
        d["axioms"] += 1
        if f[7] != "yes":
            bad += 1
    print(f"\n{'leg':<16}{'kernel':<34}{'readings':>9}{'axioms':>8}")
    for (leg, k), d in sorted(per_kernel.items()):
        print(f"{leg:<16}{k:<34}{len(d['readings']):>9}{d['axioms']:>8}")
    print(f"\ntotal rows: {len(all_rows)}  not-in-corpus rows: {bad}")
    if bad:
        print("FAIL: rows outside corpus_constraint/1", file=sys.stderr)
        sys.exit(5)
    print(f"wrote {out}")


if __name__ == "__main__":
    main()
