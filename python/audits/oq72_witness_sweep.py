#!/usr/bin/env python3
"""OQ-72 Phase-5 witness sweep: axiom_diff under BOTH keys over all pilot
within-kernel reading pairs, per leg (serial overlays, separate swipl processes).

Checks kill-condition leg (b) in full:
  C3 (pooled non-degeneracy floor): >=1 blind->(agree/disparity) conversion in
     >=7 of 10 pilot kernels (a conversion = a pair whose concept-key diff has
     agreement+disparity > 0, given exact_name is structurally all-blind).
  C2 (contradiction-specific PASS): >=1 contained live cs_axiom_contradiction
     pair surfaces in the actual diff as a same-concept agree/disparity cell.
     The two visual by-construction split pairs are PRE-EXCLUDED per R2_RULING.

Output: sweep_results.tsv + per-leg raw logs in the audit dir; summary printed.
"""
import subprocess
import sys
from pathlib import Path

REPO = Path(__file__).resolve().parents[2]
PROLOG_DIR = REPO / "prolog"
AUDIT_DIR = REPO / "audits" / "2026-07-03_oq72_concept_key_pilot"

PILOT = {
    "testsets": ["digital_money_legitimacy", "moral_causation_locus",
                 "visual_evidentiary_authority"],
    "testsets_haiku": ["marriage_authority_kernel", "vatican_ii_doctrinal_authority",
                       "software_source_status", "ai_governance_legitimacy",
                       "animal_moral_status", "tordesillas_demarcation_kernel",
                       "wto_treaty_framework"],
}

# C2: the three CONTAINED live contradiction pairs (R2_RULING pairing witness).
# (concept vantage that must appear as agree/disparity, reading A, reading B)
C2_PAIRS = [
    ("digital_money_legitimacy__issuance_legitimacy_basis",
     "sovereign_cbdc_reading", "crypto_permissionless_reading"),
    ("moral_causation_locus__causation_locus",
     "situational_reading", "dispositional_reading"),
    ("visual_evidentiary_authority__verification_feasibility",
     "indexical_realism", "epistemic_collapse"),
]

GOAL = """
[stack],
retractall(config:param(corpus_path, _)),
assertz(config:param(corpus_path, '{leg}')),
corpus_loader:load_all_testsets,
forall(member(K, [{kernels}]),
  ( cs_kernel_registry:cs_readings_for_kernel(K, Pairs),
    pairs_values(Pairs, Cs0), sort(Cs0, Cs),
    forall(( member(A, Cs), member(B, Cs), A @< B ),
      ( axiom_diff:axiom_diff(A, B, exact_name, EAg, ED, EB),
        axiom_diff:axiom_diff(A, B, concept, CAg, CD, CB),
        length(EAg, NEAg), length(ED, NED), length(EB, NEB),
        length(CAg, NCAg), length(CD, NCD), length(CB, NCB),
        axiom_diff:ax_stability_verdict(A, B, V),
        format('PAIR\\t{leg}\\t~w\\t~w\\t~w\\t~w/~w/~w\\t~w/~w/~w\\t~w~n',
               [K, A, B, NEAg, NED, NEB, NCAg, NCD, NCB, V]),
        forall(member(agree(VK, Gs), CAg),
               format('CELL\\t{leg}\\t~w\\t~w\\t~w\\tagree\\t~w\\t~w~n', [K, A, B, VK, Gs])),
        forall(member(disparity(VK, GA, GB), CD),
               format('CELL\\t{leg}\\t~w\\t~w\\t~w\\tdisparity\\t~w\\t~w|~w~n', [K, A, B, VK, GA, GB]))
      ))
  )),
halt(0)
"""


def run_leg(leg: str, kernels: list[str]) -> list[str]:
    goal = " ".join(GOAL.format(leg=leg, kernels=",".join(kernels)).split())
    proc = subprocess.run(["swipl", "-q", "-g", goal, "-t", "halt(4)"],
                          cwd=PROLOG_DIR, capture_output=True, text=True, timeout=600)
    (AUDIT_DIR / f"sweep_{leg}.log").write_text(
        f"# exit={proc.returncode}\n# --- stdout ---\n{proc.stdout}\n# --- stderr ---\n{proc.stderr}\n")
    if proc.returncode != 0:
        print(f"FAIL leg={leg} exit={proc.returncode}\n{proc.stderr}", file=sys.stderr)
        sys.exit(proc.returncode)
    return [l for l in proc.stdout.splitlines() if l.startswith(("PAIR\t", "CELL\t"))]


def main() -> None:
    rows = []
    for leg, kernels in PILOT.items():
        rows.extend(run_leg(leg, kernels))
    (AUDIT_DIR / "sweep_results.tsv").write_text("\n".join(rows) + "\n")

    pairs = [r.split("\t") for r in rows if r.startswith("PAIR\t")]
    cells = [r.split("\t") for r in rows if r.startswith("CELL\t")]

    # exact_name all-blind sanity (agree+disp must be 0 on every pair)
    exact_nonblind = [p for p in pairs if not p[5].startswith("0/0/")]
    print(f"pairs swept: {len(pairs)}; exact_name non-all-blind pairs: "
          f"{len(exact_nonblind)} {'(EXPECTED 0!)' if exact_nonblind else '(as expected)'}")

    # C3: per-kernel conversion
    conv: dict[tuple, bool] = {}
    for p in pairs:
        key = (p[1], p[2])
        ag, d, _ = p[6].split("/")
        conv[key] = conv.get(key, False) or (int(ag) + int(d) > 0)
    n_conv = sum(conv.values())
    print(f"\nC3 floor — kernels with >=1 blind->(agree/disparity) conversion: "
          f"{n_conv}/{len(conv)} (bar >=7/10): {'PASS' if n_conv >= 7 else 'FAIL'}")
    for (leg, k), c in sorted(conv.items()):
        print(f"  {leg:<16}{k:<34}{'CONVERTED' if c else 'no conversion'}")

    # C2: contained live pairs surface as agree/disparity cells
    print("\nC2 — contained live contradiction pairs in the ACTUAL diff:")
    c2_hits = 0
    for vk, ra, rb in C2_PAIRS:
        a, b = sorted([ra, rb])
        hit = [c for c in cells if c[3] == a and c[4] == b and c[6] == vk]
        status = f"{hit[0][5]} cell: {hit[0][7]}" if hit else "NOT SURFACED"
        if hit:
            c2_hits += 1
        print(f"  {vk}\n    {a} vs {b}: {status}")
    print(f"C2 (>=1 required): {c2_hits}/3 surfaced -> {'PASS' if c2_hits >= 1 else 'FAIL'}")
    print("  (visual split pairs verification_feasibility x truth_warrant_source "
          "pre-excluded per R2_RULING — cannot align by construction)")

    if n_conv >= 7 and c2_hits >= 1 and not exact_nonblind:
        print("\nKILL-CONDITION LEG (b): NOT TRIPPED — C2 and C3 both PASS.")
    else:
        print("\nKILL-CONDITION LEG (b): TRIPPED OR SANITY FAIL — stop and write up.")
        sys.exit(2)


if __name__ == "__main__":
    main()
