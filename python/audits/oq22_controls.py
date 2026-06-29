#!/usr/bin/env python3
"""
OQ-22 Phase-3 POSITIVE CONTROLS — the band-screen probe must be shown to find what it reports
absent (CLAUDE.md: every probe needs a positive control; an introduced instrument is itself a claim).

Two controls, both reusing the EXACT Phase-0/1 probe goal (process_corpus) and the EXACT starved
definition (oq22_analyze.analyze) — same clause path, so a pass here licenses the default-transform
census:

  (global)  Overlay a COMPRESSED sigmoid (sigmoid_upper/lower brought together to target the
            witnessed χ span 0.20 / ceiling 0.15) on testsets. REQUIRE: (i) the overlay actually
            took effect — max observed χ collapses well below the default (else the probe measured
            the wrong engine, the overlay-took-effect-precedes-census discipline); (ii) starvation
            becomes WIDESPREAD vs the default run. Paste default-vs-compressed counts (must differ).

  (single)  Build an otherwise-HEALTHY corpus (real non-starved testsets) + ONE constructed starved
            constraint (a real testset with ε pinned to 0.02 -> χ tiny at every observer -> all
            observer-χ in one band). REQUIRE the constructed member is INDIVIDUALLY flagged starved,
            and the healthy bed stays mostly non-starved (the probe resolves a single planted member,
            not a collapsed population).

Read-only w.r.t. the committed engine/corpus. Builds a throwaway corpus dir under prolog/ (removed
after the run; the injected file content is saved into the audit dir as evidence).

Usage:  python3 python/audits/oq22_controls.py
"""

import csv
import shutil
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PROLOG_DIR = ROOT / "prolog"
OUT_DIR = ROOT / "audits" / "2026-06-28_oq22_hub_starvation"
sys.path.insert(0, str(ROOT / "python" / "audits"))
import oq22_starvation_census as census          # noqa: E402
import oq22_analyze as analyze                    # noqa: E402

# Compressed sigmoid: NARROWEST validator-permitted f(d) range.  config_schema.pl caps
# sigmoid_upper>=0.5 and requires lower<midpoint<upper, so the originally-witnessed extreme regime
# (χ ceiling 0.15) is now VALIDATOR-FORBIDDEN (a finding). We instead pin f(d) to [0.48, 0.50]
# (width 0.02) — all params in-bounds (no validator bypass) — so χ = ε·f(d)·σ has a near-zero
# cross-observer span (<=0.02·ε) and starvation becomes near-total.  asserta prepends so the
# override is the first param/2 solution (same first-solution rule as the corpus_path overlay).
# Applied POST-[stack] with retractall+assertz so exactly ONE clause exists per param (a prepended
# asserta would leave the default clause and the ordering validator backtracks into a violating
# cross-combination). All values are individually in-bounds, so this is a valid override, not a
# validator bypass; re-validation is not triggered (it is a load-time gate, already passed).
#
# χ = ε·f(d)·σ(scope) has TWO cross-observer span sources: f(d) (power/displacement) and σ(scope)
# (the 4 observers span local 0.8 / national 1.0 / global 1.2). To drive WIDESPREAD (near-total)
# starvation both must collapse: pin f(d) to [0.48,0.50] (width 0.02) AND flatten every σ to 1.0.
# (Sigmoid-only compression leaves the σ-driven span — a finding reported separately.)
SIGMOID_ONLY_PREAMBLE = (
    ":- retractall(config:param(sigmoid_upper,_)),    assertz(config:param(sigmoid_upper, 0.50)).\n"
    ":- retractall(config:param(sigmoid_midpoint,_)), assertz(config:param(sigmoid_midpoint, 0.49)).\n"
    ":- retractall(config:param(sigmoid_lower,_)),    assertz(config:param(sigmoid_lower, 0.48)).\n"
)
_FLATTEN_SIGMA = "".join(
    f":- retractall(config:param(scope_modifier_{s},_)), assertz(config:param(scope_modifier_{s}, 1.0)).\n"
    for s in ["local", "regional", "national", "continental", "global", "universal"]
)
COMPRESSED_PREAMBLE = SIGMOID_ONLY_PREAMBLE + _FLATTEN_SIGMA

# Healthy bed: real testsets the DEFAULT-transform census flagged non-starved (full map). Picked
# from census_testsets.tsv (has_full_map=True, starved=False) — large χ spans, clear type variation.
HEALTHY_BED = [
    "apoe4_mitochondrial_vulnerability",
    "access_barrier_reading",
    "authoritative_specification_reading",
    "behavioral_adoption_friction",
    "ability_ceiling_reading",
    "adjunctification_of_university_teaching_c0",
    "animal_status_kernel__property_reading",
    "basic_law_interpretive_authority__parliamentary_sovereignty_reading",
    "bitcoin_whitepaper_purpose__nakamoto_oracle_opacity",
    "border_control_legitimacy__freedom_of_movement_primary",
]
DONOR = "apoe4_mitochondrial_vulnerability"     # copied + ε pinned to 0.02 to construct the starved one
INJECT_ID = "injected_starved_01"
INJECT_EPS = 0.02


def max_chi(name):
    rows = list(csv.DictReader((OUT_DIR / f"obs_{name}.tsv").open(), delimiter="\t"))
    vals = [float(r["chi"]) for r in rows if r["chi"] != "unknown"]
    return max(vals) if vals else None


def build_injected_file(dst_dir):
    """Copy DONOR testset, rename id+module everywhere, pin extractiveness/base_extractiveness to
    INJECT_EPS. Returns the saved-evidence text."""
    src = (PROLOG_DIR / "testsets" / f"{DONOR}.pl").read_text()
    txt = src.replace(DONOR, INJECT_ID)            # id atom + module name (constraint_<id>) both covered
    out = []
    for line in txt.splitlines():
        s = line.strip()
        if s.startswith(f"narrative_ontology:constraint_metric({INJECT_ID}, extractiveness,"):
            line = f"narrative_ontology:constraint_metric({INJECT_ID}, extractiveness, {INJECT_EPS})."
        elif s.startswith(f"domain_priors:base_extractiveness({INJECT_ID},"):
            line = f"domain_priors:base_extractiveness({INJECT_ID}, {INJECT_EPS})."
        out.append(line)
    text = "\n".join(out) + "\n"
    (dst_dir / f"{INJECT_ID}.pl").write_text(text)
    return text


def run_global_control():
    print("\n" + "#" * 78 + "\n# CONTROL (global): compressed transform -> widespread starvation\n" + "#" * 78)
    ok_d = census.process_corpus("testsets", "testsets")                       # default baseline
    ok_s = census.process_corpus("testsets_sigonly", "testsets",
                                 preamble=SIGMOID_ONLY_PREAMBLE, expected=109)  # f-only
    ok_c = census.process_corpus("testsets_compressed", "testsets",
                                 preamble=COMPRESSED_PREAMBLE, expected=109)     # f + σ flat
    if not (ok_d and ok_s and ok_c):
        print("  !! probe run failed; control INVALID")
        return False
    d_chi, c_chi = max_chi("testsets"), max_chi("testsets_compressed")
    print(f"\n  max observed χ:  default={d_chi:.4f}   compressed={c_chi:.4f}")
    # U=0.50 and σ=1.0 cap χ = ε·f·σ <= 0.50; default U=1.50 + σ up to 1.2 lets χ exceed 1.
    overlay_took = c_chi is not None and c_chi <= 0.55 and c_chi < d_chi
    print(f"  overlay-took-effect (compressed χ capped by U=0.5,σ=1.0: <=0.55 and below default): {overlay_took}")
    s_d = analyze.analyze("testsets")
    s_s = analyze.analyze("testsets_sigonly")
    s_c = analyze.analyze("testsets_compressed")
    N = s_d["n_constraints"]
    nd, ns, nc = s_d["n_starved"], s_s["n_starved"], s_c["n_starved"]
    print(f"\n  STARVED count:  default={nd}/{N}   sigmoid-only={ns}/{N}   sigmoid+σ-flat={nc}/{N}")
    print(f"  (sigmoid-only leaves the σ-driven cross-observer span -> only partial starvation: the"
          f" second Hub-1 span source)")
    # widespread = near-total under full compression, AND strictly increasing with compression depth.
    fired = overlay_took and nd < ns < nc and nc >= N * 0.8
    print(f"  GLOBAL CONTROL {'FIRED (compression -> widespread starvation; counts differ)' if fired else 'DID NOT FIRE'}")
    return fired


def run_single_control():
    print("\n" + "#" * 78 + "\n# CONTROL (single): one constructed starved member in a healthy bed\n" + "#" * 78)
    tmp = PROLOG_DIR / "oq22_inject_tmp"
    if tmp.exists():
        shutil.rmtree(tmp)
    tmp.mkdir()
    try:
        for cid in HEALTHY_BED:
            shutil.copy(PROLOG_DIR / "testsets" / f"{cid}.pl", tmp / f"{cid}.pl")
        inj_text = build_injected_file(tmp)
        (OUT_DIR / f"{INJECT_ID}.pl").write_text(inj_text)        # save evidence
        n_expected = len(HEALTHY_BED) + 1
        ok = census.process_corpus("inject_bed", "oq22_inject_tmp", expected=n_expected)
        if not ok:
            print("  !! probe run failed; control INVALID")
            return False
        s = analyze.analyze("inject_bed")
        rows = list(csv.DictReader((OUT_DIR / "census_inject_bed.tsv").open(), delimiter="\t"))
        inj = next((r for r in rows if r["id"] == INJECT_ID), None)
        bed = [r for r in rows if r["id"] != INJECT_ID]
        inj_starved = inj is not None and inj["starved"] == "True"
        bed_starved = sum(1 for r in bed if r["starved"] == "True")
        print(f"\n  injected '{INJECT_ID}' (ε={INJECT_EPS}): "
              f"starved={inj_starved}  chi=[{inj['chi_min']},{inj['chi_max']}]  mtypes={inj['mtype_vec']}")
        print(f"  healthy bed: {bed_starved}/{len(bed)} starved (expect mostly non-starved)")
        fired = inj_starved and bed_starved < len(bed)
        print(f"  SINGLE CONTROL {'FIRED (planted member individually resolved)' if fired else 'DID NOT FIRE'}")
        return fired
    finally:
        shutil.rmtree(tmp, ignore_errors=True)


def main():
    g = run_global_control()
    s = run_single_control()
    print("\n" + "=" * 78)
    print(f"PHASE-3 CONTROLS: global={'PASS' if g else 'FAIL'}  single={'PASS' if s else 'FAIL'}")
    print("=" * 78)
    return 0 if (g and s) else 1


if __name__ == "__main__":
    sys.exit(main())
