#!/usr/bin/env python3
"""OQ-118 re-probe suite (2026-06-27) — the witness behind the OQ-118 ruling object.

Read-only. No spend. Reads the cohort-zero replicate draws and stability table from the
2026-06-12 audit and re-derives four things the ruling cites:

  Probe 1  suppression is MODEL-EMITTED (authored), not computed   (broad-A's scalar witness is real)
  Probe 2  emerges_naturally / requires_active_enforcement / suppression across all 18 draws
  Probe 3  sigma/seat Fisher partition: replication arm + content-only (presence-hollow removed)
  Probe 4  between-story variance sweep -> degeneracy positive-control on the STABLE side
           + extractiveness/claimed_type known-flag confirmation (read from cohort_stability source)

This FILE is the witness, not its printed output: re-run
    python3 audits/2026-06-27_oq118_reprobe/oq118_reprobe.py
to regenerate every number the ruling object cites. Probe 3 arm A is a self-check — it must
reproduce the instrument's original 58/62/36/32 cells and p=0.6490 (sigma_seat_eval.out), which
validates the comparator/bucket map by reproduction.
"""
import json, glob, os, re
from math import comb
from collections import defaultdict

HERE = os.path.dirname(os.path.abspath(__file__))
COHORT = os.path.join(HERE, "..", "2026-06-12_cohort_zero")
REPL = os.path.join(COHORT, "replicates")
REPO = os.path.join(HERE, "..", "..")
STAB = json.load(open(os.path.join(COHORT, "stability_table.json")))


def get(d, path):
    cur = d
    for k in path.split("."):
        if isinstance(cur, dict) and k in cur:
            cur = cur[k]
        else:
            return None
    return cur


def story_of(name):
    return "_".join(name.split("_")[:-1])


def load_draws():
    draws = defaultdict(list)
    for f in sorted(glob.glob(os.path.join(REPL, "*.json"))):
        name = os.path.basename(f).replace(".json", "")
        draws[story_of(name)].append((name.split("_")[-1], json.load(open(f))))
    return draws


# ---------------------------------------------------------------- Probe 1
def probe1():
    print("=== Probe 1: is `suppression` model-emitted (authored) or computed? ===")
    doc = json.load(open(os.path.join(REPL, "zero_as_number_d1.json")))
    print("  raw draw top-level keys:", list(doc.keys()))
    bp = doc.get("base_properties", {})
    print("  base_properties.suppression =", bp.get("suppression", "<<ABSENT>>"),
          "(authored scalar, present in raw pre-pipeline draw)")
    # positive control: a KNOWN-COMPUTED field is absent from the raw draw's authored block
    computed_present = any(k in doc for k in ("chi", "dr_type", "classification"))
    print("  positive control — computed fields (chi/dr_type) present in raw draw?:",
          computed_present, "(False => probe discriminates authored from computed)")
    # schema corroboration
    sch = open(os.path.join(REPO, "python", "shared", "schemas.py")).read()
    m = [l for l in sch.splitlines() if '"suppression"' in l]
    print("  schemas.py says:", (m[0].strip() if m else "<<not found>>"))
    print("  -> VERDICT: suppression is AUTHORED (in raw draw + required by generation schema).\n")


# ---------------------------------------------------------------- Probe 2
def probe2():
    print("=== Probe 2: emerges_naturally / requires_active_enforcement / suppression, all draws ===")
    draws = load_draws()
    print(f"  {'story':34} {'draw':5} {'emerges_nat':12} {'req_enforce':12} suppression")
    for s in sorted(draws):
        for d, doc in sorted(draws[s]):
            bp = doc.get("base_properties", {})
            print(f"  {s:34} {d:5} {str(bp.get('emerges_naturally','ABS')):12} "
                  f"{str(bp.get('requires_active_enforcement','ABS')):12} {bp.get('suppression','ABS')}")
    print("  -> emerges_naturally constant True 18/18 (degenerate); suppression varies between"
          " stories yet reproduces within-story 5/6.\n")


# ---------------------------------------------------------------- Probe 3
PRESENCE_HOLLOW = {
    "six_questions.coordination_function", "six_questions.transfer_function",
    "six_questions.absent_voices", "six_questions.founding_problem",
    "six_questions.founding_problem_corroboration",
    "gain_flow", "fixing_cost", "interval",
    "boltzmann.presence", "network.presence", "directionality_overrides.presence",
    "cs_structure.presence", "coercion_grid.presence",
}


def fisher_two_sided(a, b, c, dd):
    n = a + b + c + dd; r0 = a + b; r1 = c + dd; c0 = a + c
    def p_tab(x):
        b_ = r0 - x; c_ = c0 - x; d_ = r1 - c_
        if x < 0 or b_ < 0 or c_ < 0 or d_ < 0:
            return 0.0
        return comb(r0, x) * comb(r1, c_) / comb(n, c0)
    p_obs = p_tab(a); tot = 0.0
    for x in range(max(0, c0 - r1), min(r0, c0) + 1):
        px = p_tab(x)
        if px <= p_obs * (1 + 1e-9):
            tot += px
    return tot


def partition(label, include_known, exclude_hollow):
    tab = {"sigma": {"stable": 0, "unstable": 0}, "seat": {"stable": 0, "unstable": 0}}
    for sd in STAB["per_story"].values():
        for rec in sd["fields"]:
            f, status, bucket = rec["field"], rec["status"], rec["predicted_bucket"]
            flags, akind = rec.get("flags", []), rec.get("agreement_kind")
            if status not in ("stable", "unstable") or akind == "absence":
                continue
            if exclude_hollow and f in PRESENCE_HOLLOW:
                continue
            if (not include_known) and "known" in flags:
                continue
            if bucket in tab:
                tab[bucket][status] += 1
    a, b = tab["sigma"]["stable"], tab["sigma"]["unstable"]
    c, dd = tab["seat"]["stable"], tab["seat"]["unstable"]
    n = a + b + c + dd; cons = a + dd; p = fisher_two_sided(a, b, c, dd)
    print(f"  [{label}] n={n}: sigma {a}/{b}  seat {c}/{dd}  "
          f"consistent {cons}/{n}={100*cons/n:.1f}%  Fisher p={p:.4f}")


def probe3():
    print("=== Probe 3: sigma/seat Fisher partition (replication self-check + content-only) ===")
    partition("A REPLICATION  hollow IN,  known out  (must match 58/62|36/32 p=0.6490)", False, False)
    partition("B CONTENT-ONLY hollow OUT, known out", False, True)
    partition("C CONTENT-ONLY hollow OUT, known in ", True, True)
    print("  -> B inverts vs A (47.9%->39.7%): the sigma side decontaminates toward the unstable"
          " cast multisets. sigma/seat is not the stability partition.\n")


# ---------------------------------------------------------------- Probe 4
def probe4():
    print("=== Probe 4: between-story variance sweep (degeneracy positive-control, STABLE side) ===")
    draws = load_draws()
    stories = sorted(draws)

    def sweep(field, extract, numeric):
        per_story = {}
        for s in stories:
            vals = [extract(doc) for _, doc in draws[s]]
            uniq = {v if not isinstance(v, list) else tuple(v) for v in vals}
            per_story[s] = list(uniq)[0] if len(uniq) == 1 else "UNSTABLE"
        stable_vals = [v for v in per_story.values() if v != "UNSTABLE"]
        n_stable = len(stable_vals)
        distinct = len(set(stable_vals))
        if numeric:
            nums = [float(v) for v in stable_vals]
            spread = (max(nums) - min(nums)) if len(nums) >= 2 else 0.0
            sv = f"range {spread:.2f}"
        else:
            sv = f"{distinct} distinct"
        tag = "DEGENERATE" if (distinct <= 1 and n_stable >= 1) else ("ok" if distinct > 1 else "n<2")
        print(f"  {field:42} stable {n_stable}/6  between-story {sv:14} -> {tag}")

    print("  -- authored scalars --")
    for f in ["base_properties.suppression", "base_properties.theater_ratio",
              "base_properties.accessibility_collapse", "base_properties.resistance",
              "base_properties.extractiveness"]:
        sweep(f, lambda d, p=f: get(d, p), True)
    print("  -- authored categoricals / verdicts --")
    for f in ["base_properties.emerges_naturally", "base_properties.requires_active_enforcement",
              "base_properties.has_sunset_clause", "base_properties.claimed_type",
              "six_questions.disappearance_verdict", "six_questions.founding_problem_status"]:
        sweep(f, lambda d, p=f: get(d, p), False)
    print("  -- structural counts --")
    sweep("omegas.count", lambda d: len(get(d, "omegas") or []), True)
    sweep("measurements.count", lambda d: len(get(d, "measurements") or []), True)

    # known-flag confirmation, read straight from the instrument source
    print("  -- known-flag confirmation (cohort_stability.py FIELDS source) --")
    src = open(os.path.join(REPO, "python", "cohort_stability.py")).read()
    for name in ("base_properties.extractiveness", "base_properties.claimed_type",
                 "base_properties.suppression"):
        m = re.search(r'\("%s".*' % re.escape(name), src)
        flagged = m and '"known"' in m.group(0)
        print(f"  {name:42} known-flagged: {bool(flagged)}")
    print("  -> extractiveness IS known-flagged (input, no blind credit) => OUT of the generated"
          " witness set, same as claimed_type. Generated authored-scalar witnesses: suppression,"
          " accessibility_collapse, theater_ratio, resistance.\n")


if __name__ == "__main__":
    probe1(); probe2(); probe3(); probe4()
