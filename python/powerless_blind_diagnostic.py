#!/usr/bin/env python3
"""
Powerless Blind Diagnostic
===========================
Identifies and diagnoses constraints where the engine returns 'unknown' at the
powerless perspective but returns a non-unknown type at analytical perspective.

CURRENT STATE (February 2026, post-immutability-wall fix):
  The powerless context is context(agent_power(powerless), time_horizon(biographical),
  exit_options(trapped), spatial_scope(local)).

  In constraint_indexing.pl, effective_immutability(biographical, trapped, mountain) —
  the powerless POV sees constraints as IMMUTABLE (mountain-like).

  However, drl_core.pl now has snare_immutability_check/1 which checks whether ANY
  standard higher-power perspective sees changeability (rope). Since moderate context
  (biographical/mobile) gives rope, the snare gate is NO LONGER blocked at powerless.

  The rope gate still uses effective_immutability_for_context directly, so rope
  remains blocked from powerless.

  Coalition modeling: When powerless AND eps >= 0.46 AND supp >= 0.60 AND
  victim_count >= 3, power is upgraded to organized (pi=0.4 instead of 1.5).
  This significantly reduces chi for many constraints.

  Gate consequences:
    - Mountain gate: immutability=mountain PASSES, but metric gates
      (eps <= 0.25, supp <= 0.05) fail for high-extraction constraints.
    - Snare gate: snare_immutability_check passes (moderate context gives rope).
      Gate is metric-gated: chi >= 0.66, eps >= 0.46, supp >= 0.60.
      Coalition modeling may reduce chi below threshold.
    - Rope gate:  requires effective_immutability = rope -> STILL BLOCKED.
    - Scaffold:   no immutability check, but requires has_coordination_function,
                  scaffold_temporality, and theater <= 0.70.
    - Tangled Rope: no immutability check, but requires has_coordination_function,
                    has_asymmetric_extraction, requires_active_enforcement, AND
                    metric gates (0.40 <= chi <= 0.90, eps >= 0.30, supp >= 0.40).
    - Piton:     no immutability check, but requires theater >= 0.70 AND chi <= 0.25.

Parses:
  - fingerprint_report.md  -> shift patterns and member constraints
  - corpus_data.json       -> epsilon, suppression, domain, claimed_type
  - testset .pl files      -> theater_ratio, structural predicates, victim counts

Gate thresholds (February 2026):
  Mountain:      eps <= 0.25, supp <= 0.05, immutability = mountain
  Snare:         chi >= 0.66, eps >= 0.46, supp >= 0.60, immutability via snare_immutability_check
  Scaffold:      chi <= 0.30, theater <= 0.70, has_coordination, has_temporality
  Rope:          chi <= 0.35, eps <= 0.45, immutability = rope (STILL BLOCKED at powerless)
  Tangled Rope:  0.40 <= chi <= 0.90, eps >= 0.30, supp >= 0.40,
                 requires_enforcement, has_coordination, has_asymmetric_extraction
  Piton:         chi <= 0.25, eps > 0.10, theater >= 0.70

Chi formula: chi = eps * pi(Power) * sigma(Scope)
  Coalition: if powerless AND eps >= 0.46 AND supp >= 0.60 AND victims >= 3,
             power upgrades to organized (pi=0.4)
"""

import json
import re
from collections import defaultdict, Counter
from pathlib import Path

# ── Paths ────────────────────────────────────────────────────────────────────

BASE = Path(__file__).resolve().parent.parent
FINGERPRINT = BASE / "outputs" / "fingerprint_report.md"
CORPUS_DATA = BASE / "outputs" / "corpus_data.json"
TESTSET_DIR = BASE / "prolog" / "testsets"
PROBSET_DIR = BASE / "prolog" / "probsets"

# ── Constants ────────────────────────────────────────────────────────────────

# Power modifiers (pi) — v5.0: sigmoid-derived
from sigmoid import POWER_MODIFIERS as _PM
PI_POWERLESS = _PM["powerless"]
PI_ORGANIZED = _PM["organized"]
PI_ANALYTICAL = _PM["analytical"]

# Scope modifiers (sigma)
SIGMA_LOCAL = 0.8
SIGMA_GLOBAL = 1.2

# Coalition modeling: powerless upgrades to organized when snare-like + enough victims
COALITION_VICTIM_THRESHOLD = 3  # critical_mass_threshold in config.pl
COALITION_EPS_FLOOR = 0.46      # snare_epsilon_floor
COALITION_SUPP_FLOOR = 0.60     # snare_suppression_floor

# Effective immutability for powerless context (biographical/trapped) = mountain
# BUT snare_immutability_check passes because moderate context (biographical/mobile) = rope.
# Only the ROPE gate is still structurally blocked at powerless.
POWERLESS_IMMUTABILITY = "mountain"   # biographical + trapped -> mountain

# Gate metric thresholds
MOUNTAIN_EPS_MAX = 0.25
MOUNTAIN_SUPP_MAX = 0.05
SNARE_CHI_FLOOR = 0.66
SNARE_EPS_FLOOR = 0.46
SNARE_SUPP_FLOOR = 0.60
SCAFFOLD_CHI_CEIL = 0.30
SCAFFOLD_THEATER_MAX = 0.70
ROPE_CHI_CEIL = 0.35
ROPE_EPS_CEIL = 0.45
TANGLED_CHI_FLOOR = 0.40
TANGLED_CHI_CEIL = 0.90
TANGLED_EPS_FLOOR = 0.30
TANGLED_SUPP_FLOOR = 0.40
PITON_CHI_CEIL = 0.25
PITON_EPS_FLOOR = 0.10
PITON_THEATER_FLOOR = 0.70


# ── 1. Parse fingerprint report ─────────────────────────────────────────────

def parse_fingerprint_report(path):
    """Return dict: shift_pattern_str -> list of constraint IDs."""
    families = {}
    current_pattern = None
    current_members = []

    with open(path, "r") as f:
        for line in f:
            line = line.rstrip("\n")
            m = re.match(r"^###\s+`(shift\([^)]+\))`\s+.*?(\d+)\s+constraints?", line)
            if m:
                if current_pattern is not None:
                    families[current_pattern] = current_members
                current_pattern = m.group(1)
                current_members = []
                continue
            m2 = re.match(r"^- `([^`]+)`", line)
            if m2 and current_pattern is not None:
                current_members.append(m2.group(1))
                continue
            if line.startswith("## ") and current_pattern is not None:
                families[current_pattern] = current_members
                current_pattern = None
                current_members = []

    if current_pattern is not None:
        families[current_pattern] = current_members
    return families


def parse_shift_tuple(pattern_str):
    m = re.match(r"shift\(([^,]+),\s*([^,]+),\s*([^,]+),\s*([^)]+)\)", pattern_str)
    if not m:
        return None
    return tuple(x.strip() for x in m.groups())


# ── 2. Identify powerless_blind constraints ──────────────────────────────────

def find_powerless_blind(families):
    results = []
    for pattern_str, members in families.items():
        tup = parse_shift_tuple(pattern_str)
        if tup is None:
            continue
        powerless_type, _, _, analytical_type = tup
        if powerless_type == "unknown" and analytical_type != "unknown":
            for cid in members:
                results.append((cid, tup, pattern_str))
    return results


# ── 3. Load corpus data ─────────────────────────────────────────────────────

def load_corpus_data(path):
    with open(path, "r") as f:
        data = json.load(f)
    corpus = {}
    constraints = data.get("constraints", {})
    for cid, info in constraints.items():
        metrics = info.get("metrics", {})
        corpus[cid] = {
            "extractiveness": metrics.get("extractiveness"),
            "suppression": metrics.get("suppression"),
            "domain": info.get("domain"),
            "claimed_type": info.get("claimed_type"),
            "has_beneficiaries": bool(info.get("beneficiaries")),
            "has_victims": bool(info.get("victims")),
            "requires_enforcement": metrics.get("requires_enforcement"),
            "emerges_naturally": metrics.get("emerges_naturally"),
        }
    return corpus


# ── 4. Extract theater_ratio and structural predicates from .pl files ────────

def extract_pl_metadata(*dirs):
    """Scan .pl files for theater_ratio, structural predicates, and victim counts."""
    theater = {}
    has_enforcement = set()
    has_coordination = set()
    has_asymmetric = set()
    has_sunset = set()
    victim_counts = defaultdict(int)  # constraint_id -> number of distinct victims

    re_theater = [
        re.compile(r"(?:narrative_ontology:)?constraint_metric\(\s*([a-zA-Z0-9_]+)\s*,\s*theater_ratio\s*,\s*([0-9.]+)\s*\)"),
        re.compile(r"(?:domain_priors:)?theater_ratio\(\s*([a-zA-Z0-9_]+)\s*,\s*([0-9.]+)\s*\)"),
    ]
    re_enforcement = re.compile(r"(?:domain_priors:)?requires_active_enforcement\(\s*([a-zA-Z0-9_]+)\s*\)")
    re_beneficiary = re.compile(r"(?:narrative_ontology:)?constraint_beneficiary\(\s*([a-zA-Z0-9_]+)\s*,")
    re_victim = re.compile(r"(?:narrative_ontology:)?constraint_victim\(\s*([a-zA-Z0-9_]+)\s*,")
    re_sunset = re.compile(r"(?:narrative_ontology:)?has_sunset_clause\(\s*([a-zA-Z0-9_]+)\s*\)")

    for d in dirs:
        if not d.exists():
            continue
        for plfile in d.glob("*.pl"):
            try:
                text = plfile.read_text(errors="replace")
            except Exception:
                continue
            for pat in re_theater:
                for m in pat.finditer(text):
                    theater[m.group(1)] = float(m.group(2))
            for m in re_enforcement.finditer(text):
                has_enforcement.add(m.group(1))
            for m in re_beneficiary.finditer(text):
                has_coordination.add(m.group(1))
            for m in re_victim.finditer(text):
                has_asymmetric.add(m.group(1))
                victim_counts[m.group(1)] += 1
            for m in re_sunset.finditer(text):
                has_sunset.add(m.group(1))

    return theater, has_enforcement, has_coordination, has_asymmetric, has_sunset, victim_counts


# ── 5. Gate testing with full structural logic ───────────────────────────────

def compute_chi(epsilon, pi_power, sigma_scope):
    if epsilon is None:
        return None
    return epsilon * pi_power * sigma_scope


def compute_chi_powerless(epsilon, supp, victim_count):
    """Compute chi at powerless/local context, accounting for coalition modeling.

    Coalition upgrade: if eps >= 0.46 AND supp >= 0.60 AND victims >= 3,
    powerless is upgraded to organized (pi=0.4 instead of 1.5).
    """
    if epsilon is None:
        return None, False
    coalition = (
        epsilon >= COALITION_EPS_FLOOR
        and supp is not None and supp >= COALITION_SUPP_FLOOR
        and victim_count >= COALITION_VICTIM_THRESHOLD
    )
    pi = PI_ORGANIZED if coalition else PI_POWERLESS
    return epsilon * pi * SIGMA_LOCAL, coalition


def test_powerless_gates(chi, eps, supp, theater, has_coord, has_asym, has_enf, has_sun, coalition):
    """
    Test all six gates at powerless context (biographical/trapped/local).
    Returns dict of gate_name -> (passes, failure_reasons).

    POST-FIX STATE:
    - Snare: snare_immutability_check passes (moderate context gives rope).
      Gate is purely metric-gated. Coalition modeling may reduce chi.
    - Rope: STILL BLOCKED — uses effective_immutability_for_context directly.
    """
    results = {}

    # Mountain: immutability=mountain PASSES, but metric gates usually fail
    mtn_fails = []
    if eps is not None and eps > MOUNTAIN_EPS_MAX:
        mtn_fails.append(f"eps={eps:.2f} > {MOUNTAIN_EPS_MAX}")
    if supp is not None and supp > MOUNTAIN_SUPP_MAX:
        mtn_fails.append(f"supp={supp:.2f} > {MOUNTAIN_SUPP_MAX}")
    if eps is None:
        mtn_fails.append("eps=None")
    results["mountain"] = (len(mtn_fails) == 0, mtn_fails)

    # Snare: snare_immutability_check passes (moderate std context gives rope).
    # Now purely metric-gated. Coalition modeling may reduce chi below threshold.
    snare_fails = []
    if chi is not None and chi < SNARE_CHI_FLOOR:
        reason = f"chi={chi:.4f} < {SNARE_CHI_FLOOR}"
        if coalition:
            reason += " (coalition: pi=0.4)"
        snare_fails.append(reason)
    if eps is not None and eps < SNARE_EPS_FLOOR:
        snare_fails.append(f"eps={eps:.2f} < {SNARE_EPS_FLOOR}")
    if supp is not None and supp < SNARE_SUPP_FLOOR:
        snare_fails.append(f"supp={supp:.2f} < {SNARE_SUPP_FLOOR}")
    if chi is None:
        snare_fails.append("chi=None")
    if eps is None:
        snare_fails.append("eps=None")
    results["snare"] = (len(snare_fails) == 0, snare_fails)

    # Rope: STILL STRUCTURALLY BLOCKED — effective_immutability_for_context unchanged
    results["rope"] = (False, ["BLOCKED: immutability=mountain (need rope)"])

    # Scaffold: no immutability check
    scf_fails = []
    if chi is not None and chi > SCAFFOLD_CHI_CEIL:
        scf_fails.append(f"chi={chi:.4f} > {SCAFFOLD_CHI_CEIL}")
    if not has_coord:
        scf_fails.append("missing has_coordination_function")
    if theater is not None and theater > SCAFFOLD_THEATER_MAX:
        scf_fails.append(f"theater={theater:.2f} > {SCAFFOLD_THEATER_MAX}")
    if not has_sun and has_enf:
        scf_fails.append("no sunset_clause AND requires_enforcement")
    results["scaffold"] = (len(scf_fails) == 0, scf_fails)

    # Tangled Rope: no immutability check
    tr_fails = []
    if chi is not None and chi < TANGLED_CHI_FLOOR:
        tr_fails.append(f"chi={chi:.4f} < {TANGLED_CHI_FLOOR}")
    if chi is not None and chi > TANGLED_CHI_CEIL:
        tr_fails.append(f"chi={chi:.4f} > {TANGLED_CHI_CEIL}")
    if eps is not None and eps < TANGLED_EPS_FLOOR:
        tr_fails.append(f"eps={eps:.2f} < {TANGLED_EPS_FLOOR}")
    if supp is not None and supp < TANGLED_SUPP_FLOOR:
        tr_fails.append(f"supp={supp:.2f} < {TANGLED_SUPP_FLOOR}")
    if not has_enf:
        tr_fails.append("missing requires_active_enforcement")
    if not has_coord:
        tr_fails.append("missing has_coordination_function")
    if not has_asym:
        tr_fails.append("missing has_asymmetric_extraction")
    if chi is None:
        tr_fails.append("chi=None")
    results["tangled_rope"] = (len(tr_fails) == 0, tr_fails)

    # Piton: no immutability check
    pit_fails = []
    if chi is not None and chi > PITON_CHI_CEIL:
        pit_fails.append(f"chi={chi:.4f} > {PITON_CHI_CEIL}")
    if eps is not None and eps <= PITON_EPS_FLOOR:
        pit_fails.append(f"eps={eps:.2f} <= {PITON_EPS_FLOOR}")
    if theater is None:
        pit_fails.append("theater=None (metric not found)")
    elif theater < PITON_THEATER_FLOOR:
        pit_fails.append(f"theater={theater:.2f} < {PITON_THEATER_FLOOR}")
    if chi is None:
        pit_fails.append("chi=None")
    results["piton"] = (len(pit_fails) == 0, pit_fails)

    return results


# ── 6. Classify into subpopulations ──────────────────────────────────────────

def classify_subpopulation(rec):
    """
    Classify a powerless_blind constraint into a subpopulation.

    Post-fix: snare is metric-gated (not blocked), rope is still blocked.
    Coalition modeling reduces chi for high-extraction constraints with
    enough victims, which is now a major root cause.
    """
    eps = rec["eps"]
    supp = rec["supp"]
    chi_p = rec["chi_powerless"]
    theater = rec["theater"]
    analytical_type = rec["analytical_type"]
    coalition = rec.get("coalition", False)

    if eps is None:
        return "no_metrics"

    # (a) "Coalition chi reduction" -- constraint has high base eps but coalition
    #     modeling upgrades powerless to organized (pi=0.4), dropping chi below
    #     snare threshold. The extraction is real but collective action is modeled.
    if coalition and chi_p is not None and chi_p < SNARE_CHI_FLOOR:
        return "coalition_chi_reduction"

    # (b) "Dead zone" -- chi falls in gaps between gate ranges.
    #     Dead zone = chi > 0.30 and (chi < 0.40 or chi > 0.90)
    #     AND fails mountain (eps > 0.25 or supp > 0.05)
    if chi_p is not None:
        in_dead_zone_gap = chi_p > 0.30 and chi_p < 0.40
        above_tangled_ceil = chi_p > 0.90
        mountain_metrics_fail = (eps > MOUNTAIN_EPS_MAX) or (supp is not None and supp > MOUNTAIN_SUPP_MAX)

        if mountain_metrics_fail and (in_dead_zone_gap or above_tangled_ceil):
            return "dead_zone"

    # (c) "Missing structural flag" -- would qualify for tangled_rope on metrics
    #     (0.40 <= chi <= 0.90) but missing structural predicates OR supp/eps short
    if chi_p is not None and 0.40 <= chi_p <= 0.90:
        eps_ok = eps >= TANGLED_EPS_FLOOR
        supp_ok = supp is not None and supp >= TANGLED_SUPP_FLOOR
        has_struct = rec["has_enf"] and rec["has_coord"] and rec["has_asym"]
        if not (eps_ok and supp_ok and has_struct):
            return "missing_structural"

    # (d) "Wings candidates" -- low eps (<=0.45), analytical = rope or mountain,
    #     chi_powerless in lower range, but fails mountain metric gates and
    #     scaffold/piton structural checks. Rope gate still blocked.
    if (eps <= 0.45
            and analytical_type in ("rope", "mountain")
            and chi_p is not None and chi_p <= 0.30):
        return "wings_candidate"

    # (e) "High-chi gap" -- chi > 0.90 (above tangled_rope ceiling)
    #     but doesn't meet snare metrics or mountain
    if chi_p is not None and chi_p > 0.90:
        return "high_chi_gap"

    # (f) "Other"
    return "other"


# ── 7. Histogram helper ─────────────────────────────────────────────────────

def make_histogram(values, bin_width=0.05):
    bins = defaultdict(int)
    for v in values:
        if v is None:
            bins["None"] += 1
        else:
            lo = round((v // bin_width) * bin_width, 4)
            hi = round(lo + bin_width, 4)
            label = f"[{lo:.2f}, {hi:.2f})"
            bins[label] += 1
    return sorted(bins.items())


def print_histogram(values, bin_width=0.05, label="Values"):
    hist = make_histogram(values, bin_width)
    if not hist:
        print("  (empty)")
        return
    max_count = max(c for _, c in hist) if hist else 1
    bar_scale = 50.0 / max_count if max_count > 0 else 1
    print(f"  {'Bin':<18} {'Count':>5}  Bar")
    print(f"  {'~'*18} {'~'*5}  {'~'*52}")
    for lbl, count in hist:
        bar = "#" * int(count * bar_scale)
        print(f"  {lbl:<18} {count:>5}  {bar}")


# ── Main ─────────────────────────────────────────────────────────────────────

def main():
    print("=" * 80)
    print("POWERLESS BLIND DIAGNOSTIC")
    print("Structural Dynamics Model -- February 2026 Gates")
    print("=" * 80)
    print()

    # ── Parse fingerprint report ─────────────────────────────────────────
    print("[1] Parsing fingerprint report...")
    families = parse_fingerprint_report(FINGERPRINT)
    total_constraints = sum(len(v) for v in families.values())
    print(f"    {len(families)} shift families, {total_constraints} total constraints")
    print()

    # ── Find powerless_blind ─────────────────────────────────────────────
    print("[2] Identifying powerless_blind constraints...")
    pb_list = find_powerless_blind(families)
    print(f"    Found {len(pb_list)} powerless_blind constraints")
    print()

    analytical_counts = Counter(tup[3] for _, tup, _ in pb_list)
    print("    Analytical type distribution:")
    for atype, count in analytical_counts.most_common():
        print(f"      {atype}: {count}")
    print()

    pattern_counts = Counter(pat for _, _, pat in pb_list)
    print("    Shift pattern distribution:")
    for pat, count in pattern_counts.most_common():
        print(f"      {pat}  ({count})")
    print()

    # ── Load corpus data ─────────────────────────────────────────────────
    print("[3] Loading corpus data...")
    corpus = load_corpus_data(CORPUS_DATA)
    print(f"    {len(corpus)} constraints in corpus_data.json")
    print()

    # ── Extract .pl metadata ─────────────────────────────────────────────
    print("[4] Extracting theater_ratio and structural predicates from .pl files...")
    theater_map, enforcement_set, coordination_set, asymmetric_set, sunset_set, victim_count_map = \
        extract_pl_metadata(TESTSET_DIR, PROBSET_DIR)
    print(f"    theater_ratio:              {len(theater_map)} constraints")
    print(f"    requires_active_enforcement: {len(enforcement_set)} constraints")
    print(f"    has_coordination (beneficiary): {len(coordination_set)} constraints")
    print(f"    has_asymmetric (victim):     {len(asymmetric_set)} constraints")
    print(f"    has_sunset_clause:           {len(sunset_set)} constraints")
    print(f"    coalition-eligible (victims >= {COALITION_VICTIM_THRESHOLD}): "
          f"{sum(1 for c in victim_count_map.values() if c >= COALITION_VICTIM_THRESHOLD)} constraints")
    print()

    # ── Compute metrics and classify ─────────────────────────────────────
    print("[5] Computing chi and testing gates for each constraint...")
    print()

    records = []
    missing_corpus = 0

    for cid, tup, pattern_str in pb_list:
        _, moderate_type, institutional_type, analytical_type = tup

        c_data = corpus.get(cid)
        if c_data is None:
            for k, v in corpus.items():
                if k.lower() == cid.lower():
                    c_data = v
                    break

        if c_data is None:
            missing_corpus += 1
            eps = supp = None
            domain = claimed_type = "?"
            has_benef = has_vict = False
            req_enf_corpus = None
        else:
            eps = c_data["extractiveness"]
            supp = c_data["suppression"]
            domain = c_data.get("domain", "?")
            claimed_type = c_data.get("claimed_type", "?")
            has_benef = c_data.get("has_beneficiaries", False)
            has_vict = c_data.get("has_victims", False)
            req_enf_corpus = c_data.get("requires_enforcement")

        theater = theater_map.get(cid)
        has_enf = cid in enforcement_set or (req_enf_corpus is True)
        has_coord = cid in coordination_set or has_benef
        has_asym = cid in asymmetric_set or has_vict
        has_sun = cid in sunset_set

        n_victims = victim_count_map.get(cid, 0)
        chi_p, coalition = compute_chi_powerless(eps, supp, n_victims)
        chi_a = compute_chi(eps, PI_ANALYTICAL, SIGMA_GLOBAL)

        gate_results = test_powerless_gates(
            chi_p, eps, supp, theater, has_coord, has_asym, has_enf, has_sun, coalition
        )

        rec = {
            "cid": cid, "shift": tup, "pattern": pattern_str,
            "analytical_type": analytical_type,
            "moderate_type": moderate_type,
            "institutional_type": institutional_type,
            "eps": eps, "supp": supp, "theater": theater,
            "domain": domain, "claimed_type": claimed_type,
            "chi_powerless": chi_p, "chi_analytical": chi_a,
            "has_enf": has_enf, "has_coord": has_coord,
            "has_asym": has_asym, "has_sun": has_sun,
            "coalition": coalition, "n_victims": n_victims,
            "gate_results": gate_results,
        }
        rec["subpopulation"] = classify_subpopulation(rec)
        records.append(rec)

    if missing_corpus > 0:
        print(f"    WARNING: {missing_corpus} constraints not found in corpus_data.json")
        print()

    # ══════════════════════════════════════════════════════════════════════
    # ROOT CAUSE ANALYSIS
    # ══════════════════════════════════════════════════════════════════════

    print("=" * 80)
    print("ROOT CAUSE ANALYSIS (POST-IMMUTABILITY-WALL FIX)")
    print("=" * 80)
    print()
    print("  The powerless context is:")
    print("    context(agent_power(powerless), time_horizon(biographical),")
    print("            exit_options(trapped), spatial_scope(local))")
    print()
    print("  In constraint_indexing.pl:")
    print("    effective_immutability(biographical, trapped, mountain).")
    print()
    print("  FIXED: snare_immutability_check/1 in drl_core.pl now checks whether ANY")
    print("  standard higher-power perspective sees changeability (rope). Since the")
    print("  moderate context (biographical/mobile) gives rope, the snare gate is")
    print("  NO LONGER structurally blocked at powerless. It is purely metric-gated.")
    print()
    print("  STILL BLOCKED: The rope gate still uses effective_immutability_for_context")
    print("  directly, so rope classification remains blocked from powerless.")
    print()
    print("  COALITION MODELING: When eps >= 0.46 AND supp >= 0.60 AND victims >= 3,")
    print("  powerless is upgraded to organized (pi=0.4 instead of 1.5). This reduces")
    print("  chi significantly, often below snare/tangled_rope thresholds.")
    print()

    with_data = [r for r in records if r["eps"] is not None]
    snare_pass = sum(1 for r in with_data if r["gate_results"]["snare"][0])
    coalition_count = sum(1 for r in with_data if r["coalition"])
    rope_metric_pass = sum(1 for r in with_data
        if r["chi_powerless"] is not None and r["chi_powerless"] <= ROPE_CHI_CEIL
        and r["eps"] <= ROPE_EPS_CEIL)

    print(f"  Of {len(with_data)} constraints with metrics:")
    print(f"    {snare_pass} pass snare gate (metric-gated, immutability resolved)")
    print(f"    {coalition_count} have coalition upgrade (powerless -> organized, chi reduced)")
    print(f"    {rope_metric_pass} would pass rope metrics (but rope gate still immutability-blocked)")
    print()

    # ══════════════════════════════════════════════════════════════════════
    # GATE FAILURE ANALYSIS
    # ══════════════════════════════════════════════════════════════════════

    print("=" * 80)
    print("GATE FAILURE ANALYSIS AT POWERLESS CONTEXT")
    print("=" * 80)
    print()

    gate_order = ["mountain", "snare", "rope", "scaffold", "tangled_rope", "piton"]
    gate_fail_reasons = defaultdict(lambda: Counter())
    gate_pass_counts = Counter()

    for r in records:
        for gate_name, (passes, failures) in r["gate_results"].items():
            if passes:
                gate_pass_counts[gate_name] += 1
            for f in failures:
                gate_fail_reasons[gate_name][f] += 1

    for gate_name in gate_order:
        n_pass = gate_pass_counts.get(gate_name, 0)
        blocked = "(STILL BLOCKED: immutability)" if gate_name == "rope" else ""
        print(f"  {gate_name.upper()} gate: {n_pass}/{len(records)} pass {blocked}")
        if gate_name in gate_fail_reasons:
            for reason, count in gate_fail_reasons[gate_name].most_common(5):
                print(f"    {reason}  ({count}x)")
        print()

    # ══════════════════════════════════════════════════════════════════════
    # SUBPOPULATION ANALYSIS
    # ══════════════════════════════════════════════════════════════════════

    print("=" * 80)
    print("SUBPOPULATION ANALYSIS")
    print("=" * 80)
    print()

    subpops = defaultdict(list)
    for r in records:
        subpops[r["subpopulation"]].append(r)

    subpop_labels = {
        "coalition_chi_reduction": "Coalition chi reduction (powerless->organized, chi drops below snare threshold)",
        "dead_zone":          "Dead zone (chi between gate ranges: 0.30<chi<0.40 or chi>0.90)",
        "missing_structural": "Missing structural flag (tangled_rope chi range but fails predicates/metrics)",
        "wings_candidate":    "Wings candidates (low eps, analytical=rope/mtn, rope gate still blocked)",
        "high_chi_gap":       "High-chi gap (chi_p > 0.90, above tangled_rope ceiling)",
        "no_metrics":         "No metrics (constraint not found in corpus_data.json)",
        "other":              "Other (does not fit above categories)",
    }

    sp_order = ["coalition_chi_reduction", "dead_zone", "missing_structural", "wings_candidate", "high_chi_gap", "no_metrics", "other"]

    print("  SUMMARY COUNTS:")
    print("  " + "-" * 72)
    total_classified = 0
    for sp_key in sp_order:
        count = len(subpops.get(sp_key, []))
        total_classified += count
        label = subpop_labels.get(sp_key, sp_key)
        pct = (count / len(records) * 100) if len(records) > 0 else 0
        print(f"    {count:>4}  ({pct:>5.1f}%)  {label}")
    print(f"    {'~' * 50}")
    print(f"    {total_classified:>4}          TOTAL")
    print()

    # ══════════════════════════════════════════════════════════════════════
    # TOP EXAMPLES BY SUBPOPULATION
    # ══════════════════════════════════════════════════════════════════════

    print("=" * 80)
    print("TOP EXAMPLES BY SUBPOPULATION")
    print("=" * 80)

    for sp_key in sp_order:
        members = subpops.get(sp_key, [])
        if not members:
            continue

        label = subpop_labels.get(sp_key, sp_key)
        print()
        print(f"  --- {label} ---")
        print(f"  --- {len(members)} constraints ---")
        print()

        members_sorted = sorted(members, key=lambda r: (r["chi_powerless"] is None, -(r["chi_powerless"] or 0)))

        # Domain distribution
        domain_counts = Counter(r["domain"] for r in members)
        print(f"    Domain distribution: ", end="")
        print(", ".join(f"{d}={c}" for d, c in domain_counts.most_common(6)))

        # Analytical type distribution
        at_counts = Counter(r["analytical_type"] for r in members)
        print(f"    Analytical types:    ", end="")
        print(", ".join(f"{a}={c}" for a, c in at_counts.most_common()))
        print()

        # Table header
        n_show = min(8, len(members_sorted))
        hdr = f"    {'ID':<42} {'eps':>5} {'supp':>5} {'thtr':>5} {'chi_p':>7} {'chi_a':>7} {'enf':>3} {'crd':>3} {'asy':>3} {'coa':>3} {'anal':>7}"
        print(hdr)
        print(f"    {'~'*42} {'~'*5} {'~'*5} {'~'*5} {'~'*7} {'~'*7} {'~'*3} {'~'*3} {'~'*3} {'~'*3} {'~'*7}")

        for r in members_sorted[:n_show]:
            eps_s = f"{r['eps']:.2f}" if r['eps'] is not None else "  -  "
            supp_s = f"{r['supp']:.2f}" if r['supp'] is not None else "  -  "
            thtr_s = f"{r['theater']:.2f}" if r['theater'] is not None else "  -  "
            chip_s = f"{r['chi_powerless']:.4f}" if r['chi_powerless'] is not None else "   -   "
            chia_s = f"{r['chi_analytical']:.4f}" if r['chi_analytical'] is not None else "   -   "
            enf_s = "Y" if r['has_enf'] else "n"
            coord_s = "Y" if r['has_coord'] else "n"
            asym_s = "Y" if r['has_asym'] else "n"
            coal_s = "C" if r.get('coalition') else " "
            print(f"    {r['cid']:<42} {eps_s:>5} {supp_s:>5} {thtr_s:>5} {chip_s:>7} {chia_s:>7} {enf_s:>3} {coord_s:>3} {asym_s:>3} {coal_s:>3} {r['analytical_type']:>7}")

        # Gate failure details for first 3
        print()
        print(f"    Gate failure details (first 3):")
        for r in members_sorted[:3]:
            print(f"      {r['cid']}:")
            for gate_name in gate_order:
                if gate_name not in r["gate_results"]:
                    continue
                passes, failures = r["gate_results"][gate_name]
                status = "PASS" if passes else "FAIL"
                if failures:
                    print(f"        {gate_name:>14}: {status} -- {'; '.join(failures)}")
                else:
                    print(f"        {gate_name:>14}: {status}")
        print()

    # ══════════════════════════════════════════════════════════════════════
    # DEAD ZONE CHI HISTOGRAM
    # ══════════════════════════════════════════════════════════════════════

    print("=" * 80)
    print("DEAD ZONE CHI_POWERLESS HISTOGRAM")
    print("=" * 80)
    print()

    dz_members = subpops.get("dead_zone", [])
    if dz_members:
        chi_values = [r["chi_powerless"] for r in dz_members if r["chi_powerless"] is not None]
        print_histogram(chi_values, bin_width=0.05, label="chi_powerless")
        print()

        if chi_values:
            print(f"  Dead zone chi stats:")
            print(f"    n:      {len(chi_values)}")
            print(f"    min:    {min(chi_values):.4f}")
            print(f"    max:    {max(chi_values):.4f}")
            print(f"    mean:   {sum(chi_values)/len(chi_values):.4f}")
            sv = sorted(chi_values)
            mid = len(sv) // 2
            median = sv[mid] if len(sv) % 2 else (sv[mid-1] + sv[mid]) / 2
            print(f"    median: {median:.4f}")
            print()

            # Band analysis
            print(f"  Dead zone band analysis:")
            b1 = sum(1 for v in chi_values if 0.30 < v < 0.40)
            b2 = sum(1 for v in chi_values if v > 0.90)
            print(f"    0.30 < chi < 0.40 (scaffold-tangled gap): {b1}")
            print(f"    chi > 0.90 (above tangled_rope ceiling):  {b2}")
    else:
        print("  No dead zone constraints found.")

    print()

    # ══════════════════════════════════════════════════════════════════════
    # EPSILON DISTRIBUTION
    # ══════════════════════════════════════════════════════════════════════

    print("=" * 80)
    print("EPSILON DISTRIBUTION ACROSS ALL POWERLESS_BLIND")
    print("=" * 80)
    print()

    eps_values = [r["eps"] for r in records if r["eps"] is not None]
    print_histogram(eps_values, bin_width=0.10, label="epsilon")
    print()

    # ══════════════════════════════════════════════════════════════════════
    # MISSING_STRUCTURAL DETAIL: WHAT'S MISSING?
    # ══════════════════════════════════════════════════════════════════════

    ms_members = subpops.get("missing_structural", [])
    if ms_members:
        print("=" * 80)
        print("MISSING STRUCTURAL DETAIL: WHAT FAILS FOR TANGLED_ROPE?")
        print("=" * 80)
        print()

        miss_reasons = Counter()
        for r in ms_members:
            if r["eps"] is not None and r["eps"] < TANGLED_EPS_FLOOR:
                miss_reasons["eps < 0.30"] += 1
            if r["supp"] is not None and r["supp"] < TANGLED_SUPP_FLOOR:
                miss_reasons["supp < 0.40"] += 1
            if not r["has_enf"]:
                miss_reasons["missing requires_active_enforcement"] += 1
            if not r["has_coord"]:
                miss_reasons["missing has_coordination_function"] += 1
            if not r["has_asym"]:
                miss_reasons["missing has_asymmetric_extraction"] += 1

        print(f"  What blocks tangled_rope for these {len(ms_members)} constraints:")
        for reason, count in miss_reasons.most_common():
            pct = 100 * count / len(ms_members)
            print(f"    {count:>4}  ({pct:>5.1f}%)  {reason}")
        print()

    # ══════════════════════════════════════════════════════════════════════
    # DIAGNOSTIC SUMMARY
    # ══════════════════════════════════════════════════════════════════════

    print("=" * 80)
    print("DIAGNOSTIC SUMMARY")
    print("=" * 80)
    print()
    print(f"  Total powerless_blind constraints:  {len(records)}")
    print(f"  With corpus metrics:                {sum(1 for r in records if r['eps'] is not None)}")
    print(f"  With theater_ratio:                 {sum(1 for r in records if r['theater'] is not None)}")
    print()
    print(f"  Subpopulation breakdown:")
    for sp_key in sp_order:
        count = len(subpops.get(sp_key, []))
        label = subpop_labels.get(sp_key, sp_key)
        print(f"    {count:>4}  {label}")
    print()
    print(f"  REMAINING ROOT CAUSES:")
    print(f"  ----------------------")
    print(f"  The immutability wall for snare is RESOLVED (snare_immutability_check).")
    print(f"  The rope gate remains blocked (effective_immutability = mountain).")
    print()
    print(f"  Remaining powerless_blind constraints fall into these categories:")
    print(f"    - Coalition chi reduction: powerless upgraded to organized (pi=0.4),")
    print(f"      chi drops below snare threshold despite high base extraction.")
    print(f"    - Low-eps ropes: analytical=rope but eps too low for snare,")
    print(f"      and rope gate blocked by immutability.")
    print(f"    - Dead zone: chi falls between scaffold ceiling (0.30) and")
    print(f"      tangled_rope floor (0.40).")
    print(f"    - No metrics: constraint not in corpus_data.json.")
    print()


if __name__ == "__main__":
    main()
