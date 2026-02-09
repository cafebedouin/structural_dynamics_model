#!/usr/bin/env python3
"""
Reform Threshold Report — Energy Triage for Snare-Classified Constraints
=========================================================================

For each constraint classified as snare from the powerless perspective,
identifies the minimum power level (reform threshold) at which the
constraint transitions from immutable to changeable.

This is the "energy triage" view: which snares are most accessible
to reform, and what kind of power is needed for each?

Reform threshold logic:
  The effective_immutability table in constraint_indexing.pl maps
  (time_horizon, exit_options) -> mountain|rope. The standard contexts
  define which (T, E) pairs correspond to each power level:

    powerless:     biographical/trapped     -> mountain
    moderate:      biographical/mobile      -> rope
    institutional: generational/arbitrage   -> rope
    analytical:    civilizational/analytical -> mountain AND rope

  The reform threshold is the FIRST standard power level (above the
  current) where immutability = rope (changeable).

  From powerless, this is always 'moderate' because biographical/mobile
  gives rope. Future standard contexts (organized, powerful) would
  create differentiation.

Data sources:
  - outputs/fingerprint_report.md  -> shift patterns (powerless type)
  - outputs/corpus_data.json       -> metrics, domain
  - prolog/testsets/*.pl           -> structural predicates, victim counts

Usage: python3 python/reform_threshold_report.py
"""

import json
import re
from collections import defaultdict, Counter
from pathlib import Path

# ── Paths ────────────────────────────────────────────────────────────────────

BASE = Path("/home/scott/bin/structural_dynamics_model")
FINGERPRINT = BASE / "outputs" / "fingerprint_report.md"
CORPUS_DATA = BASE / "outputs" / "corpus_data.json"
TESTSET_DIR = BASE / "prolog" / "testsets"
PROBSET_DIR = BASE / "prolog" / "probsets"

# ── Constants ────────────────────────────────────────────────────────────────

# Standard contexts and their effective_immutability results
# Each entry: (power_level, time_horizon, exit_options, immutability)
STANDARD_CONTEXTS = [
    ("powerless",     "biographical",   "trapped",    "mountain"),
    ("moderate",      "biographical",   "mobile",     "rope"),
    ("institutional", "generational",   "arbitrage",  "rope"),
    ("analytical",    "civilizational", "analytical",  "rope"),  # also mountain, but rope exists
]

# Power modifiers for chi calculation (v5.0: sigmoid-derived)
from sigmoid import POWER_MODIFIERS

# Scope modifiers
SCOPE_MODIFIERS = {
    "local": 0.8,
    "national": 1.0,
    "global": 1.2,
}

# Coalition modeling thresholds
COALITION_VICTIM_THRESHOLD = 3
COALITION_EPS_FLOOR = 0.46
COALITION_SUPP_FLOOR = 0.60

# Reform threshold action implications
THRESHOLD_IMPLICATIONS = {
    "powerless":     "Already changeable from powerless — not power-indexed",
    "moderate":      "Individual + allies/resources → personal coalition-building",
    "powerful":      "Significant resources or position → strategic positioning",
    "organized":     "Collective action required → movement-building, unionizing",
    "institutional": "Must capture/become the institution → political/regulatory strategy",
    "analytical":    "Only visible as changeable with systemic view → awareness-building first",
}


def compute_reform_threshold(from_power):
    """Compute minimum power level where immutability transitions to rope.

    Walks standard contexts in ascending power order, returns the first
    where immutability = rope.
    """
    power_order = ["powerless", "moderate", "institutional", "analytical"]
    from_idx = power_order.index(from_power) if from_power in power_order else 0

    for ctx in STANDARD_CONTEXTS:
        power, _, _, immutability = ctx
        ctx_idx = power_order.index(power) if power in power_order else -1
        if ctx_idx >= from_idx and immutability == "rope":
            return power

    return None  # Genuinely immutable at all levels (shouldn't happen for snares)


# ── Fingerprint parsing ──────────────────────────────────────────────────────

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


def find_snare_from_powerless(families):
    """Find constraints classified as snare from powerless perspective."""
    results = []
    for pattern_str, members in families.items():
        tup = parse_shift_tuple(pattern_str)
        if tup is None:
            continue
        powerless_type = tup[0]
        if powerless_type == "snare":
            for cid in members:
                results.append((cid, tup, pattern_str))
    return results


# ── Corpus + metadata ────────────────────────────────────────────────────────

def load_corpus_data(path):
    with open(path, "r") as f:
        data = json.load(f)
    corpus = {}
    for cid, info in data.get("constraints", {}).items():
        metrics = info.get("metrics", {})
        corpus[cid] = {
            "extractiveness": metrics.get("extractiveness"),
            "suppression": metrics.get("suppression"),
            "domain": info.get("domain"),
            "claimed_type": info.get("claimed_type"),
        }
    return corpus


def extract_victim_counts(*dirs):
    """Count victims per constraint from .pl files (for coalition modeling info)."""
    victim_counts = defaultdict(int)
    re_victim = re.compile(r"(?:narrative_ontology:)?constraint_victim\(\s*([a-zA-Z0-9_]+)\s*,")
    for d in dirs:
        if not d.exists():
            continue
        for plfile in d.glob("*.pl"):
            try:
                text = plfile.read_text(errors="replace")
            except Exception:
                continue
            for m in re_victim.finditer(text):
                victim_counts[m.group(1)] += 1
    return victim_counts


# ── Main ─────────────────────────────────────────────────────────────────────

def main():
    print("=" * 80)
    print("REFORM THRESHOLD REPORT")
    print("Structural Dynamics Model — Energy Triage for Snare-Classified Constraints")
    print("=" * 80)
    print()

    # ── Parse fingerprint report ─────────────────────────────────────────
    print("[1] Parsing fingerprint report...")
    families = parse_fingerprint_report(FINGERPRINT)
    total = sum(len(v) for v in families.values())
    print(f"    {len(families)} shift families, {total} total constraints")
    print()

    # ── Find snare-from-powerless ────────────────────────────────────────
    print("[2] Identifying snare-from-powerless constraints...")
    snare_list = find_snare_from_powerless(families)
    print(f"    Found {len(snare_list)} constraints classified as snare from powerless")
    print()

    if not snare_list:
        print("  No snare-from-powerless constraints found.")
        print("  This means the snare gate is not firing from powerless for any constraint.")
        print("  Check if coalition modeling is reducing chi below snare threshold for all.")
        print()

        # Also show constraints where analytical=snare but powerless=something else
        print("[2b] Checking analytical=snare constraints for comparison...")
        anal_snare = []
        for pattern_str, members in families.items():
            tup = parse_shift_tuple(pattern_str)
            if tup and tup[3] == "snare":
                for cid in members:
                    anal_snare.append((cid, tup, pattern_str))
        print(f"     Found {len(anal_snare)} constraints classified as snare from analytical")
        if anal_snare:
            print()
            print("     Powerless-type distribution for analytical=snare:")
            ptype_counts = Counter(tup[0] for _, tup, _ in anal_snare)
            for pt, count in ptype_counts.most_common():
                print(f"       {pt}: {count}")
        print()

    # ── Load corpus data ─────────────────────────────────────────────────
    print("[3] Loading corpus data...")
    corpus = load_corpus_data(CORPUS_DATA)
    victim_counts = extract_victim_counts(TESTSET_DIR, PROBSET_DIR)
    print(f"    {len(corpus)} constraints in corpus_data.json")
    print()

    # ── Compute reform thresholds ────────────────────────────────────────
    print("[4] Computing reform thresholds...")
    print()

    threshold = compute_reform_threshold("powerless")
    print(f"  Reform threshold from powerless: {threshold}")
    print(f"  Meaning: {THRESHOLD_IMPLICATIONS.get(threshold, 'Unknown')}")
    print()

    # ── Build records ────────────────────────────────────────────────────
    records = []
    for cid, tup, pattern in snare_list:
        c_data = corpus.get(cid, {})
        eps = c_data.get("extractiveness")
        supp = c_data.get("suppression")
        domain = c_data.get("domain", "?")
        n_victims = victim_counts.get(cid, 0)

        # Compute chi at powerless (with coalition modeling)
        chi_p = None
        coalition = False
        if eps is not None:
            coalition = (
                eps >= COALITION_EPS_FLOOR
                and supp is not None and supp >= COALITION_SUPP_FLOOR
                and n_victims >= COALITION_VICTIM_THRESHOLD
            )
            pi = POWER_MODIFIERS["organized"] if coalition else POWER_MODIFIERS["powerless"]
            chi_p = eps * pi * SCOPE_MODIFIERS["local"]

        records.append({
            "cid": cid,
            "shift": tup,
            "eps": eps,
            "supp": supp,
            "domain": domain,
            "chi_powerless": chi_p,
            "coalition": coalition,
            "n_victims": n_victims,
            "reform_threshold": threshold,
            "moderate_type": tup[1],
            "institutional_type": tup[2],
            "analytical_type": tup[3],
        })

    # ── Report: grouped by reform threshold ──────────────────────────────
    print("=" * 80)
    print("SNARE-FROM-POWERLESS CONSTRAINTS BY REFORM THRESHOLD")
    print("=" * 80)
    print()

    by_threshold = defaultdict(list)
    for r in records:
        by_threshold[r["reform_threshold"]].append(r)

    for thresh, members in sorted(by_threshold.items(),
                                   key=lambda x: list(THRESHOLD_IMPLICATIONS.keys()).index(x[0])
                                   if x[0] in THRESHOLD_IMPLICATIONS else 99):
        print(f"  Reform Threshold: {thresh.upper()}")
        print(f"  Action: {THRESHOLD_IMPLICATIONS.get(thresh, 'Unknown')}")
        print(f"  Count: {len(members)}")
        print()

        # Domain distribution
        domain_counts = Counter(r["domain"] for r in members)
        print(f"    Domain distribution: ", end="")
        print(", ".join(f"{d}={c}" for d, c in domain_counts.most_common(8)))
        print()

        # Table
        members_sorted = sorted(members, key=lambda r: -(r["chi_powerless"] or 0))
        hdr = f"    {'ID':<42} {'eps':>5} {'supp':>5} {'chi_p':>7} {'coa':>3} {'vict':>4} {'domain':>12} {'shift'}"
        print(hdr)
        print(f"    {'~'*42} {'~'*5} {'~'*5} {'~'*7} {'~'*3} {'~'*4} {'~'*12} {'~'*30}")

        for r in members_sorted:
            eps_s = f"{r['eps']:.2f}" if r['eps'] is not None else "  -  "
            supp_s = f"{r['supp']:.2f}" if r['supp'] is not None else "  -  "
            chip_s = f"{r['chi_powerless']:.4f}" if r['chi_powerless'] is not None else "   -   "
            coal_s = "C" if r['coalition'] else " "
            domain_s = r['domain'] or "?"
            shift_s = f"P={r['shift'][0]} M={r['moderate_type']} I={r['institutional_type']} A={r['analytical_type']}"
            print(f"    {r['cid']:<42} {eps_s:>5} {supp_s:>5} {chip_s:>7} {coal_s:>3} {r['n_victims']:>4} {domain_s:>12} {shift_s}")

        print()

    # ── Cross-perspective view ───────────────────────────────────────────
    if records:
        print("=" * 80)
        print("CROSS-PERSPECTIVE ANALYSIS")
        print("=" * 80)
        print()
        print("  How do these snare-from-powerless constraints appear at other power levels?")
        print()

        # Count what moderate/institutional/analytical see
        for perspective, idx in [("moderate", 1), ("institutional", 2), ("analytical", 3)]:
            type_counts = Counter(r["shift"][idx] for r in records)
            print(f"  {perspective.capitalize():>15}: ", end="")
            print(", ".join(f"{t}={c}" for t, c in type_counts.most_common()))

        print()
        print("  Perspectival divergence patterns:")
        pattern_counts = Counter(
            f"P=snare -> M={r['moderate_type']}, I={r['institutional_type']}, A={r['analytical_type']}"
            for r in records
        )
        for pat, count in pattern_counts.most_common():
            print(f"    {count:>3}  {pat}")

    # ── Summary ──────────────────────────────────────────────────────────
    print()
    print("=" * 80)
    print("SUMMARY")
    print("=" * 80)
    print()
    print(f"  Total snare-from-powerless: {len(records)}")
    if records:
        print(f"  Reform threshold:          {threshold} (all constraints)")
        print(f"  With coalition upgrade:    {sum(1 for r in records if r['coalition'])}")
        print()
        domains = Counter(r["domain"] for r in records)
        print(f"  Top domains:")
        for d, c in domains.most_common(5):
            print(f"    {d}: {c}")
    else:
        print(f"  NOTE: No constraints classified as snare from powerless.")
        print(f"  The snare gate's metric requirements (chi >= 0.66, eps >= 0.46,")
        print(f"  supp >= 0.60) combined with coalition modeling (which reduces")
        print(f"  chi for high-extraction constraints) may be filtering all candidates.")
    print()


if __name__ == "__main__":
    main()
