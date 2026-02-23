"""Reform threshold report — query function.

Energy triage for snare-classified constraints. Reads fingerprint JSON +
corpus data + config to identify minimum power levels for reform.
"""

import re
from collections import defaultdict, Counter
from pathlib import Path

from sigmoid import POWER_MODIFIERS

BASE = Path(__file__).resolve().parent.parent.parent.parent
TESTSET_DIR = BASE / "prolog" / "testsets"
PROBSET_DIR = BASE / "prolog" / "probsets"

# Standard contexts and their effective_immutability results
STANDARD_CONTEXTS = [
    ("powerless",     "biographical",   "trapped",    "mountain"),
    ("moderate",      "biographical",   "mobile",     "rope"),
    ("institutional", "generational",   "arbitrage",  "rope"),
    ("analytical",    "civilizational", "analytical",  "rope"),
]

# Reform threshold action implications
THRESHOLD_IMPLICATIONS = {
    "powerless":     "Already changeable from powerless -- not power-indexed",
    "moderate":      "Individual + allies/resources -> personal coalition-building",
    "powerful":      "Significant resources or position -> strategic positioning",
    "organized":     "Collective action required -> movement-building, unionizing",
    "institutional": "Must capture/become the institution -> political/regulatory strategy",
    "analytical":    "Only visible as changeable with systemic view -> awareness-building first",
}


def compute_reform_threshold(from_power):
    """Compute minimum power level where immutability transitions to rope."""
    power_order = ["powerless", "moderate", "institutional", "analytical"]
    from_idx = power_order.index(from_power) if from_power in power_order else 0

    for ctx in STANDARD_CONTEXTS:
        power, _, _, immutability = ctx
        ctx_idx = power_order.index(power) if power in power_order else -1
        if ctx_idx >= from_idx and immutability == "rope":
            return power

    return None


def parse_shift_tuple(pattern_str):
    m = re.match(r"shift\(([^,]+),\s*([^,]+),\s*([^,]+),\s*([^)]+)\)", pattern_str)
    if not m:
        return None
    return tuple(x.strip() for x in m.groups())


def extract_victim_counts(*dirs):
    """Count victims per constraint from .pl files."""
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


def query(data: dict) -> dict:
    """Loaded data -> template context for reform threshold report."""
    fingerprint = data["fingerprint"]
    corpus_raw = data["corpus"]
    config = data["config"]

    # Config-derived thresholds
    SCOPE_MODIFIERS = {
        "local": config.get('scope_modifier_local', 0.8),
        "national": config.get('scope_modifier_national', 1.0),
        "global": config.get('scope_modifier_global', 1.2),
    }
    COALITION_VICTIM_THRESHOLD = int(config.get('critical_mass_threshold', 3))
    COALITION_EPS_FLOOR = config.get('snare_epsilon_floor', 0.46)
    COALITION_SUPP_FLOOR = config.get('snare_suppression_floor', 0.60)

    # -- Build families from fingerprint JSON --
    families = {}
    for fam in fingerprint["shift_families"]:
        families[fam["pattern"]] = fam["members"]

    total = sum(len(v) for v in families.values())

    # -- Find snare-from-powerless --
    snare_list = []
    for pattern_str, members in families.items():
        tup = parse_shift_tuple(pattern_str)
        if tup is None:
            continue
        if tup[0] == "snare":
            for cid in members:
                snare_list.append((cid, tup, pattern_str))

    # [2b] — if no snare-from-powerless, find analytical=snare
    anal_snare = []
    anal_snare_ptype_counts = []
    if not snare_list:
        for pattern_str, members in families.items():
            tup = parse_shift_tuple(pattern_str)
            if tup and tup[3] == "snare":
                for cid in members:
                    anal_snare.append((cid, tup, pattern_str))
        ptype_counts = Counter(tup[0] for _, tup, _ in anal_snare)
        anal_snare_ptype_counts = ptype_counts.most_common()

    # -- Load corpus data --
    corpus = {}
    for cid, info in corpus_raw.get("constraints", {}).items():
        metrics = info.get("metrics", {})
        corpus[cid] = {
            "extractiveness": metrics.get("extractiveness"),
            "suppression": metrics.get("suppression"),
            "domain": info.get("domain"),
            "claimed_type": info.get("claimed_type"),
        }

    victim_counts = extract_victim_counts(TESTSET_DIR, PROBSET_DIR)

    # -- Compute reform threshold --
    threshold = compute_reform_threshold("powerless")

    # -- Build records --
    records = []
    for cid, tup, pattern in snare_list:
        c_data = corpus.get(cid, {})
        eps = c_data.get("extractiveness")
        supp = c_data.get("suppression")
        domain = c_data.get("domain", "?")
        n_victims = victim_counts.get(cid, 0)

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

    # -- Group by threshold --
    by_threshold = defaultdict(list)
    for r in records:
        by_threshold[r["reform_threshold"]].append(r)

    # Precompute formatted threshold sections
    threshold_sections = []
    thresh_order = list(THRESHOLD_IMPLICATIONS.keys())
    for thresh, members in sorted(by_threshold.items(),
                                   key=lambda x: thresh_order.index(x[0])
                                   if x[0] in thresh_order else 99):
        members_sorted = sorted(members, key=lambda r: -(r["chi_powerless"] or 0))
        domain_counts = Counter(r["domain"] for r in members)
        domain_dist = ", ".join(f"{d}={c}" for d, c in domain_counts.most_common(8))

        hdr = f"    {'ID':<42} {'eps':>5} {'supp':>5} {'chi_p':>7} {'coa':>3} {'vict':>4} {'domain':>12} {'shift'}"
        rule = f"    {'~'*42} {'~'*5} {'~'*5} {'~'*7} {'~'*3} {'~'*4} {'~'*12} {'~'*30}"

        rows = []
        for r in members_sorted:
            eps_s = f"{r['eps']:.2f}" if r['eps'] is not None else "  -  "
            supp_s = f"{r['supp']:.2f}" if r['supp'] is not None else "  -  "
            chip_s = f"{r['chi_powerless']:.4f}" if r['chi_powerless'] is not None else "   -   "
            coal_s = "C" if r['coalition'] else " "
            domain_s = r['domain'] or "?"
            shift_s = f"P={r['shift'][0]} M={r['moderate_type']} I={r['institutional_type']} A={r['analytical_type']}"
            rows.append(f"    {r['cid']:<42} {eps_s:>5} {supp_s:>5} {chip_s:>7} {coal_s:>3} {r['n_victims']:>4} {domain_s:>12} {shift_s}")

        threshold_sections.append({
            "thresh": thresh,
            "action": THRESHOLD_IMPLICATIONS.get(thresh, "Unknown"),
            "count": len(members),
            "domain_dist": domain_dist,
            "hdr": hdr,
            "rule": rule,
            "rows": rows,
        })

    # -- Cross-perspective analysis --
    cross_perspective = []
    if records:
        for perspective, idx in [("moderate", 1), ("institutional", 2), ("analytical", 3)]:
            type_counts = Counter(r["shift"][idx] for r in records)
            dist = ", ".join(f"{t}={c}" for t, c in type_counts.most_common())
            cross_perspective.append({
                "name": perspective.capitalize(),
                "dist": dist,
            })

    pattern_counts = []
    if records:
        pc = Counter(
            f"P=snare -> M={r['moderate_type']}, I={r['institutional_type']}, A={r['analytical_type']}"
            for r in records
        )
        pattern_counts = pc.most_common()

    # -- Summary data --
    summary_coalition = sum(1 for r in records if r['coalition'])
    summary_domains = Counter(r["domain"] for r in records).most_common(5)

    return {
        "families_count": len(families),
        "total_constraints": total,
        "snare_count": len(snare_list),
        "has_snare": bool(snare_list),
        "anal_snare_count": len(anal_snare),
        "anal_snare_ptype_counts": anal_snare_ptype_counts,
        "corpus_count": len(corpus),
        "threshold": threshold,
        "threshold_meaning": THRESHOLD_IMPLICATIONS.get(threshold, "Unknown"),
        "threshold_sections": threshold_sections,
        "has_records": bool(records),
        "records_count": len(records),
        "cross_perspective": cross_perspective,
        "pattern_counts": pattern_counts,
        "summary_coalition": summary_coalition,
        "summary_domains": summary_domains,
        "THRESHOLD_IMPLICATIONS": THRESHOLD_IMPLICATIONS,
    }
