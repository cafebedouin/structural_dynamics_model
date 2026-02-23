"""Powerless blind diagnostic — query function.

Identifies constraints where the engine returns 'unknown' at powerless
perspective but returns a non-unknown type at analytical perspective.
Reads fingerprint JSON + corpus data + config.
"""

import re
from collections import defaultdict, Counter
from pathlib import Path

from sigmoid import POWER_MODIFIERS as _PM

BASE = Path(__file__).resolve().parent.parent.parent.parent
TESTSET_DIR = BASE / "prolog" / "testsets"
PROBSET_DIR = BASE / "prolog" / "probsets"


def parse_shift_tuple(pattern_str):
    m = re.match(r"shift\(([^,]+),\s*([^,]+),\s*([^,]+),\s*([^)]+)\)", pattern_str)
    if not m:
        return None
    return tuple(x.strip() for x in m.groups())


def extract_pl_metadata(*dirs):
    """Scan .pl files for theater_ratio, structural predicates, and victim counts."""
    theater = {}
    has_enforcement = set()
    has_coordination = set()
    has_asymmetric = set()
    has_sunset = set()
    victim_counts = defaultdict(int)

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


def compute_chi(epsilon, pi_power, sigma_scope):
    if epsilon is None:
        return None
    return epsilon * pi_power * sigma_scope


def compute_chi_powerless(epsilon, supp, victim_count, cfg):
    """Compute chi at powerless/local context, accounting for coalition modeling."""
    if epsilon is None:
        return None, False
    coalition = (
        epsilon >= cfg["COALITION_EPS_FLOOR"]
        and supp is not None and supp >= cfg["COALITION_SUPP_FLOOR"]
        and victim_count >= cfg["COALITION_VICTIM_THRESHOLD"]
    )
    pi = cfg["PI_ORGANIZED"] if coalition else cfg["PI_POWERLESS"]
    return epsilon * pi * cfg["SIGMA_LOCAL"], coalition


def test_powerless_gates(chi, eps, supp, theater, has_coord, has_asym, has_enf, has_sun, coalition, cfg):
    """Test all six gates at powerless context."""
    results = {}

    # Mountain
    mtn_fails = []
    if eps is not None and eps > cfg["MOUNTAIN_EPS_MAX"]:
        mtn_fails.append(f"eps={eps:.2f} > {cfg['MOUNTAIN_EPS_MAX']}")
    if supp is not None and supp > cfg["MOUNTAIN_SUPP_MAX"]:
        mtn_fails.append(f"supp={supp:.2f} > {cfg['MOUNTAIN_SUPP_MAX']}")
    if eps is None:
        mtn_fails.append("eps=None")
    results["mountain"] = (len(mtn_fails) == 0, mtn_fails)

    # Snare
    snare_fails = []
    if chi is not None and chi < cfg["SNARE_CHI_FLOOR"]:
        reason = f"chi={chi:.4f} < {cfg['SNARE_CHI_FLOOR']}"
        if coalition:
            reason += " (coalition: pi=0.4)"
        snare_fails.append(reason)
    if eps is not None and eps < cfg["SNARE_EPS_FLOOR"]:
        snare_fails.append(f"eps={eps:.2f} < {cfg['SNARE_EPS_FLOOR']}")
    if supp is not None and supp < cfg["SNARE_SUPP_FLOOR"]:
        snare_fails.append(f"supp={supp:.2f} < {cfg['SNARE_SUPP_FLOOR']}")
    if chi is None:
        snare_fails.append("chi=None")
    if eps is None:
        snare_fails.append("eps=None")
    results["snare"] = (len(snare_fails) == 0, snare_fails)

    # Rope: STILL STRUCTURALLY BLOCKED
    results["rope"] = (False, ["BLOCKED: immutability=mountain (need rope)"])

    # Scaffold
    scf_fails = []
    if chi is not None and chi > cfg["SCAFFOLD_CHI_CEIL"]:
        scf_fails.append(f"chi={chi:.4f} > {cfg['SCAFFOLD_CHI_CEIL']}")
    if not has_coord:
        scf_fails.append("missing has_coordination_function")
    if theater is not None and theater > cfg["SCAFFOLD_THEATER_MAX"]:
        scf_fails.append(f"theater={theater:.2f} > {cfg['SCAFFOLD_THEATER_MAX']}")
    if not has_sun and has_enf:
        scf_fails.append("no sunset_clause AND requires_enforcement")
    results["scaffold"] = (len(scf_fails) == 0, scf_fails)

    # Tangled Rope
    tr_fails = []
    if chi is not None and chi < cfg["TANGLED_CHI_FLOOR"]:
        tr_fails.append(f"chi={chi:.4f} < {cfg['TANGLED_CHI_FLOOR']}")
    if chi is not None and chi > cfg["TANGLED_CHI_CEIL"]:
        tr_fails.append(f"chi={chi:.4f} > {cfg['TANGLED_CHI_CEIL']}")
    if eps is not None and eps < cfg["TANGLED_EPS_FLOOR"]:
        tr_fails.append(f"eps={eps:.2f} < {cfg['TANGLED_EPS_FLOOR']}")
    if supp is not None and supp < cfg["TANGLED_SUPP_FLOOR"]:
        tr_fails.append(f"supp={supp:.2f} < {cfg['TANGLED_SUPP_FLOOR']}")
    if not has_enf:
        tr_fails.append("missing requires_active_enforcement")
    if not has_coord:
        tr_fails.append("missing has_coordination_function")
    if not has_asym:
        tr_fails.append("missing has_asymmetric_extraction")
    if chi is None:
        tr_fails.append("chi=None")
    results["tangled_rope"] = (len(tr_fails) == 0, tr_fails)

    # Piton
    pit_fails = []
    if chi is not None and chi > cfg["PITON_CHI_CEIL"]:
        pit_fails.append(f"chi={chi:.4f} > {cfg['PITON_CHI_CEIL']}")
    if eps is not None and eps <= cfg["PITON_EPS_FLOOR"]:
        pit_fails.append(f"eps={eps:.2f} <= {cfg['PITON_EPS_FLOOR']}")
    if theater is None:
        pit_fails.append("theater=None (metric not found)")
    elif theater < cfg["PITON_THEATER_FLOOR"]:
        pit_fails.append(f"theater={theater:.2f} < {cfg['PITON_THEATER_FLOOR']}")
    if chi is None:
        pit_fails.append("chi=None")
    results["piton"] = (len(pit_fails) == 0, pit_fails)

    return results


def classify_subpopulation(rec, cfg):
    """Classify a powerless_blind constraint into a subpopulation."""
    eps = rec["eps"]
    supp = rec["supp"]
    chi_p = rec["chi_powerless"]
    theater = rec["theater"]
    analytical_type = rec["analytical_type"]
    coalition = rec.get("coalition", False)

    if eps is None:
        return "no_metrics"

    if coalition and chi_p is not None and chi_p < cfg["SNARE_CHI_FLOOR"]:
        return "coalition_chi_reduction"

    if chi_p is not None:
        in_dead_zone_gap = chi_p > 0.30 and chi_p < 0.40
        above_tangled_ceil = chi_p > 0.90
        mountain_metrics_fail = (eps > cfg["MOUNTAIN_EPS_MAX"]) or (supp is not None and supp > cfg["MOUNTAIN_SUPP_MAX"])
        if mountain_metrics_fail and (in_dead_zone_gap or above_tangled_ceil):
            return "dead_zone"

    if chi_p is not None and 0.40 <= chi_p <= 0.90:
        eps_ok = eps >= cfg["TANGLED_EPS_FLOOR"]
        supp_ok = supp is not None and supp >= cfg["TANGLED_SUPP_FLOOR"]
        has_struct = rec["has_enf"] and rec["has_coord"] and rec["has_asym"]
        if not (eps_ok and supp_ok and has_struct):
            return "missing_structural"

    if (eps <= 0.45
            and analytical_type in ("rope", "mountain")
            and chi_p is not None and chi_p <= 0.30):
        return "wings_candidate"

    if chi_p is not None and chi_p > 0.90:
        return "high_chi_gap"

    return "other"


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


def format_histogram(values, bin_width=0.05):
    """Return pre-formatted histogram lines."""
    hist = make_histogram(values, bin_width)
    if not hist:
        return ["  (empty)"]
    max_count = max(c for _, c in hist) if hist else 1
    bar_scale = 50.0 / max_count if max_count > 0 else 1
    lines = []
    lines.append(f"  {'Bin':<18} {'Count':>5}  Bar")
    lines.append(f"  {'~'*18} {'~'*5}  {'~'*52}")
    for lbl, count in hist:
        bar = "#" * int(count * bar_scale)
        lines.append(f"  {lbl:<18} {count:>5}  {bar}")
    return lines


def query(data: dict) -> dict:
    """Loaded data -> template context for powerless blind diagnostic."""
    fingerprint = data["fingerprint"]
    corpus_raw = data["corpus"]
    config = data["config"]

    # Config-derived thresholds
    PI_POWERLESS = _PM["powerless"]
    PI_ORGANIZED = _PM["organized"]
    PI_ANALYTICAL = _PM["analytical"]
    SIGMA_LOCAL = config.get('scope_modifier_local', 0.8)
    SIGMA_GLOBAL = config.get('scope_modifier_global', 1.2)

    cfg = {
        "PI_POWERLESS": PI_POWERLESS,
        "PI_ORGANIZED": PI_ORGANIZED,
        "SIGMA_LOCAL": SIGMA_LOCAL,
        "SIGMA_GLOBAL": SIGMA_GLOBAL,
        "COALITION_VICTIM_THRESHOLD": int(config.get('critical_mass_threshold', 3)),
        "COALITION_EPS_FLOOR": config.get('snare_epsilon_floor', 0.46),
        "COALITION_SUPP_FLOOR": config.get('snare_suppression_floor', 0.60),
        "MOUNTAIN_EPS_MAX": config.get('mountain_extractiveness_max', 0.25),
        "MOUNTAIN_SUPP_MAX": config.get('mountain_suppression_ceiling', 0.05),
        "SNARE_CHI_FLOOR": config.get('snare_chi_floor', 0.66),
        "SNARE_EPS_FLOOR": config.get('snare_epsilon_floor', 0.46),
        "SNARE_SUPP_FLOOR": config.get('snare_suppression_floor', 0.60),
        "SCAFFOLD_CHI_CEIL": config.get('scaffold_extraction_ceil', 0.30),
        "SCAFFOLD_THEATER_MAX": config.get('piton_theater_floor', 0.70),
        "ROPE_CHI_CEIL": config.get('rope_chi_ceiling', 0.35),
        "ROPE_EPS_CEIL": config.get('rope_epsilon_ceiling', 0.45),
        "TANGLED_CHI_FLOOR": config.get('tangled_rope_chi_floor', 0.40),
        "TANGLED_CHI_CEIL": config.get('tangled_rope_chi_ceil', 0.90),
        "TANGLED_EPS_FLOOR": config.get('tangled_rope_epsilon_floor', 0.30),
        "TANGLED_SUPP_FLOOR": config.get('tangled_rope_suppression_floor', 0.40),
        "PITON_CHI_CEIL": config.get('piton_extraction_ceiling', 0.25),
        "PITON_EPS_FLOOR": config.get('piton_epsilon_floor', 0.10),
        "PITON_THEATER_FLOOR": config.get('piton_theater_floor', 0.70),
    }

    # -- Build families from fingerprint JSON --
    families = {}
    for fam in fingerprint["shift_families"]:
        families[fam["pattern"]] = fam["members"]

    total_constraints = sum(len(v) for v in families.values())

    # -- Find powerless_blind --
    pb_list = []
    for pattern_str, members in families.items():
        tup = parse_shift_tuple(pattern_str)
        if tup is None:
            continue
        powerless_type, _, _, analytical_type = tup
        if powerless_type == "unknown" and analytical_type != "unknown":
            for cid in members:
                pb_list.append((cid, tup, pattern_str))

    analytical_counts = Counter(tup[3] for _, tup, _ in pb_list).most_common()
    pattern_counts = Counter(pat for _, _, pat in pb_list).most_common()

    # -- Load corpus data --
    corpus = {}
    for cid, info in corpus_raw.get("constraints", {}).items():
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

    # -- Extract .pl metadata --
    theater_map, enforcement_set, coordination_set, asymmetric_set, sunset_set, victim_count_map = \
        extract_pl_metadata(TESTSET_DIR, PROBSET_DIR)

    pl_stats = {
        "theater_count": len(theater_map),
        "enforcement_count": len(enforcement_set),
        "coordination_count": len(coordination_set),
        "asymmetric_count": len(asymmetric_set),
        "sunset_count": len(sunset_set),
        "coalition_eligible": sum(1 for c in victim_count_map.values() if c >= cfg["COALITION_VICTIM_THRESHOLD"]),
        "coalition_threshold": cfg["COALITION_VICTIM_THRESHOLD"],
    }

    # -- Compute metrics and classify --
    records = []
    missing_corpus = 0
    gate_order = ["mountain", "snare", "rope", "scaffold", "tangled_rope", "piton"]

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
        chi_p, coalition = compute_chi_powerless(eps, supp, n_victims, cfg)
        chi_a = compute_chi(eps, PI_ANALYTICAL, SIGMA_GLOBAL)

        gate_results = test_powerless_gates(
            chi_p, eps, supp, theater, has_coord, has_asym, has_enf, has_sun, coalition, cfg
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
        rec["subpopulation"] = classify_subpopulation(rec, cfg)
        records.append(rec)

    # -- Root cause stats --
    with_data = [r for r in records if r["eps"] is not None]
    snare_pass = sum(1 for r in with_data if r["gate_results"]["snare"][0])
    coalition_count = sum(1 for r in with_data if r["coalition"])
    rope_metric_pass = sum(1 for r in with_data
        if r["chi_powerless"] is not None and r["chi_powerless"] <= cfg["ROPE_CHI_CEIL"]
        and r["eps"] <= cfg["ROPE_EPS_CEIL"])

    # -- Gate failure analysis --
    gate_fail_reasons = defaultdict(lambda: Counter())
    gate_pass_counts = Counter()
    for r in records:
        for gate_name, (passes, failures) in r["gate_results"].items():
            if passes:
                gate_pass_counts[gate_name] += 1
            for f in failures:
                gate_fail_reasons[gate_name][f] += 1

    gate_analysis = []
    for gate_name in gate_order:
        n_pass = gate_pass_counts.get(gate_name, 0)
        blocked = "(STILL BLOCKED: immutability)" if gate_name == "rope" else ""
        reasons = gate_fail_reasons[gate_name].most_common(5)
        gate_analysis.append({
            "name": gate_name,
            "n_pass": n_pass,
            "blocked": blocked,
            "reasons": reasons,
        })

    # -- Subpopulation analysis --
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
    sp_order = ["coalition_chi_reduction", "dead_zone", "missing_structural",
                "wings_candidate", "high_chi_gap", "no_metrics", "other"]

    # Summary counts
    sp_summary = []
    for sp_key in sp_order:
        count = len(subpops.get(sp_key, []))
        label = subpop_labels.get(sp_key, sp_key)
        pct = (count / len(records) * 100) if len(records) > 0 else 0
        sp_summary.append({"key": sp_key, "count": count, "pct": pct, "label": label})

    # -- Top examples by subpopulation (precomputed) --
    sp_sections = []
    for sp_key in sp_order:
        members = subpops.get(sp_key, [])
        if not members:
            continue

        label = subpop_labels.get(sp_key, sp_key)
        members_sorted = sorted(members, key=lambda r: (r["chi_powerless"] is None, -(r["chi_powerless"] or 0)))

        domain_counts = Counter(r["domain"] for r in members)
        domain_dist = ", ".join(f"{d}={c}" for d, c in domain_counts.most_common(6))
        at_counts = Counter(r["analytical_type"] for r in members)
        at_dist = ", ".join(f"{a}={c}" for a, c in at_counts.most_common())

        n_show = min(8, len(members_sorted))
        hdr = f"    {'ID':<42} {'eps':>5} {'supp':>5} {'thtr':>5} {'chi_p':>7} {'chi_a':>7} {'enf':>3} {'crd':>3} {'asy':>3} {'coa':>3} {'anal':>7}"
        rule = f"    {'~'*42} {'~'*5} {'~'*5} {'~'*5} {'~'*7} {'~'*7} {'~'*3} {'~'*3} {'~'*3} {'~'*3} {'~'*7}"

        rows = []
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
            rows.append(f"    {r['cid']:<42} {eps_s:>5} {supp_s:>5} {thtr_s:>5} {chip_s:>7} {chia_s:>7} {enf_s:>3} {coord_s:>3} {asym_s:>3} {coal_s:>3} {r['analytical_type']:>7}")

        # Gate failure details for first 3
        gate_details = []
        for r in members_sorted[:3]:
            constraint_gates = []
            for gn in gate_order:
                if gn not in r["gate_results"]:
                    continue
                passes, failures = r["gate_results"][gn]
                status = "PASS" if passes else "FAIL"
                if failures:
                    constraint_gates.append(f"        {gn:>14}: {status} -- {'; '.join(failures)}")
                else:
                    constraint_gates.append(f"        {gn:>14}: {status}")
            gate_details.append({"cid": r["cid"], "gates": constraint_gates})

        sp_sections.append({
            "label": label,
            "count": len(members),
            "domain_dist": domain_dist,
            "at_dist": at_dist,
            "hdr": hdr,
            "rule": rule,
            "rows": rows,
            "gate_details": gate_details,
        })

    # -- Dead zone chi histogram --
    dz_members = subpops.get("dead_zone", [])
    dz_chi_values = [r["chi_powerless"] for r in dz_members if r["chi_powerless"] is not None]
    dz_histogram_lines = format_histogram(dz_chi_values, bin_width=0.05) if dz_chi_values else []

    dz_stats = {}
    if dz_chi_values:
        sv = sorted(dz_chi_values)
        mid = len(sv) // 2
        median = sv[mid] if len(sv) % 2 else (sv[mid-1] + sv[mid]) / 2
        dz_stats = {
            "n": len(dz_chi_values),
            "min": f"{min(dz_chi_values):.4f}",
            "max": f"{max(dz_chi_values):.4f}",
            "mean": f"{sum(dz_chi_values)/len(dz_chi_values):.4f}",
            "median": f"{median:.4f}",
            "band_scaffold_tangled": sum(1 for v in dz_chi_values if 0.30 < v < 0.40),
            "band_above_tangled": sum(1 for v in dz_chi_values if v > 0.90),
        }

    # -- Epsilon histogram --
    eps_values = [r["eps"] for r in records if r["eps"] is not None]
    eps_histogram_lines = format_histogram(eps_values, bin_width=0.10) if eps_values else []

    # -- Missing structural detail --
    ms_members = subpops.get("missing_structural", [])
    ms_detail = []
    if ms_members:
        miss_reasons = Counter()
        for r in ms_members:
            if r["eps"] is not None and r["eps"] < cfg["TANGLED_EPS_FLOOR"]:
                miss_reasons["eps < 0.30"] += 1
            if r["supp"] is not None and r["supp"] < cfg["TANGLED_SUPP_FLOOR"]:
                miss_reasons["supp < 0.40"] += 1
            if not r["has_enf"]:
                miss_reasons["missing requires_active_enforcement"] += 1
            if not r["has_coord"]:
                miss_reasons["missing has_coordination_function"] += 1
            if not r["has_asym"]:
                miss_reasons["missing has_asymmetric_extraction"] += 1
        for reason, count in miss_reasons.most_common():
            pct = 100 * count / len(ms_members)
            ms_detail.append({"count": count, "pct": pct, "reason": reason})

    # -- Diagnostic summary --
    with_corpus = sum(1 for r in records if r['eps'] is not None)
    with_theater = sum(1 for r in records if r['theater'] is not None)

    return {
        "families_count": len(families),
        "total_constraints": total_constraints,
        "pb_count": len(pb_list),
        "analytical_counts": analytical_counts,
        "pattern_counts": pattern_counts,
        "corpus_count": len(corpus),
        "pl_stats": pl_stats,
        "missing_corpus": missing_corpus,
        "records_count": len(records),
        "with_data_count": len(with_data),
        "snare_pass": snare_pass,
        "coalition_count": coalition_count,
        "rope_metric_pass": rope_metric_pass,
        "gate_analysis": gate_analysis,
        "sp_summary": sp_summary,
        "sp_order": sp_order,
        "sp_sections": sp_sections,
        "has_dead_zone": bool(dz_members),
        "dz_histogram_lines": dz_histogram_lines,
        "dz_stats": dz_stats,
        "eps_histogram_lines": eps_histogram_lines,
        "has_ms": bool(ms_members),
        "ms_count": len(ms_members),
        "ms_detail": ms_detail,
        "with_corpus": with_corpus,
        "with_theater": with_theater,
        "subpop_labels": {sp["key"]: sp["label"] for sp in sp_summary},
    }
