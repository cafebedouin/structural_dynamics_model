"""
Turn 2 analysis: test cultural containers typology against corpus.
Loads recon data, measures axes under 3 threshold sets, matches to 6
sketch configurations, runs empirical clustering, cross-checks against
existing orbit infrastructure.
"""

import argparse
import datetime
import json
import os
import math
from collections import Counter
from pathlib import Path

try:
    from sklearn.cluster import KMeans
    from sklearn.preprocessing import StandardScaler
    HAS_SKLEARN = True
except ImportError:
    HAS_SKLEARN = False
    print("WARNING: sklearn not available; empirical clustering will be skipped")

BASE = Path(__file__).parent.parent
RECON_PATH = BASE / "outputs" / "container_typology_recon_data.json"
CORPUS_PATH = BASE / "outputs" / "corpus_data.json"
ORBIT_DATA_PATH = BASE / "outputs" / "orbit_data.json"
OUT_CANDIDATES = BASE / "outputs" / "container_candidates.json"


# ---------------------------------------------------------------------------
# Sketch configuration definitions
# Marked revision: formal + epistemic + working
# Interpretive accretion: formal + traditional + blocked
# Diffuse reconstruction: low/no formal + no_centralized authority
# Decoupled formalization: formal + operational authority
# Implicit practice: no/informal + traditional authority
# Anchored fixity: formal + (epistemic OR traditional) + absent/blocked bandwidth
# ---------------------------------------------------------------------------

CONFIGURATIONS = {
    "marked_revision": {
        "formalization": {"formal", "textual"},
        "authority": {"epistemic"},
        "bandwidth": {"working"},
    },
    "interpretive_accretion": {
        "formalization": {"formal", "textual"},
        "authority": {"traditional"},
        "bandwidth": {"blocked", "absent"},
    },
    "diffuse_reconstruction": {
        "formalization": {"none", "informal"},
        "authority": {"hybrid", "operational"},  # no centralized = not epistemic/traditional
        "bandwidth": {"working", "blocked"},  # any
    },
    "decoupled_formalization": {
        "formalization": {"formal", "textual"},
        "authority": {"operational"},  # authority elsewhere = operational/market
        "bandwidth": {"blocked", "working"},
    },
    "implicit_practice": {
        "formalization": {"none", "informal"},
        "authority": {"traditional"},
        "bandwidth": {"absent", "blocked"},
    },
    "anchored_fixity": {
        "formalization": {"formal"},
        "authority": {"epistemic", "traditional"},
        "bandwidth": {"absent", "blocked"},
    },
}

# Domain → authority type mapping (based on domain name semantics)
DOMAIN_AUTHORITY = {
    # Epistemic: knowledge systems with internal validation norms
    "epistemology": "epistemic",
    "mathematics": "epistemic",
    "mathematical": "epistemic",
    "mathematical_logic": "epistemic",
    "physics": "epistemic",
    "cognitive_science": "epistemic",
    "artificial_intelligence": "epistemic",  # technical validity norms
    # Traditional/bureaucratic: state and institutional legitimacy
    "legal": "traditional",
    "governance": "traditional",
    "institutional": "traditional",
    "public_health": "traditional",  # state public health authority
    # Operational/market: economic and platform power
    "economic": "operational",
    "economic_policy": "operational",
    "labor": "operational",
    "labor_economics": "operational",
    "digital_economy": "operational",
    "environmental": "operational",  # market/regulatory mechanisms
    # Hybrid: political systems with mixed authority sources
    "political": "hybrid",
    "geopolitical": "hybrid",
    "political_economy": "hybrid",
    "social": "hybrid",
    "technology": "hybrid",  # market + regulatory + epistemic
    "technological": "hybrid",
    "healthcare": "hybrid",  # medical epistemic + regulatory + market
    "organizational": "hybrid",
    # Historical domains added for expanded corpus analysis
    "ancient_politics": "traditional",      # state/constitutional authority (Sparta, Athens)
    "religion": "traditional",              # doctrinal authority (Hindu caste)
    "ancient_religion": "traditional",      # divine/priestly authority (Egypt)
    "french_history": "operational",        # procedural/parliamentary authority (Parlements, Estates-General)
    "japanese_history": "traditional",      # Imperial legitimacy (nominal outer; operational split is constraint-level)
}


def get_authority(domain, top_beneficiaries):
    """Authority type: prefer domain mapping, fall back to beneficiary heuristic."""
    mapped = DOMAIN_AUTHORITY.get(domain)
    if mapped:
        return mapped, "domain_name"

    # Beneficiary heuristic
    top_b = [b for b, _ in top_beneficiaries[:5]]
    epistemic_markers = {"academic", "credential", "university", "research", "scientist"}
    traditional_markers = {"state", "government", "regulatory", "authority", "law"}
    operational_markers = {"platform", "financial", "corporation", "market", "industry"}

    scores = {"epistemic": 0, "traditional": 0, "operational": 0}
    for b in top_b:
        bl = b.lower()
        for m in epistemic_markers:
            if m in bl:
                scores["epistemic"] += 1
        for m in traditional_markers:
            if m in bl:
                scores["traditional"] += 1
        for m in operational_markers:
            if m in bl:
                scores["operational"] += 1

    if max(scores.values()) == 0:
        return "hybrid", "beneficiary_fallback"
    winner = max(scores, key=scores.get)
    return winner, "beneficiary_heuristic"


def compute_axes(domain_data, thresholds):
    """Return (formalization, authority, bandwidth) axis values."""
    td = domain_data["type_distribution"]
    sd = domain_data["signature_distribution"]
    n = domain_data["n_constraints"]
    top_b = domain_data["top_beneficiaries"]

    mountain_pct = td.get("mountain", 0) / n
    natural_law_pct = sd.get("natural_law", 0) / n

    # Formalization
    if (mountain_pct >= thresholds["formal_mountain"] or
            natural_law_pct >= thresholds["formal_nl"]):
        formalization = "formal"
    elif (mountain_pct + natural_law_pct >= thresholds["textual_combined"]):
        formalization = "textual"
    elif domain_data["mean_extractiveness"] is not None and domain_data["mean_extractiveness"] < 0.35:
        formalization = "informal"  # low extractiveness, not formal
    else:
        formalization = "none"

    # Authority
    authority, authority_source = get_authority(
        domain_data.get("_domain_name", ""), top_b
    )

    # Bandwidth
    tp = domain_data["mean_types_produced"]
    en = domain_data["emerges_naturally_rate"]
    enf = domain_data["requires_enforcement_rate"]

    if tp is None:
        bandwidth = "unknown"
    elif en >= thresholds["absent_emerges"]:
        bandwidth = "absent"
    elif tp >= thresholds["working_tp"]:
        bandwidth = "working"
    elif tp <= thresholds["blocked_tp"] and enf >= 0.85:
        bandwidth = "blocked"
    else:
        bandwidth = "blocked"  # default for low-tp domains

    return formalization, authority, bandwidth, authority_source


def match_configurations(formalization, authority, bandwidth):
    """Return list of matching configuration names."""
    matches = []
    for name, spec in CONFIGURATIONS.items():
        if (formalization in spec["formalization"] and
                authority in spec["authority"] and
                bandwidth in spec["bandwidth"]):
            matches.append(name)
    return matches


def orbit_signature_entropy(sigs):
    if not sigs:
        return 0.0
    counter = Counter(sigs)
    total = len(sigs)
    return -sum((n / total) * math.log2(n / total) for n in counter.values())


# ---------------------------------------------------------------------------
# Threshold sets
# ---------------------------------------------------------------------------

THRESHOLD_SETS = {
    "conservative": {
        "formal_mountain": 0.25,
        "formal_nl": 0.20,
        "textual_combined": 0.12,
        "working_tp": 5.0,
        "blocked_tp": 1.5,
        "absent_emerges": 0.40,
    },
    "baseline": {
        "formal_mountain": 0.15,
        "formal_nl": 0.10,
        "textual_combined": 0.07,
        "working_tp": 4.0,
        "blocked_tp": 2.5,
        "absent_emerges": 0.30,
    },
    "permissive": {
        "formal_mountain": 0.05,
        "formal_nl": 0.05,
        "textual_combined": 0.03,
        "working_tp": 3.0,
        "blocked_tp": 3.5,
        "absent_emerges": 0.20,
    },
}


def main():
    parser = argparse.ArgumentParser()
    parser.add_argument(
        "--min-domain-size", type=int, default=20,
        help="Minimum constraints for a domain to be included (default 20; use 8-10 for historical domains)",
    )
    args = parser.parse_args()

    with open(RECON_PATH) as f:
        recon = json.load(f)
    with open(CORPUS_PATH) as f:
        corpus_raw = json.load(f)
    constraints = corpus_raw["constraints"]

    # Load orbit data for cross-check
    orbit_by_id = {}
    if ORBIT_DATA_PATH.exists():
        with open(ORBIT_DATA_PATH) as f:
            orbit_raw = json.load(f)
        # orbit_data.json: per constraint with orbit_signature and contexts
        for cid, val in orbit_raw.items():
            if isinstance(val, dict):
                orbit_by_id[cid] = val.get("orbit_signature", [])
        print(f"Loaded orbit data for {len(orbit_by_id)} constraints")

    # Build domain→constraint_ids map
    domain_to_ids = {}
    for cid, c in constraints.items():
        domain = c.get("domain", "") or ""
        l1 = domain.split("/")[0].strip() if domain else "UNKNOWN"
        domain_to_ids.setdefault(l1, []).append(cid)

    by_domain = recon["by_domain"]
    viable_domains = [d for d, v in by_domain.items() if v["n_constraints"] >= args.min_domain_size]

    # ---------------------------------------------------------------------------
    # Main analysis loop
    # ---------------------------------------------------------------------------
    candidates = {}
    for domain in viable_domains:
        ddata = dict(by_domain[domain])
        ddata["_domain_name"] = domain

        # Orbit cross-check: aggregate orbit signatures for this domain's constraints
        orbit_sigs = []
        for cid in domain_to_ids.get(domain, []):
            sigs = orbit_by_id.get(cid, [])
            orbit_sigs.extend(sigs if isinstance(sigs, list) else [sigs])

        orbit_dist = dict(Counter(orbit_sigs).most_common(6))
        orbit_entropy = orbit_signature_entropy(orbit_sigs)
        dominant_orbit = max(orbit_dist, key=orbit_dist.get) if orbit_dist else "unknown"

        # Axis measurement under each threshold set
        axis_results = {}
        for ts_name, thresholds in THRESHOLD_SETS.items():
            form, auth, bw, auth_src = compute_axes(ddata, thresholds)
            matches = match_configurations(form, auth, bw)
            axis_results[ts_name] = {
                "formalization": form,
                "authority": auth,
                "authority_source": auth_src,
                "bandwidth": bw,
                "configuration_matches": matches,
            }

        # Threshold stability: consistent across all three?
        forms = {v["formalization"] for v in axis_results.values()}
        bws = {v["bandwidth"] for v in axis_results.values()}
        form_stable = len(forms) == 1
        bw_stable = len(bws) == 1

        # Configurations supported across all threshold sets
        baseline_matches = set(axis_results["baseline"]["configuration_matches"])
        conservative_matches = set(axis_results["conservative"]["configuration_matches"])
        permissive_matches = set(axis_results["permissive"]["configuration_matches"])

        robust_matches = list(baseline_matches & conservative_matches)
        threshold_sensitive = list(permissive_matches - conservative_matches)

        candidates[domain] = {
            "n_constraints": ddata["n_constraints"],
            "axes_by_threshold": axis_results,
            "formalization_stable": form_stable,
            "bandwidth_stable": bw_stable,
            "robust_configuration_matches": robust_matches,
            "threshold_sensitive_matches": threshold_sensitive,
            "orbit_distribution": orbit_dist,
            "orbit_entropy": round(orbit_entropy, 4),
            "dominant_orbit": dominant_orbit,
            # Key stats for reference
            "mountain_pct": round(ddata["type_distribution"].get("mountain", 0) / ddata["n_constraints"], 4),
            "natural_law_pct": round(ddata["signature_distribution"].get("natural_law", 0) / ddata["n_constraints"], 4),
            "mean_extractiveness": ddata["mean_extractiveness"],
            "mean_variance_ratio": ddata["mean_variance_ratio"],
            "mean_types_produced": ddata["mean_types_produced"],
            "requires_enforcement_rate": ddata["requires_enforcement_rate"],
            "emerges_naturally_rate": ddata["emerges_naturally_rate"],
        }

    # ---------------------------------------------------------------------------
    # Configuration matching summary
    # ---------------------------------------------------------------------------
    print("\n=== CONFIGURATION MATCHING SUMMARY ===")
    for ts_name in THRESHOLD_SETS:
        print(f"\nThreshold set: {ts_name}")
        config_counts = Counter()
        no_match = []
        for domain, cdata in candidates.items():
            matches = cdata["axes_by_threshold"][ts_name]["configuration_matches"]
            if matches:
                for m in matches:
                    config_counts[m] += 1
            else:
                no_match.append(domain)
        for cfg in CONFIGURATIONS:
            print(f"  {cfg}: {config_counts.get(cfg, 0)} containers")
        print(f"  [no match]: {len(no_match)} containers — {no_match[:5]}")

    print("\n=== ROBUST MATCHES (conservative AND baseline) ===")
    for domain, cdata in sorted(candidates.items()):
        form_c = cdata["axes_by_threshold"]["conservative"]["formalization"]
        form_b = cdata["axes_by_threshold"]["baseline"]["formalization"]
        auth = cdata["axes_by_threshold"]["baseline"]["authority"]
        bw_c = cdata["axes_by_threshold"]["conservative"]["bandwidth"]
        bw_b = cdata["axes_by_threshold"]["baseline"]["bandwidth"]
        robust = cdata["robust_configuration_matches"]
        sensitive = cdata["threshold_sensitive_matches"]
        print(f"  {domain:28s}  form={form_b}/{form_c}  auth={auth}  bw={bw_b}/{bw_c}  robust={robust}  sensitive={sensitive}")

    # ---------------------------------------------------------------------------
    # Empirical clustering
    # ---------------------------------------------------------------------------
    print("\n=== EMPIRICAL CLUSTERING ===")
    if HAS_SKLEARN:
        # Encode axes numerically from baseline
        encoding = {
            "formalization": {"none": 0, "informal": 1, "textual": 2, "formal": 3},
            "authority": {"epistemic": 0, "traditional": 1, "hybrid": 2, "operational": 3},
            "bandwidth": {"absent": 0, "blocked": 1, "working": 2},
        }
        domain_list = list(candidates.keys())
        X = []
        for domain in domain_list:
            axes = candidates[domain]["axes_by_threshold"]["baseline"]
            row = [
                encoding["formalization"].get(axes["formalization"], 1),
                encoding["authority"].get(axes["authority"], 2),
                encoding["bandwidth"].get(axes["bandwidth"], 1),
            ]
            X.append(row)

        inertias = {}
        for k in range(2, 9):
            km = KMeans(n_clusters=k, random_state=42, n_init=20)
            km.fit(X)
            inertias[k] = km.inertia_

        print("k -> inertia (elbow method):")
        for k, inertia in inertias.items():
            print(f"  k={k}: {inertia:.2f}")

        # Fit final model at k=6 (sketch's number) and k from elbow
        # Simple elbow: largest drop in inertia
        drops = {k: inertias[k-1] - inertias[k] for k in range(3, 9)}
        elbow_k = max(drops, key=drops.get)
        print(f"\nElbow at k={elbow_k} (largest inertia drop)")

        for k_label, k_val in [("sketch_k", 6), ("elbow_k", elbow_k)]:
            km = KMeans(n_clusters=k_val, random_state=42, n_init=20)
            labels = km.fit_predict(X)
            print(f"\nClusters at k={k_val} ({k_label}):")
            cluster_domains = {}
            for i, domain in enumerate(domain_list):
                cluster_domains.setdefault(labels[i], []).append(domain)
            for cluster_id in sorted(cluster_domains):
                members = cluster_domains[cluster_id]
                sample_axes = candidates[members[0]]["axes_by_threshold"]["baseline"]
                print(f"  Cluster {cluster_id}: {members}")
                print(f"    (representative: form={sample_axes['formalization']}, auth={sample_axes['authority']}, bw={sample_axes['bandwidth']})")

        # Store clustering results in candidates
        km6 = KMeans(n_clusters=6, random_state=42, n_init=20)
        labels6 = km6.fit_predict(X)
        for i, domain in enumerate(domain_list):
            candidates[domain]["empirical_cluster_k6"] = int(labels6[i])
        km_elbow = KMeans(n_clusters=elbow_k, random_state=42, n_init=20)
        labels_elbow = km_elbow.fit_predict(X)
        for i, domain in enumerate(domain_list):
            candidates[domain]["empirical_cluster_elbow"] = int(labels_elbow[i])

        candidates["_meta"] = {
            "elbow_k": elbow_k,
            "inertias": inertias,
        }
    else:
        print("Skipped (sklearn not available)")

    # ---------------------------------------------------------------------------
    # Orbit cross-check summary
    # ---------------------------------------------------------------------------
    print("\n=== ORBIT CROSS-CHECK ===")
    for domain, cdata in sorted(candidates.items()):
        if domain.startswith("_"):
            continue
        dom_orbit = cdata["dominant_orbit"]
        orb_ent = cdata["orbit_entropy"]
        form = cdata["axes_by_threshold"]["baseline"]["formalization"]
        print(f"  {domain:28s}  dominant_orbit={dom_orbit:15s}  orbit_entropy={orb_ent:.3f}  formalization={form}")

    # ---------------------------------------------------------------------------
    # L2 decomposition (for the 6 largest domains)
    # ---------------------------------------------------------------------------
    print("\n=== L2 DECOMPOSITION (largest 6 L1 domains) ===")
    l2_breakdown = recon.get("l2_breakdown_top6", {})
    for l1, l2s in l2_breakdown.items():
        viable_l2 = [(k, n) for k, n in l2s if n >= 20]
        print(f"  {l1}: {len(l2s)} L2 sub-domains; {len(viable_l2)} with 20+ constraints")
        for k, n in l2s[:8]:
            print(f"    {k}: {n}")

    # ---------------------------------------------------------------------------
    # Write output
    # ---------------------------------------------------------------------------
    # -----------------------------------------------------------------------
    # INPUT-PROVENANCE STAMP (OQ-296, 2026-08-18).
    #
    # Every field in this artifact is derived from RECON_PATH, which is NOT
    # regenerated by run_pipeline.py — neither this script nor
    # container_typology_recon.py is wired into it. The recon file on disk
    # predates the 2026-06-05 corpus reset, so the whole record describes a
    # different corpus than the live manifest does. The stamp records the
    # measured provenance of the input so a reader is never left inferring
    # currency from the file's own mtime.
    #
    # The stamp reports MEASUREMENTS ONLY. It deliberately does not explain WHY
    # the recon-era signature counts look the way they do — that is the spawned
    # staleness OQ's question, and asserting a mechanism here would repeat the
    # error this stamp exists to prevent.
    # -----------------------------------------------------------------------
    recon_n = recon.get("total_constraints")
    recon_mtime = datetime.datetime.utcfromtimestamp(
        os.path.getmtime(RECON_PATH)).strftime("%Y-%m-%dT%H:%M:%SZ")
    live_manifest = {}
    try:
        with open(BASE / "outputs" / "pipeline_output.json") as f:
            live_manifest = json.load(f).get("manifest", {})
    except (OSError, ValueError):
        live_manifest = {}

    candidates["_input_provenance"] = {
        "derived_entirely_from": str(RECON_PATH.relative_to(BASE)),
        "recon_file_mtime": recon_mtime,
        "recon_total_constraints": recon_n,
        "live_manifest_n_constraints": live_manifest.get("n_constraints"),
        "live_manifest_pipeline_run_at": live_manifest.get("pipeline_run_at"),
        "refreshed_by_run_pipeline": False,
        "scope": "EVERY field in this artifact — type_distribution, "
                 "signature_distribution, mountain_pct, natural_law_pct, "
                 "mean_extractiveness, orbit stats, and all derived axes — comes "
                 "from the recon file named above. None of it is a measurement of "
                 "the corpus described by the live manifest.",
        "not_a_falsifier_hit": (
            "This artifact reports non-zero natural_law_pct values. That does NOT "
            "trip OQ-296's kill condition and is not evidence that OQ-113's range "
            "finding regressed. The kill condition is about "
            "constraint_signature(_, natural_law) firing at HEAD; these are "
            "pre-reset recon values over a different corpus, not a HEAD firing. "
            "The live-leg census run 2026-08-18 (Phase 0, "
            "audits/2026-08-18_oq296_consumer_honesty/PHASE0_REWITNESS.md) returned "
            "natural_law = 0 and coordination_scaffold = 0 over a non-degenerate "
            "7-signature histogram."
        ),
        "issues": ["OQ-296"],
    }

    with open(OUT_CANDIDATES, "w") as f:
        json.dump(candidates, f, indent=2)
    print(f"\nWrote {OUT_CANDIDATES}")


if __name__ == "__main__":
    main()
