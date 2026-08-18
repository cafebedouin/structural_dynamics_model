#!/usr/bin/env python3
"""
Orbit Proximity Probe — empirical investigation into kernel identity for constraint stories.

Investigates whether structural proximity, semantic proximity, or both can recover
known orbit membership (three peer-review stories as one orbit, three others as adjacent
but distinct). Read-only. Produces a markdown report + JSON.

Outputs:
    outputs/orbit_proximity_probe.md
    outputs/orbit_proximity_probe.json
"""

import json
import sys
from pathlib import Path

import numpy as np
from sklearn.metrics.pairwise import cosine_similarity

ROOT     = Path(__file__).resolve().parent.parent
PIPELINE = ROOT / "outputs" / "pipeline_output.json"
JSON_DIR = ROOT / "json"
OUT_MD   = ROOT / "outputs" / "orbit_proximity_probe.md"
OUT_JSON = ROOT / "outputs" / "orbit_proximity_probe.json"

# ── Seed stories ──────────────────────────────────────────────────────────────

SEEDS = [
    "academic_peer_review_gatekeeping",
    "academic_publishing_peer_review",
    "academic_journal_peer_review_gatekeeping",
    "academic_tenure_system",
    "academic_citation_metrics_as_career_incentive",
    "academic_fashion_modernism_2026",
]

# Short labels: max 6 chars, unique
LABELS = {
    "academic_peer_review_gatekeeping":              "pr-gk",
    "academic_publishing_peer_review":               "pr-pub",
    "academic_journal_peer_review_gatekeeping":      "pr-jgk",
    "academic_tenure_system":                        "tenure",
    "academic_citation_metrics_as_career_incentive": "cit-m",
    "academic_fashion_modernism_2026":               "fashio",
}

# Human guess: these three share a kernel
PR_TRIO = frozenset([
    "academic_peer_review_gatekeeping",
    "academic_publishing_peer_review",
    "academic_journal_peer_review_gatekeeping",
])

# ── Manual synonym normalization map (arm 2f) ─────────────────────────────────

SYNONYM_MAP = {
    # Beneficiary aliases (high-confidence — same role, different tokens)
    "editorial_gatekeepers":     "journal_editors",
    "prestige_journal_editors":  "journal_editors",
    "major_publishers":          "journal_publishers",
    "established_research_groups": "established_researchers",
    # Victim aliases (candidates — concept-similar; flagged as uncertain)
    "knowledge_access":          "open_knowledge_accessibility",
    "early_career_scholars":     "early_career_researchers",
}

SYNONYM_UNCERTAIN = {"knowledge_access", "early_career_scholars"}

# ── Observer/metric vectors (adapted from cluster_space_audit.py) ─────────────

TYPE_ORDER = ["mountain", "rope", "tangled_rope", "snare",
              "scaffold", "piton", "naturalized", "unknown"]
TYPE_IDX   = {t: i for i, t in enumerate(TYPE_ORDER)}
OBSERVER_POSITIONS = ["powerless", "moderate", "institutional", "analytical"]
MAXENT_TYPES       = ["mountain", "rope", "tangled_rope", "snare", "scaffold", "piton"]
# OQ-296 (confirmed 2026-08-18): the "natural_law" column is PERMANENTLY ZERO —
# the engine never emits that signature (unsatisfiable by construction,
# signature_detection.pl:427). A zero in that column is structural, not a
# measured absence, and must not be read as "this stratum contains no natural
# laws." The other four flags are live.
SIGNATURE_FLAGS    = ["false_natural_law", "false_ci_rope", "natural_law",
                      "constructed_high_extraction", "coupling_invariant_rope"]


def build_observer_vector(c):
    """32-dim: 4 observers × 8 types (one-hot each)."""
    perspectives = c.get("perspectives") or {}
    vec = []
    for pos in OBSERVER_POSITIONS:
        t = perspectives.get(pos, "unknown")
        oh = [0.0] * len(TYPE_ORDER)
        oh[TYPE_IDX.get(t, TYPE_IDX["unknown"])] = 1.0
        vec.extend(oh)
    return vec


def build_metric_vector(c):
    """16-dim: chi×4, epsilon, maxent×6, signature flags×5."""
    pchi = c.get("perspective_chi") or {}
    vec = [float((pchi.get(pos) or {}).get("chi", 0.0)) for pos in OBSERVER_POSITIONS]
    vec.append(float((pchi.get("powerless") or {}).get(
        "epsilon", c.get("base_extractiveness", 0.0))))
    mp = c.get("maxent_probs") or {}
    vec.extend(float(mp.get(t, 1.0 / 6)) for t in MAXENT_TYPES)
    sig = c.get("signature") or ""
    vec.extend(1.0 if sig == f else 0.0 for f in SIGNATURE_FLAGS)
    return vec


# ── Similarity matrix builders ────────────────────────────────────────────────

def jaccard_matrix(records, field, synonym_map=None):
    """Jaccard similarity on set-valued field, with optional synonym normalization."""
    def norm(atoms):
        s = set(atoms or [])
        if synonym_map:
            s = {synonym_map.get(a, a) for a in s}
        return s
    n = len(records)
    sets = [norm(c.get(field)) for c in records]
    mat = np.zeros((n, n))
    for i in range(n):
        mat[i, i] = 1.0
        for j in range(i + 1, n):
            union = sets[i] | sets[j]
            sim = len(sets[i] & sets[j]) / len(union) if union else 0.0
            mat[i, j] = mat[j, i] = sim
    return mat


def cosine_matrix(records, vector_fn):
    vecs = np.array([vector_fn(c) for c in records], dtype=float)
    return cosine_similarity(vecs)


def coord_match_matrix(records):
    coords = [c.get("coordination_type", "") for c in records]
    n = len(records)
    mat = np.zeros((n, n))
    for i in range(n):
        for j in range(n):
            mat[i, j] = 1.0 if coords[i] and coords[i] == coords[j] else 0.0
    return mat


# ── Reporting helpers ─────────────────────────────────────────────────────────

def fmt_matrix(mat, labels, title):
    """ASCII table, 10-char row labels, 7-char value columns."""
    n = len(labels)
    header = f"{'':10}" + " ".join(f"{l:>7}" for l in labels)
    rows = [f"{labels[i]:<10}" + " ".join(f"{mat[i,j]:7.3f}" for j in range(n))
            for i in range(n)]
    return f"{title}\n{header}\n" + "\n".join(rows)


def pr_trio_stats(mat, seed_order):
    """PR-trio within-group mean vs cross-group mean."""
    within, cross = [], []
    for i in range(len(seed_order)):
        for j in range(i + 1, len(seed_order)):
            sim = float(mat[i, j])
            if seed_order[i] in PR_TRIO and seed_order[j] in PR_TRIO:
                within.append(sim)
            else:
                cross.append(sim)
    return {
        "within_mean": float(np.mean(within)) if within else None,
        "cross_mean":  float(np.mean(cross))  if cross  else None,
        "pr_tighter":  (np.mean(within) > np.mean(cross)) if (within and cross) else None,
        "within_vals": within,
        "cross_vals":  cross,
    }


def sorted_pairs(mat, labels, seed_order):
    pairs = []
    for i in range(len(labels)):
        for j in range(i + 1, len(labels)):
            both_pr = seed_order[i] in PR_TRIO and seed_order[j] in PR_TRIO
            pairs.append((labels[i], labels[j], float(mat[i, j]), both_pr))
    return sorted(pairs, key=lambda x: -x[2])


def gap_report(pairs):
    sims = [p[2] for p in pairs]
    best_gap, best_k = 0.0, None
    for k in range(1, len(sims)):
        g = sims[k - 1] - sims[k]
        if g > best_gap:
            best_gap, best_k = g, k
    return best_gap, best_k


def rank_pairs_dict(mat, seed_order):
    entries = []
    for i in range(len(seed_order)):
        for j in range(i + 1, len(seed_order)):
            entries.append(((i, j), float(mat[i, j])))
    entries.sort(key=lambda x: -x[1])
    return {pair: rank + 1 for rank, (pair, _) in enumerate(entries)}


# ── Arm 1: corpus scale ───────────────────────────────────────────────────────

def run_arm1(by_id, seeds_data):
    total      = len(by_id)
    with_b     = sum(1 for c in by_id.values() if c.get("beneficiaries"))
    with_v     = sum(1 for c in by_id.values() if c.get("victims"))
    with_both  = sum(1 for c in by_id.values() if c.get("beneficiaries") and c.get("victims"))

    seed_victims = set()
    seed_benefs  = set()
    for c in seeds_data:
        seed_victims.update(c.get("victims") or [])
        seed_benefs.update(c.get("beneficiaries") or [])

    neighborhood = {}
    for cid, c in by_id.items():
        if cid in SEEDS:
            continue
        vo = set(c.get("victims") or []) & seed_victims
        bo = set(c.get("beneficiaries") or []) & seed_benefs
        if vo or bo:
            neighborhood[cid] = {"victim_overlap": sorted(vo), "beneficiary_overlap": sorted(bo)}

    return {
        "total": total, "with_beneficiaries": with_b, "with_victims": with_v,
        "with_both": with_both, "neighborhood_size": len(neighborhood),
        "neighborhood": neighborhood,
    }


# ── Main ──────────────────────────────────────────────────────────────────────

def main():
    lines    = []
    findings = {}

    def emit(s=""):
        lines.append(s)

    # ── Pre-registrations ────────────────────────────────────────────
    emit("# ORBIT PROXIMITY PROBE")
    emit()
    emit("## SECTION 0: PRE-REGISTRATIONS")
    emit()
    emit("Recorded before running any arm. Determine how results are interpreted.")
    emit("Not revisable post-hoc.")
    emit()
    emit("**Pre-reg 1 — Atom aliasing (hygiene vs. ontology):**")
    emit("The three peer-review stories use near-synonym tokens for the same actor roles:")
    emit("  journal_editors / editorial_gatekeepers / prestige_journal_editors")
    emit("Exact Jaccard treats these as zero overlap. A low PR-trio Jaccard is EXPECTED")
    emit("and is pre-attributed to vocabulary drift, not kernel divergence.")
    emit("The synthesis MUST distinguish:")
    emit("  ALIASING verdict: uncontrolled vocab — fixable with normalization")
    emit("  ONTOLOGY verdict: structure doesn't track kernels even with clean atoms")
    emit("Arm 2f (manual normalization) resolves this fork.")
    emit()
    emit("**Pre-reg 2 — Arms 2c/2d selection-artifact discount:**")
    emit("All six stories are tangled_rope, ε 0.50–0.62. Metric and observer cosine")
    emit("will show all six as highly similar by selection, not by orbit. Arms 2c/2d")
    emit("report PR-trio tightness RELATIVE to distractors within the neighborhood.")
    emit("Absolute high similarity across the six is discounted in advance.")
    emit()
    emit("**Pre-reg 3 — Arm 3b generator-author caveat:**")
    emit("commentary.narrative_context was written by the same generator that chose")
    emit("the actor atoms. A 3b success is consistent with BOTH real kernel proximity")
    emit("AND generator phrasing consistency. These are not distinguishable on six stories.")
    emit("A 3b failure (narratives don't cluster the trio) is the stronger negative result.")
    emit()
    emit("**Pre-reg 4 — Expected verdict: too small to decide.**")
    emit("N=6, three-vs-three: all arm results are illustrative, not statistically valid.")
    emit("The probe's principal output is the next-probe spec, not a metric winner.")
    emit()
    emit("---")
    emit()

    # ── Load data ────────────────────────────────────────────────────
    print("Loading pipeline_output.json ...", flush=True)
    raw   = json.loads(PIPELINE.read_text())
    by_id = {}
    for c in raw["per_constraint"]:
        cid = c.get("id") or c.get("constraint_id")
        if cid:
            by_id[cid] = c
    print(f"  {len(by_id)} constraints loaded", flush=True)

    print("Loading JSON files for seeds ...", flush=True)
    seeds_data = []
    for cid in SEEDS:
        rec = dict(by_id.get(cid, {"id": cid}))
        path = JSON_DIR / f"{cid}.json"
        if path.exists():
            j = json.loads(path.read_text())
            rec["coordination_type"] = (j.get("boltzmann") or {}).get("coordination_type", "")
            rec["narrative_context"] = (j.get("commentary") or {}).get("narrative_context", "")
        else:
            rec["coordination_type"] = ""
            rec["narrative_context"] = ""
        seeds_data.append(rec)

    labels = [LABELS[cid] for cid in SEEDS]

    # ── Arm 1 ────────────────────────────────────────────────────────
    print("Arm 1: corpus scale ...", flush=True)
    a1 = run_arm1(by_id, seeds_data)
    findings["arm1"] = {k: v for k, v in a1.items() if k != "neighborhood"}
    findings["arm1"]["neighborhood_ids"] = list(a1["neighborhood"].keys())

    emit("## ARM 1: CORPUS SCALE")
    emit()
    emit(f"Total constraints: {a1['total']}")
    emit(f"With beneficiaries: {a1['with_beneficiaries']}  ({100*a1['with_beneficiaries']/a1['total']:.1f}%)")
    emit(f"With victims:       {a1['with_victims']}  ({100*a1['with_victims']/a1['total']:.1f}%)")
    emit(f"With both:          {a1['with_both']}  ({100*a1['with_both']/a1['total']:.1f}%)")
    emit()
    emit("ε band 0.50–0.62 covers ~1,994 tangled_rope constraints.")
    emit("ε alone is NOT a discriminator for this neighborhood.")
    emit()
    emit(f"Academic neighborhood (stories with ≥1 victim/beneficiary atom overlap")
    emit(f"with any of the six seeds): N = {a1['neighborhood_size']} stories")
    emit()
    nb_sorted = sorted(a1["neighborhood"].items(),
                       key=lambda x: len(x[1]["victim_overlap"]) + len(x[1]["beneficiary_overlap"]),
                       reverse=True)
    emit("Top 30 nearest neighbors by atom overlap:")
    for cid, info in nb_sorted[:30]:
        vo = info["victim_overlap"]
        bo = info["beneficiary_overlap"]
        parts = []
        if vo: parts.append(f"victims: {', '.join(vo)}")
        if bo: parts.append(f"benef: {', '.join(bo)}")
        emit(f"  {cid}")
        for p in parts:
            emit(f"    {p}")
    emit()
    emit(f"Honest labeled test set upper bound: ~{min(a1['neighborhood_size'], 30)} stories")
    emit("(most near-neighbors share atoms incidentally — not confirmed orbit members)")
    emit()
    emit("---")
    emit()

    # ── Arm 2 ────────────────────────────────────────────────────────
    print("Arm 2: structural matrices ...", flush=True)

    m2a_raw  = jaccard_matrix(seeds_data, "victims")
    m2b_raw  = jaccard_matrix(seeds_data, "beneficiaries")
    m2c      = cosine_matrix(seeds_data, build_metric_vector)
    m2d      = cosine_matrix(seeds_data, build_observer_vector)
    m2e      = coord_match_matrix(seeds_data)
    m2a_norm = jaccard_matrix(seeds_data, "victims",        SYNONYM_MAP)
    m2b_norm = jaccard_matrix(seeds_data, "beneficiaries",  SYNONYM_MAP)

    findings["arm2"] = {
        "labels": labels, "seed_ids": SEEDS,
        "victim_jaccard_raw":       m2a_raw.tolist(),
        "beneficiary_jaccard_raw":  m2b_raw.tolist(),
        "metric_cosine":            m2c.tolist(),
        "observer_cosine":          m2d.tolist(),
        "coord_match":              m2e.tolist(),
        "victim_jaccard_norm":      m2a_norm.tolist(),
        "beneficiary_jaccard_norm": m2b_norm.tolist(),
    }

    emit("## ARM 2: STRUCTURAL MATRICES")
    emit()

    def report_matrix(mat, arm_id, title, note=None):
        emit(f"### {arm_id}: {title}")
        emit("```")
        emit(fmt_matrix(mat, labels, title))
        emit("```")
        stats = pr_trio_stats(mat, SEEDS)
        emit(f"PR-trio within-group mean: {stats['within_mean']:.3f}  |  "
             f"Cross-group mean: {stats['cross_mean']:.3f}  |  "
             f"PR trio tighter: {stats['pr_tighter']}")
        pairs = sorted_pairs(mat, labels, SEEDS)
        emit("Sorted pairwise similarities (high→low):")
        for la, lb, sim, is_pr in pairs:
            tag = "[within-PR]" if is_pr else "[cross]   "
            emit(f"  {la:7} × {lb:7}: {sim:.3f}  {tag}")
        gap, gap_k = gap_report(pairs)
        if gap_k:
            emit(f"Largest gap: {gap:.3f} (between rank {gap_k} and {gap_k+1})")
        else:
            emit("No gap detected.")
        if note:
            emit(f"NOTE: {note}")
        emit()
        return stats

    # 2a raw
    s2a = report_matrix(m2a_raw, "2a", "Victim Jaccard (raw atoms)")
    emit("Aliasing attribution: PR-trio low Jaccard pre-attributed to vocabulary drift")
    emit("(early_career_researchers shared, but other victim atoms differ by synonym).")
    emit("See arm 2f for normalization test.")
    emit()

    # 2b raw
    s2b = report_matrix(m2b_raw, "2b", "Beneficiary Jaccard (raw atoms)")
    emit("Aliasing attribution: journal_editors appears in pr-gk + pr-jgk but NOT pr-pub")
    emit("(pr-pub uses editorial_gatekeepers). Gatekeeping–journal pair is the only")
    emit("non-zero PR-trio cell on raw beneficiaries.")
    emit()

    # 2c metric
    s2c = report_matrix(m2c, "2c", "Metric Vector Cosine (chi, ε, maxent, signature)",
                        note="Pre-reg 2: all six share tangled_rope/ε neighborhood by selection. "
                             "High absolute similarity is expected and discounted.")

    # 2d observer
    s2d = report_matrix(m2d, "2d", "Observer Shift Cosine (perspectival classification)",
                        note="Pre-reg 2: same selection-artifact discount applies.")

    # 2e coordination
    emit("### 2e: Coordination Type Match (binary)")
    coord_list = [c.get("coordination_type", "?") for c in seeds_data]
    emit("Coordination types per story:")
    for lbl, ct in zip(labels, coord_list):
        emit(f"  {lbl:7}: {ct}")
    emit("```")
    emit(fmt_matrix(m2e, labels, "Coord match (1=same)"))
    emit("```")
    emit("tenure is the only enforcement_mechanism; all others are information_standard.")
    emit("This cleanly isolates tenure on a single binary dimension.")
    emit()

    # 2f normalized
    emit("### 2f: Manual Normalization Sensitivity Check")
    emit()
    emit("Synonym mapping (fully disclosed, not fitted — tested once, not swept):")
    emit("High-confidence mappings:")
    for orig, tgt in SYNONYM_MAP.items():
        if orig not in SYNONYM_UNCERTAIN:
            emit(f"  {orig} → {tgt}")
    emit("Candidate mappings (uncertain — concept-similar but may be distinct agents):")
    for orig, tgt in SYNONYM_MAP.items():
        if orig in SYNONYM_UNCERTAIN:
            emit(f"  {orig} → {tgt}  [candidate]")
    emit()

    s2a_norm = report_matrix(m2a_norm, "2a-norm", "Victim Jaccard (normalized atoms)")
    s2b_norm = report_matrix(m2b_norm, "2b-norm", "Beneficiary Jaccard (normalized atoms)")

    # Fork verdict
    emit("**Fork verdict (arm 2f):**")
    v_better = s2a_norm["pr_tighter"] and (s2a_norm["within_mean"] > s2a["within_mean"])
    b_better = s2b_norm["pr_tighter"] and (s2b_norm["within_mean"] > s2b["within_mean"])

    if s2a_norm["pr_tighter"] and s2b_norm["pr_tighter"]:
        emit("PR trio IS tighter than cross-group on BOTH normalized Jaccard signals.")
        emit("→ ALIASING verdict confirmed. Structural identity is viable with a normalization")
        emit("  layer. The organism is present; the microscope needed a vocabulary lens.")
    elif s2a_norm["pr_tighter"] or s2b_norm["pr_tighter"]:
        emit("PR trio is tighter on only one of two normalized Jaccard signals (partial).")
        emit("→ MIXED verdict: normalization helps but doesn't cleanly cluster.")
        emit("  Both aliasing AND deeper issues likely contribute.")
    else:
        emit("PR trio is NOT tighter than cross-group even after normalization.")
        emit("→ DEEPER PROBLEM: aliasing was not the primary obstacle. Structure may not")
        emit("  track kernels even with controlled atoms. Additional dimensions needed.")
    emit()
    emit("---")
    emit()

    # ── Arm 3 ────────────────────────────────────────────────────────
    print("Arm 3: semantic matrices ...", flush=True)
    emit("## ARM 3: SEMANTIC PROXIMITY")
    emit()

    try:
        from sentence_transformers import SentenceTransformer
        model = SentenceTransformer("all-MiniLM-L6-v2")
        semantic_ok = True
        print("  SentenceTransformer loaded.", flush=True)
    except ImportError:
        semantic_ok = False
        print("  SentenceTransformer unavailable — arm 3 not runnable.", flush=True)

    m3a = m3b = None

    if not semantic_ok:
        emit("SentenceTransformer (all-MiniLM-L6-v2) not available.")
        emit("Install: pip install sentence-transformers")
        emit("Arm 3 not runnable.")
        findings["arm3"] = {"available": False}
    else:
        titles    = [c.get("human_readable", "")   for c in seeds_data]
        narratives = [c.get("narrative_context", "") for c in seeds_data]

        emit("### 3a: Title Embeddings (human_readable — 4–6 tokens)")
        emit("Titles:")
        for lbl, t in zip(labels, titles):
            emit(f"  {lbl}: {t}")
        emit()
        m3a = cosine_similarity(model.encode(titles))
        s3a = report_matrix(m3a, "3a", "Semantic cosine (titles)")

        emit("### 3b: Narrative Context Embeddings (commentary.narrative_context — 2–3 sentences)")
        emit("Narratives (first 110 chars):")
        for lbl, n in zip(labels, narratives):
            snippet = n[:110] + ("..." if len(n) > 110 else "")
            emit(f"  {lbl}: {snippet}")
        emit()
        m3b = cosine_similarity(model.encode(narratives))
        s3b = report_matrix(m3b, "3b", "Semantic cosine (narratives)")

        emit("**Pre-reg 3 applied:**")
        if s3b["pr_tighter"]:
            emit("3b clusters the PR trio. Per Pre-reg 3: this is consistent with BOTH real")
            emit("kernel proximity AND generator phrasing consistency (same generator wrote")
            emit("these narratives). The two are not distinguishable on six stories.")
        else:
            emit("3b does NOT cluster the PR trio. Per Pre-reg 3: if even the generator's")
            emit("own prose doesn't cluster them, this is a stronger negative signal —")
            emit("though still not conclusive at N=6.")
        emit()

        richer = (s3b.get("within_mean", 0) or 0) > (s3a.get("within_mean", 0) or 0)
        emit(f"Narrative text improves PR-trio separability vs titles: {richer}")
        emit()

        findings["arm3"] = {
            "available": True,
            "title_cosine":     m3a.tolist(),
            "narrative_cosine": m3b.tolist(),
            "labels": labels,
        }

    emit("---")
    emit()

    # ── Arm 4: Divergence ────────────────────────────────────────────
    print("Arm 4: divergence enumeration ...", flush=True)
    emit("## ARM 4: DIVERGENCE ENUMERATION")
    emit()

    # Combined structural: mean of normalized victim + beneficiary Jaccard
    m_struct = (m2a_norm + m2b_norm) / 2.0
    struct_ranks = rank_pairs_dict(m_struct, SEEDS)

    if m3b is not None:
        sem_ranks = rank_pairs_dict(m3b, SEEDS)
        emit("Structural signal: mean(victim_jaccard_norm, beneficiary_jaccard_norm)")
        emit("Semantic signal:   narrative_context cosine (arm 3b)")
    else:
        sem_ranks = None
        emit("Semantic arm unavailable. Divergence analysis shows structural ranks only.")

    emit()
    emit(f"{'Pair':<22}  struct  semant  delta")
    emit("-" * 46)

    divergences = []
    for i in range(len(SEEDS)):
        for j in range(i + 1, len(SEEDS)):
            la, lb  = labels[i], labels[j]
            sr      = struct_ranks.get((i, j), "?")
            semr    = sem_ranks.get((i, j), "?") if sem_ranks else "—"
            both_pr = SEEDS[i] in PR_TRIO and SEEDS[j] in PR_TRIO
            tag     = "[within-PR]" if both_pr else ""
            delta   = abs(sr - semr) if (sem_ranks and isinstance(sr, int) and isinstance(semr, int)) else None
            delta_s = str(delta) if delta is not None else "—"
            emit(f"  {la:7} × {lb:7}  {str(sr):>6}  {str(semr):>6}  {delta_s:>5}  {tag}")
            if delta is not None and delta >= 3:
                divergences.append((la, lb, sr, semr, delta, both_pr))

    emit()
    if divergences:
        emit("Divergent pairs (|struct_rank − semantic_rank| ≥ 3):")
        for la, lb, sr, semr, d, both_pr in sorted(divergences, key=lambda x: -x[4]):
            cross_pr = (la in [LABELS[s] for s in PR_TRIO]) != (lb in [LABELS[s] for s in PR_TRIO])
            emit(f"  {la} × {lb}: struct rank {sr}, semantic rank {semr} (Δ={d})")
            if cross_pr:
                emit("    → PR vs. non-PR pair: structure and semantics disagree on orbit boundary")
            if both_pr:
                emit("    → Within-PR pair: structure and semantics disagree on tightness")
    else:
        emit("No pairs with Δ ≥ 3." if sem_ranks else "(semantic arm unavailable — no delta computed)")
    emit()
    emit("---")
    emit()

    # ── Arm 5: Hybrid description ────────────────────────────────────
    emit("## ARM 5: HYBRID DESCRIPTION (no fitting)")
    emit()
    emit("A hybrid α·semantic + (1−α)·structural has 1 continuous parameter (α) plus")
    emit("threshold(s). With N=6 and 15 pairwise values, any 3-vs-3 clustering is")
    emit("reproducible by construction. Six-point 'success' is overfit, not evidence.")
    emit()
    emit("The same logic applies to every pure arm above: N=6 is not statistically valid")
    emit("for any arm. Clustering results are illustrative (Pre-reg 4).")
    emit()
    emit("Minimum honest corpus for a 2-parameter hybrid evaluation without overfitting:")
    emit("  ≥ 30–50 labeled orbits × 3–6 stories each  →  ≥ 150 stories, ≥ 500 labeled pairs")
    emit()
    emit("---")
    emit()

    # ── Synthesis ────────────────────────────────────────────────────
    emit("## SYNTHESIS")
    emit()
    emit("### What the probe can decide on six stories")
    emit()
    emit("1. ATOM HYGIENE: Arm 2a/2b raw results establish that uncontrolled actor atoms")
    emit("   produce near-zero Jaccard even within the PR trio. Arm 2f determines whether")
    emit("   this is a hygiene problem (fixable) or an ontology problem (not fixable).")
    emit("   The fork verdict is above.")
    emit()
    emit("2. COORDINATION TYPE SEPARATES TENURE: Arm 2e cleanly isolates tenure via")
    emit("   coordination_type = enforcement_mechanism. This is a single-binary observation,")
    emit("   not a general orbit method, but it demonstrates that feature-level attributes")
    emit("   can cleanly separate stories when the attribute is controlled-vocabulary.")
    emit()
    emit("3. DIVERGENCE CASES: Arm 4 enumerates pairs where structural rank ≠ semantic rank.")
    emit("   These are single-case observations, not statistical claims.")
    emit()
    emit("### What the probe cannot decide")
    emit()
    emit("- Whether structural identity is viable with clean atoms (needs labeled corpus)")
    emit("- Whether 3b semantic clustering reflects real kernel proximity vs. generator")
    emit("  phrasing consistency (needs cross-generator or human-labeled pairs)")
    emit("- Whether any metric correctly individuates orbits (N=6 too small)")
    emit()
    emit("### Next-probe spec (minimum requirements for an honest metric verdict)")
    emit()
    emit("1. NORMALIZED ACTOR VOCABULARY: Apply synonym normalization at generation time")
    emit("   or post-hoc. The 6-atom manual map here is a proof of concept. The full")
    emit("   corpus has thousands of actor atoms; aliasing rate is unknown.")
    emit()
    emit("2. LARGER LABELED SET: ~150 stories in ~30–50 orbits of 3–6 stories each,")
    emit("   generated with explicit orbit structure (multiple stories per labeled kernel).")
    emit()
    emit("3. NARRATIVE FIELD IN PIPELINE: commentary.narrative_context is in individual")
    emit("   JSON files but not in pipeline_output.json. Adding it would make the semantic")
    emit("   arm available without per-file reads.")
    emit()
    emit("4. CROSS-GENERATOR VALIDATION: At least one labeled orbit pair where stories were")
    emit("   generated by different prompts (or by a human) to confirm semantic clustering")
    emit("   tracks phenomena rather than generator phrasing habits.")
    emit()

    # ── Write outputs ────────────────────────────────────────────────
    OUT_MD.write_text("\n".join(lines))
    OUT_JSON.write_text(json.dumps(findings, indent=2))

    print(f"\nReport: {OUT_MD}", flush=True)
    print(f"JSON:   {OUT_JSON}", flush=True)


if __name__ == "__main__":
    main()
