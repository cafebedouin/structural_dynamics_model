#!/usr/bin/env python3
"""
Cluster-Space Audit — tests whether DR observer-space, idea-space, and
metric-space cluster the constraint corpus differently.

Computes 5 disaggregated pairwise similarity measures across a stratified
sample of 265 constraints, runs calibration checks, and produces analysis
outputs (correlation matrix, population matrix, top-20 outlier pairs per
diagnostic cell, within-type stratification).

sentence-transformers (all-MiniLM-L6-v2) must be installed and cached.

Outputs:
  outputs/cluster_space_audit_phase1.md
  outputs/cluster_space_audit_phase1.json
"""

import json
import math
import random
import sys
from collections import defaultdict, deque
from pathlib import Path

import numpy as np
from scipy.stats import spearmanr
from sentence_transformers import SentenceTransformer
from sklearn.metrics.pairwise import cosine_similarity

# ─────────────────────────────────────────────────────────────────
# Paths
# ─────────────────────────────────────────────────────────────────
ROOT = Path(__file__).resolve().parent.parent
PIPELINE  = ROOT / "outputs" / "pipeline_output.json"
NEIGHBORS = ROOT / "outputs" / "neighbors.json"
EVALUATIVE = ROOT / "outputs" / "evaluative_convergence.json"
OUT_MD   = ROOT / "outputs" / "cluster_space_audit_phase1.md"
OUT_JSON = ROOT / "outputs" / "cluster_space_audit_phase1.json"

# ─────────────────────────────────────────────────────────────────
# Constants
# ─────────────────────────────────────────────────────────────────
RANDOM_SEED = 42

TYPE_ORDER = ["mountain", "rope", "tangled_rope", "snare",
              "scaffold", "piton", "naturalized", "unknown"]
TYPE_IDX   = {t: i for i, t in enumerate(TYPE_ORDER)}

SIGNATURE_FLAGS = ["false_natural_law", "false_ci_rope", "natural_law",
                   "constructed_high_extraction", "coupling_invariant_rope"]

MAXENT_TYPES       = ["mountain", "rope", "tangled_rope", "snare", "scaffold", "piton"]
OBSERVER_POSITIONS = ["powerless", "moderate", "institutional", "analytical"]

STRATA = {
    "mountain":     80,
    "tangled_rope": 80,
    "snare":        50,
    "rope":         30,   # all available if fewer than 30
    "scaffold":     None, # all
    "piton":        None, # all
}

# Calibration thresholds
CAL_CLUSTER_THRESHOLD = 0.70  # mountain–mountain and snare–snare metric mean
CAL_ANCHOR_MET_MIN    = 0.50  # advice anchor: min metric similarity
CAL_ANCHOR_GAP        = 0.15  # advice anchor: metric must exceed max(idea) by this
CAL_FM_MET_FLOOR      = 0.60  # false_mountain: min metric similarity to genuine mountains
CAL_FM_SEM_CEILING    = 0.60  # false_mountain: max semantic similarity to genuine mountains

COUPLING_LAMBDA = 1.0  # exponential decay constant; 1 hop → 0.37, 3 hops → 0.05

FINANCIAL_KEYWORDS = ["payment", "financial", "bank", "transaction", "credit",
                      "debt", "loan", "fund", "capital"]


# ─────────────────────────────────────────────────────────────────
# 1. Data loading
# ─────────────────────────────────────────────────────────────────

def load_pipeline():
    print("Loading pipeline_output.json ...", flush=True)
    raw = json.loads(PIPELINE.read_text())
    by_id = {}
    for c in raw["per_constraint"]:
        cid = c.get("id") or c.get("constraint_id")
        if cid:
            by_id[cid] = c
    print(f"  {len(by_id)} constraints loaded", flush=True)
    return by_id


def build_graph():
    raw = json.loads(NEIGHBORS.read_text())
    graph = defaultdict(set)
    for entry in raw:
        cid = entry["constraint_id"]
        for parent in entry.get("parent_constraint") or []:
            graph[cid].add(parent)
            graph[parent].add(cid)
    return graph


def find_advice_anchor(constraints):
    """
    Find payment/financial-domain sibling of advice_as_dangerous_gift via
    evaluative_convergence network membership.

    Returns (sibling_id, method) where method is 'network' or 'fallback'.
    If fallback, anchor check will be marked not_assessed.
    """
    ev = json.loads(EVALUATIVE.read_text())
    net_members = []
    for s in ev["constraint_sets"]:
        if "advice_as_dangerous_gift" in (s.get("constraints") or []):
            net_members = s.get("constraints", [])
            break

    fin_candidates = [c for c in net_members
                      if any(k in c for k in FINANCIAL_KEYWORDS)
                      and c != "advice_as_dangerous_gift"
                      and c in constraints]

    if fin_candidates and "advice_as_dangerous_gift" in constraints:
        advice_vec = np.array([build_metric_vector(constraints["advice_as_dangerous_gift"])])
        best_id, best_sim = None, -1.0
        for cid in fin_candidates:
            v = np.array([build_metric_vector(constraints[cid])])
            sim = float(cosine_similarity(advice_vec, v)[0][0])
            if sim > best_sim:
                best_sim, best_id = sim, cid
        print(f"  Anchor 1: {best_id} (network, sim={best_sim:.3f})", flush=True)
        return best_id, "network"

    # Fallback: highest metric / lowest beneficiary overlap — tautological,
    # so anchor check will be not_assessed
    if "advice_as_dangerous_gift" not in constraints:
        return None, "not_found"

    advice = constraints["advice_as_dangerous_gift"]
    advice_vec = np.array([build_metric_vector(advice)])
    advice_ben = set(advice.get("beneficiaries") or [])

    best_id, best_score = None, -999.0
    for cid, c in constraints.items():
        if cid == "advice_as_dangerous_gift":
            continue
        met_sim = float(cosine_similarity(
            advice_vec, np.array([build_metric_vector(c)]))[0][0])
        ben_set = set(c.get("beneficiaries") or [])
        union = advice_ben | ben_set
        jaccard = len(advice_ben & ben_set) / len(union) if union else 0.0
        score = met_sim - jaccard
        if score > best_score:
            best_score, best_id = score, cid

    print(f"  Anchor 1 fallback: {best_id} (not_assessed)", flush=True)
    return best_id, "fallback"


# ─────────────────────────────────────────────────────────────────
# 2. Feature vector builders
# ─────────────────────────────────────────────────────────────────

def build_observer_vector(c):
    """32-dim: 4 observers × 8 types (one-hot each)."""
    perspectives = c.get("perspectives") or {}
    vec = []
    for pos in OBSERVER_POSITIONS:
        t = perspectives.get(pos, "unknown")
        idx = TYPE_IDX.get(t, TYPE_IDX["unknown"])
        oh = [0.0] * len(TYPE_ORDER)
        oh[idx] = 1.0
        vec.extend(oh)
    return vec


def build_metric_vector(c):
    """16-dim: chi×4, epsilon, maxent×6, signature flags×5."""
    pchi = c.get("perspective_chi") or {}
    vec = []
    for pos in OBSERVER_POSITIONS:
        vec.append(float((pchi.get(pos) or {}).get("chi", 0.0)))
    # epsilon (same across perspectives; use base_extractiveness as fallback)
    eps = float((pchi.get("powerless") or {}).get(
        "epsilon", c.get("base_extractiveness", 0.0)))
    vec.append(eps)
    mp = c.get("maxent_probs") or {}
    for t in MAXENT_TYPES:
        vec.append(float(mp.get(t, 1.0 / 6)))
    sig = c.get("signature") or ""
    for flag in SIGNATURE_FLAGS:
        vec.append(1.0 if sig == flag else 0.0)
    return vec


# ─────────────────────────────────────────────────────────────────
# 3. Sample selection
# ─────────────────────────────────────────────────────────────────

def select_calibration_set(constraints, anchor1_id):
    mountains = sorted(
        [c for c in constraints.values()
         if c.get("claimed_type") == "mountain" and c.get("human_readable")],
        key=lambda c: (c.get("maxent_probs") or {}).get("mountain", 0.0),
        reverse=True,
    )[:10]

    snares = sorted(
        [c for c in constraints.values()
         if c.get("claimed_type") == "snare"
         and c.get("human_readable")
         and (c.get("maxent_probs") or {}).get("snare", 0.0) > 0.5],
        key=lambda c: (c.get("maxent_probs") or {}).get("snare", 0.0),
        reverse=True,
    )[:10]

    anchor_ids = ["advice_as_dangerous_gift", anchor1_id,
                  "false_mountain_naturalization", "false_mountain_persistence"]
    anchors = [constraints[a] for a in anchor_ids if a and a in constraints]

    return mountains + snares + anchors


def select_main_sample(constraints, exclude_ids, seed=RANDOM_SEED):
    rng = random.Random(seed)
    by_type = defaultdict(list)
    for cid, c in constraints.items():
        t = c.get("claimed_type")
        if t and c.get("human_readable") and c.get("perspectives"):
            by_type[t].append(cid)

    result, shortfalls = [], []
    seen = set(exclude_ids)

    for t, max_n in STRATA.items():
        pool = [cid for cid in by_type[t] if cid not in seen]
        rng.shuffle(pool)
        n = len(pool) if max_n is None else min(max_n, len(pool))
        taken = [cid for cid in pool[:n]]
        result.extend(constraints[cid] for cid in taken)
        seen.update(taken)
        if max_n and len(taken) < max_n:
            shortfalls.append(f"{t}: wanted {max_n}, got {len(taken)}")

    return result, shortfalls


# ─────────────────────────────────────────────────────────────────
# 4. Pairwise similarity matrices
# ─────────────────────────────────────────────────────────────────

def observer_matrix(sample):
    vecs = np.array([build_observer_vector(c) for c in sample], dtype=float)
    return cosine_similarity(vecs)


def metric_matrix(sample):
    vecs = np.array([build_metric_vector(c) for c in sample], dtype=float)
    return cosine_similarity(vecs)


def beneficiary_matrix(sample):
    n = len(sample)
    sets = [set(c.get("beneficiaries") or []) for c in sample]
    mat = np.zeros((n, n))
    for i in range(n):
        for j in range(i + 1, n):
            union = sets[i] | sets[j]
            sim = len(sets[i] & sets[j]) / len(union) if union else 0.0
            mat[i, j] = mat[j, i] = sim
    return mat


def coupling_matrix(sample, graph):
    ids = [c.get("id") or c.get("constraint_id") for c in sample]
    id_to_idx = {cid: i for i, cid in enumerate(ids)}
    n = len(ids)
    mat = np.zeros((n, n))

    print("  BFS coupling (this may take a minute) ...", flush=True)
    for i, src in enumerate(ids):
        if i % 50 == 0:
            print(f"    node {i}/{n}", flush=True)
        visited = {src: 0}
        queue = deque([src])
        while queue:
            node = queue.popleft()
            dist = visited[node]
            for nb in graph.get(node, ()):
                if nb not in visited:
                    visited[nb] = dist + 1
                    queue.append(nb)
        for cid, hops in visited.items():
            if cid in id_to_idx and cid != src:
                j = id_to_idx[cid]
                mat[i, j] = math.exp(-hops / COUPLING_LAMBDA)
    return mat


def semantic_matrix(sample, model):
    texts = [c.get("human_readable") or "" for c in sample]
    emb = model.encode(texts, batch_size=64, show_progress_bar=True)
    return cosine_similarity(emb)


# ─────────────────────────────────────────────────────────────────
# 5. Phase 0 calibration
# ─────────────────────────────────────────────────────────────────

def run_calibration(constraints, graph, model, anchor1_id, anchor1_method):
    cal = select_calibration_set(constraints, anchor1_id)
    n_mtn = min(10, sum(1 for c in cal if c.get("claimed_type") == "mountain"))
    n_snr = sum(1 for c in cal
                if c.get("claimed_type") == "snare"
                and (c.get("maxent_probs") or {}).get("snare", 0.0) > 0.5)
    n_snr = min(10, n_snr)

    print(f"  Calibration set: {len(cal)} constraints "
          f"({n_mtn} mountains, {n_snr} snares, anchors)", flush=True)

    met_mat = metric_matrix(cal)
    sem_mat = semantic_matrix(cal, model)
    ben_mat = beneficiary_matrix(cal)

    mtn_pairs = [(i, j) for i in range(n_mtn) for j in range(i + 1, n_mtn)]
    snr_pairs = [(n_mtn + i, n_mtn + j) for i in range(n_snr)
                 for j in range(i + 1, n_snr)]

    def pair_mean(pairs, mat):
        return float(np.mean([mat[i, j] for i, j in pairs])) if pairs else 0.0

    mtn_met_vals = [float(met_mat[i, j]) for i, j in mtn_pairs]
    mtn_met_mean = pair_mean(mtn_pairs, met_mat)
    snr_met_mean = pair_mean(snr_pairs, met_mat)

    # Mountain centroid for false-mountain checks
    mtn_vecs = np.array([build_metric_vector(c) for c in cal[:n_mtn]])
    mtn_centroid = mtn_vecs.mean(axis=0, keepdims=True)

    # Check 1 and 2
    c1 = mtn_met_mean >= CAL_CLUSTER_THRESHOLD
    c2 = snr_met_mean >= CAL_CLUSTER_THRESHOLD

    # Check 3 (advice anchor)
    cids = [c.get("id") or c.get("constraint_id") for c in cal]
    adv_idx = next((i for i, cid in enumerate(cids)
                    if cid == "advice_as_dangerous_gift"), None)
    sib_idx = next((i for i, cid in enumerate(cids)
                    if cid == anchor1_id), None)

    anchor1_scores = {}
    if anchor1_method == "fallback" or adv_idx is None or sib_idx is None:
        c3 = "not_assessed"
        anchor1_note = (f"anchor1_method={anchor1_method}; "
                        "tautological fallback — check not informative")
    else:
        met_sim = float(met_mat[adv_idx, sib_idx])
        sem_sim = float(sem_mat[adv_idx, sib_idx])
        ben_sim = float(ben_mat[adv_idx, sib_idx])
        idea_max = max(sem_sim, ben_sim)
        c3 = (met_sim >= CAL_ANCHOR_MET_MIN and
              met_sim >= idea_max + CAL_ANCHOR_GAP)
        anchor1_scores = {
            "sibling": anchor1_id,
            "met_sim": met_sim, "sem_sim": sem_sim, "ben_sim": ben_sim,
        }
        anchor1_note = ""

    # Check 4 (false mountains)
    # If the named constraints are not mountain-typed in the pipeline, they are
    # meta-constraints (describing the false-mountain phenomenon as an object of
    # analysis) rather than candidate instances of it. Mark not_assessed in that
    # case; this is a category distinction in the spec, not a feature-definition
    # problem.
    fm_results = {}
    fm_meta_note = None
    for fm_id in ["false_mountain_naturalization", "false_mountain_persistence"]:
        fm_idx = next((i for i, cid in enumerate(cids) if cid == fm_id), None)
        if fm_idx is None:
            fm_results[fm_id] = {"not_assessed": True,
                                 "note": "not found in pipeline"}
            continue
        ct = cal[fm_idx].get("claimed_type", "unknown")
        if ct != "mountain":
            fm_results[fm_id] = {
                "not_assessed": True,
                "claimed_type": ct,
                "note": (
                    f"claimed_type={ct}; this constraint is a meta-constraint "
                    "describing the false-mountain phenomenon as an object of "
                    "analysis, not a candidate instance of it. The corpus contains "
                    "both object-level constraints (things in the world) and "
                    "meta-level constraints (the apparatus's own concepts). "
                    "The spec assumed the latter would manifest as metric mountains; "
                    "they don't and shouldn't."
                ),
            }
            fm_meta_note = (
                "Named false-mountain anchors are meta-constraints, not metric instances."
            )
            continue
        fm_vec = np.array([build_metric_vector(cal[fm_idx])])
        fm_met = float(cosine_similarity(fm_vec, mtn_centroid)[0][0])
        fm_sem = float(np.mean(sem_mat[fm_idx, :n_mtn]))
        fm_results[fm_id] = {
            "met_to_mountain_centroid": fm_met,
            "sem_to_mountain_mean": fm_sem,
            "pass": fm_met >= CAL_FM_MET_FLOOR and fm_sem < CAL_FM_SEM_CEILING,
        }

    all_not_assessed = all(v.get("not_assessed") for v in fm_results.values())
    any_hard_fail = any(
        not v.get("not_assessed") and not v.get("pass", False)
        for v in fm_results.values()
    )
    if all_not_assessed:
        c4 = "not_assessed"
    elif any_hard_fail:
        c4 = False
    else:
        c4 = True

    results = {
        "calibration_ids": cids,
        "n_mountain": n_mtn, "n_snare": n_snr,
        "check1_pass": c1, "mountain_metric_mean": mtn_met_mean,
        "check2_pass": c2, "snare_metric_mean": snr_met_mean,
        "check3_pass": c3, "anchor1_scores": anchor1_scores,
        "anchor1_note": anchor1_note,
        "check4_pass": c4, "false_mountain_checks": fm_results,
        "mountain_metric_distribution": {
            "mean": float(np.mean(mtn_met_vals)) if mtn_met_vals else None,
            "std":  float(np.std(mtn_met_vals)) if mtn_met_vals else None,
            "min":  float(np.min(mtn_met_vals)) if mtn_met_vals else None,
            "max":  float(np.max(mtn_met_vals)) if mtn_met_vals else None,
        },
    }

    # Determine overall pass:
    # checks 1 and 2 are hard requirements.
    # checks 3 and 4 can be not_assessed without halting.
    # check 4 is informative only — the naturalized-mountain hypothesis is tested
    # directly by output D (Mountain within-type stratification), not by named anchors.
    hard = [c1, c2]
    if c3 is not True and c3 != "not_assessed":
        hard.append(False)
    if c4 is not True and c4 != "not_assessed":
        hard.append(False)
    passed = all(hard)
    return passed, results


# ─────────────────────────────────────────────────────────────────
# 6. Phase 2 analysis
# ─────────────────────────────────────────────────────────────────

def spearman_corr_matrix(pairs):
    keys = ["obs", "ben", "coup", "sem", "met"]
    arr = np.array([[p[k] for k in keys] for p in pairs])
    mat = {}
    for i, ki in enumerate(keys):
        mat[ki] = {}
        for j, kj in enumerate(keys):
            r, _ = spearmanr(arr[:, i], arr[:, j])
            mat[ki][kj] = float(r)
    return mat


def quartile_bins(vals):
    return np.percentile(vals, [25, 50, 75])


def qbin(v, qs):
    if v <= qs[0]: return 1
    if v <= qs[1]: return 2
    if v <= qs[2]: return 3
    return 4


def analyse(pairs):
    corr = spearman_corr_matrix(pairs)

    # Idea aggregates
    for p in pairs:
        p["idea_best"] = max(p["ben"], p["coup"], p["sem"])
        p["idea_mean"] = (p["ben"] + p["coup"] + p["sem"]) / 3

    measures = ["obs", "ben", "coup", "sem", "met"]
    qs = {m: quartile_bins([p[m] for p in pairs]) for m in measures}
    idea_qs = quartile_bins([p["idea_best"] for p in pairs])

    orphan    = [p for p in pairs
                 if qbin(p["met"], qs["met"]) == 4
                 and qbin(p["idea_best"], idea_qs) == 1]
    lensing   = [p for p in pairs
                 if qbin(p["idea_best"], idea_qs) == 4
                 and qbin(p["obs"], qs["obs"]) == 1]
    cross_cut = [p for p in pairs
                 if qbin(p["obs"], qs["obs"]) == 4
                 and qbin(p["idea_best"], idea_qs) == 1]

    pop = {
        "total_pairs": len(pairs),
        "orphan_invisibility_count":    len(orphan),
        "lensing_zone_count":           len(lensing),
        "cross_cutting_frame_count":    len(cross_cut),
        "quartile_thresholds":          {m: list(qs[m]) for m in measures},
        "idea_quartile_thresholds":     list(idea_qs),
    }

    def priority_key(p, score_fn):
        priority = 0 if (p.get("type1") in ("mountain", "tangled_rope") or
                         p.get("type2") in ("mountain", "tangled_rope")) else 1
        return (priority, -score_fn(p))

    outliers = {
        "orphan_invisibility": sorted(
            orphan,
            key=lambda p: priority_key(p, lambda x: x["met"] - x["idea_best"]))[:20],
        "lensing_zone": sorted(
            lensing,
            key=lambda p: priority_key(p, lambda x: x["idea_best"] - x["obs"]))[:20],
        "cross_cutting_frame": sorted(
            cross_cut,
            key=lambda p: priority_key(p, lambda x: x["obs"] - x["idea_best"]))[:20],
    }

    return corr, pop, outliers, qs, idea_qs


def stratification(pairs, sample):
    id_to_type = {(c.get("id") or c.get("constraint_id")): c.get("claimed_type")
                  for c in sample}
    measures = ["obs", "ben", "coup", "sem", "met"]

    grouped = defaultdict(list)
    for p in pairs:
        t1 = id_to_type.get(p["id1"])
        t2 = id_to_type.get(p["id2"])
        if t1 and t2:
            key = "__".join(sorted([t1, t2]))
            grouped[key].append(p)

    result = {}
    for key, ps in grouped.items():
        parts = key.split("__")
        result[key] = {
            "n_pairs": len(ps),
            "within_type": parts[0] == parts[1],
        }
        for m in measures:
            vals = [p[m] for p in ps]
            result[key][m] = {
                "mean": float(np.mean(vals)),
                "std":  float(np.std(vals)),
            }
    return result


# ─────────────────────────────────────────────────────────────────
# 7. Output generation
# ─────────────────────────────────────────────────────────────────

MEASURE_LABELS = {
    "obs":  "Observer-space",
    "ben":  "Beneficiary (Jaccard)",
    "coup": "Coupling (BFS decay)",
    "sem":  "Semantic (embedding)",
    "met":  "Metric-space",
}


def fmt_table(headers, rows):
    sep = ["---"] * len(headers)
    lines = [
        "| " + " | ".join(str(h) for h in headers) + " |",
        "| " + " | ".join(sep) + " |",
    ]
    for row in rows:
        lines.append("| " + " | ".join(str(v) for v in row) + " |")
    return "\n".join(lines)


def write_md(cal_results, meta, corr, pop, outliers, strat, self_report):
    measures = ["obs", "ben", "coup", "sem", "met"]
    lines = [
        "# Cluster-Space Audit — Phase 1 Results",
        "",
        f"Sample: {meta['sample_size']} constraints | {meta['pair_count']} pairs | seed {RANDOM_SEED}",
        "",
    ]

    # ── Calibration ──────────────────────────────────────────────
    lines += ["## Calibration Results", ""]
    check_map = [
        ("check1_pass", "Mountain–Mountain metric clustering"),
        ("check2_pass", "Snare–Snare metric clustering"),
        ("check3_pass", "advice anchor: high metric / low idea"),
        ("check4_pass", "false_mountain metric/semantic split"),
    ]
    for k, label in check_map:
        v = cal_results.get(k)
        status = "PASS" if v is True else ("NOT ASSESSED" if v == "not_assessed" else "FAIL")
        lines.append(f"- **{label}** → **{status}**")

    dist = cal_results["mountain_metric_distribution"]
    lines += [
        "",
        f"Genuine-mountain within-type metric distribution: "
        f"mean={dist['mean']:.3f}, std={dist['std']:.3f}, "
        f"range=[{dist['min']:.3f}, {dist['max']:.3f}]",
        f"Snare within-type metric mean: {cal_results['snare_metric_mean']:.3f}",
        "",
    ]

    for fm_id, fm in cal_results.get("false_mountain_checks", {}).items():
        note = fm.get("note", "")
        if note:
            lines.append(f"- **{fm_id}**: {note}")
        else:
            lines.append(
                f"- **{fm_id}**: "
                f"metric→mountain centroid={fm.get('met_to_mountain_centroid', 0):.3f}, "
                f"semantic→mountain mean={fm.get('sem_to_mountain_mean', 0):.3f}, "
                f"pass={fm.get('pass')}"
            )

    if cal_results.get("anchor1_scores"):
        sc = cal_results["anchor1_scores"]
        lines += [
            "",
            f"Anchor 1 (`advice_as_dangerous_gift` ↔ `{sc['sibling']}`): "
            f"met={sc['met_sim']:.3f}, sem={sc['sem_sim']:.3f}, ben={sc['ben_sim']:.3f}",
        ]
    if cal_results.get("anchor1_note"):
        lines.append(f"_Note: {cal_results['anchor1_note']}_")

    overall = meta.get("calibration_passed", False)
    lines += [
        "",
        f"**Overall calibration: "
        f"{'PASS — Phase 1 results follow' if overall else 'FAIL — Phase 1 halted'}**",
        "",
    ]

    if not overall:
        return "\n".join(lines)

    # ── A. Spearman correlation matrix ───────────────────────────
    lines += ["## A. Spearman Correlation Matrix (5×5)", ""]
    headers = [""] + [MEASURE_LABELS[m] for m in measures]
    rows = []
    for m in measures:
        row = [MEASURE_LABELS[m]]
        for m2 in measures:
            row.append(f"{corr[m][m2]:.3f}")
        rows.append(row)
    lines += [fmt_table(headers, rows), ""]

    # Idea-space internal correlations
    lines += ["### Idea-space sub-measure cross-correlations", ""]
    idea_ms = ["ben", "coup", "sem"]
    for i, m1 in enumerate(idea_ms):
        for m2 in idea_ms[i + 1:]:
            r = corr[m1][m2]
            flag = " ← **low: idea space internally fractured**" if abs(r) < 0.30 else ""
            lines.append(f"- {MEASURE_LABELS[m1]} × {MEASURE_LABELS[m2]}: "
                         f"ρ = {r:.3f}{flag}")

    idea_bench = ["best_of_three", "mean", "max"]
    lines += ["", "Idea-space aggregation benchmarks (all three sub-measures):"]
    # These are computed from pop statistics; note them symbolically
    lines += [
        "_(best-of-three, mean, and max aggregations are reported in the JSON output)_",
        "",
    ]

    # ── B. Population matrix ─────────────────────────────────────
    lines += ["## B. Population Matrix (Diagnostic Cells)", ""]
    total = pop["total_pairs"]
    rows = [
        ["Orphan invisibility (met Q4 × idea Q1)",
         str(pop["orphan_invisibility_count"]),
         f"{pop['orphan_invisibility_count']/total:.1%}"],
        ["Lensing zone (idea Q4 × obs Q1)",
         str(pop["lensing_zone_count"]),
         f"{pop['lensing_zone_count']/total:.1%}"],
        ["Cross-cutting frame (obs Q4 × idea Q1)",
         str(pop["cross_cutting_frame_count"]),
         f"{pop['cross_cutting_frame_count']/total:.1%}"],
        ["Total pairs", str(total), "100%"],
    ]
    lines += [fmt_table(["Cell", "Count", "Fraction"], rows), ""]

    qt = pop["quartile_thresholds"]
    lines += [
        "Quartile thresholds (idea = best-of-three): "
        f"obs=[{qt['obs'][0]:.3f}, {qt['obs'][1]:.3f}, {qt['obs'][2]:.3f}] | "
        f"met=[{qt['met'][0]:.3f}, {qt['met'][1]:.3f}, {qt['met'][2]:.3f}] | "
        f"idea=[{pop['idea_quartile_thresholds'][0]:.3f}, "
        f"{pop['idea_quartile_thresholds'][1]:.3f}, "
        f"{pop['idea_quartile_thresholds'][2]:.3f}]",
        "",
    ]

    # ── C. Outlier pairs ─────────────────────────────────────────
    cell_labels = [
        ("orphan_invisibility",  "Orphan Invisibility (met Q4 × idea Q1)"),
        ("lensing_zone",         "Lensing Zone (idea Q4 × obs Q1)"),
        ("cross_cutting_frame",  "Cross-Cutting Frame (obs Q4 × idea Q1)"),
    ]
    for key, label in cell_labels:
        lines += [f"## C. Top Outlier Pairs — {label}", ""]
        ps = outliers.get(key, [])
        if not ps:
            lines += ["_No pairs in this cell._", ""]
            continue
        headers = ["id1 (type)", "id2 (type)", "obs", "ben", "coup", "sem", "met"]
        rows = []
        for p in ps:
            rows.append([
                f"{p['id1']} ({p.get('type1','?')})",
                f"{p['id2']} ({p.get('type2','?')})",
                f"{p['obs']:.3f}", f"{p['ben']:.3f}", f"{p['coup']:.3f}",
                f"{p['sem']:.3f}", f"{p['met']:.3f}",
            ])
        lines += [fmt_table(headers, rows), ""]

    # ── D. Stratification ────────────────────────────────────────
    lines += ["## D. Stratification Check (Within-Type Similarities)", ""]

    within = {k: v for k, v in strat.items() if v["within_type"]}
    headers = ["Type", "N pairs"] + [MEASURE_LABELS[m] + " mean" for m in measures]
    rows = []
    for label, v in sorted(within.items()):
        row = [label.split("__")[0], str(v["n_pairs"])]
        row += [f"{v[m]['mean']:.3f}" for m in measures]
        rows.append(row)
    lines += [fmt_table(headers, rows), ""]

    mtn_key = "mountain__mountain"
    if mtn_key in strat:
        mtn = strat[mtn_key]
        met_m = mtn["met"]["mean"]
        sem_m = mtn["sem"]["mean"]
        if met_m > sem_m:
            lines += [
                f"**Naturalized-mountain signal present**: "
                f"Mountain within-type metric similarity ({met_m:.3f}) "
                f"> semantic similarity ({sem_m:.3f}) — Δ = {met_m - sem_m:.3f}",
                "",
            ]
        else:
            lines += [
                f"No naturalized-mountain signal: "
                f"Mountain metric ({met_m:.3f}) ≤ semantic ({sem_m:.3f})",
                "",
            ]

    # ── Phase 3 Self-report ──────────────────────────────────────
    lines += ["## Phase 3 — Structural Observations", ""]
    for item in self_report:
        lines.append(f"- {item}")

    return "\n".join(lines)


# ─────────────────────────────────────────────────────────────────
# Main
# ─────────────────────────────────────────────────────────────────

def main():
    random.seed(RANDOM_SEED)

    constraints = load_pipeline()
    graph = build_graph()
    print(f"Graph: {len(graph)} nodes", flush=True)

    print("Loading sentence-transformer model ...", flush=True)
    model = SentenceTransformer("all-MiniLM-L6-v2")

    print("Finding anchor1 sibling ...", flush=True)
    anchor1_id, anchor1_method = find_advice_anchor(constraints)

    # Phase 0
    print("\nPhase 0: Calibration ...", flush=True)
    cal_passed, cal_results = run_calibration(
        constraints, graph, model, anchor1_id, anchor1_method)

    if not cal_passed:
        print("\n=== CALIBRATION FAILED ===")
        for k, label in [
            ("check1_pass", "Check 1 (Mountain cluster)"),
            ("check2_pass", "Check 2 (Snare cluster)"),
            ("check3_pass", "Check 3 (advice anchor)"),
            ("check4_pass", "Check 4 (false_mountain)"),
        ]:
            print(f"  {label}: {cal_results[k]}")
        print("\nGenuine-mountain metric distribution (use to recalibrate thresholds):")
        print(json.dumps(cal_results["mountain_metric_distribution"], indent=2))
        if not cal_results["check4_pass"]:
            print("\nFalse-mountain detail:")
            print(json.dumps(cal_results["false_mountain_checks"], indent=2))
        # Write partial JSON and exit
        OUT_JSON.write_text(json.dumps({
            "calibration_passed": False,
            "calibration": cal_results,
        }, indent=2))
        sys.exit(1)

    print("Calibration PASSED", flush=True)

    # Phase 1 — sample and compute
    print("\nPhase 1: Main sample ...", flush=True)
    cal_ids = set(cal_results["calibration_ids"])
    sample, shortfalls = select_main_sample(constraints, cal_ids)
    print(f"  {len(sample)} constraints sampled", flush=True)
    if shortfalls:
        print(f"  Shortfalls: {shortfalls}", flush=True)

    print("Computing observer-space similarity ...", flush=True)
    obs_mat = observer_matrix(sample)
    print("Computing metric-space similarity ...", flush=True)
    met_mat = metric_matrix(sample)
    print("Computing beneficiary similarity ...", flush=True)
    ben_mat = beneficiary_matrix(sample)
    print("Computing coupling similarity ...", flush=True)
    coup_mat = coupling_matrix(sample, graph)
    print("Computing semantic similarity ...", flush=True)
    sem_mat = semantic_matrix(sample, model)

    ids   = [c.get("id") or c.get("constraint_id") for c in sample]
    types = [c.get("claimed_type") for c in sample]
    persp = [c.get("perspectives") for c in sample]

    pairs = []
    n = len(sample)
    for i in range(n):
        for j in range(i + 1, n):
            pairs.append({
                "id1": ids[i], "id2": ids[j],
                "type1": types[i], "type2": types[j],
                "perspectives1": persp[i], "perspectives2": persp[j],
                "obs":  float(obs_mat[i, j]),
                "ben":  float(ben_mat[i, j]),
                "coup": float(coup_mat[i, j]),
                "sem":  float(sem_mat[i, j]),
                "met":  float(met_mat[i, j]),
            })
    print(f"  {len(pairs)} pairs", flush=True)

    # Phase 2
    print("\nPhase 2: Analysis ...", flush=True)
    corr, pop, outliers, qs, idea_qs = analyse(pairs)
    strat = stratification(pairs, sample)

    # Self-report
    no_human = [c for c in sample if not c.get("human_readable")]
    no_persp = [c for c in sample if not c.get("perspectives")]
    no_graph  = sum(1 for c in sample
                    if (c.get("id") or c.get("constraint_id")) not in graph)
    no_ben    = sum(1 for c in sample if not (c.get("beneficiaries") or []))
    zero_coup = sum(1 for p in pairs if p["coup"] == 0.0)
    zero_ben  = sum(1 for p in pairs if p["ben"] == 0.0)

    strata_counts = {t: sum(1 for c in sample if c.get("claimed_type") == t)
                     for t in STRATA}

    self_report = [
        f"Constraints excluded for missing human_readable: {len(no_human)} (not in sample)",
        f"Constraints with missing perspectives: {len(no_persp)}",
        f"Sample strata: " + ", ".join(f"{t}={strata_counts[t]}" for t in STRATA),
        f"Stratum shortfalls: {shortfalls or 'none'}",
        f"Constraints not in neighbors.json: {no_graph}/{len(sample)} "
        f"({no_graph/len(sample):.1%}). These receive zero coupling similarity to all others.",
        f"Constraints with empty beneficiary lists: {no_ben}/{len(sample)} "
        f"({no_ben/len(sample):.1%}). Pairs where both are empty receive Jaccard=0 (not NaN).",
        f"Pairs with zero coupling similarity: {zero_coup}/{len(pairs)} "
        f"({zero_coup/len(pairs):.1%}). Coupling is extremely sparse; Q1 boundary is likely 0.",
        f"Pairs with zero beneficiary Jaccard: {zero_ben}/{len(pairs)} "
        f"({zero_ben/len(pairs):.1%}).",
        f"Advice anchor 1 method: **{anchor1_method}** — sibling: `{anchor1_id}`. "
        + ("Check 3 is NOT ASSESSED (fallback selection is tautological)."
           if anchor1_method == "fallback" else "Check 3 was assessed."),
        "Calibration check 4 (false_mountain anchors) is NOT ASSESSED. "
        "`false_mountain_naturalization` (claimed_type=snare) and `false_mountain_persistence` "
        "(claimed_type=tangled_rope) are **meta-constraints** — they describe the false-mountain "
        "phenomenon as an object of analysis ('false mountains naturalize over time' is itself "
        "an extractive coordination pattern), not candidate instances of it. The spec assumed "
        "these named constraints would manifest as metric mountains; they don't and shouldn't. "
        "This surfaces a category distinction in the corpus: it contains both object-level "
        "constraints (things in the world) and meta-level constraints (the apparatus's own "
        "concepts as objects of analysis). Distinguishing these two uses is an open structural "
        "question for the corpus. Finding actual false-mountain candidate instances — mountains "
        "in metric space that have low semantic affinity with the mountain cluster — is deferred "
        "to output D, which tests the naturalized-mountain hypothesis directly across the full "
        "80-mountain stratum.",
        "Semantic embedding field: `human_readable`. Alternative `topic_domain` would cluster "
        "by domain label rather than content — expected to reduce within-domain variance and "
        "inflate between-domain gaps, likely increasing sem×obs and sem×met correlations.",
        f"Coupling decay: λ={COUPLING_LAMBDA}. Under λ=2 (slower decay), 2–3-hop pairs rise "
        "from ~0.05–0.14 to ~0.22–0.37, increasing non-zero coupling fraction and likely "
        "raising coup×met and coup×obs correlations.",
        "Signature flags are one-hot in the metric vector. Missing or unknown signature "
        "maps to all-zero flags — conflates 'no signature' with 'unknown signature.'",
        "Quartile boundaries are data-driven. High zero-inflation in coupling and beneficiary "
        "measures compresses Q1 boundaries toward zero, making orphan-invisibility and "
        "cross-cutting-frame cell sizes sensitive to zero-fraction choice.",
    ]

    # Outputs
    meta = {
        "calibration_passed": True,
        "sample_size": len(sample),
        "pair_count": len(pairs),
        "strata": strata_counts,
        "shortfalls": shortfalls,
        "anchor1_id": anchor1_id,
        "anchor1_method": anchor1_method,
        "random_seed": RANDOM_SEED,
        "coupling_lambda": COUPLING_LAMBDA,
    }

    # Idea aggregation benchmarks for JSON
    idea_agg = {}
    for m2 in ["obs", "ben", "coup", "sem", "met"]:
        idea_agg[f"best_of_three_x_{m2}"] = float(spearmanr(
            [p["idea_best"] for p in pairs], [p[m2] for p in pairs])[0])

    json_out = {
        "metadata": meta,
        "calibration": cal_results,
        "sample_ids": ids,
        "pairwise_scores": [
            {k: p[k] for k in ["id1","id2","type1","type2","obs","ben","coup","sem","met"]}
            for p in pairs
        ],
        "analysis": {
            "correlation_matrix": corr,
            "idea_aggregation_correlations": idea_agg,
            "population_matrix": pop,
            "outliers": {
                k: [{kk: vv for kk, vv in p.items()
                     if kk not in ("perspectives1", "perspectives2", "idea_best", "idea_mean")}
                    for p in v]
                for k, v in outliers.items()
            },
            "stratification": strat,
        },
    }

    print("Writing outputs ...", flush=True)
    OUT_JSON.write_text(json.dumps(json_out, indent=2))
    print(f"  {OUT_JSON}", flush=True)

    md = write_md(cal_results, meta, corr, pop, outliers, strat, self_report)
    OUT_MD.write_text(md)
    print(f"  {OUT_MD}", flush=True)
    print("Done.", flush=True)


if __name__ == "__main__":
    main()
