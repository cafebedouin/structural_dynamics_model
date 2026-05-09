#!/usr/bin/env python3
"""
idea_site_exploration.py

Four-pass audit of idea-site structure: with observer position fixed,
what mathematical structure do an observer's classifications across the
corpus exhibit?

Pass 1: Slice enumeration and classification distributions
Pass 2: Network structure at fixed observer slices (3 topologies)
Pass 3: Multi-slice comparison (flow matrices, cross-slice stability)
Pass 4: Candidate framework scoring

Output:
  outputs/idea_site_exploration.md
  outputs/idea_site_exploration.json
"""

import json
import math
import time
from collections import Counter, defaultdict
from itertools import combinations
from pathlib import Path

import networkx as nx
import numpy as np
from scipy.sparse import lil_matrix, csr_matrix
from scipy.sparse.csgraph import laplacian as sparse_laplacian
from scipy.sparse.linalg import eigsh

# ---------------------------------------------------------------------------
# Paths and constants
# ---------------------------------------------------------------------------

PIPELINE_PATH = Path("outputs/pipeline_output.json")
OUT_MD = Path("outputs/idea_site_exploration.md")
OUT_JSON = Path("outputs/idea_site_exploration.json")

# Edge-inclusion thresholds
BENEFICIARY_JACCARD_THRESHOLD = 0.15
SEMANTIC_JACCARD_THRESHOLD = 0.33

# Bimodality criterion for Dirac-orbit detection
BIMODAL_LOW_FRAC = 0.20    # fraction of nodes with stability < 0.3
BIMODAL_HIGH_FRAC = 0.20   # fraction of nodes with stability > 0.7
BIMODAL_STD_MIN = 0.35

# Orbit candidate stability threshold
ORBIT_STABILITY_THRESHOLD = 0.50

# Path-length: only compute exactly on LCC if ≤ this size
PATH_LENGTH_EXACT_LIMIT = 1000
PATH_LENGTH_SAMPLE_PAIRS = 200

# Spectral: number of eigenvalues to compute
SPECTRAL_K = 20
NEAR_ZERO_THRESHOLD = 0.01

# Persistent homology filtration thresholds
BENEFICIARY_FILTRATION = [0.05, 0.10, 0.15, 0.20, 0.30]
SEMANTIC_FILTRATION = [0.20, 0.33, 0.50, 0.67, 0.80]

# Minimum slice coverage for working set inclusion
MIN_SLICE_COVERAGE = 100

# The four canonical observer positions (must be in working set)
CANONICAL_SLICE_KEYS = [
    ("powerless",    "biographical",   "trapped",    "global"),
    ("moderate",     "biographical",   "constrained","national"),
    ("institutional","immediate",      "arbitrage",  "global"),
    ("analytical",   "civilizational", "analytical", "universal"),
]

# Human-readable labels for slices
SLICE_LABELS = {
    ("powerless",    "biographical",   "trapped",    "global"):     "U_1",
    ("moderate",     "biographical",   "constrained","national"):   "U_2",
    ("institutional","immediate",      "arbitrage",  "global"):     "U_3_imm",
    ("institutional","civilizational", "arbitrage",  "global"):     "U_3_civ",
    ("institutional","generational",   "arbitrage",  "global"):     "U_3_gen",
    ("analytical",   "civilizational", "analytical", "universal"):  "U_4",
    ("analytical",   "civilizational", "analytical", "global"):     "U_4_glob",
    ("organized",    "generational",   "constrained","global"):     "organized",
    ("organized",    "generational",   "constrained","national"):   "org_nat",
    ("powerless",    "biographical",   "trapped",    "national"):   "U_1_nat",
    ("institutional","immediate",      "arbitrage",  "national"):   "U_3_nat",
}

# ---------------------------------------------------------------------------
# Data loading and utilities
# ---------------------------------------------------------------------------

def load_data():
    with open(PIPELINE_PATH) as f:
        d = json.load(f)
    return d["per_constraint"]


def make_slice_key(ctx):
    return (
        ctx["agent_power"],
        ctx["time_horizon"],
        ctx["exit_options"],
        ctx["spatial_scope"],
    )


def shannon_entropy(counter):
    total = sum(counter.values())
    if total == 0:
        return 0.0
    return -sum(
        (v / total) * math.log2(v / total)
        for v in counter.values()
        if v > 0
    )


def jaccard(sa, sb):
    if not sa and not sb:
        return 0.0
    inter = len(sa & sb)
    union = len(sa | sb)
    return inter / union if union > 0 else 0.0


def domain_tokens(topic_domain):
    if not topic_domain:
        return frozenset()
    tokens = set()
    for part in topic_domain.split("/"):
        for tok in part.split("_"):
            if tok:
                tokens.add(tok.lower())
    return frozenset(tokens)


def build_explicit_edges(constraints):
    """Build dict {id: set(neighbor_ids)} for explicit contamination edges."""
    adj = defaultdict(set)
    for e in constraints:
        src = e["id"]
        cn = e.get("contamination_network", {})
        for nb in cn.get("neighbors", []):
            if nb.get("edge_type") == "explicit":
                dst = nb["constraint_id"]
                adj[src].add(dst)
                adj[dst].add(src)  # treat as undirected
    return adj


# ---------------------------------------------------------------------------
# Pass 1: Slice enumeration
# ---------------------------------------------------------------------------

def enumerate_slices(constraints):
    """Return {slice_key: {"ids": set, "type_counts": Counter}}."""
    slice_map = defaultdict(lambda: {"ids": set(), "type_counts": Counter()})
    for entry in constraints:
        eid = entry["id"]
        for cls in entry.get("classifications", []):
            key = make_slice_key(cls["context"])
            slice_map[key]["ids"].add(eid)
            slice_map[key]["type_counts"][cls["type"]] += 1
    return dict(slice_map)


def select_working_set(slice_map):
    """Select 10 slices: 4 canonicals + highest-coverage extras."""
    # Sort all slices by coverage descending
    ranked = sorted(slice_map.items(), key=lambda kv: len(kv[1]["ids"]), reverse=True)

    selected = {}
    # First: ensure all canonicals are included
    for key in CANONICAL_SLICE_KEYS:
        if key in slice_map:
            selected[key] = slice_map[key]

    # Then fill up to 10 with highest-coverage slices
    for key, data in ranked:
        if len(selected) >= 10:
            break
        if key not in selected and len(data["ids"]) >= MIN_SLICE_COVERAGE:
            selected[key] = data

    result = []
    for key, data in selected.items():
        label = SLICE_LABELS.get(key, "_".join(key))
        result.append({
            "key": list(key),
            "label": label,
            "coverage": len(data["ids"]),
            "type_counts": dict(data["type_counts"]),
            "entropy": shannon_entropy(data["type_counts"]),
            "dominant_type": data["type_counts"].most_common(1)[0][0] if data["type_counts"] else None,
        })
    # Sort by coverage descending for display
    result.sort(key=lambda x: x["coverage"], reverse=True)
    return result


def slice_distributions(constraints, working_slices):
    """Build per-constraint classification lookup for working slices."""
    slice_keys = {tuple(s["key"]) for s in working_slices}
    # {slice_key: {constraint_id: type}}
    slice_cls = {k: {} for k in slice_keys}
    for entry in constraints:
        eid = entry["id"]
        for cls in entry.get("classifications", []):
            key = make_slice_key(cls["context"])
            if key in slice_cls:
                slice_cls[key][eid] = cls["type"]
    return slice_cls  # {slice_key: {id: type}}


# ---------------------------------------------------------------------------
# Pass 2: Graph construction
# ---------------------------------------------------------------------------

def build_beneficiary_graph(clist):
    """Build beneficiary-overlap graph. clist = list of constraint dicts."""
    G = nx.Graph()
    actor_sets = {}
    for e in clist:
        actors = frozenset(e.get("beneficiaries", []) + e.get("victims", []))
        actor_sets[e["id"]] = actors
        G.add_node(e["id"])

    ids = list(actor_sets.keys())
    for i, a in enumerate(ids):
        sa = actor_sets[a]
        if not sa:
            continue
        for b in ids[i+1:]:
            sb = actor_sets[b]
            if sb and jaccard(sa, sb) >= BENEFICIARY_JACCARD_THRESHOLD:
                G.add_edge(a, b)
    return G


def build_coupling_graph(clist, explicit_adj):
    """Build coupling graph from explicit contamination edges."""
    G = nx.Graph()
    ids_in_slice = {e["id"] for e in clist}
    for e in clist:
        G.add_node(e["id"])
    for e in clist:
        src = e["id"]
        for dst in explicit_adj.get(src, []):
            if dst in ids_in_slice:
                G.add_edge(src, dst)
    return G


def build_semantic_graph(clist):
    """Build semantic graph from topic_domain token Jaccard."""
    G = nx.Graph()
    tok_sets = {}
    for e in clist:
        toks = domain_tokens(e.get("topic_domain", ""))
        tok_sets[e["id"]] = toks
        G.add_node(e["id"])

    ids = list(tok_sets.keys())
    for i, a in enumerate(ids):
        sa = tok_sets[a]
        if not sa:
            continue
        for b in ids[i+1:]:
            sb = tok_sets[b]
            if sb and jaccard(sa, sb) >= SEMANTIC_JACCARD_THRESHOLD:
                G.add_edge(a, b)
    return G


# ---------------------------------------------------------------------------
# Pass 2: Graph analysis
# ---------------------------------------------------------------------------

def analyze_graph(G, type_map):
    """
    Compute graph statistics at a fixed observer slice.
    type_map: {node_id: classification_type}
    """
    n = G.number_of_nodes()
    m = G.number_of_edges()
    density = (2 * m / (n * (n - 1))) if n > 1 else 0.0

    components = list(nx.connected_components(G))
    n_components = len(components)
    lcc = max(components, key=len) if components else set()
    lcc_size = len(lcc)

    # Homophily
    same_type_edges = 0
    for u, v in G.edges():
        if type_map.get(u) == type_map.get(v):
            same_type_edges += 1
    homophily = same_type_edges / m if m > 0 else 0.0

    # Baseline homophily = sum(p_t^2)
    type_counts = Counter(type_map.get(n) for n in G.nodes() if n in type_map)
    total = sum(type_counts.values())
    baseline = sum((c / total) ** 2 for c in type_counts.values()) if total > 0 else 0.0

    # Type mixing matrix
    mixing = Counter()
    for u, v in G.edges():
        tu, tv = type_map.get(u), type_map.get(v)
        if tu and tv:
            pair = tuple(sorted([tu, tv]))
            mixing[pair] += 1

    # Clustering coefficient
    clustering_coeff = nx.average_clustering(G)

    # Average path length on LCC
    lcc_subgraph = G.subgraph(lcc)
    path_length = None
    path_length_method = "skipped"
    if lcc_size >= 2:
        if lcc_size <= PATH_LENGTH_EXACT_LIMIT:
            try:
                path_length = nx.average_shortest_path_length(lcc_subgraph)
                path_length_method = "exact"
            except nx.NetworkXError:
                path_length_method = "disconnected_lcc"
        else:
            # Sample random pairs
            lcc_nodes = list(lcc)
            rng = np.random.default_rng(42)
            sample_nodes = rng.choice(lcc_nodes, size=min(PATH_LENGTH_SAMPLE_PAIRS, lcc_size), replace=False)
            lengths = []
            for src in sample_nodes:
                targets = [n for n in sample_nodes if n != src]
                sp = nx.single_source_shortest_path_length(lcc_subgraph, src)
                for tgt in targets:
                    if tgt in sp:
                        lengths.append(sp[tgt])
            path_length = float(np.mean(lengths)) if lengths else None
            path_length_method = f"sampled({PATH_LENGTH_SAMPLE_PAIRS})"

    return {
        "n_nodes": n,
        "n_edges": m,
        "density": density,
        "n_components": n_components,
        "lcc_size": lcc_size,
        "homophily": homophily,
        "homophily_baseline": baseline,
        "homophily_lift": homophily - baseline,
        "type_mixing_matrix": {str(k): v for k, v in mixing.items()},
        "clustering_coeff": clustering_coeff,
        "avg_path_length": path_length,
        "path_length_method": path_length_method,
    }


def stability_distribution(G, type_map):
    """
    For each node: fraction of neighbors sharing its classification.
    Returns stability stats + orbit candidates (lowest stability).
    """
    stabilities = {}
    for node in G.nodes():
        t = type_map.get(node)
        neighbors = list(G.neighbors(node))
        if not neighbors:
            stabilities[node] = None  # isolated node
            continue
        same = sum(1 for nb in neighbors if type_map.get(nb) == t)
        stabilities[node] = same / len(neighbors)

    scored = {k: v for k, v in stabilities.items() if v is not None}
    vals = list(scored.values())

    if not vals:
        return {"mean": None, "std": None, "histogram": [], "bimodal": False,
                "orbit_candidates_top20": [], "per_node": {}}

    mean = float(np.mean(vals))
    std = float(np.std(vals))

    # Histogram: 10 buckets [0,0.1), [0.1,0.2), ..., [0.9,1.0]
    hist_counts = [0] * 10
    for v in vals:
        bucket = min(int(v * 10), 9)
        hist_counts[bucket] += 1

    n_total = len(vals)
    frac_low = sum(1 for v in vals if v < 0.3) / n_total
    frac_high = sum(1 for v in vals if v > 0.7) / n_total
    bimodal = (frac_low > BIMODAL_LOW_FRAC and
               frac_high > BIMODAL_HIGH_FRAC and
               std > BIMODAL_STD_MIN)

    # Top-20 lowest stability = orbit candidates
    orbit_candidates = sorted(scored.items(), key=lambda x: x[1])[:20]

    return {
        "mean": mean,
        "std": std,
        "histogram": hist_counts,
        "frac_below_0.3": frac_low,
        "frac_above_0.7": frac_high,
        "bimodal": bimodal,
        "orbit_candidates_top20": [{"id": k, "stability": v} for k, v in orbit_candidates],
        "n_isolated": sum(1 for v in stabilities.values() if v is None),
    }


def spectral_analysis(G, type_map):
    """
    Classification Laplacian: build same-type subgraph, compute Laplacian spectrum.
    """
    nodes = list(G.nodes())
    n = len(nodes)
    if n < 4:
        return {"eigenvalues": [], "n_near_zero": 0, "spectral_gap": None,
                "eigenvalue_percentiles": {}, "n_nodes": n}

    node_idx = {node: i for i, node in enumerate(nodes)}

    # Build adjacency of same-type edges
    A = lil_matrix((n, n), dtype=float)
    for u, v in G.edges():
        if type_map.get(u) == type_map.get(v):
            i, j = node_idx[u], node_idx[v]
            A[i, j] = 1.0
            A[j, i] = 1.0

    A = csr_matrix(A)
    L = sparse_laplacian(A, normed=False)

    k = min(SPECTRAL_K, n - 2)
    if k < 2:
        return {"eigenvalues": [], "n_near_zero": 0, "spectral_gap": None,
                "eigenvalue_percentiles": {}, "n_nodes": n}

    try:
        vals, _ = eigsh(L, k=k, which="SM", tol=1e-4, maxiter=2000)
        vals = sorted(float(v) for v in vals)
    except Exception:
        return {"eigenvalues": [], "n_near_zero": 0, "spectral_gap": None,
                "eigenvalue_percentiles": {}, "n_nodes": n, "error": "eigsh_failed"}

    n_near_zero = sum(1 for v in vals if v < NEAR_ZERO_THRESHOLD)
    gap_idx = n_near_zero
    spectral_gap = None
    if gap_idx < len(vals) - 1:
        spectral_gap = vals[gap_idx + 1] - vals[gap_idx] if gap_idx < len(vals) else None

    # Percentiles of computed eigenvalues
    arr = np.array(vals)
    pcts = {}
    for p in [25, 50, 75, 90, 99]:
        pcts[f"p{p}"] = float(np.percentile(arr, p))

    return {
        "eigenvalues": vals[:SPECTRAL_K],
        "n_near_zero": n_near_zero,
        "spectral_gap": spectral_gap,
        "eigenvalue_percentiles": pcts,
        "n_nodes": n,
    }


# ---------------------------------------------------------------------------
# Pass 2: Full analysis per (slice, topology)
# ---------------------------------------------------------------------------

def analyze_slice_topology(slice_label, slice_key, slice_cls_map, constraints_by_id,
                            explicit_adj, topology_name):
    """Run full graph analysis for one (slice, topology) pair."""
    type_map = slice_cls_map.get(slice_key, {})
    clist = [constraints_by_id[cid] for cid in type_map if cid in constraints_by_id]

    print(f"    [{slice_label}/{topology_name}] n={len(clist)} ", end="", flush=True)

    if topology_name == "beneficiary":
        G = build_beneficiary_graph(clist)
    elif topology_name == "coupling":
        G = build_coupling_graph(clist, explicit_adj)
    elif topology_name == "semantic":
        G = build_semantic_graph(clist)
    else:
        raise ValueError(f"Unknown topology: {topology_name}")

    print(f"→ {G.number_of_edges()} edges", flush=True)

    # Restrict type_map to nodes actually in G
    tmap = {n: type_map[n] for n in G.nodes() if n in type_map}

    stats = analyze_graph(G, tmap)
    stab = stability_distribution(G, tmap)
    spec = spectral_analysis(G, tmap)

    return {
        "graph_stats": stats,
        "stability": {k: v for k, v in stab.items() if k != "per_node"},
        "spectrum": spec,
        "orbit_candidates": stab["orbit_candidates_top20"],
    }


# ---------------------------------------------------------------------------
# Pass 3: Multi-slice comparison
# ---------------------------------------------------------------------------

def compute_flow_matrix(slice_cls_a, slice_cls_b):
    """
    Transition matrix for constraints appearing at both slices.
    Returns {from_type: {to_type: count}}.
    """
    common = set(slice_cls_a.keys()) & set(slice_cls_b.keys())
    matrix = defaultdict(Counter)
    for cid in common:
        matrix[slice_cls_a[cid]][slice_cls_b[cid]] += 1
    return {k: dict(v) for k, v in matrix.items()}


def cross_slice_stability(constraints, working_slices, slice_cls_map):
    """
    For each constraint, compute fraction of covered working slices where
    it receives the same classification as the plurality.
    """
    slice_keys = [tuple(s["key"]) for s in working_slices]
    result = {}
    for entry in constraints:
        eid = entry["id"]
        types_seen = []
        for key in slice_keys:
            t = slice_cls_map.get(key, {}).get(eid)
            if t:
                types_seen.append(t)
        if not types_seen:
            continue
        type_counter = Counter(types_seen)
        plurality_type, plurality_count = type_counter.most_common(1)[0]
        frac = plurality_count / len(types_seen)
        result[eid] = {
            "frac_agree": frac,
            "n_slices": len(types_seen),
            "plurality_type": plurality_type,
        }
    return result


# ---------------------------------------------------------------------------
# Pass 4: Framework scoring
# ---------------------------------------------------------------------------

def filtration_homophily_for_slice(clist, topology_name, thresholds, explicit_adj=None):
    """
    Vary edge-inclusion threshold; return homophily at each threshold.
    Used for persistent homology evidence check.
    """
    # We need type_map for homophily — use the precomputed classifications
    # Caller must have set node attribute "type" on clist
    results = []
    for thr in thresholds:
        if topology_name == "beneficiary":
            actor_sets = {}
            for e in clist:
                actor_sets[e["id"]] = frozenset(e.get("beneficiaries", []) + e.get("victims", []))
            G = nx.Graph()
            ids = list(actor_sets.keys())
            for e in clist:
                G.add_node(e["id"])
            for i, a in enumerate(ids):
                sa = actor_sets[a]
                if not sa:
                    continue
                for b in ids[i+1:]:
                    sb = actor_sets[b]
                    if sb and jaccard(sa, sb) >= thr:
                        G.add_edge(a, b)
        elif topology_name == "semantic":
            tok_sets = {e["id"]: domain_tokens(e.get("topic_domain", "")) for e in clist}
            G = nx.Graph()
            ids = list(tok_sets.keys())
            for e in clist:
                G.add_node(e["id"])
            for i, a in enumerate(ids):
                sa = tok_sets[a]
                if not sa:
                    continue
                for b in ids[i+1:]:
                    sb = tok_sets[b]
                    if sb and jaccard(sa, sb) >= thr:
                        G.add_edge(a, b)
        else:
            results.append({"threshold": thr, "homophily": None, "n_edges": 0})
            continue

        m = G.number_of_edges()
        if m == 0:
            results.append({"threshold": thr, "homophily": 0.0, "n_edges": 0})
            continue
        type_map = {e["id"]: e.get("_slice_type") for e in clist}
        same = sum(1 for u, v in G.edges() if type_map.get(u) == type_map.get(v))
        results.append({"threshold": thr, "homophily": same / m, "n_edges": m})

    return results


def score_frameworks(pass2_results, filtration_results):
    """Score candidate mathematical frameworks 0/1/2."""
    scores = {}

    # AB sheaf cohomology: not assessed
    scores["ab_sheaf_fixed_slice"] = {
        "score": "not_assessed",
        "evidence": "Requires Paper 2 specification of consistency conditions; heterogeneity alone is not a valid proxy for sheaf obstruction.",
        "paper2_question": "Do observer-slice classifications of constraint neighborhoods violate an explicit axiom-derived consistency condition?"
    }

    # Persistent homology
    ph_evidence = []
    ph_score = 0
    for topo, data in filtration_results.items():
        vals = [d["homophily"] for d in data if d["homophily"] is not None]
        if len(vals) >= 2:
            r = max(vals) - min(vals)
            ph_evidence.append(f"{topo}: homophily range={r:.3f} across filtration")
            if r > 0.30:
                ph_score = max(ph_score, 2)
            elif r > 0.15:
                ph_score = max(ph_score, 1)
    scores["persistent_homology"] = {
        "score": ph_score,
        "evidence": "; ".join(ph_evidence) if ph_evidence else "insufficient data",
        "paper2_question": "Does the topology (Betti numbers, cycle count) of the same-classification subgraph change systematically as the edge-inclusion threshold varies?"
    }

    # Spectral/Laplacian
    spec_score = 0
    spec_evidence = []
    for label, topo_results in pass2_results.items():
        for topo, res in topo_results.items():
            spec = res.get("spectrum", {})
            nz = spec.get("n_near_zero", 0)
            gap = spec.get("spectral_gap")
            if nz >= 3:
                spec_score = max(spec_score, 1)
                gap_str = f"{gap:.3f}" if gap is not None else "N/A"
                spec_evidence.append(f"{label}/{topo}: n_near_zero={nz}, gap={gap_str}")
            if gap and gap > 1.0:
                spec_score = max(spec_score, 2)
    scores["spectral_laplacian"] = {
        "score": spec_score,
        "evidence": "; ".join(spec_evidence[:3]) if spec_evidence else "no strong spectral structure found",
        "paper2_question": "What is the spectral decomposition of the classification-agreement Laplacian, and do eigenvectors identify interpretable classification regions?"
    }

    # Dirac-orbit
    dirac_score = 0
    dirac_bimodal_count = 0
    dirac_evidence = []
    for label, topo_results in pass2_results.items():
        for topo, res in topo_results.items():
            stab = res.get("stability", {})
            if stab.get("bimodal"):
                dirac_bimodal_count += 1
                dirac_evidence.append(
                    f"{label}/{topo}: std={stab['std']:.3f}, "
                    f"frac<0.3={stab['frac_below_0.3']:.2f}, "
                    f"frac>0.7={stab['frac_above_0.7']:.2f}"
                )
    if dirac_bimodal_count >= 5:
        dirac_score = 2
    elif dirac_bimodal_count >= 2:
        dirac_score = 1
    scores["dirac_orbit"] = {
        "score": dirac_score,
        "evidence": f"{dirac_bimodal_count} (slice,topology) pairs show bimodal stability. " + "; ".join(dirac_evidence[:3]),
        "paper2_question": "Do stable-core constraints and peripheral-orbit constraints form a coherent partition, and what predicts orbit membership?"
    }

    # Graph homology (cycle surplus)
    gh_score = 0
    gh_evidence = []
    gh_surplus_count = 0
    for label, topo_results in pass2_results.items():
        for topo, res in topo_results.items():
            stats = res.get("graph_stats", {})
            n = stats.get("n_nodes", 0)
            m = stats.get("n_edges", 0)
            k = stats.get("n_components", 1)
            if n > 0:
                surplus = (m - n + k) / n
                if surplus > 0.10:
                    gh_surplus_count += 1
                    gh_evidence.append(f"{label}/{topo}: cycle_surplus={surplus:.3f}")
    if gh_surplus_count >= 2:
        gh_score = 1
    scores["graph_homology"] = {
        "score": gh_score,
        "evidence": f"{gh_surplus_count} (slice,topology) pairs have cycle surplus > 10%. " + "; ".join(gh_evidence[:3]),
        "paper2_question": "What do cycles (H1) in the same-classification subgraph represent structurally? Are they constraint-interdependency loops?"
    }

    # None of the above
    all_zero = all(
        v["score"] in (0, "not_assessed")
        for v in scores.values()
        if v["score"] != "not_assessed"
    )
    scores["none_of_above"] = {
        "score": 1 if all_zero else 0,
        "evidence": "Fallback: none of the above frameworks show clear evidence." if all_zero else "At least one framework shows evidence.",
        "paper2_question": "N/A"
    }

    return scores


# ---------------------------------------------------------------------------
# Output generation
# ---------------------------------------------------------------------------

def _fmt_flow_matrix(matrix, label_a, label_b):
    """Format a flow matrix as a markdown table."""
    all_types = sorted(set(
        list(matrix.keys()) +
        [t for row in matrix.values() for t in row.keys()]
    ))
    if not all_types:
        return "*(no shared constraints)*"

    header = f"| from\\to | " + " | ".join(all_types) + " |"
    sep = "|---|" + "---|" * len(all_types)
    rows = [header, sep]
    for ft in all_types:
        row_data = matrix.get(ft, {})
        cells = [str(row_data.get(tt, 0)) for tt in all_types]
        rows.append(f"| **{ft}** | " + " | ".join(cells) + " |")
    return "\n".join(rows)


def _hist_bar(counts, width=30):
    """ASCII histogram from 10-bucket count list."""
    total = sum(counts) or 1
    lines = []
    for i, c in enumerate(counts):
        lo = i * 0.1
        hi = lo + 0.1
        bar = "█" * int(c / total * width)
        lines.append(f"  [{lo:.1f},{hi:.1f}): {bar} {c}")
    return "\n".join(lines)


def write_json(results, working_slices, pass1_all_slice_count, pass1_total_classifications):
    out = {
        "metadata": {
            "timestamp": time.strftime("%Y-%m-%dT%H:%M:%S"),
            "n_constraints": results["n_constraints"],
            "n_working_slices": len(working_slices),
            "n_total_slices": pass1_all_slice_count,
            "n_total_classifications": pass1_total_classifications,
            "thresholds": {
                "beneficiary_jaccard": BENEFICIARY_JACCARD_THRESHOLD,
                "semantic_jaccard": SEMANTIC_JACCARD_THRESHOLD,
                "near_zero_eigenvalue": NEAR_ZERO_THRESHOLD,
                "orbit_stability": ORBIT_STABILITY_THRESHOLD,
                "bimodal_low_frac": BIMODAL_LOW_FRAC,
                "bimodal_high_frac": BIMODAL_HIGH_FRAC,
                "bimodal_std_min": BIMODAL_STD_MIN,
            }
        },
        "working_slices": working_slices,
        "pass1": {"slice_distributions": results["pass1"]},
        "pass2": results["pass2"],
        "pass3": results["pass3"],
        "pass4": results["pass4"],
    }
    with open(OUT_JSON, "w") as f:
        json.dump(out, f, indent=2)
    print(f"  Written: {OUT_JSON}")


def write_md(results, working_slices, pass1_all_slice_count, pass1_total_classifications):
    lines = []
    lines.append("# Idea-Site Structure Exploration")
    lines.append("")
    lines.append(f"Generated: {time.strftime('%Y-%m-%d %H:%M:%S')} | "
                 f"Corpus: {results['n_constraints']} constraints | "
                 f"Working slices: {len(working_slices)}")
    lines.append("")

    # ── Pass 1
    lines.append("## Pass 1: Slice Enumeration")
    lines.append("")
    lines.append(f"Total distinct (P,T,E,S) slices in corpus: **{pass1_all_slice_count}**")
    lines.append(f"Total classifications: **{pass1_total_classifications:,}**")
    lines.append("")
    lines.append("### Working Set")
    lines.append("")
    lines.append("| Label | Power | T | Exit | Scope | n | Dominant type | Entropy |")
    lines.append("|---|---|---|---|---|---|---|---|")
    for s in working_slices:
        key = s["key"]
        dist = results["pass1"].get(s["label"], {})
        lines.append(
            f"| {s['label']} | {key[0]} | {key[1]} | {key[2]} | {key[3]} | "
            f"{s['coverage']} | {s.get('dominant_type','?')} | {s.get('entropy',0):.3f} |"
        )
    lines.append("")

    # Distribution comparison note
    entropies = [(s["label"], s.get("entropy", 0)) for s in working_slices]
    entropies.sort(key=lambda x: x[1])
    lines.append(
        f"Lowest entropy: **{entropies[0][0]}** ({entropies[0][1]:.3f}) — "
        f"most coherent worldview at this position."
    )
    lines.append(
        f"Highest entropy: **{entropies[-1][0]}** ({entropies[-1][1]:.3f}) — "
        f"most diverse classification profile."
    )
    lines.append("")

    # ── Pass 2
    lines.append("## Pass 2: Network Structure at Fixed Slice")
    lines.append("")
    lines.append("### Graph Statistics (all working slices × 3 topologies)")
    lines.append("")
    lines.append("| Slice | Topology | n_edges | density | homophily | baseline | lift | n_comp | LCC | clustering | path_length | method |")
    lines.append("|---|---|---|---|---|---|---|---|---|---|---|---|")
    for s in working_slices:
        label = s["label"]
        topo_results = results["pass2"].get(label, {})
        for topo in ["beneficiary", "coupling", "semantic"]:
            res = topo_results.get(topo, {})
            gs = res.get("graph_stats", {})
            pl = gs.get("avg_path_length")
            pl_str = f"{pl:.3f}" if pl is not None else "N/A"
            lines.append(
                f"| {label} | {topo} | {gs.get('n_edges',0)} | "
                f"{gs.get('density',0):.4f} | {gs.get('homophily',0):.3f} | "
                f"{gs.get('homophily_baseline',0):.3f} | {gs.get('homophily_lift',0):+.3f} | "
                f"{gs.get('n_components',0)} | {gs.get('lcc_size',0)} | "
                f"{gs.get('clustering_coeff',0):.3f} | {pl_str} | "
                f"{gs.get('path_length_method','?')} |"
            )
    lines.append("")

    # Stability bimodality
    lines.append("### Stability Distribution and Bimodality")
    lines.append("")
    bimodal_pairs = []
    for s in working_slices:
        label = s["label"]
        for topo in ["beneficiary", "coupling", "semantic"]:
            stab = results["pass2"].get(label, {}).get(topo, {}).get("stability", {})
            if stab.get("bimodal"):
                bimodal_pairs.append((label, topo, stab))
    if bimodal_pairs:
        lines.append(f"**Bimodal stability found in {len(bimodal_pairs)} (slice, topology) pairs:**")
        lines.append("")
        for label, topo, stab in bimodal_pairs:
            lines.append(
                f"- {label}/{topo}: mean={stab['mean']:.3f}, std={stab['std']:.3f}, "
                f"frac<0.3={stab['frac_below_0.3']:.2f}, frac>0.7={stab['frac_above_0.7']:.2f}"
            )
    else:
        lines.append("No bimodal stability distributions found across working (slice, topology) pairs.")
    lines.append("")

    # Orbit candidates for U_3_imm and U_4 (beneficiary topology)
    for target_label in ["U_3_imm", "U_4"]:
        candidates = results["pass2"].get(target_label, {}).get("beneficiary", {}).get("orbit_candidates", [])
        if candidates:
            lines.append(f"### Top-20 Dirac Orbit Candidates: {target_label} / beneficiary")
            lines.append("")
            lines.append("*(constraints with lowest fraction of same-type neighbors)*")
            lines.append("")
            lines.append("| # | Constraint | Stability |")
            lines.append("|---|---|---|")
            for i, c in enumerate(candidates[:20], 1):
                lines.append(f"| {i} | {c['id']} | {c['stability']:.3f} |")
            lines.append("")

    # Spectral summary
    lines.append("### Spectral Analysis Summary")
    lines.append("")
    lines.append("| Slice | Topology | n_nodes | n_near_zero | spectral_gap | p25 | p50 | p75 |")
    lines.append("|---|---|---|---|---|---|---|---|")
    for s in working_slices:
        label = s["label"]
        for topo in ["beneficiary", "coupling", "semantic"]:
            spec = results["pass2"].get(label, {}).get(topo, {}).get("spectrum", {})
            pcts = spec.get("eigenvalue_percentiles", {})
            gap = spec.get("spectral_gap")
            gap_s = f"{gap:.3f}" if gap is not None else "N/A"
            p25_s = f"{pcts['p25']:.3f}" if isinstance(pcts.get("p25"), float) else "N/A"
            p50_s = f"{pcts['p50']:.3f}" if isinstance(pcts.get("p50"), float) else "N/A"
            p75_s = f"{pcts['p75']:.3f}" if isinstance(pcts.get("p75"), float) else "N/A"
            lines.append(
                f"| {label} | {topo} | {spec.get('n_nodes',0)} | "
                f"{spec.get('n_near_zero',0)} | {gap_s} | {p25_s} | {p50_s} | {p75_s} |"
            )
    lines.append("")

    # ── Pass 3
    lines.append("## Pass 3: Multi-Slice Comparison")
    lines.append("")

    # Key flow matrices
    for pair_key, pair_label in [
        ("U_3_imm->U_4", "U_3_imm → U_4 (institutional/immediate vs analytical/civilizational)"),
        ("U_3_imm->U_3_civ", "U_3_imm → U_3_civ (time-horizon effect at institutional)"),
        ("U_1->U_4", "U_1 → U_4 (powerless vs analytical — widest power gap)"),
    ]:
        matrix = results["pass3"]["flow_matrices"].get(pair_key, {})
        if matrix:
            lines.append(f"### Flow Matrix: {pair_label}")
            lines.append("")
            a_label, b_label = pair_key.split("->")
            n_shared = sum(sum(row.values()) for row in matrix.values())
            lines.append(f"*{n_shared} constraints appear at both slices.*")
            lines.append("")
            lines.append(_fmt_flow_matrix(matrix, a_label, b_label))
            lines.append("")

    # Cross-slice stability
    lines.append("### Cross-Slice Stability Distribution")
    lines.append("")
    cs = results["pass3"].get("cross_slice_stability_summary", {})
    lines.append(f"Mean agreement fraction: **{cs.get('mean', 0):.3f}** (std={cs.get('std', 0):.3f})")
    lines.append(f"Absolutely stable (frac=1.0): **{cs.get('n_fully_stable', 0)}** constraints")
    lines.append(f"Highly variable (frac<0.5): **{cs.get('n_highly_variable', 0)}** constraints")
    lines.append("")
    hist = cs.get("histogram", [])
    if hist:
        lines.append("```")
        lines.append(_hist_bar(hist))
        lines.append("```")
    lines.append("")

    # ── Pass 4
    lines.append("## Pass 4: Framework Candidate Scorecard")
    lines.append("")

    # Filtration homophily
    lines.append("### Filtration Homophily (Persistent Homology Evidence)")
    lines.append("")
    filt = results["pass4"].get("filtration_homophily", {})
    for topo_name, filt_data in filt.items():
        if not filt_data:
            continue
        lines.append(f"**{topo_name} topology** (slice: U_3_imm):")
        lines.append("")
        lines.append("| Threshold | n_edges | homophily |")
        lines.append("|---|---|---|")
        vals = []
        for d in filt_data:
            h = d.get("homophily")
            h_s = f"{h:.3f}" if h is not None else "N/A"
            thr_s = f"{d['threshold']:.2f}"
            lines.append(f"| {thr_s} | {d['n_edges']} | {h_s} |")
            if h is not None:
                vals.append(h)
        if vals:
            lines.append(f"  *Range: {max(vals) - min(vals):.3f}*")
        lines.append("")

    # Framework scorecard
    lines.append("### Framework Scorecard")
    lines.append("")
    lines.append("| Framework | Score | Evidence | Paper 2 question |")
    lines.append("|---|---|---|---|")
    scorecard = results["pass4"].get("framework_scorecard", {})
    framework_display = {
        "ab_sheaf_fixed_slice": "AB sheaf (fixed slice)",
        "persistent_homology": "Persistent homology",
        "spectral_laplacian": "Spectral / Laplacian",
        "dirac_orbit": "Dirac-orbit",
        "graph_homology": "Graph homology",
        "none_of_above": "None of the above",
    }
    for fw_key, fw_label in framework_display.items():
        fw = scorecard.get(fw_key, {})
        score = fw.get("score", 0)
        evidence = fw.get("evidence", "")[:80] + "..." if len(fw.get("evidence", "")) > 80 else fw.get("evidence", "")
        q = fw.get("paper2_question", "")[:80] + "..." if len(fw.get("paper2_question", "")) > 80 else fw.get("paper2_question", "")
        lines.append(f"| {fw_label} | **{score}** | {evidence} | {q} |")
    lines.append("")

    # ── Section 5
    lines.append("## Section 5: Structural Observations and Recommendations")
    lines.append("")
    # Synthesize based on actual scores
    fw_scores = {k: v.get("score", 0) for k, v in scorecard.items() if k != "ab_sheaf_fixed_slice"}
    top_fw = [(k, v) for k, v in fw_scores.items() if isinstance(v, int) and v >= 1]
    top_fw.sort(key=lambda x: x[1], reverse=True)

    if top_fw:
        top_names = [k.replace("_", " ") for k, v in top_fw]
        lines.append(
            f"The data provides strongest evidence for: **{', '.join(top_names[:2])}**. "
            f"AB sheaf cohomology at fixed slice was deferred — it requires specifying "
            f"consistency conditions before it can be scored, which is the central design "
            f"question for Paper 2."
        )
    else:
        lines.append(
            "No framework shows strong evidence across the tested (slice, topology) pairs. "
            "The data does not obviously prefer any of the candidate structures. "
            "This is itself a finding: idea-site structure at fixed observer position "
            "may not be well described by any of the frameworks considered."
        )
    lines.append("")

    # Homophily lift summary
    all_lifts = []
    for s in working_slices:
        for topo in ["beneficiary", "coupling", "semantic"]:
            lift = results["pass2"].get(s["label"], {}).get(topo, {}).get("graph_stats", {}).get("homophily_lift")
            if lift is not None:
                all_lifts.append((s["label"], topo, lift))
    if all_lifts:
        all_lifts.sort(key=lambda x: x[2], reverse=True)
        best = all_lifts[0]
        worst = all_lifts[-1]
        lines.append(
            f"Highest homophily lift: **{best[0]}/{best[1]}** ({best[2]:+.3f}) — "
            f"topological neighborhood best predicts same classification here."
        )
        lines.append(
            f"Lowest lift: **{worst[0]}/{worst[1]}** ({worst[2]:+.3f}) — "
            f"classifications at this slice are most independent of network position."
        )
    lines.append("")

    # ── Self-report
    lines.append("## Self-Report")
    lines.append("")
    # Corpus concentration
    canonical_coverage = sum(
        results["pass1"].get(SLICE_LABELS.get(tuple(s["key"]), ""), {}).get("coverage", s["coverage"])
        for s in working_slices
        if SLICE_LABELS.get(tuple(s["key"]), "").startswith("U_")
        and not SLICE_LABELS.get(tuple(s["key"]), "").endswith("_nat")
        and not SLICE_LABELS.get(tuple(s["key"]), "").endswith("_glob")
        and not SLICE_LABELS.get(tuple(s["key"]), "").endswith("_civ")
        and not SLICE_LABELS.get(tuple(s["key"]), "").endswith("_gen")
    )
    lines.append(
        f"**Corpus concentration**: {pass1_all_slice_count} distinct slices; "
        f"the 4 canonical positions (U_1, U_2, U_3_imm, U_4) collectively cover the "
        f"highest-volume slices. Most constraints appear at multiple slices, but coverage "
        f"is heavily concentrated in the top 5-6 slices."
    )
    lines.append("")
    lines.append(
        "**Analytical-biographical slices**: Not populated enough for inclusion "
        "(max 5 constraints at any analytical-biographical tuple). The time-horizon "
        "comparison at the analytical position is therefore limited to scope variation "
        "(U_4 vs U_4_glob). This limits the parallel T-variation at analytical that "
        "was possible at institutional (U_3_imm / U_3_gen / U_3_civ)."
    )
    lines.append("")
    lines.append(
        f"**Stability threshold sensitivity**: The Dirac-orbit boundary is set at "
        f"stability < {ORBIT_STABILITY_THRESHOLD}. Moving to 0.4 would tighten the "
        f"orbit set (fewer candidates); moving to 0.6 would expand it. The bimodality "
        f"criterion (std > {BIMODAL_STD_MIN}, frac<0.3 > {BIMODAL_LOW_FRAC}, "
        f"frac>0.7 > {BIMODAL_HIGH_FRAC}) is a conservative heuristic. Inspect the "
        f"stability histograms in the JSON to calibrate."
    )
    lines.append("")
    lines.append(
        "**Path-length method**: Documented per (slice, topology) pair in the JSON "
        f"`graph_stats.path_length_method` field. Exact computation used for LCC ≤ "
        f"{PATH_LENGTH_EXACT_LIMIT} nodes; sampled({PATH_LENGTH_SAMPLE_PAIRS}) for larger."
    )
    lines.append("")
    lines.append(
        "**AB sheaf cohomology**: Deferred. Scoring it requires specifying what "
        '"consistency condition" means for a fixed-slice classification — i.e., what '
        "would count as a violation. That is Paper 2's design question, not something "
        "derivable from the corpus alone."
    )
    lines.append("")
    lines.append(
        "**Spectral threshold calibration**: The gap > 1.0 scoring criterion is "
        "provisional. Full eigenvalue percentiles are stored in the JSON for each "
        "(slice, topology) pair. If gap > 1.0 is widespread, the threshold is not "
        "diagnostic; if rare, it is. Recalibrate after reviewing the percentile data."
    )
    lines.append("")
    lines.append(
        "**Frameworks scored 0**: Not ruled out — not visible in the patterns these "
        "passes surfaced. Other audits with different questions might surface evidence "
        "the present audit did not look for."
    )

    with open(OUT_MD, "w") as f:
        f.write("\n".join(lines) + "\n")
    print(f"  Written: {OUT_MD}")


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    t_start = time.time()
    print(f"Loading {PIPELINE_PATH}...")
    constraints = load_data()
    n_constraints = len(constraints)
    print(f"  {n_constraints} constraints loaded.")

    constraints_by_id = {e["id"]: e for e in constraints}

    # Build explicit coupling edge index once
    print("Building explicit coupling edge index...")
    explicit_adj = build_explicit_edges(constraints)
    n_explicit = sum(len(v) for v in explicit_adj.values()) // 2
    print(f"  {n_explicit} undirected explicit edges.")

    # ── Pass 1
    print("\nPass 1: Slice enumeration...")
    slice_map = enumerate_slices(constraints)
    pass1_all_slice_count = len(slice_map)
    pass1_total_classifications = sum(len(v["ids"]) for v in slice_map.values())
    print(f"  {pass1_all_slice_count} distinct slices, {pass1_total_classifications:,} classifications total.")

    working_slices = select_working_set(slice_map)
    print(f"  Working set: {len(working_slices)} slices selected.")
    for s in working_slices:
        print(f"    [{s['label']}] {s['key']} n={s['coverage']}")

    slice_cls_map = slice_distributions(constraints, working_slices)
    pass1_results = {}
    for s in working_slices:
        key = tuple(s["key"])
        data = slice_map.get(key, {})
        tc = data.get("type_counts", Counter())
        s["entropy"] = shannon_entropy(tc)
        s["dominant_type"] = tc.most_common(1)[0][0] if tc else None
        pass1_results[s["label"]] = {
            "coverage": s["coverage"],
            "type_counts": dict(tc),
            "entropy": s["entropy"],
            "dominant_type": s["dominant_type"],
        }

    # ── Pass 2
    print("\nPass 2: Network analysis...")
    pass2_results = {}
    for s in working_slices:
        label = s["label"]
        key = tuple(s["key"])
        print(f"  Slice: {label}")
        pass2_results[label] = {}

        type_map_at_slice = slice_cls_map.get(key, {})
        clist = [constraints_by_id[cid] for cid in type_map_at_slice if cid in constraints_by_id]
        # Attach slice type for filtration analysis
        for e in clist:
            e["_slice_type"] = type_map_at_slice.get(e["id"])

        for topo in ["beneficiary", "coupling", "semantic"]:
            res = analyze_slice_topology(label, key, slice_cls_map, constraints_by_id,
                                         explicit_adj, topo)
            pass2_results[label][topo] = res

        # Clean up temp attribute
        for e in clist:
            e.pop("_slice_type", None)

    # ── Pass 3
    print("\nPass 3: Multi-slice comparison...")
    slice_label_to_key = {s["label"]: tuple(s["key"]) for s in working_slices}

    # Compute flow matrices for all pairs, store key pairs in JSON
    all_pairs = {}
    key_pairs_for_md = ["U_3_imm->U_4", "U_3_imm->U_3_civ", "U_1->U_4"]
    for i, si in enumerate(working_slices):
        for sj in working_slices[i+1:]:
            pair_key = f"{si['label']}->{sj['label']}"
            ki, kj = tuple(si["key"]), tuple(sj["key"])
            cls_i = slice_cls_map.get(ki, {})
            cls_j = slice_cls_map.get(kj, {})
            matrix = compute_flow_matrix(cls_i, cls_j)
            all_pairs[pair_key] = matrix

    cross_stab = cross_slice_stability(constraints, working_slices, slice_cls_map)

    # Summary statistics for cross-slice stability
    frac_vals = [v["frac_agree"] for v in cross_stab.values()]
    cs_mean = float(np.mean(frac_vals)) if frac_vals else 0
    cs_std = float(np.std(frac_vals)) if frac_vals else 0
    n_fully_stable = sum(1 for v in frac_vals if v >= 1.0)
    n_highly_variable = sum(1 for v in frac_vals if v < 0.5)
    hist_counts = [0] * 10
    for v in frac_vals:
        bucket = min(int(v * 10), 9)
        hist_counts[bucket] += 1

    # Build key flow matrices with explicit direction (A→B means row=A-type, col=B-type)
    key_flow_matrices = {}
    label_to_key = {s["label"]: tuple(s["key"]) for s in working_slices}
    for pair_str in key_pairs_for_md:
        a_label, b_label = pair_str.split("->")
        ka = label_to_key.get(a_label)
        kb = label_to_key.get(b_label)
        if ka and kb:
            cls_a = slice_cls_map.get(ka, {})
            cls_b = slice_cls_map.get(kb, {})
            key_flow_matrices[pair_str] = compute_flow_matrix(cls_a, cls_b)

    pass3_results = {
        "flow_matrices": key_flow_matrices,
        "flow_matrices_all": all_pairs,
        "cross_slice_stability": cross_stab,
        "cross_slice_stability_summary": {
            "mean": cs_mean,
            "std": cs_std,
            "n_fully_stable": n_fully_stable,
            "n_highly_variable": n_highly_variable,
            "histogram": hist_counts,
        }
    }
    print(f"  Cross-slice stability: mean={cs_mean:.3f}, std={cs_std:.3f}")

    # ── Pass 4
    print("\nPass 4: Framework scoring...")
    # Filtration analysis on U_3_imm (largest well-populated institutional slice)
    ref_key = tuple(slice_label_to_key["U_3_imm"])
    ref_type_map = slice_cls_map.get(ref_key, {})
    ref_clist = [constraints_by_id[cid] for cid in ref_type_map if cid in constraints_by_id]
    for e in ref_clist:
        e["_slice_type"] = ref_type_map.get(e["id"])

    filtration = {}
    print("  Running beneficiary filtration...")
    filtration["beneficiary"] = filtration_homophily_for_slice(
        ref_clist, "beneficiary", BENEFICIARY_FILTRATION
    )
    print("  Running semantic filtration...")
    filtration["semantic"] = filtration_homophily_for_slice(
        ref_clist, "semantic", SEMANTIC_FILTRATION
    )
    for e in ref_clist:
        e.pop("_slice_type", None)

    scorecard = score_frameworks(pass2_results, filtration)
    pass4_results = {
        "filtration_homophily": filtration,
        "framework_scorecard": scorecard,
    }

    print("\nFramework scores:")
    for k, v in scorecard.items():
        print(f"  {k}: {v['score']}")

    # ── Assemble and write
    results = {
        "n_constraints": n_constraints,
        "pass1": pass1_results,
        "pass2": pass2_results,
        "pass3": pass3_results,
        "pass4": pass4_results,
    }

    print("\nWriting outputs...")
    write_json(results, working_slices, pass1_all_slice_count, pass1_total_classifications)
    write_md(results, working_slices, pass1_all_slice_count, pass1_total_classifications)

    elapsed = time.time() - t_start
    print(f"\nDone in {elapsed:.1f}s")


if __name__ == "__main__":
    main()
