"""
Orbit population characterization for Paper 2.

Extracts and analyzes the orbit population from the 7 bimodal (slice, topology)
pairs identified in Phase 1 (idea_site_exploration). Five passes:
  1. Orbit extraction — top-50 lowest-stability candidates per pair
  2. Type distribution and topic-domain enrichment
  3. Cross-slice overlap — 7x7 Jaccard matrix + universal-orbit lists (4 variants)
  4. Asymmetry — (A) minority-typed vs (B) displaced-core, two definitions
  5. Detector correlation — h1_band, FCR, FSM, beneficiaries, multi_slice_typed

Outputs:
  outputs/orbit_characterization.json
  outputs/orbit_characterization.md
"""

import json
import math
from collections import Counter, defaultdict
from pathlib import Path

import networkx as nx
import numpy as np

from shared.loader import h1_band_or_raise


def _h1_or_loud(entry):
    """OQ-51 containment: h1_band, loud on null, never silent 0. A constraint
    entirely ABSENT from the index (entry == {}) keeps the pre-existing 0 (a
    separate join concern); a PRESENT entry with a null h1_band raises loud."""
    return h1_band_or_raise(entry, "orbit_characterization") if entry else 0


# ---------------------------------------------------------------------------
# Constants
# ---------------------------------------------------------------------------

PIPELINE_PATH   = Path("outputs/pipeline_output.json")
EXPLORATION_PATH = Path("outputs/idea_site_exploration.json")
OUT_JSON = Path("outputs/orbit_characterization.json")
OUT_MD   = Path("outputs/orbit_characterization.md")

TOP_N = 50
BENEFICIARY_JACCARD = 0.15
SEMANTIC_JACCARD    = 0.33
AB_THRESHOLD = 0.10   # type-frequency threshold for displaced-core definition

EPSILON = 1e-9        # enrichment-ratio denominator guard


# ---------------------------------------------------------------------------
# Utility
# ---------------------------------------------------------------------------

def jaccard(a, b):
    if not a and not b:
        return 0.0
    return len(a & b) / len(a | b)


def domain_tokens(s):
    if not s:
        return frozenset()
    return frozenset(s.replace("/", "_").split("_"))


def make_key(ctx):
    return (ctx["agent_power"], ctx["time_horizon"], ctx["exit_options"], ctx["spatial_scope"])


# ---------------------------------------------------------------------------
# Graph builders (replicated from idea_site_exploration.py)
# ---------------------------------------------------------------------------

def build_beneficiary_graph(clist):
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
        for b in ids[i + 1:]:
            sb = actor_sets[b]
            if sb and jaccard(sa, sb) >= BENEFICIARY_JACCARD:
                G.add_edge(a, b)
    return G


def build_coupling_graph(clist, explicit_adj):
    G = nx.Graph()
    ids_in_slice = {e["id"] for e in clist}
    for e in clist:
        G.add_node(e["id"])
    for e in clist:
        src = e["id"]
        for dst in explicit_adj.get(src, set()):
            if dst in ids_in_slice:
                G.add_edge(src, dst)
    return G


def build_semantic_graph(clist):
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
        for b in ids[i + 1:]:
            sb = tok_sets[b]
            if sb and jaccard(sa, sb) >= SEMANTIC_JACCARD:
                G.add_edge(a, b)
    return G


# ---------------------------------------------------------------------------
# Step 0 — Load data and preprocessing
# ---------------------------------------------------------------------------

def load_data():
    print("Loading pipeline_output.json …")
    raw = json.loads(PIPELINE_PATH.read_text())
    constraints_idx = {c["id"]: c for c in raw["per_constraint"]}
    print(f"  {len(constraints_idx):,} constraints loaded")

    print("Loading idea_site_exploration.json …")
    exploration = json.loads(EXPLORATION_PATH.read_text())
    working_slices = exploration["working_slices"]
    print(f"  {len(working_slices)} working slices")

    # Explicit contamination adjacency
    explicit_adj = defaultdict(set)
    for cid, c in constraints_idx.items():
        for nb in c.get("contamination_network", {}).get("neighbors", []):
            if nb.get("edge_type") == "explicit":
                explicit_adj[cid].add(nb["constraint_id"])

    # Type maps: {slice_key_tuple: {cid: type}}
    slice_keys = {tuple(s["key"]) for s in working_slices}
    type_maps = {k: {} for k in slice_keys}
    for cid, c in constraints_idx.items():
        for cls in c.get("classifications", []):
            k = make_key(cls["context"])
            if k in type_maps:
                type_maps[k][cid] = cls["type"]

    # multi_slice_typed: ≥2 distinct types across working slices
    # (NOT the project's gauge-variant — that is h1_band > 0)
    multi_slice_typed_ids = set()
    for cid in constraints_idx:
        types_seen = {tm[cid] for tm in type_maps.values() if cid in tm}
        if len(types_seen) >= 2:
            multi_slice_typed_ids.add(cid)
    print(f"  multi_slice_typed: {len(multi_slice_typed_ids):,} constraints")

    # Identify bimodal pairs from pass2
    bimodal_pairs = []
    for s in working_slices:
        label = s["label"]
        key   = tuple(s["key"])
        for topo in ("beneficiary", "coupling", "semantic"):
            entry = exploration.get("pass2", {}).get(label, {}).get(topo, {})
            if entry.get("stability", {}).get("bimodal"):
                bimodal_pairs.append({
                    "label": label,
                    "key": key,
                    "topology": topo,
                    "pair_id": f"{label}/{topo}",
                })
    print(f"  {len(bimodal_pairs)} bimodal (slice, topology) pairs found")
    for bp in bimodal_pairs:
        print(f"    {bp['pair_id']}")

    return constraints_idx, exploration, working_slices, explicit_adj, type_maps, \
           multi_slice_typed_ids, bimodal_pairs


# ---------------------------------------------------------------------------
# Pass 1 — Orbit extraction (top-50 per bimodal pair)
# ---------------------------------------------------------------------------

def pass1_extract_orbits(bimodal_pairs, constraints_idx, type_maps, explicit_adj,
                          multi_slice_typed_ids):
    print("\nPass 1: orbit extraction …")
    for pair in bimodal_pairs:
        label, key, topo = pair["label"], pair["key"], pair["topology"]
        type_map = type_maps[key]
        clist = [constraints_idx[cid] for cid in type_map if cid in constraints_idx]

        if topo == "beneficiary":
            G = build_beneficiary_graph(clist)
        elif topo == "coupling":
            G = build_coupling_graph(clist, explicit_adj)
        else:
            G = build_semantic_graph(clist)

        stabilities = {}
        degrees = {}
        for node in G.nodes():
            nbrs = list(G.neighbors(node))
            degrees[node] = len(nbrs)
            if not nbrs:
                continue
            t = type_map.get(node)
            same = sum(1 for nb in nbrs if type_map.get(nb) == t)
            stabilities[node] = same / len(nbrs)

        # Top-50: lowest stability, tie-break by lowest degree
        top50_items = sorted(stabilities.items(), key=lambda x: (x[1], degrees[x[0]]))[:TOP_N]

        orbit_candidates = []
        for cid, stab in top50_items:
            entry = constraints_idx[cid]
            nbrs  = list(G.neighbors(cid))
            neigh_types = Counter(type_map.get(nb) for nb in nbrs)
            orbit_candidates.append({
                "id": cid,
                "classification": type_map.get(cid),
                "stability": stab,
                "degree": degrees[cid],
                "neighbor_type_dist": dict(neigh_types),
                "h1_band": _h1_or_loud(entry),
                "signature": entry.get("signature"),
                "topic_domain": entry.get("topic_domain") or "",
                "beneficiaries": entry.get("beneficiaries") or [],
                "victims": entry.get("victims") or [],
                "base_extractiveness": entry.get("base_extractiveness"),
                "gauge_variant": _h1_or_loud(entry) > 0,
                "multi_slice_typed": cid in multi_slice_typed_ids,
            })

        pair["orbit50"]     = orbit_candidates
        pair["stabilities"] = stabilities
        pair["degrees"]     = degrees
        pair["type_map"]    = type_map
        pair["G"]           = G
        pair["clist"]       = clist
        print(f"  {pair['pair_id']}: {len(clist)} constraints, {G.number_of_nodes()} nodes, "
              f"{G.number_of_edges()} edges, {len(stabilities)} non-isolated, "
              f"top-50 stab range [{top50_items[0][1]:.3f}, {top50_items[-1][1]:.3f}]")

    return bimodal_pairs


# ---------------------------------------------------------------------------
# Pass 2 — Type distribution and topic-domain enrichment
# ---------------------------------------------------------------------------

def enrichment_ratio(orbit_frac, nonorbit_frac):
    return (orbit_frac + EPSILON) / (nonorbit_frac + EPSILON)


def pass2_type_and_domain(bimodal_pairs):
    print("\nPass 2: type distribution and topic-domain enrichment …")
    type_enrichment   = {}
    topic_enrichment  = {}

    for pair in bimodal_pairs:
        pid       = pair["pair_id"]
        type_map  = pair["type_map"]
        orbit_ids = {c["id"] for c in pair["orbit50"]}
        all_ids   = set(type_map.keys())
        nonorbit_ids = all_ids - orbit_ids

        n_orbit    = len(orbit_ids)
        n_nonorbit = len(nonorbit_ids)

        orbit_types    = Counter(type_map[cid] for cid in orbit_ids   if cid in type_map)
        nonorbit_types = Counter(type_map[cid] for cid in nonorbit_ids if cid in type_map)

        all_types = set(orbit_types) | set(nonorbit_types)
        type_rows = []
        for t in sorted(all_types):
            oc = orbit_types.get(t, 0)
            nc = nonorbit_types.get(t, 0)
            of = oc / n_orbit    if n_orbit    else 0.0
            nf = nc / n_nonorbit if n_nonorbit else 0.0
            type_rows.append({
                "type": t,
                "orbit_count": oc,
                "nonorbit_count": nc,
                "orbit_frac": round(of, 4),
                "nonorbit_frac": round(nf, 4),
                "enrichment": round(enrichment_ratio(of, nf), 3),
            })
        type_rows.sort(key=lambda r: -r["enrichment"])
        type_enrichment[pid] = type_rows

        # Topic-domain enrichment
        orbit_domains    = Counter(
            pair["orbit50"][i]["topic_domain"]
            for i in range(len(pair["orbit50"]))
        )
        # get topic_domain for non-orbit constraints from constraints_idx
        # We have clist in pair
        cid_to_td = {c["id"]: (c.get("topic_domain") or "") for c in pair["clist"]}
        nonorbit_domains = Counter(
            cid_to_td[cid] for cid in nonorbit_ids if cid in cid_to_td
        )

        all_domains = set(orbit_domains) | set(nonorbit_domains)
        topic_rows = []
        for d in all_domains:
            if not d:
                continue
            oc = orbit_domains.get(d, 0)
            nc = nonorbit_domains.get(d, 0)
            of = oc / n_orbit    if n_orbit    else 0.0
            nf = nc / n_nonorbit if n_nonorbit else 0.0
            topic_rows.append({
                "topic_domain": d,
                "orbit_count": oc,
                "nonorbit_count": nc,
                "enrichment": round(enrichment_ratio(of, nf), 3),
            })
        topic_rows.sort(key=lambda r: -r["enrichment"])
        topic_enrichment[pid] = topic_rows[:10]

    return type_enrichment, topic_enrichment


# ---------------------------------------------------------------------------
# Pass 3 — Cross-slice overlap
# ---------------------------------------------------------------------------

def pass3_cross_slice_overlap(bimodal_pairs, constraints_idx):
    print("\nPass 3: cross-slice overlap …")
    n = len(bimodal_pairs)
    orbit_id_sets = [frozenset(c["id"] for c in p["orbit50"]) for p in bimodal_pairs]
    labels = [p["pair_id"] for p in bimodal_pairs]

    matrix = []
    for i in range(n):
        row = []
        for j in range(n):
            a, b = orbit_id_sets[i], orbit_id_sets[j]
            union = a | b
            row.append(round(len(a & b) / len(union), 4) if union else 0.0)
        matrix.append(row)

    # Count appearances per constraint
    appearance_count = Counter()
    for s in orbit_id_sets:
        appearance_count.update(s)

    # Universal orbits — four variants
    coupling_pairs    = [p for p in bimodal_pairs if p["topology"] == "coupling"]
    beneficiary_pairs = [p for p in bimodal_pairs if p["topology"] == "beneficiary"]
    n_coupling    = len(coupling_pairs)
    n_beneficiary = len(beneficiary_pairs)

    coupling_id_sets    = [frozenset(c["id"] for c in p["orbit50"]) for p in coupling_pairs]
    beneficiary_id_sets = [frozenset(c["id"] for c in p["orbit50"]) for p in beneficiary_pairs]

    coupling_count    = Counter()
    for s in coupling_id_sets:
        coupling_count.update(s)
    beneficiary_count = Counter()
    for s in beneficiary_id_sets:
        beneficiary_count.update(s)

    def make_universal_list(count_dict, threshold, constraints_idx):
        result = []
        for cid, cnt in sorted(count_dict.items(), key=lambda x: -x[1]):
            if cnt >= threshold:
                e = constraints_idx.get(cid, {})
                result.append({
                    "id": cid,
                    "appearances": cnt,
                    "h1_band": _h1_or_loud(e),
                    "signature": e.get("signature"),
                    "topic_domain": e.get("topic_domain") or "",
                    "beneficiaries": e.get("beneficiaries") or [],
                    "gauge_variant": _h1_or_loud(e) > 0,
                })
        return result

    union_universal   = make_universal_list(appearance_count, 4, constraints_idx)
    coupling_univ     = make_universal_list(coupling_count, max(3, n_coupling // 2), constraints_idx)
    beneficiary_univ  = make_universal_list(beneficiary_count, 1, constraints_idx)
    cross_topo_univ   = [
        u for u in union_universal
        if coupling_count.get(u["id"], 0) >= 1 and beneficiary_count.get(u["id"], 0) >= 1
    ]

    print(f"  Universal orbits (≥4/7): {len(union_universal)}")
    print(f"  Coupling-universal (≥{max(3, n_coupling//2)}/{n_coupling}): {len(coupling_univ)}")
    print(f"  Beneficiary-universal (≥1/{n_beneficiary}): {len(beneficiary_univ)}")
    print(f"  Cross-topology: {len(cross_topo_univ)}")

    offdiag_vals = [matrix[i][j] for i in range(n) for j in range(n) if i != j]
    print(f"  7x7 Jaccard mean off-diagonal: {np.mean(offdiag_vals):.4f}, "
          f"max: {max(offdiag_vals):.4f}")

    return {
        "matrix": matrix,
        "labels": labels,
        "union_universal": union_universal,
        "coupling_universal": coupling_univ,
        "beneficiary_universal": beneficiary_univ,
        "cross_topology_universal": cross_topo_univ,
        "n_coupling_pairs": n_coupling,
        "n_beneficiary_pairs": n_beneficiary,
        "coupling_threshold": max(3, n_coupling // 2),
    }


# ---------------------------------------------------------------------------
# Pass 4 — Asymmetry analysis
# ---------------------------------------------------------------------------

def pass4_asymmetry(bimodal_pairs):
    print("\nPass 4: asymmetry analysis …")
    orbit_class_fractions = {}
    displaced_core_orbits = {}

    for pair in bimodal_pairs:
        pid      = pair["pair_id"]
        type_map = pair["type_map"]

        n_slice     = len(type_map)
        type_counts = Counter(type_map.values())
        type_freqs  = {t: c / n_slice for t, c in type_counts.items()}
        dominant_type = type_counts.most_common(1)[0][0]

        for cand in pair["orbit50"]:
            c_type       = cand["classification"]
            neigh_types  = cand["neighbor_type_dist"]

            # Dominant neighbor type (excluding None keys)
            clean_neigh = {t: v for t, v in neigh_types.items() if t is not None}
            neigh_dominant = max(clean_neigh, key=clean_neigh.get) if clean_neigh else c_type
            neigh_dominated_by_other = (neigh_dominant != c_type) if c_type else True

            # Dominant-only definition
            cand["orbit_class_dominant"] = (
                "B_displaced_core" if c_type == dominant_type else "A_minority_typed"
            )

            # Threshold-based definition (P > 0.10)
            p_ci = type_freqs.get(c_type, 0.0)
            cand["orbit_class_threshold"] = (
                "B_displaced_core"
                if (p_ci > AB_THRESHOLD and neigh_dominated_by_other)
                else "A_minority_typed"
            )
            cand["p_type_in_slice"] = round(p_ci, 4)

            # Displacement characterization for (B) threshold orbits
            if cand["orbit_class_threshold"] == "B_displaced_core" and clean_neigh:
                total_neigh = sum(clean_neigh.values())
                top_frac    = clean_neigh[neigh_dominant] / total_neigh if total_neigh else 0.0
                cand["displacement_type"] = (
                    "single_type_majority" if top_frac > 0.5 else "mixed_neighborhood"
                )
                cand["displacement_dominant_neighbor"] = neigh_dominant
            else:
                cand["displacement_type"] = None
                cand["displacement_dominant_neighbor"] = None

        # Summary fractions
        n_total = len(pair["orbit50"])
        dom_b   = sum(1 for c in pair["orbit50"] if c["orbit_class_dominant"]   == "B_displaced_core")
        thr_b   = sum(1 for c in pair["orbit50"] if c["orbit_class_threshold"]  == "B_displaced_core")
        orbit_class_fractions[pid] = {
            "n_orbit": n_total,
            "dominant_def": {
                "B_displaced_core": dom_b,
                "A_minority_typed": n_total - dom_b,
                "frac_B": round(dom_b / n_total, 3) if n_total else 0.0,
            },
            "threshold_def": {
                "B_displaced_core": thr_b,
                "A_minority_typed": n_total - thr_b,
                "frac_B": round(thr_b / n_total, 3) if n_total else 0.0,
            },
        }

        # Top-20 (B) orbits (threshold definition)
        b_orbits = [c for c in pair["orbit50"] if c["orbit_class_threshold"] == "B_displaced_core"]
        b_orbits.sort(key=lambda c: c["stability"])
        displaced_core_orbits[pid] = b_orbits[:20]

        print(f"  {pid}: {n_total} orbits | "
              f"dominant-def B={dom_b} ({dom_b/n_total:.0%}) | "
              f"threshold-def B={thr_b} ({thr_b/n_total:.0%})")

    return orbit_class_fractions, displaced_core_orbits


# ---------------------------------------------------------------------------
# Pass 5 — Detector correlation
# ---------------------------------------------------------------------------

def pass5_detectors(bimodal_pairs, constraints_idx, multi_slice_typed_ids):
    print("\nPass 5: detector correlation …")

    all_orbit_ids = set()
    for pair in bimodal_pairs:
        all_orbit_ids.update(c["id"] for c in pair["orbit50"])

    # Per-constraint orbit frequency (for top-N tie-breaking in unflagged list)
    orbit_appearance = Counter()
    for pair in bimodal_pairs:
        for c in pair["orbit50"]:
            orbit_appearance[c["id"]] += 1

    detectors = {
        "h1_band_gt0":       lambda e: _h1_or_loud(e) > 0,
        "fcr_signature":     lambda e: e.get("signature") == "false_ci_rope",
        "fsm_signature":     lambda e: e.get("signature") == "false_summit_mountain",
        "has_beneficiaries": lambda e: bool(e.get("beneficiaries")),
        "multi_slice_typed": lambda e: e["id"] in multi_slice_typed_ids,
    }

    n_corpus = len(constraints_idx)
    n_orbit  = len(all_orbit_ids)

    # Per-detector enrichment
    enrichment_table = {}
    orbit_flagged_by = defaultdict(set)   # cid → set of detector names that flag it

    for det_name, det_fn in detectors.items():
        corpus_count = sum(1 for e in constraints_idx.values() if det_fn(e))
        orbit_count  = sum(1 for cid in all_orbit_ids
                           if det_fn(constraints_idx.get(cid, {})))
        corpus_frac  = corpus_count / n_corpus if n_corpus else 0.0
        orbit_frac   = orbit_count  / n_orbit  if n_orbit  else 0.0
        enrichment_table[det_name] = {
            "corpus_count": corpus_count,
            "orbit_count":  orbit_count,
            "corpus_frac":  round(corpus_frac, 4),
            "orbit_frac":   round(orbit_frac, 4),
            "enrichment":   round(enrichment_ratio(orbit_frac, corpus_frac), 3),
        }
        for cid in all_orbit_ids:
            e = constraints_idx.get(cid, {})
            if e:
                e["id"] = cid
            if det_fn(e):
                orbit_flagged_by[cid].add(det_name)

    print("  Detector enrichment:")
    for k, v in sorted(enrichment_table.items(), key=lambda x: -x[1]["enrichment"]):
        print(f"    {k}: orbit={v['orbit_frac']:.3f} corpus={v['corpus_frac']:.3f} "
              f"enrichment={v['enrichment']:.2f}")

    # Detector intersection cardinalities
    det_names = list(detectors.keys())
    intersection = {}
    for det in det_names:
        intersection[det] = sum(1 for cid in all_orbit_ids if det in orbit_flagged_by[cid])

    # Pairwise intersections (orbit ∩ A ∩ B)
    for i in range(len(det_names)):
        for j in range(i + 1, len(det_names)):
            a, b = det_names[i], det_names[j]
            cnt = sum(1 for cid in all_orbit_ids
                      if a in orbit_flagged_by[cid] and b in orbit_flagged_by[cid])
            intersection[f"{a}&{b}"] = cnt

    # Un-flagged orbits (not in ANY detector)
    unflagged_ids = [cid for cid in all_orbit_ids if not orbit_flagged_by[cid]]
    unflagged_ids.sort(key=lambda cid: -orbit_appearance[cid])

    unflagged_orbits = []
    for cid in unflagged_ids[:20]:
        e = constraints_idx.get(cid, {})
        unflagged_orbits.append({
            "id": cid,
            "appearances_in_orbit_lists": orbit_appearance[cid],
            "h1_band": _h1_or_loud(e),
            "signature": e.get("signature"),
            "topic_domain": e.get("topic_domain") or "",
            "beneficiaries": e.get("beneficiaries") or [],
            "victims": e.get("victims") or [],
            "base_extractiveness": e.get("base_extractiveness"),
            "multi_slice_typed": cid in multi_slice_typed_ids,
        })

    print(f"  Un-flagged orbits (not in any detector): {len(unflagged_ids)} "
          f"({len(unflagged_ids)/n_orbit:.1%} of orbit union)")

    return {
        "n_corpus": n_corpus,
        "n_orbit_union": n_orbit,
        "enrichment": enrichment_table,
        "intersection": intersection,
        "n_unflagged": len(unflagged_ids),
        "unflagged_orbits": unflagged_orbits,
    }


# ---------------------------------------------------------------------------
# Output: JSON
# ---------------------------------------------------------------------------

def write_json(bimodal_pairs, type_enrichment, topic_enrichment, p3, p4_fractions,
               p4_displaced, p5, multi_slice_typed_ids):
    orbit_lists = {}
    for pair in bimodal_pairs:
        orbit_lists[pair["pair_id"]] = [
            {k: v for k, v in c.items()
             if k not in ("neighbor_type_dist",)}
            | {"neighbor_type_dist": c["neighbor_type_dist"]}
            for c in pair["orbit50"]
        ]

    out = {
        "metadata": {
            "n_bimodal_pairs": len(bimodal_pairs),
            "top_n": TOP_N,
            "ab_threshold": AB_THRESHOLD,
        },
        "orbit_lists": orbit_lists,
        "recurrence_matrix": p3["matrix"],
        "recurrence_labels": p3["labels"],
        "universal_orbits": p3["union_universal"],           # alias for verification script
        "universal_orbits_union": p3["union_universal"],
        "universal_orbits_coupling": p3["coupling_universal"],
        "universal_orbits_beneficiary": p3["beneficiary_universal"],
        "universal_orbits_cross_topology": p3["cross_topology_universal"],
        "type_enrichment": type_enrichment,
        "topic_enrichment": topic_enrichment,
        "orbit_class_fractions": p4_fractions,
        "displaced_core_top20": {
            pid: cands for pid, cands in p4_displaced.items()
        },
        "detector_enrichment": p5["enrichment"],
        "detector_intersection": p5["intersection"],
        "n_unflagged": p5["n_unflagged"],
        "unflagged_orbits": p5["unflagged_orbits"],
    }

    OUT_JSON.write_text(json.dumps(out, indent=2))
    print(f"\nWrote {OUT_JSON} ({OUT_JSON.stat().st_size // 1024} KB)")


# ---------------------------------------------------------------------------
# Output: Markdown
# ---------------------------------------------------------------------------

def fmt_table(headers, rows, fmt=None):
    if fmt is None:
        fmt = ["{}"] * len(headers)
    col_widths = [max(len(h), max((len(str(f.format(r[i]) if isinstance(f, str) else f(r[i])))
                                   for r in rows), default=0))
                  for i, (h, f) in enumerate(zip(headers, fmt))]
    sep  = "| " + " | ".join("-" * w for w in col_widths) + " |"
    head = "| " + " | ".join(h.ljust(col_widths[i]) for i, h in enumerate(headers)) + " |"
    lines = [head, sep]
    for row in rows:
        cells = []
        for i, f in enumerate(fmt):
            val = row[i] if isinstance(row, (list, tuple)) else list(row.values())[i]
            if callable(f):
                cells.append(str(f(val)).ljust(col_widths[i]))
            else:
                cells.append(f.format(val).ljust(col_widths[i]))
        lines.append("| " + " | ".join(cells) + " |")
    return "\n".join(lines)


def write_md(bimodal_pairs, type_enrichment, topic_enrichment, p3, p4_fractions,
             p4_displaced, p5):
    lines = []
    A = lines.append

    A("# Orbit Population Characterization — Paper 2 Phase 2 Audit\n")

    # ---- Section 1: Metadata ----
    A("## 1. Metadata\n")
    A(f"- Bimodal (slice, topology) pairs: **{len(bimodal_pairs)}**")
    A(f"- Orbit candidates per pair: **{TOP_N}**")
    A(f"- Orbit union size (unique): **{p5['n_orbit_union']}**")
    A(f"- Corpus size: **{p5['n_corpus']:,}**")
    A(f"- (A)/(B) threshold: P_type > {AB_THRESHOLD}\n")

    A("### Bimodal pairs\n")
    A("| # | Pair | Slice key |")
    A("|---|------|-----------|")
    for i, pair in enumerate(bimodal_pairs, 1):
        A(f"| {i} | `{pair['pair_id']}` | {pair['key']} |")
    A("")

    # ---- Section 2: Type distribution and topic-domain enrichment ----
    A("## 2. Type Distribution and Topic-Domain Enrichment\n")
    for pid, rows in type_enrichment.items():
        A(f"### {pid}\n")
        A("| type | orbit_count | nonorbit_count | orbit_frac | nonorbit_frac | enrichment |")
        A("|------|-------------|----------------|------------|---------------|------------|")
        for r in rows:
            A(f"| {r['type']} | {r['orbit_count']} | {r['nonorbit_count']} | "
              f"{r['orbit_frac']:.3f} | {r['nonorbit_frac']:.3f} | **{r['enrichment']:.2f}** |")
        A("")
        A("**Top topic-domains enriched in orbit:**\n")
        A("| topic_domain | orbit_count | nonorbit_count | enrichment |")
        A("|--------------|-------------|----------------|------------|")
        for r in topic_enrichment.get(pid, [])[:10]:
            A(f"| {r['topic_domain'][:60]} | {r['orbit_count']} | "
              f"{r['nonorbit_count']} | **{r['enrichment']:.2f}** |")
        A("")

    # ---- Section 3: Cross-slice overlap ----
    A("## 3. Cross-Slice Orbit Overlap\n")
    A("### 3.1 Jaccard Recurrence Matrix\n")
    labels = p3["labels"]
    n = len(labels)
    header = "| |" + "|".join(f" {l} " for l in labels) + "|"
    sep    = "|---|" + "|".join("---" for _ in labels) + "|"
    A(header)
    A(sep)
    for i, row in enumerate(p3["matrix"]):
        cells = " | ".join(f"**{v:.2f}**" if i == j else f"{v:.2f}" for j, v in enumerate(row))
        A(f"| {labels[i]} | {cells} |")
    A("")

    offdiag = [p3["matrix"][i][j] for i in range(n) for j in range(n) if i != j]
    A(f"Mean off-diagonal Jaccard: **{np.mean(offdiag):.4f}** "
      f"| Max: **{max(offdiag):.4f}** | Min: **{min(offdiag):.4f}**\n")

    A("### 3.2 Universal Orbits\n")
    for variant_name, variant_key, threshold_desc in [
        ("Union (≥4 of 7 lists)", "union_universal", "≥4/7 orbit-50 lists"),
        (f"Coupling-stratified (≥{p3['coupling_threshold']} of {p3['n_coupling_pairs']} coupling lists)",
         "coupling_universal", f"≥{p3['coupling_threshold']}/{p3['n_coupling_pairs']} coupling lists"),
        (f"Beneficiary-stratified (≥1 of {p3['n_beneficiary_pairs']} beneficiary lists)",
         "beneficiary_universal", f"≥1/{p3['n_beneficiary_pairs']} beneficiary lists"),
        ("Cross-topology (≥1 coupling AND ≥1 beneficiary)", "cross_topology_universal",
         "≥1 coupling AND ≥1 beneficiary"),
    ]:
        univ_list = p3[variant_key]
        A(f"**{variant_name}** — {len(univ_list)} constraints\n")
        if univ_list:
            A("| id | appearances | h1_band | signature | topic_domain |")
            A("|----|-------------|---------|-----------|--------------|")
            for u in univ_list[:20]:
                sig = u.get("signature") or "—"
                A(f"| {u['id']} | {u['appearances']} | {u['h1_band']} | "
                  f"{sig} | {(u['topic_domain'] or '')[:50]} |")
        A("")

    # ---- Section 4: Asymmetry ----
    A("## 4. Asymmetry Analysis\n")
    A("### 4.1 (A)/(B) Fractions per Pair\n")
    A("| pair | n_orbit | dominant-def B% | threshold-def B% |")
    A("|------|---------|-----------------|------------------|")
    for pid, frac in p4_fractions.items():
        A(f"| {pid} | {frac['n_orbit']} | "
          f"{frac['dominant_def']['frac_B']:.0%} | "
          f"{frac['threshold_def']['frac_B']:.0%} |")
    A("")

    A("### 4.2 Displaced-Core (B) Orbits — Top-20 per Pair (threshold definition)\n")
    for pid, cands in p4_displaced.items():
        A(f"#### {pid}\n")
        if not cands:
            A("_No (B) orbits found._\n")
            continue
        A("| id | type | stability | p_type | top_neighbor | displacement | topic_domain |")
        A("|----|------|-----------|--------|-------------|-------------|--------------|")
        for c in cands[:20]:
            top_nb = c.get("displacement_dominant_neighbor") or "—"
            disp   = (c.get("displacement_type") or "—").replace("_", " ")
            A(f"| {c['id']} | {c['classification'] or '?'} | {c['stability']:.3f} | "
              f"{c['p_type_in_slice']:.3f} | {top_nb} | {disp} | "
              f"{(c['topic_domain'] or '')[:40]} |")
        A("")

    # ---- Section 5: Detector correlation ----
    A("## 5. Detector Correlation\n")
    A("### 5.1 Per-Detector Enrichment\n")
    A("| detector | corpus_count | orbit_count | corpus_frac | orbit_frac | enrichment |")
    A("|----------|--------------|-------------|-------------|------------|------------|")
    for det, v in sorted(p5["enrichment"].items(), key=lambda x: -x[1]["enrichment"]):
        A(f"| {det} | {v['corpus_count']} | {v['orbit_count']} | "
          f"{v['corpus_frac']:.3f} | {v['orbit_frac']:.3f} | **{v['enrichment']:.2f}** |")
    A("")

    A("### 5.2 Detector Intersection (orbit candidates)\n")
    A("| intersection | count |")
    A("|--------------|-------|")
    for k, v in sorted(p5["intersection"].items(), key=lambda x: -x[1]):
        A(f"| {k} | {v} |")
    A("")

    A(f"**Un-flagged orbit candidates** (in no detector): "
      f"**{p5['n_unflagged']}** of {p5['n_orbit_union']} "
      f"({p5['n_unflagged']/max(p5['n_orbit_union'],1):.1%})\n")
    A("| id | appearances | h1_band | signature | topic_domain |")
    A("|----|-------------|---------|-----------|--------------|")
    for u in p5["unflagged_orbits"][:20]:
        sig = u.get("signature") or "—"
        A(f"| {u['id']} | {u['appearances_in_orbit_lists']} | {u['h1_band']} | "
          f"{sig} | {(u['topic_domain'] or '')[:50]} |")
    A("")

    # ---- Section 6: Structural observations ----
    A("## 6. Structural Observations and Paper 2 Implications\n")

    # Coupling vs beneficiary topology
    coupling_pairs    = [p for p in bimodal_pairs if p["topology"] == "coupling"]
    beneficiary_pairs = [p for p in bimodal_pairs if p["topology"] == "beneficiary"]
    A(f"**Topology distribution:** {len(coupling_pairs)} coupling, "
      f"{len(beneficiary_pairs)} beneficiary, "
      f"0 semantic bimodal pairs. The orbit framework is most visible in the "
      f"contamination/coupling network. Paper 2 should ground its primary topology in "
      f"the coupling network, treating the beneficiary network as secondary evidence.\n")

    # Recurrence pattern for organized/U_4_glob/org_nat
    A("**Cross-slice recurrence (organized, U_4_glob, org_nat slices):** "
      f"The 7×7 Jaccard matrix has mean off-diagonal "
      f"{np.mean(offdiag):.4f}. "
    )
    same_slice_pairs = [
        (i, j) for i in range(len(labels)) for j in range(i+1, len(labels))
        if labels[i].split("/")[0] == labels[j].split("/")[0]
    ]
    if same_slice_pairs:
        same_slice_jac = np.mean([p3["matrix"][i][j] for i, j in same_slice_pairs])
        diff_slice_jac = np.mean([p3["matrix"][i][j]
                                  for i in range(len(labels)) for j in range(i+1, len(labels))
                                  if labels[i].split("/")[0] != labels[j].split("/")[0]])
        A(f"Same-slice cross-topology Jaccard mean: {same_slice_jac:.4f} vs "
          f"different-slice mean: {diff_slice_jac:.4f}. "
          f"{'Same-slice pairs share more orbit members, suggesting per-slice structural causes dominate.' if same_slice_jac > diff_slice_jac else 'Orbit membership is primarily topology-driven, not slice-driven.'}\n")
    else:
        A("")

    # A/B split
    all_b_dom   = sum(v["dominant_def"]["B_displaced_core"] for v in p4_fractions.values())
    all_b_thr   = sum(v["threshold_def"]["B_displaced_core"] for v in p4_fractions.values())
    total_orbit = sum(v["n_orbit"] for v in p4_fractions.values())
    A(f"**(A)/(B) split:** Under the dominant-only definition, "
      f"**{all_b_dom/total_orbit:.0%}** of orbits are (B) displaced-core. "
      f"Under the threshold definition (P > {AB_THRESHOLD}), "
      f"**{all_b_thr/total_orbit:.0%}** are (B). "
    )
    if all_b_thr / max(total_orbit, 1) >= 0.20:
        A("The (B) fraction is substantial enough to support Paper 2's orbit framework as a "
          "topological phenomenon distinct from type minority. "
          "Orbits are not merely rare-type constraints; they include mainstream-classified "
          "constraints displaced in their network neighborhoods.\n")
    else:
        A("The (B) fraction is small; the orbit population is dominated by (A) minority-typed "
          "constraints. The orbit framework largely recapitulates existing type-minority "
          "structure. Paper 2 should qualify the orbit framing accordingly.\n")

    # Un-flagged orbits
    unflagged_frac = p5["n_unflagged"] / max(p5["n_orbit_union"], 1)
    A(f"**Un-flagged orbits:** {p5['n_unflagged']} ({unflagged_frac:.1%} of orbit union) "
      f"are not captured by h1_band, FCR, FSM, beneficiary, or multi_slice_typed detectors. "
    )
    if unflagged_frac >= 0.10:
        A("This is a meaningful set — the orbit framework surfaces structural anomalies "
          "that the apparatus's existing detectors miss. These are the strongest case "
          "material for Paper 2: orbit membership as a distinct diagnostic, not a "
          "restatement of existing detection.\n")
    else:
        A("Most orbit candidates are already flagged by existing detectors. "
          "The orbit framework's independent diagnostic contribution is limited, "
          "and Paper 2 should position it as complementary to rather than independent of "
          "existing apparatus detectors.\n")

    OUT_MD.write_text("\n".join(lines))
    print(f"Wrote {OUT_MD} ({OUT_MD.stat().st_size // 1024} KB)")


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    (constraints_idx, exploration, working_slices, explicit_adj,
     type_maps, multi_slice_typed_ids, bimodal_pairs) = load_data()

    if not bimodal_pairs:
        print("ERROR: no bimodal pairs found in idea_site_exploration.json pass2")
        return

    bimodal_pairs = pass1_extract_orbits(
        bimodal_pairs, constraints_idx, type_maps, explicit_adj, multi_slice_typed_ids
    )
    type_enrichment, topic_enrichment = pass2_type_and_domain(bimodal_pairs)
    p3 = pass3_cross_slice_overlap(bimodal_pairs, constraints_idx)
    p4_fractions, p4_displaced = pass4_asymmetry(bimodal_pairs)
    p5 = pass5_detectors(bimodal_pairs, constraints_idx, multi_slice_typed_ids)

    write_json(bimodal_pairs, type_enrichment, topic_enrichment, p3,
               p4_fractions, p4_displaced, p5, multi_slice_typed_ids)
    write_md(bimodal_pairs, type_enrichment, topic_enrichment, p3,
             p4_fractions, p4_displaced, p5)

    print("\nDone.")


if __name__ == "__main__":
    main()
