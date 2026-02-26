"""Phase 3: FCA Gate Compression.

Builds boolean gate matrix, computes GF(2) rank, formal concept lattice,
and type-relative reducts.
"""

import csv
import sys
from collections import defaultdict
from pathlib import Path

import numpy as np

ROOT = Path(__file__).resolve().parent.parent
if str(ROOT / "python") not in sys.path:
    sys.path.insert(0, str(ROOT / "python"))

from shared.constants import MAXENT_TYPES
from .phase0_data import CONTEXTS


def build_gate_vector(c, config, all_signatures):
    """Build the boolean gate vector for one constraint.

    Returns (gate_values: list[bool], gate_names: list[str]).
    """
    gates = []
    names = []
    eps = c["epsilon"]
    supp = c["suppression"]
    theater = c["theater_ratio"]
    chi = c["chi"]
    h1 = c["h1"]

    # --- Core booleans ---
    gates.append(c["emerges_naturally"])
    names.append("emerges_naturally")

    gates.append(c["requires_active_enforcement"])
    names.append("requires_active_enforcement")

    has_coord = len(c["beneficiaries"]) > 0
    gates.append(has_coord)
    names.append("has_coordination_function")

    has_asym = len(c["victims"]) > 0
    gates.append(has_asym)
    names.append("has_asymmetric_extraction")

    nat_law_no_ben = (c["emerges_naturally"] and not c["requires_active_enforcement"]
                      and not has_coord)
    gates.append(nat_law_no_ben)
    names.append("natural_law_without_beneficiary")

    # --- Signature one-hot ---
    sig = c.get("signature", "")
    for s in all_signatures:
        gates.append(sig == s)
        names.append(f"sig_{s}")

    # --- Metric threshold gates (context-independent) ---
    mountain_max = config.get("mountain_extractiveness_max", 0.25)
    rope_eps_ceil = config.get("rope_epsilon_ceiling", 0.45)
    snare_eps_floor = config.get("snare_epsilon_floor", 0.46)
    tr_eps_floor = config.get("tangled_rope_epsilon_floor", 0.30)
    mountain_supp = config.get("mountain_suppression_ceiling", 0.05)
    enforcement_supp = 0.40  # tangled_rope suppression threshold
    snare_supp = 0.60
    piton_theater = config.get("piton_theater_floor", 0.70)
    rope_chi_ceil = config.get("rope_chi_ceiling", 0.35)
    snare_chi_floor = config.get("snare_chi_floor", 0.66)
    tr_chi_floor = config.get("tangled_rope_chi_floor", 0.40)
    tr_chi_ceil = config.get("tangled_rope_chi_ceil", 0.90)

    gates.append(eps <= mountain_max)
    names.append("eps_le_mountain_max")

    gates.append(eps <= rope_eps_ceil)
    names.append("eps_le_rope_ceil")

    gates.append(eps >= snare_eps_floor)
    names.append("eps_ge_snare_floor")

    gates.append(eps >= tr_eps_floor)
    names.append("eps_ge_tr_floor")

    gates.append(supp <= mountain_supp)
    names.append("supp_le_mountain")

    gates.append(supp >= enforcement_supp)
    names.append("supp_ge_enforcement")

    gates.append(supp >= snare_supp)
    names.append("supp_ge_snare")

    gates.append(theater >= piton_theater)
    names.append("theater_ge_piton")

    # --- Per-context chi threshold gates ---
    for j, ctx in enumerate(CONTEXTS):
        chi_j = chi[j]

        gates.append(chi_j <= rope_chi_ceil)
        names.append(f"chi_{ctx}_le_rope")

        gates.append(chi_j >= snare_chi_floor)
        names.append(f"chi_{ctx}_ge_snare")

        gates.append(tr_chi_floor <= chi_j <= tr_chi_ceil)
        names.append(f"chi_{ctx}_in_tr_range")

    # --- Cohomological gates ---
    gates.append(h1 == 0)
    names.append("h1_eq_0")

    gates.append(h1 >= 5)
    names.append("h1_ge_5")

    return gates, names


def build_gate_matrix(constraints, config):
    """Build the N x M boolean gate matrix.

    Returns (matrix: np.ndarray[bool], constraint_ids: list, gate_names: list).
    """
    # Discover all signatures in corpus
    all_signatures = sorted(set(c.get("signature", "") for c in constraints) - {""})

    first_gates, gate_names = build_gate_vector(constraints[0], config, all_signatures)
    N = len(constraints)
    M = len(gate_names)

    matrix = np.zeros((N, M), dtype=bool)
    matrix[0, :] = first_gates
    ids = [constraints[0]["id"]]

    for i in range(1, N):
        gates, _ = build_gate_vector(constraints[i], config, all_signatures)
        matrix[i, :] = gates
        ids.append(constraints[i]["id"])

    return matrix, ids, gate_names


def gf2_rank_analysis(matrix):
    """Compute GF(2) rank and null space dimension.

    Returns dict with rank, null_dim, and independent gate indices.
    """
    import galois

    GF2 = galois.GF(2)
    M_gf2 = GF2(matrix.astype(int))
    rank = int(np.linalg.matrix_rank(M_gf2))
    null_dim = matrix.shape[1] - rank

    return {
        "rank": rank,
        "null_dim": null_dim,
        "n_rows": matrix.shape[0],
        "n_cols": matrix.shape[1],
    }


def concept_lattice_analysis(matrix, constraint_ids, gate_names):
    """Build FCA concept lattice using the concepts library.

    Returns dict with concept count and key concepts.
    """
    import concepts

    # Build objects/properties definition for concepts library
    # concepts.Context expects: list of (object_name, [property_names...])
    objects = []
    for i, cid in enumerate(constraint_ids):
        props = frozenset(gate_names[j] for j in range(len(gate_names)) if matrix[i, j])
        objects.append((cid, props))

    # Build the context using the Definition class
    try:
        ctx = concepts.Context(
            objects=constraint_ids,
            properties=gate_names,
            bools=[list(row) for row in matrix],
        )
    except Exception as e:
        return {"error": str(e), "concept_count": -1}

    lattice = ctx.lattice

    # Concept count
    n_concepts = len(lattice)

    # If tractable, get concept details
    key_concepts = []
    if n_concepts < 2000:
        for concept in lattice:
            extent = concept.extent
            intent = concept.intent
            if len(extent) > 0 and len(intent) > 0:
                key_concepts.append({
                    "extent_size": len(extent),
                    "intent_size": len(intent),
                    "intent_gates": list(intent)[:10],  # First 10 for display
                    "extent_sample": list(extent)[:5],
                })
        # Sort by extent_size descending
        key_concepts.sort(key=lambda x: x["extent_size"], reverse=True)

    # Try to generate Graphviz
    dot_str = ""
    try:
        gv = lattice.graphviz()
        dot_str = gv.source if hasattr(gv, "source") else str(gv)
    except Exception:
        pass

    return {
        "concept_count": n_concepts,
        "n_key_concepts": len(key_concepts),
        "top_concepts": key_concepts[:20],
        "dot_str": dot_str,
    }


def type_relative_reducts(matrix, gate_names, constraints):
    """For each pair of types (analytical perspective), find separating gates.

    Returns dict mapping type_pair -> {core_gates, reduct_size, n_separating}.
    """
    # Group constraints by analytical type
    type_indices = defaultdict(list)
    for i, c in enumerate(constraints):
        # Use analytical perspective type as the "ground truth" type
        atype = c["types"][3]  # analytical = index 3
        type_indices[atype].append(i)

    # For each pair of types
    results = {}
    types_present = sorted(type_indices.keys())

    for t1_idx, t1 in enumerate(types_present):
        for t2 in types_present[t1_idx + 1:]:
            indices1 = type_indices[t1]
            indices2 = type_indices[t2]

            if not indices1 or not indices2:
                continue

            # Find gates that separate: gate is True for all of one type and False for all of other (or vice versa)
            separating = []
            for g in range(len(gate_names)):
                vals1 = set(matrix[indices1, g])
                vals2 = set(matrix[indices2, g])

                # Gate separates if one type is all True and other is all False
                if vals1 == {True} and vals2 == {False}:
                    separating.append(gate_names[g])
                elif vals1 == {False} and vals2 == {True}:
                    separating.append(gate_names[g])

            # Partial separation: gate differs for >90% of pairs
            partial_sep = []
            for g in range(len(gate_names)):
                mean1 = np.mean(matrix[indices1, g])
                mean2 = np.mean(matrix[indices2, g])
                if abs(mean1 - mean2) > 0.8:
                    partial_sep.append((gate_names[g], float(abs(mean1 - mean2))))

            results[f"{t1}_vs_{t2}"] = {
                "n_type1": len(indices1),
                "n_type2": len(indices2),
                "n_perfect_separating": len(separating),
                "perfect_separating_gates": separating,
                "n_partial_separating": len(partial_sep),
                "top_partial": sorted(partial_sep, key=lambda x: -x[1])[:10],
            }

    return results


def run_phase3(data, output_dir):
    """Run the complete Phase 3 FCA analysis.

    Returns dict with all results for the report.
    """
    output_dir = Path(output_dir)
    constraints = data["constraints"]
    config = data["config"]

    # Build gate matrix
    matrix, constraint_ids, gate_names = build_gate_matrix(constraints, config)

    # Column statistics
    col_sums = matrix.sum(axis=0)
    col_stats = {gate_names[j]: {"true_count": int(col_sums[j]),
                                  "true_pct": float(col_sums[j]) / len(constraints) * 100}
                 for j in range(len(gate_names))}

    # Remove constant columns for analysis (all True or all False)
    non_const = [j for j in range(len(gate_names))
                 if 0 < col_sums[j] < len(constraints)]
    const_gates = [gate_names[j] for j in range(len(gate_names)) if j not in non_const]
    matrix_reduced = matrix[:, non_const]
    gate_names_reduced = [gate_names[j] for j in non_const]

    # GF(2) rank
    gf2_result = gf2_rank_analysis(matrix_reduced)

    # FCA concept lattice
    fca_result = concept_lattice_analysis(matrix_reduced, constraint_ids, gate_names_reduced)

    # Type-relative reducts
    reduct_result = type_relative_reducts(matrix, gate_names, constraints)

    # --- Save outputs ---
    # gate_matrix.csv
    with open(output_dir / "gate_matrix.csv", "w", newline="") as f:
        w = csv.writer(f)
        w.writerow(["constraint_id"] + gate_names)
        for i, cid in enumerate(constraint_ids):
            w.writerow([cid] + [int(v) for v in matrix[i]])

    # gf2_rank.txt
    with open(output_dir / "gf2_rank.txt", "w") as f:
        f.write(f"Gate matrix dimensions: {matrix_reduced.shape[0]} x {matrix_reduced.shape[1]}\n")
        f.write(f"Total gates: {len(gate_names)}\n")
        f.write(f"Non-constant gates: {len(gate_names_reduced)}\n")
        f.write(f"Constant gates (removed): {const_gates}\n")
        f.write(f"GF(2) rank: {gf2_result['rank']}\n")
        f.write(f"Null space dimension: {gf2_result['null_dim']}\n")

    # FCA lattice Graphviz
    if fca_result.get("dot_str"):
        with open(output_dir / "fca_lattice.gv", "w") as f:
            f.write(fca_result["dot_str"])

    # fca_concepts.csv
    if fca_result.get("top_concepts"):
        with open(output_dir / "fca_concepts.csv", "w", newline="") as f:
            w = csv.writer(f)
            w.writerow(["extent_size", "intent_size", "intent_gates_sample"])
            for c in fca_result["top_concepts"]:
                w.writerow([c["extent_size"], c["intent_size"],
                            "; ".join(c["intent_gates"])])

    return {
        "n_total_gates": len(gate_names),
        "n_non_constant": len(gate_names_reduced),
        "constant_gates": const_gates,
        "col_stats": col_stats,
        "gf2": gf2_result,
        "fca": {
            "concept_count": fca_result.get("concept_count", -1),
            "n_key_concepts": fca_result.get("n_key_concepts", 0),
            "top_concepts": fca_result.get("top_concepts", [])[:10],
            "error": fca_result.get("error"),
        },
        "reducts": reduct_result,
    }
