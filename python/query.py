#!/usr/bin/env python3
"""CLI query engine for the structural dynamics corpus.

Loads corpus_data.json, pipeline_output.json, and enriched_omega_data.json
into a single DataFrame and supports filtering, aggregation, and detail views.

 python3 python/query.py --count                                                                 # ~1034
 python3 python/query.py --perspective powerless snare --perspective institutional rope --count  # >0
 python3 python/query.py --perspective powerless snare --perspective institutional rope --table  # constraint details
 python3 python/query.py --domain mathematical --types                                           # mostly mountain
 python3 python/query.py --type tangled_rope --count                                             # ~663
 python3 python/query.py --detail moltbook_agent_theater                                         # full profile
 python3 python/query.py --omega-severity critical --count                                       # >0
 python3 python/query.py --emerges-naturally --count                                             # >0
 python3 python/query.py --min-extractiveness 0.9 --table                                        # high-extraction constraints
 python3 python/query.py --type snare --domains                                                  # domain distribution

"""

import argparse
import json
import sys
from collections import Counter
from pathlib import Path

import pandas as pd

OUTPUT_DIR = Path(__file__).resolve().parent.parent / "outputs"

VALID_PERSPECTIVES = ["powerless", "moderate", "institutional", "analytical"]
VALID_PURITY_BANDS = ["pristine", "sound", "borderline", "degraded", "contaminated"]
VALID_OMEGA_SEVERITIES = ["critical", "high", "medium", "low"]


# ---------------------------------------------------------------------------
# Data loading
# ---------------------------------------------------------------------------

def load_json(path, label):
    """Load a JSON file, returning {} on failure."""
    try:
        with open(path, "r", encoding="utf-8") as f:
            return json.load(f)
    except (FileNotFoundError, json.JSONDecodeError) as e:
        print(f"Warning: Could not load {label} ({path}): {e}", file=sys.stderr)
        return {}


def format_orbit_signature(signature):
    """Format an orbit signature list for display."""
    if not signature:
        return "N/A"
    return "[" + ", ".join(signature) + "]"


def build_dataframe():
    """Build a unified DataFrame from the three JSON sources.

    Returns (df, severity_dict, pipeline_raw, omega_raw).
    """
    # --- Step A: corpus_data.json as base ---
    corpus_raw = load_json(OUTPUT_DIR / "corpus_data.json", "corpus_data")
    constraints = corpus_raw.get("constraints", {})

    # --- Step B: pipeline_output.json ---
    pipeline_raw = load_json(OUTPUT_DIR / "pipeline_output.json", "pipeline_output")
    type_hierarchy = pipeline_raw.get("type_hierarchy", {})
    severity_dict = {name: entry["severity"] for name, entry in type_hierarchy.items()}
    valid_types = sorted(type_hierarchy.keys())

    per_constraint = pipeline_raw.get("per_constraint", [])
    pipeline_by_id = {entry["id"]: entry for entry in per_constraint}

    # --- Step C: enriched_omega_data.json ---
    omega_raw = load_json(OUTPUT_DIR / "enriched_omega_data.json", "enriched_omega_data")
    omegas_list = omega_raw.get("omegas", [])

    # Group omegas by associated_constraint
    omegas_by_constraint = {}
    for omega in omegas_list:
        cid = omega.get("associated_constraint")
        if cid:
            omegas_by_constraint.setdefault(cid, []).append(omega)

    # --- Step D: Build rows ---
    all_ids = set(constraints.keys()) | set(pipeline_by_id.keys())
    rows = []

    for cid in sorted(all_ids):
        row = {"id": cid}

        # Corpus data
        cdata = constraints.get(cid, {})
        metrics = cdata.get("metrics", {})
        analysis = cdata.get("analysis", {})

        row["claimed_type"] = cdata.get("claimed_type")
        row["domain"] = cdata.get("domain")
        row["human_readable"] = cdata.get("human_readable")
        row["extractiveness"] = metrics.get("extractiveness")
        row["suppression"] = metrics.get("suppression")
        row["resistance"] = metrics.get("resistance")
        row["emerges_naturally"] = metrics.get("emerges_naturally")
        row["requires_enforcement"] = metrics.get("requires_enforcement")
        row["structural_signature"] = analysis.get("structural_signature")
        row["variance_ratio"] = analysis.get("variance_ratio")
        row["orbit_signature"] = analysis.get("orbit_signature")
        row["orbit_contexts"] = analysis.get("orbit_contexts")
        row["is_constructed"] = analysis.get("is_constructed")
        row["index_configs"] = analysis.get("index_configs")
        row["types_produced"] = analysis.get("types_produced")
        row["beneficiaries"] = cdata.get("beneficiaries", [])
        row["victims"] = cdata.get("victims", [])

        # Pipeline data (overrides/supplements corpus)
        pdata = pipeline_by_id.get(cid, {})
        if pdata:
            row["claimed_type"] = pdata.get("claimed_type", row["claimed_type"])
            # Prefer topic_domain (subject area) over domain (classification type)
            row["domain"] = pdata.get("topic_domain") or pdata.get("domain", row["domain"])
            row["human_readable"] = pdata.get("human_readable", row["human_readable"])
            row["purity_score"] = pdata.get("purity_score")
            row["purity_band"] = pdata.get("purity_band")
            row["signature"] = pdata.get("signature")
            row["theater_ratio"] = pdata.get("theater_ratio")
            row["emerges_naturally"] = pdata.get("emerges_naturally", row["emerges_naturally"])
            row["requires_enforcement"] = pdata.get("requires_active_enforcement", row["requires_enforcement"])

            coupling = pdata.get("coupling", {})
            row["coupling_category"] = coupling.get("category")
            row["coupling_score"] = coupling.get("score")
            row["coupling_boltzmann"] = coupling.get("boltzmann")

            perspectives = pdata.get("perspectives", {})
            for p in VALID_PERSPECTIVES:
                row[f"persp_{p}"] = perspectives.get(p)

            row["beneficiaries"] = pdata.get("beneficiaries", row["beneficiaries"])
            row["victims"] = pdata.get("victims", row["victims"])
        else:
            row["purity_score"] = None
            row["purity_band"] = None
            row["signature"] = None
            row["theater_ratio"] = None
            row["coupling_category"] = None
            row["coupling_score"] = None
            row["coupling_boltzmann"] = None
            for p in VALID_PERSPECTIVES:
                row[f"persp_{p}"] = None

        # Omega data
        constraint_omegas = omegas_by_constraint.get(cid, [])
        row["omega_count"] = len(constraint_omegas)
        row["has_omega"] = len(constraint_omegas) > 0

        if constraint_omegas:
            scores = [o.get("severity_score", 0) for o in constraint_omegas]
            max_idx = scores.index(max(scores))
            row["omega_max_severity_score"] = scores[max_idx]
            row["omega_max_severity"] = constraint_omegas[max_idx].get("severity")
            gap_classes = [o.get("gap_class") for o in constraint_omegas if o.get("gap_class")]
            row["omega_dominant_gap_class"] = Counter(gap_classes).most_common(1)[0][0] if gap_classes else None
            row["omega_severities"] = {o.get("severity") for o in constraint_omegas if o.get("severity")}
        else:
            row["omega_max_severity_score"] = None
            row["omega_max_severity"] = None
            row["omega_dominant_gap_class"] = None
            row["omega_severities"] = set()

        # Derived display columns
        row["orbit_display"] = format_orbit_signature(row["orbit_signature"])
        row["beneficiaries_str"] = ", ".join(row["beneficiaries"]) if row["beneficiaries"] else ""
        row["victims_str"] = ", ".join(row["victims"]) if row["victims"] else ""

        rows.append(row)

    df = pd.DataFrame(rows)
    return df, severity_dict, pipeline_raw, omega_raw


# ---------------------------------------------------------------------------
# Argument parsing
# ---------------------------------------------------------------------------

def build_parser():
    """Build the argument parser."""
    parser = argparse.ArgumentParser(
        description="Query the structural dynamics corpus.",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""\
examples:
  %(prog)s --count
  %(prog)s --perspective powerless snare --perspective institutional rope --table
  %(prog)s --domain mathematical --types
  %(prog)s --detail moltbook_agent_theater
  %(prog)s --type snare --min-extractiveness 0.8 --table
  %(prog)s --omega-severity critical --count
""",
    )

    # --- Filters ---
    filters = parser.add_argument_group("filters (AND-combined)")
    filters.add_argument("--type", dest="type_filter",
                         help="Filter by claimed type (e.g. snare, rope, mountain)")
    filters.add_argument("--domain", help="Filter by domain (substring match)")
    filters.add_argument("--perspective", nargs=2, action="append",
                         metavar=("LEVEL", "TYPE"),
                         help="Filter by perspective (e.g. --perspective powerless snare)")
    filters.add_argument("--orbit-contains",
                         help="Filter to constraints whose orbit contains TYPE")
    filters.add_argument("--purity-band", choices=VALID_PURITY_BANDS,
                         help="Filter by purity band")
    filters.add_argument("--signature", help="Filter by signature (substring match)")
    filters.add_argument("--min-extractiveness", type=float)
    filters.add_argument("--max-extractiveness", type=float)
    filters.add_argument("--min-suppression", type=float)
    filters.add_argument("--max-suppression", type=float)
    filters.add_argument("--min-purity", type=float)
    filters.add_argument("--max-purity", type=float)
    filters.add_argument("--min-variance", type=float)
    filters.add_argument("--max-variance", type=float)
    filters.add_argument("--has-omega", action="store_true",
                         help="Only constraints with omegas")
    filters.add_argument("--omega-severity", choices=VALID_OMEGA_SEVERITIES,
                         help="Filter by omega severity (matches any omega, not just max)")
    filters.add_argument("--emerges-naturally", action="store_true",
                         help="Only constraints that emerge naturally")
    filters.add_argument("--requires-enforcement", action="store_true",
                         help="Only constraints that require active enforcement")

    # --- Output modes ---
    modes = parser.add_argument_group("output modes (mutually exclusive)")
    mode = modes.add_mutually_exclusive_group()
    mode.add_argument("--list", action="store_true",
                      help="List constraint IDs (default)")
    mode.add_argument("--count", action="store_true",
                      help="Print count of matching constraints")
    mode.add_argument("--table", action="store_true",
                      help="Show tabular output")
    mode.add_argument("--detail", metavar="ID",
                      help="Show full detail for a constraint")
    mode.add_argument("--aggregate", metavar="FIELD",
                      help="Show summary statistics for a numeric field")
    mode.add_argument("--json", action="store_true", dest="json_output",
                      help="Output matching constraints as JSON")
    mode.add_argument("--domains", action="store_true",
                      help="Show domain value counts")
    mode.add_argument("--types", action="store_true",
                      help="Show type value counts")
    mode.add_argument("--orbits", action="store_true",
                      help="Show orbit signature value counts")

    return parser


# ---------------------------------------------------------------------------
# Filtering
# ---------------------------------------------------------------------------

def apply_filters(df, args):
    """Apply all filters to the DataFrame, returning the filtered copy."""
    mask = pd.Series(True, index=df.index)

    if args.type_filter:
        mask &= df["claimed_type"] == args.type_filter

    if args.domain:
        mask &= df["domain"].str.contains(args.domain, case=False, na=False)

    if args.perspective:
        for level, typ in args.perspective:
            col = f"persp_{level}"
            if col in df.columns:
                mask &= df[col] == typ
            else:
                print(f"Warning: unknown perspective level '{level}'", file=sys.stderr)

    if args.orbit_contains:
        target = args.orbit_contains
        mask &= df["orbit_signature"].apply(
            lambda s: target in s if isinstance(s, list) else False
        )

    if args.purity_band:
        mask &= df["purity_band"] == args.purity_band

    if args.signature:
        mask &= df["signature"].str.contains(args.signature, case=False, na=False)

    if args.min_extractiveness is not None:
        mask &= df["extractiveness"] >= args.min_extractiveness
    if args.max_extractiveness is not None:
        mask &= df["extractiveness"] <= args.max_extractiveness

    if args.min_suppression is not None:
        mask &= df["suppression"] >= args.min_suppression
    if args.max_suppression is not None:
        mask &= df["suppression"] <= args.max_suppression

    if args.min_purity is not None:
        mask &= df["purity_score"] >= args.min_purity
    if args.max_purity is not None:
        mask &= df["purity_score"] <= args.max_purity

    if args.min_variance is not None:
        mask &= df["variance_ratio"] >= args.min_variance
    if args.max_variance is not None:
        mask &= df["variance_ratio"] <= args.max_variance

    if args.has_omega:
        mask &= df["has_omega"]

    if args.omega_severity:
        sev = args.omega_severity
        mask &= df["omega_severities"].apply(
            lambda s: sev in s if isinstance(s, set) else False
        )

    if args.emerges_naturally:
        mask &= df["emerges_naturally"] == True  # noqa: E712

    if args.requires_enforcement:
        mask &= df["requires_enforcement"] == True  # noqa: E712

    return df[mask].copy()


# ---------------------------------------------------------------------------
# Output functions
# ---------------------------------------------------------------------------

def output_list(df):
    """Print one line per constraint: id, claimed_type, domain."""
    for _, row in df.sort_values("id").iterrows():
        print(f"{row['id']}  {row.get('claimed_type', '')}  {row.get('domain', '')}")
    print(f"\n{len(df)} constraints", file=sys.stderr)


def output_count(df):
    """Print the count of matching constraints."""
    print(len(df))


def output_table(df):
    """Print a tabular view of constraints."""
    cols = ["id", "human_readable", "claimed_type", "domain",
            "extractiveness", "suppression", "purity_band", "orbit_display"]
    available = [c for c in cols if c in df.columns]
    out = df.sort_values("id")[available].copy()
    if "human_readable" in out.columns:
        out["human_readable"] = out["human_readable"].apply(
            lambda x: (x[:40] + "...") if isinstance(x, str) and len(x) > 40 else x
        )
    print(out.to_string(index=False))
    print(f"\n{len(df)} constraints", file=sys.stderr)


def output_detail(df, constraint_id, pipeline_raw, omega_raw):
    """Print full detail for a single constraint."""
    matches = df[df["id"] == constraint_id]
    if matches.empty:
        print(f"Error: constraint '{constraint_id}' not found.", file=sys.stderr)
        sys.exit(1)

    row = matches.iloc[0]

    # --- Identity ---
    print("=" * 70)
    print(f"  Constraint: {row['id']}")
    print("=" * 70)
    print(f"  Human Readable : {row.get('human_readable', 'N/A')}")
    print(f"  Claimed Type   : {row.get('claimed_type', 'N/A')}")
    print(f"  Domain         : {row.get('domain', 'N/A')}")
    print(f"  Signature      : {row.get('signature', 'N/A')}")
    print(f"  Is Constructed : {row.get('is_constructed', 'N/A')}")

    # --- Metrics ---
    print()
    print("  Metrics")
    print("  " + "-" * 40)
    print(f"  Extractiveness        : {row.get('extractiveness', 'N/A')}")
    print(f"  Suppression           : {row.get('suppression', 'N/A')}")
    print(f"  Theater Ratio         : {row.get('theater_ratio', 'N/A')}")
    print(f"  Emerges Naturally     : {row.get('emerges_naturally', 'N/A')}")
    print(f"  Requires Enforcement  : {row.get('requires_enforcement', 'N/A')}")

    # --- Purity ---
    print()
    print("  Purity")
    print("  " + "-" * 40)
    print(f"  Score : {row.get('purity_score', 'N/A')}")
    print(f"  Band  : {row.get('purity_band', 'N/A')}")

    # --- Coupling ---
    print()
    print("  Coupling")
    print("  " + "-" * 40)
    print(f"  Category  : {row.get('coupling_category', 'N/A')}")
    print(f"  Score     : {row.get('coupling_score', 'N/A')}")
    print(f"  Boltzmann : {row.get('coupling_boltzmann', 'N/A')}")

    # --- Perspectives ---
    print()
    print("  Perspectives")
    print("  " + "-" * 40)
    for p in VALID_PERSPECTIVES:
        val = row.get(f"persp_{p}", "N/A")
        print(f"  {p:15s}: {val}")

    # --- Orbit ---
    print()
    print("  Orbit")
    print("  " + "-" * 40)
    print(f"  Signature      : {row.get('orbit_display', 'N/A')}")
    sig = row.get("orbit_signature")
    print(f"  Span           : {len(sig) if isinstance(sig, list) else 'N/A'}")
    print(f"  Variance Ratio : {row.get('variance_ratio', 'N/A')}")
    print(f"  Index Configs  : {row.get('index_configs', 'N/A')}")
    print(f"  Types Produced : {row.get('types_produced', 'N/A')}")

    # --- Beneficiaries / Victims ---
    print()
    print("  Beneficiaries / Victims")
    print("  " + "-" * 40)
    print(f"  Beneficiaries : {row.get('beneficiaries_str', 'N/A') or 'none'}")
    print(f"  Victims       : {row.get('victims_str', 'N/A') or 'none'}")

    # --- Omegas (from raw data) ---
    omega_list = omega_raw.get("omegas", [])
    constraint_omegas = [o for o in omega_list if o.get("associated_constraint") == constraint_id]
    print()
    print(f"  Omegas ({len(constraint_omegas)})")
    print("  " + "-" * 40)
    if constraint_omegas:
        for o in constraint_omegas:
            print(f"  - {o.get('name', 'unknown')}")
            print(f"    Severity : {o.get('severity', 'N/A')} (score: {o.get('severity_score', 'N/A')})")
            print(f"    Gap Class: {o.get('gap_class', 'N/A')}")
            print(f"    Family   : {o.get('family', 'N/A')}")
            q = o.get("question", "")
            if q:
                print(f"    Question : {q}")
    else:
        print("  (none)")

    # --- Gaps (from pipeline raw) ---
    pipeline_by_id = {e["id"]: e for e in pipeline_raw.get("per_constraint", [])}
    pdata = pipeline_by_id.get(constraint_id, {})
    gaps = pdata.get("gaps", [])
    print()
    print(f"  Gaps ({len(gaps)})")
    print("  " + "-" * 40)
    if gaps:
        for g in gaps:
            print(f"  - {g.get('gap_type', 'unknown')}: "
                  f"powerless={g.get('powerless_type', '?')} / "
                  f"institutional={g.get('institutional_type', '?')}")
    else:
        print("  (none)")

    # --- Classifications (from pipeline raw) ---
    classifications = pdata.get("classifications", [])
    print()
    print(f"  Classifications ({len(classifications)})")
    print("  " + "-" * 40)
    if classifications:
        for c in classifications:
            ctx = c.get("context", {})
            print(f"  - {c.get('type', '?'):15s}  "
                  f"power={ctx.get('agent_power', '?')}, "
                  f"horizon={ctx.get('time_horizon', '?')}, "
                  f"exit={ctx.get('exit_options', '?')}, "
                  f"scope={ctx.get('spatial_scope', '?')}")
    else:
        print("  (none)")

    print()


def output_aggregate(df, field):
    """Print summary statistics for a numeric field."""
    if field not in df.columns:
        print(f"Error: field '{field}' not found in DataFrame.", file=sys.stderr)
        print(f"Available numeric fields: {list(df.select_dtypes('number').columns)}",
              file=sys.stderr)
        sys.exit(1)

    col = df[field]
    if not pd.api.types.is_numeric_dtype(col):
        print(f"Error: field '{field}' is not numeric. Use --domains/--types/--orbits "
              f"for categorical fields.", file=sys.stderr)
        sys.exit(1)

    stats = col.describe()
    print(f"Aggregate: {field}")
    print("-" * 30)
    for label, value in stats.items():
        print(f"  {label:6s}: {value:.4f}" if isinstance(value, float) else f"  {label:6s}: {value}")


def output_json(df):
    """Output matching constraints as JSON."""
    out = df.copy()
    # Convert non-serializable columns
    if "omega_severities" in out.columns:
        out["omega_severities"] = out["omega_severities"].apply(
            lambda s: sorted(s) if isinstance(s, set) else []
        )
    if "orbit_signature" in out.columns:
        out["orbit_signature"] = out["orbit_signature"].apply(
            lambda s: s if isinstance(s, list) else None
        )
    if "beneficiaries" in out.columns:
        out["beneficiaries"] = out["beneficiaries"].apply(
            lambda s: s if isinstance(s, list) else []
        )
    if "victims" in out.columns:
        out["victims"] = out["victims"].apply(
            lambda s: s if isinstance(s, list) else []
        )
    if "orbit_contexts" in out.columns:
        out["orbit_contexts"] = out["orbit_contexts"].apply(
            lambda s: s if isinstance(s, dict) else None
        )

    records = out.to_dict(orient="records")
    # Clean up NaN -> None for JSON
    for rec in records:
        for k, v in rec.items():
            if isinstance(v, float) and pd.isna(v):
                rec[k] = None
    print(json.dumps(records, indent=2))


def output_domains(df):
    """Print domain value counts."""
    counts = df["domain"].value_counts()
    for domain, count in counts.items():
        print(f"  {count:4d}  {domain}")
    print(f"\n{len(counts)} domains", file=sys.stderr)


def output_types(df):
    """Print type value counts."""
    counts = df["claimed_type"].value_counts()
    for typ, count in counts.items():
        print(f"  {count:4d}  {typ}")
    print(f"\n{len(counts)} types", file=sys.stderr)


def output_orbits(df):
    """Print orbit signature value counts."""
    counts = df["orbit_display"].value_counts()
    for orbit, count in counts.items():
        print(f"  {count:4d}  {orbit}")
    print(f"\n{len(counts)} unique orbits", file=sys.stderr)


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    parser = build_parser()
    args = parser.parse_args()

    # Build data
    df, severity_dict, pipeline_raw, omega_raw = build_dataframe()

    # Detail mode bypasses filters
    if args.detail:
        output_detail(df, args.detail, pipeline_raw, omega_raw)
        return

    # Apply filters
    df = apply_filters(df, args)

    # Dispatch output mode
    if args.count:
        output_count(df)
    elif args.table:
        output_table(df)
    elif args.aggregate:
        output_aggregate(df, args.aggregate)
    elif args.json_output:
        output_json(df)
    elif args.domains:
        output_domains(df)
    elif args.types:
        output_types(df)
    elif args.orbits:
        output_orbits(df)
    else:
        # Default: list
        output_list(df)


if __name__ == "__main__":
    main()
