"""Naturalization-gap diagnostic.

For each JSON constraint file: computes the δ_d (deadweight) flag and the
naturalization-gap flag, then reports naturalized-mountain, pure-mountain,
and unchecked cases.

Usage:
    python3 python/naturalization_gap_diagnostic.py [--dir json/run_01/]
"""
import argparse
import json
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
DEFAULT_DIR = REPO_ROOT / "json" / "run_01"

NATURALIZED_AUTHORITIES = {"extraction", "diffuse_epistemic"}
DEADWEIGHT_EPSILON_CEILING = 0.15


def analyze_file(path: Path) -> dict:
    try:
        data = json.loads(path.read_text(encoding="utf-8"))
    except Exception as e:
        return {"file": path.name, "error": str(e)}

    bp = data.get("base_properties", {})
    cs = data.get("cs_structure", {})

    claimed_type = bp.get("claimed_type") or bp.get("type")
    eps_raw = bp.get("extractiveness")
    try:
        eps = float(eps_raw) if eps_raw is not None else None
    except (TypeError, ValueError):
        eps = None

    victims = bp.get("victims") or bp.get("victim_set") or []
    beneficiaries = bp.get("beneficiaries") or bp.get("beneficiary_set") or []

    authority = cs.get("authority_grounding") if cs else None

    # δ_d flag: claimed_type=mountain AND ε<threshold AND both victims and beneficiaries
    is_mountain = isinstance(claimed_type, str) and "mountain" in claimed_type.lower()
    has_victims = bool(victims)
    has_beneficiaries = bool(beneficiaries)
    low_eps = eps is not None and eps < DEADWEIGHT_EPSILON_CEILING

    delta_d_flag = is_mountain and low_eps and has_victims and has_beneficiaries

    # naturalization-gap flag: δ_d flag AND authority ∈ NATURALIZED_AUTHORITIES
    naturalization_gap = delta_d_flag and authority in NATURALIZED_AUTHORITIES

    # categorize
    if cs is None:
        category = "unchecked_no_cs_structure"
    elif authority == "self_enforcing":
        category = "unchecked_self_enforcing"
    elif naturalization_gap:
        category = "naturalized_mountain"
    elif delta_d_flag:
        category = "pure_mountain_deadweight"
    elif is_mountain and low_eps and not has_victims and not has_beneficiaries:
        category = "clean_mountain"
    else:
        category = "other"

    return {
        "file": path.name,
        "constraint_id": data.get("constraint_id") or data.get("id") or path.stem,
        "claimed_type": claimed_type,
        "extractiveness": eps,
        "authority_grounding": authority,
        "has_victims": has_victims,
        "has_beneficiaries": has_beneficiaries,
        "delta_d_flag": delta_d_flag,
        "naturalization_gap": naturalization_gap,
        "category": category,
    }


def main(argv=None):
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("--dir", default=str(DEFAULT_DIR),
                        help="Directory of JSON constraint files")
    parser.add_argument("--out", default=None,
                        help="Output JSON path (default: stdout only)")
    args = parser.parse_args(argv)

    json_dir = Path(args.dir)
    if not json_dir.is_dir():
        print(f"ERROR: directory not found: {json_dir}", file=sys.stderr)
        sys.exit(1)

    files = sorted(json_dir.glob("*.json"))
    if not files:
        print(f"No .json files found in {json_dir}", file=sys.stderr)
        sys.exit(1)

    results = [analyze_file(f) for f in files]

    by_category: dict[str, list] = {}
    for r in results:
        by_category.setdefault(r.get("category", "error"), []).append(r)

    naturalized = by_category.get("naturalized_mountain", [])
    pure_deadweight = by_category.get("pure_mountain_deadweight", [])
    clean_mountains = by_category.get("clean_mountain", [])
    unchecked_no_cs = by_category.get("unchecked_no_cs_structure", [])
    unchecked_se = by_category.get("unchecked_self_enforcing", [])
    other = by_category.get("other", [])
    errors = [r for r in results if "error" in r]

    report = {
        "directory": str(json_dir),
        "total_files": len(files),
        "summary": {
            "naturalized_mountain": len(naturalized),
            "pure_mountain_deadweight": len(pure_deadweight),
            "clean_mountain": len(clean_mountains),
            "unchecked_no_cs_structure": len(unchecked_no_cs),
            "unchecked_self_enforcing": len(unchecked_se),
            "other": len(other),
            "errors": len(errors),
        },
        "naturalized_mountain_cases": naturalized,
        "pure_mountain_deadweight_cases": pure_deadweight,
        "clean_mountain_cases": clean_mountains,
        "unchecked_self_enforcing_cases": unchecked_se,
    }

    print(f"\n=== NATURALIZATION-GAP DIAGNOSTIC ===")
    print(f"Directory: {json_dir}")
    print(f"Files scanned: {len(files)}")
    print(f"\nNaturalized mountains (δ_d + extraction/diffuse_epistemic authority): {len(naturalized)}")
    for r in naturalized:
        print(f"  {r['constraint_id']}: ε={r['extractiveness']}, authority={r['authority_grounding']}")
    print(f"\nPure-mountain deadweight (δ_d flag, non-naturalized authority): {len(pure_deadweight)}")
    for r in pure_deadweight:
        print(f"  {r['constraint_id']}: ε={r['extractiveness']}, authority={r['authority_grounding']}")
    print(f"\nClean mountains (ε<{DEADWEIGHT_EPSILON_CEILING}, no victims+beneficiaries): {len(clean_mountains)}")
    print(f"Unchecked (no cs_structure): {len(unchecked_no_cs)}")
    print(f"Unchecked (self_enforcing authority): {len(unchecked_se)}")
    for r in unchecked_se:
        print(f"  {r['constraint_id']}: ε={r['extractiveness']}, victims={r['has_victims']}, bens={r['has_beneficiaries']}")
    print(f"Other: {len(other)}")
    if errors:
        print(f"Errors: {len(errors)}")
        for e in errors:
            print(f"  {e['file']}: {e['error']}")

    if args.out:
        out_path = Path(args.out)
        out_path.parent.mkdir(parents=True, exist_ok=True)
        out_path.write_text(json.dumps(report, indent=2, ensure_ascii=False), encoding="utf-8")
        print(f"\nReport written to {out_path}")

    return report


if __name__ == "__main__":
    main()
