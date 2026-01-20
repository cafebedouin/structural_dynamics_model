import json
import os

ANALYSIS_FILE = "../outputs/structured_analysis.json"
GAP_FILE = "../outputs/gap_report.json"

def cross_reference():
    with open(ANALYSIS_FILE, 'r') as f: analysis = json.load(f)
    with open(GAP_FILE, 'r') as f: gaps = json.load(f)

    print(f"{'DOMAIN':<40} | {'GAPS':<5} | {'REPAIRS':<8} | {'REPAIR INTENSITY'}")
    print("-" * 75)

    for domain, gap_data in gaps.items():
        if not gap_data["is_complete"]:
            # Find repair count in analysis
            repair_count = len(analysis.get(domain, {}).get("repaired_vectors", []))
            intensity = "HIGH" if repair_count > 4 else "LOW"

            missing = []
            if not gap_data["rope"]: missing.append("ROPE")
            if not gap_data["noose"]: missing.append("NOOSE")
            if not gap_data["mountain"]: missing.append("MTN")

            print(f"{domain:<40} | {len(missing):<5} | {repair_count:<8} | {intensity} (Missing: {', '.join(missing)})")

if __name__ == "__main__":
    cross_reference()
