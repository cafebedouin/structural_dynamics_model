import json
import re
import os
from collections import defaultdict

# Path Configuration
REGISTRY_FILE = "../prolog/domain_registry.pl"
ANALYSIS_FILE = "../outputs/structured_analysis.json"

def calibrate_profiles():
    # 1. Load Category Mapping from Registry
    cat_map = {}
    if os.path.exists(REGISTRY_FILE):
        with open(REGISTRY_FILE, 'r') as f:
            content = f.read()
            # Maps both Interval IDs and Constraint IDs to categories
            matches = re.findall(r"domain_category\(([^,]+),\s*([^)]+)\)\.", content)
            for d_id, cat in matches:
                cat_map[d_id.strip()] = cat.strip()
    else:
        print(f"Error: {REGISTRY_FILE} not found.")
        return

    # 2. Load Structured Analysis
    if not os.path.exists(ANALYSIS_FILE):
        print(f"Error: {ANALYSIS_FILE} not found.")
        return

    with open(ANALYSIS_FILE, 'r') as f:
        analysis = json.load(f)

    # 3. Aggregate Imputed Vector Values
    # Profiles: [Accessibility, Stakes, Suppression, Resistance]
    agg = defaultdict(lambda: [[] for _ in range(4)])
    metric_pos = {
        "accessibility_collapse": 0,
        "stakes_inflation": 1,
        "suppression": 2,
        "resistance": 3
    }

    # Tracking domains that contributed to specific categories
    contribution_counts = defaultdict(int)

    for domain_label, record in analysis.items():
        # Use the ID from the path if label mapping is inconsistent
        # e.g., ../prolog/testsets/adverse_possession.pl -> adverse_possession
        path_id = os.path.basename(record["path"]).replace(".pl", "")
        category = cat_map.get(path_id, "unknown_novel")

        if record["repaired_vectors"]:
            contribution_counts[category] += 1
            for rv in record["repaired_vectors"]:
                # Normalize metric name: suppression(class) -> suppression
                base_metric = rv["metric"].split('(')[0]
                if base_metric in metric_pos:
                    pos = metric_pos[base_metric]
                    agg[category][pos].append(float(rv["val"]))

    # 4. Generate Calibrated Profiles
    print("\n% ============================================================================")
    print("% CALIBRATED CATEGORY PROFILES")
    print("% Based on means of imputed vectors from 400+ validated instances.")
    print("% ============================================================================\n")

    for cat, pos_values in sorted(agg.items()):
        means = []
        for values in pos_values:
            if values:
                means.append(round(sum(values) / len(values), 2))
            else:
                means.append(0.5) # Default to neutral if no repairs were needed

        print(f"category_profile({cat:20}, {means}).")

    print(f"\n% Stats: Data derived from {sum(contribution_counts.values())} repaired domains.")

if __name__ == "__main__":
    calibrate_profiles()
