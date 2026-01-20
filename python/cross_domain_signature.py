import json
import math
import os

# --- CONFIGURATION ---
# Define the keys used in your JSON structure
DATA_FILE = "../outputs/structured_analysis.json"
OUTPUT_FILE = "../outputs/signature_matches.txt"

def euclidean_distance(v1, v2):
    """Calculates the straight-line distance between two vectors."""
    return math.sqrt(sum((a - b) ** 2 for a, b in zip(v1, v2)))

def analyze_signatures():
    if not os.path.exists(DATA_FILE):
        print(f"Error: {DATA_FILE} not found.")
        return

    with open(DATA_FILE, 'r') as f:
        data = json.load(f)

    # 1. Target Vector: Calibrated 'Extractive Market' profile [Acc, Stk, Sup, Res]
    target_vector = [0.4, 0.8, 0.8, 0.6]

    results = []

    for domain_id, record in data.items():
        vectors = record.get("repaired_vectors", [])
        if not vectors:
            continue

        try:
            # 2. Extract and CAST to float. We target individual-level metrics at t=10.
            # Default to 0.5 (neutral) if a specific metric is missing.
            current_vector = [
                float(next((v['val'] for v in vectors if v['metric'] == "accessibility_collapse(individual)" and v['t'] == "10"), 0.5)),
                float(next((v['val'] for v in vectors if v['metric'] == "stakes_inflation(individual)" and v['t'] == "10"), 0.5)),
                float(next((v['val'] for v in vectors if v['metric'] == "suppression(individual)" and v['t'] == "10"), 0.5)),
                float(next((v['val'] for v in vectors if v['metric'] == "resistance(individual)" and v['t'] == "10"), 0.5))
            ]

            distance = euclidean_distance(target_vector, current_vector)
            results.append({
                "domain": domain_id,
                "distance": distance,
                "vector": current_vector
            })
        except (StopIteration, ValueError, TypeError):
            # Skip records with malformed or missing data
            continue

    # 3. Sort by proximity (lowest distance = highest resemblance)
    results.sort(key=lambda x: x['distance'])

    # 4. Console Output
    print(f"\n{'DOMAIN':<40} | {'DISTANCE':<10} | {'MATCH SCORE'}")
    print("-" * 70)
    for res in results[:15]:
        match_percentage = max(0, 100 * (1 - res['distance']))
        print(f"{res['domain']:<40} | {res['distance']:<10.4f} | {match_percentage:>6.2f}%")

    # 5. File Output
    with open(OUTPUT_FILE, 'w') as out:
        out.write(f"CROSS-DOMAIN SIGNATURE ANALYSIS\nTarget Profile: {target_vector}\n\n")
        for res in results:
            out.write(f"{res['domain']}: Distance {res['distance']:.4f}\n")

if __name__ == "__main__":
    analyze_signatures()
