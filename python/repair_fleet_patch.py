import json
import os

def apply_ontological_fix(json_path):
    with open(json_path, 'r') as f:
        data = json.load(f)

    # Standardizing the "Orphan" Ontologies
    ontology_map = {
        "algorithmic_determinism": "mountain",
        "biomountain": "mountain",
        "logic_limit": "mountain"
    }

    repaired_count = 0

    for domain, record in data.items():
        current_ontology = record.get("ontology", "").lower()
        if current_ontology in ontology_map:
            record["ontology"] = ontology_map[current_ontology]
            repaired_count += 1
            print(f"✓ Fixed: {domain} ({current_ontology} -> mountain)")

    # Save the hardened analysis
    with open(json_path, 'w') as f:
        json.dump(data, f, indent=4)

    print(f"\n--- REPAIR COMPLETE ---")
    print(f"Total Ontological Violations Resolved: {repaired_count}")

if __name__ == "__main__":
    # Ensure this points to your latest structured output
    analysis_file = "../outputs/structured_analysis.json"
    if os.path.exists(analysis_file):
        apply_ontological_fix(analysis_file)
    else:
        print("Error: structured_analysis.json not found.")
