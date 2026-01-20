import json

def category_mismatch_audit(json_path, registry_path):
    # Load analysis and category registry
    with open(json_path, 'r') as f: analysis = json.load(f)

    # Simple check for Narrative History vs Shadow Noose
    print(f"{'DOMAIN':<40} | {'REPORTED CAT' :<20} | {'ACTUAL SIGNATURE'}")
    print("-" * 80)

    for domain, record in analysis.items():
        repairs = record.get("repaired_vectors", [])
        sup = max([float(r['val']) for r in repairs if 'suppression' in r['metric']] + [0.0])

        # If the domain is a Shadow Noose but classified as History
        if sup >= 0.8:
            print(f"{domain:<40} | {'EXTRACTIVE?':<20} | [CRITICAL] Hidden Extraction Detected")

if __name__ == "__main__":
    category_mismatch_audit("../outputs/structured_analysis.json", "../prolog/domain_registry.pl")
