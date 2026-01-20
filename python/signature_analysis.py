import json

def find_shadow_nooses(json_path):
    with open(json_path, 'r') as f:
        data = json.load(f)

    print(f"{'DOMAIN':<40} | {'SUPPRESSION':<12} | {'STAKES':<8} | {'STATUS'}")
    print("-" * 75)

    for domain, record in data.items():
        # Look for high suppression and high stakes in the repairs
        repairs = record.get("repaired_vectors", [])
        sup = max([float(r['val']) for r in repairs if 'suppression' in r['metric']] + [0.0])
        stk = max([float(r['val']) for r in repairs if 'stakes' in r['metric']] + [0.0])

        # A "Shadow Noose" is a domain where suppression energy > 0.7
        if sup > 0.7 and stk > 0.7:
            status = "SHADOW NOOSE"
            print(f"{domain:<40} | {sup:<12} | {stk:<8} | {status}")

if __name__ == "__main__":
    find_shadow_nooses("../outputs/structured_analysis.json")
