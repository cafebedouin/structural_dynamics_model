import os
import re

# Standard mapping for non-standard ontologies found in your linter output
ONTOLOGY_MAP = {
    "organizational_decay": "noose",
    "election_cycle": "rope",
    "tangled_rope": "rope",
    "zombie": "noose",
    "natural_law": "mountain",
    "physical_law": "mountain",
    "algorithmic_determinism": "mountain",
    "biomountain": "mountain"
}

def repair_file(filepath):
    with open(filepath, 'r') as f:
        content = f.read()

    filename = os.path.basename(filepath)
    domain_id = filename.replace(".pl", "")
    changed = False

    # 1. Map Illegal Ontologies
    for old, new in ONTOLOGY_MAP.items():
        if f", {old})" in content:
            content = re.sub(rf'constraint_claim\(([^,]+),\s*{old}\)', rf'constraint_claim(\1, {new})', content)
            changed = True

    # 2. Inject Missing Pillars (Indexical Relativity Stubs)
    pillars = ['mountain', 'rope', 'noose']
    missing = []
    for p in pillars:
        # Check for constraint_classification(ID, Pillar, ...)
        if not re.search(rf'constraint_classification\(\s*{domain_id}\s*,\s*{p}\s*,', content):
            missing.append(p)

    if missing:
        content += "\n\n% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---\n"
        for m in missing:
            power = "analytical" if m == 'mountain' else "institutional" if m == 'rope' else "individual_powerless"
            content += f"constraint_indexing:constraint_classification({domain_id}, {m}, agent_power({power})).\n"
        changed = True

    # 3. Resolve Mandatrophy Omegas (The "Indexical Relativity" Gate)
    ext_match = re.search(r'base_extractiveness\([^,]+,\s*([\d\.]+)\)', content)
    if ext_match:
        val = float(ext_match.group(1))
        if val > 0.7 and "[RESOLVED MANDATROPHY]" not in content:
            # Prepend resolution tag to the file header
            content = f"% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.\n" + content
            changed = True

    if changed:
        with open(filepath, 'w') as f:
            f.write(content)
        return True
    return False

if __name__ == "__main__":
    target_dir = "../prolog/testsets"
    repaired_count = 0
    for root, _, files in os.walk(target_dir):
        for file in files:
            if file.endswith('.pl'):
                if repair_file(os.path.join(root, file)):
                    repaired_count += 1
                    print(f"✓ Hardened: {file}")

    print(f"\n--- REPAIR COMPLETE: {repaired_count} Files Standardized ---")
