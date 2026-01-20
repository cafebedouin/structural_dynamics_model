import os
import re

# Mapping non-standard terms to the 3-pillar standard
FIXES = {
    'algorithmic_determinism': 'mountain',
    'biomountain': 'mountain',
    'logic_limit': 'mountain'
}

def harden_testsets(directory):
    resolved = 0
    for root, _, files in os.walk(directory):
        for file in files:
            if file.endswith('.pl'):
                path = os.path.join(root, file)
                with open(path, 'r') as f:
                    content = f.read()

                new_content = content
                for old, new in FIXES.items():
                    # Specifically target constraint_claim declarations
                    pattern = rf'constraint_claim\(([^,]+),\s*{old}\)'
                    new_content = re.sub(pattern, rf'constraint_claim(\1, {new})', new_content)

                if content != new_content:
                    with open(path, 'w') as f:
                        f.write(new_content)
                    print(f"✓ Hardened: {file} ({list(FIXES.keys())})")
                    resolved += 1
    return resolved

if __name__ == "__main__":
    # Adjust path if your testsets are located elsewhere
    testset_dir = "../prolog/testsets"
    if os.path.exists(testset_dir):
        count = harden_testsets(testset_dir)
        print(f"\n--- SUCCESS: {count} Files Standardized ---")
    else:
        print(f"Error: Directory {testset_dir} not found.")
