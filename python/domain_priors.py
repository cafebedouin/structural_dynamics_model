import os
import re
import glob
import argparse

def generate_domain_registry(testsets_dir, output_path):
    # Patterns to capture BOTH the constraint ID and the Interval ID
    constraint_pattern = re.compile(r"constraint_id:\s*([a-zA-Z0-9_]+)")
    interval_pattern = re.compile(r"narrative_ontology:interval\(([a-zA-Z0-9_]+),")
    extractiveness_pattern = re.compile(r"(?:domain_priors:)?base_extractiveness\(([^,]+),\s*([\d\.]+)\)")

    registry = {}

    pl_files = glob.glob(os.path.join(testsets_dir, "*.pl"))

    for file_path in pl_files:
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()

                # Find IDs to register
                c_match = constraint_pattern.search(content)
                i_match = interval_pattern.search(content)
                e_match = extractiveness_pattern.search(content)

                ids_to_register = []
                if c_match: ids_to_register.append(c_match.group(1).strip())
                if i_match: ids_to_register.append(i_match.group(1).strip())

                if e_match and ids_to_register:
                    score = float(e_match.group(2))
                    category = "extractive_market" if score > 0.6 else "narrative_history"

                    # Register both IDs to the same category to satisfy the auditor
                    for identifier in ids_to_register:
                        registry[identifier] = category

        except Exception as e:
            print(f"Error processing {file_path}: {e}")

    # Generate Prolog Module
    header = [
        ":- module(domain_registry, [domain_category/2]).",
        "% --- AUTOMATICALLY GENERATED DOMAIN REGISTRY ---",
        "% Maps both Constraint IDs and Interval IDs to categories.",
        ""
    ]

    lines = [f"domain_category({k}, {v})." for k, v in sorted(registry.items())]

    with open(output_path, 'w', encoding='utf-8') as f:
        f.write("\n".join(header + lines))

    print(f"✓ Registry generated at {output_path} with {len(registry)} entries.")

if __name__ == "__main__":
    parser = argparse.ArgumentParser()
    parser.add_argument("--input", default="../prolog/testsets/", help="Path to .pl testsets")
    parser.add_argument("--output", default="../prolog/domain_registry.pl", help="Path to registry output")
    args = parser.parse_args()

    generate_domain_registry(args.input, args.output)
