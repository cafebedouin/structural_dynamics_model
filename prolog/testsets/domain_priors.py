import os
import re
import glob

def generate_domain_registry(directory_path):
    # Patterns for extracting mappings and self-declarations
    category_pattern = re.compile(r"domain_category\(([^,]+),\s*([^)]+)\)\.")
    # Detects both prefixed and local base_extractiveness facts
    extractiveness_pattern = re.compile(r"(?:domain_priors:)?base_extractiveness\(([^,]+),\s*([\d\.]+)\)")

    registry = {}

    # Process all .pl files in the target directory
    pl_files = glob.glob(os.path.join(directory_path, "*.pl"))

    for file_path in pl_files:
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()

                # 1. Find explicit category mappings
                cat_matches = category_pattern.findall(content)
                for domain_id, category in cat_matches:
                    registry[domain_id.strip()] = category.strip()

                # 2. Find self-declared extractiveness and infer category if not already found
                ext_matches = extractiveness_pattern.findall(content)
                for domain_id, value in ext_matches:
                    domain_id = domain_id.strip()
                    if domain_id not in registry:
                        # Applying inference logic from domain_priors.pl
                        score = float(value)
                        if score > 0.6:
                            registry[domain_id] = "extractive_market"
                        else:
                            # Fallback for moderate/low extraction
                            registry[domain_id] = "narrative_history"
        except Exception as e:
            print(f"Error processing {file_path}: {e}")

    # Generate the formatted Prolog output
    output = ["% --- AUTOMATICALLY GENERATED DOMAIN REGISTRY ---"]
    for domain_id in sorted(registry.keys()):
        line = f"domain_category({domain_id}, {registry[domain_id]})."
        output.append(line)

    return "\n".join(output)

# Usage Example:
registry_content = generate_domain_registry('./')
print(registry_content)
