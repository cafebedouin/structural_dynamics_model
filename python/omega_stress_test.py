import os
import re

# The Omegas to Stress Test
OMEGA_TARGETS = {
    "gale_shapley.pl": {"E": "0.8", "S": "0.8"},
    "planetary_boundaries.pl": {"E": "0.8", "S": "0.8"}
}

def apply_stress_test(directory):
    resolved = 0
    for filename, priors in OMEGA_TARGETS.items():
        path = os.path.join(directory, filename)
        if os.path.exists(path):
            with open(path, 'r') as f:
                content = f.read()

            # Update base_extractiveness
            new_content = re.sub(
                r'domain_priors:base_extractiveness\(([^,]+),\s*[\d\.]+\)',
                f'domain_priors:base_extractiveness(\\1, {priors["E"]})',
                content
            )
            # Update suppression_score
            new_content = re.sub(
                r'domain_priors:suppression_score\(([^,]+),\s*[\d\.]+\)',
                f'domain_priors:suppression_score(\\1, {priors["S"]})',
                new_content
            )

            if content != new_content:
                with open(path, 'w') as f:
                    f.write(new_content)
                print(f"⚡ Stress Test Applied: {filename} (E={priors['E']}, S={priors['S']})")
                resolved += 1
    return resolved

if __name__ == "__main__":
    testset_dir = "../prolog/testsets"
    if os.path.exists(testset_dir):
        count = apply_stress_test(testset_dir)
        print(f"\n--- STRESS TEST COMPLETE: {count} Domains Re-calibrated ---")
