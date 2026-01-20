import os
import json

# Configuration
TESTSETS_DIR = "../prolog/testsets/"
GAP_REPORT = "../outputs/gap_report.json"

# Mapping types to their standard agent perspective
PERSPECTIVE_MAP = {
    "rope": "agent_power(institutional)",
    "noose": "agent_power(individual_powerless)",
    "mountain": "agent_power(analytical)"
}

def inject_skeletons():
    # 1. Load the Gap Analysis
    if not os.path.exists(GAP_REPORT):
        print(f"Error: {GAP_REPORT} not found. Run gap_analysis.py first.")
        return

    with open(GAP_REPORT, 'r') as f:
        gaps = json.load(f)

    repaired_count = 0

    # 2. Iterate through domains with gaps
    for domain_id, status in gaps.items():
        if status["is_complete"]:
            continue

        file_path = os.path.join(TESTSETS_DIR, f"{domain_id}.pl")
        if not os.path.exists(file_path):
            print(f"Warning: File for {domain_id} not found at {file_path}")
            continue

        # Identify missing pillars
        missing_pillars = []
        for pillar in ["rope", "noose", "mountain"]:
            if not status[pillar]:
                missing_pillars.append(pillar)

        if not missing_pillars:
            continue

        # 3. Generate Skeleton Code
        skeleton_block = [
            "\n% ==========================================================================",
            f"% [SKELETON REPAIR] Missing Perspectival Pillars Added: {', '.join(missing_pillars).upper()}",
            "% To be calibrated: Define the narrative justification for these perspectives.",
            "% =========================================================================="
        ]

        for pillar in missing_pillars:
            perspective = PERSPECTIVE_MAP[pillar]
            predicate = f"constraint_indexing:constraint_classification({domain_id}, {pillar}, {perspective})."
            skeleton_block.append(predicate)

        # 4. Append to File
        try:
            with open(file_path, 'a') as f:
                f.write("\n" + "\n".join(skeleton_block) + "\n")
            repaired_count += 1
            print(f"✓ Injected {len(missing_pillars)} pillars into {domain_id}.pl")
        except Exception as e:
            print(f"✗ Failed to repair {domain_id}: {e}")

    print(f"\nSummary: {repaired_count} files updated with skeleton perspectives.")

if __name__ == "__main__":
    inject_skeletons()
