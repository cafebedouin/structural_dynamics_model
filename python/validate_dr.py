import re
from collections import defaultdict, Counter

def run_dr_validation(filename):
    """
    Parses Deferential Realism logs to verify indexical consistency
    and power-scaling dynamics.
    """
    # Patterns for parsing the log structure
    exec_pattern = re.compile(r"\[\d+\] EXECUTING: testsets/(.+)\.pl")
    context_pattern = re.compile(
        r"- \[context\(agent_power\((?P<who>\w+)\),time_horizon\((?P<when>\w+)\),"
        r"exit_options\((?P<where>\w+)\),spatial_scope\((?P<how_much>\w+)\)\)\]: (?P<type>\w+)"
    )
    gap_pattern = re.compile(r"(! GAP: .+|\(Mismatch\))")

    evaluations = []
    unique_constraints = set()
    gaps_detected = 0
    current_constraint = None

    # Step 1: Parsing
    with open(filename, 'r') as f:
        for line in f:
            line = line.strip()
            # Identify current test block
            m_exec = exec_pattern.search(line)
            if m_exec:
                current_constraint = m_exec.group(1)
                unique_constraints.add(current_constraint)
                continue

            # Extract classification result
            m_res = context_pattern.search(line)
            if m_res and current_constraint:
                res = m_res.groupdict()
                res['name'] = current_constraint
                evaluations.append(res)

            # Track GAPs (Indexical shifts identified by the model)
            if gap_pattern.search(line):
                gaps_detected += 1

    # Step 2: Metrics Calculation
    total_evals = len(evaluations)
    total_unique = len(unique_constraints)

    # Collision Check (Same index combination yielding different types)
    registry = defaultdict(set)
    for ev in evaluations:
        key = (ev['name'], ev['who'], ev['when'], ev['where'], ev['how_much'])
        registry[key].add(ev['type'])

    collisions = {k: v for k, v in registry.items() if len(v) > 1}
    collision_rate = (len(collisions) / total_evals * 100) if total_evals > 0 else 0

    # Type Distribution
    type_counts = Counter(ev['type'] for ev in evaluations)
    sym_map = {
        'mountain': '▲ (Mountain)', 'rope': '⟐ (Rope)',
        'tangled_rope': '⟐⟑ (Tangled Rope)', 'snare': '⟑ (Snare)',
        'scaffold': '⧗ (Scaffold)', 'piton': '⚠ (Piton)'
    }

    # Indexical Asymmetry (Classification shifts across different observers)
    asymmetry_map = defaultdict(set)
    for ev in evaluations:
        asymmetry_map[ev['name']].add(ev['type'])

    variable_constraints = [n for n, types in asymmetry_map.items() if len(types) > 1]
    asymmetry_rate = (len(variable_constraints) / total_unique * 100) if total_unique > 0 else 0

    # Step 3: Reporting
    print("="*60)
    print("DEFERENTIAL REALISM: DYNAMIC VALIDATION REPORT")
    print("="*60)
    print(f"Total Constraints in Corpus:     {total_unique}")
    print(f"Total Contextual Evaluations:    {total_evals}")
    print(f"Collision Rate:                  {collision_rate:.2f}%")
    print(f"Indexical Asymmetry Rate:        {asymmetry_rate:.2f}%")
    print(f"Total GAP/Mismatches Identified: {gaps_detected}")
    print("-" * 60)
    print("TYPE DISTRIBUTION (CLASSIFIED):")
    for t, count in sorted(type_counts.items(), key=lambda x: x[1], reverse=True):
        print(f"  {sym_map.get(t, t):<25}: {count:<4} ({count/total_evals*100:>5.1f}%)")
    print("-" * 60)

    if not collisions:
        print("[VERIFIED] 0% Collision Rate: Indexical sufficiency confirmed.")
    else:
        print(f"[!] {len(collisions)} COLLISION(S) DETECTED:")
        for k, v in collisions.items():
            print(f"  Constraint: {k[0]} | Index: {k[1:]} | Types: {v}")
    print("="*60)

if __name__ == "__main__":
    run_dr_validation('../outputs/output.txt')
