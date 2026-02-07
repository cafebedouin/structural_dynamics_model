import re
from collections import defaultdict, Counter

def analyze_high_friction_constraints(filename):
    """
    Identifies constraints where the framework identifies the most 'Gaps'
    (contradictions between observer indices).
    """
    exec_pattern = re.compile(r"\[\d+\] EXECUTING: testsets/(.+)\.pl")
    gap_pattern = re.compile(r"! GAP: .+\.")

    friction_report = defaultdict(int)
    type_sequences = defaultdict(list)
    current_constraint = None

    with open(filename, 'r') as f:
        for line in f:
            line = line.strip()
            # Track current file
            m_exec = exec_pattern.search(line)
            if m_exec:
                current_constraint = m_exec.group(1)
                continue

            # Count Gaps per constraint
            if gap_pattern.search(line) and current_constraint:
                friction_report[current_constraint] += 1

    # Sort by descending friction (Gaps)
    sorted_friction = sorted(friction_report.items(), key=lambda x: x[1], reverse=True)

    print("="*60)
    print("TOP 15 HIGH-FRICTION CONSTRAINTS (POTENTIAL TANGLED ROPES)")
    print("="*60)
    print(f"{'Constraint Name':<45} | {'Gap Count':<10}")
    print("-" * 60)
    for name, count in sorted_friction[:15]:
        print(f"{name:<45} | {count:<10}")
    print("="*60)
    print("\n[ANALYSIS] These constraints are shifting type across indices.")
    print("If Gap Count is high but Type is 'Snare', the Power Modifier")
    print("is likely pushing the extraction score (χ) past 0.66 too early.")

analyze_high_friction_constraints('../outputs/output.txt')
