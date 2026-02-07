import re
import os
from collections import Counter

def count_computed_types():
    output_log_path = os.path.join(os.path.dirname(__file__), '../outputs/output.txt')
    
    try:
        with open(output_log_path, 'r', encoding='utf-8') as f:
            content = f.read()
    except FileNotFoundError:
        print(f"Error: output.txt not found at {output_log_path}")
        return

    # This regex finds the computed type from the "Perspectives" section
    # Example: - [context(...)]: snare (Mismatch)
    matches = re.findall(r'\[context.*\]:\s+(\w+)', content)
    
    counts = Counter(matches)
    
    print("Computed Classification Counts")
    print("=" * 40)
    for classification_type, count in sorted(counts.items()):
        print(f"{classification_type:<20} {count}")
    print("=" * 40)

if __name__ == "__main__":
    count_computed_types()
