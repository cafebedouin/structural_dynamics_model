import re
import collections
from typing import List, Dict

# The 618-constraint corpus validation script
class DRValidator:
    def __init__(self, filepath: str):
        self.filepath = filepath
        self.data = []
        self.collisions = []
        self.type_counts = collections.Counter()

        # Regex to capture the indexed constraint logic: Type(C[WHO, WHEN, WHERE, HOW_MUCH])
        self.entry_pattern = re.compile(
            r"(▲|⟐|⟑|⟐⟑|⧗|⚠)\s*([\w\s/]+)\[(\w+),\s*(\w+),\s*(\w+),\s*(\w+)\]"
        )

    def parse(self):
        with open(self.filepath, 'r') as f:
            content = f.read()
            matches = self.entry_pattern.findall(content)
            for match in matches:
                entry = {
                    "type": match[0],
                    "name": match[1].strip(),
                    "who": match[2],
                    "when": match[3],
                    "where": match[4],
                    "how_much": match[5],
                    "index_key": (match[2], match[3], match[4], match[5])
                }
                self.data.append(entry)
                self.type_counts[match[0]] += 1

    def calculate_collision_rate(self):
        """
        A collision occurs if the same [Who, When, Where, How Much] index
        produces two different types for the same constraint name.
        """
        registry = {} # (name, index_key) -> type
        collision_count = 0

        for entry in self.data:
            key = (entry["name"], entry["index_key"])
            if key in registry:
                if registry[key] != entry["type"]:
                    self.collisions.append({
                        "name": entry["name"],
                        "index": entry["index_key"],
                        "type_a": registry[key],
                        "type_b": entry["type"]
                    })
                    collision_count += 1
            else:
                registry[key] = entry["type"]

        rate = (collision_count / len(self.data)) * 100 if self.data else 0
        return rate

    def report(self):
        collision_rate = self.calculate_collision_rate()
        print(f"--- Deferential Realism Validation Report ---")
        print(f"Total Constraints Analyzed: {len(self.data)}")
        print(f"Collision Rate: {collision_rate:.2f}%")
        print(f"\nType Distribution:")
        type_map = {
            "▲": "Mountain", "⟐": "Rope", "⟐⟑": "Tangled Rope",
            "⟑": "Snare", "⧗": "Scaffold", "⚠": "Piton"
        }
        for char, count in self.type_counts.items():
            print(f"  {type_map.get(char, char)}: {count} ({count/len(self.data)*100:.1f}%)")

        if self.collisions:
            print(f"\nCollisions Detected:")
            for c in self.collisions:
                print(f"  [!] {c['name']} at {c['index']} returned both {c['type_a']} and {c['type_b']}")
        else:
            print(f"\n[Verified] 0% Collision Rate: Indexical sufficiency confirmed.")

# Usage
validator = DRValidator('../outputs/output.txt')
validator.parse()
validator.report()
