#!/usr/bin/env python3
# NOTE: This script requires pyswip to be installed (`pip install pyswip`)
import re
import sys
from collections import defaultdict
from pathlib import Path
import itertools
import argparse
from pyswip import Prolog

class CorpusAnalyzer:
    def __init__(self, testsets_dir=None, prolog_stack_file=None):
        self.testsets_dir = Path(testsets_dir or "/home/scott/bin/structural_dynamics_model/prolog/testsets")
        self.prolog_stack_file = Path(prolog_stack_file or "/home/scott/bin/structural_dynamics_model/prolog/stack.pl")
        self.constraints = {}
        self.types = defaultdict(list)
        self.concepts = defaultdict(set)
        self.co_occurrences = defaultdict(int)

        self.prolog = Prolog()
        self.prolog.consult(str(self.prolog_stack_file))

        self.id_pattern = re.compile(r"(?:constraint_id|Checking Interval):\s*(?:\\s*)?([a-zA-Z0-9_]+)")
        self.explicit_pattern = re.compile(r"(?:\w+:)?(?:constraint_classification|constraint_claim)\s*\(\s*(?:\\s*)?([a-zA-Z0-9_]+),\s*([a-zA-Z0-9_]+)")
        self.score_pattern = re.compile(r"base_extractiveness\(\s*(?:\\s*)?[^,]+,\s*([\d\.]+)\s*\)")

    def classify_type(self, constraint_id, tag, score):
        # Audit logic: If it claims to be a mountain but acts like a snare.
        # This rule is custom to this script and is not part of the Prolog engine.
        if tag == "mountain" and score > 0.8: return "false_mountain"

        # Use the Prolog engine for classification
        try:
            query = f"drl_core:dr_type('{constraint_id}', Type)"
            result = list(self.prolog.query(query))
            if result:
                return result[0]['Type']
            return "unknown"
        except Exception as e:
            print(f"Error querying Prolog for {constraint_id}: {e}")
            return "unknown"

    def analyze_corpus(self):
        if not self.testsets_dir.exists(): return False
        prolog_dir = str(self.prolog_stack_file.parent)
        list(self.prolog.query(f"working_directory(_, '{prolog_dir}')"))
        for pl_file in sorted(self.testsets_dir.glob("*.pl")):
            try:
                self.prolog.consult(str(pl_file))
            except Exception:
                pass
        for pl_file in sorted(self.testsets_dir.glob("*.pl")):
            try:
                content = pl_file.read_text(encoding='utf-8')
                ids = self.id_pattern.findall(content)
                if not ids: continue

                constraint_id = ids[0]

                tag_match = self.explicit_pattern.search(content)
                tag = tag_match.group(2).strip() if tag_match else None

                score_match = self.score_pattern.search(content)
                score = float(score_match.group(1)) if score_match else 0.5

                c_type = self.classify_type(constraint_id, tag, score)
                self.types[c_type].append(constraint_id)

                parts = re.split(r'[_ \-\s]+', constraint_id.lower())
                concepts = [p for p in parts if len(p) > 2 and not p.isdigit()]
                for c1, c2 in itertools.combinations(sorted(concepts), 2):
                    self.co_occurrences[(c1, c2)] += 1
                for concept in concepts:
                    self.concepts[concept].add(constraint_id)
                self.constraints[constraint_id] = {'concepts': concepts}
            except Exception: continue
        return True

    def generate_report(self):
        print("\n" + "="*80 + "\n  META-REPORT: Test Suite Analysis\n" + "="*80)
        total = len(self.constraints)
        print(f"\n📚 CORPUS OVERVIEW\n{'-'*80}\n  Total Constraints Analyzed: {total}")
        for c_type in ['piton', 'snare', 'tangled_rope', 'rope', 'mountain', 'false_mountain']:
            count = len(self.types[c_type])
            perc = (count / total * 100) if total > 0 else 0
            print(f"    - {c_type:15s}: {count:4d} ({perc:5.1f}%) {'█' * int(perc / 5)}")
        print(f"\n🕸️  CONCEPT NETWORK\n{'-'*80}")
        top = sorted(self.co_occurrences.items(), key=lambda x: x[1], reverse=True)[:10]
        for (c1, c2), count in top:
            print(f"  {c1:20s} {'━' * min(count, 10)}━→ {c2:20s} ({count})")
        print(f"\n  💡 Found {len(self.concepts)} unique concepts.\n" + "="*80 + "\n")

if __name__ == "__main__":
    analyzer = CorpusAnalyzer()
    if analyzer.analyze_corpus(): analyzer.generate_report()
