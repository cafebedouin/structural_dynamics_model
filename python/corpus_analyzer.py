#!/usr/bin/env python3
"""
Corpus Analyzer - Analyzes the constraint corpus for conceptual connections

Identifies:
- Concept clusters and themes
- Missing connections
- Suggested scenarios based on existing patterns
- Knowledge gaps in the corpus
"""

import re
from collections import defaultdict, Counter
from pathlib import Path
import itertools

class CorpusAnalyzer:
    def __init__(self, testsets_dir='../prolog/testsets/'):
        self.testsets_dir = Path(testsets_dir)
        self.constraints = {}  # name -> metadata
        self.domains = defaultdict(list)  # domain -> [constraints]
        self.types = defaultdict(list)  # type -> [constraints]
        self.concepts = defaultdict(set)  # concept -> {constraints mentioning it}
        self.co_occurrences = defaultdict(int)  # (concept1, concept2) -> count

    def extract_concepts_from_name(self, name):
        """Extract concepts from constraint name"""
        # Split on underscores and common separators
        parts = re.split(r'[_\-\s]+', name.lower())

        # Filter out common words and numbers
        stopwords = {'the', 'of', 'and', 'or', 'in', 'on', 'at', 'to', 'for', 'a', 'an',
                    'is', 'are', 'was', 'were', 'be', 'been', 'being',
                    'system', 'interval', 'era', 'cycle'}

        concepts = []
        for part in parts:
            # Skip if it's a number, year, or stopword
            if part.isdigit() or len(part) < 3 or part in stopwords:
                continue
            # Skip if it looks like a year
            if re.match(r'\d{4}', part):
                continue
            concepts.append(part)

        return concepts

    def analyze_corpus(self):
        """Analyze all test files"""
        if not self.testsets_dir.exists():
            print(f"Error: {self.testsets_dir} does not exist")
            return False

        pl_files = list(self.testsets_dir.glob('*.pl'))

        if not pl_files:
            print(f"No .pl files found in {self.testsets_dir}")
            return False

        print(f"Analyzing {len(pl_files)} constraint files...")

        for filepath in pl_files:
            self._analyze_file(filepath)

        # Build concept co-occurrence matrix
        for constraint_name, metadata in self.constraints.items():
            concepts = metadata.get('concepts', [])

            # Record all pairs of co-occurring concepts
            for c1, c2 in itertools.combinations(sorted(concepts), 2):
                self.co_occurrences[(c1, c2)] += 1

        return True

    def _analyze_file(self, filepath):
        """Analyze a single test file"""
        constraint_name = filepath.stem

        metadata = {
            'file': filepath.name,
            'domain': None,
            'type': None,
            'concepts': self.extract_concepts_from_name(constraint_name)
        }

        try:
            with open(filepath, 'r', encoding='utf-8') as f:
                content = f.read()

            # Extract domain category
            domain_match = re.search(r"domain_priors:category_of\([^,]+,\s*(\w+)\)", content)
            if domain_match:
                metadata['domain'] = domain_match.group(1)
                self.domains[metadata['domain']].append(constraint_name)

            # Extract claimed type
            type_match = re.search(r"constraint_claim\([^,]+,\s*(\w+)\)", content)
            if type_match:
                metadata['type'] = type_match.group(1)
                self.types[metadata['type']].append(constraint_name)

        except Exception as e:
            print(f"Warning: Could not analyze {filepath.name}: {e}")

        self.constraints[constraint_name] = metadata

        # Index concepts
        for concept in metadata['concepts']:
            self.concepts[concept].add(constraint_name)

    def generate_report(self):
        """Generate corpus analysis report"""
        print("\n" + "="*80)
        print("CORPUS ANALYSIS: Conceptual Connections".center(80))
        print("="*80 + "\n")

        self._section_concept_clusters()
        self._section_domain_gaps()
        self._section_suggested_scenarios()
        self._section_concept_network()

        print("="*80 + "\n")

    def _section_concept_clusters(self):
        """Identify and report concept clusters"""
        print("🔍 CONCEPT CLUSTERS")
        print("-" * 80)

        # Find concepts that appear in multiple constraints
        frequent_concepts = {concept: constraints
                           for concept, constraints in self.concepts.items()
                           if len(constraints) >= 3}

        if frequent_concepts:
            print(f"  Found {len(frequent_concepts)} concepts appearing in 3+ constraints:\n")

            for concept, constraints in sorted(frequent_concepts.items(),
                                              key=lambda x: len(x[1]),
                                              reverse=True)[:10]:
                print(f"  {concept:20s}: {len(constraints)} constraint(s)")
                # Show a few examples
                for c in list(constraints)[:3]:
                    print(f"    - {c}")
                if len(constraints) > 3:
                    print(f"    ... and {len(constraints) - 3} more")
                print()
        else:
            print("  No strong concept clusters found (concepts appear in < 3 constraints)")

        print()

    def _section_domain_gaps(self):
        """Identify gaps in domain coverage"""
        print("🎯 DOMAIN GAPS & BALANCE")
        print("-" * 80)

        if not self.domains:
            print("  No domain information available")
            return

        # Show distribution
        print(f"  Domain Distribution ({len(self.domains)} domains):\n")

        for domain, constraints in sorted(self.domains.items(),
                                         key=lambda x: len(x[1]),
                                         reverse=True):
            count = len(constraints)
            bar = '█' * min(count, 50)
            print(f"  {domain:30s}: {count:3d} {bar}")

        # Identify gaps
        print(f"\n  Balance Analysis:")

        avg_count = sum(len(c) for c in self.domains.values()) / len(self.domains)

        overrepresented = [(d, len(c)) for d, c in self.domains.items()
                          if len(c) > avg_count * 1.5]
        underrepresented = [(d, len(c)) for d, c in self.domains.items()
                           if len(c) < 3]

        if overrepresented:
            print(f"\n  ✓ Well-represented domains:")
            for domain, count in overrepresented[:5]:
                print(f"    - {domain} ({count})")

        if underrepresented:
            print(f"\n  ⚠ Underrepresented domains (add more scenarios):")
            for domain, count in underrepresented:
                print(f"    - {domain} ({count} constraint(s))")

        print()

    def _section_suggested_scenarios(self):
        """Suggest new scenarios based on patterns"""
        print("💡 SUGGESTED SCENARIOS")
        print("-" * 80)

        suggestions = []

        # 1. Combine concepts that co-occur frequently
        if self.co_occurrences:
            top_pairs = sorted(self.co_occurrences.items(),
                             key=lambda x: x[1],
                             reverse=True)[:5]

            print("  Based on concept co-occurrence patterns:\n")
            for (c1, c2), count in top_pairs:
                # Suggest exploring these together in new context
                suggestion = f"Explore {c1} + {c2} in a new context ({count} existing linkages)"
                print(f"  • {suggestion}")

        # 2. Cross-pollinate domains
        if len(self.domains) >= 2:
            print("\n  Cross-domain scenario ideas:\n")

            # Find domains with similar concepts
            domain_concepts = {}
            for domain, constraints in self.domains.items():
                concepts = set()
                for c in constraints:
                    if c in self.constraints:
                        concepts.update(self.constraints[c]['concepts'])
                domain_concepts[domain] = concepts

            # Find domains with overlapping concepts
            for d1, d2 in itertools.combinations(list(self.domains.keys())[:10], 2):
                if d1 in domain_concepts and d2 in domain_concepts:
                    overlap = domain_concepts[d1] & domain_concepts[d2]
                    if len(overlap) >= 2:
                        concepts_str = ', '.join(list(overlap)[:3])
                        print(f"  • {d1} ↔ {d2}: shared concepts ({concepts_str})")

        # 3. Fill type gaps
        if self.types:
            print("\n  Type distribution balance:\n")

            for ctype, constraints in self.types.items():
                count = len(constraints)
                print(f"  {ctype:15s}: {count:3d}")

            # Suggest adding underrepresented types
            min_count = min(len(c) for c in self.types.values())
            underrep_types = [t for t, c in self.types.items() if len(c) == min_count]

            if underrep_types and len(underrep_types) < len(self.types):
                print(f"\n  ⚠ Add more '{', '.join(underrep_types)}' scenarios for balance")

        # 4. Concept-based suggestions
        if self.concepts:
            # Find concepts that appear alone (could be connected to others)
            lonely_concepts = {concept: constraints
                             for concept, constraints in self.concepts.items()
                             if len(constraints) == 1}

            if lonely_concepts and len(lonely_concepts) < 20:
                print(f"\n  Isolated concepts (could be connected to others):\n")
                for concept, constraints in list(lonely_concepts.items())[:5]:
                    constraint = list(constraints)[0]
                    print(f"  • Expand on '{concept}' (currently only in {constraint})")

        print()

    def _section_concept_network(self):
        """Show strongest conceptual connections"""
        print("🕸️  CONCEPT NETWORK")
        print("-" * 80)

        if not self.co_occurrences:
            print("  No concept co-occurrences found")
            return

        # Find strongest connections
        top_connections = sorted(self.co_occurrences.items(),
                                key=lambda x: x[1],
                                reverse=True)[:15]

        print(f"  Strongest conceptual connections ({len(self.co_occurrences)} total):\n")

        for (c1, c2), count in top_connections:
            # Show as a network edge
            strength = '━' * min(count, 10)
            print(f"  {c1:20s} {strength}→ {c2:20s} ({count})")

        print(f"\n  💡 These connections suggest natural clusters for future scenarios")
        print()

def main():
    import argparse

    parser = argparse.ArgumentParser(
        description='Analyze corpus for conceptual connections and gaps'
    )
    parser.add_argument(
        '--testsets',
        default='../prolog/testsets/',
        help='Path to testsets directory (default: ../prolog/testsets/)'
    )

    args = parser.parse_args()

    analyzer = CorpusAnalyzer(args.testsets)

    if not analyzer.analyze_corpus():
        sys.exit(1)

    analyzer.generate_report()

if __name__ == '__main__':
    main()
