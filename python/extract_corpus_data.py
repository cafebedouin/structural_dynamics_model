#!/usr/bin/env python3
"""
Corpus Data Extractor - Parses pipeline_output.json and raw .pl files

Combines:
- Structured pipeline output (pipeline_output.json)
- Raw structural data from .pl files (domain, human_readable, emerges/enforced)
- Merges into unified constraint objects
"""

import re
import json
from pathlib import Path
from collections import defaultdict
from typing import Dict, List, Optional, Tuple

class ConstraintData:
    """Unified constraint data structure"""

    def __init__(self, constraint_id):
        self.constraint_id = constraint_id
        self.claimed_type = None
        self.domain = None
        self.human_readable = None

        # Structural metrics
        self.extractiveness = None
        self.suppression = None
        self.resistance = None
        self.emerges_naturally = None
        self.requires_enforcement = None

        # Beneficiary data
        self.beneficiaries = []
        self.victims = []

        # Classifications (multiple per constraint)
        self.classifications = []  # [(type, context), ...]

        # From pipeline analysis
        self.structural_signature = None
        self.is_constructed = None
        self.omegas = []

        # Orbit data (from orbit_data.json)
        self.orbit_signature = None
        self.orbit_contexts = None

        # Calculated
        self.variance_ratio = None
        self.index_configs_count = None
        self.types_produced_count = None

    def to_dict(self):
        """Convert to dictionary for JSON serialization"""
        return {
            'constraint_id': self.constraint_id,
            'claimed_type': self.claimed_type,
            'domain': self.domain,
            'human_readable': self.human_readable,
            'metrics': {
                'extractiveness': self.extractiveness,
                'suppression': self.suppression,
                'resistance': self.resistance,
                'emerges_naturally': self.emerges_naturally,
                'requires_enforcement': self.requires_enforcement
            },
            'beneficiaries': self.beneficiaries,
            'victims': self.victims,
            'classifications': [
                {'type': t, 'context': ctx} for t, ctx in self.classifications
            ],
            'analysis': {
                'structural_signature': self.structural_signature,
                'is_constructed': self.is_constructed,
                'omegas': self.omegas,
                'variance_ratio': self.variance_ratio,
                'index_configs': self.index_configs_count,
                'types_produced': self.types_produced_count,
                'orbit_signature': self.orbit_signature,
                'orbit_contexts': self.orbit_contexts
            }
        }

class CorpusExtractor:
    """Main extraction engine"""

    def __init__(self, output_txt_path, testsets_dir):
        self.output_txt_path = Path(output_txt_path)
        self.testsets_dir = Path(testsets_dir)
        self.constraints = {}  # id -> ConstraintData
        self.filename_to_canonical = {}  # .pl stem -> canonical atom ID

    def extract_all(self):
        """Run complete extraction pipeline"""
        print("Step 1: Parsing pipeline_output.json...")
        self.parse_pipeline_json()

        print("Step 2: Parsing raw .pl files...")
        self.parse_pl_files()

        print("Step 3: Calculating derived metrics...")
        self.calculate_variance()

        print("Step 4: Inferring missing data...")
        self.infer_domains()

        print("Step 5: Loading orbit data...")
        self.load_orbit_data()

        return self.constraints

    def parse_pipeline_json(self):
        """Extract structured data from pipeline_output.json"""
        # NOTE: --output-txt is now used to locate sibling files (pipeline_output.json,
        # orbit_data.json). It's effectively --output-dir. Not renamed to avoid
        # breaking run_full_pipeline.sh and user aliases.
        pipeline_path = self.output_txt_path.parent / 'pipeline_output.json'
        if not pipeline_path.exists():
            print(f"Warning: {pipeline_path} not found")
            return

        with open(pipeline_path, 'r', encoding='utf-8') as f:
            pipeline = json.load(f)

        for entry in pipeline['per_constraint']:
            cid = entry['id']
            constraint = self.constraints.setdefault(cid, ConstraintData(cid))

            # Direct field mapping
            constraint.claimed_type = entry.get('claimed_type')
            constraint.extractiveness = entry.get('base_extractiveness')
            constraint.suppression = entry.get('suppression')
            constraint.resistance = entry.get('resistance')
            constraint.beneficiaries = entry.get('beneficiaries', [])
            constraint.victims = entry.get('victims', [])

            # Signature → is_constructed + structural_signature
            sig = entry.get('signature')
            if sig:
                constraint.structural_signature = sig
                constraint.is_constructed = sig not in ('natural_law',)

            # Omegas
            for omega in entry.get('omegas', []):
                constraint.omegas.append({
                    'id': omega['id'],
                    'type': omega['type']
                })

            # Perspectives → classifications (4 standard contexts)
            perspectives = entry.get('perspectives', {})
            standard_contexts = {
                'powerless': {'agent_power': 'powerless', 'time_horizon': 'biographical',
                             'exit_options': 'trapped', 'spatial_scope': 'local'},
                'moderate': {'agent_power': 'moderate', 'time_horizon': 'biographical',
                            'exit_options': 'mobile', 'spatial_scope': 'national'},
                'institutional': {'agent_power': 'institutional', 'time_horizon': 'generational',
                                 'exit_options': 'arbitrage', 'spatial_scope': 'national'},
                'analytical': {'agent_power': 'analytical', 'time_horizon': 'civilizational',
                              'exit_options': 'analytical', 'spatial_scope': 'global'},
            }
            for power, typ in perspectives.items():
                if typ and typ != 'null':
                    ctx = standard_contexts.get(power)
                    if ctx:
                        constraint.classifications.append((typ, ctx))

    def parse_pl_files(self):
        """Extract structured data from raw .pl files"""
        if not self.testsets_dir.exists():
            print(f"Warning: {self.testsets_dir} not found")
            return

        pl_files = list(self.testsets_dir.glob('*.pl'))
        print(f"  Found {len(pl_files)} .pl files")

        for filepath in pl_files:
            try:
                with open(filepath, 'r', encoding='utf-8') as f:
                    content = f.read()
            except Exception as e:
                print(f"  Error reading {filepath.name}: {e}")
                continue

            # Use canonical ID from constraint_claim (matches pipeline_output.json),
            # falling back to filename stem for .pl files without one.
            claim_match = re.search(r'constraint_claim\((\w+),', content)
            constraint_id = claim_match.group(1) if claim_match else filepath.stem

            # Track filename→canonical mapping for orbit_data.json (keyed by stem)
            if filepath.stem != constraint_id:
                self.filename_to_canonical[filepath.stem] = constraint_id

            # Get or create constraint
            if constraint_id not in self.constraints:
                self.constraints[constraint_id] = ConstraintData(constraint_id)

            constraint = self.constraints[constraint_id]

            # Extract human-readable description
            hr_match = re.search(r'human_readable:\s*["\']([^"\']+)["\']', content)
            if hr_match:
                constraint.human_readable = hr_match.group(1)

            # Extract domain
            domain_match = re.search(r'domain:\s*(\w+)', content)
            if domain_match:
                constraint.domain = domain_match.group(1).lower()

            # Alternative domain extraction from category_of
            cat_match = re.search(
                r'domain_priors:category_of\([^,]+,\s*(\w+)\)',
                content
            )
            if cat_match and not constraint.domain:
                constraint.domain = cat_match.group(1).lower()

            # Extract emerges_naturally
            if re.search(r'emerges_naturally\([^)]+\)', content):
                constraint.emerges_naturally = True
                constraint.requires_enforcement = False

            # Extract requires_active_enforcement
            if re.search(r'requires_active_enforcement\([^)]+\)', content):
                constraint.requires_enforcement = True
                constraint.emerges_naturally = False

            # Extract classifications
            # Pattern: constraint_classification(id, type, context(agent_power(...), ...))
            # Directly captures the 4 nested context params to avoid broken [^)]+ on nested parens
            for class_match in re.finditer(
                r'constraint_classification\s*\(\s*\S+,\s*'
                r'(mountain|rope|tangled_rope|snare|scaffold|piton)\s*,\s*'
                r'context\(\s*agent_power\((\w+)\)\s*,\s*'
                r'time_horizon\((\w+)\)\s*,\s*'
                r'exit_options\((\w+)\)\s*,\s*'
                r'spatial_scope\((\w+)\)\s*\)',
                content
            ):
                constraint_type = class_match.group(1)
                context_dict = {
                    'agent_power': class_match.group(2),
                    'time_horizon': class_match.group(3),
                    'exit_options': class_match.group(4),
                    'spatial_scope': class_match.group(5)
                }
                constraint.classifications.append((constraint_type, context_dict))

    def calculate_variance(self):
        """Calculate variance ratios for each constraint"""
        for constraint in self.constraints.values():
            if not constraint.classifications:
                constraint.variance_ratio = None
                constraint.index_configs_count = 0
                constraint.types_produced_count = 0
                continue

            # Get unique index configs (convert dicts to frozen tuples for set membership)
            unique_contexts = set()
            for _, context in constraint.classifications:
                if isinstance(context, dict):
                    unique_contexts.add(tuple(sorted(context.items())))
                elif isinstance(context, tuple):
                    unique_contexts.add(context)
                else:
                    unique_contexts.add(context)

            constraint.index_configs_count = len(unique_contexts)

            # Get unique types produced
            unique_types = set(t for t, _ in constraint.classifications)
            constraint.types_produced_count = len(unique_types)

            # Calculate variance ratio
            if constraint.index_configs_count > 0:
                constraint.variance_ratio = (
                    constraint.types_produced_count / constraint.index_configs_count
                )
            else:
                constraint.variance_ratio = None

    def infer_domains(self):
        """Infer domains from constraint names if missing"""
        domain_patterns = {
            'technical': ['protocol', 'algorithm', 'standard', 'rfc', 'tcp', 'ip',
                         'http', 'ssl', 'api', 'code', 'software', 'hardware'],
            'social': ['law', 'policy', 'norm', 'social', 'cultural', 'community',
                      'tradition', 'custom', 'etiquette', 'convention'],
            'economic': ['market', 'tax', 'trade', 'price', 'economic', 'financial',
                        'currency', 'credit', 'investment', 'capital', 'interest'],
            'biological': ['gene', 'evolution', 'species', 'disease', 'biology',
                          'organism', 'cell', 'protein', 'dna', 'ecology'],
            'narrative': ['ulysses', 'story', 'character', 'stephen', 'bloom',
                         'molly', 'dublin', 'joyce', 'novel', 'chp'],
            'legal': ['statute', 'regulation', 'doctrine', 'court', 'judicial',
                     'constitutional', 'criminal', 'civil', 'tort', 'contract'],
            'physical': ['gravity', 'physics', 'thermodynamics', 'quantum',
                        'mechanics', 'electromagnetic', 'particle', 'wave'],
            'mathematical': ['theorem', 'proof', 'axiom', 'lemma', 'conjecture',
                           'paradox', 'equation', 'formula', 'geometric'],
            'formal_logic': ['logic', 'proof', 'axiom', 'theorem', 'tautology',
                           'contradiction', 'inference', 'deduction']
        }

        for constraint in self.constraints.values():
            if constraint.domain:
                continue

            # Check constraint ID against patterns
            constraint_lower = constraint.constraint_id.lower()

            scores = defaultdict(int)
            for domain, keywords in domain_patterns.items():
                for keyword in keywords:
                    if keyword in constraint_lower:
                        scores[domain] += 1

            if scores:
                # Pick highest scoring domain
                best_domain = max(scores.items(), key=lambda x: x[1])[0]
                constraint.domain = best_domain

    def load_orbit_data(self):
        """Enrich constraints with orbit signatures from orbit_data.json"""
        orbit_path = self.output_txt_path.parent / 'orbit_data.json'
        if not orbit_path.exists():
            print(f"  orbit_data.json not found at {orbit_path} — skipping")
            return

        try:
            with open(orbit_path, 'r', encoding='utf-8') as f:
                orbit_data = json.load(f)
        except (json.JSONDecodeError, OSError) as e:
            print(f"  Error reading orbit_data.json: {e}")
            return

        matched = 0
        for cid, entry in orbit_data.items():
            # orbit_data.json keys by filename stem; resolve to canonical atom ID
            canonical = self.filename_to_canonical.get(cid, cid)
            if canonical in self.constraints:
                self.constraints[canonical].orbit_signature = entry.get('orbit_signature')
                self.constraints[canonical].orbit_contexts = entry.get('contexts')
                matched += 1
        print(f"  Matched orbit data for {matched}/{len(orbit_data)} constraints")

    def save_json(self, output_path):
        """Save extracted data to JSON"""
        data = {
            'constraints': {
                cid: constraint.to_dict()
                for cid, constraint in self.constraints.items()
            },
            'summary': {
                'total_constraints': len(self.constraints),
                'with_extractiveness': sum(1 for c in self.constraints.values()
                                          if c.extractiveness is not None),
                'with_suppression': sum(1 for c in self.constraints.values()
                                       if c.suppression is not None),
                'with_classifications': sum(1 for c in self.constraints.values()
                                          if c.classifications),
                'with_domain': sum(1 for c in self.constraints.values()
                                  if c.domain is not None),
                'with_orbit_data': sum(1 for c in self.constraints.values()
                                       if c.orbit_signature is not None)
            }
        }

        with open(output_path, 'w', encoding='utf-8') as f:
            json.dump(data, f, indent=2)

        print(f"\nSaved to {output_path}")
        print(f"Summary:")
        for key, value in data['summary'].items():
            pct = (value / data['summary']['total_constraints'] * 100) if data['summary']['total_constraints'] > 0 else 0
            print(f"  {key}: {value} ({pct:.1f}%)")

def main():
    import argparse

    parser = argparse.ArgumentParser(
        description='Extract corpus data from pipeline_output.json and .pl files'
    )
    parser.add_argument(
        '--output-txt',
        default='../outputs/output.txt',
        # Legacy name: actually used to locate sibling files (pipeline_output.json,
        # orbit_data.json) via parent directory. Effectively --output-dir.
        help='Path to outputs directory anchor (pipeline_output.json is read as sibling)'
    )
    parser.add_argument(
        '--testsets',
        default='../prolog/testsets/',
        help='Path to testsets directory'
    )
    parser.add_argument(
        '--json-output',
        default='../outputs/corpus_data.json',
        help='Output JSON file'
    )

    args = parser.parse_args()

    extractor = CorpusExtractor(args.output_txt, args.testsets)
    constraints = extractor.extract_all()

    print(f"\nExtracted {len(constraints)} constraints")

    extractor.save_json(args.json_output)

if __name__ == '__main__':
    main()
