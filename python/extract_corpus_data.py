#!/usr/bin/env python3
"""
Corpus Data Extractor - Parses pipeline_output.json

Reads all constraint metadata from pipeline_output.json (the single authoritative
source) and enriches with orbit data from orbit_data.json.
"""

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

    def __init__(self, output_txt_path):
        self.output_txt_path = Path(output_txt_path)
        self.constraints = {}  # id -> ConstraintData

    def extract_all(self):
        """Run complete extraction pipeline"""
        print("Step 1: Parsing pipeline_output.json...")
        self.parse_pipeline_json()

        print("Step 2: Calculating derived metrics...")
        self.calculate_variance()

        print("Step 3: Inferring missing data...")
        self.infer_domains()

        print("Step 4: Loading orbit data...")
        self.load_orbit_data()

        return self.constraints

    def parse_pipeline_json(self):
        """Extract all constraint data from pipeline_output.json"""
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

            # Fields previously parsed from .pl files
            constraint.human_readable = entry.get('human_readable')
            # Prefer topic_domain (subject area) over domain (classification type)
            constraint.domain = entry.get('topic_domain') or entry.get('domain')
            constraint.emerges_naturally = entry.get('emerges_naturally')
            constraint.requires_enforcement = entry.get('requires_active_enforcement')

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

            # Classifications from authoritative classifications array
            for cls in entry.get('classifications', []):
                constraint.classifications.append((
                    cls['type'],
                    cls['context']
                ))

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
            if cid in self.constraints:
                self.constraints[cid].orbit_signature = entry.get('orbit_signature')
                self.constraints[cid].orbit_contexts = entry.get('contexts')
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
        description='Extract corpus data from pipeline_output.json'
    )
    parser.add_argument(
        '--output-txt',
        default='../outputs/output.txt',
        help='Path to outputs directory anchor (pipeline_output.json is read as sibling)'
    )
    parser.add_argument(
        '--json-output',
        default='../outputs/corpus_data.json',
        help='Output JSON file'
    )

    args = parser.parse_args()

    extractor = CorpusExtractor(args.output_txt)
    constraints = extractor.extract_all()

    print(f"\nExtracted {len(constraints)} constraints")

    extractor.save_json(args.json_output)

if __name__ == '__main__':
    main()
