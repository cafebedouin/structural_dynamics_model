#!/usr/bin/env python3
"""
Corpus Data Extractor - Parses both output.txt and raw .pl files

Combines:
- Diagnostic output from Prolog engine (output.txt)
- Raw structural data from .pl files
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

        # From output.txt analysis
        self.structural_signature = None
        self.is_constructed = None
        self.omegas = []

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
                'types_produced': self.types_produced_count
            }
        }

class CorpusExtractor:
    """Main extraction engine"""

    def __init__(self, output_txt_path, testsets_dir):
        self.output_txt_path = Path(output_txt_path)
        self.testsets_dir = Path(testsets_dir)
        self.constraints = {}  # id -> ConstraintData

    def extract_all(self):
        """Run complete extraction pipeline"""
        print("Step 1: Parsing output.txt...")
        self.parse_output_txt()

        print("Step 2: Parsing raw .pl files...")
        self.parse_pl_files()

        print("Step 3: Calculating derived metrics...")
        self.calculate_variance()

        print("Step 4: Inferring missing data...")
        self.infer_domains()

        return self.constraints

    def parse_output_txt(self):
        """Extract diagnostic data from Prolog engine output"""
        if not self.output_txt_path.exists():
            print(f"Warning: {self.output_txt_path} not found")
            return

        with open(self.output_txt_path, 'r', encoding='utf-8') as f:
            content = f.read()

        # Build a mapping from internal constraint IDs (from interval/3) to
        # filename-based IDs.  output.txt has lines like:
        #   [7] EXECUTING: testsets/continuum_hypothesis_2026.pl
        #   ...
        #   Constraint: ch_undecidability_2026
        # We use the filename stem as the canonical ID to avoid double-counting.
        internal_to_filename = {}
        current_filename_id = None
        for line in content.splitlines():
            exec_match = re.search(r'EXECUTING:\s+testsets/(.+?)\.pl', line)
            if exec_match:
                current_filename_id = exec_match.group(1)
            constraint_match = re.search(r'^Constraint:\s+(\w+)', line)
            if constraint_match and current_filename_id:
                internal_id = constraint_match.group(1)
                if internal_id != current_filename_id:
                    internal_to_filename[internal_id] = current_filename_id

        # Split into constraint blocks
        # Pattern: "Constraint: {id}" starts a block
        constraint_blocks = re.split(r'(?=Constraint:\s+\w+)', content)

        for block in constraint_blocks:
            if not block.strip():
                continue

            # Extract constraint ID
            id_match = re.search(r'Constraint:\s+(\w+)', block)
            if not id_match:
                continue

            internal_id = id_match.group(1)
            # Use filename-based ID as canonical key when available
            constraint_id = internal_to_filename.get(internal_id, internal_id)

            # Get or create constraint data
            if constraint_id not in self.constraints:
                self.constraints[constraint_id] = ConstraintData(constraint_id)

            constraint = self.constraints[constraint_id]

            # Extract claimed type (first encountered wins — the file's
            # declared type takes priority over bridge-derived types from
            # internal/interval IDs that map to the same canonical ID)
            type_match = re.search(r'Claimed Type:\s+(\w+)', block)
            if type_match and not constraint.claimed_type:
                constraint.claimed_type = type_match.group(1)

            # Extract perspectives/classifications
            perspectives_section = re.search(
                r'Perspectives:(.*?)(?=\n\n|\nConstraint:|\Z)',
                block,
                re.DOTALL
            )

            if perspectives_section:
                persp_text = perspectives_section.group(1)
                # Parse lines like: "- [context(...)]: type"
                for match in re.finditer(
                    r'-\s*\[([^\]]+)\]:\s*(\w+)',
                    persp_text
                ):
                    context_str = match.group(1)
                    constraint_type = match.group(2)
                    constraint.classifications.append((constraint_type, context_str))

            # Extract extractiveness
            extr_match = re.search(
                r'extractive_noose.*?Intensity:\s*([\d.]+)',
                block
            )
            if extr_match:
                constraint.extractiveness = float(extr_match.group(1))

            # Alternative extraction pattern
            extr_match2 = re.search(r'Base Extractiveness:\s*([\d.]+)', block)
            if extr_match2:
                constraint.extractiveness = float(extr_match2.group(1))

            # Extract suppression
            supp_match = re.search(r'Suppression Requirement:\s*([\d.]+)', block)
            if supp_match:
                constraint.suppression = float(supp_match.group(1))

            # Extract resistance
            resist_match = re.search(r'Resistance to Change:\s*([\d.]+)', block)
            if resist_match:
                constraint.resistance = float(resist_match.group(1))

            # Detect constructed constraint
            if 'CONSTRUCTED CONSTRAINT signature' in block:
                constraint.is_constructed = True
                constraint.structural_signature = 'constructed'
            elif 'natural_constraint' in block.lower():
                constraint.is_constructed = False
                constraint.structural_signature = 'natural'

            # Extract omegas
            for omega_match in re.finditer(r'Ω:\s+(omega_\w+)\s+\((\w+)\)', block):
                omega_id = omega_match.group(1)
                omega_type = omega_match.group(2)
                constraint.omegas.append({'id': omega_id, 'type': omega_type})

    def parse_pl_files(self):
        """Extract structured data from raw .pl files"""
        if not self.testsets_dir.exists():
            print(f"Warning: {self.testsets_dir} not found")
            return

        pl_files = list(self.testsets_dir.glob('*.pl'))
        print(f"  Found {len(pl_files)} .pl files")

        for filepath in pl_files:
            constraint_id = filepath.stem

            try:
                with open(filepath, 'r', encoding='utf-8') as f:
                    content = f.read()
            except Exception as e:
                print(f"  Error reading {filepath.name}: {e}")
                continue

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
                constraint.domain = domain_match.group(1)

            # Alternative domain extraction from category_of
            cat_match = re.search(
                r'domain_priors:category_of\([^,]+,\s*(\w+)\)',
                content
            )
            if cat_match and not constraint.domain:
                constraint.domain = cat_match.group(1)

            # Extract base_extractiveness
            extr_match = re.search(
                r'base_extractiveness\([^,]+,\s*([\d.]+)\)',
                content
            )
            if extr_match and not constraint.extractiveness:
                constraint.extractiveness = float(extr_match.group(1))

            # Extract suppression_score
            supp_match = re.search(
                r'suppression_score\([^,]+,\s*([\d.]+)\)',
                content
            )
            if supp_match and not constraint.suppression:
                constraint.suppression = float(supp_match.group(1))

            # Extract emerges_naturally
            if re.search(r'emerges_naturally\([^)]+\)', content):
                constraint.emerges_naturally = True
                constraint.requires_enforcement = False

            # Extract requires_active_enforcement
            if re.search(r'requires_active_enforcement\([^)]+\)', content):
                constraint.requires_enforcement = True
                constraint.emerges_naturally = False

            # Extract beneficiaries
            for ben_match in re.finditer(
                r'constraint_beneficiary\([^,]+,\s*(\w+)\)',
                content
            ):
                beneficiary = ben_match.group(1)
                if beneficiary not in constraint.beneficiaries:
                    constraint.beneficiaries.append(beneficiary)

            # Extract victims
            for vic_match in re.finditer(
                r'constraint_victim\([^,]+,\s*(\w+)\)',
                content
            ):
                victim = vic_match.group(1)
                if victim not in constraint.victims:
                    constraint.victims.append(victim)

            # Extract claimed type from constraint_claim
            claim_match = re.search(
                r'constraint_claim\([^,]+,\s*(\w+)\)',
                content
            )
            if claim_match and not constraint.claimed_type:
                constraint.claimed_type = claim_match.group(1)

            # Extract classifications
            # Pattern: constraint_classification(id, type, context(...))
            # Only match valid type atoms (lowercase) — excludes Prolog variables
            # like Type1, T1, etc. that appear in test blocks
            for class_match in re.finditer(
                r'constraint_classification\s*\(\s*[^,]+,\s*(mountain|rope|tangled_rope|snare|scaffold|piton),\s*context\(([^)]+)\)',
                content,
                re.DOTALL
            ):
                constraint_type = class_match.group(1)
                context_params = class_match.group(2)

                # Parse context parameters
                # agent_power(X), time_horizon(Y), exit_options(Z), spatial_scope(W)
                context_dict = {}
                for param in ['agent_power', 'time_horizon', 'exit_options', 'spatial_scope']:
                    param_match = re.search(
                        rf'{param}\(([^)]+)\)',
                        context_params
                    )
                    if param_match:
                        context_dict[param] = param_match.group(1).strip()

                # Create context tuple for comparison
                context_tuple = (
                    context_dict.get('agent_power'),
                    context_dict.get('time_horizon'),
                    context_dict.get('exit_options'),
                    context_dict.get('spatial_scope')
                )

                constraint.classifications.append((constraint_type, context_tuple))

    def calculate_variance(self):
        """Calculate variance ratios for each constraint"""
        for constraint in self.constraints.values():
            if not constraint.classifications:
                constraint.variance_ratio = None
                constraint.index_configs_count = 0
                constraint.types_produced_count = 0
                continue

            # Get unique index configs
            unique_contexts = set()
            for _, context in constraint.classifications:
                if isinstance(context, tuple):
                    unique_contexts.add(context)
                else:
                    # String representation - count it
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
                                  if c.domain is not None)
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
        description='Extract corpus data from output.txt and .pl files'
    )
    parser.add_argument(
        '--output-txt',
        default='../outputs/output.txt',
        help='Path to output.txt'
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
