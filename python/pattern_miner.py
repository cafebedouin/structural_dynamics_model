#!/usr/bin/env python3
"""
Pattern Miner - Report 3: Structural Pattern Mining

Finds structural twins and patterns that suggest new constraint categories.
Identifies candidates for: Tangled Rope, Piton, Scaffold, Wings
"""

import json
from collections import defaultdict, Counter
from pathlib import Path

class PatternMiner:
    """Mines structural patterns to identify candidate categories"""

    def __init__(self, corpus_data_path):
        with open(corpus_data_path, 'r') as f:
            data = json.load(f)

        self.constraints = data['constraints']
        self.summary = data['summary']

    def analyze(self):
        """Run complete pattern mining analysis"""
        results = {}

        results['structural_twins'] = self.find_structural_twins()
        results['candidate_categories'] = self.identify_candidate_categories()
        results['hybrid_patterns'] = self.find_hybrid_patterns()
        results['transition_markers'] = self.find_transition_markers()
        results['recommendations'] = self.generate_recommendations(results)

        return results

    def structural_signature(self, constraint):
        """Calculate structural signature for grouping"""
        metrics = constraint.get('metrics', {})

        extractiveness = metrics.get('extractiveness')
        suppression = metrics.get('suppression')
        emerges = metrics.get('emerges_naturally')
        enforced = metrics.get('requires_enforcement')

        # Round metrics to 1 decimal for grouping
        if extractiveness is not None:
            extractiveness = round(extractiveness, 1)
        if suppression is not None:
            suppression = round(suppression, 1)

        return (extractiveness, suppression, emerges, enforced)

    def find_structural_twins(self):
        """Find constraints with same structure but different claimed types"""
        by_signature = defaultdict(list)

        for cid, constraint in self.constraints.items():
            signature = self.structural_signature(constraint)
            domain = constraint.get('domain') or 'unknown'
            by_signature[signature].append({
                'id': cid,
                'claimed_type': constraint.get('claimed_type'),
                'domain': domain,
                'extractiveness': constraint.get('metrics', {}).get('extractiveness'),
                'suppression': constraint.get('metrics', {}).get('suppression'),
                'emerges_naturally': constraint.get('metrics', {}).get('emerges_naturally'),
                'requires_enforcement': constraint.get('metrics', {}).get('requires_enforcement')
            })

        # Find groups with multiple types
        twins = []
        for signature, group in by_signature.items():
            if len(group) > 1:
                types = [c['claimed_type'] for c in group if c['claimed_type']]
                unique_types = set(types)

                if len(unique_types) > 1:
                    twins.append({
                        'signature': signature,
                        'count': len(group),
                        'types_present': list(unique_types),
                        'domains': list(set(c['domain'] for c in group)),
                        'examples': [c['id'] for c in group[:5]]
                    })

        # Sort by count (most common patterns first)
        twins.sort(key=lambda x: x['count'], reverse=True)

        return twins

    def identify_candidate_categories(self):
        """Identify constraints matching candidate category patterns"""
        candidates = {
            'tangled_rope': [],
            'piton': [],
            'scaffold': [],
            'wings': []
        }

        for cid, constraint in self.constraints.items():
            metrics = constraint.get('metrics', {})
            extractiveness = metrics.get('extractiveness')
            suppression = metrics.get('suppression')
            emerges = metrics.get('emerges_naturally')
            enforced = metrics.get('requires_enforcement')

            # Skip if missing critical metrics
            if extractiveness is None or suppression is None:
                continue

            domain = constraint.get('domain') or 'unknown'
            entry = {
                'constraint_id': cid,
                'claimed_type': constraint.get('claimed_type'),
                'domain': domain,
                'extractiveness': extractiveness,
                'suppression': suppression,
                'emerges_naturally': emerges,
                'requires_enforcement': enforced
            }

            # NOTE: The thresholds below are INTENTIONALLY SEPARATE from the
            # engine gate thresholds in prolog/config.pl. These are independent
            # analytical heuristics for exploratory pattern mining — they cast a
            # wider or different net than the deterministic classification gates.
            # Do NOT synchronize these with config.pl values.

            # Tangled Rope: High extraction + High suppression + Enforced
            # (Mix of snare and rope characteristics)
            # Heuristic: ε≥0.6, s≥0.6 (wider than config tangled_rope_extraction_floor=0.16)
            if (extractiveness >= 0.6 and
                suppression >= 0.6 and
                enforced):
                candidates['tangled_rope'].append(entry)

            # Piton: High suppression + Enforced but still claimed as mountain
            # (False mountain that's obviously constructed)
            # Heuristic: s≥0.7 (no piton suppression param in config)
            if (suppression >= 0.7 and
                enforced and
                constraint.get('claimed_type') == 'mountain'):
                candidates['piton'].append(entry)

            # Scaffold: Medium everything, temporary transition markers
            # (Not extreme on any dimension)
            # Heuristic: 0.3≤ε≤0.6 (different semantics from config scaffold_extraction_ceil=0.30)
            if (0.3 <= extractiveness <= 0.6 and
                0.3 <= suppression <= 0.6):
                candidates['scaffold'].append(entry)

            # Wings: Low extraction + Low suppression + Emerges naturally
            # (Enabling constraints, opposite of snare)
            # Heuristic: ε≤0.3, s≤0.3 (no config equivalent)
            if (extractiveness <= 0.3 and
                suppression <= 0.3 and
                emerges):
                candidates['wings'].append(entry)

        # Add counts and statistics
        category_stats = {}
        for category, items in candidates.items():
            if items:
                type_distribution = Counter(item['claimed_type'] for item in items if item['claimed_type'])
                domain_distribution = Counter(item['domain'] for item in items)

                category_stats[category] = {
                    'count': len(items),
                    'examples': items[:10],
                    'type_distribution': dict(type_distribution),
                    'domain_distribution': dict(domain_distribution)
                }
            else:
                category_stats[category] = {
                    'count': 0,
                    'examples': [],
                    'type_distribution': {},
                    'domain_distribution': {}
                }

        return category_stats

    def find_hybrid_patterns(self):
        """Find constraints exhibiting hybrid characteristics"""
        hybrids = []

        for cid, constraint in self.constraints.items():
            metrics = constraint.get('metrics', {})
            extractiveness = metrics.get('extractiveness')
            suppression = metrics.get('suppression')

            if extractiveness is None or suppression is None:
                continue

            # Hybrid: Both extractiveness and suppression are significant
            # (Not clearly snare, rope, or mountain)
            if extractiveness >= 0.5 and suppression >= 0.5:
                analysis = constraint.get('analysis', {})
                variance = analysis.get('variance_ratio')
                domain = constraint.get('domain') or 'unknown'

                hybrids.append({
                    'constraint_id': cid,
                    'claimed_type': constraint.get('claimed_type'),
                    'domain': domain,
                    'extractiveness': extractiveness,
                    'suppression': suppression,
                    'variance_ratio': variance,
                    'is_constructed': analysis.get('is_constructed')
                })

        # Sort by total intensity (extractiveness + suppression)
        hybrids.sort(key=lambda x: (x['extractiveness'] + x['suppression']), reverse=True)

        return hybrids

    def find_transition_markers(self):
        """Find constraints that might represent transitions between states"""
        transitions = []

        for cid, constraint in self.constraints.items():
            metrics = constraint.get('metrics', {})
            extractiveness = metrics.get('extractiveness')
            suppression = metrics.get('suppression')
            resistance = metrics.get('resistance')

            if extractiveness is None or suppression is None:
                continue

            # Transition marker: Medium values suggest in-between state
            # Check if values are in the middle range
            mid_range_count = 0
            if 0.3 <= extractiveness <= 0.7:
                mid_range_count += 1
            if 0.3 <= suppression <= 0.7:
                mid_range_count += 1
            if resistance is not None and 0.3 <= resistance <= 0.7:
                mid_range_count += 1

            # If 2+ metrics are mid-range, could be transition
            if mid_range_count >= 2:
                domain = constraint.get('domain') or 'unknown'
                transitions.append({
                    'constraint_id': cid,
                    'claimed_type': constraint.get('claimed_type'),
                    'domain': domain,
                    'extractiveness': extractiveness,
                    'suppression': suppression,
                    'resistance': resistance,
                    'mid_range_count': mid_range_count
                })

        # Sort by mid-range count
        transitions.sort(key=lambda x: x['mid_range_count'], reverse=True)

        return transitions

    def generate_recommendations(self, results):
        """Generate recommendations based on pattern findings"""
        recommendations = []

        # Check structural twins
        twins = results['structural_twins']
        if twins:
            high_count_twins = [t for t in twins if t['count'] >= 5]
            if high_count_twins:
                recommendations.append({
                    'priority': 'HIGH',
                    'finding': f"Found {len(high_count_twins)} structural signatures shared by 5+ constraints",
                    'action': "Investigate if these represent distinct categories beyond current framework",
                    'examples': [t['signature'] for t in high_count_twins[:3]]
                })

        # Check candidate categories
        candidates = results['candidate_categories']

        for category, stats in candidates.items():
            if stats['count'] >= 10:
                recommendations.append({
                    'priority': 'MEDIUM',
                    'finding': f"Found {stats['count']} constraints matching '{category}' pattern",
                    'action': f"Consider formalizing '{category}' as new category",
                    'details': f"Type distribution: {stats['type_distribution']}"
                })

        # Check hybrids
        hybrids = results['hybrid_patterns']
        if len(hybrids) >= 20:
            recommendations.append({
                'priority': 'HIGH',
                'finding': f"Found {len(hybrids)} hybrid constraints (high extraction + high suppression)",
                'action': "Strong evidence for 'Tangled Rope' category",
                'details': f"{len(hybrids)} constraints don't fit cleanly into mountain/rope/snare"
            })

        # Check transitions
        transitions = results['transition_markers']
        if len(transitions) >= 15:
            recommendations.append({
                'priority': 'MEDIUM',
                'finding': f"Found {len(transitions)} constraints with mid-range metrics",
                'action': "Consider 'Scaffold' category for temporary/transitional constraints",
                'details': "These constraints show characteristics of multiple types"
            })

        # Sort by priority
        priority_order = {'HIGH': 0, 'MEDIUM': 1, 'LOW': 2}
        recommendations.sort(key=lambda x: priority_order[x['priority']])

        return recommendations

    def generate_report(self, output_path):
        """Generate markdown report"""
        results = self.analyze()

        with open(output_path, 'w') as f:
            f.write("# Structural Pattern Mining\n\n")

            # Executive Summary
            f.write("## Executive Summary\n\n")

            recommendations = results['recommendations']
            if recommendations:
                f.write("### Key Findings\n\n")
                for i, rec in enumerate(recommendations[:5], 1):
                    f.write(f"{i}. **[{rec['priority']}]** {rec['finding']}\n")
                    f.write(f"   - Action: {rec['action']}\n")
                    if 'details' in rec:
                        f.write(f"   - Details: {rec['details']}\n")
                    f.write("\n")
            else:
                f.write("No significant patterns detected requiring new categories.\n\n")

            # Structural Twins
            f.write("## Structural Twins\n\n")
            f.write("Constraints with identical structural signatures but different claimed types.\n\n")

            twins = results['structural_twins']
            if twins:
                f.write(f"**Total twin groups found:** {len(twins)}\n\n")

                f.write("| Signature | Count | Types Present | Domains | Examples |\n")
                f.write("|-----------|-------|---------------|---------|----------|\n")

                for twin in twins[:20]:
                    sig_str = str(twin['signature'])[:30]
                    types_str = ', '.join(twin['types_present'])
                    # Filter out None domains before joining
                    valid_domains = [d for d in twin['domains'][:3] if d is not None]
                    domains_str = ', '.join(valid_domains) if valid_domains else 'unknown'
                    examples_str = ', '.join(twin['examples'][:2])

                    f.write(f"| {sig_str:30s} | {twin['count']:5d} | {types_str:20s} | {domains_str:15s} | {examples_str:30s} |\n")

                f.write("\n")
            else:
                f.write("No structural twins detected.\n\n")

            # Candidate Categories
            f.write("## Candidate Category Analysis\n\n")

            candidates = results['candidate_categories']

            for category in ['tangled_rope', 'piton', 'scaffold', 'wings']:
                stats = candidates[category]
                f.write(f"### {category.replace('_', ' ').title()}\n\n")

                if category == 'tangled_rope':
                    f.write("**Pattern:** High extraction + High suppression + Requires enforcement\n")
                    f.write("**Interpretation:** Mix of snare and rope characteristics\n\n")
                elif category == 'piton':
                    f.write("**Pattern:** High suppression + Enforced + Claimed as mountain\n")
                    f.write("**Interpretation:** False mountains that are obviously constructed\n\n")
                elif category == 'scaffold':
                    f.write("**Pattern:** Medium extractiveness + Medium suppression\n")
                    f.write("**Interpretation:** Temporary transition mechanisms\n\n")
                elif category == 'wings':
                    f.write("**Pattern:** Low extraction + Low suppression + Emerges naturally\n")
                    f.write("**Interpretation:** Enabling constraints, opposite of snare\n\n")

                if stats['count'] > 0:
                    f.write(f"**Constraints matching pattern:** {stats['count']}\n\n")

                    if stats['type_distribution']:
                        f.write("**Current type distribution:**\n")
                        for ctype, count in sorted(stats['type_distribution'].items(), key=lambda x: x[1], reverse=True):
                            f.write(f"- {ctype}: {count}\n")
                        f.write("\n")

                    if stats['examples']:
                        f.write("**Examples:**\n\n")
                        f.write("| Constraint ID | Claimed Type | Extractiveness | Suppression | Domain |\n")
                        f.write("|---------------|--------------|----------------|-------------|--------|\n")

                        for example in stats['examples'][:10]:
                            claimed = example['claimed_type'] or 'N/A'
                            f.write(f"| {example['constraint_id']:30s} | {claimed:15s} | {example['extractiveness']:14.2f} | {example['suppression']:11.2f} | {example['domain']:10s} |\n")

                        f.write("\n")
                else:
                    f.write("No constraints match this pattern.\n\n")

            # Hybrid Patterns
            f.write("## Hybrid Patterns\n\n")
            f.write("Constraints with both high extraction and high suppression.\n\n")

            hybrids = results['hybrid_patterns']
            if hybrids:
                f.write(f"**Total hybrids found:** {len(hybrids)}\n\n")

                f.write("| Constraint ID | Claimed Type | Extraction | Suppression | Total | Domain |\n")
                f.write("|---------------|--------------|------------|-------------|-------|--------|\n")

                for hybrid in hybrids[:20]:
                    total = hybrid['extractiveness'] + hybrid['suppression']
                    claimed = hybrid['claimed_type'] or 'N/A'
                    f.write(f"| {hybrid['constraint_id']:30s} | {claimed:15s} | {hybrid['extractiveness']:10.2f} | {hybrid['suppression']:11.2f} | {total:5.2f} | {hybrid['domain']:10s} |\n")

                f.write("\n")
                f.write("**Note:** High 'Total' values indicate constraints that don't fit cleanly into single category.\n\n")
            else:
                f.write("No significant hybrid patterns detected.\n\n")

            # Transition Markers
            f.write("## Transition Markers\n\n")
            f.write("Constraints with mid-range metrics suggesting transitional states.\n\n")

            transitions = results['transition_markers']
            if transitions:
                f.write(f"**Total transition markers found:** {len(transitions)}\n\n")

                f.write("| Constraint ID | Claimed Type | Extraction | Suppression | Resistance | Domain |\n")
                f.write("|---------------|--------------|------------|-------------|------------|--------|\n")

                for trans in transitions[:20]:
                    resistance_str = f"{trans['resistance']:.2f}" if trans['resistance'] is not None else 'N/A'
                    claimed = trans['claimed_type'] or 'N/A'
                    f.write(f"| {trans['constraint_id']:30s} | {claimed:15s} | {trans['extractiveness']:10.2f} | {trans['suppression']:11.2f} | {resistance_str:10s} | {trans['domain']:10s} |\n")

                f.write("\n")
            else:
                f.write("No significant transition markers detected.\n\n")

            # Recommendations
            f.write("## Recommendations\n\n")

            if recommendations:
                for i, rec in enumerate(recommendations, 1):
                    f.write(f"### {i}. {rec['finding']}\n\n")
                    f.write(f"**Priority:** {rec['priority']}\n\n")
                    f.write(f"**Recommended Action:** {rec['action']}\n\n")
                    if 'details' in rec:
                        f.write(f"**Details:** {rec['details']}\n\n")
                    if 'examples' in rec:
                        f.write("**Example signatures:**\n")
                        for ex in rec['examples']:
                            f.write(f"- {ex}\n")
                        f.write("\n")
            else:
                f.write("Current mountain/rope/snare taxonomy appears sufficient.\n")
                f.write("No strong evidence for new categories at this time.\n\n")

        print(f"Report generated: {output_path}")

def main():
    import argparse

    parser = argparse.ArgumentParser(
        description='Generate structural pattern mining report (Report 3)'
    )
    parser.add_argument(
        '--corpus-data',
        default='../outputs/corpus_data.json',
        help='Path to corpus_data.json'
    )
    parser.add_argument(
        '--output',
        default='../outputs/pattern_mining.md',
        help='Output markdown file'
    )

    args = parser.parse_args()

    miner = PatternMiner(args.corpus_data)
    miner.generate_report(args.output)

if __name__ == '__main__':
    main()
