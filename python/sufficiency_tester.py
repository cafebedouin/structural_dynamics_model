#!/usr/bin/env python3
"""
Sufficiency Tester - Report 2: Index Sufficiency Test

Determines if the 4 indices (agent_power, time_horizon, exit_options, spatial_scope)
are sufficient to explain all constraint type variance, or if new categories are needed.
"""

import json
from collections import defaultdict, Counter
from pathlib import Path

class SufficiencyTester:
    """Tests whether 4 indices sufficiently explain constraint variance"""

    def __init__(self, corpus_data_path):
        with open(corpus_data_path, 'r') as f:
            data = json.load(f)

        self.constraints = data['constraints']
        self.summary = data['summary']

    def analyze(self):
        """Run complete sufficiency analysis"""
        results = {}

        results['index_collisions'] = self.detect_index_collisions()
        results['collision_patterns'] = self.analyze_collision_patterns(results['index_collisions'])
        results['domain_sufficiency'] = self.analyze_domain_sufficiency()
        results['stability_anomalies'] = self.find_stability_anomalies()
        results['evidence_summary'] = self.summarize_evidence(results)

        return results

    def detect_index_collisions(self):
        """Find cases where same index config produces multiple types"""
        collisions = []

        for cid, constraint in self.constraints.items():
            classifications = constraint.get('classifications', [])

            if not classifications:
                continue

            # Group classifications by index configuration
            by_index = defaultdict(list)
            for classification in classifications:
                context = classification.get('context')
                ctype = classification.get('type')

                if context and ctype:
                    # Convert context to tuple for hashing
                    if isinstance(context, dict):
                        index_key = (
                            context.get('agent_power'),
                            context.get('time_horizon'),
                            context.get('exit_options'),
                            context.get('spatial_scope')
                        )
                    elif isinstance(context, tuple):
                        index_key = context
                    else:
                        # String representation - skip for collision detection
                        continue

                    by_index[index_key].append(ctype)

            # Find collisions (same index config -> multiple types)
            for index_config, types in by_index.items():
                unique_types = set(types)
                if len(unique_types) > 1:
                    collisions.append({
                        'constraint_id': cid,
                        'index_config': index_config,
                        'types_produced': list(unique_types),
                        'type_count': len(unique_types),
                        'domain': constraint.get('domain', 'unknown'),
                        'claimed_type': constraint.get('claimed_type'),
                        'extractiveness': constraint.get('metrics', {}).get('extractiveness'),
                        'suppression': constraint.get('metrics', {}).get('suppression')
                    })

        return collisions

    def analyze_collision_patterns(self, collisions):
        """Identify patterns in index collisions"""
        if not collisions:
            return {
                'total_collisions': 0,
                'constraints_affected': 0,
                'common_transitions': [],
                'domain_breakdown': []
            }

        # Count transitions (type A -> type B)
        transitions = Counter()
        constraints_affected = set()
        by_domain = defaultdict(list)

        for collision in collisions:
            constraints_affected.add(collision['constraint_id'])
            domain = collision['domain']
            by_domain[domain].append(collision)

            # Record all type pairs
            types = sorted(collision['types_produced'])
            if len(types) == 2:
                transition = f"{types[0]} ↔ {types[1]}"
                transitions[transition] += 1

        # Domain breakdown
        domain_stats = []
        for domain in sorted(by_domain.keys()):
            domain_collisions = by_domain[domain]
            domain_stats.append({
                'domain': domain,
                'collision_count': len(domain_collisions),
                'constraints_affected': len(set(c['constraint_id'] for c in domain_collisions))
            })

        # Sort by frequency
        domain_stats.sort(key=lambda x: x['collision_count'], reverse=True)

        return {
            'total_collisions': len(collisions),
            'constraints_affected': len(constraints_affected),
            'common_transitions': transitions.most_common(10),
            'domain_breakdown': domain_stats
        }

    def analyze_domain_sufficiency(self):
        """Check if indices work better in some domains than others"""
        by_domain = defaultdict(lambda: {
            'total': 0,
            'with_collisions': 0,
            'avg_variance': 0.0,
            'variances': []
        })

        for cid, constraint in self.constraints.items():
            domain = constraint.get('domain', 'unknown')
            variance = constraint.get('analysis', {}).get('variance_ratio')

            by_domain[domain]['total'] += 1
            if variance is not None:
                by_domain[domain]['variances'].append(variance)

        # Calculate stats
        domain_stats = []
        for domain, stats in by_domain.items():
            if stats['variances']:
                avg_var = sum(stats['variances']) / len(stats['variances'])
                # High variance suggests indices ARE working (capturing real differences)
                # Low variance suggests indices might be MISSING something

                domain_stats.append({
                    'domain': domain,
                    'total_constraints': stats['total'],
                    'avg_variance': avg_var,
                    'sufficiency_score': avg_var  # Higher = indices capture more variance
                })

        # Sort by sufficiency score
        domain_stats.sort(key=lambda x: x['sufficiency_score'], reverse=True)

        return domain_stats

    def find_stability_anomalies(self):
        """Find constraints that are TOO stable (might need new categories)"""
        anomalies = []

        for cid, constraint in self.constraints.items():
            analysis = constraint.get('analysis', {})
            configs = analysis.get('index_configs', 0)
            types = analysis.get('types_produced', 0)
            variance = analysis.get('variance_ratio')

            # Anomaly: Many index configs but always same type
            # This suggests the constraint doesn't care about indices
            # Could indicate need for new category
            if configs >= 5 and types == 1:
                metrics = constraint.get('metrics', {})

                anomalies.append({
                    'constraint_id': cid,
                    'index_configs': configs,
                    'consistent_type': constraint.get('claimed_type'),
                    'domain': constraint.get('domain', 'unknown'),
                    'extractiveness': metrics.get('extractiveness'),
                    'suppression': metrics.get('suppression'),
                    'emerges_naturally': metrics.get('emerges_naturally'),
                    'requires_enforcement': metrics.get('requires_enforcement')
                })

        # Sort by number of configs tested
        anomalies.sort(key=lambda x: x['index_configs'], reverse=True)

        return anomalies

    def summarize_evidence(self, results):
        """Summarize evidence for/against index sufficiency"""
        collisions = results['index_collisions']
        patterns = results['collision_patterns']
        anomalies = results['stability_anomalies']

        total_constraints = len(self.constraints)
        constraints_with_collisions = patterns['constraints_affected']
        constraints_with_anomalies = len(anomalies)

        # Calculate percentages
        collision_pct = (constraints_with_collisions / total_constraints * 100) if total_constraints > 0 else 0
        anomaly_pct = (constraints_with_anomalies / total_constraints * 100) if total_constraints > 0 else 0

        # Evidence AGAINST sufficiency
        evidence_against = []
        if collision_pct > 10:
            evidence_against.append(f"{collision_pct:.1f}% of constraints have index collisions")
        if anomaly_pct > 20:
            evidence_against.append(f"{anomaly_pct:.1f}% of constraints are index-invariant")
        if collisions:
            common_transition = patterns['common_transitions'][0] if patterns['common_transitions'] else None
            if common_transition:
                evidence_against.append(f"Most common collision: {common_transition[0]} ({common_transition[1]} cases)")

        # Evidence FOR sufficiency
        evidence_for = []
        stable_pct = 100 - collision_pct
        if collision_pct < 5:
            evidence_for.append(f"{stable_pct:.1f}% of constraints have no index collisions")

        # Check domain coverage
        domain_stats = results['domain_sufficiency']
        if domain_stats:
            high_sufficiency_domains = [d for d in domain_stats if d['sufficiency_score'] > 0.7]
            if high_sufficiency_domains:
                evidence_for.append(f"{len(high_sufficiency_domains)} domains show high index sufficiency")

        return {
            'collision_rate': collision_pct,
            'anomaly_rate': anomaly_pct,
            'evidence_against_sufficiency': evidence_against,
            'evidence_for_sufficiency': evidence_for,
            'verdict': self._calculate_verdict(collision_pct, anomaly_pct)
        }

    def _calculate_verdict(self, collision_pct, anomaly_pct):
        """Determine overall verdict on index sufficiency"""
        if collision_pct > 20 or anomaly_pct > 30:
            return "INSUFFICIENT - Indices do not explain all variance. New categories recommended."
        elif collision_pct > 10 or anomaly_pct > 20:
            return "MIXED - Indices work for most cases but gaps exist. Consider hybrid approach."
        else:
            return "SUFFICIENT - Indices explain most variance. Current framework adequate."

    def generate_report(self, output_path):
        """Generate markdown report"""
        results = self.analyze()

        with open(output_path, 'w') as f:
            f.write("# Index Sufficiency Test\n\n")

            # Executive Summary
            f.write("## Executive Summary\n\n")
            summary = results['evidence_summary']
            f.write(f"**Verdict:** {summary['verdict']}\n\n")
            f.write(f"- **Collision Rate:** {summary['collision_rate']:.1f}%\n")
            f.write(f"- **Anomaly Rate:** {summary['anomaly_rate']:.1f}%\n\n")

            # Evidence Against Sufficiency
            if summary['evidence_against_sufficiency']:
                f.write("### Evidence Against Index Sufficiency\n\n")
                for evidence in summary['evidence_against_sufficiency']:
                    f.write(f"- {evidence}\n")
                f.write("\n")

            # Evidence For Sufficiency
            if summary['evidence_for_sufficiency']:
                f.write("### Evidence For Index Sufficiency\n\n")
                for evidence in summary['evidence_for_sufficiency']:
                    f.write(f"- {evidence}\n")
                f.write("\n")

            # Index Collisions
            f.write("## Index Collisions\n\n")
            collisions = results['index_collisions']
            patterns = results['collision_patterns']

            f.write(f"**Total collisions detected:** {patterns['total_collisions']}\n")
            f.write(f"**Constraints affected:** {patterns['constraints_affected']}\n\n")

            if patterns['common_transitions']:
                f.write("### Most Common Type Transitions\n\n")
                f.write("| Transition | Frequency |\n")
                f.write("|------------|----------|\n")
                for transition, count in patterns['common_transitions']:
                    f.write(f"| {transition} | {count} |\n")
                f.write("\n")

            # Domain Breakdown
            if patterns['domain_breakdown']:
                f.write("### Collisions by Domain\n\n")
                f.write("| Domain | Collisions | Constraints Affected |\n")
                f.write("|--------|-----------|---------------------|\n")
                for domain_stat in patterns['domain_breakdown']:
                    domain = domain_stat['domain'] or 'unknown'
                    f.write(f"| {domain:20s} | {domain_stat['collision_count']:10d} | {domain_stat['constraints_affected']:20d} |\n")
                f.write("\n")

            # Collision Examples
            if collisions:
                f.write("### Collision Examples\n\n")
                f.write("Cases where same index configuration produces multiple types:\n\n")
                f.write("| Constraint ID | Index Config | Types Produced | Domain |\n")
                f.write("|---------------|--------------|----------------|--------|\n")

                for collision in collisions[:15]:
                    types_str = ', '.join(collision['types_produced'])
                    index_str = str(collision['index_config'])[:30] + "..."
                    domain = collision['domain'] or 'unknown'
                    f.write(f"| {collision['constraint_id']:30s} | {index_str:30s} | {types_str:20s} | {domain:10s} |\n")

                f.write("\n")

            # Domain Sufficiency Analysis
            f.write("## Domain Sufficiency Analysis\n\n")
            f.write("How well do indices explain variance in each domain?\n\n")
            f.write("| Domain | Constraints | Avg Variance | Sufficiency |\n")
            f.write("|--------|-------------|--------------|-------------|\n")

            for domain_stat in results['domain_sufficiency']:
                score = domain_stat['sufficiency_score']
                rating = "High" if score > 0.7 else "Medium" if score > 0.4 else "Low"
                domain = domain_stat['domain'] or 'unknown'
                f.write(f"| {domain:20s} | {domain_stat['total_constraints']:11d} | {domain_stat['avg_variance']:12.2f} | {rating:11s} |\n")

            f.write("\n")
            f.write("**Note:** Higher variance = indices capture more differences (good)\n\n")

            # Stability Anomalies
            if results['stability_anomalies']:
                f.write("## Stability Anomalies\n\n")
                f.write("Constraints tested across many index configs but always produce same type.\n")
                f.write("These may indicate need for new categories beyond current 4 indices.\n\n")

                f.write("| Constraint ID | Configs | Type | Domain | Notes |\n")
                f.write("|---------------|---------|------|--------|-------|\n")

                for anomaly in results['stability_anomalies'][:20]:
                    notes = []
                    if anomaly.get('emerges_naturally'):
                        notes.append("natural")
                    if anomaly.get('requires_enforcement'):
                        notes.append("enforced")
                    notes_str = ', '.join(notes) if notes else '-'
                    consistent_type = anomaly['consistent_type'] or 'N/A'
                    domain = anomaly['domain'] or 'unknown'

                    f.write(f"| {anomaly['constraint_id']:30s} | {anomaly['index_configs']:7d} | {consistent_type:15s} | {domain:10s} | {notes_str} |\n")

                f.write("\n")

            # Recommendations
            f.write("## Recommendations\n\n")

            if summary['collision_rate'] > 15:
                f.write("1. **High collision rate detected.** Consider:\n")
                f.write("   - Adding 5th index dimension\n")
                f.write("   - Creating hybrid categories (Tangled Rope, Piton, etc.)\n")
                f.write("   - Refining existing index definitions\n\n")

            if summary['anomaly_rate'] > 25:
                f.write("2. **Many index-invariant constraints.** These may represent:\n")
                f.write("   - True natural laws (legitimately invariant)\n")
                f.write("   - Need for categorical rather than indexical classification\n")
                f.write("   - Candidates for new categories: Scaffold, Wings\n\n")

            if summary['collision_rate'] < 10 and summary['anomaly_rate'] < 20:
                f.write("1. **Current 4 indices appear sufficient** for most constraints.\n")
                f.write("2. Consider adding new categories only for edge cases.\n")
                f.write("3. Focus on refining metric thresholds rather than structural changes.\n\n")

        print(f"Report generated: {output_path}")

def main():
    import argparse

    parser = argparse.ArgumentParser(
        description='Generate index sufficiency test (Report 2)'
    )
    parser.add_argument(
        '--corpus-data',
        default='../outputs/corpus_data.json',
        help='Path to corpus_data.json'
    )
    parser.add_argument(
        '--output',
        default='../outputs/index_sufficiency.md',
        help='Output markdown file'
    )

    args = parser.parse_args()

    tester = SufficiencyTester(args.corpus_data)
    tester.generate_report(args.output)

if __name__ == '__main__':
    main()
