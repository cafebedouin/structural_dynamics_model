#!/usr/bin/env python3
"""
Variance Analyzer - Report 1: Index Variance Analysis

Determines if constraints naturally cluster or if indices explain all variance.
"""

import json
from collections import defaultdict, Counter
from pathlib import Path

class VarianceAnalyzer:
    """Analyzes variance across index configurations"""

    def __init__(self, corpus_data_path):
        with open(corpus_data_path, 'r') as f:
            data = json.load(f)

        self.constraints = data['constraints']
        self.summary = data['summary']

    def analyze(self):
        """Run complete variance analysis"""
        results = {}

        results['summary'] = self.calculate_summary_statistics()
        results['variance_distribution'] = self.analyze_variance_distribution()
        results['domain_breakdown'] = self.analyze_by_domain()
        results['high_variance_examples'] = self.find_high_variance_examples()
        results['suspicious_stability'] = self.find_suspicious_stability()

        return results

    def calculate_summary_statistics(self):
        """Calculate overall corpus statistics"""
        total = len(self.constraints)

        with_multiple_configs = sum(
            1 for c in self.constraints.values()
            if c['analysis']['index_configs'] and c['analysis']['index_configs'] > 1
        )

        # Count by variance ratio
        high_variance = sum(
            1 for c in self.constraints.values()
            if c['analysis']['variance_ratio'] and c['analysis']['variance_ratio'] > 0.5
        )

        stable = sum(
            1 for c in self.constraints.values()
            if c['analysis']['variance_ratio'] == 1.0
        )

        return {
            'total_constraints': total,
            'with_multiple_configs': with_multiple_configs,
            'with_multiple_configs_pct': (with_multiple_configs / total * 100) if total > 0 else 0,
            'high_variance_count': high_variance,
            'high_variance_pct': (high_variance / total * 100) if total > 0 else 0,
            'stable_count': stable,
            'stable_pct': (stable / total * 100) if total > 0 else 0
        }

    def analyze_variance_distribution(self):
        """Break down variance ratios into buckets"""
        buckets = {
            '1.0 (stable)': [],
            '0.7-0.9': [],
            '0.5-0.6': [],
            '0.3-0.4': [],
            '<0.3': [],
            'null': []
        }

        for cid, constraint in self.constraints.items():
            ratio = constraint['analysis']['variance_ratio']

            if ratio is None:
                buckets['null'].append(cid)
            elif ratio == 1.0:
                buckets['1.0 (stable)'].append(cid)
            elif 0.7 <= ratio < 1.0:
                buckets['0.7-0.9'].append(cid)
            elif 0.5 <= ratio < 0.7:
                buckets['0.5-0.6'].append(cid)
            elif 0.3 <= ratio < 0.5:
                buckets['0.3-0.4'].append(cid)
            else:
                buckets['<0.3'].append(cid)

        # Convert to summary
        total = len(self.constraints)
        distribution = []

        for bucket_name in ['1.0 (stable)', '0.7-0.9', '0.5-0.6', '0.3-0.4', '<0.3', 'null']:
            constraints = buckets[bucket_name]
            count = len(constraints)
            pct = (count / total * 100) if total > 0 else 0

            # Get examples
            examples = constraints[:3] if len(constraints) > 0 else []

            distribution.append({
                'range': bucket_name,
                'count': count,
                'percentage': pct,
                'examples': examples
            })

        return distribution

    def analyze_by_domain(self):
        """Variance analysis broken down by domain"""
        by_domain = defaultdict(list)

        for cid, constraint in self.constraints.items():
            domain = constraint.get('domain', 'unknown')
            ratio = constraint['analysis']['variance_ratio']

            if ratio is not None:
                by_domain[domain].append(ratio)

        domain_stats = []
        # Filter out None domains and sort
        valid_domains = [d for d in by_domain.keys() if d is not None]
        for domain in sorted(valid_domains):
            ratios = by_domain[domain]
            n = len(ratios)

            if n == 0:
                continue

            avg_variance = sum(ratios) / n
            high_variance_count = sum(1 for r in ratios if r > 0.5)
            high_variance_pct = (high_variance_count / n * 100)

            domain_stats.append({
                'domain': domain,
                'n': n,
                'avg_variance': avg_variance,
                'high_variance_pct': high_variance_pct
            })

        # Sort by avg variance descending
        domain_stats.sort(key=lambda x: x['avg_variance'], reverse=True)

        return domain_stats

    def find_high_variance_examples(self, top_n=10):
        """Find most volatile constraints"""
        candidates = []

        for cid, constraint in self.constraints.items():
            ratio = constraint['analysis']['variance_ratio']
            if ratio and ratio > 0.5:
                candidates.append({
                    'constraint_id': cid,
                    'variance_ratio': ratio,
                    'index_configs': constraint['analysis']['index_configs'],
                    'types_produced': constraint['analysis']['types_produced'],
                    'domain': constraint.get('domain', 'unknown'),
                    'claimed_type': constraint.get('claimed_type'),
                    'classifications': constraint.get('classifications', [])
                })

        # Sort by variance ratio descending
        candidates.sort(key=lambda x: x['variance_ratio'], reverse=True)

        return candidates[:top_n]

    def find_suspicious_stability(self, threshold=5):
        """Find constraints that SHOULD vary but don't"""
        # Constraints with many index configs but low variance
        suspicious = []

        for cid, constraint in self.constraints.items():
            configs = constraint['analysis']['index_configs']
            ratio = constraint['analysis']['variance_ratio']

            if configs and configs >= threshold and ratio and ratio < 0.3:
                suspicious.append({
                    'constraint_id': cid,
                    'index_configs': configs,
                    'types_produced': constraint['analysis']['types_produced'],
                    'variance_ratio': ratio,
                    'domain': constraint.get('domain', 'unknown'),
                    'claimed_type': constraint.get('claimed_type')
                })

        # Sort by number of configs
        suspicious.sort(key=lambda x: x['index_configs'], reverse=True)

        return suspicious

    def generate_report(self, output_path):
        """Generate markdown report"""
        results = self.analyze()

        with open(output_path, 'w') as f:
            f.write("# Index Variance Analysis\n\n")

            # Summary Statistics
            f.write("## Summary Statistics\n\n")
            summary = results['summary']
            f.write(f"- **Total constraints analyzed:** {summary['total_constraints']}\n")
            f.write(f"- **Constraints with multiple index configs:** {summary['with_multiple_configs']} ({summary['with_multiple_configs_pct']:.1f}%)\n")
            f.write(f"- **High variance (>0.5):** {summary['high_variance_count']} ({summary['high_variance_pct']:.1f}%)\n")
            f.write(f"- **Stable (ratio=1.0):** {summary['stable_count']} ({summary['stable_pct']:.1f}%)\n\n")

            # Variance Distribution
            f.write("## Variance Distribution\n\n")
            f.write("| Ratio Range | Count | % of Corpus | Examples |\n")
            f.write("|-------------|-------|-------------|----------|\n")

            for bucket in results['variance_distribution']:
                examples_str = ', '.join(bucket['examples']) if bucket['examples'] else '-'
                if len(examples_str) > 50:
                    examples_str = examples_str[:47] + '...'

                f.write(f"| {bucket['range']:15s} | {bucket['count']:5d} | {bucket['percentage']:6.1f}% | {examples_str} |\n")

            f.write("\n")

            # Domain Breakdown
            f.write("## Domain Breakdown\n\n")
            f.write("| Domain | N | Avg Variance | High Variance % |\n")
            f.write("|--------|---|--------------|----------------|\n")

            for domain_stat in results['domain_breakdown']:
                f.write(f"| {domain_stat['domain']:20s} | {domain_stat['n']:3d} | {domain_stat['avg_variance']:12.2f} | {domain_stat['high_variance_pct']:14.1f}% |\n")

            f.write("\n")

            # Key Findings
            f.write("## Key Findings\n\n")

            # Automatically generated insights
            if results['domain_breakdown']:
                highest_variance_domain = results['domain_breakdown'][0]
                lowest_variance_domain = results['domain_breakdown'][-1]

                f.write(f"1. **Domain variance spread:** {highest_variance_domain['domain']} shows highest variance ({highest_variance_domain['avg_variance']:.2f}), while {lowest_variance_domain['domain']} shows lowest ({lowest_variance_domain['avg_variance']:.2f})\n\n")

            if summary['stable_pct'] > 50:
                f.write(f"2. **High stability:** {summary['stable_pct']:.1f}% of constraints are completely stable across index configs\n\n")
            elif summary['high_variance_pct'] > 30:
                f.write(f"2. **High volatility:** {summary['high_variance_pct']:.1f}% of constraints show high variance (>0.5)\n\n")

            if results['high_variance_examples']:
                f.write(f"3. **Perspective-dependent constraints:** {len(results['high_variance_examples'])} constraints show strong perspective-dependence\n\n")

            # High Variance Examples
            f.write("## High Variance Examples\n\n")
            f.write("Constraints that change type frequently based on index configuration:\n\n")
            f.write("| Constraint ID | Variance | Configs | Types | Domain | Claimed Type |\n")
            f.write("|---------------|----------|---------|-------|--------|-------------|\n")

            for example in results['high_variance_examples']:
                claimed = example.get('claimed_type') or 'N/A'
                domain = example['domain'] or 'unknown'
                f.write(f"| {example['constraint_id']:30s} | {example['variance_ratio']:.2f} | {example['index_configs']:7d} | {example['types_produced']:5d} | {domain:10s} | {claimed:12s} |\n")

            f.write("\n")

            # Detailed examples
            if results['high_variance_examples']:
                f.write("### Detailed Examples\n\n")
                for i, example in enumerate(results['high_variance_examples'][:3], 1):
                    f.write(f"**{i}. {example['constraint_id']}**\n")
                    f.write(f"- Domain: {example['domain']}\n")
                    f.write(f"- Variance: {example['variance_ratio']:.2f}\n")
                    f.write(f"- Produces {example['types_produced']} different types across {example['index_configs']} index configurations\n")

                    if example['classifications']:
                        type_counts = Counter(c['type'] for c in example['classifications'])
                        f.write(f"- Type distribution: {dict(type_counts)}\n")

                    f.write("\n")

            # Suspicious Stability
            if results['suspicious_stability']:
                f.write("## Suspicious Stability\n\n")
                f.write("Constraints with many index configs but low variance (possible modeling issues):\n\n")
                f.write("| Constraint ID | Configs | Types | Variance | Domain |\n")
                f.write("|---------------|---------|-------|----------|--------|\n")

                for item in results['suspicious_stability'][:10]:
                    f.write(f"| {item['constraint_id']:30s} | {item['index_configs']:7d} | {item['types_produced']:5d} | {item['variance_ratio']:8.2f} | {item['domain']:15s} |\n")

                f.write("\n")
                f.write("**Note:** These constraints have many perspective configurations but produce the same type. This might indicate:\n")
                f.write("- The constraint is genuinely invariant (e.g., physical laws)\n")
                f.write("- Index dimensions are not affecting classification\n")
                f.write("- Potential data quality issue\n\n")

            # Data Completeness
            f.write("## Data Completeness\n\n")
            f.write("| Field | % Complete | Impact |\n")
            f.write("|-------|-----------|--------|\n")

            total = summary['total_constraints']

            # Classifications
            with_class = sum(1 for c in self.constraints.values() if c.get('classifications'))
            f.write(f"| classifications | {(with_class/total*100):.1f}% | Core data for variance analysis |\n")

            # Variance ratio
            with_variance = sum(1 for c in self.constraints.values() if c['analysis']['variance_ratio'] is not None)
            f.write(f"| variance_ratio | {(with_variance/total*100):.1f}% | Calculated from classifications |\n")

            # Domain
            with_domain = sum(1 for c in self.constraints.values() if c.get('domain'))
            f.write(f"| domain | {(with_domain/total*100):.1f}% | Affects domain breakdown analysis |\n")

        print(f"Report generated: {output_path}")

def main():
    import argparse

    parser = argparse.ArgumentParser(
        description='Generate variance analysis report (Report 1)'
    )
    parser.add_argument(
        '--corpus-data',
        default='../outputs/corpus_data.json',
        help='Path to corpus_data.json'
    )
    parser.add_argument(
        '--output',
        default='../outputs/variance_analysis.md',
        help='Output markdown file'
    )

    args = parser.parse_args()

    analyzer = VarianceAnalyzer(args.corpus_data)
    analyzer.generate_report(args.output)

if __name__ == '__main__':
    main()
