#!/usr/bin/env python3
"""
Sufficiency Tester - Report 2: Index Sufficiency Test

Determines if the 4 indices (agent_power, time_horizon, exit_options, spatial_scope)
are sufficient to explain all constraint type variance, or if new categories are needed.

Classifies collisions into three categories:
  - Classification failures (unknown/opaque types — real insufficiency signal)
  - Expected perspectival variance (type differences explained by agent_power level)
  - Genuine collisions (unexplained type variance — actual index insufficiency)
"""

import json
from collections import defaultdict, Counter
from pathlib import Path

# Domain label normalization map: variant → canonical form
DOMAIN_NORMALIZATION = {
    'psychology': 'psychological',
    'logistics': 'logistical',
    'mathematics': 'mathematical',
    'technology': 'technological',
    'physics': 'physical',
}


class SufficiencyTester:
    """Tests whether 4 indices sufficiently explain constraint variance"""

    def __init__(self, corpus_data_path, pipeline_data_path=None):
        with open(corpus_data_path, 'r') as f:
            data = json.load(f)

        self.constraints = data['constraints']
        self.summary = data.get('summary', {})

        # Normalize domain labels and track consolidation
        self.domain_consolidation = Counter()
        self._normalize_domains()

        # Load pipeline data for enrichment
        self.pipeline_lookup = {}
        if pipeline_data_path and Path(pipeline_data_path).exists():
            self._load_pipeline_data(pipeline_data_path)

    def _normalize_domains(self):
        """Normalize domain labels to canonical forms"""
        for cid, constraint in self.constraints.items():
            domain = constraint.get('domain', 'unknown')
            if domain in DOMAIN_NORMALIZATION:
                canonical = DOMAIN_NORMALIZATION[domain]
                self.domain_consolidation[f"{domain} → {canonical}"] += 1
                constraint['domain'] = canonical

    def _load_pipeline_data(self, path):
        """Load pipeline_output.json and build lookup by constraint ID"""
        with open(path, 'r') as f:
            data = json.load(f)

        for entry in data.get('per_constraint', []):
            cid = entry.get('id')
            if cid:
                self.pipeline_lookup[cid] = entry

    def _enrich_collision(self, collision):
        """Enrich a collision with pipeline data"""
        cid = collision['constraint_id']
        pipeline = self.pipeline_lookup.get(cid, {})

        omegas = pipeline.get('omegas', [])
        collision['signature'] = pipeline.get('signature')
        collision['purity_band'] = pipeline.get('purity_band')
        collision['has_omega'] = len(omegas) > 0
        collision['omega_count'] = len(omegas)

        return collision

    def analyze(self):
        """Run complete sufficiency analysis"""
        results = {}

        # Detect raw collisions
        raw_collisions = self.detect_index_collisions()

        # Classify collisions into three categories
        classified = self.classify_collisions(raw_collisions)
        results['classification_failures'] = classified['classification_failures']
        results['perspectival_variance'] = classified['perspectival_variance']
        results['genuine_collisions'] = classified['genuine_collisions']
        results['all_collisions'] = raw_collisions

        # Collision patterns (on all collisions for context)
        results['collision_patterns'] = self.analyze_collision_patterns(raw_collisions)

        # Domain sufficiency
        results['domain_sufficiency'] = self.analyze_domain_sufficiency()

        # Stability anomalies with mountain filter
        results['stability_anomalies'] = self.find_stability_anomalies()

        # Domain consolidation info
        results['domain_consolidation'] = dict(self.domain_consolidation)

        # Evidence summary with new verdict logic
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
                        continue

                    by_index[index_key].append(ctype)

            # Find collisions (same index config -> multiple types)
            for index_config, types in by_index.items():
                unique_types = set(types)
                if len(unique_types) > 1:
                    collision = {
                        'constraint_id': cid,
                        'index_config': index_config,
                        'types_produced': sorted(unique_types),
                        'type_count': len(unique_types),
                        'domain': constraint.get('domain', 'unknown'),
                        'claimed_type': constraint.get('claimed_type'),
                        'extractiveness': constraint.get('metrics', {}).get('extractiveness'),
                        'suppression': constraint.get('metrics', {}).get('suppression')
                    }
                    # Enrich with pipeline data
                    self._enrich_collision(collision)
                    collisions.append(collision)

        return collisions

    def classify_collisions(self, collisions):
        """Classify collisions into three categories"""
        classification_failures = []
        perspectival_variance = []
        genuine_collisions = []

        failure_types = {'unknown', 'indexically_opaque'}

        for collision in collisions:
            types = set(collision['types_produced'])

            # Category 1: Classification failures
            if types & failure_types:
                collision['failure_reason'] = 'contains_' + '_or_'.join(
                    sorted(types & failure_types))
                classification_failures.append(collision)
                continue

            # Category 2: Check for perspectival variance
            if self._is_perspectival(collision):
                collision['variance_type'] = 'agent_power_perspectival'
                perspectival_variance.append(collision)
                continue

            # Category 3: Genuine collision
            genuine_collisions.append(collision)

        return {
            'classification_failures': classification_failures,
            'perspectival_variance': perspectival_variance,
            'genuine_collisions': genuine_collisions,
        }

    def _is_perspectival(self, collision):
        """Check if a collision represents expected perspectival variance.

        For each collision pair, check whether the two types have different
        agent_power profiles across the constraint's classifications.
        If the power-level sets differ (at least one exclusive level exists
        for either type) → perspectival. The overlap at the collision's
        power level is the boundary where both perspectives are valid.
        If the power sets are identical (no separation at all) → genuine.
        """
        cid = collision['constraint_id']
        constraint = self.constraints.get(cid, {})
        classifications = constraint.get('classifications', [])

        if not classifications:
            return False

        # Build map: type → set of agent_power levels seen
        type_to_powers = defaultdict(set)
        for classification in classifications:
            ctype = classification.get('type')
            context = classification.get('context')
            if ctype and context and isinstance(context, dict):
                power = context.get('agent_power')
                if power:
                    type_to_powers[ctype].add(power)

        # Check all pairs in the collision
        collision_types = collision['types_produced']
        for i in range(len(collision_types)):
            for j in range(i + 1, len(collision_types)):
                type_a = collision_types[i]
                type_b = collision_types[j]

                powers_a = type_to_powers.get(type_a, set())
                powers_b = type_to_powers.get(type_b, set())

                # If either type has no power data, can't determine
                if not powers_a or not powers_b:
                    return False

                # If power sets are identical → no power-level separation
                # → genuine collision, not perspectival
                if powers_a == powers_b:
                    return False

        # All pairs have different power-level profiles → perspectival
        return True

    def analyze_collision_patterns(self, collisions):
        """Identify patterns in index collisions"""
        if not collisions:
            return {
                'total_collisions': 0,
                'constraints_affected': 0,
                'common_transitions': [],
                'domain_breakdown': []
            }

        transitions = Counter()
        constraints_affected = set()
        by_domain = defaultdict(list)

        for collision in collisions:
            constraints_affected.add(collision['constraint_id'])
            domain = collision['domain']
            by_domain[domain].append(collision)

            types = sorted(collision['types_produced'])
            if len(types) == 2:
                transition = f"{types[0]} ↔ {types[1]}"
                transitions[transition] += 1

        domain_stats = []
        for domain in sorted(by_domain.keys(), key=lambda x: (x is None, x or "")):
            domain_collisions = by_domain[domain]
            domain_stats.append({
                'domain': domain,
                'collision_count': len(domain_collisions),
                'constraints_affected': len(set(
                    c['constraint_id'] for c in domain_collisions))
            })

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
            'variances': []
        })

        for cid, constraint in self.constraints.items():
            domain = constraint.get('domain', 'unknown')
            variance = constraint.get('analysis', {}).get('variance_ratio')

            by_domain[domain]['total'] += 1
            if variance is not None:
                by_domain[domain]['variances'].append(variance)

        domain_stats = []
        for domain, stats in by_domain.items():
            if stats['variances']:
                avg_var = sum(stats['variances']) / len(stats['variances'])
                domain_stats.append({
                    'domain': domain,
                    'total_constraints': stats['total'],
                    'avg_variance': avg_var,
                    'sufficiency_score': avg_var
                })

        domain_stats.sort(key=lambda x: x['sufficiency_score'], reverse=True)

        return domain_stats

    def find_stability_anomalies(self):
        """Find constraints that are TOO stable — excluding mountains"""
        anomalies = []

        for cid, constraint in self.constraints.items():
            # Filter OUT mountains — they SHOULD be index-invariant
            if constraint.get('claimed_type') == 'mountain':
                continue

            analysis = constraint.get('analysis', {})
            configs = analysis.get('index_configs', 0)
            types = analysis.get('types_produced', 0)

            # Anomaly: many index configs but always same type (non-mountain)
            if configs >= 6 and types == 1:
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

        anomalies.sort(key=lambda x: x['index_configs'], reverse=True)

        return anomalies

    def summarize_evidence(self, results):
        """Summarize evidence with four-rate verdict system"""
        total_collisions = len(results['all_collisions'])
        total_constraints = len(self.constraints)

        n_failures = len(results['classification_failures'])
        n_perspectival = len(results['perspectival_variance'])
        n_genuine = len(results['genuine_collisions'])

        # Count non-mountain constraints for anomaly rate
        non_mountain_constraints = [
            c for c in self.constraints.values()
            if c.get('claimed_type') != 'mountain'
        ]
        n_non_mountains = len(non_mountain_constraints)
        n_anomalies = len(results['stability_anomalies'])

        # Four rates
        classification_failure_rate = (
            n_failures / total_collisions * 100) if total_collisions > 0 else 0
        expected_variance_rate = (
            n_perspectival / total_collisions * 100) if total_collisions > 0 else 0
        genuine_collision_rate = (
            n_genuine / total_constraints * 100) if total_constraints > 0 else 0
        non_mountain_anomaly_rate = (
            n_anomalies / n_non_mountains * 100) if n_non_mountains > 0 else 0

        # Constraints affected by genuine collisions
        genuine_constraints = set(
            c['constraint_id'] for c in results['genuine_collisions'])

        return {
            'total_collisions': total_collisions,
            'total_constraints': total_constraints,
            'classification_failures': n_failures,
            'perspectival_variance': n_perspectival,
            'genuine_collisions': n_genuine,
            'genuine_constraints_affected': len(genuine_constraints),
            'stability_anomalies': n_anomalies,
            'non_mountain_total': n_non_mountains,
            'classification_failure_rate': classification_failure_rate,
            'expected_variance_rate': expected_variance_rate,
            'genuine_collision_rate': genuine_collision_rate,
            'non_mountain_anomaly_rate': non_mountain_anomaly_rate,
            'verdict': self._calculate_verdict(
                classification_failure_rate,
                genuine_collision_rate,
                non_mountain_anomaly_rate,
                expected_variance_rate
            ),
            'domain_consolidation': dict(self.domain_consolidation),
        }

    def _calculate_verdict(self, failure_rate, genuine_rate,
                           anomaly_rate, perspectival_rate):
        """Determine overall verdict based on four rates"""
        issues = []

        if failure_rate > 5:
            issues.append("classification failures")
        if genuine_rate > 10:
            issues.append("genuine collisions")
        if anomaly_rate > 20:
            issues.append("non-mountain anomalies")

        if not issues:
            if perspectival_rate > 50:
                return ("SUFFICIENT — Indices explain variance well. "
                        "High perspectival rate confirms agent_power "
                        "is working as designed.")
            return ("SUFFICIENT — Indices explain most variance. "
                    "Current framework adequate.")
        elif len(issues) == 1:
            return f"MIXED — Mostly sufficient but {issues[0]} need investigation."
        else:
            return f"INSUFFICIENT — Multiple signals: {', '.join(issues)}."

    def generate_report(self, output_path, json_output_path=None):
        """Generate markdown report and optional JSON output"""
        results = self.analyze()

        self._write_markdown(output_path, results)

        if json_output_path:
            self._write_json(json_output_path, results)

        print(f"Report generated: {output_path}")

    def _write_json(self, path, results):
        """Write structured JSON output"""
        def serialize_collision(c):
            out = dict(c)
            if 'index_config' in out and isinstance(out['index_config'], tuple):
                out['index_config'] = list(out['index_config'])
            return out

        output = {
            'evidence_summary': results['evidence_summary'],
            'classification_failures': [
                serialize_collision(c)
                for c in results['classification_failures']
            ],
            'perspectival_variance': [
                serialize_collision(c)
                for c in results['perspectival_variance']
            ],
            'genuine_collisions': [
                serialize_collision(c)
                for c in results['genuine_collisions']
            ],
            'collision_patterns': {
                'total_collisions':
                    results['collision_patterns']['total_collisions'],
                'constraints_affected':
                    results['collision_patterns']['constraints_affected'],
                'common_transitions': [
                    {'transition': t, 'count': c}
                    for t, c in
                    results['collision_patterns']['common_transitions']
                ],
                'domain_breakdown':
                    results['collision_patterns']['domain_breakdown'],
            },
            'domain_sufficiency': results['domain_sufficiency'],
            'stability_anomalies': results['stability_anomalies'],
            'domain_consolidation': results['domain_consolidation'],
        }

        with open(path, 'w') as f:
            json.dump(output, f, indent=2)

    def _write_markdown(self, output_path, results):
        """Generate markdown report"""
        summary = results['evidence_summary']

        with open(output_path, 'w') as f:
            f.write("# Index Sufficiency Test\n\n")

            # Executive Summary
            f.write("## Executive Summary\n\n")
            f.write(f"**Verdict:** {summary['verdict']}\n\n")

            f.write("### Collision Breakdown\n\n")
            f.write("| Category | Count | Rate |\n")
            f.write("|----------|------:|-----:|\n")
            f.write(
                f"| Classification failures (unknown/opaque) "
                f"| {summary['classification_failures']} "
                f"| {summary['classification_failure_rate']:.1f}% "
                f"of collisions |\n")
            f.write(
                f"| Expected perspectival variance "
                f"| {summary['perspectival_variance']} "
                f"| {summary['expected_variance_rate']:.1f}% "
                f"of collisions |\n")
            f.write(
                f"| Genuine collisions "
                f"| {summary['genuine_collisions']} "
                f"| {summary['genuine_collision_rate']:.1f}% "
                f"of constraints |\n")
            f.write(
                f"| **Total collisions** "
                f"| **{summary['total_collisions']}** "
                f"| across {summary['total_constraints']} constraints |\n")
            f.write("\n")
            f.write(
                f"- **Non-mountain anomaly rate:** "
                f"{summary['non_mountain_anomaly_rate']:.1f}% "
                f"({summary['stability_anomalies']} of "
                f"{summary['non_mountain_total']} "
                f"non-mountain constraints)\n\n")

            # Domain consolidation
            if summary['domain_consolidation']:
                f.write("### Data Quality: Domain Label Consolidation\n\n")
                f.write("The following domain labels were normalized "
                        "at load time:\n\n")
                for mapping, count in sorted(
                        summary['domain_consolidation'].items()):
                    f.write(f"- {mapping} ({count} constraints)\n")
                f.write("\n")

            # Classification Failures
            failures = results['classification_failures']
            if failures:
                f.write("## Classification Failures\n\n")
                f.write(f"**{len(failures)} collisions** involve "
                        "`unknown` or `indexically_opaque` types — "
                        "these represent real classification "
                        "insufficiency.\n\n")
                f.write("| Constraint ID | Types | Domain "
                        "| Signature |\n")
                f.write("|---------------|-------|--------"
                        "|-----------|\n")
                for c in failures[:20]:
                    types_str = ', '.join(c['types_produced'])
                    sig = c.get('signature') or '-'
                    domain = c['domain'] or 'unknown'
                    f.write(f"| {c['constraint_id']} | {types_str} "
                            f"| {domain} | {sig} |\n")
                if len(failures) > 20:
                    f.write(f"\n*...and {len(failures) - 20} more*\n")
                f.write("\n")

            # Perspectival Variance
            perspectival = results['perspectival_variance']
            if perspectival:
                f.write("## Expected Perspectival Variance\n\n")
                f.write(f"**{len(perspectival)} collisions** are explained "
                        "by agent_power perspective shifts — "
                        "the framework working as designed.\n\n")

                # Show transition patterns within perspectival
                persp_transitions = Counter()
                for c in perspectival:
                    types = sorted(c['types_produced'])
                    if len(types) == 2:
                        persp_transitions[
                            f"{types[0]} ↔ {types[1]}"] += 1

                if persp_transitions:
                    f.write("| Transition | Count |\n")
                    f.write("|------------|------:|\n")
                    for transition, count in \
                            persp_transitions.most_common(10):
                        f.write(f"| {transition} | {count} |\n")
                    f.write("\n")

            # Genuine Collisions
            genuine = results['genuine_collisions']
            if genuine:
                f.write("## Genuine Collisions\n\n")
                f.write(f"**{len(genuine)} collisions** remain "
                        "unexplained — same index config produces "
                        "different types without perspectival "
                        "justification.\n\n")
                f.write("| Constraint ID | Index Config | Types "
                        "| Domain | Purity |\n")
                f.write("|---------------|--------------|-------"
                        "|--------|--------|\n")
                for c in genuine[:20]:
                    types_str = ', '.join(c['types_produced'])
                    index_str = str(c['index_config'])[:35]
                    domain = c['domain'] or 'unknown'
                    purity = c.get('purity_band') or '-'
                    f.write(f"| {c['constraint_id']} | {index_str} "
                            f"| {types_str} | {domain} | {purity} |\n")
                if len(genuine) > 20:
                    f.write(f"\n*...and {len(genuine) - 20} more*\n")
                f.write("\n")

            # All collisions pattern summary
            patterns = results['collision_patterns']
            if patterns['common_transitions']:
                f.write("## All Collision Patterns\n\n")
                f.write(
                    f"**Total collisions:** {patterns['total_collisions']}"
                    f" across {patterns['constraints_affected']}"
                    f" constraints\n\n")
                f.write("### Most Common Type Transitions\n\n")
                f.write("| Transition | Frequency |\n")
                f.write("|------------|----------:|\n")
                for transition, count in patterns['common_transitions']:
                    f.write(f"| {transition} | {count} |\n")
                f.write("\n")

            # Domain Breakdown
            if patterns['domain_breakdown']:
                f.write("### Collisions by Domain\n\n")
                f.write("| Domain | Collisions "
                        "| Constraints Affected |\n")
                f.write("|--------|----------:"
                        "|--------------------:|\n")
                for ds in patterns['domain_breakdown']:
                    domain = ds['domain'] or 'unknown'
                    f.write(f"| {domain} | {ds['collision_count']} "
                            f"| {ds['constraints_affected']} |\n")
                f.write("\n")

            # Domain Sufficiency Analysis
            f.write("## Domain Sufficiency Analysis\n\n")
            f.write("How well do indices explain variance "
                    "in each domain?\n\n")
            f.write("| Domain | Constraints | Avg Variance "
                    "| Sufficiency |\n")
            f.write("|--------|----------:|-------------:"
                    "|:-----------:|\n")

            for ds in results['domain_sufficiency']:
                score = ds['sufficiency_score']
                rating = ("High" if score > 0.7
                          else "Medium" if score > 0.4
                          else "Low")
                domain = ds['domain'] or 'unknown'
                f.write(f"| {domain} | {ds['total_constraints']} "
                        f"| {ds['avg_variance']:.2f} | {rating} |\n")

            f.write("\n")
            f.write("**Note:** Higher variance = indices capture "
                    "more differences (good)\n\n")

            # Stability Anomalies (mountain-filtered)
            anomalies = results['stability_anomalies']
            f.write("## Stability Anomalies (Mountains Excluded)\n\n")
            f.write("Non-mountain constraints tested across 6+ index "
                    "configs that always produce the same type.\n")
            f.write("These may indicate misclassification (mountains "
                    "are excluded since they SHOULD be invariant).\n\n")

            if anomalies:
                f.write("| Constraint ID | Configs | Type "
                        "| Domain | Notes |\n")
                f.write("|---------------|--------:|------"
                        "|--------|-------|\n")
                for anomaly in anomalies[:25]:
                    notes = []
                    if anomaly.get('emerges_naturally'):
                        notes.append("natural")
                    if anomaly.get('requires_enforcement'):
                        notes.append("enforced")
                    notes_str = ', '.join(notes) if notes else '-'
                    consistent_type = anomaly['consistent_type'] or 'N/A'
                    domain = anomaly['domain'] or 'unknown'
                    f.write(
                        f"| {anomaly['constraint_id']} "
                        f"| {anomaly['index_configs']} "
                        f"| {consistent_type} "
                        f"| {domain} | {notes_str} |\n")
                if len(anomalies) > 25:
                    f.write(f"\n*...and {len(anomalies) - 25} more*\n")
                f.write("\n")
            else:
                f.write("*No non-mountain stability anomalies found.*"
                        "\n\n")

            # Recommendations
            f.write("## Recommendations\n\n")

            rec_num = 1

            if summary['classification_failure_rate'] > 5:
                f.write(
                    f"{rec_num}. **Classification failures "
                    f"({summary['classification_failure_rate']:.1f}%):** "
                    "Investigate specific index configurations producing "
                    "`unknown` types. These represent real gaps in the "
                    "classification engine.\n\n")
                rec_num += 1

            if summary['genuine_collision_rate'] > 5:
                f.write(
                    f"{rec_num}. **Genuine collision rate "
                    f"({summary['genuine_collision_rate']:.1f}%):** "
                    "Examine whether a 5th index dimension would resolve "
                    "remaining collisions, or whether refinement of "
                    "existing index boundaries is sufficient.\n\n")
                rec_num += 1

            if summary['non_mountain_anomaly_rate'] > 10:
                f.write(
                    f"{rec_num}. **Non-mountain anomaly rate "
                    f"({summary['non_mountain_anomaly_rate']:.1f}%):** "
                    "Review index-invariant non-mountains for possible "
                    "misclassification. These constraints behave like "
                    "natural laws but aren't classified as "
                    "mountains.\n\n")
                rec_num += 1

            if summary['domain_consolidation']:
                total_consolidated = sum(
                    summary['domain_consolidation'].values())
                f.write(
                    f"{rec_num}. **Data quality:** Domain label "
                    f"normalization consolidated "
                    f"{total_consolidated} constraints across "
                    f"{len(summary['domain_consolidation'])} variant "
                    "labels. Consider standardizing domain labels "
                    "upstream in the test corpus.\n\n")
                rec_num += 1

            if summary['expected_variance_rate'] > 50:
                f.write(
                    f"{rec_num}. **Perspectival health:** "
                    f"{summary['expected_variance_rate']:.1f}% of "
                    "collisions are expected perspectival variance "
                    "(agent_power driving type shifts). This confirms "
                    "the index system is capturing real structural "
                    "dynamics.\n\n")
                rec_num += 1

            if rec_num == 1:
                f.write("No significant issues detected. The 4-index "
                        "system appears adequate.\n\n")


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
        '--pipeline-data',
        default='../outputs/pipeline_output.json',
        help='Path to pipeline_output.json'
    )
    parser.add_argument(
        '--output',
        default='../outputs/index_sufficiency.md',
        help='Output markdown file'
    )
    parser.add_argument(
        '--json-output',
        default='../outputs/index_sufficiency.json',
        help='Output JSON file'
    )

    args = parser.parse_args()

    tester = SufficiencyTester(args.corpus_data, args.pipeline_data)
    tester.generate_report(args.output, args.json_output)


if __name__ == '__main__':
    main()
