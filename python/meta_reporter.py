#!/usr/bin/env python3
"""
Meta Reporter - Analyzes output.txt and generates actionable summary

Surfaces:
- Errors and warnings
- False mountains and ontological fraud
- Omegas (epistemological gaps)
- Cross-domain isomorphisms
- Data quality issues
- Recommendations for new scenarios
"""

import re
import sys
from collections import defaultdict, Counter
from pathlib import Path

class MetaReporter:
    def __init__(self, output_file='../outputs/output.txt'):
        self.output_file = Path(output_file)
        self.errors = []
        self.warnings = []
        self.false_mountains = []
        self.omegas = []
        self.isomorphisms = []
        self.passed_tests = 0
        self.failed_tests = 0
        self.constraint_types = Counter()
        self.domain_categories = Counter()
        self.validation_issues = defaultdict(list)

    def parse_output(self):
        """Parse the output.txt file"""
        if not self.output_file.exists():
            print(f"Error: {self.output_file} does not exist")
            return False

        with open(self.output_file, 'r', encoding='utf-8') as f:
            content = f.read()

        # Count test results
        self.passed_tests = content.count('[PASS]') + len(re.findall(r'test_passed', content))
        self.failed_tests = content.count('[FAIL]')

        # Extract errors
        for match in re.finditer(r'ERROR:(.+)', content):
            self.errors.append(match.group(1).strip())

        for match in re.finditer(r'\[ERROR\](.+)', content):
            self.errors.append(match.group(1).strip())

        # Extract warnings
        for match in re.finditer(r'Warning:(.+)', content):
            warning = match.group(1).strip()
            # Filter out module loading warnings (too noisy)
            if 'source_sink' not in warning and 'Local definition' not in warning:
                self.warnings.append(warning)

        for match in re.finditer(r'\[WARN\](.+)', content):
            self.warnings.append(match.group(1).strip())

        # Extract false mountains
        for match in re.finditer(r'type_1_false_mountain detected for (\w+)', content):
            constraint = match.group(1)
            if constraint not in self.false_mountains:
                self.false_mountains.append(constraint)

        # Extract omegas
        for match in re.finditer(r'omega_\w+_(\w+) \((\w+)\):', content):
            omega_type = match.group(2)
            constraint = match.group(1)
            self.omegas.append((constraint, omega_type))

        # Extract isomorphisms
        for match in re.finditer(r'(\w+) \((\w+)\) ≈ (\w+) \((\w+)\)', content):
            c1, cat1, c2, cat2 = match.groups()
            self.isomorphisms.append((c1, cat1, c2, cat2))

        # Extract constraint types
        for match in re.finditer(r'Claimed Type: (\w+)', content):
            self.constraint_types[match.group(1)] += 1

        # Extract domain categories
        for match in re.finditer(r'Category: (\w+)', content):
            cat = match.group(1)
            if cat not in ['MISSING', 'unknown']:
                self.domain_categories[cat] += 1

        # Extract validation issues
        for match in re.finditer(r'classification_mismatch: (\w+) - (\w+)-(\w+)', content):
            constraint, claimed, expected = match.groups()
            self.validation_issues['classification_mismatch'].append({
                'constraint': constraint,
                'claimed': claimed,
                'expected': expected
            })

        return True

    def generate_report(self):
        """Generate the meta-report"""
        print("\n" + "="*80)
        print("META-REPORT: Test Suite Analysis".center(80))
        print("="*80 + "\n")

        # Section 1: Test Results
        self._section_test_results()

        # Section 2: Critical Issues
        self._section_critical_issues()

        # Section 3: Epistemological Gaps (Omegas)
        self._section_omegas()

        # Section 4: Cross-Domain Insights
        self._section_cross_domain()

        # Section 5: Corpus Overview
        self._section_corpus_overview()

        # Section 6: Data Quality
        self._section_data_quality()

        # Section 7: Recommendations
        self._section_recommendations()

        print("="*80 + "\n")

    def _section_test_results(self):
        """Test execution summary"""
        print("📊 TEST EXECUTION SUMMARY")
        print("-" * 80)

        total = self.passed_tests + self.failed_tests
        if total > 0:
            pass_rate = (self.passed_tests / total) * 100
            print(f"  Total Tests:    {total}")
            print(f"  Passed:         {self.passed_tests} ({pass_rate:.1f}%)")
            print(f"  Failed:         {self.failed_tests}")

            if pass_rate >= 95:
                print(f"  Status:         ✓ EXCELLENT")
            elif pass_rate >= 80:
                print(f"  Status:         ✓ GOOD")
            elif pass_rate >= 60:
                print(f"  Status:         ⚠ FAIR - needs attention")
            else:
                print(f"  Status:         ✗ POOR - urgent fixes needed")
        else:
            print(f"  No test results found")

        print()

    def _section_critical_issues(self):
        """Critical errors and false mountains"""
        print("🔴 CRITICAL ISSUES")
        print("-" * 80)

        # Unique errors only
        unique_errors = list(set(self.errors))[:10]  # Top 10

        if unique_errors:
            print(f"  Errors Found: {len(self.errors)} ({len(unique_errors)} unique)")
            print(f"\n  Top Errors:")
            for i, error in enumerate(unique_errors, 1):
                # Truncate long errors
                display_error = error[:70] + '...' if len(error) > 70 else error
                print(f"    {i}. {display_error}")
        else:
            print(f"  ✓ No errors detected")

        if self.false_mountains:
            print(f"\n  ⚠ FALSE MOUNTAINS (Ontological Fraud): {len(self.false_mountains)}")
            print(f"  Constraints claiming to be unchangeable but requiring enforcement:")
            for constraint in self.false_mountains[:5]:  # Top 5
                print(f"    - {constraint}")
            if len(self.false_mountains) > 5:
                print(f"    ... and {len(self.false_mountains) - 5} more")
        else:
            print(f"\n  ✓ No false mountains detected")

        print()

    def _section_omegas(self):
        """Epistemological gaps requiring resolution"""
        print("❓ EPISTEMOLOGICAL GAPS (Omegas)")
        print("-" * 80)

        if self.omegas:
            omega_types = Counter(omega_type for _, omega_type in self.omegas)

            print(f"  Total Omegas: {len(self.omegas)}\n")

            for omega_type in ['conceptual', 'empirical', 'preference']:
                count = omega_types.get(omega_type, 0)
                if count > 0:
                    print(f"  {omega_type.upper()}: {count}")
                    constraints = [c for c, t in self.omegas if t == omega_type][:3]
                    for constraint in constraints:
                        print(f"    - {constraint}")
                    if len([c for c, t in self.omegas if t == omega_type]) > 3:
                        print(f"    ... and more")
                    print()

            print("  💡 Action: Review Omega Resolution Scenarios in output.txt")
        else:
            print(f"  ✓ No unresolved omegas - system is epistemically complete")

        print()

    def _section_cross_domain(self):
        """Cross-domain patterns"""
        print("🔗 CROSS-DOMAIN PATTERNS")
        print("-" * 80)

        if self.isomorphisms:
            print(f"  Structural Twins Found: {len(self.isomorphisms)}\n")

            # Group by category pairs
            category_pairs = Counter((cat1, cat2) for _, cat1, _, cat2 in self.isomorphisms)

            print(f"  Top Cross-Domain Connections:")
            for (cat1, cat2), count in category_pairs.most_common(5):
                print(f"    {cat1} ↔ {cat2}: {count} isomorphism(s)")

            print(f"\n  💡 Solutions from one domain may inform the other")
        else:
            print(f"  No cross-domain isomorphisms detected")
            print(f"  (This is normal if constraints are from same domain)")

        print()

    def _section_corpus_overview(self):
        """Overview of the corpus"""
        print("📚 CORPUS OVERVIEW")
        print("-" * 80)

        # Constraint types distribution
        if self.constraint_types:
            print(f"  Constraint Type Distribution:")
            for ctype, count in self.constraint_types.most_common():
                print(f"    {ctype:15s}: {count:3d}")
            print()

        # Domain coverage
        if self.domain_categories:
            print(f"  Domain Coverage ({len(self.domain_categories)} categories):")
            for category, count in sorted(self.domain_categories.items(),
                                         key=lambda x: x[1],
                                         reverse=True)[:10]:
                print(f"    {category:30s}: {count:3d} constraint(s)")

            # Identify underrepresented domains
            underrep = [(cat, count) for cat, count in self.domain_categories.items()
                       if count < 3]
            if underrep:
                print(f"\n  ⚠ Underrepresented domains (<3 constraints):")
                for cat, count in underrep[:5]:
                    print(f"    - {cat} ({count})")

        print()

    def _section_data_quality(self):
        """Data quality issues"""
        print("📋 DATA QUALITY")
        print("-" * 80)

        total_issues = sum(len(issues) for issues in self.validation_issues.values())

        if total_issues > 0:
            print(f"  Total Issues: {total_issues}\n")

            if 'classification_mismatch' in self.validation_issues:
                mismatches = self.validation_issues['classification_mismatch']
                print(f"  Classification Mismatches: {len(mismatches)}")
                for issue in mismatches[:5]:
                    print(f"    - {issue['constraint']}: "
                          f"claimed={issue['claimed']}, expected={issue['expected']}")
                if len(mismatches) > 5:
                    print(f"    ... and {len(mismatches) - 5} more")
                print()
        else:
            print(f"  ✓ No data quality issues detected")

        # Warnings summary
        if self.warnings:
            unique_warnings = len(set(self.warnings))
            print(f"  Warnings: {len(self.warnings)} ({unique_warnings} unique)")
            if unique_warnings <= 5:
                for warning in set(self.warnings):
                    display_warning = warning[:70] + '...' if len(warning) > 70 else warning
                    print(f"    - {display_warning}")
        else:
            print(f"  ✓ No warnings")

        print()

    def _section_recommendations(self):
        """Actionable recommendations"""
        print("💡 RECOMMENDATIONS")
        print("-" * 80)

        recommendations = []

        # Based on false mountains
        if len(self.false_mountains) > 10:
            recommendations.append(
                f"HIGH PRIORITY: Review {len(self.false_mountains)} false mountains - "
                "these represent ontological fraud"
            )

        # Based on omegas
        if self.omegas:
            conceptual_omegas = len([o for o in self.omegas if o[1] == 'conceptual'])
            if conceptual_omegas > 5:
                recommendations.append(
                    f"Resolve {conceptual_omegas} conceptual omegas through stakeholder interviews"
                )

        # Based on domain coverage
        if self.domain_categories:
            underrep = [cat for cat, count in self.domain_categories.items() if count < 3]
            if underrep:
                recommendations.append(
                    f"Add scenarios for underrepresented domains: {', '.join(underrep[:3])}"
                )

        # Based on errors
        if len(set(self.errors)) > 20:
            recommendations.append(
                "HIGH PRIORITY: Fix data quality issues - too many unique errors"
            )

        # Based on classification mismatches
        if 'classification_mismatch' in self.validation_issues:
            if len(self.validation_issues['classification_mismatch']) > 10:
                recommendations.append(
                    "Review classification mismatches - claims don't match metrics"
                )

        # Based on cross-domain patterns
        if self.isomorphisms:
            recommendations.append(
                f"Leverage {len(self.isomorphisms)} cross-domain isomorphisms for solution transfer"
            )

        # Suggest new scenarios based on gaps
        if self.domain_categories:
            # Suggest balancing
            recommendations.append(
                "Consider adding scenarios that explore: "
                "mandatrophies, pitons, tangled_ropes in underrepresented domains"
            )

        if recommendations:
            for i, rec in enumerate(recommendations, 1):
                print(f"  {i}. {rec}")
        else:
            print(f"  ✓ Corpus is well-balanced - add scenarios as needed for specific research questions")

        print()

        # Next steps
        print("  🔄 NEXT STEPS:")
        print("    1. Fix any critical errors first")
        print("    2. Review false mountains and reclassify")
        print("    3. Add scenarios for underrepresented domains")
        print("    4. Resolve high-priority omegas")
        print("    5. Re-run ./scripts/run_tests.sh to verify")
        print()

def main():
    import argparse

    parser = argparse.ArgumentParser(
        description='Generate meta-report from test suite output'
    )
    parser.add_argument(
        '--output',
        default='../outputs/output.txt',
        help='Path to output.txt file (default: ../outputs/output.txt)'
    )
    parser.add_argument(
        '--json',
        action='store_true',
        help='Output report in JSON format'
    )

    args = parser.parse_args()

    reporter = MetaReporter(args.output)

    if not reporter.parse_output():
        sys.exit(1)

    if args.json:
        import json
        data = {
            'passed_tests': reporter.passed_tests,
            'failed_tests': reporter.failed_tests,
            'errors': reporter.errors,
            'warnings': reporter.warnings,
            'false_mountains': reporter.false_mountains,
            'omegas': [{'constraint': c, 'type': t} for c, t in reporter.omegas],
            'isomorphisms': [{'c1': c1, 'cat1': cat1, 'c2': c2, 'cat2': cat2}
                           for c1, cat1, c2, cat2 in reporter.isomorphisms],
            'constraint_types': dict(reporter.constraint_types),
            'domain_categories': dict(reporter.domain_categories),
        }
        print(json.dumps(data, indent=2))
    else:
        reporter.generate_report()

if __name__ == '__main__':
    main()
