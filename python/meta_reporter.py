#!/usr/bin/env python3
import re
import sys
import os
from collections import defaultdict, Counter
from pathlib import Path

# Construct absolute paths
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
ROOT_DIR = os.path.dirname(SCRIPT_DIR)

class MetaReporter:
    def __init__(self, output_file):
        self.output_file = Path(output_file)
        self.omegas = []
        self.constraint_types = Counter()
        self.passed_tests = 0
        self.failed_tests = 0
        self.test_failures = []
        self.total_constraints_analyzed = 0 # New field for overall count

    def parse_output(self):
        if not self.output_file.exists():
            print(f"Error: {self.output_file} does not exist")
            return False

        with open(self.output_file, 'r', encoding='utf-8') as f:
            content = f.read()

        # --- Global Stats ---
        self.passed_tests = len(re.findall(r'\[PASS\]', content))
        self.failed_tests = len(re.findall(r'\[(AUDIT FAIL|FAIL)\]', content))
        self.total_constraints_analyzed = self.passed_tests + self.failed_tests # Total from execution summary
        
        # Regex to find failure summary lines.
        for match in re.finditer(r"  -\s\[(\w+)\]\s(.+?)\n\s+Reason:\s(.+)", content):
            ftype, path, detail = match.groups()
            self.test_failures.append({'type': ftype, 'path': path.strip(), 'detail': detail.strip()})
        
        # --- Scenario-Specific Stats ---
        scenario_chunks = re.split(r'(?=\[SCENARIO MANAGER\] Clearing Knowledge Base\.\.\.)', content)

        for chunk in scenario_chunks:
            if not chunk.strip():
                continue

            # Extract constraint type from this chunk (more flexible regex)
            ctype_match = re.search(r'Claimed Type:\s*(\w+)', chunk)
            if ctype_match:
                self.constraint_types[ctype_match.group(1)] += 1
            
            # Find omegas (more flexible regex for the preceding "Ω:" and type)
            for match in re.finditer(r'Ω:\s*omega_\w+_(\w+)\s*\((conceptual|empirical|preference)\)', chunk): # Removed trailing ':'
                constraint, omega_type = match.groups()
                if (constraint, omega_type) not in self.omegas:
                    self.omegas.append((constraint, omega_type))
        return True

    def generate_report(self):
        print("\n" + "="*80)
        print("META-REPORT: Test Suite Analysis".center(80))
        print("="*80 + "\n")
        self._section_test_results()
        self._section_corpus_overview()
        self._section_omegas()
        print("="*80 + "\n")

    def _section_test_results(self):
        print("📊 TEST EXECUTION SUMMARY")
        print("-" * 80)
        total = self.passed_tests + self.failed_tests
        if total > 0:
            pass_rate = (self.passed_tests / total) * 100 if total > 0 else 0
            print(f"  Total Tests Run: {total}")
            print(f"  Passed:          {self.passed_tests} ({pass_rate:.1f}%)")
            print(f"  Failed:          {self.failed_tests}")
            if self.failed_tests > 0:
                print(f"  Failure Details:")
                for failure in self.test_failures:
                    print(f"    - [{failure['type']}] {failure['path']}")
                    print(f"      Reason: {failure['detail'][:100]}")
        else:
            print("  No test results found")
        print()

    def _section_corpus_overview(self):
        print("📚 CORPUS OVERVIEW")
        print("-" * 80)
        print(f"  Total Constraints Analyzed: {self.total_constraints_analyzed}")
        if self.constraint_types:
            print(f"  Identified Constraint Type Distribution (from test output):")
            for ctype, count in self.constraint_types.most_common():
                percentage = (count / self.total_constraints_analyzed) * 100 if self.total_constraints_analyzed > 0 else 0
                print(f"    - {ctype:15s}: {count:4d} ({percentage:.1f}%)")
        else:
            print("  No explicit constraint types identified in test output.")
        print()

    def _section_omegas(self):
        print("❓ EPISTEMOLOGICAL GAPS (Omegas)")
        print("-" * 80)
        if self.omegas:
            print(f"  Total Unique Omegas Found: {len(self.omegas)}")
            omega_types = Counter(o_type for _, o_type in self.omegas)
            for omega_type, count in omega_types.most_common():
                print(f"  - {omega_type.upper()}: {count}")
        else:
            print(f"  ✓ No unresolved omegas detected")
        print()

def main():
    output_file = os.path.join(ROOT_DIR, 'outputs', 'output.txt')
    reporter = MetaReporter(output_file)
    if reporter.parse_output():
        reporter.generate_report()
    else:
        sys.exit(1)

if __name__ == '__main__':
    main()