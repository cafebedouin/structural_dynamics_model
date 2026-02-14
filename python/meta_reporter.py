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

        # v5.0-5.2: Purity, Boltzmann, Network data
        self.purity_scores = []          # list of (constraint_id, score, band)
        self.boltzmann_signatures = []   # list of (constraint_id, signature_type, confidence)
        self.coupling_results = []       # list of (constraint_id, regime, score)
        self.network_stability = []      # list of stability assessments
        self.network_drift_events = 0
        self.purity_drift_count = 0

        # Orbit data
        self.orbit_families = {}  # signature_tuple -> [constraint_ids]
        self.orbit_total = 0

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

        # --- v5.0-5.2: Purity, Boltzmann, Network ---

        # Purity scores: "  Purity:     0.312 (contaminated)"
        for match in re.finditer(
                r'Purity:\s+([\d.]+)\s+\((\w+)\)', content):
            score, band = float(match.group(1)), match.group(2)
            self.purity_scores.append((score, band))

        # Purity drift events: "[warning] purity_drift" or "[watch] purity_drift"
        self.purity_drift_count = len(re.findall(
            r'\[(warning|watch|critical)\]\s*purity_drift', content))

        # Coupling results: "  Coupling:   strongly_coupled (score=1.000, ...)"
        #                    "  Coupling:   independent (score=0.000, ...)"
        #                    "  Coupling:   inconclusive (insufficient data)"
        for match in re.finditer(
                r'Coupling:\s+(\w+)\s+\(score=([\d.]+)', content):
            regime, score = match.group(1), float(match.group(2))
            self.coupling_results.append((regime, score))
        # Also catch inconclusive coupling without a score
        inconclusive_coupling = len(re.findall(
            r'Coupling:\s+inconclusive', content))
        for _ in range(inconclusive_coupling):
            self.coupling_results.append(('inconclusive', -1.0))

        # Structural signatures: "constraint_id: false_ci_rope (confidence: high)"
        #                        "constraint_id: false_natural_law (confidence: ...)"
        #                        "constraint_id: coupling_invariant_rope (confidence: ...)"
        for match in re.finditer(
                r'(\w+):\s+(false_ci_rope|false_natural_law|coupling_invariant_rope)\s+\(confidence:\s*(\w+)\)',
                content):
            cid, sig_type, confidence = match.groups()
            self.boltzmann_signatures.append((cid, sig_type, confidence))

        # Network stability: "  Network stability: stable"
        for match in re.finditer(
                r'Network stability:\s*(\w+)', content):
            self.network_stability.append(match.group(1))

        # Network drift events
        self.network_drift_events = len(re.findall(
            r'\[(warning|watch|critical)\]\s*network_drift', content))

        # --- Orbit data (from orbit_data.json) ---
        self._load_orbit_data()

        return True

    def generate_report(self):
        print("\n" + "="*80)
        print("META-REPORT: Test Suite Analysis".center(80))
        print("="*80 + "\n")
        self._section_test_results()
        self._section_corpus_overview()
        self._section_omegas()
        self._section_purity()
        self._section_boltzmann()
        self._section_network()
        self._section_orbit_families()
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

    def _section_purity(self):
        print("🔬 STRUCTURAL PURITY OVERVIEW (v5.1)")
        print("-" * 80)
        if self.purity_scores:
            scores = [s for s, _ in self.purity_scores]
            avg = sum(scores) / len(scores)
            print(f"  Constraints with Purity Data: {len(self.purity_scores)}")
            print(f"  Average Purity:               {avg:.3f}")

            # Distribution by band
            band_counts = Counter(band for _, band in self.purity_scores)
            # Define bands in descending purity order
            band_order = ['pristine', 'sound', 'borderline', 'contaminated', 'degraded', 'inconclusive']
            print(f"  Purity Distribution:")
            for band in band_order:
                count = band_counts.get(band, 0)
                if count > 0:
                    print(f"    - {band:15s}: {count:4d}")
            # Any bands not in our expected list
            for band, count in band_counts.items():
                if band not in band_order:
                    print(f"    - {band:15s}: {count:4d}")

            if self.purity_drift_count > 0:
                print(f"  Purity Drift Events:          {self.purity_drift_count}")
        else:
            print("  No purity data found in output.")
        print()

    def _section_boltzmann(self):
        print("⚡ BOLTZMANN COMPLIANCE SUMMARY (v5.0)")
        print("-" * 80)
        if self.boltzmann_signatures or self.coupling_results:
            # Signature breakdown
            sig_types = Counter(sig for _, sig, _ in self.boltzmann_signatures)
            fnl_count = sig_types.get('false_natural_law', 0)
            ci_rope_count = sig_types.get('coupling_invariant_rope', 0)
            fcr_count = sig_types.get('false_ci_rope', 0)

            print(f"  Boltzmann Structural Signatures Detected: {len(self.boltzmann_signatures)}")
            print(f"    - False Natural Law (FNL):     {fnl_count}")
            print(f"    - Coupling-Invariant Rope:     {ci_rope_count}")
            print(f"    - False CI-Rope (FCR):         {fcr_count}")

            # Coupling regime breakdown
            if self.coupling_results:
                regime_counts = Counter(r for r, _ in self.coupling_results)
                print(f"  Coupling Analysis ({len(self.coupling_results)} constraints):")
                for regime in ['independent', 'weakly_coupled', 'strongly_coupled', 'inconclusive']:
                    count = regime_counts.get(regime, 0)
                    if count > 0:
                        print(f"    - {regime:20s}: {count:4d}")
        else:
            print("  No Boltzmann compliance data found in output.")
        print()

    def _section_network(self):
        print("🌐 NETWORK DYNAMICS (v5.2)")
        print("-" * 80)
        if self.network_stability:
            stability_counts = Counter(self.network_stability)
            total_assessments = len(self.network_stability)
            print(f"  Network Stability Assessments: {total_assessments}")
            for status in ['stable', 'degrading', 'cascading']:
                count = stability_counts.get(status, 0)
                if count > 0:
                    pct = (count / total_assessments) * 100
                    print(f"    - {status:12s}: {count:4d} ({pct:.1f}%)")
            if self.network_drift_events > 0:
                print(f"  Network Drift Events:          {self.network_drift_events}")
            else:
                print(f"  Network Drift Events:          0")
            # Cascade warnings
            cascade_count = stability_counts.get('cascading', 0)
            if cascade_count > 0:
                print(f"  ⚠ CASCADE WARNING: {cascade_count} constraint(s) in cascading state")
        else:
            print("  No network dynamics data found in output.")
        print()

    def _load_orbit_data(self):
        """Load orbit data from orbit_data.json"""
        orbit_path = os.path.join(ROOT_DIR, 'outputs', 'orbit_data.json')
        if not os.path.exists(orbit_path):
            return
        try:
            import json
            with open(orbit_path, 'r', encoding='utf-8') as f:
                orbit_data = json.load(f)
            for cid, entry in orbit_data.items():
                sig = tuple(entry.get('orbit_signature', []))
                if sig not in self.orbit_families:
                    self.orbit_families[sig] = []
                self.orbit_families[sig].append(cid)
            self.orbit_total = len(orbit_data)
        except (Exception):
            pass

    def _section_orbit_families(self):
        print("GAUGE ORBIT FAMILIES")
        print("-" * 80)
        if not self.orbit_families:
            print("  No orbit data found (run orbit analysis step first).")
            print()
            return

        print(f"  Constraints with Orbit Data: {self.orbit_total}")
        print(f"  Total Orbit Families:        {len(self.orbit_families)}")

        # Singleton families (single-type orbits = gauge-invariant)
        singleton_count = sum(
            len(members) for sig, members in self.orbit_families.items()
            if len(sig) == 1
        )
        multi_count = self.orbit_total - singleton_count
        print(f"  Gauge-Invariant:             {singleton_count}")
        print(f"  Gauge-Variant:               {multi_count}")

        # Top 5 families by size
        sorted_families = sorted(
            self.orbit_families.items(),
            key=lambda x: len(x[1]),
            reverse=True
        )
        print(f"  Top Families:")
        for sig, members in sorted_families[:5]:
            sig_str = '[' + ', '.join(sig) + ']'
            print(f"    - {sig_str:40s}: {len(members):4d}")

        # Unknown-containing orbits
        unknown_families = [
            (sig, members) for sig, members in self.orbit_families.items()
            if 'unknown' in sig or 'indexically_opaque' in sig
        ]
        if unknown_families:
            unknown_count = sum(len(m) for _, m in unknown_families)
            print(f"  Unknown-Containing Orbits:   {len(unknown_families)} families ({unknown_count} constraints)")
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