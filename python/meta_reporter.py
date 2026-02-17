#!/usr/bin/env python3
"""
Meta-reporter — pipeline health dashboard.

Reads outputs/pipeline_output.json for all classification/diagnostic data.
Hybrid: still reads outputs/output.txt for Prolog test pass/fail counts
(not available in the JSON).
"""

import json
import os
import re
import sys
from collections import Counter
from pathlib import Path

SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
ROOT_DIR = os.path.dirname(SCRIPT_DIR)


class MetaReporter:
    def __init__(self):
        self.pipeline_json = Path(ROOT_DIR) / 'outputs' / 'pipeline_output.json'
        self.output_txt = Path(ROOT_DIR) / 'outputs' / 'output.txt'
        self.orbit_json = Path(ROOT_DIR) / 'outputs' / 'orbit_data.json'

        # Test results (from output.txt)
        self.passed_tests = 0
        self.failed_tests = 0
        self.test_failures = []

        # Classification data (from pipeline_output.json)
        self.corpus_size = 0
        self.type_distribution = {}
        self.omegas = []          # list of (name, type)
        self.purity_scores = []   # list of (score, band)
        self.purity_summary = {}
        self.coupling_summary = {}
        self.signature_distribution = {}
        self.drift_event_counts = {}
        # Global network stability assessment — replaces old per-constraint
        # count which was misleading (showed 1035x "stable" from repeated
        # per-chunk regex matches, not true per-constraint assessments).
        self.network_stability = None
        self.boltzmann_summary = {}

        # Orbit data (from orbit_data.json)
        self.orbit_families = {}
        self.orbit_total = 0

    def parse(self):
        self._parse_test_results()
        self._parse_pipeline_json()
        self._load_orbit_data()
        return True

    def _parse_test_results(self):
        """Read output.txt ONLY for pass/fail counts and failure details."""
        if not self.output_txt.exists():
            return
        with open(self.output_txt, 'r', encoding='utf-8') as f:
            content = f.read()

        self.passed_tests = len(re.findall(r'\[PASS\]', content))
        self.failed_tests = len(re.findall(r'\[(AUDIT FAIL|FAIL)\]', content))

        for match in re.finditer(r"  -\s\[(\w+)\]\s(.+?)\n\s+Reason:\s(.+)", content):
            ftype, path, detail = match.groups()
            self.test_failures.append({
                'type': ftype, 'path': path.strip(), 'detail': detail.strip()
            })

    def _parse_pipeline_json(self):
        """Read pipeline_output.json for all classification/diagnostic data."""
        if not self.pipeline_json.exists():
            print(f"Error: {self.pipeline_json} does not exist", file=sys.stderr)
            return

        with open(self.pipeline_json, 'r', encoding='utf-8') as f:
            data = json.load(f)

        diag = data.get('diagnostic', {})
        val = data.get('validation', {})
        per_constraint = data.get('per_constraint', [])

        # Corpus overview
        self.corpus_size = diag.get('corpus_size', len(per_constraint))
        self.type_distribution = diag.get('type_distribution', {})

        # Omegas — aggregate from per_constraint
        seen = set()
        for c in per_constraint:
            for o in (c.get('omegas') or []):
                oid = o['id']
                if oid not in seen:
                    seen.add(oid)
                    self.omegas.append((oid, o.get('type', 'conceptual')))

        # Purity — per-constraint scores for avg calc + summary for bands
        for c in per_constraint:
            score = c.get('purity_score')
            band = c.get('purity_band')
            if score is not None:
                self.purity_scores.append((score, band))
        self.purity_summary = diag.get('purity_summary', {})

        # Drift events — aggregate (not split purity vs network)
        self.drift_event_counts = diag.get('drift_event_counts', {})

        # Coupling
        self.coupling_summary = diag.get('coupling_summary', {})

        # Boltzmann / signatures
        self.boltzmann_summary = diag.get('boltzmann_summary', {})
        self.signature_distribution = val.get('signature_distribution', {})

        # Network stability — single global assessment
        self.network_stability = diag.get('network_stability')

    def _load_orbit_data(self):
        """Load orbit data from orbit_data.json."""
        if not self.orbit_json.exists():
            return
        try:
            with open(self.orbit_json, 'r', encoding='utf-8') as f:
                orbit_data = json.load(f)
            for cid, entry in orbit_data.items():
                sig = tuple(entry.get('orbit_signature', []))
                if sig not in self.orbit_families:
                    self.orbit_families[sig] = []
                self.orbit_families[sig].append(cid)
            self.orbit_total = len(orbit_data)
        except Exception:
            pass

    # --- Report sections ---

    def generate_report(self):
        print("\n" + "=" * 80)
        print("META-REPORT: Test Suite Analysis".center(80))
        print("=" * 80 + "\n")
        self._section_test_results()
        self._section_corpus_overview()
        self._section_omegas()
        self._section_purity()
        self._section_boltzmann()
        self._section_network()
        self._section_orbit_families()
        print("=" * 80 + "\n")

    def _section_test_results(self):
        print("\U0001f4ca TEST EXECUTION SUMMARY")
        print("-" * 80)
        total = self.passed_tests + self.failed_tests
        if total > 0:
            pass_rate = (self.passed_tests / total) * 100
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
        print("\U0001f4da CORPUS OVERVIEW")
        print("-" * 80)
        print(f"  Total Constraints Analyzed: {self.corpus_size}")
        if self.type_distribution:
            print(f"  Identified Constraint Type Distribution:")
            for ctype, count in sorted(self.type_distribution.items(),
                                       key=lambda x: x[1], reverse=True):
                pct = (count / self.corpus_size) * 100 if self.corpus_size else 0
                print(f"    - {ctype:15s}: {count:4d} ({pct:.1f}%)")
        else:
            print("  No explicit constraint types identified.")
        print()

    def _section_omegas(self):
        print("\u2753 EPISTEMOLOGICAL GAPS (Omegas)")
        print("-" * 80)
        if self.omegas:
            print(f"  Total Unique Omegas Found: {len(self.omegas)}")
            omega_types = Counter(otype for _, otype in self.omegas)
            for otype, count in omega_types.most_common():
                print(f"  - {otype.upper()}: {count}")
        else:
            print(f"  \u2713 No unresolved omegas detected")
        print()

    def _section_purity(self):
        print("\U0001f52c STRUCTURAL PURITY OVERVIEW (v5.1)")
        print("-" * 80)
        if self.purity_scores:
            scores = [s for s, _ in self.purity_scores]
            avg = sum(scores) / len(scores)
            print(f"  Constraints with Purity Data: {len(self.purity_scores)}")
            print(f"  Average Purity:               {avg:.3f}")

            band_order = ['pristine', 'sound', 'borderline', 'contaminated',
                          'degraded', 'inconclusive']
            print(f"  Purity Distribution:")
            for band in band_order:
                count = self.purity_summary.get(band, 0)
                if count > 0:
                    print(f"    - {band:15s}: {count:4d}")
            for band, count in sorted(self.purity_summary.items()):
                if band not in band_order and count > 0:
                    print(f"    - {band:15s}: {count:4d}")

            total_drift = sum(self.drift_event_counts.values())
            if total_drift > 0:
                print(f"  Drift Events (all types):     {total_drift}")
                for severity in ['critical', 'warning', 'watch']:
                    count = self.drift_event_counts.get(severity, 0)
                    if count > 0:
                        print(f"    - {severity:15s}: {count:4d}")
        else:
            print("  No purity data found.")
        print()

    def _section_boltzmann(self):
        print("\u26a1 BOLTZMANN COMPLIANCE SUMMARY (v5.0)")
        print("-" * 80)
        if self.signature_distribution or self.coupling_summary:
            # Signature breakdown from validation.signature_distribution
            fnl = self.signature_distribution.get('false_natural_law', 0)
            ci_rope = self.signature_distribution.get('coupling_invariant_rope', 0)
            fcr = self.signature_distribution.get('false_ci_rope', 0)
            total_sigs = fnl + ci_rope + fcr

            print(f"  Boltzmann Structural Signatures Detected: {total_sigs}")
            print(f"    - False Natural Law (FNL):     {fnl}")
            print(f"    - Coupling-Invariant Rope:     {ci_rope}")
            print(f"    - False CI-Rope (FCR):         {fcr}")

            if self.coupling_summary:
                total_coupling = sum(self.coupling_summary.values())
                print(f"  Coupling Analysis ({total_coupling} constraints):")
                for regime in ['independent', 'weakly_coupled',
                               'strongly_coupled', 'inconclusive']:
                    count = self.coupling_summary.get(regime, 0)
                    if count > 0:
                        print(f"    - {regime:20s}: {count:4d}")
                for regime, count in sorted(self.coupling_summary.items()):
                    if regime not in ('independent', 'weakly_coupled',
                                      'strongly_coupled', 'inconclusive') and count > 0:
                        print(f"    - {regime:20s}: {count:4d}")
        else:
            print("  No Boltzmann compliance data found.")
        print()

    def _section_network(self):
        print("\U0001f310 NETWORK DYNAMICS (v5.2)")
        print("-" * 80)
        if self.network_stability:
            print(f"  Network Stability: {self.network_stability}")
            if self.network_stability == 'cascading':
                print(f"  \u26a0 CASCADE WARNING: network in cascading state")
        else:
            print("  No network dynamics data found.")
        print()

    def _section_orbit_families(self):
        print("GAUGE ORBIT FAMILIES")
        print("-" * 80)
        if not self.orbit_families:
            print("  No orbit data found (run orbit analysis step first).")
            print()
            return

        print(f"  Constraints with Orbit Data: {self.orbit_total}")
        print(f"  Total Orbit Families:        {len(self.orbit_families)}")

        singleton_count = sum(
            len(members) for sig, members in self.orbit_families.items()
            if len(sig) == 1
        )
        multi_count = self.orbit_total - singleton_count
        print(f"  Gauge-Invariant:             {singleton_count}")
        print(f"  Gauge-Variant:               {multi_count}")

        sorted_families = sorted(
            self.orbit_families.items(),
            key=lambda x: len(x[1]),
            reverse=True
        )
        print(f"  Top Families:")
        for sig, members in sorted_families[:5]:
            sig_str = '[' + ', '.join(sig) + ']'
            print(f"    - {sig_str:40s}: {len(members):4d}")

        unknown_families = [
            (sig, members) for sig, members in self.orbit_families.items()
            if 'unknown' in sig or 'indexically_opaque' in sig
        ]
        if unknown_families:
            unknown_count = sum(len(m) for _, m in unknown_families)
            print(f"  Unknown-Containing Orbits:   {len(unknown_families)} families ({unknown_count} constraints)")
        print()


def main():
    reporter = MetaReporter()
    reporter.parse()
    reporter.generate_report()


if __name__ == '__main__':
    main()
