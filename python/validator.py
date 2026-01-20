#!/usr/bin/env python3
"""
Enhanced Constraint Validator
Provides syntax checking and schema verification for Deferential Realism stories.
"""

import re
import sys
import subprocess
from pathlib import Path

class ConstraintValidator:
    def __init__(self, filepath):
        self.filepath = Path(filepath)
        try:
            self.content = self.filepath.read_text()
        except Exception as e:
            print(f"Error reading file: {e}")
            sys.exit(1)
        self.issues = []
        self.warnings = []

    def validate(self):
        """Run all validation checks."""
        self.check_syntax()
        self.check_module_declaration()
        self.check_multifile_declarations()
        self.check_schema_types()
        self.check_interval_declaration()

        return len(self.issues) == 0

    def check_syntax(self):
        """Verify Prolog syntax using the SWI-Prolog interpreter."""
        try:
            # Use swipl -s (load file) and -g halt (exit) to check for syntax errors
            result = subprocess.run(
                ['swipl', '-s', str(self.filepath), '-g', 'halt'],
                capture_output=True,
                text=True
            )
            if result.returncode != 0:
                # Capture the specific error from stderr
                error_msg = result.stderr.strip()
                self.issues.append(f"Prolog Syntax Error:\n{error_msg}")
        except FileNotFoundError:
            self.warnings.append("swipl not found in PATH; skipping deep syntax check")

    def check_module_declaration(self):
        """Verify the file has a proper module declaration."""
        if not re.search(r':- module\([^)]+,\s*\[\s*\]\)', self.content):
            self.issues.append("Missing or malformed module declaration: :- module(name, []).")

    def check_multifile_declarations(self):
        """Check for required multifile hooks for domain priors."""
        required_hooks = [
            'domain_priors:base_extractiveness/2',
            'domain_priors:suppression_score/2',
            'domain_priors:requires_active_enforcement/1'
        ]

        for hook in required_hooks:
            if hook not in self.content:
                self.warnings.append(f"Missing hook: {hook}")

    def check_schema_types(self):
        """Ensure the constraint_claim uses a recognized ontological category."""
        # Updated to include natural_law, election_cycle, and physical_law
        valid_claims = [
            'mountain', 'noose', 'rope', 'zombie',
            'natural_law', 'physical_law', 'election_cycle',
            'statutory_limit', 'market_constraint'
        ]

        claim_match = re.search(r'constraint_claim\([^,]+,\s*([^)]+)\)', self.content)
        if claim_match:
            claim_type = claim_match.group(1).strip()
            if claim_type not in valid_claims:
                self.issues.append(f"Invalid constraint_claim type: {claim_type}")

    def check_interval_declaration(self):
        """Verify the narrative interval is defined."""
        if 'narrative_ontology:interval' not in self.content:
            self.issues.append("Missing narrative interval declaration")

    def report(self):
        """Print validation report."""
        print(f"\n{'='*60}")
        print(f"Validation Report: {self.filepath.name}")
        print(f"{'='*60}\n")

        if not self.issues and not self.warnings:
            print("✓ All checks passed!")
            return True

        if self.issues:
            print("ISSUES (must fix):")
            for i, issue in enumerate(self.issues, 1):
                print(f"  {i}. ✗ {issue}")
            print()

        if self.warnings:
            print("WARNINGS (review recommended):")
            for i, warning in enumerate(self.warnings, 1):
                print(f"  {i}. ⚠ {warning}")
            print()

        return len(self.issues) == 0

def main():
    if len(sys.argv) < 2:
        print("Usage: python3 validator.py <file.pl>")
        sys.exit(1)

    validator = ConstraintValidator(sys.argv[1])
    validator.validate()
    validator.report()

if __name__ == "__main__":
    main()
