#!/usr/bin/env python3
"""
Prolog File Cleaner - Fixes common AI-generated artifacts

Common issues from Gemini and other LLMs:
1. [cite_start]% comments - should be %[cite_start]
2. ## markdown headers in Prolog files
3. Broken comment blocks
4. Extra whitespace
"""

import os
import re
import sys
from pathlib import Path

class PrologCleaner:
    def __init__(self):
        self.fixes_applied = 0
        self.files_cleaned = 0
        self.errors_found = []

    def clean_line(self, line):
        """Clean a single line of Prolog code"""
        original = line

        # Fix 1: [cite_start]% -> %[cite_start]
        # Match any citation marker followed by %
        if re.match(r'^\s*\[cite', line):
            line = re.sub(r'^(\s*)\[cite([^\]]*)\]%', r'\1%[cite\2]', line)
            if line != original:
                self.fixes_applied += 1

        # Fix 2: Remove ## markdown headers
        # Turn ## Header into % Header (proper Prolog comment)
        if line.strip().startswith('##'):
            line = re.sub(r'^(\s*)##\s*', r'\1% ', line)
            if line != original:
                self.fixes_applied += 1

        # Fix 3: Remove # at start of comment blocks (but not inside strings)
        # Match # at beginning of line followed by space (not inside quotes)
        if re.match(r'^\s*#\s+', line) and '"' not in line:
            line = re.sub(r'^(\s*)#\s+', r'\1% ', line)
            if line != original:
                self.fixes_applied += 1

        # Fix 4: Normalize comment markers (ensure space after %)
        if re.match(r'^\s*%[^\s%]', line):
            line = re.sub(r'^(\s*)%([^\s%])', r'\1% \2', line)
            if line != original:
                self.fixes_applied += 1

        return line

    def clean_file(self, filepath):
        """Clean a single Prolog file"""
        try:
            with open(filepath, 'r', encoding='utf-8') as f:
                lines = f.readlines()
        except Exception as e:
            self.errors_found.append(f"Error reading {filepath}: {e}")
            return False

        cleaned_lines = []
        file_modified = False

        for i, line in enumerate(lines, 1):
            cleaned = self.clean_line(line)
            if cleaned != line:
                file_modified = True
            cleaned_lines.append(cleaned)

        if file_modified:
            try:
                with open(filepath, 'w', encoding='utf-8') as f:
                    f.writelines(cleaned_lines)
                self.files_cleaned += 1
                return True
            except Exception as e:
                self.errors_found.append(f"Error writing {filepath}: {e}")
                return False

        return False

    def scan_for_issues(self, filepath):
        """Scan file for potential issues without fixing"""
        issues = []

        try:
            with open(filepath, 'r', encoding='utf-8') as f:
                lines = f.readlines()
        except Exception as e:
            return [f"Cannot read file: {e}"]

        for i, line in enumerate(lines, 1):
            # Check for citation artifacts
            if '[cite' in line and line.strip().startswith('[cite'):
                issues.append(f"Line {i}: Citation marker at start of line")

            # Check for markdown headers
            if line.strip().startswith('##'):
                issues.append(f"Line {i}: Markdown header (##)")

            # Check for unescaped # in comments
            if re.match(r'^\s*#\s+', line) and '"' not in line:
                issues.append(f"Line {i}: Hash comment instead of %")

            # Check for syntax errors (basic)
            if ':-' in line and not line.strip().endswith('.') and not line.strip().endswith(','):
                if i < len(lines) and not lines[i].strip().startswith('%'):
                    issues.append(f"Line {i}: Possible missing period or comma")

        return issues

    def clean_directory(self, directory, dry_run=False):
        """Clean all .pl files in directory"""
        directory = Path(directory)

        if not directory.exists():
            print(f"Error: Directory {directory} does not exist")
            return

        pl_files = list(directory.glob('*.pl'))

        if not pl_files:
            print(f"No .pl files found in {directory}")
            return

        print(f"{'[DRY RUN] ' if dry_run else ''}Scanning {len(pl_files)} Prolog files...")

        issues_by_file = {}

        for filepath in sorted(pl_files):
            issues = self.scan_for_issues(filepath)

            if issues:
                issues_by_file[filepath.name] = issues

            if not dry_run and issues:
                self.clean_file(filepath)

        # Report results
        print(f"\n{'='*70}")
        print(f"PROLOG CLEANER REPORT")
        print(f"{'='*70}\n")

        if dry_run:
            if issues_by_file:
                print(f"Found issues in {len(issues_by_file)} file(s):\n")
                for filename, issues in sorted(issues_by_file.items()):
                    print(f"  {filename}:")
                    for issue in issues:
                        print(f"    - {issue}")
                    print()
                print(f"Run without --dry-run to automatically fix these issues.")
            else:
                print("✓ No issues found in any files.")
        else:
            if self.files_cleaned > 0:
                print(f"✓ Cleaned {self.files_cleaned} file(s)")
                print(f"✓ Applied {self.fixes_applied} fix(es)")
            else:
                print("✓ No files needed cleaning")

            if self.errors_found:
                print(f"\n⚠ Errors encountered:")
                for error in self.errors_found:
                    print(f"  - {error}")

        print(f"\n{'='*70}\n")

def main():
    import argparse

    parser = argparse.ArgumentParser(
        description='Clean Prolog files by fixing common AI-generated artifacts'
    )
    parser.add_argument(
        'directory',
        nargs='?',
        default='../prolog/testsets/',
        help='Directory containing .pl files (default: ../prolog/testsets/)'
    )
    parser.add_argument(
        '--dry-run',
        action='store_true',
        help='Scan for issues without fixing them'
    )
    parser.add_argument(
        '--file',
        help='Clean a specific file instead of directory'
    )

    args = parser.parse_args()

    cleaner = PrologCleaner()

    if args.file:
        # Clean single file
        filepath = Path(args.file)
        if not filepath.exists():
            print(f"Error: File {filepath} does not exist")
            sys.exit(1)

        issues = cleaner.scan_for_issues(filepath)

        if issues:
            print(f"Issues found in {filepath.name}:")
            for issue in issues:
                print(f"  - {issue}")

            if not args.dry_run:
                print(f"\nCleaning {filepath.name}...")
                if cleaner.clean_file(filepath):
                    print(f"✓ File cleaned successfully")
                else:
                    print(f"✗ File was already clean or errors occurred")
        else:
            print(f"✓ No issues found in {filepath.name}")
    else:
        # Clean directory
        cleaner.clean_directory(args.directory, dry_run=args.dry_run)

if __name__ == '__main__':
    main()
