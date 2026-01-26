#!/usr/bin/env python3
"""
Corpus Analysis Orchestrator

Runs complete corpus analysis pipeline to answer:
1. Should DR framework extend with new categories (Piton, Scaffold, Tangled Rope, Wings)?
2. Do existing 4 indices explain all variance?

Generates three reports:
- variance_analysis.md - Index variance analysis
- index_sufficiency.md - Index sufficiency test
- pattern_mining.md - Structural pattern mining
"""

import sys
import subprocess
from pathlib import Path
from datetime import datetime

class CorpusAnalysisOrchestrator:
    """Orchestrates the complete corpus analysis pipeline"""

    def __init__(self, output_txt='../outputs/output.txt', testsets_dir='../prolog/testsets/', output_dir='../outputs/'):
        self.output_txt = Path(output_txt)
        self.testsets_dir = Path(testsets_dir)
        self.output_dir = Path(output_dir)

        # Intermediate data file
        self.corpus_data = self.output_dir / 'corpus_data.json'

        # Report files
        self.variance_report = self.output_dir / 'variance_analysis.md'
        self.sufficiency_report = self.output_dir / 'index_sufficiency.md'
        self.pattern_report = self.output_dir / 'pattern_mining.md'

    def run_pipeline(self):
        """Execute complete analysis pipeline"""
        print("="*80)
        print("CORPUS ANALYSIS PIPELINE".center(80))
        print("="*80)
        print(f"\nStarted: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")

        # Step 1: Extract corpus data
        print("Step 1: Extracting corpus data...")
        print("-" * 80)
        if not self._run_extraction():
            print("\n❌ Extraction failed. Aborting pipeline.")
            return False

        print("✓ Corpus data extracted\n")

        # Step 2: Generate variance analysis
        print("Step 2: Generating variance analysis report...")
        print("-" * 80)
        if not self._run_variance_analysis():
            print("\n❌ Variance analysis failed. Aborting pipeline.")
            return False

        print("✓ Variance analysis complete\n")

        # Step 3: Generate sufficiency test
        print("Step 3: Running index sufficiency test...")
        print("-" * 80)
        if not self._run_sufficiency_test():
            print("\n❌ Sufficiency test failed. Aborting pipeline.")
            return False

        print("✓ Sufficiency test complete\n")

        # Step 4: Generate pattern mining
        print("Step 4: Mining structural patterns...")
        print("-" * 80)
        if not self._run_pattern_mining():
            print("\n❌ Pattern mining failed. Aborting pipeline.")
            return False

        print("✓ Pattern mining complete\n")

        # Final summary
        self._print_summary()

        return True

    def _run_extraction(self):
        """Run data extraction"""
        try:
            result = subprocess.run([
                'python3', 'extract_corpus_data.py',
                '--output-txt', str(self.output_txt),
                '--testsets', str(self.testsets_dir),
                '--json-output', str(self.corpus_data)
            ], capture_output=True, text=True, check=True)

            print(result.stdout)
            return True

        except subprocess.CalledProcessError as e:
            print(f"Error: {e.stderr}")
            return False
        except Exception as e:
            print(f"Unexpected error: {e}")
            return False

    def _run_variance_analysis(self):
        """Run variance analysis report"""
        try:
            result = subprocess.run([
                'python3', 'variance_analyzer.py',
                '--corpus-data', str(self.corpus_data),
                '--output', str(self.variance_report)
            ], capture_output=True, text=True, check=True)

            print(result.stdout)
            return True

        except subprocess.CalledProcessError as e:
            print(f"Error: {e.stderr}")
            return False
        except Exception as e:
            print(f"Unexpected error: {e}")
            return False

    def _run_sufficiency_test(self):
        """Run index sufficiency test"""
        try:
            result = subprocess.run([
                'python3', 'sufficiency_tester.py',
                '--corpus-data', str(self.corpus_data),
                '--output', str(self.sufficiency_report)
            ], capture_output=True, text=True, check=True)

            print(result.stdout)
            return True

        except subprocess.CalledProcessError as e:
            print(f"Error: {e.stderr}")
            return False
        except Exception as e:
            print(f"Unexpected error: {e}")
            return False

    def _run_pattern_mining(self):
        """Run pattern mining analysis"""
        try:
            result = subprocess.run([
                'python3', 'pattern_miner.py',
                '--corpus-data', str(self.corpus_data),
                '--output', str(self.pattern_report)
            ], capture_output=True, text=True, check=True)

            print(result.stdout)
            return True

        except subprocess.CalledProcessError as e:
            print(f"Error: {e.stderr}")
            return False
        except Exception as e:
            print(f"Unexpected error: {e}")
            return False

    def _print_summary(self):
        """Print final summary"""
        print("="*80)
        print("PIPELINE COMPLETE".center(80))
        print("="*80)
        print(f"\nCompleted: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n")

        print("📊 Generated Reports:\n")
        print(f"  1. Variance Analysis:     {self.variance_report}")
        print(f"  2. Index Sufficiency:     {self.sufficiency_report}")
        print(f"  3. Pattern Mining:        {self.pattern_report}")

        print("\n📁 Intermediate Data:\n")
        print(f"  - Corpus Data (JSON):     {self.corpus_data}")

        print("\n" + "="*80)
        print("NEXT STEPS".center(80))
        print("="*80)
        print("""
1. Review the three reports in order:
   - variance_analysis.md - Understand how constraints vary across indices
   - index_sufficiency.md - Check if 4 indices are sufficient
   - pattern_mining.md - Identify candidate new categories

2. Answer the core questions:
   - Should we add new categories (Piton, Scaffold, Tangled Rope, Wings)?
   - Are the existing 4 indices sufficient?

3. Key indicators to look for:
   - High collision rate (>15%) → Indices may be insufficient
   - Many structural twins → Suggest new categories
   - Large hybrid/transition groups → Consider Tangled Rope, Scaffold
   - Many false mountains → Consider Piton category

4. Make informed decisions:
   - If indices are sufficient → Focus on refining thresholds
   - If indices are insufficient → Add 5th index or new categories
   - If patterns emerge → Formalize new categories

5. Update the DR framework accordingly and re-run tests
""")

        print("="*80 + "\n")

def main():
    import argparse

    parser = argparse.ArgumentParser(
        description='Run complete corpus analysis pipeline'
    )
    parser.add_argument(
        '--output-txt',
        default='../outputs/output.txt',
        help='Path to output.txt from Prolog tests'
    )
    parser.add_argument(
        '--testsets',
        default='../prolog/testsets/',
        help='Path to testsets directory'
    )
    parser.add_argument(
        '--output-dir',
        default='../outputs/',
        help='Output directory for reports'
    )

    args = parser.parse_args()

    orchestrator = CorpusAnalysisOrchestrator(
        output_txt=args.output_txt,
        testsets_dir=args.testsets,
        output_dir=args.output_dir
    )

    success = orchestrator.run_pipeline()

    sys.exit(0 if success else 1)

if __name__ == '__main__':
    main()
