# Usage:
####################################################
# 1. Place the Python script in your root directory.
# 2. Run python build_suite.py.
# 3. Launch Prolog: swipl validation_suite.pl.
# 4. Run the tests: ?- run_dynamic_suite.

import os
import re

DATASETS_DIR = './datasets/'
OUTPUT_FILE = 'validation_suite.pl'

# Improved Regex: Captures ID regardless of single/double quotes or no quotes
# Matches: interval('ID', ...), interval("ID", ...), or interval(id, ...)
INTERVAL_REGEX = re.compile(r"interval\s*\(\s*['\"]?([a-zA-Z0-9_]+)['\"]?\s*,")

def build_suite():
    test_entries = []

    if not os.path.exists(DATASETS_DIR):
        print(f"Error: Directory {DATASETS_DIR} not found.")
        return

    files = sorted([f for f in os.listdir(DATASETS_DIR) if f.endswith('_data.pl')])

    for idx, filename in enumerate(files, 1):
        filepath = os.path.join(DATASETS_DIR, filename)
        interval_id = 'unknown_interval'

        try:
            with open(filepath, 'r') as f:
                content = f.read()
                match = INTERVAL_REGEX.search(content)
                if match:
                    interval_id = match.group(1)
                else:
                    print(f"Warning: No interval found in {filename}")
        except Exception as e:
            print(f"Error reading {filename}: {e}")

        label = filename.replace('_data.pl', '').upper()
        # Use single quotes for atoms in Prolog if they aren't standard camelCase
        test_entries.append(f"    test_file('{filepath}', '{interval_id}', '{label}', {idx})")

    with open(OUTPUT_FILE, 'w') as out:
        out.write(":- use_module(scenario_manager).\n")
        out.write(":- dynamic test_passed/1.\n")
        out.write(":- dynamic test_failed/2.\n\n")

        out.write("run_dynamic_suite :-\n")
        out.write("    retractall(test_passed(_)),\n")
        out.write("    retractall(test_failed(_, _)),\n")
        out.write("    writeln('--- STARTING DYNAMIC VALIDATION ---'),\n")
        out.write(',\n'.join(test_entries) + ',\n')
        out.write("    count_and_report.\n\n")

        # Define helper predicates at the end to keep clauses together
        out.write("test_file(Path, ID, Label, N) :-\n")
        out.write("    format('~n[~w] DOMAIN: ~w (~w)~n', [N, Label, Path]),\n")
        out.write("    catch((run_scenario(Path, ID), assertz(test_passed(Path))), E, (assertz(test_failed(Path, E)), format('[FAIL] ~w~n', [E]))).\n\n")

        out.write("count_and_report :-\n")
        out.write("    findall(P, test_passed(P), Ps), length(Ps, PC), findall(F, test_failed(F,_), Fs), length(Fs, FC),\n")
        out.write("    format('~nDONE: ~w Passed, ~w Failed~n', [PC, FC]).\n")

if __name__ == "__main__":
    build_suite()
