# python_test_suite.py

import os
import re
import sys

# Construct absolute paths from this script's location
SCRIPT_DIR = os.path.dirname(os.path.abspath(__file__))
ROOT_DIR = os.path.dirname(SCRIPT_DIR)
DATASETS_DIR = os.path.join(ROOT_DIR, 'prolog', 'testsets')
OUTPUT_FILE = os.path.join(ROOT_DIR, 'prolog', 'validation_suite.pl')

INTERVAL_REGEX = re.compile(r"interval\s*\(\s*'?(\w+)")

def build_suite():
    if not os.path.isdir(DATASETS_DIR):
        print(f"Error: Datasets directory not found at {DATASETS_DIR}")
        sys.exit(1)

    try:
        files = sorted([f for f in os.listdir(DATASETS_DIR) if f.endswith('.pl')])
    except Exception as e:
        print(f"Error listing files in {DATASETS_DIR}: {e}")
        sys.exit(1)

    print(f"Found {len(files)} .pl files in {DATASETS_DIR}")

    test_case_facts = []
    for idx, filename in enumerate(files, 1):
        filepath = f"testsets/{filename}"
        interval_id = 'unknown_interval'
        try:
            full_path = os.path.join(DATASETS_DIR, filename)
            with open(full_path, 'r', encoding='utf-8') as f:
                content = f.read()
                match = INTERVAL_REGEX.search(content)
                if match:
                    interval_id = match.group(1)
        except Exception as e:
            print(f"Warning: Could not read or parse {filename}: {e}")

        label = filename.replace('.pl', '').upper()
        test_case_facts.append(f"test_case('{filepath}', '{interval_id}', '{label}', {idx}).")

    with open(OUTPUT_FILE, 'w', encoding='utf-8') as out:
        out.write(":- module(validation_suite, [run_dynamic_suite/0]).\n")
        out.write(":- use_module(library(prolog_stack)).\n")
        out.write(":- use_module(scenario_manager).\n")
        out.write(":- use_module(data_validation).\n")
        out.write(":- use_module(report_generator).\n\n")
        # Set CWD to the 'prolog' directory to make file paths reliable
        out.write(":- chdir('../prolog').\n\n")
        out.write(":- dynamic test_passed/1, test_failed/3, test_case/4.\n\n")

        out.write("% --- Test Case Definitions ---\n")
        out.write('\n'.join(test_case_facts) + '\n\n')

        out.write("% --- Test Suite Runner ---\n")
        out.write("run_dynamic_suite :-\n")
        out.write("    retractall(test_passed(_)),\n")
        out.write("    retractall(test_failed(_, _, _)),\n")
        out.write("    writeln('--- STARTING DYNAMIC VALIDATION ---'),\n")
        out.write("    forall(test_case(Path, ID, Label, N), run_single_test(Path, ID, Label, N)),\n")
        out.write("    count_and_report,\n")
        out.write("    % Call validate_all directly from data_validation module\n")
        out.write("    data_validation:validate_all.\n\n")

        out.write("% --- Single Test Executor ---\n")
        out.write("run_single_test(Path, ID, _Label, N) :-\n")
        out.write("    format('~n[~w] EXECUTING: ~w~n', [N, Path]),\n")
        out.write("    catch_with_backtrace(\n")
        out.write("        ( load_and_run(Path, ID) ->\n")
        out.write("            assertz(test_passed(Path)),\n")
        out.write("            format('[PASS] ~w~n', [Path])\n")
        out.write("        ;   assertz(test_failed(Path, audit_failed, 'load_and_run returned false')),\n")
        out.write("            format('[AUDIT FAIL] ~w~n', [Path])\n")
        out.write("        ),\n")
        out.write("        E,\n")
        out.write("        (   assertz(test_failed(Path, exception, E)),\n")
        out.write("            format('[FAIL] Exception for ~w: ~w~n', [Path, E]),\n")
        out.write("            print_prolog_backtrace(current_output, E)\n")
        out.write("        )\n")
        out.write("    ),\n")
        out.write("    report_generator:generate_llm_feedback(ID).\n\n")

        out.write("% --- Result Counter & Reporter ---\n")
        out.write("count_and_report :-\n")
        out.write("    findall(P, test_passed(P), Ps), length(Ps, PC),\n")
        out.write("    findall(F, test_failed(F,_,_), Fs), length(Fs, FC),\n")
        out.write("    writeln(''),\n")
        out.write("    writeln('=================================================='),\n")
        out.write("    writeln('           TEST SUITE SUMMARY'),\n")
        out.write("    writeln('=================================================='),\n")
        out.write("    format('Passed: ~w~n', [PC]),\n")
        out.write("    format('Failed: ~w~n', [FC]),\n")
        out.write("    (FC > 0 -> report_failures ; true),\n")
        out.write("    writeln('==================================================').\n\n")

        out.write("report_failures :-\n")
        out.write("    writeln('--- FAILED TESTS ---'),\n")
        out.write("    forall(test_failed(Path, Type, Detail),\n")
        out.write("           format('~n  - [~w] ~w~n    Reason: ~w~n', [Type, Path, Detail])).\n\n")

    print(f"✓ Generated {OUTPUT_FILE} with {len(test_case_facts)} tests")

if __name__ == "__main__":
    build_suite()
