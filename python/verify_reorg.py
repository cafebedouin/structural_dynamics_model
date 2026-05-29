#!/usr/bin/env python3
"""Verify python/ reorganization: AST parse, import smoke test, pipeline check."""
import ast, glob, subprocess, sys
from pathlib import Path

REPO = Path(__file__).resolve().parent.parent
DIRS = ['python/tests', 'python/sweeps', 'python/audits']
FAILURES = []

def fail(msg):
    FAILURES.append(msg)
    print(f'  FAIL: {msg}')

def ok(msg):
    print(f'  ok:   {msg}')

# 1. diff_cut_proof.py overlay reference
print('\n[1] diff_cut_proof.py Prolog overlay check')
dcf = (REPO / 'python/tests/diff_cut_proof.py').read_text()
if '[tests/test_battery_variants]' in dcf:
    ok('overlay references tests/test_battery_variants')
else:
    fail('overlay does not reference tests/test_battery_variants')

# 2. AST parse all moved files
print('\n[2] AST parse all moved files')
for d in DIRS:
    for f in sorted(glob.glob(f'{d}/*.py')):
        try:
            ast.parse(open(f).read())
            ok(f'parse {f}')
        except SyntaxError as e:
            fail(f'syntax error in {f}: {e}')

# 3. Import smoke test
print('\n[3] Import smoke test')
for d in DIRS:
    for f in sorted(glob.glob(f'{d}/*.py')):
        r = subprocess.run(
            [sys.executable, '-c',
             f'import sys; sys.argv=["test"]; '
             f'import ast; src=open("{f}").read(); '
             f'code=compile(src, "{f}", "exec"); '
             f'exec(code, {{"__file__": "{f}", "__name__": "__check__"}})'],
            capture_output=True, text=True, timeout=30, cwd=str(REPO)
        )
        if r.returncode not in (0, 1) or 'ImportError' in r.stderr or 'ModuleNotFoundError' in r.stderr:
            fail(f'import failed for {f}:\n{r.stderr[-300:]}')
        else:
            ok(f'import ok {f}')

# 4. Pipeline check
print('\n[4] Pipeline smoke test')
r = subprocess.run(
    [sys.executable, 'python/run_pipeline.py'],
    capture_output=True, text=True, timeout=120, cwd=str(REPO)
)
if r.returncode != 0 or 'FAIL' in r.stdout or 'ERROR' in r.stdout:
    fail(f'pipeline failed')
else:
    ok('pipeline all steps green')

# 5. Frozen CLI scripts
print('\n[5] Frozen CLI scripts')
for script in ['python/config_sensitivity_sweep.py',
               'python/directionality_sensitivity_sweep.py',
               'python/linter.py']:
    r = subprocess.run(
        [sys.executable, '-c',
         f'import ast; ast.parse(open("{script}").read()); print("ok")'],
        capture_output=True, text=True, cwd=str(REPO)
    )
    if 'ok' in r.stdout:
        ok(script)
    else:
        fail(f'{script}: import error')

# 6. Linter library import
print('\n[6] linter library import')
r = subprocess.run(
    [sys.executable, '-c', 'from linter import lint_file; print("ok")'],
    capture_output=True, text=True, cwd=str(REPO / 'python')
)
if 'ok' in r.stdout:
    ok('from linter import lint_file')
else:
    fail(f'linter import failed')

# Summary
print(f'\n{"="*50}')
if FAILURES:
    print(f'FAILED ({len(FAILURES)} failures):')
    for f in FAILURES:
        print(f'  - {f}')
    sys.exit(1)
else:
    count = sum(len(glob.glob(f"{d}/*.py")) for d in DIRS)
    print(f'ALL CHECKS PASSED ({count} moved files verified)')
