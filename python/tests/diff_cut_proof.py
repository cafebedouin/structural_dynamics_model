"""
Step 2 diff proof: run sigmoid baseline before and after the write_entries cut.
Compares sheaf/presheaf classification vector (h0 field) constraint-by-constraint.
"""

import subprocess, json, sys, os, tempfile, time
from pathlib import Path

# repo-root bootstrap (depth-agnostic; byte-identical in every nested script)
_here = Path(__file__).resolve()
_root = next(c for c in (_here, *_here.parents) if (c / "pyproject.toml").is_file())
sys.path.insert(0, str(_root / "python"))
from paths import PROLOG_DIR, OUTPUTS

OUT_DIR    = OUTPUTS

# Identical overlay to test_battery.py but with sigmoid (default, no retract needed)
OVERLAY = """\
:- use_module(config).
:- ( retract(config:param(corpus_path, _)) -> true ; true ),
   asserta(config:param(corpus_path, 'archives/prolog_v5')).
:- [stack].
:- [tests/test_battery_variants].
:- [product_site_export].
:- product_site_export:run_product_export_to('{outpath}'), halt.
"""

def run_export(outpath, label):
    overlay = OVERLAY.format(outpath=str(outpath))
    with tempfile.NamedTemporaryFile(mode='w', suffix='.pl', dir=PROLOG_DIR, delete=False) as f:
        f.write(overlay)
        overlay_path = f.name
    try:
        cmd = ['swipl', '--stack_limit=4G',
               '-g', f'["{Path(overlay_path).name}"]',
               '-t', 'halt(1)']
        print(f'\n[{label}] Running sigmoid baseline...', flush=True)
        t0 = time.time()
        result = subprocess.run(cmd, cwd=str(PROLOG_DIR), capture_output=True, text=True, timeout=3600)
        elapsed = time.time() - t0
        print(f'[{label}] Completed in {elapsed:.1f}s, rc={result.returncode}', flush=True)
        for line in result.stderr.strip().splitlines():
            if any(tag in line for tag in ['[corpus]', '[product_export]']):
                print(f'  {line}')
        if result.returncode != 0:
            print(f'STDERR tail:\n{result.stderr[-2000:]}')
            return False
        return True
    except subprocess.TimeoutExpired:
        print(f'[{label}] TIMEOUT')
        return False
    finally:
        os.unlink(overlay_path)


def extract_vector(path):
    """Returns {constraint_id: 'sheaf'|'presheaf'}"""
    with open(path) as f:
        data = json.load(f)
    return {c: ('sheaf' if v.get('h0', 0) == 1 else 'presheaf')
            for c, v in data.items() if isinstance(v, dict)}


def diff_vectors(vec_before, vec_after, label_before, label_after):
    all_ids = set(vec_before) | set(vec_after)
    print(f'\nConstraints in {label_before}: {len(vec_before)}')
    print(f'Constraints in {label_after}:  {len(vec_after)}')

    diffs = []
    for c in sorted(all_ids):
        b = vec_before.get(c, 'MISSING')
        a = vec_after.get(c, 'MISSING')
        if b != a:
            diffs.append((c, b, a))

    print(f'\nChanged classifications: {len(diffs)}')
    if diffs:
        print('Changed (first 30):')
        for c, b, a in diffs[:30]:
            print(f'  {c}: {b} → {a}')
    else:
        print('ZERO DIFF — cut is classification-preserving.')
    return len(diffs) == 0


if __name__ == '__main__':
    before_path = OUT_DIR / 'cut_proof_before.json'
    after_path  = OUT_DIR / 'cut_proof_after.json'

    # Step A: Run BEFORE baseline
    ok = run_export(before_path, 'BEFORE')
    if not ok:
        print('BEFORE run failed — aborting.')
        sys.exit(1)

    before_vec = extract_vector(before_path)
    sheaves_before   = sum(1 for v in before_vec.values() if v == 'sheaf')
    presheaves_before = sum(1 for v in before_vec.values() if v == 'presheaf')
    print(f'BEFORE: N={len(before_vec)}, sheaves={sheaves_before}, presheaves={presheaves_before}')

    # Step B: Also verify BEFORE matches alt3k_sigmoid.json (sanity check)
    ref_path = OUT_DIR / 'alt3k_sigmoid.json'
    if ref_path.exists():
        ref_vec = extract_vector(ref_path)
        match_count = sum(1 for c in before_vec if before_vec.get(c) == ref_vec.get(c))
        print(f'Sanity: BEFORE matches alt3k_sigmoid on {match_count}/{len(before_vec)} constraints')

    print('\n--- Apply cut manually, then re-run this script with --after ---')
    print('--- OR run: python3 diff_cut_proof.py --after ---')

    if '--after' in sys.argv:
        # Step C: Run AFTER baseline
        ok = run_export(after_path, 'AFTER')
        if not ok:
            print('AFTER run failed — aborting.')
            sys.exit(1)

        after_vec = extract_vector(after_path)
        sheaves_after   = sum(1 for v in after_vec.values() if v == 'sheaf')
        presheaves_after = sum(1 for v in after_vec.values() if v == 'presheaf')
        print(f'AFTER:  N={len(after_vec)}, sheaves={sheaves_after}, presheaves={presheaves_after}')

        green = diff_vectors(before_vec, after_vec, 'BEFORE', 'AFTER')
        sys.exit(0 if green else 1)
