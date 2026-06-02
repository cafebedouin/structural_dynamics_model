"""
Alternative Power Transformation Test — Full 3K Corpus
=======================================================
Identical to alt_power_transform_test.py but:
  1. Overrides corpus_path to 'archives/prolog_v5' (the ~3,300-constraint corpus).
  2. Does NOT use the existing product_site_orbits.json as the sigmoid baseline;
     always recomputes the sigmoid baseline against testsets_3000.
  3. Writes results to alt_power_transform_results_3k.json and
     outputs/alt3k_<variant>.json to avoid overwriting the 162-constraint run.

Purpose: reproduce the paper v5 §2.3 Jaccard claim (0.685–0.828) against the
full corpus.  Compare to the 162-constraint run stored in
alt_power_transform_results.json.
"""

import json
import os
import subprocess
import sys
import tempfile
import time

PROLOG_DIR = os.path.join(os.path.dirname(__file__), '..', '..', 'prolog')
OUTPUT_DIR = os.path.join(os.path.dirname(__file__), '..', '..', 'outputs')
RESULTS_PATH = os.path.join(os.path.dirname(__file__), '..', 'alt_power_transform_results_3k.json')

VARIANTS = [
    ('sigmoid',           True,  'baseline sigmoid'),
    ('piecewise_linear',  True,  'piecewise linear, sign-flip preserved'),
    ('piecewise_no_flip', False, 'piecewise linear, NO sign-flip (CONTROL)'),
    ('sqrt_flip',         True,  'square root / concave, sign-flip preserved'),
    ('quadratic_flip',    True,  'quadratic / convex, sign-flip preserved'),
    ('step_flip',         True,  'three-level step, sign-flip preserved'),
    ('sigmoid_shifted',   False, 'sigmoid d0=0.25, NO institutional sign-flip'),
]

# Override corpus_path to testsets_3000; always recompute sigmoid baseline.
PROLOG_OVERLAY_TEMPLATE = """\
:- use_module(config).
:- ( retract(config:param(power_function, _)) -> true ; true ),
   asserta(config:param(power_function, {variant})).
:- ( retract(config:param(corpus_path, _)) -> true ; true ),
   asserta(config:param(corpus_path, 'archives/prolog_v5')).
:- [stack].
:- [product_site_export].
:- product_site_export:run_product_export_to('{outpath}'), halt.
"""


def run_variant(variant_name: str, out_json: str) -> bool:
    overlay = PROLOG_OVERLAY_TEMPLATE.format(
        variant=variant_name,
        outpath=out_json,
    )
    with tempfile.NamedTemporaryFile(
        mode='w', suffix='.pl', dir=PROLOG_DIR, delete=False
    ) as f:
        f.write(overlay)
        overlay_path = f.name

    try:
        cmd = [
            'swipl',
            '--stack_limit=4G',
            '-g', f'["{os.path.basename(overlay_path)}"]',
            '-t', 'halt(1)',
        ]
        t0 = time.time()
        result = subprocess.run(
            cmd,
            cwd=PROLOG_DIR,
            capture_output=True,
            text=True,
            timeout=600,
        )
        elapsed = time.time() - t0
        if result.returncode != 0:
            print(f'  ERROR (exit {result.returncode}) in {elapsed:.0f}s', file=sys.stderr)
            print(result.stderr[-3000:], file=sys.stderr)
            return False
        print(f'  Done in {elapsed:.0f}s', file=sys.stderr)
        # Print swipl stderr (progress lines) to stderr
        if result.stderr:
            for line in result.stderr.strip().splitlines()[-5:]:
                print(f'    {line}', file=sys.stderr)
        return True
    except subprocess.TimeoutExpired:
        print(f'  TIMEOUT after 600s', file=sys.stderr)
        return False
    finally:
        os.unlink(overlay_path)


def parse_orbits(json_path: str) -> dict:
    with open(json_path) as f:
        return json.load(f)


def presheaf_set(orbits: dict) -> set:
    return {cid for cid, v in orbits.items() if v.get('h1', 0) > 0}


def jaccard(a: set, b: set) -> float:
    if not a and not b:
        return 1.0
    u = len(a | b)
    return len(a & b) / u if u > 0 else 0.0


def main():
    os.makedirs(OUTPUT_DIR, exist_ok=True)

    results = {}
    baseline_presheaves = None

    header = (
        f"{'Variant':<22} | {'Sign-flip':<9} | {'Total':>7} | {'Sheaves':>7} | "
        f"{'Presheaves':>10} | {'S→P':>5} | {'P→S':>5} | {'Jaccard':>7}"
    )
    sep = '-' * len(header)
    print(header)
    print(sep)

    for variant_name, sign_flip, description in VARIANTS:
        out_json = os.path.abspath(
            os.path.join(OUTPUT_DIR, f'alt3k_{variant_name}.json')
        )

        print(f'  [{variant_name}] Running against testsets_3000...', file=sys.stderr)
        success = run_variant(variant_name, out_json)

        if not success or not os.path.exists(out_json):
            print(f'  [{variant_name}] SKIPPED (run failed or no output)', file=sys.stderr)
            continue

        orbits = parse_orbits(out_json)
        total = len(orbits)
        presheaves = presheaf_set(orbits)
        sheaves = total - len(presheaves)

        if baseline_presheaves is None:
            baseline_presheaves = presheaves
            s_to_p = 0
            p_to_s = 0
            jac = 1.0
        else:
            s_to_p = len(presheaves - baseline_presheaves)
            p_to_s = len(baseline_presheaves - presheaves)
            jac = jaccard(baseline_presheaves, presheaves)

        results[variant_name] = {
            'description': description,
            'sign_flip': sign_flip,
            'total': total,
            'sheaves': sheaves,
            'presheaves': len(presheaves),
            's_to_p': s_to_p,
            'p_to_s': p_to_s,
            'jaccard_vs_sigmoid': round(jac, 4),
            'output_file': out_json,
        }

        flip_str = 'yes' if sign_flip else 'NO'
        jac_dash = '—' if variant_name == 'sigmoid' else f'{jac:.3f}'
        s_to_p_str = '—' if variant_name == 'sigmoid' else str(s_to_p)
        p_to_s_str = '—' if variant_name == 'sigmoid' else str(p_to_s)

        print(
            f"{variant_name:<22} | {flip_str:<9} | {total:>7} | {sheaves:>7} | "
            f"{len(presheaves):>10} | {s_to_p_str:>5} | {p_to_s_str:>5} | {jac_dash:>7}"
        )

    print(sep)

    with open(RESULTS_PATH, 'w') as f:
        json.dump(results, f, indent=2)
    print(f'\nFull results written to {RESULTS_PATH}')

    if not results:
        print('\nNo results — all variants failed.')
        return

    sign_flip_variants = [k for k, v in results.items() if v['sign_flip'] and k != 'sigmoid']
    no_flip_variants   = [k for k, v in results.items() if not v['sign_flip']]

    print('\n--- Interpretation (code logic) ---')
    if sign_flip_variants:
        avg_jac_flip = sum(results[k]['jaccard_vs_sigmoid'] for k in sign_flip_variants) / len(sign_flip_variants)
        print(f'Sign-flip-preserving variants ({", ".join(sign_flip_variants)}): avg Jaccard = {avg_jac_flip:.3f}')
    else:
        avg_jac_flip = 0.0
    if no_flip_variants:
        avg_jac_noflip = sum(results[k]['jaccard_vs_sigmoid'] for k in no_flip_variants) / len(no_flip_variants)
        print(f'Sign-flip-removing variants ({", ".join(no_flip_variants)}): avg Jaccard = {avg_jac_noflip:.3f}')
    else:
        avg_jac_noflip = 0.0

    if sign_flip_variants and no_flip_variants:
        if avg_jac_flip > 0.90 and avg_jac_noflip < 0.10:
            verdict = (
                'CONCLUSION: Sheaf/presheaf boundary is an invariant of the sign-flip.\n'
                'Any monotone function with f(institutional_d) < 0 produces the same\n'
                'presheaf structure for >90% of the corpus.'
            )
        elif avg_jac_flip > 0.80:
            verdict = (
                'CONCLUSION: Sign-flip is the primary driver (Jaccard >0.80 preserved),\n'
                'but sigmoid shape contributes at the margin.'
            )
        else:
            verdict = (
                'CONCLUSION: Sigmoid shape matters — sign-flip alone is insufficient\n'
                'to reproduce presheaf structure (Jaccard <0.80 for sign-flip variants).'
            )
        print(f'\n{verdict}')

    # Paper comparison
    paper_floor = 0.685
    paper_ceil  = 0.828
    print(f'\n--- Paper v5 §2.3 comparison (claimed Jaccard {paper_floor}–{paper_ceil}) ---')
    all_non_baseline = [k for k in results if k != 'sigmoid']
    for k in all_non_baseline:
        jac = results[k]['jaccard_vs_sigmoid']
        in_range = paper_floor <= jac <= paper_ceil
        marker = 'IN RANGE' if in_range else ('ABOVE' if jac > paper_ceil else 'BELOW')
        print(f'  {k:<22}  Jaccard={jac:.4f}  {marker}')
    if all_non_baseline:
        all_jac = [results[k]['jaccard_vs_sigmoid'] for k in all_non_baseline]
        print(f'  Observed range: {min(all_jac):.4f}–{max(all_jac):.4f}')
        print(f'  Paper claimed:  {paper_floor}–{paper_ceil}')


if __name__ == '__main__':
    main()
