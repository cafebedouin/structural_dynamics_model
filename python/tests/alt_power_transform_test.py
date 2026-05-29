"""
Alternative Power Transformation Test
======================================
Tests whether the sheaf/presheaf boundary is an invariant of the
institutional sign-flip (f(d_institutional) < 0) rather than the
sigmoid function specifically.

For each of 6 alternative monotone functions (plus sigmoid baseline),
runs full product-site cohomology on all ~3,301 constraints × 156 contexts,
then computes Jaccard similarity of presheaf sets vs sigmoid baseline.

Runtime: ~9 minutes (7 swipl runs × ~80s each).

Usage (from repo root):
    python3 python/alt_power_transform_test.py
"""

import json
import os
import subprocess
import sys
import tempfile
import time

PROLOG_DIR = os.path.join(os.path.dirname(__file__), '..', '..', 'prolog')
OUTPUT_DIR = os.path.join(os.path.dirname(__file__), '..', '..', 'outputs')
RESULTS_PATH = os.path.join(os.path.dirname(__file__), '..', 'alt_power_transform_results.json')

# All variants in order: baseline first, then the 6 alternatives.
# sign_flip=True means f(institutional_d=0.0) < 0.
VARIANTS = [
    ('sigmoid',           True,  'baseline sigmoid'),
    ('piecewise_linear',  True,  'piecewise linear, sign-flip preserved'),
    ('piecewise_no_flip', False, 'piecewise linear, NO sign-flip (CONTROL)'),
    ('sqrt_flip',         True,  'square root / concave, sign-flip preserved'),
    ('quadratic_flip',    True,  'quadratic / convex, sign-flip preserved'),
    ('step_flip',         True,  'three-level step, sign-flip preserved'),
    ('sigmoid_shifted',   False, 'sigmoid d0=0.25, NO institutional sign-flip'),
]

PROLOG_OVERLAY_TEMPLATE = """\
:- use_module(config).
:- ( retract(config:param(power_function, _)) -> true ; true ),
   asserta(config:param(power_function, {variant})).
:- [stack].
:- [product_site_export].
:- product_site_export:run_product_export_to('{outpath}'), halt.
"""


def run_variant(variant_name: str, out_json: str) -> bool:
    """Run product-site export for one variant. Returns True on success."""
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
        )
        elapsed = time.time() - t0
        if result.returncode != 0:
            print(f'  ERROR (exit {result.returncode}) in {elapsed:.0f}s', file=sys.stderr)
            print(result.stderr[-2000:], file=sys.stderr)
            return False
        print(f'  Done in {elapsed:.0f}s', file=sys.stderr)
        return True
    finally:
        os.unlink(overlay_path)


def parse_orbits(json_path: str) -> dict:
    """Parse product_site_orbits JSON. Returns {constraint_id: {'h0': int, 'h1': int}}."""
    with open(json_path) as f:
        return json.load(f)


def presheaf_set(orbits: dict) -> set:
    """Return set of constraint IDs where H¹ > 0 (presheaves)."""
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
        f"{'Variant':<22} | {'Sign-flip':<9} | {'Sheaves':>7} | {'Presheaves':>10} | "
        f"{'S→P':>5} | {'P→S':>5} | {'Jaccard':>7}"
    )
    sep = '-' * len(header)
    print(header)
    print(sep)

    for variant_name, sign_flip, description in VARIANTS:
        out_json = os.path.abspath(
            os.path.join(OUTPUT_DIR, f'alt_{variant_name}.json')
        )

        # Use existing baseline if already computed
        if variant_name == 'sigmoid' and os.path.exists(
            os.path.join(OUTPUT_DIR, 'product_site_orbits.json')
        ):
            out_json = os.path.abspath(
                os.path.join(OUTPUT_DIR, 'product_site_orbits.json')
            )
            print(f'  [{variant_name}] Using existing baseline', file=sys.stderr)
            success = True
        else:
            print(f'  [{variant_name}] Running...', file=sys.stderr)
            success = run_variant(variant_name, out_json)

        if not success or not os.path.exists(out_json):
            print(f'  [{variant_name}] SKIPPED (run failed)', file=sys.stderr)
            continue

        orbits = parse_orbits(out_json)
        total = len(orbits)
        presheaves = presheaf_set(orbits)
        sheaves = total - len(presheaves)

        if baseline_presheaves is None:
            # This is the sigmoid baseline
            baseline_presheaves = presheaves
            s_to_p = 0
            p_to_s = 0
            jac = 1.0
        else:
            s_to_p = len(presheaves - baseline_presheaves)   # sigmoid-sheaf → alt-presheaf
            p_to_s = len(baseline_presheaves - presheaves)   # sigmoid-presheaf → alt-sheaf
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
            f"{variant_name:<22} | {flip_str:<9} | {sheaves:>7} | {len(presheaves):>10} | "
            f"{s_to_p_str:>5} | {p_to_s_str:>5} | {jac_dash:>7}"
        )

    print(sep)

    # Write full results
    with open(RESULTS_PATH, 'w') as f:
        json.dump(results, f, indent=2)
    print(f'\nFull results written to {RESULTS_PATH}')

    # Summary interpretation
    if results:
        print('\n--- Interpretation ---')
        sign_flip_variants = [k for k, v in results.items() if v['sign_flip'] and k != 'sigmoid']
        no_flip_variants   = [k for k, v in results.items() if not v['sign_flip']]

        if sign_flip_variants:
            avg_jac_flip = sum(results[k]['jaccard_vs_sigmoid'] for k in sign_flip_variants) / len(sign_flip_variants)
            print(f'Sign-flip-preserving variants: avg Jaccard = {avg_jac_flip:.3f}')
        if no_flip_variants:
            avg_jac_noflip = sum(results[k]['jaccard_vs_sigmoid'] for k in no_flip_variants) / len(no_flip_variants)
            print(f'Sign-flip-removing variants:   avg Jaccard = {avg_jac_noflip:.3f}')

        if sign_flip_variants and no_flip_variants:
            if avg_jac_flip > 0.90 and avg_jac_noflip < 0.10:
                print(
                    '\nCONCLUSION: Sheaf/presheaf boundary is an invariant of the sign-flip.\n'
                    'Any monotone function with f(institutional_d) < 0 produces the same\n'
                    'presheaf structure for >90% of the corpus.'
                )
            elif avg_jac_flip > 0.80:
                print(
                    '\nCONCLUSION: Sign-flip is the primary driver (Jaccard >0.80 preserved),\n'
                    'but sigmoid shape contributes at the margin.'
                )
            else:
                print(
                    '\nCONCLUSION: Sigmoid shape matters — sign-flip alone is insufficient\n'
                    'to reproduce presheaf structure (Jaccard <0.80 for sign-flip variants).'
                )


if __name__ == '__main__':
    main()
