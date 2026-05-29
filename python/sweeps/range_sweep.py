"""
Range sweep for T2-redo: isolates sign-flip vs range with Hub 1 live throughout.
A1 = sigmoid baseline (Jaccard=1.0000 by definition).
Runs A2, A3, B1, B2, B3 and produces the full 6-variant comparison table.
"""

import subprocess, json, sys, os, tempfile, time, math
from pathlib import Path

PROLOG_DIR = Path('/home/scott/bin/structural_dynamics_model/prolog')
OUT_DIR    = Path('/home/scott/bin/structural_dynamics_model/outputs')
BASELINE   = OUT_DIR / 'alt3k_sigmoid.json'

OVERLAY_TEMPLATE = """\
:- use_module(config).
:- ( retract(config:param(power_function, _)) -> true ; true ),
   asserta(config:param(power_function, {variant})).
:- ( retract(config:param(corpus_path, _)) -> true ; true ),
   asserta(config:param(corpus_path, 'testsets_3000')).
:- [stack].
:- [tests/test_battery_variants].
:- [product_site_export].
:- product_site_export:run_product_export_to('{outpath}'), halt.
"""

# Variant definitions: (label, prolog_name, L, U, sign_flip, span)
VARIANTS = [
    # A1 = sigmoid baseline, handled separately
    ('A2', 'range_a2', -0.20, 1.00, True,  1.20),
    ('A3', 'range_a3', -0.20, 0.65, True,  0.85),
    ('B1', 'range_b1',  0.02, 1.72, False, 1.70),
    ('B2', 'range_b2',  0.02, 1.22, False, 1.20),
    ('B3', 'range_b3',  0.02, 0.87, False, 0.85),
]


def sigmoid(d, L, U, d0=0.50, k=6.0):
    return L + (U - L) / (1 + math.exp(-k * (d - d0)))


def run_variant(prolog_name, out_json):
    overlay = OVERLAY_TEMPLATE.format(variant=prolog_name, outpath=str(out_json))
    with tempfile.NamedTemporaryFile(mode='w', suffix='.pl', dir=PROLOG_DIR, delete=False) as f:
        f.write(overlay)
        overlay_path = f.name
    try:
        cmd = ['swipl', '--stack_limit=4G',
               '-g', f'["{Path(overlay_path).name}"]',
               '-t', 'halt(1)']
        t0 = time.time()
        result = subprocess.run(cmd, cwd=str(PROLOG_DIR), capture_output=True,
                                text=True, timeout=600)
        elapsed = time.time() - t0
        if result.returncode != 0:
            print(f'    ERROR (exit {result.returncode}) in {elapsed:.0f}s', flush=True)
            print(result.stderr[-1500:], flush=True)
            return False
        for line in result.stderr.strip().splitlines():
            if '[product_export]' in line or '[corpus]' in line:
                print(f'      {line}')
        print(f'    Done in {elapsed:.1f}s', flush=True)
        return True
    except subprocess.TimeoutExpired:
        print('    TIMEOUT', flush=True)
        return False
    finally:
        os.unlink(overlay_path)


def load_presheaf_set(path):
    with open(path) as f:
        d = json.load(f)
    return {c for c, v in d.items() if isinstance(v, dict) and v.get('h0', 0) == 0}


def jaccard_stats(base_set, var_set):
    inter = len(base_set & var_set)
    union_ = len(base_set | var_set)
    jac = inter / union_ if union_ else 0.0
    sp = len(var_set - base_set)
    ps = len(base_set - var_set)
    return jac, inter, sp, ps


if __name__ == '__main__':
    print('=' * 72)
    print('RANGE SWEEP — T2 REDO')
    print('  Thresholds: rope_chi=0.35, TR_floor=0.40, TR_ceil=0.90, snare_chi=0.66')
    print('  d0=0.50, k=6.0 for all variants. A arm: sign-flip. B arm: no sign-flip.')
    print('=' * 72)

    # f(d) profile table
    print()
    print('f(d) PROFILES:')
    ds = [0.0, 0.25, 0.50, 0.75, 1.0]
    header = '%-28s  %6s %6s %6s %6s %6s  %6s %6s  flip  span   chi_range(eps=0.70)'
    print(header % ('Variant', 'd=0.0', 'd=0.25', 'd=0.50', 'd=0.75', 'd=1.0', 'L', 'U'))
    print('-' * 100)

    all_specs = [('A1 (sigmoid baseline)', 'sigmoid', -0.20, 1.50, True, 1.70)] + \
                [(f'{label} ({pn})', pn, L, U, flip, span) for label, pn, L, U, flip, span in VARIANTS]

    for name, pn, L, U, flip, span in all_specs:
        vals = [sigmoid(d, L, U) for d in ds]
        f_min = sigmoid(0.0, L, U)
        f_max = sigmoid(1.0, L, U)
        chi_min = f_min * 0.70
        chi_max = f_max * 0.70
        flip_str = 'YES' if flip else 'NO '
        SNARE = 0.66
        TR_FL = 0.40
        if chi_max >= SNARE:
            span_str = 'rope->TR->snare'
        elif chi_max * 1.2 >= SNARE:
            span_str = 'rope->TR->snare(glo)'
        elif chi_max >= TR_FL:
            span_str = 'rope->TR'
        else:
            span_str = '*** STARVED ***'
        row = '%-28s  ' % name[:28]
        row += '  '.join('%6.3f' % v for v in vals)
        row += '  %6.3f %6.3f  %s  %.2f   [%.3f,%.3f] %s' % (
            L, U, flip_str, span, chi_min, chi_max, span_str)
        print(row)
    print()

    # Load baseline
    base_set = load_presheaf_set(BASELINE)
    print(f'Sigmoid baseline: N_presheaves={len(base_set)}, Jaccard=1.0000')
    print()

    # Run variants
    results = {}
    for label, pn, L, U, flip, span in VARIANTS:
        out_path = OUT_DIR / f'alt3k_{pn}.json'
        if out_path.exists():
            try:
                var_set = load_presheaf_set(out_path)
                print(f'[{label} {pn}] Using cached result', flush=True)
                results[label] = (pn, L, U, flip, span, var_set)
                continue
            except Exception:
                print(f'[{label} {pn}] Cache invalid, re-running', flush=True)
                out_path.unlink()
        print(f'[{label} {pn}] Running...', flush=True)
        ok = run_variant(pn, out_path)
        if not ok:
            print(f'  FAILED — excluding from comparison')
            results[label] = None
            continue
        var_set = load_presheaf_set(out_path)
        results[label] = (pn, L, U, flip, span, var_set)

    # Results table
    print()
    print('=' * 72)
    print('JACCARD RESULTS (vs sigmoid baseline, 3380 constraints)')
    print('=' * 72)
    print()
    print('%-28s  %5s  %6s  %7s  %7s  %8s' % (
        'Variant', 'span', 'flip', 'N_presh', 's->p', 'p->s', 'Jaccard'))
    print('_' * 76)
    # Wait I need to redo this format
    print()

    # A arm
    print('ARM A (sign-flip preserved):')
    a1_jac = 1.0000
    a1_n   = len(base_set)
    print('  %-26s  span=1.70  N=%4d  s->p=  0  p->s=  0  Jaccard=1.0000  (baseline)' % ('A1 [−0.20,+1.50] sigmoid', a1_n))

    arm_a = [('A2', 'range_a2', -0.20, 1.00, 1.20), ('A3', 'range_a3', -0.20, 0.65, 0.85)]
    arm_b = [('B1', 'range_b1',  0.02, 1.72, 1.70), ('B2', 'range_b2',  0.02, 1.22, 1.20),
             ('B3', 'range_b3',  0.02, 0.87, 0.85)]

    arm_a_jacs = {1.70: 1.0000}
    for label, pn, L, U, span in arm_a:
        r = results.get(label)
        if r is None:
            print(f'  {label} [{L:+.2f},{U:+.2f}]  FAILED')
            continue
        _, _, _, _, _, var_set = r
        jac, inter, sp, ps = jaccard_stats(base_set, var_set)
        arm_a_jacs[span] = jac
        print('  %-26s  span=%.2f  N=%4d  s->p=%3d  p->s=%3d  Jaccard=%.4f' % (
            f'{label} [{L:+.2f},{U:+.2f}]', span, len(var_set), sp, ps, jac))

    print()
    print('ARM B (no sign-flip, floor=0.02):')
    arm_b_jacs = {}
    for label, pn, L, U, span in arm_b:
        r = results.get(label)
        if r is None:
            print(f'  {label} [{L:+.2f},{U:+.2f}]  FAILED')
            continue
        _, _, _, _, _, var_set = r
        jac, inter, sp, ps = jaccard_stats(base_set, var_set)
        arm_b_jacs[span] = jac
        print('  %-26s  span=%.2f  N=%4d  s->p=%3d  p->s=%3d  Jaccard=%.4f' % (
            f'{label} [{L:+.2f},{U:+.2f}]', span, len(var_set), sp, ps, jac))

    # A-vs-B gap table
    print()
    print('A-vs-B SIGN-FLIP GAPS (matched span):')
    print('  %-8s  %-10s  %-10s  %-10s' % ('span', 'Arm A (jac)', 'Arm B (jac)', 'gap A-B'))
    shared_spans = sorted(set(arm_a_jacs) & set(arm_b_jacs))
    for span in shared_spans:
        a = arm_a_jacs.get(span, float('nan'))
        b = arm_b_jacs.get(span, float('nan'))
        gap = a - b
        print('  %-8.2f  %-10.4f  %-10.4f  %+.4f' % (span, a, b, gap))

    # Span-variation within arms
    print()
    print('SPAN VARIATION WITHIN ARMS:')
    if len(arm_a_jacs) >= 2:
        spans_a = sorted(arm_a_jacs, reverse=True)
        print('  Arm A: ' + '  '.join('span=%.2f→%.4f' % (s, arm_a_jacs[s]) for s in spans_a))
        max_drop_a = max(arm_a_jacs.values()) - min(arm_a_jacs.values())
        print(f'  Arm A max drop: {max_drop_a:.4f}')
    if len(arm_b_jacs) >= 2:
        spans_b = sorted(arm_b_jacs, reverse=True)
        print('  Arm B: ' + '  '.join('span=%.2f→%.4f' % (s, arm_b_jacs[s]) for s in spans_b))
        max_drop_b = max(arm_b_jacs.values()) - min(arm_b_jacs.values())
        print(f'  Arm B max drop: {max_drop_b:.4f}')

    # Verdict
    print()
    print('=' * 72)
    print('H1 VERDICT LOGIC:')
    print('  Branch 1: span-variation large in both arms, A-vs-B gap small')
    print('            → range dominates → H1 CONFIRMED, H0 gap was range-proxy')
    print('  Branch 2: A-vs-B gap large at every span, span-variation small')
    print('            → sign-flip dominates → H1 ELIMINATED, H0 clean')
    print('  Branch 3: both contribute, neither dominates → H0 and H1 both partial')
    print()

    all_gaps = [arm_a_jacs.get(s, float('nan')) - arm_b_jacs.get(s, float('nan'))
                for s in shared_spans if s in arm_a_jacs and s in arm_b_jacs]
    all_drops_a = [arm_a_jacs[max(arm_a_jacs)] - arm_a_jacs[s] for s in arm_a_jacs]
    all_drops_b = [arm_b_jacs[max(arm_b_jacs)] - arm_b_jacs[s] for s in arm_b_jacs]
    mean_gap = sum(all_gaps) / len(all_gaps) if all_gaps else float('nan')
    max_drop_a = max(all_drops_a) if all_drops_a else float('nan')
    max_drop_b = max(all_drops_b) if all_drops_b else float('nan')

    print(f'  Mean A-vs-B gap: {mean_gap:+.4f}')
    print(f'  Max span-drop Arm A: {max_drop_a:.4f}')
    print(f'  Max span-drop Arm B: {max_drop_b:.4f}')

    # Write results
    results_out = {
        'arm_a_jacs': {str(k): v for k, v in arm_a_jacs.items()},
        'arm_b_jacs': {str(k): v for k, v in arm_b_jacs.items()},
        'mean_ab_gap': mean_gap,
        'max_span_drop_a': max_drop_a,
        'max_span_drop_b': max_drop_b,
    }
    with open(OUT_DIR / 'range_sweep_results.json', 'w') as f:
        json.dump(results_out, f, indent=2)
    print()
    print('Results written to outputs/range_sweep_results.json')
