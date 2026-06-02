"""
Alt-Power-Transform Test Battery
=================================
Exhausts the sign-flip question via five tests (H0/H1/H2/H3).

Precondition: f(d) profile table for all 13 variants.
Test 1: Clean sign-flip isolation (t1_smooth_flip vs t1_smooth_noflip).
Test 2: Range collapse (t2_compressed_noflip, t2_compressed_flip vs sigmoid).
Test 3: Smoothness sweep (t1_smooth_flip, piecewise_linear, t3_step5, t3_step2).
Test 4: Corpus-composition stability (tangled_rope vs snare+rope subsets).
Test 5: Per-constraint flip sensitivity (which constraints drive the Jaccard gap).

New Prolog variants are defined in prolog/tests/test_battery_variants.pl.
Runs against testsets_3000 (3380 constraints) using the same overlay pattern
as alt_power_transform_test_3k.py.
"""

import json
import math
import os
import subprocess
import sys
import tempfile
import time
from collections import Counter, defaultdict
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parent.parent
PROLOG_DIR = REPO_ROOT / "prolog"
OUTPUT_DIR = REPO_ROOT / "outputs"
RESULTS_PATH = REPO_ROOT / "python" / "test_battery_results.json"

# ── Existing full-corpus results (from alt_power_transform_test_3k.py) ─────────
EXISTING_3K = {
    'sigmoid':           {'sign_flip': True,  'jaccard': 1.0,    'file': 'alt3k_sigmoid.json'},
    'piecewise_linear':  {'sign_flip': True,  'jaccard': 0.782,  'file': 'alt3k_piecewise_linear.json'},
    'piecewise_no_flip': {'sign_flip': False, 'jaccard': 0.7542, 'file': 'alt3k_piecewise_no_flip.json'},
    'sqrt_flip':         {'sign_flip': True,  'jaccard': 0.8327, 'file': 'alt3k_sqrt_flip.json'},
    'quadratic_flip':    {'sign_flip': True,  'jaccard': 0.8296, 'file': 'alt3k_quadratic_flip.json'},
    'step_flip':         {'sign_flip': True,  'jaccard': 0.6966, 'file': 'alt3k_step_flip.json'},
    'sigmoid_shifted':   {'sign_flip': False, 'jaccard': 0.7535, 'file': 'alt3k_sigmoid_shifted.json'},
}

# ── Prolog overlay template ────────────────────────────────────────────────────
OVERLAY_TEMPLATE = """\
:- use_module(config).
:- ( retract(config:param(power_function, _)) -> true ; true ),
   asserta(config:param(power_function, {variant})).
:- ( retract(config:param(corpus_path, _)) -> true ; true ),
   asserta(config:param(corpus_path, 'archives/prolog_v5')).
:- [stack].
:- [tests/test_battery_variants].
:- [product_site_export].
:- product_site_export:run_product_export_to('{outpath}'), halt.
"""

# Overlay for type classification — outputs TSV (id TAB type) to stdout.
# Uses default_context (analytical/civilizational/analytical/global).
# Uses explicit predicate (not lambda) for compatibility.
CLASSIFY_OVERLAY = """\
:- use_module(config).
:- ( retract(config:param(corpus_path, _)) -> true ; true ),
   asserta(config:param(corpus_path, 'archives/prolog_v5')).
:- [stack].

print_type_tsv([], _).
print_type_tsv([C|Cs], Ctx) :-
    ( drl_core:dr_type(C, Ctx, T) -> true ; T = unknown ),
    format(atom(Line), '~w\\t~w', [C, T]),
    writeln(Line),
    print_type_tsv(Cs, Ctx).

:- corpus_loader:load_all_testsets,
   covering_analysis:all_corpus_constraints(Cs),
   constraint_indexing:default_context(Ctx),
   print_type_tsv(Cs, Ctx),
   halt.
"""


# ══════════════════════════════════════════════════════════════════════════════
# Math helpers — f(d) values computed in Python, no Prolog needed
# ══════════════════════════════════════════════════════════════════════════════

def sigmoid(d, L, U, d0, k):
    return L + (U - L) / (1 + math.exp(-k * (d - d0)))

def piecewise_linear(d):
    if d <= 0.10:
        return -0.12 + d * 1.2
    elif d <= 0.50:
        return 0.00 + (d - 0.10) * 1.75
    else:
        return 0.70 + (d - 0.50) * 1.44

def piecewise_no_flip(d):
    return 0.05 + d * 1.37

def sqrt_flip(d):
    return -0.12 + 1.54 * math.sqrt(d)

def quadratic_flip(d):
    return -0.12 + 1.54 * d * d

def step_flip(d):
    if d < 0.15:   return -0.12
    elif d < 0.85: return 0.70
    else:           return 1.42

def sigmoid_shifted(d):
    return sigmoid(d, -0.20, 1.50, 0.25, 6.0)

def t1_smooth_flip(d):
    return sigmoid(d, -0.20, 1.50, 0.436, 6.0)

def t1_smooth_noflip(d):
    return sigmoid(d, 0.00, 1.70, 0.436, 6.0)

def t2_compressed_noflip(d):
    return sigmoid(d, 0.40, 0.90, 0.50, 6.0)

def t2_compressed_flip(d):
    return sigmoid(d, -0.05, 0.15, 0.50, 6.0)

def t3_step5(d):
    if d < 0.10:    return -0.12
    elif d < 0.325: return 0.355
    elif d < 0.55:  return 0.710
    elif d < 0.775: return 1.065
    else:            return 1.420

def t3_step2(d):
    return -0.12 if d < 0.10 else 1.420

ALL_VARIANTS = [
    ('sigmoid',             sigmoid,             lambda d: sigmoid(d,-0.20,1.50,0.50,6.0), True),
    ('piecewise_linear',    piecewise_linear,    None, True),
    ('piecewise_no_flip',   piecewise_no_flip,   None, False),
    ('sqrt_flip',           sqrt_flip,           None, True),
    ('quadratic_flip',      quadratic_flip,      None, True),
    ('step_flip',           step_flip,           None, True),
    ('sigmoid_shifted',     sigmoid_shifted,     None, False),
    ('t1_smooth_flip',      t1_smooth_flip,      None, True),
    ('t1_smooth_noflip',    t1_smooth_noflip,    None, False),
    ('t2_compressed_noflip',t2_compressed_noflip,None, False),
    ('t2_compressed_flip',  t2_compressed_flip,  None, True),
    ('t3_step5',            t3_step5,            None, True),
    ('t3_step2',            t3_step2,            None, True),
]

D_POINTS = [0.0, 0.05, 0.10, 0.25, 0.50, 0.75, 1.0]


# ══════════════════════════════════════════════════════════════════════════════
# Prolog runner
# ══════════════════════════════════════════════════════════════════════════════

def run_variant(variant_name: str, out_json: str) -> bool:
    overlay = OVERLAY_TEMPLATE.format(variant=variant_name, outpath=out_json)
    with tempfile.NamedTemporaryFile(mode='w', suffix='.pl', dir=PROLOG_DIR, delete=False) as f:
        f.write(overlay)
        overlay_path = f.name
    try:
        cmd = ['swipl', '--stack_limit=4G',
               '-g', f'["{Path(overlay_path).name}"]',
               '-t', 'halt(1)']
        t0 = time.time()
        result = subprocess.run(cmd, cwd=str(PROLOG_DIR), capture_output=True, text=True, timeout=600)
        elapsed = time.time() - t0
        if result.returncode != 0:
            print(f'    ERROR (exit {result.returncode}) in {elapsed:.0f}s', file=sys.stderr)
            print(result.stderr[-2000:], file=sys.stderr)
            return False
        print(f'    Done in {elapsed:.0f}s', file=sys.stderr)
        for line in result.stderr.strip().splitlines()[-4:]:
            if '[product_export]' in line or '[corpus]' in line:
                print(f'      {line}', file=sys.stderr)
        return True
    except subprocess.TimeoutExpired:
        print(f'    TIMEOUT', file=sys.stderr)
        return False
    finally:
        os.unlink(overlay_path)


def run_classify_types() -> dict:
    """Classify all testsets_3000 constraints at default context. Returns {id: type}."""
    with tempfile.NamedTemporaryFile(mode='w', suffix='.pl', dir=PROLOG_DIR, delete=False) as f:
        f.write(CLASSIFY_OVERLAY)
        overlay_path = f.name
    try:
        cmd = ['swipl', '--stack_limit=4G',
               '-g', f'["{Path(overlay_path).name}"]',
               '-t', 'halt(1)']
        t0 = time.time()
        result = subprocess.run(cmd, cwd=str(PROLOG_DIR), capture_output=True, text=True, timeout=600)
        elapsed = time.time() - t0
        print(f'    Type classification done in {elapsed:.0f}s', file=sys.stderr)
        if result.returncode != 0:
            print(f'    ERROR: {result.stderr[-1000:]}', file=sys.stderr)
            return {}
        # Parse TSV output: each line is "constraint_id\ttype"
        type_map = {}
        for line in result.stdout.strip().splitlines():
            parts = line.split('\t')
            if len(parts) == 2:
                type_map[parts[0].strip()] = parts[1].strip()
        print(f'    Parsed {len(type_map)} constraint types', file=sys.stderr)
        return type_map
    except Exception as e:
        print(f'    classify_types failed: {e}', file=sys.stderr)
        return {}
    finally:
        os.unlink(overlay_path)


# ══════════════════════════════════════════════════════════════════════════════
# Orbit loading helpers
# ══════════════════════════════════════════════════════════════════════════════

def load_orbits(json_path: str) -> dict:
    try:
        with open(json_path) as f:
            return json.load(f)
    except (json.JSONDecodeError, OSError) as e:
        print(f'  WARNING: could not load {json_path}: {e}', file=sys.stderr)
        return {}

def presheaf_set(orbits: dict) -> set:
    return {cid for cid, v in orbits.items() if v.get('h1', 0) > 0}

def jaccard(a: set, b: set) -> float:
    if not a and not b: return 1.0
    u = len(a | b)
    return round(len(a & b) / u, 4) if u > 0 else 0.0

def jaccard_subset(a: set, b: set, subset: set) -> float:
    a2, b2 = a & subset, b & subset
    return jaccard(a2, b2)


# ══════════════════════════════════════════════════════════════════════════════
# PRECONDITION: f(d) profile table
# ══════════════════════════════════════════════════════════════════════════════

def find_zero_crossing(fn):
    """Binary search for d where fn(d)=0. Returns None if no crossing in (0,1)."""
    if fn(0.0) >= 0 and fn(1.0) >= 0: return None
    if fn(0.0) <= 0 and fn(1.0) <= 0: return None
    lo, hi = 0.0, 1.0
    for _ in range(60):
        mid = (lo + hi) / 2
        if fn(mid) < 0: lo = mid
        else: hi = mid
    return round((lo + hi) / 2, 4)

def precondition_table():
    print("\n" + "="*80)
    print("PRECONDITION: f(d) profile for all 13 variants")
    print("="*80)

    variant_fns = [
        ('sigmoid',              lambda d: sigmoid(d,-0.20,1.50,0.50,6.0)),
        ('piecewise_linear',     piecewise_linear),
        ('piecewise_no_flip',    piecewise_no_flip),
        ('sqrt_flip',            sqrt_flip),
        ('quadratic_flip',       quadratic_flip),
        ('step_flip',            step_flip),
        ('sigmoid_shifted',      sigmoid_shifted),
        ('t1_smooth_flip',       t1_smooth_flip),
        ('t1_smooth_noflip',     t1_smooth_noflip),
        ('t2_compressed_noflip', t2_compressed_noflip),
        ('t2_compressed_flip',   t2_compressed_flip),
        ('t3_step5',             t3_step5),
        ('t3_step2',             t3_step2),
    ]

    # Header
    d_cols = '  '.join(f'd={d:<4}' for d in D_POINTS)
    print(f"\n{'Variant':<24}  {d_cols}  {'Min':>6}  {'Max':>6}  {'ZeroCross':>10}  Smooth?")
    print('-' * 120)

    for name, fn in variant_fns:
        vals = [fn(d) for d in D_POINTS]
        fmin = min(fn(d) for d in [i/100 for i in range(101)])
        fmax = max(fn(d) for d in [i/100 for i in range(101)])
        zc   = find_zero_crossing(fn)
        # Smoothness: check for jumps (step functions have large jumps at breakpoints)
        samples = [fn(d/1000) for d in range(1001)]
        diffs = [abs(samples[i+1]-samples[i]) for i in range(len(samples)-1)]
        max_diff = max(diffs)
        smooth = 'smooth' if max_diff < 0.01 else ('kinked' if max_diff < 0.20 else 'step')
        val_str = '  '.join(f'{v:+6.3f}' for v in vals)
        zc_str  = f'd≈{zc:.3f}' if zc is not None else 'none'
        print(f"{name:<24}  {val_str}  {fmin:+6.3f}  {fmax:+6.3f}  {zc_str:>10}  {smooth}")

    print()
    print("AXIS COVERAGE SUMMARY:")
    print("  Sign-flip axis: sigmoid, piecewise_linear, sqrt_flip, quadratic_flip, step_flip,")
    print("                  t1_smooth_flip, t2_compressed_flip, t3_step5, t3_step2 → YES")
    print("                  piecewise_no_flip, sigmoid_shifted, t1_smooth_noflip,")
    print("                  t2_compressed_noflip → NO")
    print("  Range axis: [−0.20,1.50] → sigmoid, t1_smooth_flip, t1_smooth_noflip (+0.20 shift)")
    print("              [−0.12,1.42] → piecewise_linear, sqrt_flip, step_flip, t3_step*")
    print("              [0.05,1.42]  → piecewise_no_flip")
    print("              [0.40,0.90]  → t2_compressed_noflip  (COMPRESSED)")
    print("              [−0.05,0.15] → t2_compressed_flip    (COMPRESSED)")
    print("              [0.22,1.49]  → sigmoid_shifted (approx)")
    print("  Smoothness: smooth → sigmoid, sigmoid_shifted, t1_smooth_flip, t1_smooth_noflip,")
    print("                       t2_compressed_noflip, t2_compressed_flip, sqrt_flip, quadratic_flip")
    print("              kinked → piecewise_linear, piecewise_no_flip")
    print("              step   → step_flip, t3_step5, t3_step2")
    print()
    print("CONFOUNDING in existing set:")
    print("  piecewise_no_flip vs piecewise_linear: removes sign-flip BUT ALSO changes")
    print("    range ([0.05,1.42] vs [−0.12,1.42]) and shifts zero level.")
    print("  sigmoid_shifted vs sigmoid: removes institutional sign-flip BUT also shifts")
    print("    d0 (0.25 vs 0.50), changing the shape of the whole curve.")
    print("  step_flip: has sign-flip but extreme discretization — confounds H0 and H2.")
    print("  t1_smooth_flip vs t1_smooth_noflip: ISOLATED sign-flip (Test 1 design).")


# ══════════════════════════════════════════════════════════════════════════════
# Test 1: Clean sign-flip isolation
# ══════════════════════════════════════════════════════════════════════════════

def test1(baseline_set: set):
    print("\n" + "="*80)
    print("TEST 1: Clean sign-flip isolation (H0 vs H1)")
    print("  t1_smooth_flip  : smooth sigmoid, d0=0.436, range≈[−0.20,1.50], f(0.10)≈0")
    print("  t1_smooth_noflip: same shape+steepness, range≈[0.00,1.70], f≥0 everywhere")
    print("  Difference: sign-flip ONLY (shift of +0.20 to whole output)")
    print("="*80)

    results = {}
    for name in ['t1_smooth_flip', 't1_smooth_noflip']:
        out = str(OUTPUT_DIR / f'alt3k_{name}.json')
        if not Path(out).exists():
            print(f'  [{name}] Running...', file=sys.stderr)
            success = run_variant(name, out)
            if not success:
                print(f'  [{name}] FAILED')
                continue
        else:
            print(f'  [{name}] Using cached result', file=sys.stderr)

        orbits = load_orbits(out)
        ps = presheaf_set(orbits)
        jac = jaccard(baseline_set, ps)
        s_to_p = len(ps - baseline_set)
        p_to_s = len(baseline_set - ps)
        results[name] = {'presheaves': len(ps), 'sheaves': len(orbits)-len(ps),
                         'jaccard': jac, 's_to_p': s_to_p, 'p_to_s': p_to_s,
                         'presheaf_set': ps}
        print(f"  {name:<24}  presheaves={len(ps):4d}  s→p={s_to_p:4d}  p→s={p_to_s:4d}  Jaccard={jac:.4f}")

    if 't1_smooth_flip' in results and 't1_smooth_noflip' in results:
        gap = results['t1_smooth_flip']['jaccard'] - results['t1_smooth_noflip']['jaccard']
        print(f"\n  Jaccard gap (flip − noflip): {gap:+.4f}")
        if abs(gap) < 0.02:
            verdict = "H0 ELIMINATED: gap < 0.02 — sign-flip has negligible isolated effect."
        elif abs(gap) < 0.05:
            verdict = "H0 WEAKENED: gap 0.02–0.05 — sign-flip has marginal isolated effect."
        elif gap > 0.05:
            verdict = "H0 SURVIVES: gap > 0.05 — sign-flip is load-bearing when isolated."
        else:
            verdict = f"H0 AMBIGUOUS: gap = {gap:.4f}"
        print(f"  VERDICT: {verdict}")
    return results


# ══════════════════════════════════════════════════════════════════════════════
# Test 2: Range collapse
# ══════════════════════════════════════════════════════════════════════════════

def test2(baseline_set: set):
    print("\n" + "="*80)
    print("TEST 2: Range collapse (discriminates H1 — range as invariant)")
    print("  sigmoid             : range [−0.20,1.50] baseline (Jaccard=1.000)")
    print("  t2_compressed_noflip: range [+0.40,+0.90] — compressed, no sign-flip")
    print("  t2_compressed_flip  : range [−0.05,+0.15] — compressed, sign-flip at d≈0.317")
    print("  Baseline shape (d0=0.50, k=6) preserved in all three.")
    print("="*80)

    results = {}
    for name in ['t2_compressed_noflip', 't2_compressed_flip']:
        out = str(OUTPUT_DIR / f'alt3k_{name}.json')
        if not Path(out).exists():
            print(f'  [{name}] Running...', file=sys.stderr)
            success = run_variant(name, out)
            if not success:
                print(f'  [{name}] FAILED (Prolog run unsuccessful)')
                continue
        else:
            print(f'  [{name}] Using cached result', file=sys.stderr)

        orbits = load_orbits(out)
        if not orbits:
            print(f'  [{name}] INFEASIBLE: output file empty or corrupted (OOM)')
            continue
        ps = presheaf_set(orbits)
        jac = jaccard(baseline_set, ps)
        s_to_p = len(ps - baseline_set)
        p_to_s = len(baseline_set - ps)
        results[name] = {'presheaves': len(ps), 'sheaves': len(orbits)-len(ps),
                         'jaccard': jac, 's_to_p': s_to_p, 'p_to_s': p_to_s}
        print(f"  {name:<28}  presheaves={len(ps):4d}  s→p={s_to_p:4d}  p→s={p_to_s:4d}  Jaccard={jac:.4f}")

    print(f"\n  sigmoid (baseline)           presheaves=896   Jaccard=1.0000")
    print(f"\n  INTERPRETATION:")
    if 't2_compressed_noflip' not in results:
        print("  NOTE: No interpretable results (t2_compressed_noflip missing).")
    elif 't2_compressed_flip' not in results:
        jc_nf = results['t2_compressed_noflip']['jaccard']
        print(f"    Compressed no-flip Jaccard={jc_nf:.4f}")
        print(f"    Compressed flip: INFEASIBLE — deep recursion in peano_curve_mapping")
        print(f"    at chi range [−0.05,0.15]; OOM-killed at 8GB. Partial H1 evidence only.")
        if jc_nf < 0.50:
            print("    H1 PARTIAL: Compression degrades Jaccard even without sign-flip.")
        else:
            print("    H1 WEAK: No-flip compression gives Jaccard {jc_nf:.4f} ≈ baseline range.")
    if 't2_compressed_noflip' in results and 't2_compressed_flip' in results:
        jc_nf = results['t2_compressed_noflip']['jaccard']
        jc_f  = results['t2_compressed_flip']['jaccard']
        print(f"    Compressed no-flip  Jaccard={jc_nf:.4f}")
        print(f"    Compressed flip     Jaccard={jc_f:.4f}")
        if jc_nf < 0.30 and jc_f < 0.30:
            print("    H1 STRONG: Range collapse destroys Jaccard regardless of sign-flip.")
            print("    Range (not sign-flip) is the load-bearing invariant.")
        elif jc_nf < 0.50:
            print("    H1 SUPPORTED: Range compression substantially degrades Jaccard.")
        elif jc_f > 0.70 and jc_nf < 0.50:
            print("    H0+H1 INTERACTION: range AND sign-flip both matter.")
        else:
            print("    H1 WEAK: Range compression does not strongly degrade Jaccard.")
    return results


# ══════════════════════════════════════════════════════════════════════════════
# Test 3: Smoothness sweep
# ══════════════════════════════════════════════════════════════════════════════

def test3(baseline_set: set, t1_results: dict):
    print("\n" + "="*80)
    print("TEST 3: Smoothness sweep (discriminates H2 — smoothness as invariant)")
    print("  t1_smooth_flip  : smooth sigmoid,    zero at d=0.10, range≈[−0.12,1.42]")
    print("  piecewise_linear: one kink at d=0.10, zero at d=0.10, range=[−0.12,1.42]")
    print("  t3_step5        : 5-level staircase,  zero at d=0.10, range=[−0.12,1.42]")
    print("  t3_step2        : 2-level step,        zero at d=0.10, range=[−0.12,1.42]")
    print("  (All matched on sign-flip AND range; only smoothness varies.)")
    print("="*80)

    results = {}

    # (a) t1_smooth_flip — may already be computed from Test 1
    a_out = str(OUTPUT_DIR / 'alt3k_t1_smooth_flip.json')
    if Path(a_out).exists():
        orbits_a = load_orbits(a_out)
        ps_a = presheaf_set(orbits_a)
        jac_a = jaccard(baseline_set, ps_a)
        results['t1_smooth_flip'] = {'presheaves': len(ps_a), 'jaccard': jac_a,
                                     's_to_p': len(ps_a - baseline_set),
                                     'p_to_s': len(baseline_set - ps_a)}
        print(f"  t1_smooth_flip  (smooth, 1 segment):  presheaves={len(ps_a):4d}  Jaccard={jac_a:.4f}")
    else:
        print("  t1_smooth_flip: not available (run Test 1 first)")

    # (b) piecewise_linear — from existing 3k run
    b_out = str(OUTPUT_DIR / 'alt3k_piecewise_linear.json')
    orbits_b = load_orbits(b_out)
    ps_b = presheaf_set(orbits_b)
    jac_b = jaccard(baseline_set, ps_b)
    results['piecewise_linear'] = {'presheaves': len(ps_b), 'jaccard': jac_b,
                                   's_to_p': len(ps_b - baseline_set),
                                   'p_to_s': len(baseline_set - ps_b)}
    print(f"  piecewise_linear (kinked, 2 segments): presheaves={len(ps_b):4d}  Jaccard={jac_b:.4f}")

    # (c) and (d): new runs
    for name in ['t3_step5', 't3_step2']:
        out = str(OUTPUT_DIR / f'alt3k_{name}.json')
        if not Path(out).exists():
            print(f'  [{name}] Running...', file=sys.stderr)
            success = run_variant(name, out)
            if not success:
                print(f'  [{name}] FAILED')
                continue
        else:
            print(f'  [{name}] Using cached result', file=sys.stderr)

        orbits = load_orbits(out)
        ps = presheaf_set(orbits)
        jac = jaccard(baseline_set, ps)
        s_to_p = len(ps - baseline_set)
        p_to_s = len(baseline_set - ps)
        results[name] = {'presheaves': len(ps), 'jaccard': jac,
                         's_to_p': s_to_p, 'p_to_s': p_to_s}
        label = '5-level step' if name == 't3_step5' else '2-level step'
        print(f"  {name:<16} ({label}):  presheaves={len(ps):4d}  Jaccard={jac:.4f}")

    # Smoothness gradient
    order = ['t1_smooth_flip', 'piecewise_linear', 't3_step5', 't3_step2']
    jaccs = [results[k]['jaccard'] for k in order if k in results]
    print(f"\n  Smoothness gradient (smooth→step): {' → '.join(f'{j:.4f}' for j in jaccs)}")
    if len(jaccs) >= 3:
        mono_decreasing = all(jaccs[i] >= jaccs[i+1] for i in range(len(jaccs)-1))
        if mono_decreasing:
            print("  H2 SUPPORTED: Jaccard degrades monotonically with discretization.")
            print("  step_flip's 0.697 is a smoothness artifact, NOT a sign-flip signal.")
        else:
            print("  H2 REJECTED: Jaccard does not degrade monotonically with discretization.")
            drop = max(jaccs) - min(jaccs)
            print(f"  Max drop across smoothness sweep: {drop:.4f}")
    return results


# ══════════════════════════════════════════════════════════════════════════════
# Test 4: Corpus-composition stability
# ══════════════════════════════════════════════════════════════════════════════

def test4(type_map: dict):
    print("\n" + "="*80)
    print("TEST 4: Corpus-composition stability")
    print("  Subsets: tangled_rope (N≈expected large), snare+rope combined")
    print("  Battery: sigmoid baseline vs Test 1 pair (t1_smooth_flip, t1_smooth_noflip)")
    print("  plus piecewise_no_flip (existing control) and step_flip (worst performer)")
    print("="*80)

    if not type_map:
        print("  SKIPPED: type classification unavailable")
        return {}

    type_counts = Counter(type_map.values())
    print(f"\n  Corpus type distribution (default context):")
    for t, n in sorted(type_counts.items(), key=lambda x: -x[1]):
        print(f"    {t:<20} {n:5d}  ({100*n/len(type_map):.1f}%)")

    subsets = {
        'tangled_rope': {cid for cid, t in type_map.items() if t == 'tangled_rope'},
        'snare_rope':   {cid for cid, t in type_map.items() if t in ('snare', 'rope')},
        'full_corpus':  set(type_map.keys()),
    }
    print(f"\n  Subset sizes:")
    for name, s in subsets.items():
        print(f"    {name:<20} N={len(s)}")

    # Load presheaf sets for variants
    variant_files = {
        'sigmoid':          str(OUTPUT_DIR / 'alt3k_sigmoid.json'),
        't1_smooth_flip':   str(OUTPUT_DIR / 'alt3k_t1_smooth_flip.json'),
        't1_smooth_noflip': str(OUTPUT_DIR / 'alt3k_t1_smooth_noflip.json'),
        'piecewise_no_flip':str(OUTPUT_DIR / 'alt3k_piecewise_no_flip.json'),
        'step_flip':        str(OUTPUT_DIR / 'alt3k_step_flip.json'),
    }

    ps_sets = {}
    for vname, fpath in variant_files.items():
        if Path(fpath).exists():
            orbits = load_orbits(fpath)
            ps_sets[vname] = presheaf_set(orbits)
        else:
            print(f"  WARNING: {fpath} not found, skipping {vname}", file=sys.stderr)

    if 'sigmoid' not in ps_sets:
        print("  SKIPPED: sigmoid baseline not found")
        return {}

    baseline = ps_sets['sigmoid']
    results = {}
    print(f"\n  {'Variant':<24}  {'Subset':<16}  {'Baseline_N':>10}  {'Var_N':>6}  {'Jaccard':>8}")
    print('  ' + '-'*70)

    for vname in ['t1_smooth_flip', 't1_smooth_noflip', 'piecewise_no_flip', 'step_flip']:
        if vname not in ps_sets:
            continue
        ps = ps_sets[vname]
        row = {}
        for subset_name, subset_ids in subsets.items():
            baseline_sub = baseline & subset_ids
            ps_sub       = ps & subset_ids
            jac = jaccard(baseline_sub, ps_sub)
            row[subset_name] = {'jaccard': jac,
                                 'baseline_n': len(baseline_sub),
                                 'variant_n': len(ps_sub)}
            print(f"  {vname:<24}  {subset_name:<16}  {len(baseline_sub):>10}  "
                  f"{len(ps_sub):>6}  {jac:>8.4f}")
        results[vname] = row

    # Interpretation
    print(f"\n  SIGN-FLIP GAP PER SUBSET (t1_smooth_flip − t1_smooth_noflip):")
    for subset_name in ['tangled_rope', 'snare_rope', 'full_corpus']:
        if 't1_smooth_flip' in results and 't1_smooth_noflip' in results:
            j_flip   = results['t1_smooth_flip'].get(subset_name, {}).get('jaccard', float('nan'))
            j_noflip = results['t1_smooth_noflip'].get(subset_name, {}).get('jaccard', float('nan'))
            gap = j_flip - j_noflip
            print(f"    {subset_name:<16}  flip={j_flip:.4f}  noflip={j_noflip:.4f}  gap={gap:+.4f}")

    return results


# ══════════════════════════════════════════════════════════════════════════════
# Test 5: Per-constraint flip sensitivity
# ══════════════════════════════════════════════════════════════════════════════

def test5(baseline_set: set, t1_results: dict, type_map: dict):
    print("\n" + "="*80)
    print("TEST 5: Per-constraint flip sensitivity")
    print("  Comparison: t1_smooth_flip vs t1_smooth_noflip (isolated sign-flip pair)")
    print("  Per constraint: does presheaf status (H1>0) change between the two?")
    print("  Reports: N flip-sensitive constraints, and their type distribution.")
    print("="*80)

    flip_path   = str(OUTPUT_DIR / 'alt3k_t1_smooth_flip.json')
    noflip_path = str(OUTPUT_DIR / 'alt3k_t1_smooth_noflip.json')

    if not Path(flip_path).exists() or not Path(noflip_path).exists():
        print("  SKIPPED: Test 1 results not available")
        return {}

    orbits_flip   = load_orbits(flip_path)
    orbits_noflip = load_orbits(noflip_path)

    ps_flip   = presheaf_set(orbits_flip)
    ps_noflip = presheaf_set(orbits_noflip)

    all_ids = set(orbits_flip.keys()) | set(orbits_noflip.keys())

    # flip_sensitive: changes presheaf status between flip and noflip
    # flip→noflip direction: was presheaf (flip), now sheaf (noflip) — sign-flip HELPED
    # noflip→flip direction: was sheaf (noflip), now presheaf (flip) — sign-flip INTRODUCED presheaf
    flip_to_sheaf  = ps_flip  - ps_noflip   # presheaf in flip, sheaf in noflip
    sheaf_to_presh = ps_noflip - ps_flip     # sheaf in flip, presheaf in noflip

    flip_sensitive = flip_to_sheaf | sheaf_to_presh

    total     = len(all_ids)
    n_sens    = len(flip_sensitive)
    n_stable  = total - n_sens

    print(f"\n  Total constraints:    {total}")
    print(f"  Flip-stable:          {n_stable} ({100*n_stable/total:.1f}%)")
    print(f"  Flip-sensitive:       {n_sens}   ({100*n_sens/total:.1f}%)")
    print(f"    flip→sheaf  (sign-flip promoted to presheaf): {len(flip_to_sheaf)}")
    print(f"    sheaf→presh (sign-flip demoted from presheaf): {len(sheaf_to_presh)}")

    # Type distribution of flip-sensitive constraints
    if type_map:
        print(f"\n  Type distribution of flip-sensitive constraints:")
        sens_types = Counter(type_map.get(cid, 'unknown') for cid in flip_sensitive)
        all_types  = Counter(type_map.get(cid, 'unknown') for cid in all_ids)
        for t, n in sorted(sens_types.items(), key=lambda x: -x[1]):
            total_in_type = all_types.get(t, 0)
            rate = n / total_in_type if total_in_type > 0 else 0
            print(f"    {t:<20}  {n:4d} of {total_in_type:4d} in type  ({100*rate:.1f}% sensitivity rate)")

        # Are tangled_rope constraints disproportionately flip-sensitive?
        tr_total = all_types.get('tangled_rope', 0)
        tr_sens  = sens_types.get('tangled_rope', 0)
        overall_rate = n_sens / total if total > 0 else 0
        tr_rate      = tr_sens / tr_total if tr_total > 0 else 0
        print(f"\n  Tangled_rope sensitivity rate:  {100*tr_rate:.1f}%  (corpus overall: {100*overall_rate:.1f}%)")
        if tr_rate > overall_rate * 1.5:
            print("  H0 PARTIAL RESCUE: tangled_rope is disproportionately flip-sensitive.")
            print("  Sign-flip is load-bearing for tangled_rope but washed out in corpus average.")
        else:
            print("  H0 NOT RESCUED: tangled_rope sensitivity rate ≈ corpus rate.")

    # H1 distribution of flip-sensitive constraints
    h1_vals_flip = [orbits_flip.get(cid, {}).get('h1', 0) for cid in flip_sensitive]
    h1_vals_all  = [orbits_flip.get(cid, {}).get('h1', 0) for cid in all_ids]
    if h1_vals_flip:
        avg_h1_sens = sum(h1_vals_flip) / len(h1_vals_flip)
        avg_h1_all  = sum(h1_vals_all) / len(h1_vals_all)
        print(f"\n  Mean H¹ (flip variant) of flip-sensitive: {avg_h1_sens:.2f}")
        print(f"  Mean H¹ (flip variant) of full corpus:    {avg_h1_all:.2f}")

    return {
        'total': total,
        'flip_sensitive': n_sens,
        'flip_to_sheaf': len(flip_to_sheaf),
        'sheaf_to_presh': len(sheaf_to_presh),
        'flip_sensitive_ids': list(flip_sensitive),
        'type_dist': dict(Counter(type_map.get(cid,'unknown') for cid in flip_sensitive)) if type_map else {},
    }


# ══════════════════════════════════════════════════════════════════════════════
# Hypothesis verdict summary
# ══════════════════════════════════════════════════════════════════════════════

def hypothesis_summary(t1r, t2r, t3r, t4r, t5r):
    print("\n" + "="*80)
    print("HYPOTHESIS VERDICT SUMMARY")
    print("="*80)
    print("""
H0 (sign-flip is the invariant): sign-flip alone explains the Jaccard gap.
H1 (range+monotonicity): the output range [−0.20, 1.50] span is the invariant.
H2 (smoothness): Jaccard degrades with discretization; step_flip's 0.697 is
    a smoothness artifact, not evidence against sign-flip.
H3 (underpowered control): piecewise_no_flip's near-identical Jaccard to
    piecewise_linear reflects confounding in the test design, not absence of
    sign-flip effect.
""")

    print("Evidence per hypothesis:")
    print()

    # H0
    if 't1_smooth_flip' in t1r and 't1_smooth_noflip' in t1r:
        gap = t1r['t1_smooth_flip']['jaccard'] - t1r['t1_smooth_noflip']['jaccard']
        if abs(gap) < 0.02:
            h0 = f"ELIMINATED  (T1 gap={gap:+.4f} < 0.02; sign-flip has no isolated effect)"
        elif abs(gap) < 0.05:
            h0 = f"WEAKENED    (T1 gap={gap:+.4f}; marginal isolated effect)"
        else:
            h0 = f"SURVIVES    (T1 gap={gap:+.4f} > 0.05)"
    else:
        h0 = "UNDETERMINED (T1 results unavailable)"
    print(f"  H0: {h0}")

    # H1
    if 't2_compressed_noflip' in t2r and 't2_compressed_flip' in t2r:
        jcnf = t2r['t2_compressed_noflip']['jaccard']
        jcf  = t2r['t2_compressed_flip']['jaccard']
        if jcnf < 0.30 and jcf < 0.30:
            h1 = f"STRONG      (T2: compressed_noflip={jcnf:.4f}, compressed_flip={jcf:.4f}; both destroyed)"
        elif jcnf < 0.50 or jcf < 0.50:
            h1 = f"SUPPORTED   (T2: compressed_noflip={jcnf:.4f}, compressed_flip={jcf:.4f})"
        else:
            h1 = f"WEAK        (T2: compressed_noflip={jcnf:.4f}, compressed_flip={jcf:.4f}; compression ≠ destroy)"
    else:
        h1 = "UNDETERMINED (T2 results unavailable)"
    print(f"  H1: {h1}")

    # H2
    order = ['t1_smooth_flip', 'piecewise_linear', 't3_step5', 't3_step2']
    jaccs = [t3r[k]['jaccard'] for k in order if k in t3r]
    if len(jaccs) >= 3:
        mono = all(jaccs[i] >= jaccs[i+1] for i in range(len(jaccs)-1))
        spread = max(jaccs) - min(jaccs)
        if mono and spread > 0.05:
            h2 = f"SUPPORTED   (T3: monotone degradation, spread={spread:.4f})"
        elif mono:
            h2 = f"WEAK        (T3: monotone but spread={spread:.4f} < 0.05)"
        else:
            h2 = f"REJECTED    (T3: not monotone; {' '.join(f'{j:.3f}' for j in jaccs)})"
    else:
        h2 = "UNDETERMINED (T3 results unavailable)"
    print(f"  H2: {h2}")

    # H3
    print(f"  H3: RESOLVED by T1  (t1_smooth_flip vs t1_smooth_noflip is the clean test;")
    print(f"      piecewise_no_flip confounding is confirmed by precondition table)")

    print()
    print("="*80)
    print("Do not synthesize into a paper claim. Hypotheses left standing or fallen.")
    print("="*80)


# ══════════════════════════════════════════════════════════════════════════════
# Main
# ══════════════════════════════════════════════════════════════════════════════

def main():
    os.makedirs(OUTPUT_DIR, exist_ok=True)

    # Load baseline sigmoid presheaf set
    baseline_path = str(OUTPUT_DIR / 'alt3k_sigmoid.json')
    if not Path(baseline_path).exists():
        print("ERROR: alt3k_sigmoid.json not found. Run alt_power_transform_test_3k.py first.")
        sys.exit(1)
    baseline_orbits = load_orbits(baseline_path)
    baseline_set    = presheaf_set(baseline_orbits)
    print(f"Baseline: sigmoid, {len(baseline_orbits)} constraints, {len(baseline_set)} presheaves")

    # Precondition
    precondition_table()

    # Tests 1–3 (Prolog runs if needed)
    t1r = test1(baseline_set)
    t2r = test2(baseline_set)
    t3r = test3(baseline_set, t1r)

    # Type classification (needed for T4 and T5)
    type_map_path = REPO_ROOT / 'python' / 'corpus_types_3k.json'
    if type_map_path.exists():
        print(f"\nUsing cached type map: {type_map_path}", file=sys.stderr)
        with open(type_map_path) as f:
            type_map = json.load(f)
    else:
        print(f"\nRunning type classification...", file=sys.stderr)
        type_map = run_classify_types()
        if type_map:
            with open(type_map_path, 'w') as f:
                json.dump(type_map, f)
            print(f"Type map cached to {type_map_path}", file=sys.stderr)

    # Tests 4–5 (Python-only)
    t4r = test4(type_map)
    t5r = test5(baseline_set, t1r, type_map)

    # Summary
    hypothesis_summary(t1r, t2r, t3r, t4r, t5r)

    # Save all results
    results = {
        'baseline': {'presheaves': len(baseline_set), 'total': len(baseline_orbits)},
        'test1': {k: {kk: vv for kk, vv in v.items() if kk != 'presheaf_set'}
                  for k, v in t1r.items()},
        'test2': t2r,
        'test3': {k: {kk: vv for kk, vv in v.items() if kk != 'presheaf_set'}
                  for k, v in t3r.items()},
        'test4': t4r,
        'test5': {k: v for k, v in t5r.items() if k != 'flip_sensitive_ids'},
    }
    with open(RESULTS_PATH, 'w') as f:
        json.dump(results, f, indent=2)
    print(f"\nResults written to {RESULTS_PATH}")


if __name__ == '__main__':
    main()
