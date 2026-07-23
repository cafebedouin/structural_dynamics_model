#!/usr/bin/env python3
"""OQ-232 positive-control probe: does the redesigned two-falsifier set discriminate
the worlds the old falsifier could not?

LOGICAL DEMONSTRATION, NEVER EMPIRICAL. Every quantity below is computed inside a
declared model. The chi->rating LINK FUNCTION is an INVENTED MODELING ASSUMPTION
(no engine predicate maps chi to a survey rating); all assertions therefore run
under TWO links (linear, threshold) and every link-dependent result is labeled.

Canonical constants (verified against source this session):
  sigmoid f(d) = L + (U-L)/(1+e^(-k(d-d0))), L=-0.20 U=1.50 d0=0.50 k=6.0
    (config.pl:130-133)
  chi = eps * f(d) * sigma(S); eps=0.70 declared, sigma=1.0 (national)
  d = BaseD(P) + ExitMod(E), clamped [0,1] (constraint_indexing.pl:470-472)
  BaseD, HasVictims=true row (constraint_indexing.pl:479-489):
    powerless .85, moderate .70, powerful .50, organized .45, institutional .15
  ExitMod (constraint_indexing.pl:494-499): trapped +0.05, mobile 0.00

Declared modeling choices (invented, not engine facts):
  - Adaptation suppresses the REPORT in chi units: report_chi = chi - s*index(P or E).
  - E-indexed shape g: g(trapped)=1, g(mobile)=0.
  - P-indexed shape h (PRIMARY, proportional): h(P) = BaseD(P) -- adaptation
    magnitude proportional to powerlessness, the shape under which the review-3
    cancellation concern (observed slope = axiom slope - adaptation slope) is exact.
    A CONCENTRATED shape (h=1 at powerless, else 0) is used only in the
    negative-branch positive control, labeled as such.
  - World B (axiom false): reports position-invariant at CHI_B = 0.20 (a mild,
    uniformly-experienced constraint). This is an EXISTENCE instantiation: the old
    observable's firing in B needs only that some axiom-false world produces it.
  - Links: linear = min-max over the attainable chi range [eps*L, eps*U], clamped;
    threshold = 1 if chi >= 0.5 else 0 (hard saturating link).
  - Instrument resolution: linear 7-point scale -> 1/6 of range; threshold ->
    full scale (declared). Verdicts computed on unquantized link output with
    "distinguishable" = at least one scale step.
  - Non-extractive report: rating <= 1/3 of scale (bottom third).
  - P-arm verdict: end-to-end rating diff, powerless minus institutional, at held
    E = mobile. positive if >= res, negative if <= -res, else flat.
  - E-arm (2x2): held P = moderate, cohorts trapped vs mobile. Cell 4 = both
    reports non-extractive.
  - Old observable: the powerless+trapped rater reports non-extractive.

Worlds:
  A: axiom true + E-indexed adaptation      report = chi - s*g(E)
  B: axiom false (position-invariant)       report = CHI_B
  C: axiom true + P-indexed adaptation      report = chi - s*h(P)
  D: world C at s ~= s_c (adaptation magnitude near the axiom's own P-slope):
     the cancellation world. Implemented as C evaluated at the computed s_c and
     as band membership during the sweep.

Assertions (fail loud) follow plan A.5 (i)-(v).
Output: printed AND written to discrimination_probe.log next to this file.
"""

import math
import os
import sys

# ---------------------------------------------------------------- constants
L, U, D0, K = -0.20, 1.50, 0.50, 6.0
EPS, SIG = 0.70, 1.0
BASED = {'powerless': 0.85, 'moderate': 0.70, 'powerful': 0.50,
         'organized': 0.45, 'institutional': 0.15}
EXITMOD = {'trapped': 0.05, 'mobile': 0.00}
P_ORDER = ['institutional', 'organized', 'powerful', 'moderate', 'powerless']

G = {'trapped': 1.0, 'mobile': 0.0}          # E-indexed adaptation shape
H = dict(BASED)                              # P-indexed shape, proportional (primary)
H_CONC = {p: (1.0 if p == 'powerless' else 0.0) for p in BASED}  # control only

CHI_B = 0.20                                 # axiom-false world's uniform chi
CHI_MIN, CHI_MAX = EPS * SIG * L, EPS * SIG * U
CUTOFF = 1.0 / 3.0                           # non-extractive: bottom third of scale
THETA = 0.5                                  # threshold-link cut
HELD_P_EARM = 'moderate'
S_GRID = [round(i * 0.005, 3) for i in range(0, 501)]   # s in [0, 2.5]

_lines = []


def out(msg=''):
    print(msg)
    _lines.append(msg)


def f(d):
    return L + (U - L) / (1.0 + math.exp(-K * (d - D0)))


def chi(p, e):
    d = max(0.0, min(1.0, BASED[p] + EXITMOD[e]))
    return EPS * f(d) * SIG


def link_linear(c):
    return max(0.0, min(1.0, (c - CHI_MIN) / (CHI_MAX - CHI_MIN)))


def link_threshold(c):
    return 1.0 if c >= THETA else 0.0


LINKS = {'linear': (link_linear, 1.0 / 6.0), 'threshold': (link_threshold, 1.0)}

# ---------------------------------------------------------------- worlds


def world_A(p, e, s):
    return chi(p, e) - s * G[e]


def world_B(p, e, s):
    return CHI_B


def world_C(p, e, s, h=None):
    return chi(p, e) - s * (h or H)[p]


# ---------------------------------------------------------------- observables


def obs_old(world, s, link):
    """Old falsifier's observable: powerless+trapped rater reports non-extractive."""
    return link(world('powerless', 'trapped', s)) <= CUTOFF + 1e-9


def obs_earm(world, s, link):
    """E-arm 2x2 Cell 4: at held P, trapped AND mobile cohorts both non-extractive."""
    return (link(world(HELD_P_EARM, 'trapped', s)) <= CUTOFF + 1e-9 and
            link(world(HELD_P_EARM, 'mobile', s)) <= CUTOFF + 1e-9)


def parm_verdict(world, s, link, res):
    """P-arm sign read: end-to-end rating diff (powerless - institutional), E held mobile."""
    diff = link(world('powerless', 'mobile', s)) - link(world('institutional', 'mobile', s))
    if diff >= res - 1e-9:
        return 'positive'
    if diff <= -(res - 1e-9):
        return 'negative'
    return 'flat'


def signature(world, s, link, res):
    return (obs_earm(world, s, link), parm_verdict(world, s, link, res))


def first_s(pred):
    for s in S_GRID:
        if pred(s):
            return s
    return None


def main():
    out('=' * 78)
    out('OQ-232 discrimination probe -- logical demonstration under declared links')
    out('=' * 78)

    out('\n--- Ground truth chi per position (E=mobile / E=trapped) ---')
    for p in reversed(P_ORDER):
        out(f'  {p:>13}: chi_mobile={chi(p,"mobile"):+.4f}  chi_trapped={chi(p,"trapped"):+.4f}')

    axiom_ee = chi('powerless', 'mobile') - chi('institutional', 'mobile')
    adapt_ee = H['powerless'] - H['institutional']
    s_c = axiom_ee / adapt_ee
    out(f'\n  Axiom 2 end-to-end P-slope (chi units, E=mobile): {axiom_ee:+.4f}')
    out(f'  Proportional-adaptation end-to-end per unit s:    {adapt_ee:.2f}')
    out(f'  Cancellation point s_c = {s_c:.4f} (adaptation magnitude equal to the'
        f' axiom\'s own P-slope)')

    # ================================================================ criterion
    out('\n' + '=' * 78)
    out('A.3 magnitude criterion: chi-bound, per-link translation, feasibility')
    out('=' * 78)
    out('  Axiom 2 bound on the between-arm experienced-chi gap at fixed P:')
    out('  |dchi| <= eps*sigma*|f(d+0.05) - f(d)|  (trapped vs mobile)')
    max_bound = 0.0
    for p in reversed(P_ORDER):
        d = BASED[p]
        b = EPS * SIG * abs(f(d + 0.05) - f(d))
        max_bound = max(max_bound, b)
        lin = b / (CHI_MAX - CHI_MIN)
        out(f'    held P={p:>13}: chi-bound={b:.4f}  linear-translation={lin:.4f} rating units'
            f'  3x={3*lin:.4f}')
    res_lin = LINKS['linear'][1]
    out(f'  Declared linear instrument resolution (7-point): {res_lin:.4f} rating units')
    out(f'  Max linear-translated bound over positions: {max_bound/(CHI_MAX-CHI_MIN):.4f}'
        f'  (3x: {3*max_bound/(CHI_MAX-CHI_MIN):.4f})')
    assert max_bound / (CHI_MAX - CHI_MIN) < res_lin, 'expected 1x bound < resolution'
    crossers = [p for p in P_ORDER
                if 3 * EPS * SIG * abs(f(BASED[p] + 0.05) - f(BASED[p]))
                / (CHI_MAX - CHI_MIN) >= res_lin]
    out('  ASSERT PASS: the 1x bound sits below one 7-point scale step at EVERY held')
    out('  position -- at 1x the magnitude criterion decides nothing at a realistic')
    out('  7-point instrument (any observed gap is within one scale step of the bound).')
    out(f'  The 3x-inflated bound CROSSES one scale step at mid-slope positions'
        f' {crossers}:')
    out('  the feasibility verdict itself flips inside the 1x-3x band, which is why')
    out('  the 3x safety factor is REGISTERED AS ARBITRARY and the 1x-3x band')
    out('  DECLARED INDETERMINATE -- the indeterminacy is live, not academic.')
    assert crossers, 'expected the 3x bound to cross resolution somewhere mid-slope'

    out('\n  Threshold-link breaker (assertion iv): a sub-bound chi gap straddling theta')
    lo, hi = THETA - 0.001, THETA + 0.001
    gap = link_threshold(hi) - link_threshold(lo)
    out(f'    dchi = {hi-lo:.3f} (<< bound {max_bound:.4f}) -> rating gap = {gap:.1f}'
        f' (full scale)')
    assert gap == 1.0, 'threshold link must amplify a sub-bound gap to full scale'
    out('  ASSERT PASS: the threshold link maps an arbitrarily small chi gap to a')
    out('  full-scale rating gap -- the magnitude criterion BREAKS under a saturating')
    out('  link. STATED LIMIT confirmed: the criterion is licensed only under an')
    out('  approximately linear reporting channel (the paper owes that sentence).')

    # ================================================================ sweep
    worlds = {'A': world_A, 'B': world_B, 'C': world_C}
    results = {}
    for lname, (link, res) in LINKS.items():
        out('\n' + '=' * 78)
        out(f'SWEEP under {lname} link (res = {res:.4f})')
        out('=' * 78)
        r = {}

        # -- assertion (v): probe's own control -- world A at s=0, nothing fires
        assert not obs_old(world_A, 0.0, link), f'({lname}) control: old fired at s=0'
        assert not obs_earm(world_A, 0.0, link), f'({lname}) control: E-arm fired at s=0'
        v0 = parm_verdict(world_A, 0.0, link, res)
        assert v0 == 'positive', f'({lname}) control: P-arm at s=0 read {v0}, not positive'
        out('\n(v) CONTROL PASS: world A at s=0 -> old silent, E-arm silent, P-arm')
        out('    positive (the axiom\'s own predicted signature; no falsifier fires).')

        # -- assertion (i): old observable fires in A, B, C, D
        sA = first_s(lambda s: obs_old(world_A, s, link))
        sB = first_s(lambda s: obs_old(world_B, s, link))
        sC = first_s(lambda s: obs_old(world_C, s, link))
        fires_D = obs_old(lambda p, e, s: world_C(p, e, s), s_c, link)
        out(f'\n(i) OLD observable ("a trapped worker rates non-extractive"), first firing s:')
        out(f'      A (axiom true, E-adaptation): s >= {sA}')
        out(f'      B (axiom false):              s >= {sB} (fires at all s; no adaptation needed)')
        out(f'      C (axiom true, P-adaptation): s >= {sC}')
        out(f'      D (cancellation, s={s_c:.3f}):  fires = {fires_D}')
        assert sA is not None and sB == 0.0 and sC is not None and fires_D, \
            f'({lname}) old observable must fire in all four worlds'
        out('    ASSERT PASS: the old observable fires in A, B, C and D -- consistent')
        out('    with axiom-true-plus-adaptation and axiom-false alike: NON-DISCRIMINATING.')

        # -- assertion (ii): E-arm falsifier fires in B, C, D; never in A
        eA = first_s(lambda s: obs_earm(world_A, s, link))
        eB = first_s(lambda s: obs_earm(world_B, s, link))
        eC = first_s(lambda s: obs_earm(world_C, s, link))
        eD = obs_earm(world_C, s_c, link)
        out(f'\n(ii) E-ARM falsifier (2x2 Cell 4, held P={HELD_P_EARM}), first firing s:')
        out(f'      A: {eA}   B: {eB}   C: s >= {eC}   D (s={s_c:.3f}): {eD}')
        assert eA is None, f'({lname}) E-arm fired in world A (it must not: Cell 2 obtains)'
        assert eB == 0.0 and eC is not None and eD, \
            f'({lname}) E-arm must fire in B, C, D'
        out('    ASSERT PASS: fires in B, C, D and never in A. Firing in C and D --')
        out('    where Axiom 2 is TRUE -- demonstrates the conjunct is necessary: Cell 4')
        out('    refutes (Axiom 2 AND no P-channel adaptation), never Axiom 2 alone.')

        # -- assertion (iii): P-arm sign read
        out(f'\n(iii) P-ARM sign read (end-to-end powerless-institutional, E held mobile):')
        vA = {parm_verdict(world_A, s, link, res) for s in S_GRID}
        assert vA == {'positive'}, f'({lname}) world A P-arm not uniformly positive: {vA}'
        vB = {parm_verdict(world_B, s, link, res) for s in S_GRID}
        assert vB == {'flat'}, f'({lname}) world B P-arm not uniformly flat: {vB}'
        out(f'      A: positive at every swept s.  B: flat at every swept s.')
        seq = [(s, parm_verdict(world_C, s, link, res)) for s in S_GRID]
        first_flat = first_s(lambda s: parm_verdict(world_C, s, link, res) == 'flat')
        first_neg = first_s(lambda s: parm_verdict(world_C, s, link, res) == 'negative')
        verdict_set = {v for _, v in seq}
        out(f'      C: verdicts over sweep = {sorted(verdict_set)};'
            f' first flat at s={first_flat}; first negative at s={first_neg}')
        vD = parm_verdict(world_C, s_c, link, res)
        assert vD == 'flat', f'({lname}) world D (s=s_c) must read flat, read {vD}'
        out(f'      D (s={s_c:.3f}): flat -- the cancellation signature, as pre-registered.')

        # -- separation interval: where does C's signature collapse onto B's?
        sigB = signature(world_B, 1.0, link, res)   # (True, 'flat'), s-independent
        collapse = [s for s in S_GRID if signature(world_C, s, link, res) == sigB]
        band_lo = collapse[0] if collapse else None
        band_hi = collapse[-1] if collapse else None
        band_closed = (band_hi is not None and band_hi < S_GRID[-1])
        for s in S_GRID:
            assert signature(world_A, s, link, res) != sigB, \
                f'({lname}) world A collapsed onto B at s={s}'
        out(f'\n      SEPARATION: world-B signature = (E-arm fired, P-arm flat).')
        if band_lo is None:
            out('      No s collapses C onto B under this link.')
        else:
            pct = (band_lo - s_c) / s_c * 100.0
            out(f'      C collapses onto B for s in [{band_lo}, '
                f'{band_hi if band_closed else "+inf (whole remaining sweep)"}]')
            out(f'      Band lower edge = s_c {pct:+.1f}% (s_c = {s_c:.3f}).')
            if not band_closed:
                out('      BAND UNBOUNDED ABOVE at this resolution: past the lower edge the')
                out('      bounded scale floors both ends of the P range, so the NEGATIVE')
                out('      verdict (dominant adaptation) is UNREACHABLE -- flat absorbs it.')
        assert s_c in [s for s in collapse] or (band_lo is not None and band_lo <= s_c), \
            f'({lname}) s_c not flagged inside the cancellation band'
        out(f'      ASSERT PASS: D (s=s_c) lies inside the declared cancellation band.')
        r.update(band_lo=band_lo, band_hi=band_hi, band_closed=band_closed,
                 first_neg=first_neg, e_arm_first_C=eC)
        results[lname] = r

    # ============================================ negative-branch positive control
    out('\n' + '=' * 78)
    out('Negative-verdict POSITIVE CONTROL (absence claims need a control that can fire)')
    out('=' * 78)
    out('  Config: fine-resolution instrument (res=0.01, ~101-point) + CONCENTRATED')
    out('  P-adaptation (h=1 at powerless, 0 elsewhere) -- labeled control, not a world.')
    link, res_fine = link_linear, 0.01
    wC_conc = lambda p, e, s: world_C(p, e, s, h=H_CONC)
    neg_s = first_s(lambda s: parm_verdict(wC_conc, s, link, res_fine) == 'negative')
    out(f'  First s with a NEGATIVE P-arm verdict: {neg_s}')
    assert neg_s is not None, 'negative verdict unreachable even in the control config'
    out('  ASSERT PASS: the verdict machinery CAN produce "negative". Its absence in')
    out('  the primary sweep (proportional adaptation, 7-point linear link) is a real')
    out('  absence -- a resolution+shape feasibility fact, not a dead code branch.')

    # ================================================================ headline
    out('\n' + '=' * 78)
    out('HEADLINE FINDINGS (in the pre-registered reporting form)')
    out('=' * 78)
    lin, thr = results['linear'], results['threshold']
    pct_lin = (lin['band_lo'] - s_c) / s_c * 100.0
    pct_thr = (thr['band_lo'] - s_c) / s_c * 100.0
    out(f'''
1. Separation holds except for adaptation magnitudes within the cancellation
   neighborhood of the axiom's own P-slope (s_c = {s_c:.3f} chi-units):
     linear 7-point link:  separation on s in [0, {lin['band_lo']}); band = [{lin['band_lo']}, +inf)
                           (lower edge {pct_lin:+.1f}% of s_c; UNBOUNDED ABOVE -- the
                           bounded scale floors the inversion, so band width at the
                           declared 7-point resolution is INFINITE, not the finite
                           +/-X the plan's expected-form sentence anticipated)
     threshold link:       separation on s in [0, {thr['band_lo']}); band = [{thr['band_lo']}, +inf)
                           (lower edge {pct_thr:+.1f}% of s_c -- saturation widens the
                           band far below s_c; sign survives only up to saturation)
   The cancellation hole and the instrument-resolution feasibility limit are ONE
   finding: the failing neighborhood's lower edge moves with resolution, and its
   upper edge exists only for instruments fine enough to see below the
   institutional position's headroom (control: reachable at res=0.01 under
   concentrated adaptation).

2. The P-arm sign table's NEGATIVE row is empirically empty at the declared
   7-point instrument under proportional adaptation: dominant P-adaptation
   presents as FLAT, so flat = (axiom false) OR (comparable-magnitude
   cancellation) OR (dominant adaptation, floored). The flat row's disjunction
   gains a third disjunct; the sign read still separates A (positive at every s)
   from every collapsed world, and the E-arm conjunct is still demonstrated
   necessary. Coverage did NOT collapse: the separation interval [0, {lin['band_lo']})
   is non-empty and covers all sub-cancellation adaptation magnitudes.

3. E-arm magnitude criterion: chi-bound translates to < one scale step at every
   held position on a 7-point instrument (max {max_bound/(CHI_MAX-CHI_MIN):.3f} vs step {res_lin:.3f});
   the threshold link amplifies a sub-bound gap to full scale. The criterion is
   licensed only under an approximately linear reporting channel at fine
   resolution -- declared limits, confirmed, not discovered late.
''')
    out('ALL ASSERTIONS PASSED (worlds A-D x two links, s-sweep 0..2.5 step 0.005,')
    out('s=0 control, negative-branch positive control, criterion checks).')

    log = os.path.join(os.path.dirname(os.path.abspath(__file__)),
                       'discrimination_probe.log')
    with open(log, 'w') as fh:
        fh.write('\n'.join(_lines) + '\n')
    print(f'\n[log written: {log}]')


if __name__ == '__main__':
    sys.exit(main())
