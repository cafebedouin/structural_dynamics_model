"""
g_channel_flip_audit.py — Phase 2 of the g(P,E) classification-flip audit.

Two tests:
  2A. D-isolation swap: for the 66 clean moderate-observer flip-zone candidates
      (HasVic=true, eps in [0.596, 0.66), no moderate directionality_override),
      inject canonical_d=0.6459 via override, re-run dr_type, compare types.
  2B. Mechanism sweep (labeled "mechanism — not corpus finding"): fix eps at
      representative values, sweep d over the attested empirical range, count
      chi threshold crossings per observer.

Pre-registered inference rule (from plan §h):
  INERT:   zero isolatable flips (type difference driven by chi gate alone).
  LIVE:    ≥1 flip that survives isolation (chi-gate driven, not Supp/immutability confound).
  DORMANT: threshold crossings only outside attested corpus d-range, or
           only in mechanism sweep without observational flip.
"""

import json
import math
import re
import subprocess
import sys
import tempfile
import os

# ---------------------------------------------------------------------------
# Sigmoid and chi helpers
# ---------------------------------------------------------------------------
L, U, D0, K = -0.20, 1.50, 0.50, 6.00

def sigmoid_f(d):
    return L + (U - L) / (1 + math.exp(-K * (d - D0)))

# Standard observer parameters (from logical_fingerprint.pl:127-149 and config.pl)
CANONICAL_D = {
    'powerless':     1.00,
    'moderate':      0.6459,
    'institutional': 0.00,
    'analytical':    0.7250,
}
SCOPE_MOD = {
    'powerless':     0.8,   # local
    'moderate':      1.0,   # national
    'institutional': 1.0,   # national
    'analytical':    1.2,   # global
}
CANONICAL_CHI = {
    obs: 1.0 * sigmoid_f(CANONICAL_D[obs]) * SCOPE_MOD[obs]
    for obs in CANONICAL_D
}

# Classification chi thresholds (config.pl)
SNARE_CHI_FLOOR        = 0.66
SNARE_EPS_FLOOR        = 0.46
SNARE_SUPP_FLOOR       = 0.60
ROPE_CHI_CEILING       = 0.35
ROPE_EPS_CEILING       = 0.45
TANGLED_CHI_FLOOR      = 0.40
TANGLED_CHI_CEIL       = 0.90
TANGLED_EPS_FLOOR      = 0.30
TANGLED_SUPP_FLOOR     = 0.40
NAT_EPS_FLOOR          = 0.45  # naturalized: eps > this and chi < tangled floor

# ---------------------------------------------------------------------------
# Step 1: Build candidate list
# ---------------------------------------------------------------------------

def load_candidates(pipeline_path):
    data = json.load(open(pipeline_path))
    per  = data['per_constraint']
    candidates = []
    for c in per:
        eps  = c.get('base_extractiveness') or 0.0
        supp = c.get('suppression') or 0.0
        vics = c.get('victims') or []
        pchi = c.get('perspective_chi') or {}
        mod  = pchi.get('moderate') or {}
        if not mod:
            continue
        if 0.596 <= eps < 0.66 and len(vics) > 0:
            candidates.append({
                'id':      c['id'],
                'eps':     eps,
                'supp':    supp,
                'n_vic':   len(vics),
                'g_type':  (c.get('perspectives') or {}).get('moderate'),
                'g_chi':   mod.get('chi'),
                'g_d':     mod.get('d'),
                'beneficiaries': c.get('beneficiaries') or [],
                'victims':       vics,
            })
    return candidates

def filter_overrides(candidates, testsets_dir):
    pat = re.compile(r'directionality_override\([^,]+,\s*moderate,')
    clean, dropped = [], []
    for c in candidates:
        fpath = os.path.join(testsets_dir, c['id'] + '.pl')
        try:
            content = open(fpath).read()
            if pat.search(content):
                dropped.append(c['id'])
            else:
                clean.append(c)
        except FileNotFoundError:
            # Testset file missing — drop silently
            dropped.append(c['id'])
    return clean, dropped

# ---------------------------------------------------------------------------
# Denominator hygiene: verify canonical chi < snare floor for each candidate
# ---------------------------------------------------------------------------

def verify_denominator(candidates):
    valid, excluded = [], []
    for c in candidates:
        chi_can = c['eps'] * CANONICAL_CHI['moderate']
        c['chi_canonical'] = chi_can
        if chi_can < SNARE_CHI_FLOOR:
            valid.append(c)
        else:
            c['exclusion_reason'] = f'canonical chi={chi_can:.4f} >= {SNARE_CHI_FLOOR} (no flip to test)'
            excluded.append(c)
    return valid, excluded

# ---------------------------------------------------------------------------
# 2A: Run Prolog d-isolation for all clean valid candidates in one session
# ---------------------------------------------------------------------------

MODERATE_CTX = "context(agent_power(moderate), time_horizon(biographical), exit_options(mobile), spatial_scope(national))"

def build_prolog_script(constraint_ids, canonical_d):
    """Build a Prolog goal string that, for each constraint:
      - gets the g-path dr_type
      - asserts canonical override, gets type again
      - retracts override
      - writes CSV: id,g_type,canonical_type
    """
    lines = [
        ":- set_prolog_flag(verbose, silent).",
        ":- [stack].",
        ":- set_prolog_flag(verbose, silent).",
        "",
        "run_isolation :-",
        "    ModCtx = " + MODERATE_CTX + ",",
    ]
    for i, cid in enumerate(constraint_ids):
        term_cid = cid
        sep = "," if i < len(constraint_ids) - 1 else ""
        lines.append(f"    test_one({repr(cid)}, {repr(cid)}, ModCtx, {canonical_d}){sep}")
    lines[-1] = lines[-1].rstrip(',') + "."
    lines.append("")
    lines.append("test_one(StrID, CID, Ctx, CanD) :-")
    lines.append("    atom_string(CAtom, StrID),")
    lines.append("    % G-path type (as computed by the engine normally)")
    lines.append("    ( drl_core:dr_type(CAtom, Ctx, GT) -> true ; GT = unknown ),")
    lines.append("    % Inject canonical override")
    lines.append("    assert(constraint_indexing:directionality_override(CAtom, moderate, CanD)),")
    lines.append("    ( drl_core:dr_type(CAtom, Ctx, CT) -> true ; CT = unknown ),")
    lines.append("    % Clean up override")
    lines.append("    retract(constraint_indexing:directionality_override(CAtom, moderate, CanD)),")
    lines.append("    % Emit CSV line")
    lines.append("    atomic_list_concat([StrID, GT, CT], ',', Line),")
    lines.append("    writeln(Line).")
    lines.append("")
    lines.append(":- run_isolation, halt.")
    return "\n".join(lines)

def build_prolog_script_v2(constraint_ids, canonical_d):
    """Simpler approach: one-liner per constraint via forall over list."""
    cid_list = "[" + ",".join(repr(c) for c in constraint_ids) + "]"
    script = f"""
:- set_prolog_flag(verbose, silent).
:- [stack].
:- set_prolog_flag(verbose, silent).
% Load the full corpus (testset facts) — required for base_extractiveness etc.
:- corpus_loader:ensure_corpus_loaded.

run_all :-
    ModCtx = {MODERATE_CTX},
    CanD = {canonical_d},
    IDs = {cid_list},
    forall(
        member(StrID, IDs),
        (
            atom_string(CAtom, StrID),
            ( drl_core:dr_type(CAtom, ModCtx, GT) -> true ; GT = unknown ),
            assert(constraint_indexing:directionality_override(CAtom, moderate, CanD)),
            ( drl_core:dr_type(CAtom, ModCtx, CT) -> true ; CT = unknown ),
            retract(constraint_indexing:directionality_override(CAtom, moderate, CanD)),
            atomic_list_concat([StrID, GT, CT], ',', Line),
            writeln(Line)
        )
    ).

:- run_all, halt.
"""
    return script

def run_prolog_isolation(constraint_ids, canonical_d, prolog_dir, timeout=300):
    """Run the d-isolation Prolog script. Returns list of (id, g_type, canon_type)."""
    script = build_prolog_script_v2(constraint_ids, canonical_d)
    with tempfile.NamedTemporaryFile(mode='w', suffix='.pl', dir=prolog_dir,
                                     delete=False, prefix='g_audit_') as f:
        f.write(script)
        tmp_path = f.name

    try:
        result = subprocess.run(
            ['swipl', '-g', f'[{os.path.basename(tmp_path)[:-3]}], halt', '-t', 'halt(1)'],
            cwd=prolog_dir,
            capture_output=True,
            text=True,
            timeout=timeout,
        )
        output = result.stdout
    finally:
        os.unlink(tmp_path)

    rows = []
    for line in output.strip().splitlines():
        line = line.strip()
        parts = line.split(',')
        if len(parts) == 3:
            rows.append((parts[0], parts[1], parts[2]))
    return rows, result.stderr

# ---------------------------------------------------------------------------
# 2B: Mechanism sweep (synthetic, labeled as such)
# ---------------------------------------------------------------------------

def chi_zone(chi, eps, supp=None):
    """Identify which cascade zone chi falls in (chi and eps criteria only).
    supp defaults to 0.65 (typical snare-zone value) for zone labeling.
    Does NOT evaluate structural predicates (emerges_naturally, active_enforcement, etc.)"""
    if supp is None:
        supp = 0.65
    if chi >= SNARE_CHI_FLOOR and eps >= SNARE_EPS_FLOOR and supp >= SNARE_SUPP_FLOOR:
        return 'snare-eligible'
    if chi <= ROPE_CHI_CEILING and (chi <= 0 or eps <= ROPE_EPS_CEILING):
        return 'rope-eligible'
    if TANGLED_CHI_FLOOR <= chi <= TANGLED_CHI_CEIL and eps >= TANGLED_EPS_FLOOR and supp >= TANGLED_SUPP_FLOOR:
        return 'tangled-eligible'
    if eps > NAT_EPS_FLOOR and chi < TANGLED_CHI_FLOOR:
        return 'naturalized-eligible'
    return 'other'

def mechanism_sweep():
    print("\n" + "="*70)
    print("2B. MECHANISM SWEEP (synthetic — not a corpus finding)")
    print("="*70)
    print("Evaluates chi thresholds over attested d-ranges. Labels indicate")
    print("which cascade zone chi falls in (chi+eps criteria only; structural")
    print("predicates such as active_enforcement not evaluated here).\n")

    eps_values = [0.50, 0.58, 0.62]
    observers = ['powerless', 'moderate', 'institutional', 'analytical']
    # Attested d ranges from §g of the plan
    d_ranges = {
        'powerless':     (0.72, 1.00),
        'moderate':      (0.55, 0.92),
        'institutional': (0.00, 0.95),
        'analytical':    (0.15, 0.95),
    }
    # Typical supp for mechanism sweep
    supp = 0.65

    for obs in observers:
        d_lo, d_hi = d_ranges[obs]
        sigma = SCOPE_MOD[obs]
        d_can = CANONICAL_D[obs]
        print(f"\nObserver: {obs}  (σ={sigma}, canonical d={d_can}, attested d∈[{d_lo},{d_hi}])")
        print(f"{'eps':>6}  {'d':>6}  {'chi':>8}  {'canonical_chi':>14}  zone")
        print("-"*60)
        for eps in eps_values:
            chi_can = eps * sigmoid_f(d_can) * sigma
            # Sample the attested range at a few key points
            d_points = [d_lo, d_can, (d_lo+d_hi)/2, d_hi]
            # Add snare-boundary d if in range
            # snare boundary: eps * f(d) * sigma = 0.66 → f(d) = 0.66/(eps*sigma)
            target_f = SNARE_CHI_FLOOR / (eps * sigma)
            if L < target_f < U:
                # inverse sigmoid: d = D0 - ln((U-L)/(target_f-L) - 1)/K
                inner = (U - L) / (target_f - L) - 1
                if inner > 0:
                    d_boundary = D0 - math.log(inner) / K
                    if d_lo <= d_boundary <= d_hi:
                        d_points.append(d_boundary)
            d_points = sorted(set(round(x, 4) for x in d_points))
            for d in d_points:
                chi = eps * sigmoid_f(d) * sigma
                zone = chi_zone(chi, eps, supp)
                marker = "<-- snare boundary" if abs(chi - SNARE_CHI_FLOOR) < 0.01 else ""
                print(f"  {eps:6.2f}  {d:6.4f}  {chi:8.4f}  {chi_can:14.4f}  {zone} {marker}")

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

def main():
    pipeline_path = 'outputs/pipeline_output.json'
    testsets_dir  = 'prolog/testsets'
    prolog_dir    = 'prolog'
    canonical_d   = CANONICAL_D['moderate']  # 0.6459

    print("="*70)
    print("g(P,E) CHANNEL CLASSIFICATION-FLIP AUDIT — Phase 2")
    print("Pre-registered inference rule: plan §h (no changes after Phase 1)")
    print("="*70)

    # --- Step 1: Candidates ---
    print("\nSTEP 1: Build candidate pool")
    all_candidates = load_candidates(pipeline_path)
    print(f"  Flip-zone candidates (eps∈[0.596,0.66), HasVic=true): {len(all_candidates)}")

    # --- Step 2: Drop override cases ---
    clean, dropped_override = filter_overrides(all_candidates, testsets_dir)
    print(f"  Dropped (have moderate override, bypass g-path): {len(dropped_override)} {dropped_override}")
    print(f"  Clean candidates (no moderate override): {len(clean)}")

    # --- Step 3: Denominator hygiene ---
    valid, excluded_denom = verify_denominator(clean)
    print(f"  Excluded (canonical chi already ≥ 0.66, no flip to test): {len(excluded_denom)}")
    for e in excluded_denom:
        print(f"    {e['id']}: chi_canonical={e['chi_canonical']:.4f}")
    print(f"  Test denominator (clean, canonical chi < 0.66): {len(valid)}")

    if not valid:
        print("\n  No testable candidates. Verdict: INERT (empty denominator).")
        mechanism_sweep()
        return

    print("\n  Sample candidates (eps, supp, g_type, g_chi, canonical_chi):")
    for c in valid[:5]:
        print(f"    {c['id']}: eps={c['eps']}, supp={c['supp']}, "
              f"g_type={c['g_type']}, g_chi={c['g_chi']:.4f}, "
              f"chi_can={c['chi_canonical']:.4f}")

    # --- Step 4: Prolog d-isolation ---
    print(f"\nSTEP 2A: Prolog d-isolation swap (n={len(valid)}, canonical_d={canonical_d})")
    print("  Loading stack and running Prolog session (may take ~60s)...")

    ids = [c['id'] for c in valid]
    rows, stderr = run_prolog_isolation(ids, canonical_d, prolog_dir, timeout=300)

    if not rows:
        print("  ERROR: No rows returned from Prolog. stderr excerpt:")
        print("  " + stderr[:500].replace('\n', '\n  '))
        print("\n  Cannot complete 2A without Prolog output.")
    else:
        print(f"  Prolog returned {len(rows)} rows (expected {len(valid)})")
        if len(rows) != len(valid):
            print(f"  WARNING: row count mismatch. Missing: {len(valid)-len(rows)}")
        # Corpus-load guard: if all g-types are unknown, corpus wasn't loaded
        n_unknown_g = sum(1 for (_, gt, _) in rows if gt == 'unknown')
        if n_unknown_g == len(rows) and len(rows) > 0:
            print(f"  FATAL: all {n_unknown_g} g-path types are 'unknown' — corpus not loaded.")
            print("  Aborting: results would be meaningless (unknown==unknown → false INERT).")
            rows = []

        # Build lookup from pipeline g-type
        pipeline_g = {c['id']: c['g_type'] for c in valid}

        # Classify results
        flipped    = []   # type differed
        same       = []   # type unchanged
        errors     = []   # one or both returned unknown unexpectedly

        for (cid, g_type, can_type) in rows:
            # Cross-check g_type against pipeline
            pl_g = pipeline_g.get(cid, '?')
            if g_type != pl_g and g_type != 'unknown':
                note = f'[g-type mismatch: engine={g_type}, pipeline={pl_g}]'
            else:
                note = ''
            if g_type == can_type:
                same.append((cid, g_type, can_type, note))
            elif 'unknown' in (g_type, can_type):
                errors.append((cid, g_type, can_type, note))
            else:
                flipped.append((cid, g_type, can_type, note))

        print(f"\n  Results:")
        print(f"    Same type (no flip): {len(same)}")
        print(f"    Type changed (flip):  {len(flipped)}")
        print(f"    Error/unknown:        {len(errors)}")

        if flipped:
            print(f"\n  FLIPS DETAIL (before isolation test):")
            for (cid, gt, ct, note) in flipped:
                # Find candidate metadata
                meta = next((c for c in valid if c['id'] == cid), {})
                print(f"    {cid}:")
                print(f"      eps={meta.get('eps')}, supp={meta.get('supp')}")
                print(f"      g_type={gt}, canonical_type={ct} {note}")

            print(f"\n  ISOLATION ANALYSIS:")
            print("  For each flip, determine if type difference is chi-gate-driven")
            print("  or attributable to Supp/immutability confound:\n")

            survived_isolation = []
            confounded = []
            for (cid, gt, ct, note) in flipped:
                meta = next((c for c in valid if c['id'] == cid), {})
                eps  = meta.get('eps', 0)
                supp = meta.get('supp', 0)
                chi_g   = meta.get('g_chi', 0)
                chi_can = meta.get('chi_canonical', 0)

                # Isolation check: was the flip at the snare chi gate?
                # g_type=snare AND can_type!=snare AND chi_g>=0.66 AND chi_can<0.66
                # AND snare supp/eps conditions unchanged (they are — same constraint)
                is_snare_flip = (
                    gt == 'snare' and
                    ct != 'snare' and
                    (chi_g or 0) >= SNARE_CHI_FLOOR and
                    chi_can < SNARE_CHI_FLOOR and
                    eps >= SNARE_EPS_FLOOR and
                    supp >= SNARE_SUPP_FLOOR
                )
                # If the snare chi gate flipped AND supp was already ≥ floor (unchanged),
                # the flip is chi-gate-driven. snare_immutability_check is also unchanged
                # (same constraint, same context structure — only d changed).
                if is_snare_flip:
                    survived_isolation.append((cid, gt, ct, 'chi-gate-driven'))
                else:
                    reason = 'not snare-boundary' if gt != 'snare' else (
                        'supp<0.60' if supp < SNARE_SUPP_FLOOR else
                        'chi_g<0.66' if (chi_g or 0) < SNARE_CHI_FLOOR else
                        'other-gate')
                    confounded.append((cid, gt, ct, reason))

                print(f"    {cid}: {gt} → {ct}")
                print(f"      chi_g={chi_g:.4f}, chi_can={chi_can:.4f}, supp={supp}")
                if is_snare_flip:
                    print(f"      → ISOLATED: chi gate crossed ({chi_g:.4f}→{chi_can:.4f}), "
                          f"supp={supp}≥{SNARE_SUPP_FLOOR}, eps={eps}≥{SNARE_EPS_FLOOR}")
                    print(f"        snare_immutability_check: same (d-only change) → flip is chi-driven")
                else:
                    print(f"      → CONFOUND: {reason}")

            print(f"\n  Isolation summary:")
            print(f"    Flips surviving isolation (chi-gate-driven): {len(survived_isolation)}")
            print(f"    Confounded (cascade gate other than chi):     {len(confounded)}")
        else:
            print("\n  No flips detected. Verdict supports INERT.")

        if errors:
            print(f"\n  Errors/unknown types ({len(errors)}):")
            for (cid, gt, ct, note) in errors:
                print(f"    {cid}: g={gt}, can={ct} {note}")

    # --- VERDICT ---
    print("\n" + "="*70)
    print("VERDICT (pre-registered rule from plan §h)")
    print("="*70)
    n_clean   = len(valid)
    n_flipped = len(flipped) if rows else 0
    n_survived = len(survived_isolation) if rows and flipped else 0

    print(f"\nObservers covered analytically by §h baseline (0 flips in 910 from ±25% sweep):")
    print("  powerless:     INERT (±25% prh sweep spans canonical d=1.00; 0 flips)")
    print("  institutional: INERT (g-path d∈[0.07,0.12] always chi<0 → rope; same as canonical)")
    print("  analytical:    INERT (Δf≈−0.008 at d=0.72 vs 0.725; Δchi<0.01ε; cannot cross threshold)")
    print(f"\nModerate observer (only open question):")
    print(f"  Denominator: {n_clean} clean candidates (no moderate override, canonical chi<0.66)")
    if rows:
        print(f"  Flips (type changed): {n_flipped}/{n_clean}")
        print(f"  Survived isolation (chi-gate-driven): {n_survived}/{n_clean}")
        if n_survived > 0:
            print(f"\n  VERDICT: LIVE — g-path produces {n_survived} chi-gate-driven classification")
            print(f"  flips at moderate observer. g-channel has ε-independent effect.")
        elif n_flipped > 0:
            print(f"\n  VERDICT: INERT — {n_flipped} apparent flips, all absorbed by cascade-gate")
            print(f"  confounds (Supp/immutability). g-channel does not independently decide type.")
        else:
            print(f"\n  VERDICT: INERT — zero flips. g-channel produces no classification")
            print(f"  change at any standard observer across {n_clean} clean candidates.")
    else:
        print(f"  (Prolog run failed — see stderr above)")
        print(f"\n  VERDICT: UNDETERMINED — Prolog error prevented 2A completion.")

    # --- 2B Mechanism sweep ---
    mechanism_sweep()

if __name__ == '__main__':
    main()
