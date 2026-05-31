"""
Tripwire graduation tests for fabricated-default sites (OQ-33 / audit plan).
OPEN-1: D1b drl_composition.pl:180 BaseX=0.5 → 999.9
OPEN-2: D2 drl_core.pl:96 Value=0 → 999.9
OPEN-3: D20 boltzmann_compliance.pl:245 BaseEps=0.5 → 999.9
OPEN-3: D21 boltzmann_compliance.pl:251 Supp=0 → 999.9
OPEN-6: D1a drl_composition.pl:179 Supp=0.5 → 999.9
"""
import subprocess, tempfile, os, json, sys, re, shutil
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
PROLOG_DIR = REPO_ROOT / "prolog"
OUTPUTS_DIR = REPO_ROOT / "outputs"


# ── Prolog query templates ───────────────────────────────────────────────────

TEMPORAL_QUERY = """\
:- [stack],
   corpus_loader:load_all_testsets,
   use_module(covering_analysis),
   use_module(drl_composition),
   use_module(constraint_indexing),
   constraint_indexing:default_context(Ctx),
   findall(C-T-Type, (
       covering_analysis:all_corpus_constraints(Cs),
       member(C, Cs),
       catch(drl_composition:constraint_history(C, Ctx, Timeline), _, Timeline=[]),
       member(state(T, Type), Timeline)
   ), Rows),
   length(Rows, N),
   format('ROWS ~w~n', [N]),
   forall(member(C-T-Type, Rows),
       format('ROW ~w ~w ~w~n', [C, T, Type])),
   halt.
"""

STATIC_CLASSIFY_QUERY = """\
:- [stack],
   corpus_loader:load_all_testsets,
   use_module(covering_analysis),
   use_module(product_site_export),
   product_site_export:run_product_export_to('/dev/null'),
   halt.
"""

# Use dr_type directly for static classification
STATIC_DR_TYPE_QUERY = """\
:- [stack],
   corpus_loader:load_all_testsets,
   use_module(covering_analysis),
   use_module(constraint_indexing),
   constraint_indexing:default_context(Ctx),
   findall(C-Type, (
       covering_analysis:all_corpus_constraints(Cs),
       member(C, Cs),
       catch(drl_core:dr_type(C, Ctx, Type), _, Type=error)
   ), Rows),
   length(Rows, N),
   format('ROWS ~w~n', [N]),
   forall(member(C-Type, Rows),
       format('ROW ~w ~w~n', [C, Type])),
   halt.
"""

BOLTZMANN_QUERY = """\
:- [stack],
   corpus_loader:load_all_testsets,
   use_module(covering_analysis),
   use_module(boltzmann_compliance),
   findall(C-Verdict, (
       covering_analysis:all_corpus_constraints(Cs),
       member(C, Cs),
       catch(boltzmann_compliance:boltzmann_compliant(C, Verdict), _, Verdict=error)
   ), Rows),
   length(Rows, N),
   format('ROWS ~w~n', [N]),
   forall(member(C-Verdict, Rows),
       format('ROW ~w ~w~n', [C, Verdict])),
   halt.
"""


def run_prolog(query_str, timeout=300):
    """Run a Prolog query from PROLOG_DIR, return stdout lines."""
    qfile = tempfile.NamedTemporaryFile(mode='w', suffix='.pl', dir=PROLOG_DIR,
                                        delete=False)
    try:
        qfile.write(query_str)
        qfile.flush()
        qfile.close()
        result = subprocess.run(
            ['swipl', '-q', '-t', 'halt(1)', '-g', f'[{Path(qfile.name).stem}]'],
            capture_output=True, text=True, cwd=PROLOG_DIR, timeout=timeout)
        lines = result.stdout.splitlines()
        return lines, result.returncode
    finally:
        os.unlink(qfile.name)


def parse_rows(lines):
    """Parse 'ROW C T Type' or 'ROW C Type' lines into a dict."""
    rows = {}
    for line in lines:
        parts = line.strip().split()
        if parts and parts[0] == 'ROW':
            key = tuple(parts[1:-1])  # (C,) or (C, T)
            rows[key] = parts[-1]
    return rows


def patch_and_run(filepath, old_text, new_text, query_str, label):
    """Backup file, apply patch, run query, restore, return (baseline, patched)."""
    backup = filepath.with_suffix('.pl.tripwire_backup')
    original = filepath.read_text()

    if old_text not in original:
        print(f"  ERROR: patch text not found in {filepath.name}")
        return None, None

    print(f"  Patching {filepath.name}: {old_text!r} → {new_text!r}")
    shutil.copy2(filepath, backup)
    try:
        # Baseline (original)
        print(f"  Running baseline...")
        baseline_lines, _ = run_prolog(query_str)
        baseline = parse_rows(baseline_lines)
        print(f"    Baseline rows: {len(baseline)}")

        # Patched
        patched_text = original.replace(old_text, new_text, 1)
        filepath.write_text(patched_text)
        print(f"  Running patched...")
        patched_lines, _ = run_prolog(query_str)
        patched = parse_rows(patched_lines)
        print(f"    Patched rows: {len(patched)}")
    finally:
        shutil.copy2(backup, filepath)
        backup.unlink()

    return baseline, patched


def count_flips(baseline, patched, count_unknown_to_nonunknown=False):
    """Count keys where type changed. Returns (flipped, total_changed, details)."""
    flipped = 0
    total_changed = 0
    details = []
    all_keys = set(baseline) | set(patched)
    for key in all_keys:
        b = baseline.get(key, 'missing')
        p = patched.get(key, 'missing')
        if b != p:
            total_changed += 1
            # Primary: non-unknown → unknown (poisoning breaks classification)
            if b != 'unknown' and p == 'unknown':
                flipped += 1
                details.append({'key': key, 'baseline': b, 'patched': p, 'direction': 'to_unknown'})
            elif count_unknown_to_nonunknown and b == 'unknown' and p != 'unknown':
                flipped += 1
                details.append({'key': key, 'baseline': b, 'patched': p, 'direction': 'from_unknown'})
            else:
                details.append({'key': key, 'baseline': b, 'patched': p, 'direction': 'other'})
    return flipped, total_changed, details


def run_all():
    results = {}

    # ── OPEN-6: D1a — temporal Supp=0.5 → 999.9 ──────────────────────────
    print("\n=== OPEN-6: D1a drl_composition.pl:179 Supp=0.5 → 999.9 ===")
    f = PROLOG_DIR / "drl_composition.pl"
    # Exact match from line 179: single-line if-then-else form
    D1A_OLD = "(narrative_ontology:measurement(_, C, suppression_requirement, Time, Supp) -> true ; Supp = 0.5),"
    D1A_NEW = "(narrative_ontology:measurement(_, C, suppression_requirement, Time, Supp) -> true ; Supp = 999.9),"
    bl, pt = patch_and_run(f, D1A_OLD, D1A_NEW, TEMPORAL_QUERY, "D1a")
    if bl is not None:
        flipped, changed, details = count_flips(bl, pt)
        non_unknown_baseline = sum(1 for v in bl.values() if v != 'unknown')
        print(f"  D1a: {flipped} flips to unknown / {non_unknown_baseline} non-unknown baseline rows / {changed} total changed")
        results['D1a'] = {'flipped_to_unknown': flipped, 'total_changed': changed,
                          'non_unknown_baseline': non_unknown_baseline,
                          'details_sample': details[:5]}
    else:
        results['D1a'] = {'error': 'patch_not_found'}

    # ── OPEN-1: D1b — temporal BaseX=0.5 → 999.9 ─────────────────────────
    print("\n=== OPEN-1: D1b drl_composition.pl:180 BaseX=0.5 → 999.9 ===")
    # Exact match from line 180: single-line if-then-else form
    D1B_OLD = "(narrative_ontology:measurement(_, C, base_extractiveness, Time, BaseX) -> true ; BaseX = 0.5),"
    D1B_NEW = "(narrative_ontology:measurement(_, C, base_extractiveness, Time, BaseX) -> true ; BaseX = 999.9),"
    bl, pt = patch_and_run(f, D1B_OLD, D1B_NEW, TEMPORAL_QUERY, "D1b")
    if bl is not None:
        flipped, changed, details = count_flips(bl, pt)
        non_unknown_baseline = sum(1 for v in bl.values() if v != 'unknown')
        print(f"  D1b: {flipped} flips to unknown / {non_unknown_baseline} non-unknown baseline rows / {changed} total changed")
        results['D1b'] = {'flipped_to_unknown': flipped, 'total_changed': changed,
                          'non_unknown_baseline': non_unknown_baseline,
                          'details_sample': details[:5]}
    else:
        results['D1b'] = {'error': 'patch_not_found'}

    # ── OPEN-2: D2 — static Value=0 → 999.9 ──────────────────────────────
    print("\n=== OPEN-2: D2 drl_core.pl:96 Value=0 → 999.9 ===")
    g = PROLOG_DIR / "drl_core.pl"
    D2_OLD = "(narrative_ontology:constraint_metric(Constraint, ActualMetricName, Value) -> true ; Value = 0)."
    D2_NEW = "(narrative_ontology:constraint_metric(Constraint, ActualMetricName, Value) -> true ; Value = 999.9)."
    bl, pt = patch_and_run(g, D2_OLD, D2_NEW, STATIC_DR_TYPE_QUERY, "D2")
    if bl is not None:
        flipped, changed, details = count_flips(bl, pt)
        non_unknown_baseline = sum(1 for v in bl.values() if v != 'unknown')
        print(f"  D2: {flipped} flips / {non_unknown_baseline} non-unknown baseline / {changed} total changed")
        results['D2'] = {'flipped_to_unknown': flipped, 'total_changed': changed,
                         'non_unknown_baseline': non_unknown_baseline,
                         'details_sample': details[:10]}
    else:
        results['D2'] = {'error': 'patch_not_found'}

    # ── OPEN-3: D20 — Boltzmann BaseEps=0.5 → 999.9 ──────────────────────
    print("\n=== OPEN-3: D20 boltzmann_compliance.pl:245 BaseEps=0.5 → 999.9 ===")
    h = PROLOG_DIR / "boltzmann_compliance.pl"
    # Multi-line form (lines 243-247)
    D20_OLD = "    (   narrative_ontology:constraint_metric(C, ExtMetricName, BaseEps)\n    ->  true\n    ;   BaseEps = 0.5\n    ),"
    D20_NEW = "    (   narrative_ontology:constraint_metric(C, ExtMetricName, BaseEps)\n    ->  true\n    ;   BaseEps = 999.9\n    ),"
    bl, pt = patch_and_run(h, D20_OLD, D20_NEW, BOLTZMANN_QUERY, "D20")
    if bl is not None:
        flipped, changed, details = count_flips(bl, pt)
        print(f"  D20: {flipped} flips to unknown / {changed} total changed / {len(bl)} baseline rows")
        results['D20'] = {'flipped_to_unknown': flipped, 'total_changed': changed,
                          'total_rows': len(bl),
                          'details_sample': details[:5]}
    else:
        results['D20'] = {'error': 'patch_not_found'}

    # ── OPEN-3: D21 — Boltzmann Supp=0 → 999.9 ───────────────────────────
    print("\n=== OPEN-3: D21 boltzmann_compliance.pl:251 Supp=0 → 999.9 ===")
    # Multi-line form (lines 249-253)
    D21_OLD = "    (   narrative_ontology:constraint_metric(C, SuppMetricName, Supp)\n    ->  true\n    ;   Supp = 0\n    ),"
    D21_NEW = "    (   narrative_ontology:constraint_metric(C, SuppMetricName, Supp)\n    ->  true\n    ;   Supp = 999.9\n    ),"
    bl, pt = patch_and_run(h, D21_OLD, D21_NEW, BOLTZMANN_QUERY, "D21")
    if bl is not None:
        flipped, changed, details = count_flips(bl, pt)
        print(f"  D21: {flipped} flips to unknown / {changed} total changed / {len(bl)} baseline rows")
        results['D21'] = {'flipped_to_unknown': flipped, 'total_changed': changed,
                          'total_rows': len(bl),
                          'details_sample': details[:5]}
    else:
        results['D21'] = {'error': 'patch_not_found'}

    # Write results
    out_path = OUTPUTS_DIR / "tripwire_fabricated_defaults_results.json"
    out_path.write_text(json.dumps(results, indent=2))
    print(f"\nResults written to {out_path}")
    return results


if __name__ == "__main__":
    os.chdir(PROLOG_DIR)
    run_all()
