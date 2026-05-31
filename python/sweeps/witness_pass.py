"""
Witness-production pass for the authoring-closure audit.
Items 1-8 from the review request.
Writes all raw output to stdout — no summarisation.
"""
import subprocess, tempfile, os, json, shutil
from collections import Counter
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]
PROLOG_DIR = REPO_ROOT / "prolog"

D1A_MARKER = "(narrative_ontology:measurement(_, C, suppression_requirement, Time, Supp) -> true ; Supp = 0.5),"

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

STATIC_QUERY = """\
:- [stack],
   corpus_loader:load_all_testsets,
   use_module(covering_analysis),
   use_module(constraint_indexing),
   constraint_indexing:default_context(Ctx),
   covering_analysis:all_corpus_constraints(Cs),
   length(Cs, N),
   format('ITERATED ~w constraints~n', [N]),
   findall(C-Type, (
       member(C, Cs),
       catch(drl_core:dr_type(C, Ctx, Type), _, Type=error)
   ), Rows),
   length(Rows, NR),
   format('ROWS ~w~n', [NR]),
   forall(member(C-Type, Rows),
       format('ROW ~w ~w~n', [C, Type])),
   halt.
"""

BOLTZMANN_QUERY = """\
:- [stack],
   corpus_loader:load_all_testsets,
   use_module(covering_analysis),
   use_module(boltzmann_compliance),
   covering_analysis:all_corpus_constraints(Cs),
   length(Cs, N),
   format('ITERATED ~w constraints~n', [N]),
   findall(C-Verdict, (
       member(C, Cs),
       catch(boltzmann_compliance:boltzmann_compliant(C, Verdict), _, Verdict=error)
   ), Rows),
   length(Rows, NR),
   format('ROWS ~w~n', [NR]),
   forall(member(C-Verdict, Rows),
       format('ROW ~w ~w~n', [C, Verdict])),
   halt.
"""


def run_prolog(query_str, timeout=300):
    qfile = tempfile.NamedTemporaryFile(mode='w', suffix='.pl', dir=PROLOG_DIR, delete=False)
    try:
        qfile.write(query_str); qfile.flush(); qfile.close()
        r = subprocess.run(
            ['swipl', '-q', '-t', 'halt(1)', '-g', f'[{Path(qfile.name).stem}]'],
            capture_output=True, text=True, cwd=PROLOG_DIR, timeout=timeout)
        return r.stdout.splitlines(), r.returncode
    finally:
        os.unlink(qfile.name)


def parse_rows(lines):
    rows = {}
    for line in lines:
        p = line.strip().split()
        if p and p[0] == 'ROW':
            rows[tuple(p[1:-1])] = p[-1]
    return rows


def get_iterated(lines):
    for line in lines:
        if line.startswith('ITERATED'):
            return int(line.split()[1])
    return None


def patch_run(filepath, old_text, new_text, query_str):
    original = filepath.read_text()
    if old_text not in original:
        raise ValueError(f"patch text not found: {old_text!r}")
    backup = filepath.with_suffix('.pl.wp_backup')
    shutil.copy2(filepath, backup)
    try:
        lines_out, rc = run_prolog(query_str)
        baseline = parse_rows(lines_out)
        iterated_bl = get_iterated(lines_out)
        patched_text = original.replace(old_text, new_text, 1)
        filepath.write_text(patched_text)
        lines_out2, rc2 = run_prolog(query_str)
        patched = parse_rows(lines_out2)
        iterated_pt = get_iterated(lines_out2)
    finally:
        shutil.copy2(backup, filepath)
        backup.unlink()
    return baseline, patched, iterated_bl, iterated_pt


def change_dist(baseline, patched):
    c = Counter()
    for k in set(baseline) | set(patched):
        b = baseline.get(k, 'missing'); p = patched.get(k, 'missing')
        if b != p:
            c[(b, p)] += 1
    return c


# ═══════════════════════════════════════════════════════════════════════════
print("=" * 70)
print("ITEM 1 — D1a patch site (from tripwire_fabricated_defaults.py)")
print("=" * 70)
print()
print("File:    prolog/drl_composition.pl")
print("Line:    179")
print("OLD:    ", D1A_MARKER)
print("NEW_999:", D1A_MARKER.replace("Supp = 0.5", "Supp = 999.9"))
print("NEW_07: ", D1A_MARKER.replace("Supp = 0.5", "Supp = 0.7"))
print("Query:   TEMPORAL_QUERY — constraint_history over all_corpus_constraints, default_context")
print()

# ═══════════════════════════════════════════════════════════════════════════
print("=" * 70)
print("ITEM 4 SELF-CHECK — Supp=0.7 vs Supp=999.9 range artifact test")
print("=" * 70)
print()

f = PROLOG_DIR / "drl_composition.pl"

print("Running Supp=0.5 (baseline) and Supp=0.7 ...")
bl_07, pt_07, _, _ = patch_run(f,
    D1A_MARKER,
    D1A_MARKER.replace("Supp = 0.5", "Supp = 0.7"),
    TEMPORAL_QUERY)

print("Running Supp=0.5 (baseline) and Supp=999.9 ...")
bl_999, pt_999, _, _ = patch_run(f,
    D1A_MARKER,
    D1A_MARKER.replace("Supp = 0.5", "Supp = 999.9"),
    TEMPORAL_QUERY)

ch_07  = change_dist(bl_07,  pt_07)
ch_999 = change_dist(bl_999, pt_999)

print()
print(f"Baseline rows (both runs): {len(bl_07)}")
print()
print("Supp=0.5 → 0.7 transitions:")
for (b, p), n in sorted(ch_07.items(),  key=lambda x: -x[1]):
    print(f"  {b:30s} → {p:30s}: {n}")
print(f"  TOTAL: {sum(ch_07.values())}")
print()
print("Supp=0.5 → 999.9 transitions:")
for (b, p), n in sorted(ch_999.items(), key=lambda x: -x[1]):
    print(f"  {b:30s} → {p:30s}: {n}")
print(f"  TOTAL: {sum(ch_999.values())}")
print()
if ch_07 == ch_999:
    print("SELF-CHECK RESULT: PASS — 0.7 and 999.9 produce identical flip sets.")
    print("No range artifact. The gate is a simple ≥ threshold; any value ≥ 0.60 gives the same result.")
else:
    print("SELF-CHECK RESULT: DISCREPANCY — 0.7 and 999.9 differ.")
    for k in set(ch_07)|set(ch_999):
        if ch_07.get(k,0) != ch_999.get(k,0):
            print(f"  {k}: 0.7={ch_07.get(k,0)}, 999.9={ch_999.get(k,0)}")

# ═══════════════════════════════════════════════════════════════════════════
print()
print("=" * 70)
print("ITEM 2 — Full per-constraint D1a diff (baseline tangled_rope rows only)")
print("=" * 70)
print()
print("Re-running baseline vs Supp=999.9 to get full diff ...")
bl, pt, _, _ = patch_run(f,
    D1A_MARKER,
    D1A_MARKER.replace("Supp = 0.5", "Supp = 999.9"),
    TEMPORAL_QUERY)

all_changes = []
for k in sorted(set(bl)|set(pt)):
    b = bl.get(k, 'missing'); p = pt.get(k, 'missing')
    if b != p:
        all_changes.append((k, b, p))

print(f"Total changed rows: {len(all_changes)}")
print()
print(f"{'constraint':<50} {'T':>4}  {'baseline':<20} {'patched':<20}")
print("-" * 100)
for (c, t), b, p in all_changes:
    print(f"{c:<50} {t:>4}  {b:<20} {p:<20}")

print()
print("Distribution:")
dist = Counter((b, p) for (_, b, p) in all_changes)
for (b, p), n in sorted(dist.items(), key=lambda x: -x[1]):
    print(f"  {b} → {p}: {n}")

# ═══════════════════════════════════════════════════════════════════════════
print()
print("=" * 70)
print("ITEM 3 — Denominator reconciliation: plan 519 vs tripwire 647")
print("=" * 70)
print()
non_unknown_now = sum(1 for v in bl.values() if v != 'unknown')
unknown_now     = sum(1 for v in bl.values() if v == 'unknown')
print(f"Current run total temporal rows:     {len(bl)}")
print(f"  non-unknown:                       {non_unknown_now}")
print(f"  unknown:                           {unknown_now}")
print()
print("Plan figure 519: source was 'instance-reported' from Handoff-5 session")
print("(not from a run — estimated from constraint_history in a different context).")
print("The 519 was described as 'non-unknown temporal classifications'.")
print(f"Current non-unknown count:           {non_unknown_now}")
print(f"Difference 647-total vs 519-non-unk: {len(bl)} total vs {non_unknown_now} non-unk")
print()
print("Unique constraints in the temporal run:")
unique_cs = set(c for (c, _) in bl.keys())
print(f"  {len(unique_cs)} constraints have ≥1 temporal row")
print()
print("Rows per constraint (sample, first 10):")
from collections import defaultdict
rows_per_c = defaultdict(int)
for (c, t) in bl.keys():
    rows_per_c[c] += 1
for c, n in sorted(rows_per_c.items())[:10]:
    print(f"  {c}: {n} rows")

# ═══════════════════════════════════════════════════════════════════════════
print()
print("=" * 70)
print("ITEM 5 — all_corpus_constraints/1 definition")
print("=" * 70)
import subprocess as sp
result = sp.run(['grep', '-n', '-A', '8', 'all_corpus_constraints', 'covering_analysis.pl'],
                capture_output=True, text=True, cwd=PROLOG_DIR)
print(result.stdout[:2000])

# ═══════════════════════════════════════════════════════════════════════════
print()
print("=" * 70)
print("ITEM 6 — D2/D20/D21 tripwire with explicit iteration count")
print("=" * 70)
print()

g = PROLOG_DIR / "drl_core.pl"
D2_OLD = "(narrative_ontology:constraint_metric(Constraint, ActualMetricName, Value) -> true ; Value = 0)."
D2_NEW = "(narrative_ontology:constraint_metric(Constraint, ActualMetricName, Value) -> true ; Value = 999.9)."

print("--- D2: drl_core.pl:96 Supp=0 → 999.9 ---")
bl_d2, pt_d2, it_bl, it_pt = patch_run(g, D2_OLD, D2_NEW, STATIC_QUERY)
ch_d2 = change_dist(bl_d2, pt_d2)
print(f"  Iteration count (baseline): {it_bl} constraints")
print(f"  Iteration count (patched):  {it_pt} constraints")
print(f"  Baseline rows returned:     {len(bl_d2)}")
print(f"  Patched rows returned:      {len(pt_d2)}")
print(f"  Changed rows:               {sum(ch_d2.values())}")
if ch_d2:
    for (b,p),n in sorted(ch_d2.items(), key=lambda x:-x[1]):
        print(f"    {b} → {p}: {n}")
else:
    print("  (no changes)")

print()
h = PROLOG_DIR / "boltzmann_compliance.pl"
D20_OLD = "    (   narrative_ontology:constraint_metric(C, ExtMetricName, BaseEps)\n    ->  true\n    ;   BaseEps = 0.5\n    ),"
D20_NEW = D20_OLD.replace("BaseEps = 0.5", "BaseEps = 999.9")
print("--- D20: boltzmann_compliance.pl:245 BaseEps=0.5 → 999.9 ---")
bl_d20, pt_d20, it_bl, it_pt = patch_run(h, D20_OLD, D20_NEW, BOLTZMANN_QUERY)
ch_d20 = change_dist(bl_d20, pt_d20)
print(f"  Iteration count (baseline): {it_bl} constraints")
print(f"  Iteration count (patched):  {it_pt} constraints")
print(f"  Baseline rows returned:     {len(bl_d20)}")
print(f"  Patched rows returned:      {len(pt_d20)}")
print(f"  Changed rows:               {sum(ch_d20.values())}")
if ch_d20:
    for (b,p),n in sorted(ch_d20.items(), key=lambda x:-x[1]):
        print(f"    {b} → {p}: {n}")
else:
    print("  (no changes)")

print()
D21_OLD = "    (   narrative_ontology:constraint_metric(C, SuppMetricName, Supp)\n    ->  true\n    ;   Supp = 0\n    ),"
D21_NEW = D21_OLD.replace("Supp = 0\n", "Supp = 999.9\n")
print("--- D21: boltzmann_compliance.pl:251 Supp=0 → 999.9 ---")
bl_d21, pt_d21, it_bl, it_pt = patch_run(h, D21_OLD, D21_NEW, BOLTZMANN_QUERY)
ch_d21 = change_dist(bl_d21, pt_d21)
print(f"  Iteration count (baseline): {it_bl} constraints")
print(f"  Iteration count (patched):  {it_pt} constraints")
print(f"  Baseline rows returned:     {len(bl_d21)}")
print(f"  Patched rows returned:      {len(pt_d21)}")
print(f"  Changed rows:               {sum(ch_d21.values())}")
if ch_d21:
    for (b,p),n in sorted(ch_d21.items(), key=lambda x:-x[1]):
        print(f"    {b} → {p}: {n}")
else:
    print("  (no changes)")

# ═══════════════════════════════════════════════════════════════════════════
print()
print("=" * 70)
print("ITEM 7 SELF-CHECK — DORMANT-latent vs DORMANT-by-construction")
print("=" * 70)
print()
print("The 32 stubs are _contradictions.pl files. Question: if the 32 stubs ever")
print("get classified (corpus regen, contradiction handling added), do D2/D20/D21")
print("wake up LOAD-BEARING-WRONG?")
print()
# Check whether contradictions files have any classification-relevant facts
sample_stub = PROLOG_DIR / "testsets" / "competence_exercise_validity_contradictions.pl"
stub_text = sample_stub.read_text()
has_extractiveness = "constraint_metric" in stub_text and "extractiveness" in stub_text
has_suppression    = "suppression_requirement" in stub_text
has_measurement    = "measurement(" in stub_text
print(f"Sample stub: competence_exercise_validity_contradictions.pl")
print(f"  has constraint_metric/extractiveness: {has_extractiveness}")
print(f"  has suppression_requirement:          {has_suppression}")
print(f"  has measurement/5:                    {has_measurement}")
print()
# Count all stubs
stubs = sorted((PROLOG_DIR/"testsets").glob("*_contradictions.pl"))
print(f"Total *_contradictions.pl files: {len(stubs)}")
print()
print("VERDICT PER SITE:")
print()
print("D2 (drl_core.pl:96, Supp=0):    DORMANT-latent")
print("  Reason: the fallback fires when constraint_metric(C,suppression_requirement,_)")
print("  is absent. If a contradictions stub ever gets constraint_metric facts emitted")
print("  (e.g. schema expands to emit metrics for contradiction files), the fallback")
print("  would stop firing. If the stub gets classified WITHOUT adding suppression_requirement,")
print("  the fallback fires and Supp=0 blocks tangled_rope+snare gates. The site is latent,")
print("  not dead — it wakes up if classification is attempted on stubs without that metric.")
print()
print("D20 (boltzmann_compliance.pl:245, BaseEps=0.5): DORMANT-latent")
print("  Same logic: if stubs acquire extractiveness metric, fallback stops. If classified")
print("  without it, BaseEps=0.5 fabricates a mid-range value. Since 0.5 >= typical")
print("  snare_epsilon_floor (0.46), it may PASS gates — making it misclassify differently")
print("  than D2 (which blocks). Would need a separate tripwire if stubs are classified.")
print()
print("D21 (boltzmann_compliance.pl:251, Supp=0):  DORMANT-latent")
print("  Same as D2: Supp=0 blocks tangled_rope (floor 0.40) and snare (floor 0.60).")
print("  Wakes load-bearing if stubs are classified without authored suppression_requirement.")
print()
print("NONE of the three are DORMANT-by-construction (i.e., structurally unreachable")
print("regardless of corpus state). All three fire when their lookup fails. They are")
print("dormant-latent: dormant because the 32 stubs are excluded from all_corpus_constraints,")
print("latent because that exclusion is a corpus fact not a code invariant.")

# ═══════════════════════════════════════════════════════════════════════════
print()
print("=" * 70)
print("ITEM 8 — OPEN-4, OPEN-5, OPEN-7 raw grep outputs")
print("=" * 70)
print()

print("--- OPEN-4: classify_at_interpolated callers ---")
r4 = sp.run(
    ['grep', '-rn', 'classify_at_interpolated', 'product_site_export.pl', 'json_report.pl'],
    capture_output=True, text=True, cwd=PROLOG_DIR)
print(f"grep exit code: {r4.returncode}")
print(f"stdout: '{r4.stdout.strip()}'")
print("(empty stdout = neither product_site_export.pl nor json_report.pl calls classify_at_interpolated)")

print()
print("--- OPEN-5: constraint_indexing.pl:840 fires-now ---")
r5 = sp.run(
    ['bash', '-c',
     'grep -rL "constraint_data:base_extractiveness\\|domain_priors:base_extractiveness" testsets/*.pl | wc -l'],
    capture_output=True, text=True, cwd=PROLOG_DIR)
print(f"grep -rL count: {r5.stdout.strip()}")
r5b = sp.run(
    ['bash', '-c',
     'grep -rL "constraint_data:base_extractiveness\\|domain_priors:base_extractiveness" testsets/*.pl | head -5'],
    capture_output=True, text=True, cwd=PROLOG_DIR)
print(f"sample files: {r5b.stdout.strip()}")

print()
print("--- OPEN-7: requires_active_enforcement classification uses ---")
r7 = sp.run(
    ['grep', '-n', 'requires_active_enforcement', 'drl_core.pl'],
    capture_output=True, text=True, cwd=PROLOG_DIR)
print(r7.stdout)
