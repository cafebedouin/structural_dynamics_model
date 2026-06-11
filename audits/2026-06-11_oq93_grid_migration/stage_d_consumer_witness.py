#!/usr/bin/env python3
"""Stage D witness — OQ-93 consumer wiring two-sided control + fire-on-migration.

Pre-registered (PREREGISTRATION.md):
  1. SIGNAL two-sided: level_gradient_divergence/2 FIRES on the divergent
     probe story; SILENT on flat-structural genuine mountain, rising, falling.
  2. FCR end-to-end: rope-appearing fixture + divergent grid -> false_ci_rope
     fires with level_gradient_divergence(GS, GI) in FailedTests; flat twin's
     evidence carries NO divergence term (whatever its other tests do).
  3. FSM end-to-end: mountain+beneficiary fixture + divergent grid ->
     fsm_evidence(_, _, divergence(_,_)) with confidence bumped one rung
     (low->medium at BCount=1, CScore=0); flat twin -> LevelDiv=open,
     confidence low (identical to pre-wiring behavior).
  4. Extraction-blindness omega: question carries the witnessed-process tail
     on the divergent FCR fixture's gap... (omega needs the snare/rope gap —
     witnessed via direct omega_from_gap call on the fixture).
  5. Fire-on-migration (i): Tn-only 16/32 fixture through load_and_run shows
     kappa with [CONDITIONAL: grid authored 16/32].
  6. Fire-on-migration (ii): moderate->yellow cap — corpus census recorded
     (fired-or-why-not).

Run from repo root:
  python3 audits/2026-06-11_oq93_grid_migration/stage_d_consumer_witness.py
"""
import subprocess
import sys
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
AUDIT = ROOT / "audits/2026-06-11_oq93_grid_migration"
PROBES = ROOT / "audits/2026-06-10_oq93_grid_viability_probe/stories"

results = []


def rec(label, ok, detail=""):
    results.append((label, "PASS" if ok else "FAIL", detail))


def swipl(goal, timeout=300):
    p = subprocess.run(["swipl", "-g", goal, "-t", "halt(1)"],
                       cwd=ROOT / "prolog", capture_output=True, text=True,
                       timeout=timeout)
    return p


# 1. SIGNAL two-sided over the five probe stories
for story, expect_fire in [("grid_probe_divergent", True),
                           ("grid_probe_flat_authored", False),
                           ("grid_probe_rising", False),
                           ("grid_probe_falling", False)]:
    goal = (
        "use_module(narrative_ontology), use_module(signature_detection), "
        f"consult('{PROBES}/{story}.pl'), "
        f"( signature_detection:level_gradient_divergence({story}, D) "
        f"-> format('SIG fired ~w~n', [D]) ; format('SIG silent~n', []) ), halt."
    )
    p = swipl(goal)
    out = [l for l in p.stdout.splitlines() if l.startswith("SIG ")]
    out = out[-1] if out else f"NO-OUT {p.stderr.strip()[-120:]}"
    ok = ("fired" in out) == expect_fire
    rec(f"1 signal {'FIRES' if expect_fire else 'silent'} on {story}", ok, out)

# 2. FCR end-to-end
goal = (
    "use_module(narrative_ontology), use_module(signature_detection), "
    f"consult('{AUDIT}/fcr_div_fixture.pl'), "
    "( signature_detection:false_ci_rope(fcr_div_fixture, fcr_evidence(App, FT, _, _, _, _)) "
    "-> format('FCR fired app=~w tests=~w~n', [App, FT]) ; format('FCR silent~n', []) ), halt."
)
p = swipl(goal)
out = [l for l in p.stdout.splitlines() if l.startswith("FCR ")]
out = out[-1] if out else f"NO-OUT {p.stderr.strip()[-150:]}"
rec("2a FCR fires on divergent rope-appearing fixture with divergence in FailedTests",
    "FCR fired" in out and "level_gradient_divergence(" in out, out)

goal = (
    "use_module(narrative_ontology), use_module(signature_detection), "
    f"consult('{AUDIT}/fcr_flat_fixture.pl'), "
    "( signature_detection:false_ci_rope(fcr_flat_fixture, fcr_evidence(_, FT, _, _, _, _)) "
    "-> format('FCR fired tests=~w~n', [FT]) ; format('FCR silent~n', []) ), halt."
)
p = swipl(goal)
out = [l for l in p.stdout.splitlines() if l.startswith("FCR ")]
out = out[-1] if out else f"NO-OUT {p.stderr.strip()[-150:]}"
rec("2b flat twin carries NO divergence term (silent or divergence-free tests)",
    "level_gradient_divergence(" not in out, out)

# 3. FSM end-to-end + confidence rung
for fx, expect_div, expect_conf in [("fsm_div_fixture", True, "medium"),
                                    ("fsm_flat_fixture", False, "low")]:
    goal = (
        "use_module(narrative_ontology), use_module(signature_detection), "
        f"consult('{AUDIT}/{fx}.pl'), "
        f"( signature_detection:false_summit_mountain({fx}, fsm_evidence(BC, CS, LD)) "
        f"-> signature_detection:signature_confidence({fx}, false_summit_mountain, Conf), "
        f"   format('FSM fired bc=~w cs=~w ld=~w conf=~w~n', [BC, CS, LD, Conf]) "
        f"; format('FSM silent~n', []) ), halt."
    )
    p = swipl(goal)
    out = [l for l in p.stdout.splitlines() if l.startswith("FSM ")]
    out = out[-1] if out else f"NO-OUT {p.stderr.strip()[-150:]}"
    want_ld = "ld=divergence(" if expect_div else "ld=open"
    rec(f"3 FSM on {fx}: LevelDiv {'divergence' if expect_div else 'open'}, "
        f"confidence {expect_conf}",
        "FSM fired" in out and want_ld in out and f"conf={expect_conf}" in out, out)

# 4. extraction-blindness omega witnessed-process tail (direct omega_from_gap)
goal = (
    "use_module(narrative_ontology), use_module(signature_detection), "
    "use_module(report_generator), "
    f"consult('{AUDIT}/fcr_div_fixture.pl'), "
    "report_generator:omega_from_gap(fcr_div_fixture, gap(snare_masked_as_rope, snare, rope), OID, _, Q), "
    "format('OMEGA ~w :: ~w~n', [OID, Q]), halt."
)
p = swipl(goal)
out = [l for l in p.stdout.splitlines() if l.startswith("OMEGA ")]
out = out[-1] if out else f"NO-OUT {p.stderr.strip()[-150:]}"
rec("4 extraction-blindness omega carries witnessed-process tail on divergent fixture",
    "witnessed process" in out and "level-gradient crossing" in out, out[:220])

# 5. Fire-on-migration (i): CONDITIONAL kappa tag on 16/32 Tn-only fixture
goal = (
    "use_module(scenario_manager), "
    f"scenario_manager:load_and_run('{AUDIT}/grid_probe_tn_only.pl', grid_probe_tn_only), halt."
)
p = swipl(goal, timeout=600)
tag_ok = "[CONDITIONAL: grid authored 16/32]" in p.stdout
kappa_line = [l for l in p.stdout.splitlines() if "Aggregate Magnitude" in l]
(AUDIT / "stage_d_tn_only_run.txt").write_text(p.stdout[-8000:])
rec("5 fire-on-migration (i): kappa [CONDITIONAL: grid authored 16/32] tag FIRES",
    tag_ok, kappa_line[-1].strip() if kappa_line else "no kappa line")

# 6. Fire-on-migration (ii): moderate->yellow cap census (fired-or-why-not)
goal = (
    "[stack], corpus_loader:load_all_testsets, "
    "findall(C-G-B, ( corpus_loader:corpus_constraint(C), "
    "  catch(signature_detection:signature_grade(C, G), _, fail), G == correction, "
    "  catch(( diagnostic_summary:diagnostic_summary(C, S), "
    "          diagnostic_summary:verdict_join(C, S, verdict_join(_, B, _, _, _, _, _)) ), _, fail) ), L), "
    "aggregate_all(count, ( member(_-_-B2, L), B2 == green ), NGreenBase), "
    "length(L, NCorr), "
    "format('CAPCENSUS corr=~w green_base=~w detail=~w~n', [NCorr, NGreenBase, L]), halt."
)
p = swipl(goal, timeout=900)
out = [l for l in p.stdout.splitlines() if l.startswith("CAPCENSUS")]
out = out[-1] if out else f"NO-OUT {p.stderr.strip()[-200:]}"
rec("6 fire-on-migration (ii): cap census recorded (fires iff any correction-grade "
    "carrier has base green)", out.startswith("CAPCENSUS"), out[:400])

for label, verdict, detail in results:
    print(f"  [{verdict}] {label}" + (f" — {detail}" if detail else ""))
n_fail = sum(1 for _, v, _ in results if v == "FAIL")
print(f"{len(results) - n_fail}/{len(results)} witness cases pass")
sys.exit(1 if n_fail else 0)
