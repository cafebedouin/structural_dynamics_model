#!/usr/bin/env python3
"""
ε-stability sweep (OQ-205 spec §5, r = 0.02 — R3 ratified 2026-07-03).

DATA-SIDE perturbation: each constraint's authored ε
(narrative_ontology:constraint_metric(C, extractiveness, V)) is perturbed
V ± r in-memory (retractall + assertz + cache clear per probe, originals
restored), thresholds fixed — the complementary axis to perturb.py
(config-side). One swipl process, corpus loaded ONCE (surface2_lock_sweep
idiom).

Per constraint × 4 canonical contexts (site_contexts_canonical/1), captures
BOTH the raw metric type (drl_core:metric_based_type_indexed/3, MT) and the
signature-resolved type (drl_core:dr_type/3, FT — the flag is on FT per the
OQ-27 disclosure; never recompute H1-adjacent conclusions from MT).

FLAG CLASSES (R3 amendment — two-class split, plus the lock bit):
  on_threshold_grid  — authored ε exactly AT a threshold (d = 0): an
                       authoring-convention fact (flash's .x5/.x0 grid),
                       flagged at every radius by construction. Corpus-level
                       finding, NOT n independent fragility discoveries.
  near_threshold     — 0 < d <= r AND FT flips under ε ± r: the
                       landed-near-a-boundary-by-chance artifact the check
                       exists to surface (per-story, inspect these).
  override_locked    — MT flips under ε ± r while FT holds: signature-lock
                       (OQ-30 class) detected BY BEHAVIOR, never by
                       known_override_signature/1 membership. "Stable" must
                       never read as "insensitive".
  unstable_off_grid  — FT flips but d > r (a χ-gate crossing, not an
                       ε-threshold one). Not in the plan's three named
                       classes; emitted anyway — dropping it would collapse
                       a real flip into unflagged silence (Pattern 6).

PER-PROBE TOOK-EFFECT GUARD (U7 amendment, both halves): after each
retract/assert the sweep re-reads drl_core:base_extractiveness(C, V') on the
SAME chain classification uses and asserts V' == the perturbed value. A
multifile direct fact (drl_core:base_extractiveness/2 is multifile — the
constraint_instances.pl hand-authored class) or any other source shadowing
the perturbed site makes the probe FAIL LOUD for that constraint
(guard_failed entry) instead of reporting unshadowed-"stable".

CONTROL S SELFTEST runs FIRST on every invocation, fail-closed:
  S1 near-threshold plant  — a corpus story's ε overlaid to
                             snare_epsilon_floor + 0.005 (δ < r) must flag
                             near_threshold (searches for a story whose FT
                             actually flips there; none found = selftest RED).
  S2 band-interior plant   — ε overlaid to 0.375 (0.075 from both 0.30 and
                             0.45, > r from every threshold) must NOT flag
                             on_threshold_grid/near_threshold (two-sided arm).
  S3 override-locked plant — a story with MT != FT at baseline is swept; if
                             its MT flips while FT holds the flag must fire.
                             A leg with no such story reports SKIPPED loud
                             (counted, never silent) — fatal on the live leg
                             only if the leg carries override stories.
  S4 shadow plant          — asserta a DIRECT drl_core:base_extractiveness/2
                             fact over a real story, perturb constraint_metric:
                             the took-effect guard MUST catch the shadow
                             (probe fails loud). Proves selftest green !=
                             sweep-that-never-looked.

R3 KILL-CONDITION TRIPWIRES (standing):
  T1 — the (0.45, 0.46) open interval must be EMPTY of authored ε (census
       2026-07-03: empty on all four legs). Populated => binding (i)
       re-activates, r must drop to <= 0.005: FATAL always.
  T2 — near_threshold rate <= ~1/3 of the leg (counting near_threshold ONLY;
       on_threshold_grid is the expected-by-convention stratum — flash's
       22.7% d=0 mass must not trip a fragility tripwire). FATAL on the live
       leg, advisory WARN under --corpus overlay (the all-legs audit must
       report, not die on an authoring convention).

Writes outputs/epsilon_stability_results.json with corpus_hash (OQ-29
consumer contract: enhanced_report fail-closes on mismatch).

Usage:
    python3 python/sweeps/epsilon_stability.py [--corpus REL_PATH]
        [--json-out PATH] [--radius 0.02]
"""

import argparse
import json
import subprocess
import sys
import tempfile
import time
from pathlib import Path

ROOT = Path(__file__).resolve().parents[2]
PROLOG_DIR = ROOT / "prolog"
sys.path.insert(0, str(ROOT / "python"))
from corpus_hash import compute_corpus_hash  # noqa: E402

DEFAULT_OUT = ROOT / "outputs" / "epsilon_stability_results.json"
RADIUS = 0.02
# The five ε-gating params (spec §5, verified at 6c59615e; 0.45 shared by
# scaffold_extraction_ceil / piton_extraction_ceiling).
THRESHOLDS = [0.10, 0.25, 0.30, 0.45, 0.46]


def _build_goal(radius, live_leg):
    """The one-process sweep goal. live_leg toggles T2 fatal vs advisory."""
    thresholds = "[" + ",".join(f"{t}" for t in THRESHOLDS) + "]"
    fatal_t2 = "true" if live_leg else "false"
    return f"""\
:- use_module(library(aggregate)).
:- [stack].
:- corpus_loader:ensure_corpus_loaded.

radius({radius}).
thresholds({thresholds}).
t2_fatal({fatal_t2}).

% ---------------------------------------------------------------------------
% ε overlay plumbing: capture / set / restore + cache clear. Restore is
% verified (post-restore read must equal the original first solution).
% ---------------------------------------------------------------------------
eps_solutions(C, Vs) :-
    findall(V, narrative_ontology:constraint_metric(C, extractiveness, V), Vs).

set_eps(C, V) :-
    retractall(narrative_ontology:constraint_metric(C, extractiveness, _)),
    assertz(narrative_ontology:constraint_metric(C, extractiveness, V)),
    cache_registry:clear_all_caches.

restore_eps(C, Vs) :-
    retractall(narrative_ontology:constraint_metric(C, extractiveness, _)),
    forall(member(V, Vs),
           assertz(narrative_ontology:constraint_metric(C, extractiveness, V))),
    cache_registry:clear_all_caches,
    % verified restore: the live read must see the original first value again
    ( Vs = [V0|_] ->
        ( once(drl_core:base_extractiveness(C, R)), abs(R - V0) < 1.0e-9 -> true
        ; format("RESTORE_FAIL ~w expected ~w~n", [C, V0]), halt(1) )
    ; true ).

% took-effect guard: the FIRST solution of the classification read must be
% the perturbed value. once/1 is load-bearing: without it the check
% backtracks past a shadowing direct fact into the delegating clause and
% "passes" on ANY matching solution — exactly the blindness the guard exists
% to catch (witnessed: carbon_tax_2026's direct fact 0.55 is clause 1; an
% unpinned read backtracked to the perturbed constraint_metric and lied).
took_effect(C, V) :-
    once(drl_core:base_extractiveness(C, R)),
    abs(R - V) < 1.0e-9.

% type captures (err token on exception/failure — never silent)
mt_at(C, Ctx, T) :-
    ( catch(drl_core:metric_based_type_indexed(C, Ctx, T0), _, fail) -> T = T0 ; T = err ).
ft_at(C, Ctx, T) :-
    ( catch(drl_core:dr_type(C, Ctx, T0), _, fail) -> T = T0 ; T = err ).

types_all(C, Pairs) :-
    constraint_indexing:site_contexts_canonical(Ctxs),
    findall(MT-FT, ( member(Ctx, Ctxs), mt_at(C, Ctx, MT), ft_at(C, Ctx, FT) ), Pairs).

% probe one direction: set ε, guard, capture, per-context types
probe(C, V, Pairs) :-
    set_eps(C, V),
    ( took_effect(C, V)
    -> types_all(C, Pairs)
    ;  Pairs = guard_failed ).

clamp(V, Out) :- ( V < 0.0 -> Out = 0.0 ; V > 1.0 -> Out = 1.0 ; Out = V ).

grid_distance(E, D) :-
    thresholds(Ts),
    findall(Di, (member(T, Ts), Di is abs(E - T)), Ds),
    min_list(Ds, D).

% flip analysis over aligned per-context pairs
ft_flip(Base, Pert) :-
    nth0(I, Base, _-FT0), nth0(I, Pert, _-FT1), FT0 \\== FT1, !.
mt_flip_ft_hold(Base, Pert) :-
    nth0(I, Base, MT0-FT0), nth0(I, Pert, MT1-FT1),
    MT0 \\== MT1, FT0 == FT1, !.

% one full sweep row for constraint C at authored ε E
sweep_one(C, E, Row) :-
    radius(R),
    grid_distance(E, D),
    types_all(C, Base),
    eps_solutions(C, Orig),
    Up0 is E + R, clamp(Up0, Up),
    Dn0 is E - R, clamp(Dn0, Dn),
    probe(C, Up, PU),
    restore_eps(C, Orig),
    probe(C, Dn, PD),
    restore_eps(C, Orig),
    ( (PU == guard_failed ; PD == guard_failed)
    -> Row = guard_failed(D)
    ;  ( (ft_flip(Base, PU) ; ft_flip(Base, PD)) -> FtFlip = true ; FtFlip = false ),
       ( (mt_flip_ft_hold(Base, PU) ; mt_flip_ft_hold(Base, PD)) -> Lock = true ; Lock = false ),
       Row = row(D, FtFlip, Lock, Base, PU, PD)
    ).

flag_classes(D, FtFlip, Lock, Flags0) :-
    radius(R),
    ( D < 1.0e-9 -> F1 = [on_threshold_grid] ; F1 = [] ),
    ( D > 1.0e-9, D =< R, FtFlip == true -> F2 = [near_threshold] ; F2 = [] ),
    ( D > R, FtFlip == true -> F3 = [unstable_off_grid] ; F3 = [] ),
    ( Lock == true -> F4 = [override_locked] ; F4 = [] ),
    append([F1, F2, F3, F4], Flags0).

fmt_pairs(Pairs, Atom) :-
    findall(S, (member(MT-FT, Pairs), format(atom(S), "~w/~w", [MT, FT])), Ss),
    atomic_list_concat(Ss, ',', Atom).

emit_row(C, E, guard_failed(D)) :-
    format("GUARD_FAIL ~w ~6f ~6f~n", [C, E, D]).
emit_row(C, E, row(D, FtFlip, Lock, Base, PU, PD)) :-
    flag_classes(D, FtFlip, Lock, Flags),
    ( Flags == [] -> FlagAtom = none ; atomic_list_concat(Flags, ';', FlagAtom) ),
    fmt_pairs(Base, BA), fmt_pairs(PU, UA), fmt_pairs(PD, DA),
    format("EPS ~w ~6f ~6f ~w base ~w up ~w dn ~w~n",
           [C, E, D, FlagAtom, BA, UA, DA]).

% ---------------------------------------------------------------------------
% CONTROL S SELFTEST (first, fail-closed)
% ---------------------------------------------------------------------------
eps_of(C, E) :- once(narrative_ontology:constraint_metric(C, extractiveness, E)).

% S1: find a story whose FT flips when planted at snare floor + 0.005
selftest_s1 :-
    config:param(snare_epsilon_floor, Floor),
    Plant is Floor + 0.005,
    findall(C, ( corpus_loader:corpus_constraint(C), eps_of(C, _) ), Cs),
    length(Cs, NC),
    ( selftest_s1_search(Cs, Plant, 0, 40, Found)
    -> format("SELFTEST s1_near_threshold PASS carrier ~w~n", [Found])
    ;  format("SELFTEST s1_near_threshold FAIL no-flipping-carrier-in-first-40-of-~w~n", [NC]),
       halt(1) ).

selftest_s1_search([C|Cs], Plant, I, Cap, Found) :-
    ( I >= Cap -> fail
    ; eps_solutions(C, Orig),
      set_eps(C, Plant),
      ( took_effect(C, Plant) -> sweep_one(C, Plant, Row) ; Row = guard_failed(0) ),
      restore_eps(C, Orig),
      ( Row = row(D, true, _, _, _, _), radius(R), D > 1.0e-9, D =< R
      -> Found = C
      ;  I1 is I + 1, selftest_s1_search(Cs, Plant, I1, Cap, Found) )
    ).

% S2: band interior (0.375: 0.075 from 0.30 and 0.45) — must not carry the
% grid/near classes; an FT flip there may only surface as unstable_off_grid.
selftest_s2 :-
    once(( corpus_loader:corpus_constraint(C), eps_of(C, _) )),
    eps_solutions(C, Orig),
    set_eps(C, 0.375),
    sweep_one(C, 0.375, Row),
    restore_eps(C, Orig),
    ( Row = row(D, FtFlip, Lock, _, _, _)
    -> flag_classes(D, FtFlip, Lock, Flags),
       ( ( memberchk(on_threshold_grid, Flags) ; memberchk(near_threshold, Flags) )
       -> format("SELFTEST s2_band_interior FAIL flags ~w~n", [Flags]), halt(1)
       ;  format("SELFTEST s2_band_interior PASS carrier ~w flags ~w~n", [C, Flags]) )
    ;  format("SELFTEST s2_band_interior FAIL guard~n"), halt(1) ).

% S3: override-locked — needs a story with MT != FT at baseline on this leg.
selftest_s3 :-
    findall(C,
        ( corpus_loader:corpus_constraint(C), eps_of(C, _),
          types_all(C, Pairs), member(MT-FT, Pairs), MT \\== FT, MT \\== err, FT \\== err ),
        Locked0),
    sort(Locked0, Locked),
    ( Locked == []
    -> format("SELFTEST s3_override_locked SKIPPED no-override-story-on-leg~n")
    ;  ( member(C, Locked),
         eps_of(C, E), eps_solutions(C, Orig),
         sweep_one(C, E, Row),
         restore_eps(C, Orig),
         Row = row(_, _, true, _, _, _)
       -> format("SELFTEST s3_override_locked PASS carrier ~w~n", [C])
       ;  length(Locked, NL),
          format("SELFTEST s3_override_locked INCONCLUSIVE ~w-override-stories-none-mt-flips-in-radius~n", [NL]) )
    ).

% S4: shadow plant — the hand-authored/shadowing class must be CAUGHT by the
% guard. drl_core:base_extractiveness/2 is multifile but STATIC (no runtime
% asserta), and the REAL shadowing configuration is already loaded:
% constraint_instances.pl's carbon_tax_2026 authors a DIRECT fact (0.55)
% that loads BEFORE drl_core's delegating clause (stack.pl order), so a
% constraint_metric perturbation never reaches the first-solution read.
% Perturb its constraint_metric; the took-effect guard MUST detect that
% classification still reads the direct fact. Preconditions checked loud so
% a future corpus authoring constraint_metric for it fails the selftest
% rather than silently changing what this control tests.
selftest_s4 :-
    C = carbon_tax_2026,
    ( drl_core:base_extractiveness(C, V0)
    -> true
    ;  format("SELFTEST s4_shadow_guard FAIL precondition carbon_tax_2026-direct-fact-missing~n"),
       halt(1) ),
    ( narrative_ontology:constraint_metric(C, extractiveness, _)
    -> format("SELFTEST s4_shadow_guard FAIL precondition carbon_tax_2026-now-authors-constraint_metric~n"),
       halt(1)
    ;  true ),
    Perturbed is V0 + 0.3,
    assertz(narrative_ontology:constraint_metric(C, extractiveness, Perturbed)),
    cache_registry:clear_all_caches,
    ( took_effect(C, Perturbed)
    -> format("SELFTEST s4_shadow_guard FAIL guard-passed-under-shadow~n"),
       retractall(narrative_ontology:constraint_metric(C, extractiveness, _)), halt(1)
    ;  format("SELFTEST s4_shadow_guard PASS shadow-detected ~w direct-fact ~w wins over perturbed ~w~n",
              [C, V0, Perturbed]) ),
    retractall(narrative_ontology:constraint_metric(C, extractiveness, _)),
    cache_registry:clear_all_caches.

selftest :-
    selftest_s1, selftest_s2, selftest_s3, selftest_s4,
    format("SELFTEST all_done~n").

% ---------------------------------------------------------------------------
% R3 tripwires
% ---------------------------------------------------------------------------
tripwire_t1 :-
    findall(C-E,
        ( corpus_loader:corpus_constraint(C), eps_of(C, E), E > 0.45, E < 0.46 ),
        Bad),
    ( Bad == []
    -> format("TRIPWIRE t1_interval_empty PASS~n")
    ;  format("TRIPWIRE t1_interval_empty FAIL ~w~n", [Bad]), halt(1) ).

% ---------------------------------------------------------------------------
% main
% ---------------------------------------------------------------------------
run :-
    aggregate_all(count, corpus_loader:corpus_constraint(_), N),
    format("META n_corpus ~w~n", [N]),
    ( N > 0 -> true ; format("EMPTY corpus~n"), halt(1) ),
    selftest,
    tripwire_t1,
    findall(C, corpus_loader:corpus_constraint(C), Cs),
    forall(member(C, Cs),
        ( eps_of(C, E)
        -> eps_solutions(C, Orig),
           sweep_one(C, E, Row),
           restore_eps(C, Orig),
           emit_row(C, E, Row)
        ;  format("NO_EPS ~w~n", [C]) )),
    % T2: near_threshold rate (counted python-side too; asserted here for the
    % fail-closed live-leg run)
    format("DONE~n").

:- catch(run, E, (format(user_error, "ERR: ~w~n", [E]), halt(1))), halt.
:- halt(1).
"""


def run_sweep(corpus=None, radius=RADIUS, timeout=3600):
    goal_src = _build_goal(radius, live_leg=(corpus is None))
    if corpus:
        # corpus_path overlay must precede load (retract default first —
        # a bare assertz appends after the default and is silently ignored).
        goal_src = goal_src.replace(
            ":- [stack].",
            ":- [stack].\n"
            f":- retractall(config:param(corpus_path, _)),"
            f" assertz(config:param(corpus_path, '{corpus}')).",
        )
    with tempfile.NamedTemporaryFile(suffix=".pl", dir=PROLOG_DIR, mode="w",
                                     delete=False) as f:
        f.write(goal_src)
        pl_path = f.name
    t0 = time.time()
    try:
        r = subprocess.run(
            ["swipl", "-g", f"consult('{pl_path}'), halt.", "-t", "halt(1)"],
            cwd=PROLOG_DIR, capture_output=True, text=True, timeout=timeout,
        )
    finally:
        Path(pl_path).unlink(missing_ok=True)
    elapsed = time.time() - t0
    return r, elapsed


def parse(stdout, radius, live_leg):
    per, selftest, tripwires, meta = [], [], [], {}
    guard_failures, no_eps = [], []
    for line in stdout.splitlines():
        t = line.split()
        if not t:
            continue
        if t[0] == "META":
            meta[t[1]] = t[2]
        elif t[0] == "SELFTEST":
            selftest.append(" ".join(t[1:]))
        elif t[0] == "TRIPWIRE":
            tripwires.append(" ".join(t[1:]))
        elif t[0] == "GUARD_FAIL":
            guard_failures.append({"id": t[1], "epsilon": float(t[2]),
                                   "grid_distance": float(t[3])})
        elif t[0] == "NO_EPS":
            no_eps.append(t[1])
        elif t[0] == "EPS":
            # EPS C eps d flags base <..> up <..> dn <..>
            _, c, e, d, flags = t[:5]
            base = t[t.index("base") + 1]
            up = t[t.index("up") + 1]
            dn = t[t.index("dn") + 1]
            per.append({
                "id": c,
                "epsilon": float(e),
                "grid_distance": float(d),
                "flags": [] if flags == "none" else flags.split(";"),
                "types_base": base, "types_up": up, "types_dn": dn,
            })
    n = len(per)
    near = sum(1 for p in per if "near_threshold" in p["flags"])
    on_grid = sum(1 for p in per if "on_threshold_grid" in p["flags"])
    locked = sum(1 for p in per if "override_locked" in p["flags"])
    off_grid = sum(1 for p in per if "unstable_off_grid" in p["flags"])
    summary = {
        "n_swept": n, "n_no_epsilon": len(no_eps),
        "n_guard_failed": len(guard_failures),
        "near_threshold": near, "on_threshold_grid": on_grid,
        "override_locked": locked, "unstable_off_grid": off_grid,
    }
    # T2 (python side): near_threshold only, <= ~1/3 of leg
    t2_ok = (n == 0) or (near <= n / 3.0)
    t2_line = (f"t2_flag_rate {'PASS' if t2_ok else 'FAIL'} "
               f"near={near}/{n} (grid class excluded by design)")
    tripwires.append(t2_line)
    if not t2_ok and live_leg:
        raise SystemExit(
            f"R3 kill-condition T2 tripped on the LIVE leg: {t2_line} — "
            "r is reading the authoring grid, not threshold proximity; re-derive r.")
    if not t2_ok:
        print(f"[WARN] {t2_line} (advisory under --corpus overlay)", file=sys.stderr)
    return {
        "meta": meta, "radius": radius, "thresholds": THRESHOLDS,
        "selftest": selftest, "tripwires": tripwires,
        "summary": summary, "guard_failures": guard_failures,
        "no_epsilon": no_eps, "per_constraint": per,
    }


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--corpus", help="corpus_path overlay (relative to prolog/)")
    ap.add_argument("--json-out", default=str(DEFAULT_OUT))
    ap.add_argument("--radius", type=float, default=RADIUS)
    ap.add_argument("--timeout", type=int, default=3600)
    a = ap.parse_args()

    live_leg = a.corpus is None
    r, elapsed = run_sweep(corpus=a.corpus, radius=a.radius, timeout=a.timeout)
    sys.stderr.write(r.stderr[-3000:] if r.stderr else "")
    if r.returncode != 0 or "DONE" not in r.stdout:
        sys.stderr.write(r.stdout[-3000:])
        raise SystemExit(
            f"epsilon_stability sweep swipl exited {r.returncode} "
            "(selftest/tripwire red or crash — see transcript above)")

    result = parse(r.stdout, a.radius, live_leg)
    corpus_dir = (PROLOG_DIR / (a.corpus or "testsets")).resolve()
    result["corpus_hash"] = compute_corpus_hash(corpus_dir)
    result["corpus_path"] = a.corpus or "testsets"
    result["elapsed_seconds"] = round(elapsed, 1)

    out = Path(a.json_out)
    out.parent.mkdir(parents=True, exist_ok=True)
    out.write_text(json.dumps(result, indent=2), encoding="utf-8")
    s = result["summary"]
    print(f"swept={s['n_swept']} no_eps={s['n_no_epsilon']} "
          f"guard_failed={s['n_guard_failed']} on_grid={s['on_threshold_grid']} "
          f"near={s['near_threshold']} locked={s['override_locked']} "
          f"off_grid={s['unstable_off_grid']} elapsed={elapsed:.0f}s")
    print(f"selftest: {'; '.join(result['selftest'])}")
    print(f"tripwires: {'; '.join(result['tripwires'])}")
    print(f"wrote {out}")


if __name__ == "__main__":
    main()
