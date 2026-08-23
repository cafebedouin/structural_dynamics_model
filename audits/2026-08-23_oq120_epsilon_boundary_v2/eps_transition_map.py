#!/usr/bin/env python3
"""
OQ-120 Phase 0 — ε TRANSITION MAP (audit fork, not pipeline apparatus).

A FORK of python/sweeps/epsilon_stability.py, not an edit of it. That file is
wired apparatus (run_pipeline.py:1359/:1457; consumers enrich_pipeline_json.py,
enhanced_report.py, epsilon_authorship_readout.py) with a FATAL T2 tripwire a
grid sweep would trip. Copied verbatim from it: eps_solutions/2, set_eps/2,
restore_eps/2, took_effect/2 (INCLUDING the once/1 — it guards the shadowing
direct fact at constraint_instances.pl:152), mt_at/3, ft_at/3 with err tokens,
selftest S4 (here as control C3), the retractall+assertz corpus overlay, and
compute_corpus_hash. Deliberately NOT copied: radius/1, flag_classes/4,
sweep_one/3, T2, DEFAULT_OUT.

WHAT IT MEASURES. Per story, per canonical seat, over a grid of ε values: the
raw metric type (MT), the signature-resolved type (FT), and a vector of
independently-evaluated GATE BITS. Per located transition it emits the SET of
bits that changed (`deciding_gates`) — never a scalar "the deciding gate".
Crossing 0.46 flips snare_epsilon_floor f->t, coalition_fired f->t AND
snare_chi_floor t->f at once; a scalar attributor would have to pick one, which
is a narrative, not a measurement.

NON-AUTHORING-FACING (OQ-78 ruling 3, ISSUES.md:3585). The output contains exact
ε values at which types flip. It must never feed a prompt, a seed file, or
`epsilon_bin`. --json-out is required, has no default, and refuses to resolve
into outputs/.

Usage:
    eps_transition_map.py --corpus testsets_haiku --json-out PATH [--stories-only]
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

# The 16 gate bits, in emission order. Derived by reading drl_core.pl:389-472
# clause by clause, not from the plan's estimate of 13 — the derived set is the
# witness. scaffold_extraction_ceil and piton_extraction_ceiling are both 0.45
# and therefore always co-move; kept distinct because they are distinct gates.
GATE_BITS = [
    "piton_epsilon_floor", "snare_epsilon_floor", "rope_epsilon_ceiling",
    "tangled_rope_epsilon_floor", "naturalized_epsilon_floor",
    "snare_chi_floor", "rope_chi_ceiling", "rope_chi_nonpositive",
    "tangled_rope_chi_floor", "tangled_rope_chi_ceil",
    "scaffold_extraction_ceil", "piton_extraction_ceiling",
    "naturalized_chi_ceiling",
    "snare_suppression_floor", "tangled_rope_suppression_floor",
    "coalition_fired",
]

# C4's carrier: naturally-arising, on the haiku leg, suppression 0.28 (< the
# 0.60 snare floor), so the snare gate fails on suppression at EVERY ε. The
# two-sided arm of the attributor itself.
C4_CARRIER = "copyright_constitutional_mandate__judicial_ambiguity_reading"

PROLOG_SRC = r"""
:- use_module(library(aggregate)).
:- [stack].
:- corpus_loader:ensure_corpus_loaded.

% ---------------------------------------------------------------------------
% ε overlay plumbing — COPIED VERBATIM from python/sweeps/epsilon_stability.py.
% Restore is verified (post-restore read must equal the original first value).
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
    ( Vs = [V0|_] ->
        ( once(drl_core:base_extractiveness(C, R)), abs(R - V0) < 1.0e-9 -> true
        ; format("RESTORE_FAIL ~w expected ~w~n", [C, V0]), halt(1) )
    ; true ).

% took-effect guard. once/1 is LOAD-BEARING: without it the check backtracks
% past a shadowing direct fact into the delegating clause and "passes".
took_effect(C, V) :-
    once(drl_core:base_extractiveness(C, R)),
    abs(R - V) < 1.0e-9.

mt_at(C, Ctx, T) :-
    ( catch(drl_core:metric_based_type_indexed(C, Ctx, T0), _, fail) -> T = T0 ; T = err ).
ft_at(C, Ctx, T) :-
    ( catch(drl_core:dr_type(C, Ctx, T0), _, fail) -> T = T0 ; T = err ).

% ---------------------------------------------------------------------------
% GATE BITS — each evaluated INDEPENDENTLY of the cascade's clause order, so a
% transition reports every gate that moved rather than the first clause to fire.
% number/1 guards come FIRST everywhere: purity/suppression carry atom absence
% tokens, and `V >= T` THROWS on an atom (CLAUDE.md OQ-60 rule).
% ---------------------------------------------------------------------------
num_ge(V, T) :- number(V), V >= T.
num_gt(V, T) :- number(V), V >  T.
num_le(V, T) :- number(V), V =< T.
num_lt(V, T) :- number(V), V <  T.

bit(G, t) :- call(G), !.
bit(_, f).

gate_bits(E, Chi, Supp, Coal, Bits) :-
    config:param(piton_epsilon_floor, PEF),
    config:param(snare_epsilon_floor, SEF),
    config:param(rope_epsilon_ceiling, REC),
    config:param(tangled_rope_epsilon_floor, TREF),
    config:param(snare_chi_floor, SCF),
    config:param(rope_chi_ceiling, RCC),
    config:param(tangled_rope_chi_floor, TRCF),
    config:param(tangled_rope_chi_ceil, TRCC),
    config:param(scaffold_extraction_ceil, SEC),
    config:param(piton_extraction_ceiling, PEC),
    config:param(snare_suppression_floor, SSF),
    config:param(tangled_rope_suppression_floor, TRSF),
    bit(num_gt(E, PEF),     B1),
    bit(num_ge(E, SEF),     B2),
    bit(num_le(E, REC),     B3),
    bit(num_ge(E, TREF),    B4),
    bit(num_gt(E, REC),     B5),
    bit(num_ge(Chi, SCF),   B6),
    bit(num_le(Chi, RCC),   B7),
    bit(num_le(Chi, 0.0),   B8),
    bit(num_gt(Chi, TRCF),  B9),
    bit(num_le(Chi, TRCC),  B10),
    bit(num_le(Chi, SEC),   B11),
    bit(num_le(Chi, PEC),   B12),
    bit(num_lt(Chi, TRCF),  B13),
    bit(num_ge(Supp, SSF),  B14),
    bit(num_ge(Supp, TRSF), B15),
    ( Coal == t -> B16 = t ; B16 = f ),
    Bits = [piton_epsilon_floor-B1, snare_epsilon_floor-B2, rope_epsilon_ceiling-B3,
            tangled_rope_epsilon_floor-B4, naturalized_epsilon_floor-B5,
            snare_chi_floor-B6, rope_chi_ceiling-B7, rope_chi_nonpositive-B8,
            tangled_rope_chi_floor-B9, tangled_rope_chi_ceil-B10,
            scaffold_extraction_ceil-B11, piton_extraction_ceiling-B12,
            naturalized_chi_ceiling-B13,
            snare_suppression_floor-B14, tangled_rope_suppression_floor-B15,
            coalition_fired-B16].

% ---------------------------------------------------------------------------
% Per-seat state. d / f(d) / σ are derived on the RESOLVED context, exactly the
% way extractiveness_for_agent/3 derives the d that χ uses — deriving them from
% the unresolved atom forks χ from its own reported f(d) (witnessed 2026-06-30).
% ---------------------------------------------------------------------------
seat_row(C, Ctx, row(D, FD, SM, Chi, Supp, Coal, MT, FT, Bits)) :-
    Ctx = context(agent_power(P), T, Ex, spatial_scope(Sc)),
    ( catch(constraint_indexing:resolve_coalition_power(P, C, RP0), _, fail) -> RP = RP0 ; RP = P ),
    ( RP == P -> Coal = f ; Coal = t ),
    RCtx = context(agent_power(RP), T, Ex, spatial_scope(Sc)),
    ( catch(constraint_indexing:derive_directionality(C, RCtx, D0), _, fail) -> D = D0 ; D = err ),
    ( catch(constraint_indexing:resolve_displacement(RP, Dl0), _, fail) -> Dl = Dl0 ; Dl = 0.0 ),
    ( number(D), number(Dl)
    -> Deff is max(0.0, min(1.0, D + Dl)),
       ( catch(constraint_indexing:sigmoid_f(Deff, FD0), _, fail) -> FD = FD0 ; FD = err )
    ;  FD = err ),
    ( catch(constraint_indexing:scope_modifier(Sc, SM0), _, fail) -> SM = SM0 ; SM = err ),
    ( catch(constraint_indexing:extractiveness_for_agent(C, Ctx, Chi0), _, fail) -> Chi = Chi0 ; Chi = err ),
    ( catch(drl_core:get_raw_suppression(C, Sp0), _, fail) -> Supp = Sp0 ; Supp = unknown ),
    ( catch(drl_core:base_extractiveness(C, E0), _, fail) -> E = E0 ; E = err ),
    mt_at(C, Ctx, MT),
    ft_at(C, Ctx, FT),
    gate_bits(E, Chi, Supp, Coal, Bits).

snap(C, Eps, Snap) :-
    set_eps(C, Eps),
    ( took_effect(C, Eps)
    -> constraint_indexing:site_contexts_canonical(Ctxs),
       findall(Ctx-Row, (member(Ctx, Ctxs), seat_row(C, Ctx, Row)), Snap)
    ;  Snap = guard_failed ).

types_of(guard_failed, guard_failed) :- !.
types_of(Snap, Ts) :-
    findall(MT-FT, member(_-row(_,_,_,_,_,_,MT,FT,_), Snap), Ts).

% ---------------------------------------------------------------------------
% GRID: the 0.01 rail (101 points; the only reportable resolution, OQ-78
% ruling 1) UNION bracket triples {T-d, T, T+d} at d=1e-4 for the eight
% thresholds. The chi-side thresholds are included because their epsilon-
% crossings cannot be pre-placed: f*sigma is itself epsilon-dependent.
% ---------------------------------------------------------------------------
bracket_thresholds([0.10, 0.25, 0.30, 0.35, 0.45, 0.46, 0.60, 0.66]).

grid(G) :-
    findall(V, (between(0, 100, I), V is I / 100.0), Rail),
    bracket_thresholds(Ts),
    findall(V,
        ( member(T, Ts), member(Dl, [-0.0001, 0.0, 0.0001]),
          V0 is T + Dl, V0 >= 0.0, V0 =< 1.0, V = V0 ),
        Brk),
    append(Rail, Brk, All),
    sort(All, G).

% adaptive bisection to 1e-4
bisect(_, Lo, SLo, Hi, SHi, Lo, SLo, Hi, SHi) :- Hi - Lo =< 1.00001e-4, !.
bisect(C, Lo, SLo, Hi, SHi, OLo, OSLo, OHi, OSHi) :-
    Mid is (Lo + Hi) / 2.0,
    snap(C, Mid, SM),
    types_of(SLo, TL), types_of(SM, TM),
    ( TL == TM
    -> bisect(C, Mid, SM, Hi, SHi, OLo, OSLo, OHi, OSHi)
    ;  bisect(C, Lo, SLo, Mid, SM, OLo, OSLo, OHi, OSHi) ).

walk(C, [P0|Ps], Trans) :-
    snap(C, P0, S0),
    ( S0 == guard_failed -> Trans = guard_failed
    ; walk_(C, P0, S0, Ps, [], Trans) ).

walk_(_, _, _, [], Acc, Acc).
walk_(C, P0, S0, [P1|Ps], Acc0, Acc) :-
    snap(C, P1, S1),
    ( S1 == guard_failed
    -> Acc = guard_failed
    ;  types_of(S0, T0), types_of(S1, T1),
       ( T0 == T1
       -> Acc1 = Acc0
       ;  bisect(C, P0, S0, P1, S1, Lo, SLo, Hi, SHi),
          Acc1 = [tr(Lo, SLo, Hi, SHi)|Acc0] ),
       walk_(C, P1, S1, Ps, Acc1, Acc) ).

bit_delta(B0, B1, DG) :-
    findall(N, (member(N-V0, B0), memberchk(N-V1, B1), V0 \== V1), DG).

nf(V, A) :- number(V), !, format(atom(A), "~6f", [V]).
nf(V, V).

emit_transition(C, tr(Lo, SLo, Hi, SHi)) :-
    forall(
      ( nth0(I, SLo, Ctx-row(D0,FD0,SM0,Chi0,Sp0,Co0,MT0,FT0,B0)),
        nth0(I, SHi, _-row(D1,FD1,SM1,Chi1,Sp1,Co1,MT1,FT1,B1)),
        MT0-FT0 \== MT1-FT1 ),
      ( Ctx = context(agent_power(P),_,_,_),
        bit_delta(B0, B1, DG),
        ( DG == [] -> DGA = none ; atomic_list_concat(DG, ';', DGA) ),
        nf(D0,A1), nf(FD0,A2), nf(SM0,A3), nf(Chi0,A4), nf(Sp0,A5),
        nf(D1,A6), nf(FD1,A7), nf(SM1,A8), nf(Chi1,A9), nf(Sp1,A10),
        format("TR ~w ~6f ~6f ~w ~w ~w ~w ~w ~w | ~w ~w ~w ~w ~w ~w | ~w ~w ~w ~w ~w ~w~n",
               [C, Lo, Hi, P, MT0, FT0, MT1, FT1, DGA,
                A1,A2,A3,A4,A5,Co0, A6,A7,A8,A9,A10,Co1]) )).

authored_eps(C, E) :- once(narrative_ontology:constraint_metric(C, extractiveness, E)).

sweep_story(C) :-
    ( authored_eps(C, E0) -> true ; E0 = none ),
    ( E0 == none
    -> format("NO_EPS ~w~n", [C])
    ;  eps_solutions(C, Orig),
       grid(G),
       walk(C, G, Trans),
       restore_eps(C, Orig),
       ( Trans == guard_failed
       -> format("GUARD_FAIL ~w ~6f~n", [C, E0])
       ;  length(Trans, NT),
          aggregate_all(count, narrative_ontology:constraint_victim(C, _), NV),
          ( catch(drl_core:get_raw_suppression(C, SpA), _, fail) -> true ; SpA = unknown ),
          nf(SpA, SpAtom),
          ( narrative_ontology:constraint_claim(C, CT0) -> CT = CT0 ; CT = none ),
          format("STORY ~w ~6f ~w ~w ~w ~w~n", [C, E0, NT, NV, SpAtom, CT]),
          forall(member(T, Trans), emit_transition(C, T)) ) ).

% ===========================================================================
% CONTROLS. Run FIRST, fail-closed, printed loud. Every skip prints
% "SKIPPED <reason>" and is counted. A skipped control is never a passed one.
% ===========================================================================

% --- C3: the S4 shadow guard, verbatim from epsilon_stability.py. Proves the
% took-effect guard is CALLED and CAN DECLINE.
control_c3 :-
    C = carbon_tax_2026,
    ( drl_core:base_extractiveness(C, V0)
    -> true
    ;  format("CONTROL c3 SKIPPED precondition-carbon_tax_2026-direct-fact-missing~n"), fail ),
    ( narrative_ontology:constraint_metric(C, extractiveness, _)
    -> format("CONTROL c3 SKIPPED precondition-carbon_tax_2026-now-authors-constraint_metric~n"), fail
    ;  true ),
    Perturbed is V0 + 0.3,
    assertz(narrative_ontology:constraint_metric(C, extractiveness, Perturbed)),
    cache_registry:clear_all_caches,
    ( took_effect(C, Perturbed)
    -> format("CONTROL c3 FAIL guard-passed-under-shadow~n"),
       retractall(narrative_ontology:constraint_metric(C, extractiveness, _)),
       cache_registry:clear_all_caches, fail
    ;  format("CONTROL c3 PASS shadow-detected direct-fact ~w wins over perturbed ~w~n",
              [V0, Perturbed]) ),
    retractall(narrative_ontology:constraint_metric(C, extractiveness, _)),
    cache_registry:clear_all_caches.
control_c3 :- format("CONTROL c3 SKIPPED see-above~n").

% --- C1 carrier selection: a PROCEDURE, not a hope. Stories with
% suppression >= snare floor and FEWER THAN critical_mass victims (so d cannot
% jump via resolve_coalition_power), ranked by max seat chi descending.
% The victims<3 criterion and S10 are NOT redundant: S10 keeps C1's override
% key stable at the ANALYTICAL seat, victims<3 keeps the POWERLESS seat from
% jumping at 0.46 for an unrelated reason. C1 reads across all four seats.
c1_candidate(Score-C) :-
    corpus_loader:corpus_constraint(C),
    authored_eps(C, _),
    catch(drl_core:get_raw_suppression(C, Sp), _, fail),
    number(Sp),
    config:param(snare_suppression_floor, SF), Sp >= SF,
    aggregate_all(count, narrative_ontology:constraint_victim(C, _), NV),
    config:param(critical_mass_threshold, CM), NV < CM,
    constraint_indexing:site_contexts_canonical(Ctxs),
    findall(Chi, ( member(Cx, Ctxs),
                   catch(constraint_indexing:extractiveness_for_agent(C, Cx, Chi), _, fail),
                   number(Chi) ), Chis),
    Chis \== [],
    max_list(Chis, Score).

% Sweep ONE story on the rail only, under an analytical d=1.0 override, and
% report whether a transition lands at exactly 0.46 carrying snare_epsilon_floor.
c1_probe(C, Verdict, Detail) :-
    eps_solutions(C, Orig),
    retractall(constraint_indexing:directionality_override(C, _, _)),
    assertz(constraint_indexing:directionality_override(C, analytical, 1.0)),
    cache_registry:clear_all_caches,
    findall(V, (between(0, 100, I), V is I / 100.0), Rail),
    walk(C, Rail, Trans),
    restore_eps(C, Orig),
    retractall(constraint_indexing:directionality_override(C, analytical, 1.0)),
    cache_registry:clear_all_caches,
    ( Trans == guard_failed
    -> Verdict = guard_failed, Detail = 'took-effect-guard-failed'
    ;  ( member(tr(Lo, SLo, Hi, SHi), Trans),
         abs(Hi - 0.46) < 1.0e-6,
         nth0(I2, SLo, _-row(_,_,_,_,_,_,MT0,FT0,B0)),
         nth0(I2, SHi, _-row(_,_,_,_,_,_,MT1,FT1,B1)),
         ( MT0 \== MT1 ; FT0 \== FT1 ),
         bit_delta(B0, B1, DG),
         memberchk(snare_epsilon_floor, DG)
       -> Verdict = fired,
          atomic_list_concat(DG, ';', DGA),
          format(atom(Detail), "lo=~4f hi=~4f ~w/~w -> ~w/~w gates=~w",
                 [Lo, Hi, MT0, FT0, MT1, FT1, DGA])
       ;  Verdict = no_fire,
          length(Trans, NT),
          format(atom(Detail), "~w-transitions-none-at-0.46-with-snare_epsilon_floor", [NT]) ) ).

% --- C2: the SAME story and grid with snare_epsilon_floor overlaid to 0.90.
% C1's 0.46 transition MUST vanish. The only arm that declines on the thing
% under test. retractall+assertz+clear_all_caches, NOT probe_harness:
% with_overlay/3 — snapshot/2 collects only clause(M:T,true), so a config RULE
% would be silently skipped (OQ-302/326).
control_c1_c2 :-
    findall(S-C, c1_candidate(S-C), Cands0),
    sort(0, @>=, Cands0, Cands),
    length(Cands, NC),
    format("CONTROL c1 candidates ~w~n", [NC]),
    ( Cands == []
    -> format("CONTROL c1 SKIPPED no-carrier-on-leg (supp>=floor AND victims<critical_mass)~n"),
       format("CONTROL c2 SKIPPED no-c1-carrier~n")
    ;  ( member(_-C, Cands), c1_probe(C, fired, Det)
       -> format("CONTROL c1 PASS carrier ~w ~w~n", [C, Det]),
          config:param(snare_epsilon_floor, Orig),
          retractall(config:param(snare_epsilon_floor, _)),
          assertz(config:param(snare_epsilon_floor, 0.90)),
          cache_registry:clear_all_caches,
          c1_probe(C, V2, Det2),
          retractall(config:param(snare_epsilon_floor, _)),
          assertz(config:param(snare_epsilon_floor, Orig)),
          cache_registry:clear_all_caches,
          ( V2 == fired
          -> format("CONTROL c2 FAIL transition-survived-threshold-move ~w~n", [Det2])
          ;  format("CONTROL c2 PASS transition-vanished-under-floor-0.90 (~w: ~w)~n", [V2, Det2]) ),
          ( config:param(snare_epsilon_floor, Orig) -> format("CONTROL c2 restore-verified ~w~n",[Orig])
          ; format("CONTROL c2 RESTORE_FAIL~n"), halt(1) )
       ;  Cands = [Top-CTop|_],
          format("CONTROL c1 SKIPPED no-candidate-fired (top ~w chi ~4f of ~w candidates)~n",
                 [CTop, Top, NC]),
          format("CONTROL c2 SKIPPED no-c1-carrier~n") ) ).

% --- C4: the naturally-arising two-sided arm. Its suppression (0.28) is below
% the 0.60 snare floor, so the snare gate fails on SUPPRESSION at every eps.
% Over the 55 rail points eps in [0.46, 1.00] it must report
% snare_epsilon_floor SATISFIED and snare_suppression_floor BLOCKING at every
% one, and never attribute a transition to snare_epsilon_floor: 0 fires,
% 55 declines.
control_c4 :-
    C = '%%C4%%',
    ( corpus_loader:corpus_constraint(C)
    -> true
    ;  format("CONTROL c4 SKIPPED carrier-not-on-this-leg~n"), fail ),
    ( catch(drl_core:get_raw_suppression(C, CSupp), _, fail), number(CSupp)
    -> true
    ;  format("CONTROL c4 SKIPPED precondition-carrier-has-no-numeric-suppression~n"), fail ),
    config:param(snare_suppression_floor, C4Floor),
    ( CSupp < C4Floor
    -> true
    ;  format("CONTROL c4 SKIPPED precondition-carrier-suppression-~4f-NOT-below-snare-floor-~4f-on-this-leg (per-leg redraw property; the control's premise is absent here, it did not fail)~n",
              [CSupp, C4Floor]), fail ),
    eps_solutions(C, Orig),
    findall(V, (between(46, 100, I), V is I / 100.0), Rail55),
    length(Rail55, NPts),
    constraint_indexing:site_contexts_canonical(Ctxs),
    findall(ok,
      ( member(Eps, Rail55),
        snap(C, Eps, Snap), Snap \== guard_failed,
        forall(member(_-row(_,_,_,_,_,_,_,_,Bits), Snap),
               ( memberchk(snare_epsilon_floor-t, Bits),
                 memberchk(snare_suppression_floor-f, Bits) )) ),
      Oks),
    length(Oks, NOk),
    % and: does any transition on this range attribute to snare_epsilon_floor?
    walk(C, Rail55, Trans),
    restore_eps(C, Orig),
    ( Trans == guard_failed
    -> format("CONTROL c4 SKIPPED guard-failed~n")
    ;  findall(1, ( member(tr(_, SLo, _, SHi), Trans),
                    nth0(I3, SLo, _-row(_,_,_,_,_,_,_,_,B0)),
                    nth0(I3, SHi, _-row(_,_,_,_,_,_,_,_,B1)),
                    bit_delta(B0, B1, DG), memberchk(snare_epsilon_floor, DG) ), Fires),
       length(Fires, NF),
       length(Ctxs, NSeats),
       ( NOk =:= NPts, NF =:= 0
       -> format("CONTROL c4 PASS carrier-supp ~4f declined ~w/~w rail points (all ~w seats: snare_epsilon_floor SATISFIED, snare_suppression_floor BLOCKING), snare_epsilon_floor-attributed transitions ~w~n",
                 [CSupp, NOk, NPts, NSeats, NF])
       ;  format("CONTROL c4 FAIL declined ~w/~w points, snare_epsilon_floor-attributed transitions ~w~n",
                 [NOk, NPts, NF]) ) ).
control_c4 :- format("CONTROL c4 SKIPPED see-above~n").

% Census of directionality_override facts actually LOADED — replaces the
% prose-contaminated grep in the recon pass.
override_census :-
    findall(C-P-D, constraint_indexing:directionality_override(C, P, D), Os),
    length(Os, N),
    format("OVERRIDE_CENSUS n ~w~n", [N]),
    forall(member(C2-P2-D2, Os), format("OVERRIDE ~w ~w ~w~n", [C2, P2, D2])).

run :-
    aggregate_all(count, corpus_loader:corpus_constraint(_), N),
    format("META n_corpus ~w~n", [N]),
    ( N > 0 -> true ; format("EMPTY corpus~n"), halt(1) ),
    aggregate_all(count, corpus_loader:corpus_story(_), NS),
    format("META n_stories ~w~n", [NS]),
    grid(G), length(G, NG), format("META n_grid ~w~n", [NG]),
    ( control_c3 -> true ; true ),
    control_c1_c2,
    ( control_c4 -> true ; true ),
    override_census,
    ( %%STORIES%% ->
        findall(C, corpus_loader:corpus_constraint(C), Cs),
        forall(member(C, Cs), sweep_story(C))
    ; format("META stories_skipped controls_only~n") ),
    format("DONE~n").

:- catch(run, E, (format(user_error, "ERR: ~w~n", [E]), halt(1))), halt.
:- halt(1).
"""


def build_goal(corpus, stories, c4_carrier):
    src = PROLOG_SRC.replace("%%C4%%", c4_carrier)
    src = src.replace("%%STORIES%%", "true" if stories else "fail")
    if corpus:
        # corpus_path overlay must precede load. retractall FIRST — a bare
        # assertz appends after config.pl's default clause and is silently
        # ignored (witnessed 2026-06-13: 44 loaded instead of 960).
        src = src.replace(
            ":- [stack].",
            ":- [stack].\n"
            f":- retractall(config:param(corpus_path, _)),"
            f" assertz(config:param(corpus_path, '{corpus}')).",
        )
    return src


def run_prolog(corpus, stories, timeout, c4_carrier):
    src = build_goal(corpus, stories, c4_carrier)
    with tempfile.NamedTemporaryFile(suffix=".pl", dir=PROLOG_DIR, mode="w",
                                     delete=False) as f:
        f.write(src)
        pl_path = f.name
    t0 = time.time()
    try:
        r = subprocess.run(
            ["swipl", "-g", f"consult('{pl_path}'), halt.", "-t", "halt(1)"],
            cwd=PROLOG_DIR, capture_output=True, text=True, timeout=timeout,
        )
    finally:
        Path(pl_path).unlink(missing_ok=True)
    return r, time.time() - t0


def _num(tok):
    try:
        return float(tok)
    except ValueError:
        return tok


def parse(stdout):
    meta, controls, overrides = {}, [], []
    stories, transitions, guard_failed, no_eps = [], [], [], []
    for line in stdout.splitlines():
        t = line.split()
        if not t:
            continue
        if t[0] == "META":
            meta[t[1]] = " ".join(t[2:])
        elif t[0] == "CONTROL":
            controls.append(" ".join(t[1:]))
        elif t[0] == "OVERRIDE_CENSUS":
            meta["n_overrides"] = t[2]
        elif t[0] == "OVERRIDE":
            overrides.append({"id": t[1], "power": t[2], "d": _num(t[3])})
        elif t[0] == "NO_EPS":
            no_eps.append(t[1])
        elif t[0] == "GUARD_FAIL":
            guard_failed.append({"id": t[1], "epsilon": _num(t[2])})
        elif t[0] == "STORY":
            stories.append({"id": t[1], "epsilon": _num(t[2]),
                            "n_transitions": int(t[3]), "n_victims": int(t[4]),
                            "suppression": _num(t[5]), "claimed_type": t[6]})
        elif t[0] == "TR":
            # TR id lo hi power MT0 FT0 MT1 FT1 gates | d f sm chi supp coal | d f sm chi supp coal
            head, lo_s, hi_s = t[1:9], None, None
            parts = " ".join(t[1:]).split("|")
            h = parts[0].split()
            lo = parts[1].split()
            hi = parts[2].split()
            transitions.append({
                "id": h[0], "eps_lo": float(h[1]), "eps_hi": float(h[2]),
                "seat": h[3],
                "mt_lo": h[4], "ft_lo": h[5], "mt_hi": h[6], "ft_hi": h[7],
                "deciding_gates": [] if h[8] == "none" else h[8].split(";"),
                "lo": {"d": _num(lo[0]), "f_d": _num(lo[1]), "scope_mod": _num(lo[2]),
                       "chi": _num(lo[3]), "supp": _num(lo[4]), "coalition_fired": lo[5] == "t"},
                "hi": {"d": _num(hi[0]), "f_d": _num(hi[1]), "scope_mod": _num(hi[2]),
                       "chi": _num(hi[3]), "supp": _num(hi[4]), "coalition_fired": hi[5] == "t"},
            })
    return {"meta": meta, "controls": controls, "overrides": overrides,
            "stories": stories, "transitions": transitions,
            "guard_failed": guard_failed, "no_epsilon": no_eps}


def main():
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("--corpus", help="corpus_path overlay relative to prolog/ (default: testsets)")
    ap.add_argument("--json-out", required=True,
                    help="REQUIRED, no default. Must not resolve into outputs/.")
    ap.add_argument("--controls-only", action="store_true")
    ap.add_argument("--c4-carrier", default=C4_CARRIER)
    ap.add_argument("--timeout", type=int, default=14400)
    a = ap.parse_args()

    out = Path(a.json_out).resolve()
    if (ROOT / "outputs") in out.parents or out.parent == (ROOT / "outputs"):
        raise SystemExit(
            f"REFUSED: --json-out resolves into outputs/ ({out}). This is an audit "
            "fork; it must not write into the pipeline's artifact tree.")

    r, elapsed = run_prolog(a.corpus, not a.controls_only, a.timeout, a.c4_carrier)
    sys.stderr.write(r.stderr[-2000:] if r.stderr else "")
    if r.returncode != 0 or "DONE" not in r.stdout:
        sys.stderr.write(r.stdout[-4000:])
        raise SystemExit(f"eps_transition_map swipl exited {r.returncode} "
                         "(control red or crash — see transcript above)")

    result = parse(r.stdout)
    corpus_dir = (PROLOG_DIR / (a.corpus or "testsets")).resolve()
    result["corpus_path"] = a.corpus or "testsets"
    result["corpus_hash"] = compute_corpus_hash(corpus_dir)
    result["corpus_file_count"] = len(list(corpus_dir.glob("*.pl")))
    result["elapsed_seconds"] = round(elapsed, 1)
    result["gate_bits"] = GATE_BITS

    out.parent.mkdir(parents=True, exist_ok=True)
    out.write_text(json.dumps(result, indent=2), encoding="utf-8")

    print(f"corpus={result['corpus_path']} files={result['corpus_file_count']} "
          f"stories_swept={len(result['stories'])} transitions={len(result['transitions'])} "
          f"guard_failed={len(result['guard_failed'])} no_eps={len(result['no_epsilon'])} "
          f"elapsed={elapsed:.0f}s")
    for c in result["controls"]:
        print(f"  control: {c}")
    print(f"wrote {out}")


if __name__ == "__main__":
    main()
