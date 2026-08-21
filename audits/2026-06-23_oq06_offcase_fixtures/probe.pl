/* OQ-06 Phase C — Witness matrix for the four off-case conjuncts of
   cs_pattern_detection:cs_drift_unacknowledged/2 and
   cs_axiom_engine:cs_axiom_foreclosed/2.

   Mirrors audits/2026-06-09_oq07_mismatch_runtime_probe/probe.pl.

   Three witness families per the Phase B branch (decided from search_output.txt):
     (1) Real-corpus FIRE control — each predicate fires on >=1 genuine UID on a
         NAMED leg, proving end-to-end dispatch on real data.
     (2) Transient matched-pair MATRIX — for EACH of the four conjuncts: a SILENT
         row (off-fixture, all siblings satisfied -> predicate must NOT fire) and a
         paired FIRED row (the one off-field flipped on -> predicate MUST fire).
         Synthetic fresh UIDs isolate a single conjunct (real UIDs differ in many
         fields). Corpus-independent; run on testsets.
     (3) Real off-case SILENCE — for the three conjuncts with live corpus off-cases
         (drift-C4, axiom-C2, axiom-C4), the predicate is silent on the genuine UID
         found in Phase A. drift-C3 has no live off-case (witnessed absent on all
         four corpuses) so it is closed by the transient matrix alone.

   cs_axiom/3 is multifile-but-static; the synthetic axiom fixtures need it
   assertable, so it is declared dynamic for this process only (does not change how
   the predicate READS it). cs_drift_state/3 and cs_axiom_grounding/3 are dynamic.

   Read-only: writes no output files. Run from prolog/:
     swipl -g "[stack], consult('<this>'), probe_testsets, halt"  -t "halt(1)"
     swipl -g "[stack], consult('<this>'), probe_haiku, halt"     -t "halt(1)"
*/

:- use_module(probe_harness).

oq326_reach_undeclared(F,
    reach_undeclared(retrofit('2026-08-21',
        "bare with_asserted/2: no template, so no declared query shape (OQ-326 clause 4')"), F)).

expect_silent(Label, Facts, Goal) :-
    maplist(oq326_reach_undeclared, Facts, WFacts),
    (   probe_harness:with_asserted(WFacts, \+ Goal)
    ->  format("  SILENT [PASS] ~w~n", [Label])
    ;   format("  SILENT [FAIL] ~w   <-- predicate FIRED; off-conjunct did NOT block~n", [Label])
    ).

expect_fired(Label, Facts, Goal) :-
    maplist(oq326_reach_undeclared, Facts, WFacts),
    (   probe_harness:with_asserted(WFacts, Goal)
    ->  format("  FIRED  [PASS] ~w~n", [Label])
    ;   format("  FIRED  [FAIL] ~w   <-- predicate SILENT; sibling-satisfied firing path broken~n", [Label])
    ).

load(Path) :-
    ( retract(config:param(corpus_path, _)) -> true ; true ),
    asserta(config:param(corpus_path, Path)),
    corpus_loader:load_all_testsets,
    dynamic(narrative_ontology:cs_axiom/3).

fire_control(Path) :-
    format("~n[1] REAL-CORPUS FIRE CONTROL  (corpus=~w)~n", [Path]),
    aggregate_all(count, cs_pattern_detection:cs_drift_unacknowledged(_,_), DN),
    aggregate_all(count, cs_axiom_engine:cs_axiom_foreclosed(_,_), AN),
    ( DN > 0 -> DV = 'PASS' ; DV = 'FAIL' ),
    ( AN > 0 -> AV = 'PASS' ; AV = 'FAIL' ),
    format("  [~w] cs_drift_unacknowledged fires on ~w real UID(s)~n", [DV, DN]),
    format("  [~w] cs_axiom_foreclosed fires on ~w real UID(s)~n", [AV, AN]).

% ---- Transient matched-pair matrix (synthetic fresh UIDs) ----
transient_matrix :-
    format("~n[2] TRANSIENT MATCHED-PAIR MATRIX (synthetic UIDs, single-conjunct isolation)~n"),

    format(" drift C3 (Dir=stable):~n"),
    expect_silent("gap(stable, substantial, false)  [C3 off, C4 ok]",
        [narrative_ontology:cs_drift_state('SX-C3', m, gap(stable, substantial, false))],
        cs_pattern_detection:cs_drift_unacknowledged('SX-C3', _)),
    expect_fired("gap(authority_erosion, substantial, false)  [C3 flipped on]",
        [narrative_ontology:cs_drift_state('SX-C3', m, gap(authority_erosion, substantial, false))],
        cs_pattern_detection:cs_drift_unacknowledged('SX-C3', _)),

    format(" drift C4 (Mag=minor):~n"),
    expect_silent("gap(authority_erosion, minor, false)  [C4 off, C3 ok]",
        [narrative_ontology:cs_drift_state('SX-C4', m, gap(authority_erosion, minor, false))],
        cs_pattern_detection:cs_drift_unacknowledged('SX-C4', _)),
    expect_fired("gap(authority_erosion, substantial, false)  [C4 flipped on]",
        [narrative_ontology:cs_drift_state('SX-C4', m, gap(authority_erosion, substantial, false))],
        cs_pattern_detection:cs_drift_unacknowledged('SX-C4', _)),

    format(" axiom C2 (grounding \\= empirically_contingent):~n"),
    expect_silent("grounding=deontological  [C2 off, C1/C3/C4 ok]",
        [narrative_ontology:cs_axiom('SX-A2', foundational, sx_atom2),
         narrative_ontology:cs_axiom_grounding('SX-A2', sx_atom2, deontological),
         narrative_ontology:cs_drift_state('SX-A2', m, gap(axiom_overriding, substantial, false))],
        cs_axiom_engine:cs_axiom_foreclosed('SX-A2', _)),
    expect_fired("grounding=empirically_contingent  [C2 flipped on]",
        [narrative_ontology:cs_axiom('SX-A2', foundational, sx_atom2),
         narrative_ontology:cs_axiom_grounding('SX-A2', sx_atom2, empirically_contingent),
         narrative_ontology:cs_drift_state('SX-A2', m, gap(axiom_overriding, substantial, false))],
        cs_axiom_engine:cs_axiom_foreclosed('SX-A2', _)),

    format(" axiom C4 (Mag=minor):~n"),
    expect_silent("gap(axiom_overriding, minor, false)  [C4 off, C1/C2/C3 ok]",
        [narrative_ontology:cs_axiom('SX-A4', foundational, sx_atom4),
         narrative_ontology:cs_axiom_grounding('SX-A4', sx_atom4, empirically_contingent),
         narrative_ontology:cs_drift_state('SX-A4', m, gap(axiom_overriding, minor, false))],
        cs_axiom_engine:cs_axiom_foreclosed('SX-A4', _)),
    expect_fired("gap(axiom_overriding, substantial, false)  [C4 flipped on]",
        [narrative_ontology:cs_axiom('SX-A4', foundational, sx_atom4),
         narrative_ontology:cs_axiom_grounding('SX-A4', sx_atom4, empirically_contingent),
         narrative_ontology:cs_drift_state('SX-A4', m, gap(axiom_overriding, substantial, false))],
        cs_axiom_engine:cs_axiom_foreclosed('SX-A4', _)).

% ---- Real off-case silence (precise (UID,Atom) / UID query on genuine data) ----
real_silence_testsets :-
    format("~n[3] REAL OFF-CASE SILENCE (genuine corpus UIDs, testsets)~n"),
    DriftC4UID = '0b5146c6-c6af-448f-90aa-0fe49c99350f',
    (   cs_pattern_detection:cs_drift_unacknowledged(DriftC4UID, G)
    ->  format("  [FAIL] drift-C4 real UID ~w FIRED with ~w (UID has another firing drift_state)~n", [DriftC4UID, G])
    ;   format("  [PASS] drift-C4 real UID ~w SILENT (Mag=minor blocks)~n", [DriftC4UID])
    ),
    AxC2UID = 'e0fb873f-3ba6-4786-86e3-6756def171ff',
    AxC2Atom = state_racial_classification_categorically_prohibited,
    (   cs_axiom_engine:cs_axiom_foreclosed(AxC2UID, AxC2Atom)
    ->  format("  [FAIL] axiom-C2 real (UID,Atom) FIRED~n")
    ;   format("  [PASS] axiom-C2 real ~w / ~w SILENT (grounding\\=ec blocks)~n", [AxC2UID, AxC2Atom])
    ).

real_silence_haiku :-
    format("~n[3b] REAL OFF-CASE SILENCE (axiom-C4, testsets_haiku)~n"),
    AxC4UID = 'b65e1d35-366b-423e-a27f-23349c61da90',
    AxC4Atom = verifiable_significance_distinguishes_legitimate_from_promotional,
    (   cs_axiom_engine:cs_axiom_foreclosed(AxC4UID, AxC4Atom)
    ->  format("  [FAIL] axiom-C4 real (UID,Atom) FIRED~n")
    ;   format("  [PASS] axiom-C4 real ~w / ~w SILENT (Mag=minor blocks)~n", [AxC4UID, AxC4Atom])
    ).

% ---- Restore / no-residual witness (post-matrix) ----
% with_asserted auto-retracts and verify_restore throws probe_restore_failed on a
% bad restore. A dynamic/1 swap on cs_axiom/3 is exactly the kind of change that could
% silently defeat that, so witness it POSITIVELY: after the full matrix, NO synthetic
% SX-* fact may survive in any of the three fact predicates the fixtures touched.
restore_witness :-
    format("~n[4] RESTORE / NO-RESIDUAL WITNESS (post-matrix)~n"),
    findall(U,   (narrative_ontology:cs_axiom(U,_,_),           sub_atom(U,0,3,_,'SX-')), AxR),
    findall(U,   (narrative_ontology:cs_drift_state(U,_,_),      sub_atom(U,0,3,_,'SX-')), DrR),
    findall(U-A, (narrative_ontology:cs_axiom_grounding(U,A,_),  sub_atom(U,0,3,_,'SX-')), GrR),
    length(AxR, NA), length(DrR, ND), length(GrR, NG),
    (   NA =:= 0, ND =:= 0, NG =:= 0
    ->  format("  [PASS] zero residual synthetic facts: cs_axiom=0 cs_drift_state=0 cs_axiom_grounding=0~n"),
        format("         with_asserted restore-verify clean even after the dynamic/1 swap on cs_axiom/3~n")
    ;   format("  [FAIL] residual SX-* facts leaked: cs_axiom=~w cs_drift_state=~w cs_axiom_grounding=~w~n", [NA,ND,NG])
    ).

probe_testsets :-
    format("OQ-06 PHASE C — witness matrix (testsets)~n"),
    load(testsets),
    fire_control(testsets),
    transient_matrix,
    real_silence_testsets,
    restore_witness,
    format("~n==== probe_testsets COMPLETE ====~n").

probe_haiku :-
    format("OQ-06 PHASE C — axiom-C4 real off-case (testsets_haiku)~n"),
    load(testsets_haiku),
    format("~n[1b] FIRE CONTROL (axiom, testsets_haiku)~n"),
    aggregate_all(count, cs_axiom_engine:cs_axiom_foreclosed(_,_), AN),
    ( AN > 0 -> AV = 'PASS' ; AV = 'FAIL' ),
    format("  [~w] cs_axiom_foreclosed fires on ~w real UID(s)~n", [AV, AN]),
    real_silence_haiku,
    format("~n==== probe_haiku COMPLETE ====~n").
