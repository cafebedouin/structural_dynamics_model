/* OQ-06 Phase A — Witness the absence (the search the OQ never showed).

   Question: for each of the four off-case conjuncts of cs_drift_unacknowledged/2
   and cs_axiom_foreclosed/2, is there a LIVE off-case UID anywhere in the four
   real corpuses, or is the off-case genuinely absent?

   "No live off-case UID" is an empty-result claim → a fact about THIS search, not
   the corpus, until the search is shown to find. So every off-bucket carries a
   TWO-SIDED planted control (sensitivity + specificity) run on the live corpus,
   using the EXACT bucket-filter predicates the enumeration uses (a control over a
   different filter proves nothing). Plus a per-corpus overlay fingerprint so a
   no-op asserta that silently reads the default corpus is caught.

   Read-only: nothing here writes output files. All fixtures are transient
   (with_asserted) and never land in testsets/.

   ONE CORPUS PER FRESH PROCESS. load_all_testsets/0 is guarded by corpus_loaded/0
   (no-op once loaded) and consult/1 ACCUMULATES narrative_ontology facts across
   loads (no retract between corpuses). So a sequential 4-corpus scan in one
   process would load only the first corpus and pollute every count. Each corpus
   gets its own swipl invocation (the OQ-07 single-overlay discipline).

   Run from prolog/ (driver runs these 5 invocations, output appended):
     swipl -g "[stack], consult('<this>'), controls, halt"            -t "halt(1)"
     swipl -g "[stack], consult('<this>'), scan(testsets), halt"      -t "halt(1)"
     swipl -g "[stack], consult('<this>'), scan(testsets_haiku), halt" -t "halt(1)"
     swipl -g "[stack], consult('<this>'), scan(testsets_flash), halt" -t "halt(1)"
     swipl -g "[stack], consult('<this>'), scan('archives/datasets/kernel_v1'), halt" -t "halt(1)"
*/

:- use_module(probe_harness).

% ---- Bucket filters (THE shared filter code: enumeration AND controls use these) ----
% Each off-bucket sets exactly ONE conjunct to its off-position with all siblings
% satisfied, so a member can only be there because that one conjunct is off.

bucket_c3_off(UID-gap(stable,Mag,false)) :-          % cs_drift C3 off: Dir=stable (C4 ok: Mag\=minor)
    narrative_ontology:cs_drift_state(UID, _, gap(stable, Mag, false)),
    Mag \= minor.

bucket_c4_off(UID-gap(Dir,minor,false)) :-           % cs_drift C4 off: Mag=minor (C3 ok: Dir\=stable)
    narrative_ontology:cs_drift_state(UID, _, gap(Dir, minor, false)),
    Dir \= stable.

bucket_drift_oncase(UID-gap(Dir,Mag,false)) :-       % fires cs_drift_unacknowledged (dispatch proof)
    narrative_ontology:cs_drift_state(UID, _, gap(Dir, Mag, false)),
    Dir \= stable, Mag \= minor.

bucket_axiom_c2_off(UID-Atom) :-                     % cs_axiom C2 off: grounding\=empirically_contingent
    narrative_ontology:cs_axiom(UID, _, Atom),
    narrative_ontology:cs_axiom_grounding(UID, Atom, G),
    G \= empirically_contingent,
    narrative_ontology:cs_drift_state(UID, _, gap(axiom_overriding, Mag, false)),
    Mag \= minor.

bucket_axiom_c4_off(UID-Atom) :-                     % cs_axiom C4 off: Mag=minor (C2 ok: empirically_contingent)
    narrative_ontology:cs_axiom(UID, _, Atom),
    narrative_ontology:cs_axiom_grounding(UID, Atom, empirically_contingent),
    narrative_ontology:cs_drift_state(UID, _, gap(axiom_overriding, minor, false)).

bucket_axiom_oncase(UID-Atom) :-                     % fires cs_axiom_foreclosed (dispatch proof)
    narrative_ontology:cs_axiom(UID, _, Atom),
    narrative_ontology:cs_axiom_grounding(UID, Atom, empirically_contingent),
    narrative_ontology:cs_drift_state(UID, _, gap(axiom_overriding, Mag, false)),
    Mag \= minor.

% ---- Reporting helpers ----

count_bucket(Pred, Count, Sample) :-
    G =.. [Pred, X],
    findall(X, call(G), Xs),
    length(Xs, Count),
    ( Xs = [S0|_] -> Sample = S0 ; Sample = none ).

report_bucket(Label, Pred) :-
    count_bucket(Pred, Count, Sample),
    format("    ~w~t~28|count=~w~t~44|sample=~w~n", [Label, Count, Sample]).

% ---- Per-corpus overlay + enumeration ----

load_corpus(Path) :-
    ( retract(config:param(corpus_path, _)) -> true ; true ),
    asserta(config:param(corpus_path, Path)),
    corpus_loader:load_all_testsets.

fingerprint(Path) :-
    config:param(corpus_path, Active),
    aggregate_all(count, narrative_ontology:cs_drift_state(_,_,_), DriftN),
    aggregate_all(count, narrative_ontology:cs_axiom(_,_,_), AxN),
    format("  OVERLAY-FINGERPRINT: requested=~w active=~w  cs_drift_state/3=~w  cs_axiom/3=~w~n",
           [Path, Active, DriftN, AxN]),
    ( Active == Path -> true
    ; format("  *** HALT: active corpus_path \\= requested — overlay did NOT take ***~n"), halt(2) ).

scan(Path) :-
    format("~n==== CORPUS: ~w ====~n", [Path]),
    load_corpus(Path),
    fingerprint(Path),
    format("  -- cs_drift_unacknowledged buckets --~n"),
    report_bucket('C3-off (Dir=stable)',     bucket_c3_off),
    report_bucket('C4-off (Mag=minor)',      bucket_c4_off),
    report_bucket('on-case (fires)',         bucket_drift_oncase),
    format("  -- cs_axiom_foreclosed buckets --~n"),
    report_bucket('C2-off (grounding\\=ec)',  bucket_axiom_c2_off),
    report_bucket('C4-off (Mag=minor)',      bucket_axiom_c4_off),
    report_bucket('on-case (fires)',         bucket_axiom_oncase).

% ---- Two-sided planted controls (run once on live testsets) ----
% Sensitivity: plant a fact the off-bucket SHOULD catch -> count must rise by 1.
% Specificity: plant an on-case fact it SHOULD MISS       -> count must NOT change.
% Both use the SAME bucket predicate as the enumeration.

oq326_reach_undeclared(F,
    reach_undeclared(retrofit('2026-08-21',
        "bare with_asserted/2: no template, so no declared query shape (OQ-326 clause 4')"), F)).

control(Pred, PlantSens, PlantSpec) :-
    count_bucket(Pred, Base, _),
    maplist(oq326_reach_undeclared, PlantSens, WSens),
    maplist(oq326_reach_undeclared, PlantSpec, WSpec),
    probe_harness:with_asserted(WSens,
        count_bucket(Pred, Sens, _)),
    probe_harness:with_asserted(WSpec,
        count_bucket(Pred, Spec, _)),
    ( Sens =:= Base+1 -> SensV = 'PASS' ; SensV = 'FAIL' ),
    ( Spec =:= Base    -> SpecV = 'PASS' ; SpecV = 'FAIL' ),
    format("    ~w~t~24|base=~w sens=~w[~w] spec=~w[~w]~n",
           [Pred, Base, Sens, SensV, Spec, SpecV]),
    ( (SensV == 'PASS', SpecV == 'PASS') -> true
    ; format("    *** CONTROL FAILED for ~w — bucket filter is broken, empty=ambiguous ***~n", [Pred]) ).

controls :-
    format("~n==== TWO-SIDED PLANTED CONTROLS (on live testsets) ====~n"),
    load_corpus(testsets),
    % cs_axiom/3 is multifile-but-static (cs_drift_state/3 + cs_axiom_grounding/3
    % are dynamic; cs_axiom/3 is not). The planted control needs to assert a
    % synthetic cs_axiom anchor — declare it dynamic for THIS control process
    % only (each scan/control runs in its own halting process, no leak). This
    % does not change how the bucket filter READS cs_axiom, so the filter test
    % is unaffected.
    dynamic(narrative_ontology:cs_axiom/3),
    % C3-off: catch gap(stable,substantial,false); miss on-case gap(authority_erosion,substantial,false)
    control(bucket_c3_off,
        [narrative_ontology:cs_drift_state('SYN-C3-SENS', m, gap(stable, substantial, false))],
        [narrative_ontology:cs_drift_state('SYN-C3-SPEC', m, gap(authority_erosion, substantial, false))]),
    % C4-off: catch gap(authority_erosion,minor,false); miss on-case gap(authority_erosion,substantial,false)
    control(bucket_c4_off,
        [narrative_ontology:cs_drift_state('SYN-C4-SENS', m, gap(authority_erosion, minor, false))],
        [narrative_ontology:cs_drift_state('SYN-C4-SPEC', m, gap(authority_erosion, substantial, false))]),
    % axiom C2-off: catch deontological triple; miss empirically_contingent triple (on-case for C2)
    control(bucket_axiom_c2_off,
        [narrative_ontology:cs_axiom('SYN-A2-SENS', role, ax2),
         narrative_ontology:cs_axiom_grounding('SYN-A2-SENS', ax2, deontological),
         narrative_ontology:cs_drift_state('SYN-A2-SENS', m, gap(axiom_overriding, substantial, false))],
        [narrative_ontology:cs_axiom('SYN-A2-SPEC', role, ax2s),
         narrative_ontology:cs_axiom_grounding('SYN-A2-SPEC', ax2s, empirically_contingent),
         narrative_ontology:cs_drift_state('SYN-A2-SPEC', m, gap(axiom_overriding, substantial, false))]),
    % axiom C4-off: catch empirically_contingent+minor; miss empirically_contingent+substantial (on-case)
    control(bucket_axiom_c4_off,
        [narrative_ontology:cs_axiom('SYN-A4-SENS', role, ax4),
         narrative_ontology:cs_axiom_grounding('SYN-A4-SENS', ax4, empirically_contingent),
         narrative_ontology:cs_drift_state('SYN-A4-SENS', m, gap(axiom_overriding, minor, false))],
        [narrative_ontology:cs_axiom('SYN-A4-SPEC', role, ax4s),
         narrative_ontology:cs_axiom_grounding('SYN-A4-SPEC', ax4s, empirically_contingent),
         narrative_ontology:cs_drift_state('SYN-A4-SPEC', m, gap(axiom_overriding, substantial, false))]).

% No :- initialization. Each entry point (controls/0, scan/1) is invoked in its
% own fresh swipl process by the shell driver (see header).
