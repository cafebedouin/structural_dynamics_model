/* Signature-identity witness probe — 2026-06-12
 *
 * QUESTION (pre-registered): the kernel_v1 naming-drift triple (press/Reformation
 * topic, three generation runs, three names for "the same" reading) — do the three
 * draws land NEAR each other in the engine's structural-identity space
 * (logical_fingerprint dimensions), and do topic-distinct stories land APART?
 *
 * Within-pairs (3): the triple, pairwise.
 * Between-pairs (9): triple x 3 topic-distinct controls.
 * Probe positive controls: (a) full fingerprints printed — all-unknown fingerprints
 * would make matches vacuous; (b) at least one between-pair must show >=1 false,
 * proving the matcher can return false.
 *
 * Either outcome is the finding: separation => the math carries cross-draw identity;
 * overlap => the math identifies story-kinds, not stories.
 */

:- ( retract(config:param(corpus_path, _)) -> true ; true ),
   asserta(config:param(corpus_path, 'archives/datasets/kernel_v1')).

:- [stack].
:- use_module(logical_fingerprint).
:- corpus_loader:load_all_testsets.

triple(press_reformation_causality__technological_inevitability).
triple(press_reformation_causation__technological_determinism).
triple(technology_reformation_causality__technological_determinism_reading).

control(gpl_copyleft_scope__enforcement_vacuum_reading).
control(neural_interface_standardization).
control(blockchain_settlement_finality).

dims([shift, properties, voids, actors, drift, zone, coupling]).

% --- id-resolution positive control: every probe id must be a corpus member ---
check_membership :-
    forall(( triple(C) ; control(C) ),
           ( corpus_loader:corpus_constraint(C)
           -> format("MEMBER ok: ~w~n", [C])
           ;  format("MEMBER **MISSING**: ~w~n", [C])
           )).

print_all_fingerprints :-
    forall(( triple(C) ; control(C) ),
           logical_fingerprint:print_fingerprint(C)).

pair_report(C1, C2, Label) :-
    dims(Ds),
    ( fingerprint_match(C1, C2, Ds, Matches)
    -> include([_-true]>>true, Matches, Trues),
       length(Trues, NT),
       length(Ds, ND),
       format("~w  ~w | ~w  -> ~w/~w  ~w~n", [Label, C1, C2, NT, ND, Matches])
    ;  format("~w  ~w | ~w  -> MATCH-CALL FAILED~n", [Label, C1, C2])
    ).

run_pairs :-
    format("~n=== WITHIN-TRIPLE PAIRS (same material, 3 draws) ===~n"),
    forall(( triple(C1), triple(C2), C1 @< C2 ),
           pair_report(C1, C2, 'WITHIN ')),
    format("~n=== BETWEEN PAIRS (triple x topic-distinct controls) ===~n"),
    forall(( triple(C1), control(C2) ),
           pair_report(C1, C2, 'BETWEEN')),
    format("~n=== CONTROL-CONTROL PAIRS (baseline among distinct topics) ===~n"),
    forall(( control(C1), control(C2), C1 @< C2 ),
           pair_report(C1, C2, 'CTRLCTL')).

run_probe :-
    format("=== MEMBERSHIP CHECK ===~n"),
    check_membership,
    format("~n=== FULL FINGERPRINTS (vacuity guard) ===~n"),
    print_all_fingerprints,
    run_pairs.
