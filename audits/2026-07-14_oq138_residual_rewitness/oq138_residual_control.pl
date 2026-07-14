:- initialization(main).

% Residual-shape POSITIVE CONTROL for the OQ-138 close.
% CI-rope control proves the probe detects metric!=final & buckets by signature; it does NOT
% prove the RESIDUAL column can read non-zero, nor that it DISCRIMINATES residual from CI-rope.
%   A. clause liveness — each of the 7 residual clauses CHANGES a type when tripped
%   B. real dispatch   — integrate_signature_with_modal (the actual signature code path) routes a
%                        residual-shaped seat to a changed type, and the probe's bucketing lands it
%                        in RESIDUAL while EXCLUDING a CI-rope-shaped changer (the discrimination).

pair(mountain, coordination_scaffold).
pair(mountain, constructed_low_extraction).
pair(mountain, constructed_high_extraction).
pair(mountain, constructed_constraint).
pair(unknown,  coordination_scaffold).
pair(unknown,  constructed_low_extraction).
pair(unknown,  constructed_constraint).

residual_sig(S) :-
    memberchk(S, [coordination_scaffold, constructed_low_extraction,
                  constructed_constraint, constructed_high_extraction]).

% the probe's exact RESIDUAL predicate over a changer tuple
is_residual_changer(_C-S-MT-_FT) :-
    residual_sig(S),
    ( S == constructed_high_extraction -> MT == mountain ; true ).

main :-
    [stack],
    corpus_loader:ensure_corpus_loaded,
    format("~n=== CONTROL A: residual clause liveness (each must CHANGE the type) ===~n", []),
    forall(pair(MT, Sig),
           ( signature_detection:resolve_modal_signature_conflict(MT, Sig, R),
             ( R \== MT -> V = 'FIRES(change)' ; V = 'DEAD(no-change)' ),
             format("  ~w + ~w -> ~w   [~w]~n", [MT, Sig, R, V]) )),
    % CONTROL B: route through the REAL dispatch wrapper resolve_with_perspectival_check/4 (the clause
    % integrate_signature_with_modal calls; takes the signature as an arg, so NO injection needed).
    ( signature_detection:resolve_with_perspectival_check(synth_c, mountain, coordination_scaffold, FT) -> true
    ; FT = '<dispatch failed>' ),
    format("~n=== CONTROL B: real signature dispatch on residual-shaped seat ===~n", []),
    format("  resolve_with_perspectival_check(_, ModalType=mountain, sig=coordination_scaffold) -> ~w~n",
           [FT]),
    % build the two candidate changer tuples: one residual-shaped, one CI-rope-shaped
    ResidualTuple = synth_resid_ctrl-coordination_scaffold-mountain-FT,
    CiRopeTuple   = synth_cirope_ctrl-coupling_invariant_rope-scaffold-rope,
    ( is_residual_changer(ResidualTuple) -> RIn = yes ; RIn = no ),
    ( is_residual_changer(CiRopeTuple)   -> CIn = yes ; CIn = no ),
    format("~n  RESIDUAL bucketing:  residual-shaped tuple in RESIDUAL? ~w   (must be yes)~n", [RIn]),
    format("                       CI-rope-shaped tuple in RESIDUAL?  ~w   (must be no)~n", [CIn]),
    ( ( FT == rope, RIn == yes, CIn == no )
    -> format("~n[CONTROL B PASS] real dispatch routes residual seat (mountain->rope) AND the column~n"),
       format("                 reads it as RESIDUAL while excluding the CI-rope shape (discriminates)~n")
    ;  format("~n[CONTROL B FAIL] one of: dispatch!=rope / residual not bucketed / CI-rope leaked in~n") ),
    halt.
main :- format("CONTROL FAILED~n", []), halt(1).
