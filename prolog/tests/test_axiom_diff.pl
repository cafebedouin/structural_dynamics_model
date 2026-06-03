% ============================================================================
% TESTS: axiom_diff — the cyclopean operator at the axiom layer (OQ-59 #4)
% ============================================================================
% Run (from prolog/):
%   swipl -g "[stack], corpus_loader:load_all_testsets, \
%     [tests/test_axiom_diff], run_tests, halt" -t "halt(1)"
%
% Freezes the #4 findings:
%   - exact_name self-diff = all-agreement / 0 disparity / 0 blind (positive control)
%   - exact_name cross-reading (absolute pair) = all-blind (no mechanical axiom
%     identity: the two readings share zero axiom names)
%   - concept key with a DECLARED seat (the 4 westphalian-absolute axioms mapped
%     to 2 concepts) = the grounding INVERSION: 2 disparity vantages, 0 blind
%     (sovereignty_absolute: conventional vs deontological; noninterference:
%      deontological vs conventional)
% ============================================================================

:- use_module(axiom_diff).

abs_a(westphalia_sovereignty__absolute_non_intervention).   % conventional sovereignty / deontological interference
abs_b(westphalian_sovereignty__absolute_sovereignty).       % deontological sovereignty / conventional interference

:- begin_tests(axiom_diff).

% --- Positive control: self-diff under exact_name sees identity --------------
test(self_diff_exact_name_all_agree) :-
    abs_a(A),
    axiom_diff:axiom_diff(A, A, exact_name, Agree, Disp, Blind),
    Disp == [], Blind == [],
    axiom_diff:axioms_of(A, Axs), length(Axs, NAx),
    length(Agree, NAg),
    NAg =:= NAx.

% --- The structural finding: exact_name cross-reading is all-blind -----------
test(absolute_pair_exact_name_all_blind) :-
    abs_a(A), abs_b(B),
    axiom_diff:axiom_diff(A, B, exact_name, Agree, Disp, Blind),
    Agree == [], Disp == [],
    % every axiom on both sides is blind (no shared name)
    axiom_diff:axioms_of(A, AxA), axiom_diff:axioms_of(B, AxB),
    length(AxA, NA), length(AxB, NB),
    length(Blind, NBl),
    NBl =:= NA + NB.

% --- The declared seat: concept key reveals the grounding INVERSION ----------
test(absolute_pair_concept_grounding_inversion,
     [ setup(declare_westphalian_concepts), cleanup(retract_westphalian_concepts) ]) :-
    abs_a(A), abs_b(B),
    axiom_diff:axiom_diff(A, B, concept, Agree, Disp, Blind),
    Agree == [], Blind == [],
    length(Disp, 2),
    % sovereignty_absolute: A conventional vs B deontological
    memberchk(disparity(sovereignty_absolute, [conventional], [deontological]), Disp),
    % noninterference: A deontological vs B conventional  (the inversion)
    memberchk(disparity(noninterference, [deontological], [conventional]), Disp).

test(verdict_key_fragile_with_concept_seat,
     [ setup(declare_westphalian_concepts), cleanup(retract_westphalian_concepts) ]) :-
    abs_a(A), abs_b(B),
    % exact_name -> 0 disparity (all blind); concept -> 2 disparity => regime flips
    axiom_diff:ax_stability_verdict(A, B, V),
    V == key_fragile.

:- end_tests(axiom_diff).

% The declared alignment seat (the human's ruling on which axioms are "the same
% axiom"): the two readings' bespoke names mapped onto two shared concepts.
declare_westphalian_concepts :-
    assertz(axiom_diff:axiom_concept(territorial_sovereignty_categorically_inviolable, sovereignty_absolute)),
    assertz(axiom_diff:axiom_concept(sovereignty_unconditionally_protected, sovereignty_absolute)),
    assertz(axiom_diff:axiom_concept(external_interference_per_se_illegitimate, noninterference)),
    assertz(axiom_diff:axiom_concept(non_interference_categorically_legitimate, noninterference)).

retract_westphalian_concepts :-
    retractall(axiom_diff:axiom_concept(_, _)).
