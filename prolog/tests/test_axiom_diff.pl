% ============================================================================
% TESTS: axiom_diff — the cyclopean operator at the axiom layer (OQ-59 #4)
% ============================================================================
% Run (from prolog/): CORPUS-INDEPENDENT since 2026-07-04 —
%   swipl -g "[stack], [tests/test_axiom_diff], run_tests, halt" -t "halt(1)"
% (Originally these tests read the westphalia readings from the LIVE corpus.
%  The 2026-06-20 regime swap regenerated the twins with different bespoke
%  axiom names and removed the pair from testsets/, so the corpus-anchored
%  version was silently unrunnable-green from then on — no leg carries the
%  4 mapped names. Fixed 2026-07-04 by freezing the original substrate
%  (axiom names + groundings recorded in the OQ-59 ruling) as fixture facts:
%  the tests' value is the frozen INVERSION finding and the join behavior,
%  not live corpus content. OQ-72 registry tests below are likewise
%  fixture-local.)
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

abs_a(oq59_wf_fixture_absolute_non_intervention).   % conventional sovereignty / deontological interference
abs_b(oq59_wf_fixture_absolute_sovereignty).        % deontological sovereignty / conventional interference

% Frozen substrate of the original westphalia pair (the OQ-59 #4 ruling era):
% the corpus-resident originals were regenerated with new bespoke names in the
% 2026-06-20 regime swap, so the ruling's substrate is preserved HERE.
:- multifile narrative_ontology:cs_story_uid/2.
:- multifile narrative_ontology:cs_axiom/3.
:- multifile narrative_ontology:cs_axiom_grounding/3.

narrative_ontology:cs_story_uid(oq59_wf_fixture_absolute_non_intervention, 'oq59-wf-fixture-a').
narrative_ontology:cs_axiom('oq59-wf-fixture-a', foundational, territorial_sovereignty_categorically_inviolable).
narrative_ontology:cs_axiom_grounding('oq59-wf-fixture-a', territorial_sovereignty_categorically_inviolable, conventional).
narrative_ontology:cs_axiom('oq59-wf-fixture-a', foundational, external_interference_per_se_illegitimate).
narrative_ontology:cs_axiom_grounding('oq59-wf-fixture-a', external_interference_per_se_illegitimate, deontological).
narrative_ontology:cs_story_uid(oq59_wf_fixture_absolute_sovereignty, 'oq59-wf-fixture-b').
narrative_ontology:cs_axiom('oq59-wf-fixture-b', foundational, sovereignty_unconditionally_protected).
narrative_ontology:cs_axiom_grounding('oq59-wf-fixture-b', sovereignty_unconditionally_protected, deontological).
narrative_ontology:cs_axiom('oq59-wf-fixture-b', foundational, non_interference_categorically_legitimate).
narrative_ontology:cs_axiom_grounding('oq59-wf-fixture-b', non_interference_categorically_legitimate, conventional).

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
    % Retract ONLY the four test-local seat declarations — the RATIFIED
    % registry facts (axiom_concept_registry.pl, OQ-72) must survive cleanup;
    % a blanket retractall here would silently unload the baked seat for the
    % rest of the session.
    retractall(axiom_diff:axiom_concept(territorial_sovereignty_categorically_inviolable, _)),
    retractall(axiom_diff:axiom_concept(sovereignty_unconditionally_protected, _)),
    retractall(axiom_diff:axiom_concept(external_interference_per_se_illegitimate, _)),
    retractall(axiom_diff:axiom_concept(non_interference_categorically_legitimate, _)).

% ============================================================================
% OQ-72 REGRESSION: the RATIFIED registry drives the concept key
% ============================================================================
% Frozen substrate snapshot of the digital_money_legitimacy pilot pair
% (sovereign_cbdc_reading vs crypto_permissionless_reading; R3-ratified
% assignments 2026-07-04, audits/2026-07-03_oq72_concept_key_pilot/). The
% axiom_concept/2 facts come from axiom_concept_registry.pl loaded by stack.pl
% — deliberately NOT asserted here: if the registry loses these mappings, the
% concept diff below reverts toward all-blind and these tests go red.
% Fixture UIDs are synthetic; axioms_of/2 keys by UID, so reusing the real
% axiom names cannot collide with corpus-loaded readings.

:- multifile narrative_ontology:cs_story_uid/2.
:- multifile narrative_ontology:cs_axiom/3.
:- multifile narrative_ontology:cs_axiom_grounding/3.

narrative_ontology:cs_story_uid(oq72_reg_fixture_cbdc, 'oq72-reg-fixture-a').
narrative_ontology:cs_axiom('oq72-reg-fixture-a', foundational, state_monopoly_on_legitimate_issuance).
narrative_ontology:cs_axiom_grounding('oq72-reg-fixture-a', state_monopoly_on_legitimate_issuance, conventional).
narrative_ontology:cs_axiom('oq72-reg-fixture-a', secondary, transaction_visibility_required_for_policy).
narrative_ontology:cs_axiom_grounding('oq72-reg-fixture-a', transaction_visibility_required_for_policy, instrumental).
narrative_ontology:cs_story_uid(oq72_reg_fixture_crypto, 'oq72-reg-fixture-b').
narrative_ontology:cs_axiom('oq72-reg-fixture-b', foundational, consensus_suffices_for_legitimacy).
narrative_ontology:cs_axiom_grounding('oq72-reg-fixture-b', consensus_suffices_for_legitimacy, conventional).
narrative_ontology:cs_axiom('oq72-reg-fixture-b', foundational, state_permission_unnecessary).
narrative_ontology:cs_axiom_grounding('oq72-reg-fixture-b', state_permission_unnecessary, deontological).

:- begin_tests(axiom_concept_registry).

% The registry is LOADED (via stack.pl), not test-asserted: the mapping for the
% C1 pair must hold with no setup at all.
test(registry_loaded_c1_pair_mapped) :-
    axiom_diff:axiom_concept(state_monopoly_on_legitimate_issuance, C),
    axiom_diff:axiom_concept(consensus_suffices_for_legitimacy, C),
    C == digital_money_legitimacy__issuance_legitimacy_basis.

% exact_name stays structurally all-blind on the pilot pair (2+2 bespoke names).
test(pilot_pair_exact_name_all_blind) :-
    axiom_diff:axiom_diff(oq72_reg_fixture_cbdc, oq72_reg_fixture_crypto,
                          exact_name, Agree, Disp, Blind),
    Agree == [], Disp == [],
    length(Blind, 4).

% Under the ratified registry the same pair yields the non-degenerate diff:
% the contradiction-pair concept surfaces as DISPARITY (same subject, opposed
% grounding sets — the westphalia shape), the unpaired concept reads blind.
test(pilot_pair_concept_key_disparity) :-
    axiom_diff:axiom_diff(oq72_reg_fixture_cbdc, oq72_reg_fixture_crypto,
                          concept, Agree, Disp, Blind),
    Agree == [],
    Disp == [disparity(digital_money_legitimacy__issuance_legitimacy_basis,
                       [conventional], [conventional, deontological])],
    Blind == [blind(digital_money_legitimacy__transaction_visibility, a, [instrumental])].

:- end_tests(axiom_concept_registry).
