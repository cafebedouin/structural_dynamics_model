% ============================================================================
% CONSTRAINT STORY: behaviorist_counterfactual_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-19
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_behaviorist_counterfactual_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: behaviorist_counterfactual_reading
 *   human_readable: Behaviorist Counterfactual Test of Preference Authenticity
 *   domain: moral_psychology/philosophy_of_autonomy/political_theory
 *
 * SUMMARY:
 *   This story instantiates the behaviorist-counterfactual reading of the
 *   authentic preference boundary: authenticity is defined entirely by
 *   whether a stated preference would survive being handed back its
 *   foreclosed alternative, with no residual fact about authenticity beyond
 *   what the test reveals. Applied at institutional scale, this criterion is
 *   used by policy designers and choice-architecture institutions to certify
 *   preferences as authentic (and hence not requiring correction) whenever
 *   the disposition-under-re-exposure test cannot be run in the agent's
 *   favor, or when running it confirms the adapted preference. The reading's
 *   structural signature is its treatment of permanent foreclosure: for
 *   agents whose world will never hand back the option (Sen's classic cases —
 *   famine-adjusted expectations, historically destroyed occupational
 *   mobility, caste- or colonially-foreclosed alternatives), the question of
 *   authenticity does not remain open, it becomes categorically inapplicable.
 *   That is a stronger and more extractive move than merely lacking data — it
 *   removes the conceptual standing to ask the question at all for exactly
 *   the population most likely to be harmed by an unexamined answer.
 *
 * KEY AGENTS:
 *   - adaptive_preference_theorists: administer the operational criterion (institutional/analytical)
 *   - policy_designers_using_revealed_choice: apply the test to justify interventions (institutional/mobile)
 *   - institutions_administering_choice_architecture: shape preferences and certify them via the same mechanism (organized/arbitrage)
 *   - permanently_foreclosed_option_holders: for whom the test can never run — Sen's cases (powerless/trapped)
 *   - identity_adapted_subordinated_agents: whose trained disposition is indistinguishable from settled preference under this test (powerless/identity_locked)
 *   - philosophers_of_autonomy_outside_test: excluded competing readings (phenomenological, genealogical, capability) (moderate/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(behaviorist_counterfactual_reading, 0.61).
domain_priors:suppression_score(behaviorist_counterfactual_reading, 0.58).
domain_priors:theater_ratio(behaviorist_counterfactual_reading, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(behaviorist_counterfactual_reading, extractiveness, 0.61).
narrative_ontology:constraint_metric(behaviorist_counterfactual_reading, suppression_requirement, 0.58).
narrative_ontology:constraint_metric(behaviorist_counterfactual_reading, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(behaviorist_counterfactual_reading, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(behaviorist_counterfactual_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(behaviorist_counterfactual_reading, tangled_rope).
narrative_ontology:human_readable(behaviorist_counterfactual_reading, "Behaviorist Counterfactual Test of Preference Authenticity").
narrative_ontology:topic_domain(behaviorist_counterfactual_reading, "moral_psychology/philosophy_of_autonomy/political_theory").

domain_priors:requires_active_enforcement(behaviorist_counterfactual_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(behaviorist_counterfactual_reading, '463eb310-a4c6-4343-af5a-15c198150ad0').
narrative_ontology:cs_kernel_codification('463eb310-a4c6-4343-af5a-15c198150ad0', distributed).
narrative_ontology:cs_authority_grounding('463eb310-a4c6-4343-af5a-15c198150ad0', distributed).
narrative_ontology:cs_reading_relation('463eb310-a4c6-4343-af5a-15c198150ad0', authentic_preference_boundary__phenomenological_endorsement_reading, coexists_with).
narrative_ontology:cs_reading_relation('463eb310-a4c6-4343-af5a-15c198150ad0', authentic_preference_boundary__genealogical_origin_reading, coexists_with).
narrative_ontology:cs_reading_relation('463eb310-a4c6-4343-af5a-15c198150ad0', authentic_preference_boundary__capability_traction_reading, influences).
narrative_ontology:cs_axiom('463eb310-a4c6-4343-af5a-15c198150ad0', foundational, authenticity_exhausted_by_disposition).
narrative_ontology:cs_axiom_status(authenticity_exhausted_by_disposition, holdable).
narrative_ontology:cs_axiom_grounding('463eb310-a4c6-4343-af5a-15c198150ad0', authenticity_exhausted_by_disposition, conventional).
narrative_ontology:cs_axiom('463eb310-a4c6-4343-af5a-15c198150ad0', foundational, no_fact_beyond_counterfactual_test_result).
narrative_ontology:cs_axiom_status(no_fact_beyond_counterfactual_test_result, holdable).
narrative_ontology:cs_axiom_grounding('463eb310-a4c6-4343-af5a-15c198150ad0', no_fact_beyond_counterfactual_test_result, empirically_contingent).
narrative_ontology:cs_axiom('463eb310-a4c6-4343-af5a-15c198150ad0', secondary, permanent_foreclosure_voids_rather_than_defers_question).
narrative_ontology:cs_axiom_status(permanent_foreclosure_voids_rather_than_defers_question, holdable).
narrative_ontology:cs_axiom_grounding('463eb310-a4c6-4343-af5a-15c198150ad0', permanent_foreclosure_voids_rather_than_defers_question, empirically_contingent).
narrative_ontology:cs_reference_frame('463eb310-a4c6-4343-af5a-15c198150ad0', operationalist_dispositional_criterion).
narrative_ontology:cs_drift_state('463eb310-a4c6-4343-af5a-15c198150ad0', post_adaptive_preferences_literature, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('463eb310-a4c6-4343-af5a-15c198150ad0', '').
narrative_ontology:cs_kernel_id(behaviorist_counterfactual_reading, authentic_preference_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(behaviorist_counterfactual_reading, adaptive_preference_theorists).
narrative_ontology:constraint_beneficiary(behaviorist_counterfactual_reading, policy_designers_using_revealed_choice).
narrative_ontology:constraint_beneficiary(behaviorist_counterfactual_reading, institutions_administering_choice_architecture).
narrative_ontology:constraint_victim(behaviorist_counterfactual_reading, permanently_foreclosed_option_holders).
narrative_ontology:constraint_victim(behaviorist_counterfactual_reading, identity_adapted_subordinated_agents).
narrative_ontology:constraint_victim(behaviorist_counterfactual_reading, colonized_and_caste_bound_populations).
narrative_ontology:constraint_vindicates(behaviorist_counterfactual_reading, authenticity_is_exhausted_by_disposition).
narrative_ontology:constraint_vindicates(behaviorist_counterfactual_reading, no_residual_first_person_fact_of_preference).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Construct and defend the counterfactual-re-exposure test as the operational definition of authentic preference. They administer the criterion in academic and policy discourse, deciding what counts as a valid 'handing back' of the foreclosed option, and their professional standing rests on the test being treated as exhaustive rather than partial.
narrative_ontology:constraint_stakeholder(behaviorist_counterfactual_reading, adaptive_preference_theorists, agenda_setter,
    institutional, generational, analytical, global).

% Use the behaviorist test to justify policy interventions (welfare eligibility, paternalistic nudges, development programs) by asking whether a stated preference would survive re-exposure to a foregone option. Because the test requires only observable disposition, it lets them bypass costly first-person testimony and claim empirical rigor for contested normative judgments.
narrative_ontology:constraint_stakeholder(behaviorist_counterfactual_reading, policy_designers_using_revealed_choice, beneficiary,
    institutional, biographical, mobile, national).

% Design the environments (labor markets, marriage markets, consumer platforms) in which preferences form and are later tested. They benefit twice: once from shaping the preference, and again from being the party that certifies its authenticity via the counterfactual test they also control access to.
narrative_ontology:constraint_stakeholder(behaviorist_counterfactual_reading, institutions_administering_choice_architecture, beneficiary,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(behaviorist_counterfactual_reading, institutions_administering_choice_architecture, agenda_setter).

% Sen's cases: the widow who never learns literacy exists as an option, the famine survivor whose baseline expectations have permanently adjusted downward, the person born into a closed social role whose counterfactual world will structurally never materialize. For them the re-exposure test cannot be run even in principle — not merely 'not yet run.' Under this reading, their preference has no answer, ever; the question of authenticity is not deferred but voided.
narrative_ontology:constraint_stakeholder(behaviorist_counterfactual_reading, permanently_foreclosed_option_holders, payer,
    powerless, civilizational, trapped, global).

% Have internalized a subordinate role (domestic servitude, caste occupation, gendered division of labor) so thoroughly that their disposition toward the foreclosed alternative, if ever tested, would likely reject it — not because the alternative is undesired in some deeper sense but because the test measures only the trained disposition, which the same system that foreclosed the option also shaped. The test cannot distinguish a genuinely settled preference from a successfully adapted one.
narrative_ontology:constraint_stakeholder(behaviorist_counterfactual_reading, identity_adapted_subordinated_agents, payer,
    powerless, generational, identity_locked, national).

% Entire populations whose historical alternatives (pre-colonial economic forms, occupational mobility across caste lines) were destroyed at civilizational scale, not individual scale. There is no re-exposure mechanism that could ever restore the original counterfactual, so the dispositional test is structurally inapplicable to them yet is still invoked by outside observers to assess whether their current preferences are 'authentic.'
narrative_ontology:constraint_stakeholder(behaviorist_counterfactual_reading, colonized_and_caste_bound_populations, payer,
    powerless, civilizational, trapped, continental).

% Phenomenologists, genealogists, and capability theorists who argue authenticity has content beyond dispositional survival under counterfactual re-exposure — first-person endorsement, causal history of formation, or capability sets. They are structurally excluded from this reading's own definitional frame: the behaviorist reading treats their objections as unfalsifiable metaphysics rather than as competing accounts of the same phenomenon.
narrative_ontology:constraint_stakeholder(behaviorist_counterfactual_reading, philosophers_of_autonomy_outside_test, excluded,
    moderate, civilizational, analytical, global).

% Trace the logical consequences of defining authenticity purely behaviorally: that for agents in permanently foreclosed positions the concept becomes inapplicable rather than merely unresolved, and that the test cannot in principle distinguish adaptation from settled preference because both produce the same disposition.
narrative_ontology:constraint_stakeholder(behaviorist_counterfactual_reading, philosophical_and_empirical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(behaviorist_counterfactual_reading, institutions_administering_choice_architecture).
narrative_ontology:fixing_cost_class(behaviorist_counterfactual_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a testable, non-metaphysical criterion for authenticity that can be operationalized in policy and empirical research without requiring access to contested first-person facts — solving the genuine problem that 'what someone really wants' is otherwise unverifiable from the outside.
% TRANSFER_FUNCTION: Moves the authority to certify preferences as authentic from the agent's own first-person report to whichever institution controls or can simulate the counterfactual re-exposure — and moves the cost of permanent foreclosure onto those for whom no such re-exposure will ever occur, converting their situation from 'unresolved' to 'definitionally outside the concept.'
% ABSENT_VOICES: Agents in Sen-type permanent foreclosure have no seat in the discourse that defines their preferences as untestable-therefore-inapplicable; phenomenological and genealogical theorists are treated as making category errors rather than raising genuine counter-readings within the same debate.
% DISAPPEARANCE_RATIONALE: If the behaviorist criterion vanished, policy designers and choice-architecture institutions would lose their preferred operational test and would need to fall back on first-person testimony, capability assessment, or genealogical scrutiny — each of which reallocates authority differently. Adaptive preference theorists dispute whether the world would rearrange (they hold the test tracks something real) while capability and genealogical theorists hold that removing it would simply expose that authenticity work was already being done by other, more defensible means.
% FOUNDING_PROBLEM: How can anyone, including the agent themselves, verify that a stated preference is not simply an adaptation to unjust constraint, without appealing to unfalsifiable introspection?
% FOUNDING_PROBLEM_CORROBORATION: Adaptive preference theorists and policy designers attest the problem remains live and that the test is the best available instrument. Capability theorists (Nussbaum, Sen's own later work) and phenomenological theorists, writing from outside the beneficiary set, attest that the behaviorist reduction does not solve the founding problem but merely relocates it — it cannot distinguish a survived-preference from a successfully-installed one, which was the original worry.
narrative_ontology:disappearance_verdict(behaviorist_counterfactual_reading, contested).
narrative_ontology:founding_problem_status(behaviorist_counterfactual_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(behaviorist_counterfactual_reading, '8080348c4e16a265fafc924dcde83360dfd170fc',
    'becd0f87568a1ec0be97d1229ae702098dbd6568', '2026-07-16',
    'unspecified', 'agent/example_platform_commission.json',
    'claude-sonnet-5', 'unspecified').
narrative_ontology:story_seed(behaviorist_counterfactual_reading, 'none', 1).
narrative_ontology:epsilon_provenance(behaviorist_counterfactual_reading, 0.61, 'claude-sonnet-5', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(behaviorist_counterfactual_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(behaviorist_counterfactual_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(behaviorist_counterfactual_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.61) and suppression (0.58) are substantial but not extreme: the coordination function (a verifiable, non-metaphysical criterion for a genuinely hard problem) is real, which keeps this well short of a pure snare. Accessibility collapse is high (0.72) because once the behaviorist criterion is accepted as exhaustive, alternative accounts of authenticity (phenomenological, genealogical, capability-based) are treated as unfalsifiable rather than as live competitors — the conceptual space collapses even though the test itself does not physically coerce anyone. Resistance is moderate (0.45): capability theorists and phenomenologists actively contest the reduction, but the affected populations (permanently foreclosed agents) mostly cannot resist because the very criterion that would need contesting is inapplicable to their situation by construction.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda-setter seat (adaptive preference theorists), the test is a triumph of operationalism — it replaces contested introspection with observable disposition. From the payer seat (permanently foreclosed populations), the same criterion is experienced as a door that closes twice: first the original foreclosure, then the conceptual foreclosure of even being able to ask whether their preference is authentic. The engine should compute a tangled-rope reading from the agenda-setter seat's data and a considerably harsher reading from the payer seats' data, given identical base extractiveness — the seat divergence is the point of this story.
 *
 * DIRECTIONALITY LOGIC:
 *   Adaptive preference theorists and the institutions that administer choice architecture are structural beneficiaries: they gain analytical/operational authority and policy legitimacy from a criterion they control the application of. Permanently foreclosed agents and identity-adapted subordinated agents are structural targets: the test's inapplicability or false-positive risk falls entirely on them, and their trapped/identity-locked exit options mean the derivation correctly pushes their directionality toward the full-target end — there is no arbitrage available to a population for whom the counterfactual literally cannot be constructed.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (verifying preference against unfalsifiable introspection) remains partially live in ordinary cases, but for the permanently-foreclosed subpopulation the mandate has already outlived any defensible function: the test was designed to distinguish authentic preference from adaptation, and for exactly the population where that distinction matters most (people who adapted to unjust constraint), the test cannot be run and defaults to treating the adapted disposition as the only available datum — which is precisely the false-positive the test was meant to prevent. This is not mandatrophy in the sense of an institution outliving its purpose; it is a narrower failure where the criterion's domain of applicability silently excludes its hardest cases while continuing to be invoked as if it covered them.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_divergence_authentic_preference_boundary,
    'Is authenticity exhausted by dispositional behavior under counterfactual re-exposure (this reading), or does it retain first-person, genealogical, or capability content that survives even when re-exposure is impossible (the sibling readings)?',
    'No empirical test resolves this by construction — it is the meta-question of which criterion of authenticity is correct. Resolution would require philosophical argument about whether unfalsifiability of a criterion (first-person endorsement, causal history, capability presence) counts against its validity or merely against its testability.',
    'Under this reading, permanently foreclosed agents have no authenticity fact about their preferences at all — the question is voided. Under the phenomenological or genealogical siblings, their preferences could still be judged inauthentic (or authentic) on grounds independent of re-exposure, restoring standing to ask the question even where the test cannot run. The choice of reading determines whether an entire class of agents (Sen''s cases) has any conceptual purchase on their own authenticity.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(kernel_reading_divergence_authentic_preference_boundary, conceptual, 'Which kernel reading of authentic_preference_boundary governs — this dispositional one or a sibling with residual first-person, genealogical, or capability content.').

omega_variable(
    adaptation_versus_settlement_indistinguishability,
    'Can the counterfactual re-exposure test, even where it CAN be run, actually distinguish a genuinely settled preference from a successfully installed adaptation — or does it necessarily conflate them because both produce the same disposition?',
    'Would require an independent (non-dispositional) marker of adaptation versus settlement to check the test against — but the whole point of the behaviorist reading is to deny that any such independent marker exists or matters. This is close to unfalsifiable within the reading''s own terms.',
    'If the test cannot distinguish these cases even in principle, then wherever it returns a positive result (preference survives re-exposure) for an agent shaped by unjust constraint, the reading is certifying adaptation as authenticity — which is the core objection genealogical theorists raise against this specific reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(adaptation_versus_settlement_indistinguishability, conceptual, 'Whether the dispositional test can separate settled preference from trained adaptation even in cases where it is administrable.').

omega_variable(
    who_administers_counterfactual_construction,
    'Who decides what counts as a faithful re-exposure to the foreclosed alternative, and does that administrative discretion itself introduce extraction?',
    'Trace specific policy applications (e.g., welfare-to-work programs, development interventions) to see whether the institution administering the re-exposure test also benefits from a particular outcome of that test, which would indicate circularity rather than neutral measurement.',
    'If the administering institution has a stake in the test''s outcome, the ''objective, dispositional'' framing is cover for a discretionary judgment call — pushing the classification from tangled_rope toward snare for the populations subject to institutionally-administered re-exposure.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(who_administers_counterfactual_construction, empirical, 'Whether the institutions administering the counterfactual test have a stake in its results, undermining claimed neutrality.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(behaviorist_counterfactual_reading, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(beha_tr_t0, behaviorist_counterfactual_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(beha_tr_t8, behaviorist_counterfactual_reading, theater_ratio, 8, 0.16).
narrative_ontology:measurement(beha_tr_t16, behaviorist_counterfactual_reading, theater_ratio, 16, 0.19).
narrative_ontology:measurement(beha_tr_t24, behaviorist_counterfactual_reading, theater_ratio, 24, 0.22).
narrative_ontology:measurement(beha_tr_t32, behaviorist_counterfactual_reading, theater_ratio, 32, 0.25).
narrative_ontology:measurement(beha_tr_t40, behaviorist_counterfactual_reading, theater_ratio, 40, 0.28).

% Extraction over time
narrative_ontology:measurement(beha_be_t0, behaviorist_counterfactual_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(beha_be_t8, behaviorist_counterfactual_reading, base_extractiveness, 8, 0.48).
narrative_ontology:measurement(beha_be_t16, behaviorist_counterfactual_reading, base_extractiveness, 16, 0.53).
narrative_ontology:measurement(beha_be_t24, behaviorist_counterfactual_reading, base_extractiveness, 24, 0.57).
narrative_ontology:measurement(beha_be_t32, behaviorist_counterfactual_reading, base_extractiveness, 32, 0.6).
narrative_ontology:measurement(beha_be_t40, behaviorist_counterfactual_reading, base_extractiveness, 40, 0.61).

% Suppression requirement over time
narrative_ontology:measurement(beha_su_t0, behaviorist_counterfactual_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(beha_su_t8, behaviorist_counterfactual_reading, suppression_requirement, 8, 0.41).
narrative_ontology:measurement(beha_su_t16, behaviorist_counterfactual_reading, suppression_requirement, 16, 0.46).
narrative_ontology:measurement(beha_su_t24, behaviorist_counterfactual_reading, suppression_requirement, 24, 0.51).
narrative_ontology:measurement(beha_su_t32, behaviorist_counterfactual_reading, suppression_requirement, 32, 0.55).
narrative_ontology:measurement(beha_su_t40, behaviorist_counterfactual_reading, suppression_requirement, 40, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(behaviorist_counterfactual_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(behaviorist_counterfactual_reading, 0.08).
narrative_ontology:affects_constraint(behaviorist_counterfactual_reading, phenomenological_endorsement_reading).
narrative_ontology:affects_constraint(behaviorist_counterfactual_reading, genealogical_origin_reading).
narrative_ontology:affects_constraint(behaviorist_counterfactual_reading, capability_traction_reading).

% DUAL FORMULATION NOTE:
% Four sibling stories decompose the single natural-language concept 'authentic preference' (the authentic_preference_boundary kernel) into structurally distinct constraints, each with its own ε and victim set. This story (behaviorist_counterfactual_reading) is the most extractive toward permanently-foreclosed populations because it is the only reading that treats their situation as voiding the authenticity question rather than leaving it open or answerable by other means. phenomenological_endorsement_reading, genealogical_origin_reading, and capability_traction_reading are separate files with their own metrics; do not average ε across them.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
