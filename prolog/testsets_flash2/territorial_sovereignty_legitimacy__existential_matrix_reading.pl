% ============================================================================
% CONSTRAINT STORY: territorial_sovereignty_legitimacy__existential_matrix_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_territorial_sovereignty_legitimacy__existential_matrix_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
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
 *   constraint_id: territorial_sovereignty_legitimacy__existential_matrix_reading
 *   human_readable: Existential Matrix Reading of Territorial Sovereignty Legitimacy
 *   domain: political_theory/international_relations/territorial_sovereignty
 *
 * SUMMARY:
 *   This constraint models the 'existential matrix' reading of territorial
 *   sovereignty legitimacy, where territorial control is seen as a
 *   non-negotiable precondition for collective survival and identity. This
 *   framing renders legal or historical arguments secondary, making conflict
 *   fundamentally zero-sum. The constraint is classified as a Snare due to
 *   its high extraction, suppression, and the identifiable victims it
 *   creates, despite being claimed by its proponents as a 'natural' or
 *   inevitable state of affairs. The metrics reflect the ongoing,
 *   high-intensity conflict and the suppression of alternatives.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.95).
domain_priors:suppression_score(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.98).
domain_priors:theater_ratio(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 0.98).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 0.85).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(territorial_sovereignty_legitimacy__existential_matrix_reading, resistance, 0.99).

% --- Constraint claim ---
narrative_ontology:constraint_claim(territorial_sovereignty_legitimacy__existential_matrix_reading, snare).
narrative_ontology:human_readable(territorial_sovereignty_legitimacy__existential_matrix_reading, "Existential Matrix Reading of Territorial Sovereignty Legitimacy").
narrative_ontology:topic_domain(territorial_sovereignty_legitimacy__existential_matrix_reading, "political_theory/international_relations/territorial_sovereignty").

domain_priors:requires_active_enforcement(territorial_sovereignty_legitimacy__existential_matrix_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(territorial_sovereignty_legitimacy__existential_matrix_reading, '38948b5e-65cb-4df8-95c7-227d5312e438').
narrative_ontology:cs_kernel_codification('38948b5e-65cb-4df8-95c7-227d5312e438', implicit).
narrative_ontology:cs_authority_grounding('38948b5e-65cb-4df8-95c7-227d5312e438', extraction).
narrative_ontology:cs_interpretation_layer_present('38948b5e-65cb-4df8-95c7-227d5312e438').
narrative_ontology:cs_reading_relation('38948b5e-65cb-4df8-95c7-227d5312e438', territorial_sovereignty_legitimacy__covenant_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('38948b5e-65cb-4df8-95c7-227d5312e438', territorial_sovereignty_legitimacy__self_determination_reading, coexists_with).
narrative_ontology:cs_axiom('38948b5e-65cb-4df8-95c7-227d5312e438', foundational, territorial_control_is_existential_precondition).
narrative_ontology:cs_axiom_status(territorial_control_is_existential_precondition, holdable).
narrative_ontology:cs_axiom_grounding('38948b5e-65cb-4df8-95c7-227d5312e438', territorial_control_is_existential_precondition, empirically_contingent).
narrative_ontology:cs_axiom('38948b5e-65cb-4df8-95c7-227d5312e438', foundational, territorial_conflict_is_zero_sum).
narrative_ontology:cs_axiom_status(territorial_conflict_is_zero_sum, holdable).
narrative_ontology:cs_axiom_grounding('38948b5e-65cb-4df8-95c7-227d5312e438', territorial_conflict_is_zero_sum, empirically_contingent).
narrative_ontology:cs_reference_frame('38948b5e-65cb-4df8-95c7-227d5312e438', unconditional_survival_imperative).
narrative_ontology:cs_drift_state('38948b5e-65cb-4df8-95c7-227d5312e438', contemporary_global_interdependence, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('38948b5e-65cb-4df8-95c7-227d5312e438', '').
narrative_ontology:cs_kernel_id(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__existential_matrix_reading, dominant_ethno_national_group).
narrative_ontology:constraint_beneficiary(territorial_sovereignty_legitimacy__existential_matrix_reading, hardline_political_factions).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, subordinate_ethno_national_group).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, peace_advocates).
narrative_ontology:constraint_victim(territorial_sovereignty_legitimacy__existential_matrix_reading, international_law_frameworks).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Perceives territorial control as non-negotiable for its collective survival and identity. Any compromise is seen as an existential threat, justifying maximalist claims and actions. Benefits from the zero-sum framing as it legitimizes aggressive defense and expansion.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, dominant_ethno_national_group, agenda_setter,
    institutional, generational, identity_locked, national).

% Experiences the constraint as a constant threat to its own survival and identity, facing displacement, suppression, and denial of self-determination. Any attempt to assert rights is met with overwhelming force, reinforcing the zero-sum nature of the conflict.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, subordinate_ethno_national_group, payer,
    powerless, generational, trapped, national).

% Thrive on the existential framing, using it to mobilize support, consolidate power, and delegitimize any calls for compromise or peaceful resolution. Their political survival is tied to the perpetuation of the zero-sum conflict.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, hardline_political_factions, beneficiary,
    organized, biographical, constrained, national).

% Attempt to promote dialogue, compromise, and shared solutions, but are marginalized by the dominant existential narrative. Their efforts are often dismissed as naive or traitorous by hardline factions, making their influence minimal.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, peace_advocates, excluded,
    moderate, biographical, constrained, global).

% Provide a juridical basis for territorial claims and conflict resolution, but are rendered largely irrelevant by the existential framing. Their principles are invoked selectively or dismissed entirely when they conflict with perceived survival imperatives.
narrative_ontology:constraint_stakeholder(territorial_sovereignty_legitimacy__existential_matrix_reading, international_law_frameworks, excluded,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(territorial_sovereignty_legitimacy__existential_matrix_reading, international_law_frameworks).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the actions of the dominant group by providing a unifying, existential justification for territorial control and zero-sum conflict, ensuring internal cohesion against perceived external threats.
% TRANSFER_FUNCTION: Transfers resources, security, and identity affirmation to the dominant group by legitimizing the suppression and displacement of the subordinate group, effectively transferring their land and future to the dominant entity.
% ABSENT_VOICES: Any voice advocating for shared sovereignty, territorial compromise, or a non-zero-sum approach is systematically excluded or silenced, as such views directly undermine the existential premise of the conflict. International legal bodies are also effectively sidelined.
% DISAPPEARANCE_RATIONALE: If this existential reading of sovereignty vanished, the entire basis for the conflict would collapse. The dominant group would lose its primary justification for its actions, opening space for juridical and self-determination arguments to gain traction, fundamentally altering the political landscape and power dynamics.
% FOUNDING_PROBLEM: The perceived existential threat to a people's collective survival and identity in a contested territory, leading to a belief that only absolute territorial control can guarantee security.
% FOUNDING_PROBLEM_CORROBORATION: Both dominant and subordinate ethno-national groups, as well as many international observers, attest to the live nature of existential fears, though they attribute the source of the threat differently. Historical narratives and ongoing conflicts provide corroboration from outside the immediate benefiting parties.
narrative_ontology:disappearance_verdict(territorial_sovereignty_legitimacy__existential_matrix_reading, world_rearranges).
narrative_ontology:founding_problem_status(territorial_sovereignty_legitimacy__existential_matrix_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(territorial_sovereignty_legitimacy__existential_matrix_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(territorial_sovereignty_legitimacy__existential_matrix_reading, 'none', 1).
narrative_ontology:epsilon_provenance(territorial_sovereignty_legitimacy__existential_matrix_reading, 0.95, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(territorial_sovereignty_legitimacy__existential_matrix_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(territorial_sovereignty_legitimacy__existential_matrix_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(territorial_sovereignty_legitimacy__existential_matrix_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is extremely high (0.95) because the reading justifies the complete appropriation of land and resources by one group at the expense of another's very existence. Suppression (0.98) is near-total, as any resistance or alternative framing is met with overwhelming force, both military and ideological. Theater ratio (0.85) is high because legal and diplomatic processes are often engaged in performatively, while the underlying existential logic dictates a zero-sum outcome regardless. Resistance (0.99) is also extremely high, reflecting the continuous, violent opposition from the subordinate group whose survival is at stake. Accessibility collapse (0.9) is high because the existential framing makes any alternative (e.g., shared sovereignty, two-state solution) appear as an unacceptable risk to survival, effectively collapsing the space of viable options.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the dominant group, this is a necessary, even 'natural,' response to an existential threat, making it appear as a Mountain or a Rope for survival. From the subordinate group's perspective, it is a pure Snare, designed to dispossess and eliminate. The engine's classification as Snare reflects the objective structural extraction and suppression, regardless of the subjective framing by beneficiaries.
 *
 * DIRECTIONALITY LOGIC:
 *   The dominant ethno-national group and hardline political factions are clear beneficiaries, as the constraint legitimizes their control and power. The subordinate ethno-national group is the primary victim, bearing the full cost of displacement, loss of land, and suppression of identity. Peace advocates and international law frameworks are excluded, as their perspectives are incompatible with the zero-sum logic.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_vs_constructed_existentialism,
    'Is the ''existential threat'' a genuine, irreducible condition of the territory, or is it a constructed narrative maintained by political actors to justify maximalist claims?',
    'Analysis of historical periods of coexistence and cooperation, or counterfactual scenarios where external pressures are removed. If the ''existential'' conflict persists without external instigation, it suggests a more fundamental basis; if it dissipates, it suggests a constructed narrative.',
    'If constructed, the constraint''s extractiveness and suppression are even more clearly a product of human agency and political choice, strengthening the Snare classification. If genuinely natural, it might lean towards a Mountain of human conflict, though still highly extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_vs_constructed_existentialism, conceptual, 'Distinguishing genuine existential threat from politically constructed narratives.').

omega_variable(
    zero_sum_vs_positive_sum_potential,
    'Is the conflict over territorial sovereignty inherently zero-sum, or are there unexploited positive-sum solutions that the existential framing actively suppresses?',
    'Empirical study of successful territorial compromises in other regions, or detailed economic/social modeling of potential shared-sovereignty arrangements. If such models show viable positive-sum outcomes, the zero-sum claim is falsified.',
    'If positive-sum solutions are viable, the constraint''s suppression of alternatives is even more egregious, reinforcing the Snare classification and highlighting the active suppression of beneficial outcomes. If truly zero-sum, the constraint''s high extractiveness is a tragic consequence of irreducible conflict.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(zero_sum_vs_positive_sum_potential, empirical, 'Assessing the true nature of the territorial conflict as zero-sum or potentially positive-sum.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(territorial_sovereignty_legitimacy__existential_matrix_reading, 1948, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(terr_tr_t1948, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 1948, 0.6).
narrative_ontology:measurement(terr_tr_t1967, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 1967, 0.7).
narrative_ontology:measurement(terr_tr_t1993, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 1993, 0.65).
narrative_ontology:measurement(terr_tr_t2000, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 2000, 0.75).
narrative_ontology:measurement(terr_tr_t2014, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 2014, 0.8).
narrative_ontology:measurement(terr_tr_t2024, territorial_sovereignty_legitimacy__existential_matrix_reading, theater_ratio, 2024, 0.85).

% Extraction over time
narrative_ontology:measurement(terr_be_t1948, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 1948, 0.85).
narrative_ontology:measurement(terr_be_t1967, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 1967, 0.9).
narrative_ontology:measurement(terr_be_t1993, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 1993, 0.88).
narrative_ontology:measurement(terr_be_t2000, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 2000, 0.92).
narrative_ontology:measurement(terr_be_t2014, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 2014, 0.94).
narrative_ontology:measurement(terr_be_t2024, territorial_sovereignty_legitimacy__existential_matrix_reading, base_extractiveness, 2024, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(terr_su_t1948, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 1948, 0.8).
narrative_ontology:measurement(terr_su_t1967, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 1967, 0.88).
narrative_ontology:measurement(terr_su_t1993, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 1993, 0.85).
narrative_ontology:measurement(terr_su_t2000, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 2000, 0.92).
narrative_ontology:measurement(terr_su_t2014, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 2014, 0.96).
narrative_ontology:measurement(terr_su_t2024, territorial_sovereignty_legitimacy__existential_matrix_reading, suppression_requirement, 2024, 0.98).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(territorial_sovereignty_legitimacy__existential_matrix_reading, identity_coordination).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy__covenant_continuity_reading).
narrative_ontology:affects_constraint(territorial_sovereignty_legitimacy__existential_matrix_reading, territorial_sovereignty_legitimacy__self_determination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'territorial_sovereignty_legitimacy' kernel. This 'existential_matrix_reading' emphasizes survival and identity as the basis for sovereignty, making conflict zero-sum. It directly influences and is influenced by the 'covenant_continuity_reading' and 'self_determination_reading' by framing the terms of the debate and the perceived stakes.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
