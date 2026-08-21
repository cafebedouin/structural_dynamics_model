% ============================================================================
% CONSTRAINT STORY: temple_sacrifice_obligation__messianic_suspension
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_temple_sacrifice_obligation__messianic_suspension, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
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
 *   constraint_id: temple_sacrifice_obligation__messianic_suspension
 *   human_readable: Temple Sacrifice Obligation: Messianic Suspension Reading
 *   domain: religious/halakhic_authority/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents a specific halakhic (Jewish legal) reading of
 *   the obligation for Temple sacrifices after the destruction of the Second
 *   Temple. It posits that the obligation is neither fulfilled nor violated,
 *   but rather suspended, awaiting the messianic restoration of the Temple.
 *   This reading provides a coherent theological framework for adherents,
 *   relieving them of an impossible burden while maintaining the sanctity and
 *   future relevance of the commandment. It is one of several interpretations
 *   within a broader kernel concerning the status of Temple obligations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(temple_sacrifice_obligation__messianic_suspension, 0.05).
domain_priors:suppression_score(temple_sacrifice_obligation__messianic_suspension, 0.05).
domain_priors:theater_ratio(temple_sacrifice_obligation__messianic_suspension, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, extractiveness, 0.05).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(temple_sacrifice_obligation__messianic_suspension, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(temple_sacrifice_obligation__messianic_suspension, rope).
narrative_ontology:human_readable(temple_sacrifice_obligation__messianic_suspension, "Temple Sacrifice Obligation: Messianic Suspension Reading").
narrative_ontology:topic_domain(temple_sacrifice_obligation__messianic_suspension, "religious/halakhic_authority/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(temple_sacrifice_obligation__messianic_suspension, 'ccbf1bfe-e365-4464-a5c4-3a79f2216c1c').
narrative_ontology:cs_kernel_codification('ccbf1bfe-e365-4464-a5c4-3a79f2216c1c', fixed_text).
narrative_ontology:cs_authority_grounding('ccbf1bfe-e365-4464-a5c4-3a79f2216c1c', lineage).
narrative_ontology:cs_interpretation_layer_present('ccbf1bfe-e365-4464-a5c4-3a79f2216c1c').
narrative_ontology:cs_reading_relation('ccbf1bfe-e365-4464-a5c4-3a79f2216c1c', temple_sacrifice_obligation__study_as_occupation, coexists_with).
narrative_ontology:cs_reading_relation('ccbf1bfe-e365-4464-a5c4-3a79f2216c1c', temple_sacrifice_obligation__study_as_archiving, coexists_with).
narrative_ontology:cs_axiom('ccbf1bfe-e365-4464-a5c4-3a79f2216c1c', foundational, obligation_contingent_on_temple_existence).
narrative_ontology:cs_axiom_status(obligation_contingent_on_temple_existence, holdable).
narrative_ontology:cs_axiom_grounding('ccbf1bfe-e365-4464-a5c4-3a79f2216c1c', obligation_contingent_on_temple_existence, deontological).
narrative_ontology:cs_axiom('ccbf1bfe-e365-4464-a5c4-3a79f2216c1c', foundational, messianic_redemption_restores_temple).
narrative_ontology:cs_axiom_status(messianic_redemption_restores_temple, holdable).
narrative_ontology:cs_axiom_grounding('ccbf1bfe-e365-4464-a5c4-3a79f2216c1c', messianic_redemption_restores_temple, theological).
narrative_ontology:cs_reference_frame('ccbf1bfe-e365-4464-a5c4-3a79f2216c1c', post_second_temple_destruction_halakha).
narrative_ontology:cs_drift_state('ccbf1bfe-e365-4464-a5c4-3a79f2216c1c', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('ccbf1bfe-e365-4464-a5c4-3a79f2216c1c', '').
narrative_ontology:cs_kernel_id(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__messianic_suspension, halakhic_authorities).
narrative_ontology:constraint_beneficiary(temple_sacrifice_obligation__messianic_suspension, adherents).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__messianic_suspension, messianic_era_theology).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__messianic_suspension, divine_mercy_doctrine).
narrative_ontology:constraint_vindicates(temple_sacrifice_obligation__messianic_suspension, halakhic_continuity_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interpret and transmit the understanding that the obligation for Temple sacrifices is suspended, providing theological and practical guidance to the community. They benefit from the stability and coherence this interpretation provides to the religious framework.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, halakhic_authorities, agenda_setter,
    institutional, generational, analytical, global).

% Are relieved of the burden of an obligation that is currently impossible to fulfill, while maintaining their commitment to the tradition. They benefit from the clarity and lack of current practical demands regarding this central religious commandment.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, adherents, beneficiary,
    moderate, biographical, constrained, global).

% Advocate for more immediate or symbolic actions related to the Temple service, sometimes challenging the mainstream halakhic consensus on suspension. Their views are generally outside the dominant discourse on this specific interpretation.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, messianic_activists, excluded,
    organized, biographical, constrained, regional).

% Study the historical development, theological implications, and halakhic reasoning behind the doctrine of messianic suspension, without being directly bound by its practical application.
narrative_ontology:constraint_stakeholder(temple_sacrifice_obligation__messianic_suspension, analytical_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the religious community's understanding and practice regarding a central divine commandment (Temple sacrifices) that cannot currently be fulfilled due to the absence of the Temple, maintaining theological consistency and communal cohesion.
% TRANSFER_FUNCTION: Theological burden of an unfulfillable obligation is transferred from the present generation of adherents to a future messianic era, ensuring the obligation's continuity without current practical demands.
% ABSENT_VOICES: Messianic activists who might argue for a more immediate or symbolic fulfillment of the obligation, or for actions that hasten the messianic era, are largely excluded from the mainstream halakhic discourse that upholds this specific reading of suspension.
% DISAPPEARANCE_RATIONALE: If this understanding of suspension vanished, the entire theological and halakhic framework for understanding the Temple service and its future restoration would collapse. This would lead to profound disorientation, fragmentation, and potentially a crisis of faith within the religious community, as a central commandment would be seen as either permanently nullified or perpetually violated.
% FOUNDING_PROBLEM: How to maintain the integrity and divine authority of the Temple sacrifice obligation and the community's commitment to it, after the destruction of the Second Temple rendered its performance impossible, without declaring the obligation null, violated, or requiring impossible actions.
% FOUNDING_PROBLEM_CORROBORATION: Historical rabbinic texts (Talmud, Midrash, later codes), communal liturgical practices (prayers for Temple restoration), and theological treatises from across centuries attest to this problem and its resolution. This is corroborated by a broad range of non-benefiting scholars and historians of religion who study Jewish legal and theological development.
narrative_ontology:disappearance_verdict(temple_sacrifice_obligation__messianic_suspension, world_rearranges).
narrative_ontology:founding_problem_status(temple_sacrifice_obligation__messianic_suspension, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(temple_sacrifice_obligation__messianic_suspension, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(temple_sacrifice_obligation__messianic_suspension, 'none', 1).
narrative_ontology:epsilon_provenance(temple_sacrifice_obligation__messianic_suspension, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(temple_sacrifice_obligation__messianic_suspension_tests).
:- end_tests(temple_sacrifice_obligation__messianic_suspension_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is very low (0.05) because this reading imposes no current practical burden or cost on adherents; rather, it offers relief and clarity. Suppression is also very low (0.05) as it's a widely accepted theological position that requires no active coercion to maintain. Theater ratio is low (0.10) because the position is a genuine theological and halakhic stance, not a performative cover for other functions. Accessibility collapse is high (0.90) because the physical means for performing sacrifices (the Temple itself) are absent, making the obligation genuinely unfulfillable in the present. Resistance is low (0.10) as this reading is a cornerstone of post-Temple Jewish thought.
 *
 * PERSPECTIVAL GAP:
 *   While this reading is widely accepted, other perspectives exist. For instance, messianic activists might view this suspension as too passive, arguing for more active engagement with the obligation's future. However, within the framework of this specific reading, the suspension is a necessary and divinely sanctioned state, not a failure of will or commitment.
 *
 * DIRECTIONALITY LOGIC:
 *   Halakhic authorities are beneficiaries as they maintain the coherence and authority of the religious system. Adherents are also beneficiaries, as they are freed from an impossible obligation while remaining faithful. There are no direct victims, as the constraint's core function is to suspend an obligation, not to extract from anyone. Messianic activists, while excluded from this specific interpretation's consensus, are not 'victims' of the constraint itself, but rather hold a different reading of the kernel.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    messianic_timing_ambiguity,
    'What is the precise theological and practical implication of the indefinite nature of ''pending messianic restoration''?',
    'Theological consensus on specific messianic signs or events, or a shift in halakhic interpretation regarding the duration of suspension.',
    'If the messianic era is perceived as indefinitely distant or its arrival criteria become highly contested, the ''suspension'' could be re-evaluated, potentially leading to new interpretations of the obligation''s status or the emergence of alternative forms of ''fulfillment''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(messianic_timing_ambiguity, conceptual, 'Ambiguity regarding the timeline and conditions for messianic restoration.').

omega_variable(
    scope_of_suspension,
    'Does the suspension apply to all aspects of the Temple service, or are there preparatory obligations (e.g., architectural planning, priestly training) that remain active even during suspension?',
    'Further halakhic rulings or communal consensus on the scope of active preparation for the Temple''s rebuilding.',
    'If significant preparatory obligations are deemed active, the constraint''s extractiveness might subtly increase for those involved in such preparations, and its classification might shift towards a ''tangled_rope'' for those specific actors, as they bear costs for a future benefit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(scope_of_suspension, conceptual, 'The precise scope of what is suspended versus what remains an active, albeit preparatory, obligation.').

omega_variable(
    legitimacy_of_study_as_fulfillment_challenge,
    'How do sibling readings, particularly ''study_as_occupation'', challenge this reading''s premise of pure suspension by positing study as a form of ''fulfillment'' or ''occupation'' of the obligation?',
    'Analysis of the internal coherence and communal acceptance of each reading, and the extent to which they are seen as mutually exclusive or complementary within the broader halakhic tradition.',
    'If ''study_as_occupation'' gains wider acceptance as a legitimate form of current fulfillment, it could diminish the ''beneficiary'' status of adherents under the ''messianic_suspension'' reading, as a new, active obligation (study) would emerge, potentially increasing perceived extractiveness for those who prefer pure suspension.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legitimacy_of_study_as_fulfillment_challenge, conceptual, 'The conceptual tension between pure suspension and study as a form of active engagement with the obligation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(temple_sacrifice_obligation__messianic_suspension, 0, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(temp_tr_t0, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 0, 0.1).
narrative_ontology:measurement(temp_tr_t500, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 500, 0.1).
narrative_ontology:measurement(temp_tr_t1000, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 1000, 0.1).
narrative_ontology:measurement(temp_tr_t1500, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 1500, 0.1).
narrative_ontology:measurement(temp_tr_t2000, temple_sacrifice_obligation__messianic_suspension, theater_ratio, 2000, 0.1).

% Extraction over time
narrative_ontology:measurement(temp_be_t0, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(temp_be_t500, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 500, 0.05).
narrative_ontology:measurement(temp_be_t1000, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 1000, 0.05).
narrative_ontology:measurement(temp_be_t1500, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 1500, 0.05).
narrative_ontology:measurement(temp_be_t2000, temple_sacrifice_obligation__messianic_suspension, base_extractiveness, 2000, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(temp_su_t0, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 0, 0.05).
narrative_ontology:measurement(temp_su_t500, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 500, 0.05).
narrative_ontology:measurement(temp_su_t1000, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 1000, 0.05).
narrative_ontology:measurement(temp_su_t1500, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 1500, 0.05).
narrative_ontology:measurement(temp_su_t2000, temple_sacrifice_obligation__messianic_suspension, suppression_requirement, 2000, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(temple_sacrifice_obligation__messianic_suspension, identity_coordination).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation__study_as_occupation).
narrative_ontology:affects_constraint(temple_sacrifice_obligation__messianic_suspension, temple_sacrifice_obligation__study_as_archiving).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'temple_sacrifice_obligation' kernel, each representing a distinct halakhic interpretation of the status of Temple sacrifices in the absence of the Temple. This reading focuses on the suspension of the obligation, while others focus on study as a form of engagement.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
