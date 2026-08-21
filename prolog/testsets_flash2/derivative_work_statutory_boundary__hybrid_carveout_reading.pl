% ============================================================================
% CONSTRAINT STORY: derivative_work_statutory_boundary__hybrid_carveout_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_derivative_work_statutory_boundary__hybrid_carveout_reading, []).

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
 *   constraint_id: derivative_work_statutory_boundary__hybrid_carveout_reading
 *   human_readable: Derivative Work Boundary: Hybrid Commercial Carveout
 *   domain: intellectual_property_law/technology_governance/information_economics
 *
 * SUMMARY:
 *   This constraint represents a specific reading of the derivative work
 *   boundary in intellectual property law, where non-commercial
 *   transformative uses are permitted without authorization, but commercial
 *   uses require licensing. It attempts to balance creator incentives with
 *   public access and transformative creativity. This is one reading of the
 *   'derivative_work_statutory_boundary' kernel, distinct from the
 *   'enclosure_reading' (any use requires authorization) and
 *   'coordination_reading' (only fixed recastings are derivative).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.55).
domain_priors:suppression_score(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.65).
domain_priors:theater_ratio(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(derivative_work_statutory_boundary__hybrid_carveout_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(derivative_work_statutory_boundary__hybrid_carveout_reading, tangled_rope).
narrative_ontology:human_readable(derivative_work_statutory_boundary__hybrid_carveout_reading, "Derivative Work Boundary: Hybrid Commercial Carveout").
narrative_ontology:topic_domain(derivative_work_statutory_boundary__hybrid_carveout_reading, "intellectual_property_law/technology_governance/information_economics").

domain_priors:requires_active_enforcement(derivative_work_statutory_boundary__hybrid_carveout_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(derivative_work_statutory_boundary__hybrid_carveout_reading, '55c86917-5fd2-4ef8-b128-76d9e14792bb').
narrative_ontology:cs_kernel_codification('55c86917-5fd2-4ef8-b128-76d9e14792bb', formalized).
narrative_ontology:cs_authority_grounding('55c86917-5fd2-4ef8-b128-76d9e14792bb', lineage).
narrative_ontology:cs_interpretation_layer_present('55c86917-5fd2-4ef8-b128-76d9e14792bb').
narrative_ontology:cs_reading_relation('55c86917-5fd2-4ef8-b128-76d9e14792bb', derivative_work_statutory_boundary__enclosure_reading, coexists_with).
narrative_ontology:cs_reading_relation('55c86917-5fd2-4ef8-b128-76d9e14792bb', derivative_work_statutory_boundary__coordination_reading, coexists_with).
narrative_ontology:cs_axiom('55c86917-5fd2-4ef8-b128-76d9e14792bb', foundational, incentive_to_create_requires_commercial_control).
narrative_ontology:cs_axiom_status(incentive_to_create_requires_commercial_control, holdable).
narrative_ontology:cs_axiom_grounding('55c86917-5fd2-4ef8-b128-76d9e14792bb', incentive_to_create_requires_commercial_control, instrumental).
narrative_ontology:cs_axiom('55c86917-5fd2-4ef8-b128-76d9e14792bb', foundational, transformative_creativity_benefits_public_domain).
narrative_ontology:cs_axiom_status(transformative_creativity_benefits_public_domain, holdable).
narrative_ontology:cs_axiom_grounding('55c86917-5fd2-4ef8-b128-76d9e14792bb', transformative_creativity_benefits_public_domain, deontological).
narrative_ontology:cs_reference_frame('55c86917-5fd2-4ef8-b128-76d9e14792bb', balanced_incentive_and_access).
narrative_ontology:cs_drift_state('55c86917-5fd2-4ef8-b128-76d9e14792bb', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('55c86917-5fd2-4ef8-b128-76d9e14792bb', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, copyright_holders).
narrative_ontology:constraint_beneficiary(derivative_work_statutory_boundary__hybrid_carveout_reading, non_commercial_creators).
narrative_ontology:constraint_victim(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_transformative_developers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Hold the exclusive right to authorize derivative works. Under this reading, they can license commercial transformative uses for a fee, while non-commercial uses are outside their control. They benefit from the revenue stream and the ability to control commercial exploitation.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, copyright_holders, agenda_setter,
    institutional, generational, mobile, global).

% Seek to create new works that build upon existing copyrighted material for commercial gain. They must obtain licenses from copyright holders, incurring costs and potential restrictions. Their options are to pay, negotiate, or abandon commercialization.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, commercial_transformative_developers, payer,
    moderate, biographical, constrained, global).

% Create transformative works (fan fiction, remixes, parodies) without commercial intent. This reading exempts them from needing authorization, allowing them to operate freely without licensing burdens. They are net beneficiaries of this carveout.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, non_commercial_creators, beneficiary,
    powerless, immediate, mobile, global).

% Interpret and apply copyright law, shaping the boundaries of derivative works. They observe the practical effects of this hybrid approach on innovation and cultural production, and may influence future legal developments.
narrative_ontology:constraint_stakeholder(derivative_work_statutory_boundary__hybrid_carveout_reading, legal_scholars_and_courts, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Balances the incentive for original creation (through commercial licensing) with the public interest in transformative creativity (through non-commercial exemptions), aiming to foster both without stifling either.
% TRANSFER_FUNCTION: Moves licensing fees and control over commercial exploitation from commercial transformative developers to copyright holders, while granting creative freedom to non-commercial creators.
% ABSENT_VOICES: Advocates for a broader 'fair use' or 'transformative use' doctrine that would extend exemptions to commercial uses, arguing that such uses also contribute to public good and innovation. They are often marginalized in legislative debates dominated by established copyright industries.
% DISAPPEARANCE_RATIONALE: If this specific carveout vanished, either all transformative uses would require authorization (stifling non-commercial creativity) or none would (eroding copyright holders' control over commercial exploitation), leading to a significant reorganization of creative industries and digital culture.
% FOUNDING_PROBLEM: The tension between protecting original creators' rights and enabling subsequent creators to build upon existing works, particularly in the digital age where remix culture is prevalent and commercial lines are blurred.
% FOUNDING_PROBLEM_CORROBORATION: Legal scholars, digital rights advocates, and industry groups all attest to the ongoing nature of this tension, though they disagree on the optimal balance. Court decisions and legislative proposals frequently address this issue.
narrative_ontology:disappearance_verdict(derivative_work_statutory_boundary__hybrid_carveout_reading, world_rearranges).
narrative_ontology:founding_problem_status(derivative_work_statutory_boundary__hybrid_carveout_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(derivative_work_statutory_boundary__hybrid_carveout_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(derivative_work_statutory_boundary__hybrid_carveout_reading, 'none', 1).
narrative_ontology:epsilon_provenance(derivative_work_statutory_boundary__hybrid_carveout_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(derivative_work_statutory_boundary__hybrid_carveout_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(derivative_work_statutory_boundary__hybrid_carveout_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(derivative_work_statutory_boundary__hybrid_carveout_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.55) is moderate because it imposes costs only on commercial developers, while non-commercial creators are exempt. Suppression (0.65) is significant as it requires active enforcement (litigation, takedowns) to prevent unauthorized commercial exploitation. Theater ratio (0.20) is low, as the distinction between commercial and non-commercial use is a genuine, actively applied legal standard, not merely performative. The metrics reflect the partial extraction and enforcement inherent in a hybrid approach.
 *
 * PERSPECTIVAL GAP:
 *   Copyright holders perceive this as a fair balance that protects their investment while allowing some public use. Commercial developers see it as an extractive gatekeeping mechanism. Non-commercial creators view it as a necessary freedom. The engine's per-seat classification will reflect these structural asymmetries.
 *
 * DIRECTIONALITY LOGIC:
 *   Copyright holders are beneficiaries (d near 0.0) as they retain control and revenue from commercial uses. Non-commercial creators are also beneficiaries (d near 0.0) due to their exemption. Commercial transformative developers are targets (d near 1.0) as they bear the costs of licensing. Legal scholars and courts are observers (d near 0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling the constraint as pure extraction (snare) by acknowledging the genuine coordination function for non-commercial creators. It also avoids mislabeling it as pure coordination (rope) by recognizing the significant extraction from commercial developers. The 'tangled_rope' classification captures this hybrid nature.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    commercial_non_commercial_boundary_drift,
    'How stable is the distinction between ''commercial'' and ''non-commercial'' use in practice, especially with evolving monetization models (e.g., Patreon, ad-supported free content)?',
    'Empirical analysis of court decisions and licensing practices over time, tracking reclassification of uses from non-commercial to commercial, or vice-versa.',
    'If the boundary drifts towards broader ''commercial'' interpretation, extractiveness and suppression would increase for more creators, potentially shifting the constraint closer to a Snare. If it drifts towards broader ''non-commercial'' interpretation, extractiveness would decrease, moving it closer to a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(commercial_non_commercial_boundary_drift, empirical, 'The practical stability and interpretation of the commercial/non-commercial distinction.').

omega_variable(
    transformative_use_threshold_ambiguity,
    'What is the effective threshold for ''transformative use'' that qualifies for the non-commercial carveout, and how consistently is it applied across different media and contexts?',
    'Content analysis of legal rulings and industry guidelines, combined with expert surveys on what constitutes sufficient transformation in various creative fields.',
    'A high and inconsistently applied threshold would increase uncertainty and de facto suppression for non-commercial creators, raising the effective extractiveness. A clear, low threshold would reinforce the beneficiary status of non-commercial creators.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transformative_use_threshold_ambiguity, conceptual, 'Clarity and consistency of the ''transformative use'' standard.').

omega_variable(
    alternative_framing_enclosure_vs_hybrid,
    'Is this constraint a genuine hybrid, or is it a ''soft'' enclosure reading that merely delays full commercialization for some creators?',
    'Comparative legal analysis with jurisdictions that adopt a pure ''enclosure_reading'' versus those with more expansive fair use doctrines. Longitudinal study of creator behavior and market entry under different regimes.',
    'If it''s a soft enclosure, the true extractiveness is higher, and the coordination function for non-commercial creators is more theatrical, pushing the classification closer to a Snare. If it''s a genuine hybrid, the current Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_framing_enclosure_vs_hybrid, conceptual, 'Whether the hybrid carveout is a true balance or a form of delayed enclosure.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(derivative_work_statutory_boundary__hybrid_carveout_reading, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(deri_tr_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(deri_tr_t5, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 5, 0.18).
narrative_ontology:measurement(deri_tr_t10, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(deri_tr_t15, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 15, 0.22).
narrative_ontology:measurement(deri_tr_t20, derivative_work_statutory_boundary__hybrid_carveout_reading, theater_ratio, 20, 0.2).

% Extraction over time
narrative_ontology:measurement(deri_be_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(deri_be_t5, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(deri_be_t10, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 10, 0.55).
narrative_ontology:measurement(deri_be_t15, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 15, 0.57).
narrative_ontology:measurement(deri_be_t20, derivative_work_statutory_boundary__hybrid_carveout_reading, base_extractiveness, 20, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(deri_su_t0, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(deri_su_t5, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 5, 0.62).
narrative_ontology:measurement(deri_su_t10, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(deri_su_t15, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 15, 0.63).
narrative_ontology:measurement(deri_su_t20, derivative_work_statutory_boundary__hybrid_carveout_reading, suppression_requirement, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(derivative_work_statutory_boundary__hybrid_carveout_reading, resource_allocation).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary__enclosure_reading).
narrative_ontology:affects_constraint(derivative_work_statutory_boundary__hybrid_carveout_reading, derivative_work_statutory_boundary__coordination_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'derivative_work_statutory_boundary' kernel. This 'hybrid_carveout_reading' attempts to balance creator rights and transformative use, contrasting with the 'enclosure_reading' (broader rights for original creators) and the 'coordination_reading' (broader rights for transformative users).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
