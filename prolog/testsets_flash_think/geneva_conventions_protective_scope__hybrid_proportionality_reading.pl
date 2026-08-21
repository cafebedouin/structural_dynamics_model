% ============================================================================
% CONSTRAINT STORY: geneva_conventions_protective_scope__hybrid_proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_geneva_conventions_protective_scope__hybrid_proportionality_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: geneva_conventions_protective_scope__hybrid_proportionality_reading
 *   human_readable: Geneva Conventions Protective Scope: Hybrid Proportionality Reading
 *   domain: international_humanitarian_law/legal_theory/armed_conflict_studies
 *
 * SUMMARY:
 *   This constraint is the 'hybrid proportionality reading' of the 'Geneva
 *   Conventions Protective Scope' kernel. It emphasizes the scaling of
 *   protections by conflict type (AP I for international armed conflict, AP
 *   II/Common Article 3 for non-international) and the central role of
 *   proportionality analysis in determining their application. This reading
 *   introduces a calculus that can be leveraged by stronger parties, leading
 *   to variable protection for victims. Sibling readings include the
 *   'state-centric reading' and the 'universal-rights reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.65).
domain_priors:suppression_score(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.7).
domain_priors:theater_ratio(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(geneva_conventions_protective_scope__hybrid_proportionality_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(geneva_conventions_protective_scope__hybrid_proportionality_reading, tangled_rope).
narrative_ontology:human_readable(geneva_conventions_protective_scope__hybrid_proportionality_reading, "Geneva Conventions Protective Scope: Hybrid Proportionality Reading").
narrative_ontology:topic_domain(geneva_conventions_protective_scope__hybrid_proportionality_reading, "international_humanitarian_law/legal_theory/armed_conflict_studies").

domain_priors:requires_active_enforcement(geneva_conventions_protective_scope__hybrid_proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(geneva_conventions_protective_scope__hybrid_proportionality_reading, '69931bc5-8a3d-467d-8236-8bb9f44d4420').
narrative_ontology:cs_kernel_codification('69931bc5-8a3d-467d-8236-8bb9f44d4420', fixed_text).
narrative_ontology:cs_authority_grounding('69931bc5-8a3d-467d-8236-8bb9f44d4420', lineage).
narrative_ontology:cs_interpretation_layer_present('69931bc5-8a3d-467d-8236-8bb9f44d4420').
narrative_ontology:cs_reading_relation('69931bc5-8a3d-467d-8236-8bb9f44d4420', geneva_conventions_protective_scope__state_centric_reading, coexists_with).
narrative_ontology:cs_reading_relation('69931bc5-8a3d-467d-8236-8bb9f44d4420', geneva_conventions_protective_scope__universal_rights_reading, influences).
narrative_ontology:cs_axiom('69931bc5-8a3d-467d-8236-8bb9f44d4420', foundational, protection_scales_by_conflict_type).
narrative_ontology:cs_axiom_status(protection_scales_by_conflict_type, holdable).
narrative_ontology:cs_axiom_grounding('69931bc5-8a3d-467d-8236-8bb9f44d4420', protection_scales_by_conflict_type, conventional).
narrative_ontology:cs_axiom('69931bc5-8a3d-467d-8236-8bb9f44d4420', foundational, proportionality_is_key_to_application).
narrative_ontology:cs_axiom_status(proportionality_is_key_to_application, holdable).
narrative_ontology:cs_axiom_grounding('69931bc5-8a3d-467d-8236-8bb9f44d4420', proportionality_is_key_to_application, deontological).
narrative_ontology:cs_reference_frame('69931bc5-8a3d-467d-8236-8bb9f44d4420', balance_military_necessity_humanity).
narrative_ontology:cs_drift_state('69931bc5-8a3d-467d-8236-8bb9f44d4420', contemporary_asymmetric_conflict_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('69931bc5-8a3d-467d-8236-8bb9f44d4420', '').
narrative_ontology:cs_kernel_id(geneva_conventions_protective_scope__hybrid_proportionality_reading, geneva_conventions_protective_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, states_with_military_advantage).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, military_legal_advisors).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilians_in_niac).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, non_state_armed_groups).
narrative_ontology:constraint_victim(geneva_conventions_protective_scope__hybrid_proportionality_reading, humanitarian_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilians_in_iac).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These states interpret and apply IHL, often leveraging the proportionality calculus and conflict classification to minimize perceived constraints on their military operations, while maintaining a veneer of compliance. They benefit from the legal ambiguity.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, states_with_military_advantage, agenda_setter,
    institutional, generational, constrained, global).

% Professionals tasked with interpreting IHL for military operations. Their expertise is crucial in applying proportionality and conflict classification, often in ways that align with state interests, thereby benefiting from the complexity of the framework.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, military_legal_advisors, beneficiary,
    organized, biographical, identity_locked, national).

% Civilians in international armed conflicts (IACs) theoretically receive the highest level of protection under AP I, but their actual protection is still subject to the proportionality calculus applied by belligerents.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilians_in_iac, beneficiary,
    powerless, immediate, trapped, local).

% Civilians in non-international armed conflicts (NIACs) receive lesser protections under AP II/Common Article 3, making them more vulnerable to harm deemed 'proportionate' to military objectives by state or non-state actors.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, civilians_in_niac, payer,
    powerless, immediate, trapped, local).

% These groups are typically subject to NIAC rules, which offer fewer protections than IACs. They are often targets of military operations where proportionality is applied, and their members may not be afforded combatant status, increasing their vulnerability.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, non_state_armed_groups, payer,
    moderate, biographical, constrained, regional).

% NGOs and international bodies that advocate for stronger and more consistent application of IHL, often challenging interpretations of proportionality and conflict classification that lead to civilian harm. They bear the cost of constant legal and moral pressure.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, humanitarian_advocates, payer,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(geneva_conventions_protective_scope__hybrid_proportionality_reading, humanitarian_advocates, observer).

% These courts investigate and prosecute violations of IHL, thereby influencing the interpretation and application of proportionality and conflict classification, though their reach is limited by state cooperation.
narrative_ontology:constraint_stakeholder(geneva_conventions_protective_scope__hybrid_proportionality_reading, international_criminal_courts, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To provide a legal framework for humanitarian protection during armed conflict, balancing military necessity with humanitarian concerns, and differentiating protections based on the classification of the conflict (international vs. non-international).
% TRANSFER_FUNCTION: Transfers the burden of suffering (or the risk of it) from military actors to civilians and combatants whose harm is deemed 'proportionate' to military advantage, or whose conflict type grants lesser protections. It also transfers interpretive power to states and their legal advisors.
% ABSENT_VOICES: Victims of conflicts where proportionality is applied to justify significant civilian harm, and non-state armed groups who lack a strong voice in shaping IHL, are largely absent from the interpretive discourse, their experiences mediated through state-centric legal frameworks.
% DISAPPEARANCE_RATIONALE: If this framework vanished overnight, the entire legal and normative structure for regulating armed conflict would collapse. States would lose a key justification for their actions, and the already precarious protections for civilians and combatants would disappear, leading to greater chaos and suffering.
% FOUNDING_PROBLEM: To mitigate the brutality of armed conflict by establishing minimum standards of humane treatment and protection for those not participating in hostilities, and to regulate the conduct of hostilities.
% FOUNDING_PROBLEM_CORROBORATION: The International Committee of the Red Cross (ICRC), United Nations bodies, human rights organizations, and the ongoing reality of armed conflicts worldwide consistently corroborate the live status of the founding problem and the continued necessity of IHL.
narrative_ontology:disappearance_verdict(geneva_conventions_protective_scope__hybrid_proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(geneva_conventions_protective_scope__hybrid_proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(geneva_conventions_protective_scope__hybrid_proportionality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(geneva_conventions_protective_scope__hybrid_proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(geneva_conventions_protective_scope__hybrid_proportionality_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(geneva_conventions_protective_scope__hybrid_proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(geneva_conventions_protective_scope__hybrid_proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(geneva_conventions_protective_scope__hybrid_proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) stems from the discretion inherent in proportionality analysis and the differential application of protections based on conflict classification, which can be exploited to justify harm. Suppression (0.70) is high due to the binding nature of IHL on states and the lack of viable alternatives to this framework. Theater ratio (0.40) reflects the performative aspect of claiming adherence to IHL while applying interpretations that reduce protection, though the framework still serves a genuine, albeit contested, function. The metrics show a gradual increase in extractiveness and suppression over time, reflecting the evolving challenges and interpretations in modern conflicts.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of states and their legal advisors, this reading provides a necessary balance between military necessity and humanitarian concerns, a functional coordination mechanism. From the perspective of civilians in NIACs and humanitarian advocates, it represents a structure that permits significant extraction of suffering, masked by legalistic proportionality arguments. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   States with military advantage and their legal advisors are beneficiaries, as the framework's complexity allows them to interpret rules in their favor, minimizing constraints. Civilians in NIACs and non-state armed groups are victims, as they receive lesser protections and are more vulnerable to the proportionality calculus. Humanitarian advocates bear the cost of challenging these interpretations. Civilians in IACs are beneficiaries in principle, but their actual protection is still subject to the same interpretive discretion.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_calculus_ambiguity,
    'Is the proportionality calculus an objective legal standard or a subjective justification for military action?',
    'Analysis of judicial precedents from international criminal courts and consistent state practice, particularly in cases where military advantage is low and civilian harm is high.',
    'If subjective, the constraint''s extractiveness is higher, as it allows for greater discretion in inflicting harm. If objective, it functions more as a coordination mechanism, albeit with inherent trade-offs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_calculus_ambiguity, conceptual, 'Ambiguity in the objectivity of proportionality analysis.').

omega_variable(
    conflict_classification_impact,
    'Does the distinction between International Armed Conflict (IAC) and Non-International Armed Conflict (NIAC) genuinely reflect different humanitarian needs, or does it primarily serve to limit state obligations?',
    'Empirical studies comparing humanitarian outcomes and protection levels for civilians in IACs versus NIACs, controlling for other conflict variables.',
    'If the distinction primarily limits obligations, the constraint''s suppression and extractiveness are higher for NIAC victims. If it reflects genuine differences in operational realities, the scaling is a necessary coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(conflict_classification_impact, empirical, 'Impact of conflict classification on protection scope.').

omega_variable(
    legal_ambiguity_as_beneficiary,
    'To what extent does the inherent legal ambiguity in IHL''s application (e.g., ''military necessity'', ''feasible precautions'') primarily benefit stronger military powers by providing interpretive leeway?',
    'Content analysis of military legal manuals, state reports, and academic critiques to identify patterns of interpretation that consistently favor military objectives over humanitarian protection, particularly by powerful states.',
    'If ambiguity consistently benefits stronger parties, the constraint''s extractiveness is higher, and the ''states_with_military_advantage'' stakeholder''s directionality is more strongly towards beneficiary.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legal_ambiguity_as_beneficiary, empirical, 'Role of legal ambiguity in benefiting powerful actors.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(geneva_conventions_protective_scope__hybrid_proportionality_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gene_tr_t1949, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 1949, 0.2).
narrative_ontology:measurement(gene_tr_t1965, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 1965, 0.25).
narrative_ontology:measurement(gene_tr_t1980, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(gene_tr_t1995, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 1995, 0.35).
narrative_ontology:measurement(gene_tr_t2010, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(gene_tr_t2024, geneva_conventions_protective_scope__hybrid_proportionality_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(gene_be_t1949, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 1949, 0.45).
narrative_ontology:measurement(gene_be_t1965, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 1965, 0.5).
narrative_ontology:measurement(gene_be_t1980, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 1980, 0.55).
narrative_ontology:measurement(gene_be_t1995, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 1995, 0.6).
narrative_ontology:measurement(gene_be_t2010, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(gene_be_t2024, geneva_conventions_protective_scope__hybrid_proportionality_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(gene_su_t1949, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 1949, 0.55).
narrative_ontology:measurement(gene_su_t1965, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 1965, 0.6).
narrative_ontology:measurement(gene_su_t1980, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(gene_su_t1995, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 1995, 0.68).
narrative_ontology:measurement(gene_su_t2010, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 2010, 0.69).
narrative_ontology:measurement(gene_su_t2024, geneva_conventions_protective_scope__hybrid_proportionality_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(geneva_conventions_protective_scope__hybrid_proportionality_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
