% ============================================================================
% CONSTRAINT STORY: common_article_3_scope__state_centric_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_common_article_3_scope__state_centric_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: common_article_3_scope__state_centric_reading
 *   human_readable: Common Article 3 Scope (State-Centric Reading)
 *   domain: international_humanitarian_law/law_of_armed_conflict
 *
 * SUMMARY:
 *   This constraint represents the 'state-centric' reading of Common Article
 *   3 (CA3) of the Geneva Conventions, which limits its application to armed
 *   conflicts meeting specific intensity and organization thresholds, thereby
 *   excluding low-level violence and routine law enforcement actions from the
 *   scope of international humanitarian law (IHL). This reading is actively
 *   maintained by many states to preserve maximum operational discretion for
 *   their national forces and to avoid international scrutiny for internal
 *   security operations. The constraint is claimed as a 'rope' by its
 *   proponents (a necessary coordination mechanism for states), but its
 *   metrics reflect a 'tangled rope' or 'snare' in practice, due to its
 *   substantial extraction from victims and active suppression of broader
 *   interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(common_article_3_scope__state_centric_reading, 0.65).
domain_priors:suppression_score(common_article_3_scope__state_centric_reading, 0.78).
domain_priors:theater_ratio(common_article_3_scope__state_centric_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, suppression_requirement, 0.78).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(common_article_3_scope__state_centric_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(common_article_3_scope__state_centric_reading, tangled_rope).
narrative_ontology:human_readable(common_article_3_scope__state_centric_reading, "Common Article 3 Scope (State-Centric Reading)").
narrative_ontology:topic_domain(common_article_3_scope__state_centric_reading, "international_humanitarian_law/law_of_armed_conflict").

domain_priors:requires_active_enforcement(common_article_3_scope__state_centric_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(common_article_3_scope__state_centric_reading, '1fa52102-e01a-42de-8d58-3f343a578aa8').
narrative_ontology:cs_kernel_codification('1fa52102-e01a-42de-8d58-3f343a578aa8', fixed_text).
narrative_ontology:cs_authority_grounding('1fa52102-e01a-42de-8d58-3f343a578aa8', lineage).
narrative_ontology:cs_interpretation_layer_present('1fa52102-e01a-42de-8d58-3f343a578aa8').
narrative_ontology:cs_reading_relation('1fa52102-e01a-42de-8d58-3f343a578aa8', common_article_3_scope__expansive_human_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('1fa52102-e01a-42de-8d58-3f343a578aa8', common_article_3_scope__icrc_customary_reading, coexists_with).
narrative_ontology:cs_axiom('1fa52102-e01a-42de-8d58-3f343a578aa8', foundational, state_sovereignty_primacy_in_internal_affairs).
narrative_ontology:cs_axiom_status(state_sovereignty_primacy_in_internal_affairs, holdable).
narrative_ontology:cs_axiom_grounding('1fa52102-e01a-42de-8d58-3f343a578aa8', state_sovereignty_primacy_in_internal_affairs, deontological).
narrative_ontology:cs_axiom('1fa52102-e01a-42de-8d58-3f343a578aa8', foundational, high_threshold_for_ihl_application).
narrative_ontology:cs_axiom_status(high_threshold_for_ihl_application, holdable).
narrative_ontology:cs_axiom_grounding('1fa52102-e01a-42de-8d58-3f343a578aa8', high_threshold_for_ihl_application, conventional).
narrative_ontology:cs_reference_frame('1fa52102-e01a-42de-8d58-3f343a578aa8', post_westphalian_state_sovereignty).
narrative_ontology:cs_drift_state('1fa52102-e01a-42de-8d58-3f343a578aa8', contemporary_human_rights_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('1fa52102-e01a-42de-8d58-3f343a578aa8', '').
narrative_ontology:cs_kernel_id(common_article_3_scope__state_centric_reading, common_article_3_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, state_governments).
narrative_ontology:constraint_beneficiary(common_article_3_scope__state_centric_reading, national_armed_forces).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, irregular_combatants_below_threshold).
narrative_ontology:constraint_victim(common_article_3_scope__state_centric_reading, civilians_in_low_intensity_conflicts).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Advocate for and enforce a narrow interpretation of Common Article 3, limiting its application to conflicts meeting high thresholds of intensity and organization. This preserves maximum operational discretion for national armed forces and law enforcement, avoiding international humanitarian law obligations in 'low-level' violence.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, state_governments, agenda_setter,
    institutional, generational, arbitrage, global).

% Benefit from the reduced legal scrutiny and fewer constraints on their conduct in situations deemed below the threshold of armed conflict. They operate under domestic law enforcement rules rather than IHL, which often grants broader powers and fewer protections to detainees.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, national_armed_forces, beneficiary,
    institutional, biographical, constrained, national).

% Are denied the protections of Common Article 3, being treated as criminals or terrorists under domestic law rather than combatants or persons hors de combat. They face harsher penalties, lack prisoner-of-war status, and are subject to potentially more brutal interrogation and detention regimes.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, irregular_combatants_below_threshold, payer,
    powerless, immediate, trapped, local).

% Experience violence and deprivation without the full protective framework of IHL, as the conflict is not classified as an 'armed conflict' under this reading. Their suffering is framed as a domestic law and order issue, not an international humanitarian crisis.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, civilians_in_low_intensity_conflicts, payer,
    powerless, immediate, trapped, local).

% Seek to apply Common Article 3 more broadly to protect all victims of armed violence. They are often denied access or their mandates are challenged by states adhering to the narrow interpretation, limiting their ability to provide assistance and monitor compliance.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, international_humanitarian_organizations, excluded,
    organized, generational, constrained, global).

% Must determine the threshold of armed conflict to establish jurisdiction over war crimes. The state-centric reading creates a higher bar for their intervention, potentially shielding perpetrators of violence in 'sub-threshold' conflicts from international prosecution.
narrative_ontology:constraint_stakeholder(common_article_3_scope__state_centric_reading, international_criminal_courts, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for states to coordinate their understanding of when international humanitarian law applies, ensuring a common (albeit narrow) legal basis for military operations and law enforcement actions in situations of violence.
% TRANSFER_FUNCTION: Transfers legal discretion and reduced accountability from international humanitarian law bodies and victim groups to state governments and their armed forces, by limiting the scope of IHL application.
% ABSENT_VOICES: Victims of violence in situations deemed 'below threshold' by states, as well as human rights advocates and international humanitarian organizations, are effectively excluded from the legal determination process that limits CA3's protective scope. They would argue for a broader application based on human suffering, not state-defined thresholds.
% DISAPPEARANCE_RATIONALE: If this narrow reading vanished, states would face immediate pressure to apply IHL more broadly, increasing accountability for their forces and extending protections to more individuals in situations of violence. This would fundamentally alter military doctrine, legal frameworks, and international relations concerning internal conflicts.
% FOUNDING_PROBLEM: States sought to preserve sovereignty and operational flexibility for their armed forces and law enforcement in internal security operations, preventing international humanitarian law from encroaching on domestic jurisdiction for 'low-level' violence.
% FOUNDING_PROBLEM_CORROBORATION: State governments and their legal advisors consistently attest that this problem remains live, citing national security concerns and the need to distinguish between criminal acts and armed conflict. International humanitarian organizations and human rights bodies, however, contest this, arguing that the problem is a pretext for avoiding accountability; their reports and legal analyses provide an external corroboration of the *existence* of the problem, even if they dispute its *legitimacy*.
narrative_ontology:disappearance_verdict(common_article_3_scope__state_centric_reading, world_rearranges).
narrative_ontology:founding_problem_status(common_article_3_scope__state_centric_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(common_article_3_scope__state_centric_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(common_article_3_scope__state_centric_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(common_article_3_scope__state_centric_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(common_article_3_scope__state_centric_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(common_article_3_scope__state_centric_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.65) is substantial because it denies IHL protections to a significant population of victims in 'sub-threshold' conflicts, allowing states to operate with fewer constraints. Suppression (0.78) is high due to active state resistance to expansive interpretations, including diplomatic pressure, legal arguments, and denial of access to international bodies. Theater ratio (0.20) is low, as the legal arguments are genuinely held and actively defended, not merely performative. The increasing extractiveness and suppression over time reflect a hardening of this interpretation in response to challenges from human rights and humanitarian law advocates.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of state governments, this reading is a necessary 'rope' for coordinating national security and preserving sovereignty. From the perspective of victims and humanitarian organizations, it functions as a 'snare' or 'tangled rope,' extracting protections and enabling abuses under the guise of domestic jurisdiction. The engine's classification will likely reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   State governments and their armed forces are clear beneficiaries (d near 0.0) as they gain legal flexibility and reduced accountability. Irregular combatants and civilians in low-intensity conflicts are the primary victims (d near 1.0) as they are denied IHL protections. International humanitarian organizations are excluded (d near 1.0) as their efforts to broaden CA3's application are actively resisted. International criminal courts are observers, their jurisdiction directly impacted by this reading's narrow scope.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    threshold_objectivity,
    'Are the ''intensity'' and ''organization'' thresholds for CA3 application objectively determinable, or are they subject to political manipulation by states?',
    'Analysis of state practice in classifying conflicts: if classifications consistently align with state interests rather than objective criteria, the thresholds are politically malleable.',
    'If thresholds are subjective, the constraint''s effective suppression and extractiveness are higher, as states can arbitrarily deny IHL protections. If objective, the constraint is a more legitimate ''rope'' for legal clarity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(threshold_objectivity, empirical, 'Objectivity of CA3 application thresholds.').

omega_variable(
    humanitarian_law_vs_human_rights_overlap,
    'To what extent does the state-centric reading of CA3 create a ''protection gap'' between IHL and international human rights law (IHRL) in situations of violence?',
    'Comparative legal analysis of protections offered by IHRL versus IHL in ''sub-threshold'' conflicts, and empirical study of victim experiences in such gaps.',
    'A significant protection gap would highlight the extractive nature of this reading, as it leaves victims vulnerable. If IHRL adequately covers the gap, the extraction is mitigated.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(humanitarian_law_vs_human_rights_overlap, conceptual, 'Protection gap between IHL and IHRL due to narrow CA3 scope.').

omega_variable(
    state_sovereignty_vs_individual_protection,
    'Is the state-centric reading primarily driven by a legitimate concern for state sovereignty and operational effectiveness, or by a desire to avoid accountability for human rights violations?',
    'Examination of state justifications for narrow interpretations in specific cases, cross-referenced with independent human rights reports and judicial findings.',
    'If driven by accountability avoidance, the constraint is a ''snare.'' If by legitimate sovereignty concerns, it is a ''tangled rope'' with a genuine (though contested) coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(state_sovereignty_vs_individual_protection, preference, 'Motivation for state-centric CA3 interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(common_article_3_scope__state_centric_reading, 1949, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(comm_tr_t1949, common_article_3_scope__state_centric_reading, theater_ratio, 1949, 0.1).
narrative_ontology:measurement(comm_tr_t1970, common_article_3_scope__state_centric_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(comm_tr_t1990, common_article_3_scope__state_centric_reading, theater_ratio, 1990, 0.18).
narrative_ontology:measurement(comm_tr_t2010, common_article_3_scope__state_centric_reading, theater_ratio, 2010, 0.19).
narrative_ontology:measurement(comm_tr_t2024, common_article_3_scope__state_centric_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(comm_be_t1949, common_article_3_scope__state_centric_reading, base_extractiveness, 1949, 0.5).
narrative_ontology:measurement(comm_be_t1970, common_article_3_scope__state_centric_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(comm_be_t1990, common_article_3_scope__state_centric_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(comm_be_t2010, common_article_3_scope__state_centric_reading, base_extractiveness, 2010, 0.63).
narrative_ontology:measurement(comm_be_t2024, common_article_3_scope__state_centric_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(comm_su_t1949, common_article_3_scope__state_centric_reading, suppression_requirement, 1949, 0.6).
narrative_ontology:measurement(comm_su_t1970, common_article_3_scope__state_centric_reading, suppression_requirement, 1970, 0.68).
narrative_ontology:measurement(comm_su_t1990, common_article_3_scope__state_centric_reading, suppression_requirement, 1990, 0.72).
narrative_ontology:measurement(comm_su_t2010, common_article_3_scope__state_centric_reading, suppression_requirement, 2010, 0.75).
narrative_ontology:measurement(comm_su_t2024, common_article_3_scope__state_centric_reading, suppression_requirement, 2024, 0.78).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(common_article_3_scope__state_centric_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, common_article_3_scope__expansive_human_rights_reading).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, common_article_3_scope__icrc_customary_reading).
narrative_ontology:affects_constraint(common_article_3_scope__state_centric_reading, definition_of_non_international_armed_conflict).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Common Article 3 scope kernel. It represents the state-centric interpretation, which limits CA3's application to conflicts meeting high intensity and organization thresholds. The other readings (expansive human rights and ICRC customary) offer broader interpretations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
