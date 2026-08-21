% ============================================================================
% CONSTRAINT STORY: article_27_veto_power__oligopoly_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_article_27_veto_power__oligopoly_reading, []).

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
 *   constraint_id: article_27_veto_power__oligopoly_reading
 *   human_readable: UN Security Council Veto Power (Oligopoly Reading)
 *   domain: international_relations/institutional_design/constitutional_law
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(article_27_veto_power__oligopoly_reading, 0.85).
domain_priors:suppression_score(article_27_veto_power__oligopoly_reading, 0.9).
domain_priors:theater_ratio(article_27_veto_power__oligopoly_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, accessibility_collapse, 0.95).
narrative_ontology:constraint_metric(article_27_veto_power__oligopoly_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(article_27_veto_power__oligopoly_reading, snare).
narrative_ontology:human_readable(article_27_veto_power__oligopoly_reading, "UN Security Council Veto Power (Oligopoly Reading)").
narrative_ontology:topic_domain(article_27_veto_power__oligopoly_reading, "international_relations/institutional_design/constitutional_law").

domain_priors:requires_active_enforcement(article_27_veto_power__oligopoly_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(article_27_veto_power__oligopoly_reading, 'aa4ccc7f-6d1b-4972-bcbf-3495e327cb54').
narrative_ontology:cs_kernel_codification('aa4ccc7f-6d1b-4972-bcbf-3495e327cb54', fixed_text).
narrative_ontology:cs_authority_grounding('aa4ccc7f-6d1b-4972-bcbf-3495e327cb54', extraction).
narrative_ontology:cs_interpretation_layer_present('aa4ccc7f-6d1b-4972-bcbf-3495e327cb54').
narrative_ontology:cs_reading_relation('aa4ccc7f-6d1b-4972-bcbf-3495e327cb54', article_27_veto_power__coordination_reading, coexists_with).
narrative_ontology:cs_reading_relation('aa4ccc7f-6d1b-4972-bcbf-3495e327cb54', article_27_veto_power__sovereignty_reading, coexists_with).
narrative_ontology:cs_axiom('aa4ccc7f-6d1b-4972-bcbf-3495e327cb54', foundational, great_power_privilege_is_structural).
narrative_ontology:cs_axiom_status(great_power_privilege_is_structural, holdable).
narrative_ontology:cs_axiom_grounding('aa4ccc7f-6d1b-4972-bcbf-3495e327cb54', great_power_privilege_is_structural, conventional).
narrative_ontology:cs_axiom('aa4ccc7f-6d1b-4972-bcbf-3495e327cb54', foundational, institutional_immutability_serves_oligopoly).
narrative_ontology:cs_axiom_status(institutional_immutability_serves_oligopoly, holdable).
narrative_ontology:cs_axiom_grounding('aa4ccc7f-6d1b-4972-bcbf-3495e327cb54', institutional_immutability_serves_oligopoly, conventional).
narrative_ontology:cs_reference_frame('aa4ccc7f-6d1b-4972-bcbf-3495e327cb54', post_wwii_great_power_consensus).
narrative_ontology:cs_drift_state('aa4ccc7f-6d1b-4972-bcbf-3495e327cb54', contemporary_multipolar_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('aa4ccc7f-6d1b-4972-bcbf-3495e327cb54', '').
narrative_ontology:cs_kernel_id(article_27_veto_power__oligopoly_reading, article_27_veto_power).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(article_27_veto_power__oligopoly_reading, p5_states).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, non_p5_member_states).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, un_general_assembly).
narrative_ontology:constraint_victim(article_27_veto_power__oligopoly_reading, international_law_reformers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The five permanent members of the UN Security Council (China, France, Russia, United Kingdom, United States) who possess the veto power. They use this power to protect their national interests, block resolutions they oppose, and prevent any reform of the UN Charter that would dilute their authority or redistribute power within the Security Council. They benefit from the structural entrenchment of their geopolitical oligopoly.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, p5_states, agenda_setter,
    institutional, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(article_27_veto_power__oligopoly_reading, p5_states, beneficiary).

% The vast majority of UN member states who do not possess veto power. They are subject to the Security Council's decisions (or lack thereof due to vetoes) and are systematically blocked from achieving institutional reforms that would make the Council more representative or accountable. Their interests are often overridden or ignored, and they have no effective means to exit the UN system or challenge the P5's entrenched power.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, non_p5_member_states, payer,
    organized, biographical, trapped, global).
narrative_ontology:stakeholder_secondary_role(article_27_veto_power__oligopoly_reading, non_p5_member_states, excluded).

% The main deliberative body of the UN, representing all member states. While it can discuss and make recommendations, it lacks the power to compel action from the Security Council or to initiate reforms to the Charter that would alter the P5's veto power. It serves as a forum for the non-P5 majority to voice frustration, but its influence is structurally limited.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, un_general_assembly, excluded,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_27_veto_power__oligopoly_reading, un_general_assembly, observer).

% Academics, NGOs, and civil society groups advocating for reforms to international institutions, including the UN Security Council and the veto power. They invest significant intellectual and political capital in proposing alternatives but face the insurmountable barrier of P5 opposition to any change that would diminish their privilege. Their efforts are largely suppressed by the Charter's immutability.
narrative_ontology:constraint_stakeholder(article_27_veto_power__oligopoly_reading, international_law_reformers, payer,
    moderate, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(article_27_veto_power__oligopoly_reading, international_law_reformers, observer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Nominally, to ensure great power cooperation for the maintenance of international peace and security by preventing any Security Council resolution from compelling a nuclear state into military confrontation it rejects. However, from this reading, its primary function has become the structural entrenchment of geopolitical oligopoly.
% TRANSFER_FUNCTION: Transfers decision-making authority, geopolitical influence, and the power to block institutional evolution from the non-P5 majority of UN member states to the P5 states, allowing the latter to extract ongoing authority rents.
% ABSENT_VOICES: States that have withdrawn from international cooperation due to perceived UN paralysis or bias, or those whose national interests are consistently overridden by P5 vetoes. Also, potential alternative global governance structures that are suppressed by the P5's entrenchment.
% DISAPPEARANCE_RATIONALE: If the P5 veto power vanished overnight, the global geopolitical landscape would undergo a profound rearrangement. The Security Council would likely become more active and representative, but also potentially more prone to deadlock or action without great power consensus. New power blocs might emerge, and the international system would seek new mechanisms for balancing power and ensuring collective security, potentially leading to a more equitable but initially more volatile order.
% FOUNDING_PROBLEM: To prevent a third world war by ensuring that major global powers (the victors of WWII) would cooperate on collective security issues and that no single great power could be forced into military action against its will, thereby avoiding direct confrontation between nuclear states.
% FOUNDING_PROBLEM_CORROBORATION: P5 states consistently assert that the founding problem of preventing great power conflict remains live and that the veto is essential for global stability. Non-P5 states, international legal scholars, and civil society organizations widely contest this, arguing that the veto has frequently paralyzed the Security Council in the face of humanitarian crises and aggression, demonstrating that the original problem has either evolved or the mechanism is no longer fit for purpose; they cite numerous instances of veto use for narrow national interests rather than collective security.
narrative_ontology:disappearance_verdict(article_27_veto_power__oligopoly_reading, world_rearranges).
narrative_ontology:founding_problem_status(article_27_veto_power__oligopoly_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(article_27_veto_power__oligopoly_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(article_27_veto_power__oligopoly_reading, 'none', 1).
narrative_ontology:epsilon_provenance(article_27_veto_power__oligopoly_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(article_27_veto_power__oligopoly_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(article_27_veto_power__oligopoly_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(article_27_veto_power__oligopoly_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */


/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    veto_necessity_for_stability,
    'Is the P5 veto power a necessary evil for maintaining global stability by preventing great power conflict, or is it primarily a mechanism for geopolitical oligopoly and paralysis?',
    'Comparative analysis of international crises and conflicts where the veto was used vs. where it was not, alongside counterfactual modeling of scenarios without the veto. Examination of alternative security architectures proposed by non-P5 states.',
    'If proven necessary for stability, the extractiveness might be re-evaluated as a ''cost of coordination'' (Tangled Rope); if primarily oligopolistic, the Snare classification is reinforced, and calls for reform gain stronger normative grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(veto_necessity_for_stability, conceptual, 'Ambiguity between the veto as a stability mechanism and an oligopoly tool.').

omega_variable(
    charter_immutability_function,
    'Is the UN Charter''s immutability (due to the veto) a design feature intended to ensure foundational stability, or a design flaw that enables structural extraction by preventing necessary institutional evolution?',
    'Historical analysis of attempts to reform the Charter and the P5''s consistent blocking of such efforts. Legal and political theory analysis of ''constitutional entrenchment'' in international law.',
    'If a feature, the suppression of reform is a legitimate aspect of the system''s design; if a flaw, it highlights the extractive nature of the P5''s power and the need for external pressure for reform.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(charter_immutability_function, conceptual, 'Whether Charter immutability is a feature or a flaw.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(article_27_veto_power__oligopoly_reading, 1945, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arti_tr_t1945, article_27_veto_power__oligopoly_reading, theater_ratio, 1945, 0.25).
narrative_ontology:measurement(arti_tr_t1965, article_27_veto_power__oligopoly_reading, theater_ratio, 1965, 0.2).
narrative_ontology:measurement(arti_tr_t1985, article_27_veto_power__oligopoly_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(arti_tr_t2005, article_27_veto_power__oligopoly_reading, theater_ratio, 2005, 0.12).
narrative_ontology:measurement(arti_tr_t2025, article_27_veto_power__oligopoly_reading, theater_ratio, 2025, 0.1).

% Extraction over time
narrative_ontology:measurement(arti_be_t1945, article_27_veto_power__oligopoly_reading, base_extractiveness, 1945, 0.6).
narrative_ontology:measurement(arti_be_t1965, article_27_veto_power__oligopoly_reading, base_extractiveness, 1965, 0.7).
narrative_ontology:measurement(arti_be_t1985, article_27_veto_power__oligopoly_reading, base_extractiveness, 1985, 0.78).
narrative_ontology:measurement(arti_be_t2005, article_27_veto_power__oligopoly_reading, base_extractiveness, 2005, 0.82).
narrative_ontology:measurement(arti_be_t2025, article_27_veto_power__oligopoly_reading, base_extractiveness, 2025, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(arti_su_t1945, article_27_veto_power__oligopoly_reading, suppression_requirement, 1945, 0.7).
narrative_ontology:measurement(arti_su_t1965, article_27_veto_power__oligopoly_reading, suppression_requirement, 1965, 0.78).
narrative_ontology:measurement(arti_su_t1985, article_27_veto_power__oligopoly_reading, suppression_requirement, 1985, 0.85).
narrative_ontology:measurement(arti_su_t2005, article_27_veto_power__oligopoly_reading, suppression_requirement, 2005, 0.88).
narrative_ontology:measurement(arti_su_t2025, article_27_veto_power__oligopoly_reading, suppression_requirement, 2025, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(article_27_veto_power__oligopoly_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, un_security_council_resolution_process).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, international_criminal_court_jurisdiction).
narrative_ontology:affects_constraint(article_27_veto_power__oligopoly_reading, responsibility_to_protect_doctrine).

% DUAL FORMULATION NOTE:
% This constraint is one reading ('oligopoly_reading') of the 'article_27_veto_power' kernel. Other readings include 'coordination_reading' (veto as necessary for great power cooperation) and 'sovereignty_reading' (veto as an expression of Westphalian sovereignty). Each reading yields a distinct constraint with different ε values and classifications, linked here as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
