% ============================================================================
% CONSTRAINT STORY: equal_protection_kernel__antisubordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_kernel__antisubordination_reading, []).

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
 *   constraint_id: equal_protection_kernel__antisubordination_reading
 *   human_readable: Equal Protection Clause: Antisubordination Reading
 *   domain: constitutional_law/education_policy/civil_rights
 *
 * SUMMARY:
 *   This constraint represents the 'antisubordination' reading of the Equal
 *   Protection Clause, which interprets the clause as primarily prohibiting
 *   state action that creates or perpetuates a caste-like system of racial
 *   hierarchy. It permits race-conscious measures designed to dismantle such
 *   hierarchy, distinguishing between invidious discrimination and remedial
 *   action. This reading is one of several competing interpretations of the
 *   Equal Protection Clause, each generating a distinct constraint.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__antisubordination_reading, 0.6).
domain_priors:suppression_score(equal_protection_kernel__antisubordination_reading, 0.7).
domain_priors:theater_ratio(equal_protection_kernel__antisubordination_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__antisubordination_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__antisubordination_reading, "Equal Protection Clause: Antisubordination Reading").
narrative_ontology:topic_domain(equal_protection_kernel__antisubordination_reading, "constitutional_law/education_policy/civil_rights").

domain_priors:requires_active_enforcement(equal_protection_kernel__antisubordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__antisubordination_reading, 'f84990e0-b0f4-4d96-a0e1-7a75776f2408').
narrative_ontology:cs_kernel_codification('f84990e0-b0f4-4d96-a0e1-7a75776f2408', fixed_text).
narrative_ontology:cs_authority_grounding('f84990e0-b0f4-4d96-a0e1-7a75776f2408', lineage).
narrative_ontology:cs_interpretation_layer_present('f84990e0-b0f4-4d96-a0e1-7a75776f2408').
narrative_ontology:cs_reading_relation('f84990e0-b0f4-4d96-a0e1-7a75776f2408', equal_protection_kernel__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('f84990e0-b0f4-4d96-a0e1-7a75776f2408', equal_protection_kernel__remedial_reading, coexists_with).
narrative_ontology:cs_axiom('f84990e0-b0f4-4d96-a0e1-7a75776f2408', foundational, equal_protection_forbids_caste).
narrative_ontology:cs_axiom_status(equal_protection_forbids_caste, holdable).
narrative_ontology:cs_axiom_grounding('f84990e0-b0f4-4d96-a0e1-7a75776f2408', equal_protection_forbids_caste, deontological).
narrative_ontology:cs_axiom('f84990e0-b0f4-4d96-a0e1-7a75776f2408', secondary, race_conscious_remedies_permissible).
narrative_ontology:cs_axiom_status(race_conscious_remedies_permissible, holdable).
narrative_ontology:cs_axiom_grounding('f84990e0-b0f4-4d96-a0e1-7a75776f2408', race_conscious_remedies_permissible, instrumental).
narrative_ontology:cs_reference_frame('f84990e0-b0f4-4d96-a0e1-7a75776f2408', post_brown_substantive_equality).
narrative_ontology:cs_drift_state('f84990e0-b0f4-4d96-a0e1-7a75776f2408', contemporary_conservative_court, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('f84990e0-b0f4-4d96-a0e1-7a75776f2408', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__antisubordination_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__antisubordination_reading, historically_subordinated_groups).
narrative_ontology:constraint_victim(equal_protection_kernel__antisubordination_reading, state_actors_entrenching_hierarchy).
narrative_ontology:constraint_victim(equal_protection_kernel__antisubordination_reading, dominant_groups_claiming_reverse_discrimination).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_kernel__antisubordination_reading, remedial_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups benefit from state action designed to dismantle caste-like subordination. Their identity is often tied to the historical struggle against discrimination, making exit from the 'victim' category complex. They advocate for race-conscious remedies.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, historically_subordinated_groups, beneficiary,
    organized, generational, identity_locked, national).

% These are government entities (legislatures, courts, agencies) that implement policies aimed at dismantling racial hierarchy. They interpret the Equal Protection Clause through an antisubordination lens, permitting race-conscious measures.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, state_actors_dismantling_hierarchy, agenda_setter,
    institutional, generational, constrained, national).

% These are government entities whose actions (e.g., maintaining segregated systems, discriminatory practices) are deemed to entrench racial hierarchy. This reading forbids their actions, imposing costs in terms of legal challenges and policy reversals.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, state_actors_entrenching_hierarchy, payer,
    institutional, generational, constrained, national).

% Members of historically dominant groups who perceive themselves as disadvantaged by race-conscious remedial policies. This reading denies their claims of 'reverse discrimination' under the Equal Protection Clause, effectively making them bear the cost of historical redress.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, dominant_groups_claiming_reverse_discrimination, payer,
    powerful, biographical, mobile, national).

% Legal scholars and advocacy groups who argue for a strictly colorblind interpretation of the Equal Protection Clause, opposing any state action that classifies by race, regardless of purpose. Their arguments are often rejected by this reading.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, colorblind_advocates, excluded,
    organized, generational, constrained, national).

% Legal scholars and advocacy groups who support race-conscious measures but primarily on the grounds of remedying specific past discrimination or achieving diversity. While aligned with antisubordination in outcome, their theoretical grounding differs.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, remedial_advocates, beneficiary,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state action to address and dismantle systemic racial subordination, ensuring that policies are aligned with the goal of achieving substantive equality for historically oppressed groups.
% TRANSFER_FUNCTION: Transfers legal and political legitimacy to race-conscious state actions aimed at dismantling hierarchy, while denying such legitimacy to actions that entrench it. It shifts the burden of proof and legal standing in favor of historically subordinated groups.
% ABSENT_VOICES: Strict colorblind advocates are often marginalized in this reading's discourse, as their categorical opposition to racial classification is seen as undermining the antisubordination goal. They would argue that any racial classification, even for remedial purposes, is inherently discriminatory.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the legal landscape for civil rights would fundamentally shift. State actors would lose a key justification for race-conscious remedial policies, potentially leading to a rollback of efforts to address systemic inequality. The legal standing of historically subordinated groups to challenge discriminatory practices would be weakened, and dominant groups might more easily challenge affirmative action.
% FOUNDING_PROBLEM: The Equal Protection Clause was designed to address the historical legacy of slavery and racial discrimination, preventing the state from creating or maintaining a caste system based on race.
% FOUNDING_PROBLEM_CORROBORATION: Historians, civil rights organizations, and legal scholars (outside of those directly benefiting from specific policies) corroborate that systemic racial subordination, while evolving, remains a live problem in various forms, requiring ongoing state intervention. Empirical studies on wealth gaps, educational disparities, and criminal justice outcomes provide corroborating evidence.
narrative_ontology:disappearance_verdict(equal_protection_kernel__antisubordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__antisubordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__antisubordination_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(equal_protection_kernel__antisubordination_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_kernel__antisubordination_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_kernel__antisubordination_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_kernel__antisubordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) is moderate-high because this reading imposes significant costs on state actors and dominant groups whose actions or claims are deemed to entrench hierarchy. Suppression (0.7) is high as it actively suppresses alternative interpretations (like strict colorblindness) that would undermine its core purpose. Theater ratio (0.2) is low, as the enforcement is genuinely aimed at its stated goal, though some performative aspects may exist in policy justification. Resistance (0.8) is high due to ongoing legal and political challenges from those who oppose race-conscious policies.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of historically subordinated groups, this reading is a vital tool for justice and equality. From the perspective of dominant groups claiming reverse discrimination, it is an unfair imposition. State actors are divided, with some embracing it as a mandate for social justice and others resisting it as an overreach. The engine's per-seat classification will reflect these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically subordinated groups are primary beneficiaries (d=0.0-0.2) as the constraint is designed to protect and empower them. State actors implementing antisubordination policies are agenda-setters (d=0.2-0.4). State actors entrenching hierarchy and dominant groups claiming reverse discrimination are targets/payers (d=0.8-1.0), as the constraint actively works against their interests or claims. Colorblind advocates are excluded (d=0.9-1.0) as their core premise is rejected by this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subordination_definition_ambiguity,
    'How is ''caste-like subordination'' precisely defined and measured in contemporary society, beyond historical context?',
    'Development of clear, judicially cognizable metrics for systemic subordination (e.g., wealth gaps, educational attainment, criminal justice disparities) that are consistently applied across cases.',
    'A clear definition would strengthen the antisubordination reading''s application, making it less vulnerable to challenges that it is an open-ended mandate. Ambiguity allows for inconsistent application and weakens its legal force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordination_definition_ambiguity, conceptual, 'Ambiguity in defining the target of antisubordination.').

omega_variable(
    remedial_efficacy_empirical_basis,
    'Are race-conscious measures, as permitted by this reading, empirically effective at dismantling caste-like subordination?',
    'Longitudinal empirical studies tracking the impact of specific race-conscious policies on indicators of subordination, with rigorous causal inference.',
    'If empirically ineffective, the justification for race-conscious measures under this reading would weaken, potentially pushing it closer to the ''remedial'' reading''s stricter requirements for demonstrated past discrimination. If effective, it reinforces the reading''s pragmatic and normative force.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_efficacy_empirical_basis, empirical, 'Empirical effectiveness of antisubordination remedies.').

omega_variable(
    colorblind_vs_antisubordination_tension,
    'Can a legal framework simultaneously uphold a strong antisubordination principle and a strict colorblind principle without internal contradiction?',
    'Philosophical and legal analysis of the logical coherence of these two principles within a single constitutional framework, potentially leading to a synthesis or a declaration of irreconcilable conflict.',
    'If irreconcilable, one reading must logically foreclose the other within a coherent legal system, forcing a choice. If a synthesis is possible, it would reduce the conceptual tension and allow for a more unified interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(colorblind_vs_antisubordination_tension, conceptual, 'Conceptual tension between colorblindness and antisubordination.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__antisubordination_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1954, equal_protection_kernel__antisubordination_reading, theater_ratio, 1954, 0.1).
narrative_ontology:measurement(equa_tr_t1970, equal_protection_kernel__antisubordination_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(equa_tr_t1990, equal_protection_kernel__antisubordination_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(equa_tr_t2010, equal_protection_kernel__antisubordination_reading, theater_ratio, 2010, 0.25).
narrative_ontology:measurement(equa_tr_t2024, equal_protection_kernel__antisubordination_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(equa_be_t1954, equal_protection_kernel__antisubordination_reading, base_extractiveness, 1954, 0.4).
narrative_ontology:measurement(equa_be_t1970, equal_protection_kernel__antisubordination_reading, base_extractiveness, 1970, 0.5).
narrative_ontology:measurement(equa_be_t1990, equal_protection_kernel__antisubordination_reading, base_extractiveness, 1990, 0.6).
narrative_ontology:measurement(equa_be_t2010, equal_protection_kernel__antisubordination_reading, base_extractiveness, 2010, 0.65).
narrative_ontology:measurement(equa_be_t2024, equal_protection_kernel__antisubordination_reading, base_extractiveness, 2024, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1954, equal_protection_kernel__antisubordination_reading, suppression_requirement, 1954, 0.6).
narrative_ontology:measurement(equa_su_t1970, equal_protection_kernel__antisubordination_reading, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(equa_su_t1990, equal_protection_kernel__antisubordination_reading, suppression_requirement, 1990, 0.75).
narrative_ontology:measurement(equa_su_t2010, equal_protection_kernel__antisubordination_reading, suppression_requirement, 2010, 0.7).
narrative_ontology:measurement(equa_su_t2024, equal_protection_kernel__antisubordination_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__antisubordination_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is the 'antisubordination' reading of the Equal Protection Clause. It is structurally distinct from the 'colorblind' and 'remedial' readings, which are modeled as separate constraints due to their differing ε values and stakeholder structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
