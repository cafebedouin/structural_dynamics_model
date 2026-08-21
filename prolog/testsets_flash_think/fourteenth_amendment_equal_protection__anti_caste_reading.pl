% ============================================================================
% CONSTRAINT STORY: fourteenth_amendment_equal_protection__anti_caste_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_fourteenth_amendment_equal_protection__anti_caste_reading, []).

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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: fourteenth_amendment_equal_protection__anti_caste_reading
 *   human_readable: Equal Protection: Anti-Caste Corrective Action
 *   domain: constitutional_law/political_philosophy/civil_rights
 *
 * SUMMARY:
 *   This constraint represents the 'anti-caste' reading of the Fourteenth
 *   Amendment's Equal Protection Clause, which mandates active state
 *   intervention to dismantle racial, gender, and status hierarchies. It
 *   legitimates corrective action, including classifications, to achieve
 *   substantive equality. This reading is distinct from a 'formal equality'
 *   reading, which would primarily prohibit state-sponsored discrimination
 *   without requiring active remediation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__anti_caste_reading, 0.8).
domain_priors:suppression_score(fourteenth_amendment_equal_protection__anti_caste_reading, 0.75).
domain_priors:theater_ratio(fourteenth_amendment_equal_protection__anti_caste_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, extractiveness, 0.8).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment_equal_protection__anti_caste_reading, tangled_rope).
narrative_ontology:human_readable(fourteenth_amendment_equal_protection__anti_caste_reading, "Equal Protection: Anti-Caste Corrective Action").
narrative_ontology:topic_domain(fourteenth_amendment_equal_protection__anti_caste_reading, "constitutional_law/political_philosophy/civil_rights").

domain_priors:requires_active_enforcement(fourteenth_amendment_equal_protection__anti_caste_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment_equal_protection__anti_caste_reading, '740970d0-196e-41a4-b954-5e1d63b7ef8b').
narrative_ontology:cs_kernel_codification('740970d0-196e-41a4-b954-5e1d63b7ef8b', fixed_text).
narrative_ontology:cs_authority_grounding('740970d0-196e-41a4-b954-5e1d63b7ef8b', lineage).
narrative_ontology:cs_interpretation_layer_present('740970d0-196e-41a4-b954-5e1d63b7ef8b').
narrative_ontology:cs_reading_relation('740970d0-196e-41a4-b954-5e1d63b7ef8b', fourteenth_amendment_equal_protection__formal_equality_reading, forecloses).
narrative_ontology:cs_axiom('740970d0-196e-41a4-b954-5e1d63b7ef8b', foundational, structural_inequality_is_state_concern).
narrative_ontology:cs_axiom_status(structural_inequality_is_state_concern, holdable).
narrative_ontology:cs_axiom_grounding('740970d0-196e-41a4-b954-5e1d63b7ef8b', structural_inequality_is_state_concern, deontological).
narrative_ontology:cs_axiom('740970d0-196e-41a4-b954-5e1d63b7ef8b', foundational, corrective_action_is_required_for_substantive_equality).
narrative_ontology:cs_axiom_status(corrective_action_is_required_for_substantive_equality, holdable).
narrative_ontology:cs_axiom_grounding('740970d0-196e-41a4-b954-5e1d63b7ef8b', corrective_action_is_required_for_substantive_equality, instrumental).
narrative_ontology:cs_reference_frame('740970d0-196e-41a4-b954-5e1d63b7ef8b', reconstruction_era_anti_caste_intent).
narrative_ontology:cs_drift_state('740970d0-196e-41a4-b954-5e1d63b7ef8b', contemporary_jurisprudence, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('740970d0-196e-41a4-b954-5e1d63b7ef8b', '').
narrative_ontology:cs_kernel_id(fourteenth_amendment_equal_protection__anti_caste_reading, fourteenth_amendment_equal_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, subordinated_groups).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, dominant_groups_benefiting_from_hierarchy).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, general_taxpayers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Historically marginalized groups (racial, gender, status) who are the intended recipients of corrective state action. They benefit from policies designed to dismantle systemic barriers and redistribute opportunities, but their identity is often tied to the very hierarchies being addressed.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, subordinated_groups, beneficiary,
    powerless, generational, identity_locked, national).

% Government bodies (e.g., civil rights divisions, education departments) tasked with designing and enforcing policies that actively dismantle hierarchy. They gain mandate and resources through this reading, but operate within political and legal constraints.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, state_agencies_implementing_remedies, agenda_setter,
    institutional, biographical, constrained, national).

% Groups who have historically benefited from existing racial, gender, or status hierarchies. They bear the costs of corrective action through altered access to opportunities, resource redistribution, or changes in social status. Their resistance to these changes is often significant.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, dominant_groups_benefiting_from_hierarchy, payer,
    powerful, generational, constrained, national).

% The judiciary, particularly the Supreme Court, which interprets the Equal Protection Clause and adjudicates the legality of state corrective actions. Their interpretations shape the scope and enforceability of this reading.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, courts, agenda_setter,
    institutional, civilizational, analytical, national).

% Legal scholars, activists, and political groups who argue that Equal Protection prohibits state classification by race or status, even for remedial purposes. They are excluded from the framework of this 'anti-caste' reading, which legitimates such classifications for corrective action.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, formal_equality_advocates, excluded,
    organized, biographical, mobile, national).

% Bear the financial costs of state corrective action and remedial programs through taxation. While some may support the goals, the direct financial burden is a cost.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, general_taxpayers, payer,
    moderate, immediate, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(fourteenth_amendment_equal_protection__anti_caste_reading, subordinated_groups).
narrative_ontology:fixing_cost_class(fourteenth_amendment_equal_protection__anti_caste_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state and societal efforts to identify, measure, and actively dismantle systemic racial, gender, and status hierarchies, ensuring a more equitable distribution of opportunities and resources.
% TRANSFER_FUNCTION: Transfers opportunities, resources, and social status from groups historically advantaged by hierarchy to historically subordinated groups, often through state-mandated programs and policies.
% ABSENT_VOICES: Advocates for a 'formal equality' reading of the Equal Protection Clause are structurally excluded from this framework; they would argue that state-mandated corrective action itself constitutes impermissible discrimination.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, state agencies would cease active dismantling efforts, existing hierarchies would likely reassert themselves, and the legal basis for many civil rights and affirmative action programs would collapse, leading to a significant reorganization of social and political structures.
% FOUNDING_PROBLEM: The persistence of racial, gender, and status-based hierarchies and their discriminatory effects, despite formal legal equality, leading to systemic disadvantage for certain groups.
% FOUNDING_PROBLEM_CORROBORATION: Sociological studies, economic data on wealth and income disparities, and ongoing civil rights advocacy from outside the direct beneficiaries consistently corroborate that systemic hierarchies and their effects remain a live problem, requiring active intervention.
narrative_ontology:disappearance_verdict(fourteenth_amendment_equal_protection__anti_caste_reading, world_rearranges).
narrative_ontology:founding_problem_status(fourteenth_amendment_equal_protection__anti_caste_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fourteenth_amendment_equal_protection__anti_caste_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(fourteenth_amendment_equal_protection__anti_caste_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fourteenth_amendment_equal_protection__anti_caste_reading, 0.8, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(fourteenth_amendment_equal_protection__anti_caste_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__anti_caste_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(fourteenth_amendment_equal_protection__anti_caste_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.80) is high because this reading requires significant redistribution of opportunities and resources, impacting those who benefited from prior arrangements. Suppression (0.75) is high due to the active enforcement needed to overcome resistance to dismantling established hierarchies. Theater ratio (0.40) is moderate, reflecting genuine efforts alongside potential bureaucratic inefficiencies or symbolic gestures. Resistance (0.80) is high, as any challenge to existing power structures inevitably generates strong opposition. Accessibility collapse (0.60) is moderate; while it aims to collapse discriminatory alternatives, full success is elusive due to persistent societal resistance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of subordinated groups, this reading is a necessary mechanism for justice and liberation. From the perspective of dominant groups, it can be perceived as reverse discrimination or an overreach of state power. State agencies see it as their mandate, while formal equality advocates view it as a violation of core constitutional principles.
 *
 * DIRECTIONALITY LOGIC:
 *   Subordinated groups are the primary beneficiaries, as the constraint aims to improve their social and economic position. Dominant groups and general taxpayers are payers, bearing the costs of redistribution and remedial programs. State agencies and courts act as agenda-setters, interpreting and enforcing the mandate. Formal equality advocates are excluded, as their core premise is incompatible with this reading's approach.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    anti_caste_vs_formal_equality_framing,
    'Is the Equal Protection Clause fundamentally about prohibiting state-sponsored discrimination (formal equality) or about dismantling systemic hierarchy (anti-caste)?',
    'A definitive Supreme Court ruling that explicitly adopts one reading and forecloses the other, or a constitutional amendment clarifying the clause''s intent.',
    'If the formal equality reading were to definitively foreclose the anti-caste reading, all state corrective actions based on group classification would be invalidated, leading to a re-entrenchment of existing hierarchies. Conversely, if the anti-caste reading were to definitively foreclose formal equality, the state''s power to implement broad remedial programs would be significantly expanded.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(anti_caste_vs_formal_equality_framing, conceptual, 'The core conceptual ambiguity between two competing interpretations of Equal Protection.').

omega_variable(
    efficacy_of_corrective_action,
    'To what extent do state corrective actions genuinely dismantle hierarchy versus creating new forms of stratification or becoming performative?',
    'Longitudinal empirical studies tracking the actual impact of specific remedial programs on intergenerational mobility, wealth gaps, and social integration, disaggregated by group.',
    'If corrective actions are found to be largely ineffective or counterproductive, the justification for the high extractiveness and suppression of this reading would be undermined, potentially leading to reclassification as a Piton or Snare. If highly effective, it would strengthen the ''Tangled Rope'' classification by validating its coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_of_corrective_action, empirical, 'Empirical question about the real-world efficacy and unintended consequences of anti-caste policies.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment_equal_protection__anti_caste_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(four_tr_t1954, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 1954, 0.2).
narrative_ontology:measurement(four_tr_t1968, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 1968, 0.25).
narrative_ontology:measurement(four_tr_t1980, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 1980, 0.3).
narrative_ontology:measurement(four_tr_t1995, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 1995, 0.35).
narrative_ontology:measurement(four_tr_t2010, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 2010, 0.38).
narrative_ontology:measurement(four_tr_t2024, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(four_be_t1954, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 1954, 0.6).
narrative_ontology:measurement(four_be_t1968, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 1968, 0.7).
narrative_ontology:measurement(four_be_t1980, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 1980, 0.75).
narrative_ontology:measurement(four_be_t1995, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 1995, 0.78).
narrative_ontology:measurement(four_be_t2010, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 2010, 0.79).
narrative_ontology:measurement(four_be_t2024, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 2024, 0.8).

% Suppression requirement over time
narrative_ontology:measurement(four_su_t1954, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 1954, 0.55).
narrative_ontology:measurement(four_su_t1968, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 1968, 0.65).
narrative_ontology:measurement(four_su_t1980, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 1980, 0.7).
narrative_ontology:measurement(four_su_t1995, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 1995, 0.72).
narrative_ontology:measurement(four_su_t2010, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 2010, 0.74).
narrative_ontology:measurement(four_su_t2024, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fourteenth_amendment_equal_protection__anti_caste_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(fourteenth_amendment_equal_protection__anti_caste_reading, fourteenth_amendment_equal_protection__formal_equality_reading).

% DUAL FORMULATION NOTE:
% This constraint is the 'anti-caste' reading of the Fourteenth Amendment's Equal Protection Clause, which is part of a constraint family including the 'formal equality' reading. Both are distinct interpretations of the same constitutional text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
