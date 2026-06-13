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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: fourteenth_amendment_equal_protection__anti_caste_reading
 *   human_readable: 14th Amendment Equal Protection (Anti-Caste Reading)
 *   domain: constitutional_law/political_philosophy/civil_rights
 *
 * SUMMARY:
 *   This constraint represents the 'anti-caste' reading of the 14th
 *   Amendment's Equal Protection Clause, which mandates active state
 *   intervention to dismantle racial, gender, and status hierarchies. It is
 *   one reading of the broader 'fourteenth_amendment_equal_protection'
 *   kernel. This reading legitimates affirmative action and other corrective
 *   policies, placing subordinated groups in the beneficiary set and
 *   requiring state institutions to bear the costs of systemic change. The
 *   high extractiveness reflects the significant redistribution of resources
 *   and opportunities required to achieve substantive equality.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__anti_caste_reading, 0.65).
domain_priors:suppression_score(fourteenth_amendment_equal_protection__anti_caste_reading, 0.45).
domain_priors:theater_ratio(fourteenth_amendment_equal_protection__anti_caste_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment_equal_protection__anti_caste_reading, tangled_rope).
narrative_ontology:human_readable(fourteenth_amendment_equal_protection__anti_caste_reading, "14th Amendment Equal Protection (Anti-Caste Reading)").
narrative_ontology:topic_domain(fourteenth_amendment_equal_protection__anti_caste_reading, "constitutional_law/political_philosophy/civil_rights").

domain_priors:requires_active_enforcement(fourteenth_amendment_equal_protection__anti_caste_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment_equal_protection__anti_caste_reading, '702a3f1e-ef2d-4782-ac5d-39c20c4b758c').
narrative_ontology:cs_kernel_codification('702a3f1e-ef2d-4782-ac5d-39c20c4b758c', fixed_text).
narrative_ontology:cs_authority_grounding('702a3f1e-ef2d-4782-ac5d-39c20c4b758c', lineage).
narrative_ontology:cs_interpretation_layer_present('702a3f1e-ef2d-4782-ac5d-39c20c4b758c').
narrative_ontology:cs_reading_relation('702a3f1e-ef2d-4782-ac5d-39c20c4b758c', fourteenth_amendment_equal_protection__formal_equality_reading, coexists_with).
narrative_ontology:cs_axiom('702a3f1e-ef2d-4782-ac5d-39c20c4b758c', foundational, substantive_equality_mandate).
narrative_ontology:cs_axiom_status(substantive_equality_mandate, holdable).
narrative_ontology:cs_axiom_grounding('702a3f1e-ef2d-4782-ac5d-39c20c4b758c', substantive_equality_mandate, deontological).
narrative_ontology:cs_axiom('702a3f1e-ef2d-4782-ac5d-39c20c4b758c', foundational, state_action_to_dismantle_hierarchy).
narrative_ontology:cs_axiom_status(state_action_to_dismantle_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('702a3f1e-ef2d-4782-ac5d-39c20c4b758c', state_action_to_dismantle_hierarchy, instrumental).
narrative_ontology:cs_reference_frame('702a3f1e-ef2d-4782-ac5d-39c20c4b758c', post_brown_v_board_substantive_equality).
narrative_ontology:cs_drift_state('702a3f1e-ef2d-4782-ac5d-39c20c4b758c', contemporary_judicial_retrenchment, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('702a3f1e-ef2d-4782-ac5d-39c20c4b758c', '').
narrative_ontology:cs_kernel_id(fourteenth_amendment_equal_protection__anti_caste_reading, fourteenth_amendment_equal_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, subordinated_racial_groups).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, subordinated_gender_groups).
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, subordinated_status_groups).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, state_institutions_resisting_remedy).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, dominant_groups_losing_privilege).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__anti_caste_reading, structural_inequality_doctrine).
narrative_ontology:constraint_vindicates(fourteenth_amendment_equal_protection__anti_caste_reading, corrective_justice_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets the Equal Protection Clause to mandate state action to dismantle systemic hierarchies. Its rulings shape the scope and legitimacy of remedial programs, often facing resistance from other branches and states.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, supreme_court_majority, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from state-mandated corrective actions aimed at addressing historical and ongoing racial discrimination. They advocate for robust enforcement and expansion of anti-caste principles.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, subordinated_racial_groups, beneficiary,
    organized, generational, constrained, national).

% Benefit from state-mandated corrective actions aimed at addressing historical and ongoing gender discrimination. They advocate for robust enforcement and expansion of anti-caste principles.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, subordinated_gender_groups, beneficiary,
    organized, generational, constrained, national).

% Benefit from state-mandated corrective actions aimed at addressing systemic discrimination based on other forms of social status. They advocate for robust enforcement and expansion of anti-caste principles.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, subordinated_status_groups, beneficiary,
    organized, generational, constrained, national).

% Are compelled to implement policies that actively dismantle existing hierarchies, which often requires reallocating resources, changing established practices, and confronting institutional inertia. They bear the direct costs of implementing remedial programs and face legal challenges for non-compliance.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, state_institutions_resisting_remedy, payer,
    institutional, biographical, constrained, national).

% Experience a reduction in unearned advantages or preferential access as a result of anti-caste state action. They often resist these changes through legal challenges, political lobbying, and public discourse, framing them as 'reverse discrimination'.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, dominant_groups_losing_privilege, payer,
    powerful, biographical, mobile, national).

% Argue that the Equal Protection Clause only prohibits explicit state classification, not mandates for corrective action. They are excluded from the anti-caste reading's core interpretive framework, though their arguments influence public and judicial debate.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, formal_equality_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state and federal efforts to identify and dismantle systemic hierarchies, ensuring that various government bodies work towards a common goal of achieving substantive equality.
% TRANSFER_FUNCTION: Transfers resources, opportunities, and status from historically dominant groups and resistant state institutions to historically subordinated racial, gender, and status groups through state-mandated corrective action.
% ABSENT_VOICES: Advocates for a purely 'colorblind' or 'gender-neutral' interpretation of Equal Protection are structurally excluded from the anti-caste reading's framework, as their arguments against corrective action are deemed to perpetuate, rather than dismantle, hierarchy.
% DISAPPEARANCE_RATIONALE: If this reading vanished, state-mandated affirmative action and other corrective programs would cease, leading to a rapid re-entrenchment of existing racial, gender, and status hierarchies. The legal and political landscape of civil rights would fundamentally shift.
% FOUNDING_PROBLEM: The 14th Amendment was ratified to address the systemic inequalities and caste-like structures that persisted after slavery, particularly for Black Americans, but also for other groups facing state-sanctioned discrimination.
% FOUNDING_PROBLEM_CORROBORATION: Historians, civil rights organizations, and legal scholars outside the direct beneficiary groups corroborate that the founding problem of systemic hierarchy and caste-like structures remains live, albeit in evolved forms. Empirical data on wealth gaps, educational disparities, and representation across various sectors supports this assessment.
narrative_ontology:disappearance_verdict(fourteenth_amendment_equal_protection__anti_caste_reading, world_rearranges).
narrative_ontology:founding_problem_status(fourteenth_amendment_equal_protection__anti_caste_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fourteenth_amendment_equal_protection__anti_caste_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(fourteenth_amendment_equal_protection__anti_caste_reading, 'none', 1).

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
 *   The extractiveness (0.65) is substantial because this reading demands active redistribution and dismantling of existing power structures, which imposes costs on those who benefit from the status quo. Suppression (0.45) is moderate; while there is active enforcement of anti-discrimination laws, resistance from dominant groups and state institutions is significant and ongoing. Theater ratio (0.15) is low, indicating that the efforts are largely genuine, though some performative compliance may exist. Accessibility collapse is low (0.3) because alternatives (e.g., formal equality arguments) are actively pursued by opposing groups, and resistance is high (0.7) due to the contentious nature of redistributive justice.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of subordinated groups, this reading is a necessary corrective mechanism, a 'rope' or 'scaffold' to achieve justice. From the perspective of dominant groups or resisting state institutions, it is an 'snare' or 'tangled_rope' that unfairly extracts from them. The engine's classification will reflect this divergence based on the declared structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Subordinated racial, gender, and status groups are clear beneficiaries (d near 0.0) as the constraint is designed to uplift them. State institutions resisting remedy and dominant groups losing privilege are targets (d near 1.0) as they bear the costs of implementing and adapting to anti-caste policies. Formal equality advocates are excluded, as their interpretive framework is fundamentally at odds with the anti-caste reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    anti_caste_vs_formal_equality_legitimacy,
    'Is the anti-caste reading a legitimate interpretation of the 14th Amendment''s original intent and evolving constitutional principles, or does it overstep judicial authority by mandating social engineering?',
    'Continued judicial precedent, legislative action, and public consensus over generations. The resolution is primarily conceptual and preference-driven, not purely empirical.',
    'If deemed illegitimate, the anti-caste reading would be overturned or significantly curtailed, shifting the constraint towards a ''formal_equality_reading'' and reducing its extractiveness for dominant groups. If affirmed, its mandate for corrective action would strengthen.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(anti_caste_vs_formal_equality_legitimacy, conceptual, 'Ambiguity regarding the legitimate scope of Equal Protection interpretation.').

omega_variable(
    effectiveness_of_corrective_action,
    'Are state corrective actions (e.g., affirmative action) effective in dismantling systemic hierarchies and achieving substantive equality, or do they create new forms of discrimination or unintended consequences?',
    'Longitudinal empirical studies on the outcomes of specific policies, disaggregated by group and context. This requires robust data collection and causal inference.',
    'If proven ineffective or counterproductive, the justification for the anti-caste reading''s active enforcement would weaken, potentially leading to policy shifts or judicial re-evaluation. If proven effective, it would strengthen the reading''s empirical grounding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_of_corrective_action, empirical, 'Empirical efficacy of anti-caste policies in achieving their stated goals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment_equal_protection__anti_caste_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(four_tr_t1954, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 1954, 0.05).
narrative_ontology:measurement(four_tr_t1970, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(four_tr_t1990, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(four_tr_t2010, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 2010, 0.18).
narrative_ontology:measurement(four_tr_t2024, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(four_be_t1954, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 1954, 0.4).
narrative_ontology:measurement(four_be_t1970, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(four_be_t1990, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 1990, 0.68).
narrative_ontology:measurement(four_be_t2010, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(four_be_t2024, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(four_su_t1954, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 1954, 0.3).
narrative_ontology:measurement(four_su_t1970, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 1970, 0.5).
narrative_ontology:measurement(four_su_t1990, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 1990, 0.6).
narrative_ontology:measurement(four_su_t2010, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 2010, 0.5).
narrative_ontology:measurement(four_su_t2024, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 2024, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fourteenth_amendment_equal_protection__anti_caste_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is the 'anti_caste_reading' of the 'fourteenth_amendment_equal_protection' kernel. It is structurally distinct from the 'formal_equality_reading' of the same kernel, which focuses on prohibiting explicit state classification rather than mandating corrective action. Both readings are live and contested.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
