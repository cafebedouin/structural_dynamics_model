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
 *   constraint_id: fourteenth_amendment_equal_protection__anti_caste_reading
 *   human_readable: 14th Amendment Equal Protection (Anti-Caste Reading)
 *   domain: constitutional_law/political_philosophy/civil_rights
 *
 * SUMMARY:
 *   This constraint represents the 'anti-caste' reading of the Fourteenth
 *   Amendment's Equal Protection Clause, which mandates active state
 *   intervention to dismantle systemic racial, gender, and status
 *   hierarchies. It views equality not merely as formal neutrality but as
 *   substantive equity, requiring corrective action. This reading is in
 *   active contest with a 'formal equality' reading that emphasizes state
 *   neutrality and prohibits explicit classifications.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(fourteenth_amendment_equal_protection__anti_caste_reading, 0.65).
domain_priors:suppression_score(fourteenth_amendment_equal_protection__anti_caste_reading, 0.4).
domain_priors:theater_ratio(fourteenth_amendment_equal_protection__anti_caste_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(fourteenth_amendment_equal_protection__anti_caste_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(fourteenth_amendment_equal_protection__anti_caste_reading, tangled_rope).
narrative_ontology:human_readable(fourteenth_amendment_equal_protection__anti_caste_reading, "14th Amendment Equal Protection (Anti-Caste Reading)").
narrative_ontology:topic_domain(fourteenth_amendment_equal_protection__anti_caste_reading, "constitutional_law/political_philosophy/civil_rights").

domain_priors:requires_active_enforcement(fourteenth_amendment_equal_protection__anti_caste_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(fourteenth_amendment_equal_protection__anti_caste_reading, '2cbdfbd8-67e4-47d6-9968-3006b8bb8110').
narrative_ontology:cs_kernel_codification('2cbdfbd8-67e4-47d6-9968-3006b8bb8110', fixed_text).
narrative_ontology:cs_authority_grounding('2cbdfbd8-67e4-47d6-9968-3006b8bb8110', lineage).
narrative_ontology:cs_interpretation_layer_present('2cbdfbd8-67e4-47d6-9968-3006b8bb8110').
narrative_ontology:cs_reading_relation('2cbdfbd8-67e4-47d6-9968-3006b8bb8110', fourteenth_amendment_equal_protection__formal_equality_reading, coexists_with).
narrative_ontology:cs_axiom('2cbdfbd8-67e4-47d6-9968-3006b8bb8110', foundational, equality_requires_substantive_equity).
narrative_ontology:cs_axiom_status(equality_requires_substantive_equity, holdable).
narrative_ontology:cs_axiom_grounding('2cbdfbd8-67e4-47d6-9968-3006b8bb8110', equality_requires_substantive_equity, deontological).
narrative_ontology:cs_axiom('2cbdfbd8-67e4-47d6-9968-3006b8bb8110', foundational, state_has_duty_to_dismantle_hierarchy).
narrative_ontology:cs_axiom_status(state_has_duty_to_dismantle_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('2cbdfbd8-67e4-47d6-9968-3006b8bb8110', state_has_duty_to_dismantle_hierarchy, deontological).
narrative_ontology:cs_reference_frame('2cbdfbd8-67e4-47d6-9968-3006b8bb8110', post_brown_v_board_substantive_equality).
narrative_ontology:cs_drift_state('2cbdfbd8-67e4-47d6-9968-3006b8bb8110', contemporary_judicial_challenges, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('2cbdfbd8-67e4-47d6-9968-3006b8bb8110', '').
narrative_ontology:cs_kernel_id(fourteenth_amendment_equal_protection__anti_caste_reading, fourteenth_amendment_equal_protection).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(fourteenth_amendment_equal_protection__anti_caste_reading, historically_subordinated_groups).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, institutions_perpetuating_hierarchy).
narrative_ontology:constraint_victim(fourteenth_amendment_equal_protection__anti_caste_reading, individuals_benefiting_from_status_quo).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups benefit from state-led corrective actions aimed at dismantling systemic hierarchies. Their ability to exit the 'constrained' state depends on the success of these interventions. They advocate for robust enforcement of this reading.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, historically_subordinated_groups, beneficiary,
    organized, generational, constrained, national).

% These institutions (e.g., discriminatory hiring practices, segregated housing policies) bear the costs of corrective action, facing legal challenges, mandated changes, and resource reallocation. Their 'exit' is to dismantle their own discriminatory structures.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, institutions_perpetuating_hierarchy, payer,
    institutional, biographical, constrained, national).

% Individuals who benefit from existing hierarchical structures (e.g., through unearned advantages or preferential access) may perceive corrective actions as a cost or disadvantage. Their 'exit' might involve adapting to new norms or relocating, but the structural changes affect their perceived entitlements.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, individuals_benefiting_from_status_quo, payer,
    moderate, biographical, mobile, local).

% Government agencies and courts tasked with implementing and enforcing anti-caste policies. They set the agenda for corrective action and bear the political and administrative costs of challenging entrenched hierarchies. Their 'exit' is constrained by legal mandates and political will.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, state_actors_implementing_remedies, agenda_setter,
    institutional, generational, constrained, national).

% Advocates for a 'colorblind' or 'gender-neutral' application of Equal Protection, who view state corrective action as a form of reverse discrimination. They are excluded from the core interpretive framework of this reading, but actively contest its legitimacy in public and legal discourse.
narrative_ontology:constraint_stakeholder(fourteenth_amendment_equal_protection__anti_caste_reading, formal_equality_advocates, excluded,
    organized, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state and societal efforts to identify and dismantle systemic racial, gender, and status hierarchies, ensuring that all citizens have genuinely equal opportunities and protections.
% TRANSFER_FUNCTION: Transfers resources, opportunities, and status from historically dominant groups and institutions to historically subordinated groups, aiming to rectify past and ongoing injustices.
% ABSENT_VOICES: Advocates of a purely 'formal equality' reading are structurally excluded from the interpretive framework of this anti-caste reading, as their core premise (state neutrality) directly conflicts with the need for active state intervention. They would argue that corrective action itself constitutes discrimination.
% DISAPPEARANCE_RATIONALE: If this reading vanished, state-led efforts to address systemic inequality would cease, leading to a re-entrenchment or exacerbation of existing racial, gender, and status hierarchies. The legal and social landscape would revert to a more formally neutral but substantively unequal state.
% FOUNDING_PROBLEM: The original problem was the persistence of racial, gender, and status-based subordination despite formal legal equality, leading to de facto caste systems.
% FOUNDING_PROBLEM_CORROBORATION: Sociological studies, economic data on wealth and income disparities, and ongoing reports from civil rights organizations consistently corroborate that systemic hierarchies persist and the founding problem remains live, even if its manifestations evolve. This is attested by independent academic research and non-governmental organizations.
narrative_ontology:disappearance_verdict(fourteenth_amendment_equal_protection__anti_caste_reading, world_rearranges).
narrative_ontology:founding_problem_status(fourteenth_amendment_equal_protection__anti_caste_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(fourteenth_amendment_equal_protection__anti_caste_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(fourteenth_amendment_equal_protection__anti_caste_reading, 'none', 1).
narrative_ontology:epsilon_provenance(fourteenth_amendment_equal_protection__anti_caste_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) reflects the significant reordering of resources and opportunities required to dismantle entrenched hierarchies, impacting those who benefit from the status quo. Suppression (0.4) is moderate, as this reading faces significant legal and political resistance, but it actively suppresses discriminatory practices. Theater ratio (0.2) is low, indicating that the efforts are genuinely aimed at achieving substantive equality, though implementation can be imperfect. Accessibility collapse (0.3) is low because alternatives (e.g., formal equality arguments) remain robust, and resistance (0.7) is high due to ongoing legal and political challenges.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is stark: beneficiaries see this as essential justice, while payers see it as unfair redistribution or 'reverse discrimination.' The agenda-setters navigate this tension, attempting to implement policies that achieve substantive equality while facing legal and political challenges from those who adhere to a formal equality perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically subordinated groups are beneficiaries (d=0.0-0.2) as the constraint aims to uplift them. Institutions perpetuating hierarchy and individuals benefiting from the status quo are targets (d=0.8-1.0) as they bear the costs of dismantling existing structures. State actors implementing remedies are agenda-setters (d=0.4-0.6), balancing coordination with the costs of enforcement. Formal equality advocates are excluded (d=1.0) as their position is fundamentally opposed to the active intervention mandated by this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling genuine efforts to correct systemic injustice as pure extraction by recognizing the coordination function of dismantling hierarchy. It acknowledges that achieving substantive equality requires active intervention, which will necessarily 'extract' from those who benefit from the unequal status quo. The 'live' status of the founding problem indicates that the mandate has not atrophied.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    substantive_vs_formal_equality,
    'Is the Equal Protection Clause primarily concerned with formal equality (equal treatment under the law) or substantive equality (equal outcomes and dismantling of systemic hierarchy)?',
    'Continued judicial interpretation, legislative action, and societal consensus shifts over generations. Empirical evidence of persistent disparities despite formal equality may sway interpretation.',
    'If resolved towards formal equality, this anti-caste reading would be foreclosed, and state corrective action would be deemed unconstitutional. If resolved towards substantive equality, this reading would be strengthened, legitimizing broader state intervention.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(substantive_vs_formal_equality, conceptual, 'The fundamental conceptual ambiguity in interpreting Equal Protection.').

omega_variable(
    efficacy_of_corrective_action,
    'Are state corrective actions (e.g., affirmative action, targeted programs) effective in dismantling systemic hierarchies and achieving substantive equality, or do they create new forms of discrimination or unintended consequences?',
    'Longitudinal empirical studies, social science research, and policy evaluations assessing the actual impact of specific interventions on various groups over time.',
    'Evidence of inefficacy or negative consequences would weaken the empirical grounding of this reading, potentially leading to its re-evaluation or modification. Strong evidence of efficacy would bolster its legitimacy and support broader implementation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(efficacy_of_corrective_action, empirical, 'Empirical question regarding the effectiveness and unintended consequences of anti-caste interventions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(fourteenth_amendment_equal_protection__anti_caste_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(four_tr_t1954, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 1954, 0.1).
narrative_ontology:measurement(four_tr_t1970, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(four_tr_t1990, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 1990, 0.2).
narrative_ontology:measurement(four_tr_t2010, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(four_tr_t2024, fourteenth_amendment_equal_protection__anti_caste_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(four_be_t1954, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 1954, 0.4).
narrative_ontology:measurement(four_be_t1970, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 1970, 0.55).
narrative_ontology:measurement(four_be_t1990, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement(four_be_t2010, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 2010, 0.6).
narrative_ontology:measurement(four_be_t2024, fourteenth_amendment_equal_protection__anti_caste_reading, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(four_su_t1954, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 1954, 0.2).
narrative_ontology:measurement(four_su_t1970, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 1970, 0.35).
narrative_ontology:measurement(four_su_t1990, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 1990, 0.45).
narrative_ontology:measurement(four_su_t2010, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 2010, 0.4).
narrative_ontology:measurement(four_su_t2024, fourteenth_amendment_equal_protection__anti_caste_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(fourteenth_amendment_equal_protection__anti_caste_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the Fourteenth Amendment's Equal Protection Clause. It is structurally distinct from the 'formal equality' reading, which emphasizes state neutrality and prohibits explicit classifications. Both readings are part of the larger 'fourteenth_amendment_equal_protection' kernel.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
