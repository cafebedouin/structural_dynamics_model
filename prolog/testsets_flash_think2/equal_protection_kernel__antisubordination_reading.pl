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
 *   constraint_id: equal_protection_kernel__antisubordination_reading
 *   human_readable: Equal Protection Clause: Antisubordination Reading
 *   domain: Constitutional Law / Education Policy / Civil Rights
 *
 * SUMMARY:
 *   This constraint represents the antisubordination reading of the Equal
 *   Protection Clause, which interprets the clause as primarily targeting
 *   caste-like subordination of historically oppressed groups. Under this
 *   reading, state action that entrenches hierarchy is forbidden, while
 *   action that dismantles it is permitted, even if race-conscious. This
 *   reading stands in contrast to colorblind and remedial interpretations,
 *   and its practical application has fluctuated significantly over time,
 *   particularly in the context of affirmative action.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__antisubordination_reading, 0.35).
domain_priors:suppression_score(equal_protection_kernel__antisubordination_reading, 0.4).
domain_priors:theater_ratio(equal_protection_kernel__antisubordination_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__antisubordination_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__antisubordination_reading, "Equal Protection Clause: Antisubordination Reading").
narrative_ontology:topic_domain(equal_protection_kernel__antisubordination_reading, "Constitutional Law / Education Policy / Civil Rights").

domain_priors:requires_active_enforcement(equal_protection_kernel__antisubordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__antisubordination_reading, '8dd34e78-1932-4717-ba79-d08819b78a8c').
narrative_ontology:cs_kernel_codification('8dd34e78-1932-4717-ba79-d08819b78a8c', fixed_text).
narrative_ontology:cs_authority_grounding('8dd34e78-1932-4717-ba79-d08819b78a8c', lineage).
narrative_ontology:cs_interpretation_layer_present('8dd34e78-1932-4717-ba79-d08819b78a8c').
narrative_ontology:cs_reading_relation('8dd34e78-1932-4717-ba79-d08819b78a8c', equal_protection_kernel__remedial_reading, coexists_with).
narrative_ontology:cs_reading_relation('8dd34e78-1932-4717-ba79-d08819b78a8c', equal_protection_kernel__colorblind_reading, forecloses).
narrative_ontology:cs_axiom('8dd34e78-1932-4717-ba79-d08819b78a8c', foundational, prohibition_on_caste_like_subordination).
narrative_ontology:cs_axiom_status(prohibition_on_caste_like_subordination, holdable).
narrative_ontology:cs_axiom_grounding('8dd34e78-1932-4717-ba79-d08819b78a8c', prohibition_on_caste_like_subordination, deontological).
narrative_ontology:cs_axiom('8dd34e78-1932-4717-ba79-d08819b78a8c', foundational, state_action_to_dismantle_hierarchy_permitted).
narrative_ontology:cs_axiom_status(state_action_to_dismantle_hierarchy_permitted, holdable).
narrative_ontology:cs_axiom_grounding('8dd34e78-1932-4717-ba79-d08819b78a8c', state_action_to_dismantle_hierarchy_permitted, conventional).
narrative_ontology:cs_reference_frame('8dd34e78-1932-4717-ba79-d08819b78a8c', warren_court_era_active_dismantling).
narrative_ontology:cs_drift_state('8dd34e78-1932-4717-ba79-d08819b78a8c', contemporary_conservative_court, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('8dd34e78-1932-4717-ba79-d08819b78a8c', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__antisubordination_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__antisubordination_reading, historically_subordinated_groups).
narrative_ontology:constraint_victim(equal_protection_kernel__antisubordination_reading, state_actors_entrenching_hierarchy).
narrative_ontology:constraint_victim(equal_protection_kernel__antisubordination_reading, dominant_groups_benefiting_from_hierarchy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups are the primary beneficiaries of the antisubordination reading, as it aims to dismantle systemic barriers and permit state action to achieve substantive equality. Their ability to exit subordination is constrained by historical and ongoing systemic issues, but this reading provides a legal pathway for redress.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, historically_subordinated_groups, beneficiary,
    powerless, generational, constrained, national).

% State entities (e.g., legislatures, agencies, school boards) whose actions perpetuate or create racial hierarchy are targeted by this reading. They bear the cost of being forbidden from enacting discriminatory policies and may be compelled to implement remedial measures. Their exit options are constrained by judicial review and legal challenges.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, state_actors_entrenching_hierarchy, payer,
    institutional, immediate, constrained, national).

% Groups that benefit from existing racial hierarchies (e.g., through preferential access to education or employment) bear the cost when the state takes action to dismantle those hierarchies. They often resist such measures through legal challenges and political advocacy, but their ability to maintain the status quo is constrained by the antisubordination principle.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, dominant_groups_benefiting_from_hierarchy, payer,
    organized, biographical, constrained, national).

% These organizations and individuals actively champion the antisubordination reading, bringing legal challenges and advocating for policies consistent with its principles. They shape the legal and public discourse around racial justice.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, civil_rights_advocates, agenda_setter,
    organized, generational, mobile, national).

% The Supreme Court is the ultimate arbiter of the Equal Protection Clause's meaning, and its interpretations determine the practical application and scope of the antisubordination reading. Individual justices' philosophies significantly influence its trajectory.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, national).

% Advocates for a colorblind interpretation of the Equal Protection Clause are structurally excluded from the core premise of the antisubordination reading, as their view directly contradicts the idea that race-conscious measures can be permissible to dismantle hierarchy. They actively contest this reading in legal and political arenas.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, colorblind_advocates, excluded,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a legal framework for state actors and civil society to coordinate efforts towards dismantling systemic racial hierarchy and achieving substantive equality, by permitting race-conscious remedial measures and forbidding actions that entrench subordination.
% TRANSFER_FUNCTION: Transfers legal authority and social resources from state actions and societal structures that perpetuate racial hierarchy to those that aim to dismantle it, thereby shifting power and opportunity towards historically subordinated groups.
% ABSENT_VOICES: Advocates for a purely colorblind interpretation of the Equal Protection Clause are absent from the foundational premise of this reading, as their view that all racial classifications are forbidden directly contradicts the antisubordination goal. They would argue that any race-conscious measure, even remedial, is unconstitutional.
% DISAPPEARANCE_RATIONALE: If the antisubordination reading vanished, state action could more easily entrench racial hierarchy without legal challenge, and race-conscious remedial efforts would lose their constitutional grounding. This would fundamentally alter the legal landscape for civil rights and likely exacerbate existing inequalities, leading to a significant societal rearrangement.
% FOUNDING_PROBLEM: The Equal Protection Clause was built to address the problem of racial caste and subordination, particularly in the aftermath of slavery, by ensuring that the state does not perpetuate systems of racial hierarchy.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights organizations, historical scholars, and sociological studies consistently attest that the problem of racial hierarchy and its effects remain live issues in American society. Legal scholars and dissenting justices also corroborate the ongoing relevance of the antisubordination principle in addressing these issues, even as its application is contested.
narrative_ontology:disappearance_verdict(equal_protection_kernel__antisubordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__antisubordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__antisubordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(equal_protection_kernel__antisubordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__antisubordination_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_kernel__antisubordination_reading_tests).
:- end_tests(equal_protection_kernel__antisubordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.35 at end) and suppression (0.40 at end) reflect the reading's declining practical force over the specified interval. While initially strong in its ability to extract benefits from those who profit from hierarchy and suppress discriminatory state action, judicial decisions have progressively weakened its application. Resistance is high (0.70) due to persistent legal and political challenges from those advocating for colorblindness. Theater ratio (0.40 at end) has increased as the reading's functional impact has diminished, leading to more performative adherence or attempts to circumvent its spirit. The measurement series track this decline from a stronger position in 1978 to a significantly weaker one by 2023.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of historically subordinated groups and civil rights advocates, this reading is a crucial tool for achieving justice and equality. From the perspective of state actors or dominant groups benefiting from hierarchy, it is an overreach that imposes unfair burdens or 'reverse discrimination.' The Supreme Court's perspective has shifted over time, reflecting internal ideological divisions.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically subordinated groups are the primary beneficiaries (low d) as the reading aims to dismantle barriers they face. State actors entrenching hierarchy and dominant groups benefiting from it are the targets (high d), as the reading extracts from their ability to maintain the status quo. Civil rights advocates act as agenda-setters, while the Supreme Court adjudicates the reading's application. Colorblind advocates are excluded from the core premise of this reading, making them a structural target of its legal force.
 *
 * MANDATROPHY ANALYSIS:
 *   The antisubordination reading's mandate remains live as long as racial hierarchy and subordination persist. Its function is to provide a legal mechanism to address these issues. The challenge is not mandatrophy, but rather the active contestation and erosion of its application by competing interpretations, which threatens to render its mandate unfulfilled rather than obsolete.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    antisubordination_vs_colorblind_primacy,
    'Is the Equal Protection Clause''s primary purpose to prevent racial hierarchy (antisubordination) or to prohibit all racial classifications (colorblindness)?',
    'A definitive and stable judicial consensus, or a constitutional amendment clarifying the clause''s intent.',
    'If antisubordination is affirmed as primary, race-conscious remedial measures would be broadly permissible. If colorblindness is affirmed, such measures would be largely forbidden, fundamentally altering civil rights law.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(antisubordination_vs_colorblind_primacy, conceptual, 'The core conceptual disagreement over the Equal Protection Clause''s fundamental purpose.').

omega_variable(
    effectiveness_of_remedial_measures,
    'Do state actions permitted under the antisubordination reading (e.g., affirmative action) effectively dismantle caste-like subordination and achieve substantive equality?',
    'Longitudinal, interdisciplinary empirical studies (sociological, economic, educational) assessing the actual impact of such policies on racial disparities and systemic barriers.',
    'Empirical evidence of effectiveness would strengthen the policy justification and political viability of the antisubordination reading. Evidence of ineffectiveness or unintended consequences could weaken its support and lead to calls for alternative approaches.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_of_remedial_measures, empirical, 'Empirical efficacy of antisubordination-aligned policies in achieving their stated goals.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__antisubordination_reading, 1978, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1978, equal_protection_kernel__antisubordination_reading, theater_ratio, 1978, 0.2).
narrative_ontology:measurement(equa_tr_t1990, equal_protection_kernel__antisubordination_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(equa_tr_t2000, equal_protection_kernel__antisubordination_reading, theater_ratio, 2000, 0.3).
narrative_ontology:measurement(equa_tr_t2010, equal_protection_kernel__antisubordination_reading, theater_ratio, 2010, 0.35).
narrative_ontology:measurement(equa_tr_t2023, equal_protection_kernel__antisubordination_reading, theater_ratio, 2023, 0.4).

% Extraction over time
narrative_ontology:measurement(equa_be_t1978, equal_protection_kernel__antisubordination_reading, base_extractiveness, 1978, 0.75).
narrative_ontology:measurement(equa_be_t1990, equal_protection_kernel__antisubordination_reading, base_extractiveness, 1990, 0.65).
narrative_ontology:measurement(equa_be_t2000, equal_protection_kernel__antisubordination_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(equa_be_t2010, equal_protection_kernel__antisubordination_reading, base_extractiveness, 2010, 0.45).
narrative_ontology:measurement(equa_be_t2023, equal_protection_kernel__antisubordination_reading, base_extractiveness, 2023, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1978, equal_protection_kernel__antisubordination_reading, suppression_requirement, 1978, 0.8).
narrative_ontology:measurement(equa_su_t1990, equal_protection_kernel__antisubordination_reading, suppression_requirement, 1990, 0.7).
narrative_ontology:measurement(equa_su_t2000, equal_protection_kernel__antisubordination_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(equa_su_t2010, equal_protection_kernel__antisubordination_reading, suppression_requirement, 2010, 0.5).
narrative_ontology:measurement(equa_su_t2023, equal_protection_kernel__antisubordination_reading, suppression_requirement, 2023, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__antisubordination_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_kernel__antisubordination_reading, equal_protection_kernel__remedial_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__antisubordination_reading, equal_protection_kernel__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_kernel__antisubordination_reading, affirmative_action_in_higher_education).
narrative_ontology:affects_constraint(equal_protection_kernel__antisubordination_reading, voting_rights_act_enforcement).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'equal_protection_kernel' alongside 'colorblind_reading' and 'remedial_reading'. Each reading instantiates a distinct constraint with its own ε and structural properties, reflecting different interpretations of the same constitutional text.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
