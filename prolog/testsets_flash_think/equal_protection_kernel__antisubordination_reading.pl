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
 *   constraint_id: equal_protection_kernel__antisubordination_reading
 *   human_readable: Equal Protection: Antisubordination Reading
 *   domain: constitutional_law/education_policy/civil_rights
 *
 * SUMMARY:
 *   This constraint story instantiates the 'antisubordination' reading of the
 *   Equal Protection Clause, which interprets the clause as primarily
 *   targeting caste-like subordination of historically oppressed groups.
 *   Under this reading, state action that entrenches hierarchy is forbidden,
 *   while action that dismantles it is permitted, even if race-conscious.
 *   This contrasts sharply with 'colorblind' readings that forbid all racial
 *   classifications. The metrics reflect the ongoing struggle to implement
 *   this reading against significant resistance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_kernel__antisubordination_reading, 0.68).
domain_priors:suppression_score(equal_protection_kernel__antisubordination_reading, 0.75).
domain_priors:theater_ratio(equal_protection_kernel__antisubordination_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(equal_protection_kernel__antisubordination_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_kernel__antisubordination_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_kernel__antisubordination_reading, "Equal Protection: Antisubordination Reading").
narrative_ontology:topic_domain(equal_protection_kernel__antisubordination_reading, "constitutional_law/education_policy/civil_rights").

domain_priors:requires_active_enforcement(equal_protection_kernel__antisubordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_kernel__antisubordination_reading, '651e56dd-cee3-4135-aca7-8a036eb2f708').
narrative_ontology:cs_kernel_codification('651e56dd-cee3-4135-aca7-8a036eb2f708', fixed_text).
narrative_ontology:cs_authority_grounding('651e56dd-cee3-4135-aca7-8a036eb2f708', lineage).
narrative_ontology:cs_interpretation_layer_present('651e56dd-cee3-4135-aca7-8a036eb2f708').
narrative_ontology:cs_reading_relation('651e56dd-cee3-4135-aca7-8a036eb2f708', equal_protection_kernel__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('651e56dd-cee3-4135-aca7-8a036eb2f708', equal_protection_kernel__remedial_reading, influences).
narrative_ontology:cs_axiom('651e56dd-cee3-4135-aca7-8a036eb2f708', foundational, racial_classification_is_not_per_se_harmful).
narrative_ontology:cs_axiom_status(racial_classification_is_not_per_se_harmful, holdable).
narrative_ontology:cs_axiom_grounding('651e56dd-cee3-4135-aca7-8a036eb2f708', racial_classification_is_not_per_se_harmful, deontological).
narrative_ontology:cs_axiom('651e56dd-cee3-4135-aca7-8a036eb2f708', foundational, equal_protection_requires_dismantling_hierarchy).
narrative_ontology:cs_axiom_status(equal_protection_requires_dismantling_hierarchy, holdable).
narrative_ontology:cs_axiom_grounding('651e56dd-cee3-4135-aca7-8a036eb2f708', equal_protection_requires_dismantling_hierarchy, deontological).
narrative_ontology:cs_reference_frame('651e56dd-cee3-4135-aca7-8a036eb2f708', post_brown_v_board_era).
narrative_ontology:cs_drift_state('651e56dd-cee3-4135-aca7-8a036eb2f708', contemporary_jurisprudence, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('651e56dd-cee3-4135-aca7-8a036eb2f708', '').
narrative_ontology:cs_kernel_id(equal_protection_kernel__antisubordination_reading, equal_protection_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_kernel__antisubordination_reading, historically_subordinated_groups).
narrative_ontology:constraint_victim(equal_protection_kernel__antisubordination_reading, institutions_entrenching_hierarchy).
narrative_ontology:constraint_victim(equal_protection_kernel__antisubordination_reading, dominant_groups_benefiting_from_hierarchy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups are the intended beneficiaries of the antisubordination principle, as it aims to dismantle systemic barriers and address historical disadvantage. Their 'exit' from subordination is constrained by deeply entrenched societal structures.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, historically_subordinated_groups, beneficiary,
    powerless, generational, constrained, national).

% These are state or private institutions whose practices, even if facially neutral, perpetuate caste-like subordination. The antisubordination reading targets their actions, requiring them to dismantle rather than entrench hierarchy, incurring costs of reform and legal challenge.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, institutions_entrenching_hierarchy, payer,
    institutional, generational, constrained, national).

% These groups benefit from existing social and economic hierarchies, even if not intentionally perpetuating them. The antisubordination reading may require adjustments to policies that indirectly favor them, leading to perceived 'costs' or 'reverse discrimination' claims.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, dominant_groups_benefiting_from_hierarchy, payer,
    powerful, generational, constrained, national).

% These organizations actively champion the antisubordination reading in legal and political discourse, pushing for policies and judicial interpretations that align with its principles. They are key drivers of its enforcement and evolution.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, civil_rights_advocates, agenda_setter,
    organized, generational, mobile, national).

% Courts, particularly the Supreme Court, are the primary interpreters and enforcers of the Equal Protection Clause. Their rulings shape the application and scope of the antisubordination reading, balancing it against other interpretive principles.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, courts, agenda_setter,
    institutional, generational, constrained, national).

% Advocates for a colorblind interpretation of the Equal Protection Clause are structurally excluded from the core premise of the antisubordination reading, which permits race-conscious measures. They actively contest this reading in public and legal forums.
narrative_ontology:constraint_stakeholder(equal_protection_kernel__antisubordination_reading, colorblind_advocates, excluded,
    organized, generational, mobile, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_kernel__antisubordination_reading, historically_subordinated_groups).
narrative_ontology:fixing_cost_class(equal_protection_kernel__antisubordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate state action towards the dismantling of caste-like subordination and the prevention of policies that entrench racial hierarchy, ensuring that the Equal Protection Clause serves its original purpose of achieving substantive equality.
% TRANSFER_FUNCTION: Transfers social and political capital, as well as access to opportunities, from dominant groups and institutions that benefit from existing hierarchies to historically subordinated groups, by permitting remedial and race-conscious state action.
% ABSENT_VOICES: Those who benefit from existing, often unacknowledged, hierarchies are often absent from the conversation about the need for antisubordination. They would argue that any race-conscious measure is inherently discriminatory and violates 'equal protection' for all individuals, regardless of group status.
% DISAPPEARANCE_RATIONALE: If the antisubordination reading vanished, the legal justification for many civil rights remedies and affirmative action policies would disappear. State action would likely revert to a purely colorblind approach, potentially entrenching existing hierarchies and leading to a significant rearrangement of social and political power dynamics, with negative impacts on historically subordinated groups.
% FOUNDING_PROBLEM: The Equal Protection Clause was established to address the systemic subordination of formerly enslaved people and prevent the re-establishment of caste-like systems in the post-Civil War era.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights organizations, legal scholars, and historical analyses outside of the direct beneficiaries consistently attest that the problem of caste-like subordination, though evolved, remains live in contemporary society, evidenced by persistent racial disparities in wealth, health, education, and criminal justice outcomes. This is corroborated by sociological data and UN reports on racial discrimination.
narrative_ontology:disappearance_verdict(equal_protection_kernel__antisubordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_kernel__antisubordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_kernel__antisubordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(equal_protection_kernel__antisubordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_kernel__antisubordination_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

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
 *   The `extractiveness` (0.68) is high because this reading actively seeks to reallocate power and resources away from existing hierarchies, which is perceived as extractive by those who benefit from the status quo. `Suppression` (0.75) is also high, as it requires active enforcement to forbid actions that entrench hierarchy and to permit (and protect) remedial measures against strong opposition. `Theater_ratio` is low (0.15) because this is a live, contested legal principle with significant real-world impact and active litigation, not merely performative. `Resistance` (0.80) is very high due to the intense political and legal opposition from other interpretive camps.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of historically subordinated groups and civil rights advocates, this reading is a necessary tool for justice and equality, reducing systemic extraction. From the perspective of dominant groups and colorblind advocates, it is itself an extractive and discriminatory measure, imposing costs and 'reverse discrimination'. The engine's computation of per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically subordinated groups are the primary beneficiaries (low d) as the reading aims to dismantle barriers they face. Institutions and dominant groups that benefit from existing hierarchies are the targets (high d), as the reading imposes costs on them to reform practices or relinquish unearned advantages. Civil rights advocates and courts act as agenda-setters, driving the interpretation and enforcement. Colorblind advocates are structurally excluded from the core premise of this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification as a 'tangled_rope' prevents mislabeling by acknowledging both its genuine coordination function (dismantling subordination) and its inherently extractive nature (from those who benefit from existing hierarchies). It is not a 'snare' because it aims to solve a genuine problem of systemic injustice, but it is not a 'rope' because it requires active enforcement against powerful interests and imposes significant costs on identifiable groups, leading to asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    definition_of_subordination,
    'How is ''caste-like subordination'' precisely defined and empirically measured in contemporary society, beyond historical context?',
    'Development of clear, judicially accepted sociological and economic indicators for systemic subordination, and consistent application across cases.',
    'A clear definition would strengthen the antisubordination reading''s application, making its enforcement more consistent. An ambiguous definition leaves it vulnerable to challenges from colorblind interpretations, potentially weakening its impact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(definition_of_subordination, conceptual, 'Ambiguity in defining the core concept of ''subordination''.').

omega_variable(
    scope_of_remedial_action,
    'What is the permissible scope and duration of ''state action that dismantles hierarchy'' under this reading, particularly when it involves race-conscious measures?',
    'Further judicial clarification on ''narrow tailoring'' and ''compelling interest'' within an antisubordination framework, distinct from the remedial reading''s stricter tests.',
    'If the scope is too broad, it risks being conflated with or challenged by the colorblind reading. If too narrow, it may fail to effectively dismantle systemic subordination, reducing its practical impact.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_remedial_action, empirical, 'Uncertainty regarding the practical limits of race-conscious remedial action.').

omega_variable(
    resistance_to_dismantling_hierarchy,
    'To what extent does resistance to the antisubordination reading stem from genuine concerns about individual rights versus a desire to maintain existing social hierarchies?',
    'Sociological and political science research analyzing the motivations and impacts of opposition to antisubordination policies, including public opinion surveys and legal advocacy analysis.',
    'If resistance is primarily about maintaining hierarchy, it underscores the extractive nature of the status quo and the necessity of the antisubordination reading. If it''s genuinely about individual rights, it highlights a fundamental conceptual tension within equal protection jurisprudence.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(resistance_to_dismantling_hierarchy, empirical, 'Nature of opposition to antisubordination principles.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_kernel__antisubordination_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1954, equal_protection_kernel__antisubordination_reading, theater_ratio, 1954, 0.1).
narrative_ontology:measurement(equa_tr_t1970, equal_protection_kernel__antisubordination_reading, theater_ratio, 1970, 0.12).
narrative_ontology:measurement(equa_tr_t1985, equal_protection_kernel__antisubordination_reading, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(equa_tr_t2000, equal_protection_kernel__antisubordination_reading, theater_ratio, 2000, 0.18).
narrative_ontology:measurement(equa_tr_t2010, equal_protection_kernel__antisubordination_reading, theater_ratio, 2010, 0.16).
narrative_ontology:measurement(equa_tr_t2024, equal_protection_kernel__antisubordination_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(equa_be_t1954, equal_protection_kernel__antisubordination_reading, base_extractiveness, 1954, 0.5).
narrative_ontology:measurement(equa_be_t1970, equal_protection_kernel__antisubordination_reading, base_extractiveness, 1970, 0.6).
narrative_ontology:measurement(equa_be_t1985, equal_protection_kernel__antisubordination_reading, base_extractiveness, 1985, 0.7).
narrative_ontology:measurement(equa_be_t2000, equal_protection_kernel__antisubordination_reading, base_extractiveness, 2000, 0.65).
narrative_ontology:measurement(equa_be_t2010, equal_protection_kernel__antisubordination_reading, base_extractiveness, 2010, 0.68).
narrative_ontology:measurement(equa_be_t2024, equal_protection_kernel__antisubordination_reading, base_extractiveness, 2024, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1954, equal_protection_kernel__antisubordination_reading, suppression_requirement, 1954, 0.6).
narrative_ontology:measurement(equa_su_t1970, equal_protection_kernel__antisubordination_reading, suppression_requirement, 1970, 0.7).
narrative_ontology:measurement(equa_su_t1985, equal_protection_kernel__antisubordination_reading, suppression_requirement, 1985, 0.8).
narrative_ontology:measurement(equa_su_t2000, equal_protection_kernel__antisubordination_reading, suppression_requirement, 2000, 0.75).
narrative_ontology:measurement(equa_su_t2010, equal_protection_kernel__antisubordination_reading, suppression_requirement, 2010, 0.78).
narrative_ontology:measurement(equa_su_t2024, equal_protection_kernel__antisubordination_reading, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_kernel__antisubordination_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
