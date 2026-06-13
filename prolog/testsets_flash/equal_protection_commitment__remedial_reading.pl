% ============================================================================
% CONSTRAINT STORY: equal_protection_commitment__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_commitment__remedial_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: equal_protection_commitment__remedial_reading
 *   human_readable: Equal Protection: Remedial Reading
 *   domain: constitutional_law/political_philosophy/social_policy
 *
 * SUMMARY:
 *   This constraint represents the 'remedial reading' of the Equal Protection
 *   Clause, which interprets it as forbidding the perpetuation of a caste
 *   system and permitting race-conscious measures to dismantle historical
 *   subordination. This reading allows state actors to implement programs
 *   that benefit historically subordinated groups, potentially at the expense
 *   of historically privileged groups. It is a highly contested
 *   interpretation within constitutional law and political philosophy.
 *
 * KEY AGENTS:
 *   - historically_subordinated_groups: Primary beneficiary (powerless/constrained) — benefits from remedial policies.
 *   - state_actors_implementing_remedies: Agenda setter/beneficiary (institutional/analytical) — implements and defends remedial policies.
 *   - historically_privileged_groups_denied_access: Primary victim (organized/constrained) — bears costs of remedial policies.
 *   - colorblind_legal_scholars: Observer (analytical/analytical) — critiques the remedial reading as violating colorblind principles.
 *   - diversity_advocates: Observer (organized/analytical) — supports race-conscious measures for different reasons (diversity, not remediation).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_commitment__remedial_reading, 0.55).
domain_priors:suppression_score(equal_protection_commitment__remedial_reading, 0.7).
domain_priors:theater_ratio(equal_protection_commitment__remedial_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__remedial_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__remedial_reading, "Equal Protection: Remedial Reading").
narrative_ontology:topic_domain(equal_protection_commitment__remedial_reading, "constitutional_law/political_philosophy/social_policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__remedial_reading, '01365bd0-07dd-48ac-af92-1039e97ed2c4').
narrative_ontology:cs_kernel_codification('01365bd0-07dd-48ac-af92-1039e97ed2c4', fixed_text).
narrative_ontology:cs_authority_grounding('01365bd0-07dd-48ac-af92-1039e97ed2c4', lineage).
narrative_ontology:cs_interpretation_layer_present('01365bd0-07dd-48ac-af92-1039e97ed2c4').
narrative_ontology:cs_reading_relation('01365bd0-07dd-48ac-af92-1039e97ed2c4', equal_protection_commitment__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('01365bd0-07dd-48ac-af92-1039e97ed2c4', equal_protection_commitment__diversity_reading, coexists_with).
narrative_ontology:cs_axiom('01365bd0-07dd-48ac-af92-1039e97ed2c4', foundational, equal_protection_forbids_caste_system).
narrative_ontology:cs_axiom_status(equal_protection_forbids_caste_system, holdable).
narrative_ontology:cs_axiom_grounding('01365bd0-07dd-48ac-af92-1039e97ed2c4', equal_protection_forbids_caste_system, deontological).
narrative_ontology:cs_axiom('01365bd0-07dd-48ac-af92-1039e97ed2c4', foundational, race_conscious_measures_are_remedial).
narrative_ontology:cs_axiom_status(race_conscious_measures_are_remedial, holdable).
narrative_ontology:cs_axiom_grounding('01365bd0-07dd-48ac-af92-1039e97ed2c4', race_conscious_measures_are_remedial, instrumental).
narrative_ontology:cs_reference_frame('01365bd0-07dd-48ac-af92-1039e97ed2c4', post_civil_rights_remedial_jurisprudence).
narrative_ontology:cs_drift_state('01365bd0-07dd-48ac-af92-1039e97ed2c4', contemporary_supreme_court_precedent, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('01365bd0-07dd-48ac-af92-1039e97ed2c4', '').
narrative_ontology:cs_kernel_id(equal_protection_commitment__remedial_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, historically_subordinated_groups).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, state_actors_implementing_remedies).
narrative_ontology:constraint_victim(equal_protection_commitment__remedial_reading, historically_privileged_groups_denied_access).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups benefit from policies designed to remedy past and ongoing discrimination, such as affirmative action in education or employment. Their ability to exit systemic disadvantage is constrained by historical factors, making the remedial measures critical for their advancement.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, historically_subordinated_groups, beneficiary,
    powerless, generational, constrained, national).

% Government agencies, universities, and other state entities that design and implement race-conscious remedial programs. They are responsible for enforcing the remedial reading and defending it against legal challenges. They benefit from fulfilling their mandate to promote equality.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, state_actors_implementing_remedies, agenda_setter,
    institutional, generational, analytical, national).

% Individuals or groups who, due to remedial policies, may be denied opportunities (e.g., university admission, government contracts) that they might otherwise have received. They bear the direct costs of these policies and often organize to challenge them legally and politically.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, historically_privileged_groups_denied_access, payer,
    organized, biographical, constrained, national).

% Academics and legal practitioners who argue that the Equal Protection Clause mandates a 'colorblind' approach, forbidding any state use of racial classifications. They critique the remedial reading as a violation of constitutional principles and a form of reverse discrimination.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, colorblind_legal_scholars, observer,
    analytical, generational, analytical, national).

% Groups and individuals who support race-conscious measures, but primarily for the purpose of achieving educational or institutional diversity, rather than solely for remediation of past wrongs. Their arguments often overlap with, but are distinct from, the remedial reading's core premise.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, diversity_advocates, observer,
    organized, generational, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equal_protection_commitment__remedial_reading, historically_subordinated_groups).
narrative_ontology:fixing_cost_class(equal_protection_commitment__remedial_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Aims to coordinate societal efforts to dismantle a historical caste system by permitting and sometimes requiring race-conscious measures, thereby ensuring a more equitable distribution of opportunities and resources.
% TRANSFER_FUNCTION: Transfers opportunities (e.g., educational slots, employment, contracts) from historically privileged groups to historically subordinated groups, as a means to remedy systemic inequalities.
% ABSENT_VOICES: Those who believe that any racial classification by the state is inherently unconstitutional and harmful, regardless of intent, are often marginalized in the discourse surrounding the remedial reading, particularly within institutions committed to its implementation. They would argue for a strictly colorblind approach.
% DISAPPEARANCE_RATIONALE: If this remedial reading of Equal Protection vanished, state actors would likely cease race-conscious programs, leading to a significant rearrangement of access to education, employment, and other opportunities. Historically subordinated groups would lose a key mechanism for advancement, and the legal landscape around civil rights would fundamentally shift.
% FOUNDING_PROBLEM: The perpetuation of a racial caste system in the United States, characterized by systemic discrimination and subordination of certain groups, despite formal legal equality.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights organizations, sociological studies, and some legal scholars attest that the founding problem of a racial caste system, while evolved, remains live in various forms (e.g., wealth gaps, residential segregation, disparities in criminal justice). Critics, often from historically privileged groups, contest this, arguing that the caste system has been dismantled and that remedial measures are no longer necessary or are themselves discriminatory.
narrative_ontology:disappearance_verdict(equal_protection_commitment__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__remedial_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__remedial_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(equal_protection_commitment__remedial_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_commitment__remedial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_commitment__remedial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_commitment__remedial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.55) is substantial because identifiable groups bear direct costs (e.g., denied admission or contracts) for the benefit of others. Suppression (0.7) is high because the state actively enforces these race-conscious measures, and legal challenges to them are often suppressed or rejected by courts aligned with this reading. The theater ratio is low (0.2) as the measures are genuinely intended to achieve their stated remedial goals, even if their effectiveness is debated. Accessibility collapse is moderate (0.4) as alternatives for historically privileged groups are constrained, but not entirely eliminated, and resistance is high (0.8) due to ongoing legal and political challenges.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of historically subordinated groups and state actors, this constraint is a necessary tool for justice and coordination, aiming to correct systemic imbalances. From the perspective of historically privileged groups denied access, it is a form of reverse discrimination and pure extraction. The engine's classification will reflect this divergence based on the declared beneficiary/victim structure and associated metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically subordinated groups are beneficiaries (d=0.0-0.2) as the constraint aims to uplift them. State actors implementing remedies are also beneficiaries (d=0.1-0.3) as they fulfill their mandate and gain legitimacy. Historically privileged groups denied access are victims (d=0.8-1.0) as they bear the direct costs. The high extractiveness and suppression are amplified for these victims.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not subject to mandatrophy in the traditional sense, as its mandate (dismantling a caste system) is framed as an ongoing, unresolved problem. However, if empirical evidence were to conclusively show that the 'caste system' no longer exists or that remedial measures are ineffective or counterproductive, the constraint's justification would erode, potentially leading to a reclassification towards a Piton or Snare if it continued to be enforced without a live problem to solve.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    remedial_vs_colorblind_ambiguity,
    'Is the Equal Protection Clause fundamentally colorblind, or does it permit (or require) race-conscious measures to remedy historical subordination?',
    'Supreme Court precedent explicitly affirming or rejecting the remedial reading''s core premise, or a constitutional amendment clarifying the scope of equal protection.',
    'If the colorblind reading prevails, this constraint (remedial_reading) would be reclassified as a Snare, as its ''beneficiaries'' would be seen as unjustly favored and its ''victims'' as truly harmed. If the remedial reading is fully affirmed, its extractiveness would be seen as legitimate and necessary, potentially reclassifying it as a Rope or even a Mountain of justice.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(remedial_vs_colorblind_ambiguity, conceptual, 'Ambiguity between colorblind and remedial interpretations of Equal Protection.').

omega_variable(
    remedial_reading_legitimacy,
    'Does the remedial reading of Equal Protection genuinely dismantle a caste system, or does it perpetuate racial classification and create new forms of discrimination?',
    'Longitudinal empirical studies on social mobility, wealth accumulation, and educational attainment across racial groups, specifically evaluating the impact of race-conscious remedial policies.',
    'Empirical evidence of successful dismantling of subordination would strengthen the claim of coordination and reduce perceived extraction. Evidence of new forms of discrimination or lack of systemic change would increase perceived extraction and suppression, pushing towards a Snare classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_reading_legitimacy, empirical, 'Empirical effectiveness and unintended consequences of the remedial reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__remedial_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t0, equal_protection_commitment__remedial_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(equa_tr_t10, equal_protection_commitment__remedial_reading, theater_ratio, 10, 0.2).
narrative_ontology:measurement(equa_tr_t20, equal_protection_commitment__remedial_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(equa_tr_t30, equal_protection_commitment__remedial_reading, theater_ratio, 30, 0.2).

% Extraction over time
narrative_ontology:measurement(equa_be_t0, equal_protection_commitment__remedial_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(equa_be_t10, equal_protection_commitment__remedial_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(equa_be_t20, equal_protection_commitment__remedial_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(equa_be_t30, equal_protection_commitment__remedial_reading, base_extractiveness, 30, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t0, equal_protection_commitment__remedial_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(equa_su_t10, equal_protection_commitment__remedial_reading, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(equa_su_t20, equal_protection_commitment__remedial_reading, suppression_requirement, 20, 0.7).
narrative_ontology:measurement(equa_su_t30, equal_protection_commitment__remedial_reading, suppression_requirement, 30, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__remedial_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_commitment__remedial_reading, equal_protection_commitment__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__remedial_reading, equal_protection_commitment__diversity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'equal_protection_commitment' kernel. Each reading represents a distinct structural claim about the Equal Protection Clause, with different beneficiaries, victims, and operational metrics. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
