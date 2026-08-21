% ============================================================================
% CONSTRAINT STORY: equal_protection_clause__remedial_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equal_protection_clause__remedial_reading, []).

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
 *   constraint_id: equal_protection_clause__remedial_reading
 *   human_readable: Equal Protection Clause: Remedial Reading
 *   domain: Constitutional Law / Political Philosophy / Education Policy
 *
 * SUMMARY:
 *   This constraint represents the 'remedial reading' of the Equal Protection
 *   Clause, which interprets the clause as requiring race-conscious policies
 *   to actively redress historical group subordination and achieve
 *   substantive equality. It mandates interventions that may involve
 *   differential treatment based on race, with the explicit goal of
 *   overcoming systemic disadvantages. This reading is highly contested,
 *   particularly by those who advocate for a 'colorblind' interpretation of
 *   equal protection.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equal_protection_clause__remedial_reading, 0.78).
domain_priors:suppression_score(equal_protection_clause__remedial_reading, 0.65).
domain_priors:theater_ratio(equal_protection_clause__remedial_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, extractiveness, 0.78).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(equal_protection_clause__remedial_reading, resistance, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_clause__remedial_reading, scaffold).
narrative_ontology:human_readable(equal_protection_clause__remedial_reading, "Equal Protection Clause: Remedial Reading").
narrative_ontology:topic_domain(equal_protection_clause__remedial_reading, "Constitutional Law / Political Philosophy / Education Policy").

domain_priors:requires_active_enforcement(equal_protection_clause__remedial_reading).
narrative_ontology:has_sunset_clause(equal_protection_clause__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_clause__remedial_reading, '1ba06226-9629-47e1-8536-6ada40aa8c44').
narrative_ontology:cs_kernel_codification('1ba06226-9629-47e1-8536-6ada40aa8c44', fixed_text).
narrative_ontology:cs_authority_grounding('1ba06226-9629-47e1-8536-6ada40aa8c44', lineage).
narrative_ontology:cs_interpretation_layer_present('1ba06226-9629-47e1-8536-6ada40aa8c44').
narrative_ontology:cs_reading_relation('1ba06226-9629-47e1-8536-6ada40aa8c44', equal_protection_clause__colorblind_reading, forecloses).
narrative_ontology:cs_reading_relation('1ba06226-9629-47e1-8536-6ada40aa8c44', equal_protection_clause__diversity_reading, coexists_with).
narrative_ontology:cs_axiom('1ba06226-9629-47e1-8536-6ada40aa8c44', foundational, substantive_equality_mandate).
narrative_ontology:cs_axiom_status(substantive_equality_mandate, holdable).
narrative_ontology:cs_axiom_grounding('1ba06226-9629-47e1-8536-6ada40aa8c44', substantive_equality_mandate, deontological).
narrative_ontology:cs_axiom('1ba06226-9629-47e1-8536-6ada40aa8c44', foundational, historical_subordination_requires_remediation).
narrative_ontology:cs_axiom_status(historical_subordination_requires_remediation, holdable).
narrative_ontology:cs_axiom_grounding('1ba06226-9629-47e1-8536-6ada40aa8c44', historical_subordination_requires_remediation, empirically_contingent).
narrative_ontology:cs_reference_frame('1ba06226-9629-47e1-8536-6ada40aa8c44', post_reconstruction_amendments_intent).
narrative_ontology:cs_drift_state('1ba06226-9629-47e1-8536-6ada40aa8c44', contemporary_jurisprudence, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('1ba06226-9629-47e1-8536-6ada40aa8c44', '').
narrative_ontology:cs_kernel_id(equal_protection_clause__remedial_reading, equal_protection_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_clause__remedial_reading, historically_subordinated_racial_groups).
narrative_ontology:constraint_victim(equal_protection_clause__remedial_reading, individual_members_of_non_preferred_groups).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equal_protection_clause__remedial_reading, diversity_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups are the intended beneficiaries of race-conscious policies designed to counteract the lingering effects of historical discrimination. Their identity and well-being are deeply tied to the success of such policies, making exit from the framework of group-based redress difficult.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, historically_subordinated_racial_groups, beneficiary,
    organized, generational, identity_locked, national).

% Individuals who, under this reading, may be disadvantaged in competitive processes (e.g., university admissions, employment) due to race-conscious policies. They bear the direct costs of these policies, often perceiving them as individual discrimination rather than group-level remediation. Their options are to challenge the policies legally or accept the outcome.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, individual_members_of_non_preferred_groups, payer,
    moderate, biographical, constrained, local).

% Universities and other public institutions tasked with implementing race-conscious policies to achieve substantive equality. They navigate complex legal and political landscapes, balancing the mandate for remediation with challenges from those who perceive themselves as victims of reverse discrimination. Their compliance is actively enforced by courts.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, educational_institutions, agenda_setter,
    institutional, generational, constrained, national).

% The ultimate arbiters of the Equal Protection Clause, responsible for interpreting and enforcing this reading. They issue rulings that shape policy, often facing intense public scrutiny and political pressure. Their decisions define the scope and limits of race-conscious remediation.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, courts, agenda_setter,
    institutional, civilizational, analytical, national).

% Advocates who argue that the Equal Protection Clause forbids all racial classifications, regardless of intent, and that individuals should be treated without regard to race. They are structurally excluded from the remedial reading's framework, which necessitates race-conscious measures, and actively resist its implementation.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, colorblind_advocates, excluded,
    organized, biographical, mobile, national).

% Advocates who support race-conscious policies for reasons of educational diversity, rather than direct remediation. While their goals may sometimes align with the remedial reading's outcomes, their underlying justification is distinct. They benefit from the legal space created for race-conscious policies, even if the rationale differs.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, diversity_advocates, beneficiary,
    organized, biographical, mobile, national).

% Academics and legal experts who analyze the historical context, philosophical underpinnings, and practical effects of this reading. They provide critical commentary and theoretical frameworks, influencing judicial and public discourse without directly implementing or being subject to the constraint.
narrative_ontology:constraint_stakeholder(equal_protection_clause__remedial_reading, legal_scholars, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates public institutions and policy to actively address and dismantle the structural legacies of historical racial subordination, aiming for a future state of substantive equality.
% TRANSFER_FUNCTION: Transfers opportunities, resources, and status from individuals not belonging to historically subordinated groups to members of those groups, as a means of rectifying past injustices and achieving a more equitable distribution of societal goods.
% ABSENT_VOICES: Colorblind advocates are fundamentally excluded from the conversation, as their core premise (race-neutrality) directly contradicts the remedial reading's necessity for race-consciousness. They would argue that such policies perpetuate racial division and violate individual rights.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, institutions would immediately cease race-conscious policies, leading to significant shifts in demographic representation in education and employment. The legal and political landscape around civil rights would fundamentally reorganize, likely leading to increased disparities for historically subordinated groups and a re-entrenchment of colorblind legal principles.
% FOUNDING_PROBLEM: The persistence of systemic racial inequality and the lingering effects of slavery, Jim Crow, and other forms of de jure and de facto discrimination, which rendered formal equality insufficient to achieve actual equality.
% FOUNDING_PROBLEM_CORROBORATION: Proponents of this reading, including civil rights organizations and many legal scholars, attest that the founding problem of systemic racial inequality remains live, citing ongoing disparities in wealth, education, and health outcomes. Opponents, including colorblind advocates, contest this, arguing that the problem has been largely solved or that race-conscious remedies are counterproductive. Independent sociological and economic studies often corroborate the persistence of disparities, though their causes are debated.
narrative_ontology:disappearance_verdict(equal_protection_clause__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_clause__remedial_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_clause__remedial_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(equal_protection_clause__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_clause__remedial_reading, 0.78, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equal_protection_clause__remedial_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(equal_protection_clause__remedial_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equal_protection_clause__remedial_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.78) is high because this reading mandates policies that reallocate opportunities based on race, extracting from some individuals to benefit others. Suppression (0.65) is moderate-high, reflecting the active enforcement required to implement these policies against significant legal and political resistance, and the suppression of purely colorblind alternatives. Theater ratio (0.15) is low, as the policies are intended to have direct, functional effects. Resistance (0.85) is very high, as this reading faces continuous legal challenges and public opposition. Accessibility collapse (0.4) is moderate, as it limits certain individual-level alternatives in favor of group-level remediation, but other avenues for advancement still exist.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of historically subordinated groups, this reading is a necessary tool for justice and equality, correcting deep-seated historical wrongs. From the perspective of individual members of non-preferred groups, it can be perceived as unfair discrimination, violating principles of individual merit. The courts, as agenda-setters, must navigate these conflicting perspectives, often leading to complex and evolving jurisprudence.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically subordinated racial groups are the primary beneficiaries, as the policies are designed to improve their societal position. Individual members of non-preferred groups are the primary payers/victims, as they may face disadvantages in competitive contexts. Educational institutions and courts act as agenda-setters, implementing and enforcing the policies. Colorblind advocates are structurally excluded, as their core premise is incompatible with this reading.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    remediation_completion_criteria,
    'What objective criteria define the ''completion'' of remediation, at which point race-conscious policies would sunset?',
    'Empirical consensus on specific, measurable indicators of substantive equality (e.g., parity in wealth, education, health outcomes) that are directly attributable to the policies, or a legislative/judicial declaration of completion.',
    'If clear, measurable criteria are established and met, the constraint would transition from a Scaffold to a Piton or Rope as its function atrophies. If criteria remain undefined or perpetually unmet, the ''temporary'' nature becomes theatrical, increasing the theater_ratio and potentially reclassifying it as a Tangled Rope or Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(remediation_completion_criteria, conceptual, 'Ambiguity regarding the conditions for sunsetting race-conscious policies.').

omega_variable(
    group_vs_individual_rights_priority,
    'Does the Equal Protection Clause primarily protect individual rights against racial discrimination, or does it permit/require group-based remedies to achieve collective equality?',
    'A definitive Supreme Court ruling or constitutional amendment that explicitly prioritizes either individual colorblindness or group-based substantive equality in all contexts.',
    'If individual rights are prioritized, this remedial reading would be foreclosed, likely shifting the classification towards a Mountain (colorblindness as natural law) or Rope (coordination around individual merit). If group rights are prioritized, the remedial reading''s legitimacy would be strengthened, potentially reducing resistance and increasing its stability as a Scaffold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(group_vs_individual_rights_priority, conceptual, 'Fundamental tension between individual and group rights in equal protection jurisprudence.').

omega_variable(
    causality_of_disparities,
    'To what extent are contemporary racial disparities directly attributable to historical subordination, versus other socio-economic factors?',
    'Comprehensive, longitudinal empirical studies that rigorously control for confounding variables and establish clear causal links between historical discrimination and current disparities, or the absence thereof.',
    'If a strong causal link is empirically disproven, the justification for race-conscious remediation weakens, potentially reducing its perceived legitimacy and increasing its theater_ratio. If the link is strongly affirmed, the remedial reading''s justification is reinforced, potentially reducing resistance from some quarters.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(causality_of_disparities, empirical, 'Empirical basis for attributing current disparities to historical subordination.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_clause__remedial_reading, 1960, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1960, equal_protection_clause__remedial_reading, theater_ratio, 1960, 0.05).
narrative_ontology:measurement(equa_tr_t1975, equal_protection_clause__remedial_reading, theater_ratio, 1975, 0.1).
narrative_ontology:measurement(equa_tr_t1990, equal_protection_clause__remedial_reading, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(equa_tr_t2005, equal_protection_clause__remedial_reading, theater_ratio, 2005, 0.18).
narrative_ontology:measurement(equa_tr_t2024, equal_protection_clause__remedial_reading, theater_ratio, 2024, 0.15).

% Extraction over time
narrative_ontology:measurement(equa_be_t1960, equal_protection_clause__remedial_reading, base_extractiveness, 1960, 0.6).
narrative_ontology:measurement(equa_be_t1975, equal_protection_clause__remedial_reading, base_extractiveness, 1975, 0.8).
narrative_ontology:measurement(equa_be_t1990, equal_protection_clause__remedial_reading, base_extractiveness, 1990, 0.75).
narrative_ontology:measurement(equa_be_t2005, equal_protection_clause__remedial_reading, base_extractiveness, 2005, 0.7).
narrative_ontology:measurement(equa_be_t2024, equal_protection_clause__remedial_reading, base_extractiveness, 2024, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1960, equal_protection_clause__remedial_reading, suppression_requirement, 1960, 0.4).
narrative_ontology:measurement(equa_su_t1975, equal_protection_clause__remedial_reading, suppression_requirement, 1975, 0.7).
narrative_ontology:measurement(equa_su_t1990, equal_protection_clause__remedial_reading, suppression_requirement, 1990, 0.65).
narrative_ontology:measurement(equa_su_t2005, equal_protection_clause__remedial_reading, suppression_requirement, 2005, 0.6).
narrative_ontology:measurement(equa_su_t2024, equal_protection_clause__remedial_reading, suppression_requirement, 2024, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_clause__remedial_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, equal_protection_clause__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_clause__remedial_reading, equal_protection_clause__diversity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the Equal Protection Clause kernel. Each reading instantiates a different constraint with unique structural properties and implications for policy and rights.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
