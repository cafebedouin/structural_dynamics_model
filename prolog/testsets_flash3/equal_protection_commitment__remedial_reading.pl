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
    narrative_ontology:constraint_vindicates/2,
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
 *   constraint_id: equal_protection_commitment__remedial_reading
 *   human_readable: Equal Protection: Remedial Reading (Dismantling Subordination)
 *   domain: constitutional_law/social_policy
 *
 * SUMMARY:
 *   This constraint represents the 'remedial reading' of the Equal Protection
 *   Clause, which holds that the clause forbids the perpetuation of a caste
 *   system and permits race-conscious measures specifically designed to
 *   dismantle systemic subordination. This reading is distinct from
 *   'colorblind' and 'diversity' interpretations. It acknowledges that
 *   race-conscious measures, while potentially imposing costs on some, are
 *   necessary to achieve the anti-subordination goal of the Fourteenth
 *   Amendment. The classification as a Tangled Rope reflects its genuine
 *   coordination function (dismantling subordination) coupled with asymmetric
 *   extraction (costs borne by historically privileged groups).
 *
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
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__remedial_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__remedial_reading, "Equal Protection: Remedial Reading (Dismantling Subordination)").
narrative_ontology:topic_domain(equal_protection_commitment__remedial_reading, "constitutional_law/social_policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__remedial_reading, '823a5300-d153-4cda-bb8d-f6d5bacbed61').
narrative_ontology:cs_kernel_codification('823a5300-d153-4cda-bb8d-f6d5bacbed61', fixed_text).
narrative_ontology:cs_authority_grounding('823a5300-d153-4cda-bb8d-f6d5bacbed61', lineage).
narrative_ontology:cs_interpretation_layer_present('823a5300-d153-4cda-bb8d-f6d5bacbed61').
narrative_ontology:cs_reading_relation('823a5300-d153-4cda-bb8d-f6d5bacbed61', equal_protection_commitment__colorblind_reading, coexists_with).
narrative_ontology:cs_reading_relation('823a5300-d153-4cda-bb8d-f6d5bacbed61', equal_protection_commitment__diversity_reading, coexists_with).
narrative_ontology:cs_axiom('823a5300-d153-4cda-bb8d-f6d5bacbed61', foundational, anti_subordination_is_primary_goal).
narrative_ontology:cs_axiom_status(anti_subordination_is_primary_goal, holdable).
narrative_ontology:cs_axiom_grounding('823a5300-d153-4cda-bb8d-f6d5bacbed61', anti_subordination_is_primary_goal, deontological).
narrative_ontology:cs_axiom('823a5300-d153-4cda-bb8d-f6d5bacbed61', foundational, race_conscious_measures_are_permissible_remedies).
narrative_ontology:cs_axiom_status(race_conscious_measures_are_permissible_remedies, holdable).
narrative_ontology:cs_axiom_grounding('823a5300-d153-4cda-bb8d-f6d5bacbed61', race_conscious_measures_are_permissible_remedies, conventional).
narrative_ontology:cs_reference_frame('823a5300-d153-4cda-bb8d-f6d5bacbed61', post_brown_anti_subordination_framework).
narrative_ontology:cs_drift_state('823a5300-d153-4cda-bb8d-f6d5bacbed61', contemporary_judicial_challenges, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('823a5300-d153-4cda-bb8d-f6d5bacbed61', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(equal_protection_commitment__remedial_reading, equal_protection_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, historically_subordinated_groups).
narrative_ontology:constraint_beneficiary(equal_protection_commitment__remedial_reading, state_actors_implementing_remedies).
narrative_ontology:constraint_victim(equal_protection_commitment__remedial_reading, historically_privileged_groups_denied_access).
narrative_ontology:constraint_vindicates(equal_protection_commitment__remedial_reading, anti_subordination_principle).
narrative_ontology:constraint_vindicates(equal_protection_commitment__remedial_reading, remedial_justice_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The ultimate arbiter of equal protection, interpreting the Fourteenth Amendment. Its rulings define the scope of permissible race-conscious measures, often navigating between competing readings of the clause. Its decisions are binding on all other state actors.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, supreme_court, agenda_setter,
    institutional, generational, constrained, national).

% Benefit from race-conscious remedial programs designed to dismantle systemic subordination and address historical disadvantage. Their access to education, employment, and other opportunities is enhanced by these measures. Exit from this identity is not a meaningful option.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, historically_subordinated_groups, beneficiary,
    powerless, generational, identity_locked, national).

% Government agencies, universities, and other public bodies that implement race-conscious programs to achieve remedial goals. They benefit from the legal clarity and authority to address historical injustices, though their actions are often challenged in court.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, state_actors_implementing_remedies, beneficiary,
    institutional, biographical, constrained, national).

% Bear the costs of remedial programs when they are denied preferential access or opportunities in favor of members of historically subordinated groups. They often perceive these measures as 'reverse discrimination' and actively resist them through litigation and political action.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, historically_privileged_groups_denied_access, payer,
    powerful, biographical, constrained, national).

% Analyze, debate, and advocate for particular interpretations of equal protection, including the remedial reading. They influence judicial and public opinion, shaping the ongoing contest over the constraint's meaning and application.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, legal_scholars_and_advocates, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state efforts to address and dismantle systemic racial subordination by providing a legal framework for race-conscious remedial measures, ensuring consistency across various government programs and institutions.
% TRANSFER_FUNCTION: Transfers opportunities, resources, and status from historically privileged groups (who might otherwise receive them based on 'merit' or existing structures) to historically subordinated groups, aiming to rectify past and ongoing injustices.
% ABSENT_VOICES: The voices of those who believe that any race-conscious measure, even remedial, is inherently discriminatory and unconstitutional are often present in the legal debate but are structurally excluded from the internal logic of the remedial reading itself, which prioritizes anti-subordination. They would argue for a strictly 'colorblind' approach.
% DISAPPEARANCE_RATIONALE: If the remedial reading of equal protection vanished, state actors would lose the legal basis for race-conscious programs aimed at dismantling subordination. This would likely lead to a re-entrenchment of existing inequalities, a significant shift in social policy, and a reorganization of advocacy efforts around new legal theories or political strategies.
% FOUNDING_PROBLEM: The Fourteenth Amendment's Equal Protection Clause was enacted to abolish the legal caste system of slavery and ensure equality for formerly enslaved people, addressing the problem of systemic racial subordination.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights organizations, historians, and many legal scholars attest that the problem of systemic racial subordination, though evolved, remains live. This is corroborated by ongoing disparities in wealth, education, health, and criminal justice outcomes, as documented by independent research institutions and government reports.
narrative_ontology:disappearance_verdict(equal_protection_commitment__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__remedial_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__remedial_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(equal_protection_commitment__remedial_reading, 'none', 1).
narrative_ontology:epsilon_provenance(equal_protection_commitment__remedial_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.55) is substantial because the remedial reading explicitly reallocates opportunities, creating identifiable 'victims' among historically privileged groups. Suppression (0.70) is high due to the active legal and political enforcement required to sustain remedial programs against challenges, and the suppression of alternative 'colorblind' interpretations in this specific reading. The theater ratio (0.20) is relatively low, indicating that the constraint's primary function remains genuine, though some performative aspects may exist in justifying specific programs. The measurements show a peak in extractiveness and suppression around 2003, reflecting intense legal challenges and public debate, followed by a slight decline as the legal landscape shifted.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of historically subordinated groups and state actors implementing remedies, this constraint is a necessary and just mechanism for achieving equality. From the perspective of historically privileged groups denied access, it is an unjust form of 'reverse discrimination.' The engine's per-seat classification will reflect this divergence, with beneficiaries experiencing it as a Rope or Scaffold, and victims as a Snare.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically subordinated groups and state actors implementing remedies are clear beneficiaries (low d), as the constraint actively works to their advantage. Historically privileged groups denied access are clear targets (high d), as they bear the direct costs of the remedial measures. Legal scholars and advocates, and the Supreme Court, occupy more analytical or agenda-setting roles, with d values closer to symmetric or slightly beneficiary depending on their specific stance within this reading.
 *
 * MANDATROPHY ANALYSIS:
 *   The remedial reading directly addresses the founding problem of systemic racial subordination, which is still considered 'live.' This prevents mandatrophy, as the constraint's mandate remains relevant and actively pursued. The classification as Tangled Rope acknowledges the inherent tension and costs of actively dismantling a caste system, preventing mislabeling it as pure extraction while recognizing its coercive aspects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    remedial_vs_colorblind_framing,
    'Is the Equal Protection Clause fundamentally ''colorblind'' (forbidding all racial classifications) or ''anti-subordination'' (permitting race-conscious remedies)?',
    'A definitive Supreme Court ruling that explicitly forecloses one interpretation in favor of the other, or a constitutional amendment clarifying the clause''s intent.',
    'If the ''colorblind'' reading were to definitively prevail, this remedial reading would be reclassified as a Snare, as its coordination function would be deemed illegitimate, and its extraction would be seen as pure coercion. Conversely, if the anti-subordination principle were more firmly entrenched, the constraint might move closer to a Rope for its beneficiaries.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(remedial_vs_colorblind_framing, conceptual, 'The core conceptual contest over the meaning of equal protection.').

omega_variable(
    empirical_efficacy_of_remedies,
    'Are race-conscious remedial measures empirically effective at dismantling systemic subordination, or do they perpetuate racial divisions without achieving their stated goals?',
    'Longitudinal studies tracking the impact of remedial programs on various indicators of subordination, with robust counterfactual analysis.',
    'If empirically shown to be ineffective, the ''coordination function'' aspect of this Tangled Rope would weaken, potentially pushing it towards a Snare if the extraction persists without demonstrable benefit. If highly effective, its legitimacy would be strengthened, potentially reducing perceived extractiveness for some.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(empirical_efficacy_of_remedies, empirical, 'The empirical question of whether remedial measures work as intended.').

omega_variable(
    beneficiary_victim_inversion,
    'Does the beneficiary/victim structure of this constraint invert depending on the observer''s position, making it a ''Snare for some, Rope for others''?',
    'Detailed per-seat analysis of effective extraction (chi) for all stakeholders, comparing the computed type for historically subordinated groups versus historically privileged groups.',
    'If the inversion is stark and consistent, it confirms the Tangled Rope classification''s core dynamic. If the perceived extraction is uniform across all seats, it would challenge the ''remedial'' aspect and suggest a different underlying structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(beneficiary_victim_inversion, empirical, 'Whether the constraint''s impact is fundamentally different for different groups.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__remedial_reading, 1954, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(equa_tr_t1954, equal_protection_commitment__remedial_reading, theater_ratio, 1954, 0.1).
narrative_ontology:measurement(equa_tr_t1978, equal_protection_commitment__remedial_reading, theater_ratio, 1978, 0.15).
narrative_ontology:measurement(equa_tr_t2003, equal_protection_commitment__remedial_reading, theater_ratio, 2003, 0.25).
narrative_ontology:measurement(equa_tr_t2024, equal_protection_commitment__remedial_reading, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(equa_be_t1954, equal_protection_commitment__remedial_reading, base_extractiveness, 1954, 0.45).
narrative_ontology:measurement(equa_be_t1978, equal_protection_commitment__remedial_reading, base_extractiveness, 1978, 0.55).
narrative_ontology:measurement(equa_be_t2003, equal_protection_commitment__remedial_reading, base_extractiveness, 2003, 0.6).
narrative_ontology:measurement(equa_be_t2024, equal_protection_commitment__remedial_reading, base_extractiveness, 2024, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1954, equal_protection_commitment__remedial_reading, suppression_requirement, 1954, 0.6).
narrative_ontology:measurement(equa_su_t1978, equal_protection_commitment__remedial_reading, suppression_requirement, 1978, 0.7).
narrative_ontology:measurement(equa_su_t2003, equal_protection_commitment__remedial_reading, suppression_requirement, 2003, 0.75).
narrative_ontology:measurement(equa_su_t2024, equal_protection_commitment__remedial_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__remedial_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(equal_protection_commitment__remedial_reading, equal_protection_commitment__colorblind_reading).
narrative_ontology:affects_constraint(equal_protection_commitment__remedial_reading, equal_protection_commitment__diversity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'equal_protection_commitment' kernel. Each reading instantiates a different constraint with unique structural properties and classifications, reflecting the ongoing contest over the Fourteenth Amendment's meaning.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
