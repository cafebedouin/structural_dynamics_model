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
 *   Clause, which interprets it as forbidding the perpetuation of a caste
 *   system and permitting race-conscious measures to dismantle subordination.
 *   It is one of several competing readings of the 'equal protection
 *   commitment' kernel. The classification as a Tangled Rope reflects its
 *   dual function: it genuinely coordinates efforts to address historical
 *   injustice, but it also involves asymmetric extraction from historically
 *   privileged groups who are denied access in favor of remedial
 *   beneficiaries. Its persistence relies on active enforcement and judicial
 *   interpretation.
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
narrative_ontology:constraint_metric(equal_protection_commitment__remedial_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equal_protection_commitment__remedial_reading, tangled_rope).
narrative_ontology:human_readable(equal_protection_commitment__remedial_reading, "Equal Protection: Remedial Reading (Dismantling Subordination)").
narrative_ontology:topic_domain(equal_protection_commitment__remedial_reading, "constitutional_law/social_policy").

domain_priors:requires_active_enforcement(equal_protection_commitment__remedial_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equal_protection_commitment__remedial_reading, '1d4d9fea-0725-4f2e-9fae-2bf471fca1c6').
narrative_ontology:cs_kernel_codification('1d4d9fea-0725-4f2e-9fae-2bf471fca1c6', fixed_text).
narrative_ontology:cs_authority_grounding('1d4d9fea-0725-4f2e-9fae-2bf471fca1c6', lineage).
narrative_ontology:cs_interpretation_layer_present('1d4d9fea-0725-4f2e-9fae-2bf471fca1c6').
narrative_ontology:cs_reading_relation('1d4d9fea-0725-4f2e-9fae-2bf471fca1c6', equal_protection_commitment__colorblind_reading, coexists_with).
narrative_ontology:cs_reading_relation('1d4d9fea-0725-4f2e-9fae-2bf471fca1c6', equal_protection_commitment__diversity_reading, coexists_with).
narrative_ontology:cs_axiom('1d4d9fea-0725-4f2e-9fae-2bf471fca1c6', foundational, equal_protection_forbids_caste_perpetuation).
narrative_ontology:cs_axiom_status(equal_protection_forbids_caste_perpetuation, holdable).
narrative_ontology:cs_axiom_grounding('1d4d9fea-0725-4f2e-9fae-2bf471fca1c6', equal_protection_forbids_caste_perpetuation, deontological).
narrative_ontology:cs_axiom('1d4d9fea-0725-4f2e-9fae-2bf471fca1c6', foundational, race_conscious_measures_dismantle_subordination).
narrative_ontology:cs_axiom_status(race_conscious_measures_dismantle_subordination, holdable).
narrative_ontology:cs_axiom_grounding('1d4d9fea-0725-4f2e-9fae-2bf471fca1c6', race_conscious_measures_dismantle_subordination, instrumental).
narrative_ontology:cs_reference_frame('1d4d9fea-0725-4f2e-9fae-2bf471fca1c6', post_civil_rights_substantive_equality).
narrative_ontology:cs_drift_state('1d4d9fea-0725-4f2e-9fae-2bf471fca1c6', contemporary_judicial_skepticism, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('1d4d9fea-0725-4f2e-9fae-2bf471fca1c6', '2024-07-30T12:00:00Z').
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

% Benefit from race-conscious measures designed to remedy past and present discrimination, aiming to dismantle systemic subordination. Their identity is often tied to the historical context of the caste system this reading seeks to overcome.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, historically_subordinated_groups, beneficiary,
    powerless, generational, identity_locked, national).

% Implement and defend race-conscious policies (e.g., affirmative action, targeted programs) to address racial inequality and dismantle structures of subordination. They face legal challenges and political resistance.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, state_actors_implementing_remedies, agenda_setter,
    institutional, biographical, constrained, national).

% Bear the costs of remedial policies when they are denied preferential access or opportunities due to race-conscious measures. They often perceive these policies as 'reverse discrimination' and challenge them legally.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, historically_privileged_groups_denied_access, payer,
    powerful, biographical, constrained, national).

% The ultimate arbiter of equal protection, whose evolving jurisprudence shapes the permissible scope of race-conscious remedies. Its decisions directly enforce or constrain this reading.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, supreme_court, agenda_setter,
    institutional, civilizational, analytical, national).

% Analyze, debate, and advocate for or against this reading, influencing public opinion and judicial interpretation. They provide the intellectual scaffolding for legal challenges and defenses.
narrative_ontology:constraint_stakeholder(equal_protection_commitment__remedial_reading, legal_scholars_and_advocates, observer,
    analytical, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates state and societal efforts to identify and dismantle systemic racial subordination, ensuring that remedies are targeted and effective without perpetuating new forms of discrimination.
% TRANSFER_FUNCTION: Transfers opportunities, resources, and status from historically privileged groups (who may have benefited from past discrimination) to historically subordinated groups, aiming to achieve substantive equality.
% ABSENT_VOICES: Those who believe that any race-conscious measure is inherently discriminatory, regardless of intent, are often excluded from the policy-making process for remedial programs, though their legal challenges are heard by the courts.
% DISAPPEARANCE_RATIONALE: If this reading of equal protection vanished, state actors would lose a primary legal justification for race-conscious remedial programs. The legal landscape for addressing systemic inequality would fundamentally shift, likely leading to a rollback of existing programs and a re-entrenchment of existing disparities, forcing civil rights advocates to find new legal theories.
% FOUNDING_PROBLEM: The historical and ongoing perpetuation of a racial caste system in the United States, despite formal legal equality, leading to persistent disparities in social, economic, and political life.
% FOUNDING_PROBLEM_CORROBORATION: Civil rights organizations, sociological studies, and historical analyses consistently corroborate the ongoing existence and impact of systemic racial subordination. While some political actors dispute the 'live' status, independent academic and advocacy groups outside the direct beneficiaries of remedial programs attest to its persistence.
narrative_ontology:disappearance_verdict(equal_protection_commitment__remedial_reading, world_rearranges).
narrative_ontology:founding_problem_status(equal_protection_commitment__remedial_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(equal_protection_commitment__remedial_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The extractiveness (0.55) is substantial because it reallocates opportunities based on race, which is perceived as a cost by those who lose out. Suppression (0.70) is high due to the active legal and political battles required to implement and defend these policies against challenges from other readings. The 'claimed_type' is Rope from the perspective of its proponents (solving a coordination problem of justice), but the metrics reflect the contested and extractive reality of its operation, leading to a computed Tangled Rope classification.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of historically subordinated groups and state actors implementing remedies, this reading is a necessary mechanism for justice and coordination. From the perspective of historically privileged groups denied access, it is an extractive and discriminatory constraint. The engine's classification captures this divergence by computing different per-seat types.
 *
 * DIRECTIONALITY LOGIC:
 *   Historically subordinated groups and state actors implementing remedies are beneficiaries (d near 0.0-0.2), as the constraint subsidizes their efforts to achieve equality. Historically privileged groups denied access are targets (d near 0.8-1.0), as the constraint extracts opportunities from them. The Supreme Court and legal scholars are observers/agenda-setters, with directionality depending on their specific jurisprudential leanings and roles in shaping the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling genuine efforts to dismantle subordination as pure extraction. By recognizing the coordination function (dismantling caste) alongside the asymmetric extraction (from privileged groups), it highlights the inherent tension and contestability of remedial policies, rather than dismissing them as either purely beneficial or purely harmful. The 'live' status of the founding problem indicates it is not a Piton, as its mandate is still actively pursued, albeit with significant resistance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    remedial_vs_colorblind_framing,
    'Is the Equal Protection Clause fundamentally about achieving substantive equality (remedial reading) or about prohibiting all state-sponsored racial distinctions (colorblind reading)?',
    'A definitive Supreme Court ruling that explicitly forecloses one interpretation in favor of the other, or a constitutional amendment clarifying the clause''s intent.',
    'If the colorblind reading were to definitively foreclose the remedial reading, all race-conscious measures would become unconstitutional, fundamentally altering the constraint''s beneficiaries and victims and shifting its classification towards a Snare for historically subordinated groups.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(remedial_vs_colorblind_framing, conceptual, 'The core conceptual disagreement between the remedial and colorblind interpretations of equal protection.').

omega_variable(
    subordination_empirical_status,
    'To what extent do systemic racial subordination and a de facto caste system persist in contemporary society, justifying race-conscious remedies?',
    'Comprehensive, longitudinal empirical studies on racial disparities in wealth, education, health, and criminal justice, with broad consensus among social scientists.',
    'If empirical evidence conclusively showed the absence of systemic subordination, the justification for race-conscious remedies would erode, potentially shifting the constraint''s classification towards a Snare (pure extraction) for those denied access, as the coordination function would be moot.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(subordination_empirical_status, empirical, 'The empirical basis for the remedial reading''s core premise regarding ongoing subordination.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equal_protection_commitment__remedial_reading, 1960, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(equa_be_t1960, equal_protection_commitment__remedial_reading, base_extractiveness, 1960, 0.45).
narrative_ontology:measurement(equa_be_t1980, equal_protection_commitment__remedial_reading, base_extractiveness, 1980, 0.5).
narrative_ontology:measurement(equa_be_t2000, equal_protection_commitment__remedial_reading, base_extractiveness, 2000, 0.55).
narrative_ontology:measurement(equa_be_t2024, equal_protection_commitment__remedial_reading, base_extractiveness, 2024, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(equa_su_t1960, equal_protection_commitment__remedial_reading, suppression_requirement, 1960, 0.6).
narrative_ontology:measurement(equa_su_t1980, equal_protection_commitment__remedial_reading, suppression_requirement, 1980, 0.65).
narrative_ontology:measurement(equa_su_t2000, equal_protection_commitment__remedial_reading, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(equa_su_t2024, equal_protection_commitment__remedial_reading, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equal_protection_commitment__remedial_reading, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of the 'equal_protection_commitment' kernel, each with its own structural properties and classification. This 'remedial_reading' focuses on dismantling subordination, while the 'colorblind_reading' forbids all racial classifications and the 'diversity_reading' permits race for educational diversity.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
