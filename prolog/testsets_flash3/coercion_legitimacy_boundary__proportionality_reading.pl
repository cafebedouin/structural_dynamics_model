% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__proportionality_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__proportionality_reading, []).

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
 *   constraint_id: coercion_legitimacy_boundary__proportionality_reading
 *   human_readable: Public Health Coercion Proportionality Principle
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the 'proportionality reading' of public health
 *   coercion, where the legitimacy of state mandates (e.g., vaccination,
 *   isolation) scales with the severity and transmissibility of the disease.
 *   Measles, with its high R0 and potential for severe outcomes, justifies
 *   mandates, while seasonal flu, with lower severity and more manageable
 *   transmission, typically does not. This reading seeks a middle ground
 *   between absolute bodily autonomy and unlimited state power, emphasizing
 *   case-by-case adjudication based on scientific evidence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__proportionality_reading, 0.45).
domain_priors:suppression_score(coercion_legitimacy_boundary__proportionality_reading, 0.6).
domain_priors:theater_ratio(coercion_legitimacy_boundary__proportionality_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__proportionality_reading, "Public Health Coercion Proportionality Principle").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__proportionality_reading, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__proportionality_reading, '8bebac54-a5cb-4046-b2e5-a96d7531e352').
narrative_ontology:cs_kernel_codification('8bebac54-a5cb-4046-b2e5-a96d7531e352', formalized).
narrative_ontology:cs_authority_grounding('8bebac54-a5cb-4046-b2e5-a96d7531e352', lineage).
narrative_ontology:cs_interpretation_layer_present('8bebac54-a5cb-4046-b2e5-a96d7531e352').
narrative_ontology:cs_reading_relation('8bebac54-a5cb-4046-b2e5-a96d7531e352', coercion_legitimacy_boundary__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('8bebac54-a5cb-4046-b2e5-a96d7531e352', coercion_legitimacy_boundary__public_health_primary, coexists_with).
narrative_ontology:cs_axiom('8bebac54-a5cb-4046-b2e5-a96d7531e352', foundational, coercion_must_be_proportionate_to_harm).
narrative_ontology:cs_axiom_status(coercion_must_be_proportionate_to_harm, holdable).
narrative_ontology:cs_axiom_grounding('8bebac54-a5cb-4046-b2e5-a96d7531e352', coercion_must_be_proportionate_to_harm, deontological).
narrative_ontology:cs_axiom('8bebac54-a5cb-4046-b2e5-a96d7531e352', foundational, collective_good_can_override_individual_autonomy_conditionally).
narrative_ontology:cs_axiom_status(collective_good_can_override_individual_autonomy_conditionally, holdable).
narrative_ontology:cs_axiom_grounding('8bebac54-a5cb-4046-b2e5-a96d7531e352', collective_good_can_override_individual_autonomy_conditionally, instrumental).
narrative_ontology:cs_reference_frame('8bebac54-a5cb-4046-b2e5-a96d7531e352', historical_public_health_jurisprudence).
narrative_ontology:cs_drift_state('8bebac54-a5cb-4046-b2e5-a96d7531e352', contemporary_pandemic_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('8bebac54-a5cb-4046-b2e5-a96d7531e352', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__proportionality_reading, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, general_public).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, vulnerable_populations).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, individuals_subject_to_mandates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for assessing disease threats and implementing interventions, including mandates. They balance individual rights against collective health outcomes, applying a proportionality test to justify coercive measures based on pathogen severity and transmissibility.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from reduced disease transmission and protection of herd immunity, particularly against highly contagious and severe pathogens. They generally accept mandates when the threat is clear and the intervention is proportionate.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, general_public, beneficiary,
    organized, biographical, mobile, national).

% Are disproportionately protected by public health mandates, as they may be unable to be vaccinated or are at higher risk of severe outcomes. Their safety is a primary justification for coercive measures under this reading.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, vulnerable_populations, beneficiary,
    powerless, immediate, trapped, local).

% Bear the direct cost of coercion (e.g., mandatory vaccination, isolation). They accept these burdens when the disease threat is severe and the mandate is clearly justified by public health necessity, but resist when the proportionality is questionable.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, individuals_subject_to_mandates, payer,
    moderate, immediate, constrained, local).

% Monitor public health policies to ensure that coercive measures are strictly necessary, narrowly tailored, and proportionate to the threat, upholding individual rights and challenging overreach.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, civil_liberties_advocates, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective action to mitigate public health crises by establishing a framework for when individual autonomy can be overridden for the greater good, based on a proportional assessment of disease threat.
% TRANSFER_FUNCTION: Transfers a portion of individual autonomy (e.g., choice over medical interventions) to the state, in exchange for collective protection from severe communicable diseases.
% ABSENT_VOICES: Individuals who believe in absolute bodily autonomy, regardless of public health threat, are often marginalized in policy discussions that prioritize collective well-being, particularly during severe outbreaks.
% DISAPPEARANCE_RATIONALE: If this principle vanished, public health authorities would lose a key legal and ethical framework for intervention. This would lead to either unchecked individual autonomy (potentially increasing disease burden) or arbitrary state coercion (eroding trust), forcing a re-evaluation of the state's role in health.
% FOUNDING_PROBLEM: How to balance individual liberty with collective safety during epidemics, particularly when highly transmissible and severe diseases threaten widespread harm.
% FOUNDING_PROBLEM_CORROBORATION: Public health authorities, medical ethicists, and constitutional scholars widely attest that this problem remains live, as new pathogens and evolving social values continually challenge the boundaries of legitimate state coercion. Legal precedents and ethical guidelines from outside the direct beneficiaries support the ongoing relevance of this balancing act.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__proportionality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(coercion_legitimacy_boundary__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__proportionality_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__proportionality_reading_tests).
:- end_tests(coercion_legitimacy_boundary__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.45) is moderate because coercion is applied selectively and is intended to be temporary, justified by a clear public good. Suppression (0.6) is substantial as mandates require active enforcement and limit individual choices, but it's not absolute due to legal challenges and public debate. Theater ratio (0.1) is low, as the justification for mandates is generally grounded in real public health threats, not performative displays. Accessibility collapse (0.4) is moderate, as alternatives to compliance (e.g., legal challenge, social ostracization) exist but are constrained. Resistance (0.5) is moderate, reflecting ongoing debates and occasional non-compliance, particularly when proportionality is questioned.
 *
 * PERSPECTIVAL GAP:
 *   Public health authorities view this as a necessary and ethical framework for collective safety. Individuals subject to mandates, especially for less severe diseases, may perceive it as an overreach, highlighting the tension between individual rights and collective good. The engine's per-seat classification would reflect this divergence, with authorities seeing a Rope-like coordination and individuals experiencing a more Snare-like extraction when proportionality is perceived to fail.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities are the agenda-setters, balancing competing interests. The general public and vulnerable populations are beneficiaries, gaining protection from disease. Individuals subject to mandates are payers, bearing the direct cost of reduced autonomy. Civil liberties advocates act as observers, ensuring the proportionality principle is upheld.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_measurement_ambiguity,
    'How is ''severity'' and ''transmissibility'' objectively measured and weighted to determine the threshold for legitimate coercion, and who adjudicates disputes over these metrics?',
    'Establishment of an independent, interdisciplinary scientific panel with transparent methodologies for risk assessment, and a clear legal framework for judicial review of public health mandates.',
    'Lack of clear, agreed-upon metrics and adjudication processes can lead to arbitrary application of coercion, increasing extractiveness and suppression, potentially shifting the constraint towards a Snare. Clear metrics would reinforce its Tangled Rope nature.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_measurement_ambiguity, empirical, 'Ambiguity in defining and measuring the ''proportionality'' criteria for public health coercion.').

omega_variable(
    long_term_autonomy_erosion,
    'Does the repeated application of ''proportional'' coercion, even if justified in individual cases, lead to a gradual erosion of the societal expectation of bodily autonomy, making future coercion easier to implement?',
    'Longitudinal sociological studies tracking public attitudes towards autonomy and state intervention over decades, alongside legal analysis of precedent creep in public health law.',
    'If long-term erosion occurs, the constraint''s effective suppression and extractiveness would be higher than currently measured, as the ''cost'' of coercion diminishes over time due to habituation, potentially shifting it towards a Snare or Piton.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(long_term_autonomy_erosion, conceptual, 'Risk of ''autonomy creep'' from repeated proportional coercion.').

omega_variable(
    framing_under_determination,
    'Is this constraint best framed as a ''proportionality principle'' or as a ''negotiated compromise'' between competing ethical frameworks?',
    'Analysis of legal and ethical discourse: if the language consistently emphasizes ''balancing'' and ''trade-offs'' rather than ''inherent scaling,'' the compromise framing is stronger. If the language emphasizes ''inherent scaling'' of legitimacy, the proportionality framing is stronger.',
    'If framed as a negotiated compromise, the constraint''s stability might be lower, as it depends on ongoing agreement rather than an inherent principle. If framed as a principle, it might appear more robust, but could mask underlying power dynamics. The classification might shift from Tangled Rope to a more fragile Scaffold if the compromise is unstable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_under_determination, conceptual, 'Alternative framings of the constraint as a principle vs. a compromise.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__proportionality_reading, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t1900, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(coer_tr_t1950, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(coer_tr_t2000, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(coer_tr_t2024, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(coer_be_t1900, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 1900, 0.3).
narrative_ontology:measurement(coer_be_t1950, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 1950, 0.4).
narrative_ontology:measurement(coer_be_t2000, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 2000, 0.45).
narrative_ontology:measurement(coer_be_t2024, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 2024, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t1900, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 1900, 0.5).
narrative_ontology:measurement(coer_su_t1950, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 1950, 0.55).
narrative_ontology:measurement(coer_su_t2000, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 2000, 0.6).
narrative_ontology:measurement(coer_su_t2024, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__proportionality_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
