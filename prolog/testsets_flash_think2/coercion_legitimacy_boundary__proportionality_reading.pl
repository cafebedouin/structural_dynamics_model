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
 *   constraint_id: coercion_legitimacy_boundary__proportionality_reading
 *   human_readable: Coercion Legitimacy Boundary (Proportionality Reading)
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint, the 'proportionality_reading' of the
 *   'coercion_legitimacy_boundary' kernel, posits that the legitimacy of
 *   public health coercion scales with the severity and transmission dynamics
 *   of a disease. It argues that high-R0, high-mortality diseases (like
 *   measles) justify mandates, while low-severity diseases (like seasonal
 *   flu) do not. This reading seeks to balance collective harm prevention
 *   with individual autonomy through a case-by-case, evidence-based
 *   adjudication. It stands in contrast to the 'bodily_autonomy_primary'
 *   reading (which rejects all coercion) and the 'public_health_primary'
 *   reading (which prioritizes collective health over individual autonomy
 *   more broadly).
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__proportionality_reading, 0.55).
domain_priors:suppression_score(coercion_legitimacy_boundary__proportionality_reading, 0.65).
domain_priors:theater_ratio(coercion_legitimacy_boundary__proportionality_reading, 0.12).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 0.12).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__proportionality_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__proportionality_reading, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__proportionality_reading, "Coercion Legitimacy Boundary (Proportionality Reading)").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__proportionality_reading, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__proportionality_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__proportionality_reading, '3e7bb59a-af4c-47fb-bb48-40e171a5dc9f').
narrative_ontology:cs_kernel_codification('3e7bb59a-af4c-47fb-bb48-40e171a5dc9f', formalized).
narrative_ontology:cs_authority_grounding('3e7bb59a-af4c-47fb-bb48-40e171a5dc9f', expertise).
narrative_ontology:cs_interpretation_layer_present('3e7bb59a-af4c-47fb-bb48-40e171a5dc9f').
narrative_ontology:cs_reading_relation('3e7bb59a-af4c-47fb-bb48-40e171a5dc9f', coercion_legitimacy_boundary__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('3e7bb59a-af4c-47fb-bb48-40e171a5dc9f', coercion_legitimacy_boundary__public_health_primary, influences).
narrative_ontology:cs_axiom('3e7bb59a-af4c-47fb-bb48-40e171a5dc9f', foundational, coercion_must_be_proportionate_to_threat).
narrative_ontology:cs_axiom_status(coercion_must_be_proportionate_to_threat, holdable).
narrative_ontology:cs_axiom_grounding('3e7bb59a-af4c-47fb-bb48-40e171a5dc9f', coercion_must_be_proportionate_to_threat, empirically_contingent).
narrative_ontology:cs_axiom('3e7bb59a-af4c-47fb-bb48-40e171a5dc9f', foundational, individual_autonomy_is_defeasible_for_collective_safety).
narrative_ontology:cs_axiom_status(individual_autonomy_is_defeasible_for_collective_safety, holdable).
narrative_ontology:cs_axiom_grounding('3e7bb59a-af4c-47fb-bb48-40e171a5dc9f', individual_autonomy_is_defeasible_for_collective_safety, deontological).
narrative_ontology:cs_reference_frame('3e7bb59a-af4c-47fb-bb48-40e171a5dc9f', evidence_based_public_health_governance).
narrative_ontology:cs_drift_state('3e7bb59a-af4c-47fb-bb48-40e171a5dc9f', post_covid_pandemic_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('3e7bb59a-af4c-47fb-bb48-40e171a5dc9f', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__proportionality_reading, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, vulnerable_populations).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__proportionality_reading, general_public).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, individuals_subject_to_mandates).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__proportionality_reading, bodily_autonomy_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for assessing disease threats, recommending or implementing public health interventions, and justifying the proportionality of coercive measures based on scientific evidence. They benefit from the ability to act decisively in crises.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, public_health_authorities, agenda_setter,
    institutional, generational, analytical, global).

% Bear the direct costs of coercive measures, such as mandatory vaccination, quarantine, or mask-wearing. Their autonomy is curtailed for the collective good, but the constraint dictates this only when the threat is severe and transmission high.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, individuals_subject_to_mandates, payer,
    powerless, immediate, constrained, local).

% Benefit directly from coercive measures that reduce disease transmission, as they are at higher risk of severe illness or death. Their safety is prioritized by the proportionality calculus.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, vulnerable_populations, beneficiary,
    powerless, immediate, trapped, local).

% Benefits from the overall reduction of disease burden and societal disruption, allowing for a more stable and healthy environment. They also bear diffuse costs through general compliance and economic impacts.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, general_public, beneficiary,
    moderate, biographical, mobile, national).

% Analyze the ethical implications of public health coercion, evaluating whether interventions are truly proportionate, necessary, and minimally restrictive. They provide critical commentary and shape the discourse around the constraint.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, medical_ethicists, observer,
    analytical, biographical, analytical, global).

% Argue for the primacy of individual consent and bodily integrity, often opposing coercive public health measures regardless of disease severity. While their arguments are part of the public debate, this reading's framework often overrides their categorical objections.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__proportionality_reading, bodily_autonomy_advocates, excluded,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(coercion_legitimacy_boundary__proportionality_reading, public_health_authorities).
narrative_ontology:fixing_cost_class(coercion_legitimacy_boundary__proportionality_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for state intervention in public health crises, balancing individual rights with collective safety to prevent widespread disease and societal disruption, specifically by scaling coercion to the severity and transmissibility of the pathogen.
% TRANSFER_FUNCTION: Transfers a degree of individual autonomy (e.g., choice over vaccination, movement) to public health authorities in exchange for collective protection from severe infectious diseases, but only when the threat meets a high bar of proportionality.
% ABSENT_VOICES: Bodily autonomy advocates, who would argue for a much higher bar for any state coercion, or its categorical rejection. Their perspective is considered but often overridden by the proportionality calculus, which prioritizes collective safety under specific conditions.
% DISAPPEARANCE_RATIONALE: If this proportionality framework vanished, public health responses would either be paralyzed by individual resistance (if autonomy were absolute) or become overly broad and arbitrary (if public health were absolute), leading to either uncontrolled outbreaks or rights abuses. The balance it seeks to strike is critical for societal function during epidemics.
% FOUNDING_PROBLEM: How to justify state intervention in individual medical decisions during epidemics without infringing on fundamental rights, particularly when scientific understanding of disease transmission and severity is evolving, and public trust is fragile.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars, public health historians, and international human rights organizations corroborate the enduring nature of this problem, citing historical precedents and ongoing legal challenges that continuously test the boundaries of legitimate state coercion.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__proportionality_reading, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__proportionality_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__proportionality_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(coercion_legitimacy_boundary__proportionality_reading, 'none', 1).
narrative_ontology:epsilon_provenance(coercion_legitimacy_boundary__proportionality_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__proportionality_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(coercion_legitimacy_boundary__proportionality_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(coercion_legitimacy_boundary__proportionality_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it serves a genuine coordination function (protecting public health) but involves asymmetric extraction (curtailing individual autonomy) and requires active enforcement. Extractiveness (0.55) is moderate, reflecting that coercion is applied only when deemed necessary and proportionate, but it still represents a significant cost to individuals. Suppression (0.65) is also moderate, as mandates actively suppress individual choice, but are not absolute and are subject to legal challenge. The theater ratio (0.12) is low, indicating that the justification for coercion is generally based on functional, scientific assessment rather than performative maintenance. The temporal measurements reflect a hypothetical period where disease threats (and thus coercive measures) fluctuate, showing an increase in extractiveness and suppression during a crisis, followed by a return to a more moderate baseline.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public health authorities, this constraint is a necessary and ethical tool for societal protection. From the perspective of individuals subject to mandates, it represents a significant infringement on personal liberty, even if justified by severe threats. Medical ethicists and legal scholars often debate the precise location of the proportionality boundary, highlighting the inherent tension within the constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities are the agenda-setters and primary beneficiaries, gaining the capacity to protect the population. Vulnerable populations and the general public are also beneficiaries, receiving protection from disease. Individuals subject to mandates and bodily autonomy advocates are the payers/victims, bearing the cost of curtailed autonomy. The proportionality principle aims to ensure that the burden on victims is justified by the benefit to beneficiaries, but the extraction is still real.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_measurement_ambiguity,
    'How is ''severity'' and ''transmission dynamics'' objectively measured and weighted to determine the threshold for legitimate coercion?',
    'Development of standardized, internationally recognized epidemiological metrics and ethical frameworks for assessing disease threat and the proportionality of interventions, with transparent public review.',
    'Clearer metrics would reduce contestation over specific mandates, potentially lowering perceived extractiveness and resistance. Ambiguity allows for arbitrary application or political manipulation, increasing extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_measurement_ambiguity, empirical, 'Ambiguity in defining and measuring proportionality criteria for public health coercion.').

omega_variable(
    coercion_legitimacy_kernel_reading,
    'Is this constraint a genuine proportionality framework, or is it a cover for broader public health authority overreach?',
    'Longitudinal analysis of mandate application across diverse pathogens and political contexts, comparing outcomes against stated proportionality criteria and independent ethical reviews. If coercion is applied disproportionately to minor threats, it suggests a drift towards the ''public_health_primary'' reading without its explicit justification.',
    'If it''s a genuine proportionality framework, its Tangled Rope classification holds. If it''s a cover for overreach, it would reclassify towards a Snare, as the coordination story would be revealed as pretext for extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_legitimacy_kernel_reading, conceptual, 'Whether the proportionality reading genuinely limits coercion or serves as a flexible justification for state power.').

omega_variable(
    internalized_suppression_of_autonomy,
    'To what extent does the threat of coercion, even if proportionate, lead to internalized suppression of individual autonomy, where individuals self-censor or comply without genuine consent?',
    'Sociological studies on public perception of mandates, psychological research on compliance mechanisms, and analysis of long-term shifts in health-seeking behaviors post-mandate. If compliance persists even when mandates are lifted, it suggests internalized suppression.',
    'If internalized suppression is significant, the effective suppression of the constraint is higher than structural measures suggest, as individuals carry the suppression with them, impacting their future health choices.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(internalized_suppression_of_autonomy, empirical, 'Structural vs. internalized suppression mechanism in public health mandates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__proportionality_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t0, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(coer_tr_t6, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 6, 0.11).
narrative_ontology:measurement(coer_tr_t12, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 12, 0.13).
narrative_ontology:measurement(coer_tr_t18, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 18, 0.14).
narrative_ontology:measurement(coer_tr_t24, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 24, 0.13).
narrative_ontology:measurement(coer_tr_t30, coercion_legitimacy_boundary__proportionality_reading, theater_ratio, 30, 0.12).

% Extraction over time
narrative_ontology:measurement(coer_be_t0, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(coer_be_t6, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 6, 0.45).
narrative_ontology:measurement(coer_be_t12, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(coer_be_t18, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 18, 0.7).
narrative_ontology:measurement(coer_be_t24, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 24, 0.6).
narrative_ontology:measurement(coer_be_t30, coercion_legitimacy_boundary__proportionality_reading, base_extractiveness, 30, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t0, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(coer_su_t6, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 6, 0.55).
narrative_ontology:measurement(coer_su_t12, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 12, 0.7).
narrative_ontology:measurement(coer_su_t18, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 18, 0.75).
narrative_ontology:measurement(coer_su_t24, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 24, 0.7).
narrative_ontology:measurement(coer_su_t30, coercion_legitimacy_boundary__proportionality_reading, suppression_requirement, 30, 0.65).


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
