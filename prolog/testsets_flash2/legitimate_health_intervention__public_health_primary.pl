% ============================================================================
% CONSTRAINT STORY: legitimate_health_intervention__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_legitimate_health_intervention__public_health_primary, []).

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
 *   constraint_id: legitimate_health_intervention__public_health_primary
 *   human_readable: Public Health Primary: Intervention Legitimacy from Morbidity/Mortality Reduction
 *   domain: public_health/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the 'public health primary' reading of
 *   legitimate health intervention, where the state's authority to intervene
 *   derives from its ability to measurably reduce population-level morbidity
 *   and mortality. Individual refusal of interventions is framed as imposing
 *   an externality on the collective. This reading prioritizes collective
 *   health outcomes, often leading to policies that restrict individual
 *   liberties for the greater good. The constraint is classified as a Tangled
 *   Rope due to its genuine coordination function (disease control) coupled
 *   with significant extraction from those whose autonomy is curtailed,
 *   requiring active enforcement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__public_health_primary, 0.7).
domain_priors:suppression_score(legitimate_health_intervention__public_health_primary, 0.65).
domain_priors:theater_ratio(legitimate_health_intervention__public_health_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, extractiveness, 0.7).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__public_health_primary, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__public_health_primary, "Public Health Primary: Intervention Legitimacy from Morbidity/Mortality Reduction").
narrative_ontology:topic_domain(legitimate_health_intervention__public_health_primary, "public_health/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__public_health_primary, 'd887660d-fa4d-4778-b84b-74dba2a05073').
narrative_ontology:cs_kernel_codification('d887660d-fa4d-4778-b84b-74dba2a05073', formalized).
narrative_ontology:cs_authority_grounding('d887660d-fa4d-4778-b84b-74dba2a05073', expertise).
narrative_ontology:cs_interpretation_layer_present('d887660d-fa4d-4778-b84b-74dba2a05073').
narrative_ontology:cs_reading_relation('d887660d-fa4d-4778-b84b-74dba2a05073', legitimate_health_intervention__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('d887660d-fa4d-4778-b84b-74dba2a05073', legitimate_health_intervention__proportionality_reading, influences).
narrative_ontology:cs_axiom('d887660d-fa4d-4778-b84b-74dba2a05073', foundational, collective_health_supersedes_individual_choice).
narrative_ontology:cs_axiom_status(collective_health_supersedes_individual_choice, holdable).
narrative_ontology:cs_axiom_grounding('d887660d-fa4d-4778-b84b-74dba2a05073', collective_health_supersedes_individual_choice, deontological).
narrative_ontology:cs_axiom('d887660d-fa4d-4778-b84b-74dba2a05073', foundational, measurable_morbidity_mortality_reduction_is_legitimacy_basis).
narrative_ontology:cs_axiom_status(measurable_morbidity_mortality_reduction_is_legitimacy_basis, holdable).
narrative_ontology:cs_axiom_grounding('d887660d-fa4d-4778-b84b-74dba2a05073', measurable_morbidity_mortality_reduction_is_legitimacy_basis, empirically_contingent).
narrative_ontology:cs_reference_frame('d887660d-fa4d-4778-b84b-74dba2a05073', epidemiological_imperative).
narrative_ontology:cs_drift_state('d887660d-fa4d-4778-b84b-74dba2a05073', contemporary_rights_discourse, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('d887660d-fa4d-4778-b84b-74dba2a05073', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__public_health_primary, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, immunocompromised_individuals).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, healthcare_systems).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, general_population).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, unvaccinated_individuals).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, religious_objectors).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, anti_vaccine_advocates).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Mandates and enforces public health interventions (e.g., vaccination requirements, mask mandates) based on epidemiological data to reduce population-level morbidity and mortality. Justifies actions by prioritizing collective health outcomes over individual preferences.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Directly benefit from reduced disease transmission due to widespread adherence to public health measures, as they are highly vulnerable to severe outcomes. Their health and safety depend on the compliance of others.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, immunocompromised_individuals, beneficiary,
    powerless, immediate, trapped, local).

% Benefit from reduced patient load and strain on resources during epidemics, allowing them to maintain quality of care for all patients. They advocate for measures that prevent system overload.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, healthcare_systems, beneficiary,
    institutional, biographical, constrained, national).

% Benefits from a healthier society, reduced risk of infection, and fewer disruptions to daily life (e.g., school closures, economic shutdowns). Most members comply with interventions for collective good.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, general_population, beneficiary,
    organized, biographical, mobile, national).

% Bear the costs of public health interventions through restrictions on employment, travel, or access to public spaces if they refuse vaccination. They are seen as imposing externalities on the collective.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, unvaccinated_individuals, payer,
    moderate, immediate, constrained, local).

% Face significant personal and social costs for refusing interventions based on deeply held religious beliefs. Their identity is often fused with their objection, making exit (compliance) a violation of self.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, religious_objectors, payer,
    powerless, biographical, identity_locked, local).

% Actively resist public health mandates, viewing them as infringements on personal liberty. They organize opposition and seek legal challenges, bearing the costs of fines or social exclusion.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, anti_vaccine_advocates, payer,
    organized, generational, constrained, national).

% Adjudicate legal challenges to public health mandates, balancing collective welfare against individual rights. Their rulings shape the boundaries of legitimate state intervention.
narrative_ontology:constraint_stakeholder(legitimate_health_intervention__public_health_primary, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective action to mitigate infectious disease spread and reduce population-level morbidity/mortality, ensuring healthcare system capacity and protecting vulnerable groups.
% TRANSFER_FUNCTION: Transfers individual autonomy and choice (e.g., regarding medical interventions, personal behavior) to public health authorities in exchange for collective health benefits and reduced societal risk.
% ABSENT_VOICES: Individuals who prioritize absolute bodily autonomy or who believe the state has no legitimate role in health decisions are often marginalized or excluded from the policy-making process, their concerns framed as selfish or misinformed.
% DISAPPEARANCE_RATIONALE: If this principle vanished, public health authorities would lose their primary justification for interventions. Compliance with health measures would plummet, leading to increased disease outbreaks, overwhelmed healthcare systems, and a breakdown of collective protection for vulnerable populations. Society would have to reorganize around a different framework for managing health crises.
% FOUNDING_PROBLEM: The historical problem of managing infectious diseases that pose a collective threat, where individual actions have population-level consequences (e.g., cholera outbreaks, smallpox epidemics).
% FOUNDING_PROBLEM_CORROBORATION: Epidemiological data, historical public health successes (e.g., polio eradication), and ongoing threats from emerging infectious diseases (e.g., COVID-19) corroborate that the problem of collective disease management is still live. International health organizations and medical professionals widely attest to this.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__public_health_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(legitimate_health_intervention__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(legitimate_health_intervention__public_health_primary, 0.7, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(legitimate_health_intervention__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(legitimate_health_intervention__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(legitimate_health_intervention__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.7) because individuals are compelled to accept interventions that may not directly benefit them or may carry perceived risks, for the benefit of the population. Suppression (0.65) is substantial, as non-compliance often leads to penalties, loss of employment, or restrictions on movement. Resistance is also high (0.75) due to strong individual rights advocacy and anti-mandate movements. The claimed type is Tangled Rope because it genuinely coordinates collective health outcomes but does so through asymmetric extraction from specific groups, requiring active enforcement to maintain.
 *
 * PERSPECTIVAL GAP:
 *   Public health authorities perceive this as a necessary and legitimate coordination mechanism, while those who bear the costs of intervention (e.g., unvaccinated individuals) experience it as coercive extraction. The engine's per-seat classification will reflect this divergence, showing a Rope-like experience for beneficiaries and a Snare-like experience for victims.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities, healthcare systems, and the general population are beneficiaries, gaining from reduced disease burden and system stability. Immunocompromised individuals are primary beneficiaries, as their survival depends on collective protection. Unvaccinated individuals, religious objectors, and anti-vaccine advocates are victims, bearing the costs of compliance or exclusion. Their directionality is high, reflecting the extraction of their autonomy and the imposition of penalties.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (reducing morbidity/mortality) remains live, preventing mislabeling as a Piton. However, the contestation over its status (founding_problem_status: contested) and high resistance indicate that the balance between coordination and extraction is under constant scrutiny. The classification as Tangled Rope accurately captures this ongoing tension, where the coordination function is real but the extractive component is significant and actively resisted.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_threshold_ambiguity,
    'At what threshold of population-level morbidity/mortality reduction does an intervention become ''legitimate'' under this reading, and how does this threshold compare to the ''proportionality_reading''?',
    'Expert consensus on epidemiological models and ethical frameworks, or judicial rulings that define acceptable risk-benefit ratios for collective interventions.',
    'A lower threshold would expand the scope of legitimate interventions, increasing extraction from individuals. A higher threshold would restrict interventions, potentially reducing collective benefits but increasing individual autonomy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_threshold_ambiguity, conceptual, 'Ambiguity in the quantitative threshold for legitimate public health intervention.').

omega_variable(
    externality_quantification_ambiguity,
    'How precisely can the ''externality imposition'' of individual refusal be quantified, and does this quantification justify the level of suppression applied?',
    'Rigorous epidemiological modeling of transmission dynamics and economic analysis of societal costs (e.g., healthcare burden, lost productivity) attributable to non-compliance.',
    'If externalities are small or poorly quantified, the suppression appears disproportionate, shifting the constraint towards a Snare. If large and well-quantified, it reinforces the Tangled Rope classification by justifying the coordination function''s costs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_quantification_ambiguity, empirical, 'Uncertainty in quantifying the negative externalities of individual refusal.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (e.g., legal penalties, employment termination) or internalized (e.g., social pressure, fear of ostracism)?',
    'Post-exit suppression trajectory: if suppression persists (e.g., self-censorship, continued social isolation) after formal mandates are removed, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making the constraint more resilient and potentially more extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in public health compliance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__public_health_primary, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_health_intervention__public_health_primary, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(legi_be_t5, legitimate_health_intervention__public_health_primary, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(legi_be_t10, legitimate_health_intervention__public_health_primary, base_extractiveness, 10, 0.7).
narrative_ontology:measurement(legi_be_t15, legitimate_health_intervention__public_health_primary, base_extractiveness, 15, 0.68).
narrative_ontology:measurement(legi_be_t20, legitimate_health_intervention__public_health_primary, base_extractiveness, 20, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_health_intervention__public_health_primary, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(legi_su_t5, legitimate_health_intervention__public_health_primary, suppression_requirement, 5, 0.6).
narrative_ontology:measurement(legi_su_t10, legitimate_health_intervention__public_health_primary, suppression_requirement, 10, 0.65).
narrative_ontology:measurement(legi_su_t15, legitimate_health_intervention__public_health_primary, suppression_requirement, 15, 0.63).
narrative_ontology:measurement(legi_su_t20, legitimate_health_intervention__public_health_primary, suppression_requirement, 20, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__public_health_primary, enforcement_mechanism).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, bodily_autonomy_primary).
narrative_ontology:affects_constraint(legitimate_health_intervention__public_health_primary, proportionality_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'legitimate_health_intervention' kernel. It emphasizes population-level health outcomes, influencing (and being influenced by) readings that prioritize individual autonomy or proportionality.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
