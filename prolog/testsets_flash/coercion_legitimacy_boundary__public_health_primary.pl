% ============================================================================
% CONSTRAINT STORY: coercion_legitimacy_boundary__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coercion_legitimacy_boundary__public_health_primary, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: coercion_legitimacy_boundary__public_health_primary
 *   human_readable: Public Health Primary Coercion Legitimacy Boundary
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the reading of the coercion legitimacy
 *   boundary kernel where public health imperatives take precedence over
 *   individual autonomy. It allows the state to compel medical interventions
 *   (e.g., vaccination, quarantine) when a collective harm-prevention goal is
 *   at stake. This reading is often invoked during epidemics or for routine
 *   childhood vaccinations. The structural delta from other readings is that
 *   unvaccinated individuals become coerced subjects (victims), while
 *   immunocompromised individuals become protected beneficiaries, leading to
 *   a higher base extractiveness from the enforcement apparatus.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coercion_legitimacy_boundary__public_health_primary, 0.65).
domain_priors:suppression_score(coercion_legitimacy_boundary__public_health_primary, 0.75).
domain_priors:theater_ratio(coercion_legitimacy_boundary__public_health_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, extractiveness, 0.65).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(coercion_legitimacy_boundary__public_health_primary, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coercion_legitimacy_boundary__public_health_primary, tangled_rope).
narrative_ontology:human_readable(coercion_legitimacy_boundary__public_health_primary, "Public Health Primary Coercion Legitimacy Boundary").
narrative_ontology:topic_domain(coercion_legitimacy_boundary__public_health_primary, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(coercion_legitimacy_boundary__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(coercion_legitimacy_boundary__public_health_primary, 'f2ad2f98-6375-4f65-a411-b2f59468c2d9').
narrative_ontology:cs_kernel_codification('f2ad2f98-6375-4f65-a411-b2f59468c2d9', formalized).
narrative_ontology:cs_authority_grounding('f2ad2f98-6375-4f65-a411-b2f59468c2d9', lineage).
narrative_ontology:cs_interpretation_layer_present('f2ad2f98-6375-4f65-a411-b2f59468c2d9').
narrative_ontology:cs_reading_relation('f2ad2f98-6375-4f65-a411-b2f59468c2d9', coercion_legitimacy_boundary__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('f2ad2f98-6375-4f65-a411-b2f59468c2d9', coercion_legitimacy_boundary__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('f2ad2f98-6375-4f65-a411-b2f59468c2d9', foundational, collective_harm_prevention_trumps_individual_autonomy).
narrative_ontology:cs_axiom_status(collective_harm_prevention_trumps_individual_autonomy, holdable).
narrative_ontology:cs_axiom_grounding('f2ad2f98-6375-4f65-a411-b2f59468c2d9', collective_harm_prevention_trumps_individual_autonomy, deontological).
narrative_ontology:cs_axiom('f2ad2f98-6375-4f65-a411-b2f59468c2d9', secondary, state_has_duty_to_protect_public_health).
narrative_ontology:cs_axiom_status(state_has_duty_to_protect_public_health, holdable).
narrative_ontology:cs_axiom_grounding('f2ad2f98-6375-4f65-a411-b2f59468c2d9', state_has_duty_to_protect_public_health, deontological).
narrative_ontology:cs_reference_frame('f2ad2f98-6375-4f65-a411-b2f59468c2d9', historical_public_health_mandates).
narrative_ontology:cs_drift_state('f2ad2f98-6375-4f65-a411-b2f59468c2d9', contemporary_anti_mandate_movements, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('f2ad2f98-6375-4f65-a411-b2f59468c2d9', '').
narrative_ontology:cs_kernel_id(coercion_legitimacy_boundary__public_health_primary, coercion_legitimacy_boundary).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, public_health_authorities).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, immunocompromised_individuals).
narrative_ontology:constraint_beneficiary(coercion_legitimacy_boundary__public_health_primary, general_public).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, unvaccinated_individuals).
narrative_ontology:constraint_victim(coercion_legitimacy_boundary__public_health_primary, religious_objectors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Tasked with protecting population health, they interpret and enforce policies that may include mandatory vaccination or treatment. They benefit from the ability to implement broad interventions to prevent collective harm, but are constrained by legal challenges and public resistance.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Cannot safely receive certain medical interventions and rely on herd immunity for protection. They are direct beneficiaries of policies that compel others to vaccinate, as it reduces their risk of exposure to preventable diseases.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, immunocompromised_individuals, beneficiary,
    powerless, biographical, trapped, local).

% Benefits from reduced disease transmission and the maintenance of public health infrastructure. They generally support measures that protect collective well-being, but may experience indirect costs or minor inconveniences from mandates.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, general_public, beneficiary,
    organized, biographical, mobile, national).

% Are compelled to undergo medical interventions against their will or face social/economic penalties (e.g., exclusion from public spaces, employment restrictions). They bear the direct costs of coerced autonomy and often resist such mandates.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, unvaccinated_individuals, payer,
    moderate, immediate, constrained, local).

% Face a direct conflict between their religious beliefs and state-mandated medical interventions. Their identity is often fused with their objection, making exit (compliance) a profound personal cost. They bear the extraction of compelled action or social exclusion.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, religious_objectors, payer,
    powerless, generational, identity_locked, national).

% Monitor and challenge policies that infringe on individual autonomy, even when justified by public health. They analyze the legal and ethical boundaries of state power and advocate for less coercive alternatives.
narrative_ontology:constraint_stakeholder(coercion_legitimacy_boundary__public_health_primary, civil_liberties_advocates, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective action to prevent the spread of infectious diseases, ensuring a minimum level of population immunity and protecting vulnerable groups who cannot be vaccinated.
% TRANSFER_FUNCTION: Transfers individual autonomy (the right to refuse medical intervention) from individuals to the state, in exchange for collective health security. The cost is borne by those compelled; the benefit accrues to the population.
% ABSENT_VOICES: Individuals who prioritize absolute bodily autonomy above all collective considerations are often marginalized in public health discourse, their concerns framed as selfish or misinformed. They would argue for non-coercive public health strategies.
% DISAPPEARANCE_RATIONALE: If the state's power to compel medical intervention for public health vanished, there would be a significant increase in vaccine-preventable diseases, particularly impacting vulnerable populations. Public health strategies would need to fundamentally reorganize around voluntary compliance, potentially leading to greater social fragmentation and health disparities.
% FOUNDING_PROBLEM: The historical challenge of controlling infectious diseases that pose a threat to entire populations, where individual choices can have widespread negative externalities.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem remains live, as evidenced by ongoing outbreaks of preventable diseases and the emergence of new pathogens. Medical professionals, epidemiologists, and international health organizations corroborate the necessity of collective action for disease control, independent of political or individual belief systems.
narrative_ontology:disappearance_verdict(coercion_legitimacy_boundary__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(coercion_legitimacy_boundary__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(coercion_legitimacy_boundary__public_health_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(coercion_legitimacy_boundary__public_health_primary, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coercion_legitimacy_boundary__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(coercion_legitimacy_boundary__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(coercion_legitimacy_boundary__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely solves a coordination problem (preventing disease spread) but does so with significant asymmetric extraction (compelling individuals) and requires active enforcement. Extractiveness (0.65) is high due to the direct infringement on bodily autonomy. Suppression (0.75) is also high, reflecting the state's coercive power and the limited exit options for those who refuse. Theater ratio (0.1) is low, as the enforcement is generally direct and functional, not performative, though public health messaging can sometimes be seen as theatrical in its framing of risk.
 *
 * PERSPECTIVAL GAP:
 *   Public health authorities and immunocompromised individuals would experience this as a necessary Rope, ensuring collective safety. Unvaccinated individuals and religious objectors would experience it as a Snare, directly extracting their autonomy. The general public would likely see it as a beneficial, if sometimes inconvenient, Rope. The engine's per-seat classification will reflect these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities and the general public are beneficiaries (d near 0.0-0.2) as they gain collective protection and the ability to manage public health crises. Immunocompromised individuals are strong beneficiaries (d near 0.0) as their survival often depends on this constraint. Unvaccinated individuals and religious objectors are targets (d near 0.8-1.0) as their autonomy is directly extracted, and their exit options are severely constrained or identity-locked. Civil liberties advocates are observers (d near 0.5) analyzing the balance.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint's mandate (preventing collective harm) is still live, especially with ongoing infectious disease threats. The classification as Tangled Rope, rather than Snare, acknowledges the genuine coordination function, preventing mislabeling it as pure extraction. However, the high extractiveness and suppression indicate a need for careful scrutiny to ensure the 'public health primary' axiom is not over-applied beyond genuine collective threats, which would shift it towards a Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    public_health_vs_autonomy_balance,
    'At what point does the collective harm-prevention benefit genuinely outweigh individual autonomy, and how is this threshold determined?',
    'Empirical epidemiological data on disease transmission and severity, combined with ethical frameworks for balancing rights, and judicial review of specific mandates.',
    'If the threshold is set too low, the constraint operates as a Snare, over-extracting autonomy. If too high, it fails its coordination function, leading to preventable collective harm. Resolution would clarify the legitimate scope of coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(public_health_vs_autonomy_balance, conceptual, 'The precise balance point between public health and individual autonomy.').

omega_variable(
    coercion_necessity_alternatives,
    'Are there less coercive, equally effective alternatives to achieve the same public health outcomes (e.g., education, incentives, voluntary measures)?',
    'Comparative studies of public health interventions in different jurisdictions, evaluating the efficacy of non-coercive strategies against coercive ones for similar threats.',
    'If effective non-coercive alternatives exist, the ''requires_active_enforcement'' and high ''suppression'' become unjustified, pushing the constraint towards a Snare. If not, the Tangled Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coercion_necessity_alternatives, empirical, 'Availability and efficacy of non-coercive public health alternatives.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine ''public_health_primary'' reading of the coercion legitimacy boundary, or is it a ''proportionality_reading'' that has been over-applied?',
    'Analysis of the specific disease context (severity, transmissibility) and the scope of the mandate. If mandates are applied broadly to low-risk scenarios, it suggests an over-application of the ''public_health_primary'' axiom.',
    'If it''s an over-applied proportionality reading, the extractiveness is higher than justified, and the constraint should be re-evaluated under the proportionality framework, likely shifting its classification towards a Snare in those contexts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Distinguishing between a true ''public_health_primary'' reading and an over-applied ''proportionality_reading''.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coercion_legitimacy_boundary__public_health_primary, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coer_tr_t1900, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(coer_tr_t1950, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(coer_tr_t2000, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(coer_tr_t2010, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 2010, 0.1).
narrative_ontology:measurement(coer_tr_t2020, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 2020, 0.15).
narrative_ontology:measurement(coer_tr_t2024, coercion_legitimacy_boundary__public_health_primary, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(coer_be_t1900, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 1900, 0.5).
narrative_ontology:measurement(coer_be_t1950, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 1950, 0.55).
narrative_ontology:measurement(coer_be_t2000, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(coer_be_t2010, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 2010, 0.62).
narrative_ontology:measurement(coer_be_t2020, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 2020, 0.68).
narrative_ontology:measurement(coer_be_t2024, coercion_legitimacy_boundary__public_health_primary, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(coer_su_t1900, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 1900, 0.6).
narrative_ontology:measurement(coer_su_t1950, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 1950, 0.65).
narrative_ontology:measurement(coer_su_t2000, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 2000, 0.7).
narrative_ontology:measurement(coer_su_t2010, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 2010, 0.72).
narrative_ontology:measurement(coer_su_t2020, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 2020, 0.8).
narrative_ontology:measurement(coer_su_t2024, coercion_legitimacy_boundary__public_health_primary, suppression_requirement, 2024, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coercion_legitimacy_boundary__public_health_primary, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'coercion_legitimacy_boundary' kernel, specifically the 'public_health_primary' reading. Other readings, such as 'bodily_autonomy_primary' and 'proportionality_reading', represent distinct constraints with different structural properties and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
