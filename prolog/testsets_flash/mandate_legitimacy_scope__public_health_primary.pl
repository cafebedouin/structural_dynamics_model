% ============================================================================
% CONSTRAINT STORY: mandate_legitimacy_scope__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_mandate_legitimacy_scope__public_health_primary, []).

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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: mandate_legitimacy_scope__public_health_primary
 *   human_readable: State Authority to Compel Vaccination (Public Health Primary Reading)
 *   domain: public_health_ethics/constitutional_law/medical_autonomy
 *
 * SUMMARY:
 *   This constraint story models the 'public health primary' reading of state
 *   authority to compel vaccination. Under this reading, the state's power is
 *   legitimate when necessary to protect vulnerable populations from serious
 *   harm. The core tension is between individual autonomy and collective
 *   well-being, with this reading prioritizing the latter. The constraint is
 *   framed as a Tangled Rope because it genuinely coordinates public health
 *   outcomes but does so through asymmetric extraction from unvaccinated
 *   individuals.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(mandate_legitimacy_scope__public_health_primary, 0.65).
domain_priors:suppression_score(mandate_legitimacy_scope__public_health_primary, 0.7).
domain_priors:theater_ratio(mandate_legitimacy_scope__public_health_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, extractiveness, 0.65).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(mandate_legitimacy_scope__public_health_primary, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(mandate_legitimacy_scope__public_health_primary, tangled_rope).
narrative_ontology:human_readable(mandate_legitimacy_scope__public_health_primary, "State Authority to Compel Vaccination (Public Health Primary Reading)").
narrative_ontology:topic_domain(mandate_legitimacy_scope__public_health_primary, "public_health_ethics/constitutional_law/medical_autonomy").

domain_priors:requires_active_enforcement(mandate_legitimacy_scope__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(mandate_legitimacy_scope__public_health_primary, 'a74c007e-c9a4-44a4-845c-90dcb0089b50').
narrative_ontology:cs_kernel_codification('a74c007e-c9a4-44a4-845c-90dcb0089b50', formalized).
narrative_ontology:cs_authority_grounding('a74c007e-c9a4-44a4-845c-90dcb0089b50', lineage).
narrative_ontology:cs_interpretation_layer_present('a74c007e-c9a4-44a4-845c-90dcb0089b50').
narrative_ontology:cs_reading_relation('a74c007e-c9a4-44a4-845c-90dcb0089b50', mandate_legitimacy_scope__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('a74c007e-c9a4-44a4-845c-90dcb0089b50', mandate_legitimacy_scope__proportionality_reading, coexists_with).
narrative_ontology:cs_axiom('a74c007e-c9a4-44a4-845c-90dcb0089b50', foundational, collective_health_trumps_individual_autonomy_in_crisis).
narrative_ontology:cs_axiom_status(collective_health_trumps_individual_autonomy_in_crisis, holdable).
narrative_ontology:cs_axiom_grounding('a74c007e-c9a4-44a4-845c-90dcb0089b50', collective_health_trumps_individual_autonomy_in_crisis, deontological).
narrative_ontology:cs_axiom('a74c007e-c9a4-44a4-845c-90dcb0089b50', foundational, state_has_duty_to_protect_vulnerable_from_preventable_harm).
narrative_ontology:cs_axiom_status(state_has_duty_to_protect_vulnerable_from_preventable_harm, holdable).
narrative_ontology:cs_axiom_grounding('a74c007e-c9a4-44a4-845c-90dcb0089b50', state_has_duty_to_protect_vulnerable_from_preventable_harm, deontological).
narrative_ontology:cs_reference_frame('a74c007e-c9a4-44a4-845c-90dcb0089b50', police_power_for_public_good).
narrative_ontology:cs_drift_state('a74c007e-c9a4-44a4-845c-90dcb0089b50', contemporary_rights_discourse, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('a74c007e-c9a4-44a4-845c-90dcb0089b50', '').
narrative_ontology:cs_kernel_id(mandate_legitimacy_scope__public_health_primary, mandate_legitimacy_scope).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, vulnerable_populations).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, public_health_system).
narrative_ontology:constraint_victim(mandate_legitimacy_scope__public_health_primary, unvaccinated_individuals).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(mandate_legitimacy_scope__public_health_primary, healthcare_providers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These authorities interpret and enforce public health laws, including vaccine mandates. Their legitimacy is grounded in protecting the collective good, and they bear the responsibility for managing epidemics and health crises. They face political and legal challenges when implementing mandates.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, state_public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Individuals who cannot be vaccinated (e.g., immunocompromised, infants) or for whom vaccines are less effective. They rely on herd immunity provided by high vaccination rates in the general population for protection from serious harm. Without mandates, they face significantly higher risks.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, vulnerable_populations, beneficiary,
    powerless, immediate, trapped, local).

% Individuals who are compelled to be vaccinated against their will or face restrictions (e.g., employment, travel) for non-compliance. They bear the direct cost of the mandate (loss of autonomy, potential side effects) for the benefit of others. Their resistance is often high.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, unvaccinated_individuals, payer,
    moderate, biographical, constrained, local).

% Benefit from reduced disease burden and strain on healthcare systems due to mandates. They also administer vaccines and counsel patients, often acting as front-line enforcers of public health policy. They face ethical dilemmas balancing individual autonomy and public health.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, healthcare_providers, beneficiary,
    organized, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(mandate_legitimacy_scope__public_health_primary, healthcare_providers, agenda_setter).

% Adjudicate legal challenges to vaccine mandates, balancing state police powers against individual rights. Their rulings shape the boundaries of legitimate state authority in public health, often reflecting different readings of the underlying kernel.
narrative_ontology:constraint_stakeholder(mandate_legitimacy_scope__public_health_primary, constitutional_courts, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective action to achieve herd immunity, protecting the entire population, especially the most vulnerable, from infectious disease outbreaks. It ensures a baseline level of public health security that individual choices alone cannot guarantee.
% TRANSFER_FUNCTION: Transfers a duty to protect (via vaccination) from the state to unvaccinated individuals, and transfers a reduction in disease risk from vaccinated individuals to vulnerable populations. It also transfers a degree of individual bodily autonomy to the collective good.
% ABSENT_VOICES: Individuals who believe in absolute bodily autonomy, regardless of collective harm, are often excluded from the policy-making process, their concerns framed as individualistic rather than legitimate public health considerations. Their arguments are often heard in courts rather than legislative bodies.
% DISAPPEARANCE_RATIONALE: If state authority to compel vaccination vanished, vaccination rates would likely drop, leading to increased outbreaks of vaccine-preventable diseases. Vulnerable populations would face significantly higher risks, and healthcare systems would be strained. The social contract around collective health would fundamentally shift.
% FOUNDING_PROBLEM: The historical problem of widespread infectious diseases causing mass mortality and morbidity, particularly impacting vulnerable groups, which could not be controlled by individual action alone.
% FOUNDING_PROBLEM_CORROBORATION: Public health organizations (e.g., WHO, CDC), medical associations, and epidemiologists universally corroborate that the threat of infectious diseases remains live and that collective immunity is essential. This corroboration comes from outside the direct beneficiaries of mandates, grounded in scientific consensus.
narrative_ontology:disappearance_verdict(mandate_legitimacy_scope__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(mandate_legitimacy_scope__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(mandate_legitimacy_scope__public_health_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(mandate_legitimacy_scope__public_health_primary, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandate_legitimacy_scope__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(mandate_legitimacy_scope__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(mandate_legitimacy_scope__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is substantial because it compels a medical intervention, a significant imposition on individual liberty. Suppression (0.70) is high due to legal enforcement mechanisms and social pressure. Theater ratio (0.10) is low, as the public health function is generally genuine, though some enforcement may be performative. Accessibility collapse (0.40) is moderate; alternatives to vaccination (e.g., isolation, masking) exist but are less effective for collective protection. Resistance (0.75) is high, reflecting significant opposition to mandates.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of vulnerable populations and public health authorities, this constraint is a necessary coordination mechanism. From the perspective of unvaccinated individuals, it is a coercive imposition on their bodily autonomy. The engine's per-seat classification will reflect this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   State public health authorities and vulnerable populations are primary beneficiaries (d near 0.0), as they gain protection and legitimacy. Unvaccinated individuals are the primary targets (d near 1.0), bearing the direct costs of compelled vaccination. Healthcare providers are mixed, benefiting from reduced disease burden but also acting as agents of enforcement. Constitutional courts are observers, analyzing the constraint's legitimacy.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the constraint as a pure Snare by acknowledging the genuine coordination function (protecting vulnerable populations). However, it also prevents mislabeling as a pure Rope by recognizing the significant extraction from unvaccinated individuals. The 'contested' status of the founding problem highlights ongoing debate about whether the current level of threat justifies the degree of compulsion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    bodily_autonomy_vs_public_health,
    'What is the irreducible tension between individual bodily autonomy and the collective good in public health mandates?',
    'Conceptual clarification through ethical frameworks that define the limits of state power and individual rights in a shared social space. No empirical resolution.',
    'If bodily autonomy is deemed an absolute right, this constraint would be reclassified as a Snare; if public health is always paramount, it would lean towards a Rope. This omega highlights the preference-based nature of the kernel''s interpretation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bodily_autonomy_vs_public_health, conceptual, 'The fundamental ethical conflict at the heart of vaccine mandates.').

omega_variable(
    necessity_of_compulsion,
    'Is compulsion truly ''necessary'' to protect vulnerable populations, or could less restrictive alternatives (e.g., education, incentives) achieve similar public health outcomes?',
    'Empirical studies comparing public health outcomes (e.g., vaccination rates, disease incidence) in jurisdictions with and without mandates, controlling for other factors. Also, analysis of the efficacy of non-coercive interventions.',
    'If compulsion is found not to be strictly necessary, the ''extractiveness'' and ''suppression'' metrics would be re-evaluated downwards, potentially shifting the classification towards a Rope or even a Scaffold if temporary. If non-compulsory measures are insufficient, the Tangled Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(necessity_of_compulsion, empirical, 'Whether the coercive aspect of the mandate is truly indispensable for its public health goal.').

omega_variable(
    vulnerable_population_definition,
    'How broadly or narrowly should ''vulnerable populations'' be defined, and does this definition shift the perceived necessity and legitimacy of mandates?',
    'Consensus-building among medical ethicists, public health experts, and legal scholars on a standardized definition, or legislative action to codify specific criteria. This is a conceptual and preference-based question.',
    'A narrow definition might reduce the perceived scope and necessity of mandates, lowering extractiveness. A broad definition would reinforce the public health primary reading and potentially justify more extensive mandates.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(vulnerable_population_definition, conceptual, 'The scope of who is considered ''vulnerable'' and thus protected by mandates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(mandate_legitimacy_scope__public_health_primary, 1900, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mand_tr_t1900, mandate_legitimacy_scope__public_health_primary, theater_ratio, 1900, 0.05).
narrative_ontology:measurement(mand_tr_t1950, mandate_legitimacy_scope__public_health_primary, theater_ratio, 1950, 0.08).
narrative_ontology:measurement(mand_tr_t2000, mandate_legitimacy_scope__public_health_primary, theater_ratio, 2000, 0.09).
narrative_ontology:measurement(mand_tr_t2024, mandate_legitimacy_scope__public_health_primary, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(mand_be_t1900, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 1900, 0.5).
narrative_ontology:measurement(mand_be_t1950, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 1950, 0.55).
narrative_ontology:measurement(mand_be_t2000, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 2000, 0.6).
narrative_ontology:measurement(mand_be_t2024, mandate_legitimacy_scope__public_health_primary, base_extractiveness, 2024, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(mand_su_t1900, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 1900, 0.6).
narrative_ontology:measurement(mand_su_t1950, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 1950, 0.65).
narrative_ontology:measurement(mand_su_t2000, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 2000, 0.68).
narrative_ontology:measurement(mand_su_t2024, mandate_legitimacy_scope__public_health_primary, suppression_requirement, 2024, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(mandate_legitimacy_scope__public_health_primary, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'mandate_legitimacy_scope' kernel. Other readings include 'bodily_autonomy_primary' and 'proportionality_reading', which offer alternative justifications or limitations on state authority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
