% ============================================================================
% CONSTRAINT STORY: vaccine_mandate_legitimacy__public_health_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_vaccine_mandate_legitimacy__public_health_primacy_reading, []).

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
 *   constraint_id: vaccine_mandate_legitimacy__public_health_primacy_reading
 *   human_readable: Vaccine Mandate Legitimacy (Public Health Primacy Reading)
 *   domain: public_health/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'public_health_primacy_reading' of
 *   the 'vaccine_mandate_legitimacy' kernel. It asserts that the state's duty
 *   to prevent collective harm justifies mandatory vaccination, framing
 *   unvaccinated status as an externality. This reading prioritizes
 *   collective welfare over individual autonomy in public health crises. The
 *   metrics reflect the substantial extraction from unvaccinated individuals
 *   and the high suppression required to enforce compliance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.65).
domain_priors:suppression_score(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.75).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__public_health_primacy_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__public_health_primacy_reading, "Vaccine Mandate Legitimacy (Public Health Primacy Reading)").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__public_health_primacy_reading, "public_health/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__public_health_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__public_health_primacy_reading, '23dd5034-bd52-44c9-8974-38ba4b4461e7').
narrative_ontology:cs_kernel_codification('23dd5034-bd52-44c9-8974-38ba4b4461e7', formalized).
narrative_ontology:cs_authority_grounding('23dd5034-bd52-44c9-8974-38ba4b4461e7', lineage).
narrative_ontology:cs_interpretation_layer_present('23dd5034-bd52-44c9-8974-38ba4b4461e7').
narrative_ontology:cs_reading_relation('23dd5034-bd52-44c9-8974-38ba4b4461e7', vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('23dd5034-bd52-44c9-8974-38ba4b4461e7', vaccine_mandate_legitimacy__risk_stratification_reading, influences).
narrative_ontology:cs_axiom('23dd5034-bd52-44c9-8974-38ba4b4461e7', foundational, collective_welfare_trumps_individual_liberty_in_crisis).
narrative_ontology:cs_axiom_status(collective_welfare_trumps_individual_liberty_in_crisis, holdable).
narrative_ontology:cs_axiom_grounding('23dd5034-bd52-44c9-8974-38ba4b4461e7', collective_welfare_trumps_individual_liberty_in_crisis, deontological).
narrative_ontology:cs_axiom('23dd5034-bd52-44c9-8974-38ba4b4461e7', foundational, unvaccinated_status_is_a_public_health_externality).
narrative_ontology:cs_axiom_status(unvaccinated_status_is_a_public_health_externality, holdable).
narrative_ontology:cs_axiom_grounding('23dd5034-bd52-44c9-8974-38ba4b4461e7', unvaccinated_status_is_a_public_health_externality, empirically_contingent).
narrative_ontology:cs_reference_frame('23dd5034-bd52-44c9-8974-38ba4b4461e7', state_police_power_doctrine).
narrative_ontology:cs_drift_state('23dd5034-bd52-44c9-8974-38ba4b4461e7', contemporary_pandemic_response, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('23dd5034-bd52-44c9-8974-38ba4b4461e7', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccinated_public).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, unvaccinated_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for protecting population health, they assert the state's duty to prevent collective harm justifies mandates. They benefit from enhanced authority and compliance, enabling broader public health interventions. Exit options are constrained by their institutional mandate and public expectations.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from reduced disease transmission and a sense of collective safety, which is enhanced by mandates. They bear minimal direct costs from the mandate itself, primarily supporting the public health framework. Their exit options are generally unconstrained by this specific mandate.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccinated_public, beneficiary,
    organized, biographical, mobile, national).

% Bear the direct costs of mandates, including restrictions on employment, travel, and public access. Their unvaccinated status is framed as an externality. Exit options are identity-locked for those whose refusal is tied to deeply held beliefs, making compliance a high personal cost.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, unvaccinated_individuals, payer,
    powerless, immediate, identity_locked, local).

% Observe and challenge mandates on grounds of individual rights and bodily autonomy. They analyze the proportionality of state power and its impact on minority groups, but do not directly benefit or pay from the mandate's operation.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, civil_liberties_advocates, observer,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective action to achieve herd immunity and reduce disease transmission, ensuring public health outcomes that individual choices alone might not achieve.
% TRANSFER_FUNCTION: Transfers individual liberty (the choice to remain unvaccinated) to the state, in exchange for collective health benefits and reduced societal risk. The cost of non-compliance is borne by unvaccinated individuals.
% ABSENT_VOICES: Individuals who prioritize absolute bodily autonomy or who are medically unable to be vaccinated are often marginalized in the public health discourse, their concerns framed as secondary to collective welfare.
% DISAPPEARANCE_RATIONALE: If the state's authority to mandate vaccines vanished, public health strategies would need fundamental re-evaluation, potentially leading to increased disease burden, economic disruption from outbreaks, and a shift towards voluntary compliance models with uncertain outcomes. The social contract around collective health would be significantly altered.
% FOUNDING_PROBLEM: The problem of preventing widespread infectious disease outbreaks and ensuring population-level immunity, especially when individual choices create collective risk.
% FOUNDING_PROBLEM_CORROBORATION: Public health organizations, medical professionals, and international health bodies consistently corroborate the ongoing need for mechanisms to prevent infectious disease, citing historical and contemporary outbreaks. This corroboration comes from outside the direct beneficiaries of mandate authority.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__public_health_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__public_health_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__public_health_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__public_health_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(vaccine_mandate_legitimacy__public_health_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(vaccine_mandate_legitimacy__public_health_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(vaccine_mandate_legitimacy__public_health_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.65) because mandates impose significant costs on unvaccinated individuals, restricting their participation in society. Suppression is also high (0.75) due to the active enforcement mechanisms (e.g., vaccine passports, employment requirements) needed to ensure compliance against resistance. The theater ratio is low (0.10) as the mandate's function is direct and actively pursued, not performative. Accessibility collapse is moderate (0.60) as alternatives to vaccination (e.g., frequent testing) are often less convenient or more costly, and resistance is high (0.70) due to strong opposition from those prioritizing individual liberty.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public health authorities and the vaccinated public, the mandate is a necessary and legitimate coordination mechanism. From the perspective of unvaccinated individuals, it is a coercive extraction that infringes on fundamental rights. The engine's per-seat classification will reflect this divergence, with beneficiaries experiencing a 'rope-like' function and victims experiencing a 'snare-like' function.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities are the agenda-setters and primary beneficiaries, gaining enhanced power and achieving their public health goals. The vaccinated public are also beneficiaries, experiencing reduced risk. Unvaccinated individuals are the primary victims/payers, bearing the costs of compliance or exclusion. Civil liberties advocates act as observers, analyzing the constraint's impact without direct benefit or cost from its operation.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not experiencing mandatrophy; its mandate (preventing collective harm) is considered live and actively pursued by its beneficiaries. The contest is over the legitimacy of the mandate's justification and its proportionality, not its obsolescence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    proportionality_of_harm,
    'Is the collective harm prevented by mandates sufficiently severe and directly attributable to unvaccinated status to justify the imposed individual costs?',
    'Epidemiological data on transmission rates, severity of illness, and healthcare system burden directly linked to unvaccinated populations, compared against the social and economic costs of mandates.',
    'If the collective harm is found to be disproportionately low relative to individual costs, the mandate''s justification weakens, potentially shifting its classification towards a Snare. If high, the Tangled Rope classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_of_harm, empirical, 'Assesses the proportionality between collective benefit and individual cost.').

omega_variable(
    alternative_coordination_mechanisms,
    'Are there less coercive, equally effective alternative mechanisms to achieve the same public health outcomes (e.g., enhanced education, voluntary incentives, targeted interventions)?',
    'Comparative studies of public health outcomes in jurisdictions employing different strategies (mandates vs. voluntary approaches) under similar epidemiological conditions.',
    'If effective, less coercive alternatives exist, the ''coordination'' aspect of the Tangled Rope diminishes, increasing its ''extraction'' component and pushing it closer to a Snare. If no viable alternatives, the Tangled Rope classification is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(alternative_coordination_mechanisms, empirical, 'Examines the necessity of coercion for public health coordination.').

omega_variable(
    framing_of_externality,
    'Is the ''unvaccinated status as externality'' framing a neutral description of risk, or a rhetorical device to justify coercion?',
    'Analysis of public discourse, policy documents, and scientific communication for framing effects, and comparison with how other public health risks are communicated and managed.',
    'If primarily a rhetorical device, the suppression and extractiveness metrics are more clearly indicative of a Snare, as the coordination narrative serves as cover. If a neutral description, the Tangled Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(framing_of_externality, conceptual, 'Examines the conceptual framing of unvaccinated status.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__public_health_primacy_reading, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(vacc_be_t5, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(vacc_be_t10, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 10, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(vacc_su_t5, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 5, 0.68).
narrative_ontology:measurement(vacc_su_t10, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 10, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(vaccine_mandate_legitimacy__public_health_primacy_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
