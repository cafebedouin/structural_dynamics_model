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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: legitimate_health_intervention__public_health_primary
 *   human_readable: Public Health Primary Intervention Mandate
 *   domain: public_health_policy/medical_ethics/constitutional_law
 *
 * SUMMARY:
 *   This constraint represents the 'public_health_primary' reading of
 *   legitimate health interventions, where the state's authority to mandate
 *   measures (like vaccination or isolation) derives from their measurable
 *   impact on population-level morbidity and mortality. Individual refusal is
 *   framed as an externality imposed on the collective, justifying coercive
 *   enforcement. This reading prioritizes collective well-being over
 *   individual autonomy when a clear public health threat exists.
 *
 * KEY AGENTS:
 *   - public_health_authorities: Agenda setter (institutional/analytical) — sets policy, enforces mandates.
 *   - immunocompromised_individuals: Beneficiary (powerless) — protected by reduced disease transmission.
 *   - general_population: Beneficiary (organized) — benefits from reduced disease burden, but also subject to mandates.
 *   - unvaccinated_individuals: Payer/Victim (powerless/constrained) — bears costs of mandates (e.g., job loss, access restrictions).
 *   - religious_objectors: Payer/Victim (powerless/constrained) — faces direct conflict between belief and mandate.
 *   - anti_vaccine_advocates: Payer/Victim (moderate/constrained) — actively resists, but faces significant suppression.
 *   - constitutional_courts: Observer (institutional/analytical) — adjudicates challenges to mandates, balancing rights.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(legitimate_health_intervention__public_health_primary, 0.78).
domain_priors:suppression_score(legitimate_health_intervention__public_health_primary, 0.85).
domain_priors:theater_ratio(legitimate_health_intervention__public_health_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, extractiveness, 0.78).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(legitimate_health_intervention__public_health_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(legitimate_health_intervention__public_health_primary, tangled_rope).
narrative_ontology:human_readable(legitimate_health_intervention__public_health_primary, "Public Health Primary Intervention Mandate").
narrative_ontology:topic_domain(legitimate_health_intervention__public_health_primary, "public_health_policy/medical_ethics/constitutional_law").

domain_priors:requires_active_enforcement(legitimate_health_intervention__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(legitimate_health_intervention__public_health_primary, '0cd3ac80-84a0-4a24-9c84-b51df70e0c25').
narrative_ontology:cs_kernel_codification('0cd3ac80-84a0-4a24-9c84-b51df70e0c25', formalized).
narrative_ontology:cs_authority_grounding('0cd3ac80-84a0-4a24-9c84-b51df70e0c25', expertise).
narrative_ontology:cs_interpretation_layer_present('0cd3ac80-84a0-4a24-9c84-b51df70e0c25').
narrative_ontology:cs_reading_relation('0cd3ac80-84a0-4a24-9c84-b51df70e0c25', legitimate_health_intervention__bodily_autonomy_primary, forecloses).
narrative_ontology:cs_reading_relation('0cd3ac80-84a0-4a24-9c84-b51df70e0c25', legitimate_health_intervention__proportionality_reading, influences).
narrative_ontology:cs_axiom('0cd3ac80-84a0-4a24-9c84-b51df70e0c25', foundational, collective_health_supremacy).
narrative_ontology:cs_axiom_status(collective_health_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('0cd3ac80-84a0-4a24-9c84-b51df70e0c25', collective_health_supremacy, deontological).
narrative_ontology:cs_axiom('0cd3ac80-84a0-4a24-9c84-b51df70e0c25', foundational, individual_externality_imposition).
narrative_ontology:cs_axiom_status(individual_externality_imposition, holdable).
narrative_ontology:cs_axiom_grounding('0cd3ac80-84a0-4a24-9c84-b51df70e0c25', individual_externality_imposition, empirically_contingent).
narrative_ontology:cs_reference_frame('0cd3ac80-84a0-4a24-9c84-b51df70e0c25', epidemiological_consensus_public_good).
narrative_ontology:cs_drift_state('0cd3ac80-84a0-4a24-9c84-b51df70e0c25', contemporary_pandemic_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('0cd3ac80-84a0-4a24-9c84-b51df70e0c25', '').
narrative_ontology:cs_kernel_id(legitimate_health_intervention__public_health_primary, legitimate_health_intervention).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, immunocompromised_individuals).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, healthcare_systems).
narrative_ontology:constraint_beneficiary(legitimate_health_intervention__public_health_primary, general_population).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, unvaccinated_individuals).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, religious_objectors).
narrative_ontology:constraint_victim(legitimate_health_intervention__public_health_primary, anti_vaccine_advocates).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__public_health_primary, herd_immunity_principle).
narrative_ontology:constraint_vindicates(legitimate_health_intervention__public_health_primary, public_good_over_private_interest).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To reduce population-level morbidity and mortality from infectious diseases, ensuring collective immunity and protecting vulnerable individuals who cannot be vaccinated.
% TRANSFER_FUNCTION: Transfers the burden of disease risk from the collective (especially vulnerable groups) to individuals who refuse public health interventions, through mandates and restrictions.
% ABSENT_VOICES: Individuals with rare medical contraindications to vaccines, or those whose specific religious beliefs are not widely understood, may feel their voices are absent in the broad policy debates, leading to their marginalization in policy formulation.
% DISAPPEARANCE_RATIONALE: If this constraint vanished overnight, public health authorities would lose their primary tool for managing epidemics. Vaccination rates would likely drop, leading to increased disease outbreaks, overwhelmed healthcare systems, and a significant rise in preventable deaths and long-term disabilities. Society would have to reorganize around a higher baseline of disease risk.
% FOUNDING_PROBLEM: The historical problem of widespread infectious diseases causing mass casualties and societal disruption, necessitating collective action to protect public health.
% FOUNDING_PROBLEM_CORROBORATION: Epidemiologists, public health organizations (e.g., WHO, CDC), and medical professionals universally corroborate that the founding problem of infectious disease threats remains live. Historical data on pandemics and ongoing endemic diseases provide strong evidence from outside the direct beneficiaries of enforcement.
narrative_ontology:disappearance_verdict(legitimate_health_intervention__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(legitimate_health_intervention__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(legitimate_health_intervention__public_health_primary, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(legitimate_health_intervention__public_health_primary, 'none', 1).

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
 *   The high extractiveness (0.78) and suppression (0.85) reflect the coercive nature of public health mandates, which impose significant costs on non-compliant individuals (e.g., loss of employment, restricted access to public spaces). The 'tangled_rope' classification is chosen because there is a genuine coordination function (reducing disease burden for the collective, especially vulnerable populations) alongside asymmetric extraction from those who refuse to comply. The low theater ratio (0.1) indicates that the enforcement is largely functional, directly aimed at achieving public health outcomes, rather than performative. Accessibility collapse is moderate (0.6) as alternatives to compliance (e.g., avoiding public spaces, remote work) exist but are severely constrained. Resistance is high (0.7) due to strong individual and ideological opposition.
 *
 * PERSPECTIVAL GAP:
 *   Public health authorities and immunocompromised individuals experience this as a necessary, life-saving coordination mechanism. Unvaccinated individuals and religious objectors experience it as a highly extractive and suppressive imposition on their bodily autonomy and beliefs. The engine's per-seat classification will reflect this divergence, with beneficiaries seeing a Rope-like function and victims experiencing a Snare-like extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities are clear beneficiaries (d=0.0-0.1) as their mandate is fulfilled and their power is exercised. Immunocompromised individuals and the general population are also beneficiaries (d=0.0-0.2) as they are protected from disease. Unvaccinated individuals, religious objectors, and anti-vaccine advocates are clear targets (d=0.8-1.0) as they bear the direct costs of compliance or non-compliance. Constitutional courts are analytical observers (d=0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not experiencing mandatrophy; its mandate (reducing morbidity/mortality) is actively pursued and its enforcement is functional. The challenge is not obsolescence but the ethical and legal contestation of its scope and methods. The classification as Tangled Rope prevents mislabeling it as a pure Snare by acknowledging its genuine coordination function, while still highlighting its extractive and suppressive aspects.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine reflection of public health necessity, or an overreach of state power?',
    'Empirical data on disease transmission rates and severity, combined with legal precedent on state police powers in public health emergencies.',
    'If public health necessity is overwhelmingly demonstrated, the constraint is a legitimate Tangled Rope. If overreach, it leans towards Snare.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identification, empirical, 'This constraint is the ''public_health_primary'' reading of the ''legitimate_health_intervention'' kernel. Sibling readings (''bodily_autonomy_primary'', ''proportionality_reading'') would shift the victim/beneficiary balance and the perceived legitimacy of enforcement.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (e.g., legal mandates) or internalized (e.g., social pressure, fear of exclusion)?',
    'Post-exit suppression trajectory: if refusal persists after legal mandates are removed, reclassify as partially internalized. Surveys on reasons for compliance/non-compliance.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests, as individuals carry the suppression with them. If purely structural, removal of mandates would lead to immediate behavioral shifts.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for vaccine mandates and access restrictions.').

omega_variable(
    proportionality_threshold_ambiguity,
    'At what threshold of population-level morbidity/mortality does individual refusal constitute an unacceptable externality, justifying high suppression?',
    'Consensus among epidemiologists and public health ethicists on risk assessment, and judicial review of public health orders.',
    'A higher threshold would reduce the legitimacy of high suppression, pushing the constraint towards Snare. A lower threshold would reinforce its Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_threshold_ambiguity, preference, 'The ''proportionality_reading'' sibling would contest this threshold, arguing for a more nuanced balance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(legitimate_health_intervention__public_health_primary, 0, 15).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(legi_tr_t0, legitimate_health_intervention__public_health_primary, theater_ratio, 0, 0.15).
narrative_ontology:measurement(legi_tr_t5, legitimate_health_intervention__public_health_primary, theater_ratio, 5, 0.12).
narrative_ontology:measurement(legi_tr_t10, legitimate_health_intervention__public_health_primary, theater_ratio, 10, 0.1).
narrative_ontology:measurement(legi_tr_t15, legitimate_health_intervention__public_health_primary, theater_ratio, 15, 0.1).

% Extraction over time
narrative_ontology:measurement(legi_be_t0, legitimate_health_intervention__public_health_primary, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(legi_be_t5, legitimate_health_intervention__public_health_primary, base_extractiveness, 5, 0.68).
narrative_ontology:measurement(legi_be_t10, legitimate_health_intervention__public_health_primary, base_extractiveness, 10, 0.75).
narrative_ontology:measurement(legi_be_t15, legitimate_health_intervention__public_health_primary, base_extractiveness, 15, 0.78).

% Suppression requirement over time
narrative_ontology:measurement(legi_su_t0, legitimate_health_intervention__public_health_primary, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(legi_su_t5, legitimate_health_intervention__public_health_primary, suppression_requirement, 5, 0.75).
narrative_ontology:measurement(legi_su_t10, legitimate_health_intervention__public_health_primary, suppression_requirement, 10, 0.8).
narrative_ontology:measurement(legi_su_t15, legitimate_health_intervention__public_health_primary, suppression_requirement, 15, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(legitimate_health_intervention__public_health_primary, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'legitimate_health_intervention' kernel. Other readings (e.g., 'bodily_autonomy_primary', 'proportionality_reading') are distinct constraints with different structural properties and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
