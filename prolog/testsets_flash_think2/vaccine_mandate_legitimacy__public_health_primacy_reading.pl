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
 *   constraint_id: vaccine_mandate_legitimacy__public_health_primacy_reading
 *   human_readable: Vaccine Mandate Legitimacy (Public Health Primacy Reading)
 *   domain: public_health_policy/constitutional_law/bioethics
 *
 * SUMMARY:
 *   This constraint story instantiates the 'public_health_primacy_reading' of
 *   the 'vaccine_mandate_legitimacy' kernel. It posits that the state's duty
 *   to prevent collective harm justifies mandate authority, treating
 *   unvaccinated status as an externality. This reading emphasizes the
 *   collective good over individual choice in public health emergencies,
 *   leading to the imposition of mandates and the suppression of
 *   non-compliance. The metrics reflect the substantial extraction and active
 *   enforcement required to maintain this arrangement.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.7).
domain_priors:suppression_score(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.8).
domain_priors:theater_ratio(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(vaccine_mandate_legitimacy__public_health_primacy_reading, resistance, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(vaccine_mandate_legitimacy__public_health_primacy_reading, tangled_rope).
narrative_ontology:human_readable(vaccine_mandate_legitimacy__public_health_primacy_reading, "Vaccine Mandate Legitimacy (Public Health Primacy Reading)").
narrative_ontology:topic_domain(vaccine_mandate_legitimacy__public_health_primacy_reading, "public_health_policy/constitutional_law/bioethics").

domain_priors:requires_active_enforcement(vaccine_mandate_legitimacy__public_health_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(vaccine_mandate_legitimacy__public_health_primacy_reading, 'c12177a0-fb7f-4dab-84f6-1cf9125d1e90').
narrative_ontology:cs_kernel_codification('c12177a0-fb7f-4dab-84f6-1cf9125d1e90', formalized).
narrative_ontology:cs_authority_grounding('c12177a0-fb7f-4dab-84f6-1cf9125d1e90', lineage).
narrative_ontology:cs_interpretation_layer_present('c12177a0-fb7f-4dab-84f6-1cf9125d1e90').
narrative_ontology:cs_reading_relation('c12177a0-fb7f-4dab-84f6-1cf9125d1e90', vaccine_mandate_legitimacy__bodily_autonomy_primacy_reading, forecloses).
narrative_ontology:cs_reading_relation('c12177a0-fb7f-4dab-84f6-1cf9125d1e90', vaccine_mandate_legitimacy__risk_stratification_reading, influences).
narrative_ontology:cs_axiom('c12177a0-fb7f-4dab-84f6-1cf9125d1e90', foundational, collective_good_primacy).
narrative_ontology:cs_axiom_status(collective_good_primacy, holdable).
narrative_ontology:cs_axiom_grounding('c12177a0-fb7f-4dab-84f6-1cf9125d1e90', collective_good_primacy, deontological).
narrative_ontology:cs_axiom('c12177a0-fb7f-4dab-84f6-1cf9125d1e90', foundational, unvaccinated_status_as_externality).
narrative_ontology:cs_axiom_status(unvaccinated_status_as_externality, holdable).
narrative_ontology:cs_axiom_grounding('c12177a0-fb7f-4dab-84f6-1cf9125d1e90', unvaccinated_status_as_externality, empirically_contingent).
narrative_ontology:cs_reference_frame('c12177a0-fb7f-4dab-84f6-1cf9125d1e90', state_police_power_doctrine).
narrative_ontology:cs_drift_state('c12177a0-fb7f-4dab-84f6-1cf9125d1e90', contemporary_pandemic_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('c12177a0-fb7f-4dab-84f6-1cf9125d1e90', '').
narrative_ontology:cs_kernel_id(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccine_mandate_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_authorities).
narrative_ontology:constraint_beneficiary(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccinated_public).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, unvaccinated_individuals).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, civil_liberties_advocates).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(vaccine_mandate_legitimacy__public_health_primacy_reading, employers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Sets and enforces vaccine mandates, justifying them as essential for collective health and safety. Gains authority and compliance from the population, enabling coordinated public health responses.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_authorities, agenda_setter,
    institutional, generational, arbitrage, national).

% Benefits from reduced disease transmission, protection of healthcare capacity, and a sense of collective security. Generally supports mandates as a necessary measure for public good.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, vaccinated_public, beneficiary,
    organized, biographical, mobile, national).

% Bears the direct costs of mandates, including loss of employment, restrictions on travel and public access, and social exclusion. Often resists mandates on grounds of individual liberty or medical concerns, but faces severe consequences for non-compliance.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, unvaccinated_individuals, payer,
    powerless, immediate, identity_locked, local).

% Opposes mandates on constitutional or human rights grounds, viewing them as an overreach of state power. Bears the costs of legal challenges and public advocacy, often aligning with unvaccinated individuals.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, civil_liberties_advocates, payer,
    organized, generational, constrained, national).

% Administers vaccines and implements mandate policies within healthcare settings. Supports mandates as a tool to protect patients and staff, and to maintain system functionality.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, healthcare_providers, agenda_setter,
    institutional, biographical, constrained, local).

% Implements and enforces vaccine mandates for their workforce, facing compliance costs, potential labor shortages, and legal challenges. Benefits from reduced workplace transmission and continuity of operations.
narrative_ontology:constraint_stakeholder(vaccine_mandate_legitimacy__public_health_primacy_reading, employers, payer,
    organized, biographical, constrained, local).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(vaccine_mandate_legitimacy__public_health_primacy_reading, public_health_authorities).
narrative_ontology:fixing_cost_class(vaccine_mandate_legitimacy__public_health_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate collective action to reduce disease transmission, protect vulnerable populations, and maintain the functional capacity of healthcare systems during a public health crisis.
% TRANSFER_FUNCTION: Transfers individual autonomy and choice regarding medical procedures from unvaccinated individuals to the state, in exchange for perceived collective safety and public health stability.
% ABSENT_VOICES: Those who advocate for absolute bodily autonomy or who believe that individual medical decisions should never be subject to state coercion are structurally marginalized in this reading, which prioritizes collective well-being.
% DISAPPEARANCE_RATIONALE: If the state's authority to mandate vaccines for collective harm prevention vanished overnight, public health responses to future pandemics would be fundamentally altered, potentially leading to higher disease burdens, greater strain on healthcare systems, and different social arrangements for collective risk management.
% FOUNDING_PROBLEM: Uncontrolled spread of infectious diseases, overwhelming healthcare systems, and significant morbidity and mortality, particularly during pandemics.
% FOUNDING_PROBLEM_CORROBORATION: Public health organizations (e.g., WHO, CDC), medical associations, and the broad epidemiological consensus corroborate the ongoing threat of infectious diseases and the efficacy of vaccines in mitigating these threats. This corroboration comes from outside the direct beneficiaries of mandate authority.
narrative_ontology:disappearance_verdict(vaccine_mandate_legitimacy__public_health_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(vaccine_mandate_legitimacy__public_health_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(vaccine_mandate_legitimacy__public_health_primacy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(vaccine_mandate_legitimacy__public_health_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(vaccine_mandate_legitimacy__public_health_primacy_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is high (0.7) because mandates impose significant costs on unvaccinated individuals (e.g., job loss, travel restrictions) for the benefit of the collective. Suppression is also high (0.8) due to the active enforcement mechanisms (legal, social, economic) required to ensure compliance and marginalize alternatives. The theater ratio is low (0.1) because the enforcement is genuinely aimed at achieving public health outcomes, not merely performative. Accessibility collapse is moderate-high (0.7) as mandates significantly restrict options for the unvaccinated. Resistance is high (0.75) reflecting the significant public and legal opposition to such mandates.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public health authorities and the vaccinated public, this constraint is a necessary and legitimate 'tangled rope' that coordinates collective action for public good. From the perspective of unvaccinated individuals and civil liberties advocates, it is a highly extractive 'snare' that unjustly curtails fundamental freedoms. The engine's computation of per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities are clear beneficiaries, gaining enhanced power and compliance. The vaccinated public also benefits from reduced disease risk. Unvaccinated individuals are the primary targets, bearing the direct costs and having their autonomy curtailed. Civil liberties advocates, while not directly subject to mandates, bear the costs of challenging them and defending individual rights. Employers, while benefiting from a healthier workforce, also bear compliance costs and face operational challenges.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    collective_harm_threshold_ambiguity,
    'What specific threshold of collective harm (e.g., R0 value, ICU occupancy) justifies the imposition of vaccine mandates, and is this threshold consistently applied?',
    'Establishment of clear, pre-defined epidemiological and healthcare capacity metrics that trigger and de-trigger mandate authority, with independent review of their application.',
    'If thresholds are arbitrary or inconsistently applied, the justification for mandates weakens, potentially reclassifying the constraint as more extractive. If clear and consistently met, it strengthens the coordination claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(collective_harm_threshold_ambiguity, empirical, 'Ambiguity regarding the empirical conditions that justify mandate authority.').

omega_variable(
    externality_definition_ambiguity,
    'Is unvaccinated status truly an externality that imposes measurable, unconsented costs on others, or is it primarily a private health choice with only diffuse, indirect collective impact?',
    'Rigorous epidemiological modeling and public health economics to quantify the direct and indirect costs imposed by unvaccinated individuals on the healthcare system and other individuals, distinguishing from general population health risks.',
    'If the externality is weak or unquantifiable, the justification for state coercion weakens, pushing the constraint towards a ''snare''. If strong and measurable, it reinforces the ''tangled rope'' classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(externality_definition_ambiguity, conceptual, 'Ambiguity in defining unvaccinated status as a public health externality.').

omega_variable(
    proportionality_assessment_ambiguity,
    'Is the scope and severity of vaccine mandates proportional to the collective harm they aim to prevent, considering less restrictive alternatives?',
    'Independent, multi-disciplinary review of mandate policies against a framework of proportionality, including analysis of alternative interventions (e.g., testing, masking) and their effectiveness.',
    'If mandates are found disproportionate, their legitimacy is undermined, increasing their perceived extractiveness. If proportional, the ''tangled rope'' classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(proportionality_assessment_ambiguity, conceptual, 'Ambiguity in assessing the proportionality of mandates to the public health threat.').

omega_variable(
    reading_identity,
    'This constraint is the ''public_health_primacy_reading'' of the ''vaccine_mandate_legitimacy'' kernel. What would change if a different reading were adopted?',
    'Analysis of legal and policy outcomes under alternative readings (e.g., ''bodily_autonomy_primacy_reading'' or ''risk_stratification_reading'').',
    'Adopting the ''bodily_autonomy_primacy_reading'' would likely classify mandates as a ''snare'' due to categorical impermissibility of coercion. The ''risk_stratification_reading'' would likely lead to a more nuanced ''scaffold'' or ''rope'' classification for targeted mandates, but a ''snare'' for blanket ones.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_identity, conceptual, 'This omega documents the kernel and reading identity, and the impact of alternative readings.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(vaccine_mandate_legitimacy__public_health_primacy_reading, 0, 3).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vacc_tr_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(vacc_tr_t1, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 1, 0.1).
narrative_ontology:measurement(vacc_tr_t2, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 2, 0.1).
narrative_ontology:measurement(vacc_tr_t3, vaccine_mandate_legitimacy__public_health_primacy_reading, theater_ratio, 3, 0.1).

% Extraction over time
narrative_ontology:measurement(vacc_be_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(vacc_be_t1, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 1, 0.65).
narrative_ontology:measurement(vacc_be_t2, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 2, 0.68).
narrative_ontology:measurement(vacc_be_t3, vaccine_mandate_legitimacy__public_health_primacy_reading, base_extractiveness, 3, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(vacc_su_t0, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(vacc_su_t1, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 1, 0.75).
narrative_ontology:measurement(vacc_su_t2, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 2, 0.78).
narrative_ontology:measurement(vacc_su_t3, vaccine_mandate_legitimacy__public_health_primacy_reading, suppression_requirement, 3, 0.8).


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
