% ============================================================================
% CONSTRAINT STORY: public_health_mandate_authority__public_health_primary
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_health_mandate_authority__public_health_primary, []).

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
 *   constraint_id: public_health_mandate_authority__public_health_primary
 *   human_readable: Public Health Mandate (Public Health Primary Reading)
 *   domain: public_health/constitutional_rights/bioethics
 *
 * SUMMARY:
 *   This constraint represents the 'public health primary' reading of public
 *   health mandates, where the obligation to protect the vulnerable commons
 *   (immunocompromised individuals, healthcare infrastructure) via collective
 *   action is paramount. It frames mandates as necessary coordination
 *   mechanisms, with individual resistance viewed as an externality. This
 *   reading prioritizes collective welfare and system stability over
 *   individual autonomy when these values conflict. The immunocompromised
 *   enter the victim set when mandates fail, while the unvaccinated are
 *   excluded from the victim set, framed as free-riders imposing an
 *   externality. High extractiveness is observed on mandate-resistant
 *   individuals through coercion via employment or service loss.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_health_mandate_authority__public_health_primary, 0.75).
domain_priors:suppression_score(public_health_mandate_authority__public_health_primary, 0.8).
domain_priors:theater_ratio(public_health_mandate_authority__public_health_primary, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, extractiveness, 0.75).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(public_health_mandate_authority__public_health_primary, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_health_mandate_authority__public_health_primary, tangled_rope).
narrative_ontology:human_readable(public_health_mandate_authority__public_health_primary, "Public Health Mandate (Public Health Primary Reading)").
narrative_ontology:topic_domain(public_health_mandate_authority__public_health_primary, "public_health/constitutional_rights/bioethics").

domain_priors:requires_active_enforcement(public_health_mandate_authority__public_health_primary).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_health_mandate_authority__public_health_primary, '9d36a181-781f-4c9e-b708-3f288b276ad2').
narrative_ontology:cs_kernel_codification('9d36a181-781f-4c9e-b708-3f288b276ad2', formalized).
narrative_ontology:cs_authority_grounding('9d36a181-781f-4c9e-b708-3f288b276ad2', expertise).
narrative_ontology:cs_interpretation_layer_present('9d36a181-781f-4c9e-b708-3f288b276ad2').
narrative_ontology:cs_reading_relation('9d36a181-781f-4c9e-b708-3f288b276ad2', public_health_mandate_authority__bodily_autonomy_primary, coexists_with).
narrative_ontology:cs_reading_relation('9d36a181-781f-4c9e-b708-3f288b276ad2', public_health_mandate_authority__proportionality_reading, influences).
narrative_ontology:cs_axiom('9d36a181-781f-4c9e-b708-3f288b276ad2', foundational, collective_health_trumps_individual_autonomy_in_crisis).
narrative_ontology:cs_axiom_status(collective_health_trumps_individual_autonomy_in_crisis, holdable).
narrative_ontology:cs_axiom_grounding('9d36a181-781f-4c9e-b708-3f288b276ad2', collective_health_trumps_individual_autonomy_in_crisis, deontological).
narrative_ontology:cs_axiom('9d36a181-781f-4c9e-b708-3f288b276ad2', foundational, healthcare_infrastructure_is_a_vulnerable_commons).
narrative_ontology:cs_axiom_status(healthcare_infrastructure_is_a_vulnerable_commons, holdable).
narrative_ontology:cs_axiom_grounding('9d36a181-781f-4c9e-b708-3f288b276ad2', healthcare_infrastructure_is_a_vulnerable_commons, empirically_contingent).
narrative_ontology:cs_reference_frame('9d36a181-781f-4c9e-b708-3f288b276ad2', utilitarian_public_health_maximization).
narrative_ontology:cs_drift_state('9d36a181-781f-4c9e-b708-3f288b276ad2', contemporary_polarization_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('9d36a181-781f-4c9e-b708-3f288b276ad2', '').
narrative_ontology:cs_kernel_id(public_health_mandate_authority__public_health_primary, public_health_mandate_authority).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, immunocompromised_individuals).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, healthcare_system).
narrative_ontology:constraint_beneficiary(public_health_mandate_authority__public_health_primary, general_public).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, mandate_resistant_individuals).
narrative_ontology:constraint_victim(public_health_mandate_authority__public_health_primary, businesses_facing_mandate_costs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Responsible for implementing and enforcing public health mandates, justifying them as necessary to protect the collective good. They bear the political cost of enforcement but benefit from a healthier population and reduced strain on healthcare infrastructure.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, public_health_authorities, agenda_setter,
    institutional, generational, constrained, national).

% Directly benefit from reduced pathogen transmission, as mandates provide a layer of protection they cannot achieve through individual action. Their health and safety are directly tied to collective compliance.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, immunocompromised_individuals, beneficiary,
    powerless, immediate, trapped, local).

% Benefits from reduced patient load during outbreaks, preventing system collapse and allowing for normal operation. Mandates reduce the burden on staff and resources.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, healthcare_system, beneficiary,
    institutional, biographical, constrained, regional).

% Benefits from overall reduced disease prevalence, allowing for greater social and economic stability. They experience a collective good, but also bear some indirect costs of mandate enforcement.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, general_public, beneficiary,
    organized, biographical, mobile, national).

% Bear the direct costs of compliance (e.g., vaccination, masking) or face consequences for non-compliance (e.g., job loss, restricted access to services). They are framed as free-riders imposing externalities on the vulnerable.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, mandate_resistant_individuals, payer,
    moderate, immediate, constrained, local).

% Incur costs related to implementing and enforcing mandates (e.g., checking vaccination status, providing PPE). They face reduced patronage if mandates are too strict, or public backlash if too lax.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, businesses_facing_mandate_costs, payer,
    powerful, immediate, constrained, local).

% Argue that mandates infringe on individual freedoms and bodily autonomy, even if for a collective good. Their arguments are often sidelined in this reading, which prioritizes collective welfare.
narrative_ontology:constraint_stakeholder(public_health_mandate_authority__public_health_primary, civil_liberties_advocates, excluded,
    organized, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective action to reduce pathogen transmission and protect vulnerable populations and critical infrastructure (e.g., hospitals) from overload during public health crises.
% TRANSFER_FUNCTION: Transfers individual autonomy and choice (e.g., regarding medical interventions, movement) from mandate-resistant individuals to the collective good of public health and safety.
% ABSENT_VOICES: Those who prioritize individual bodily autonomy above collective health mandates are often excluded from the core decision-making process, their concerns framed as secondary to the public good. Their arguments are heard in legal challenges but not in the initial policy formulation under this reading.
% DISAPPEARANCE_RATIONALE: If public health mandates and their enforcement vanished overnight during a pandemic, pathogen transmission would likely increase, vulnerable populations would face higher risks, and healthcare systems could be overwhelmed, leading to significant societal disruption and reorganization around individual risk management.
% FOUNDING_PROBLEM: The problem of communicable diseases spreading rapidly through populations, overwhelming healthcare systems, and disproportionately harming vulnerable individuals, requiring collective action beyond individual choice.
% FOUNDING_PROBLEM_CORROBORATION: Public health organizations (WHO, CDC), medical professionals, and epidemiologists consistently corroborate that the problem of communicable disease spread and its impact on vulnerable populations remains a live and ongoing threat, requiring collective intervention. This is attested by scientific consensus and historical precedent, not just by the benefiting parties.
narrative_ontology:disappearance_verdict(public_health_mandate_authority__public_health_primary, world_rearranges).
narrative_ontology:founding_problem_status(public_health_mandate_authority__public_health_primary, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(public_health_mandate_authority__public_health_primary, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(public_health_mandate_authority__public_health_primary, 'none', 1).
narrative_ontology:epsilon_provenance(public_health_mandate_authority__public_health_primary, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_health_mandate_authority__public_health_primary_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(public_health_mandate_authority__public_health_primary, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(public_health_mandate_authority__public_health_primary_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.75) and suppression (0.8) reflect the coercive nature of mandates under this reading, where individual choices are overridden for collective benefit. The low theater ratio (0.1) indicates that the mandates are genuinely intended to achieve their stated public health goals, not merely for show. Accessibility collapse is moderate (0.6) as alternatives to compliance (e.g., avoiding public spaces, remote work) exist but are often highly constrained. Resistance is high (0.7) due to strong opposition from those prioritizing individual liberties.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of public health authorities and beneficiaries, the mandate is a necessary, effective, and legitimate coordination mechanism. From the perspective of mandate-resistant individuals, it is an extractive and suppressive imposition on their fundamental rights. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Public health authorities, the immunocompromised, the healthcare system, and the general public are beneficiaries, as they gain protection and stability. Mandate-resistant individuals and businesses facing mandate costs are victims, bearing the direct costs and restrictions. The directionality for mandate-resistant individuals is high (closer to 1.0) due to the direct imposition of costs and limited exit options, while for beneficiaries it is low (closer to 0.0).
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    mandate_efficacy_empirical_basis,
    'What is the empirical efficacy of specific mandates (e.g., mask mandates, vaccine passports) in reducing transmission and protecting vulnerable populations, independent of compliance rates?',
    'Rigorous, large-scale epidemiological studies and randomized controlled trials comparing outcomes in mandated vs. non-mandated populations, controlling for confounding factors.',
    'If efficacy is low, the justification for high extractiveness and suppression weakens, potentially reclassifying the constraint towards a Snare or Piton. If efficacy is high, it strengthens the Tangled Rope classification by validating the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_efficacy_empirical_basis, empirical, 'Uncertainty regarding the direct causal impact of mandates on public health outcomes.').

omega_variable(
    bodily_autonomy_vs_collective_good_framing,
    'Is the prioritization of collective public health over individual bodily autonomy a universally accepted ethical principle, or a context-dependent value judgment?',
    'Cross-cultural ethical analysis and philosophical debate on the limits of state power in health crises. This is a conceptual, not empirical, resolution.',
    'If it''s a context-dependent value judgment, the ''public_health_primary'' reading becomes one of several equally valid (though competing) ethical frameworks, potentially shifting its classification towards a more contested Tangled Rope or even Snare from alternative ethical seats. If universal, the ethical grounding of this reading is strengthened.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(bodily_autonomy_vs_collective_good_framing, conceptual, 'Ambiguity in the ethical grounding of prioritizing collective health over individual rights.').

omega_variable(
    mandate_resistant_identity_lock,
    'To what extent is resistance to public health mandates driven by deeply held identity (e.g., political, religious, personal freedom ideology) versus practical concerns or misinformation?',
    'Sociological and psychological studies on motivations for non-compliance, including qualitative interviews and longitudinal tracking of belief systems. Post-mandate behavior analysis.',
    'If resistance is primarily identity-locked, the ''constrained'' exit option for mandate-resistant individuals is understated; their effective directionality is higher, amplifying their perceived extraction. This would push the constraint closer to a Snare from their seat, as exit becomes psychologically or socially prohibitive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_resistant_identity_lock, empirical, 'Understanding the nature of resistance to mandates and its impact on perceived extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_health_mandate_authority__public_health_primary, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(publ_tr_t0, public_health_mandate_authority__public_health_primary, theater_ratio, 0, 0.1).
narrative_ontology:measurement(publ_tr_t5, public_health_mandate_authority__public_health_primary, theater_ratio, 5, 0.1).
narrative_ontology:measurement(publ_tr_t10, public_health_mandate_authority__public_health_primary, theater_ratio, 10, 0.1).

% Extraction over time
narrative_ontology:measurement(publ_be_t0, public_health_mandate_authority__public_health_primary, base_extractiveness, 0, 0.65).
narrative_ontology:measurement(publ_be_t5, public_health_mandate_authority__public_health_primary, base_extractiveness, 5, 0.72).
narrative_ontology:measurement(publ_be_t10, public_health_mandate_authority__public_health_primary, base_extractiveness, 10, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(publ_su_t0, public_health_mandate_authority__public_health_primary, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(publ_su_t5, public_health_mandate_authority__public_health_primary, suppression_requirement, 5, 0.75).
narrative_ontology:measurement(publ_su_t10, public_health_mandate_authority__public_health_primary, suppression_requirement, 10, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
