% ============================================================================
% CONSTRAINT STORY: ietf_openness_commitment__legitimacy_erosion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ietf_openness_commitment__legitimacy_erosion_reading, []).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
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
 *   constraint_id: ietf_openness_commitment__legitimacy_erosion_reading
 *   human_readable: IETF Rough Consensus: Legitimacy Erosion Reading
 *   domain: technology_governance/internet_standards/institutional_economics
 *
 * SUMMARY:
 *   This constraint describes the 'rough consensus' mechanism within the
 *   IETF, viewed through the lens of legitimacy erosion. While ostensibly a
 *   coordination mechanism for open standards, this reading highlights how
 *   the process itself becomes a target for capture by well-resourced
 *   factions. These factions extract procedural legitimacy to ratify
 *   self-serving outcomes, leading to a decline in the credibility of the
 *   consensus mechanism and marginalization of independent voices. The
 *   constraint is claimed as a Tangled Rope, reflecting its dual nature as
 *   both a coordination function and a site of asymmetric extraction.
 *
 * KEY AGENTS:
 *   - ietf_working_group_chairs: Agenda setter (institutional/constrained)
 *   - well_resourced_factions: Beneficiary (organized/arbitrage)
 *   - independent_participants: Payer (moderate/constrained)
 *   - consensus_mechanism_credibility: Victim (powerless/trapped)
 *   - future_internet_users: Victim (powerless/trapped)
 *   - ietf_leadership: Observer (institutional/constrained)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ietf_openness_commitment__legitimacy_erosion_reading, 0.7).
domain_priors:suppression_score(ietf_openness_commitment__legitimacy_erosion_reading, 0.6).
domain_priors:theater_ratio(ietf_openness_commitment__legitimacy_erosion_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(ietf_openness_commitment__legitimacy_erosion_reading, resistance, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ietf_openness_commitment__legitimacy_erosion_reading, tangled_rope).
narrative_ontology:human_readable(ietf_openness_commitment__legitimacy_erosion_reading, "IETF Rough Consensus: Legitimacy Erosion Reading").
narrative_ontology:topic_domain(ietf_openness_commitment__legitimacy_erosion_reading, "technology_governance/internet_standards/institutional_economics").

domain_priors:requires_active_enforcement(ietf_openness_commitment__legitimacy_erosion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(ietf_openness_commitment__legitimacy_erosion_reading, '691e6f9d-a8a9-4cbe-a914-172d6b978dca').
narrative_ontology:cs_kernel_codification('691e6f9d-a8a9-4cbe-a914-172d6b978dca', formalized).
narrative_ontology:cs_authority_grounding('691e6f9d-a8a9-4cbe-a914-172d6b978dca', practice).
narrative_ontology:cs_interpretation_layer_present('691e6f9d-a8a9-4cbe-a914-172d6b978dca').
narrative_ontology:cs_reading_relation('691e6f9d-a8a9-4cbe-a914-172d6b978dca', ietf_openness_commitment__commons_stewardship_reading, influences).
narrative_ontology:cs_reading_relation('691e6f9d-a8a9-4cbe-a914-172d6b978dca', ietf_openness_commitment__capture_substrate_reading, coexists_with).
narrative_ontology:cs_axiom('691e6f9d-a8a9-4cbe-a914-172d6b978dca', foundational, consensus_legitimacy_is_fragile).
narrative_ontology:cs_axiom_status(consensus_legitimacy_is_fragile, holdable).
narrative_ontology:cs_axiom_grounding('691e6f9d-a8a9-4cbe-a914-172d6b978dca', consensus_legitimacy_is_fragile, empirically_contingent).
narrative_ontology:cs_axiom('691e6f9d-a8a9-4cbe-a914-172d6b978dca', secondary, procedural_fairness_is_extractable).
narrative_ontology:cs_axiom_status(procedural_fairness_is_extractable, holdable).
narrative_ontology:cs_axiom_grounding('691e6f9d-a8a9-4cbe-a914-172d6b978dca', procedural_fairness_is_extractable, empirically_contingent).
narrative_ontology:cs_reference_frame('691e6f9d-a8a9-4cbe-a914-172d6b978dca', ideal_rough_consensus_process).
narrative_ontology:cs_drift_state('691e6f9d-a8a9-4cbe-a914-172d6b978dca', contemporary_internet_governance_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('691e6f9d-a8a9-4cbe-a914-172d6b978dca', '').
narrative_ontology:cs_kernel_id(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__legitimacy_erosion_reading, well_resourced_factions).
narrative_ontology:constraint_beneficiary(ietf_openness_commitment__legitimacy_erosion_reading, agenda_setting_chairs).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, consensus_mechanism_credibility).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, independent_participants).
narrative_ontology:constraint_victim(ietf_openness_commitment__legitimacy_erosion_reading, future_internet_users).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(ietf_openness_commitment__legitimacy_erosion_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(ietf_openness_commitment__legitimacy_erosion_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ietf_openness_commitment__legitimacy_erosion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(ietf_openness_commitment__legitimacy_erosion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ietf_openness_commitment__legitimacy_erosion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.7) because the process is effectively co-opted to serve specific interests, leading to standards that benefit a few at the expense of the many. Suppression (0.6) is significant, not through overt coercion, but through the structural disadvantage faced by independent participants against well-resourced factions (e.g., 'loudest voice' dynamics, resource-intensive participation). Theater ratio (0.4) is moderate, as the formal procedures of consensus are maintained, but their underlying function of genuinely open, bottom-up decision-making is compromised. The temporal measurements show a clear trend of increasing extractiveness and suppression over time, reflecting the gradual erosion of legitimacy.
 *
 * PERSPECTIVAL GAP:
 *   Well-resourced factions perceive the mechanism as a legitimate, if competitive, means to achieve consensus, benefiting from its coordination function. Independent participants and the 'credibility' itself experience it as an extractive process where their input is marginalized, leading to a sense of betrayal of the founding principles. IETF leadership observes this divergence and its impact on the institution's long-term health.
 *
 * DIRECTIONALITY LOGIC:
 *   Well-resourced factions are clear beneficiaries (d=0.0-0.2) as they effectively 'buy' legitimacy for their proposals. IETF working group chairs are agenda setters, benefiting from the functioning of the process but also bearing the cost of maintaining its integrity under pressure (d=0.3-0.5). Independent participants and future internet users are victims (d=0.8-1.0) as they bear the costs of biased standards and the erosion of a public good. The 'consensus_mechanism_credibility' is a direct victim, as its value is extracted and diminished.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading suggests a form of mandatrophy where the original mandate of open, decentralized standards development has been subtly subverted. The mechanism persists, but its function has drifted from pure coordination to a hybrid of coordination and extraction, where the 'coordination' serves as cover for the 'extraction' of legitimacy. The classification as Tangled Rope prevents mislabeling it as a pure Rope (ignoring extraction) or a pure Snare (ignoring its residual coordination function).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    capture_mechanism_identification,
    'What specific mechanisms (e.g., ''loudest voice'' dynamics, resource asymmetry in draft authorship, strategic non-response) contribute most to the erosion of consensus legitimacy?',
    'Empirical studies of IETF working group dynamics, content analysis of mailing lists, and participant surveys to identify and quantify specific influence tactics.',
    'Identifying dominant capture mechanisms would allow for targeted procedural reforms to restore legitimacy, potentially shifting the constraint towards a more genuine Rope. Without this, interventions may be ineffective.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capture_mechanism_identification, empirical, 'Pinpointing the specific procedural vulnerabilities exploited for capture.').

omega_variable(
    legitimacy_measurement_validity,
    'How can the ''credibility'' or ''legitimacy'' of a decentralized consensus mechanism be objectively measured, beyond participant sentiment?',
    'Development of robust, multi-indicator metrics for standards body legitimacy, potentially including adoption rates of standards by diverse implementers, diversity of participant demographics, and independent audits of process adherence.',
    'A clearer, more objective measure of legitimacy would strengthen the ability to detect and respond to erosion, making the ''consensus_mechanism_credibility'' victim status more actionable. Without it, the assessment remains vulnerable to subjective interpretation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(legitimacy_measurement_validity, conceptual, 'Defining and measuring the abstract concept of ''consensus legitimacy''.').

omega_variable(
    founding_problem_status_ambiguity,
    'Is the founding problem of open, decentralized standards development truly ''contested'', or has it shifted to ''dead'' for some actors while remaining ''live'' for others?',
    'Further analysis of stakeholder motivations and historical context, particularly examining whether well-resourced factions genuinely seek open standards or primarily leverage the ''openness'' narrative for strategic advantage.',
    'If the founding problem is ''dead'' for key beneficiaries, the constraint is closer to a pure Snare, as its original coordination function is entirely superseded by extraction. If it remains ''live'' for a significant portion of participants, the Tangled Rope classification is more accurate.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(founding_problem_status_ambiguity, conceptual, 'Ambiguity in whether the original problem the IETF was built to solve is still relevant to all participants.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ietf_openness_commitment__legitimacy_erosion_reading, 1992, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ietf_tr_t1992, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 1992, 0.05).
narrative_ontology:measurement(ietf_tr_t2000, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 2000, 0.15).
narrative_ontology:measurement(ietf_tr_t2008, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 2008, 0.25).
narrative_ontology:measurement(ietf_tr_t2016, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 2016, 0.35).
narrative_ontology:measurement(ietf_tr_t2024, ietf_openness_commitment__legitimacy_erosion_reading, theater_ratio, 2024, 0.4).

% Extraction over time
narrative_ontology:measurement(ietf_be_t1992, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 1992, 0.2).
narrative_ontology:measurement(ietf_be_t2000, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 2000, 0.35).
narrative_ontology:measurement(ietf_be_t2008, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 2008, 0.5).
narrative_ontology:measurement(ietf_be_t2016, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 2016, 0.65).
narrative_ontology:measurement(ietf_be_t2024, ietf_openness_commitment__legitimacy_erosion_reading, base_extractiveness, 2024, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(ietf_su_t1992, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 1992, 0.1).
narrative_ontology:measurement(ietf_su_t2000, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 2000, 0.25).
narrative_ontology:measurement(ietf_su_t2008, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 2008, 0.4).
narrative_ontology:measurement(ietf_su_t2016, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 2016, 0.55).
narrative_ontology:measurement(ietf_su_t2024, ietf_openness_commitment__legitimacy_erosion_reading, suppression_requirement, 2024, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ietf_openness_commitment__legitimacy_erosion_reading, information_standard).
narrative_ontology:boltzmann_floor_override(ietf_openness_commitment__legitimacy_erosion_reading, 0.05).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment__commons_stewardship_reading).
narrative_ontology:affects_constraint(ietf_openness_commitment__legitimacy_erosion_reading, ietf_openness_commitment__capture_substrate_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'ietf_openness_commitment' kernel. This 'legitimacy erosion' reading focuses on the decline in the perceived fairness and integrity of the 'rough consensus' process itself, distinct from the 'commons stewardship' reading (which emphasizes the positive function) and the 'capture substrate' reading (which focuses on how the process enables capture).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
