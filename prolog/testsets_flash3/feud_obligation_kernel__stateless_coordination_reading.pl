% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__stateless_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_feud_obligation_kernel__stateless_coordination_reading, []).

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
 *   constraint_id: feud_obligation_kernel__stateless_coordination_reading
 *   human_readable: Blood-Feud Obligations (Stateless Coordination Reading)
 *   domain: legal_anthropology/medieval_history/comparative_political_systems
 *
 * SUMMARY:
 *   This constraint story represents the 'stateless coordination' reading of
 *   blood-feud obligations, viewing them as a functional, self-enforcing
 *   mechanism for justice and deterrence in the absence of centralized
 *   authority. It emphasizes the coordination benefits for kin groups and
 *   aggrieved parties, with minimal extraction inherent to the system itself,
 *   and low suppression of alternatives like wergild. This reading contrasts
 *   with those that see feuds as destructive cycles or morally illegitimate.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__stateless_coordination_reading, 0.25).
domain_priors:suppression_score(feud_obligation_kernel__stateless_coordination_reading, 0.15).
domain_priors:theater_ratio(feud_obligation_kernel__stateless_coordination_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__stateless_coordination_reading, rope).
narrative_ontology:human_readable(feud_obligation_kernel__stateless_coordination_reading, "Blood-Feud Obligations (Stateless Coordination Reading)").
narrative_ontology:topic_domain(feud_obligation_kernel__stateless_coordination_reading, "legal_anthropology/medieval_history/comparative_political_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__stateless_coordination_reading, 'fc3295c3-56b5-49c6-9842-fbf1e74580a1').
narrative_ontology:cs_kernel_codification('fc3295c3-56b5-49c6-9842-fbf1e74580a1', implicit).
narrative_ontology:cs_authority_grounding('fc3295c3-56b5-49c6-9842-fbf1e74580a1', practice).
narrative_ontology:cs_interpretation_layer_present('fc3295c3-56b5-49c6-9842-fbf1e74580a1').
narrative_ontology:cs_reading_relation('fc3295c3-56b5-49c6-9842-fbf1e74580a1', feud_obligation_kernel__extraction_cycle_reading, coexists_with).
narrative_ontology:cs_reading_relation('fc3295c3-56b5-49c6-9842-fbf1e74580a1', feud_obligation_kernel__christianized_pacification_reading, coexists_with).
narrative_ontology:cs_axiom('fc3295c3-56b5-49c6-9842-fbf1e74580a1', foundational, kin_group_collective_responsibility).
narrative_ontology:cs_axiom_status(kin_group_collective_responsibility, holdable).
narrative_ontology:cs_axiom_grounding('fc3295c3-56b5-49c6-9842-fbf1e74580a1', kin_group_collective_responsibility, conventional).
narrative_ontology:cs_axiom('fc3295c3-56b5-49c6-9842-fbf1e74580a1', foundational, reciprocal_violence_as_deterrent).
narrative_ontology:cs_axiom_status(reciprocal_violence_as_deterrent, holdable).
narrative_ontology:cs_axiom_grounding('fc3295c3-56b5-49c6-9842-fbf1e74580a1', reciprocal_violence_as_deterrent, empirically_contingent).
narrative_ontology:cs_reference_frame('fc3295c3-56b5-49c6-9842-fbf1e74580a1', stateless_kin_justice_system).
narrative_ontology:cs_drift_state('fc3295c3-56b5-49c6-9842-fbf1e74580a1', emergence_of_centralized_authority, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('fc3295c3-56b5-49c6-9842-fbf1e74580a1', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, kin_groups).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, aggrieved_parties).
narrative_ontology:constraint_victim(feud_obligation_kernel__stateless_coordination_reading, defectors_from_obligation).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Receive a mechanism for justice and deterrence in the absence of state authority. Their collective honor and survival depend on upholding the feud obligation. Defection means social ostracization and vulnerability.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, kin_groups, beneficiary,
    organized, generational, identity_locked, local).

% Obtain redress for wrongs committed against their kin, ensuring that transgressions are met with consequences. Without the feud, they would have no recourse for serious offenses.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, aggrieved_parties, beneficiary,
    moderate, immediate, constrained, local).

% Bear the costs of failing to uphold their kin's honor or participate in the feud. This can include social expulsion, loss of protection, and severe reputational damage. Their identity is fused with kin-group membership.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, defectors_from_obligation, payer,
    powerless, biographical, identity_locked, local).

% Facilitate alternative dispute resolution through compensation (wergild). Their existence demonstrates that the feud is not the only option, but a primary one for certain offenses. They are not directly part of the feud but offer an alternative path.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, wergild_negotiators, observer,
    moderate, biographical, mobile, local).

% Represent nascent state or ecclesiastical powers that seek to suppress feuding in favor of centralized justice. They are excluded from the internal logic of the feud but exert external pressure for its cessation.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, external_authorities, excluded,
    institutional, generational, analytical, regional).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a decentralized system for maintaining order, deterring crime, and delivering justice in societies lacking formal state institutions, by establishing clear reciprocal obligations for redress.
% TRANSFER_FUNCTION: Transfers the obligation to seek redress (or offer compensation) from the individual victim to the entire kin group, and transfers the burden of deterrence from a state to the threat of reciprocal violence.
% ABSENT_VOICES: External authorities (e.g., nascent states, church officials) are absent from the internal logic of the feud; they would argue for centralized justice and an end to private vengeance, but their authority is not recognized within this system.
% DISAPPEARANCE_RATIONALE: If blood-feud obligations vanished overnight in a stateless society, the social order would collapse into unchecked violence and crime, as the primary mechanism for justice and deterrence would be gone. Kin groups would lose their collective defense mechanism.
% FOUNDING_PROBLEM: How to maintain social order, deter serious offenses, and provide justice in a society without a centralized state or formal legal system capable of enforcing laws.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological studies of stateless societies and historical accounts of early medieval Europe corroborate that feuding served a vital social function in the absence of state power. The problem is 'live' in that the mechanism still functions where state capacity is absent or weak.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__stateless_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__stateless_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__stateless_coordination_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(feud_obligation_kernel__stateless_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__stateless_coordination_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(feud_obligation_kernel__stateless_coordination_reading_tests).
:- end_tests(feud_obligation_kernel__stateless_coordination_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.25, declining to 0.15) because the primary function is coordination and justice, not rent collection. The 'cost' is the violence itself, which is a feature of the system, not an extraction from it in this reading. Suppression is low (0.15, declining to 0.05) because alternative dispute mechanisms (like wergild) often coexist, and participation is driven by social obligation and necessity rather than coercion. Theater ratio is negligible (0.05, declining to 0.01) as the system is highly functional and direct in its operation. The declining metrics over time reflect the gradual emergence of alternative, less 'costly' forms of justice or the eventual rise of state power, making the feud less necessary.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of kin groups, the feud is a necessary and beneficial system for survival and honor. From the perspective of external authorities, it is a barbaric practice to be suppressed. This story adopts the internal, functional perspective of the kin groups.
 *
 * DIRECTIONALITY LOGIC:
 *   Kin groups and aggrieved parties are beneficiaries (d near 0.0) as they gain a system of justice and protection. Defectors from obligation are targets (d near 1.0) as they bear the full social cost of non-compliance. Wergild negotiators are observers, offering an alternative but not directly participating in the feud's core mechanism. External authorities are excluded, as their claims to justice are outside the feud's framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    feud_as_coordination_vs_extraction,
    'Is the blood-feud primarily a coordination mechanism for justice and deterrence, or a destructive cycle of reciprocal extraction?',
    'Longitudinal studies comparing societies with and without feuding in stateless contexts, measuring net social welfare, population stability, and productive capacity. Analysis of historical records for evidence of net resource depletion vs. social stability.',
    'If primarily extractive, the constraint would reclassify as a Snare or Tangled Rope, with significantly higher extractiveness and victim declarations for all participants. If primarily coordination, the current Rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feud_as_coordination_vs_extraction, empirical, 'Ambiguity between the coordination benefits and the extractive costs of feuding.').

omega_variable(
    legitimacy_of_private_vengeance,
    'Is private vengeance a legitimate form of justice, or is justice inherently a function of centralized authority?',
    'This is a conceptual/preference question, not empirically resolvable. Resolution depends on the adopted normative framework (e.g., natural law, state positivism, anthropological functionalism).',
    'If private vengeance is deemed illegitimate, the ''stateless coordination'' reading''s justification for the feud''s ''justice'' function collapses, potentially shifting its classification towards a more negative type from an external moral perspective.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(legitimacy_of_private_vengeance, conceptual, 'Conceptual debate over the moral and legal legitimacy of decentralized justice systems.').

omega_variable(
    identity_lock_vs_structural_necessity,
    'Is the ''identity_locked'' exit option for kin groups and defectors primarily due to internalized identity fusion, or structural necessity in a stateless context?',
    'Comparative analysis of kin-group cohesion and individual agency in stateless societies vs. those with nascent state structures. If kin-group identity persists strongly even when state alternatives are available, it suggests stronger internalized identity lock. If it dissolves rapidly with state emergence, it suggests structural necessity.',
    'If primarily internalized identity lock, the effective suppression for these agents is higher than the structural measure suggests, as they carry the constraint with them. If primarily structural, the constraint''s persistence is more directly tied to the absence of external alternatives.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_vs_structural_necessity, empirical, 'Distinguishing between internalized identity lock and external structural necessity for participation in the feud.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__stateless_coordination_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(feud_tr_t25, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 25, 0.04).
narrative_ontology:measurement(feud_tr_t50, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 50, 0.03).
narrative_ontology:measurement(feud_tr_t75, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 75, 0.02).
narrative_ontology:measurement(feud_tr_t100, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 100, 0.01).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(feud_be_t25, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 25, 0.23).
narrative_ontology:measurement(feud_be_t50, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 50, 0.2).
narrative_ontology:measurement(feud_be_t75, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 75, 0.18).
narrative_ontology:measurement(feud_be_t100, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(feud_su_t25, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 25, 0.12).
narrative_ontology:measurement(feud_su_t50, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 50, 0.1).
narrative_ontology:measurement(feud_su_t75, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 75, 0.08).
narrative_ontology:measurement(feud_su_t100, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 100, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__stateless_coordination_reading, identity_coordination).
narrative_ontology:affects_constraint(feud_obligation_kernel__stateless_coordination_reading, wergild_negotiation_norms).
narrative_ontology:affects_constraint(feud_obligation_kernel__stateless_coordination_reading, state_formation_dynamics).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'feud_obligation_kernel'. This 'stateless_coordination_reading' emphasizes the functional aspects of feuding in the absence of state power, contrasting with the 'extraction_cycle_reading' and 'christianized_pacification_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
