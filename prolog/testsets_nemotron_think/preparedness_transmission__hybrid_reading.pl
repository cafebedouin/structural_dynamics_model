% ============================================================================
% CONSTRAINT STORY: preparedness_transmission__hybrid_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_preparedness_transmission__hybrid_reading, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: preparedness_transmission__hybrid_reading
 *   human_readable: Stratified Preparedness Transmission — Infrastructure Competence High, Civilian Coordination Decayed
 *   domain: disaster_risk_management/institutional_memory/civil_defense
 *
 * SUMMARY:
 *   Civil defense preparedness transmission has stratified into two layers:
 *   the physical infrastructure layer (shelters, warning sirens, hardened
 *   comms, engineering standards) maintains high competence through
 *   professional engineering practice, code cycles, and structural testing;
 *   the civilian coordination layer (evacuation routing, shelter management,
 *   population warning compliance, neighbor-to-neighbor mutual aid) has
 *   decayed as drills became performative, warden systems atrophied, and
 *   public knowledge evaporated. The D5 break — the point where exercised
 *   knowledge fails to transmit to the next generation — sits in the
 *   coordination layer, not the physical layer. Institutions continue to
 *   certify readiness based on infrastructure passing inspection while the
 *   coordination capacity that would actually save lives in a mass event has
 *   hollowed out.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(preparedness_transmission__hybrid_reading, 0.68).
domain_priors:suppression_score(preparedness_transmission__hybrid_reading, 0.55).
domain_priors:theater_ratio(preparedness_transmission__hybrid_reading, 0.72).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, theater_ratio, 0.72).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, accessibility_collapse, 0.62).
narrative_ontology:constraint_metric(preparedness_transmission__hybrid_reading, resistance, 0.48).

% --- Constraint claim ---
narrative_ontology:constraint_claim(preparedness_transmission__hybrid_reading, tangled_rope).
narrative_ontology:human_readable(preparedness_transmission__hybrid_reading, "Stratified Preparedness Transmission — Infrastructure Competence High, Civilian Coordination Decayed").
narrative_ontology:topic_domain(preparedness_transmission__hybrid_reading, "disaster_risk_management/institutional_memory/civil_defense").

domain_priors:requires_active_enforcement(preparedness_transmission__hybrid_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(preparedness_transmission__hybrid_reading, '4e539af4-8c72-49a0-93f9-bd745c271b64').
narrative_ontology:cs_kernel_codification('4e539af4-8c72-49a0-93f9-bd745c271b64', formalized).
narrative_ontology:cs_authority_grounding('4e539af4-8c72-49a0-93f9-bd745c271b64', lineage).
narrative_ontology:cs_interpretation_layer_present('4e539af4-8c72-49a0-93f9-bd745c271b64').
narrative_ontology:cs_reading_relation('4e539af4-8c72-49a0-93f9-bd745c271b64', preparedness_transmission__competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('4e539af4-8c72-49a0-93f9-bd745c271b64', preparedness_transmission__husk_reading, influences).
narrative_ontology:cs_axiom('4e539af4-8c72-49a0-93f9-bd745c271b64', foundational, stratified_transmission_is_structural).
narrative_ontology:cs_axiom_status(stratified_transmission_is_structural, holdable).
narrative_ontology:cs_axiom_grounding('4e539af4-8c72-49a0-93f9-bd745c271b64', stratified_transmission_is_structural, empirically_contingent).
narrative_ontology:cs_axiom('4e539af4-8c72-49a0-93f9-bd745c271b64', foundational, infrastructure_competence_does_not_imply_coordination_competence).
narrative_ontology:cs_axiom_status(infrastructure_competence_does_not_imply_coordination_competence, holdable).
narrative_ontology:cs_axiom_grounding('4e539af4-8c72-49a0-93f9-bd745c271b64', infrastructure_competence_does_not_imply_coordination_competence, empirically_contingent).
narrative_ontology:cs_reference_frame('4e539af4-8c72-49a0-93f9-bd745c271b64', cold_war_mass_mobilization_readiness).
narrative_ontology:cs_drift_state('4e539af4-8c72-49a0-93f9-bd745c271b64', post_cold_war_all_hazards_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('4e539af4-8c72-49a0-93f9-bd745c271b64', '').
narrative_ontology:cs_kernel_id(preparedness_transmission__hybrid_reading, preparedness_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, civil_defense_institutions).
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, infrastructure_engineering_corps).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, civilian_population).
narrative_ontology:constraint_victim(preparedness_transmission__hybrid_reading, emergency_responders).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(preparedness_transmission__hybrid_reading, emergency_responders).
narrative_ontology:constraint_vindicates(preparedness_transmission__hybrid_reading, engineering_standards_reliability).
narrative_ontology:constraint_vindicates(preparedness_transmission__hybrid_reading, institutional_continuity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer the civil defense framework: mandate drills, certify shelters, allocate preparedness budgets, and define readiness standards. They benefit from the appearance of an operational system — budget authority, statutory mandate, and bureaucratic survival depend on the system being seen as functional. They can reallocate resources across layers but face political cost for admitting decay.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, civil_defense_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Design, build, and maintain physical shelters, warning systems, and hardened infrastructure. Their competence is exercised and verified through regular structural testing; engineering knowledge transmits through professional licensure, codes, and peer review. They collect professional prestige, contract revenue, and institutional recognition from the infrastructure layer's performance. Their exit options are strong — transferable skills, professional recognition outside the civil defense system.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, infrastructure_engineering_corps, beneficiary,
    organized, biographical, mobile, national).

% Depend on evacuation routes, shelter access, communication protocols, and neighbor-to-neighbor coordination that the system claims to provide. They pay through taxes, compliance with drills, and opportunity cost of participation. When coordination fails, they bear the mortality and displacement costs directly. Exit is effectively trapped — geographic immobility, no alternative protection provider, and the system's monopoly on authorized shelter access.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, civilian_population, payer,
    powerless, biographical, trapped, local).

% Execute the coordination layer under stress: manage evacuations, operate shelters, triage casualties. They benefit from the infrastructure layer (hardened facilities, comms) but pay the operational cost of the coordination decay — improvising with broken protocols, covering for missing civilian wardens, absorbing public blame when the system fails. Exit is constrained: professional identity tied to the mission, limited lateral transfer, but some mobility across jurisdictions.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, emergency_responders, payer,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(preparedness_transmission__hybrid_reading, emergency_responders, beneficiary).

% Legislative committees, auditors general, and independent review boards that examine readiness reports and drill outcomes. They see the stratified results — infrastructure passing inspection while tabletop exercises reveal coordination gaps — but their corrective leverage is limited to hearings, budget conditions, and public reports. They do not operate the system and do not bear its failure costs directly.
narrative_ontology:constraint_stakeholder(preparedness_transmission__hybrid_reading, policy_oversight_bodies, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains a national civil defense system that provides physical shelter infrastructure, warning networks, and standardized engineering protections against disaster threats — a genuine coordination problem solved by centralized technical standards and capital investment.
% TRANSFER_FUNCTION: Moves institutional legitimacy, budget authority, and professional prestige to civil defense institutions and engineering corps; moves mortality risk, displacement burden, and improvisation cost to civilian populations and emergency responders when the coordination layer fails.
% ABSENT_VOICES: Community-level mutual-aid networks, informal neighborhood wardens, and displaced persons from prior coordination failures — they would testify that the official system's coordination layer is hollow, but they are excluded from readiness certification and drill design processes.
% DISAPPEARANCE_RATIONALE: If the stratified system vanished overnight, the engineering corps would continue building to code (their competence is independent), but civil defense institutions would lose their mandate, civilians would immediately organize informal mutual-aid networks (as seen in past disasters), and emergency responders would revert to ad-hoc coordination — the world rearranges because the coordination layer is already functionally absent for those who need it.
% FOUNDING_PROBLEM: Cold War-era requirement to protect civilian populations from nuclear attack through mass sheltering, warning, and evacuation — a threat model demanding both hardened infrastructure and population-scale coordination.
% FOUNDING_PROBLEM_CORROBORATION: Engineering corps and civil defense institutions attest the founding problem persists in evolved form (all-hazards protection). Independent disaster sociologists, community resilience researchers, and after-action reports from recent events attest the coordination layer was built for a mass-mobilization threat model that no longer matches actual disaster dynamics — the founding problem has mutated but the arrangement has not.
narrative_ontology:disappearance_verdict(preparedness_transmission__hybrid_reading, world_rearranges).
narrative_ontology:founding_problem_status(preparedness_transmission__hybrid_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(preparedness_transmission__hybrid_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(preparedness_transmission__hybrid_reading, 'none', 1).
narrative_ontology:epsilon_provenance(preparedness_transmission__hybrid_reading, 0.68, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(preparedness_transmission__hybrid_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(preparedness_transmission__hybrid_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(preparedness_transmission__hybrid_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extraction (0.68) reflects the asymmetric transfer: institutions and engineers collect legitimacy and resources from a system that presents as protective but delivers coordination failure to those who need it. Suppression (0.55) is moderate — the system doesn't forcibly prevent informal mutual aid, but it monopolizes authorized shelter access, controls drill narratives, and certifies readiness in ways that crowd out alternative coordination. Theater ratio (0.72) is high — drills, inspections, and certification rituals continue at near-Cold War frequency while the coordination knowledge they once transmitted has decayed. Accessibility collapse (0.62) reflects that alternatives exist (community organizing, informal networks) but are structurally suppressed by the official system's monopoly on legitimacy and shelter access. Resistance (0.48) is moderate — emergency responders and some communities push back, but the institutional monopoly on the 'preparedness' label limits effective challenge.
 *
 * PERSPECTIVAL GAP:
 *   From the institutional seat, the system is a Rope — genuine infrastructure coordination that they maintain. From the civilian seat, it is a Snare — extraction of compliance and tax revenue for a coordination promise that fails at the moment of need. From the engineering seat, it is a Mountain — physical laws and engineering standards that genuinely hold. From the responder seat, it is a Tangled Rope — real infrastructure they depend on, paired with a hollow coordination layer they must improvise around. The engine computes this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Civil defense institutions are structural beneficiaries (d near 0.1-0.2) — they collect budget, mandate, and survival from the system's appearance. Infrastructure engineering corps are beneficiaries with strong exit (d near 0.15) — they gain professional standing and contracts, with transferable skills. Civilian population are full targets (d near 0.9-1.0) — trapped, bearing mortality costs, no alternative provider. Emergency responders are partial targets with constrained exit (d near 0.6-0.7) — they get infrastructure benefits but pay operational costs of coordination failure. Policy oversight are analytical observers (d=0.5) — they see the structure but don't bear its costs or collect its rents.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (Cold War mass sheltering) has mutated (all-hazards, climate-driven, localized events) but the arrangement persists in its original form. The mandate has atrophied in the coordination layer while the infrastructure layer remains live. This is not pure extraction (institutions don't primarily enrich themselves) nor pure coordination (the coordination layer fails). It is a stratified Tangled Rope where one layer coordinates and another extracts — the mandatrophy is layer-specific, not whole-system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_framing,
    'Is the stratified transmission pattern a stable structural feature of the system, or a transitional state between the competence and husk readings?',
    'Longitudinal tracking of drill outcomes vs. infrastructure inspections across multiple disaster cycles; if coordination decay accelerates while infrastructure holds, the hybrid is transitional toward husk; if coordination stabilizes at a lower-but-functional level, the hybrid is a stable stratified equilibrium.',
    'If transitional, the constraint is on a trajectory toward husk (piton/snare); if stable, the stratified Tangled Rope is the system''s enduring form. Affects whether the coordination layer''s decay is a bug or a feature of the current arrangement.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_framing, empirical, 'Whether the hybrid reading describes a stable equilibrium or a decay trajectory.').

omega_variable(
    coordination_layer_recoverability,
    'Can the civilian coordination layer be rebuilt without reconstructing the institutional mandate, or does coordination decay require institutional replacement?',
    'Natural experiments from jurisdictions that decentralized civil defense to community level (e.g., Japan''s neighborhood associations, Chile''s post-2010 reforms) — measure whether coordination knowledge regenerates when institutional monopoly relaxes.',
    'If recoverable without institutional replacement, the extraction is contingent on institutional form; if not, the coordination decay is structurally locked to the current institutional arrangement — supporting a snare classification for the coordination layer.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(coordination_layer_recoverability, empirical, 'Whether the coordination decay is reversible within the current institutional structure.').

omega_variable(
    infrastructure_coordination_coupling,
    'Does the infrastructure layer''s high competence depend on the coordination layer''s decay (resource diversion, attention capture), or are they genuinely independent?',
    'Budget and personnel flow analysis: track whether engineering corps resources increase as coordination staffing decreases; interview retired engineers on whether coordination duties competed with infrastructure maintenance.',
    'If coupled, the stratification is extractive by design — infrastructure excellence is subsidized by coordination neglect. If independent, the hybrid is a genuine coexistence of two differently-maintained subsystems.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(infrastructure_coordination_coupling, empirical, 'Whether the two layers'' competence trajectories are structurally coupled or independent.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(preparedness_transmission__hybrid_reading, 1990, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(prep_tr_t1990, preparedness_transmission__hybrid_reading, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(prep_tr_t1998, preparedness_transmission__hybrid_reading, theater_ratio, 1998, 0.38).
narrative_ontology:measurement(prep_tr_t2005, preparedness_transmission__hybrid_reading, theater_ratio, 2005, 0.52).
narrative_ontology:measurement(prep_tr_t2012, preparedness_transmission__hybrid_reading, theater_ratio, 2012, 0.61).
narrative_ontology:measurement(prep_tr_t2018, preparedness_transmission__hybrid_reading, theater_ratio, 2018, 0.68).
narrative_ontology:measurement(prep_tr_t2025, preparedness_transmission__hybrid_reading, theater_ratio, 2025, 0.72).

% Extraction over time
narrative_ontology:measurement(prep_be_t1990, preparedness_transmission__hybrid_reading, base_extractiveness, 1990, 0.35).
narrative_ontology:measurement(prep_be_t1998, preparedness_transmission__hybrid_reading, base_extractiveness, 1998, 0.42).
narrative_ontology:measurement(prep_be_t2005, preparedness_transmission__hybrid_reading, base_extractiveness, 2005, 0.51).
narrative_ontology:measurement(prep_be_t2012, preparedness_transmission__hybrid_reading, base_extractiveness, 2012, 0.58).
narrative_ontology:measurement(prep_be_t2018, preparedness_transmission__hybrid_reading, base_extractiveness, 2018, 0.64).
narrative_ontology:measurement(prep_be_t2025, preparedness_transmission__hybrid_reading, base_extractiveness, 2025, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(prep_su_t1990, preparedness_transmission__hybrid_reading, suppression_requirement, 1990, 0.3).
narrative_ontology:measurement(prep_su_t1998, preparedness_transmission__hybrid_reading, suppression_requirement, 1998, 0.38).
narrative_ontology:measurement(prep_su_t2005, preparedness_transmission__hybrid_reading, suppression_requirement, 2005, 0.45).
narrative_ontology:measurement(prep_su_t2012, preparedness_transmission__hybrid_reading, suppression_requirement, 2012, 0.5).
narrative_ontology:measurement(prep_su_t2018, preparedness_transmission__hybrid_reading, suppression_requirement, 2018, 0.53).
narrative_ontology:measurement(prep_su_t2025, preparedness_transmission__hybrid_reading, suppression_requirement, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(preparedness_transmission__hybrid_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, preparedness_transmission__competence_reading).
narrative_ontology:affects_constraint(preparedness_transmission__hybrid_reading, preparedness_transmission__husk_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the 'preparedness transmission' label into three structurally distinct readings. The competence_reading claims live knowledge transmission (low extraction, Mountain/Rope). The husk_reading claims ritualized performance (high theater, Piton/Snare). This hybrid_reading claims stratified transmission — infrastructure Mountain, coordination Snare — linked by shared institutional mandate but different competence trajectories. The epsilon values diverge: competence_reading ε ≈ 0.15, husk_reading ε ≈ 0.75, hybrid_reading ε = 0.68 (coordination-layer-weighted).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(preparedness_transmission__hybrid_reading, organized, 0.65).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
