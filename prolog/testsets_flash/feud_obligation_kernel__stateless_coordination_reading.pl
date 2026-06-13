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
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   This constraint describes blood-feud obligations as a self-enforcing
 *   mechanism for justice and deterrence in societies lacking centralized
 *   state authority. Kin groups are bound by honor to avenge wrongs, creating
 *   a system of reciprocal enforcement. This reading emphasizes the
 *   functional aspects of feuds in maintaining social order and preventing
 *   unchecked aggression, rather than their destructive or religiously
 *   condemned aspects. It is one reading of the 'feud_obligation_kernel'.
 *
 * KEY AGENTS:
 *   - kin_groups_seeking_justice: Primary beneficiary (organized/constrained) — receive satisfaction and uphold honor.
 *   - community_members_seeking_deterrence: Secondary beneficiary (moderate/mobile) — benefit from reduced crime due to fear of reprisal.
 *   - defectors_from_feud_obligations: Primary victim (powerless/identity_locked) — face honor loss, kin expulsion, and continued targeting.
 *   - kin_groups_failing_to_uphold_honor: Secondary victim (organized/identity_locked) — suffer reputational damage and potential social ostracization.
 *   - religious_authorities: Excluded (institutional/analytical) — would condemn feuds as unchristian and advocate for centralized justice.
 *   - emerging_state_actors: Observer (institutional/analytical) — would seek to suppress feuds to consolidate their own authority.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__stateless_coordination_reading, 0.2).
domain_priors:suppression_score(feud_obligation_kernel__stateless_coordination_reading, 0.3).
domain_priors:theater_ratio(feud_obligation_kernel__stateless_coordination_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__stateless_coordination_reading, rope).
narrative_ontology:human_readable(feud_obligation_kernel__stateless_coordination_reading, "Blood-Feud Obligations (Stateless Coordination Reading)").
narrative_ontology:topic_domain(feud_obligation_kernel__stateless_coordination_reading, "legal_anthropology/medieval_history/comparative_political_systems").

domain_priors:requires_active_enforcement(feud_obligation_kernel__stateless_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__stateless_coordination_reading, '12193f2a-8493-48db-9afe-464f8bb648b1').
narrative_ontology:cs_kernel_codification('12193f2a-8493-48db-9afe-464f8bb648b1', implicit).
narrative_ontology:cs_authority_grounding('12193f2a-8493-48db-9afe-464f8bb648b1', practice).
narrative_ontology:cs_interpretation_layer_present('12193f2a-8493-48db-9afe-464f8bb648b1').
narrative_ontology:cs_reading_relation('12193f2a-8493-48db-9afe-464f8bb648b1', feud_obligation_kernel__extraction_cycle_reading, coexists_with).
narrative_ontology:cs_reading_relation('12193f2a-8493-48db-9afe-464f8bb648b1', feud_obligation_kernel__christianized_pacification_reading, coexists_with).
narrative_ontology:cs_axiom('12193f2a-8493-48db-9afe-464f8bb648b1', foundational, kin_honor_demands_retribution).
narrative_ontology:cs_axiom_status(kin_honor_demands_retribution, holdable).
narrative_ontology:cs_axiom_grounding('12193f2a-8493-48db-9afe-464f8bb648b1', kin_honor_demands_retribution, conventional).
narrative_ontology:cs_axiom('12193f2a-8493-48db-9afe-464f8bb648b1', foundational, self_help_is_legitimate_justice).
narrative_ontology:cs_axiom_status(self_help_is_legitimate_justice, holdable).
narrative_ontology:cs_axiom_grounding('12193f2a-8493-48db-9afe-464f8bb648b1', self_help_is_legitimate_justice, conventional).
narrative_ontology:cs_reference_frame('12193f2a-8493-48db-9afe-464f8bb648b1', stateless_kin_based_justice).
narrative_ontology:cs_drift_state('12193f2a-8493-48db-9afe-464f8bb648b1', early_state_formation_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('12193f2a-8493-48db-9afe-464f8bb648b1', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, kin_groups_seeking_justice).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, community_members_seeking_deterrence).
narrative_ontology:constraint_victim(feud_obligation_kernel__stateless_coordination_reading, defectors_from_feud_obligations).
narrative_ontology:constraint_victim(feud_obligation_kernel__stateless_coordination_reading, kin_groups_failing_to_uphold_honor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These groups are bound by honor to avenge wrongs against their members. They receive satisfaction for grievances and uphold their family's reputation, which is crucial for their social standing and security. Exiting the obligation means dishonor and vulnerability.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, kin_groups_seeking_justice, beneficiary,
    organized, generational, identity_locked, local).

% Individuals and families within the community who benefit from the general deterrence effect of the feud system. While not directly involved in every feud, they experience a safer environment due to the fear of reprisal. They can relocate if the system becomes too unstable.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, community_members_seeking_deterrence, beneficiary,
    moderate, biographical, mobile, local).

% Individuals who fail to uphold their kin's honor or refuse to participate in a feud. They face severe social ostracization, loss of protection from their kin, and may become targets for further violence without recourse. Their identity is fused with their kin group's honor.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, defectors_from_feud_obligations, payer,
    powerless, biographical, identity_locked, local).

% Entire kin groups that fail to meet their feud obligations. They suffer collective reputational damage, lose standing in the community, and may find themselves isolated and vulnerable to aggression from other groups. Their collective identity is tied to their honor.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, kin_groups_failing_to_uphold_honor, payer,
    organized, generational, identity_locked, local).

% Ecclesiastical figures who condemn blood feuds as sinful and advocate for peaceful resolution or centralized justice under divine law. They are outside the immediate enforcement mechanism of the feud but exert moral pressure.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, religious_authorities, excluded,
    institutional, civilizational, analytical, regional).

% Early forms of centralized authority (e.g., kings, regional lords) who view feuds as a challenge to their power and a barrier to territorial consolidation. They observe the system with the intent to eventually suppress it and replace it with state-controlled justice.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, emerging_state_actors, observer,
    institutional, generational, analytical, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(feud_obligation_kernel__stateless_coordination_reading, kin_groups_seeking_justice).
narrative_ontology:fixing_cost_class(feud_obligation_kernel__stateless_coordination_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a self-enforcing system of justice and deterrence in stateless societies by obligating kin groups to avenge wrongs, thereby preventing unchecked aggression and maintaining a fragile social order.
% TRANSFER_FUNCTION: Transfers the obligation of vengeance and the risk of violence among kin groups, ensuring that wrongs are met with reprisal, and thereby transferring a sense of justice and security to those who uphold the system.
% ABSENT_VOICES: Religious authorities and emerging state actors are largely excluded from the direct operation of the feud, though they exert external pressure. They would advocate for alternative, centralized forms of justice and condemn the cycle of violence.
% DISAPPEARANCE_RATIONALE: If blood-feud obligations vanished overnight in a stateless society, the existing social order would collapse into unchecked violence and chaos, as the primary mechanism for justice and deterrence would be gone. New, likely more brutal, forms of self-help would emerge.
% FOUNDING_PROBLEM: The absence of a centralized authority capable of enforcing laws and providing justice, leading to unchecked aggression and a lack of deterrence for wrongs committed against individuals or kin groups.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological studies of stateless societies and historical accounts from periods before strong state formation corroborate the problem of unchecked violence and the functional role of feuds. These sources are external to the direct participants and do not benefit from the feud itself.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__stateless_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__stateless_coordination_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__stateless_coordination_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(feud_obligation_kernel__stateless_coordination_reading, 'none', 1).

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
 *   Extractiveness is low (0.2) because the primary 'cost' is the obligation to participate, which is also the 'benefit' of deterrence and justice. Suppression is moderate (0.3) as alternatives like wergild (blood money) or arbitration often coexist, but social pressure and honor codes are strong. Theater ratio is low (0.1) as the system is largely functional in its stated purpose. The slight decrease in extractiveness and suppression over time reflects the gradual emergence of alternative dispute mechanisms or early state-like structures.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of kin groups upholding the feud, it is a necessary and just system (Rope). From the perspective of those who might prefer peaceful resolution or are caught in cycles of violence, it could be seen as extractive (Snare) or morally wrong (Christianized Pacification reading). This story focuses on the internal logic of the 'stateless coordination' perspective.
 *
 * DIRECTIONALITY LOGIC:
 *   Kin groups seeking justice are beneficiaries (d=0.0-0.2) as they gain honor and deterrence. Defectors or kin groups failing to uphold honor are victims (d=0.8-1.0) as they face severe social and physical consequences. Community members benefit from deterrence (d=0.2-0.4). Religious authorities and emerging state actors are outside the system, observing or seeking to dismantle it.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling a functional, albeit brutal, coordination mechanism as pure extraction. While it has costs, the 'justice' and 'deterrence' functions are genuinely served in the absence of alternatives. Mandatrophy is not resolved, as the system persists due to the continued absence of a superior enforcement capacity, not due to inertia after its function has ceased.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identification,
    'Is this constraint a genuine stateless coordination mechanism, or is it primarily an extraction cycle or a violation of divine law?',
    'Historical and anthropological analysis of alternative dispute resolution mechanisms (e.g., wergild, arbitration) and their prevalence; examination of long-term societal stability and resource depletion in feud-governed societies.',
    'If primarily an extraction cycle, the classification shifts to Snare; if a violation of divine law, the classification shifts to a Tangled Rope enforced by religious authority. This reading emphasizes the coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identification, conceptual, 'This constraint is the ''stateless_coordination_reading'' of the ''feud_obligation_kernel''.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (lack of alternatives to feud) or internalized (honor code, fear of kin expulsion)?',
    'Post-feud-abolition trajectory: if honor-based violence persists after state enforcement is introduced, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in maintaining feud obligations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__stateless_coordination_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_tr_t0, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(feud_tr_t50, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 50, 0.12).
narrative_ontology:measurement(feud_tr_t100, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(feud_be_t0, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(feud_be_t50, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 50, 0.2).
narrative_ontology:measurement(feud_be_t100, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 100, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(feud_su_t0, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 0, 0.35).
narrative_ontology:measurement(feud_su_t50, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 50, 0.3).
narrative_ontology:measurement(feud_su_t100, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 100, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__stateless_coordination_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(feud_obligation_kernel__stateless_coordination_reading, 0.08).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'feud_obligation_kernel', alongside 'extraction_cycle_reading' and 'christianized_pacification_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
