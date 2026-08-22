% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__stateless_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:suppression_profile/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   human_readable: Blood-Feud Obligation: Stateless Coordination Reading
 *   domain: legal_anthropology/medieval_history/comparative_political_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the stateless_coordination_reading of
 *   the feud_obligation_kernel. It treats blood-feud obligations as a
 *   self-enforcing mechanism that solves the collective-action problem of
 *   justice and deterrence in the absence of centralized enforcement. Kinship
 *   groups are the primary beneficiaries, receiving reciprocal protection and
 *   honor-restoration; defectors from the obligation bear costs of expulsion
 *   and social death. Wergild and other compensation mechanisms coexist with
 *   low suppression. The reading stands in contest with the
 *   extraction_cycle_reading (which sees feud as destructive rent-seeking)
 *   and the christianized_pacification_reading (which sees feud as violation
 *   of divine monopoly on legitimate violence).
 *
 * KEY AGENTS:
 *   - Kinship groups (beneficiary, organized, identity-locked) â receive justice and deterrence through reciprocal obligation
 *   - Defectors (payer, powerless, trapped) â bear honor loss and kinship expulsion for non-compliance
 *   - Wergild mediators (observer, moderate, mobile) â offer alternative dispute resolution without suppression
 *   - Ecclesiastical authorities (observer, institutional, analytical) â advance alternative moral frameworks delegitimizing feud violence
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__stateless_coordination_reading, 0.45).
domain_priors:suppression_score(feud_obligation_kernel__stateless_coordination_reading, 0.3).
domain_priors:theater_ratio(feud_obligation_kernel__stateless_coordination_reading, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 0.35).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, accessibility_collapse, 0.5).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, resistance, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__stateless_coordination_reading, rope).
narrative_ontology:human_readable(feud_obligation_kernel__stateless_coordination_reading, "Blood-Feud Obligation: Stateless Coordination Reading").
narrative_ontology:topic_domain(feud_obligation_kernel__stateless_coordination_reading, "legal_anthropology/medieval_history/comparative_political_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__stateless_coordination_reading, '44fab1d0-14f8-4fea-9dc1-c824349b8120').
narrative_ontology:cs_kernel_codification('44fab1d0-14f8-4fea-9dc1-c824349b8120', distributed).
narrative_ontology:cs_authority_grounding('44fab1d0-14f8-4fea-9dc1-c824349b8120', self_enforcing).
narrative_ontology:cs_reading_relation('44fab1d0-14f8-4fea-9dc1-c824349b8120', feud_obligation_kernel__extraction_cycle_reading, coexists_with).
narrative_ontology:cs_reading_relation('44fab1d0-14f8-4fea-9dc1-c824349b8120', feud_obligation_kernel__christianized_pacification_reading, coexists_with).
narrative_ontology:cs_axiom('44fab1d0-14f8-4fea-9dc1-c824349b8120', foundational, kinship_reciprocity_legitimate_justice).
narrative_ontology:cs_axiom_status(kinship_reciprocity_legitimate_justice, holdable).
narrative_ontology:cs_axiom_grounding('44fab1d0-14f8-4fea-9dc1-c824349b8120', kinship_reciprocity_legitimate_justice, conventional).
narrative_ontology:cs_axiom('44fab1d0-14f8-4fea-9dc1-c824349b8120', foundational, honor_deterrence_self_enforcing).
narrative_ontology:cs_axiom_status(honor_deterrence_self_enforcing, holdable).
narrative_ontology:cs_axiom_grounding('44fab1d0-14f8-4fea-9dc1-c824349b8120', honor_deterrence_self_enforcing, instrumental).
narrative_ontology:cs_reference_frame('44fab1d0-14f8-4fea-9dc1-c824349b8120', kinship_reciprocity_equilibrium).
narrative_ontology:cs_drift_state('44fab1d0-14f8-4fea-9dc1-c824349b8120', early_state_consolidation, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('44fab1d0-14f8-4fea-9dc1-c824349b8120', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, kinship_groups).
narrative_ontology:constraint_victim(feud_obligation_kernel__stateless_coordination_reading, defectors).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Kinship networks that participate in reciprocal obligation to secure justice for slain kin and deter future violence; they benefit from the deterrence equilibrium and the restoration of honor through retaliation or negotiated settlement. Exit means abandoning kin identity and the protection it affords.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, kinship_groups, beneficiary,
    organized, generational, identity_locked, regional).

% Individuals who refuse or fail to meet feud obligations and consequently suffer expulsion from kinship networks, loss of honor, and social death; they bear the costs of the system's enforcement and have no fallback protective institution.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, defectors, payer,
    powerless, immediate, trapped, local).

% Practitioners of blood-price compensation who offer a non-violent alternative to feud obligations; they coexist with the feud system and are not actively suppressed, providing a partial exit from the violence cycle.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, wergild_mediators, observer,
    moderate, biographical, mobile, regional).

% Church officials who delegitimize feud violence as sinful but operate within societies where kinship obligations remain the primary enforcement mechanism; they observe and record the practice while advancing alternative moral frameworks.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, ecclesiastical_authorities, observer,
    institutional, civilizational, analytical, continental).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: In the absence of centralized enforcement, kinship groups coordinate reciprocal protection and retaliation to deter violence and restore honor after a killing, solving the collective action problem of securing justice without a state.
% TRANSFER_FUNCTION: Moves the obligation of violent retaliation or compensation from the offended kinship group to the offending kinship group; also moves social standing and protective status away from defectors and toward compliant kin members.
% ABSENT_VOICES: Centralized state authorities and Christian pacifist theologians are structurally marginal in this reading; they would advocate for a monopoly over legitimate violence or divine-law prohibitions on vengeance but are absent from the stateless coordination frame.
% DISAPPEARANCE_RATIONALE: Without the feud obligation, stateless societies lose their primary enforcement mechanism and deterrence framework; kinship groups would face higher predation risk and would likely intensify wergild or fragment into smaller protective units.
% FOUNDING_PROBLEM: The absence of a centralized state monopoly on violence leaves kinship groups without a third-party enforcer to punish killers and deter future violence.
% FOUNDING_PROBLEM_CORROBORATION: Anthropologists and legal historians outside the benefiting kinship groups attest that stateless societies face genuine collective-action problems in securing justice; however, they contest whether blood-feud is the optimal or only solution.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__stateless_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__stateless_coordination_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__stateless_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_kimi2', 'agent/example_platform_commission.json',
    'kimi-k2.6', 'max_tokens=32000,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(feud_obligation_kernel__stateless_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__stateless_coordination_reading, 0.45, 'kimi-k2.6', 'none', direct).

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
 *   Extractiveness is moderate (0.45) because the obligation imposes real costs on participants (risk of violence, mandatory retaliation) and severe costs on defectors, but these are framed as the price of coordination rather than asymmetric rent. Suppression is low (0.30) because wergild and compensation alternatives are not actively blocked; the constraint is self-enforcing through honor rather than centrally enforced. Theater_ratio rises from 0.10 to 0.35 over the interval as state formation begins to outcompete kinship justice, making feud performance increasingly symbolic. Accessibility_collapse is moderate (0.50) because in a genuinely stateless context there is no accessible alternative to kin-based enforcement, yet wergild provides a partial alternative. Resistance is low (0.25) because within the stateless frame the obligation is widely accepted as legitimate.
 *
 * PERSPECTIVAL GAP:
 *   The kinship group seat experiences the constraint as protective coordination; the defector seat experiences it as punitive exclusion. The ecclesiastical observer seat sees sinful violence; the wergild mediator sees a bypassable alternative. The engine computes these divergences from the structural role and exit data.
 *
 * DIRECTIONALITY LOGIC:
 *   Kinship_groups are declared beneficiaries with identity-locked exit, placing them near the full-beneficiary end (low d, low Ï). Defectors are declared payers with trapped exit, placing them near the full-target end (high d, high Ï). The divergence between these seats is structurally sharp: the same arrangement that secures the group extracts from the individual who breaks its rules. No directionality overrides are needed because the beneficiary/payer declarations cleanly map to the structural relationships.
 *
 * MANDATROPHY ANALYSIS:
 *   The classification prevents mislabeling by requiring the coordination function to be stated independently of the transfer function. Here, the coordination function (justice/deterrence in stateless contexts) is genuine, and the transfer (obligation imposition on defectors) is structurally separable from pure extraction. The constraint does not meet the snare profile because suppression is low and alternatives coexist; it does not meet the tangled_rope profile because active enforcement is absent (self-enforcing honor) and the victim set is a behavioral category rather than a persistently exploited group. The engine will test this claim against the metrics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    feud_coordination_or_extraction,
    'Does the blood-feud obligation primarily coordinate kinship groups for mutual security, or does it extract productive capacity through cyclical violence?',
    'Comparative analysis of feud frequency against homicide rates and economic productivity across stateless societies; resolution by observing whether feud societies experience net deterrence or net depletion.',
    'If net depletion, the constraint computes as tangled_rope or snare rather than rope; if net deterrence, the coordination reading is structurally vindicated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feud_coordination_or_extraction, empirical, 'Whether blood-feud is coordinative or extractive at scale').

omega_variable(
    self_enforcement_sustainability,
    'Can honor-based self-enforcement sustain a stable deterrence equilibrium indefinitely, or does it inevitably decay into feuding cycles or state capture?',
    'Longitudinal anthropological and historical study of feud systems tracking escalation rates and institutional replacement by state justice.',
    'If decay is inevitable, the constraint is a scaffold (transitional) rather than a permanent rope; if stable, it remains rope.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(self_enforcement_sustainability, empirical, 'Long-term stability of self-enforcing honor equilibrium').

omega_variable(
    suppression_of_wergild,
    'Does the feud obligation actively suppress wergild and other alternatives, or do they genuinely coexist?',
    'Historical legal records tracking the frequency of wergild settlements versus feud violence in the same jurisdictions.',
    'If wergild is suppressed, suppression rises and the constraint shifts toward snare; if coexistence is genuine, the rope classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_of_wergild, empirical, 'Whether alternative dispute mechanisms are suppressed').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__stateless_coordination_reading, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_stateless_coord_tr_t0, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(feud_stateless_coord_tr_t10, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 10, 0.15).
narrative_ontology:measurement(feud_stateless_coord_tr_t20, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 20, 0.22).
narrative_ontology:measurement(feud_stateless_coord_tr_t30, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 30, 0.28).
narrative_ontology:measurement(feud_stateless_coord_tr_t40, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 40, 0.32).
narrative_ontology:measurement(feud_stateless_coord_tr_t50, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 50, 0.35).

% Extraction over time
narrative_ontology:measurement(feud_stateless_coord_be_t0, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(feud_stateless_coord_be_t10, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 10, 0.35).
narrative_ontology:measurement(feud_stateless_coord_be_t20, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(feud_stateless_coord_be_t30, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 30, 0.41).
narrative_ontology:measurement(feud_stateless_coord_be_t40, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 40, 0.43).
narrative_ontology:measurement(feud_stateless_coord_be_t50, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 50, 0.45).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(feud_obligation_kernel__stateless_coordination_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:affects_constraint(feud_obligation_kernel__stateless_coordination_reading, extraction_cycle_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__stateless_coordination_reading, christianized_pacification_reading).

% DUAL FORMULATION NOTE:
% The feud_obligation_kernel decomposes into three structurally distinct constraints: the stateless_coordination_reading (coordination function dominant), the extraction_cycle_reading (extractive depletion dominant), and the christianized_pacification_reading (divine-legitimacy rejection). Each reading carries a different Îµ, beneficiary/victim structure, and classification. They are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
