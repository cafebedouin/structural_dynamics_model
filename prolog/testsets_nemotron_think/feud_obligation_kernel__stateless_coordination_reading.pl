% ============================================================================
% CONSTRAINT STORY: feud_obligation_kernel__stateless_coordination_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-03
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
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
 *   constraint_id: feud_obligation_kernel__stateless_coordination_reading
 *   human_readable: Blood-Feud Obligation as Stateless Coordination Mechanism
 *   domain: legal_anthropology/medieval_history/comparative_political_systems
 *
 * SUMMARY:
 *   Blood-feud obligations in stateless societies (medieval Iceland, Albanian
 *   highlands, Scottish borders, pre-state Germanic law) function as a
 *   self-enforcing coordination mechanism: kinship groups mutually commit to
 *   avenge wrongs against their members, creating deterrence without police.
 *   The system coexists with wergild (compensation payments) as a
 *   lower-violence resolution track. This reading — the stateless
 *   coordination reading — frames the feud as a genuine solution to the
 *   Hobbesian problem, acknowledging its extractive costs (defectors
 *   expelled, casualties borne) but claiming the coordination function is
 *   primary and the extraction is the price of credibility. The sibling
 *   readings (extraction_cycle, christianized_pacification) frame the same
 *   institution as predatory or sinful respectively.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(feud_obligation_kernel__stateless_coordination_reading, 0.42).
domain_priors:suppression_score(feud_obligation_kernel__stateless_coordination_reading, 0.28).
domain_priors:theater_ratio(feud_obligation_kernel__stateless_coordination_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(feud_obligation_kernel__stateless_coordination_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(feud_obligation_kernel__stateless_coordination_reading, tangled_rope).
narrative_ontology:human_readable(feud_obligation_kernel__stateless_coordination_reading, "Blood-Feud Obligation as Stateless Coordination Mechanism").
narrative_ontology:topic_domain(feud_obligation_kernel__stateless_coordination_reading, "legal_anthropology/medieval_history/comparative_political_systems").

domain_priors:requires_active_enforcement(feud_obligation_kernel__stateless_coordination_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(feud_obligation_kernel__stateless_coordination_reading, 'fc3e6e10-5d2e-48ef-951a-e4600e656c03').
narrative_ontology:cs_kernel_codification('fc3e6e10-5d2e-48ef-951a-e4600e656c03', implicit).
narrative_ontology:cs_authority_grounding('fc3e6e10-5d2e-48ef-951a-e4600e656c03', practice).
narrative_ontology:cs_interpretation_layer_present('fc3e6e10-5d2e-48ef-951a-e4600e656c03').
narrative_ontology:cs_reading_relation('fc3e6e10-5d2e-48ef-951a-e4600e656c03', feud_obligation_kernel__extraction_cycle_reading, coexists_with).
narrative_ontology:cs_reading_relation('fc3e6e10-5d2e-48ef-951a-e4600e656c03', feud_obligation_kernel__christianized_pacification_reading, influences).
narrative_ontology:cs_axiom('fc3e6e10-5d2e-48ef-951a-e4600e656c03', foundational, kin_justice_self_enforcing).
narrative_ontology:cs_axiom_status(kin_justice_self_enforcing, holdable).
narrative_ontology:cs_axiom_grounding('fc3e6e10-5d2e-48ef-951a-e4600e656c03', kin_justice_self_enforcing, conventional).
narrative_ontology:cs_axiom('fc3e6e10-5d2e-48ef-951a-e4600e656c03', secondary, wergild_complementary_not_substitutive).
narrative_ontology:cs_axiom_status(wergild_complementary_not_substitutive, holdable).
narrative_ontology:cs_axiom_grounding('fc3e6e10-5d2e-48ef-951a-e4600e656c03', wergild_complementary_not_substitutive, conventional).
narrative_ontology:cs_reference_frame('fc3e6e10-5d2e-48ef-951a-e4600e656c03', customary_kin_justice).
narrative_ontology:cs_drift_state('fc3e6e10-5d2e-48ef-951a-e4600e656c03', state_formation_era, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('fc3e6e10-5d2e-48ef-951a-e4600e656c03', '').
narrative_ontology:cs_kernel_id(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, feud_participants).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, kin_groups).
narrative_ontology:constraint_victim(feud_obligation_kernel__stateless_coordination_reading, defectors).
narrative_ontology:constraint_victim(feud_obligation_kernel__stateless_coordination_reading, feud_casualties).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(feud_obligation_kernel__stateless_coordination_reading, wergild_practitioners).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__stateless_coordination_reading, customary_law_authority).
narrative_ontology:constraint_vindicates(feud_obligation_kernel__stateless_coordination_reading, kin_based_justice).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Members of kin groups who abide by feud obligations receive credible deterrence against aggression and a recognized path to justice through kinship enforcement. Their identity and social standing are fused with the feud system; exit means loss of kin protection and social personhood. They gain security and status but bear the obligation to avenge kin.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, feud_participants, beneficiary,
    organized, generational, identity_locked, regional).

% Individuals who refuse feud obligations or violate kin norms face honor loss, kinship expulsion, and vulnerability to violence without redress. They bear the extraction of the system — their productive capacity and safety are forfeit — with no realistic exit because expulsion from the kin group in a stateless context is social and often physical death.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, defectors, payer,
    powerless, immediate, trapped, local).

% Corporate kin groups (clans, lineages) administer the feud system, declare obligations, negotiate settlements (wergild), and enforce compliance. They collect the coordination benefit — internal order and external deterrence — and also bear the costs of maintaining the system. Their authority derives from customary practice and the threat of collective force.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, kin_groups, agenda_setter,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(feud_obligation_kernel__stateless_coordination_reading, kin_groups, beneficiary).

% Parties who resolve disputes through compensation payments (wergild) rather than blood vengeance. They operate within the same normative universe but use the lower-violence track. They benefit from the feud system's existence (it gives wergild its threat backdrop) but are not bound to the vengeance track. Their exit to pure commercial or state law is more feasible.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, wergild_practitioners, beneficiary,
    moderate, biographical, mobile, regional).

% Non-combatants — women, children, elderly, bystanders — killed or maimed in feud violence. They bear the ultimate extraction of the system without any role in its operation or benefit from its coordination. Their suffering is the externality that makes the system extractive rather than purely coordinative.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, feud_casualties, payer,
    powerless, immediate, trapped, local).

% Scholars who study feud systems as functional coordination mechanisms. They see the full structure — the justice provided, the extraction imposed, the alternatives suppressed or permitted — without being subject to the system's enforcement.
narrative_ontology:constraint_stakeholder(feud_obligation_kernel__stateless_coordination_reading, external_anthropologists, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides credible deterrence and dispute resolution in stateless societies where no centralized enforcement exists, using kinship solidarity as the enforcement backbone.
% TRANSFER_FUNCTION: Moves security and honor-status from defectors and casualties to compliant kin groups; moves productive capacity from vengeance cycles to negotiated settlements (wergild) when the coordination track operates.
% ABSENT_VOICES: Feud casualties (dead, maimed, displaced non-combatants) who cannot speak; merchant and artisan classes whose commerce is disrupted by feud violence; women in patriarchal kin systems whose interests are mediated by male kin; future generations locked into inherited feuds.
% DISAPPEARANCE_RATIONALE: If feud obligations vanished overnight in a stateless society, the vacuum would be filled by either warlordism, state formation, or commercial protection rackets — the coordination function is real and its loss rearranges the social order. Where states already exist, the verdict shifts toward world_unchanged.
% FOUNDING_PROBLEM: How to achieve credible deterrence and norm enforcement without a monopoly on violence — the classic stateless order problem.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians (e.g., Fritz Kern on medieval law, Paul Hyams on English feud) and anthropologists (Max Gluckman, E.E. Evans-Pritchard) document that feud systems recede as state courts and royal justice expand — the founding problem is solved by state formation, not by the feud system itself. The kin groups' own chroniclers (sagas, annals) attest the problem persists only where the state fails to penetrate.
narrative_ontology:disappearance_verdict(feud_obligation_kernel__stateless_coordination_reading, world_rearranges).
narrative_ontology:founding_problem_status(feud_obligation_kernel__stateless_coordination_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(feud_obligation_kernel__stateless_coordination_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(feud_obligation_kernel__stateless_coordination_reading, 'none', 1).
narrative_ontology:epsilon_provenance(feud_obligation_kernel__stateless_coordination_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

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
 *   Extractiveness is moderate (0.42) — the system extracts heavily from defectors and casualties but provides real security to participants. Suppression is low (0.28) because wergild operates openly as an alternative; the feud system does not need to crush it — wergild's existence actually stabilizes the system by providing an exit valve. Theater is low (0.18) — the violence is functional, not performative, though ritualized vengeance displays increase theater in later periods. Accessibility collapse is moderate (0.55) — alternatives (wergild, flight, submission) exist but are constrained by honor norms. Resistance is moderate (0.45) — defectors resist, but the system's legitimacy among participants is high.
 *
 * PERSPECTIVAL GAP:
 *   From the kin-group seat, the feud is a rope (coordination with manageable costs). From the defector/casualty seat, it is a snare (extraction with no exit). The engine's per-seat classification will capture this divergence. The claimed_type tangled_rope reflects the system-level reality: both coordination and extraction are structurally real.
 *
 * DIRECTIONALITY LOGIC:
 *   Feud participants and kin groups are structural beneficiaries (d near 0.0-0.2) — they receive justice and deterrence, their identity is locked to the system. Defectors and casualties are structural targets (d near 0.8-1.0) — they bear the extraction with trapped exit. Wergild practitioners sit near symmetric (d ~0.5) — they use the system's threat backdrop but avoid its costs. The engine will compute per-seat χ from these structural positions.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (stateless deterrence) is dead where states exist, but the constraint persists in residual zones (organized crime, weak-state regions, honor cultures). This is mandatrophy: the coordination function has atrophied in state-saturated zones but the constraint remains through identity lock and institutional inertia. In stateless zones the founding problem remains live — the classification must be indexed to the spatial scope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_structure_feud_kernel,
    'How does the classification of blood-feud obligations change across the three declared readings of the feud_obligation_kernel (stateless_coordination, extraction_cycle, christianized_pacification)?',
    'Compare the beneficiary/victim structures, extractiveness values, and claimed_types across the three constraint stories generated for this kernel. The structural delta is: coordination reading puts participants in beneficiaries and defectors in victims; extraction reading puts kin elites in beneficiaries and all productive members in victims; Christian reading puts ecclesiastical/royal authority in beneficiaries and feud participants in victims.',
    'If the three readings produce widely divergent ε and classifications for the same historical institution, the kernel label ''blood feud'' conflates structurally distinct constraints — confirming the ε-invariance principle requires decomposition. If they converge, the label captures a unified structure.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_structure_feud_kernel, conceptual, 'Whether the three kernel readings instantiate one constraint or three structurally distinct ones.').

omega_variable(
    suppression_mechanism_feud,
    'Is the measured suppression (0.28) in feud systems structural (kin groups actively prevent exit to state law) or internalized (honor norms make exit unthinkable)?',
    'Compare suppression trajectories in regions where state courts become available: if feud participation drops sharply, suppression was structural; if participation persists despite state alternatives, internalized honor norms dominate. Historical data from Icelandic Commonwealth to Norwegian rule, Albanian highlands under Ottoman then state rule.',
    'If internalized, effective suppression is higher than structural measure suggests — agents carry the constraint with them. This would increase χ for identity-locked participants and support reclassification toward snare for those seats.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_feud, empirical, 'Structural vs. internalized suppression in kinship-based enforcement.').

omega_variable(
    wergild_separability,
    'Is wergild (compensation) a genuine alternative track that reduces extraction, or a pressure valve that stabilizes the feud system by bleeding off resistance?',
    'Analyze feud intensity in systems with vs. without wergild options. If wergild systems show lower casualty rates and shorter feud durations, it is a genuine coordination complement. If wergild systems show same or higher total violence (compensation + vengeance), it is a stabilizer for the extraction cycle.',
    'If wergild is a stabilizer, the coordination reading''s claim of ''low suppression of alternatives'' is misleading — the alternative exists but serves the extraction function. This would increase ε and support extraction_cycle_reading''s framing.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wergild_separability, empirical, 'Whether the coexistence of wergild indicates genuine pluralism or extraction stabilization.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(feud_obligation_kernel__stateless_coordination_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(feud_obligation_kernel__stateless_coordination_reading_tr_t0, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(feud_obligation_kernel__stateless_coordination_reading_tr_t20, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 20, 0.15).
narrative_ontology:measurement(feud_obligation_kernel__stateless_coordination_reading_tr_t40, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 40, 0.18).
narrative_ontology:measurement(feud_obligation_kernel__stateless_coordination_reading_tr_t60, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 60, 0.22).
narrative_ontology:measurement(feud_obligation_kernel__stateless_coordination_reading_tr_t80, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 80, 0.25).
narrative_ontology:measurement(feud_obligation_kernel__stateless_coordination_reading_tr_t100, feud_obligation_kernel__stateless_coordination_reading, theater_ratio, 100, 0.18).

% Extraction over time
narrative_ontology:measurement(feud_obligation_kernel__stateless_coordination_reading_be_t0, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(feud_obligation_kernel__stateless_coordination_reading_be_t20, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(feud_obligation_kernel__stateless_coordination_reading_be_t40, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 40, 0.42).
narrative_ontology:measurement(feud_obligation_kernel__stateless_coordination_reading_be_t60, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 60, 0.45).
narrative_ontology:measurement(feud_obligation_kernel__stateless_coordination_reading_be_t80, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 80, 0.44).
narrative_ontology:measurement(feud_obligation_kernel__stateless_coordination_reading_be_t100, feud_obligation_kernel__stateless_coordination_reading, base_extractiveness, 100, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(feud_obligation_kernel__stateless_coordination_reading_su_t0, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(feud_obligation_kernel__stateless_coordination_reading_su_t20, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 20, 0.28).
narrative_ontology:measurement(feud_obligation_kernel__stateless_coordination_reading_su_t40, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 40, 0.32).
narrative_ontology:measurement(feud_obligation_kernel__stateless_coordination_reading_su_t60, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 60, 0.3).
narrative_ontology:measurement(feud_obligation_kernel__stateless_coordination_reading_su_t80, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 80, 0.28).
narrative_ontology:measurement(feud_obligation_kernel__stateless_coordination_reading_su_t100, feud_obligation_kernel__stateless_coordination_reading, suppression_requirement, 100, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(feud_obligation_kernel__stateless_coordination_reading, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(feud_obligation_kernel__stateless_coordination_reading, 0.12).
narrative_ontology:affects_constraint(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel__extraction_cycle_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__stateless_coordination_reading, feud_obligation_kernel__christianized_pacification_reading).
narrative_ontology:affects_constraint(feud_obligation_kernel__stateless_coordination_reading, state_formation_centralized_justice).
narrative_ontology:affects_constraint(feud_obligation_kernel__stateless_coordination_reading, wergild_compensation_system).

% DUAL FORMULATION NOTE:
% This constraint (stateless_coordination_reading) and its two siblings form the feud_obligation_kernel constraint family. The coordination reading claims ε≈0.42 (tangled_rope); the extraction reading would claim higher ε (snare); the Christian reading would claim ε≈0 for the feud itself but high ε for the pacification constraint that replaces it. All three share the kernel_id feud_obligation_kernel and are linked via affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(feud_obligation_kernel__stateless_coordination_reading, organized, 0.15).
constraint_indexing:directionality_override(feud_obligation_kernel__stateless_coordination_reading, powerless, 0.92).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
