% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__survival_competence_reading, []).

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
 *   constraint_id: catastrophe_memory_kernel__survival_competence_reading
 *   human_readable: Ritual as Survival-Competence Transmission
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint story captures one reading of the
 *   catastrophe_memory_kernel: the survival_competence_reading. The kernel is
 *   the claim that ritual encodes and transmits something essential across
 *   generations of a persecuted community. This reading holds that the
 *   'something' is adaptive capacity for persecution-survival — specific,
 *   rehearsed operational patterns (concealment, resource-hiding, mutual-aid
 *   activation, collective decision-making under surveillance). The ritual
 *   system is a training ground: mourning-practice as survival-training. The
 *   beneficiary is community resilience under threat; the victim is
 *   assimilation pressure, which extracts boundary-maintenance costs from
 *   individuals who would otherwise shed distinctive practices. Moderate
 *   extractiveness (0.38) reflects that the rehearsal burden is real but
 *   bounded — the community calibrates ritual intensity to the perceived
 *   threat level. Active enforcement is required: elders police transmission
 *   fidelity, and the community sanctions non-participation because
 *   free-riding on the survival repertoire collapses the mutual-aid trust
 *   structure.
 *
 * KEY AGENTS:
 *   - persecuted_community_collective: Primary beneficiary (organized/identity_locked) — receives adaptive capacity
 *   - elder_ritual_practitioners: Agenda-setter and secondary beneficiary (institutional/constrained) — maintains and transmits the repertoire
 *   - assimilation_pressure_vectors: Primary payer (powerful/mobile) — state, market, and cultural forces that reward shedding distinctiveness
 *   - boundary_maintenance_cost_bearers: Secondary payer (moderate/constrained) — individual members bearing daily rehearsal costs
 *   - diaspora_historians: Observer (analytical/analytical) — documents and analyzes the system
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__survival_competence_reading, 0.38).
domain_priors:suppression_score(catastrophe_memory_kernel__survival_competence_reading, 0.22).
domain_priors:theater_ratio(catastrophe_memory_kernel__survival_competence_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, extractiveness, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__survival_competence_reading, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__survival_competence_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__survival_competence_reading, "Ritual as Survival-Competence Transmission").
narrative_ontology:topic_domain(catastrophe_memory_kernel__survival_competence_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__survival_competence_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__survival_competence_reading, '9ee82e86-ca3b-488d-addf-06d050ce1b71').
narrative_ontology:cs_kernel_codification('9ee82e86-ca3b-488d-addf-06d050ce1b71', distributed).
narrative_ontology:cs_authority_grounding('9ee82e86-ca3b-488d-addf-06d050ce1b71', practice).
narrative_ontology:cs_interpretation_layer_present('9ee82e86-ca3b-488d-addf-06d050ce1b71').
narrative_ontology:cs_reading_relation('9ee82e86-ca3b-488d-addf-06d050ce1b71', catastrophe_memory_kernel__boundary_maintenance_reading, influences).
narrative_ontology:cs_reading_relation('9ee82e86-ca3b-488d-addf-06d050ce1b71', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('9ee82e86-ca3b-488d-addf-06d050ce1b71', catastrophe_memory_kernel__trauma_encoding_reading, coexists_with).
narrative_ontology:cs_axiom('9ee82e86-ca3b-488d-addf-06d050ce1b71', foundational, ritual_encodes_operational_survival_competence).
narrative_ontology:cs_axiom_status(ritual_encodes_operational_survival_competence, holdable).
narrative_ontology:cs_axiom_grounding('9ee82e86-ca3b-488d-addf-06d050ce1b71', ritual_encodes_operational_survival_competence, empirically_contingent).
narrative_ontology:cs_axiom('9ee82e86-ca3b-488d-addf-06d050ce1b71', foundational, mourning_practice_is_survival_training).
narrative_ontology:cs_axiom_status(mourning_practice_is_survival_training, holdable).
narrative_ontology:cs_axiom_grounding('9ee82e86-ca3b-488d-addf-06d050ce1b71', mourning_practice_is_survival_training, empirically_contingent).
narrative_ontology:cs_reference_frame('9ee82e86-ca3b-488d-addf-06d050ce1b71', persecution_survival_repertoire_intact).
narrative_ontology:cs_drift_state('9ee82e86-ca3b-488d-addf-06d050ce1b71', contemporary_bureaucratic_erasure_era, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('9ee82e86-ca3b-488d-addf-06d050ce1b71', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__survival_competence_reading, persecuted_community_collective).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__survival_competence_reading, elder_ritual_practitioners).
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__survival_competence_reading, community_resilience_under_threat).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__survival_competence_reading, assimilation_pressure_vectors).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__survival_competence_reading, boundary_maintenance_cost_bearers).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__survival_competence_reading, ritual_as_adaptive_capacity).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__survival_competence_reading, mourning_practice_as_survival_training).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The community as a whole receives the adaptive capacity encoded in ritual — rehearsed response patterns for displacement, concealment, resource-hiding, and collective decision-making under threat. These patterns have historically enabled survival during pogroms, expulsions, and state persecution. The community cannot exit the need for this capacity without abandoning its identity; exit from the ritual system means loss of the transmitted competence.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, persecuted_community_collective, beneficiary,
    organized, generational, identity_locked, regional).

% Elders and designated ritual specialists (cantors, rabbis, lay leaders) maintain, adapt, and transmit the ritual repertoire. They decide which catastrophe-patterns are rehearsed, how intensely, and when to introduce variations. Their authority derives from lineage and demonstrated competence in the ritual system. They benefit from the status and continuity their role provides, but are constrained by the requirement to preserve functional efficacy — failed transmission means community vulnerability.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, elder_ritual_practitioners, agenda_setter,
    institutional, biographical, constrained, local).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_kernel__survival_competence_reading, elder_ritual_practitioners, beneficiary).

% State policies, majority-culture norms, economic incentives, and educational systems that reward shedding distinctive practices. These forces extract the cost of boundary maintenance from the community: every hour spent in ritual rehearsal is an hour not spent acquiring majority-culture capital; every distinctive marker maintained is a friction point in institutional navigation. The constraint's operation — ritual as survival-training — directly opposes assimilation by making distinctiveness functionally necessary, not merely symbolic.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, assimilation_pressure_vectors, payer,
    powerful, generational, mobile, global).

% Individual community members — particularly youth and those at the economic margins — who bear the daily costs of maintaining the ritual system: time, social friction, economic opportunity cost, and the psychological weight of catastrophe-rehearsal. They pay the extraction in the form of foregone assimilation benefits. Their exit is constrained: leaving the community means losing the mutual-aid network that the ritual system sustains, but staying means accepting the rehearsal burden.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, boundary_maintenance_cost_bearers, payer,
    moderate, biographical, constrained, local).

% Scholars who study the ritual system as a documented case of cultural adaptive capacity. They do not participate in the ritual but analyze its structural function, transmission fidelity, and historical efficacy. Their seat is analytical: they see the full pattern without bearing its costs or collecting its benefits.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__survival_competence_reading, diaspora_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Ritual rehearses and transmits the community's catastrophe-response repertoire: concealment protocols, resource-caching patterns, collective decision-making under surveillance, mutual-aid activation sequences, and identity-preservation practices that function under coercion. This solves the coordination problem of preserving operational competence across generations when the threat environment is intermittent but catastrophic — the community cannot wait for the next persecution to learn how to survive it.
% TRANSFER_FUNCTION: Moves time, attention, and psychological capacity from individual assimilation-pursuit into collective ritual rehearsal. The community pays in foregone majority-culture capital and daily friction; the ritual system returns a tested survival repertoire. The extraction is the assimilation benefits not captured; the gain is the competence preserved.
% ABSENT_VOICES: The would-be assimilated — community members who would exit the ritual system entirely if the mutual-aid network did not make exit prohibitively costly. They are structurally excluded from the ritual's governance because their departure would collapse the very resilience the system produces. Also absent: the majority-culture institutions that benefit from a docile, assimilated minority — they do not sit at the ritual table but shape the assimilation pressure that makes the ritual necessary.
% DISAPPEARANCE_RATIONALE: If the ritual-as-survival-training system vanished overnight, the community would lose its rehearsed catastrophe-response patterns. At the next persecution wave — state violence, expulsion, systematic discrimination — the community would lack the coordinated concealment, resource-hiding, and mutual-aid protocols that have historically enabled survival. The mutual-aid network itself, which runs on the ritual's trust infrastructure, would degrade. The world rearranges: a community that survives persecution becomes one that does not.
% FOUNDING_PROBLEM: How does a persecuted minority preserve the operational knowledge of how to survive catastrophe across generations when the catastrophe itself is intermittent, the majority culture suppresses transmission, and the community cannot rely on written manuals (which are confiscated, destroyed, or used as evidence)?
% FOUNDING_PROBLEM_CORROBORATION: Diaspora historians (observer seat) and comparative ethnographers corroborate the founding problem: the ritual repertoire matches documented survival needs across multiple persecution waves (medieval expulsions, early modern pogroms, 20th-century state violence). The community's own elders attest the problem is live — persecution patterns have mutated (surveillance, bureaucratic exclusion, cultural erasure) but not ceased. Assimilationist reformers within the community contest this, arguing the founding problem is dead in liberal democracies and the ritual now serves only boundary-maintenance.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__survival_competence_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__survival_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(catastrophe_memory_kernel__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__survival_competence_reading, 0.38, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__survival_competence_reading_tests).
:- end_tests(catastrophe_memory_kernel__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.38) is moderate: the ritual system extracts time and assimilation-opportunity from members, but returns tested survival competence. The extraction is not monopoly rent — it is the price of a coordination function that has no market substitute. Suppression (0.22) is low-to-moderate: the constraint persists through internalized commitment and mutual-aid dependence, not external coercion. Theater (0.15) is low: the rehearsal is functional, not performative — but a slow rise over the interval reflects periods when persecution recedes and the ritual risks becoming symbolic rather than operational. Accessibility collapse (0.45) is moderate: alternatives (written manuals, ad-hoc learning) exist but fail under the conditions the ritual is designed for (intermittent catastrophe, suppressed transmission). Resistance (0.35) is moderate: youth and marginal members resist the rehearsal burden, especially when persecution seems distant; elders enforce participation through moral authority and mutual-aid gatekeeping.
 *
 * PERSPECTIVAL GAP:
 *   From the elder practitioner seat, the constraint is genuine coordination (rope-like): they see the repertoire, know its history, and enforce its transmission because it works. From the boundary_maintenance_cost_bearer seat, the same constraint feels extractive (snare-adjacent): the rehearsal burden is immediate and the catastrophe is abstract. From the diaspora historian seat, the constraint is a documented adaptive system (mountain-adjacent in its structural elegance). The engine computes this divergence from the power/exit/role declarations — the authored claim (tangled_rope) does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   The persecuted community collective is the structural beneficiary (d near 0.0): the constraint subsidizes its survival. Elder practitioners sit near symmetric (d ~0.5): they administer the system and benefit from status, but bear the burden of fidelity. Assimilation pressure vectors are not 'targets' in the extraction sense — they are the environmental pressure the constraint evolves to resist. The constraint extracts from boundary_maintenance_cost_bearers (individual members, d near 0.7): they pay the rehearsal cost in foregone assimilation. The identity_locked exit of the collective and the constrained exit of individuals reflect that leaving the ritual system means losing the mutual-aid network that makes survival possible — exit is structurally inhibited by the very competence the ritual preserves.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (surviving intermittent catastrophe) is contested as live vs. dead. If dead, the constraint becomes mandatrophic — a ritual system whose survival function has atrophied but which persists through boundary-maintenance inertia. The current metrics (moderate extractiveness, low theater, active enforcement) suggest the system is still functionally engaged, not yet a piton. But the rising theater_ratio and the assimilationist reformer contestation signal a drift trajectory toward mandatrophy if persecution pressure continues to mutate toward bureaucratic/cultural erasure rather than acute violence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    survival_competence_vs_boundary_maintenance,
    'Is the ritual''s adaptive capacity for persecution-survival structurally distinct from its boundary-maintenance function, or are they the same mechanism described from different angles?',
    'Compare ritual elements across communities with different persecution histories: if the same ritual elements serve survival-training in high-persecution contexts and pure boundary-maintenance in low-persecution contexts, the functions are separable; if they co-vary perfectly, they are one mechanism.',
    'If separable, the survival_competence_reading and boundary_maintenance_reading describe two constraints that can be independently gained or lost. If unified, they are one constraint with two observational facets — the ε-invariance principle would require a single story.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(survival_competence_vs_boundary_maintenance, conceptual, 'Whether survival-training and boundary-maintenance are one constraint or two.').

omega_variable(
    persecution_mutation_adaptation_gap,
    'Does the ritual repertoire adapt fast enough to track mutation in persecution modalities (from acute violence to bureaucratic exclusion to algorithmic surveillance), or is there a structural lag that degrades its survival value?',
    'Longitudinal ethnography of ritual change rates vs. persecution-modality change rates; war-gaming exercises testing ritual protocols against novel threat scenarios.',
    'A structural lag means the constraint''s coordination function is degrading — extractiveness may rise (rehearsal continues) while survival value falls, pushing toward piton or snare classification. No lag means the tangled_rope classification is stable.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(persecution_mutation_adaptation_gap, empirical, 'Whether ritual adaptation tracks persecution mutation.').

omega_variable(
    kernel_reading_frame_dependence,
    'Does this reading''s classification (tangled_rope) depend on framing the kernel as ''survival-competence transmission'' rather than ''boundary-maintenance'' or ''trauma-encoding''?',
    'Re-author the constraint with the same structural data but the boundary_maintenance_reading or trauma_encoding_reading framing; compare engine outputs. If classification changes with framing alone, the ε-invariance principle is violated — the kernel contains multiple constraints.',
    'If framing changes classification, the kernel decomposes into multiple constraints (as the BGS decomposition). If classification is framing-invariant, the reading is a stable perspective on one constraint.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_frame_dependence, conceptual, 'Whether the reading''s classification is framing-invariant per ε-invariance.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__survival_competence_reading, 0, 120).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catastrophe_memory_kernel__survival_competence_reading_tr_t0, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(catastrophe_memory_kernel__survival_competence_reading_tr_t20, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(catastrophe_memory_kernel__survival_competence_reading_tr_t40, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 40, 0.12).
narrative_ontology:measurement(catastrophe_memory_kernel__survival_competence_reading_tr_t60, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 60, 0.13).
narrative_ontology:measurement(catastrophe_memory_kernel__survival_competence_reading_tr_t80, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 80, 0.14).
narrative_ontology:measurement(catastrophe_memory_kernel__survival_competence_reading_tr_t100, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 100, 0.15).
narrative_ontology:measurement(catastrophe_memory_kernel__survival_competence_reading_tr_t120, catastrophe_memory_kernel__survival_competence_reading, theater_ratio, 120, 0.15).

% Extraction over time
narrative_ontology:measurement(catastrophe_memory_kernel__survival_competence_reading_be_t0, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(catastrophe_memory_kernel__survival_competence_reading_be_t20, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 20, 0.28).
narrative_ontology:measurement(catastrophe_memory_kernel__survival_competence_reading_be_t40, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 40, 0.32).
narrative_ontology:measurement(catastrophe_memory_kernel__survival_competence_reading_be_t60, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 60, 0.35).
narrative_ontology:measurement(catastrophe_memory_kernel__survival_competence_reading_be_t80, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 80, 0.37).
narrative_ontology:measurement(catastrophe_memory_kernel__survival_competence_reading_be_t100, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 100, 0.38).
narrative_ontology:measurement(catastrophe_memory_kernel__survival_competence_reading_be_t120, catastrophe_memory_kernel__survival_competence_reading, base_extractiveness, 120, 0.38).

% Suppression requirement over time
narrative_ontology:measurement(catastrophe_memory_kernel__survival_competence_reading_su_t0, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(catastrophe_memory_kernel__survival_competence_reading_su_t20, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 20, 0.18).
narrative_ontology:measurement(catastrophe_memory_kernel__survival_competence_reading_su_t40, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 40, 0.2).
narrative_ontology:measurement(catastrophe_memory_kernel__survival_competence_reading_su_t60, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 60, 0.21).
narrative_ontology:measurement(catastrophe_memory_kernel__survival_competence_reading_su_t80, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 80, 0.22).
narrative_ontology:measurement(catastrophe_memory_kernel__survival_competence_reading_su_t100, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 100, 0.22).
narrative_ontology:measurement(catastrophe_memory_kernel__survival_competence_reading_su_t120, catastrophe_memory_kernel__survival_competence_reading, suppression_requirement, 120, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__survival_competence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_kernel__survival_competence_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__boundary_maintenance_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__survival_competence_reading, catastrophe_memory_kernel__trauma_encoding_reading).

% DUAL FORMULATION NOTE:
% This constraint and its three siblings form the catastrophe_memory_kernel constraint family. All four share the same ritual repertoire as referent but differ in the payload they identify as transmitted: survival-competence (this reading), boundary-enforcement, symbolic-continuity, trauma-encoding. The survival_competence_reading is upstream: its operational repertoire is what the boundary_maintenance_reading enforces, the symbol_continuity_reading preserves, and the trauma_encoding_reading warns about. Network edges reflect this causal priority.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_kernel__survival_competence_reading, organized, 0.1).
constraint_indexing:directionality_override(catastrophe_memory_kernel__survival_competence_reading, institutional, 0.35).
constraint_indexing:directionality_override(catastrophe_memory_kernel__survival_competence_reading, powerful, 0.85).
constraint_indexing:directionality_override(catastrophe_memory_kernel__survival_competence_reading, moderate, 0.7).
constraint_indexing:directionality_override(catastrophe_memory_kernel__survival_competence_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
