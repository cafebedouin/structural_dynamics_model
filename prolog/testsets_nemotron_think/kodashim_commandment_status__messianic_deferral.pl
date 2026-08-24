% ============================================================================
% CONSTRAINT STORY: kodashim_commandment_status__messianic_deferral
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kodashim_commandment_status__messianic_deferral, []).

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
 *   constraint_id: kodashim_commandment_status__messianic_deferral
 *   human_readable: Kodashim Commandment Status — Messianic Deferral Reading
 *   domain: religious/halakhic/commitment_system
 *
 * SUMMARY:
 *   The messianic_deferral reading of kodashim commandment status holds that
 *   Temple sacrificial laws remain binding in suspended form — the
 *   commandment is not obsolete but awaits messianic restoration. Study of
 *   these laws (seder kodashim) is obligatory not as academic exercise but as
 *   operational readiness maintenance. This reading instantiates a
 *   tangled_rope: it performs a genuine coordination function (preserving a
 *   complex legal corpus across millennia of exile) while extracting
 *   opportunity costs from the present generation whose material needs are
 *   subordinated to the messianic contingency. The coordination function is
 *   real — without this framework, the sacrificial law would likely have been
 *   lost — but the extraction is asymmetric: the authorities and institutions
 *   that maintain the framework benefit from its continuity, while the
 *   present generation bears the cost.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kodashim_commandment_status__messianic_deferral, 0.42).
domain_priors:suppression_score(kodashim_commandment_status__messianic_deferral, 0.38).
domain_priors:theater_ratio(kodashim_commandment_status__messianic_deferral, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, extractiveness, 0.42).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, suppression_requirement, 0.38).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, accessibility_collapse, 0.55).
narrative_ontology:constraint_metric(kodashim_commandment_status__messianic_deferral, resistance, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kodashim_commandment_status__messianic_deferral, tangled_rope).
narrative_ontology:human_readable(kodashim_commandment_status__messianic_deferral, "Kodashim Commandment Status — Messianic Deferral Reading").
narrative_ontology:topic_domain(kodashim_commandment_status__messianic_deferral, "religious/halakhic/commitment_system").

domain_priors:requires_active_enforcement(kodashim_commandment_status__messianic_deferral).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kodashim_commandment_status__messianic_deferral, '4a9476ed-60d5-4268-9061-deb57e1aa210').
narrative_ontology:cs_kernel_codification('4a9476ed-60d5-4268-9061-deb57e1aa210', formalized).
narrative_ontology:cs_authority_grounding('4a9476ed-60d5-4268-9061-deb57e1aa210', lineage).
narrative_ontology:cs_interpretation_layer_present('4a9476ed-60d5-4268-9061-deb57e1aa210').
narrative_ontology:cs_reading_relation('4a9476ed-60d5-4268-9061-deb57e1aa210', kodashim_commandment_status__study_as_performance, coexists_with).
narrative_ontology:cs_reading_relation('4a9476ed-60d5-4268-9061-deb57e1aa210', kodashim_commandment_status__performance_only, forecloses).
narrative_ontology:cs_axiom('4a9476ed-60d5-4268-9061-deb57e1aa210', foundational, commandment_persists_in_suspended_form).
narrative_ontology:cs_axiom_status(commandment_persists_in_suspended_form, holdable).
narrative_ontology:cs_axiom_grounding('4a9476ed-60d5-4268-9061-deb57e1aa210', commandment_persists_in_suspended_form, theological).
narrative_ontology:cs_axiom('4a9476ed-60d5-4268-9061-deb57e1aa210', secondary, study_as_readiness_maintenance).
narrative_ontology:cs_axiom_status(study_as_readiness_maintenance, holdable).
narrative_ontology:cs_axiom_grounding('4a9476ed-60d5-4268-9061-deb57e1aa210', study_as_readiness_maintenance, theological).
narrative_ontology:cs_reference_frame('4a9476ed-60d5-4268-9061-deb57e1aa210', covenantal_continuity_through_exile).
narrative_ontology:cs_drift_state('4a9476ed-60d5-4268-9061-deb57e1aa210', contemporary_secular_modernity, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4a9476ed-60d5-4268-9061-deb57e1aa210', '').
narrative_ontology:cs_kernel_id(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, rabbinic_authorities).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, yeshiva_institutions).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, messianic_continuity_framework).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, present_generation_community_members).
narrative_ontology:constraint_victim(kodashim_commandment_status__messianic_deferral, material_needs_deferred).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kodashim_commandment_status__messianic_deferral, future_restored_community).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__messianic_deferral, covenantal_permanence_doctrine).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__messianic_deferral, torah_eternality_principle).
narrative_ontology:constraint_vindicates(kodashim_commandment_status__messianic_deferral, messianic_restoration_certainty).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Define and enforce the halakhic framework that maintains kodashim study as obligatory preparation for Temple restoration. Their authority derives from the chain of transmission; they administer curricula, allocate communal resources to yeshivas, and determine the boundaries of legitimate interpretation. Exit would mean abandoning the vocational and identity structure that constitutes their role.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, rabbinic_authorities, agenda_setter,
    institutional, generational, identity_locked, global).

% Receive communal funding, prestige, and student bodies organized around kodashim study as a core curriculum. They structure their educational mission around the deferral narrative. Could theoretically pivot to other areas of Torah study, but the institutional identity and donor base are built on maintaining the full halakhic corpus including sacrificial law.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, yeshiva_institutions, beneficiary,
    organized, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(kodashim_commandment_status__messianic_deferral, yeshiva_institutions, agenda_setter).

% Bear the opportunity cost of time, attention, and communal resources directed toward studying laws with no current practical application. Their material needs (housing, healthcare, livelihood, secular education) are structurally subordinated to the messianic preparation framework. Exit options are constrained by community belonging, family ties, and the identity cost of leaving the interpretive framework.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, present_generation_community_members, payer,
    moderate, biographical, constrained, local).

% The concrete unmet needs — poverty relief, mental health services, vocational training, elderly care — that receive reduced communal funding because resources flow to yeshiva infrastructure and kodashim curriculum. These needs have no voice in halakhic priority-setting; they are represented only indirectly through communal welfare organizations that operate within the same authority structure.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, material_needs_deferred, payer,
    powerless, immediate, trapped, local).

% The hypothetical restored community that would inherit preserved sacrificial law knowledge. They benefit if restoration occurs and the knowledge is intact. They cannot advocate for themselves; their interests are represented only through the authorities who claim to act on their behalf.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, future_restored_community, beneficiary,
    powerless, civilizational, analytical, universal).

% The voices arguing that communal resources should prioritize present human welfare over messianic contingency — advocates for secular education, professional integration, social services. They are structurally excluded from halakhic decision-making because the authority framework does not recognize secular welfare as a competing halakhic value.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, secular_modern_needs, excluded,
    moderate, biographical, mobile, national).

% Scholars of halakhic history and sociology who analyze the deferral framework as a institutional adaptation strategy. They observe the resource flows, the identity maintenance function, and the contested status of the founding problem without participating in the authority structure.
narrative_ontology:constraint_stakeholder(kodashim_commandment_status__messianic_deferral, academic_halakhic_observers, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Preserving the complete sacrificial law corpus (kodashim) across the exile period so that operational knowledge survives intact for immediate implementation upon Temple restoration, maintaining covenantal continuity despite the absence of the cultic center.
% TRANSFER_FUNCTION: Moves present-generation intellectual labor, communal funding, curricular time, and halakhic authority attention from immediate material and spiritual needs to the maintenance of dormant legal frameworks that have no current practical application.
% ABSENT_VOICES: Those whose material needs — poverty relief, healthcare access, vocational training, mental health services, secular education — are structurally deprioritized in favor of messianic preparation. They exist within the community but are excluded from halakhic priority-setting because the authority framework recognizes only covenantal obligations, not welfare claims, as legitimate resource claims.
% DISAPPEARANCE_RATIONALE: If the messianic deferral framework vanished overnight, yeshiva curricula would shift away from kodashim toward practically applicable halakha; communal funding would redirect toward present welfare needs; rabbinic authority would lose a major organizing narrative for institutional maintenance; the covenantal continuity claim would fracture, forcing a reorganization of Jewish legal self-understanding.
% FOUNDING_PROBLEM: How to maintain covenantal continuity and the integrity of sacrificial law during exile without a Temple, given the divine promise of eventual restoration.
% FOUNDING_PROBLEM_CORROBORATION: Classical sources (Rambam, Raavad, Talmudic discussions of kodashim in exile) attest the founding problem was live in the classical period. Contemporary critics outside the beneficiary institutions — including religious Zionist thinkers who argue restoration has begun, academic historians who note the problem's transformation under modernity, and communal welfare advocates who argue present needs constitute a new halakhic priority — corroborate that the founding problem's status is disputed, not settled.
narrative_ontology:disappearance_verdict(kodashim_commandment_status__messianic_deferral, world_rearranges).
narrative_ontology:founding_problem_status(kodashim_commandment_status__messianic_deferral, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kodashim_commandment_status__messianic_deferral, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-23',
    'no_scope_rebuild_nemotron_think', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=model_default').
narrative_ontology:story_seed(kodashim_commandment_status__messianic_deferral, 'none', 1).
narrative_ontology:epsilon_provenance(kodashim_commandment_status__messianic_deferral, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kodashim_commandment_status__messianic_deferral_tests).
:- end_tests(kodashim_commandment_status__messianic_deferral_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.42) reflects substantial but not total opportunity cost — kodashim study occupies significant curricular space but not the entirety of halakhic education. Suppression (0.38) is moderate: the framework persists through institutional inertia and identity formation rather than overt coercion; exit is constrained but not impossible. Theater ratio (0.28) indicates some performative maintenance — study continues even as restoration recedes — but the core preservation function remains genuine. Accessibility collapse (0.55) is middling: alternative frameworks (study_as_performance, performance_only) exist and are known, but the deferral narrative maintains strong institutional hold. Resistance (0.35) is present but fragmented — critics exist but lack a unified alternative authority structure.
 *
 * PERSPECTIVAL GAP:
 *   From the agenda_setter seat (rabbinic authorities), the constraint appears as genuine coordination — they preserve a divine trust. From the payer seats (present generation, material needs), the same structure appears as extraction — resources flow to an institution that serves a future that may never arrive. The engine computes this divergence from the structural data; the claimed_type (tangled_rope) captures the structural reality that both coordination and extraction are simultaneously true.
 *
 * DIRECTIONALITY LOGIC:
 *   Rabbinic authorities and yeshiva institutions are structural beneficiaries (d ~ 0.15-0.25): they collect institutional prestige, funding, and identity from maintaining the framework. Present-generation community members are targets (d ~ 0.65-0.75): they bear opportunity costs with constrained exit. Material_needs_deferred are fully trapped targets (d ~ 0.9): they have no voice and bear diffuse costs. Future_restored_community is a theoretical beneficiary at civilizational horizon (d ~ 0.0) but cannot advocate. Secular_modern_needs are excluded (mobile exit) — they would redirect resources but are outside the authority frame. The identity_locked exit for authorities reflects vocational and identity fusion: their role IS the maintenance of this framework.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (covenantal continuity without Temple) was live in 70 CE. Its status is now contested: religious Zionists argue restoration has begun (changing the problem), historians argue modernity has transformed the conditions, welfare advocates argue present needs constitute a new obligation. The deferral framework persists despite contested founding problem status — this is the mandatrophy signature. The constraint has not resolved its mandatrophy; it maintains itself through institutional inertia and identity_locked authorities.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    committer_frame_kernel_reading,
    'How does the structural classification change across the three readings of the kodashim_commandment_status kernel?',
    'Author separate constraint stories for each reading (messianic_deferral, study_as_performance, performance_only) with independent ε, beneficiaries, victims, and claimed_type. Compare computed seat classifications across the family.',
    'If messianic_deferral computes as tangled_rope while study_as_performance computes as rope (coordination without extraction) and performance_only computes as mountain (no current obligation, no extraction), the kernel decomposition is validated. If all three compute similarly, the readings may not be structurally distinct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(committer_frame_kernel_reading, conceptual, 'Whether the three declared readings of the kodashim kernel are structurally distinct constraints per ε-invariance.').

omega_variable(
    coordination_genuineness,
    'Is the kodashim preservation function genuinely coordination (knowledge would be lost without the framework) or has it become a cover story for institutional maintenance?',
    'Counterfactual historical analysis: in communities that abandoned kodashim study, was the knowledge actually lost? Compare Karaite, Reform, and secular Jewish trajectories. If knowledge survives without the deferral framework, the coordination claim weakens.',
    'If coordination is genuine, tangled_rope holds. If coordination is cover, the constraint reclassifies toward snare — extraction without coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coordination_genuineness, empirical, 'Whether the preservation coordination function is structurally necessary or institutionally constructed.').

omega_variable(
    messianic_contingency_sincerity,
    'Do the agenda_setters genuinely believe in imminent messianic restoration, or has the contingency become a rhetorical device for institutional perpetuation?',
    'Analyze internal discourse: do authorities allocate resources as if restoration is imminent (e.g., training kohanim for actual service, preparing architectural plans)? Or is the language purely symbolic? Track resource allocation patterns over time.',
    'If belief is sincere, the extraction is a genuine insurance premium. If belief is performative, the extraction is rent-seeking masked as contingency preparation — pushing toward snare classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(messianic_contingency_sincerity, preference, 'Whether the messianic contingency is a genuine belief structuring resource allocation or a performative justification.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (institutional barriers to exit) or internalized (community members believe subordinating present needs to messianic preparation is religiously correct)?',
    'Post-exit trajectory study: track individuals who leave the deferral framework — do they continue to feel religious obligation toward kodashim study, or does the suppression dissolve? If suppression persists after exit, it is partially internalized.',
    'If internalized, effective suppression is higher than structural measure suggests — the constraint travels with the agent. This would increase χ for payer seats beyond the engine''s structural calculation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in the deferral framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kodashim_commandment_status__messianic_deferral, 70, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kodashim_messianic_deferral_tr_t70, kodashim_commandment_status__messianic_deferral, theater_ratio, 70, 0.1).
narrative_ontology:measurement(kodashim_messianic_deferral_tr_t500, kodashim_commandment_status__messianic_deferral, theater_ratio, 500, 0.12).
narrative_ontology:measurement(kodashim_messianic_deferral_tr_t1000, kodashim_commandment_status__messianic_deferral, theater_ratio, 1000, 0.15).
narrative_ontology:measurement(kodashim_messianic_deferral_tr_t1500, kodashim_commandment_status__messianic_deferral, theater_ratio, 1500, 0.2).
narrative_ontology:measurement(kodashim_messianic_deferral_tr_t1800, kodashim_commandment_status__messianic_deferral, theater_ratio, 1800, 0.25).
narrative_ontology:measurement(kodashim_messianic_deferral_tr_t1948, kodashim_commandment_status__messianic_deferral, theater_ratio, 1948, 0.27).
narrative_ontology:measurement(kodashim_messianic_deferral_tr_t1967, kodashim_commandment_status__messianic_deferral, theater_ratio, 1967, 0.28).
narrative_ontology:measurement(kodashim_messianic_deferral_tr_t2024, kodashim_commandment_status__messianic_deferral, theater_ratio, 2024, 0.28).

% Extraction over time
narrative_ontology:measurement(kodashim_messianic_deferral_be_t70, kodashim_commandment_status__messianic_deferral, base_extractiveness, 70, 0.15).
narrative_ontology:measurement(kodashim_messianic_deferral_be_t500, kodashim_commandment_status__messianic_deferral, base_extractiveness, 500, 0.22).
narrative_ontology:measurement(kodashim_messianic_deferral_be_t1000, kodashim_commandment_status__messianic_deferral, base_extractiveness, 1000, 0.28).
narrative_ontology:measurement(kodashim_messianic_deferral_be_t1500, kodashim_commandment_status__messianic_deferral, base_extractiveness, 1500, 0.35).
narrative_ontology:measurement(kodashim_messianic_deferral_be_t1800, kodashim_commandment_status__messianic_deferral, base_extractiveness, 1800, 0.38).
narrative_ontology:measurement(kodashim_messianic_deferral_be_t1948, kodashim_commandment_status__messianic_deferral, base_extractiveness, 1948, 0.4).
narrative_ontology:measurement(kodashim_messianic_deferral_be_t1967, kodashim_commandment_status__messianic_deferral, base_extractiveness, 1967, 0.42).
narrative_ontology:measurement(kodashim_messianic_deferral_be_t2024, kodashim_commandment_status__messianic_deferral, base_extractiveness, 2024, 0.42).

% Suppression requirement over time
narrative_ontology:measurement(kodashim_messianic_deferral_su_t70, kodashim_commandment_status__messianic_deferral, suppression_requirement, 70, 0.2).
narrative_ontology:measurement(kodashim_messianic_deferral_su_t500, kodashim_commandment_status__messianic_deferral, suppression_requirement, 500, 0.25).
narrative_ontology:measurement(kodashim_messianic_deferral_su_t1000, kodashim_commandment_status__messianic_deferral, suppression_requirement, 1000, 0.3).
narrative_ontology:measurement(kodashim_messianic_deferral_su_t1500, kodashim_commandment_status__messianic_deferral, suppression_requirement, 1500, 0.33).
narrative_ontology:measurement(kodashim_messianic_deferral_su_t1800, kodashim_commandment_status__messianic_deferral, suppression_requirement, 1800, 0.35).
narrative_ontology:measurement(kodashim_messianic_deferral_su_t1948, kodashim_commandment_status__messianic_deferral, suppression_requirement, 1948, 0.37).
narrative_ontology:measurement(kodashim_messianic_deferral_su_t1967, kodashim_commandment_status__messianic_deferral, suppression_requirement, 1967, 0.38).
narrative_ontology:measurement(kodashim_messianic_deferral_su_t2024, kodashim_commandment_status__messianic_deferral, suppression_requirement, 2024, 0.38).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kodashim_commandment_status__messianic_deferral, identity_coordination).
narrative_ontology:boltzmann_floor_override(kodashim_commandment_status__messianic_deferral, 0.08).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status__study_as_performance).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, kodashim_commandment_status__performance_only).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, halakhic_authority_structure__exile_continuity).
narrative_ontology:affects_constraint(kodashim_commandment_status__messianic_deferral, communal_resource_allocation__yeshiva_funding).

% DUAL FORMULATION NOTE:
% This is the messianic_deferral reading of the kodashim_commandment_status kernel. It differs from study_as_performance (ε ≈ 0.15, claimed rope — study IS fulfillment, no extraction) and performance_only (ε ≈ 0.05, claimed mountain — commandment suspended, no current obligation). The three readings share the kernel but instantiate different constraints with different ε, beneficiaries, and victims. Linked via affects_constraints for contamination analysis.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(kodashim_commandment_status__messianic_deferral, institutional, 0.15).
constraint_indexing:directionality_override(kodashim_commandment_status__messianic_deferral, organized, 0.25).
constraint_indexing:directionality_override(kodashim_commandment_status__messianic_deferral, moderate, 0.7).
constraint_indexing:directionality_override(kodashim_commandment_status__messianic_deferral, powerless, 0.9).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
