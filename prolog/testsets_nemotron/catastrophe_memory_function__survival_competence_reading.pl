% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_function__survival_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_function__survival_competence_reading, []).

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
 *   constraint_id: catastrophe_memory_function__survival_competence_reading
 *   human_readable: Catastrophe Memory as Survival-Competence Transmission (D5 Reading)
 *   domain: religious_studies/ritual_theory/collective_memory
 *
 * SUMMARY:
 *   This constraint story instantiates the survival_competence_reading of the
 *   catastrophe_memory_function kernel. It models commemorative ritual not as
 *   mourning-practice (D1/D4) but as a distributed transmission system for
 *   survival-competence (D5) — the adaptive capacity to endure and transform
 *   institutions through catastrophe. The paradigmatic case is Passover: the
 *   seder rehearses the transition from slavery to freedom as embodied
 *   knowledge, transmitting not just memory but the operational grammar of
 *   liberation. The constraint coordinates practitioner communities across
 *   generations without central enforcement, using ritual as an embodied
 *   epistemology that survives institutional collapse. Extraction is low
 *   because participation is voluntary and the gains (resilience, continuity)
 *   are distributed; suppression is low because exit is open (communities
 *   that abandon the ritual simply lose its adaptive benefit); theater is low
 *   because the rehearsal is the function.
 *
 * KEY AGENTS:
 *   - practitioner_communities: Primary beneficiaries (organized/biographical/constrained) — enact the ritual, receive adaptive capacity
 *   - transmission_lineages: Agenda-setters (organized/generational/arbitrage) — maintain the ritual form, authorize modifications, coordinate across communities
 *   - decentralized_resilience_networks: Beneficiaries (organized/civilizational/mobile) — inherit the adaptive grammar without necessarily maintaining the ritual form
 *   - scholarly_observers: Observers (analytical/civilizational/analytical) — analyze the transmission system from outside
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_function__survival_competence_reading, 0.18).
domain_priors:suppression_score(catastrophe_memory_function__survival_competence_reading, 0.22).
domain_priors:theater_ratio(catastrophe_memory_function__survival_competence_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, extractiveness, 0.18).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(catastrophe_memory_function__survival_competence_reading, resistance, 0.12).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_function__survival_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_function__survival_competence_reading, "Catastrophe Memory as Survival-Competence Transmission (D5 Reading)").
narrative_ontology:topic_domain(catastrophe_memory_function__survival_competence_reading, "religious_studies/ritual_theory/collective_memory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_function__survival_competence_reading, 'd0e9cfc1-058b-446a-9157-b3352eef9aad').
narrative_ontology:cs_kernel_codification('d0e9cfc1-058b-446a-9157-b3352eef9aad', distributed).
narrative_ontology:cs_authority_grounding('d0e9cfc1-058b-446a-9157-b3352eef9aad', practice).
narrative_ontology:cs_interpretation_layer_present('d0e9cfc1-058b-446a-9157-b3352eef9aad').
narrative_ontology:cs_reading_relation('d0e9cfc1-058b-446a-9157-b3352eef9aad', catastrophe_memory_function__mourning_practice_reading, coexists_with).
narrative_ontology:cs_reading_relation('d0e9cfc1-058b-446a-9157-b3352eef9aad', catastrophe_memory_function__hybrid_transformation_reading, influences).
narrative_ontology:cs_axiom('d0e9cfc1-058b-446a-9157-b3352eef9aad', foundational, survival_competence_is_load_bearing_function).
narrative_ontology:cs_axiom_status(survival_competence_is_load_bearing_function, holdable).
narrative_ontology:cs_axiom_grounding('d0e9cfc1-058b-446a-9157-b3352eef9aad', survival_competence_is_load_bearing_function, empirically_contingent).
narrative_ontology:cs_axiom('d0e9cfc1-058b-446a-9157-b3352eef9aad', foundational, embodied_rehearsal_necessary_for_institutional_transformation).
narrative_ontology:cs_axiom_status(embodied_rehearsal_necessary_for_institutional_transformation, holdable).
narrative_ontology:cs_axiom_grounding('d0e9cfc1-058b-446a-9157-b3352eef9aad', embodied_rehearsal_necessary_for_institutional_transformation, empirically_contingent).
narrative_ontology:cs_reference_frame('d0e9cfc1-058b-446a-9157-b3352eef9aad', pre_institutional_survival_grammar).
narrative_ontology:cs_drift_state('d0e9cfc1-058b-446a-9157-b3352eef9aad', contemporary_resilience_crisis, gap(revival_pressure, substantial, false)).
narrative_ontology:cs_created_at('d0e9cfc1-058b-446a-9157-b3352eef9aad', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, practitioner_communities).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, transmission_lineages).
narrative_ontology:constraint_beneficiary(catastrophe_memory_function__survival_competence_reading, decentralized_resilience_networks).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__survival_competence_reading, ritual_as_embodied_epistemology).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__survival_competence_reading, distributed_memory_survives_institutional_collapse).
narrative_ontology:constraint_vindicates(catastrophe_memory_function__survival_competence_reading, adaptive_capacity_through_rehearsal).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Enact the ritual annually (e.g., Passover seder, Tisha B'Av, disaster commemorations). Bear participation costs: time, dietary restrictions, cognitive load of rehearsal. Receive adaptive capacity: embodied grammar for institutional transformation, distributed memory that survives collapse, communal cohesion under pressure. Exit is constrained — leaving the community means losing the rehearsal, but communities can and do modify or abandon the ritual without penalty.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, practitioner_communities, beneficiary,
    organized, biographical, constrained, global).

% Maintain the ritual form across generations (rabbinic authorities, familial transmission chains, communal elders). Authorize modifications (new liturgy, adapted practice) while preserving the adaptive grammar. Collect interpretive authority and communal status from the role. Exit is arbitrage-grade: lineages can shift emphasis, join other traditions, or secularize the grammar without losing their core function.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, transmission_lineages, agenda_setter,
    organized, generational, arbitrage, global).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_function__survival_competence_reading, transmission_lineages, beneficiary).

% Inherit the adaptive grammar without necessarily maintaining the ritual form (e.g., secular liberation movements using Exodus narrative, disaster response networks using communal rehearsal models). Collect the survival-competence payload — the transferable operational grammar — without the commemorative vehicle. Exit is mobile: networks adopt, adapt, or discard the grammar as conditions change.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, decentralized_resilience_networks, beneficiary,
    organized, civilizational, mobile, global).

% Analyze the transmission system from outside: historians of religion, ritual theorists, cognitive anthropologists, resilience researchers. Neither collect nor pay; they map the structure. Their exit is analytical — they can change frameworks without personal cost.
narrative_ontology:constraint_stakeholder(catastrophe_memory_function__survival_competence_reading, scholarly_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Transmits survival-competence (adaptive capacity for institutional transformation and decentralized continuity) across generations without central enforcement, using embodied rehearsal as the transmission medium. Solves the problem: how does a community preserve the knowledge of how to survive catastrophe when institutions that normally carry knowledge collapse?
% TRANSFER_FUNCTION: Moves adaptive grammar (the operational knowledge of liberation, resilience, transformation) from transmission_lineages to practitioner_communities and decentralized_resilience_networks, via the vehicle of commemorative ritual. The commemorative frame (mourning, boundary-maintenance) is the carrier wave; the survival-competence is the payload.
% ABSENT_VOICES: Communities that experienced catastrophe but did not develop ritualized survival-transmission (or lost it) — their absence is structural: the constraint only exists where the transmission succeeded. Also absent: communities that maintain the commemorative frame but have lost the survival-competence payload (hollowed-out ritual) — they would testify that the coordination function has degraded.
% DISAPPEARANCE_RATIONALE: If this constraint vanished overnight, practitioner communities would lose their primary embodied rehearsal for institutional transformation. Transmission lineages would lose their coordinating function. Decentralized resilience networks would lose a proven adaptive grammar. Alternative transmission modes (texts, oral instruction, digital archives) would partially compensate but lack the embodied, distributed, institution-surviving properties of ritual rehearsal. The world would rearrange: communities facing catastrophe would be less prepared.
% FOUNDING_PROBLEM: How to transmit the knowledge of how to survive and transform through catastrophe across generations when every institution that carries knowledge (temples, states, schools, families) can be destroyed. The founding problem is the fragility of institutional memory itself.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem is attested by the historical record: communities that maintained ritualized survival-rehearsal (Jewish communities through exile, African diaspora communities through slavery, Indigenous communities through colonization) demonstrated measurable adaptive capacity that communities without such rehearsal lacked. This is corroborated by historical sociologists (e.g., Armstrong on rituals of resistance), resilience theorists (e.g., Folke on social memory), and cognitive anthropologists (e.g., Whitehouse on ritual modes) — all outside the beneficiary communities.
narrative_ontology:disappearance_verdict(catastrophe_memory_function__survival_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_function__survival_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_function__survival_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(catastrophe_memory_function__survival_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_function__survival_competence_reading, 0.18, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_function__survival_competence_reading_tests).
:- end_tests(catastrophe_memory_function__survival_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The claimed_type is rope: a genuine coordination problem (transmitting survival-competence across generations without central authority) solved with minimal coercive overhead. Extractiveness (0.18) reflects the cost of participation (time, cognitive load, dietary restrictions) — real but not extractive because participants are net beneficiaries. Suppression (0.22) reflects the mild social pressure to participate within communities that maintain the ritual, not enforcement against exit. Theater ratio (0.15) acknowledges that some ritual elements become performative over time, but the core rehearsal remains functional. Accessibility_collapse (0.65) is moderate-high: once a community understands the ritual as survival-transmission, alternative transmission modes (texts, oral instruction alone) are recognized as incomplete — the embodied rehearsal is not fully substitutable. Resistance (0.12) is low: the constraint meets little active resistance because it delivers genuine adaptive value. The measurement series spans ~1500 years (0 = late Second Temple period, 1500 = contemporary), showing extractiveness and theater rising modestly as institutional layers accumulate, then stabilizing.
 *
 * PERSPECTIVAL GAP:
 *   From the practitioner_community seat, the constraint is experienced as rope (genuine coordination, voluntary participation, net benefit). From the transmission_lineage seat, it approaches mountain-like stability (the form persists because it works, not because it's enforced). From the scholarly_observer seat, the low extraction and suppression confirm rope classification. The engine computes this seat divergence from the structural data: different power/exit profiles yield different effective extraction values from the same base ε.
 *
 * DIRECTIONALITY LOGIC:
 *   Practitioner communities (organized, biographical, constrained exit) are primary beneficiaries: they bear the participation cost but receive the adaptive capacity directly. Their directionality is near-symmetric (d ≈ 0.4-0.5) — costs and benefits are both real and roughly balanced. Transmission lineages (organized, generational, arbitrage exit) are agenda-setters: they maintain the form and authorize changes, collecting status and authority from the role. Their directionality is beneficiary-leaning (d ≈ 0.2-0.3) — they extract coordination rents (interpretive authority) but also bear maintenance costs. Decentralized resilience networks (organized, civilizational, mobile) are diffuse beneficiaries: they inherit the adaptive grammar without maintaining the ritual. Their directionality is strongly beneficiary (d ≈ 0.1) — they collect the downstream benefit without the participation cost. No victims are declared: exit is open, and communities that leave simply forgo the adaptive benefit.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (transmitting survival-competence without central institutions) remains live — recurrent catastrophe (exile, persecution, displacement, climate crisis) makes the adaptive grammar continuously relevant. The constraint has not suffered mandatrophy: its function has not atrophied because the problem it solves recurs. The hybrid_transformation_reading suggests the commemorative frame may be the vector that keeps the survival function viable; if so, the mourning_practice_reading is not a rival but a necessary carrier. This reading does not foreclose the others — it identifies the survival-competence transmission as the load-bearing function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_disambiguation,
    'Is the survival-competence reading (D5) a distinct structural claim about what ritual transmits, or a selective emphasis within a unified commemorative function?',
    'Cross-cultural comparative analysis of ritual corpora: if communities facing recurrent catastrophe reliably develop D5-dense ritual repertoires independent of D1/D4 density, the readings are structurally distinct constraints. If D5 density covaries with D1/D4 across all cases, they are facets of one constraint.',
    'If structurally distinct, each reading gets its own ε and classification; the kernel is a family label, not a single constraint. If unified, the ε values across readings must converge on the same referent.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_disambiguation, conceptual, 'Whether the kernel decomposes into multiple ε-invariant constraints or one constraint with multiple observational angles').

omega_variable(
    commemorative_vs_instrumental,
    'Does the commemorative frame (mourning, boundary-maintenance) serve as cover for the instrumental survival function, or are they genuinely co-constitutive?',
    'Historical tracing of ritual change under pressure: when communities lose the commemorative frame but retain survival competence (or vice versa), which persists? The component that persists under selective pressure is the structurally load-bearing one.',
    'If commemorative is cover, the D5 reading reveals the true constraint and the D1/D4 readings are false summits. If co-constitutive, the hybrid_transformation_reading is the correct structural decomposition.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(commemorative_vs_instrumental, empirical, 'Whether the commemorative and survival functions are separable or fused in the constraint''s operation').

omega_variable(
    institutional_transformation_mechanism,
    'What specific mechanism transmits adaptive capacity for institutional transformation through ritual rehearsal?',
    'Micro-sociological analysis of ritual performance: identify the cognitive, social, and material channels through which rehearsal updates institutional priors. Compare with non-ritual transmission pathways (texts, oral instruction, apprenticeship).',
    'If mechanism is identified and distinct from other transmission modes, D5 is a coordination type (identity_coordination or attachment_coordination). If mechanism reduces to general social learning, D5 is not a distinct coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_transformation_mechanism, empirical, 'The causal pathway from ritual rehearsal to institutional adaptive capacity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_function__survival_competence_reading, 0, 1500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(catastrophe_survival_tr_t0, catastrophe_memory_function__survival_competence_reading, theater_ratio, 0, 0.05).
narrative_ontology:measurement(catastrophe_survival_tr_t250, catastrophe_memory_function__survival_competence_reading, theater_ratio, 250, 0.08).
narrative_ontology:measurement(catastrophe_survival_tr_t500, catastrophe_memory_function__survival_competence_reading, theater_ratio, 500, 0.12).
narrative_ontology:measurement(catastrophe_survival_tr_t750, catastrophe_memory_function__survival_competence_reading, theater_ratio, 750, 0.15).
narrative_ontology:measurement(catastrophe_survival_tr_t1000, catastrophe_memory_function__survival_competence_reading, theater_ratio, 1000, 0.15).
narrative_ontology:measurement(catastrophe_survival_tr_t1250, catastrophe_memory_function__survival_competence_reading, theater_ratio, 1250, 0.14).
narrative_ontology:measurement(catastrophe_survival_tr_t1500, catastrophe_memory_function__survival_competence_reading, theater_ratio, 1500, 0.15).

% Extraction over time
narrative_ontology:measurement(catastrophe_survival_be_t0, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 0, 0.08).
narrative_ontology:measurement(catastrophe_survival_be_t250, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 250, 0.12).
narrative_ontology:measurement(catastrophe_survival_be_t500, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 500, 0.15).
narrative_ontology:measurement(catastrophe_survival_be_t750, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 750, 0.18).
narrative_ontology:measurement(catastrophe_survival_be_t1000, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 1000, 0.18).
narrative_ontology:measurement(catastrophe_survival_be_t1250, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 1250, 0.16).
narrative_ontology:measurement(catastrophe_survival_be_t1500, catastrophe_memory_function__survival_competence_reading, base_extractiveness, 1500, 0.18).

% Suppression requirement over time
narrative_ontology:measurement(catastrophe_survival_su_t0, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(catastrophe_survival_su_t250, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 250, 0.15).
narrative_ontology:measurement(catastrophe_survival_su_t500, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 500, 0.2).
narrative_ontology:measurement(catastrophe_survival_su_t750, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 750, 0.22).
narrative_ontology:measurement(catastrophe_survival_su_t1000, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 1000, 0.22).
narrative_ontology:measurement(catastrophe_survival_su_t1250, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 1250, 0.2).
narrative_ontology:measurement(catastrophe_survival_su_t1500, catastrophe_memory_function__survival_competence_reading, suppression_requirement, 1500, 0.22).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_function__survival_competence_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_function__survival_competence_reading, 0.08).
narrative_ontology:affects_constraint(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function__mourning_practice_reading).
narrative_ontology:affects_constraint(catastrophe_memory_function__survival_competence_reading, catastrophe_memory_function__hybrid_transformation_reading).

% DUAL FORMULATION NOTE:
% The catastrophe_memory_function kernel decomposes into three constraint stories: this survival_competence_reading (D5 — rope), mourning_practice_reading (D1/D4 — likely rope or scaffold), and hybrid_transformation_reading (D1/D4+D5 — likely tangled_rope if the commemorative frame extracts compliance for the survival function). All three are linked via affects_constraints. The survival_competence_reading has the lowest extractiveness because it isolates the coordination function; the hybrid reading likely shows higher extraction where commemorative obligation enforces survival rehearsal.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_function__survival_competence_reading, organized, 0.25).
constraint_indexing:directionality_override(catastrophe_memory_function__survival_competence_reading, analytical, 0.5).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
