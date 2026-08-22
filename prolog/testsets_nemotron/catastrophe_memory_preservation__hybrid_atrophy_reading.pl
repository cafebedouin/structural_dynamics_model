% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_preservation__hybrid_atrophy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-20
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_preservation__hybrid_atrophy_reading, []).

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
    narrative_ontology:suppression_profile/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_secondary_role/3,
    narrative_ontology:stakeholder_non_agent/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_memory_preservation__hybrid_atrophy_reading
 *   human_readable: Catastrophe Memory Ritual — Hybrid Atrophy Reading
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   A ritual complex originally developed to encode and transmit
 *   survival-critical threat-recognition skills (flood patterns, fire
 *   behavior, predator cues, resource failure signals) across generations.
 *   Under modernity — institutionalized disaster response, technological
 *   early-warning systems, state emergency infrastructure — the operational
 *   payoff of the ritual's threat-recognition content has collapsed to near
 *   zero. The ritual persists as elaborate mourning-practice: commemorative
 *   reenactment, symbolic substitution, and identity-performance that extract
 *   substantial time, material resources, and emotional labor from
 *   present-generation practitioners while delivering no adaptive survival
 *   benefit. The constraint is claimed as a piton: a former rope (survival
 *   coordination) whose function has atrophied, leaving mostly theatrical
 *   maintenance. The beneficiary has shifted from historical survival (past
 *   generations who actually used the encoded competence) to in-group
 *   identity (current community cohesion maintained through shared costly
 *   signaling). The victim is the present generation who inherit the full
 *   cost burden without the adaptive payoff.
 *
 * KEY AGENTS:
 *   - historical_survival: Primary beneficiary (historical) — past generations who received genuine adaptive value from threat-recognition encoding
 *   - in_group_identity: Current beneficiary — community cohesion maintained through shared costly ritual performance
 *   - present_generation_practitioners: Primary victim (powerless/constrained) — bear full extractive cost (time, resources, emotional labor) with zero adaptive return
 *   - ritual_specialists: Agenda setter (organized/moderate) — administer the practice, control transmission, benefit from role status
 *   - external_observers: Observer (analytical) — see full structure of atrophy and extraction
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.42).
domain_priors:suppression_score(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.31).
domain_priors:theater_ratio(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, extractiveness, 0.42).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, suppression_requirement, 0.31).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 0.68).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, accessibility_collapse, 0.38).
narrative_ontology:constraint_metric(catastrophe_memory_preservation__hybrid_atrophy_reading, resistance, 0.27).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_preservation__hybrid_atrophy_reading, piton).
narrative_ontology:human_readable(catastrophe_memory_preservation__hybrid_atrophy_reading, "Catastrophe Memory Ritual — Hybrid Atrophy Reading").
narrative_ontology:topic_domain(catastrophe_memory_preservation__hybrid_atrophy_reading, "religious_studies/collective_memory/ritual_practice").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_preservation__hybrid_atrophy_reading, '844f2a7e-449f-4871-9609-c676557460a6').
narrative_ontology:cs_kernel_codification('844f2a7e-449f-4871-9609-c676557460a6', distributed).
narrative_ontology:cs_authority_grounding('844f2a7e-449f-4871-9609-c676557460a6', practice).
narrative_ontology:cs_interpretation_layer_present('844f2a7e-449f-4871-9609-c676557460a6').
narrative_ontology:cs_reading_relation('844f2a7e-449f-4871-9609-c676557460a6', catastrophe_memory_preservation__survival_competence_reading, forecloses).
narrative_ontology:cs_reading_relation('844f2a7e-449f-4871-9609-c676557460a6', catastrophe_memory_preservation__mourning_practice_reading, coexists_with).
narrative_ontology:cs_axiom('844f2a7e-449f-4871-9609-c676557460a6', foundational, survival_function_atrophied_under_modernity).
narrative_ontology:cs_axiom_status(survival_function_atrophied_under_modernity, holdable).
narrative_ontology:cs_axiom_grounding('844f2a7e-449f-4871-9609-c676557460a6', survival_function_atrophied_under_modernity, empirically_contingent).
narrative_ontology:cs_axiom('844f2a7e-449f-4871-9609-c676557460a6', foundational, identity_function_emerged_as_parasitic_on_survival_form).
narrative_ontology:cs_axiom_status(identity_function_emerged_as_parasitic_on_survival_form, holdable).
narrative_ontology:cs_axiom_grounding('844f2a7e-449f-4871-9609-c676557460a6', identity_function_emerged_as_parasitic_on_survival_form, conventional).
narrative_ontology:cs_reference_frame('844f2a7e-449f-4871-9609-c676557460a6', operational_survival_transmission).
narrative_ontology:cs_drift_state('844f2a7e-449f-4871-9609-c676557460a6', modernity_onset, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('844f2a7e-449f-4871-9609-c676557460a6', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__hybrid_atrophy_reading, historical_survival).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__hybrid_atrophy_reading, in_group_identity).
narrative_ontology:constraint_victim(catastrophe_memory_preservation__hybrid_atrophy_reading, present_generation_practitioners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(catastrophe_memory_preservation__hybrid_atrophy_reading, ritual_specialists).
narrative_ontology:constraint_vindicates(catastrophe_memory_preservation__hybrid_atrophy_reading, ritual_as_cultural_transmission_vehicle).
narrative_ontology:constraint_vindicates(catastrophe_memory_preservation__hybrid_atrophy_reading, collective_memory_as_identity_anchor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Past generations who received genuine adaptive value from the ritual's threat-recognition encoding. They are not present agents but a structural beneficiary class: the ritual's operation at t=0 subsidized their survival competence. They cannot exit because they are historical; they benefit retroactively from the constraint's original function.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, historical_survival, beneficiary,
    analytical, generational, analytical, universal).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_preservation__hybrid_atrophy_reading, historical_survival).

% The collective identity cohesion maintained through shared costly ritual performance. This is not an agent but a structural beneficiary: the ritual extracts from present practitioners to produce in-group boundary maintenance and identity coherence. It collects no rents but is the current 'beneficiary' of the extraction in the sense that the extraction serves this function.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, in_group_identity, beneficiary,
    analytical, generational, analytical, universal).
narrative_ontology:stakeholder_non_agent(catastrophe_memory_preservation__hybrid_atrophy_reading, in_group_identity).

% Inherit the full ritual complex (multi-day enactments, material offerings, emotional labor of commemorative mourning) with zero adaptive survival payoff. Exit requires fracturing family/community identity, abandoning ancestral practice, and facing social shunning. They experience the constraint as obligatory mourning for a competence they never needed and a catastrophe they never witnessed. The cost is biographical — a significant fraction of discretionary time and resources across adult life.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, present_generation_practitioners, payer,
    powerless, biographical, identity_locked, local).

% Hereditary or trained specialists who administer the ritual, control its transmission, and derive status/livelihood from their role. They benefit from the practice's persistence (role status, material support from community) but are also constrained by it — they cannot unilaterally simplify or sunset it without losing their authority. Their exit is constrained: leaving the role means abandoning their vocational identity and community standing.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, ritual_specialists, agenda_setter,
    organized, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(catastrophe_memory_preservation__hybrid_atrophy_reading, ritual_specialists, beneficiary).

% Anthropologists, historians, disaster scholars who study the ritual as a case of cultural transmission atrophy. They see the full structure: the historical survival function, the modern substitute infrastructure, the identity-locked extraction, the theatrical maintenance. They neither collect from nor pay into the constraint.
narrative_ontology:constraint_stakeholder(catastrophe_memory_preservation__hybrid_atrophy_reading, external_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_preservation__hybrid_atrophy_reading, in_group_identity).
narrative_ontology:fixing_cost_class(catastrophe_memory_preservation__hybrid_atrophy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally: distributed threat-recognition competence across generations so that each cohort could survive recurrent catastrophes without rediscovering warning signs. Currently: maintains in-group identity cohesion through shared costly signaling — the ritual persists as a coordination mechanism for 'who belongs' rather than 'who survives.'
% TRANSFER_FUNCTION: Moves time, material resources (offerings, feasts, specialized objects), and emotional labor (performative grief, commemorative enactment) from present-generation practitioners to the maintenance of in-group identity boundaries. At t=0, the transfer was to survival competence (the practitioners themselves benefited). At t=30, the transfer is to identity maintenance (the practitioners pay, the group boundary receives).
% ABSENT_VOICES: The dead — past generations who actually needed the survival competence — cannot object to the ritual's repurposing. Future generations who will inherit the atrophied practice without the historical justification are not yet present. Dissident practitioners who privately doubt the practice but perform it publicly are excluded from the conversation by identity-lock.
% DISAPPEARANCE_RATIONALE: If the ritual vanished overnight, the community's primary identity-anchor would collapse. Kinship recognition, marriage alliances, status hierarchies, and intergenerational authority structures all route through the ritual complex. The community would not immediately dissolve but would undergo a legitimacy crisis requiring new identity-coordination mechanisms. The survival function would not be missed (modern substitutes exist); the identity function would leave a vacuum.
% FOUNDING_PROBLEM: Recurrent environmental catastrophes (flood cycles, fire regimes, predator pressure, resource collapse) that killed whole cohorts who lacked encoded threat-recognition. The ritual was built as a distributed memory system: each generation performs the enactment, the performance encodes the warning signs, the next generation learns by doing.
% FOUNDING_PROBLEM_CORROBORATION: Disaster historians and climate scientists corroborate that the specific catastrophe regime (predictable flood cycles, specific fire behavior patterns) has been superseded by technological early-warning and state emergency infrastructure. The ritual's own specialists acknowledge in private interviews that 'the old signs don't matter like they used to' but maintain the practice for 'the ancestors' sake.' No living practitioner claims the ritual still teaches actionable survival skills.
narrative_ontology:disappearance_verdict(catastrophe_memory_preservation__hybrid_atrophy_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_preservation__hybrid_atrophy_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_preservation__hybrid_atrophy_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(catastrophe_memory_preservation__hybrid_atrophy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.42, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_preservation__hybrid_atrophy_reading_tests).
:- end_tests(catastrophe_memory_preservation__hybrid_atrophy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness starts high (0.71) when the ritual's threat-recognition content had genuine survival payoff — the cost was the price of competence. As modernity provides substitute survival infrastructure (t=5-15), extractiveness declines because the operational justification evaporates. Theater ratio rises inversely (0.12 → 0.68): early ritual was mostly functional (low theater); late ritual is mostly performance of a function that no longer exists. Suppression is moderate (0.31) and stable — the constraint persists through social expectation and identity pressure, not active coercion. Accessibility collapse is low (0.38): alternatives (modern warning systems, insurance, state aid) exist and work, but the ritual persists because identity-locked participants cannot recognize them as substitutes. Resistance is low (0.27): the victim group is identity-locked and does not effectively resist; exit requires identity fracture.
 *
 * PERSPECTIVAL GAP:
 *   From the ritual specialist / agenda-setter seat: the constraint appears as scaffold with a sunset clause (modernity was supposed to replace it) that failed to sunset — they maintain it as transitional support that never transitioned. From the present-generation practitioner seat (identity-locked, constrained exit): the constraint appears as snare — costly, enforced, no exit, no payoff. From the external observer seat: the constraint computes as piton — atrophied function, high theater, persistent inertia. The engine computes this seat divergence from the structural data; the authored claim (piton) reflects the observer seat.
 *
 * DIRECTIONALITY LOGIC:
 *   Historical survival was the original beneficiary (d ≈ 0.0 — constraint subsidized past generations with genuine competence). In-group identity is the current beneficiary (d ≈ 0.15 — receives cohesion subsidy from the ritual's costly signaling). Present-generation practitioners are the victims (d ≈ 0.85 — identity-locked, constrained exit, bear full cost). Ritual specialists are agenda_setters with moderate power and constrained exit (d ≈ 0.45 — they administer and benefit from role status but are also bound by the practice). The directionality derivation chains from beneficiary/victim declarations + exit options: identity_locked exit on the victim side pushes d toward 1.0; agenda_setter with role-benefit on the specialist side pushes d toward 0.5.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate was survival-competence transmission — a genuine coordination problem (rope) that modernity solved via substitute infrastructure. The arrangement persists because the identity-function (mourning-practice) was never declared as the mandate; the community never held a 'sunset ceremony' for the survival-function. Mandatrophy is unresolved: the original mandate is dead (survival competence is externally provided), but the constraint persists under the old mandate's authority while actually serving a new, undeclared mandate (identity maintenance). This prevents mislabeling the current extraction as coordination — it is extraction wearing a coordination corpse.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine atrophied survival mechanism, or is it structurally a different constraint (mourning practice or surviving competence) misread through a hybrid lens?',
    'Comparative analysis across the three sibling readings of the catastrophe_memory_preservation kernel: measure extractiveness trajectories, beneficiary structures, and type classifications independently. If the readings compute to different types with non-overlapping metric profiles, they are distinct constraints, not one constraint viewed from three angles.',
    'If this reading computes as piton while survival_competence_reading computes as mountain/rope and mourning_practice_reading computes as scaffold/rope, the kernel label ''catastrophe memory preservation'' conflates structurally distinct constraints. The framework would model them as three stories linked by network.affects_constraints, not as one story with measurement-dependent classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Whether the kernel decomposes into distinct constraints per ε-invariance.').

omega_variable(
    extractiveness_trajectory_ambiguity,
    'Does the extractiveness decline represent genuine functional atrophy, or a shift from survival-extraction to identity-extraction that maintains the same rate?',
    'Longitudinal ethnographic tracking of resource flows: measure what the ritual extracts (time, material, emotional labor) and to whom it accrues at each historical phase. If total extraction is stable but the recipient shifts from ''survival fitness'' to ''in-group boundary maintenance,'' the decline is a category error.',
    'If extraction is stable with shifted beneficiary, the constraint is a snare or tangled_rope with transformed extraction logic, not a piton. Piton classification requires genuine extraction decay with persistent theater.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(extractiveness_trajectory_ambiguity, empirical, 'Whether extractiveness decline is real atrophy or beneficiary substitution.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (community enforcement, doctrinal requirement) or internalized (participants believe the practice is obligatory for their identity)?',
    'Post-exit suppression trajectory: track individuals who leave the practicing community. If suppression experience persists after exit (guilt, identity fragmentation, social shunning), reclassify as partially internalized. If suppression ends at community boundary, it is structural.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit. This would elevate the computed type toward snare/tangled_rope for identity-locked participants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for identity-locked practitioners.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_preservation__hybrid_atrophy_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 0, 0.12).
narrative_ontology:measurement(cata_tr_t5, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 5, 0.22).
narrative_ontology:measurement(cata_tr_t10, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 10, 0.35).
narrative_ontology:measurement(cata_tr_t15, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 15, 0.48).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 20, 0.58).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 25, 0.64).
narrative_ontology:measurement(cata_tr_t30, catastrophe_memory_preservation__hybrid_atrophy_reading, theater_ratio, 30, 0.68).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 0, 0.71).
narrative_ontology:measurement(cata_be_t5, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(cata_be_t10, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement(cata_be_t15, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 15, 0.49).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 20, 0.42).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 25, 0.38).
narrative_ontology:measurement(cata_be_t30, catastrophe_memory_preservation__hybrid_atrophy_reading, base_extractiveness, 30, 0.35).

% Suppression authored static: scalar-only by design, no temporal series
narrative_ontology:suppression_profile(catastrophe_memory_preservation__hybrid_atrophy_reading, static).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_preservation__hybrid_atrophy_reading, identity_coordination).
narrative_ontology:boltzmann_floor_override(catastrophe_memory_preservation__hybrid_atrophy_reading, 0.1).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_preservation__hybrid_atrophy_reading, catastrophe_memory_preservation__mourning_practice_reading).

% DUAL FORMULATION NOTE:
% This constraint family decomposes the kernel 'catastrophe memory preservation' into three readings with distinct structural profiles. The hybrid_atrophy_reading claims a temporal trajectory (rope→piton) with shifting beneficiary. The survival_competence_reading claims persistent operational function (mountain/rope). The mourning_practice_reading claims static identity-function (scaffold/rope). They are linked because the survival_competence_reading's claim is often cited as evidence for the hybrid_atrophy_reading's t=0 state, and the mourning_practice_reading's claim is cited as evidence for its t=end state.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(catastrophe_memory_preservation__hybrid_atrophy_reading, organized, 0.45).
constraint_indexing:directionality_override(catastrophe_memory_preservation__hybrid_atrophy_reading, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
