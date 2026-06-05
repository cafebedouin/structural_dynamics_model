% ============================================================================
% CONSTRAINT STORY: tsunami_stone_commitment__behavioral_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_tsunami_stone_behavioral_competence, []).

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
 *   constraint_id: tsunami_stone_commitment__behavioral_competence_reading
 *   human_readable: Tsunami Stone Commitment: Behavioral Competence Reading
 *   domain: disaster_anthropology/institutional_memory/commitment_systems
 *
 * SUMMARY:
 *   Stone inscriptions embedded in Japanese coastal landscapes (Anyo Ihin no
 *   Hi, 'Memorial Stone of High Tides') function as a commitment system that
 *   maintains disaster preparedness knowledge across generational cycles.
 *   This constraint exemplifies a durable institutional memory mechanism: the
 *   stone embodies a narrative (tsunami reached this height; evacuate to
 *   higher ground when signs appear) that persists through intergenerational
 *   transmission without written archives or centralized enforcement. The
 *   behavioral_competence_reading instantiates the hypothesis that the stone
 *   constraint maintains GENUINE behavioral force — communities interpret the
 *   stone as a binding normative anchor that structures evacuation behavior
 *   during actual disasters. This reading assumes the transmission mechanism
 *   is robust, the knowledge content remains actionable, and the stone
 *   operates as a coordination device (low extraction, high functionality).
 *   The sibling reading (commemorative_husk_reading) posits that the stone's
 *   behavioral content has atrophied into symbolic commemoration —
 *   communities treat the stone as culturally meaningful but no longer
 *   operationally binding. These readings coexist as competing
 *   interpretations of the same kernel and depend on empirical validation of
 *   whether behavioral competence persists across contemporary communities
 *   with varying degrees of urbanization, educational displacement, and
 *   narrative standardization.
 *
 * KEY AGENTS:
 *   - Coastal Communities: Primary agents (powerless/constrained, generational horizon) — maintain and transmit the stone's narrative; bear modest constraint costs (ritual participation, knowledge transmission); benefit from disaster preparedness coordination
 *   - Regional Disaster Prevention Authorities: Secondary institutional actors (institutional/mobile, generational horizon) — leverage the stone system as a distributed, low-cost enforcement mechanism for preparedness norms
 *   - Anthropological/Historical Observers: Analytical position (analytical/analytical, civilizational horizon) — evaluate whether stones exemplify durable institutional memory or commemorate decayed functions
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(tsunami_stone_commitment__behavioral_competence_reading, 0.08).
domain_priors:suppression_score(tsunami_stone_commitment__behavioral_competence_reading, 0.12).
domain_priors:theater_ratio(tsunami_stone_commitment__behavioral_competence_reading, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, extractiveness, 0.08).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(tsunami_stone_commitment__behavioral_competence_reading, rope).
narrative_ontology:human_readable(tsunami_stone_commitment__behavioral_competence_reading, "Tsunami Stone Commitment: Behavioral Competence Reading").
narrative_ontology:topic_domain(tsunami_stone_commitment__behavioral_competence_reading, "disaster_anthropology/institutional_memory/commitment_systems").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(tsunami_stone_commitment__behavioral_competence_reading, '5bb3c161-849b-43cc-b435-011b2a3697d9').
narrative_ontology:cs_kernel_codification('5bb3c161-849b-43cc-b435-011b2a3697d9', fixed_text).
narrative_ontology:cs_authority_grounding('5bb3c161-849b-43cc-b435-011b2a3697d9', practice).
narrative_ontology:cs_interpretation_layer_present('5bb3c161-849b-43cc-b435-011b2a3697d9').
narrative_ontology:cs_reading_relation('5bb3c161-849b-43cc-b435-011b2a3697d9', tsunami_stone_commitment__commemorative_husk_reading, coexists_with).
narrative_ontology:cs_axiom('5bb3c161-849b-43cc-b435-011b2a3697d9', foundational, stone_inscription_maintains_behavioral_competence).
narrative_ontology:cs_axiom_status(stone_inscription_maintains_behavioral_competence, holdable).
narrative_ontology:cs_axiom_grounding('5bb3c161-849b-43cc-b435-011b2a3697d9', stone_inscription_maintains_behavioral_competence, empirically_contingent).
narrative_ontology:cs_axiom('5bb3c161-849b-43cc-b435-011b2a3697d9', secondary, intergenerational_transmission_preserves_actionable_knowledge).
narrative_ontology:cs_axiom_status(intergenerational_transmission_preserves_actionable_knowledge, holdable).
narrative_ontology:cs_axiom_grounding('5bb3c161-849b-43cc-b435-011b2a3697d9', intergenerational_transmission_preserves_actionable_knowledge, empirically_contingent).
narrative_ontology:cs_reference_frame('5bb3c161-849b-43cc-b435-011b2a3697d9', behavioral_disaster_preparedness_anchor).
narrative_ontology:cs_drift_state('5bb3c161-849b-43cc-b435-011b2a3697d9', contemporary_urbanization_era, gap(practice_drift, minor, false)).
narrative_ontology:cs_created_at('5bb3c161-849b-43cc-b435-011b2a3697d9', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(tsunami_stone_commitment__behavioral_competence_reading, coastal_communities).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COASTAL COMMUNITY / GENERATIONAL (ROPE) — The transmission mechanism solves the collective action problem of disaster preparedness across generations. The stone constraint coordinates intergenerational knowledge by embedding disaster memory in physical infrastructure. Low extractiveness; high genuine coordination function. The community bears modest constraint costs (visiting/maintaining stones, rehearsing narratives) in exchange for protective knowledge that persists through cultural drift. No identifiable beneficiary extracts from this agent.
constraint_indexing:constraint_classification(tsunami_stone_commitment__behavioral_competence_reading, rope,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 2: REGIONAL AUTHORITY / GENERATIONAL (ROPE) — Disaster prevention authorities benefit from the stone system as a low-cost, scalable mechanism for maintaining preparedness norms across dispersed populations. The stones distribute the enforcement burden: each community maintains its own transmission. Institutional actors perceive pure coordination — the constraint solves the collective action problem of keeping disaster preparedness alive during interregnum periods (decades without major events). No extraction; genuine coordination.
constraint_indexing:constraint_classification(tsunami_stone_commitment__behavioral_competence_reading, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER / CIVILIZATIONAL (ROPE) — Stone inscriptions as a commitment device exemplify how societies create durable institutional memory in the absence of written archives or centralized enforcement. The constraint is pure coordination: it solves the problem of transmitting disaster knowledge across lifespans that exceed personal memory. The functional mechanism (repeated ritual, physical landmark, intergenerational rehearsal) is the same across all stone-marker cultures. Low theater, high behavioral competence. This reading instantiates the behavioral_competence thesis: the stones work because they ARE the mechanism, not because they perform a mechanism.
constraint_indexing:constraint_classification(tsunami_stone_commitment__behavioral_competence_reading, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(tsunami_stone_commitment__behavioral_competence_reading_tests).
:- end_tests(tsunami_stone_commitment__behavioral_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.08): Very low. The stone constraint exhibits minimal extraction because there is no identifiable beneficiary capturing asymmetric value. Coastal communities maintain the stone transmission and receive protective knowledge — symmetric benefit. Regional authorities benefit from the distributed enforcement mechanism but do not extract material resources or surplus from communities. The low extractiveness reflects the behavioral_competence reading: the constraint is a pure coordination mechanism solving the collective action problem of transmitting disaster knowledge across generational gaps. If the sibling reading (commemorative husk) were correct, extractiveness might rise (communities bear ritual costs for symbolic meaning without behavioral function), but this reading assumes behavioral competence persists. Suppression (0.12): Low. The constraint operates through voluntary transmission within communities; there are no formal legal barriers to exit. Families can cease maintaining the stone's narrative, and communities can reprioritize disaster preparation norms. However, suppression is not zero because social conformity within communities enforces participation — elders expect younger cohorts to learn the narrative, and narrative deviation carries modest social cost. The low suppression reflects that the mechanism depends on internalized norm adoption rather than coercive enforcement. Theater ratio (0.25): Low. The stone constraint exhibits minimal performative content because the constraint IS the mechanism, not a representation of a mechanism. The ritual of visiting the stone, reciting the narrative, and rehearsing evacuation behavior are functional activities, not theater. The low theater distinguishes this rope constraint from a piton (which would have theater ≥0.70). Theater rises slightly across the interval (0.20→0.25) as urbanization increases and direct experience with tsunami hazard decreases — younger cohorts may emphasize the commemorative significance over the behavioral content as personal hazard salience declines. However, the constraint remains primarily functional across the measurement period, supporting the behavioral_competence reading.
 *
 * PERSPECTIVAL GAP:
 *   All three perspectives classify the constraint as rope, indicating low perspectival variance. This uniformity is diagnostic: when a constraint appears as pure coordination across beneficiary, authority, and analytical positions, it suggests a genuinely functional mechanism with no hidden extraction. The coastal community perceives the constraint as solving the intergenerational knowledge transmission problem. Regional authorities perceive it as a distributed enforcement solution. The analytical observer perceives it as an exemplar of durable institutional memory. The absence of a snare or tangled_rope perspective reflects the core hypothesis of the behavioral_competence reading: the constraint does not extract asymmetric value, and it is not enforced through coercion or suppression. A snare perspective would appear if behavioral content had atrophied (sibling reading) and communities were being forced to maintain ritual for symbolic rather than functional reasons — that is the structure of the commemorative_husk reading, which this constraint story does NOT instantiate.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) values follow from the structural beneficiary/victim declarations and exit options. Coastal communities (beneficiary + constrained exit due to social conformity) derive d ≈0.35 via the beneficiary/constrained chain; regional authorities (beneficiary + mobile exit) derive d ≈0.15 via the beneficiary/mobile chain; analytical observer derives d ≈0.72 (analytical position). The chi formula produces low effective extractiveness across all positions because base ε is very low (0.08) and f(d) factors are modest — even the analytical position's higher f(d) value produces χ ≈0.11, still in rope territory. The low extractiveness is robust to perspective variation, supporting the classification of this constraint as pure coordination.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves mandatrophy by instantiating pure coordination with transparent beneficiary structure. The coastal communities benefit from disaster preparedness knowledge; regional authorities benefit from distributed enforcement; no third party extracts value. The constraint exhibits low extractiveness, low suppression, low theater, and uniform rope classification across all perspectives. Mandatrophy would arise if the same structural data supported both rope and snare classifications depending on observer position — but all three perspectives classify as rope because the extraction mechanism is genuinely absent. The constraint exemplifies how institutional memory mechanisms can operate as pure coordination without becoming extraction mechanisms, even across generations and with modest social conformity pressures.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_efficacy_vs_commemorative_performance,
    'Does the stone constraint maintain behavioral force through genuine intergenerational knowledge transmission, or does it function as a commemorative symbol with degraded behavioral content?',
    'Longitudinal ethnographic observation of evacuation behavior during actual disaster events; comparison of evacuation patterns in communities with active stone transmission vs. those where stone meaning has become ceremonial; interview data on whether young cohorts can articulate the specific hazard the stone references and respond appropriately.',
    'If behavioral competence is maintained: constraint is rope (low ε, high coordination). If behavioral content has atrophied: constraint is piton (ε low but theater_ratio high, sustained through ritual inertia). The reading classification depends entirely on this empirical question.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(behavioral_efficacy_vs_commemorative_performance, empirical, 'Whether the stone constraint maintains genuine behavioral competence or has degraded to commemorative performance').

omega_variable(
    alternative_kernel_reading_coexistence,
    'If the sibling reading (commemorative_husk_reading) is correct — stones function as symbolic commemoration with atrophied behavioral content — does that reading logically foreclose this behavioral_competence_reading, or can both readings coexist as different parties'' interpretations of the same constraint?',
    'Institutional analysis: do some communities maintain genuine behavioral transmission while others have ceremonially degraded transmission? If yes: readings coexist (different parties'' empirical realities). If the empirical status is uniform: only one reading is defensible.',
    'If coexistence confirmed: the cs_structure relation should be ''coexists_with''. If one reading forecloses the other: the relation should be ''forecloses'' and the other reading''s claim to be a legitimate framing collapses.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_kernel_reading_coexistence, empirical, 'Whether behavioral and commemorative readings coexist or one forecloses the other').

omega_variable(
    intergenerational_transmission_robustness,
    'How robust is the intergenerational transmission mechanism to migration, urbanization, educational displacement, and narrative innovation? Under what conditions does the stone''s meaning persist vs. degrade?',
    'Comparative ethnography of stone transmission in communities with high vs. low outmigration, high vs. low formal education emphasis, high vs. low narrative standardization. Tracking of transmission quality across generation transitions (parent-to-child, teacher-to-student).',
    'If robust across all conditions: behavioral_competence reading is structural (not contingent on specific social conditions). If robust only under narrow conditions: reading is valid only for those conditions, and ε may rise (more constraint needed to maintain transmission when social forces oppose it).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_transmission_robustness, empirical, 'Robustness of intergenerational transmission under social change').

omega_variable(
    contested_kernel_identification,
    'The kernel ''tsunami_stone_commitment'' is contested between this reading (behavioral competence: stones function as durable coordination mechanism) and a sibling reading (commemorative husk: stones function as symbols with atrophied behavioral force). What empirical or interpretive evidence distinguishes the readings?',
    'The cs_structure block declares the contested kernel and routing through reading_relations and axioms. This omega documents that the kernel''s empirical status depends on behavioral data (actual evacuation compliance, knowledge retention) and interpretive framing (whether communities treat stones as binding normative anchors or as cultural symbols). The committer-axis ambiguity is located in the behavioral competence question and the coexistence question above.',
    'This omega instantiates the distinction between readings: behavioral_competence assumes the stone IS the mechanism; commemorative_husk assumes the stone has become decoupled from the mechanism. The engine''s resolution depends on empirical validation of which framing dominates in actual communities.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(contested_kernel_identification, empirical, 'Kernel contest: behavioral competence vs. commemorative decay').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(tsunami_stone_commitment__behavioral_competence_reading, 0, 6).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tsbc_tr_t0, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 0, 0.2).
narrative_ontology:measurement(tsbc_tr_t3, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 3, 0.23).
narrative_ontology:measurement(tsbc_tr_t6, tsunami_stone_commitment__behavioral_competence_reading, theater_ratio, 6, 0.25).

% Extraction over time
narrative_ontology:measurement(tsbc_be_t0, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(tsbc_be_t3, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 3, 0.07).
narrative_ontology:measurement(tsbc_be_t6, tsunami_stone_commitment__behavioral_competence_reading, base_extractiveness, 6, 0.08).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(tsunami_stone_commitment__behavioral_competence_reading, information_standard).
narrative_ontology:boltzmann_floor_override(tsunami_stone_commitment__behavioral_competence_reading, 0.02).
narrative_ontology:affects_constraint(tsunami_stone_commitment__behavioral_competence_reading, tsunami_stone_commitment__commemorative_husk_reading).

% DUAL FORMULATION NOTE:
% The tsunami_stone_commitment kernel decomposes into two constraint stories representing competing readings. The behavioral_competence_reading models the constraint as pure coordination with maintained functional content (ε=0.08, rope). The commemorative_husk_reading models the constraint as a degraded institutional form where behavioral content has atrophied into ritual performance (expected ε ≤0.25, piton or degraded rope). These readings diverge on a single empirical question: whether intergenerational transmission maintains behavioral competence. They share identical kernel (stone inscription + transmission) and authority structure (distributed community maintenance) but differ in their reference frame and drift assessment. The network edge indicates that if empirical validation confirms one reading, the other reading's status as a defensible framing must be reevaluated.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
