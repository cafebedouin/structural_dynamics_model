% ============================================================================
% CONSTRAINT STORY: stone_land_use_rule__commemorative_husk
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stone_land_use_rule__commemorative_husk, []).

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
 *   constraint_id: stone_land_use_rule__commemorative_husk
 *   human_readable: Stone Land-Use Rule: Commemorative Husk Reading
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   A stone marker commemorates a historical flood boundary, originally
 *   functioning as a behavioral land-use prohibition that shaped spatial
 *   practice for generations: no building below the stone line. Over time —
 *   through cognitive forgetting, institutional drift, or active reframing —
 *   the stone's warning function has decayed into pure symbolism.
 *   Contemporary residents and planners view the stone as a memorial artifact
 *   documenting cultural heritage, not as an active constraint on
 *   development. Building decisions are now made according to market logic,
 *   waterfront convenience, and engineered flood defense (levees, retention)
 *   rather than with reference to the stone's location. The downstream
 *   community at historical flood risk bears the full cost of this
 *   institutional memory failure: they face unmanaged flood exposure because
 *   the informal behavioral constraint has dissolved and no formal legal
 *   alternative has replaced it. This constraint story instantiates the
 *   'commemorative_husk' reading of the contested kernel
 *   'stone_land_use_rule' — a reading that asserts the stone's behavioral
 *   competence has decayed and the warning is now purely symbolic.
 *
 * KEY AGENTS:
 *   - Downstream Community at Historical Flood Risk: Primary victim (powerless/trapped) — lives in floodplain below stone; structurally unable to exit; faces extraction of flood risk management burden
 *   - Waterfront Developers and Upstream Property Owners: Primary beneficiary (institutional/arbitrage) — benefit from freedom to develop; stone's behavioral competence decay enables convenient waterfront use without constraint
 *   - Municipal Flood Risk Manager: Moderate agent (moderate/constrained) — manages flood risk through engineered solutions (expensive alternatives to stone enforcement); bears cost of degraded institutional memory
 *   - Anthropologist/Disaster Analyst Observer: Analytical position (analytical/analytical) — sees structural failure of institutional memory systems; traces extractive drift from behavioral competence to symbolic gesture
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stone_land_use_rule__commemorative_husk, 0.68).
domain_priors:suppression_score(stone_land_use_rule__commemorative_husk, 0.42).
domain_priors:theater_ratio(stone_land_use_rule__commemorative_husk, 0.85).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, extractiveness, 0.68).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, suppression_requirement, 0.42).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, theater_ratio, 0.85).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stone_land_use_rule__commemorative_husk, snare).
narrative_ontology:human_readable(stone_land_use_rule__commemorative_husk, "Stone Land-Use Rule: Commemorative Husk Reading").
narrative_ontology:topic_domain(stone_land_use_rule__commemorative_husk, "disaster_anthropology/institutional_memory/land_use_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(stone_land_use_rule__commemorative_husk, 'b3fffd64-0c8a-423b-9f9a-c5b07d7aca77').
narrative_ontology:cs_kernel_codification('b3fffd64-0c8a-423b-9f9a-c5b07d7aca77', fixed_text).
narrative_ontology:cs_authority_grounding('b3fffd64-0c8a-423b-9f9a-c5b07d7aca77', practice).
narrative_ontology:cs_interpretation_layer_present('b3fffd64-0c8a-423b-9f9a-c5b07d7aca77').
narrative_ontology:cs_reading_relation('b3fffd64-0c8a-423b-9f9a-c5b07d7aca77', stone_land_use_rule__behavioral_competence, coexists_with).
narrative_ontology:cs_axiom('b3fffd64-0c8a-423b-9f9a-c5b07d7aca77', foundational, institutional_memory_requires_active_transmission).
narrative_ontology:cs_axiom_status(institutional_memory_requires_active_transmission, holdable).
narrative_ontology:cs_axiom_grounding('b3fffd64-0c8a-423b-9f9a-c5b07d7aca77', institutional_memory_requires_active_transmission, empirically_contingent).
narrative_ontology:cs_axiom('b3fffd64-0c8a-423b-9f9a-c5b07d7aca77', foundational, symbolic_memorial_cannot_substitute_for_behavioral_constraint).
narrative_ontology:cs_axiom_status(symbolic_memorial_cannot_substitute_for_behavioral_constraint, holdable).
narrative_ontology:cs_axiom_grounding('b3fffd64-0c8a-423b-9f9a-c5b07d7aca77', symbolic_memorial_cannot_substitute_for_behavioral_constraint, deontological).
narrative_ontology:cs_reference_frame('b3fffd64-0c8a-423b-9f9a-c5b07d7aca77', communal_oral_tradition_enforcing_avoidance).
narrative_ontology:cs_drift_state('b3fffd64-0c8a-423b-9f9a-c5b07d7aca77', contemporary_developer_dominated_planning, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('b3fffd64-0c8a-423b-9f9a-c5b07d7aca77', '2026-02-26T14:32:18Z').
narrative_ontology:cs_kernel_id(stone_land_use_rule__commemorative_husk, stone_land_use_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stone_land_use_rule__commemorative_husk, waterfront_developers).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__commemorative_husk, property_owners_upstream_of_stone).
narrative_ontology:constraint_victim(stone_land_use_rule__commemorative_husk, downstream_communities_at_historical_flood_risk).
narrative_ontology:constraint_victim(stone_land_use_rule__commemorative_husk, institutional_memory_systems).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DOWNSTREAM COMMUNITY (SNARE) — Structurally trapped in the floodplain; cannot exit without abandoning home and livelihood. The stone's behavioral competence has decayed to pure symbol. They face the full extraction of historical flood risk with zero behavioral constraint on land-use decisions upstream. The constraint appears absent — the stone is 'just a memorial' — while the actual danger persists unmanaged. Maximum experienced extraction.
constraint_indexing:constraint_classification(stone_land_use_rule__commemorative_husk, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: WATERFRONT DEVELOPER / UPSTREAM PROPERTY OWNERS (ROPE) — Experience the stone as pure coordination: it marks a historical boundary, documents a past event, and enables development planning without behavioral constraint. The stone has shed its enforcement function and become a locational anchor — an informational standard. They benefit from the freedom to develop while enjoying the symbolic memorial. Net beneficiary — extraction runs away from them.
constraint_indexing:constraint_classification(stone_land_use_rule__commemorative_husk, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: MUNICIPAL FLOOD RISK MANAGER (TANGLED ROPE) — Constrained by budget limits and competing priorities. The stone's presence offers genuine coordination benefit: it documents the historical flood boundary, reducing legal liability and enabling informed planning. But the manager also bears extraction pressure: they cannot enforce development restriction solely on the stone's authority (the behavioral competence has decayed), and they must implement expensive engineered solutions (levees, retention) instead. Mixed benefit and cost — genuine coordination function with asymmetric extraction alongside it.
constraint_indexing:constraint_classification(stone_land_use_rule__commemorative_husk, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (SNARE) — This reading's own classification view. The constraint exhibits high theater (memorial ceremony, cultural significance) and high extractiveness (institutional memory degradation enables development risk that downstream communities cannot escape). The behavioral enforcement mechanism has dissolved; the symbolic gesture persists. The analytical observer sees this as a structural failure of institutional memory systems to sustain land-use warnings across generational horizons — extractive drift toward developer convenience, masked as respect for cultural heritage.
constraint_indexing:constraint_classification(stone_land_use_rule__commemorative_husk, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stone_land_use_rule__commemorative_husk_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(stone_land_use_rule__commemorative_husk, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(stone_land_use_rule__commemorative_husk, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(stone_land_use_rule__commemorative_husk, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(stone_land_use_rule__commemorative_husk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. In this reading, the stone's behavioral enforcement has decayed, leaving downstream communities with unmanaged flood risk. Developers and property owners extract the benefit of unrestricted waterfront development while communities bear the flood exposure cost. The extractiveness is not maximal (0.95) because engineered alternatives (levees, retention) offer partial mitigation — but they are expensive and imperfect substitutes for the lost informal constraint. The trajectory over 60 time units shows rising extractiveness as behavioral competence decays: at t=0, the stone still functions (ε=0.15, snare classification is weaker because the behavioral constraint works), but by t=60, the decay is complete (ε=0.68, snare classification is strong). Suppression (0.42): Moderate. The measurement trajectory shows INVERSE correlation with theater: as theater rises (symbolic memorial gains importance), suppression falls (the mechanism of suppression — customary avoidance — weakens). At t=0, suppression is high (0.75) because custom-based avoidance requires strong social reinforcement; by t=60, suppression has fallen (0.42) because the behavioral mechanism has dissolved and only residual institutional/legal suppression remains (zoning codes, if they exist). Theater ratio (0.85): High. The stone has become predominantly performative — memorial ceremonies, cultural heritage preservation, symbolic documentation — with minimal functional behavioral enforcement. This high theater is diagnostically characteristic of a piton, but the snare classification dominates because extractiveness is high and the constraint still actively harms downstream communities despite its symbolic degradation.
 *
 * PERSPECTIVAL GAP:
 *   Downstream community perceives the stone as a failed institutional promise — a memorial that should have been a warning, whose behavioral meaning has been lost. Waterfront developer perceives the stone as purely informational — marking a historical boundary relevant for heritage management and legal liability, but not for current development decisions. Municipal manager perceives mixed benefit and burden — the stone documents the historical risk (liability protection, planning data) but provides no enforcement mechanism (expensive alternatives required). Analytical observer perceives this as extractive institutional failure — the decay of behavioral competence is not neutral; it systematically benefits those with capital (developers) and harms those without exit options (trapped communities). The perspectives converge on one fact (the stone's behavioral competence has decayed) but diverge radically on classification because each observer's exit options and power position create different experienced extractiveness.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (waterfront developers, upstream property owners) have institutional power and arbitrage exit options — they can develop elsewhere but prefer convenient waterfront sites. The stone's behavioral competence decay benefits them by removing informal constraint. Derivation: beneficiary + institutional + arbitrage yields low d (~0.15), producing negative f(d) and low/negative effective extraction from their perspective. They experience the constraint as rope (coordination benefit from knowing the historical boundary). Victims (downstream communities) are powerless and trapped — they cannot exit the floodplain and have no alternative flood management. Derivation: victim + powerless + trapped yields high d (~0.95), producing high f(d) and high effective extraction from their perspective. They experience snare (full extraction cost, no escape). The perspectival gap reflects the structural asymmetry: the same institutional memory failure benefits one group while harming another. The analytical observer sees the stone as a snare because the overall system extracts from vulnerable communities while benefiting developers — the behavioral competence decay is the mechanism that enables this extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates how institutional memory failure can resolve the mandatrophy differently than active extraction. If the stone still functioned (behavioral_competence reading), it would classify as mountain or rope depending on perspective — an immutable spatial boundary that communities practice. But in this reading (commemorative_husk), the decay of behavioral competence creates a snare from the downstream community's perspective: extraction without effective constraint mechanism. The mandatrophy is resolved by recognizing that extractiveness has changed over time (rising from 0.15 to 0.68 across the interval), driven by institutional drift rather than intentional coercion. The suppression mechanism has weakened as the behavioral mechanism dissolved — the snare is sustained now primarily by the fact that trapped communities cannot exit, not by active suppression of alternatives (which has fallen from 0.75 to 0.42). This pattern is characteristic of constraints that degrade from functional to extractive through institutional inertia rather than design.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    behavioral_competence_decay_mechanism,
    'Is the stone''s loss of behavioral enforcement due to cognitive forgetting, institutional drift, or active suppression of the land-use prohibition?',
    'Historical analysis: interviews with residents about whether the stone''s meaning was taught to previous generations and lost; examination of land-use records and development patterns before/after behavioral competence decay; discourse analysis of official documents (permits, planning memos, cultural heritage policies) for evidence of active reframing from ''warning'' to ''memorial''',
    'If cognitive forgetting: extractiveness reclassifies as moderate (natural decay, not targeted suppression). If institutional drift: extractiveness confirmed high (systematic failure to maintain behavioral meaning). If active suppression: extractiveness confirmed and snare classification deepens — the decay is intentional management, not accidental loss.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(behavioral_competence_decay_mechanism, empirical, 'Mechanism of behavioral enforcement decay').

omega_variable(
    kernel_reading_contest_resolution,
    'Is the stone''s actual current function behavioral land-use enforcement (behavioral_competence reading) or memorial symbolism (this commemorative_husk reading)?',
    'Observational study: document whether land-use decisions are made with reference to stone''s location; interview developers, planners, residents about stone''s role in decision logic; measure spatial correlation between stone location and building decisions pre/post decay; compare behavioral competence reading''s epsilon predictions (low extractiveness from stone enforcement) against this reading''s predictions (high extractiveness from symbolic degradation) using actual development patterns',
    'If behavioral_competence reading is correct: stone still enforces; extractiveness should be low (~0.15); constraint is mountain or rope, not snare. If commemorative_husk reading is correct (this one): behavioral competence has dissolved; extractiveness high (~0.68); constraint is snare. The two readings logically coexist (both live across different communities'' perceptions) but one reading of factual reality about actual stone function will resolve which reading''s epsilon is accurate.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_contest_resolution, empirical, 'Which reading captures actual stone function: enforcement or memorial').

omega_variable(
    intergenerational_knowledge_transmission,
    'If behavioral competence has decayed, what institutional mechanisms could restore it? What would be required to re-embed the stone''s warning in daily practice?',
    'Comparative case study: examine other stones/markers whose behavioral competence persisted; identify key institutional structures (oral tradition, ceremony, legal codification, visible infrastructure) that sustained meaning. Design and pilot intervention (educational program, zoning codification, visibility infrastructure) and measure change in behavioral reference to stone across 2-3 generation cohorts.',
    'If restoration is possible through targeted institutional work: extractiveness is contingent on institutional choice; the decay is not inevitable. If institutional mechanisms are structurally degraded (e.g., oral tradition has no transmission pathway left): extractiveness is locked in and only engineered solutions can recover behavioral constraint. Affects scaffold/piton vs snare classification in counterfactual analysis.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(intergenerational_knowledge_transmission, empirical, 'Feasibility and cost of restoring behavioral competence').

omega_variable(
    reading_kernel_ambiguity,
    'This constraint is ONE reading of the contested kernel ''stone_land_use_rule''. What structural feature of the stone and its institutional context determines which reading (behavioral_competence vs commemorative_husk) is operative?',
    'For EACH reading, extract its ground-truth empirical predictions about stone function. (1) behavioral_competence reading predicts: stone location correlates with avoided building; developers cite stone in planning documents; communities practice avoidance; enforcement is informal/customary. (2) commemorative_husk reading (this one) predicts: stone location does NOT correlate with avoided building; developers cite stone only in heritage/cultural context, not land-use context; communities view stone as memorial, not warning; development proceeds per market logic, independent of stone. Compare predictions against observational data from the actual site. ONE reading''s factual predictions will be falsified by evidence. BOTH readings coexist as live political positions, but they cannot both be factually accurate about actual stone function.',
    'If behavioral_competence predictions confirmed: this reading (commemorative_husk) is factually false. Reclassify to behavioral_competence constraint file with much lower extractiveness. If commemorative_husk predictions confirmed: the kernel contest is about how to RESTORE lost behavioral competence, not about its current status. Extraction risk from development is real and unmanaged.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_kernel_ambiguity, empirical, 'Kernel contest: which reading factually describes actual stone function').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stone_land_use_rule__commemorative_husk, 0, 60).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(stone_comm_tr_t0, stone_land_use_rule__commemorative_husk, theater_ratio, 0, 0.35).
narrative_ontology:measurement(stone_comm_tr_t20, stone_land_use_rule__commemorative_husk, theater_ratio, 20, 0.6).
narrative_ontology:measurement(stone_comm_tr_t40, stone_land_use_rule__commemorative_husk, theater_ratio, 40, 0.78).
narrative_ontology:measurement(stone_comm_tr_t60, stone_land_use_rule__commemorative_husk, theater_ratio, 60, 0.85).

% Extraction over time
narrative_ontology:measurement(stone_comm_be_t0, stone_land_use_rule__commemorative_husk, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(stone_comm_be_t20, stone_land_use_rule__commemorative_husk, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(stone_comm_be_t40, stone_land_use_rule__commemorative_husk, base_extractiveness, 40, 0.55).
narrative_ontology:measurement(stone_comm_be_t60, stone_land_use_rule__commemorative_husk, base_extractiveness, 60, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(stone_comm_su_t0, stone_land_use_rule__commemorative_husk, suppression_requirement, 0, 0.75).
narrative_ontology:measurement(stone_comm_su_t20, stone_land_use_rule__commemorative_husk, suppression_requirement, 20, 0.58).
narrative_ontology:measurement(stone_comm_su_t40, stone_land_use_rule__commemorative_husk, suppression_requirement, 40, 0.45).
narrative_ontology:measurement(stone_comm_su_t60, stone_land_use_rule__commemorative_husk, suppression_requirement, 60, 0.42).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stone_land_use_rule__commemorative_husk, information_standard).
narrative_ontology:affects_constraint(stone_land_use_rule__commemorative_husk, stone_land_use_rule__behavioral_competence).

% DUAL FORMULATION NOTE:
% The stone_land_use_rule kernel decomposes into two constraint stories: (1) behavioral_competence reading — stone functions as live land-use prohibition; epsilon low, classification rope/mountain from most perspectives. (2) commemorative_husk reading (this story) — stone's behavioral enforcement has decayed; epsilon high (0.68), classification snare from downstream community perspective. The two readings are distinct because they differ radically in empirical fact (does the stone constrain behavior?) and in epsilon value (can differ by ~0.50 points). The sibling behavioral_competence story would predict development patterns different from what this story predicts. One reading's predictions will be falsified by evidence.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
