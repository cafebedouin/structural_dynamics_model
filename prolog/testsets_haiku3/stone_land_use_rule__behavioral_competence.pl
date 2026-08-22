% ============================================================================
% CONSTRAINT STORY: stone_land_use_rule__behavioral_competence
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_stone_land_use_rule__behavioral_competence, []).

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
    narrative_ontology:measurement_basis/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: stone_land_use_rule__behavioral_competence
 *   human_readable: Stone Land-Use Rule: Behavioral Competence Reading
 *   domain: land-use governance / institutional memory / disaster anthropology
 *
 * SUMMARY:
 *   A community downstream of a flood plain maintains a behavioral land-use
 *   prohibition through a physical marker (a stone) placed at the boundary of
 *   a 78-year-old flood's maximum extent. Under the behavioral-competence
 *   reading, the stone is LIVE: daily spatial practice (construction
 *   planning, agricultural work, children's orientation) enforces compliance
 *   without formal administration. The constraint's persistence depends on
 *   the continuous enactment of the boundary in ordinary movement and
 *   settlement decisions, not on fading memory or symbolic attachment. The
 *   reading posits that the stone is efficacious precisely because it is
 *   embedded in practical routine, not because it is commemorated or revered.
 *   The sibling reading (commemorative-husk) treats the same physical
 *   artifact as a memorial whose behavioral force has decayed to symbolic
 *   gesture—a reading we are NOT generating here. The two readings diverge on
 *   whether the constraint is functionally binding (behavioral-competence) or
 *   merely symbolically present (commemorative-husk), and they cannot coexist
 *   in a single framework of how the constraint operates.
 *
 * KEY AGENTS:
 *   - downstream_communities: primary beneficiary (moderate power, constrained exit); maintain the constraint through daily practice and social enforcement
 *   - land_use_planners_and_developers: primary payer (organized power, constrained exit); bear the cost of sub-optimal development footprints
 *   - flood_dynamics_hydrologists: observer seat (analytical power); verify the stone's empirical referent against hydrological records
 *   - regional_government_administration: excluded (institutional power, constrained exit); could formalize but are not consulted; would lose the low-overhead enforcement if they intervened
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stone_land_use_rule__behavioral_competence, 0.18).
domain_priors:suppression_score(stone_land_use_rule__behavioral_competence, 0.22).
domain_priors:theater_ratio(stone_land_use_rule__behavioral_competence, 0.08).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, extractiveness, 0.18).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, suppression_requirement, 0.22).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, theater_ratio, 0.08).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, accessibility_collapse, 0.72).
narrative_ontology:constraint_metric(stone_land_use_rule__behavioral_competence, resistance, 0.31).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stone_land_use_rule__behavioral_competence, rope).
narrative_ontology:human_readable(stone_land_use_rule__behavioral_competence, "Stone Land-Use Rule: Behavioral Competence Reading").
narrative_ontology:topic_domain(stone_land_use_rule__behavioral_competence, "land-use governance / institutional memory / disaster anthropology").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(stone_land_use_rule__behavioral_competence, 'a29fdad3-0c8e-436e-96f0-c5bc89c86ebe').
narrative_ontology:cs_kernel_codification('a29fdad3-0c8e-436e-96f0-c5bc89c86ebe', implicit).
narrative_ontology:cs_authority_grounding('a29fdad3-0c8e-436e-96f0-c5bc89c86ebe', practice).
narrative_ontology:cs_interpretation_layer_present('a29fdad3-0c8e-436e-96f0-c5bc89c86ebe').
narrative_ontology:cs_reading_relation('a29fdad3-0c8e-436e-96f0-c5bc89c86ebe', stone_land_use_rule__commemorative_husk, forecloses).
narrative_ontology:cs_axiom('a29fdad3-0c8e-436e-96f0-c5bc89c86ebe', foundational, embodied_practice_sustains_constraint).
narrative_ontology:cs_axiom_status(embodied_practice_sustains_constraint, holdable).
narrative_ontology:cs_axiom_grounding('a29fdad3-0c8e-436e-96f0-c5bc89c86ebe', embodied_practice_sustains_constraint, conventional).
narrative_ontology:cs_axiom('a29fdad3-0c8e-436e-96f0-c5bc89c86ebe', foundational, daily_spatial_enactment_is_primary_enforcement).
narrative_ontology:cs_axiom_status(daily_spatial_enactment_is_primary_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('a29fdad3-0c8e-436e-96f0-c5bc89c86ebe', daily_spatial_enactment_is_primary_enforcement, instrumental).
narrative_ontology:cs_reference_frame('a29fdad3-0c8e-436e-96f0-c5bc89c86ebe', distributed_embodied_knowledge_regime).
narrative_ontology:cs_drift_state('a29fdad3-0c8e-436e-96f0-c5bc89c86ebe', contemporary_demographic_turnover, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('a29fdad3-0c8e-436e-96f0-c5bc89c86ebe', '2026-06-12T00:00:00Z').
narrative_ontology:cs_kernel_id(stone_land_use_rule__behavioral_competence, stone_land_use_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stone_land_use_rule__behavioral_competence, downstream_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(stone_land_use_rule__behavioral_competence, land_use_planners_and_developers).
narrative_ontology:constraint_vindicates(stone_land_use_rule__behavioral_competence, spatial_memory_through_material_practice).
narrative_ontology:constraint_vindicates(stone_land_use_rule__behavioral_competence, embodied_risk_literacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Inhabit land downstream of a historically catastrophic flood zone. The stone's placement at the perimeter of the dangerous zone encodes a 78-year collective memory of where the water reached in the last major event. They benefit from the constraint because daily travel, agriculture, and construction decisions route around the stone-marked boundary, maintaining practical knowledge of flood risk without requiring institutional memory or formal documentation. The constraint persists because families have structured settlement patterns around it; leaving would mean surrendering generations of embedded spatial knowledge.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, downstream_communities, beneficiary,
    moderate, generational, constrained, regional).

% Want to develop or optimize land use in the region. The stone-marked prohibition forecloses economically attractive buildable land; developers pay the cost of working around the boundary (slope difficulty, reduced developable footprint, infrastructure routing challenges). They cannot move or ignore the stone because community compliance with the prohibition and local enforcement through social disapproval create enforcement without formal law. The constraint binds because the community enacts it daily, not because a registry or zoning code does.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, land_use_planners_and_developers, payer,
    organized, biographical, constrained, regional).

% Understand whether the stone-marked boundary reflects actual flood extent and whether it remains a reliable predictor of future inundation. They document the empirical relationship between the stone's location and hydrological records, providing external corroboration of the constraint's referent (the 78-year flood extent as it actually occurred).
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, flood_dynamics_hydrologists, observer,
    analytical, generational, analytical, regional).

% Could formalize the constraint into zoning or building codes, but are not consulted in its operation or maintenance. The rule persists as a community practice, not a government directive. If regional administration tried to override or formalize the constraint, they would face community resistance and would lose the embodied, low-overhead enforcement mechanism that makes the constraint effective without bureaucratic overhead.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__behavioral_competence, regional_government_administration, excluded,
    institutional, generational, constrained, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(stone_land_use_rule__behavioral_competence, diffuse).
narrative_ontology:fixing_cost_class(stone_land_use_rule__behavioral_competence, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Maintains practical, transmissible knowledge of flood risk across generations without requiring archival systems, formal documentation, or institutional continuity. The stone anchors spatial memory: daily movement around it reinforces the risk boundary; teaching children the stone's meaning embeds the knowledge in habitus rather than text. The coordination problem solved is: how does risk literacy survive multiple generations, demographic turnover, and forgetting?
% TRANSFER_FUNCTION: Does not move goods or money. Instead, it transfers cognitive burden from institutional memory to spatial practice: the cost of remembering the flood's extent is paid through the inconvenience of working around the marked boundary. Developers and planners pay by routing infrastructure around steeper, less developable land; households pay by accepting constrained buildable area.
% ABSENT_VOICES: Regional government administration and formal urban planners are structurally excluded from the constraint's operation. They would argue for efficiency and development; their absence means the constraint is never adjudicated against economic optimization frameworks. Engineers designing infrastructure systems for the region are also partially excluded — they must route around the boundary but are not part of the community dialogue about whether the boundary is correct.
% DISAPPEARANCE_RATIONALE: If the stone disappeared and the constraint were forgotten, settlement would expand onto the marked flood plain; flood risk literacy would degrade; when the next major flood occurs (whether within the 78-year interval or beyond), communities without the embodied knowledge would build and live in the hazard zone and suffer loss that the constraint was preventing. The rearrangement is catastrophic.
% FOUNDING_PROBLEM: A major flood 78 years before the interval start killed residents and destroyed property in a specific zone. Survivors and their descendants needed a durable way to teach subsequent generations which land was dangerous, without relying on writing, institutional continuity, or formal administration.
% FOUNDING_PROBLEM_CORROBORATION: Hydrologists confirm the marked boundary aligns with documented inundation extent from the founding flood. Community elders and multi-generation families attest that the stone's location and significance have been transmitted continuously as a part of local orientation and risk practice. Regional archival records and flood maps from external government sources corroborate that the marked zone was indeed the impact zone. The founding problem remains live because flood risk is permanent on that landscape; generations still need to know which land is dangerous.
narrative_ontology:disappearance_verdict(stone_land_use_rule__behavioral_competence, world_rearranges).
narrative_ontology:founding_problem_status(stone_land_use_rule__behavioral_competence, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(stone_land_use_rule__behavioral_competence, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(stone_land_use_rule__behavioral_competence, 'none', 1).
narrative_ontology:epsilon_provenance(stone_land_use_rule__behavioral_competence, 0.18, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stone_land_use_rule__behavioral_competence_tests).
:- end_tests(stone_land_use_rule__behavioral_competence_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is LOW (0.18) because the constraint solves a genuine coordination problem—maintaining distributed risk literacy—without concentrating benefits or imposing concentrated costs on identifiable victims. The cost is diffuse (all land developers, all households) and the benefit is dispersed (all downstream residents avoid flood risk). Suppression is also LOW (0.22) because the constraint does not require coercive infrastructure; it persists because people willingly enact it through ordinary spatial practice. The measurement series shows slight fluctuation (extractiveness dips slightly at t=52, theater peaks at t=52) consistent with variation in development pressure and cultural transmission strength, but the constraint's stability is the dominant pattern. Theater is very low (0.08) because most of the constraint's operation is functional: people route around the stone because it marks real risk, not because they are performing reverence or maintaining appearances. The suppression_requirement rises gradually from 0.15 to 0.22 across the interval, reflecting increasing pressure from development and demographic turnover that must be overcome by social enforcement to keep the boundary intact.
 *
 * PERSPECTIVAL GAP:
 *   From the downstream communities' seat, the constraint is a sophisticated knowledge technology whose persistence is a sign of cultural competence and risk wisdom. From the developers' seat, it is a frustrating restriction on profitable land use with no visible justification (the flood risk is not currently present, and the stone's meaning must be constantly re-explained). The engine will compute these divergences from the structural data: the communities will see coordination, the developers will see limitation, even though both are looking at the same constraint. This gap is not a defect—it is the measurement the system exists to take.
 *
 * DIRECTIONALITY LOGIC:
 *   Downstream communities are near the beneficiary end (d ≈ 0.2): they collect the coordination benefit of embodied risk literacy without running the coordination infrastructure themselves—the constraint runs itself through daily practice. Land-use planners and developers are near the target end (d ≈ 0.75): they bear the cost of constrained land use and must route around the boundary, but they do not capture the risk-mitigation benefit; they are organized and can articulate grievances, which moderates their effective d somewhat from a fully trapped position. Regional government sits outside the constraint's directionality structure—excluded, not extracted from. The low extractiveness reflects that the constraint's operation does not concentrate rents or impose extractive overhead; it is genuinely coordinative.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (flood risk literacy across generations) is live and will remain live as long as flood hazard persists on the landscape. The disappearance verdict is world_rearranges: if the constraint vanished, settlement would expand onto the hazard zone and future floods would strike unprepared residents. This alignment (live founding problem + rearrangement verdict) indicates the constraint is NOT mandatrophic. However, the suppression_requirement measurement series shows gradual increase (0.15 to 0.22), reflecting erosion of the constraint's self-enforcement as demographic turnover, out-migration, and development pressure weaken community consensus. If this trend continues, the constraint could become mandatrophic at some future point—the founding problem would remain live but the constraint would fail to solve it. The behavioral-competence reading places this risk in sharp focus: if the daily practice dissolves (people stop enacting the boundary), the stone becomes a lonely monument, and the constraint shifts toward the commemorative-husk reading. The measurement series documents the fragility this reading assumes.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    embodied_knowledge_transmission_fragility,
    'Can embodied spatial knowledge of the flood boundary survive three or four generations without reinforcement by catastrophic experience, or does the knowledge degrade into bare symbolism without periodic disaster to re-vivify it?',
    'Long-term ethnographic study of communities with ancient risk-marker stones across multiple generational cohorts, comparing knowledge retention in communities that experience periodic small floods (reinforcement events) vs. those that do not. Also: interviews with communities whose markers have become purely symbolic (commemorative-husk trajectory) to trace when and why behavioral enactment ceased.',
    'If embodied knowledge is inherently fragile without reinforcement, the behavioral-competence reading has a built-in expiration date—the constraint will converge toward the commemorative-husk reading over time. If knowledge can be sustained through teaching and deliberate transmission without disaster, the reading is more robust across longer intervals.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(embodied_knowledge_transmission_fragility, empirical, 'Whether behavioral enactment of the constraint can persist across generations without catastrophic reinforcement events.').

omega_variable(
    flood_risk_recurrence_uncertainty,
    'Is the 78-year interval since the founding flood representative of the actual recurrence interval for major floods on this landscape? Or is 78 years an arbitrary human-scale interval that bears no relationship to true hydrological frequency?',
    'Paleoclimate and sediment-core analysis of the flood plain to establish long-term flood frequency (data from multiple centuries or millennia). Comparison of the founding flood''s magnitude to historical and paleo-records to determine whether it was a 100-year, 500-year, or millennial event.',
    'If the founding flood was a true 100-year or 50-year event, the marked boundary is likely to be breached again within a few human generations, and the constraint''s binding force will be re-vivified by disaster. If the founding flood was a rare millennial event, the constraint may become purely vestigial long before the next breach occurs, and the behavioral-competence reading will fail as the community forgets that the stone marks real risk. The constraint''s long-term efficacy depends on flood frequency.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(flood_risk_recurrence_uncertainty, empirical, 'Whether the flood that the stone commemorates is likely to recur within timeframes relevant to maintaining community knowledge.').

omega_variable(
    reading_coexistence_vs_foreclosure,
    'Do the behavioral-competence and commemorative-husk readings genuinely foreclose each other (mutually incompatible within a single explanatory framework), or do they coexist as two valid descriptions of the same constraint at different observational scales or time windows?',
    'Ethnographic study distinguishing between: (a) readings adopted by different parties in the same community (behavioral-competence for the majority, commemorative-husk for some younger cohorts or outsiders), indicating coexistence; (b) readings adopted by the same community at different times (behavioral-competence during high-salience periods, commemorative-husk during low-salience periods), indicating temporal coexistence; (c) readings that genuinely contradict each other in community accounts of what the stone does and means, indicating foreclosure.',
    'If the readings coexist, they form a constraint family where both stories are true but describe different aspects or temporal phases. If they foreclose, the engine''s cs_axiom_contradiction gate will flag the relationship and the sibling reading will be marked as logically incompatible with this one''s core premises.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reading_coexistence_vs_foreclosure, conceptual, 'Whether behavioral-competence and commemorative-husk readings logically exclude each other or coexist as valid descriptions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stone_land_use_rule__behavioral_competence, 0, 78).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ston_tr_t0, stone_land_use_rule__behavioral_competence, theater_ratio, 0, 0.05).
narrative_ontology:measurement_basis(ston_tr_t0, observed).
narrative_ontology:measurement(ston_tr_t13, stone_land_use_rule__behavioral_competence, theater_ratio, 13, 0.06).
narrative_ontology:measurement_basis(ston_tr_t13, observed).
narrative_ontology:measurement(ston_tr_t26, stone_land_use_rule__behavioral_competence, theater_ratio, 26, 0.07).
narrative_ontology:measurement_basis(ston_tr_t26, observed).
narrative_ontology:measurement(ston_tr_t39, stone_land_use_rule__behavioral_competence, theater_ratio, 39, 0.08).
narrative_ontology:measurement_basis(ston_tr_t39, observed).
narrative_ontology:measurement(ston_tr_t52, stone_land_use_rule__behavioral_competence, theater_ratio, 52, 0.09).
narrative_ontology:measurement_basis(ston_tr_t52, observed).
narrative_ontology:measurement(ston_tr_t65, stone_land_use_rule__behavioral_competence, theater_ratio, 65, 0.08).
narrative_ontology:measurement_basis(ston_tr_t65, observed).
narrative_ontology:measurement(ston_tr_t78, stone_land_use_rule__behavioral_competence, theater_ratio, 78, 0.08).
narrative_ontology:measurement_basis(ston_tr_t78, observed).

% Extraction over time
narrative_ontology:measurement(ston_be_t0, stone_land_use_rule__behavioral_competence, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(ston_be_t0, observed).
narrative_ontology:measurement(ston_be_t13, stone_land_use_rule__behavioral_competence, base_extractiveness, 13, 0.17).
narrative_ontology:measurement_basis(ston_be_t13, observed).
narrative_ontology:measurement(ston_be_t26, stone_land_use_rule__behavioral_competence, base_extractiveness, 26, 0.18).
narrative_ontology:measurement_basis(ston_be_t26, observed).
narrative_ontology:measurement(ston_be_t39, stone_land_use_rule__behavioral_competence, base_extractiveness, 39, 0.19).
narrative_ontology:measurement_basis(ston_be_t39, observed).
narrative_ontology:measurement(ston_be_t52, stone_land_use_rule__behavioral_competence, base_extractiveness, 52, 0.18).
narrative_ontology:measurement_basis(ston_be_t52, observed).
narrative_ontology:measurement(ston_be_t65, stone_land_use_rule__behavioral_competence, base_extractiveness, 65, 0.17).
narrative_ontology:measurement_basis(ston_be_t65, observed).
narrative_ontology:measurement(ston_be_t78, stone_land_use_rule__behavioral_competence, base_extractiveness, 78, 0.18).
narrative_ontology:measurement_basis(ston_be_t78, observed).

% Suppression requirement over time
narrative_ontology:measurement(ston_su_t0, stone_land_use_rule__behavioral_competence, suppression_requirement, 0, 0.15).
narrative_ontology:measurement_basis(ston_su_t0, observed).
narrative_ontology:measurement(ston_su_t13, stone_land_use_rule__behavioral_competence, suppression_requirement, 13, 0.17).
narrative_ontology:measurement_basis(ston_su_t13, observed).
narrative_ontology:measurement(ston_su_t26, stone_land_use_rule__behavioral_competence, suppression_requirement, 26, 0.19).
narrative_ontology:measurement_basis(ston_su_t26, observed).
narrative_ontology:measurement(ston_su_t39, stone_land_use_rule__behavioral_competence, suppression_requirement, 39, 0.21).
narrative_ontology:measurement_basis(ston_su_t39, observed).
narrative_ontology:measurement(ston_su_t52, stone_land_use_rule__behavioral_competence, suppression_requirement, 52, 0.22).
narrative_ontology:measurement_basis(ston_su_t52, observed).
narrative_ontology:measurement(ston_su_t65, stone_land_use_rule__behavioral_competence, suppression_requirement, 65, 0.22).
narrative_ontology:measurement_basis(ston_su_t65, observed).
narrative_ontology:measurement(ston_su_t78, stone_land_use_rule__behavioral_competence, suppression_requirement, 78, 0.22).
narrative_ontology:measurement_basis(ston_su_t78, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stone_land_use_rule__behavioral_competence, attachment_coordination).
narrative_ontology:boltzmann_floor_override(stone_land_use_rule__behavioral_competence, 0.12).
narrative_ontology:affects_constraint(stone_land_use_rule__behavioral_competence, stone_land_use_rule__commemorative_husk).

% DUAL FORMULATION NOTE:
% The stone_land_use_rule kernel has two structurally distinct readings: behavioral_competence (this story) treats the constraint as live, functionally binding, persisting through daily spatial practice, with low extractiveness and low theater; commemorative_husk treats the same stone as a symbolic memorial whose behavioral force has decayed, with rising theater and declining functional enforcement. The readings differ on whether the constraint's operation is primarily practical (behavioral_competence) or primarily symbolic (commemorative_husk). They coexist as different parties' and different generations' readings of the same material artifact and social practice. This story generates behavioral_competence in isolation; the sibling reading (commemorative_husk) is a separate story. Both stories link to each other via network.affects_constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(stone_land_use_rule__behavioral_competence, organized, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
