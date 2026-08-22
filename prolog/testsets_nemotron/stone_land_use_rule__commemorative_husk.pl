% ============================================================================
% CONSTRAINT STORY: stone_land_use_rule__commemorative_husk
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-08-04
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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: stone_land_use_rule__commemorative_husk
 *   human_readable: Tsunami Warning Stone as Commemorative Husk
 *   domain: disaster_anthropology/institutional_memory/land_use_governance
 *
 * SUMMARY:
 *   Along Japan's Sanriku coast, hundreds of tsunami warning stones (tsunami
 *   ishihi) mark historical high-water lines with inscriptions like 'Do not
 *   build below this point.' The behavioral_competence reading treats these
 *   as live land-use prohibitions enforced by daily spatial practice and
 *   communal memory. This reading — commemorative_husk — documents the decay
 *   of that function: stones remain physically present and are ceremonially
 *   maintained, but municipal zoning routinely permits construction seaward
 *   of the stones, developers treat them as heritage assets rather than
 *   regulatory boundaries, and residents' evacuation behavior is shaped by
 *   modern warning systems, not stone inscriptions. The constraint persists
 *   as a commemorative husk — its warning function atrophied, its physical
 *   form maintained theatrically, while the land-use decisions it once
 *   governed proceed independently.
 *
 * KEY AGENTS:
 *   - coastal_developers: Primary beneficiary (powerful/arbitrage) — captures waterfront land value freed by ignoring stone warnings
 *   - municipal_planning_offices: Agenda setter (institutional/arbitrage) — administers zoning that disregards stone locations while performing commemorative maintenance
 *   - coastal_residents: Primary victim (powerless/identity_locked) — bears disaster risk from development below stone lines; identity fused to coastal lifeways makes exit unthinkable
 *   - future_generations: Victim (powerless/trapped) — inherits degraded warning infrastructure and accumulated coastal exposure
 *   - tourism_promotion_agencies: Beneficiary (organized/mobile) — markets stones as cultural heritage while benefiting from development they enable
 *   - disaster_anthropologists: Observer (analytical/analytical) — documents the decay trajectory and its structural drivers
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stone_land_use_rule__commemorative_husk, 0.72).
domain_priors:suppression_score(stone_land_use_rule__commemorative_husk, 0.25).
domain_priors:theater_ratio(stone_land_use_rule__commemorative_husk, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, extractiveness, 0.72).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stone_land_use_rule__commemorative_husk, piton).
narrative_ontology:human_readable(stone_land_use_rule__commemorative_husk, "Tsunami Warning Stone as Commemorative Husk").
narrative_ontology:topic_domain(stone_land_use_rule__commemorative_husk, "disaster_anthropology/institutional_memory/land_use_governance").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(stone_land_use_rule__commemorative_husk, '613da12d-67da-45e1-a730-f01877892ec1').
narrative_ontology:cs_kernel_codification('613da12d-67da-45e1-a730-f01877892ec1', distributed).
narrative_ontology:cs_authority_grounding('613da12d-67da-45e1-a730-f01877892ec1', practice).
narrative_ontology:cs_interpretation_layer_present('613da12d-67da-45e1-a730-f01877892ec1').
narrative_ontology:cs_reading_relation('613da12d-67da-45e1-a730-f01877892ec1', stone_land_use_rule__behavioral_competence, coexists_with).
narrative_ontology:cs_axiom('613da12d-67da-45e1-a730-f01877892ec1', foundational, commemoration_supersedes_warning).
narrative_ontology:cs_axiom_status(commemoration_supersedes_warning, holdable).
narrative_ontology:cs_axiom_grounding('613da12d-67da-45e1-a730-f01877892ec1', commemoration_supersedes_warning, conventional).
narrative_ontology:cs_axiom('613da12d-67da-45e1-a730-f01877892ec1', secondary, heritage_performance_legitimizes_development).
narrative_ontology:cs_axiom_status(heritage_performance_legitimizes_development, holdable).
narrative_ontology:cs_axiom_grounding('613da12d-67da-45e1-a730-f01877892ec1', heritage_performance_legitimizes_development, instrumental).
narrative_ontology:cs_reference_frame('613da12d-67da-45e1-a730-f01877892ec1', stone_as_live_prohibition).
narrative_ontology:cs_drift_state('613da12d-67da-45e1-a730-f01877892ec1', post_bubble_development_era, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('613da12d-67da-45e1-a730-f01877892ec1', '').
narrative_ontology:cs_kernel_id(stone_land_use_rule__commemorative_husk, stone_land_use_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stone_land_use_rule__commemorative_husk, coastal_developers).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__commemorative_husk, municipal_planning_offices).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__commemorative_husk, tourism_promotion_agencies).
narrative_ontology:constraint_victim(stone_land_use_rule__commemorative_husk, coastal_residents).
narrative_ontology:constraint_victim(stone_land_use_rule__commemorative_husk, future_generations).
narrative_ontology:constraint_vindicates(stone_land_use_rule__commemorative_husk, institutional_amnesia_as_governance_strategy).
narrative_ontology:constraint_vindicates(stone_land_use_rule__commemorative_husk, disaster_memory_decay_under_development_pressure).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop residential and commercial properties on coastal land seaward of tsunami warning stones. The stones' commemorative status adds cultural cachet to projects while their warning function is ignored in permitting. Captures the land-value premium of waterfront locations without bearing the disaster risk — that risk is externalized to residents and future generations. Can shift investments to other regions if local politics change.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, coastal_developers, beneficiary,
    powerful, biographical, arbitrage, regional).

% Administer zoning and building permits that routinely approve construction below stone warning lines. Maintain the stones physically (cleaning, protective shelters, signage) and ceremonially (annual memorial events, school visits) — this commemorative theater legitimizes the planning regime while enabling the development that expands the tax base. Officials rotate across jurisdictions; the institutional position persists regardless of individual turnover.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, municipal_planning_offices, agenda_setter,
    institutional, generational, arbitrage, regional).
narrative_ontology:stakeholder_secondary_role(stone_land_use_rule__commemorative_husk, municipal_planning_offices, beneficiary).

% Live in communities developed below stone warning lines. Their identity, livelihood, and social world are fused to the coastal lifeway — fishing, generational homes, community networks. They cannot 'choose' to exit the risk because leaving means dissolving the self. They participate in stone ceremonies (maintenance, memorials) which reinforces the commemorative frame while the material risk accumulates. Evacuation plans exist but are calibrated to modern warning systems, not stone inscriptions.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, coastal_residents, payer,
    powerless, biographical, identity_locked, local).

% Inherit the accumulated coastal exposure: denser development in inundation zones, degraded warning infrastructure, and a commemorative culture that obscures the risk. No voice in current decisions; the constraint's decay is a transfer from their future safety to present development value. Exit is structurally impossible — they do not yet exist to choose.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, future_generations, payer,
    powerless, generational, trapped, local).

% Market tsunami stones as cultural heritage destinations — 'memorial tourism' routes, interpretive centers, guided walks. This narrative frames the stones as objects of remembrance rather than active warnings, which aligns with the development interests that fund tourism promotion. Can redirect marketing to other heritage assets if the commemorative frame becomes politically inconvenient.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, tourism_promotion_agencies, beneficiary,
    organized, biographical, mobile, regional).

% Document the decay trajectory of stone-based warning systems across the Sanriku coast and comparable settings worldwide. Analyze the structural drivers: development pressure, institutional amnesia, identity-locked populations, and the commemorative capture of disaster memory. Their work does not change the constraint but creates the analytical record that makes the drift visible.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, disaster_anthropologists, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Originally: a distributed, intergenerational spatial memory system that prevented settlement in tsunami inundation zones without centralized enforcement — the stones themselves were the coordinate, readable by anyone walking the coast. Currently: a commemorative coordination that aligns heritage performance, tourism narrative, and development permitting around a shared symbolic object, enabling waterfront development while maintaining the appearance of disaster respect.
% TRANSFER_FUNCTION: Moves disaster risk from developers and municipal budgets (who would bear the cost of genuine setback zoning, elevation requirements, or relocation) onto coastal residents and future generations. Moves land-value premium from the commons (risk-constrained coastal land) to private developers who build below stone lines. The commemorative maintenance budget is a small transfer from public funds to the heritage-performance apparatus that legitimizes the larger transfer.
% ABSENT_VOICES: The voices of those who died in past tsunamis and whose memories the stones were erected to preserve — they would object to development below the lines they died to mark. The voices of children in coastal communities who participate in stone ceremonies but are not told the stones' warnings are functionally ignored. The voices of geological time — the next tsunami will arrive regardless of commemorative framing.
% DISAPPEARANCE_RATIONALE: If all tsunami warning stones vanished overnight, the commemorative infrastructure (ceremonies, tourism routes, heritage budgets) would collapse — municipal planning offices would lose the heritage veneer that legitimizes waterfront permitting, tourism agencies would lose a flagship product, and residents would lose the symbolic anchor of their identity. But the material land-use pattern would not change: development below the former stone lines would continue because the zoning regime that enables it is independent of the stones' physical presence. The world rearranges symbolically and institutionally; the material risk landscape stays the same.
% FOUNDING_PROBLEM: Preventing settlement in tsunami inundation zones in a pre-modern context where no centralized warning system, evacuation infrastructure, or scientific inundation modeling existed. The stones were a distributed, intergenerational memory technology: each generation could read the coast and know where the sea had reached.
% FOUNDING_PROBLEM_CORROBORATION: Disaster historians and geologists (outside the beneficiary set) confirm that modern tsunami warning systems, inundation modeling, and evacuation planning have superseded the stones' original spatial-memory function. The Japan Meteorological Agency's warning system and municipal evacuation plans are the operational successors. Municipal planning offices (beneficiaries) claim the founding problem is 'live' in a commemorative sense — that the stones maintain 'disaster awareness' — but this is a reframing that obscures the functional decay. No independent technical authority attests that the stones currently function as land-use constraints.
narrative_ontology:disappearance_verdict(stone_land_use_rule__commemorative_husk, world_rearranges).
narrative_ontology:founding_problem_status(stone_land_use_rule__commemorative_husk, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(stone_land_use_rule__commemorative_husk, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_nemotron', 'agent/example_platform_commission.json',
    'nvidia/nemotron-3-ultra-550b-a55b:free', 'max_tokens=65536,temperature=model_default,reasoning=disabled').
narrative_ontology:story_seed(stone_land_use_rule__commemorative_husk, 'none', 1).
narrative_ontology:epsilon_provenance(stone_land_use_rule__commemorative_husk, 0.72, 'nvidia/nemotron-3-ultra-550b-a55b:free', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(stone_land_use_rule__commemorative_husk_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(stone_land_use_rule__commemorative_husk, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(stone_land_use_rule__commemorative_husk_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness rises from 0.15 to 0.72 over 50 years as waterfront development value increases and stone warnings are systematically overridden — the constraint's form legitimizes the development that extracts value from risk displacement. Theater ratio rises from 0.10 to 0.78 as commemorative maintenance (cleaning, signage, ceremonies) replaces any behavioral enforcement. Suppression requirement falls from 0.85 to 0.18 because the constraint no longer suppresses development — it enables it by providing a heritage veneer. Accessibility collapse is moderate (0.35) because alternatives (relocation, elevation, genuine zoning) exist but are politically disfavored. Resistance is low (0.15) because the constraint's decay serves powerful interests and residents' identity-locked position prevents effective opposition.
 *
 * PERSPECTIVAL GAP:
 *   From the planning office seat, the stones are heritage assets being properly maintained — a coordination success. From the coastal resident seat, the same stones are a betrayal: their warning function has been hollowed out while development proceeds below them. From the developer seat, the stones are a marketing amenity that adds cultural cachet to waterfront properties. The engine computes this divergence from the structural data: agenda_setter with arbitrage exit sees low effective extraction; identity_locked payer sees high effective extraction; analytical observer sees the full drift trajectory.
 *
 * DIRECTIONALITY LOGIC:
 *   Coastal developers (powerful, arbitrage exit) are structural beneficiaries: they capture the land-value differential created by ignoring stone warnings. Municipal planning offices (institutional, arbitrage) are agenda setters who administer the zoning regime that overrides the stones while performing commemorative theater. Coastal residents (powerless, identity_locked) are primary victims: they bear the disaster risk from development below stone lines, and their identity fusion with coastal lifeways makes exit structurally unavailable — they cannot 'choose' to leave the constraint's effects. Future generations (powerless, trapped) inherit the accumulated exposure. Tourism agencies (organized, mobile) benefit from the heritage narrative without bearing risk. The extraction flows from risk-bearing residents to value-capturing developers, mediated by planning offices that legitimize the transfer through commemorative maintenance.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem — preventing settlement in tsunami inundation zones — is dead in its original form (modern warning systems and evacuation planning have superseded stone-based spatial memory), but the arrangement persists because the commemorative form enables waterfront development that benefits powerful actors. The piton classification captures this: the constraint's primary function (behavioral land-use prohibition) has atrophied, but the physical and ceremonial form persists due to institutional inertia and the value it unlocks for developers. No party profits enough to maintain the original function, and no party is hurt enough to restore it — the cost of genuine zoning enforcement exceeds what planning offices bear, while the distributed risk to residents is insufficiently concentrated to drive political action.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_commitment,
    'Is this constraint one reading of the contested kernel ''stone_land_use_rule''? What does the sibling reading change structurally?',
    'Compare the behavioral_competence reading''s land-use prohibition against this reading''s commemorative-only operation; the delta is whether building decisions are constrained by stone location.',
    'Confirms this is the commemorative_husk reading of kernel stone_land_use_rule; the sibling reading behavioral_competence would instantiate a genuine land-use constraint with low extractiveness and high suppression.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_commitment, conceptual, 'This story instantiates the commemorative_husk reading of kernel stone_land_use_rule; sibling reading is behavioral_competence.').

omega_variable(
    memory_decay_mechanism,
    'Is the stone''s decay from warning to symbol driven by passive forgetting, active institutional suppression, or development pressure displacing memory?',
    'Trace municipal council records for explicit deliberations about stone warnings vs. development permits; interview planning officials on whether stone locations factor into zoning decisions.',
    'If active suppression, the constraint is a snare using commemoration as cover; if passive decay with development capture, it is a piton where the arrangement persists because no one bears enough cost to restore its function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(memory_decay_mechanism, empirical, 'Whether the warning''s decay was passive, actively suppressed, or displaced by development pressure.').

omega_variable(
    extraction_without_enforcement,
    'How does extraction operate without active enforcement? Who pays and who collects when the constraint is purely symbolic?',
    'Measure land-value differentials between stone-constrained and unconstrained parcels; track permit approvals that violate stone warnings; identify who captures the value of ignored warnings.',
    'If developers capture waterfront value while residents bear disaster risk, extraction is real despite absent enforcement — the constraint''s form enables the transfer by legitimizing development.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_without_enforcement, empirical, 'Whether symbolic constraints can extract value without active enforcement machinery.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stone_land_use_rule__commemorative_husk, 0, 50).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ston_tr_t0, stone_land_use_rule__commemorative_husk, theater_ratio, 0, 0.1).
narrative_ontology:measurement_basis(ston_tr_t0, observed).
narrative_ontology:measurement(ston_tr_t10, stone_land_use_rule__commemorative_husk, theater_ratio, 10, 0.25).
narrative_ontology:measurement_basis(ston_tr_t10, observed).
narrative_ontology:measurement(ston_tr_t20, stone_land_use_rule__commemorative_husk, theater_ratio, 20, 0.45).
narrative_ontology:measurement_basis(ston_tr_t20, observed).
narrative_ontology:measurement(ston_tr_t30, stone_land_use_rule__commemorative_husk, theater_ratio, 30, 0.6).
narrative_ontology:measurement_basis(ston_tr_t30, observed).
narrative_ontology:measurement(ston_tr_t40, stone_land_use_rule__commemorative_husk, theater_ratio, 40, 0.7).
narrative_ontology:measurement_basis(ston_tr_t40, observed).
narrative_ontology:measurement(ston_tr_t50, stone_land_use_rule__commemorative_husk, theater_ratio, 50, 0.78).
narrative_ontology:measurement_basis(ston_tr_t50, observed).

% Extraction over time
narrative_ontology:measurement(ston_be_t0, stone_land_use_rule__commemorative_husk, base_extractiveness, 0, 0.15).
narrative_ontology:measurement_basis(ston_be_t0, observed).
narrative_ontology:measurement(ston_be_t10, stone_land_use_rule__commemorative_husk, base_extractiveness, 10, 0.28).
narrative_ontology:measurement_basis(ston_be_t10, observed).
narrative_ontology:measurement(ston_be_t20, stone_land_use_rule__commemorative_husk, base_extractiveness, 20, 0.42).
narrative_ontology:measurement_basis(ston_be_t20, observed).
narrative_ontology:measurement(ston_be_t30, stone_land_use_rule__commemorative_husk, base_extractiveness, 30, 0.55).
narrative_ontology:measurement_basis(ston_be_t30, observed).
narrative_ontology:measurement(ston_be_t40, stone_land_use_rule__commemorative_husk, base_extractiveness, 40, 0.64).
narrative_ontology:measurement_basis(ston_be_t40, observed).
narrative_ontology:measurement(ston_be_t50, stone_land_use_rule__commemorative_husk, base_extractiveness, 50, 0.72).
narrative_ontology:measurement_basis(ston_be_t50, observed).

% Suppression requirement over time
narrative_ontology:measurement(ston_su_t0, stone_land_use_rule__commemorative_husk, suppression_requirement, 0, 0.85).
narrative_ontology:measurement_basis(ston_su_t0, observed).
narrative_ontology:measurement(ston_su_t10, stone_land_use_rule__commemorative_husk, suppression_requirement, 10, 0.65).
narrative_ontology:measurement_basis(ston_su_t10, observed).
narrative_ontology:measurement(ston_su_t20, stone_land_use_rule__commemorative_husk, suppression_requirement, 20, 0.4).
narrative_ontology:measurement_basis(ston_su_t20, observed).
narrative_ontology:measurement(ston_su_t30, stone_land_use_rule__commemorative_husk, suppression_requirement, 30, 0.25).
narrative_ontology:measurement_basis(ston_su_t30, observed).
narrative_ontology:measurement(ston_su_t40, stone_land_use_rule__commemorative_husk, suppression_requirement, 40, 0.2).
narrative_ontology:measurement_basis(ston_su_t40, observed).
narrative_ontology:measurement(ston_su_t50, stone_land_use_rule__commemorative_husk, suppression_requirement, 50, 0.18).
narrative_ontology:measurement_basis(ston_su_t50, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stone_land_use_rule__commemorative_husk, identity_coordination).
narrative_ontology:boltzmann_floor_override(stone_land_use_rule__commemorative_husk, 0.12).
narrative_ontology:affects_constraint(stone_land_use_rule__commemorative_husk, stone_land_use_rule__behavioral_competence).
narrative_ontology:affects_constraint(stone_land_use_rule__commemorative_husk, coastal_zoning_reform).
narrative_ontology:affects_constraint(stone_land_use_rule__commemorative_husk, disaster_memory_institutionalization).

% DUAL FORMULATION NOTE:
% Kernel stone_land_use_rule decomposes into two structurally distinct constraints: behavioral_competence (active land-use prohibition, low extraction, genuine coordination) and commemorative_husk (symbolic maintenance, high extraction, development enablement). They are linked because the commemorative form derives legitimacy from the behavioral claim while structurally contradicting it — the heritage narrative legitimizes the development the warning would forbid.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(stone_land_use_rule__commemorative_husk, institutional, 0.35).
constraint_indexing:directionality_override(stone_land_use_rule__commemorative_husk, powerless, 0.85).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
