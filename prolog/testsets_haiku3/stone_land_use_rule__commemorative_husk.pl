% ============================================================================
% CONSTRAINT STORY: stone_land_use_rule__commemorative_husk
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
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
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
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
 *   constraint_id: stone_land_use_rule__commemorative_husk
 *   human_readable: Stone Memorial as Decayed Land-Use Warning
 *   domain: institutional_memory/disaster_anthropology/land_use
 *
 * SUMMARY:
 *   A stone monument marks a waterfront zone flooded repeatedly in prior
 *   centuries, with local knowledge and the artifact encoding a land-use
 *   restriction: do not build here. Over generations, the stone's functional
 *   meaning has degraded from operational constraint to symbolic
 *   commemoration. Developers now build freely on the marked land; the
 *   municipal administration maintains the stone as a historical artifact and
 *   ceremonial focal point without enforcing the underlying prohibition. The
 *   constraint's extractiveness has risen as the flood-vulnerable waterfront
 *   has filled with profitable infrastructure, while the theater of memorial
 *   maintenance has increased to mask the evaporated warning function. This
 *   reading instantiates the constraint as a decayed warning—zero remaining
 *   land-use effect, high extraction toward developers, diffuse risk transfer
 *   to low-income waterfront residents.
 *
 * KEY AGENTS:
 *   - Waterfront developers: extract benefit from unconstrained development on marked flood zone
 *   - Municipal administration: agenda-setter maintaining memorial theater; permits development
 *   - Flood-vulnerable populations: bear flood risk on land nominally warned against; powerless exit
 *   - Historical preservation community: curate stone as artifact; institutional interest aligns with theater
 *   - Indigenous knowledge holders: excluded from memorial administration; original prohibitory knowledge displaced
 *   - Flood-risk engineers: technical authority competing with or superseding stone's authority
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(stone_land_use_rule__commemorative_husk, 0.81).
domain_priors:suppression_score(stone_land_use_rule__commemorative_husk, 0.15).
domain_priors:theater_ratio(stone_land_use_rule__commemorative_husk, 0.78).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, extractiveness, 0.81).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, theater_ratio, 0.78).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, accessibility_collapse, 0.22).
narrative_ontology:constraint_metric(stone_land_use_rule__commemorative_husk, resistance, 0.42).

% --- Constraint claim ---
narrative_ontology:constraint_claim(stone_land_use_rule__commemorative_husk, piton).
narrative_ontology:human_readable(stone_land_use_rule__commemorative_husk, "Stone Memorial as Decayed Land-Use Warning").
narrative_ontology:topic_domain(stone_land_use_rule__commemorative_husk, "institutional_memory/disaster_anthropology/land_use").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(stone_land_use_rule__commemorative_husk, '841fe9f0-0080-463c-ae6d-a01ebdca4cbe').
narrative_ontology:cs_kernel_codification('841fe9f0-0080-463c-ae6d-a01ebdca4cbe', fixed_text).
narrative_ontology:cs_authority_grounding('841fe9f0-0080-463c-ae6d-a01ebdca4cbe', practice).
narrative_ontology:cs_reading_relation('841fe9f0-0080-463c-ae6d-a01ebdca4cbe', stone_land_use_rule__behavioral_competence, coexists_with).
narrative_ontology:cs_axiom('841fe9f0-0080-463c-ae6d-a01ebdca4cbe', foundational, institutional_memory_via_artifact_primary).
narrative_ontology:cs_axiom_status(institutional_memory_via_artifact_primary, holdable).
narrative_ontology:cs_axiom_grounding('841fe9f0-0080-463c-ae6d-a01ebdca4cbe', institutional_memory_via_artifact_primary, conventional).
narrative_ontology:cs_axiom('841fe9f0-0080-463c-ae6d-a01ebdca4cbe', secondary, symbolic_preservation_decoupled_from_enforcement).
narrative_ontology:cs_axiom_status(symbolic_preservation_decoupled_from_enforcement, holdable).
narrative_ontology:cs_axiom_grounding('841fe9f0-0080-463c-ae6d-a01ebdca4cbe', symbolic_preservation_decoupled_from_enforcement, conventional).
narrative_ontology:cs_reference_frame('841fe9f0-0080-463c-ae6d-a01ebdca4cbe', stone_as_operational_warning).
narrative_ontology:cs_drift_state('841fe9f0-0080-463c-ae6d-a01ebdca4cbe', contemporary_heritage_reframing, gap(practice_drift, severe, false)).
narrative_ontology:cs_created_at('841fe9f0-0080-463c-ae6d-a01ebdca4cbe', '').
narrative_ontology:cs_kernel_id(stone_land_use_rule__commemorative_husk, stone_land_use_rule).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(stone_land_use_rule__commemorative_husk, waterfront_developers).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__commemorative_husk, municipal_administration).
narrative_ontology:constraint_victim(stone_land_use_rule__commemorative_husk, flood_vulnerable_populations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(stone_land_use_rule__commemorative_husk, historical_preservation_community).
narrative_ontology:constraint_vindicates(stone_land_use_rule__commemorative_husk, collective_memory_persistence_via_artifact).
narrative_ontology:constraint_vindicates(stone_land_use_rule__commemorative_husk, historical_precedent_as_governance_anchor).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop valuable waterfront property unconstrained by the stone's nominal land-use warning. The stone's decayed status permits construction decisions driven by profit and convenience; they benefit from the memorial's symbolic immunity from enforcement while the historical prohibition it commemorates has evaporated as operational constraint.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, waterfront_developers, beneficiary,
    powerful, biographical, arbitrage, regional).

% Maintains the stone and its surrounding ceremonial/commemorative perimeter. The stone provides a low-cost, theater-intensive memorial to historical disaster without the political cost of enforcing the underlying land-use restriction it originally encoded. Administers the artifact; permits development adjacent to it.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, municipal_administration, agenda_setter,
    institutional, generational, constrained, regional).

% Occupy or depend on infrastructure and housing built on land the stone nominally warned against building on. They absorb flood risk without understanding the stone's original function; the memorial's symbolic status masks the operational constraint that eroded, leaving them exposed. Exit requires geographic relocation with limited economic means.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, flood_vulnerable_populations, payer,
    powerless, immediate, trapped, local).

% Values the stone as a cultural artifact and disaster memory anchor. They curate its interpretation as memorial; their institutional interest in artifact preservation aligns with municipal theater maintenance. They do not typically contest development adjacent to the stone.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, historical_preservation_community, beneficiary,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(stone_land_use_rule__commemorative_husk, historical_preservation_community, observer).

% May hold original knowledge of the land-use warning encoded in the stone (seasonal flood patterns, prior disasters) and would contest development on the grounds of both practical disaster prevention and cultural stewardship. Their voices are excluded from the administrative memorial process; the stone's symbolic reframing as history-object rather than as operational warning displaces their practical knowledge.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, indigenous_communities, excluded,
    moderate, civilizational, constrained, regional).

% Produce technical assessments of waterfront flood risk that increasingly contradict the stone's nominal warning or justify development conditional on engineered mitigation. Their expertise either renders the stone technically obsolete or replaces it as the source of land-use authority.
narrative_ontology:constraint_stakeholder(stone_land_use_rule__commemorative_husk, flood_risk_engineers, observer,
    powerful, generational, mobile, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(stone_land_use_rule__commemorative_husk, waterfront_developers).
narrative_ontology:fixing_cost_class(stone_land_use_rule__commemorative_husk, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The stone originally coordinated spatial practice: residents and builders avoided construction in the marked flood zone, distributing risk away from vulnerable low-lying waterfront and maintaining settlement on safer upland. The coordination problem was: how to transmit knowledge of recurring flood danger across generations and make it operationally binding on settlement decisions?
% TRANSFER_FUNCTION: In the commemorative reading, the stone transfers the liability of historical remembrance from active behavioral enforcement to passive symbolic gesture. The extraction is diffuse: flood risk is moved back to the waterfront (where profitable development now occurs), and the symbolic cost of maintaining the memorial absorbs administrative resources while conferring no protective benefit.
% ABSENT_VOICES: Indigenous communities whose original knowledge the stone encoded are excluded from the memorial's administrative reframing. Flood-vulnerable populations who would object to development on the marked land are not represented in heritage conversations. Flood-risk engineers are consulted but their technical warnings often compete with rather than reinforce the stone's authority.
% DISAPPEARANCE_RATIONALE: If the stone vanished, waterfront development would accelerate visibly; there would be no counternarrative (however decayed) to the developer-favorable trajectory. The removal would erase even the symbolic anchor, leaving flood risk untempered by any institutional memory object. Conversely, if the stone were re-activated as an operational constraint (no building on the marked land), development patterns would reverse and resource allocation would shift—the land-use restriction would function again.
% FOUNDING_PROBLEM: A major flood (seasonal or catastrophic) killed people and destroyed property on low-lying waterfront; survivors and descendants marked the danger zone with a stone to warn future settlers and builders away. The problem was preventing recurrence through spatial avoidance.
% FOUNDING_PROBLEM_CORROBORATION: Flood-risk engineers' technical assessments confirm that the marked zone remains flood-prone and that development there increases vulnerability. Indigenous knowledge holders confirm the periodic nature of flooding. Municipal archives document the original prohibition. Municipal administration and developers both attest that the contemporary constraint is purely memorial; no one claims active land-use enforcement tied to the stone.
narrative_ontology:disappearance_verdict(stone_land_use_rule__commemorative_husk, world_rearranges).
narrative_ontology:founding_problem_status(stone_land_use_rule__commemorative_husk, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(stone_land_use_rule__commemorative_husk, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(stone_land_use_rule__commemorative_husk, 'none', 1).
narrative_ontology:epsilon_provenance(stone_land_use_rule__commemorative_husk, 0.81, 'claude-haiku-4-5-20251001', 'none', direct).

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
 *   Extractiveness rises from 0.34 to 0.81 across the interval as development intensifies on the marked waterfront and the flood-vulnerable population's exposure grows. Theater rises from 0.28 to 0.78 as the stone shifts from a partly-enforced warning to a purely ceremonial memorial; the gap between theater and extractiveness (0.78 vs 0.81) is minimal because almost all the stone's contemporary function is symbolic maintenance with no behavioral enforcement. Suppression is consistently low (0.15 at endpoints) because the constraint persists not through coercion but through institutional inertia and the absence of any organized resistance to development (flood-vulnerable populations lack power to resist; the memorial's symbolic immunity from critique makes enforcement unnecessary). The measurement grid is shared across all three metrics: every metric is authored at every time point within the interval, enabling coherent temporal analysis.
 *
 * PERSPECTIVAL GAP:
 *   The developer and municipal administration seats experience the stone as a coordination success (valuable memorial maintained cheaply); the flood-vulnerable populations experience it as a failed constraint (the warning that should protect them has become merely symbolic). The engine computes these divergent perceptions from power (developers and municipalities are powerful/institutional; flood-vulnerable populations are powerless) and exit (developers have arbitrage options; flood populations are trapped). The claim/metric independence is operative: the constraint is claimed as piton (decayed theater, no real function) while the metrics show high extractiveness and high theater—the gap between claim and metrics IS the diagnostic fact, not an error.
 *
 * DIRECTIONALITY LOGIC:
 *   Developers benefit structurally (d ≈ 0.1, strong beneficiary) because the stone's decayed status permits profitable waterfront development unconstrained by the original prohibition. Municipal administration is the agenda-setter (d ≈ 0.35, coordinating the memorial but also captured by development interests). Flood-vulnerable populations are targets (d ≈ 0.95, nearly full target) because they bear flood risk on land the stone nominally warned against and have no exit. The historical preservation community benefits slightly (d ≈ 0.25) through institutional legitimacy. Indigenous communities are excluded (d not computed in the engine for role=excluded seats, but structurally would be near 1.0 if included, as their knowledge is displaced without compensation). Flood-risk engineers are observers (d = 0.5 by convention).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits classic piton structure: the founding problem (flood prevention) is dead; the administrative actor (municipality) could change the arrangement (rezone the waterfront as no-build) but does not because the cost of fixing (losing development revenue, confronting property interests) exceeds the municipality's direct flood exposure (they manage the risk through infrastructure, not land use). The beneficiary (developers) is diffuse across multiple enterprises, so no single actor captures enough to maintain the constraint actively. The theater of memorial maintenance absorbs the extractive reality, permitting the constraint to persist without explicit enforcement. The payer (flood-vulnerable populations) lack the power to demand resurrection of the constraint. The mandatrophy is resolved not by fixing the underlying land-use rule but by transforming the stone's meaning from operational prohibition to historical artifact, a form of legitimacy drift that makes the constraint's absence seem natural rather than extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    memory_decay_vs_deliberate_displacement,
    'Did the stone''s warning function decay through organic social forgetting, or was it deliberately displaced by administrative and development interests reframing it as memorial rather than prohibition?',
    'Historical reconstruction from municipal archives, oral history from long-term residents and indigenous knowledge holders, and comparison of the stone''s treatment over time (when was the memorial framing initiated; what constraints were loosened around it).',
    'If decay is organic, the constraint is a natural piton—institutional inertia with no malevolent agent. If deliberately displaced, the constraint is an active extraction mechanism disguised as preservation. The classification remains piton in both cases, but the mechanism differs: natural forgetting vs. institutional capture.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(memory_decay_vs_deliberate_displacement, empirical, 'Whether the stone''s functional obsolescence arose from social forgetting or institutional reframing.').

omega_variable(
    suppression_internalization_in_flood_populations,
    'Do flood-vulnerable populations accept the waterfront development as inevitable because of engineered flood control, or because they have internalized the displacement of the stone''s authority by heritage and technical discourses?',
    'Qualitative interviews with long-term waterfront residents and recent arrivals; comparison of flood-preparedness behavior and knowledge of the stone''s original function; measurement of resistance movements against waterfront zoning.',
    'If internalized, the measured low suppression (0.15) understates the constraint''s coercive effect—the targets have incorporated the suppression without external enforcement. If engineered-control driven, the suppression is genuinely low because alternative structural mechanisms (dikes, levees) replace the land-use restriction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(suppression_internalization_in_flood_populations, conceptual, 'Whether suppression is structural (low barriers to development) or internalized (communities accept waterfront building as safe via engineering).').

omega_variable(
    stone_as_kernel_vs_stone_as_constraint,
    'Is the stone itself the kernel (a fixed, ambiguous artifact subject to reading), or is the land-use rule the kernel (the stable institutional commitment) and the stone merely one expression of it?',
    'Examine whether the stone''s physical presence anchors the readings or whether the readings derive from the underlying land-use commitment independent of the artifact. Test by hypothetically removing the stone: do the readings and their structural implications change?',
    'If the stone is the kernel, the two readings (behavioral, commemorative) are interpretations of a single object and may be expected to coexist or compete for epistemic authority over the same artifact. If the land-use rule is the kernel, the stone is one manifestation among several (written codes, oral tradition, practice), and the readings reflect different commitments to maintaining the rule itself. The decomposition into two constraints changes if the kernel boundary shifts.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(stone_as_kernel_vs_stone_as_constraint, conceptual, 'Whether the contested entity is the stone-as-artifact or the land-use prohibition it encodes.').

omega_variable(
    indigenous_knowledge_displacement_mechanism,
    'Is the indigenous knowledge the stone encodes preserved anywhere outside the stone itself, or does reframing the stone as heritage-object (rather than as operational constraint) constitute a final displacement of that knowledge from practical governance?',
    'Documentation audit of indigenous knowledge in municipal planning, land-management institutions, and community memory; comparison of waterfront decision-making before and after stone reframing.',
    'If knowledge is preserved elsewhere, the stone''s displacement is symbolic but not epistemic. If the stone was the sole institutional anchor for the knowledge, reframing it as memorial constitutes knowledge loss at the governance level, even if cultural memory persists. This affects whether the constraint is accurately classified as piton (inert artifact) or as snare (suppressing an alternative knowledge system).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indigenous_knowledge_displacement_mechanism, empirical, 'Whether reframing the stone as heritage displaces indigenous knowledge from governance institutions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(stone_land_use_rule__commemorative_husk, 0, 80).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ston_tr_t0, stone_land_use_rule__commemorative_husk, theater_ratio, 0, 0.28).
narrative_ontology:measurement_basis(ston_tr_t0, observed).
narrative_ontology:measurement(ston_tr_t10, stone_land_use_rule__commemorative_husk, theater_ratio, 10, 0.41).
narrative_ontology:measurement_basis(ston_tr_t10, observed).
narrative_ontology:measurement(ston_tr_t20, stone_land_use_rule__commemorative_husk, theater_ratio, 20, 0.54).
narrative_ontology:measurement_basis(ston_tr_t20, observed).
narrative_ontology:measurement(ston_tr_t40, stone_land_use_rule__commemorative_husk, theater_ratio, 40, 0.68).
narrative_ontology:measurement_basis(ston_tr_t40, observed).
narrative_ontology:measurement(ston_tr_t60, stone_land_use_rule__commemorative_husk, theater_ratio, 60, 0.75).
narrative_ontology:measurement_basis(ston_tr_t60, observed).
narrative_ontology:measurement(ston_tr_t80, stone_land_use_rule__commemorative_husk, theater_ratio, 80, 0.78).
narrative_ontology:measurement_basis(ston_tr_t80, observed).

% Extraction over time
narrative_ontology:measurement(ston_be_t0, stone_land_use_rule__commemorative_husk, base_extractiveness, 0, 0.34).
narrative_ontology:measurement_basis(ston_be_t0, observed).
narrative_ontology:measurement(ston_be_t10, stone_land_use_rule__commemorative_husk, base_extractiveness, 10, 0.45).
narrative_ontology:measurement_basis(ston_be_t10, observed).
narrative_ontology:measurement(ston_be_t20, stone_land_use_rule__commemorative_husk, base_extractiveness, 20, 0.58).
narrative_ontology:measurement_basis(ston_be_t20, observed).
narrative_ontology:measurement(ston_be_t40, stone_land_use_rule__commemorative_husk, base_extractiveness, 40, 0.71).
narrative_ontology:measurement_basis(ston_be_t40, observed).
narrative_ontology:measurement(ston_be_t60, stone_land_use_rule__commemorative_husk, base_extractiveness, 60, 0.79).
narrative_ontology:measurement_basis(ston_be_t60, observed).
narrative_ontology:measurement(ston_be_t80, stone_land_use_rule__commemorative_husk, base_extractiveness, 80, 0.81).
narrative_ontology:measurement_basis(ston_be_t80, observed).

% Suppression requirement over time
narrative_ontology:measurement(ston_su_t0, stone_land_use_rule__commemorative_husk, suppression_requirement, 0, 0.22).
narrative_ontology:measurement_basis(ston_su_t0, observed).
narrative_ontology:measurement(ston_su_t10, stone_land_use_rule__commemorative_husk, suppression_requirement, 10, 0.18).
narrative_ontology:measurement_basis(ston_su_t10, observed).
narrative_ontology:measurement(ston_su_t20, stone_land_use_rule__commemorative_husk, suppression_requirement, 20, 0.15).
narrative_ontology:measurement_basis(ston_su_t20, observed).
narrative_ontology:measurement(ston_su_t40, stone_land_use_rule__commemorative_husk, suppression_requirement, 40, 0.12).
narrative_ontology:measurement_basis(ston_su_t40, observed).
narrative_ontology:measurement(ston_su_t60, stone_land_use_rule__commemorative_husk, suppression_requirement, 60, 0.09).
narrative_ontology:measurement_basis(ston_su_t60, observed).
narrative_ontology:measurement(ston_su_t80, stone_land_use_rule__commemorative_husk, suppression_requirement, 80, 0.15).
narrative_ontology:measurement_basis(ston_su_t80, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(stone_land_use_rule__commemorative_husk, resource_allocation).
narrative_ontology:boltzmann_floor_override(stone_land_use_rule__commemorative_husk, 0.12).
narrative_ontology:affects_constraint(stone_land_use_rule__commemorative_husk, stone_land_use_rule__behavioral_competence).

% DUAL FORMULATION NOTE:
% The stone_land_use_rule kernel decomposes into two constraints: behavioral_competence (stone as live operational prohibition enforcing spatial practice) and commemorative_husk (stone as memorial artifact with decayed warning function, high extraction toward development). The two readings share a physical referent (the stone) but instantiate different constraints with different epsilon values, different beneficiary/victim structures, and different temporal trajectories. The behavioral reading establishes the original coordination problem and the stone's effectiveness; the commemorative reading describes the constraint's decay and the drift toward pure extraction. Both readings are live positions held by different institutional and community actors; neither forecloses the other within the ongoing dispute over land-use governance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(stone_land_use_rule__commemorative_husk, powerless, 0.95).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
