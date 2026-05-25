% ============================================================================
% CONSTRAINT STORY: gaza_evacuation_route_accessibility
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_gaza_evacuation_route_accessibility, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: gaza_evacuation_route_accessibility
 *   human_readable: Gaza Evacuation Route Accessibility Constraint
 *   domain: conflict/humanitarian/logistics
 *
 * SUMMARY:
 *   The evacuation route in Gaza represents a critical constraint governing
 *   civilian freedom of movement under conflict conditions. Nominally,
 *   evacuation routes enable populations to leave conflict zones and access
 *   safety and humanitarian assistance. Structurally, the routes are
 *   controlled by checkpoint authorities who determine access eligibility,
 *   timing, and capacity. The constraint exhibits core characteristics of a
 *   Snare: high extractiveness (0.68), high suppression (0.75), and active
 *   enforcement of movement restrictions. Civilians are trapped — they have
 *   no alternative route and no way to exit the zone without passing through
 *   checkpoints. Humanitarian organizations are constrained — they depend on
 *   checkpoint approval for convoy access. International frameworks are
 *   degraded, persisting through institutional inertia while enforcement
 *   mechanisms weaken. The analytical observer identifies a tangled rope
 *   structure: the route genuinely serves coordination functions (enabling
 *   escape from conflict, facilitating humanitarian delivery) while
 *   simultaneously serving extraction functions (controlling population
 *   movements, consolidating power through access management, collecting
 *   intelligence through checkpoint interactions). The theater ratio (0.55)
 *   reflects moderate performative activity — checkpoints operate with stated
 *   humanitarian protocols and international law compliance, creating a
 *   legitimacy cover that obscures the extraction mechanism. The
 *   extractiveness has increased over the measurement interval (0.52 → 0.68)
 *   as approval procedures tightened and capacity bottlenecks deepened, while
 *   theater has remained relatively stable, suggesting that performative
 *   compliance continues even as actual extraction increases.
 *
 * KEY AGENTS:
 *   - Civilian Population Seeking Evacuation: Primary victims (powerless/trapped) — dependent on checkpoint access with no alternatives; bear full cost of movement restrictions
 *   - Humanitarian Organizations: Secondary victims (moderate/constrained) — operationally dependent on checkpoint approval; face resource constraints and operational restrictions
 *   - Checkpoint Authority: Primary beneficiary (institutional/arbitrage) — controls access, determines flow rates, operates with discretionary power; can redirect authority or restrict access entirely
 *   - Military Command Structure: Secondary beneficiary (institutional/arbitrage) — benefits from population monitoring and movement intelligence through checkpoint system
 *   - International Aid Coordinator: Complex position (powerful/constrained) — has diplomatic leverage but constrained by negotiation complexity; experiences constraint as both coordination problem and extraction mechanism
 *   - International Legal Framework: Institutional actor (institutional/constrained) — humanitarian law persists nominally but enforcement is degraded; theater maintains legitimacy while actual protections weaken
 *   - Analytical Observer: Sees full structure (analytical/analytical) — identifies tangled rope pattern where coordination and extraction functions are coupled
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(gaza_evacuation_route_accessibility, 0.68).
domain_priors:suppression_score(gaza_evacuation_route_accessibility, 0.75).
domain_priors:theater_ratio(gaza_evacuation_route_accessibility, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(gaza_evacuation_route_accessibility, extractiveness, 0.68).
narrative_ontology:constraint_metric(gaza_evacuation_route_accessibility, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(gaza_evacuation_route_accessibility, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(gaza_evacuation_route_accessibility, snare).
narrative_ontology:human_readable(gaza_evacuation_route_accessibility, "Gaza Evacuation Route Accessibility Constraint").
narrative_ontology:topic_domain(gaza_evacuation_route_accessibility, "conflict/humanitarian/logistics").

domain_priors:requires_active_enforcement(gaza_evacuation_route_accessibility).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(gaza_evacuation_route_accessibility, military_command_structure).
narrative_ontology:constraint_beneficiary(gaza_evacuation_route_accessibility, checkpoint_control_authorities).
narrative_ontology:constraint_victim(gaza_evacuation_route_accessibility, civilian_population_seeking_evacuation).
narrative_ontology:constraint_victim(gaza_evacuation_route_accessibility, humanitarian_organizations).
narrative_ontology:constraint_victim(gaza_evacuation_route_accessibility, displaced_persons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CIVILIAN EVACUEE (SNARE) — Trapped by geography and military control. The evacuation route exists nominally but is controlled by checkpoint authorities who determine access, timing, and movement capacity. No alternative exists; leaving the designated zone requires passage through the constraint. Civilians experience maximum extraction: their freedom of movement is the commodity being controlled.
constraint_indexing:constraint_classification(gaza_evacuation_route_accessibility, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: HUMANITARIAN ORGANIZATION (SNARE) — Constrained by dependency on checkpoint authorities for convoy access and operational approval. Can theoretically withdraw operations but faces pressure to remain and serve populations. Experiences extraction through operational restrictions, capacity limitations, and approval delays. Suppression is high — alternatives (informal routes, unauthorized convoys) carry severe penalties.
constraint_indexing:constraint_classification(gaza_evacuation_route_accessibility, snare,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: INTERNATIONAL AID COORDINATOR (TANGLED ROPE) — Powerful institutional actor with diplomatic leverage but constrained by negotiation complexity. Experiences the constraint as both coordination problem (facilitating civilian movement, enabling humanitarian access) and extraction mechanism (having to negotiate permissions, facing denial of access). Benefits from appearing to manage crisis; bears cost of access restrictions.
constraint_indexing:constraint_classification(gaza_evacuation_route_accessibility, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: CHECKPOINT AUTHORITY (ROPE) — Primary beneficiary with arbitrage options. Controls the constraint as a coordination mechanism: determines safe passage procedures, manages flow rates, and coordinates with military structures. Experiences the constraint as coordination problem that they are solving through checkpoint administration. Can divert authority to alternative checkpoint systems or completely restrict flow (arbitrage options).
constraint_indexing:constraint_classification(gaza_evacuation_route_accessibility, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: INTERNATIONAL LEGAL FRAMEWORK (PITON) — Humanitarian law, refugee conventions, and access agreements exist nominally but enforcement is degraded. Theater ratio of performative compliance (checkpoints ostensibly follow humanitarian protocols while operating as extraction mechanisms) is high. International frameworks persist through institutional inertia rather than effective implementation. Suppression is high but obscured by procedural legitimacy.
constraint_indexing:constraint_classification(gaza_evacuation_route_accessibility, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — The evacuation route serves dual function: genuine coordination (enabling civilian movement, humanitarian access) AND asymmetric extraction (controlling movement patterns, monitoring populations, consolidating power through access control). Active enforcement is required to maintain both functions simultaneously. This is the canonical structure of tangled rope: coordination benefit exists alongside high extraction, with suppression obscuring the asymmetry.
constraint_indexing:constraint_classification(gaza_evacuation_route_accessibility, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(gaza_evacuation_route_accessibility_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(gaza_evacuation_route_accessibility, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gaza_evacuation_route_accessibility, TypeOther, context(agent_power(powerful), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(gaza_evacuation_route_accessibility, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(gaza_evacuation_route_accessibility, TR),
    TR >= 0.70.

:- end_tests(gaza_evacuation_route_accessibility_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The evacuation route exists but is fundamentally controlled by checkpoint authorities who determine who can evacuate, when, and under what conditions. This control flow yields significant extraction: movement becomes a commodity that authorities allocate. The value reflects both the direct cost of access denial (people unable to evacuate) and the indirect cost of the uncertainty and delay imposed by discretionary approval. Suppression (0.75): High. Multiple barriers prevent independent evacuation: geographic constraints (routes must pass through controlled territory), legal barriers (unauthorized movement is prohibited), enforcement barriers (military presence deters informal routes), and informational barriers (populations may not know alternatives or procedures). The suppression is structurally high, though some informal routes may exist. Theater ratio (0.55): Moderate. The checkpoint system operates with stated humanitarian protocols, international law compliance, and civilian protection rhetoric. However, this theater does not dominate — much of the actual operation is straightforward logistics (queuing, credential verification, movement direction). The moderate value reflects that theater is significant but not the primary function, unlike in piton-classified constraints where theater would exceed 0.70. The extractiveness trajectory (0.52 → 0.68 over 90 days) indicates intensification of access control, while theater ratio remains stable (0.48 → 0.55), suggesting that performative elements are not increasing — the rise in extractiveness reflects tightening of actual control, not escalating theater.
 *
 * PERSPECTIVAL GAP:
 *   The constraint presents a stark perspectival gap. The checkpoint authority (Rope) sees coordination: they are managing a logistics problem, allocating access fairly according to their criteria, operating within their mandate. They have discretion and exit options. The trapped civilian (Snare) sees pure extraction: they have no choice, no alternative, and no negotiating position. They can only comply. The humanitarian organization (Tangled Rope) sees both: they are coordinating humanitarian response (coordination benefit) while also being constrained by checkpoint restrictions (extraction cost). The international legal framework (Piton) sees a degraded system: they understand that humanitarian law should protect civilians, but enforcement is weak; they maintain procedures performatively. The international aid coordinator (Tangled Rope) sees a problem requiring negotiation and leverage: the constraint is not immutable from their position, but it is not easily changed either. The analytical observer (Tangled Rope) sees that this gap is not accidental: the checkpoint system structurally serves both coordination (enabling movement under control) and extraction (controlling movement to concentrate power). The gap reveals that the different perspectives are not measuring the same phenomenon from different angles — they are measuring genuinely different causal relationships that the same institutional structure mediates.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality differs radically across agents. For the trapped civilian (powerless/trapped), d ≈ 0.95 → f(d) ≈ 1.42: maximum experienced extraction. They cannot escape; they depend on checkpoint approval for the only viable route out. The checkpoint authority (institutional/arbitrage) derives d ≈ 0.05 → f(d) ≈ -0.12: they experience negative effective extraction because they control the constraint's operation. They benefit from it; they are not subjected to it. The humanitarian organization (moderate/constrained) derives d ≈ 0.70 → f(d) ≈ 1.10: they experience significant extraction because they face costs (operational restrictions, approval delays, conditional access) but also have some benefits (access to populations, coordinating role). The analytical observer (analytical/analytical) uses canonical d ≈ 0.73 → f(d) ≈ 1.15: neutral position, not beneficiary or victim, but seeing the full structure including both extraction and coordination functions. The beneficiary/victim declarations feed directly into these calculations: civilians are victims (high d), checkpoint authorities are beneficiaries (low d), humanitarian organizations are mixed (moderate d).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint exhibits the classic mandatrophy pattern of coercion-coordination confusion. The checkpoint authority's mandate is humanitarian (enable civilian evacuation, ensure safe passage) but the mechanism is extraction (control movement, concentrate power through access allocation). The mandatrophy resolves through the tangled rope classification: both mandates are operative. The system genuinely does enable some evacuation (coordination function exists) while also enabling extractive control (extraction function exists). The analytical observer identifies this as tangled rope specifically because: (1) beneficiaries exist (checkpoint authority, military command) who benefit from the constraint's operation; (2) victims exist (civilian population, humanitarian access) who bear costs from the constraint's restrictions; (3) active enforcement is required to maintain both functions; (4) the extraction is asymmetric (not symmetric coordination cost). The snare classification from the trapped civilian's perspective is also accurate — from that position, the constraint appears as pure extraction because the coordination benefit (being able to evacuate) is so constrained by restrictions and delays that it is barely realized. The mandatrophy resolves by noting that the constraint can be simultaneously Rope (from checkpoint authority view), Snare (from trapped civilian view), and Tangled Rope (from analytical view) because these are measuring different causal structures from different structural positions. There is no single 'true' type — the presheaf of perspectives over the constraint's structural positions IS the answer.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    evacuation_route_operational_intent,
    'Is the evacuation route designed to enable civilian movement or to concentrate and control populations?',
    'Analysis of route design (capacity constraints vs. geographic alternatives), checkpoint placement (bottleneck vs. distributed control), approval patterns (permissive vs. restrictive), and outcome data (evacuation rates vs. population concentrations)',
    'If primary intent is enabling: reclassify as Rope or Scaffold (coordination dominant). If primary intent is control: confirm Snare (extraction dominant). The mechanism is the same; intent determines whether suppression is coordination cost or coercion.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(evacuation_route_operational_intent, empirical, 'Whether evacuation route prioritizes civilian movement or population control').

omega_variable(
    suppression_mechanism_structural_vs_internalized,
    'Is suppression of evacuation due to external physical/logistical barriers (structural) or psychological barriers (internalized fear/distrust)?',
    'Comparison of evacuation rates when external barriers are removed (if occurs) vs. lingering hesitation; analysis of documented checkpoint interactions; assessment of whether populations will use routes when barriers are temporarily lifted',
    'If primarily structural: suppression metric reflects objective barriers. If partially internalized: actual suppression is higher than structural measure suggests — populations carry suppression internally even if barriers drop.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_structural_vs_internalized, empirical, 'Whether suppression is external barrier or internalized psychological constraint').

omega_variable(
    humanitarian_coordination_vs_extraction_coupling,
    'Does the humanitarian access requirement create genuine coordination benefit, or does it primarily serve as legitimacy cover for extraction control?',
    'Analysis of how much humanitarian capacity is actually enabled by the route vs. how much is disabled by checkpoint restrictions; assessment of whether checkpoint authorities use humanitarian access as leverage for other concessions; comparison of actual vs. stated humanitarian needs satisfaction',
    'If genuine coordination: tangled rope classification is appropriate. If primarily extractive: reclassify as pure Snare with humanitarian theater as suppression mechanism.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(humanitarian_coordination_vs_extraction_coupling, empirical, 'Whether humanitarian access is genuine coordination function or extraction cover').

omega_variable(
    alternative_informal_route_viability,
    'Do informal evacuation routes exist and are they materially viable, or are suppression barriers preventing their use?',
    'Documentation of informal route usage; analysis of barriers (military enforcement, geographic impassability, safety hazards); assessment of population knowledge of alternatives',
    'If viable alternatives exist: exit_options should be upgraded from ''trapped'' to ''constrained'' for powerless agents. If alternatives are suppressed: confirms trapped classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_informal_route_viability, empirical, 'Viability and accessibility of informal evacuation routes').

omega_variable(
    checkpoint_approval_decision_process_transparency,
    'Is the checkpoint approval process for evacuation transparent and rule-based, or opaque and discretionary?',
    'Documentation of approval criteria, decision timelines, appeal procedures, and consistency of application across similar cases; analysis of denial reasons and patterns',
    'If transparent/rule-based: some mitigation of extraction mechanism through predictability. If opaque/discretionary: increases suppression and enables rent-seeking behavior within checkpoint authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(checkpoint_approval_decision_process_transparency, empirical, 'Transparency and consistency of evacuation approval decision-making').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(gaza_evacuation_route_accessibility, 0, 90).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(gaza_evac_tr_t0, gaza_evacuation_route_accessibility, theater_ratio, 0, 0.48).
narrative_ontology:measurement(gaza_evac_tr_t30, gaza_evacuation_route_accessibility, theater_ratio, 30, 0.52).
narrative_ontology:measurement(gaza_evac_tr_t60, gaza_evacuation_route_accessibility, theater_ratio, 60, 0.55).
narrative_ontology:measurement(gaza_evac_tr_t90, gaza_evacuation_route_accessibility, theater_ratio, 90, 0.55).

% Extraction over time
narrative_ontology:measurement(gaza_evac_be_t0, gaza_evacuation_route_accessibility, base_extractiveness, 0, 0.52).
narrative_ontology:measurement(gaza_evac_be_t30, gaza_evacuation_route_accessibility, base_extractiveness, 30, 0.64).
narrative_ontology:measurement(gaza_evac_be_t60, gaza_evacuation_route_accessibility, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(gaza_evac_be_t90, gaza_evacuation_route_accessibility, base_extractiveness, 90, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(gaza_evacuation_route_accessibility, resource_allocation).
narrative_ontology:affects_constraint(gaza_evacuation_route_accessibility, checkpoint_control_authority).
narrative_ontology:affects_constraint(gaza_evacuation_route_accessibility, humanitarian_access_restrictions).
narrative_ontology:affects_constraint(gaza_evacuation_route_accessibility, displacement_population_concentration).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(gaza_evacuation_route_accessibility, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
