% ============================================================================
% CONSTRAINT STORY: unclos_dispute_resolution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unclos_dispute_resolution, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: unclos_dispute_resolution
 *   human_readable: UNCLOS Dispute Resolution Mechanism
 *   domain: international_law/maritime_governance
 *
 * SUMMARY:
 *   The United Nations Convention on the Law of the Sea (UNCLOS) established
 *   an international dispute resolution mechanism to arbitrate maritime
 *   boundary disputes, resource allocation conflicts, and maritime rights
 *   claims without recourse to military force. The mechanism appears neutral
 *   — a formal procedure applying established law. However, the dispute
 *   resolution process structurally extracts from weaker states through
 *   litigation costs, technical expertise requirements, and procedural
 *   complexity that favor well-resourced nations. Small island states and
 *   developing coastal nations are trapped within UNCLOS (cannot exit
 *   dependence on ocean access and international maritime law) yet cannot
 *   afford effective participation in disputes. Wealthy maritime nations can
 *   extend litigation, hire specialized counsel, and exit to bilateral
 *   negotiation if outcomes trend unfavorably. The mechanism coordinates by
 *   preventing resource conflicts from escalating to military confrontation,
 *   yet distributes outcomes asymmetrically. Theater ratio has increased over
 *   the interval (0.42 to 0.58) as environmental impact documentation has
 *   become more elaborate while enforcement of environmental protections has
 *   remained weak, reflecting Goodhart drift where procedural complexity
 *   substitutes for actual constraint.
 *
 * KEY AGENTS:
 *   - Small Island States: Primary victims (powerless/trapped) — existential dependence on ocean access; cannot afford litigation; subject to tribunal outcomes without meaningful voice
 *   - Developing Coastal Nations: Secondary victims (moderate/constrained) — face significant litigation costs; benefit somewhat from tribunal protection against unilateral appropriation; constrained by resource and expertise gaps
 *   - Wealthy Maritime Nations: Primary beneficiaries (powerful/arbitrage) — can afford extended litigation; can exit to bilateral negotiation; benefit from legal legitimization of resource claims
 *   - International Tribunal for the Law of the Sea: Institutional beneficiary (institutional/arbitrage) — case volume sustains institutional relevance and budget; can adjust interpretations to maintain authority
 *   - Ocean Commons / Environmental Advocates: Victims (organized/constrained) — environmental impact documentation is theater; enforcement of environmental protections is minimal
 *   - Regional Coalitions / NGO Networks: Reform agents (organized/constrained) — building capacity to reduce extraction asymmetry; sunset logic through distributed legal expertise
 *   - Analytical Observer: Systemic view (analytical/analytical) — sees coordination function (prevents warfare) and extraction mechanism (cost asymmetry) as dual features
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unclos_dispute_resolution, 0.52).
domain_priors:suppression_score(unclos_dispute_resolution, 0.65).
domain_priors:theater_ratio(unclos_dispute_resolution, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unclos_dispute_resolution, extractiveness, 0.52).
narrative_ontology:constraint_metric(unclos_dispute_resolution, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(unclos_dispute_resolution, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unclos_dispute_resolution, tangled_rope).
narrative_ontology:human_readable(unclos_dispute_resolution, "UNCLOS Dispute Resolution Mechanism").
narrative_ontology:topic_domain(unclos_dispute_resolution, "international_law/maritime_governance").

domain_priors:requires_active_enforcement(unclos_dispute_resolution).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unclos_dispute_resolution, powerful_maritime_states).
narrative_ontology:constraint_beneficiary(unclos_dispute_resolution, international_tribunal).
narrative_ontology:constraint_victim(unclos_dispute_resolution, small_island_states).
narrative_ontology:constraint_victim(unclos_dispute_resolution, developing_coastal_nations).
narrative_ontology:constraint_victim(unclos_dispute_resolution, ocean_commons).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: SMALL ISLAND STATES (SNARE) — Trapped within UNCLOS dispute mechanisms with no realistic exit. Cannot exit the treaty (existential dependence on ocean access). Cannot afford legal representation or extended litigation. Dispute resolution extracts sovereignty (forced arbitration outcomes) with minimal coordination benefit. High suppression — limited financial resources, no asymmetric power in proceedings, minimal voice in outcomes.
constraint_indexing:constraint_classification(unclos_dispute_resolution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: DEVELOPING COASTAL NATIONS (TANGLED ROPE) — Constrained by costs of litigation and technical capacity gaps, but benefit from dispute resolution mechanism's existence (prevents resource-rich nations from unilateral appropriation). Mixed extraction: procedure is biased toward wealthy litigants, yet mechanism provides some protection against total expropriation. Active enforcement required to maintain the procedural system.
constraint_indexing:constraint_classification(unclos_dispute_resolution, tangled_rope,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 3: WEALTHY MARITIME NATIONS (ROPE) — Can afford specialized maritime law expertise, can extend litigation timelines, can exit to bilateral negotiation if unfavorable trend emerges. Benefit from the dispute mechanism as coordination: legitimizes their resource claims through legal procedure rather than military force. Extraction minimal — the constraint enables their preferred outcome while appearing neutral.
constraint_indexing:constraint_classification(unclos_dispute_resolution, rope,
    context(agent_power(powerful),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: INTERNATIONAL TRIBUNAL (ROPE) — Institutional beneficiary. Dispute volume sustains tribunal budget and institutional relevance. Experiences mechanism as pure coordination — dispute processing is the tribunal's function. High arbitrage: can redirect non-payment or non-compliance cases to other forums, can adjust interpretations to maintain institutional authority.
constraint_indexing:constraint_classification(unclos_dispute_resolution, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: OCEAN COMMONS (PITON) — The dispute mechanism performs environmental representation while lacking actual enforcement power. Environmental impact assessments are written into proceedings but rarely bind outcomes. Theater ratio is high — extensive ecological documentation is generated, but final allocation decisions typically favor established fisheries and resource extraction over conservation. Mechanism persists through international norm adherence despite low functional protection of ocean resources.
constraint_indexing:constraint_classification(unclos_dispute_resolution, piton,
    context(agent_power(organized),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: REGIONAL COALITIONS (SCAFFOLD) — Organized entities working to build capacity (legal training, technical expertise) to enable smaller states to participate effectively in dispute resolution. See the mechanism as temporary coordination failure solvable by capacity building. Sunset rationale: as legal capacity and technical expertise distribute more broadly (through NGO networks, regional organizations), smaller states' constraint loosens and the extractive asymmetry declines.
constraint_indexing:constraint_classification(unclos_dispute_resolution, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From civilizational view, UNCLOS dispute resolution genuinely coordinates maritime access without overt warfare (the coordination function is real — prevents resource conflicts from becoming military conflicts). Yet the mechanism extracts disproportionately from weaker states through litigation costs and expertise asymmetry. Effective extraction increases with power inequality. Theater ratio reflects that procedure appears neutral while outcomes track wealth and naval capacity.
constraint_indexing:constraint_classification(unclos_dispute_resolution, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unclos_dispute_resolution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(unclos_dispute_resolution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(unclos_dispute_resolution, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(unclos_dispute_resolution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(unclos_dispute_resolution, TR),
    TR >= 0.70.

:- end_tests(unclos_dispute_resolution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): Moderate-high. The dispute mechanism extracts from weaker states through litigation costs, technical expertise barriers, and procedural complexity. However, extraction is not maximal because the mechanism does provide genuine coordination value — it prevents maritime resource conflicts from escalating to military confrontation and offers some protection against unilateral appropriation. Wealthy nations benefit from legitimization of their claims; developing nations benefit from constraint on wealthy nations' unilateral action, but asymmetrically. Suppression (0.65): Moderate-high. Financial barriers to litigation (specialized counsel, extended proceedings, technical expert witnesses) are substantial. Procedural complexity favors technically sophisticated participants. Power asymmetry in negotiating strength within formal procedures is significant. However, suppression is not total because UNCLOS provides a formal procedure with stated neutrality and some smaller states do successfully litigate. Theater ratio (0.58): Moderate. Environmental impact assessments, scientific documentation, and procedural formality create performative elements. However, the mechanism also produces genuine legal outcomes affecting resource allocation. The theater has increased over the measurement interval as environmental documentation requirements have expanded while actual environmental enforcement has remained weak (Goodhart drift: procedure substitutes for outcome).
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits maximum perspectival gap between beneficiaries and victims. Wealthy maritime nations (powerful/arbitrage) see pure coordination (Rope) — the mechanism enables their preferred outcomes through legal legitimacy rather than military force. Small island states (powerless/trapped) see pure extraction (Snare) — forced participation in a procedure they cannot afford and cannot exit. The ITLOS (institutional/arbitrage) sees its own function as coordination (Rope) — case processing sustains institutional relevance. The analytical observer sees the true structure: genuine coordination coupled with asymmetric extraction (Tangled Rope). The perspectival gap between the beneficiary's rope and the victim's snare reveals the mechanism's dual function. The piton perspective (ocean commons) notes that environmental documentation is elaborate and performative while actual environmental protection is weak — the mechanism has shifted from substantive protection toward procedural theater.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality values (d) are derived from structural position within the dispute mechanism. Wealthy maritime nations (beneficiaries with arbitrage options) derive d ≈ 0.15: they benefit from the mechanism's legitimacy while maintaining exit options to bilateral negotiation. This produces negative effective extraction — the mechanism subsidizes their preferred outcomes. Small island states (victims trapped in UNCLOS dependency) derive d ≈ 0.95: they cannot exit, bear full cost of litigation participation, and experience outcomes as imposed. This produces high effective extraction. Developing coastal nations (victims but with some constrained options) derive d ≈ 0.70: they cannot afford extended litigation but benefit somewhat from procedural protection. The ITLOS (institutional beneficiary) derives d ≈ 0.05: case volume sustains institutional function; the mechanism is their core activity. The analytical observer uses canonical d ≈ 0.73 (analytical power atom) to capture civilizational-scale assessment: the mechanism genuinely coordinates maritime peace while extracting from weaker states.
 *
 * MANDATROPHY ANALYSIS:
 *   RESOLVED: The constraint is structurally Tangled Rope. It possesses both genuine coordination function (prevents maritime conflicts from becoming military confrontations) and asymmetric extraction (weaker states bear disproportionate litigation costs relative to benefits). The mandatrophy is resolved by recognizing that the beneficiary perspectives (wealthy nations, ITLOS) genuinely experience the mechanism as pure coordination (Rope) because they benefit and have exit options. The victim perspectives (small island states, developing nations) genuinely experience the mechanism as extraction (Snare or Tangled Rope at best) because costs are asymmetric and exit is unavailable. The analytical observer's Tangled Rope classification integrates both realities: the constraint coordinated action while extracting from weaker participants. The mechanism appears neutral (theater ratio 0.58) but produces outcomes correlated with power and wealth rather than legal merit alone.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    litigation_cost_barrier_measurement,
    'What threshold of litigation cost distinguishes fair procedural burden from extractive barrier?',
    'Comparative analysis of case success rates, settlement patterns, and tribunal outcomes correlated with litigant GDP and maritime resource value; longitudinal tracking of participation by nation income level',
    'If barrier is systematic: constraint is snare for poorest nations, tangled rope for moderate nations. If threshold is merely procedurally uniform: constraint is rope for all participants (disputes are genuinely arbitrated).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(litigation_cost_barrier_measurement, empirical, 'Threshold distinguishing procedural cost from extractive barrier').

omega_variable(
    environmental_enforcement_power,
    'Do environmental impact findings in UNCLOS disputes actually constrain resource allocation outcomes, or are they performed documentation?',
    'Longitudinal analysis of tribunal rulings; correlation between environmental impact assessments and allocation decisions; comparison of stated environmental protections vs actual enforcement',
    'If findings constrain outcomes: ocean commons is beneficiary (rope perspective valid). If purely performative: ocean commons is victim (piton perspective confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(environmental_enforcement_power, empirical, 'Whether environmental findings constrain allocation outcomes').

omega_variable(
    capacity_building_sunset_timeline,
    'At what capacity threshold would the litigation cost barrier cease to function as extraction mechanism?',
    'Simulation of distributed legal capacity; tracking of NGO training networks and regional legal expertise; correlation of capacity increases with litigation cost ratios; empirical test of whether capability parity reduces outcome gaps',
    'If achievable < 20 years: scaffold sunset is structural. If > 50 years: sunset is aspirational, not real constraint property.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(capacity_building_sunset_timeline, empirical, 'Timeline for capacity building to eliminate extraction via litigation cost').

omega_variable(
    exit_option_reality,
    'For small island states, is bilateral negotiation a realistic exit from UNCLOS dispute resolution, or is it structurally unavailable?',
    'Case analysis of bilateral negotiations vs tribunal cases; outcome comparison; tracking of power imbalance in bilateral vs multilateral dispute resolution',
    'If bilateral negotiation is unavailable: exit_options is ''trapped'' (not ''constrained''). If available but disadvantageous: trapped is correct.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(exit_option_reality, empirical, 'Whether small states have realistic exit to bilateral negotiation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unclos_dispute_resolution, 1982, 2012).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unclos_tr_t0, unclos_dispute_resolution, theater_ratio, 0, 0.42).
narrative_ontology:measurement(unclos_tr_t15, unclos_dispute_resolution, theater_ratio, 15, 0.54).
narrative_ontology:measurement(unclos_tr_t30, unclos_dispute_resolution, theater_ratio, 30, 0.58).

% Extraction over time
narrative_ontology:measurement(unclos_be_t0, unclos_dispute_resolution, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(unclos_be_t15, unclos_dispute_resolution, base_extractiveness, 15, 0.47).
narrative_ontology:measurement(unclos_be_t30, unclos_dispute_resolution, base_extractiveness, 30, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unclos_dispute_resolution, enforcement_mechanism).
narrative_ontology:boltzmann_floor_override(unclos_dispute_resolution, 0.18).
narrative_ontology:affects_constraint(unclos_dispute_resolution, maritime_boundary_disputes).
narrative_ontology:affects_constraint(unclos_dispute_resolution, ocean_resource_allocation).
narrative_ontology:affects_constraint(unclos_dispute_resolution, small_state_maritime_vulnerability).

% DUAL FORMULATION NOTE:
% UNCLOS dispute resolution is an institutional mechanism that coordinates maritime access while extracting from weaker states through procedural barriers. This constraint is upstream of specific maritime disputes (boundary conflicts, resource allocation) which it shapes through the litigation cost asymmetry. The mechanism's theater ratio (0.58) reflects that environmental documentation has become increasingly elaborate while enforcement power has stagnated — a Goodhart drift where process complexity substitutes for substantive constraint.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(unclos_dispute_resolution, institutional, 0.05).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
