% ============================================================================
% CONSTRAINT STORY: landscape_of_fear_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_landscape_of_fear_2026, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: landscape_of_fear_2026
 *   human_readable: The Landscape of Fear
 *   domain: biological/behavioral_ecology
 *
 * SUMMARY:
 *   The landscape of fear is a structural constraint wherein the mere
 *   presence or realistic perception of predators induces prey behavioral
 *   modifications that reduce foraging efficiency, suppress reproduction, and
 *   increase mortality from non-predation causes far beyond the direct
 *   killing power of predators themselves. This constraint exemplifies a pure
 *   snare from most perspectives: prey populations experience extraction of
 *   energy, reproduction, and survival without meaningful coordination
 *   benefit. The fear landscape emerges naturally from evolutionary selection
 *   pressures and the neurobiology of threat perception — all organisms with
 *   survival-relevant sensory systems will exhibit fear responses to
 *   predation cues. The constraint is nearly universal in ecosystems with
 *   predators and exhibits minimal theater ratio (0.34) because the mechanism
 *   is functional, not performative: fear responses directly alter behavior
 *   in ways that reduce predation risk. However, the analytical observer
 *   risks naturalizing this as an immutable feature of evolutionary life,
 *   when in fact it is a highly contingent outcome of current predator-prey
 *   arms races. The constraint differs from a pure mountain because the
 *   specific configuration of the fear landscape — which cues trigger fear,
 *   which habitats are perceived as safe, which behavioral trade-offs prey
 *   make — is shaped by recent evolutionary history and can shift as predator
 *   presence and tactics change.
 *
 * KEY AGENTS:
 *   - Individual Prey Organism: Primary victim (powerless/trapped) — bears full cost of fear-driven behavioral suppression; cannot exit predation risk perception
 *   - Prey Population: Primary victim (moderate/constrained) — experiences reduced fecundity and altered age structure; can migrate but predator landscapes are pervasive
 *   - Energy Allocation / Metabolism: Victim (powerless/trapped) — abstract metabolic system that bears the energetic cost of vigilance and stress response
 *   - Reproductive Fitness: Victim (powerless/trapped) — abstract fitness component extracted by reproduction-safety tradeoff in high-fear contexts
 *   - Predator Population: Primary beneficiary (institutional/arbitrage) — benefits from prey behavioral modifications that reduce search costs and create predictable habitat use
 *   - Evolutionary Observer: Analytical perspective (analytical/analytical) — risks naturalizing contingent outcome as universal law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(landscape_of_fear_2026, 0.58).
domain_priors:suppression_score(landscape_of_fear_2026, 0.72).
domain_priors:theater_ratio(landscape_of_fear_2026, 0.34).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(landscape_of_fear_2026, extractiveness, 0.58).
narrative_ontology:constraint_metric(landscape_of_fear_2026, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(landscape_of_fear_2026, theater_ratio, 0.34).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(landscape_of_fear_2026, accessibility_collapse, 0.89).
narrative_ontology:constraint_metric(landscape_of_fear_2026, resistance, 0.08).

% --- Constraint claim ---
narrative_ontology:constraint_claim(landscape_of_fear_2026, snare).
narrative_ontology:human_readable(landscape_of_fear_2026, "The Landscape of Fear").
narrative_ontology:topic_domain(landscape_of_fear_2026, "biological/behavioral_ecology").

domain_priors:emerges_naturally(landscape_of_fear_2026).

% --- Structural relationships ---
narrative_ontology:constraint_victim(landscape_of_fear_2026, prey_populations).
narrative_ontology:constraint_victim(landscape_of_fear_2026, energy_allocation_metabolism).
narrative_ontology:constraint_victim(landscape_of_fear_2026, reproductive_fitness).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EVOLUTIONARY BIOLOGIST / NATURAL LAW VIEW (MOUNTAIN) — From a universal/civilizational perspective, predator-induced fear constraints emerge directly from fundamental evolutionary pressures and neurological architecture. Prey perception of predation risk is an immutable consequence of survival selection — organisms that fail to exhibit fear responses are selected out. This constraint has zero degrees of freedom: any prey population in a heterogeneous environment with predators will exhibit fear-driven behavior modification. The accessibility collapse (0.89) reflects that no prey organism can engineer around the neurobiology of threat detection. Resistance (0.08) shows minimal capacity to resist or overcome: the constraint is embedded in the nervous system itself.
constraint_indexing:constraint_classification(landscape_of_fear_2026, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

% PERSPECTIVE 2: INDIVIDUAL PREY ORGANISM (SNARE) — Cannot exit the constraint of fear perception; trapped in real-time threat assessment in a landscape where predators are present. Bears the full cost of behavioral restriction: reduced foraging efficiency, energy depletion, suppressed reproduction, increased vulnerability to other mortality sources. Maximum experienced extraction. The prey organism has no alternative but to allocate resources to vigilance, predator avoidance, and habitat use decisions that prioritize safety over energy intake. The suppression (0.72) reflects strong coercion: the neurobiological fear response operates beneath voluntary control. Exit options are zero — the organism cannot choose not to perceive predation risk.
constraint_indexing:constraint_classification(landscape_of_fear_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 3: PREY POPULATION (SNARE) — Constrained by the persistent presence of predators and the population-wide behavioral modifications they induce. The population experiences reduced fecundity, increased juvenile mortality from starvation (prey are so cautious they fail to acquire sufficient nutrition), and altered age structure. The fitness cost is substantial and inescapable — no population can maintain breeding sites or forage without encountering predation risk. Some regional mobility exists (migration to predator-free patches if available), but at the geographic scales where this matters, predator landscapes are pervasive. Suppression is high: the population cannot negotiate or escape the fundamental constraint of predator presence.
constraint_indexing:constraint_classification(landscape_of_fear_2026, snare,
    context(agent_power(moderate),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: PREDATOR POPULATION (ROPE) — Benefits from the constraint of prey fear. Predator presence generates behavioral modifications in prey that reduce prey vigilance when predators are actually absent and create predictable habitat use patterns when predators are present. Both effects reduce predator search costs. The fear landscape functions as a coordination mechanism: predator persistence drives prey spatial distribution and temporal activity patterns toward configurations that benefit predator encounter rates. The predator experiences this as a beneficial coordination system — their presence organizes the prey landscape in advantageous ways. Exit options include arbitrage: predators can adjust hunting pressure, migrate, or shift to alternative prey. The constraint is functional and generating clear benefits.
constraint_indexing:constraint_classification(landscape_of_fear_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: ENERGY ALLOCATION AND METABOLISM (SNARE) — The metabolic costs of fear are non-negotiable: vigilance behavior, predator avoidance, freezing responses, and stress hormone production all extract energy from the prey organism's budget. Resources allocated to fear-driven behavioral suppression are unavailable for growth, reproduction, or immune function. This victim is abstract — metabolism itself — but it bears real extraction. A prey organism in a high-fear landscape may spend 30-50% of foraging time in vigilance rather than feeding, directly reducing caloric intake and energy reserves. Suppression is absolute: the organism cannot opt out of thermodynamic costs of physiological stress response.
constraint_indexing:constraint_classification(landscape_of_fear_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 6: REPRODUCTIVE FITNESS (SNARE) — The fear landscape extracts directly from breeding success. Prey organisms suppress or delay reproduction in high-fear contexts; females exhibit reduced fecundity; mating opportunities are constrained by habitat use changes (prey avoid open areas where predation risk is highest, reducing encounter rates with mates). Juveniles experience elevated mortality from starvation because parental prey are too cautious to forage efficiently. The fitness cost is large and inescapable. No prey population can escape the reproduction-safety tradeoff generated by predator presence. Suppression is total: the constraint operates through neurobiological fear responses that prey cannot override without losing survival.
constraint_indexing:constraint_classification(landscape_of_fear_2026, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(landscape_of_fear_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(landscape_of_fear_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(landscape_of_fear_2026, TypeOther, context(agent_power(analytical), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(landscape_of_fear_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(landscape_of_fear_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): High but not maximum. Prey populations lose substantial resources to fear-driven behavioral suppression (reduced feeding, suppressed reproduction, metabolic stress costs). The extraction is real and substantial. However, extractiveness does not reach the 0.70+ range because fear responses do provide adaptive benefit: they genuinely reduce predation mortality when calibrated appropriately. The constraint extracts more than predator killing alone because fear suppresses behaviors even during predator absence, but the extraction is not pure rent-seeking — it serves a survival function. Suppression (0.72): High. Prey have minimal capacity to resist or modify fear responses; the constraint operates at the neurobiological level through evolved threat-detection systems that prey cannot negotiate or escape. Theater ratio (0.34): Low. Fear-induced behavioral changes are functionally active, not performative. They directly affect survival outcomes. The mechanism is not theatrical; it works. The theater is low precisely because the snare is not relying on deception or performative maintenance — it operates through real threat perception and real fitness costs.
 *
 * PERSPECTIVAL GAP:
 *   The constraint exhibits a severe perspectival gap between the primary victim (individual prey organism, which perceives pure snare) and the primary beneficiary (predator population, which perceives rope-like coordination benefit). From the prey organism's perspective, the fear landscape is a coercive extraction mechanism with no exit: the organism must allocate resources to vigilance and predator avoidance regardless of its own preferences or needs. From the predator's perspective, the fear landscape is a beneficial coordination system: prey presence generates predictable spatial and temporal distributions that reduce predator search costs. The analytical observer risks collapsing this gap by naturalizing the constraint as an inevitable feature of evolution. The mountain perspective claims that predator-induced fear is immutable law; but the structural data shows it is contingent on current predator presence, prey evolutionary history, and landscape configuration. If predators were absent or if prey evolved in predator-free environments long enough, fear responses would degrade toward piton (vestigial, theater-driven). The constraint is not a law of nature; it is a current equilibrium outcome.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality value reflects structural position in the fear extraction flow. Prey organisms are full victims: they experience fear as a coercive constraint with no exit options and no compensation. High d values derive from victim status + trapped exit → maximum f(d) → high χ. Predator populations are beneficiaries: they derive clear advantage from prey fear responses that organize prey behavior and habitat use. Low d values derive from beneficiary status + institutional/arbitrage exit → negative f(d) → negative χ. The powerless/trapped perspectives (individual prey, metabolism, reproductive fitness) all derive d ≈ 0.95 (near 1.0, full target), producing f(d) ≈ 1.42, maximum experienced extraction. The institutional/arbitrage perspective (predator population) derives d ≈ 0.05 (near 0.0, full beneficiary), producing f(d) ≈ -0.12, negative extraction (net benefit). This directionality structure confirms the snare classification: asymmetric extraction flowing from trapped victims to beneficiaries with exit options.
 *
 * MANDATROPHY ANALYSIS:
 *   The landscape of fear resolves the mandatrophy by revealing the asymmetric structure: prey cannot perceive this as coordination because they have no exit options and derive no benefit. The predator perceives coordination (rope), but that perception is not mutual — prey do not experience the same coordination benefit. This breaks the symmetry required for genuine rope: rope requires low-extraction coordination where both parties benefit. The fear landscape is extraction of fear-driven behavioral change FROM prey TO predators, disguised as coordination from the predator's perspective. The mountain perspective (analytical observer claiming natural law) is exposed as naturalization of a contingent evolutionary outcome. If prey evolved complete predator immunity or predators went extinct, the fear landscape would collapse. It is not immutable; it is a snare maintained by current evolutionary conditions and predator presence.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    predator_presence_perception_accuracy,
    'To what extent do prey fear responses calibrate to actual predation risk, versus responding to predator cues regardless of current threat level?',
    'Experimental manipulation of predator presence vs predator cues (scent, sound, visual) while monitoring prey behavioral response and actual predation rates. Correlation analysis between perceived risk and realized mortality.',
    'If prey accurately calibrate: fear landscape is adaptive coordination with predators (higher rope component). If prey respond to cues decoupled from actual risk: fear landscape is extractive even from predator absence (pure snare). Current evidence suggests intermediate: some calibration, substantial false-positive fear responses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(predator_presence_perception_accuracy, empirical, 'Whether prey fear responses match actual predation risk or overestimate threat').

omega_variable(
    fear_induced_mortality_magnitude,
    'What fraction of prey mortality in typical predator-prey systems is directly attributable to fear-induced behavioral change (starvation, reduced reproduction, increased vulnerability to other predators) versus direct predation?',
    'Long-term population studies with predator removal experiments, mortality attribution analysis, fitness component decomposition in prey populations with and without predator presence.',
    'If fear-induced mortality > direct predation: snare classification is confirmed (extraction via fear dominates extraction via killing). If fear-induced mortality < 20% of total: classification should shift toward tangled_rope (predator presence provides some adaptive benefit).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(fear_induced_mortality_magnitude, empirical, 'Magnitude of mortality from fear versus direct predation').

omega_variable(
    fear_response_evolutionary_origin,
    'Is the fear landscape constraint a product of current predator-prey arms races, or a vestigial response from ancestral predation pressures that may no longer exist?',
    'Comparative analysis of fear responses across species with different predation histories. Phylogenetic reconstruction of threat-response evolution. Test whether prey in predator-free environments (islands, isolated populations) show reduced fear responses.',
    'If current arms race: fear landscape is dynamic and responsive (snare with active extraction). If vestigial: fear landscape is piton (performative responses maintained by inertia, theater rising).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(fear_response_evolutionary_origin, empirical, 'Whether fear responses are adaptive to current threats or vestigial').

omega_variable(
    landscape_heterogeneity_escape_routes,
    'Does spatial heterogeneity in predation risk (safe refugia, variable predator density) provide genuine escape routes from the fear constraint, or do fear responses persist even in low-risk zones?',
    'Field experiments with artificial refuge provision; behavioral measurement in high-risk vs low-risk microhabitats; tracking of prey space use and foraging efficiency across risk gradients.',
    'If refugia enable escape: constraint has constrained exit option, classification shifts toward tangled_rope or scaffold. If fear persists across landscape: constraint is truly trapped (pure snare confirmed).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(landscape_heterogeneity_escape_routes, empirical, 'Whether spatial heterogeneity provides escape from fear landscape').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(landscape_of_fear_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(lof_tr_t0, landscape_of_fear_2026, theater_ratio, 0, 0.32).
narrative_ontology:measurement(lof_tr_t5, landscape_of_fear_2026, theater_ratio, 5, 0.33).
narrative_ontology:measurement(lof_tr_t10, landscape_of_fear_2026, theater_ratio, 10, 0.34).

% Extraction over time
narrative_ontology:measurement(lof_be_t0, landscape_of_fear_2026, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(lof_be_t5, landscape_of_fear_2026, base_extractiveness, 5, 0.5).
narrative_ontology:measurement(lof_be_t10, landscape_of_fear_2026, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(landscape_of_fear_2026, global_infrastructure).
narrative_ontology:affects_constraint(landscape_of_fear_2026, prey_habitat_selection).
narrative_ontology:affects_constraint(landscape_of_fear_2026, reproductive_ecology).
narrative_ontology:affects_constraint(landscape_of_fear_2026, trophic_cascade_regulation).

% DUAL FORMULATION NOTE:
% The landscape of fear can be decomposed into direct predation mortality (constraint: predation_mortality) and fear-induced behavioral changes (constraint: landscape_of_fear_2026). These are structurally distinct. The direct predation mortality operates as a culling mechanism; the fear-induced behavior operates as a behavioral suppression mechanism. Both are present in real ecosystems, and their relative magnitudes determine whether the ecosystem is better modeled as a simple predator-prey cycle (predation_mortality dominant) or as a fear-mediated landscape (landscape_of_fear_2026 dominant). Field data suggests landscape_of_fear_2026 accounts for substantially more mortality and fitness loss in many systems.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
