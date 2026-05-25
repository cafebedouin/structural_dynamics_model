% ============================================================================
% CONSTRAINT STORY: first_mover_network_effects
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_first_mover_network_effects, []).

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
 *   constraint_id: first_mover_network_effects
 *   human_readable: First Mover Network Effects Lock-In
 *   domain: economic/technology/market_structure
 *
 * SUMMARY:
 *   First mover network effects create a constraint that exhibits the full
 *   six-type range depending on the observer's structural position. When a
 *   platform or technology achieves critical mass, positive feedback (more
 *   users increase value for all users, creating incentive for additional
 *   users to join) creates self-reinforcing dominance. This coordination
 *   mechanism is genuine — standardization, ecosystem development, and scale
 *   economies produce real value. However, this same mechanism creates
 *   lock-in that suppresses competition and can trap latecomers in an
 *   inferior position even when their product quality is superior. The
 *   constraint operates at the boundary between coordination and extraction:
 *   beneficiaries experience coordination value; victims experience
 *   suppression and trapped alternatives. The extractiveness trajectory
 *   (0.35→0.58) reflects increasing rent extraction as network position
 *   strengthens and switching costs accumulate. Theater ratio (0.25→0.35)
 *   indicates that the coordination function is real and functional (low
 *   theater), not performative — the contrast with verification_bottleneck's
 *   theater ratio (0.72) is diagnostically important. Network effects work;
 *   they just also extract.
 *
 * KEY AGENTS:
 *   - First Mover Incumbent: Primary beneficiary (institutional/arbitrage) — captures network value and switching cost premium; exits available through platform evolution or adjacent markets
 *   - Early Adopter Coalition: Secondary beneficiary (organized/mobile) — locked in but benefits from ecosystem maturity; can arbitrage between platform vendors if ecosystem fragments
 *   - Excluded Latecomer: Primary victim (powerless/trapped) — cannot overcome critical mass threshold or switching cost barrier; structurally trapped in secondary market segments
 *   - Secondary Entrant: Secondary victim (moderate/constrained) — can achieve niche market share but blocked from main market dominance; constrained by coordination barriers but mobile within segments
 *   - Open Standards Coalition: Organized agents (organized/mobile) — regulators, standards bodies, interoperability advocates building alternative pathways with policy sunset mechanisms (interoperability mandates, data portability rules)
 *   - Technological Disruption Frame: Institutional narrative (institutional/arbitrage) — market ideology that assumes creative destruction and disruptive innovation reset lock-in, persists as theater despite evidence that network effects transfer to new platforms
 *   - Analytical Observer: Civilizational view (analytical/analytical) — risks naturalizing network effects as immutable laws rather than policy-contingent outcomes
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(first_mover_network_effects, 0.58).
domain_priors:suppression_score(first_mover_network_effects, 0.48).
domain_priors:theater_ratio(first_mover_network_effects, 0.35).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(first_mover_network_effects, extractiveness, 0.58).
narrative_ontology:constraint_metric(first_mover_network_effects, suppression_requirement, 0.48).
narrative_ontology:constraint_metric(first_mover_network_effects, theater_ratio, 0.35).

% --- Constraint claim ---
narrative_ontology:constraint_claim(first_mover_network_effects, tangled_rope).
narrative_ontology:human_readable(first_mover_network_effects, "First Mover Network Effects Lock-In").
narrative_ontology:topic_domain(first_mover_network_effects, "economic/technology/market_structure").

domain_priors:requires_active_enforcement(first_mover_network_effects).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(first_mover_network_effects, first_mover_incumbent).
narrative_ontology:constraint_beneficiary(first_mover_network_effects, early_adopter_coalition).
narrative_ontology:constraint_victim(first_mover_network_effects, latecomer_competitors).
narrative_ontology:constraint_victim(first_mover_network_effects, consumer_welfare).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: EXCLUDED LATECOMER (SNARE) — New entrants face insurmountable coordination barriers: switching costs, established ecosystem lock-in, critical mass thresholds, and winner-take-most dynamics. No viable exit path. Cannot catch up through superior product alone; must overcome installed base inertia. Maximum extraction and suppression experienced by firms entering after critical mass achieved.
constraint_indexing:constraint_classification(first_mover_network_effects, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: SECONDARY ENTRANT (TANGLED ROPE) — Can achieve market share in adjacent segments or through niche differentiation, but main market access is constrained. Genuine coordination benefits exist (ecosystem interoperability, standardization) but asymmetrically distributed. Can exit through acquisition, niche dominance, or geographic expansion, but at significant cost. Mixed coordination and extraction.
constraint_indexing:constraint_classification(first_mover_network_effects, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: FIRST MOVER INCUMBENT (ROPE) — Experiences the constraint as pure coordination: network effects create genuine value through standardization, ecosystem development, and scale economies. Can arbitrage between markets or technologies if network position erodes. Extraction flows toward this agent, but they perceive the mechanism as solving legitimate coordination problems. Zero coercion experienced by the beneficiary.
constraint_indexing:constraint_classification(first_mover_network_effects, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 4: OPEN STANDARDS COALITION (SCAFFOLD) — Organized agents (regulators, standards bodies, interoperability advocacy) recognize network lock-in as a temporary coordination failure with a sunset: mandatory interoperability requirements, open API standards, and interoperable data portability protocols create alternative pathways. High agency and clear exit mechanism. Scaffold classification reflects the sunset logic built into antitrust remedies and open standards mandates.
constraint_indexing:constraint_classification(first_mover_network_effects, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: TECHNOLOGICAL DISRUPTION FRAME (PITON) — Market narrative claims that disruption and creative destruction solve network lock-in: new technologies (cloud, mobile, blockchain) allegedly create reset moments where incumbents lose advantage. This frame persists theatrically despite evidence that network effects transfer to new platforms (iOS/Android duopoly, cloud platform consolidation, blockchain exchange dominance). Theater ratio reflects the gap between disruption-as-narrative and disruption-as-rare-outcome. The institutional expectation of renewal maintains the frame despite degraded functional performance.
constraint_indexing:constraint_classification(first_mover_network_effects, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, network effects are presented as immutable features of technology economics: positive feedback, lock-in, and winner-take-most outcomes are depicted as laws of nature rather than contingent institutional arrangements. This perspective risks naturalizing what is actually a policy-dependent outcome — the strength of network effects depends on interoperability mandates, portability requirements, data ownership rules, and switching cost regulation. The engine's false summit detector should identify this as a naturalization of contingent institutional design.
constraint_indexing:constraint_classification(first_mover_network_effects, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(first_mover_network_effects_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(first_mover_network_effects, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(first_mover_network_effects, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(first_mover_network_effects, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(first_mover_network_effects, TR),
    TR >= 0.70.

:- end_tests(first_mover_network_effects_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The first mover captures genuine network value (coordination benefit), but also rent extraction through switching cost lock-in and installer base advantage. The trajectory from 0.35→0.58 reflects that early-stage network effects are coordination-heavy (genuine value creation) but mature network effects become extraction-heavy (latecomers face lock-in). At t=10, 58% represents real extraction not justified by quality or efficiency differences. Suppression (0.48): Moderate. Barriers to entry and exit are substantial (switching costs, ecosystem requirements, critical mass thresholds) but not absolute. Latecomers can enter adjacent segments, and regulatory intervention can lower switching costs. The barrier is binding but not insurmountable. Theater ratio (0.25→0.35): Low, remaining low. The coordination function is genuine and functional — standardization and ecosystem value are not performative. Theater creeps upward as the narrative of disruption and disruption becomes more prominent relative to actual evidence of platform reset, but the constraint itself is low-theater because the coordination actually works.
 *
 * PERSPECTIVAL GAP:
 *   Six distinct classifications from the same constraint structure: (1) Snare from the trapped latecomer's view — pure extraction, no coordination benefit perceived because they're locked out. (2) Tangled Rope from the secondary entrant's moderate view — genuine ecosystem benefits (coordination) mixed with extraction barriers and asymmetric position. (3) Rope from the first mover's institutional view — coordination mechanism solving genuine collective action problem; extraction experienced as fair beneficiary reward. (4) Scaffold from the organized coalition's generational view — sunset mechanism real and structural (interoperability mandates, data portability, regulation can lower switching costs), so high agency to escape. (5) Piton from the disruption narrative frame — technological reset supposedly solves network lock-in (creative destruction), but the frame persists theatrically despite evidence that network effects transfer (iOS/Android duopoly, cloud platform consolidation, blockchain exchange dominance). (6) Mountain from the analytical view at civilizational scope — network effects presented as immutable laws of technology, but the structural data reveals contingency on policy design (interoperability rules, switching cost regulation, data ownership architecture).
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality (d) varies by agent. First mover incumbent: d≈0.10 (net beneficiary, arbitrage options, low f(d), experiences low/negative chi). Early adopter coalition: d≈0.30 (mixed beneficiary, mobile options, moderate f(d)). Secondary entrant: d≈0.70 (victim status, constrained exit, high f(d), high chi). Excluded latecomer: d≈0.95 (full target, trapped status, maximum f(d), maximum chi). The directionality spread (0.10 to 0.95) is large because exit options and structural relationship are highly differentiated. Beneficiaries with arbitrage have low d; victims with trapping have high d. Suppression (0.48) is unscaled — it applies uniformly to all agents regardless of their directionality. Extractiveness (0.58) scales by f(d): the incumbent experiences χ ≈ 0.58 × f(0.10) × 1.0 ≈ 0.25 (low effective extraction, high benefit). The latecomer experiences χ ≈ 0.58 × f(0.95) × 1.2 ≈ 0.99 (high effective extraction, maximum vulnerability).
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint demonstrates mandatrophy resolution through perspectival completeness. The constraint is NOT exclusively Rope (pure coordination) — that would misclassify the extraction latecomers experience. It is NOT exclusively Snare (pure extraction) — that would misclassify the genuine ecosystem coordination benefits. Tangled Rope classification correctly captures both mechanisms simultaneously: genuine coordination function (ecosystem, standardization, value creation) AND asymmetric extraction (lock-in, switching costs, installed base advantage). The mandatrophy is resolved by showing that the constraint embodies both Rope dynamics (solving coordination problems) and Snare dynamics (creating barriers to entry). The full perspectival range (Mountain→Piton→Scaffold→Rope→Tangled Rope→Snare) shows that the constraint is structurally complex: a coordination mechanism that works AND an extraction mechanism that locks in winners. The false summit (Mountain) is rejected by the structural data showing policy-contingency: network effects are not immutable laws but policy-dependent outcomes (interoperability mandates can weaken them, data portability requirements can lower switching costs, regulatory intervention can reset timing). The analytics shows that mandatrophy is resolved not by choosing one type but by recognizing the constraint's multi-faceted structure across positions.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    network_effect_strength_measurement,
    'What observable data distinguishes genuine network effects from switching cost artificial lock-in?',
    'Empirical separation: measure value increase from ecosystem size vs. switching cost barriers independently. Conduct hypothetical portability experiments or policy interventions mandating interoperability.',
    'If network effects dominate switching costs: coordination mechanism is genuine (Rope from beneficiary perspective). If switching costs dominate: extraction mechanism is extractive (Snare from latecomer perspective). Classifies as Tangled Rope only if both are substantial and genuine.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(network_effect_strength_measurement, empirical, 'Separating genuine network effects from artificial switching costs').

omega_variable(
    interoperability_feasibility,
    'How technically feasible is forced interoperability or data portability for this platform architecture without destroying the coordination function?',
    'Engineering analysis of API design requirements; case studies of interoperability retrofits (Windows domain interoperability, payment system clearing houses); cost estimates for achieving portability at scale.',
    'If technically feasible: scaffold sunset is real and mandates can succeed (Scaffold classification confirmed). If infeasible: remedies become theater, and lock-in persists (Piton classification for intervention frame).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interoperability_feasibility, empirical, 'Technical feasibility of forced interoperability without destroying function').

omega_variable(
    platform_lifecycle_disruption_frequency,
    'How often do dominant networks actually get displaced by technological disruption, and how often do network effects transfer to the new platform?',
    'Historical analysis: Mainframe→Personal Computer→Mobile→Cloud. Did network effects reset? (No: concentration increased with each transition.) Analysis of blockchain/Web3 claims vs. actual exchange consolidation dynamics.',
    'If disruption genuinely resets lock-in: technological disruption frame is predictive (theater ratio < 0.40). If network effects transfer consistently: frame is narrative rather than structural (theater ratio > 0.60, Piton classification confirmed).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(platform_lifecycle_disruption_frequency, empirical, 'Whether technological disruption resets or transfers network effects').

omega_variable(
    extraction_flow_directionality_ambiguity,
    'Does the first mover extract rent through superior service, or through installed base lock-in preventing exit even when competitor quality is higher?',
    'Comparative analysis: latecomer entrant quality metrics vs. market share; survey data on switching cost perception vs. feature preference; natural experiments from interoperability mandates (EU Digital Markets Act, etc.).',
    'If first mover wins on quality: extraction is coordinate value distribution (Rope). If latecomer wins on quality but lacks market share: extraction is rent-seeking (Snare). Tangled Rope classification requires both genuine coordination benefit AND evidence of extraction despite latecomer superiority.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(extraction_flow_directionality_ambiguity, empirical, 'Whether extraction is from value creation or lock-in premium').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(first_mover_network_effects, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(fmne_tr_t0, first_mover_network_effects, theater_ratio, 0, 0.25).
narrative_ontology:measurement(fmne_tr_t5, first_mover_network_effects, theater_ratio, 5, 0.3).
narrative_ontology:measurement(fmne_tr_t10, first_mover_network_effects, theater_ratio, 10, 0.35).

% Extraction over time
narrative_ontology:measurement(fmne_be_t0, first_mover_network_effects, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(fmne_be_t5, first_mover_network_effects, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(fmne_be_t10, first_mover_network_effects, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(first_mover_network_effects, resource_allocation).
narrative_ontology:affects_constraint(first_mover_network_effects, platform_switching_costs).
narrative_ontology:affects_constraint(first_mover_network_effects, ecosystem_lock_in).
narrative_ontology:affects_constraint(first_mover_network_effects, interoperability_bottleneck).

% DUAL FORMULATION NOTE:
% First mover network effects decompose into three structurally distinct constraints: (1) resource_allocation coordination (genuine network value from scale and standardization), (2) switching_cost extraction (cost barriers to exit), (3) ecosystem lock-in (critical mass threshold preventing entry). Each has different ε, different beneficiaries/victims, different sunset mechanisms. This story models the aggregate constraint; related stories model the constituent mechanisms separately.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(first_mover_network_effects, institutional, 0.15).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
