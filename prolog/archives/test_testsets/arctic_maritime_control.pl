% ============================================================================
% CONSTRAINT STORY: arctic_maritime_control
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_arctic_maritime_control, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: arctic_maritime_control
 *   human_readable: Arctic Maritime Control Regime
 *   domain: geopolitical
 *
 * SUMMARY:
 *   The Arctic Maritime Control Regime is the complex web of international
 *   law (UNCLOS), multilateral agreements (Polar Code), and unilateral
 *   national claims that govern the increasingly accessible Arctic Ocean. As
 *   climate change melts sea ice, new shipping routes like the Northern Sea
 *   Route and Northwest Passage are opening, alongside access to vast oil,
 *   gas, and mineral reserves. This has transformed the region into a zone of
 *   intense geopolitical and economic competition. The regime is not a single
 *   treaty but an emergent system of control characterized by overlapping
 *   jurisdictions and active enforcement by Arctic states.
 *
 * KEY AGENTS:
 *   - Arctic Littoral States (Russia, Canada, USA, etc.): Primary beneficiaries (institutional/arbitrage) — enforce territorial claims, control sea lanes, and license resource extraction.
 *   - Non-Arctic States (China, EU nations): Constrained actors (organized/constrained) — seek access for shipping and resources but are limited by the existing power structure.
 *   - Indigenous Arctic Communities: Primary victims (powerless/trapped) — face environmental and cultural disruption from increased industrial activity in their homelands.
 *   - Multinational Resource Corporations: Secondary beneficiaries (powerful/mobile) — leverage the legal regime to secure extraction rights.
 *   - Global Climate Stability: Abstract victim (powerless/trapped) — negatively impacted by the fossil fuel extraction and increased shipping the regime enables.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(arctic_maritime_control, 0.55).
domain_priors:suppression_score(arctic_maritime_control, 0.75).
domain_priors:theater_ratio(arctic_maritime_control, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(arctic_maritime_control, extractiveness, 0.55).
narrative_ontology:constraint_metric(arctic_maritime_control, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(arctic_maritime_control, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(arctic_maritime_control, tangled_rope).
narrative_ontology:human_readable(arctic_maritime_control, "Arctic Maritime Control Regime").
narrative_ontology:topic_domain(arctic_maritime_control, "geopolitical").

domain_priors:requires_active_enforcement(arctic_maritime_control).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(arctic_maritime_control, arctic_littoral_states).
narrative_ontology:constraint_beneficiary(arctic_maritime_control, multinational_resource_corporations).
narrative_ontology:constraint_beneficiary(arctic_maritime_control, global_shipping_companies).
narrative_ontology:constraint_victim(arctic_maritime_control, non_arctic_states).
narrative_ontology:constraint_victim(arctic_maritime_control, indigenous_arctic_communities).
narrative_ontology:constraint_victim(arctic_maritime_control, global_climate_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIGENOUS COMMUNITIES (SNARE) — Trapped within the sovereign territory of Arctic states, their traditional livelihoods and environments are disrupted by shipping and extraction with minimal consultation or benefit. They bear the costs of environmental degradation without the power to exit or meaningfully resist. d≈0.95, f(d)≈1.42, σ=0.9 → χ≈0.72.
constraint_indexing:constraint_classification(arctic_maritime_control, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: GLOBAL CLIMATE (SNARE) — An abstract victim. The regime enables fossil fuel extraction and carbon-intensive shipping, directly undermining global climate stability. This collective good has no agency and cannot exit the consequences. d≈0.95, f(d)≈1.42, σ=1.2 → χ≈0.94.
constraint_indexing:constraint_classification(arctic_maritime_control, snare,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 3: NON-ARCTIC STATES (TANGLED ROPE) — These powerful actors are constrained by the legal claims and military presence of Arctic states. They recognize the coordination function (e.g., Polar Code for safety) but are subject to extraction through transit fees and exclusion from resource claims. d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.50.
constraint_indexing:constraint_classification(arctic_maritime_control, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: ARCTIC LITTORAL STATE (ROPE) — From their perspective, the regime is a pure coordination mechanism for managing sovereign territory, ensuring safe navigation (Polar Code), and exploiting national resources under international law (UNCLOS). They are the primary beneficiaries. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.07. Negative effective extraction indicates a net subsidy.
constraint_indexing:constraint_classification(arctic_maritime_control, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (TANGLED ROPE) — This view recognizes both the essential coordination function (preventing conflict, setting safety standards) and the high degree of asymmetric extraction (concentrated resource rights, strategic control of sea lanes). The high suppression and extraction values clearly indicate a hybrid system, not a pure Rope. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.76.
constraint_indexing:constraint_classification(arctic_maritime_control, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(arctic_maritime_control_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(arctic_maritime_control, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(arctic_maritime_control, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(arctic_maritime_control, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(arctic_maritime_control_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.55): High. The regime facilitates the concentration of immense wealth (trillions in resources) and strategic advantage (control of shipping lanes) in the hands of a few states and corporations. Suppression (0.75): Very High. Alternatives to compliance are minimal. Access is controlled by the littoral states' coast guards and navies, backed by contested but forceful legal claims. Non-Arctic states cannot easily challenge this control without risking major diplomatic or military incidents. Theater Ratio (0.30): Low. While diplomatic forums like the Arctic Council exist, their influence has waned. The primary reality of the regime is functional: the collection of transit fees, the enforcement of the Polar Code, and military patrols are all concrete, non-performative actions.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. An Arctic state like Russia or Canada perceives the regime as a legitimate exercise of sovereignty and a necessary coordination tool (Rope) for managing its territory. A non-Arctic state like China sees a system that unfairly restricts its access to a global commons, blending necessary safety rules with extractive tolls and exclusions (Tangled Rope). Indigenous communities experience the same regime as a mechanism of dispossession and environmental destruction imposed from outside (Snare).
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries are the Arctic states and the corporations they license; their institutional power and ability to set the rules gives them a low directionality (d), resulting in a Rope classification from their viewpoint. The victims are non-Arctic states, indigenous communities, and the global environment. Indigenous communities are trapped with no exit, yielding the highest d and a Snare classification. Non-Arctic states are constrained but have agency, placing them in the middle with a Tangled Rope classification. This distribution of outcomes from a single set of base properties is characteristic of a highly contested and asymmetric system.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a classic case for resolving mandatrophy. Labeling it a 'Rope' (as Arctic states do) would be a severe misclassification, ignoring the high suppression and extraction imposed on other actors. Conversely, labeling it a pure 'Snare' would miss the genuine and necessary coordination functions it provides (e.g., maritime safety via the Polar Code, deconfliction). The analytical classification of Tangled Rope correctly identifies the hybrid nature of the system: it is a mechanism of both coordination and coercive, asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_status_of_passages,
    'What is the definitive legal status of the Northwest Passage and Northern Sea Route: internal waters or international straits?',
    'A binding ruling from the International Court of Justice or a new multilateral treaty accepted by all major powers.',
    'If ruled international straits, suppression would decrease significantly, shifting the classification toward Rope for more actors. If confirmed as internal waters, the Snare/Tangled Rope classifications are solidified.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_status_of_passages, conceptual, 'Legal status of Arctic sea routes (internal vs. international waters)').

omega_variable(
    climate_acceleration_impact,
    'Will the rate of ice melt accelerate beyond projections, triggering a chaotic ''gold rush'' that overwhelms the existing control regime?',
    'Long-term climate monitoring and analysis of state behavior in response to newly accessible areas.',
    'A chaotic rush would likely dissolve the coordination function, turning the regime into a pure Snare based on military dominance, or leading to its total collapse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(climate_acceleration_impact, empirical, 'Impact of accelerated ice melt on regime stability').

omega_variable(
    arctic_council_effectiveness,
    'Can the Arctic Council be revived as an effective multilateral governance body, or will it remain sidelined by great power competition?',
    'Observation of diplomatic outcomes, joint scientific and safety projects, and de-escalation of military posturing over a 5-10 year period.',
    'An effective Council would strengthen the coordination function, lowering effective extraction and making the regime more Rope-like. A defunct Council reinforces unilateral enforcement, increasing suppression and solidifying its Snare-like characteristics.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(arctic_council_effectiveness, empirical, 'Future effectiveness of the Arctic Council as a coordinating body').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(arctic_maritime_control, 2010, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arct_tr_t2010, arctic_maritime_control, theater_ratio, 2010, 0.2).
narrative_ontology:measurement(arct_tr_t2018, arctic_maritime_control, theater_ratio, 2018, 0.25).
narrative_ontology:measurement(arct_tr_t2025, arctic_maritime_control, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(arct_be_t2010, arctic_maritime_control, base_extractiveness, 2010, 0.4).
narrative_ontology:measurement(arct_be_t2018, arctic_maritime_control, base_extractiveness, 2018, 0.48).
narrative_ontology:measurement(arct_be_t2025, arctic_maritime_control, base_extractiveness, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(arctic_maritime_control, global_infrastructure).
narrative_ontology:affects_constraint(arctic_maritime_control, global_shipping_logistics).
narrative_ontology:affects_constraint(arctic_maritime_control, fossil_fuel_markets).
narrative_ontology:affects_constraint(arctic_maritime_control, unclos_legal_framework).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
