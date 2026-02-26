% ============================================================================
% CONSTRAINT STORY: ancestral_pueblo_hydrology
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_ancestral_pueblo_hydrology, []).

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
 *   constraint_id: ancestral_pueblo_hydrology
 *   human_readable: Ancestral Puebloan Hydrological Debt
 *   domain: environmental/social
 *
 * SUMMARY:
 *   This constraint models the socio-environmental system of the Ancestral
 *   Puebloans in the Four Corners region, particularly during the period
 *   leading to the abandonment of major centers like Chaco Canyon and Mesa
 *   Verde around 1300 AD. The 'hydrological debt' represents the accumulated
 *   environmental degradation (deforestation, soil erosion, water table
 *   depletion) and social fragility resulting from a successful but
 *   ultimately unsustainable adaptation to an arid environment. A period of
 *   population growth and agricultural intensification, supported by
 *   sophisticated water management, created a system that was highly
 *   vulnerable to the severe, prolonged 'Great Drought' of the late 13th
 *   century.
 *
 * KEY AGENTS:
 *   - Puebloan Farmers: Primary victims (powerless/trapped) — dependent on the agricultural system and directly exposed to its failure.
 *   - Puebloan Elites: Primary beneficiaries (institutional/arbitrage) — directed the water management projects and controlled the agricultural surplus that supported their status.
 *   - Descendant Communities: Inheritors of the legacy (organized/constrained) — carry forward cultural knowledge but are also shaped by the historical trauma of the collapse.
 *   - Analytical Observer: Modern archaeologist or climatologist (analytical/analytical) — able to reconstruct the long-term dynamics of the climate and social system.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(ancestral_pueblo_hydrology, 0.65).
domain_priors:suppression_score(ancestral_pueblo_hydrology, 0.7).
domain_priors:theater_ratio(ancestral_pueblo_hydrology, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ancestral_pueblo_hydrology, extractiveness, 0.65).
narrative_ontology:constraint_metric(ancestral_pueblo_hydrology, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ancestral_pueblo_hydrology, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(ancestral_pueblo_hydrology, tangled_rope).
narrative_ontology:human_readable(ancestral_pueblo_hydrology, "Ancestral Puebloan Hydrological Debt").
narrative_ontology:topic_domain(ancestral_pueblo_hydrology, "environmental/social").

domain_priors:requires_active_enforcement(ancestral_pueblo_hydrology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(ancestral_pueblo_hydrology, puebloan_elites).
narrative_ontology:constraint_victim(ancestral_pueblo_hydrology, puebloan_farmers).
narrative_ontology:constraint_victim(ancestral_pueblo_hydrology, future_generations).
narrative_ontology:constraint_victim(ancestral_pueblo_hydrology, regional_hydrology).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE FARMER (SNARE) — Trapped by sunk costs in an agricultural system dependent on increasingly unreliable water. Bears the full cost of crop failure and environmental degradation, ultimately forced into migration. The system extracts their livelihood and security. d≈0.95, f(d)≈1.42, σ=0.9 → χ≈0.83.
constraint_indexing:constraint_classification(ancestral_pueblo_hydrology, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: THE ELITE (ROPE) — Experiences the system as pure coordination. They direct labor to build and maintain water infrastructure, manage food surplus, and lead rituals, all for the perceived collective good. The social hierarchy and resource control are seen as necessary functions of a complex society. d≈0.05, f(d)≈-0.12, σ=0.9 → χ≈-0.07.
constraint_indexing:constraint_classification(ancestral_pueblo_hydrology, rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 3: DESCENDANT COMMUNITIES (TANGLED ROPE) — Inherit both the cultural knowledge of water management (a coordination benefit) and the legacy of collapse and displacement (an extracted cost). They are constrained by this history but organized through cultural continuity. The story of their ancestors is both a guide and a burden.
constraint_indexing:constraint_classification(ancestral_pueblo_hydrology, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 4: THE FATALIST VIEW (MOUNTAIN) — This perspective naturalizes the collapse, attributing it solely to an insurmountable external shock (the Great Drought). It frames the outcome as an inevitable clash with natural law, ignoring the social and environmental debt that created the underlying vulnerability. The engine will flag this as a false summit, as the base properties (ε=0.65) are inconsistent with a Mountain.
constraint_indexing:constraint_classification(ancestral_pueblo_hydrology, mountain,
    context(agent_power(powerless),
            time_horizon(civilizational),
            exit_options(trapped),
            spatial_scope(universal))).

% PERSPECTIVE 5: THE ANALYTICAL OBSERVER (TANGLED ROPE) — Sees the full system dynamics. The water management infrastructure was a genuine coordination solution, but it enabled population growth that exceeded the region's long-term carrying capacity, creating a highly extractive system vulnerable to climatic shocks. The classification captures both the coordination function and the asymmetric extraction. d≈0.73, f(d)≈1.15, σ=0.9 → χ≈0.67.
constraint_indexing:constraint_classification(ancestral_pueblo_hydrology, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ancestral_pueblo_hydrology_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ancestral_pueblo_hydrology, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ancestral_pueblo_hydrology, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(ancestral_pueblo_hydrology, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(ancestral_pueblo_hydrology_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.65): High. Represents the rate of resource consumption (water, timber, soil fertility) exceeding the natural rate of replenishment, leading to a systemic deficit or 'debt'. Suppression (0.70): High. The commitment to maize agriculture and settled life in large communities created high sunk costs, suppressing alternatives like nomadic foraging which could not support the established population density. Theater Ratio (0.30): Low. The water management systems and associated social rituals were highly functional for centuries. The ratio rises slightly over time as functional solutions may have begun to fail, increasing reliance on purely supplicatory rituals.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the farmer, the system becomes a Snare as the climate shifts and the land fails, trapping them in a losing battle. For the elite, it remains a Rope—a complex coordination problem they are tasked with solving. The analytical observer sees the synthesis: a Tangled Rope, where the very mechanisms of coordination (waterworks, food storage) are what enable the unsustainable extraction and create the asymmetric risk profile that defines the system's fragility.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (puebloan_elites) with arbitrage exit options (ability to relocate with retained status/resources) experience the system as coordination, leading to a low 'd' value and a Rope classification. Victims (puebloan_farmers) who are trapped by their agricultural investments bear the full cost of failure, leading to a high 'd' value and a Snare classification. The analytical view balances these, recognizing both functions, resulting in the Tangled Rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a powerful example of how a system can be simultaneously a brilliant coordination solution and a devastatingly extractive one. Mandatrophy is avoided by refusing to collapse the analysis into a single judgment. The system wasn't simply 'bad' (a Snare) or 'good' (a Rope). It was a complex adaptation (Tangled Rope) whose internal logic was rational from the perspective of its managers (Rope) but catastrophic for its most vulnerable participants and its environmental foundation (Snare). The DR framework captures this essential duality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    social_vs_climate_driver,
    'Was the collapse primarily driven by the severity of the 13th-century Great Drought (external shock) or by internal social dynamics (elite mismanagement, conflict, overpopulation)?',
    'Higher-resolution paleoclimatic data correlated with archaeological evidence of social stratification, resource hoarding, and violence just prior to abandonment.',
    'A stronger climate driver pushes the classification towards Mountain (an unavoidable natural limit). Stronger social drivers confirm the Tangled Rope/Snare classification (a failure of a human system).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(social_vs_climate_driver, empirical, 'Primary driver of collapse: external climate shock vs. internal social failure.').

omega_variable(
    elite_intent_and_knowledge,
    'Were the elites knowingly operating an unsustainable system for their own benefit, or were they good-faith coordinators overwhelmed by unprecedented environmental change?',
    'Analysis of food storage patterns (communal vs. elite-controlled), settlement patterns, and evidence of responses to prior, smaller droughts.',
    'Evidence of good-faith coordination strengthens the Rope perspective. Evidence of resource hoarding and control points towards a more extractive Snare.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(elite_intent_and_knowledge, conceptual, 'Degree to which elites understood and managed systemic risk vs. extracted benefits.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ancestral_pueblo_hydrology, 900, 1300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(ance_tr_t900, ancestral_pueblo_hydrology, theater_ratio, 900, 0.15).
narrative_ontology:measurement(ance_tr_t1100, ancestral_pueblo_hydrology, theater_ratio, 1100, 0.25).
narrative_ontology:measurement(ance_tr_t1300, ancestral_pueblo_hydrology, theater_ratio, 1300, 0.3).

% Extraction over time
narrative_ontology:measurement(ance_be_t900, ancestral_pueblo_hydrology, base_extractiveness, 900, 0.2).
narrative_ontology:measurement(ance_be_t1100, ancestral_pueblo_hydrology, base_extractiveness, 1100, 0.5).
narrative_ontology:measurement(ance_be_t1300, ancestral_pueblo_hydrology, base_extractiveness, 1300, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(ancestral_pueblo_hydrology, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
