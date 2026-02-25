% ============================================================================
% CONSTRAINT STORY: bedouin_sedentary_transition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_bedouin_sedentary_transition, []).

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
 *   constraint_id: bedouin_sedentary_transition
 *   human_readable: The Bedouin Sedentary Transition
 *   domain: social/economic/technological
 *
 * SUMMARY:
 *   This constraint models the socio-economic transformation of Bedouin
 *   tribes in the Arabian Peninsula, primarily driven by the discovery of oil
 *   in 1938 and subsequent state-building policies. The transition involves a
 *   shift from a millennium-old, resilient nomadic pastoralism to a settled
 *   existence dependent on state-provided welfare, housing, and employment.
 *   This process, framed as 'modernization,' fundamentally restructures
 *   Bedouin society, economy, and political autonomy, trading
 *   self-sufficiency for the benefits and dependencies of the modern
 *   nation-state.
 *
 * KEY AGENTS:
 *   - Nation-State Builders: Primary beneficiary (institutional/arbitrage) — Gains territorial control, a governable populace, and national unity.
 *   - Traditional Nomadic Culture: Primary victim (powerless/trapped) — An entire way of life based on mobility and pastoralism is rendered unviable.
 *   - Sedentarized Bedouin Generations: Hybrid agents (moderate/constrained) — Receive material benefits like education and healthcare but lose cultural autonomy and traditional skills.
 *   - Global Energy Markets: Indirect beneficiary (institutional/arbitrage) — Benefits from the political stability that a settled population provides in key resource extraction zones.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(bedouin_sedentary_transition, 0.68).
domain_priors:suppression_score(bedouin_sedentary_transition, 0.75).
domain_priors:theater_ratio(bedouin_sedentary_transition, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(bedouin_sedentary_transition, extractiveness, 0.68).
narrative_ontology:constraint_metric(bedouin_sedentary_transition, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(bedouin_sedentary_transition, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(bedouin_sedentary_transition, tangled_rope).
narrative_ontology:human_readable(bedouin_sedentary_transition, "The Bedouin Sedentary Transition").
narrative_ontology:topic_domain(bedouin_sedentary_transition, "social/economic/technological").

domain_priors:requires_active_enforcement(bedouin_sedentary_transition).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(bedouin_sedentary_transition, nation_state_builders).
narrative_ontology:constraint_beneficiary(bedouin_sedentary_transition, global_energy_markets).
narrative_ontology:constraint_victim(bedouin_sedentary_transition, traditional_nomadic_culture).
narrative_ontology:constraint_victim(bedouin_sedentary_transition, pastoral_economic_autonomy).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRADITIONAL NOMAD (SNARE) — Experiences the transition as a trap. Traditional migration routes are cut by national borders, grazing lands are enclosed, and the pastoral economy is made unviable. Dependency on state welfare replaces self-sufficiency, effectively trapping them in a new economic model with no viable exit. d≈0.95, f(d)≈1.42, σ=0.9 → χ≈0.87.
constraint_indexing:constraint_classification(bedouin_sedentary_transition, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: NATION-STATE BUILDER (ROPE) — Views the transition as a necessary coordination mechanism for modernization and national unity. Sedentarization integrates a mobile population into the state's infrastructure (taxation, conscription, education), secures borders, and allows for centralized resource management. From this view, it's a benevolent project of social development. d≈0.05, f(d)≈-0.12, σ=1.0 → χ≈-0.08.
constraint_indexing:constraint_classification(bedouin_sedentary_transition, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: SEDENTARIZED YOUTH (TANGLED ROPE) — Experiences both the benefits (education, healthcare, modern amenities) and the costs (loss of cultural heritage, disconnection from traditional skills). They are constrained, unable to return to a lifestyle they never knew, but benefit from the new system. The constraint is a hybrid of genuine coordination and asymmetric extraction of cultural identity. d≈0.65, f(d)≈1.00, σ=0.8 → χ≈0.54.
constraint_indexing:constraint_classification(bedouin_sedentary_transition, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (TANGLED ROPE) — The system's claimed type. It recognizes the genuine coordination function (providing modern services) but also the high extraction of autonomy and the high suppression of the nomadic alternative. The state's provision of welfare is structurally coupled with the elimination of a competing mode of social and economic organization. This is the canonical Tangled Rope signature. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.94.
constraint_indexing:constraint_classification(bedouin_sedentary_transition, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bedouin_sedentary_transition_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(bedouin_sedentary_transition, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bedouin_sedentary_transition, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(bedouin_sedentary_transition, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(bedouin_sedentary_transition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.68) is high, representing the fundamental transfer of autonomy, economic independence, and political sovereignty from the tribe to the state. While the state provides material goods, it extracts the very basis of the former way of life. Suppression (0.75) is also high; maintaining a nomadic lifestyle is actively undermined by fixed national borders, land privatization, the decline of pastoral markets, and powerful economic incentives to settle. Theater Ratio (0.40) reflects the state's narrative of benevolent modernization and progress, which masks the underlying function of social control and state consolidation.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. The state sees a Rope: a successful project of national integration and development. The traditional Bedouin elder sees a Snare: the destruction of their world and a gilded cage of dependency. The younger, settled Bedouin sees a Tangled Rope: they live with the daily reality of both the system's benefits (a job, a house) and its costs (a lost heritage, a sense of alienation). This perspectival divergence is the core of the constraint's structure.
 *
 * DIRECTIONALITY LOGIC:
 *   The Nation-State is a clear beneficiary with arbitrage (it sets the rules), leading to a low 'd' value and a Rope classification. The traditional nomadic culture is the ultimate victim, trapped with no exit, leading to a high 'd' value and a Snare classification. The Sedentarized Youth are victims of cultural loss but beneficiaries of material provision, and their constrained exit options place them in the middle, experiencing the system as a Tangled Rope. The analytical view aligns with this hybrid classification, recognizing both the coordination and extraction functions.
 *
 * MANDATROPHY ANALYSIS:
 *   This case resolves a potential mandatrophy by demonstrating that a single process can be both a genuine provision of public goods (Rope) and a coercive elimination of alternatives (Snare). The framework avoids a binary 'good' or 'bad' judgment, instead classifying the structure from multiple valid viewpoints. The state isn't lying when it claims to be providing services, and the nomad isn't exaggerating when they claim their world was destroyed. Both are structurally correct perspectives on a Tangled Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    agency_versus_coercion,
    'What was the true ratio of voluntary adoption of a ''better life'' (pull factors) versus the structural elimination of the nomadic alternative (push factors)?',
    'Archival analysis of state policies from 1950-1980, combined with oral histories from elders who lived through the initial transition period to quantify perceived choice.',
    'If primarily voluntary, the constraint''s base extractiveness (ε) would be lower, shifting classifications toward Rope/Scaffold. If primarily coercive, the Snare classification becomes dominant.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(agency_versus_coercion, empirical, 'Ratio of voluntary choice vs structural coercion in sedentarization').

omega_variable(
    cultural_resilience,
    'Is the loss of the nomadic lifestyle leading to complete cultural assimilation, or is a new, resilient, hybrid Bedouin identity forming within settled communities?',
    'Longitudinal ethnographic studies tracking the evolution of language, oral poetry, social customs, and kinship structures in sedentarized communities over multiple generations.',
    'Evidence of a resilient, adaptive culture would lower the perceived extraction, as cultural capital is not being completely destroyed but transformed. This would support a Tangled Rope over a Snare classification from more perspectives.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(cultural_resilience, empirical, 'Degree of cultural assimilation vs formation of a new hybrid identity').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(bedouin_sedentary_transition, 1938, 2030).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(bedo_tr_t0, bedouin_sedentary_transition, theater_ratio, 0, 0.05).
narrative_ontology:measurement(bedo_tr_t32, bedouin_sedentary_transition, theater_ratio, 32, 0.25).
narrative_ontology:measurement(bedo_tr_t92, bedouin_sedentary_transition, theater_ratio, 92, 0.4).

% Extraction over time
narrative_ontology:measurement(bedo_be_t0, bedouin_sedentary_transition, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(bedo_be_t32, bedouin_sedentary_transition, base_extractiveness, 32, 0.45).
narrative_ontology:measurement(bedo_be_t92, bedouin_sedentary_transition, base_extractiveness, 92, 0.68).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(bedouin_sedentary_transition, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
