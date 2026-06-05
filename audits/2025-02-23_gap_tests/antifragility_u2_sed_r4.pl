% ============================================================================
% CONSTRAINT STORY: antifragility_u2_sed_r4
% ============================================================================
% Version: 4.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-16
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_antifragility_u2_sed_r4, []).

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
 *   constraint_id: antifragility_u2_sed_r4
 *   human_readable: Antifragility (Gaining from Disorder)
 *   domain: technological/economic/biological
 *
 * SUMMARY:
 *   Antifragility describes the property of systems that gain from disorder,
 *   volatility, and stressors. This constraint story analyzes the
 *   socio-economic application of this concept, where the upside from
 *   volatility is often captured by a few agents, while the downside is
 *   externalized and absorbed by a much larger, more fragile population. This
 *   creates a significant perspectival gap between those who can harness it
 *   and those who are broken by it.
 *
 * KEY AGENTS:
 *   - Optimized Serfs (Victim): Agents in fragile, optimized roles who bear the costs of volatility (powerless/trapped).
 *   - Antifragile Practitioners (Beneficiary): Agents with resources and strategies to profit from volatility (moderate/arbitrage).
 *   - Institutional Fragilistas (Enforcer/Victim): Bureaucrats who enforce stability, inadvertently creating larger systemic fragilities (institutional/constrained).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(antifragility_u2_sed_r4, 0.75).
domain_priors:suppression_score(antifragility_u2_sed_r4, 0.65).
domain_priors:theater_ratio(antifragility_u2_sed_r4, 0.55).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(antifragility_u2_sed_r4, extractiveness, 0.75).
narrative_ontology:constraint_metric(antifragility_u2_sed_r4, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(antifragility_u2_sed_r4, theater_ratio, 0.55).

% --- Constraint claim ---
narrative_ontology:constraint_claim(antifragility_u2_sed_r4, tangled_rope).
narrative_ontology:human_readable(antifragility_u2_sed_r4, "Antifragility (Gaining from Disorder)").
narrative_ontology:topic_domain(antifragility_u2_sed_r4, "technological/economic/biological").

domain_priors:requires_active_enforcement(antifragility_u2_sed_r4).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(antifragility_u2_sed_r4, antifragile_practitioners).
narrative_ontology:constraint_victim(antifragility_u2_sed_r4, fragile_institutions).
narrative_ontology:constraint_victim(antifragility_u2_sed_r4, optimized_serfs).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For the agent whose job or livelihood is optimized for stability, volatility is a catastrophic threat from which there is no escape. The system extracts their resilience.
constraint_indexing:constraint_classification(antifragility_u2_sed_r4, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For the agent with resources and knowledge to adopt a 'barbell' strategy (e.g., 90% safe assets, 10% high-risk), volatility is a resource to be harvested. It's a coordination tool for generating alpha.
constraint_indexing:constraint_classification(antifragility_u2_sed_r4, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(arbitrage),
            spatial_scope(global))).

% Institutions tasked with maintaining stability see both the coordination function (preventing collapse) and the extractive cost (bailouts, moral hazard), which benefits a select few at the expense of the whole.
constraint_indexing:constraint_classification(antifragility_u2_sed_r4, tangled_rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(constrained),
            spatial_scope(national))).

% From a long-term, evolutionary perspective, the process of gaining from disorder is a fundamental property of all complex adaptive systems. It is an unchangeable law of nature.
constraint_indexing:constraint_classification(antifragility_u2_sed_r4, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(antifragility_u2_sed_r4_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(antifragility_u2_sed_r4, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(antifragility_u2_sed_r4, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(antifragility_u2_sed_r4, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(antifragility_u2_sed_r4_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.75) represents the asymmetric payoff structure ('convexity') where beneficiaries capture unbounded upside from positive 'black swan' events, while the costs of negative events are socialized or borne by the victims. Suppression (0.65) reflects the structural barriers—lack of capital, information asymmetry, regulatory capture—that prevent victims from adopting antifragile strategies.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. For the practitioner, it's a Rope for navigating reality. For the victim, it's a Snare that punishes them for the stability the system demands. For the institutional actor, it's a Tangled Rope of managing short-term stability at the cost of long-term systemic health. For the analyst, it's a Mountain, an inescapable feature of evolution.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the structural flow of risk. Beneficiaries (practitioners) are those who can successfully transfer risk away from themselves. Victims (serfs, fragile institutions) are the recipients of that transferred risk. The system is actively enforced by institutional actors who prioritize the illusion of stability, which facilitates this risk transfer.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by classifying the constraint as a Tangled Rope from the analytical perspective. This correctly identifies that the system has a genuine coordination function (maintaining short-term societal stability) but that this function is inextricably linked to a highly extractive process. A pure Snare classification would miss the coordination element that gives the system its legitimacy, while a Rope classification would ignore the vast population of victims.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    is_extraction_predatory_or_functional,
    'Is the extraction from fragile agents a necessary byproduct of a functional evolutionary process, or is it an engineered, predatory feature of a specific socio-economic system?',
    'Comparative analysis of systems with high vs. low 'skin-in-the-game' for decision-makers. If systems with insulated decision-makers show higher extraction, it points towards a predatory design.',
    'If purely functional, the constraint is a Mountain. If predatory, it is a Snare. The current Tangled Rope classification reflects the ambiguity.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(is_extraction_predatory_or_functional, empirical, 'Distinguishing between necessary evolutionary cost and engineered predatory extraction.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(antifragility_u2_sed_r4, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(antifragility_tr_t0, antifragility_u2_sed_r4, theater_ratio, 0, 0.15).
narrative_ontology:measurement(antifragility_tr_t5, antifragility_u2_sed_r4, theater_ratio, 5, 0.35).
narrative_ontology:measurement(antifragility_tr_t10, antifragility_u2_sed_r4, theater_ratio, 10, 0.55).

% Extraction over time
narrative_ontology:measurement(antifragility_be_t0, antifragility_u2_sed_r4, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(antifragility_be_t5, antifragility_u2_sed_r4, base_extractiveness, 5, 0.6).
narrative_ontology:measurement(antifragility_be_t10, antifragility_u2_sed_r4, base_extractiveness, 10, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(antifragility_u2_sed_r4, resource_allocation).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
