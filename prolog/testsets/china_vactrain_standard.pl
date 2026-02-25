% ============================================================================
% CONSTRAINT STORY: china_vactrain_standard
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_china_vactrain_standard, []).

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
 *   constraint_id: china_vactrain_standard
 *   human_readable: China's Ultra-High-Speed Vacuum-Tube Maglev Standard
 *   domain: technological/economic
 *
 * SUMMARY:
 *   China's development of an ultra-high-speed vacuum-tube maglev standard
 *   represents a state-led effort to create a new technological paradigm in
 *   transportation. The constraint is the standard itself: a set of rules,
 *   protocols, and physical specifications that enables interoperability but
 *   also concentrates power. While it offers a genuine coordination
 *   function—enabling a new mode of transport—its development is funded by
 *   the state, executed by state-owned enterprises, and designed to create a
 *   strategic advantage, leading to significant extractive properties.
 *
 * KEY AGENTS:
 *   - Chinese State Planners: Primary beneficiary (institutional/arbitrage) — sees a national coordination project to boost economy and prestige.
 *   - State-Owned Engineering Firms: Primary beneficiary (organized/mobile) — receives massive, long-term contracts and develops valuable IP.
 *   - Future Adopting Nations: Primary victim (powerless/trapped) — risks technological lock-in and long-term economic dependency.
 *   - Competing Transport Sectors: Secondary victim (organized/constrained) — faces a heavily subsidized competitor designed to make them obsolete.
 *   - Analytical Observer: Analytical agent (analytical/analytical) — recognizes the dual nature of the standard as both coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(china_vactrain_standard, 0.55).
domain_priors:suppression_score(china_vactrain_standard, 0.7).
domain_priors:theater_ratio(china_vactrain_standard, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(china_vactrain_standard, extractiveness, 0.55).
narrative_ontology:constraint_metric(china_vactrain_standard, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(china_vactrain_standard, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(china_vactrain_standard, tangled_rope).
narrative_ontology:human_readable(china_vactrain_standard, "China's Ultra-High-Speed Vacuum-Tube Maglev Standard").
narrative_ontology:topic_domain(china_vactrain_standard, "technological/economic").

domain_priors:requires_active_enforcement(china_vactrain_standard).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(china_vactrain_standard, chinese_state_planners).
narrative_ontology:constraint_beneficiary(china_vactrain_standard, state_owned_engineering_firms).
narrative_ontology:constraint_beneficiary(china_vactrain_standard, high_tech_component_suppliers).
narrative_ontology:constraint_victim(china_vactrain_standard, future_adopting_nations).
narrative_ontology:constraint_victim(china_vactrain_standard, competing_transport_sectors).
narrative_ontology:constraint_victim(china_vactrain_standard, chinese_taxpayers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: FUTURE ADOPTING NATION (SNARE) — Once committed, a nation is trapped by high switching costs, technological dependency, and reliance on the originator for maintenance and upgrades. The standard becomes a mechanism for long-term economic and political extraction. d≈0.95, f(d)≈1.42, σ=1.0 → χ≈0.78.
constraint_indexing:constraint_classification(china_vactrain_standard, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: CHINESE STATE PLANNER (ROPE) — From the perspective of the standard-setter, this is a pure coordination good. It integrates the national economy, creates technological prestige, and establishes a valuable export. The state can arbitrage its control over the standard for strategic gain. d≈0.05, f(d)≈-0.12, σ=1.1 → χ≈-0.07. Negative effective extraction signifies a net subsidy/benefit.
constraint_indexing:constraint_classification(china_vactrain_standard, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: DOMESTIC AIRLINE INDUSTRY (TANGLED ROPE) — This established industry is constrained; it cannot exit the transport market but faces a state-backed competitor designed to supplant it. It experiences the standard as both a national infrastructure project (coordination) and a direct, coercive threat to its existence (extraction). d≈0.55, f(d)≈0.75, σ=1.0 → χ≈0.41.
constraint_indexing:constraint_classification(china_vactrain_standard, tangled_rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: ANALYTICAL OBSERVER (TANGLED ROPE) — The observer sees both the genuine coordination function (a new, potentially revolutionary mode of transport) and the high potential for asymmetric extraction (geopolitical lock-in, resource concentration, suppression of alternatives). This matches the claimed_type. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.76.
constraint_indexing:constraint_classification(china_vactrain_standard, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(china_vactrain_standard_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(china_vactrain_standard, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(china_vactrain_standard, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(china_vactrain_standard, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(china_vactrain_standard_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (ε=0.55) is high because the value captured by the state and its chosen firms goes far beyond a simple coordination fee. It includes geopolitical leverage, technological dominance, and the channeling of vast national resources. Suppression (0.70) is high due to the immense capital investment required, which, combined with state backing, effectively forecloses the market to alternative standards. Theater Ratio (0.15) is currently low as the project is in a functional R&D and engineering phase, not yet a legacy system maintained for show.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For the state planners who control it, the standard is a Rope, a tool for national development. For a future nation that must adopt it to join the network, it becomes a Snare, locking them into a dependency relationship. For competing industries, it's a Tangled Rope—a nationally beneficial project that is simultaneously destroying their business model. This highlights how infrastructure standards are never neutral; they allocate power and create structural winners and losers.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (Chinese state, SOEs) have arbitrage or mobile exit options, leading to a low derived directionality (d) and a low or negative effective extraction (χ), classifying the constraint as a Rope from their view. Victims (adopting nations) are defined as trapped, leading to a high d and a high χ, resulting in a Snare classification. Other actors with constrained exit options fall in between. The analytical perspective, which accounts for all these relationships, correctly identifies the hybrid nature of the system as a Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This case avoids mandatrophy by refusing to label the standard as purely 'good' (Rope) or 'bad' (Snare). The Deferential Realism framework reveals that it is structurally both simultaneously. The classification depends on the observer's position relative to the flow of power and resources defined by the standard. An analysis that produced only one classification would be a form of mandatrophy, either by ignoring the genuine coordination function or by ignoring the coercive, extractive potential.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    economic_viability,
    'Will the vactrain system be economically self-sustaining or require permanent, massive state subsidies?',
    'Long-term operational cost and revenue data post-deployment, compared against initial capital expenditure and ongoing maintenance.',
    'If self-sustaining, it functions more like a Rope/Tangled Rope with a clear coordination benefit. If it requires permanent subsidies, the extraction from taxpayers is higher, pushing it closer to a Snare for that group.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_viability, empirical, 'Whether the system is economically viable or a permanent subsidy drain').

omega_variable(
    geopolitical_adoption_model,
    'Will the standard be exported via open protocols or through tied financing and proprietary technology, creating dependency?',
    'Analysis of the first international deployment agreements, intellectual property licensing terms, and maintenance contracts.',
    'Open protocols would lower the suppression and extraction for adopting nations (Rope-like). Tied, proprietary models would confirm the Snare classification from their perspective.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(geopolitical_adoption_model, preference, 'Whether the standard is exported as an open platform or a dependency-creating tool').

omega_variable(
    technical_feasibility_at_scale,
    'Can the immense technical challenges (e.g., maintaining vacuum over hundreds of kilometers, ensuring passenger safety) be solved reliably and cost-effectively?',
    'Performance data from full-scale, long-distance operational lines over several years.',
    'If major technical failures occur, the project''s function collapses, and it risks becoming a massive Piton—a monument to ambition maintained for theatrical reasons despite being non-functional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(technical_feasibility_at_scale, empirical, 'Whether the core technology is reliable and safe at continental scale').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(china_vactrain_standard, 2020, 2040).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(chin_tr_t0, china_vactrain_standard, theater_ratio, 0, 0.1).
narrative_ontology:measurement(chin_tr_t10, china_vactrain_standard, theater_ratio, 10, 0.12).
narrative_ontology:measurement(chin_tr_t20, china_vactrain_standard, theater_ratio, 20, 0.15).

% Extraction over time
narrative_ontology:measurement(chin_be_t0, china_vactrain_standard, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(chin_be_t10, china_vactrain_standard, base_extractiveness, 10, 0.45).
narrative_ontology:measurement(chin_be_t20, china_vactrain_standard, base_extractiveness, 20, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(china_vactrain_standard, global_infrastructure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
