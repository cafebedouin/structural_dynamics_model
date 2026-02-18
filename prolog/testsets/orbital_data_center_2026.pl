% ============================================================================
% CONSTRAINT STORY: orbital_data_center_2026
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_orbital_data_center_2026, []).

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
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: orbital_data_center_2026
 *   human_readable: SpaceX Million-Satellite Orbital Compute Network
 *   domain: technological/geopolitical
 *
 * SUMMARY:
 *   A proposed network of one million satellites functioning as a single,
 *   orbital data center. By leveraging Starship's launch capacity and direct
 *   solar power, the system provides a global compute utility independent of
 *   terrestrial power grids and national jurisdictions. While offering a genuine
 *   coordination function (global compute access), its centralized control
 *   and potential for monopolistic pricing create a highly extractive dynamic.
 *
 * KEY AGENTS (by structural relationship):
 *   - terrestrial_competitors_and_excluded_nations: Primary target (powerless/trapped) — bears extraction through market suppression and lack of access.
 *   - spacex_and_large_compute_customers: Primary beneficiary (institutional/arbitrage) — benefits from a novel, powerful compute utility.
 *   - Analytical Observer: Sees the dual nature of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(orbital_data_center_2026, 0.82).
domain_priors:suppression_score(orbital_data_center_2026, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(orbital_data_center_2026, 0.45).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(orbital_data_center_2026, extractiveness, 0.82).
narrative_ontology:constraint_metric(orbital_data_center_2026, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(orbital_data_center_2026, theater_ratio, 0.45).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(orbital_data_center_2026, tangled_rope).
narrative_ontology:human_readable(orbital_data_center_2026, "SpaceX Million-Satellite Orbital Compute Network").
narrative_ontology:topic_domain(orbital_data_center_2026, "technological/geopolitical").

% --- Binary flags ---
domain_priors:requires_active_enforcement(orbital_data_center_2026). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(orbital_data_center_2026, spacex_and_large_compute_customers).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(orbital_data_center_2026, terrestrial_competitors_and_excluded_nations).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE EXCLUDED / COMPETITOR (SNARE)
% Terrestrial data centers and nations without access are suppressed by the
% new monopoly. From their perspective, it is pure extraction.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
constraint_indexing:constraint_classification(orbital_data_center_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% For SpaceX and its primary customers (e.g., AI firms, allied governments),
% the system is a powerful coordination tool, providing compute resources
% independent of terrestrial constraints.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(orbital_data_center_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The observer sees both the genuine coordination function and the immense
% extractive potential of a privately-owned global utility, classifying it
% as a Tangled Rope.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
constraint_indexing:constraint_classification(orbital_data_center_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(orbital_data_center_2026_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target and beneficiary.
    constraint_indexing:constraint_classification(orbital_data_center_2026, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(orbital_data_center_2026, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget \= TypeBeneficiary,
    TypeTarget == snare,
    TypeBeneficiary == rope.

test(analytical_classification) :-
    constraint_indexing:constraint_classification(orbital_data_center_2026, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_gates) :-
    narrative_ontology:constraint_beneficiary(orbital_data_center_2026, _),
    narrative_ontology:constraint_victim(orbital_data_center_2026, _),
    domain_priors:requires_active_enforcement(orbital_data_center_2026).

:- end_tests(orbital_data_center_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (0.82): Extremely high, reflecting the potential for
 *     monopolistic pricing and control over a fundamental utility (computation).
 *     The owner can extract enormous rents from a captive global market.
 *   - Suppression Score (0.75): High. A functional million-satellite network
 *     would create insurmountable barriers to entry for competitors, effectively
 *     suppressing terrestrial alternatives and future orbital competitors.
 *   - Theater Ratio (0.45): Moderate. While there is a significant "future of
 *     humanity" narrative, the system is primarily functional, not performative.
 *
 * PERSPECTIVAL GAP:
 *   - The gap is stark. For beneficiaries (SpaceX, large customers), it's a
 *     paradigm-shifting Rope that solves major logistical problems (power,
 *     cooling, data sovereignty). For victims (competitors, excluded nations),
 *     it's a Snare that destroys their market viability and leaves them
 *     dependent on a single private entity.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `spacex_and_large_compute_customers`. They gain access to a
 *     unique, powerful resource, giving them a competitive advantage. Their
 *     `arbitrage` exit option reflects their power to negotiate terms or even
 *     develop alternative systems.
 *   - Victim: `terrestrial_competitors_and_excluded_nations`. They are the
 *     targets of extraction. Competitors are priced out or rendered obsolete.
 *     Excluded nations face a new form of digital divide. Their `trapped`
 *     exit option reflects their inability to compete or build a parallel system.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 *   The high extraction score (0.82) risks misclassifying this as a pure Snare.
 *   However, the system provides a genuine, non-trivial coordination function:
 *   a globally accessible, high-performance computing utility. This is a real
 *   service, not just a rent-seeking mechanism. The Tangled Rope classification
 *   correctly captures this duality, acknowledging both the valuable service
 *   (the Rope aspect) and the coercive, extractive monopoly it creates (the
 *   Snare aspect). Without recognizing the coordination function, one would
 *   miss why powerful actors would willingly adopt the system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_orbital_data_center_2026,
    'Will the network be governed as a neutral public utility or a private coercive monopoly?',
    'Analysis of FCC licensing conditions, international treaties on orbital assets, and observed pricing structures after deployment.',
    'If governed as a utility, extraction would decrease and it would shift towards a Rope. If governed as a private monopoly, it remains a Tangled Rope or degrades into a pure Snare.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(orbital_data_center_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Required for high-extraction constraints (base_extractiveness > 0.46).
% Models the intensification of the constraint over its initial deployment phase.
%
% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(odc_tr_t0, orbital_data_center_2026, theater_ratio, 0, 0.10).
narrative_ontology:measurement(odc_tr_t5, orbital_data_center_2026, theater_ratio, 5, 0.25).
narrative_ontology:measurement(odc_tr_t10, orbital_data_center_2026, theater_ratio, 10, 0.45).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(odc_ex_t0, orbital_data_center_2026, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(odc_ex_t5, orbital_data_center_2026, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(odc_ex_t10, orbital_data_center_2026, base_extractiveness, 10, 0.82).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(orbital_data_center_2026, global_infrastructure).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */