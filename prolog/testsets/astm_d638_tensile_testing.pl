% ============================================================================
% CONSTRAINT STORY: astm_d638_tensile_testing
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_astm_d638_tensile_testing, []).

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
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: astm_d638_tensile_testing
 *   human_readable: ASTM D638 Tensile Property Standard for Plastics
 *   domain: technological
 *
 * SUMMARY:
 *   ASTM D638 establishes the standard method for determining the tensile
 *   properties of unreinforced and reinforced plastics. It mandates specific
 *   specimen geometries ("dogbones"), testing speeds, and environmental
 *   conditions to ensure data reproducibility and comparability across
 *   global supply chains. It functions as a core coordination mechanism for
 *   the plastics industry.
 *
 * KEY AGENTS (by structural relationship):
 *   - Lab Technicians & Small Lab Owners: Primary target (powerless/trapped) — bear the costs of compliance, training, and capital equipment.
 *   - Global Supply Chains & QA Managers: Primary beneficiary (institutional/arbitrage) — benefit from the interoperability and reduced transaction costs the standard provides.
 *   - ASTM Committee D20: Architect/Beneficiary (institutional/arbitrage) - The rule-making body that maintains the standard.
 *   - Innovative Testing Startups: Victim (organized/constrained) - Their novel methods are suppressed by the standard's incumbency.
 *   - Analytical Observer: Analytical observer — sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% Rationale: Low extraction; the primary "cost" is compliance labor and
% equipment, while the benefit is market-wide interoperability and safety.
domain_priors:base_extractiveness(astm_d638_tensile_testing, 0.10).
% Rationale: High suppression. Alternatives (e.g., ISO 527) are visible but
% suppressed by regional market dominance and the "stickiness" of specified
% testing equipment and historical data sets.
domain_priors:suppression_score(astm_d638_tensile_testing, 0.60).
% Rationale: The standard is highly functional with little performative waste.
domain_priors:theater_ratio(astm_d638_tensile_testing, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(astm_d638_tensile_testing, extractiveness, 0.10).
narrative_ontology:constraint_metric(astm_d638_tensile_testing, suppression_requirement, 0.60).
narrative_ontology:constraint_metric(astm_d638_tensile_testing, theater_ratio, 0.05).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(astm_d638_tensile_testing, rope).

% --- Binary flags ---
% Requires active enforcement (via audits, lab certification) to be valid in trade.
domain_priors:requires_active_enforcement(astm_d638_tensile_testing).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(astm_d638_tensile_testing, global_supply_chains).
narrative_ontology:constraint_beneficiary(astm_d638_tensile_testing, testing_equipment_manufacturers).
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(astm_d638_tensile_testing, small_testing_labs).
narrative_ontology:constraint_victim(astm_d638_tensile_testing, innovative_non_standard_testing_startups).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Do not add measurement_basis, beneficiary/victim, or other metadata.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE LAB TECHNICIAN / SMALL LAB OWNER (SNARE)
% Agent who bears the costs of compliance and capital equipment.
% Engine derives d from: victim membership + trapped exit -> d ≈ 0.95 -> high χ.
% The high suppression score and enforcement make this a Snare, despite low ε.
% It feels like a Mountain (immutable rules), but is structurally a Snare
% because it's a human-made system that traps participants via certification.
constraint_indexing:constraint_classification(astm_d638_tensile_testing, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE ASTM COMMITTEE / QA MANAGER (ROPE)
% Agent who benefits from the coordination function. Engine derives d from:
%   beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → low/negative χ
constraint_indexing:constraint_classification(astm_d638_tensile_testing, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE MATERIAL PROCUREMENT MANAGER (ROPE)
% An agent with mobility who can choose between standards (e.g., ASTM vs ISO).
% The constraint is a useful but non-coercive coordination tool.
constraint_indexing:constraint_classification(astm_d638_tensile_testing, rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (ROPE)
% Default analytical context. Recognizes the primary function is coordination,
% classifying it as a Rope despite the snare-like properties for trapped agents.
constraint_indexing:constraint_classification(astm_d638_tensile_testing, rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(astm_d638_tensile_testing_tests).

test(perspectival_gap) :-
    % Verify perspectival gap between target (Snare) and beneficiary (Rope).
    constraint_indexing:constraint_classification(astm_d638_tensile_testing, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(astm_d638_tensile_testing, rope, context(agent_power(institutional), _, _, _)).

test(low_extractiveness_check) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(astm_d638_tensile_testing, ExtMetricName, E),
    E < 0.2.

:- end_tests(astm_d638_tensile_testing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.10) is low because industrial standards are
 *   quintessential coordination mechanisms that create more value than they
 *   extract. The high suppression score (0.60) reflects the significant
 *   incumbency advantage and switching costs that lock out alternatives like
 *   ISO 527 in certain markets (e.g., US aerospace). The theater ratio is
 *   very low (0.05) as the standard is almost entirely functional.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark: for institutional beneficiaries (QA managers, ASTM
 *   committee), it's a pure Rope—a tool for coordination. For the powerless
 *   and trapped agent (lab technician, small lab owner), the high capital
 *   costs, mandatory certification, and high suppression of alternatives
 *   make it a Snare. They cannot easily exit the system without losing their
 *   livelihood, and they bear the direct costs of compliance.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `global_supply_chains` and `testing_equipment_manufacturers`
 *     directly profit from the interoperability and guaranteed market for
 *     compliant machines.
 *   - Victims: `small_testing_labs` face high capital barriers, while
 *     `innovative_non-standard_testing_startups` are structurally suppressed
 *     by the standard's dominance. This maps the costs of the system.
 *
 * MANDATROPHY ANALYSIS:
 *   This case demonstrates how a low-extraction Rope (ε=0.10) can still
 *   function as a Snare for a specific index (powerless, trapped). The
 *   classification is not determined by ε alone, but by the effective
 *   extraction χ, which is amplified for the victim by their high
 *   directionality (d≈0.95) and the constraint's high suppression score.
 *   This correctly identifies the coercive aspect without mislabeling the
 *   entire coordination system as purely extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% Omega variables — open questions the framework cannot yet resolve
%
% /5 form: narrative detail for story context
omega_variable(
    omega_astm_d638,
    'Will ASTM D638 and ISO 527 ever fully merge into a single global standard?',
    'Monitor joint committee activities, ballot results, and adoption rates in key industries (automotive, aerospace).',
    'If Yes: The constraint would become a global Rope with even higher suppression, potentially approaching a Piton if innovation stagnates. If No: The current Rope/Snare dynamic persists, with continued regional arbitrage.',
    confidence_without_resolution(medium)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_astm_d638, empirical, 'The potential for convergence between ASTM D638 and ISO 527 standards.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(astm_d638_tensile_testing, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% No temporal data required. Base extractiveness (0.10) is below the
% threshold (0.46) for mandatory lifecycle drift monitoring.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% An industrial standard is a classic example of an information standard.
narrative_ontology:coordination_type(astm_d638_tensile_testing, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed. The structural derivation from beneficiary/victim
% declarations and exit options accurately models the directionality for
% all key agents in this scenario.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */