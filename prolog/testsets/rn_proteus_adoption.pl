% ============================================================================
% CONSTRAINT STORY: rn_proteus_adoption
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_rn_proteus_adoption, []).

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
    constraint_indexing:directionality_override/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: rn_proteus_adoption
 *   human_readable: Royal Navy's adoption of the Leonardo Proteus uncrewed helicopter
 *   domain: technological
 *
 * SUMMARY:
 *   The UK Royal Navy has initiated a two-year experimental contract to integrate
 *   the Leonardo Proteus, an uncrewed helicopter, into its fleet starting in
 *   2026. This technology will take on roles like resupply and surveillance,
 *   currently performed by manned aircraft. The constraint is the institutional
 *   and technological path dependency created by this pilot program, which shifts
 *   risk, cost, and roles within the naval aviation structure.
 *
 * KEY AGENTS (by structural relationship):
 *   - Manned Helicopter Crews: Primary target (organized/constrained) — face role displacement and potential career path obsolescence.
 *   - Leonardo (Manufacturer): Primary beneficiary (institutional/arbitrage) — secures a key contract and market position.
 *   - Royal Navy Command: Secondary beneficiary (institutional/constrained) — gains new capabilities and potential long-term cost savings.
 *   - Analytical Observer: Sees the full structure, including the temporary/experimental nature of the constraint.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(rn_proteus_adoption, 0.20).
domain_priors:suppression_score(rn_proteus_adoption, 0.35).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(rn_proteus_adoption, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(rn_proteus_adoption, extractiveness, 0.20).
narrative_ontology:constraint_metric(rn_proteus_adoption, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(rn_proteus_adoption, theater_ratio, 0.15).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(rn_proteus_adoption, scaffold).

% --- Binary flags ---
narrative_ontology:has_sunset_clause(rn_proteus_adoption).      % Mandatory if Scaffold. The 2-year trial is a de facto sunset clause.
domain_priors:requires_active_enforcement(rn_proteus_adoption). % Military procurement and doctrine changes require enforcement.

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(rn_proteus_adoption, defense_contractors_leonardo).
narrative_ontology:constraint_beneficiary(rn_proteus_adoption, royal_navy_command).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(rn_proteus_adoption, manned_helicopter_crews).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   ========================================================================== */

% PERSPECTIVE 1: THE MANNED HELICOPTER CREWS (TARGET)
% They experience a system coordinating their potential replacement. While the
% threat is existential, the base extraction (ε=0.20) of this initial pilot
% program is too low to classify as a Tangled Rope, even from their powerless
% perspective. It registers as a coercive Rope.
constraint_indexing:constraint_classification(rn_proteus_adoption, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2A: LEONARDO (MANUFACTURER BENEFICIARY)
% As the primary commercial beneficiary with high exit (can sell to other navies),
% Leonardo perceives this as a pure coordination mechanism that subsidizes them.
% d is low, f(d) is negative, leading to a Rope classification.
constraint_indexing:constraint_classification(rn_proteus_adoption, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 2B: ROYAL NAVY COMMAND (INSTITUTIONAL BENEFICIARY)
% As the adopting institution, the RN also sees a coordination tool. Their exit
% is 'constrained' by the significant investment, leading to a slightly higher
% directionality than Leonardo, but still firmly classifying as Rope.
constraint_indexing:constraint_classification(rn_proteus_adoption, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SCAFFOLD)
% The analytical view sees a temporary support structure. The low effective
% extraction (χ ≈ 0.20 * 1.15 * 1.2 = 0.276 <= 0.30) combined with the de facto
% sunset clause (the 2-year trial) meets the criteria for a Scaffold.
constraint_indexing:constraint_classification(rn_proteus_adoption, scaffold,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(rn_proteus_adoption_tests).

test(perspectival_gap_scaffold_vs_rope, [nondet]) :-
    % Verify the key gap: participants see Rope, analysis sees Scaffold.
    constraint_indexing:constraint_classification(rn_proteus_adoption, rope,
        context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(rn_proteus_adoption, scaffold,
        context(agent_power(analytical), _, _, _)).

test(target_is_not_snare_due_to_low_epsilon, [nondet]) :-
    % Verify that even for the target, the classification is not Snare.
    constraint_indexing:constraint_classification(rn_proteus_adoption, TypeTarget,
        context(agent_power(powerless), _, exit_options(trapped), _)),
    TypeTarget \= snare.

test(scaffold_conditions_met) :-
    % Verify the structural data for Scaffold classification is present.
    narrative_ontology:has_sunset_clause(rn_proteus_adoption),
    narrative_ontology:constraint_beneficiary(rn_proteus_adoption, _).

:- end_tests(rn_proteus_adoption_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.20) is set low to reflect the initial,
 *   experimental nature of this two-year adoption. While the long-term
 *   implication is the automation of human roles (a highly extractive process),
 *   the constraint being modeled *here* is the pilot program itself, which has
 *   limited immediate displacement. The suppression score (0.35) reflects the
 *   incipient path dependency. The key structural feature is the two-year
 *   trial period, which functions as a `has_sunset_clause`, making this a
 *   canonical Scaffold from the analytical perspective.
 *
 * PERSPECTIVAL GAP:
 *   The gap is between the participants (who see a Rope) and the analytical
 *   observer (who sees a Scaffold). The beneficiaries (Leonardo, RN) see a
 *   straightforward coordination tool. The targets (crews) also see a Rope,
 *   but a coercive one coordinating their potential obsolescence. Their
 *   perspective doesn't classify as Tangled Rope because the current base
 *   extraction is too low. The analytical view correctly identifies the
 *   constraint's temporary and conditional nature, classifying it as a Scaffold
 *   designed to test a future, more permanent system.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: Leonardo and Royal Navy Command directly benefit from the
 *     new capabilities and contracts. Their beneficiary status + exit options
 *     (arbitrage for Leonardo, constrained for RN) derive low directionality (d)
 *     values, resulting in low/negative effective extraction (χ) and a Rope view.
 *   - Victim: Manned helicopter crews are structurally targeted for displacement.
 *     Their victim status + trapped exit option derives a high d value (≈0.95),
 *     amplifying the base extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly navigates the ambiguity of technological pilot
 *   programs. A critic might prematurely label this a Snare ("automation killing
 *   jobs"), while proponents would frame it as a pure Rope ("capability upgrade").
 *   The Scaffold classification is more precise: it acknowledges the coordination
 *   function and the potential for extraction, but correctly identifies its core
 *   identity as a *temporary, conditional structure with a built-in review
 *   mechanism*. It is a support structure for a potential future constraint. If
 *   the program becomes permanent and extraction rises, it will lifecycle-drift
 *   into a Tangled Rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_rn_proteus_adoption,
    'Is the two-year trial a genuine "scaffold" with a real possibility of termination, or is it political theater for a decision that is already a foregone conclusion?',
    'Observing the RN''s procurement decisions and resource allocation at the end of the two-year contract in 2028.',
    'If genuine, the Scaffold classification holds. If theater, the true base extractiveness (ε) is much higher, and this is already a nascent Tangled Rope disguised as a Scaffold.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(rn_proteus_adoption, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This models the potential lifecycle drift if the Scaffold is successful and
% becomes a permanent, more extractive system. The initial state is low-
% extraction exploration, which evolves into a higher-extraction Tangled Rope
% as automation displaces more roles.
%
% Theater ratio over time (remains low and functional):
narrative_ontology:measurement(rn_proteus_adoption_tr_t0, rn_proteus_adoption, theater_ratio, 0, 0.10).
narrative_ontology:measurement(rn_proteus_adoption_tr_t5, rn_proteus_adoption, theater_ratio, 5, 0.15).
narrative_ontology:measurement(rn_proteus_adoption_tr_t10, rn_proteus_adoption, theater_ratio, 10, 0.15).

% Extraction over time (models drift from Scaffold to Tangled Rope):
narrative_ontology:measurement(rn_proteus_adoption_ex_t0, rn_proteus_adoption, base_extractiveness, 0, 0.10).
narrative_ontology:measurement(rn_proteus_adoption_ex_t5, rn_proteus_adoption, base_extractiveness, 5, 0.20).
narrative_ontology:measurement(rn_proteus_adoption_ex_t10, rn_proteus_adoption, base_extractiveness, 10, 0.45).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This system coordinates the movement of physical goods and sensor data.
narrative_ontology:coordination_type(rn_proteus_adoption, resource_allocation).

% Network relationships (structural influence edges)
% This adoption directly influences the doctrine and operational viability of
% existing manned helicopter fleets.
narrative_ontology:affects_constraint(rn_proteus_adoption, rn_manned_helicopter_doctrine).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The automatic derivation
% based on the declared beneficiary/victim groups and their associated exit
% options accurately models the structural relationships.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */