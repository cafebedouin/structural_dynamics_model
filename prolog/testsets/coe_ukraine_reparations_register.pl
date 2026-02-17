% ============================================================================
% CONSTRAINT STORY: coe_ukraine_reparations_register
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-17
% ============================================================================

:- module(constraint_coe_ukraine_reparations_register, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: coe_ukraine_reparations_register
 *   human_readable: Council of Europe's Register of Damage for Ukraine
 *   domain: geopolitical/legal
 *
 * SUMMARY:
 *   The Council of Europe (CoE), with support from the US, Canada, and Japan,
 *   has established an international "Register of Damage" to record and legally
 *   document claims of loss and injury from Russia's invasion of Ukraine. This
 *   register is the first component of a broader international compensation
 *   mechanism designed to make Russia financially liable for the damages,
 *   potentially using frozen Russian state assets for reparations. The constraint
 *   is this legal and financial mechanism of accountability and extraction.
 *
 * KEY AGENTS (by structural relationship):
 *   - Russian Federation: Primary target (institutional/trapped) — The entity from which reparations are to be extracted.
 *   - Ukrainian state and citizens: Primary beneficiary (organized/constrained for state, powerless/trapped for citizens) — Receives compensation and legal validation of damages.
 *   - Council of Europe members: Institutional architects (institutional/arbitrage) — Design and operate the mechanism, reinforcing international law.
 *   - Analytical Observer: Analytical agent — Observes the dual function of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coe_ukraine_reparations_register, 0.65).
domain_priors:suppression_score(coe_ukraine_reparations_register, 0.80).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(coe_ukraine_reparations_register, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coe_ukraine_reparations_register, extractiveness, 0.65).
narrative_ontology:constraint_metric(coe_ukraine_reparations_register, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(coe_ukraine_reparations_register, theater_ratio, 0.15).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(coe_ukraine_reparations_register, tangled_rope).
narrative_ontology:human_readable(coe_ukraine_reparations_register, "Council of Europe's Register of Damage for Ukraine").

% --- Binary flags ---
domain_priors:requires_active_enforcement(coe_ukraine_reparations_register). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(coe_ukraine_reparations_register, ukrainian_state_and_citizens).
narrative_ontology:constraint_beneficiary(coe_ukraine_reparations_register, council_of_europe_members).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(coe_ukraine_reparations_register, russian_federation).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three)

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

% PERSPECTIVE 1: THE PRIMARY TARGET (RUSSIAN FEDERATION)
% As the target of extraction with no ability to opt-out, Russia perceives
% a purely coercive mechanism. The engine derives d from its victim status and
% trapped exit options, resulting in d ≈ 0.95 and high effective extraction (χ).
constraint_indexing:constraint_classification(coe_ukraine_reparations_register, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE INSTITUTIONAL ARCHITECTS (COUNCIL OF EUROPE)
% As the designers and beneficiaries (upholding international law), the CoE
% perceives a coordination mechanism. The engine derives d from their beneficiary
% status and arbitrage exit, resulting in d ≈ 0.05 and negative effective extraction.
constraint_indexing:constraint_classification(coe_ukraine_reparations_register, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The observer sees both the vital coordination function (registering claims)
% and the massive, coercive extraction function (reparations). High ε and high
% suppression, combined with both beneficiary and victim groups, results
% in a Tangled Rope classification.
constraint_indexing:constraint_classification(coe_ukraine_reparations_register, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: THE DIRECT BENEFICIARY (UKRAINIAN STATE)
% For Ukraine, the mechanism's primary function is coordination and validation
% of claims. While the ultimate goal is extraction from Russia, Ukraine's direct
% interaction is with a system that brings order to chaos. It is a powerful rope
% that enables future recovery.
constraint_indexing:constraint_classification(coe_ukraine_reparations_register, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE INDIVIDUAL CLAIMANT (UKRAINIAN CITIZEN)
% A powerless individual who has suffered loss. For them, the register is a
% pure coordination mechanism (a Rope) to formalize their claim for future
% compensation. They are trapped by the circumstances of the war, making this
% register their only viable path to restitution.
constraint_indexing:constraint_classification(coe_ukraine_reparations_register, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coe_ukraine_reparations_register_tests).

test(perspectival_gap_target_architect, [nondet]) :-
    % Verify the gap between the Russian (target) and CoE (architect) views.
    constraint_indexing:constraint_classification(coe_ukraine_reparations_register, snare, C1),
    C1 = context(agent_power(institutional), _, exit_options(trapped), _),
    constraint_indexing:constraint_classification(coe_ukraine_reparations_register, rope, C2),
    C2 = context(agent_power(institutional), _, exit_options(arbitrage), _),
    write('Perspectival gap validated: Target (Snare) vs. Architect (Rope).').

test(analytical_claim_matches_type, [nondet]) :-
    % The analytical view should align with the declared constraint_claim.
    narrative_ontology:constraint_claim(coe_ukraine_reparations_register, ClaimType),
    constraint_indexing:constraint_classification(coe_ukraine_reparations_register, ClaimType, context(agent_power(analytical),_,_,_)).

test(tangled_rope_structural_gates_pass) :-
    % A Tangled Rope must have beneficiary, victim, and active enforcement.
    narrative_ontology:constraint_beneficiary(coe_ukraine_reparations_register, _),
    narrative_ontology:constraint_victim(coe_ukraine_reparations_register, _),
    domain_priors:requires_active_enforcement(coe_ukraine_reparations_register).

:- end_tests(coe_ukraine_reparations_register_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.65): Set high to reflect the goal of extracting hundreds of billions of dollars in reparations. The mechanism's primary purpose is large-scale value transfer.
 *   - Suppression (0.80): Set high because the mechanism is non-consensual for its target (Russia) and relies on coercive power (e.g., asset seizure) to function, suppressing any alternative where Russia does not pay.
 *   - The analytical classification is Tangled Rope because the constraint possesses two inseparable functions: a genuine, large-scale coordination function (registering and validating millions of disparate claims) and a clear, asymmetric extraction function (forcing reparations from one party to another).
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For Russia (the target), it is a pure Snare: a coercive, inescapable trap designed solely for extraction. For the Council of Europe (the architects), it is a Rope: a tool to coordinate international legal principles, enforce accountability, and create order. For Ukraine (the direct beneficiary, both state and citizens), it is also a Rope, providing the structure needed to pursue justice and reconstruction. This difference is driven by their structural relationship to the constraint, which the directionality engine captures through beneficiary/victim declarations and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: 'ukrainian_state_and_citizens' and 'council_of_europe_members'. The former receives the material compensation, the latter reinforces the international legal order it presides over.
 *   - Victim: 'russian_federation'. The entity from which value is explicitly extracted.
 *   - The system derives directionality (d) from these declarations. Russia's victim status + `trapped` exit gives d≈1.0 (full target). The CoE's beneficiary status + `arbitrage` exit gives d≈0.0 (full beneficiary), creating the maximum perspectival difference.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This is a prime example of an inter-institutional constraint. Both Russia and the CoE are institutional actors, but they have fundamentally different relationships to the mechanism. The model captures this not by creating a single, confused "institutional" view, but by defining two distinct institutional perspectives differentiated by their `exit_options` (`trapped` vs. `arbitrage`). This precisely models the power asymmetry at the heart of the constraint.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this as a simple Snare would be incorrect, as it would ignore the massive and essential coordination function of the claims register. Without this coordination, individual claims would be legally void and practically impossible to pursue. Classifying it as a Rope would be equally wrong, ignoring the coercive, high-extraction nature of the reparations. The Tangled Rope classification correctly identifies that the coordination function is the very vehicle for the extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_coe_reparations,
    'Is the reparations register a functionally enforceable legal tool or a symbolic political statement with limited material impact?',
    'Observation of actual seizure of frozen Russian state assets and transfer of funds to Ukraine within the next 5-10 years.',
    'If enforceable, it remains a Tangled Rope. If enforcement fails and it becomes primarily symbolic, it will degrade into a Piton, with high theater_ratio and low functional extraction.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(coe_ukraine_reparations_register, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This is a high-extraction constraint (ε > 0.46), so temporal data is required.
% As the constraint is new, we model its creation and potential intensification.
% T=0 represents the moment of creation. T=10 represents a decade of operation.

% Theater ratio over time (starts low, may rise slightly if bureaucracy grows)
narrative_ontology:measurement(coe_rr_tr_t0, coe_ukraine_reparations_register, theater_ratio, 0, 0.10).
narrative_ontology:measurement(coe_rr_tr_t5, coe_ukraine_reparations_register, theater_ratio, 5, 0.12).
narrative_ontology:measurement(coe_rr_tr_t10, coe_ukraine_reparations_register, theater_ratio, 10, 0.15).

% Extraction over time (starts high and intensifies as legal tools are sharpened)
narrative_ontology:measurement(coe_rr_ex_t0, coe_ukraine_reparations_register, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(coe_rr_ex_t5, coe_ukraine_reparations_register, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(coe_rr_ex_t10, coe_ukraine_reparations_register, base_extractiveness, 10, 0.65).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: It serves as both an information standard and an
% enforcement mechanism. Enforcement is the terminal goal.
narrative_ontology:coordination_type(coe_ukraine_reparations_register, enforcement_mechanism).

% Network relationships: This constraint is enabled by and structurally
% linked to sanctions regimes and asset seizure mechanisms.
narrative_ontology:affects_constraint(sanctions_regime_russia, coe_ukraine_reparations_register).
narrative_ontology:affects_constraint(frozen_russian_assets_seizure, coe_ukraine_reparations_register).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this constraint. The standard derivation chain,
% using the declared beneficiary/victim groups and the distinct exit options
% for the institutional actors (trapped vs. arbitrage), correctly computes
% the directionality values needed to generate the observed perspectival gap.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */