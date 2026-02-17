% ============================================================================
% CONSTRAINT STORY: ec_meta_manus_block
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_ec_meta_manus_block, []).

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
    constraint_indexing:directionality_override/3,
    domain_priors:emerges_naturally/1,
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: ec_meta_manus_block
 *   human_readable: European Commission's block of Meta's acquisition of Manus VR
 *   domain: economic/technological
 *
 * SUMMARY:
 *   In 2023, the European Commission (EC) blocked Meta Platform's acquisition of
 *   Manus VR, a Dutch startup specializing in neural interface technology for
 *   virtual reality. The EC argued the deal would harm competition in the nascent
 *   market for VR input devices, stifle innovation, and lead to higher prices.
 *   This regulatory action constrains Meta's corporate strategy while aiming
 *   to coordinate a more competitive market for other players.
 *
 * KEY AGENTS (by structural relationship):
 *   - Meta Platforms, Inc.: Primary target (institutional/constrained) — bears the cost of the blocked acquisition and lost market opportunity.
 *   - European Commission: Primary beneficiary (institutional/arbitrage) — successfully enforced its competition policy, acting as the rule-maker.
 *   - Rival Tech Firms & Startups: Secondary beneficiary (organized/mobile) — benefit from the preserved market space and reduced threat of Meta's dominance.
 *   - European Consumers: Tertiary beneficiary (powerless/constrained) — theoretically benefit from future competition, lower prices, and more choice.
 *   - Analytical Observer: Analytical perspective — sees both the market coordination function and the coercive extraction from Meta.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% The constraint extracts significant opportunity cost from Meta and imposes legal/strategic costs.
domain_priors:base_extractiveness(ec_meta_manus_block, 0.55).
% The EC's legal authority makes this block highly coercive and difficult to circumvent for Meta.
domain_priors:suppression_score(ec_meta_manus_block, 0.85).
% The action was substantive and legally grounded, not performative.
domain_priors:theater_ratio(ec_meta_manus_block, 0.10).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(ec_meta_manus_block, extractiveness, 0.55).
narrative_ontology:constraint_metric(ec_meta_manus_block, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(ec_meta_manus_block, theater_ratio, 0.10).

% --- NL Profile Metrics (required for mountain constraints) ---
% Not applicable; this is a human-constructed regulatory constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(ec_meta_manus_block, tangled_rope).
narrative_ontology:human_readable(ec_meta_manus_block, "European Commission's block of Meta's acquisition of Manus VR").

% --- Binary flags ---
% The block required active investigation and a formal ruling by the EC.
domain_priors:requires_active_enforcement(ec_meta_manus_block). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(ec_meta_manus_block, european_commission).
narrative_ontology:constraint_beneficiary(ec_meta_manus_block, rival_tech_firms_and_startups).
narrative_ontology:constraint_beneficiary(ec_meta_manus_block, european_consumers).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(ec_meta_manus_block, meta_platforms_inc).

% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three met)

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

% --- INTER-INSTITUTIONAL PERSPECTIVES ---
% The constraint operates between two institutional actors, Meta and the EC,
% with opposing structural relationships.

% PERSPECTIVE 1: THE PRIMARY TARGET (META PLATFORMS, INC.)
% As the victim with constrained exit options within the EU market, Meta perceives
% the block as a highly coercive and extractive act.
% Engine derives d from: victim membership + constrained exit -> high d -> high χ.
constraint_indexing:constraint_classification(ec_meta_manus_block, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (EUROPEAN COMMISSION)
% As the rule-maker and beneficiary with maximum flexibility (arbitrage exit),
% the EC perceives its action as a pure market-coordination mechanism.
% Engine derives d from: beneficiary membership + arbitrage exit -> low d -> negative χ.
constraint_indexing:constraint_classification(ec_meta_manus_block, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: SECONDARY BENEFICIARIES (RIVAL FIRMS)
% Rival firms see this as a beneficial coordination rule that levels the playing field.
constraint_indexing:constraint_classification(ec_meta_manus_block, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(continental))).

% PERSPECTIVE 4: TERTIARY BENEFICIARIES (CONSUMERS)
% Consumers, though individually powerless, are beneficiaries. The constraint appears
% as a protective rope ensuring future choice.
constraint_indexing:constraint_classification(ec_meta_manus_block, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).


% PERSPECTIVE 5: THE ANALYTICAL OBSERVER
% The analytical view integrates both the coordination function and the asymmetric
% extraction, classifying the constraint as a Tangled Rope. This matches the claim.
constraint_indexing:constraint_classification(ec_meta_manus_block, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ec_meta_manus_block_tests).

test(perspectival_gap_institutional) :-
    % Verify the core inter-institutional gap between Meta and the EC.
    constraint_indexing:constraint_classification(ec_meta_manus_block, snare, context(agent_power(institutional), _, exit_options(constrained), _)),
    constraint_indexing:constraint_classification(ec_meta_manus_block, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    true.

test(analytical_claim_matches) :-
    % Ensure the analytical classification matches the declared constraint claim.
    constraint_indexing:constraint_classification(ec_meta_manus_block, Type, context(agent_power(analytical), _, _, _)),
    narrative_ontology:constraint_claim(ec_meta_manus_block, Type).

test(tangled_rope_structural_gates_pass) :-
    % Verify that all three structural requirements for a Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(ec_meta_manus_block, _),
    narrative_ontology:constraint_victim(ec_meta_manus_block, _),
    domain_priors:requires_active_enforcement(ec_meta_manus_block).

:- end_tests(ec_meta_manus_block_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (0.55): Represents the significant opportunity cost to Meta (a multi-billion dollar strategic play) and the direct costs of the failed M&A process.
 *   - Suppression Score (0.85): Reflects the high coercive power of the European Commission's regulatory authority. Meta has limited recourse beyond legal appeals, which are costly and uncertain.
 *   - Tangled Rope Classification: The constraint is a textbook Tangled Rope because it possesses both a genuine coordination function (preserving market competition for the benefit of rivals and consumers) and a clear, asymmetric extractive component (the denial of a strategic asset to Meta).
 *
 * PERSPECTIVAL GAP:
 *   The profound gap between Meta (Snare) and the EC/rivals (Rope) is the central feature.
 *   - Meta experiences the constraint as pure coercion. From their indexical position, the 'coordination benefit' is an externality; the immediate reality is a targeted, costly obstruction.
 *   - The EC and other market players experience the constraint as pure coordination. They are the beneficiaries of the market structure being enforced, and they bear none of the direct costs imposed on Meta.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is derived from the structural roles. `meta_platforms_inc` is declared the victim, which drives its directionality `d` towards 1.0, resulting in high effective extraction (χ) and a Snare classification. The `european_commission`, `rival_tech_firms`, and `european_consumers` are declared beneficiaries, driving their `d` towards 0.0, resulting in low or negative χ and a Rope classification.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This is a prime example of an inter-institutional constraint. Both Meta and the EC are `institutional` actors, but their relationship to the constraint is diametrically opposed. The system differentiates them not by power level but by their structural role (victim vs. beneficiary) and exit options (`constrained` for Meta vs. `arbitrage` for the EC). This produces the correct perspectival gap without needing manual directionality overrides.
 *
 * MANDATROPHY ANALYSIS:
 *   This framework correctly identifies the dual nature of the regulatory action. A simplistic analysis might label it purely as "protecting consumers" (Rope) or purely as "anti-business government overreach" (Snare). The Tangled Rope classification acknowledges that both are true from different perspectives and that the constraint's structure contains both coordination and extraction. This prevents mischaracterization and captures the inherent political tension of antitrust enforcement.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_ec_meta_manus_block,
    'Does blocking this acquisition genuinely foster a competitive ecosystem of innovators, or does it merely protect existing tech giants (like Apple, Google) from a new competitor in the VR space?',
    'Empirical observation of the VR/AR input device market over the next 5-10 years, tracking startup funding, market share distribution, and consumer prices.',
    'If it fosters innovation, the Rope classification is strengthened. If it entrenches existing players, the extraction component becomes more pronounced, potentially shifting the analytical view closer to a Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(ec_meta_manus_block, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% The constraint intensified from non-existent to fully enforced over the period
% of the EC's investigation and ruling. This is required as ε > 0.46.

% Theater ratio over time (remains low):
narrative_ontology:measurement(ec_meta_manus_block_tr_t0, ec_meta_manus_block, theater_ratio, 0, 0.0).
narrative_ontology:measurement(ec_meta_manus_block_tr_t5, ec_meta_manus_block, theater_ratio, 5, 0.05).
narrative_ontology:measurement(ec_meta_manus_block_tr_t10, ec_meta_manus_block, theater_ratio, 10, 0.10).

% Extraction over time (ramps up to the final ruling):
narrative_ontology:measurement(ec_meta_manus_block_ex_t0, ec_meta_manus_block, base_extractiveness, 0, 0.0).
narrative_ontology:measurement(ec_meta_manus_block_ex_t5, ec_meta_manus_block, base_extractiveness, 5, 0.25).
narrative_ontology:measurement(ec_meta_manus_block_ex_t10, ec_meta_manus_block, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The EC's action is a mechanism for enforcing a market structure.
narrative_ontology:coordination_type(ec_meta_manus_block, enforcement_mechanism).

% Network relationships (structural influence edges)
% This ruling sets a precedent that will influence future regulatory reviews
% of tech mergers and acquisitions.
narrative_ontology:affects_constraint(ec_meta_manus_block, future_tech_merger_reviews).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The automatic derivation chain,
% using the declared beneficiary/victim roles and the differing exit_options
% for the institutional actors, correctly computes the directionality values
% and produces the observed perspectival gap. This serves as a canonical example
% of the derivation system functioning as intended.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */