% ============================================================================
% CONSTRAINT STORY: womens_sizing_chaos
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_womens_sizing_chaos, []).

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
    narrative_ontology:omega_variable/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: womens_sizing_chaos
 *   human_readable: The chaotic and non-standardized system of US women's clothing sizes.
 *   domain: economic/social
 *
 * SUMMARY:
 *   This constraint describes the modern system of women's apparel sizing in the US, which
 *   emerged from the decay of a mid-20th century government-led standardization effort (PS 42-70).
 *   The original standard was a flawed Rope, built on a non-representative dataset. It has since
 *   been abandoned by individual brands in favor of "vanity sizing," where sizes are arbitrarily
 *   changed for marketing purposes. The result is a system with high theater (brands pretend to
 *   have a coherent sizing system) but no actual coordination function, imposing significant
 *   search and transaction costs on consumers.
 *
 * KEY AGENTS (by structural relationship):
 *   - Women Consumers: Primary target (powerless/trapped) — bear the costs of inconsistency, wasted time, and returns.
 *   - Apparel Brands: Primary beneficiary (institutional/arbitrage) — benefit from the lack of a standard, using vanity sizing as a marketing tool to flatter consumers and drive sales.
 *   - US Dept. of Commerce: Historical actor (institutional/analytical) — created the original standard but no longer enforces it.
 *   - Analytical Observer: Sees the full structure of the degraded system.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(womens_sizing_chaos, 0.22). % Costs are primarily wasted time, shipping/return fees, and psychological frustration, not direct capital extraction.
domain_priors:suppression_score(womens_sizing_chaos, 0.65).   % Structural property (raw, unscaled). Alternatives (e.g., bespoke tailoring) are inaccessible to the mass market. Consumers are forced to engage with the system.
domain_priors:theater_ratio(womens_sizing_chaos, 0.80).       % Piton detection (>= 0.70). Extremely high ratio of performative activity (marketing, vanity sizing, "find your fit" tools) to functional coordination.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(womens_sizing_chaos, extractiveness, 0.22).
narrative_ontology:constraint_metric(womens_sizing_chaos, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(womens_sizing_chaos, theater_ratio, 0.80).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(womens_sizing_chaos, piton).

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(womens_sizing_chaos, apparel_brands).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(womens_sizing_chaos, womens_consumers).

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

% PERSPECTIVE 1: THE PRIMARY TARGET (THE CONSUMER)
% Trapped in a high-theater, low-function system. The lack of coordination
% imposes costs they cannot avoid. The system is an inertial, frustrating
% piece of social infrastructure that no longer serves its original purpose.
% It classifies as a Piton.
constraint_indexing:constraint_classification(womens_sizing_chaos, piton,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (THE BRAND)
% For an individual brand, the *lack* of a mandatory standard is a feature,
% not a bug. It allows them to use vanity sizing as a competitive tool. This
% freedom is a form of coordination benefit for them.
% Engine derives d from beneficiary + arbitrage exit -> d ≈ 0.05 -> f(d) ≈ -0.12,
% resulting in negative extraction (a subsidy). This classifies as a Rope.
constraint_indexing:constraint_classification(womens_sizing_chaos, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% Sees the atrophied coordination function, the high ratio of performative
% marketing to actual utility, and the historical decay. This is the
% canonical definition of a Piton.
constraint_indexing:constraint_classification(womens_sizing_chaos, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(womens_sizing_chaos_tests).

test(perspectival_gap_consumer_vs_brand) :-
    % Verify that consumers (target) and brands (beneficiary) see the
    % constraint differently.
    constraint_indexing:constraint_classification(womens_sizing_chaos, TypeTarget,
        context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(womens_sizing_chaos, TypeBeneficiary,
        context(agent_power(institutional), _, exit_options(arbitrage), _)),
    assertion(TypeTarget == piton),
    assertion(TypeBeneficiary == rope),
    TypeTarget \= TypeBeneficiary.

test(piton_threshold_validation) :-
    % Verify the constraint meets the Piton criteria from the analytical view.
    narrative_ontology:constraint_metric(womens_sizing_chaos, theater_ratio, TR),
    narrative_ontology:constraint_metric(womens_sizing_chaos, extractiveness, E),
    assertion(TR >= 0.70),
    assertion(E =< 0.25).

:- end_tests(womens_sizing_chaos_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The chosen classification is Piton, reflecting a system whose original
 *   coordination function (interoperable sizing) has atrophied, but which
 *   persists due to institutional inertia and theatrical maintenance. The
 *   high theater_ratio (0.80) captures the essence of vanity sizing: brands
 *   invest heavily in the *performance* of sizing while undermining its
 *   actual function. The base_extractiveness (0.22) is low but non-zero,
 *   representing the aggregated costs of returns, wasted time, and consumer
 *   frustration, which are real but not as direct as financial debt.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For consumers (powerless/trapped), the system is a
 *   Piton: a useless, immovable piece of infrastructure they must navigate.
 *   For apparel brands (institutional/arbitrage), the chaos is a feature. The
 *   *absence* of a standard functions as a Rope, coordinating a market where
 *   they are free to use sizing as a competitive marketing tactic. One agent's
 *   broken tool is another's weapon.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `apparel_brands`. They benefit from the flexibility to
 *     manipulate sizing to flatter customers and create brand-specific fit,
 *     driving loyalty and sales. The lack of a standard is their subsidy.
 *   - Victim: `womens_consumers`. They bear the direct costs of the failed
 *     coordination through wasted time, money on returns, and the cognitive
 *     load of tracking inconsistent sizes across dozens of brands.
 *   The engine correctly derives a low `d` for brands (negative effective extraction)
 *   and a high `d` for consumers (positive effective extraction), creating the gap.
 *
 * MANDATROPHY ANALYSIS:
 *   This case is a classic example of Mandatrophy. A system created for
 *   coordination (a Rope) has decayed into a Piton. Simply labeling the
 *   system as "bad" or "extractive" (a Snare) would be inaccurate; the base
 *   extraction is too low. The Piton classification correctly identifies the
 *   pathology: not raw extraction, but functional decay masked by high theater.
 *   It prevents misdiagnosing the problem as malice when it is primarily neglect
 *   and perverse incentives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% /5 form: narrative detail for story context
omega_variable(
    omega_womens_sizing_chaos,
    'Is a functional, universal sizing standard feasible or even desirable in the modern apparel market?',
    'A large-scale study of consumer preference (standardization vs. vanity sizing) combined with an economic analysis of the costs/benefits of re-standardization for brands.',
    'If feasible, the current Piton could be replaced by a new Rope. If not, the Piton is an equilibrium state, and solutions must focus on mitigating its harms (e.g., better recommendation tech) rather than replacing it.',
    confidence_without_resolution(low)
).

% /3 form: typed classification for reporting engine (REQUIRED)
narrative_ontology:omega_variable(omega_womens_sizing_chaos, empirical, 'The feasibility and net benefit of re-establishing a universal sizing standard for women\'s apparel.').

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(womens_sizing_chaos, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint is a perfect example of lifecycle drift, specifically the
% decay of a Rope into a Piton. The data models this transition.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(wsc_tr_t0, womens_sizing_chaos, theater_ratio, 0, 0.10).  % T=0 (1970s): Standard exists, low theater.
narrative_ontology:measurement(wsc_tr_t5, womens_sizing_chaos, theater_ratio, 5, 0.50).  % T=5 (1990s): Vanity sizing emerges, theater rises.
narrative_ontology:measurement(wsc_tr_t10, womens_sizing_chaos, theater_ratio, 10, 0.80). % T=10 (Present): System is pure theater.

% Extraction over time (triggers extraction_accumulation detection):
% Extraction from the flawed sample was arguably higher initially, then decreased
% as the system became pure noise.
narrative_ontology:measurement(wsc_ex_t0, womens_sizing_chaos, base_extractiveness, 0, 0.30).  % T=0: The standard imposes costs on non-conforming body types.
narrative_ontology:measurement(wsc_ex_t5, womens_sizing_chaos, base_extractiveness, 5, 0.25).  % T=5: As the standard diffuses, the specific harm lessens, replaced by general chaos.
narrative_ontology:measurement(wsc_ex_t10, womens_sizing_chaos, base_extractiveness, 10, 0.22). % T=10: Harm is now from inconsistency, not a bad standard.

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% The original constraint was a standard for information exchange.
narrative_ontology:coordination_type(womens_sizing_chaos, information_standard).

% The pressures of fast fashion, which prioritize speed and novelty over
% interoperability, structurally undermine any attempt at standardization.
narrative_ontology:affects_constraint(fast_fashion_production_cycle, womens_sizing_chaos).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The automatic derivation
% from beneficiary/victim declarations and exit options accurately models the
% structural relationships between consumers and brands.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */