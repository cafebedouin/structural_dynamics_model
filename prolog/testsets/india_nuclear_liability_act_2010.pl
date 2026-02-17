% ============================================================================
% CONSTRAINT STORY: india_nuclear_liability_act_2010
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-22
% ============================================================================

:- module(constraint_india_nuclear_liability_act_2010, []).

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
 *   constraint_id: india_nuclear_liability_act_2010
 *   human_readable: India's Civil Liability for Nuclear Damage Act of 2010
 *   domain: economic/political
 *
 * SUMMARY:
 *   This constraint is the legal framework established by India to govern
 *   liability in the event of a nuclear accident. It caps the financial
 *   liability of nuclear plant operators at a fixed amount (approx. $322M),
 *   while also providing a legal channel for operators to seek recourse from
 *   equipment suppliers if faults are proven. The law was a prerequisite
 *   for private and foreign investment in India's civil nuclear sector,
 *   but critics argue it transfers the immense tail risk of a catastrophic
 *   accident from corporations to the Indian public.
 *
 * KEY AGENTS (by structural relationship):
 *   - potential_victims_of_accident: Primary target (powerless/trapped) — bears the uncompensated cost of a major disaster.
 *   - nuclear_operators_and_suppliers: Primary beneficiary (institutional/arbitrage) — receives a liability cap, making their business model viable.
 *   - indian_government: Institutional architect (institutional/constrained) — seeks to coordinate industrial development but is politically constrained.
 *   - analytical_observer: Analytical observer — sees the dual function of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(india_nuclear_liability_act_2010, 0.55).
domain_priors:suppression_score(india_nuclear_liability_act_2010, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(india_nuclear_liability_act_2010, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(india_nuclear_liability_act_2010, extractiveness, 0.55).
narrative_ontology:constraint_metric(india_nuclear_liability_act_2010, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(india_nuclear_liability_act_2010, theater_ratio, 0.20).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(india_nuclear_liability_act_2010, tangled_rope).
narrative_ontology:human_readable(india_nuclear_liability_act_2010, "India's Civil Liability for Nuclear Damage Act of 2010").

% --- Binary flags ---
domain_priors:requires_active_enforcement(india_nuclear_liability_act_2010). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(india_nuclear_liability_act_2010, nuclear_operators_and_suppliers).
narrative_ontology:constraint_beneficiary(india_nuclear_liability_act_2010, indian_government).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(india_nuclear_liability_act_2010, potential_victims_of_accident).
%
% Gate requirements:
%   Tangled Rope: beneficiary + victim + requires_active_enforcement (all three are present)

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × f(d) × σ(S)
   where f(d) is the sigmoid directionality function:
     f(d) = -0.20 + 1.70 / (1 + e^(-6*(d - 0.50)))
   The engine derives d from beneficiary/victim membership + exit_options.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   CONTEXT ARITY: All context() terms must have exactly 4 arguments.
   Linter Rule 23 rejects files with context arity ≠ 4.
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (SNARE)
% The Indian public, who bears the uncompensated tail risk of an accident.
% victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ.
% χ = 0.55 * 1.42 * 1.0 (national) ≈ 0.78. This is a Snare (χ > 0.66).
constraint_indexing:constraint_classification(india_nuclear_liability_act_2010, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% Nuclear operators and foreign suppliers who require a liability cap to invest.
% beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ.
% χ = 0.55 * -0.12 * 1.0 (national) ≈ -0.066. This is a Rope.
constraint_indexing:constraint_classification(india_nuclear_liability_act_2010, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analytical perspective sees both the coordination function and the
% asymmetric extraction.
% d ≈ 0.72 → f(d) ≈ 1.15. Global scope amplifies χ.
% χ = 0.55 * 1.15 * 1.2 (global) ≈ 0.759. Within Tangled Rope range [0.40, 0.90].
constraint_indexing:constraint_classification(india_nuclear_liability_act_2010, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---
% The Indian Government, architect of the bill. It benefits by achieving its
% energy policy goals, but is politically constrained and must manage the
% public fallout, giving it less favorable terms than the unconstrained
% corporate beneficiaries.
% beneficiary membership + constrained exit → higher d than arbitrage.
% This still resolves to a Rope, but with a less negative (or slightly positive) χ,
% representing a less 'pure' coordination benefit than for the suppliers.
constraint_indexing:constraint_classification(india_nuclear_liability_act_2010, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(india_nuclear_liability_act_2010_tests).

test(perspectival_gap_is_snare_vs_rope) :-
    constraint_indexing:constraint_classification(india_nuclear_liability_act_2010, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(india_nuclear_liability_act_2010, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    format('... Perspectival gap validated: Snare (powerless) vs Rope (institutional)~n').

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(india_nuclear_liability_act_2010, tangled_rope, context(agent_power(analytical), _, _, _)),
    format('... Analytical classification is Tangled Rope~n').

test(tangled_rope_gate_requirements_met) :-
    narrative_ontology:constraint_beneficiary(india_nuclear_liability_act_2010, _),
    narrative_ontology:constraint_victim(india_nuclear_liability_act_2010, _),
    domain_priors:requires_active_enforcement(india_nuclear_liability_act_2010),
    format('... Tangled Rope structural requirements (beneficiary, victim, enforcement) are met~n').

:- end_tests(india_nuclear_liability_act_2010_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): High. Represents the massive value of tail risk
 *     transferred from corporations to the public. The liability cap is orders of
 *     magnitude smaller than the potential cost of a Chernobyl- or Fukushima-scale
 *     disaster.
 *   - Suppression (0.75): High. The law is a state-level act that forecloses all
 *     alternative liability frameworks. Individuals and communities have no
 *     mechanism to opt-out or negotiate different terms, and political opposition
 *     was overcome to pass the bill.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark and defines the political controversy.
 *   - For beneficiaries (operators/suppliers), the liability cap is a pure
 *     coordination mechanism (Rope) that removes an infinite-risk barrier,
 *     enabling the nuclear industry to function. From their perspective, with
 *     arbitrage exit options, the law is purely enabling.
 *   - For the primary target (potential victims), the same law is a Snare. It
 *     traps them in a system where the profits from nuclear energy are privatized,
 *     while the most catastrophic risks are socialized. Their inability to exit
 *     or insure against this risk makes the extraction severe.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: `nuclear_operators_and_suppliers` are the direct financial
 *     beneficiaries. They can enter a market that would otherwise be closed to them.
 *     The `indian_government` is a secondary beneficiary, achieving geopolitical
 *     and energy policy goals.
 *   - Victim: The `potential_victims_of_accident` group bears the cost. This isn't
 *     a direct financial transfer, but a transfer of uncompensated risk, which is
 *     a form of extraction. The directionality engine correctly assigns a high `d`
 *     value to this trapped group and a low `d` value to the mobile beneficiaries,
 *     generating the Snare/Rope gap.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a canonical example of a Tangled Rope. A pure Snare analysis would miss
 *   the fact that the law *does* solve a genuine coordination problem: without
 *   some form of liability limitation, the industry cannot exist. A pure Rope
 *   analysis would ignore the massive, asymmetric transfer of risk. The Tangled
 *   Rope classification from the analytical perspective correctly identifies the
 *   hybrid nature of the constraint—it is a coordination solution achieved via
 *   asymmetric extraction. This prevents mislabeling it as either pure malevolence
 *   or pure public good.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_india_nuclear_liability_act_2010,
    'What is the true, actuarially sound value of the tail risk of a catastrophic nuclear accident in India?',
    'Long-term empirical data on next-generation reactor safety performance and failure modes, combined with detailed local geographical and population density models.',
    'If the true risk is vanishingly small, the liability cap is reasonable and the constraint is more Rope-like (ε is lower). If the risk is systematically underestimated, the cap is highly extractive and the constraint is more Snare-like (ε is higher).',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(india_nuclear_liability_act_2010, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Base extraction is high (0.55 > 0.46), so temporal data is required.
% T=0 (2008): Post US-India deal, bill formulation begins.
% T=5 (2013): Post-Fukushima (2011), political theater to defend the cap increases.
% T=10 (2018): Law is entrenched and normalized.

% Theater ratio over time:
narrative_ontology:measurement(inla2010_tr_t0, india_nuclear_liability_act_2010, theater_ratio, 0, 0.10).
narrative_ontology:measurement(inla2010_tr_t5, india_nuclear_liability_act_2010, theater_ratio, 5, 0.30).
narrative_ontology:measurement(inla2010_tr_t10, india_nuclear_liability_act_2010, theater_ratio, 10, 0.20).

% Extraction over time:
narrative_ontology:measurement(inla2010_ex_t0, india_nuclear_liability_act_2010, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(inla2010_ex_t5, india_nuclear_liability_act_2010, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(inla2010_ex_t10, india_nuclear_liability_act_2010, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The law allocates financial risk to enable a market.
narrative_ontology:coordination_type(india_nuclear_liability_act_2010, resource_allocation).

% Network relationships: The 2010 Act was a direct consequence of the 2008
% US-India civil nuclear agreement, which created the conditions for US firms
% to enter the Indian market, conditional on a liability framework.
narrative_ontology:affects_constraint(us_india_civil_nuclear_agreement_2008, india_nuclear_liability_act_2010).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed for this constraint. The standard derivation from
% beneficiary/victim declarations and exit options accurately models the
% structural relationships and generates the correct perspectival gap.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */