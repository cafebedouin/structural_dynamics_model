% ============================================================================
% CONSTRAINT STORY: indo_german_defense_pact
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-24
% ============================================================================

:- module(constraint_indo_german_defense_pact, []).

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
 *   constraint_id: indo_german_defense_pact
 *   human_readable: India-Germany Defense Industrial Partnership
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   The strategic partnership between India and Germany aims to deepen defense
 *   industry collaboration through co-development, co-production, and
 *   technology transfer. While framed as a mutually beneficial coordination
 *   effort to diversify India's military hardware away from Russia and counter
 *   regional threats, it also establishes a long-term, asymmetric dependency
 *   where Germany acts as a high-end technology supplier to a capital-intensive
 *   Indian market. This creates a structural extraction channel alongside the
 *   genuine strategic coordination.
 *
 * KEY AGENTS (by structural relationship):
 *   - German Defense Industry: Primary beneficiary (institutional/arbitrage) — gains a major export market and strategic partner.
 *   - Indian Government/MoD: Ambivalent actor (institutional/constrained) — benefits from modernization but bears the financial cost and dependency risk.
 *   - Indian Taxpayers & Domestic SMEs: Primary target (powerless/trapped) — bears the immense financial cost and risks market capture by foreign primes.
 *   - Analytical Observer: Sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(indo_german_defense_pact, 0.48).
domain_priors:suppression_score(indo_german_defense_pact, 0.65).   % Structural property (raw, unscaled). High due to limited alternative high-tech suppliers.
domain_priors:theater_ratio(indo_german_defense_pact, 0.30).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(indo_german_defense_pact, extractiveness, 0.48).
narrative_ontology:constraint_metric(indo_german_defense_pact, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(indo_german_defense_pact, theater_ratio, 0.30).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(indo_german_defense_pact, tangled_rope).
narrative_ontology:human_readable(indo_german_defense_pact, "India-Germany Defense Industrial Partnership").

% --- Binary flags ---
domain_priors:requires_active_enforcement(indo_german_defense_pact). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(indo_german_defense_pact, german_defense_industry).
narrative_ontology:constraint_beneficiary(indo_german_defense_pact, indian_government).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(indo_german_defense_pact, indian_taxpayers).
narrative_ontology:constraint_victim(indo_german_defense_pact, indian_domestic_smes).
narrative_ontology:constraint_victim(indo_german_defense_pact, indian_government). % The govt is both beneficiary and victim of the costs/dependencies

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

% PERSPECTIVE 1: THE PRIMARY TARGET (Indian Taxpayers)
% They see a massive, multi-decade financial drain with indirect benefits,
% paying for systems that create foreign, not domestic, value.
% Engine derives: victim + trapped exit -> d ≈ 0.95 -> f(d) ≈ 1.42 -> high χ -> SNARE
constraint_indexing:constraint_classification(indo_german_defense_pact, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (German Defense Industry)
% They see a lucrative, long-term market secured by strategic alignment,
% with minimal downside risk.
% Engine derives: beneficiary + arbitrage exit -> d ≈ 0.05 -> f(d) ≈ -0.12 -> negative χ -> ROPE
constraint_indexing:constraint_classification(indo_german_defense_pact, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVE ---
% The Indian Government is also an institutional actor, but with a different
% structural position than its German counterpart. Its exit options are limited.

% PERSPECTIVE 3: The Indian Government (Constrained Institutional Actor)
% It simultaneously pursues the coordination goal (strategic autonomy, modernization)
% while bearing the costs and dependency risks of the extraction.
% Engine derives: beneficiary/victim + constrained exit -> middling d -> f(d) > 0 -> TANGLED ROPE
constraint_indexing:constraint_classification(indo_german_defense_pact, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER
% Sees both the valid coordination function and the asymmetric extraction.
% Engine derives d ≈ 0.72 -> f(d) ≈ 1.15. With high ε, this classifies as Tangled Rope.
constraint_indexing:constraint_classification(indo_german_defense_pact, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(indo_german_defense_pact_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify the core perspectival gap between the primary target and beneficiary.
    constraint_indexing:constraint_classification(indo_german_defense_pact, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(indo_german_defense_pact, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(inter_institutional_gap) :-
    % Verify that the two institutional actors classify the constraint differently.
    constraint_indexing:constraint_classification(indo_german_defense_pact, TypeGerman, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(indo_german_defense_pact, TypeIndian, context(agent_power(institutional), _, exit_options(constrained), _)),
    TypeGerman \= TypeIndian,
    TypeGerman = rope,
    TypeIndian = tangled_rope.

test(tangled_rope_conditions_met) :-
    % Verify that the structural requirements for a Tangled Rope are declared.
    narrative_ontology:constraint_beneficiary(indo_german_defense_pact, _),
    narrative_ontology:constraint_victim(indo_german_defense_pact, _),
    domain_priors:requires_active_enforcement(indo_german_defense_pact).

:- end_tests(indo_german_defense_pact_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.48): This reflects the significant, long-term financial outflow from India to Germany and the creation of technological dependency. It's not outright plunder, but it's a highly asymmetric value exchange inherent in high-tech arms deals.
 *   - Suppression (0.65): India's geopolitical need to diversify from Russian arms and modernize its forces is urgent. The number of nations with top-tier submarine or fighter jet technology willing to engage in meaningful transfer is very small, heavily suppressing alternative options.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. The German defense industry sees a pure coordination win (Rope): they solve India's problem (supply diversification) and get paid well, with a global scope and other customers if this fails (arbitrage). Indian taxpayers see a pure extraction scheme (Snare): a massive financial burden for equipment whose strategic value is abstract, with no ability to opt out (trapped).
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: The 'german_defense_industry' is the clearest beneficiary. The 'indian_government' is also listed as a beneficiary because it achieves real strategic goals (modernization, geopolitical alignment).
 *   - Victims: 'indian_taxpayers' and 'indian_domestic_smes' bear the direct financial costs and the opportunity costs of market capture. The 'indian_government' is also a victim of the dependency and high costs, creating its ambivalent structural position. This dual role is key to its Tangled Rope classification.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This story is a prime example of inter-institutional dynamics. Both the German industry and Indian government are 'institutional' actors, but their structural positions differ sharply, captured by their `exit_options`. Germany has `arbitrage` (many potential buyers globally), leading to a low derived directionality (d) and a Rope classification. India has `constrained` options (few alternative suppliers for this level of tech), leading to a higher d and a Tangled Rope classification. The framework correctly models that not all institutions experience a constraint symmetrically.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies the dual nature of the pact. A naive analysis might label it purely as a Snare (focusing only on the cost to India) or a Rope (focusing only on the strategic alignment). The Tangled Rope classification, supported by the inter-institutional perspectives, correctly identifies that it is BOTH: a genuine coordination mechanism layered with significant, asymmetric extraction. This prevents mislabeling a complex geopolitical arrangement as either purely cooperative or purely exploitative.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_indo_german_defense_pact,
    'Is the "technology transfer" clause substantive enough to create sovereign Indian defense capabilities, or is it a "screwdriver clause" designed to perpetuate dependency?',
    'Analysis of final contract terms for specific projects (e.g., P75I submarines) over a 10-15 year period, measuring the percentage of value-add and critical IP developed and owned indigenously.',
    'If substantive, the constraint drifts towards Rope (ε decreases). If a screwdriver clause, it hardens into a Snare (ε remains high, theater ratio increases as "Make in India" proves hollow).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(indo_german_defense_pact, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This relationship evolved from diplomatic overtures to hard-nosed commercial/strategic pacts.
% The initial phase had more theater and lower realized extraction. Over time, as
% contracts are signed, the extraction becomes concrete and dependency sets in.

% Theater ratio over time (declines as deals become concrete):
narrative_ontology:measurement(igdp_tr_t0, indo_german_defense_pact, theater_ratio, 0, 0.45).
narrative_ontology:measurement(igdp_tr_t5, indo_german_defense_pact, theater_ratio, 5, 0.35).
narrative_ontology:measurement(igdp_tr_t10, indo_german_defense_pact, theater_ratio, 10, 0.30).

% Extraction over time (increases as dependency is locked in):
narrative_ontology:measurement(igdp_ex_t0, indo_german_defense_pact, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(igdp_ex_t5, indo_german_defense_pact, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(igdp_ex_t10, indo_german_defense_pact, base_extractiveness, 10, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: The pact allocates high-end technological and capital resources.
narrative_ontology:coordination_type(indo_german_defense_pact, resource_allocation).

% Network relationships: This pact directly impacts and is influenced by broader
% geopolitical constraints, particularly India's historic reliance on Russia.
narrative_ontology:affects_constraint(indo_german_defense_pact, india_russia_relations).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this story. The structural declarations
% (beneficiary/victim lists) combined with the distinct exit_options
% (arbitrage vs. constrained) for the institutional actors allow the
% directionality derivation engine to compute accurate and differentiated
% d-values automatically.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */