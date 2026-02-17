% ============================================================================
% CONSTRAINT STORY: eu_digital_services_act
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-06-11
% ============================================================================

:- module(constraint_eu_digital_services_act, []).

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
 *   constraint_id: eu_digital_services_act
 *   human_readable: EU Digital Services Act (DSA)
 *   domain: technological/political
 *
 * SUMMARY:
 *   The European Union's Digital Services Act (DSA) imposes a comprehensive
 *   set of rules on online platforms, particularly "very large online
 *   platforms" (VLOPs), to combat illegal content, protect user rights, and
 *   increase transparency. The US government and affected US-based tech
 *   companies argue that the regulation unfairly targets them, creating
 *   significant compliance costs and functioning as digital protectionism.
 *   This creates a classic perspectival gap between the regulator (who sees
 *   a coordination mechanism) and the regulated (who see an extractive snare).
 *
 * KEY AGENTS (by structural relationship):
 *   - US-based VLOPs (e.g., Google, Meta): Primary target (organized/constrained) — bear the costs of compliance, risk fines, and face business model restrictions.
 *   - European Commission: Primary beneficiary (institutional/arbitrage) — gains regulatory power and achieves its stated policy goals of creating a "safer digital space."
 *   - US Government: Secondary institutional actor (institutional/constrained) — sides with its domestic companies, viewing the DSA as a trade barrier, but has limited power to directly alter EU law.
 *   - European Consumers: Nominal beneficiaries (powerless/trapped) — intended to benefit from increased safety and choice, but as individuals have no exit from the system.
 *   - Analytical Observer: Sees both the coordination function and the asymmetric extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_digital_services_act, 0.55).
domain_priors:suppression_score(eu_digital_services_act, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(eu_digital_services_act, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_digital_services_act, extractiveness, 0.55).
narrative_ontology:constraint_metric(eu_digital_services_act, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(eu_digital_services_act, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this human-constructed regulatory framework.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(eu_digital_services_act, tangled_rope).
narrative_ontology:human_readable(eu_digital_services_act, "EU Digital Services Act (DSA)").

% --- Binary flags ---
domain_priors:requires_active_enforcement(eu_digital_services_act). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(eu_digital_services_act, eu_regulatory_bodies).
narrative_ontology:constraint_beneficiary(eu_digital_services_act, european_consumers).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(eu_digital_services_act, us_based_gatekeeper_platforms).
%
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
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (US-based VLOPs)
% As organized victims with constrained exit (leaving the EU market is not
% viable), the engine derives a high `d`. The high base extraction (ε=0.55) and
% suppression (0.75), amplified by f(d) and continental scope, results in a
% Snare classification. They experience the regulation as purely extractive.
constraint_indexing:constraint_classification(eu_digital_services_act, snare,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (European Commission)
% As an institutional beneficiary with arbitrage exit (they write and can
% amend the rules), the engine derives a very low `d`. This leads to a low
% or negative effective extraction (χ), classifying the DSA as a pure
% coordination mechanism, or Rope.
constraint_indexing:constraint_classification(eu_digital_services_act, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The analytical view sees both the coordination function (beneficiaries exist)
% and the asymmetric extraction (victims exist), along with the need for
% active enforcement. This combination perfectly matches the signature of a
% Tangled Rope.
constraint_indexing:constraint_classification(eu_digital_services_act, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---
% The US Government's perspective demonstrates the inter-institutional conflict.

% Perspective 4: Opposing Institutional Actor (US Government)
% Though an institutional power, its exit options are constrained in altering
% EU law. It aligns with the victims. The engine derives a moderately high 'd',
% leading to a high χ. From its view, the DSA is not a valid coordination
% mechanism but an extractive Snare targeting its domestic industry.
constraint_indexing:constraint_classification(eu_digital_services_act, snare,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% Perspective 5: The Nominal Beneficiary (European Consumer)
% An individual consumer is a powerless, trapped beneficiary. They cannot opt
% out. The engine derives a moderate 'd' (beneficiary status lowers it,
% trapped status raises it). The high base extraction remains visible, resulting
% in a Tangled Rope classification. They get the benefits, but also perceive
% the coercive nature of the system.
constraint_indexing:constraint_classification(eu_digital_services_act, tangled_rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_digital_services_act_tests).

test(perspectival_gap_regulator_vs_regulated) :-
    % Verify the core perspectival gap between the EU Commission and US Tech.
    constraint_indexing:constraint_classification(eu_digital_services_act, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(eu_digital_services_act, snare, context(agent_power(organized), _, exit_options(constrained), _)).

test(inter_institutional_conflict) :-
    % Verify the gap between the two institutional actors (EU vs US Gov).
    constraint_indexing:constraint_classification(eu_digital_services_act, TypeEU, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(eu_digital_services_act, TypeUS, context(agent_power(institutional), _, exit_options(constrained), _)),
    TypeEU \= TypeUS,
    TypeEU = rope,
    TypeUS = snare.

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(eu_digital_services_act, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    % Ensure all three conditions for a Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(eu_digital_services_act, _),
    narrative_ontology:constraint_victim(eu_digital_services_act, _),
    domain_priors:requires_active_enforcement(eu_digital_services_act).

:- end_tests(eu_digital_services_act_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): Set to reflect the significant, non-trivial costs of compliance, potential for massive fines (up to 10% of global turnover), and required changes to core business models. It's high enough to cause conflict but not a pure wealth transfer.
 *   - Suppression (0.75): The DSA is law in the EU, a market too large for any VLOP to exit. Compliance is mandatory, so suppression of alternatives is very high.
 *   - Theater (0.15): The regulation has functional enforcement mechanisms and concrete goals, it is not primarily performative.
 *   The combination of a genuine coordination goal (market safety, user rights) with high, asymmetrically applied extraction and enforcement makes this a canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is profound. The EU Commission (beneficiary, arbitrage exit) experiences the DSA as a pure Rope—a tool for beneficial coordination with negligible personal cost. US Tech firms (victims, constrained exit) experience it as a Snare—a coercive, extractive tool that targets them specifically without providing proportional benefit. This difference is driven entirely by their structural relationship to the constraint, as captured by the directionality `d` derived from beneficiary/victim status and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: The `eu_regulatory_bodies` benefit by gaining power and fulfilling their mandate. `european_consumers` are the intended beneficiaries. This establishes a coordination function.
 *   - Victims: The `us_based_gatekeeper_platforms` are explicitly named and targeted by the regulation. They bear the direct financial and operational costs. This establishes asymmetric extraction.
 *   The engine correctly derives a low `d` for beneficiaries and a high `d` for victims, mathematically producing the observed perspectival gap.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This story is a prime example of inter-institutional conflict. Both the EU Commission and the US Government are 'institutional' actors, but their differing structural positions and exit options lead to opposite classifications. The EU has `arbitrage` exit (as the rule-maker), while the US Government has `constrained` exit (it can apply diplomatic pressure but cannot veto EU law). This structural difference, captured by the model, explains why one institution sees a Rope and the other sees a Snare in the very same law.
 *
 * MANDATROPHY ANALYSIS:
 *   The Tangled Rope classification prevents mandatrophy. A naive analysis might label the DSA a Snare (focusing only on US complaints) or a Rope (focusing only on EU justifications). The Deferential Realism framework, by requiring perspectives from both the beneficiary and victim, correctly identifies the dual nature of the constraint: it is a system that performs a real coordination function *through* the mechanism of asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_eu_dsa,
    'Is the DSA primarily a genuine market-correcting coordination tool or a form of digital protectionism designed to disadvantage foreign firms?',
    'Long-term observation of market outcomes (5-10 years): emergence of viable EU-based competitors, changes in innovation rates for regulated vs. unregulated services, and analysis of enforcement actions for bias.',
    'If primarily coordination -> ε might be lower than estimated, and its Rope-like properties are dominant. If primarily protectionism -> ε is accurate or underestimated, and its Snare-like properties are dominant.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(eu_digital_services_act, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for this high-extraction (ε > 0.46) constraint.
% Models the intensification of the regulation from initial proposals (lower ε)
% to the final, enforced law (higher ε). Theater remains low and functional.

% Theater ratio over time (stable):
narrative_ontology:measurement(eu_dsa_tr_t0, eu_digital_services_act, theater_ratio, 0, 0.15).
narrative_ontology:measurement(eu_dsa_tr_t5, eu_digital_services_act, theater_ratio, 5, 0.15).
narrative_ontology:measurement(eu_dsa_tr_t10, eu_digital_services_act, theater_ratio, 10, 0.15).

% Extraction over time (increasing during legislative process):
narrative_ontology:measurement(eu_dsa_ex_t0, eu_digital_services_act, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(eu_dsa_ex_t5, eu_digital_services_act, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(eu_dsa_ex_t10, eu_digital_services_act, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(eu_digital_services_act, enforcement_mechanism).

% Network relationships (structural influence edges)
% The DSA is a sister regulation to the Digital Markets Act (DMA) and builds
% upon principles from GDPR.
narrative_ontology:affects_constraint(eu_digital_services_act, eu_digital_markets_act).
narrative_ontology:affects_constraint(gdpr_compliance, eu_digital_services_act).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this story. The standard derivation chain,
% using beneficiary/victim declarations and exit options, accurately models
% the structural relationships and generates the correct directionality
% values (d) for all key agents.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */