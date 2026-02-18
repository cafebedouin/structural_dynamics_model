% ============================================================================
% CONSTRAINT STORY: eu_mercosur_trade_agreement
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_eu_mercosur_trade_agreement, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: eu_mercosur_trade_agreement
 *   human_readable: EU-Mercosur Free Trade Agreement
 *   domain: economic/political
 *
 * SUMMARY:
 *   A proposed free trade agreement between the European Union and the Mercosur
 *   bloc (Brazil, Argentina, Paraguay, Uruguay). The agreement aims to
 *   coordinate trade and create one of the world's largest free-trade areas,
 *   but it generates significant asymmetric costs. Proponents cite strategic
 *   advantages and market access, while opponents highlight devastating impacts
 *   on specific sectors (EU agriculture) and the environment (Amazon deforestation).
 *
 * KEY AGENTS (by structural relationship):
 *   - Amazon Indigenous Communities: Primary target (powerless/trapped) — bear costs of environmental destruction and land displacement.
 *   - EU Small Farmers: Primary target (organized/constrained) — bear costs of increased competition from cheaper imports.
 *   - EU Industrial Exporters: Primary beneficiary (institutional/arbitrage) — gain access to a large new market and critical resources.
 *   - Mercosur Agribusiness Exporters: Primary beneficiary (organized/arbitrage) — gain access to the lucrative EU market.
 *   - European Commission: Institutional proponent (institutional/arbitrage) — pursues strategic geopolitical goals.
 *   - French Government: Institutional opponent (institutional/constrained) — constrained by domestic political pressure from farmers.
 *   - Analytical Observer: Sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_mercosur_trade_agreement, 0.48).
domain_priors:suppression_score(eu_mercosur_trade_agreement, 0.65).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(eu_mercosur_trade_agreement, 0.20).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_mercosur_trade_agreement, extractiveness, 0.48).
narrative_ontology:constraint_metric(eu_mercosur_trade_agreement, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(eu_mercosur_trade_agreement, theater_ratio, 0.20).

% --- NL Profile Metrics (required for mountain constraints) ---
% Not a mountain constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(eu_mercosur_trade_agreement, tangled_rope).
narrative_ontology:human_readable(eu_mercosur_trade_agreement, "EU-Mercosur Free Trade Agreement").
narrative_ontology:topic_domain(eu_mercosur_trade_agreement, "economic/political").

% --- Binary flags ---
domain_priors:requires_active_enforcement(eu_mercosur_trade_agreement). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(eu_mercosur_trade_agreement, eu_industrial_exporters).
narrative_ontology:constraint_beneficiary(eu_mercosur_trade_agreement, mercosur_agribusiness_exporters).
narrative_ontology:constraint_beneficiary(eu_mercosur_trade_agreement, european_commission).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(eu_mercosur_trade_agreement, eu_small_farmers).
narrative_ontology:constraint_victim(eu_mercosur_trade_agreement, amazon_indigenous_communities).
narrative_ontology:constraint_victim(eu_mercosur_trade_agreement, french_government). % Victim of political fallout.

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

% PERSPECTIVE 1: THE MOST VULNERABLE TARGET (SNARE)
% Amazon indigenous communities who face land displacement and ecological collapse.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42.
% χ = 0.48 * 1.42 * 1.2 (global) ≈ 0.82. This is a clear Snare.
constraint_indexing:constraint_classification(eu_mercosur_trade_agreement, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (ROPE)
% EU industrial exporters who gain market access.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12.
% χ = 0.48 * -0.12 * 1.2 (global) ≈ -0.07. This is a clear Rope.
constraint_indexing:constraint_classification(eu_mercosur_trade_agreement, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Default analytical context. Recognizes both the coordination function and
% the asymmetric extraction. Engine derives d ≈ 0.72 → f(d) ≈ 1.15.
% χ = 0.48 * 1.15 * 1.2 (global) ≈ 0.66. With genuine coordination, this is a Tangled Rope.
constraint_indexing:constraint_classification(eu_mercosur_trade_agreement, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---
% The conflict between EU bodies highlights different institutional experiences.

% Perspective 4A: The Proponent Institution (European Commission)
% Sees the deal as a strategic tool (Rope), has arbitrage exit (can pursue other deals).
% As a beneficiary with arbitrage exit, d is low, χ is low.
constraint_indexing:constraint_classification(eu_mercosur_trade_agreement, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective 4B: The Opponent Institution (French Government)
% Constrained by domestic political reality (farmer protests). Experiences the deal's
% political blowback. Classified as a 'victim' of the political constraint.
% Engine derives d from victim + constrained exit → higher d → higher χ.
% They see the extractive elements more clearly because they bear the political cost.
constraint_indexing:constraint_classification(eu_mercosur_trade_agreement, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 5: THE ORGANIZED TARGET (SNARE)
% EU small farmers have political power but are economically trapped/constrained.
% Engine derives d from: victim membership + constrained exit -> high d.
% The effective extraction is still high enough to classify as a Snare.
constraint_indexing:constraint_classification(eu_mercosur_trade_agreement, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_mercosur_trade_agreement_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify gap between the most powerless victim and the primary beneficiary.
    constraint_indexing:constraint_classification(eu_mercosur_trade_agreement, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(eu_mercosur_trade_agreement, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(inter_institutional_gap) :-
    % Verify gap between proponent (EC) and opponent (France) institutions.
    constraint_indexing:constraint_classification(eu_mercosur_trade_agreement, TypeProponent, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(eu_mercosur_trade_agreement, TypeOpponent, context(agent_power(institutional), _, exit_options(constrained), _)),
    TypeProponent \= TypeOpponent.

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(eu_mercosur_trade_agreement, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    % Tangled Rope requires beneficiary, victim, and enforcement.
    narrative_ontology:constraint_beneficiary(eu_mercosur_trade_agreement, _),
    narrative_ontology:constraint_victim(eu_mercosur_trade_agreement, _),
    domain_priors:requires_active_enforcement(eu_mercosur_trade_agreement).

:- end_tests(eu_mercosur_trade_agreement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.48): High. The economic and environmental costs are not incidental; they are large, predictable outcomes concentrated on specific groups. This value places the constraint squarely in the high-extraction category.
 *   - Suppression (0.65): High. For an EU farmer or an Amazonian tribe, there are few-to-no alternatives to operating within the reality this bloc-level deal creates. It suppresses alternative market structures.
 *   - Theater (0.20): Low. While there is political signaling (e.g., environmental clauses critics call weak), the core function of liberalizing trade is real and potent.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme. For an industrial exporter (institutional/arbitrage), the deal is a pure Rope: it lowers barriers and coordinates access to new markets, creating immense value. For an indigenous community (powerless/trapped), it is a pure Snare: a destructive external force that extracts their environment and way of life with no recourse. This disparity is the defining feature of a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality is driven by a clear division of costs and benefits.
 *   - Beneficiaries: `eu_industrial_exporters` and `mercosur_agribusiness_exporters` directly benefit from market access. The `european_commission` benefits strategically. These declarations give them low `d` values.
 *   - Victims: `eu_small_farmers` and `amazon_indigenous_communities` bear direct economic and environmental costs. The `french_government` bears the political cost of farmer unrest. These declarations give them high `d` values.
 *   The engine correctly maps these structural roles to high/low effective extraction (χ), producing the perspectival gap.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This is a canonical example of inter-institutional perspectival gaps. The European Commission and the French Government are both `institutional` actors, but experience the constraint differently.
 *   - The Commission has `arbitrage` exit; if this deal fails, it can pivot to other strategic priorities. It is structurally a beneficiary.
 *   - The French Government has `constrained` exit; it cannot easily ignore the massive domestic farmer protests. It is a victim of the political instability the deal creates.
 *   This difference in exit options, captured in the model, correctly generates different `d` values and thus different classifications (Rope vs. Tangled Rope), modeling the real-world political tension within the EU.
 *
 * MANDATROPHY ANALYSIS:
 *   The analytical classification as a Tangled Rope is crucial for avoiding mandatrophy.
 *   - Ascribing it as a pure Rope (the beneficiary view) would be a catastrophic error, ignoring the severe, concentrated extraction.
 *   - Ascribing it as a pure Snare (the victim view) would also be an error, ignoring the very real, large-scale coordination function it serves for international trade.
 *   The Tangled Rope classification correctly identifies the hybrid nature: a mechanism that performs a genuine coordination function but does so by creating significant, asymmetrically distributed costs.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_eu_mercosur_trade_agreement,
    'Are the environmental protection clauses within the agreement enforceable and effective, or merely theatrical?',
    'Empirical analysis of deforestation rates and enforcement actions post-ratification (over a 5-10 year period).',
    'If clauses are effective, base_extractiveness (ε) would be lower (~0.35) and theater_ratio would be lower. If they are theatrical, the current ε=0.48 is correct and theater_ratio could even be higher.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(eu_mercosur_trade_agreement, 0, 20). % Reflects 20+ years of negotiation.

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint has a long lifecycle, with its extractive properties becoming
% more apparent over time as initial safeguards are negotiated away.
%
% Theater ratio over time (slight increase as symbolic clauses are added):
narrative_ontology:measurement(eu_mercosur_tr_t0, eu_mercosur_trade_agreement, theater_ratio, 0, 0.10).
narrative_ontology:measurement(eu_mercosur_tr_t10, eu_mercosur_trade_agreement, theater_ratio, 10, 0.15).
narrative_ontology:measurement(eu_mercosur_tr_t20, eu_mercosur_trade_agreement, theater_ratio, 20, 0.20).

% Extraction over time (increases as pro-business lobbies weaken protections):
narrative_ontology:measurement(eu_mercosur_ex_t0, eu_mercosur_trade_agreement, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(eu_mercosur_ex_t10, eu_mercosur_trade_agreement, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(eu_mercosur_ex_t20, eu_mercosur_trade_agreement, base_extractiveness, 20, 0.48).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(eu_mercosur_trade_agreement, resource_allocation).

% Network relationships (structural influence edges)
% This agreement is deeply intertwined with agricultural and environmental policy.
narrative_ontology:affects_constraint(eu_mercosur_trade_agreement, eu_common_agricultural_policy).
narrative_ontology:affects_constraint(eu_mercosur_trade_agreement, brazil_amazon_deforestation_policy).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides needed for this constraint. The structural derivation chain,
% using beneficiary/victim declarations and exit options, accurately captures
% the directionality for all key agents, including the inter-institutional gap.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */