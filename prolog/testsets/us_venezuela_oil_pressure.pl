% ============================================================================
% CONSTRAINT STORY: us_venezuela_oil_pressure
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-28
% ============================================================================

:- module(constraint_us_venezuela_oil_pressure, []).

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
 *   constraint_id: us_venezuela_oil_pressure
 *   human_readable: "US Geopolitical & Economic Pressure on Venezuela's Oil Sector"
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   This constraint models the decades-long United States policy of exerting
 *   diplomatic, economic, and covert pressure on Venezuela to ensure its
 *   vast oil reserves remain accessible and favorable to US corporate and
 *   geopolitical interests. This policy intensified significantly when
 *   Venezuelan governments (e.g., Chávez, Maduro) pursued nationalization
 *   and sought greater sovereignty over their resources, leading to sanctions
 *   and other coercive measures. The constraint functions to coordinate
 *   resource access for US entities while extracting wealth and sovereignty
 *   from Venezuela.
 *
 * KEY AGENTS (by structural relationship):
 *   - US Oil Corporations & Geopolitical Interests: Primary beneficiary (institutional/arbitrage) — secure preferential access to oil reserves and maintain regional influence.
 *   - Venezuelan Sovereignty Advocates (incl. the state & public): Primary target (organized/trapped) — bear the costs of sanctions, lost revenue, and reduced political autonomy.
 *   - US Government: Institutional enforcer (institutional/arbitrage) — deploys sanctions and diplomatic pressure to maintain the constraint.
 *   - Venezuelan Government: Institutional target (institutional/constrained) — attempts to govern while being the direct object of the pressure campaign.
 *   - Analytical Observer: Sees the full structure of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(us_venezuela_oil_pressure, 0.68). % Represents lost revenue & sovereignty.
domain_priors:suppression_score(us_venezuela_oil_pressure, 0.85).   % Sanctions, diplomatic threats, coup support suppress alternatives.
domain_priors:theater_ratio(us_venezuela_oil_pressure, 0.40).       % Rhetoric of "democracy promotion" vs underlying economic drivers.

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(us_venezuela_oil_pressure, extractiveness, 0.68).
narrative_ontology:constraint_metric(us_venezuela_oil_pressure, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(us_venezuela_oil_pressure, theater_ratio, 0.40).

% --- NL Profile Metrics (required for mountain constraints) ---
% N/A for this constraint.

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(us_venezuela_oil_pressure, tangled_rope).
narrative_ontology:human_readable(us_venezuela_oil_pressure, "US Geopolitical & Economic Pressure on Venezuela's Oil Sector").

% --- Binary flags ---
domain_priors:requires_active_enforcement(us_venezuela_oil_pressure). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.
%
% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(us_venezuela_oil_pressure, us_oil_corporations).
narrative_ontology:constraint_beneficiary(us_venezuela_oil_pressure, us_geopolitical_interests).
%
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(us_venezuela_oil_pressure, venezuelan_sovereignty_advocates).
narrative_ontology:constraint_victim(us_venezuela_oil_pressure, venezuelan_public).
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
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMARY TARGET (VENEZUELAN PUBLIC)
% As a population subject to sanctions and economic hardship, they are powerless
% and trapped. The coordination aspect is invisible; they only experience the
% extraction. The high ε and suppression result in a clear Snare classification.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42.
% χ = 0.68 * 1.42 * 1.0 (national scope) ≈ 0.96, which is >> 0.66 (Snare threshold).
constraint_indexing:constraint_classification(us_venezuela_oil_pressure, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE PRIMARY BENEFICIARY (US OIL CORPORATIONS)
% For the primary beneficiaries, the constraint is a pure coordination mechanism
% that guarantees favorable terms and access. They are insulated from the costs.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12.
% χ = 0.68 * -0.12 * 1.0 ≈ -0.08, indicating a net subsidy.
constraint_indexing:constraint_classification(us_venezuela_oil_pressure, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER
% The analytical view sees both the coordination function (for the US) and the
% asymmetric extraction (from Venezuela). With high ε, high suppression, and active
% enforcement, it fits the definition of a Tangled Rope perfectly.
constraint_indexing:constraint_classification(us_venezuela_oil_pressure, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% --- INTER-INSTITUTIONAL PERSPECTIVES ---
% The constraint operates between two state-level institutional actors with
% vastly different structural positions.

% Perspective 4A: US Government (Institutional Enforcer)
% As the enforcer and a direct beneficiary of geopolitical influence, the US
% government perceives the policy as a coordination tool (Rope) for achieving
% foreign policy and economic objectives. Exit is 'arbitrage' as it can shift policy.
constraint_indexing:constraint_classification(us_venezuela_oil_pressure, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Perspective 4B: Venezuelan Government (Institutional Target)
% As the target, the Venezuelan state experiences the constraint as highly
% coercive. Its institutional power is undermined, and its exit options are
% severely limited ('constrained'). It is a victim. The directionality `d` is
% high, leading to a Tangled Rope classification that borders on Snare.
constraint_indexing:constraint_classification(us_venezuela_oil_pressure, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).


/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(us_venezuela_oil_pressure_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify the core perspectival gap between the powerless target and institutional beneficiary.
    constraint_indexing:constraint_classification(us_venezuela_oil_pressure, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(us_venezuela_oil_pressure, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    format('Perspectival gap validated: powerless sees Snare, institutional sees Rope.~n').

test(inter_institutional_gap) :-
    % Verify the gap between the two institutional actors.
    constraint_indexing:constraint_classification(us_venezuela_oil_pressure, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(us_venezuela_oil_pressure, tangled_rope, context(agent_power(institutional), _, exit_options(constrained), _)),
    format('Inter-institutional gap validated: arbitrage exit sees Rope, constrained exit sees Tangled Rope.~n').

test(analytical_claim_matches_type) :-
    narrative_ontology:constraint_claim(us_venezuela_oil_pressure, tangled_rope),
    constraint_indexing:constraint_classification(us_venezuela_oil_pressure, tangled_rope, context(agent_power(analytical), _, _, _)),
    format('Analytical claim matches Tangled Rope type.~n').

test(tangled_rope_gates_pass) :-
    narrative_ontology:constraint_beneficiary(us_venezuela_oil_pressure, _),
    narrative_ontology:constraint_victim(us_venezuela_oil_pressure, _),
    domain_priors:requires_active_enforcement(us_venezuela_oil_pressure),
    format('Tangled Rope structural gates (beneficiary, victim, enforcement) passed.~n').


:- end_tests(us_venezuela_oil_pressure_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.68): High, reflecting the immense value transfer (in terms of oil revenue, discounted prices, and geopolitical control) from Venezuela to US-aligned interests.
 *   - Suppression (0.85): Extremely high, as the policy's effectiveness relies on actively preventing Venezuela from pursuing alternative economic partners or policies through sanctions, diplomatic isolation, and support for opposition movements.
 *   - The combination of a clear coordination function (for beneficiaries) and severe asymmetric extraction (from victims) makes this a canonical Tangled Rope from an analytical view.
 *
 * PERSPECTIVAL GAP:
 *   The gap is immense and defines the conflict. For US oil corporations, this is a 'Rope'—a rational system for coordinating access to a vital resource. For the Venezuelan public, it is a 'Snare'—an inescapable trap that drains their national wealth and causes widespread hardship. This gap is not a matter of opinion but a direct result of their different structural positions relative to the flow of value.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: 'us_oil_corporations' and 'us_geopolitical_interests' directly benefit from suppressed oil prices and control over a strategic asset. The system is designed to subsidize their operations. The engine derives a low `d` for them.
 *   - Victims: 'venezuelan_sovereignty_advocates' and the 'venezuelan_public' bear the direct costs. The constraint extracts resources and political autonomy from them. The engine derives a high `d` for them.
 *   This clear directionality of value flow is the core of the constraint's structure.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This story is a powerful example of inter-institutional asymmetry. Both the US and Venezuelan governments are 'institutional' actors, but their relationship to the constraint is opposite. The US government, with 'arbitrage' exit options (it can choose from a menu of foreign policy tools), acts as the enforcer and sees a Rope. The Venezuelan government, with 'constrained' exit options (it cannot easily escape US hegemony or sanctions), is the target and experiences a coercive Tangled Rope. The model captures this difference not by changing the power atom, but by differentiating their exit options and victim/beneficiary status, which drives the directionality calculation.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification avoids two common errors. First, it rejects the simplistic "pure evil" view by acknowledging the genuine (if self-serving) coordination function, classifying it as a Tangled Rope, not just a Snare. Second, it rejects the beneficiary's narrative that this is purely a "coordination" mechanism (a Rope) by rigorously accounting for the massive, asymmetric extraction imposed on the target. The framework forces a reconciliation of both functions into a single, more accurate structural classification.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_us_venezuela_oil_pressure,
    'Is the primary driver of the policy direct economic extraction for corporations, or is it a geopolitical strategy for regional stability where corporate benefit is a secondary effect?',
    'Declassified internal policy memos from multiple US administrations detailing the cost-benefit analysis behind sanctions and diplomatic strategies.',
    'If primarily economic, the ε score is accurate. If primarily geopolitical, the nature of "extraction" shifts from monetary value to political compliance, and ε might be slightly lower but suppression even higher.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(us_venezuela_oil_pressure, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint has intensified over decades, moving from a more balanced (though still unequal)
% relationship to one of overt economic warfare. This is classic extraction_accumulation drift.
% T=0 (1970s), T=5 (1990s/Chavez era), T=10 (2010s/Maduro era).

% Theater ratio over time: rhetoric shifts from business partnership to human rights.
narrative_ontology:measurement(us_ven_oil_tr_t0, us_venezuela_oil_pressure, theater_ratio, 0, 0.15).
narrative_ontology:measurement(us_ven_oil_tr_t5, us_venezuela_oil_pressure, theater_ratio, 5, 0.30).
narrative_ontology:measurement(us_ven_oil_tr_t10, us_venezuela_oil_pressure, theater_ratio, 10, 0.40).

% Extraction over time: pressure and sanctions increase the value transfer.
narrative_ontology:measurement(us_ven_oil_ex_t0, us_venezuela_oil_pressure, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(us_ven_oil_ex_t5, us_venezuela_oil_pressure, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(us_ven_oil_ex_t10, us_venezuela_oil_pressure, base_extractiveness, 10, 0.68).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% This constraint is a coercive mechanism for controlling access to a key commodity.
narrative_ontology:coordination_type(us_venezuela_oil_pressure, resource_allocation).

% The pressure on Venezuela's oil sector has direct, structural impacts on the
% global energy market and other geopolitical constraints.
narrative_ontology:affects_constraint(us_venezuela_oil_pressure, global_oil_prices).
narrative_ontology:affects_constraint(us_venezuela_oil_pressure, us_sanctions_regime_credibility).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this story. The standard derivation chain,
% using beneficiary/victim declarations in combination with the distinct
% exit options for each agent (trapped, constrained, arbitrage), accurately
% models the directionality and power asymmetries of the constraint.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */