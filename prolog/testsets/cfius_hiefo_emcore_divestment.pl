% ============================================================================
% CONSTRAINT STORY: cfius_hiefo_emcore_divestment
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-22
% ============================================================================

:- module(constraint_cfius_hiefo_emcore_divestment, []).

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
 *   constraint_id: cfius_hiefo_emcore_divestment
 *   human_readable: CFIUS authority to force divestment of strategic assets
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   This constraint models the power of the Committee on Foreign Investment
 *   in the United States (CFIUS) to retroactively unwind a transaction on
 *   national security grounds. The specific instance is the 2017 executive
 *   order forcing the Chinese company Hiefo to divest its ownership of the
 *   U.S. tech firm Emcore, which produces sensitive aerospace and defense
 *   components. The constraint is the legal and executive framework that
 *   enables this forced sale.
 *
 * KEY AGENTS (by structural relationship):
 *   - foreign_investors_in_strategic_sectors (e.g., Hiefo): Primary target (organized/trapped) — bears the extraction of a forced sale at a potentially suboptimal price and timing.
 *   - employees_of_divested_firm: Secondary target (powerless/trapped) — face job insecurity and disruption from the forced sale.
 *   - us_national_security_apparatus (e.g., CFIUS): Primary beneficiary (institutional/arbitrage) — achieves its goal of preventing technology transfer and securing domestic supply chains.
 *   - domestic_defense_industry: Secondary beneficiary (organized/mobile) — benefits from a protected technological ecosystem and reduced foreign competition for strategic assets.
 *   - Analytical observer: Analytical observer — sees both the valid security coordination and the coercive extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
% A forced divestment is highly extractive, representing the loss of a major asset.
domain_priors:base_extractiveness(cfius_hiefo_emcore_divestment, 0.65).
% The target has no alternative but to comply with the executive order.
domain_priors:suppression_score(cfius_hiefo_emcore_divestment, 0.90).   % Structural property (raw, unscaled).
% The action is functional (preventing tech transfer), not performative.
domain_priors:theater_ratio(cfius_hiefo_emcore_divestment, 0.10).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(cfius_hiefo_emcore_divestment, extractiveness, 0.65).
narrative_ontology:constraint_metric(cfius_hiefo_emcore_divestment, suppression_requirement, 0.90).
narrative_ontology:constraint_metric(cfius_hiefo_emcore_divestment, theater_ratio, 0.10).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(cfius_hiefo_emcore_divestment, tangled_rope).
narrative_ontology:human_readable(cfius_hiefo_emcore_divestment, "CFIUS authority to force divestment of strategic assets").
narrative_ontology:topic_domain(cfius_hiefo_emcore_divestment, "geopolitical/economic").

% --- Binary flags ---
% This constraint requires the full power of the executive branch to enforce.
domain_priors:requires_active_enforcement(cfius_hiefo_emcore_divestment). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain: the engine computes
% d (directionality) from agent membership in these groups + exit_options.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(cfius_hiefo_emcore_divestment, us_national_security_apparatus).
narrative_ontology:constraint_beneficiary(cfius_hiefo_emcore_divestment, domestic_defense_industry).

% Who bears disproportionate cost?
narrative_ontology:constraint_victim(cfius_hiefo_emcore_divestment, foreign_investors_in_strategic_sectors).
narrative_ontology:constraint_victim(cfius_hiefo_emcore_divestment, employees_of_divested_firm).

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

% PERSPECTIVE 1: THE POWERLESS TARGET (SNARE)
% Employees of the divested firm face job insecurity and disruption. They have
% no influence over the decision and are trapped by the consequences.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
% χ = 0.65 * f(0.95) * σ(national) ≈ 0.65 * 1.42 * 1.0 ≈ 0.923 (Snare)
constraint_indexing:constraint_classification(cfius_hiefo_emcore_divestment, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE ORGANIZED TARGET (SNARE)
% Hiefo, the foreign investor, is subject to a binding executive order. They
% are trapped; their only option is to divest.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42 → high χ
% χ = 0.65 * f(0.95) * σ(national) ≈ 0.65 * 1.42 * 1.0 ≈ 0.923 (Snare)
constraint_indexing:constraint_classification(cfius_hiefo_emcore_divestment, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 3: THE PRIMARY BENEFICIARY (ROPE)
% The US government (CFIUS) wields the constraint as a tool. It has full
% arbitrage over its application.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12 → negative χ
% χ = 0.65 * f(0.05) * σ(national) ≈ 0.65 * -0.12 * 1.0 ≈ -0.078 (Rope)
constraint_indexing:constraint_classification(cfius_hiefo_emcore_divestment, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% An observer sees both the coordination function (protecting national security)
% and the coercive, asymmetric extraction (forcing a sale).
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15 for analytical perspective.
% χ = 0.65 * f(0.72) * σ(global) ≈ 0.65 * 1.15 * 1.2 ≈ 0.897 (Tangled Rope)
constraint_indexing:constraint_classification(cfius_hiefo_emcore_divestment, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 5: SECONDARY BENEFICIARY (ROPE)
% The domestic defense industry benefits from the protection but does not
% control the mechanism. They have mobile exit (lobbying, adapting).
% Engine derives d from: beneficiary + mobile exit -> d ≈ 0.15 -> f(d) ≈ -0.01 -> near-zero χ
% χ = 0.65 * f(0.15) * σ(national) ≈ 0.65 * -0.01 * 1.0 ≈ -0.0065 (Rope)
constraint_indexing:constraint_classification(cfius_hiefo_emcore_divestment, rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cfius_hiefo_emcore_divestment_tests).

test(perspectival_gap_target_beneficiary) :-
    constraint_indexing:constraint_classification(cfius_hiefo_emcore_divestment, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(cfius_hiefo_emcore_divestment, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    true.

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(cfius_hiefo_emcore_divestment, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    narrative_ontology:constraint_beneficiary(cfius_hiefo_emcore_divestment, _),
    narrative_ontology:constraint_victim(cfius_hiefo_emcore_divestment, _),
    domain_priors:requires_active_enforcement(cfius_hiefo_emcore_divestment).

:- end_tests(cfius_hiefo_emcore_divestment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.65): High. Represents the significant financial loss and strategic setback of being forced to sell a major corporate asset under duress.
 *   - Suppression (0.90): Near total. The presidential order backing a CFIUS recommendation offers no effective appeal or alternative for the targeted entity. Compliance is the only option.
 *   - The combination of a valid coordination function and high, coercive extraction makes this a textbook Tangled Rope from a systemic viewpoint.
 *
 * PERSPECTIVAL GAP:
 *   - For the US Government (beneficiary), the CFIUS process is a pure coordination tool (Rope). It aligns multiple agencies and industry interests to achieve a shared national security goal with minimal overhead *for them*. The extractive cost is entirely externalized.
 *   - For the targets (both the foreign investor Hiefo and the firm's employees), the process is a pure Snare. They are trapped by a rule they cannot influence, which extracts immense value (for the investor) or creates severe instability (for the employees) with no corresponding benefit. The coordination function is invisible or irrelevant to them; they only experience the coercion.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiaries: The `us_national_security_apparatus` benefits directly by preventing potential technology transfer. The `domestic_defense_industry` benefits indirectly through a more secure and less competitive domestic market for strategic assets.
 *   - Victims: `foreign_investors_in_strategic_sectors` and `employees_of_divested_firm` are the clear victims. They bear the full financial and strategic cost of the divestment order. The system's directionality logic correctly assigns a very high `d` value to these groups when their exit is `trapped`, leading to the Snare classification.
 *
 * MANDATROPHY ANALYSIS:
 *   This is a critical case for preventing Mandatrophy. A simplistic analysis might label CFIUS as pure state coercion (Snare). Conversely, a purely pro-government view would label it as necessary coordination (Rope). The Deferential Realism framework, by indexing to perspective, correctly identifies that it is *both simultaneously*. The analytical classification of Tangled Rope acknowledges the legitimate coordination function while refusing to ignore the highly asymmetric extraction imposed on its targets. This prevents the mislabeling of a hybrid system as either pure coordination or pure extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_cfius_hiefo_emcore_divestment,
    'Is the national security risk cited by CFIUS a material threat or a pretext for economic protectionism?',
    'Access to the classified intelligence assessments that formed the basis of the CFIUS recommendation and executive order.',
    'If the threat is material, the coordination function is primary and the extraction is a justified side effect (Tangled Rope). If it is pretext, the extraction is the primary goal and the security rationale is theater (closer to a pure Snare).',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing. Interval represents the modern era of CFIUS.
narrative_ontology:interval(cfius_hiefo_emcore_divestment, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the intensification of CFIUS's power over time,
% from an advisory body to one with powerful enforcement capabilities.
% This represents extraction_accumulation, as the mechanism's potential to
% extract value has increased.
% T=0: Pre-Exon-Florio (advisory)
% T=5: Post-Exon-Florio (blocking power)
% T=10: Post-FIRRMA (modern, assertive era)

% Theater ratio over time (remains low and functional)
narrative_ontology:measurement(cfius_hiefo_emcore_divestment_tr_t0, cfius_hiefo_emcore_divestment, theater_ratio, 0, 0.10).
narrative_ontology:measurement(cfius_hiefo_emcore_divestment_tr_t5, cfius_hiefo_emcore_divestment, theater_ratio, 5, 0.10).
narrative_ontology:measurement(cfius_hiefo_emcore_divestment_tr_t10, cfius_hiefo_emcore_divestment, theater_ratio, 10, 0.10).

% Extraction over time (shows significant accumulation)
narrative_ontology:measurement(cfius_hiefo_emcore_divestment_ex_t0, cfius_hiefo_emcore_divestment, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(cfius_hiefo_emcore_divestment_ex_t5, cfius_hiefo_emcore_divestment, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(cfius_hiefo_emcore_divestment_ex_t10, cfius_hiefo_emcore_divestment, base_extractiveness, 10, 0.65).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type: It is an enforcement mechanism for national security policy.
narrative_ontology:coordination_type(cfius_hiefo_emcore_divestment, enforcement_mechanism).

% Network relationships: CFIUS power directly influences the integrity and
% composition of strategic supply chains.
narrative_ontology:affects_constraint(cfius_hiefo_emcore_divestment, semiconductor_supply_chain_integrity).


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are necessary for this constraint. The structural derivation
% from beneficiary/victim declarations combined with the distinct exit_options
% (trapped vs. arbitrage) accurately captures the directionality for the
% key agents involved.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */