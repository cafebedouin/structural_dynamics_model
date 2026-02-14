% ============================================================================
% CONSTRAINT STORY: eu_unanimity_rule_foreign_policy
% ============================================================================
% Version: 6.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% ============================================================================

:- module(constraint_eu_unanimity_rule_foreign_policy, []).

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
    domain_priors:emerges_naturally/1.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: eu_unanimity_rule_foreign_policy
 *   human_readable: EU Unanimity Requirement for Foreign Policy and Financial Decisions
 *   domain: geopolitical
 *
 * SUMMARY:
 *   The European Union's requirement for unanimous agreement among all member
 *   states on key foreign policy and financial matters, such as sanction
 *   regimes or large aid packages. While intended to protect national sovereignty
 *   and ensure consensus, it grants a veto to any single member, which can be
 *   used to block the will of the majority for transactional leverage. This
 *   story models the blockage of a €50 billion aid package to Ukraine by a
 *   small bloc of member states (Hungary, Slovakia, Czech Republic).
 *
 * KEY AGENTS (by structural relationship):
 *   - Ukraine: Primary target (powerless/trapped) — The direct victim of the blocked funding, with no alternative recourse.
 *   - Dissenter Bloc (Hungary, etc.): Primary beneficiary (institutional/arbitrage) — Uses the veto to extract concessions and exert influence far beyond its economic weight.
 *   - EU Majority States (Germany, France, etc.): Secondary victim (institutional/constrained) — Their collective policy goals are thwarted by the rule they are bound to uphold.
 *   - Analytical Observer: Sees the dual function of coordination and extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(eu_unanimity_rule_foreign_policy, 0.55).
domain_priors:suppression_score(eu_unanimity_rule_foreign_policy, 0.75).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(eu_unanimity_rule_foreign_policy, 0.15).       % Piton detection (>= 0.70)

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(eu_unanimity_rule_foreign_policy, extractiveness, 0.55).
narrative_ontology:constraint_metric(eu_unanimity_rule_foreign_policy, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(eu_unanimity_rule_foreign_policy, theater_ratio, 0.15).

% --- Constraint claim (must match analytical perspective type) ---
narrative_ontology:constraint_claim(eu_unanimity_rule_foreign_policy, tangled_rope).

% --- Binary flags ---
domain_priors:requires_active_enforcement(eu_unanimity_rule_foreign_policy). % Required for Tangled Rope

% --- Structural relationships (REQUIRED for non-mountain constraints) ---
% These feed the directionality derivation chain.

% Who benefits from this constraint existing?
narrative_ontology:constraint_beneficiary(eu_unanimity_rule_foreign_policy, dissenter_bloc_eu).
% Who bears disproportionate cost?
narrative_ontology:constraint_victim(eu_unanimity_rule_foreign_policy, ukraine).
narrative_ontology:constraint_victim(eu_unanimity_rule_foreign_policy, eu_majority_states).

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

% PERSPECTIVE 1: UKRAINE (THE PRIMARY TARGET)
% As the direct victim with no recourse, Ukraine experiences the unanimity rule
% as a pure trap that denies essential aid.
% Engine derives d from: victim membership + trapped exit → d ≈ 0.95 → f(d) ≈ 1.42
% → high χ. A continental scope (σ=1.1) further amplifies extraction.
constraint_indexing:constraint_classification(eu_unanimity_rule_foreign_policy, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(continental))).

% PERSPECTIVE 2: THE DISSENTER BLOC (THE PRIMARY BENEFICIARY)
% The bloc uses the rule as a tool of leverage, gaining power and concessions.
% It's a coordination rule that protects their interests.
% Engine derives d from: beneficiary membership + arbitrage exit → d ≈ 0.05 → f(d) ≈ -0.12
% → low/negative χ. They see it as a perfect Rope.
constraint_indexing:constraint_classification(eu_unanimity_rule_foreign_policy, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(continental))).

% PERSPECTIVE 3: THE EU MAJORITY (INTER-INSTITUTIONAL VICTIM)
% These institutional actors are bound by the rule, which thwarts their
% collective will. Their exit is constrained by treaty obligations.
% Engine derives d from: victim membership + constrained exit → intermediate d
% → intermediate χ. They see the extractive and coordination aspects.
constraint_indexing:constraint_classification(eu_unanimity_rule_foreign_policy, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 4: THE ANALYTICAL OBSERVER
% The observer sees the full structure: a rule with a genuine coordination
% function that has been captured to enable asymmetric extraction.
% Engine derives d ≈ 0.72 → f(d) ≈ 1.15.
constraint_indexing:constraint_classification(eu_unanimity_rule_foreign_policy, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(eu_unanimity_rule_foreign_policy_tests).

test(perspectival_gap_target_beneficiary) :-
    % Verify the gap between the primary target (Ukraine) and beneficiary (Dissenters).
    constraint_indexing:constraint_classification(eu_unanimity_rule_foreign_policy, snare, context(agent_power(powerless), _, exit_options(trapped), _)),
    constraint_indexing:constraint_classification(eu_unanimity_rule_foreign_policy, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)).

test(inter_institutional_gap) :-
    % Verify the gap between the two institutional actors.
    constraint_indexing:constraint_classification(eu_unanimity_rule_foreign_policy, rope, context(agent_power(institutional), _, exit_options(arbitrage), _)),
    constraint_indexing:constraint_classification(eu_unanimity_rule_foreign_policy, tangled_rope, context(agent_power(institutional), _, exit_options(constrained), _)).

test(analytical_view_is_tangled_rope) :-
    constraint_indexing:constraint_classification(eu_unanimity_rule_foreign_policy, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_gate_requirements_met) :-
    % A constraint is a Tangled Rope only if it has both coordination and extraction, and is enforced.
    narrative_ontology:constraint_beneficiary(eu_unanimity_rule_foreign_policy, _),
    narrative_ontology:constraint_victim(eu_unanimity_rule_foreign_policy, _),
    domain_priors:requires_active_enforcement(eu_unanimity_rule_foreign_policy).

:- end_tests(eu_unanimity_rule_foreign_policy_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   - Base Extractiveness (ε=0.55): High, as the power to veto a €50 billion strategic
 *     package represents immense extractive potential.
 *   - Suppression (0.75): High, the unanimity rule structurally eliminates the
 *     alternative of qualified majority voting, leaving no procedural recourse.
 *   - Theater (0.15): Low, the veto is a highly functional act with immediate,
 *     non-performative consequences.
 *   This combination, along with the dual presence of beneficiaries and victims,
 *   makes it a canonical Tangled Rope.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme. For the dissenter bloc, the rule is a Rope—a pure
 *   coordination mechanism that protects their sovereignty and gives them
 *   leverage (arbitrage exit). For Ukraine, it is a Snare—a trap with no exit
 *   that directly threatens its existence. This gap arises because the
 *   directionality of extraction is inverted between the two perspectives.
 *
 * DIRECTIONALITY LOGIC:
 *   - Beneficiary: `dissenter_bloc_eu`. They directly benefit by weaponizing the
 *     veto for transactional gain (e.g., unblocking frozen funds) or to enforce
 *     a divergent foreign policy.
 *   - Victims: `ukraine` and `eu_majority_states`. Ukraine bears the primary,
 *     existential cost. The EU majority bears the secondary cost of policy
 *     paralysis and diminished geopolitical influence. The engine derives
 *     directionality (d) from these declarations plus exit options, correctly
 *     assigning a low d to the bloc and a high d to Ukraine.
 *
 * INTER-INSTITUTIONAL DYNAMICS:
 *   This story highlights a key inter-institutional conflict. Both the dissenter
 *   bloc and the EU majority are `institutional` actors, but they experience the
 *   constraint differently due to their structural relationship and exit options.
 *   The dissenters have `arbitrage` exit (they can trade their veto). The
 *   majority has `constrained` exit (they are bound by the treaty). The v6.0
 *   engine captures this nuance, resulting in different classifications (Rope vs.
 *   Tangled Rope) for actors with the same power level.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification correctly identifies the dual nature of the unanimity
 *   rule. A naive analysis might label it a Snare (focusing only on Ukraine's
 *   perspective) or a Rope (focusing only on its original intent to protect
 *   sovereignty). The Tangled Rope classification avoids this by requiring proof
 *   of BOTH a coordination function (`constraint_beneficiary`) and asymmetric
 *   extraction (`constraint_victim`), providing a complete structural picture.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_eu_unanimity_rule_foreign_policy,
    'Is the unanimity rule now primarily a tool for transactional extortion, or does it still serve its intended coordination function of protecting vital national interests?',
    'Analysis of the past 20 years of veto usage: ratio of vetoes used for transactional leverage on unrelated files vs. vetoes used to block policies with direct, existential national security implications for the vetoing state.',
    'If primarily extortion, its true ε is even higher (~0.70), and it is degrading into a pure Snare. If primarily for protection, its ε is lower (~0.40) and it remains a stable (if high-friction) Tangled Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(eu_unanimity_rule_foreign_policy, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data shows the rule's extractive potential increasing over time as
% the EU expanded and geopolitical friction grew, making the veto more valuable.
% This is a case of extraction_accumulation drift.

% Theater ratio over time (stable and low):
narrative_ontology:measurement(eu_unanimity_tr_t0, eu_unanimity_rule_foreign_policy, theater_ratio, 0, 0.10).
narrative_ontology:measurement(eu_unanimity_tr_t5, eu_unanimity_rule_foreign_policy, theater_ratio, 5, 0.12).
narrative_ontology:measurement(eu_unanimity_tr_t10, eu_unanimity_rule_foreign_policy, theater_ratio, 10, 0.15).

% Extraction over time (increasing significantly):
narrative_ontology:measurement(eu_unanimity_ex_t0, eu_unanimity_rule_foreign_policy, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(eu_unanimity_ex_t5, eu_unanimity_rule_foreign_policy, base_extractiveness, 5, 0.35).
narrative_ontology:measurement(eu_unanimity_ex_t10, eu_unanimity_rule_foreign_policy, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% This rule is a core procedural enforcement mechanism.
narrative_ontology:coordination_type(eu_unanimity_rule_foreign_policy, enforcement_mechanism).

% Network relationships (structural influence edges)
% The unanimity rule directly impacts the viability of funding and support mechanisms.
narrative_ontology:affects_constraint(eu_unanimity_rule_foreign_policy, ukraine_war_funding).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

% No overrides are needed for this story. The standard derivation chain, using
% the declared beneficiary/victim groups and the different exit_options for
% the institutional actors (`arbitrage` vs. `constrained`), is sufficient to
% model the complex directionality of this constraint accurately.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */